/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package org.ofbiz.entity.tenant;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicLong;
import java.util.function.BooleanSupplier;
import java.util.function.Consumer;
import java.util.function.LongSupplier;
import java.util.regex.Pattern;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.Tenants;

/**
 * SCIPIO: 4.0.0: Pooled runtime (W1-01c, G17): per-store limits of heavy work, so that one busy store does not slow
 * the other stores of the same JVM.
 *
 * <p>Heavy work: requests that match {@code tenant.resolver.heavyRequestPattern} (reports, exports, back-office
 * apps) and jobs whose service matches {@code tenant.heavy.servicePattern} (reindex, imports, exports). Each store
 * has one {@link Gate} with the limits of its plan ({@link Tenants.Plan}); a plan change updates the limits in place:</p>
 * <ul>
 * <li>heavyRequestSlots: heavy requests of the store that run at once;</li>
 * <li>heavyQueue: heavy requests that wait for a slot, each at most {@code tenant.heavy.maxWaitMs};</li>
 * <li>heavyShare, heavyBurstSeconds: a time budget. The heavy work of the store may run heavyShare percent of the
 *     time of one thread, on average; a full budget gives heavyBurstSeconds at full speed.</li>
 * </ul>
 * <p>Enforcement of the budget: a heavy thread pays its run time (wall time: it includes the database work) into the
 * budget at its database calls ({@link #charge}) and at its end. It never waits there: a thread at a database call
 * can hold a connection, an open result set, row locks and a transaction. A thread waits for the budget only before
 * it starts, when it holds no transaction, no database resource and no JVM slot:</p>
 * <ol>
 * <li>a heavy request waits in the store queue ({@link #enterRequest}), or gets 429 with Retry-After;</li>
 * <li>a heavy job waits before its service starts ({@link #enterJob}, at most tenant.heavy.maxWaitMs; never when a
 *     transaction is in place).</li>
 * </ol>
 * <p>A long report or job thus runs to its end at full speed. Its debt (at most heavyBurstSeconds of run time) delays
 * the next heavy work of the store. The budget is best effort: only the URIs and services of the two patterns pay.
 * Other work of the store has the normal request slots, the plan's jobThreads and the plan's dbPoolMax.</p>
 * <p>All stores together run at most {@code tenant.heavy.maxPerJvm} heavy requests at once (fair queue; 503 with
 * Retry-After after the wait). Only running requests hold a JVM slot. Queued and running jobs per store (plan
 * jobThreads) are counted here for the job poller. The state of a deleted store is dropped.</p>
 *
 * <p>Runtime only: never call the static methods during the build (the static initializers of Debug load DelegatorFactory, which is not on the build class path).
 * The nested classes have no runtime dependencies (unit tests).</p>
 */
public final class TenantLoad {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final long MAX_WAIT_MS = UtilProperties.getPropertyAsLong("general", "tenant.heavy.maxWaitMs", 10000L);
    private static final Pattern HEAVY_SERVICE = Pattern.compile(UtilProperties.getPropertyValue("general",
            "tenant.heavy.servicePattern", "rebuildSolrIndex.*|.*(Export|Import|Report|Sitemap).*"));
    /** tenant.resolver.maxConcurrentHeavyRequests: -1 = the plan's heavyRequestSlots (the resolver handles 0 = off) */
    private static final int HEAVY_SLOTS = UtilProperties.getPropertyAsInteger("general", "tenant.resolver.maxConcurrentHeavyRequests", -1);
    private static final int MAX_PER_JVM = maxPerJvm();
    /** Interval of the check for deleted stores. */
    private static final long SWEEP_MS = TimeUnit.MINUTES.toMillis(10);

    private static final Limiter LIMITER = new Limiter(MAX_PER_JVM, MAX_WAIT_MS, TenantLoad::inTransaction, TimeUnit.NANOSECONDS::sleep);
    private static final AtomicLong nextSweep = new AtomicLong();

    private TenantLoad() {}

    /** True in the pooled runtime (many stores in one JVM). */
    public static boolean isPooled() {
        return Tenants.isPooled();
    }

    private static int maxPerJvm() {
        int value = UtilProperties.getPropertyAsInteger("general", "tenant.heavy.maxPerJvm", -1);
        return (value > 0) ? value : Math.max(2, Runtime.getRuntime().availableProcessors() / 2);
    }

    /** True when the thread has a transaction, or when the status is not known (then the thread must not wait). */
    private static boolean inTransaction() {
        try {
            return TransactionUtil.isTransactionInPlace();
        } catch (Exception e) {
            return true;
        }
    }

    /** Sleeps the thread (a parameter, for unit tests). */
    @FunctionalInterface
    public interface Sleeper {
        void sleep(long nanos) throws InterruptedException;
    }

    /**
     * The limits and the state of the heavy work of one store: slots, a queue, and a time budget (token bucket of
     * nanoseconds; it fills at share nanoseconds per nanosecond up to burst, and the debt stops at -burst).
     */
    public static final class Gate {
        private static final long MIN_DEBT_NANOS = TimeUnit.SECONDS.toNanos(1);
        private final LongSupplier clock;
        private int slots;
        private int queue;
        private int sharePercent;
        private int burstSeconds;
        private double share;
        private long burstNanos;
        private int running;
        private int waiting;
        private double budgetNanos;
        private long refilled;

        /**
         * @param slots heavy requests at once (0 = no limit)
         * @param queue heavy requests that may wait for a slot
         * @param sharePercent percent of the time of one thread (0 = no budget)
         * @param burstSeconds the full budget
         * @param clock nanosecond clock (System::nanoTime)
         */
        public Gate(int slots, int queue, int sharePercent, int burstSeconds, LongSupplier clock) {
            this.clock = clock;
            this.refilled = clock.getAsLong();
            apply(slots, queue, sharePercent, burstSeconds);
            this.budgetNanos = burstNanos;
        }

        private void apply(int slots, int queue, int sharePercent, int burstSeconds) {
            this.slots = slots;
            this.queue = Math.max(0, queue);
            this.sharePercent = Math.max(0, sharePercent);
            this.burstSeconds = Math.max(0, burstSeconds);
            this.share = this.sharePercent / 100.0;
            this.burstNanos = TimeUnit.SECONDS.toNanos(this.burstSeconds);
        }

        /**
         * Sets new limits (a plan change). The running and waiting requests and the budget stay; the budget is at most
         * the new burst.
         * @return true when a limit changed
         */
        public synchronized boolean setLimits(int slots, int queue, int sharePercent, int burstSeconds) {
            if (this.slots == slots && this.queue == Math.max(0, queue) && this.sharePercent == Math.max(0, sharePercent)
                    && this.burstSeconds == Math.max(0, burstSeconds)) {
                return false;
            }
            refill();
            apply(slots, queue, sharePercent, burstSeconds);
            budgetNanos = Math.max(-maxDebtNanos(), Math.min(burstNanos, budgetNanos));
            notifyAll();
            return true;
        }

        private long maxDebtNanos() {
            return Math.max(burstNanos, MIN_DEBT_NANOS);
        }

        private void refill() {
            long now = clock.getAsLong();
            if (share > 0) {
                budgetNanos = Math.min(burstNanos, budgetNanos + (now - refilled) * share);
            }
            refilled = now;
        }

        /** Nanoseconds until the budget is no longer negative. */
        private long debtNanos() {
            return (share > 0 && budgetNanos < 0) ? (long) (-budgetNanos / share) : 0L;
        }

        private boolean free() {
            return (slots <= 0 || running < slots) && budgetNanos >= 0;
        }

        private static long retryAfterMs(long debtNanos) {
            return Math.max(1000L, TimeUnit.NANOSECONDS.toMillis(debtNanos));
        }

        /**
         * Takes a slot for a heavy request: at once, or after a wait in the queue of at most maxWaitMs.
         * @return 0 when the request got a slot (call {@link #exit}), else the milliseconds after which to try again
         */
        public synchronized long enter(long maxWaitMs) throws InterruptedException {
            refill();
            if (free()) {
                running++;
                return 0L;
            }
            long maxWaitNanos = TimeUnit.MILLISECONDS.toNanos(Math.max(0L, maxWaitMs));
            if (waiting >= queue || debtNanos() > maxWaitNanos) {
                return retryAfterMs(debtNanos());
            }
            waiting++;
            try {
                long deadline = clock.getAsLong() + maxWaitNanos;
                while (true) {
                    long left = deadline - clock.getAsLong();
                    if (left <= 0) {
                        return retryAfterMs(debtNanos());
                    }
                    // a slot comes back with notifyAll; the budget comes back with time
                    long nap = (budgetNanos < 0) ? Math.min(left, Math.max(debtNanos(), TimeUnit.MILLISECONDS.toNanos(1))) : left;
                    TimeUnit.NANOSECONDS.timedWait(this, nap);
                    refill();
                    if (free()) {
                        running++;
                        return 0L;
                    }
                }
            } finally {
                waiting--;
            }
        }

        /**
         * Pays run time into the budget. The debt stops at -burst (at least 1 s), so that one long task does not
         * block the store for longer than burst / share.
         * @return the nanoseconds until the budget is back (0 = no debt)
         */
        public synchronized long charge(long nanos) {
            refill();
            if (share <= 0) {
                return 0L;
            }
            budgetNanos = Math.max(-maxDebtNanos(), budgetNanos - nanos);
            return debtNanos();
        }

        /** Gives back a slot that {@link #enter} returned 0 for. */
        public synchronized void exit() {
            running = Math.max(0, running - 1);
            notifyAll();
        }

        synchronized boolean isIdle() { return running == 0 && waiting == 0; }
        public synchronized int getRunning() { return running; }
        public synchronized int getWaiting() { return waiting; }
        /** The budget in milliseconds (negative: the store is over its share). */
        public synchronized long getBudgetMs() { refill(); return TimeUnit.NANOSECONDS.toMillis((long) budgetNanos); }

        @Override
        public synchronized String toString() {
            return "slots=" + slots + ", queue=" + queue + ", share=" + sharePercent + "%, burst=" + burstSeconds + "s";
        }
    }

    /** The heavy work of one thread: a request (with a store slot and a JVM slot) or a job (budget only). */
    public static final class Ticket {
        /** A thread pays into the budget at most this often at database calls (10 ms: the charge stays cheap). */
        private static final long CHARGE_INTERVAL_NANOS = 10_000_000L;
        private final String tenantId;
        private final Gate gate;
        private final boolean request;
        private long lastCharge;

        Ticket(String tenantId, Gate gate, boolean request) {
            this.tenantId = tenantId;
            this.gate = gate;
            this.request = request;
            this.lastCharge = System.nanoTime();
        }

        public String getTenantId() { return tenantId; }

        /** Pays the run time since the last charge. @return the store's debt in nanoseconds */
        long charge(long now) {
            long debt = gate.charge(now - lastCharge);
            lastCharge = now;
            return debt;
        }
    }

    /** The result of {@link #enterRequest}: a ticket, or a refusal with the HTTP status and Retry-After seconds. */
    public static final class Admission {
        static final Admission NO_LIMIT = new Admission(null, 200, 0L);
        private final Ticket ticket;
        private final int status;
        private final long retryAfterSeconds;

        Admission(Ticket ticket, int status, long retryAfterMs) {
            this.ticket = ticket;
            this.status = status;
            this.retryAfterSeconds = Math.max(1L, (retryAfterMs + 999L) / 1000L);
        }

        public boolean isAdmitted() { return ticket != null; }
        public Ticket getTicket() { return ticket; }
        /** 429: the store is over its own limit; 503: all heavy slots of the JVM are busy. */
        public int getStatus() { return status; }
        public long getRetryAfterSeconds() { return retryAfterSeconds; }
    }

    /**
     * The state of all stores of the JVM: gates, JVM slots, the heavy work of each thread and the job counts. No
     * runtime dependencies (unit tests); {@link TenantLoad} holds the one instance of the JVM.
     */
    static final class Limiter {
        private final Semaphore jvmSlots;
        private final long maxWaitMs;
        private final BooleanSupplier inTransaction;
        private final Sleeper sleeper;
        private final Map<String, Gate> gates = new ConcurrentHashMap<>();
        private final Map<String, AtomicInteger> jobsByStore = new ConcurrentHashMap<>();
        private final ThreadLocal<Ticket> held = new ThreadLocal<>();

        Limiter(int maxPerJvm, long maxWaitMs, BooleanSupplier inTransaction, Sleeper sleeper) {
            this.jvmSlots = new Semaphore(maxPerJvm, true);
            this.maxWaitMs = maxWaitMs;
            this.inTransaction = inTransaction;
            this.sleeper = sleeper;
        }

        /** The one gate of the store, with these limits; onChange gets a new gate or a gate whose limits changed. */
        Gate gate(String tenantId, int slots, int queue, int sharePercent, int burstSeconds, LongSupplier clock, Consumer<Gate> onChange) {
            Gate gate = gates.get(tenantId);
            if (gate == null) {
                Gate created = new Gate(slots, queue, sharePercent, burstSeconds, clock);
                gate = gates.putIfAbsent(tenantId, created);
                if (gate == null) {
                    onChange.accept(created);
                    return created;
                }
            }
            if (gate.setLimits(slots, queue, sharePercent, burstSeconds)) {
                onChange.accept(gate);
            }
            return gate;
        }

        /** A store slot (with the store queue and budget), then a JVM slot. Each refusal gives back what it took. */
        Admission enterRequest(String tenantId, Gate gate) {
            if (tenantId == null || held.get() != null) {
                return Admission.NO_LIMIT;
            }
            long start = System.nanoTime();
            boolean storeSlot = false;
            boolean jvmSlot = false;
            try {
                long retry = gate.enter(maxWaitMs);
                if (retry > 0) {
                    return new Admission(null, 429, retry);
                }
                storeSlot = true;
                long left = maxWaitMs - TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start);
                jvmSlot = jvmSlots.tryAcquire(Math.max(0L, left), TimeUnit.MILLISECONDS);
                if (!jvmSlot) {
                    return new Admission(null, 503, 5000L);
                }
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                return new Admission(null, 503, 5000L);
            } finally {
                if (storeSlot && !jvmSlot) {
                    gate.exit();
                }
            }
            Ticket ticket = new Ticket(tenantId, gate, true);
            held.set(ticket);
            return new Admission(ticket, 200, 0L);
        }

        /**
         * Marks the thread as a heavy job of the store, then waits while the store is over its budget (at most
         * maxWaitMs). The job has no transaction and no slot yet; with a transaction in place it does not wait.
         */
        Ticket enterJob(String tenantId, Gate gate) {
            if (tenantId == null || held.get() != null) {
                return null;
            }
            Ticket ticket = new Ticket(tenantId, gate, false);
            held.set(ticket);
            long debt = ticket.charge(System.nanoTime());
            if (debt > 0 && !inTransaction.getAsBoolean()) {
                try {
                    sleeper.sleep(Math.min(debt, TimeUnit.MILLISECONDS.toNanos(maxWaitMs)));
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
                ticket.lastCharge = System.nanoTime(); // the wait is not run time
            }
            return ticket;
        }

        /** The database path: pays the run time of the thread's heavy work. Never waits. */
        void charge() {
            Ticket ticket = held.get();
            if (ticket != null) {
                long now = System.nanoTime();
                if (now - ticket.lastCharge >= Ticket.CHARGE_INTERVAL_NANOS) {
                    ticket.charge(now);
                }
            }
        }

        void exit(Ticket ticket) {
            if (ticket == null) {
                return;
            }
            if (held.get() == ticket) {
                held.remove();
            }
            ticket.charge(System.nanoTime());
            if (ticket.request) {
                ticket.gate.exit();
                jvmSlots.release();
            }
        }

        Runnable jobQueued(String tenantId) {
            AtomicInteger count = jobsByStore.computeIfAbsent(tenantId, k -> new AtomicInteger());
            count.incrementAndGet();
            AtomicInteger once = new AtomicInteger();
            return () -> {
                if (once.getAndIncrement() == 0) {
                    count.decrementAndGet();
                }
            };
        }

        int getActiveJobs(String tenantId) {
            AtomicInteger count = jobsByStore.get(tenantId);
            return (count != null) ? Math.max(0, count.get()) : 0;
        }

        int getFreeJvmSlots() {
            return jvmSlots.availablePermits();
        }

        Gate getGate(String tenantId) {
            return gates.get(tenantId);
        }

        Set<String> storeIds() {
            Set<String> ids = new HashSet<>(gates.keySet());
            ids.addAll(jobsByStore.keySet());
            return ids;
        }

        /** Drops the state of a deleted store: its gate when no request uses it, its job count when it is 0. */
        void forget(String tenantId) {
            gates.computeIfPresent(tenantId, (k, g) -> g.isIdle() ? null : g);
            jobsByStore.computeIfPresent(tenantId, (k, c) -> (c.get() <= 0) ? null : c);
        }
    }

    private static Gate gate(String tenantId) {
        sweep();
        Tenants.Plan plan = Tenants.getPlan(tenantId);
        int slots = Math.max(0, (HEAVY_SLOTS >= 0) ? HEAVY_SLOTS : plan.getHeavyRequestSlots());
        return LIMITER.gate(tenantId, slots, plan.getHeavyQueue(), plan.getHeavyShare(), plan.getHeavyBurstSeconds(), System::nanoTime, g -> {
            if (Debug.infoOn()) {
                Debug.logInfo("Tenant: heavy work of store [" + tenantId + "]: " + g + ", JVM slots=" + MAX_PER_JVM, module);
            }
        });
    }

    /** Drops the state of deleted stores, at most every SWEEP_MS. */
    private static void sweep() {
        long now = System.currentTimeMillis();
        long next = nextSweep.get();
        if (now < next || !nextSweep.compareAndSet(next, now + SWEEP_MS)) {
            return;
        }
        for (String tenantId : LIMITER.storeIds()) {
            try {
                if (!Tenants.exists(tenantId)) {
                    LIMITER.forget(tenantId);
                }
            } catch (RuntimeException e) {
                Debug.logWarning("Tenant: cannot check store [" + tenantId + "]: " + e.toString(), module);
            }
        }
    }

    /**
     * Admits a heavy request of the store: a store slot (with the store queue and budget), then a JVM slot. The caller
     * calls {@link #exit} with the ticket in finally. Store slots: tenant.resolver.maxConcurrentHeavyRequests, or the
     * plan's heavyRequestSlots (0 = no slot limit; the budget still applies when the plan has a share).
     * @return an admission; when it is not admitted, send its status with Retry-After
     */
    public static Admission enterRequest(String tenantId) {
        if (tenantId == null) {
            return Admission.NO_LIMIT;
        }
        return LIMITER.enterRequest(tenantId, gate(tenantId));
    }

    /**
     * Marks the job thread as heavy work of the store when the service matches tenant.heavy.servicePattern: the job
     * waits while its store is over its budget (before its service starts, at most tenant.heavy.maxWaitMs), then
     * pays its run time into the budget. A job takes no slot. Returns null when the job is not heavy.
     */
    public static Ticket enterJob(String tenantId, String serviceName) {
        if (tenantId == null || serviceName == null || !HEAVY_SERVICE.matcher(serviceName).matches()) {
            return null;
        }
        return LIMITER.enterJob(tenantId, gate(tenantId));
    }

    /** Ends the heavy work of the thread: pays the rest of its run time and gives back its slots. Null is a no-op. */
    public static void exit(Ticket ticket) {
        LIMITER.exit(ticket);
    }

    /**
     * The entity engine calls this before each database statement and while it reads rows. A heavy thread pays its
     * run time since the last charge into its store's budget. It never waits here (see the class comment).
     */
    public static void charge() {
        LIMITER.charge();
    }

    /** The job poller counts a queued job of a store here; the returned callback ends the count (call it once). */
    public static Runnable jobQueued(String tenantId) {
        return LIMITER.jobQueued(tenantId);
    }

    /** Jobs of the store that are queued or running in this JVM. */
    public static int getActiveJobs(String tenantId) {
        return LIMITER.getActiveJobs(tenantId);
    }

    /** Heavy slots of all stores in this JVM. */
    public static int getMaxPerJvm() {
        return MAX_PER_JVM;
    }

    /** For logs and tests: running, waiting and budget of the store's gate, or null. */
    public static String describe(String tenantId) {
        Gate g = LIMITER.getGate(tenantId);
        return (g != null) ? "running=" + g.getRunning() + ", waiting=" + g.getWaiting() + ", budgetMs=" + g.getBudgetMs() : null;
    }
}

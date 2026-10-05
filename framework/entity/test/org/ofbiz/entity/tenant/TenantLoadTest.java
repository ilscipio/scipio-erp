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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicLong;

import org.junit.jupiter.api.Test;

/**
 * SCIPIO: 4.0.0: W1-01c: the per-store limits of heavy work (G17): slots, queue and time budget of one store, and the
 * JVM-wide {@link TenantLoad.Limiter} (no runtime dependencies).
 */
public class TenantLoadTest {

    private static final long SECOND = TimeUnit.SECONDS.toNanos(1);

    private static TenantLoad.Gate gate(TenantLoad.Limiter limiter, String tenantId, int slots, int queue, int share, int burst) {
        return limiter.gate(tenantId, slots, queue, share, burst, System::nanoTime, g -> { });
    }

    @Test
    public void slotsRefuseWhenTheQueueIsFull() throws Exception {
        TenantLoad.Gate gate = new TenantLoad.Gate(1, 0, 0, 0, System::nanoTime);
        assertEquals(0L, gate.enter(0));
        long retry = gate.enter(0);
        assertTrue(retry >= 1000L, "a store over its slots gets a Retry-After: " + retry);
        gate.exit();
        assertEquals(0L, gate.enter(0));
    }

    @Test
    public void queuedRequestGetsTheSlotThatComesBack() throws Exception {
        TenantLoad.Gate gate = new TenantLoad.Gate(1, 1, 0, 0, System::nanoTime);
        assertEquals(0L, gate.enter(0));
        CompletableFuture<Long> waiter = CompletableFuture.supplyAsync(() -> {
            try {
                return gate.enter(5000);
            } catch (InterruptedException e) {
                return -1L;
            }
        });
        long until = System.currentTimeMillis() + 2000;
        while (gate.getWaiting() == 0 && System.currentTimeMillis() < until) {
            Thread.sleep(5);
        }
        assertEquals(1, gate.getWaiting());
        assertTrue(gate.enter(5000) > 0, "the queue holds one request: the next one is refused at once");
        gate.exit();
        assertEquals(0L, waiter.get(5, TimeUnit.SECONDS));
        assertEquals(1, gate.getRunning());
    }

    @Test
    public void budgetLimitsTheShareOfTime() throws Exception {
        AtomicLong now = new AtomicLong(1000 * SECOND);
        // no slot limit, 25 % of one thread, 1 s burst
        TenantLoad.Gate gate = new TenantLoad.Gate(0, 2, 25, 1, now::get);
        assertEquals(0L, gate.enter(0));
        // 2 s of heavy work: 1 s over the budget; at 25 % the budget is back after 4 s
        assertEquals(4 * SECOND, gate.charge(2 * SECOND));
        long retry = gate.enter(0);
        assertEquals(4000L, retry, "a new heavy request of the store waits for its budget");
        now.addAndGet(4 * SECOND);
        assertEquals(0L, gate.enter(0));
        assertEquals(0L, gate.getBudgetMs());
        // the budget fills up to the burst only
        now.addAndGet(100 * SECOND);
        assertEquals(1000L, gate.getBudgetMs());
    }

    /** Finding 8: the debt stops at -burst. */
    @Test
    public void debtStopsAtMinusBurst() {
        AtomicLong now = new AtomicLong(0);
        TenantLoad.Gate gate = new TenantLoad.Gate(0, 2, 25, 2, now::get);
        // 1000 s of run time: the debt stops at -2 s, so the budget is back after 2 s / 25 % = 8 s
        assertEquals(8 * SECOND, gate.charge(1000 * SECOND));
        assertEquals(-2000L, gate.getBudgetMs());
    }

    @Test
    public void noShareMeansNoBudget() throws Exception {
        TenantLoad.Gate gate = new TenantLoad.Gate(0, 0, 0, 0, System::nanoTime);
        assertEquals(0L, gate.charge(1000 * SECOND));
        assertEquals(0L, gate.enter(0));
        assertEquals(0L, gate.enter(0));
    }

    /** Finding 6: one gate per store; a plan change keeps the running requests and the budget. Deleted stores go. */
    @Test
    public void planChangeKeepsTheGate() throws Exception {
        TenantLoad.Limiter limiter = new TenantLoad.Limiter(2, 0, () -> false, n -> { });
        AtomicLong now = new AtomicLong(0);
        List<TenantLoad.Gate> changes = new CopyOnWriteArrayList<>();
        TenantLoad.Gate gate = limiter.gate("a", 1, 2, 25, 4, now::get, changes::add);
        assertEquals(0L, gate.enter(0));
        gate.charge(6 * SECOND); // budget -2 s
        assertSame(gate, limiter.gate("a", 1, 2, 25, 4, now::get, changes::add));
        assertEquals(1, changes.size(), "same limits: no change");
        TenantLoad.Gate changed = limiter.gate("a", 2, 4, 50, 60, now::get, changes::add);
        assertSame(gate, changed, "a plan change updates the gate in place");
        assertEquals(2, changes.size());
        assertEquals(1, changed.getRunning(), "the running request stays");
        assertEquals(-2000L, changed.getBudgetMs(), "the debt stays");
        changed.exit();
        // a deleted store: the idle gate and the job count of 0 go away
        limiter.jobQueued("a").run();
        limiter.forget("a");
        assertNull(limiter.getGate("a"));
        assertTrue(limiter.storeIds().isEmpty());
    }

    /** Finding 1: an interrupt while the request waits for a JVM slot gives back the store slot. */
    @Test
    public void interruptGivesBackTheStoreSlot() {
        TenantLoad.Limiter limiter = new TenantLoad.Limiter(1, 1000, () -> false, n -> { });
        TenantLoad.Gate gate = gate(limiter, "a", 1, 0, 0, 0);
        Thread.currentThread().interrupt();
        TenantLoad.Admission admission = limiter.enterRequest("a", gate);
        assertTrue(Thread.interrupted(), "the interrupt stays set");
        assertEquals(503, admission.getStatus());
        assertEquals(0, gate.getRunning(), "the store slot is free again");
        TenantLoad.Admission next = limiter.enterRequest("a", gate);
        assertTrue(next.isAdmitted(), "the store does not get 429 after the interrupt");
        limiter.exit(next.getTicket());
        assertEquals(0, gate.getRunning());
        assertEquals(1, limiter.getFreeJvmSlots());
    }

    /** Finding 3: a store over its budget holds no JVM slot while it waits; the other stores get the slots. */
    @Test
    public void throttledStoreHoldsNoJvmSlot() throws Exception {
        List<Long> sleeps = new CopyOnWriteArrayList<>();
        TenantLoad.Limiter limiter = new TenantLoad.Limiter(2, 5000, () -> false, n -> sleeps.add(n));
        TenantLoad.Gate a = gate(limiter, "a", 1, 2, 25, 1);
        TenantLoad.Gate b = gate(limiter, "b", 1, 2, 25, 1);
        // store a: its heavy request runs and pays more than its budget at its database calls
        TenantLoad.Admission a1 = limiter.enterRequest("a", a);
        assertTrue(a1.isAdmitted());
        a.charge(TimeUnit.MILLISECONDS.toNanos(1250)); // budget -0.25 s: debt 1 s
        limiter.charge();
        assertTrue(sleeps.isEmpty(), "a request never sleeps after its start");
        limiter.exit(a1.getTicket());
        assertEquals(2, limiter.getFreeJvmSlots());
        // the next request of store a waits in the store queue for its budget, without a JVM slot
        CompletableFuture<TenantLoad.Admission> a2 = CompletableFuture.supplyAsync(() -> limiter.enterRequest("a", a));
        long until = System.currentTimeMillis() + 2000;
        while (a.getWaiting() == 0 && System.currentTimeMillis() < until) {
            Thread.sleep(5);
        }
        assertEquals(1, a.getWaiting());
        assertEquals(2, limiter.getFreeJvmSlots(), "the waiting request holds no JVM slot");
        TenantLoad.Admission b1 = limiter.enterRequest("b", b);
        assertTrue(b1.isAdmitted(), "another store gets a JVM slot while store a waits");
        TenantLoad.Admission a2Result = a2.get(5, TimeUnit.SECONDS);
        assertTrue(a2Result.isAdmitted(), "store a runs when its budget is back");
        assertEquals(0, limiter.getFreeJvmSlots());
        limiter.exit(b1.getTicket());
        limiter.exit(a2Result.getTicket());
        assertEquals(2, limiter.getFreeJvmSlots());
        assertTrue(sleeps.isEmpty());
    }

    /** Finding 2: no sleep at a database call, and no sleep at a job start inside a transaction. */
    @Test
    public void noSleepInsideATransaction() {
        List<Long> sleeps = new CopyOnWriteArrayList<>();
        AtomicBoolean inTransaction = new AtomicBoolean(true);
        TenantLoad.Limiter limiter = new TenantLoad.Limiter(2, 3000, inTransaction::get, n -> sleeps.add(n));
        TenantLoad.Gate gate = gate(limiter, "a", 1, 2, 25, 1);
        gate.charge(100 * SECOND); // the store is over its budget: debt 4 s
        TenantLoad.Ticket job = limiter.enterJob("a", gate);
        assertNotNull(job);
        limiter.charge();
        assertTrue(sleeps.isEmpty(), "inside a transaction a heavy job does not sleep");
        limiter.exit(job);
        // the database path never sleeps, also without a transaction
        inTransaction.set(false);
        TenantLoad.Gate other = gate(limiter, "b", 1, 0, 25, 1);
        TenantLoad.Admission request = limiter.enterRequest("b", other);
        assertTrue(request.isAdmitted());
        other.charge(100 * SECOND);
        limiter.charge();
        assertTrue(sleeps.isEmpty(), "a database call never sleeps");
        limiter.exit(request.getTicket());
        // outside a transaction a heavy job waits before it starts, at most maxWaitMs
        job = limiter.enterJob("a", gate);
        assertEquals(1, sleeps.size());
        assertEquals(3 * SECOND, sleeps.get(0).longValue());
        assertNull(limiter.enterJob("a", gate), "one heavy ticket per thread");
        limiter.exit(job);
    }
}

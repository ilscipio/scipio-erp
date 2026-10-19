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
package com.ilscipio.scipio.channel.core;

import java.time.Clock;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Optional;
import java.util.Set;

/**
 * The work list of stock pushes. A worker (the hub through the MCP topic "channel", or {@link ChannelDispatcher}
 * in the same JVM) claims due tasks, pushes them, and reports the result.
 *
 * <p>Rules: (1) A worker never gets a second task of a listing while another task of that listing is leased, so an old value
 * cannot overwrite a newer one. A lease that expires (the worker died) frees the task. (2) A claim is a compare and set, so two
 * workers never take the same task. (3) A claim reads the stock again (when the queue has a {@link StockSource}); the task
 * carries the quantity of that moment. A task whose quantity the channel knows already is closed without a push.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class SyncQueue {
    /**
     * The lease of a stock claim. Review round 2: 60 s, longer than the push window of the hub (70 % of the hub lease, which
     * is also 60 s: 42 s), so a push that starts in the window ends inside the lease. A report after the lease is fenced anyway
     * (point 1: {@link #succeeded} and {@link #failed} act only while the listing row still holds the lease of the task).
     */
    public static final Duration DEFAULT_LEASE = Duration.ofSeconds(60);

    private static final String SUPERSEDED = "Superseded";
    static final String STOCK_ERROR = "STOCK_SYNC_FAILED";

    private final ChannelStore store;
    private final Clock clock;
    private final StockSource stock;

    public SyncQueue(ChannelStore store, Clock clock) {
        this(store, clock, null);
    }

    public SyncQueue(ChannelStore store, Clock clock, StockSource stock) {
        this.store = store;
        this.clock = clock;
        this.stock = stock;
    }

    /** Claims up to {@code limit} due tasks, oldest first. */
    public List<SyncTask> claim(int limit, Duration lease) {
        Instant now = clock.instant();
        List<SyncTask> open = store.openTasks();
        Set<String> leased = new HashSet<>();
        for (SyncTask t : open) {
            if (t.isLeased(now)) {
                leased.add(key(t));
            }
        }
        List<SyncTask> due = new ArrayList<>();
        for (SyncTask t : open) {
            if (t.isClaimable(now)) {
                due.add(t);
            }
        }
        due.sort(Comparator.comparing((SyncTask t) -> t.createdDate).thenComparing(t -> t.taskId));
        List<SyncTask> claimed = new ArrayList<>();
        for (SyncTask found : due) {
            if (claimed.size() >= limit) {
                break;
            }
            if (!leased.add(key(found))) {
                continue; // another task of the listing is leased
            }
            Duration hold = lease == null ? DEFAULT_LEASE : lease;
            Optional<Listing> listing = store.listing(found.productId, found.channelId);
            // review round 2, point 1: a task whose lease ended gets a new id for the new claim, so the late report of the
            // old worker finds the lease of another task and records nothing
            boolean expired = found.state == SyncTask.State.CLAIMED;
            String taskId = expired ? store.nextId("STK") : found.taskId;
            // R4-15 (review C3): the write lease of the listing row is the one lock for each writer, also across JVMs
            if (listing.isPresent() && !store.tryLockListing(found.productId, found.channelId, taskId, now.plus(hold), now)) {
                continue; // a publish or another worker writes this listing now
            }
            SyncTask t = expired ? successor(found, taskId, found.quantity, now) : found;
            SyncTask before = t.copy();
            t.claim(now, hold);
            boolean closed = false;
            if (stock != null) {
                Optional<ChannelSetting> setting = store.setting(t.channelId);
                if (setting.isPresent() && listing.isPresent()) {
                    t.quantity = store.stockRule(t.channelId)
                            .apply(stock.availableToPromise(t.productId, setting.get().productStoreId));
                    t.externalId = listing.get().externalId;
                    // a successor never trusts the pushed quantity: the push of the old worker may still land
                    if (!expired && listing.get().lastPushedQuantity != null && listing.get().lastPushedQuantity == t.quantity
                            && t.attempts == 0) {
                        t.succeed(now); // the channel knows the value
                        closed = true;
                    }
                }
            }
            if (!(expired ? supersede(found, t, now) : store.saveTaskIfUnchanged(t, before))) {
                store.unlockListing(t.productId, t.channelId, taskId);
                continue; // another worker took it
            }
            if (closed) {
                store.unlockListing(t.productId, t.channelId, t.taskId);
            } else {
                claimed.add(t);
            }
        }
        return claimed;
    }

    /** The answer of {@link #lease}. */
    public static final class Lease {
        public enum Kind {
            /** The listing takes no stock yet (a new listing): no other writer, no lease. */
            NONE,
            /** Another task of the listing is leased: try again after {@link #retryAfter}. */
            BUSY,
            /** The hub holds {@link #task}; it sends {@code task.quantity} and reports the task. */
            LEASED
        }

        public final Kind kind;
        public final SyncTask task;
        public final Duration retryAfter;
        /** The quantity of the state of this moment (also for NONE, review C4); null without a channel setting or stock source. */
        public final Integer quantity;

        private Lease(Kind kind, SyncTask task, Duration retryAfter, Integer quantity) {
            this.kind = kind;
            this.task = task;
            this.retryAfter = retryAfter;
            this.quantity = quantity;
        }
    }

    /**
     * R4-15 (J5-01): each quantity write to a channel holds the lease of the listing. A publish writes the quantity with the
     * listing (upsert), so it takes the lease too: the open task of the listing, or a new one, claimed with the quantity of
     * this moment (0 while the store is paused). A pause or a resume during the lease plans one more task, which waits for
     * the report of this one (rule 1 of the claim). The worker reports the task: succeeded, failed, or {@link #released}.
     */
    public Lease lease(String channelId, String productId, Duration lease) {
        Instant now = clock.instant();
        Optional<Listing> listing = store.listing(productId, channelId);
        Optional<ChannelSetting> setting = store.setting(channelId);
        Integer stateQty = stock == null || !setting.isPresent() ? null
                : Integer.valueOf(store.stockRule(channelId).apply(stock.availableToPromise(productId, setting.get().productStoreId)));
        if (stock == null || !listing.isPresent() || !listing.get().takesStock() || !setting.isPresent()) {
            // review round 2, point 7: a quantity only for a paused store (0) or a new listing; a listing that exists with a
            // quantity of its own (fixed, made to order) keeps the quantity of the hub
            boolean pausedNow = setting.isPresent() && stock instanceof PausedStockSource
                    && ((PausedStockSource) stock).isPaused(setting.get().productStoreId);
            boolean isNew = !listing.isPresent() || listing.get().externalId == null || listing.get().externalId.isEmpty();
            return new Lease(Lease.Kind.NONE, null, null, pausedNow || isNew ? stateQty : null);
        }
        Duration hold = lease == null ? DEFAULT_LEASE : lease;
        for (int attempt = 0; attempt < 3; attempt++) {
            SyncTask open = null;
            for (SyncTask t : store.openTasksOf(channelId, productId)) {
                if (t.isLeased(now)) {
                    return new Lease(Lease.Kind.BUSY, null, Duration.between(now, t.leaseUntil), null);
                }
                if (open == null || t.createdDate.isBefore(open.createdDate)) {
                    open = t;
                }
            }
            boolean expired = open != null && open.state == SyncTask.State.CLAIMED; // its lease ended: the new claim gets a new id
            String taskId = open != null && !expired ? open.taskId : store.nextId("STK");
            // review C3: the compare and set on the listing row decides; a second writer (also of another JVM, also one that
            // did not see the task of the first) finds the lease taken
            if (!store.tryLockListing(productId, channelId, taskId, now.plus(hold), now)) {
                return new Lease(Lease.Kind.BUSY, null, Duration.ofSeconds(Math.max(1, hold.getSeconds() / 4)), null);
            }
            int qty = store.stockRule(channelId).apply(stock.availableToPromise(productId, setting.get().productStoreId));
            if (open == null || expired) {
                SyncTask t = expired ? successor(open, taskId, qty, now)
                        : new SyncTask(taskId, channelId, productId, listing.get().externalId, qty, now);
                t.externalId = listing.get().externalId;
                t.claim(now, hold);
                if (expired) {
                    if (!supersede(open, t, now)) {
                        store.unlockListing(productId, channelId, taskId);
                        continue;
                    }
                } else {
                    store.saveTask(t);
                }
                return new Lease(Lease.Kind.LEASED, t, null, qty);
            }
            SyncTask before = open.copy();
            if (open.state == SyncTask.State.PENDING && open.dueDate.isAfter(now)) {
                open.dueDate = now; // the publish writes now; the wait of a failed push does not hold it back
            }
            open.quantity = qty;
            open.externalId = listing.get().externalId;
            open.claim(now, hold);
            if (store.saveTaskIfUnchanged(open, before)) {
                return new Lease(Lease.Kind.LEASED, open, null, qty);
            }
            store.unlockListing(productId, channelId, taskId); // another worker took the task in the meantime: read again
        }
        return new Lease(Lease.Kind.BUSY, null, Duration.ofSeconds(1), null);
    }

    /** R4-15: the worker held the lease and sent no quantity (no upsert): the task is due again at once, without an attempt. */
    public void released(String taskId) {
        SyncTask t = require(taskId);
        SyncTask before = t.copy();
        t.release(clock.instant());
        save(t, before);
        store.unlockListing(t.productId, t.channelId, t.taskId);
    }

    /**
     * The channel took the quantity. Review round 2, point 1 (fencing): the report acts only while the listing row still holds
     * the lease of this task. When the lease ended during a slow push and another writer took the listing, the report records
     * no quantity (a late 0 must not overwrite a newer 5): the task goes back to due and the pushed quantity is forgotten, so
     * the next plan pushes the quantity of its moment.
     */
    public void succeeded(String taskId) {
        Instant now = clock.instant();
        SyncTask t = require(taskId);
        if (t.state != SyncTask.State.CLAIMED) {
            // superseded: the late push may have overwritten a newer value, so the next plan pushes again; a repeated report of
            // a finished task changes nothing
            if (t.lastError != null && t.lastError.startsWith(SUPERSEDED)) {
                store.clearPushedQuantity(t.productId, t.channelId);
            }
            return;
        }
        SyncTask before = t.copy();
        Optional<Listing> opt = store.listing(t.productId, t.channelId);
        if (opt.isPresent()) {
            Listing l = opt.get();
            l.lastPushedQuantity = t.quantity;
            l.lastSyncDate = now;
            if (l.errorsJson != null && l.errorsJson.contains(STOCK_ERROR)) {
                l.errorsJson = null; // clear only the stock error; the errors of a listing call stay
                l.fixHint = null;
            }
            if (!store.saveListingIfLeased(l, taskId)) {
                leaseLost(t, before, now);
                return;
            }
        }
        t.succeed(now);
        save(t, before);
        store.unlockListing(t.productId, t.channelId, t.taskId);
    }

    /**
     * The push failed. A task that fails for good (not retryable, or too many attempts) writes the error and a
     * fix hint on the listing, so that the app shows one fix card. Errors of the listing itself stay. The listing write is
     * fenced like {@link #succeeded}.
     */
    public SyncTask failed(String taskId, boolean retryable, Duration retryAfter, String error, String fixHint) {
        Instant now = clock.instant();
        SyncTask t = require(taskId);
        if (t.state != SyncTask.State.CLAIMED) {
            return t; // superseded or released: a late failure records nothing
        }
        SyncTask before = t.copy();
        t.fail(now, retryable, retryAfter, error);
        if (t.state == SyncTask.State.FAILED) {
            Optional<Listing> opt = store.listing(t.productId, t.channelId);
            if (opt.isPresent()) {
                Listing l = opt.get();
                if (l.errorsJson == null || l.errorsJson.contains(STOCK_ERROR)) {
                    l.lastSyncDate = now;
                    l.errorsJson = "[{\"code\":\"" + STOCK_ERROR + "\",\"message\":" + jsonString(error) + "}]";
                    l.fixHint = fixHint != null ? fixHint : "The stock could not be sent to the channel. Check the connection of the channel.";
                    if (!store.saveListingIfLeased(l, taskId)) {
                        SyncTask fresh = require(taskId);
                        leaseLost(fresh, fresh.copy(), now);
                        return fresh;
                    }
                }
            }
        }
        save(t, before);
        store.unlockListing(t.productId, t.channelId, t.taskId);
        return t;
    }

    /** The report came after the lease: record nothing; the task is due again and the listing pushes again. */
    private void leaseLost(SyncTask t, SyncTask before, Instant now) {
        if (t.state == SyncTask.State.CLAIMED) {
            t.release(now);
            store.saveTaskIfUnchanged(t, before);
        }
        store.clearPushedQuantity(t.productId, t.channelId);
    }

    /** A new pending task (new id) that takes over the work of a task whose lease ended. */
    private static SyncTask successor(SyncTask old, String taskId, int quantity, Instant now) {
        SyncTask t = new SyncTask(taskId, old.channelId, old.productId, old.externalId, quantity, old.createdDate);
        t.attempts = old.attempts;
        t.dueDate = now;
        return t;
    }

    /** Ends the old task (compare and set) and saves its successor. False: another worker changed the old task first. */
    private boolean supersede(SyncTask old, SyncTask successor, Instant now) {
        SyncTask ended = old.copy();
        ended.succeed(now);
        ended.lastError = SUPERSEDED + ": the lease ended before the report";
        if (!store.saveTaskIfUnchanged(ended, old)) {
            return false;
        }
        store.saveTask(successor);
        return true;
    }

    private void save(SyncTask t, SyncTask before) {
        if (!store.saveTaskIfUnchanged(t, before)) {
            throw new IllegalStateException("Task " + t.taskId + " changed in the meantime (lease lost)");
        }
    }

    private SyncTask require(String taskId) {
        return store.task(taskId).orElseThrow(() -> new IllegalArgumentException("Unknown sync task " + taskId));
    }

    private static String key(SyncTask t) {
        return t.channelId + "|" + t.productId;
    }

    static String jsonString(String s) {
        if (s == null) {
            return "null";
        }
        StringBuilder sb = new StringBuilder("\"");
        for (char c : s.toCharArray()) {
            if (c == '"' || c == '\\') {
                sb.append('\\').append(c);
            } else if (c < 0x20) {
                sb.append(' ');
            } else {
                sb.append(c);
            }
        }
        return sb.append('"').toString();
    }
}

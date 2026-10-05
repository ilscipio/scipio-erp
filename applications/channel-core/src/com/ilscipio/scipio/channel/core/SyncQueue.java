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
    public static final Duration DEFAULT_LEASE = Duration.ofSeconds(30);
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
        for (SyncTask t : due) {
            if (claimed.size() >= limit) {
                break;
            }
            if (!leased.add(key(t))) {
                continue; // another task of the listing is leased
            }
            SyncTask before = t.copy();
            t.claim(now, lease == null ? DEFAULT_LEASE : lease);
            boolean closed = false;
            if (stock != null) {
                Optional<ChannelSetting> setting = store.setting(t.channelId);
                Optional<Listing> listing = store.listing(t.productId, t.channelId);
                if (setting.isPresent() && listing.isPresent()) {
                    t.quantity = store.stockRule(t.channelId)
                            .apply(stock.availableToPromise(t.productId, setting.get().productStoreId));
                    t.externalId = listing.get().externalId;
                    if (listing.get().lastPushedQuantity != null && listing.get().lastPushedQuantity == t.quantity
                            && t.attempts == 0) {
                        t.succeed(now); // the channel knows the value
                        closed = true;
                    }
                }
            }
            if (!store.saveTaskIfUnchanged(t, before)) {
                continue; // another worker took it
            }
            if (!closed) {
                claimed.add(t);
            }
        }
        return claimed;
    }

    /** The channel took the quantity. */
    public void succeeded(String taskId) {
        Instant now = clock.instant();
        SyncTask t = require(taskId);
        SyncTask before = t.copy();
        t.succeed(now);
        save(t, before);
        Optional<Listing> opt = store.listing(t.productId, t.channelId);
        if (opt.isPresent()) {
            Listing l = opt.get();
            l.lastPushedQuantity = t.quantity;
            l.lastSyncDate = now;
            if (l.errorsJson != null && l.errorsJson.contains(STOCK_ERROR)) {
                l.errorsJson = null; // clear only the stock error; the errors of a listing call stay
                l.fixHint = null;
            }
            store.saveListing(l);
        }
    }

    /**
     * The push failed. A task that fails for good (not retryable, or too many attempts) writes the error and a
     * fix hint on the listing, so that the app shows one fix card. Errors of the listing itself stay.
     */
    public SyncTask failed(String taskId, boolean retryable, Duration retryAfter, String error, String fixHint) {
        Instant now = clock.instant();
        SyncTask t = require(taskId);
        SyncTask before = t.copy();
        t.fail(now, retryable, retryAfter, error);
        save(t, before);
        if (t.state == SyncTask.State.FAILED) {
            Optional<Listing> opt = store.listing(t.productId, t.channelId);
            if (opt.isPresent()) {
                Listing l = opt.get();
                if (l.errorsJson == null || l.errorsJson.contains(STOCK_ERROR)) {
                    l.lastSyncDate = now;
                    l.errorsJson = "[{\"code\":\"" + STOCK_ERROR + "\",\"message\":" + jsonString(error) + "}]";
                    l.fixHint = fixHint != null ? fixHint : "The stock could not be sent to the channel. Check the connection of the channel.";
                    store.saveListing(l);
                }
            }
        }
        return t;
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

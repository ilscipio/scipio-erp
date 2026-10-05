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
import java.time.Instant;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Plans the stock pushes: after a stock change of a product, each channel that lists the product gets a
 * {@link SyncTask} with its quantity (ATP - buffer, capped). This is the "stock" flow of blueprint 7.3.
 *
 * <p>Rules: (1) one open pending task for each listing; a new value replaces the quantity of a pending task.
 * (2) A value equal to the last value that the channel knows creates no task. (3) A stock change never waits for a channel.
 * (4) The planner holds no lock: a change of a stored task is a compare and set ({@link ChannelStore#saveTaskIfUnchanged}),
 * and {@link SyncQueue#claim} reads the stock again, so a stale quantity never reaches a channel.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class StockSync {
    private final ChannelStore store;
    private final StockSource stock;
    private final Clock clock;

    public StockSync(ChannelStore store, StockSource stock, Clock clock) {
        this.store = store;
        this.stock = stock;
        this.clock = clock;
    }

    /** Plans the pushes for one product. Returns the tasks that were created or changed. */
    public List<SyncTask> stockChanged(String productId) {
        Instant now = clock.instant();
        List<SyncTask> changed = new ArrayList<>();
        for (ChannelSetting setting : store.activeSettings()) {
            Optional<Listing> opt = store.listing(productId, setting.channelId);
            if (!opt.isPresent() || !opt.get().takesStock()) {
                continue;
            }
            Listing listing = opt.get();
            int qty = store.stockRule(setting.channelId).apply(stock.availableToPromise(productId, setting.productStoreId));
            SyncTask task = plan(listing, qty, now);
            if (task != null) {
                changed.add(task);
            }
        }
        return changed;
    }

    private SyncTask plan(Listing listing, int qty, Instant now) {
        for (int attempt = 0; attempt < 3; attempt++) {
            SyncTask pending = null;
            SyncTask claimed = null;
            for (SyncTask t : store.openTasksOf(listing.channelId, listing.productId)) {
                if (t.state == SyncTask.State.PENDING) {
                    pending = t;
                } else {
                    claimed = t;
                }
            }
            if (pending != null) {
                if (pending.quantity == qty && java.util.Objects.equals(pending.externalId, listing.externalId)) {
                    return null;
                }
                SyncTask before = pending.copy();
                if (claimed == null && listing.lastPushedQuantity != null && listing.lastPushedQuantity == qty) {
                    // back to the value that the channel knows: the pending push is not needed
                    pending.state = SyncTask.State.DONE;
                    pending.doneDate = now;
                    if (store.saveTaskIfUnchanged(pending, before)) {
                        return null;
                    }
                    continue;
                }
                pending.retarget(qty, now);
                pending.externalId = listing.externalId;
                if (store.saveTaskIfUnchanged(pending, before)) {
                    return pending;
                }
                continue; // a worker took the task in the meantime: read again
            }
            Integer known = claimed != null ? Integer.valueOf(claimed.quantity) : listing.lastPushedQuantity;
            if (known != null && known == qty) {
                return null;
            }
            SyncTask t = new SyncTask(store.nextId("STK"), listing.channelId, listing.productId, listing.externalId, qty, now);
            store.saveTask(t);
            return t;
        }
        return null;
    }
}

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
import java.util.List;
import java.util.concurrent.Callable;

/**
 * Buyer-data retention of channel orders (W1-08 decision 2).
 *
 * <p>Rule: each channel has {@link ChannelSetting#retentionDays} and {@link ChannelSetting#retentionFrom}. Amazon: 30 days
 * from the day the order is closed (blueprint 10, Data Protection Policy). A channel without a limit keeps the buyer data
 * for the tax retention of the store. The daily run erases the buyer data of each order that is due: it removes the
 * name, e-mail, phone, notes and addresses of the buyer, and keeps the order, its amounts and its tax records.
 * A channel notice (eBay BUYER_DATA_DELETION) erases the buyer data at once, on every channel of the connector,
 * also on a channel that is switched off.</p>
 *
 * <p>Each order is erased in its own transaction ({@link Transactional}) together with its erase date. An order
 * that fails stays due and shows in the result; one failed order does not roll back the others.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class RetentionRun {

    /** Port to the erase of the buyer data of one store order. It throws when it cannot erase all data. */
    public interface BuyerDataEraser {
        void erase(String orderId) throws Exception;
    }

    /** Runs a unit of work in its own transaction. */
    public interface Transactional {
        Transactional NONE = new Transactional() {
            @Override
            public <T> T run(Callable<T> work) throws Exception {
                return work.call();
            }
        };

        <T> T run(Callable<T> work) throws Exception;
    }

    public static final class Result {
        public final int erased;
        public final List<String> failedOrderIds;

        Result(int erased, List<String> failed) {
            this.erased = erased;
            this.failedOrderIds = failed;
        }
    }

    private final ChannelStore store;
    private final BuyerDataEraser eraser;
    private final Clock clock;
    private final Transactional tx;

    public RetentionRun(ChannelStore store, BuyerDataEraser eraser, Clock clock) {
        this(store, eraser, clock, Transactional.NONE);
    }

    public RetentionRun(ChannelStore store, BuyerDataEraser eraser, Clock clock, Transactional tx) {
        this.store = store;
        this.eraser = eraser;
        this.clock = clock;
        this.tx = tx;
    }

    /** The due date, or null when the rule gives none (no limit, or the order is not closed for a CLOSED rule). */
    public static Instant dueDate(ChannelSetting s, Instant placed, Instant closed) {
        if (s.retentionDays == null) {
            return null;
        }
        Instant from = s.retentionFrom == ChannelSetting.RetentionFrom.PLACED ? placed : closed;
        return from == null ? null : from.plus(Duration.ofDays(s.retentionDays));
    }

    /** Sets the due date of a new order ref (a PLACED rule has a date at once). */
    static void start(ChannelSetting s, OrderRef ref) {
        ref.buyerDataDueDate = dueDate(s, ref.placedDate, ref.closedDate);
    }

    /** The order is shipped or cancelled: the CLOSED clock starts. */
    static void close(ChannelStore store, ChannelSetting s, OrderRef ref, Instant closedAt) {
        ref.closedDate = closedAt;
        ref.buyerDataDueDate = dueDate(s, ref.placedDate, closedAt);
        store.saveOrderRef(ref);
    }

    /** The store ships the order (the hub calls this after confirmShipment). */
    public void orderClosed(String orderId) {
        OrderRef ref = store.orderRefByOrderId(orderId)
                .orElseThrow(() -> new IllegalArgumentException("No channel order for " + orderId));
        if (ref.closedDate == null) {
            ChannelSetting s = store.setting(ref.channelId)
                    .orElseThrow(() -> new IllegalStateException("No setting for " + ref.channelId));
            close(store, s, ref, clock.instant());
        }
    }

    /** Erases the buyer data of each due order. A failed erase stays due and shows in the result. */
    public Result run() {
        Instant now = clock.instant();
        int erased = 0;
        List<String> failed = new ArrayList<>();
        for (OrderRef ref : store.orderRefsDueForErasure(now)) {
            if (eraseOne(ref, now)) {
                erased++;
            } else {
                failed.add(ref.orderId);
            }
        }
        return new Result(erased, failed);
    }

    /** The channel says: erase the buyer (eBay account-deletion notice). All channels of the connector. */
    public Result buyerDeletion(String connectorId, String buyerExternalId) {
        if (buyerExternalId == null || buyerExternalId.trim().isEmpty()) {
            throw new IllegalArgumentException("buyerExternalId is required");
        }
        Instant now = clock.instant();
        List<String> channelIds = new ArrayList<>();
        for (ChannelSetting s : store.allSettings()) {
            if (s.connectorId.equals(connectorId.trim().toLowerCase(java.util.Locale.ROOT))) {
                channelIds.add(s.channelId);
            }
        }
        int erased = 0;
        List<String> failed = new ArrayList<>();
        for (OrderRef ref : store.orderRefsOfBuyer(channelIds, buyerExternalId)) {
            if (eraseOne(ref, now)) {
                erased++;
            } else {
                failed.add(ref.orderId);
            }
        }
        return new Result(erased, failed);
    }

    private boolean eraseOne(final OrderRef ref, final Instant now) {
        try {
            tx.run(() -> {
                eraser.erase(ref.orderId);
                ref.buyerDataErasedDate = now;
                ref.buyerExternalId = null;
                store.saveOrderRef(ref);
                return null;
            });
            return true;
        } catch (Exception e) {
            ref.buyerDataErasedDate = null;
            return false;
        }
    }
}

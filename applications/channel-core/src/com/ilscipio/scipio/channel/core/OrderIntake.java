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

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * Order intake (blueprint 7.3 flow "orders"): the hub hands a channel order to the store; the store makes one order
 * through the order services with the external id, the stock falls, and the other channels get their new quantity.
 *
 * <p>Rules: (1) idempotent by channel and external order id: the hub can repeat a call. The ref row is the lock:
 * it is written (as a placeholder) before the store order, and the unique key of channel and external order id stops a
 * parallel call. (2) An order that waits for payment makes no store order. A new order that is already cancelled or
 * refunded makes none. (3) An order with a SKU that the store does not know makes no store order; the answer names the
 * SKUs. (4) The currency of the order must equal the currency of the channel. (5) The retention clock starts here
 * (see {@link RetentionRun}). (6) A cancel update for an open store order cancels it, and the stock returns.
 * (7) REJECTED writes nothing that stays: the service returns an error, so the transaction rolls back.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class OrderIntake {
    public static final String PLACEHOLDER_PREFIX = "RSV";

    public enum Status { CREATED, DUPLICATE, WAITING, IGNORED, UNMAPPED, REJECTED }

    /** A line with its store product. */
    public static final class ResolvedLine {
        public final String productId;
        public final IncomingOrder.Line line;

        ResolvedLine(String productId, IncomingOrder.Line line) {
            this.productId = productId;
            this.line = line;
        }
    }

    public static final class Result {
        public final Status status;
        public final String orderId;
        public final String message;
        public final List<String> unmappedSkus;

        Result(Status status, String orderId, String message, List<String> unmappedSkus) {
            this.status = status;
            this.orderId = orderId;
            this.message = message;
            this.unmappedSkus = unmappedSkus;
        }
    }

    /** Port to the order services: makes the store order and returns its id; cancels an open store order. */
    public interface OrderCreator {
        String create(ChannelSetting setting, IncomingOrder order, List<ResolvedLine> lines) throws Exception;

        /** Cancels the store order and frees its stock reservation. */
        default void cancel(String orderId) throws Exception {
            throw new UnsupportedOperationException("cancel is not supported");
        }
    }

    private final ChannelStore store;
    private final OrderCreator creator;
    private final StockSync stockSync;

    public OrderIntake(ChannelStore store, OrderCreator creator, StockSync stockSync) {
        this.store = store;
        this.creator = creator;
        this.stockSync = stockSync;
    }

    public Result intake(String channelId, IncomingOrder order) {
        Optional<ChannelSetting> opt = store.setting(channelId);
        if (!opt.isPresent() || !opt.get().active) {
            return new Result(Status.REJECTED, null, "Channel " + channelId + " is unknown or not active.", null);
        }
        ChannelSetting setting = opt.get();

        Optional<OrderRef> existing = store.orderRef(channelId, order.externalOrderId);
        if (existing.isPresent()) {
            return update(setting, existing.get(), order);
        }
        if (!setting.currencyUomId.equals(order.currency)) {
            return new Result(Status.REJECTED, null, "The order currency " + order.currency + " is not the currency "
                    + setting.currencyUomId + " of channel " + channelId + ". Fix the currency of the channel.", null);
        }
        if ("PENDING_PAYMENT".equals(order.status)) {
            return new Result(Status.WAITING, null, "The order waits for payment.", null);
        }
        if ("CANCELLED".equals(order.status) || "REFUNDED".equals(order.status)) {
            return new Result(Status.IGNORED, null, "A " + order.status.toLowerCase(java.util.Locale.ROOT)
                    + " order that the store never saw needs no store order.", null);
        }

        List<ResolvedLine> resolved = new ArrayList<>();
        List<String> unmapped = new ArrayList<>();
        for (IncomingOrder.Line line : order.lines) {
            Optional<String> productId = resolve(channelId, line);
            if (productId.isPresent()) {
                resolved.add(new ResolvedLine(productId.get(), line));
            } else {
                unmapped.add(line.sku != null ? line.sku : line.externalListingId);
            }
        }
        if (!unmapped.isEmpty()) {
            return new Result(Status.UNMAPPED, null, "The store has no product for the SKU " + unmapped
                    + ". Map the SKU to a product, then send the order again.", unmapped);
        }

        String placeholder = store.reserveOrderRef(channelId, order.externalOrderId);
        if (placeholder == null) {
            return new Result(Status.DUPLICATE, null, "The same order is in progress.", null);
        }
        String orderId;
        try {
            orderId = creator.create(setting, order, resolved);
        } catch (Exception e) {
            store.releaseOrderRef(placeholder);
            return new Result(Status.REJECTED, null, "The store order failed: " + e.getMessage(), null);
        }

        OrderRef ref = new OrderRef(orderId, channelId, order.externalOrderId);
        ref.feesAmount = order.fees;
        ref.placedDate = order.placedAt;
        ref.buyerExternalId = order.buyerExternalId;
        RetentionRun.start(setting, ref);
        store.replaceOrderRef(placeholder, ref);
        if (order.isClosed()) {
            RetentionRun.close(store, setting, ref, order.updatedAt);
        }
        stockChanged(resolved);
        return new Result(Status.CREATED, orderId, null, null);
    }

    /** A repeat of a known order: a cancel cancels the open store order; a shipment closes it. */
    private Result update(ChannelSetting setting, OrderRef ref, IncomingOrder order) {
        if (ref.orderId.startsWith(PLACEHOLDER_PREFIX)) {
            return new Result(Status.DUPLICATE, null, "The same order is in progress.", null);
        }
        if (ref.closedDate == null && order.isClosed()) {
            if ("CANCELLED".equals(order.status)) {
                try {
                    creator.cancel(ref.orderId);
                } catch (Exception e) {
                    return new Result(Status.REJECTED, ref.orderId, "The store order could not be cancelled: " + e.getMessage(), null);
                }
                List<ResolvedLine> lines = new ArrayList<>();
                for (IncomingOrder.Line line : order.lines) {
                    Optional<String> productId = resolve(setting.channelId, line);
                    if (productId.isPresent()) {
                        lines.add(new ResolvedLine(productId.get(), line));
                    }
                }
                stockChanged(lines);
            }
            RetentionRun.close(store, setting, ref, order.updatedAt);
        }
        return new Result(Status.DUPLICATE, ref.orderId, null, null);
    }

    private void stockChanged(List<ResolvedLine> lines) {
        if (stockSync != null) {
            for (ResolvedLine rl : lines) {
                stockSync.stockChanged(rl.productId);
            }
        }
    }

    private Optional<String> resolve(String channelId, IncomingOrder.Line line) {
        if (line.externalListingId != null && !line.externalListingId.isEmpty()) {
            Optional<Listing> l = store.listingByExternalId(channelId, line.externalListingId);
            if (l.isPresent()) {
                return Optional.of(l.get().productId);
            }
        }
        if (line.sku != null && !line.sku.isEmpty()) {
            return store.productIdBySku(line.sku);
        }
        return Optional.empty();
    }
}

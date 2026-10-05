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

import java.math.BigDecimal;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * The events that the store writes to the outbox, with their payloads. Each method runs in the transaction of its cause
 * (the service ECAs in {@code ChannelServices} call it through {@code ChannelServiceImpl}), so an event exists if and only if its cause is committed.
 *
 * <ul>
 * <li>{@code ORDER_CREATED}: a sales order exists (any channel, or the own store). Key: the order id.</li>
 * <li>{@code ORDER_NEEDS_SHIPPING}: a sales order with physical items is approved. Key: the order id.</li>
 * <li>{@code STOCK_CHANGED}: one inventory detail row. Key: the inventory item and the detail row.</li>
 * </ul>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-07).</p>
 */
public final class OutboxEvents {
    public static final String ORDER_CREATED = "ORDER_CREATED";
    public static final String ORDER_NEEDS_SHIPPING = "ORDER_NEEDS_SHIPPING";
    public static final String STOCK_CHANGED = "STOCK_CHANGED";

    public static final String SALES_ORDER = "SALES_ORDER";
    public static final String ORDER_APPROVED = "ORDER_APPROVED";

    private OutboxEvents() {
    }

    /** What the event needs to know of an order. */
    public static final class OrderFacts {
        public String orderId;
        public String orderTypeId;
        public String statusId;
        public String productStoreId;
        public String salesChannelEnumId;
        /** The channel of the product store (ChannelSetting), or null for the own store. */
        public String channelId;
        public BigDecimal grandTotal;
        public String currencyUom;
        public int itemCount;
        /** At least one open item is a physical product. */
        public boolean needsShipping;
    }

    /** What the event needs to know of a stock change. */
    public static final class StockFacts {
        public String productId;
        public String inventoryItemId;
        public String inventoryItemDetailSeqId;
        public String facilityId;
        public String orderId;
        public BigDecimal availableToPromiseDiff;
        public BigDecimal quantityOnHandDiff;
    }

    /** Writes ORDER_CREATED for a sales order, and ORDER_NEEDS_SHIPPING when the order is approved already. Returns the count of events written. */
    public static int orderCreated(EventOutbox outbox, OrderFacts o) {
        if (!isSales(o)) {
            return 0;
        }
        int n = outbox.record(ORDER_CREATED, ORDER_CREATED + ":" + o.orderId, orderPayload(o)) == null ? 0 : 1;
        return n + orderApproved(outbox, o);
    }

    /** Writes ORDER_NEEDS_SHIPPING when the order is an approved sales order with a physical item. Returns the count of events written. */
    public static int orderApproved(EventOutbox outbox, OrderFacts o) {
        if (!isSales(o) || !ORDER_APPROVED.equals(o.statusId) || !o.needsShipping) {
            return 0;
        }
        return outbox.record(ORDER_NEEDS_SHIPPING, ORDER_NEEDS_SHIPPING + ":" + o.orderId, orderPayload(o)) == null ? 0 : 1;
    }

    /** Writes STOCK_CHANGED. A change without a product writes nothing. */
    public static int stockChanged(EventOutbox outbox, StockFacts s) {
        if (s == null || s.productId == null || s.productId.isEmpty()) {
            return 0;
        }
        Map<String, Object> p = new LinkedHashMap<>();
        p.put("productId", s.productId);
        p.put("inventoryItemId", s.inventoryItemId);
        p.put("inventoryItemDetailSeqId", s.inventoryItemDetailSeqId);
        p.put("facilityId", s.facilityId);
        p.put("orderId", s.orderId);
        p.put("availableToPromiseDiff", s.availableToPromiseDiff);
        p.put("quantityOnHandDiff", s.quantityOnHandDiff);
        String key = s.inventoryItemId != null && s.inventoryItemDetailSeqId != null
                ? STOCK_CHANGED + ":" + s.inventoryItemId + ":" + s.inventoryItemDetailSeqId : null;
        return outbox.record(STOCK_CHANGED, key, p) == null ? 0 : 1;
    }

    private static boolean isSales(OrderFacts o) {
        return o != null && o.orderId != null && !o.orderId.isEmpty() && SALES_ORDER.equals(o.orderTypeId);
    }

    private static Map<String, Object> orderPayload(OrderFacts o) {
        Map<String, Object> p = new LinkedHashMap<>();
        p.put("orderId", o.orderId);
        p.put("statusId", o.statusId);
        p.put("productStoreId", o.productStoreId);
        p.put("salesChannelEnumId", o.salesChannelEnumId);
        p.put("channelId", o.channelId);
        p.put("grandTotal", o.grandTotal);
        p.put("currencyUom", o.currencyUom);
        p.put("itemCount", o.itemCount);
        p.put("needsShipping", o.needsShipping);
        return p;
    }
}

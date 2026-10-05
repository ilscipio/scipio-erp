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
import java.time.Instant;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * A channel order as the hub hands it to the store (the MCP action order_intake). channel-core has its own type: it never
 * depends on the hub types (blueprint 3.3). The fields follow ChannelOrder of the connector contract (W1-09a).
 * All amounts have the one currency {@link #currency}.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class IncomingOrder {
    public static final class Line {
        public final String externalLineId;
        public final String sku;
        public final String externalListingId;
        public final String title;
        public final int quantity;
        public final BigDecimal unitPrice;
        public final BigDecimal tax;

        public Line(String externalLineId, String sku, String externalListingId, String title, int quantity,
                BigDecimal unitPrice, BigDecimal tax) {
            this.externalLineId = Listing.req(externalLineId, "externalLineId");
            this.sku = sku;
            this.externalListingId = externalListingId;
            this.title = title;
            if (quantity < 1) {
                throw new IllegalArgumentException("quantity must be 1 or more");
            }
            this.quantity = quantity;
            this.unitPrice = unitPrice;
            this.tax = tax == null ? BigDecimal.ZERO : tax;
        }
    }

    public final String externalOrderId;
    /** PENDING_PAYMENT, PAID, PARTLY_SHIPPED, SHIPPED, CANCELLED or REFUNDED (the contract's ChannelOrder.Status). */
    public final String status;
    public final Instant placedAt;
    public final Instant updatedAt;
    public final String buyerExternalId;
    public final String buyerName;
    public final String buyerEmail;
    public final Map<String, String> shipTo;
    public final List<Line> lines;
    public final String currency;
    public final BigDecimal shipping;
    public final BigDecimal tax;
    public final BigDecimal total;
    public final BigDecimal fees;
    public final boolean taxCollectedByChannel;
    public final String buyerNote;

    public IncomingOrder(String externalOrderId, String status, Instant placedAt, Instant updatedAt, String buyerExternalId,
            String buyerName, String buyerEmail, Map<String, String> shipTo, List<Line> lines, String currency,
            BigDecimal shipping, BigDecimal tax, BigDecimal total, BigDecimal fees, boolean taxCollectedByChannel,
            String buyerNote) {
        this.externalOrderId = Listing.req(externalOrderId, "externalOrderId");
        this.status = Listing.req(status, "status");
        this.placedAt = java.util.Objects.requireNonNull(placedAt, "placedAt");
        this.updatedAt = updatedAt != null ? updatedAt : placedAt;
        this.buyerExternalId = buyerExternalId;
        this.buyerName = buyerName;
        this.buyerEmail = buyerEmail;
        this.shipTo = shipTo == null ? Collections.<String, String>emptyMap() : new LinkedHashMap<>(shipTo);
        if (lines == null || lines.isEmpty()) {
            throw new IllegalArgumentException("An order has at least one line");
        }
        this.lines = new ArrayList<>(lines);
        this.currency = Listing.req(currency, "currency");
        this.shipping = shipping == null ? BigDecimal.ZERO : shipping;
        this.tax = tax == null ? BigDecimal.ZERO : tax;
        this.total = java.util.Objects.requireNonNull(total, "total");
        this.fees = fees;
        this.taxCollectedByChannel = taxCollectedByChannel;
        this.buyerNote = buyerNote;
    }

    /** True for the states where the order is finished for the retention clock: shipped, cancelled, refunded. */
    public boolean isClosed() {
        return "SHIPPED".equals(status) || "CANCELLED".equals(status) || "REFUNDED".equals(status);
    }

    // ---- read from the loose map that the MCP call gives ----

    @SuppressWarnings("unchecked")
    public static IncomingOrder fromMap(Map<String, ?> m) {
        List<Line> lines = new ArrayList<>();
        Object rawLines = m.get("lines");
        if (rawLines instanceof List) {
            for (Object o : (List<Object>) rawLines) {
                Map<String, ?> lm = (Map<String, ?>) o;
                lines.add(new Line(str(lm, "externalLineId"), str(lm, "sku"), str(lm, "externalListingId"), str(lm, "title"),
                        Integer.parseInt(String.valueOf(lm.get("quantity"))), dec(lm, "unitPrice"), dec(lm, "tax")));
            }
        }
        Map<String, String> ship = new LinkedHashMap<>();
        Object rawShip = m.get("shipTo");
        if (rawShip instanceof Map) {
            for (Map.Entry<String, Object> e : ((Map<String, Object>) rawShip).entrySet()) {
                if (e.getValue() != null) {
                    ship.put(e.getKey(), String.valueOf(e.getValue()));
                }
            }
        }
        Object taxByChannel = m.get("taxCollectedByChannel");
        return new IncomingOrder(str(m, "externalOrderId"), str(m, "status"), Instant.parse(str(m, "placedAt")),
                m.get("updatedAt") == null ? null : Instant.parse(str(m, "updatedAt")), str(m, "buyerExternalId"),
                str(m, "buyerName"), str(m, "buyerEmail"), ship, lines, str(m, "currency"), dec(m, "shipping"),
                dec(m, "tax"), dec(m, "total"), dec(m, "fees"),
                taxByChannel != null && Boolean.parseBoolean(String.valueOf(taxByChannel)), str(m, "buyerNote"));
    }

    private static String str(Map<String, ?> m, String k) {
        Object v = m.get(k);
        return v == null ? null : String.valueOf(v);
    }

    private static BigDecimal dec(Map<String, ?> m, String k) {
        Object v = m.get(k);
        return v == null ? null : new BigDecimal(String.valueOf(v));
    }
}

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

/**
 * Link between a store order and a channel order: the row of ChannelOrderRef.
 * The buyer-data fields drive the retention rule ({@link RetentionRun}).
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class OrderRef {
    public final String orderId;
    public final String channelId;
    public final String externalOrderId;
    public String payoutId;
    public BigDecimal feesAmount;
    public Instant placedDate;
    /** The buyer id at the channel (for the eBay account-deletion notice); no name, no e-mail. */
    public String buyerExternalId;
    /** Day the order was shipped or cancelled; null while open. */
    public Instant closedDate;
    /** Day the buyer data goes; null: no channel limit or the order is open. */
    public Instant buyerDataDueDate;
    public Instant buyerDataErasedDate;

    public OrderRef(String orderId, String channelId, String externalOrderId) {
        this.orderId = Listing.req(orderId, "orderId");
        this.channelId = Listing.req(channelId, "channelId");
        this.externalOrderId = Listing.req(externalOrderId, "externalOrderId");
    }

    public OrderRef copy() {
        OrderRef r = new OrderRef(orderId, channelId, externalOrderId);
        r.payoutId = payoutId;
        r.feesAmount = feesAmount;
        r.placedDate = placedDate;
        r.buyerExternalId = buyerExternalId;
        r.closedDate = closedDate;
        r.buyerDataDueDate = buyerDataDueDate;
        r.buyerDataErasedDate = buyerDataErasedDate;
        return r;
    }
}

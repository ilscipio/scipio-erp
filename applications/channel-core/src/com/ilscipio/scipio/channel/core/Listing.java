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

import java.time.Instant;

/**
 * One product (a sellable SKU) on one channel: the row of the entity ChannelListing.
 *
 * <p>Variation rule (W1-08 decision 1): a variation product is one listing per variant SKU. The virtual parent has no
 * listing. {@link #variationGroupId} (the virtual product id) and {@link #variationAxesJson} tie the rows of one group
 * together. The hub hands them to the connector as attributes of the ProductView (see {@link VariationPlanner}).</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class Listing {
    public final String productId;
    public final String channelId;
    public String externalId;
    public ListingState state = ListingState.DRAFT;
    public Instant lastSyncDate;
    public String errorsJson;
    public String fixHint;
    public String variationGroupId;
    public String variationAxesJson;
    /** Quantity that the channel last acknowledged; null before the first push. */
    public Integer lastPushedQuantity;

    public Listing(String productId, String channelId) {
        this.productId = req(productId, "productId");
        this.channelId = req(channelId, "channelId");
    }

    public Listing copy() {
        Listing l = new Listing(productId, channelId);
        l.externalId = externalId;
        l.state = state;
        l.lastSyncDate = lastSyncDate;
        l.errorsJson = errorsJson;
        l.fixHint = fixHint;
        l.variationGroupId = variationGroupId;
        l.variationAxesJson = variationAxesJson;
        l.lastPushedQuantity = lastPushedQuantity;
        return l;
    }

    /** Sets the id on the channel. A new id is a new listing: the last acknowledged quantity no longer holds. */
    public void changeExternalId(String newExternalId) {
        if (newExternalId == null ? externalId != null : !newExternalId.equals(externalId)) {
            lastPushedQuantity = null;
        }
        externalId = newExternalId;
    }

    /** True when a stock update can go to the channel. */
    public boolean takesStock() {
        return state.takesStock() && externalId != null && !externalId.isEmpty();
    }

    static String req(String v, String name) {
        if (v == null || v.trim().isEmpty()) {
            throw new IllegalArgumentException(name + " is required");
        }
        return v;
    }
}

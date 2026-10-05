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

import java.util.Locale;

/**
 * Settings of one channel: the row of ChannelSetting.
 *
 * <p>W1-08 decisions. One channel per marketplace and one channel account per marketplace: {@link #channelId} is
 * {@code <connectorId>-<marketplace>}, for example {@code ebay-us} and {@code ebay-de}. Each channel has its own
 * ProductStore, currency, price rule, stock rule, retention rule and hub account ({@link #accountId}).
 * Currency and tax: {@link #currencyUomId} is the currency of the marketplace and {@link #pricesIncludeTax} says whether
 * the price that the buyer sees includes tax. Retention: {@link #retentionDays} and {@link #retentionFrom} say when
 * the buyer data of a channel order goes.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class ChannelSetting {

    /** Start of the retention period. */
    public enum RetentionFrom {
        /** From the order date. */
        PLACED,
        /** From the day the order is shipped or cancelled (the order is closed). */
        CLOSED
    }

    public final String channelId;
    /** The connector id of the hub, for example {@code ebay}. Equals ChannelConnector.channelId(). */
    public final String connectorId;
    /** Marketplace code, lower case, for example {@code us}, {@code de}. */
    public final String marketplaceId;
    public final String productStoreId;
    /** Id of the ChannelAccount in the hub (a reference only; the hub master table holds the account). */
    public final String accountId;
    public final String currencyUomId;
    public final boolean pricesIncludeTax;
    public final String salesChannelEnumId;
    /** Days to keep buyer data; null: no channel limit (the tax retention of the store rules). */
    public final Integer retentionDays;
    public final RetentionFrom retentionFrom;
    public final boolean active;

    public ChannelSetting(String connectorId, String marketplaceId, String productStoreId, String accountId,
            String currencyUomId, boolean pricesIncludeTax, String salesChannelEnumId, Integer retentionDays,
            RetentionFrom retentionFrom, boolean active) {
        this.connectorId = lower(Listing.req(connectorId, "connectorId"));
        this.marketplaceId = lower(Listing.req(marketplaceId, "marketplaceId"));
        this.channelId = channelId(this.connectorId, this.marketplaceId);
        this.productStoreId = Listing.req(productStoreId, "productStoreId");
        this.accountId = Listing.req(accountId, "accountId");
        this.currencyUomId = Listing.req(currencyUomId, "currencyUomId").trim().toUpperCase(Locale.ROOT);
        if (!this.currencyUomId.matches("[A-Z]{3}")) {
            throw new IllegalArgumentException("currencyUomId must be an ISO 4217 code: " + currencyUomId);
        }
        this.pricesIncludeTax = pricesIncludeTax;
        this.salesChannelEnumId = salesChannelEnumId;
        if (retentionDays != null && retentionDays < 0) {
            throw new IllegalArgumentException("retentionDays must not be negative");
        }
        this.retentionDays = retentionDays;
        this.retentionFrom = retentionFrom == null ? RetentionFrom.CLOSED : retentionFrom;
        this.active = active;
    }

    public static String channelId(String connectorId, String marketplaceId) {
        return lower(connectorId) + "-" + lower(marketplaceId);
    }

    private static String lower(String s) {
        return s.trim().toLowerCase(Locale.ROOT);
    }
}

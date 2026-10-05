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

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

/**
 * Default currency, price tax rule and buyer-data retention for a marketplace (W1-08 decisions 2 and 3).
 * The values are defaults for the setup flow. The seller can change each of them in the ChannelSetting row.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08). Sources: blueprint 10 (Amazon: 30-day limit on buyer PII), 7.5 (marketplaces collect
 * tax), 1 (launch currencies).</p>
 */
public final class MarketplaceDefaults {

    public final String currencyUomId;
    public final boolean pricesIncludeTax;
    public final Integer retentionDays;
    public final ChannelSetting.RetentionFrom retentionFrom;

    private MarketplaceDefaults(String currency, boolean incl, Integer days, ChannelSetting.RetentionFrom from) {
        this.currencyUomId = currency;
        this.pricesIncludeTax = incl;
        this.retentionDays = days;
        this.retentionFrom = from;
    }

    private static final Map<String, String> CURRENCY = new HashMap<>();
    private static final Map<String, Boolean> TAX_INCLUSIVE = new HashMap<>();

    private static void market(String code, String currency, boolean taxInclusive) {
        CURRENCY.put(code, currency);
        TAX_INCLUSIVE.put(code, taxInclusive);
    }

    static {
        // US and Hong Kong: the price shows without tax (US sales tax is added at checkout; Hong Kong has no sales tax).
        market("us", "USD", false);
        market("hk", "HKD", false);
        // EU, UK, Switzerland, Australia: consumer prices include VAT or GST.
        for (String eu : new String[] {"de", "at", "fr", "be", "nl", "lu", "ie", "es", "it"}) {
            market(eu, "EUR", true);
        }
        market("gb", "GBP", true);
        market("ch", "CHF", true);
        market("au", "AUD", true);
    }

    /**
     * Defaults for a connector on a marketplace. Amazon: 30 days after the order is closed. The other connectors:
     * no channel limit. The eBay account-deletion notice erases buyer data at any time (see {@link RetentionRun}).
     *
     * @throws IllegalArgumentException for a marketplace without defaults
     */
    public static MarketplaceDefaults of(String connectorId, String marketplaceId) {
        String market = marketplaceId.trim().toLowerCase(Locale.ROOT);
        String currency = CURRENCY.get(market);
        if (currency == null) {
            throw new IllegalArgumentException("No defaults for marketplace " + marketplaceId);
        }
        Integer days = "amazon".equals(connectorId.trim().toLowerCase(Locale.ROOT)) ? Integer.valueOf(30) : null;
        return new MarketplaceDefaults(currency, TAX_INCLUSIVE.get(market), days, ChannelSetting.RetentionFrom.CLOSED);
    }
}

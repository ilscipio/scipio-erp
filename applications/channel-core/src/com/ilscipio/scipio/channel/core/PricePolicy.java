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
import java.util.List;

/**
 * Price of a product on a channel (W1-08 decision 3): a channel has one currency and one tax rule.
 *
 * <p>Rule: the price is a ProductPrice row of the channel store group in the currency of the channel, and its
 * taxInPrice flag equals the tax rule of the channel. channel-core never converts a currency and never adds or removes
 * tax: a wrong tax or a wrong rate on a marketplace is a legal fault of the seller. A missing or wrong row gives a fix hint
 * and the listing does not go out. The amount that the hub gives the connector is the price that the buyer sees.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class PricePolicy {
    private PricePolicy() {
    }

    /** One ProductPrice row (the DEFAULT_PRICE row of the channel store group). */
    public static final class PriceRow {
        public final String currencyUomId;
        public final BigDecimal price;
        public final boolean taxInPrice;

        public PriceRow(String currencyUomId, BigDecimal price, boolean taxInPrice) {
            this.currencyUomId = currencyUomId;
            this.price = price;
            this.taxInPrice = taxInPrice;
        }
    }

    public static final class Result {
        public final BigDecimal price;
        public final String currencyUomId;
        public final String errorCode;
        public final String fixHint;

        private Result(BigDecimal price, String currency, String errorCode, String fixHint) {
            this.price = price;
            this.currencyUomId = currency;
            this.errorCode = errorCode;
            this.fixHint = fixHint;
        }

        public boolean isOk() {
            return errorCode == null;
        }
    }

    public static Result resolve(ChannelSetting setting, List<PriceRow> rows) {
        String want = setting.pricesIncludeTax ? "with tax" : "without tax";
        PriceRow inCurrency = null;
        for (PriceRow r : rows) {
            if (r.price == null || r.price.signum() <= 0 || !setting.currencyUomId.equals(r.currencyUomId)) {
                continue;
            }
            if (r.taxInPrice == setting.pricesIncludeTax) {
                return new Result(r.price, setting.currencyUomId, null, null);
            }
            inCurrency = r;
        }
        if (inCurrency != null) {
            return new Result(null, setting.currencyUomId, "PRICE_TAX_MISMATCH", "The price in " + setting.currencyUomId
                    + " for " + setting.channelId + " is " + (inCurrency.taxInPrice ? "with" : "without")
                    + " tax. This channel needs a price " + want + ". Enter the price " + want + " for this channel.");
        }
        return new Result(null, setting.currencyUomId, "NO_PRICE", "Set a price in " + setting.currencyUomId + " " + want
                + " for " + setting.channelId + ".");
    }
}

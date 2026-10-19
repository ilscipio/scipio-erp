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
import java.util.Objects;
import java.util.function.Predicate;

/**
 * The stock source of a store that the seller can pause (R4-15, J5-01). A paused product store gives 0 for each product, also
 * for a product that is always available (service, digital good, no inventory). A live store gives the value of the inner
 * source, so after a resume each product goes back to its normal rule, never to a stored number.
 *
 * <p>The plan, the claim and the channel data read the quantity at their own moment. So a late or repeated run sends the state
 * of that moment, and never the state of an older pause or resume.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (R4-15).</p>
 */
public final class PausedStockSource implements StockSource {
    private final StockSource inner;
    private final Predicate<String> paused;

    /**
     * @param inner the stock of a live store
     * @param paused true for a paused product store id (read at each call)
     */
    public PausedStockSource(StockSource inner, Predicate<String> paused) {
        this.inner = Objects.requireNonNull(inner, "inner");
        this.paused = Objects.requireNonNull(paused, "paused");
    }

    /** True when the product store is paused now (read at the call). */
    public boolean isPaused(String productStoreId) {
        return productStoreId != null && paused.test(productStoreId);
    }

    @Override
    public BigDecimal availableToPromise(String productId, String productStoreId) {
        if (productStoreId != null && paused.test(productStoreId)) {
            return BigDecimal.ZERO;
        }
        return inner.availableToPromise(productId, productStoreId);
    }
}

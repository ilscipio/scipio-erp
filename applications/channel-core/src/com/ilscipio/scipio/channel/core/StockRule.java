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
import java.math.RoundingMode;

/**
 * Stock rule of one channel: the row of ChannelStockRule. Available to sell = ATP - buffer, at most maxQuantity.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public final class StockRule {
    public static final StockRule NONE = new StockRule(0, null);

    public final int buffer;
    /** Null: no cap. */
    public final Integer maxQuantity;

    public StockRule(int buffer, Integer maxQuantity) {
        if (buffer < 0) {
            throw new IllegalArgumentException("buffer must not be negative");
        }
        if (maxQuantity != null && maxQuantity < 0) {
            throw new IllegalArgumentException("maxQuantity must not be negative");
        }
        this.buffer = buffer;
        this.maxQuantity = maxQuantity;
    }

    /** The quantity to show on the channel for the given ATP: never below 0. */
    public int apply(BigDecimal availableToPromise) {
        long atp = availableToPromise == null ? 0 : availableToPromise.setScale(0, RoundingMode.FLOOR).longValue();
        long q = Math.max(0, atp - buffer);
        if (maxQuantity != null) {
            q = Math.min(q, maxQuantity);
        }
        return (int) Math.min(q, Integer.MAX_VALUE);
    }
}

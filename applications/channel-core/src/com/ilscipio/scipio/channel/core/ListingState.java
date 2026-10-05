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

/**
 * State of a {@link Listing} (blueprint 7.3): draft, pending, live, rejected, ended.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08).</p>
 */
public enum ListingState {
    DRAFT, PENDING, LIVE, REJECTED, ENDED;

    /** A listing that a channel holds and that takes a stock update. */
    public boolean takesStock() {
        return this == PENDING || this == LIVE;
    }

    public static ListingState parse(String s) {
        if (s == null || s.trim().isEmpty()) {
            return DRAFT;
        }
        return valueOf(s.trim().toUpperCase(java.util.Locale.ROOT));
    }
}

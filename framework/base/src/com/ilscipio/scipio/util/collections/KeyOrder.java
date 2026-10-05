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
package com.ilscipio.scipio.util.collections;

import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.TreeMap;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * Map and set key ordering scheme.
 *
 * <p>SCIPIO: 3.0.0: Added.</p>
 */
public enum KeyOrder {

    NONE,
    LINKED,
    SORTED;

    public static KeyOrder from(String name, KeyOrder defaultValue) throws IllegalArgumentException {
        return (name != null && !name.isEmpty()) ? KeyOrder.valueOf(name.toUpperCase()) : defaultValue;
    }

    public static KeyOrder from(String name) throws IllegalArgumentException {
        return from(name, null);
    }

}

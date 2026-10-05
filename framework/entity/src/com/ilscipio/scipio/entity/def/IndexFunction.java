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
package com.ilscipio.scipio.entity.def;

/**
 * Function to apply to a field in an index.
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
public enum IndexFunction {

    /**
     * No function applied.
     */
    NONE(""),

    /**
     * Convert to lowercase before indexing.
     */
    LOWER("lower"),

    /**
     * Convert to uppercase before indexing.
     */
    UPPER("upper");

    private final String xmlValue;

    IndexFunction(String xmlValue) {
        this.xmlValue = xmlValue;
    }

    /**
     * Returns the XML attribute value for this function.
     */
    public String getXmlValue() {
        return xmlValue;
    }

    /**
     * Parses a function from its XML value.
     */
    public static IndexFunction fromXmlValue(String value) {
        if (value == null || value.isEmpty()) {
            return NONE;
        }
        for (IndexFunction func : values()) {
            if (func.xmlValue.equals(value)) {
                return func;
            }
        }
        throw new IllegalArgumentException("Unknown index function: " + value);
    }
}

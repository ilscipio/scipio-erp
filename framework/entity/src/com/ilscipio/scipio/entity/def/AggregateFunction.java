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
 * Aggregate functions for view-entity aliases.
 *
 * <p>Corresponds to aggregate-function type in entitymodel.xsd.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
public enum AggregateFunction {
    /** No function applied */
    NONE(""),
    /** Minimum value */
    MIN("min"),
    /** Maximum value */
    MAX("max"),
    /** Sum of values */
    SUM("sum"),
    /** Average of values */
    AVG("avg"),
    /** Count of rows */
    COUNT("count"),
    /** Count of distinct values */
    COUNT_DISTINCT("count-distinct"),
    /** Convert to uppercase */
    UPPER("upper"),
    /** Convert to lowercase */
    LOWER("lower"),
    /** Extract year from date */
    EXTRACT_YEAR("extract-year"),
    /** Extract month from date */
    EXTRACT_MONTH("extract-month"),
    /** Extract day from date */
    EXTRACT_DAY("extract-day"),
    /** Extract hour from timestamp */
    EXTRACT_HOUR("extract-hour"),
    /** Extract minute from timestamp */
    EXTRACT_MINUTE("extract-minute");

    private final String xmlValue;

    AggregateFunction(String xmlValue) {
        this.xmlValue = xmlValue;
    }

    public String getXmlValue() {
        return xmlValue;
    }

    /**
     * Returns the AggregateFunction for the given XML value.
     */
    public static AggregateFunction fromXmlValue(String xmlValue) {
        if (xmlValue == null || xmlValue.isEmpty()) {
            return NONE;
        }
        for (AggregateFunction f : values()) {
            if (f.xmlValue.equals(xmlValue)) {
                return f;
            }
        }
        throw new IllegalArgumentException("Unknown aggregate function: " + xmlValue);
    }
}

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
 * Defines how to combine conditions in a condition-list.
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
public enum ConditionCombine {
    /**
     * Combine with AND (all conditions must match).
     */
    AND("and"),

    /**
     * Combine with OR (any condition must match).
     */
    OR("or");

    private final String xmlValue;

    ConditionCombine(String xmlValue) {
        this.xmlValue = xmlValue;
    }

    /**
     * Returns the XML value used in entitymodel.xsd.
     */
    public String getXmlValue() {
        return xmlValue;
    }

    /**
     * Returns the enum value for the given XML value.
     */
    public static ConditionCombine fromXmlValue(String xmlValue) {
        for (ConditionCombine combine : values()) {
            if (combine.xmlValue.equals(xmlValue)) {
                return combine;
            }
        }
        return AND;
    }
}

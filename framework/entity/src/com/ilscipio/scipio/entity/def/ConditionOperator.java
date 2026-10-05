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
 * Condition operators for entity-condition expressions.
 *
 * <p>Corresponds to condition-expr operator attribute in entitymodel.xsd.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
public enum ConditionOperator {
    LESS("less"),
    GREATER("greater"),
    LESS_EQUALS("less-equals"),
    GREATER_EQUALS("greater-equals"),
    EQUALS("equals"),
    NOT_EQUALS("not-equals"),
    IN("in"),
    BETWEEN("between"),
    LIKE("like");

    private final String xmlValue;

    ConditionOperator(String xmlValue) {
        this.xmlValue = xmlValue;
    }

    public String getXmlValue() {
        return xmlValue;
    }

    /**
     * Returns the ConditionOperator for the given XML value.
     */
    public static ConditionOperator fromXmlValue(String xmlValue) {
        if (xmlValue == null || xmlValue.isEmpty()) {
            return EQUALS; // default
        }
        for (ConditionOperator op : values()) {
            if (op.xmlValue.equals(xmlValue)) {
                return op;
            }
        }
        throw new IllegalArgumentException("Unknown condition operator: " + xmlValue);
    }
}

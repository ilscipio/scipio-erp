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
 * Relation type enumeration for entity relationships.
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
public enum RelationType {

    /**
     * One-to-one relationship with foreign key constraint.
     */
    ONE("one"),

    /**
     * One-to-many relationship.
     */
    MANY("many"),

    /**
     * One-to-one relationship without foreign key constraint.
     *
     * <p>Use when the related entity may not exist or for cross-datasource relations.</p>
     */
    ONE_NOFK("one-nofk");

    private final String xmlValue;

    RelationType(String xmlValue) {
        this.xmlValue = xmlValue;
    }

    /**
     * Returns the XML attribute value for this relation type.
     */
    public String getXmlValue() {
        return xmlValue;
    }

    /**
     * Parses a relation type from its XML value.
     */
    public static RelationType fromXmlValue(String value) {
        for (RelationType type : values()) {
            if (type.xmlValue.equals(value)) {
                return type;
            }
        }
        throw new IllegalArgumentException("Unknown relation type: " + value);
    }
}

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
package com.ilscipio.scipio.widget.def.menu;

/**
 * Recursive mode for include-elements, include-actions, and include-menu-items directives.
 *
 * <p>Defines how to recurse when included menus have their own extends and includes.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for menu annotations support.</p>
 */
public enum RecursiveMode {
    /**
     * Do not recurse - only include direct elements from the target menu.
     */
    NO("no"),

    /**
     * Full recursion - include elements from both extends and includes of the target menu.
     * This is the default behavior.
     */
    FULL("full"),

    /**
     * Only recurse through include directives, not extends.
     */
    INCLUDES_ONLY("includes-only"),

    /**
     * Only recurse through extends, not include directives.
     */
    EXTENDS_ONLY("extends-only");

    private final String xmlValue;

    RecursiveMode(String xmlValue) {
        this.xmlValue = xmlValue;
    }

    /**
     * Returns the XML attribute value for this recursive mode.
     */
    public String getXmlValue() {
        return xmlValue;
    }

    /**
     * Returns the RecursiveMode for the given XML value.
     *
     * @param xmlValue the XML attribute value
     * @return the RecursiveMode, or FULL if not found
     */
    public static RecursiveMode fromXmlValue(String xmlValue) {
        if (xmlValue == null || xmlValue.isEmpty()) {
            return FULL;
        }
        for (RecursiveMode mode : values()) {
            if (mode.xmlValue.equals(xmlValue)) {
                return mode;
            }
        }
        return FULL;
    }
}

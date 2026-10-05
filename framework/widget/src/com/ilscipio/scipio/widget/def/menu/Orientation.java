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
 * Enum for menu orientation, equivalent to widget-menu.xsd orientation attribute.
 *
 * <p>SCIPIO: 4.0.0: Added for menu annotations support.</p>
 */
public enum Orientation {
    HORIZONTAL("horizontal"),
    VERTICAL("vertical");

    private final String value;

    Orientation(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public String getXmlValue() {
        return value;
    }

    @Override
    public String toString() {
        return value;
    }
}

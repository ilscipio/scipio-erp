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
package com.ilscipio.scipio.widget.def.tree;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines output field mapping for sub-nodes, equivalent to widget-tree.xsd out-field-map element.
 *
 * <p>SCIPIO: 4.0.0: Added for tree annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface OutFieldMap {

    /**
     * Source field name; required.
     */
    String fieldName();

    /**
     * Target field name; defaults to fieldName if not specified.
     */
    String toFieldName() default "";
}

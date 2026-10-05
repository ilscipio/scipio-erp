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

import com.ilscipio.scipio.widget.def.screen.FieldMap;

/**
 * Defines entity-and action for tree sub-nodes, equivalent to widget-tree.xsd entity-and element.
 *
 * <p>SCIPIO: 4.0.0: Added for tree annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface EntityAnd {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Entity name; required.
     */
    String entityName() default "";

    /**
     * List name for results.
     */
    String list() default "";

    /**
     * Whether to use cache.
     */
    boolean useCache() default false;

    /**
     * Filter by date: true, false, by-name.
     */
    String filterByDate() default "false";

    /**
     * Result set type: forward, scroll.
     */
    String resultSetType() default "scroll";

    /**
     * Field mappings for filter conditions.
     */
    FieldMap[] fieldMaps() default {};

    /**
     * Fields to select.
     */
    String[] selectFields() default {};

    /**
     * Order by fields.
     */
    String[] orderBy() default {};
}

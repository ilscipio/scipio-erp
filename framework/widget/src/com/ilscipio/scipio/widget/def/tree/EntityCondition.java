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
 * Defines entity-condition action for tree sub-nodes, equivalent to widget-tree.xsd entity-condition element.
 *
 * <p>SCIPIO: 4.0.0: Added for tree annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface EntityCondition {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Entity name; required.
     */
    String entityName() default "";

    /**
     * Whether to use cache.
     */
    boolean useCache() default false;

    /**
     * Filter by date: true, false, by-name.
     */
    String filterByDate() default "false";

    /**
     * Whether to select distinct.
     */
    boolean distinct() default false;

    /**
     * Delegator name.
     */
    String delegatorName() default "";

    /**
     * List name for results.
     */
    String list() default "";

    /**
     * Result set type: forward, scroll.
     */
    String resultSetType() default "scroll";

    /**
     * Condition expression - simplified condition.
     * Format: "fieldName operator value" or multiple joined with AND/OR.
     *
     * @deprecated SCIPIO: 4.0.0: Legacy flat form; use {@link #conditions()}/{@link #conditionLists()}
     * for a structured equivalent of widget-tree.xsd condition-list/condition-expr. Only consulted when
     * both {@link #conditions()} and {@link #conditionLists()} are empty.
     */
    @Deprecated
    String conditionExpr() default "";

    /**
     * Combine operator for the top-level condition list: and, or.
     */
    String combine() default "and";

    /**
     * Top-level condition expressions, equivalent to direct condition-expr children of the
     * top-level widget-tree.xsd condition-list (or a bare condition-expr with no wrapping list).
     */
    TreeConditionExpr[] conditions() default {};

    /**
     * Top-level nested condition lists, equivalent to direct condition-list children of the
     * top-level widget-tree.xsd condition-list (e.g. an "or" group nested inside an "and" group).
     */
    TreeConditionList[] conditionLists() default {};

    /**
     * Fields to select.
     */
    String[] selectFields() default {};

    /**
     * Order by fields.
     */
    String[] orderBy() default {};
}

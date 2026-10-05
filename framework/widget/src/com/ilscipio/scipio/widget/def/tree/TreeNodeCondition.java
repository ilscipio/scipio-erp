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

import com.ilscipio.scipio.widget.def.condition.Condition;

/**
 * Defines a tree node condition, equivalent to widget-tree.xsd condition element.
 *
 * <p>Simplified condition annotation that supports common condition patterns.
 * For complex conditions, use the {@link #conditions()} attribute with functional {@link Condition} annotations.</p>
 *
 * <p>Priority of condition evaluation (first defined wins):
 * <ol>
 *   <li>{@link #conditions()} - Functional interface conditions (recommended)</li>
 *   <li>{@link #permission()} - Simple permission check</li>
 *   <li>{@link #ifEmpty()}/{@link #ifNotEmpty()} - Simple empty checks</li>
 *   <li>{@link #conditionExpr()} - Legacy flexible expression</li>
 * </ol>
 * </p>
 *
 * <p>SCIPIO: 4.0.0: Added for tree annotations support. Enhanced with functional conditions.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface TreeNodeCondition {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Functional condition(s) using the {@link Condition} annotation.
     * If multiple conditions are provided, they are AND-ed together.
     *
     * <p>Example - simple permission:
     * <pre>
     * condition = @TreeNodeCondition(conditions = {
     *     @Condition(type = HasPermission.class, params = {"PARTYMGR", "_ADMIN"})
     * })
     * </pre>
     * </p>
     */
    Condition[] conditions() default {};

    /**
     * Permission name for if-has-permission condition.
     */
    String permission() default "";

    /**
     * Permission action for if-has-permission condition.
     */
    String permissionAction() default "";

    /**
     * Field name for if-empty condition.
     */
    String ifEmpty() default "";

    /**
     * Field name for if-not-empty condition (negated if-empty).
     */
    String ifNotEmpty() default "";

    /**
     * Flexible condition expression.
     * Supports ${} expressions that evaluate to boolean.
     */
    String conditionExpr() default "";
}

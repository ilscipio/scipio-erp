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

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import com.ilscipio.scipio.widget.def.condition.Condition;

/**
 * Defines a menu item condition, equivalent to widget-menu.xsd condition element.
 *
 * <p>For simple conditions, use one of the simple condition attributes (permission, ifEmpty).
 * For more complex conditions, use the {@link #conditions()} attribute with functional {@link Condition} annotations.</p>
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
 * <p>SCIPIO: 4.0.0: Added for menu annotations support. Enhanced with functional conditions.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface MenuItemCondition {

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
     * condition = @MenuItemCondition(conditions = {
     *     @Condition(type = HasPermission.class, params = {"PARTYMGR", "_ADMIN"})
     * })
     * </pre>
     * </p>
     *
     * <p>Example - multiple conditions (AND-ed together):
     * <pre>
     * condition = @MenuItemCondition(conditions = {
     *     @Condition(type = HasPermission.class, params = {"PARTYMGR", "_ADMIN"}),
     *     @Condition(type = NotEmpty.class, params = {"partyId"})
     * })
     * </pre>
     * </p>
     *
     * <p>Note: Composite condition types (And, Or, Xor, Not) are not supported at the
     * individual Condition annotation level due to Java's cyclic annotation limitations.
     * Multiple conditions are automatically AND-ed together at the container level.</p>
     */
    Condition[] conditions() default {};

    /** SCIPIO: 4.0.0: OR groups of simple conditions (AND-ed with the other entries of this condition). */
    com.ilscipio.scipio.widget.def.screen.OrCondition[] or() default {};

    /** SCIPIO: 4.0.0: XOR groups of simple conditions. */
    com.ilscipio.scipio.widget.def.screen.XorCondition[] xor() default {};

    /** SCIPIO: 4.0.0: Negates the whole condition (XML: condition/not). */
    boolean not() default false;

    /**
     * SCIPIO: Condition mode: inherit, omit, disable, disable-with-submenu.
     */
    String mode() default "";

    /**
     * SCIPIO: Pass style when condition passes.
     */
    String passStyle() default "";

    /**
     * SCIPIO: Disabled style when condition fails.
     */
    String disabledStyle() default "";

    // Simple conditions - use one of these

    /**
     * Permission to check (e.g., "PARTYMGR_VIEW").
     * Can include action suffix after underscore.
     */
    String permission() default "";

    /**
     * Permission action (e.g., "_VIEW", "_CREATE").
     * Used with permission() attribute.
     */
    String permissionAction() default "";

    /**
     * If-empty condition - tests if specified field is empty.
     */
    String ifEmpty() default "";

    /**
     * If-not-empty condition - tests if specified field is NOT empty.
     */
    String ifNotEmpty() default "";

    /**
     * SCIPIO: Flexible condition expression.
     * Must evaluate to "true" or "false".
     */
    String conditionExpr() default "";
}

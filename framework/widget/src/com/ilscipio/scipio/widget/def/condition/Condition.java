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
package com.ilscipio.scipio.widget.def.condition;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Annotation for defining widget conditions using the functional interface pattern.
 *
 * <p>This annotation can express simple conditions:</p>
 * <pre>
 * {@code @Condition(type = HasPermission.class, params = {"PARTYMGR", "_ADMIN"})}
 * </pre>
 *
 * <p>For composite conditions, use {@link Conditions} annotation at the container level:</p>
 * <pre>
 * {@code @Conditions({
 *     @Condition(type = HasPermission.class, params = {"PARTYMGR", "_ADMIN"}),
 *     @Condition(type = NotEmpty.class, params = {"partyId"})
 * })}
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for annotation-based widget condition support.</p>
 *
 * @see WidgetCondition
 * @see ConditionEvaluator
 * @see Conditions
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({}) // Used as nested annotation only
public @interface Condition {

    /**
     * The condition implementation class.
     *
     * <p>Must implement {@link WidgetCondition}.</p>
     *
     * @return The condition class
     */
    Class<? extends WidgetCondition> type();

    /**
     * Parameters for the condition.
     *
     * <p>Passed to {@link WidgetCondition#init(String[])} when the condition is instantiated.</p>
     *
     * <p>Parameter meanings vary by condition type:</p>
     * <ul>
     *   <li>{@code HasPermission}: [permission, action?]</li>
     *   <li>{@code Empty/NotEmpty}: [fieldName]</li>
     *   <li>{@code Compare}: [field, operator, value, type?, format?]</li>
     *   <li>{@code Regexp}: [field, expression]</li>
     * </ul>
     *
     * @return The parameters array
     */
    String[] params() default {};

    /**
     * Nested conditions for composite conditions (Not, Or, Xor, And).
     *
     * <p>Used by composite condition types that wrap other conditions:</p>
     * <ul>
     *   <li>{@code Not}: Single nested condition to negate</li>
     *   <li>{@code Or}: Multiple conditions where at least one must be true</li>
     *   <li>{@code Xor}: Multiple conditions where exactly one must be true</li>
     *   <li>{@code And}: Multiple conditions where all must be true (usually flattened)</li>
     * </ul>
     *
     * <p>Example:</p>
     * <pre>
     * {@code @Condition(type = Not.class, nested = {
     *     @NestedCondition(type = EmptySection.class, params = {"left-column"})
     * })}
     * </pre>
     *
     * <p>Note: Uses {@link NestedCondition} to prevent cyclic type references.
     * Due to Java annotation limitations, supports one level of composite nesting.
     * For deeper nesting (composite inside composite), refactor into separate logical
     * screens/sections or use XML format.</p>
     *
     * <p>SCIPIO: 4.0.0: Added for composite condition support.</p>
     *
     * @return The nested conditions array
     */
    NestedCondition[] nested() default {};

    /**
     * The members of a composite condition, as a flat tree of any depth.
     *
     * <p>Preferred over {@link #nested()}: a {@link NestedCondition} chain stops at a fixed
     * depth, and anything deeper had to fall back to an always-true condition. Each
     * {@link ConditionNode} names its parent by index, so no depth limit applies.</p>
     *
     * <p>SCIPIO: 4.0.0: Added for composite conditions of arbitrary depth.</p>
     *
     * @return The condition tree in pre-order
     */
    ConditionNode[] tree() default {};
}

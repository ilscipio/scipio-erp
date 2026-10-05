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
 * Annotation for defining nested widget conditions (one level deep).
 *
 * <p>This annotation is identical to {@link Condition} but does NOT have a nested()
 * attribute to prevent cyclic type references in Java annotations.</p>
 *
 * <p>Use this for the nested conditions inside composite conditions (Not, Or, Xor):</p>
 * <pre>
 * {@code @Condition(type = Not.class, nested = {
 *     @NestedCondition(type = EmptySection.class, params = {"left-column"})
 * })}
 * </pre>
 *
 * <p>Or conditions with multiple nested conditions:</p>
 * <pre>
 * {@code @Condition(type = Or.class, nested = {
 *     @NestedCondition(type = HasPermission.class, params = {"PARTYMGR", "_ADMIN"}),
 *     @NestedCondition(type = HasPermission.class, params = {"SETUP", "_ADMIN"})
 * })}
 * </pre>
 *
 * <p>Due to Java annotation limitations, supports one level of nesting only.
 * For deeper nesting (composite inside composite), refactor into separate logical
 * screens/sections or use XML format.</p>
 *
 * <p>SCIPIO: 4.0.0: Added to support composite condition annotations.</p>
 *
 * @see Condition
 * @see WidgetCondition
 * @see ConditionEvaluator
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({}) // Used as nested annotation only
public @interface NestedCondition {

    /**
     * The condition implementation class.
     *
     * <p>Must implement {@link WidgetCondition}.</p>
     *
     * @return The condition class
     */
    Class<? extends WidgetCondition> type();

    /**
     * Negates this nested condition.
     *
     * <p>Lets a composite hold negated members, so that De Morgan rewrites such as
     * {@code not(or(a, b))} to {@code and(not a, not b)} stay representable without a
     * second level of composite nesting.</p>
     *
     * <p>SCIPIO: 4.0.0: Added; without it, not(or(...)) fell back to an always-true condition.</p>
     *
     * @return true to negate the condition
     */
    boolean not() default false;

    /**
     * Members of this condition when it is itself a composite (And, Or, Xor, Not).
     *
     * <p>SCIPIO: 4.0.0: Added; a composite inside a composite fell back to an always-true condition.</p>
     *
     * @return The nested conditions array
     */
    NestedCondition2[] nested() default {};

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
}

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
package com.ilscipio.scipio.widget.def.screen;

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a condition-to-field action for screen widgets.
 *
 * <p>Evaluates a condition and stores the boolean result in a context field.</p>
 *
 * <p>Example XML equivalent:</p>
 * <pre>{@code
 * <condition-to-field field="isLoggedIn">
 *     <if-not-empty field="userLogin"/>
 * </condition-to-field>
 * }</pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(ConditionToFieldActionList.class)
public @interface ConditionToFieldAction {

    /**
     * The context field to store the condition result.
     */
    String field();

    /**
     * The type of the result field (e.g., "String", "Boolean"). Default is String.
     */
    String type() default "String";

    /**
     * Whether to use global scope for the result field.
     */
    boolean global() default false;

    /**
     * The scope to set the field in (e.g., "screen", "request").
     */
    String toScope() default "screen";

    /**
     * Only evaluate the condition if the field matches this state.
     * Values: "empty" (only if field is empty/null), "not-empty" (only if field has value).
     */
    String onlyIfField() default "";

    /**
     * The condition to evaluate. Use either this or the simple condition attributes.
     */
    Condition condition() default @Condition;
}

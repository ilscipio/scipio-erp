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
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Represents an else-if block within an {@link IfAction}.
 *
 * <p>Contains a condition and a block of code to be evaluated/executed
 * when a parent condition evaluates to false.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface ElseIfBlock {

    /**
     * Condition boolean expression (supporting flexible expressions).
     * Must evaluate strictly to the value "true" or "false".
     * Alternative to using the condition() element.
     */
    String conditionExpr() default "";

    /**
     * The condition that must be true for 'then' actions to execute.
     * Alternative to conditionExpr() - use for complex conditions.
     */
    Condition condition() default @Condition;

    /**
     * Actions to execute when this else-if condition is true.
     */
    Actions then() default @Actions;
}

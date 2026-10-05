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
 * SCIPIO: 4.0.0: Second-level conditional action block, used inside {@link Actions#ifs()} of an
 * {@link IfAction} branch (XML: an &lt;if&gt; nested inside &lt;then&gt;/&lt;else-if&gt;/&lt;else&gt;).
 * Its branches use {@link Actions2}, which cannot nest further (annotation cycle limit).
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.ANNOTATION_TYPE})
public @interface IfAction2 {

    /** Groovy/UEL condition expression (XML: if/@condition). Takes precedence over {@link #condition()}. */
    String conditionExpr() default "";

    /** Structured condition (XML: if/condition). */
    Condition condition() default @Condition;

    /** Actions of the then branch. */
    Actions2 then() default @Actions2;

    /** else-if branches. */
    ElseIfBlock2[] elseIf() default {};

    /** Actions of the else branch. */
    Actions2 elseActions() default @Actions2;

    /** Declaration order relative to the sibling {@link Action#order()} values of the enclosing branch. */
    int order() default -1;
}

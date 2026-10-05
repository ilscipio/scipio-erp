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
 * Defines a condition expression for entity queries.
 *
 * <p>Example XML equivalent:</p>
 * <pre>{@code
 * <condition-expr field-name="roleTypeId" operator="equals" value="INTERNAL_ORGANIZATIO"/>
 * }</pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface ConditionExpr {

    /**
     * The field name to compare.
     */
    String fieldName();

    /**
     * The comparison operator.
     * Valid values: equals, not-equals, less, greater, less-equals, greater-equals,
     * in, not-in, between, like, not-like
     */
    String operator() default "equals";

    /**
     * The literal value to compare against.
     */
    String value() default "";

    /**
     * A field to get the value from (instead of literal value).
     */
    String fromField() default "";

    /**
     * Environment name (for FlexibleStringExpander).
     */
    String envName() default "";

    /**
     * Whether to ignore case in comparison.
     */
    boolean ignoreCase() default false;

    /**
     * Whether to ignore the condition if the value is empty.
     */
    boolean ignoreIfEmpty() default false;

    /**
     * Whether to ignore the condition if the value is null.
     */
    boolean ignoreIfNull() default false;
}

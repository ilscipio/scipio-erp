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
package com.ilscipio.scipio.widget.def.form;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines an entity constraint for entity-options, equivalent to widget-form.xsd entity-constraint element.
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface EntityConstraint {

    /**
     * Field name to constrain; required.
     */
    String name();

    /**
     * Operator: "less", "greater", "less-equals", "greater-equals", "equals", "not-equals", "in", "not-in", "between", "like".
     */
    String operator() default "equals";

    /**
     * Environment variable name to get value from.
     */
    String envName() default "";

    /**
     * Literal value for the constraint.
     */
    String value() default "";

    /**
     * Whether to ignore this constraint if value is null.
     */
    boolean ignoreIfNull() default false;

    /**
     * Whether to ignore this constraint if value is empty.
     */
    boolean ignoreIfEmpty() default false;
}

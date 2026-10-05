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
package com.ilscipio.scipio.entity.def;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a field reference within a complex alias expression.
 *
 * <p>Corresponds to complex-alias-field element in entitymodel.xsd.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface ComplexAliasField {

    /**
     * Entity alias containing the field; optional if using value.
     */
    String entityAlias() default "";

    /**
     * Field name from the entity; optional if using value.
     */
    String field() default "";

    /**
     * Default value if field is null; optional.
     */
    String defaultValue() default "";

    /**
     * Literal value to use instead of a field; optional.
     */
    String value() default "";

    /**
     * Aggregate function to apply to the field; optional.
     */
    AggregateFunction function() default AggregateFunction.NONE;
}

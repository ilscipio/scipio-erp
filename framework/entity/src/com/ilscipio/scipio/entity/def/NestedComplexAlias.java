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
 * Defines a nested complex alias expression for one level of nesting.
 *
 * <p>Used within {@link ComplexAlias} to support expressions like
 * ((a * b) + c) where (a * b) is a nested complex alias.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface NestedComplexAlias {

    /**
     * Operator to combine fields/values; required.
     *
     * <p>Can be any SQL-compatible operator: +, -, *, /, etc.</p>
     */
    String operator();

    /**
     * Fields/values to combine with the operator; optional.
     */
    ComplexAliasField[] fields() default {};
}

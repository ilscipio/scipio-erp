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
 * Defines a complex alias expression (computed field) in a view-entity.
 *
 * <p>Corresponds to complex-alias element in entitymodel.xsd.</p>
 *
 * <p>Allows creating expressions like (discountPercent * 100) or combining fields
 * with operators like +, -, *, /, etc.</p>
 *
 * <p>Example for a price calculation:</p>
 * <pre>
 * {@literal @}Alias(name = "totalPrice",
 *        complexAlias = {@literal @}ComplexAlias(
 *            operator = "*",
 *            fields = {
 *                {@literal @}ComplexAliasField(entityAlias = "OI", field = "quantity"),
 *                {@literal @}ComplexAliasField(entityAlias = "OI", field = "unitPrice")
 *            }
 *        ))
 * </pre>
 *
 * <p>For deeply nested expressions (rare), use XML entity definitions or
 * the expression string format.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface ComplexAlias {

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

    /**
     * First level nested complex alias; optional.
     *
     * <p>For single-level nesting of complex expressions.</p>
     */
    NestedComplexAlias[] nested() default {};
}

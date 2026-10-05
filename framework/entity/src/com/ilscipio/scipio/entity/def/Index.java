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

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a database index for an entity, equivalent to entitymodel.xsd index element.
 *
 * <p>This annotation is repeatable and can be applied multiple times to define multiple indexes.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * // Simple index on one field
 * {@literal @}Index(name = "PRODUCT_NAME_IDX", fields = @IndexField(name = "internalName"))
 *
 * // Unique index
 * {@literal @}Index(name = "PRODUCT_CODE_IDX", unique = true, fields = @IndexField(name = "productCode"))
 *
 * // Composite index with function
 * {@literal @}Index(
 *     name = "PRODUCT_SEARCH_IDX",
 *     fields = {
 *         {@literal @}IndexField(name = "internalName", function = IndexFunction.LOWER),
 *         {@literal @}IndexField(name = "productTypeId")
 *     }
 * )
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(IndexList.class)
public @interface Index {

    /**
     * Index name; required.
     *
     * <p>Must be unique within the database.</p>
     */
    String name();

    /**
     * Index fields; required.
     */
    IndexField[] fields();

    /**
     * Whether this is a unique index.
     *
     * <p>Default is false.</p>
     */
    boolean unique() default false;

    /**
     * Index description; optional.
     */
    String description() default "";
}

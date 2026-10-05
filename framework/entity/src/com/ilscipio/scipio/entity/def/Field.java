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
 * Defines an entity field, equivalent to entitymodel.xsd field element.
 *
 * <p>This annotation is repeatable and can be applied multiple times to define all fields.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}Field(name = "productId", type = "id-ne")
 * {@literal @}Field(name = "productName", type = "name", description = "Product display name")
 * {@literal @}Field(name = "listPrice", type = "currency-amount", notNull = true)
 * </pre>
 *
 * <p>Common field types:</p>
 * <ul>
 *   <li>{@code id}, {@code id-ne} - Identifier fields (VARCHAR)</li>
 *   <li>{@code name}, {@code description} - Text fields</li>
 *   <li>{@code very-long} - Large text (TEXT/CLOB)</li>
 *   <li>{@code date-time} - Timestamp</li>
 *   <li>{@code date} - Date only</li>
 *   <li>{@code currency-amount} - Decimal for currency</li>
 *   <li>{@code indicator} - Y/N flag (CHAR(1))</li>
 *   <li>{@code blob} - Binary data</li>
 *   <li>{@code json} - JSON text storage</li>
 * </ul>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(FieldList.class)
public @interface Field {

    /**
     * Field name; required.
     *
     * <p>This name is used in Java code to access the field value.</p>
     */
    String name();

    /**
     * Field type; required.
     *
     * <p>Must be a valid type from fieldtype*.xml (e.g., "id", "name", "description",
     * "very-long", "date-time", "currency-amount", "indicator", "blob", "json").</p>
     */
    String type();

    /**
     * Database column name; optional.
     *
     * <p>If not specified, defaults to the field name converted to upper case with underscores.</p>
     */
    String colName() default "";

    /**
     * Field description; optional.
     */
    String description() default "";

    /**
     * Encryption mode for the field.
     *
     * <p>Values: "false" (default), "true", or "salt".</p>
     */
    String encrypt() default "false";

    /**
     * Enable audit logging for value changes.
     *
     * <p>When true, changes are recorded in EntityAuditLog. Default is false.</p>
     */
    boolean enableAuditLog() default false;

    /**
     * Field cannot be null in database.
     *
     * <p>Default is false.</p>
     */
    boolean notNull() default false;

    /**
     * Field set name for grouped field selection; optional.
     *
     * <p>Fields with the same field-set are selected together in generated queries.</p>
     */
    String fieldSet() default "";

    /**
     * Whether to include this field in default SELECT operations.
     *
     * <p>Empty string (default) means true. Set to "false" to exclude from default selects.
     * Typically used for view-entity fields not part of group-by.</p>
     */
    String select() default "";

    /**
     * Validators to apply to this field; optional.
     */
    Validate[] validators() default {};
}

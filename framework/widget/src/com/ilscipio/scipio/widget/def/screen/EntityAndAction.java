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
 * Performs an entity query with AND conditions on the specified fields.
 *
 * <p>Example XML equivalent:</p>
 * <pre>{@code
 * <entity-and entity-name="ProductCategory" list="productCategories">
 *     <field-map field-name="productCategoryTypeId" value="CATALOG_CATEGORY"/>
 *     <order-by field-name="categoryName"/>
 * </entity-and>
 * }</pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
@Repeatable(EntityAndActionList.class)
public @interface EntityAndAction {

    /**
     * The entity name to query.
     */
    String entityName();

    /**
     * The name of the list field to store the results.
     */
    String list();

    /**
     * Whether to use the entity cache. Default false.
     */
    boolean useCache() default false;

    /**
     * Whether to filter by date (from/thru dates). Default false.
     */
    boolean filterByDate() default false;

    /**
     * Field mappings for the query conditions.
     */
    FieldMap[] fieldMaps() default {};

    /**
     * Fields to select (if empty, all fields are selected).
     */
    String[] selectFields() default {};

    /**
     * Order by field names.
     */
    String[] orderBy() default {};

    /**
     * Result set type: "forward" or "scroll". Default "scroll".
     */
    String resultSetType() default "scroll";

    /**
     * Limit range start (for pagination).
     */
    int limitStart() default -1;

    /**
     * Limit range size (for pagination).
     */
    int limitSize() default -1;

    /**
     * Whether to use an iterator instead of loading all results.
     */
    boolean useIterator() default false;
}

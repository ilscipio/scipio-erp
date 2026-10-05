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
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a Scipio entity, equivalent to entitymodel.xsd entity element.
 *
 * <p>This annotation can be applied to an interface or class to define an entity.
 * The annotated type serves as a marker and can optionally define field accessor methods.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}Entity(
 *     name = "Product",
 *     packageName = "org.ofbiz.product.product",
 *     tableName = "PRODUCT",
 *     title = "Product Entity"
 * )
 * {@literal @}Field(name = "productId", type = "id-ne")
 * {@literal @}Field(name = "productName", type = "name")
 * {@literal @}PrimaryKey(field = "productId")
 * public interface ProductEntity {}
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Entity {

    /**
     * Entity name; required.
     *
     * <p>This name must be globally unique within the system.</p>
     */
    String name();

    /**
     * Package name for the entity; required.
     *
     * <p>Used for logical grouping of entities (e.g., "org.ofbiz.product.product").</p>
     */
    String packageName();

    /**
     * Database table name; optional.
     *
     * <p>If not specified, defaults to the entity name converted to upper case with underscores.</p>
     */
    String tableName() default "";

    /**
     * Entity description; optional.
     */
    String description() default "";

    /**
     * Entity title for documentation; optional.
     */
    String title() default "";

    /**
     * Entity author for documentation; optional.
     */
    String author() default "";

    /**
     * Entity copyright for documentation; optional.
     */
    String copyright() default "";

    /**
     * Entity version for documentation; optional.
     */
    String version() default "";

    /**
     * Default resource name for properties files; optional.
     */
    String defaultResourceName() default "";

    /**
     * Name of entity this entity depends on; optional.
     *
     * <p>Used for ordering entity loading and database creation.</p>
     */
    String dependentOn() default "";

    /**
     * Sequence bank size for ID generation; optional.
     *
     * <p>Default is 10, maximum is 5000.</p>
     */
    int sequenceBankSize() default 0;

    /**
     * Enable optimistic locking using lastUpdatedStamp field.
     *
     * <p>Default is false.</p>
     */
    boolean enableLock() default false;

    /**
     * Disable automatic timestamp fields (lastUpdatedStamp, createdStamp, etc.).
     *
     * <p>Default is false (stamps are added).</p>
     */
    boolean noAutoStamp() default false;

    /**
     * Disable caching for this entity.
     *
     * <p>Default is false (caching enabled).</p>
     */
    boolean neverCache() default false;

    /**
     * Disable database schema checks for this entity.
     *
     * <p>Default is false (checks enabled).</p>
     */
    boolean neverCheck() default false;

    /**
     * Automatically clear cache when entity is modified.
     *
     * <p>Default is true.</p>
     */
    boolean autoClearCache() default true;

    /**
     * Indicates this entity redefines an existing entity.
     *
     * <p>When true, suppresses "Entity is defined more than once" warnings.
     * Default is false.</p>
     */
    boolean redefinition() default false;

    /**
     * Entity fields.
     *
     * <p>May also be specified by repeatable {@link Field} annotations on @Entity.</p>
     */
    Field[] fields() default {};

    /**
     * Primary key fields.
     *
     * <p>May also be specified by repeatable {@link PrimaryKey} annotations on @Entity.</p>
     */
    PrimaryKey[] primaryKeys() default {};

    /**
     * Entity relations.
     *
     * <p>May also be specified by repeatable {@link Relation} annotations on @Entity.</p>
     */
    Relation[] relations() default {};

    /**
     * Entity indexes.
     *
     * <p>May also be specified by repeatable {@link Index} annotations on @Entity.</p>
     */
    Index[] indexes() default {};
}

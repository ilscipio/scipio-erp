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
 * Extends an existing entity definition with additional fields, relations, or indexes.
 *
 * <p>Corresponds to extend-entity element in entitymodel.xsd.</p>
 *
 * <p>This annotation allows adding new fields, relations, and indexes to an entity
 * defined elsewhere (in XML or another annotation). It can also override certain
 * entity-level attributes like default-resource-name.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * // Add a custom field to Party entity
 * {@literal @}ExtendEntity(
 *     name = "Party",
 *     fields = {
 *         {@literal @}Field(name = "customField", type = "id-vlong", description = "Custom extension field")
 *     },
 *     indexes = {
 *         {@literal @}Index(name = "PARTY_CUSTOM_IDX", fields = {@literal @}IndexField(name = "customField"))
 *     }
 * )
 * public interface PartyExtension {}
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface ExtendEntity {

    /**
     * Name of the entity to extend; required.
     *
     * <p>Must match an existing entity name defined elsewhere.</p>
     */
    String name();

    /**
     * Default resource name for properties files; optional.
     *
     * <p>Overrides the default-resource-name of the original entity.</p>
     */
    String defaultResourceName() default "";

    /**
     * Name of entity this entity depends on; optional.
     *
     * <p>Overrides or adds to the dependent-on of the original entity.</p>
     */
    String dependentOn() default "";

    /**
     * Sequence bank size for ID generation; optional.
     *
     * <p>Overrides the sequence-bank-size of the original entity.
     * Default is 0 (use original value), maximum is 5000.</p>
     */
    int sequenceBankSize() default 0;

    /**
     * Enable optimistic locking; optional.
     *
     * <p>If specified, overrides the enable-lock of the original entity.</p>
     */
    String enableLock() default "";

    /**
     * Disable automatic timestamp fields; optional.
     *
     * <p>If specified, overrides the no-auto-stamp of the original entity.</p>
     */
    String noAutoStamp() default "";

    /**
     * Disable caching for this entity; optional.
     *
     * <p>If specified, overrides the never-cache of the original entity.</p>
     */
    String neverCache() default "";

    /**
     * Automatically clear cache when entity is modified; optional.
     *
     * <p>If specified, overrides the auto-clear-cache of the original entity.</p>
     */
    String autoClearCache() default "";

    /**
     * Additional fields to add to the entity.
     *
     * <p>Field overrides can modify type, colName, description, and enable-audit-log
     * of existing fields.</p>
     *
     * <p>May also be specified by repeatable {@link Field} annotations on @ExtendEntity.</p>
     */
    Field[] fields() default {};

    /**
     * Additional relations to add to the entity.
     *
     * <p>May also be specified by repeatable {@link Relation} annotations on @ExtendEntity.</p>
     */
    Relation[] relations() default {};

    /**
     * Additional indexes to add to the entity.
     *
     * <p>May also be specified by repeatable {@link Index} annotations on @ExtendEntity.</p>
     */
    Index[] indexes() default {};
}

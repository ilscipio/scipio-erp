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
 * Defines an entity relationship, equivalent to entitymodel.xsd relation element.
 *
 * <p>This annotation is repeatable and can be applied multiple times to define all relations.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * // One-to-one relation
 * {@literal @}Relation(
 *     type = RelationType.ONE,
 *     relEntityName = "ProductType",
 *     keyMaps = @KeyMap(fieldName = "productTypeId")
 * )
 *
 * // One-to-many relation with title
 * {@literal @}Relation(
 *     type = RelationType.MANY,
 *     title = "Main",
 *     relEntityName = "OrderItem",
 *     keyMaps = @KeyMap(fieldName = "orderId")
 * )
 *
 * // Relation with different field names
 * {@literal @}Relation(
 *     type = RelationType.ONE,
 *     relEntityName = "Party",
 *     keyMaps = @KeyMap(fieldName = "createdByUserLogin", relFieldName = "partyId")
 * )
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for entity annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
@Repeatable(RelationList.class)
public @interface Relation {

    /**
     * Relation type; required.
     */
    RelationType type();

    /**
     * Related entity name; required.
     */
    String relEntityName();

    /**
     * Key mappings for the relation; required.
     */
    KeyMap[] keyMaps();

    /**
     * Relation title; optional.
     *
     * <p>Used to distinguish multiple relations to the same entity.</p>
     */
    String title() default "";

    /**
     * Relation description; optional.
     */
    String description() default "";

    /**
     * Foreign key constraint name; optional.
     *
     * <p>If not specified, a name will be auto-generated.</p>
     */
    String fkName() default "";
}

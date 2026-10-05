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
package com.ilscipio.scipio.product.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class InventoryServices {

    /**
     * Create a InventoryItemTempRes record
     */
    @Service(
        name = "createInventoryItemTempRes",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a InventoryItemTempRes record",
        defaultEntityName = "InventoryItemTempRes",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateInventoryItemTempRes {}

    /**
     * Update a InventoryItemTempRes record
     */
    @Service(
        name = "updateInventoryItemTempRes",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a InventoryItemTempRes record",
        defaultEntityName = "InventoryItemTempRes",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInventoryItemTempRes {}

    /**
     * Delete a InventoryItemTempRes record
     */
    @Service(
        name = "deleteInventoryItemTempRes",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a InventoryItemTempRes record",
        defaultEntityName = "InventoryItemTempRes",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInventoryItemTempRes {}

    /**
     * Create a InventoryItemAttribute
     */
    @Service(
        name = "createInventoryItemAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a InventoryItemAttribute",
        defaultEntityName = "InventoryItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateInventoryItemAttribute {}

    /**
     * Update a InventoryItemAttribute
     */
    @Service(
        name = "updateInventoryItemAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a InventoryItemAttribute",
        defaultEntityName = "InventoryItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInventoryItemAttribute {}

    /**
     * Delete a InventoryItemAttribute
     */
    @Service(
        name = "deleteInventoryItemAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a InventoryItemAttribute",
        defaultEntityName = "InventoryItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInventoryItemAttribute {}

    /**
     * Create a InventoryItemTypeAttr
     */
    @Service(
        name = "createInventoryItemTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a InventoryItemTypeAttr",
        defaultEntityName = "InventoryItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateInventoryItemTypeAttr {}

    /**
     * Update a InventoryItemTypeAttr
     */
    @Service(
        name = "updateInventoryItemTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a InventoryItemTypeAttr",
        defaultEntityName = "InventoryItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInventoryItemTypeAttr {}

    /**
     * Delete a InventoryItemTypeAttr
     */
    @Service(
        name = "deleteInventoryItemTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a InventoryItemTypeAttr",
        defaultEntityName = "InventoryItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInventoryItemTypeAttr {}

    /**
     * Create a Lot
     */
    @Service(
        name = "createLot",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Lot",
        defaultEntityName = "Lot",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateLot {}

    /**
     * Update a Lot
     */
    @Service(
        name = "updateLot",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Lot",
        defaultEntityName = "Lot",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateLot {}

    /**
     * Delete a Lot
     */
    @Service(
        name = "deleteLot",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Lot",
        defaultEntityName = "Lot",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteLot {}

}

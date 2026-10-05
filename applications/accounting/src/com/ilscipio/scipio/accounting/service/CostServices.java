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
package com.ilscipio.scipio.accounting.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CostServices {

    /**
     * Create a CostComponentCalc
     */
    @Service(
        name = "createCostComponentCalc",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/cost/CostServices.xml",
        invoke = "createCostComponentCalc",
        description = "Create a CostComponentCalc",
        defaultEntityName = "CostComponentCalc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "OUT", include = "pk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgCostPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateCostComponentCalc {}

    /**
     * Update a CostComponentCalc
     */
    @Service(
        name = "updateCostComponentCalc",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/cost/CostServices.xml",
        invoke = "updateCostComponentCalc",
        description = "Update a CostComponentCalc",
        defaultEntityName = "CostComponentCalc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgCostPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateCostComponentCalc {}

    /**
     * Remove a CostComponentCalc
     */
    @Service(
        name = "removeCostComponentCalc",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/cost/CostServices.xml",
        invoke = "removeCostComponentCalc",
        description = "Remove a CostComponentCalc",
        defaultEntityName = "CostComponentCalc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgCostPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveCostComponentCalc {}

    /**
     * Create a WorkEffortCostCalc entry
     */
    @Service(
        name = "createWorkEffortCostCalc",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/cost/CostServices.xml",
        invoke = "createWorkEffortCostCalc",
        description = "Create a WorkEffortCostCalc entry",
        defaultEntityName = "WorkEffortCostCalc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgCostPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateWorkEffortCostCalc {}

    /**
     * Remove a WorkEffortCostCalc entry
     */
    @Service(
        name = "removeWorkEffortCostCalc",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/cost/CostServices.xml",
        invoke = "removeWorkEffortCostCalc",
        description = "Remove a WorkEffortCostCalc entry",
        defaultEntityName = "WorkEffortCostCalc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgCostPermissionCheck", mainAction = "DELETE")
    )
    public interface RemoveWorkEffortCostCalc {}

    /**
     * Create Product Average Cost record
     */
    @Service(
        name = "createProductAverageCost",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Product Average Cost record",
        defaultEntityName = "ProductAverageCost",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgCostPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true"),
            @OverrideAttribute(name = "averageCost", optional = "false")
        }
    )
    public interface CreateProductAverageCost {}

    /**
     * Update a Product Average Cost record
     */
    @Service(
        name = "updateProductAverageCost",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Product Average Cost record",
        defaultEntityName = "ProductAverageCost",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgCostPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateProductAverageCost {}

    /**
     * Delete a Product Average Cost record
     */
    @Service(
        name = "deleteProductAverageCost",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Product Average Cost record",
        defaultEntityName = "ProductAverageCost",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgCostPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteProductAverageCost {}

    /**
     * Update a Product Average Cost record on receive inventory
     */
    @Service(
        name = "updateProductAverageCostOnReceiveInventory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/cost/CostServices.xml",
        invoke = "updateProductAverageCostOnReceiveInventory",
        description = "Update a Product Average Cost record on receive inventory",
        defaultEntityName = "ProductAverageCost",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "quantityAccepted", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "acctgCostPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateProductAverageCostOnReceiveInventory {}

    /**
     * Get Average cost of a product
     */
    @Service(
        name = "getProductAverageCost",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/cost/CostServices.xml",
        invoke = "getProductAverageCost",
        description = "Get Average cost of a product",
        auth = "true",
        attributes = {
            @Attribute(name = "inventoryItem", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "unitCost", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetProductAverageCost {}

}

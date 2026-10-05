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
public class FacilityServices {

    @Service(
        name = "facilityGenericPermission",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "facilityGenericPermission",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface FacilityGenericPermission {}

    /**
     * ProductFacility Permission Checking Logic
     */
    @Service(
        name = "checkProductFacilityRelatedPermission",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "checkProductFacilityRelatedPermission",
        description = "ProductFacility Permission Checking Logic",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface CheckProductFacilityRelatedPermission {}

    /**
     * Create an InventoryItem
     */
    @Service(
        name = "createInventoryItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createInventoryItem",
        description = "Create an InventoryItem",
        defaultEntityName = "InventoryItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"availableToPromiseTotal", "quantityOnHandTotal"})
        },
        attributes = {
            @Attribute(name = "isReturned", type = "String", mode = "IN", defaultValue = "N")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "inventoryItemTypeId", optional = "false"),
            @OverrideAttribute(name = "productId", optional = "false"),
            @OverrideAttribute(name = "facilityId", optional = "false")
        }
    )
    public interface CreateInventoryItem {}

    /**
     *          
     */
    @Service(
        name = "createInventoryItemCheckSetAtpQoh",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createInventoryItemCheckSetAtpQoh",
        description = "\n        ",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "availableToPromiseTotal", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "quantityOnHandTotal", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface CreateInventoryItemCheckSetAtpQoh {}

    /**
     * Update an InventoryItem
     */
    @Service(
        name = "updateInventoryItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "updateInventoryItem",
        description = "Update an InventoryItem",
        defaultEntityName = "InventoryItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"availableToPromiseTotal", "quantityOnHandTotal"})
        },
        attributes = {
            @Attribute(name = "oldOwnerPartyId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "oldProductId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateInventoryItem {}

    /**
     * If product store setOwnerUponIssuance is Y or empty, set the inventory item owner upon issuance.
     */
    @Service(
        name = "changeOwnerUponIssuance",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "changeOwnerUponIssuance",
        description = "If product store setOwnerUponIssuance is Y or empty, set the inventory item owner upon issuance.",
        auth = "true",
        attributes = {
            @Attribute(name = "itemIssuanceId", type = "String", mode = "IN")
        }
    )
    public interface ChangeOwnerUponIssuance {}

    /**
     * Create an inventory item status record
     */
    @Service(
        name = "createInventoryItemStatus",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createInventoryItemStatus",
        description = "Create an inventory item status record",
        defaultEntityName = "InventoryItemStatus",
        entityAttributes = {
            @EntityAttributes(mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "inventoryItemId", optional = "false"),
            @OverrideAttribute(name = "statusId", optional = "false")
        }
    )
    public interface CreateInventoryItemStatus {}

    /**
     * Check Product Inventory Discontinuation
     */
    @Service(
        name = "checkProductInventoryDiscontinuation",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "checkProductInventoryDiscontinuation",
        description = "Check Product Inventory Discontinuation",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN")
        }
    )
    public interface CheckProductInventoryDiscontinuation {}

    /**
     * Check and, if empty, fills with default values ownerPartyId, currencyUomId, unitCost
     */
    @Service(
        name = "inventoryItemCheckSetDefaultValues",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "inventoryItemCheckSetDefaultValues",
        description = "Check and, if empty, fills with default values ownerPartyId, currencyUomId, unitCost",
        defaultEntityName = "InventoryItem",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItem", type = "Map", mode = "IN", optional = "true")
        }
    )
    public interface InventoryItemCheckSetDefaultValues {}

    /**
     * Create an createInventoryItemDetail - note that the quantityOnHand and availableToPromise are relative (positive or negative) and will be added to the corresponding value on the given InventoryItem
     */
    @Service(
        name = "createInventoryItemDetail",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createInventoryItemDetail",
        description = "Create an createInventoryItemDetail - note that the quantityOnHand and availableToPromise are relative (positive or negative) and will be added to the corresponding value on the given InventoryItem",
        defaultEntityName = "InventoryItemDetail",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"effectiveDate"})
        },
        overrideAttributes = {
            @OverrideAttribute(name = "inventoryItemDetailSeqId", mode = "OUT")
        }
    )
    public interface CreateInventoryItemDetail {}

    /**
     *              Sums all availableToPromiseDiff and quantityOnHandDiff elements for the inventoryItemId and sets the availableToPromise and quantityOnHand fields on the corresponding InventoryItem.             Meant to be run as an Entity ECA triggered on any modify operation on the InventoryItemDetail entity.         
     */
    @Service(
        name = "updateInventoryItemFromDetail",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "updateInventoryItemFromDetail",
        description = "\n            Sums all availableToPromiseDiff and quantityOnHandDiff elements for the inventoryItemId and sets the availableToPromise and quantityOnHand fields on the corresponding InventoryItem.\n            Meant to be run as an Entity ECA triggered on any modify operation on the InventoryItemDetail entity.\n        ",
        defaultEntityName = "InventoryItemDetail",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN")
        }
    )
    public interface UpdateInventoryItemFromDetail {}

    /**
     * Sets the ATP/QOH totals for serialized inventory items
     */
    @Service(
        name = "updateSerializedInventoryTotals",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "updateSerializedInventoryTotals",
        description = "Sets the ATP/QOH totals for serialized inventory items",
        defaultEntityName = "InventoryItem",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN")
        }
    )
    public interface UpdateSerializedInventoryTotals {}

    /**
     * Create an InventoryItemVariance - note that the quantityOnHand and availableToPromise are relative and will be added to the corresponding value on the given InventoryItem
     */
    @Service(
        name = "createInventoryItemVariance",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createInventoryItemVariance",
        description = "Create an InventoryItemVariance - note that the quantityOnHand and availableToPromise are relative and will be added to the corresponding value on the given InventoryItem",
        defaultEntityName = "InventoryItemVariance",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateInventoryItemVariance {}

    /**
     * Create an PhysicalInventory
     */
    @Service(
        name = "createPhysicalInventory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createPhysicalInventory",
        description = "Create an PhysicalInventory",
        defaultEntityName = "PhysicalInventory",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreatePhysicalInventory {}

    /**
     * Create a PhysicalInventory and an InventoryItemVariance
     */
    @Service(
        name = "createPhysicalInventoryAndVariance",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createPhysicalInventoryAndVariance",
        description = "Create a PhysicalInventory and an InventoryItemVariance",
        auth = "true",
        transactionTimeout = "600",
        entityAttributes = {
            @EntityAttributes(entityName = "InventoryItemVariance", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "PhysicalInventory", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "InventoryItemVariance", mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "physicalInventoryId", mode = "OUT")
        }
    )
    public interface CreatePhysicalInventoryAndVariance {}

    /**
     * Get Marketing Packages Available From Components In Inventory
     */
    @Service(
        name = "getMktgPackagesAvailable",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "getMktgPackagesAvailable",
        description = "Get Marketing Packages Available From Components In Inventory",
        useTransaction = "false",
        log = "quiet",
        logEca = "quiet",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityOnHandTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "availableToPromiseTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "useEntityCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, entity cache when looking up records, where possible (SCIPIO)"),
            @Attribute(name = "useInventoryCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, use ProductFacility.lastInventoryCount or other inventory cache. Current default: false; legacy default: false (SCIPIO)\n                WARNING: statusId cannot be used to calculate cache and if set no caching will occur")
        }
    )
    public interface GetMktgPackagesAvailable {}

    /**
     * Get Inventory Availability for a Product
     */
    @Service(
        name = "getProductInventoryAvailable",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "getProductInventoryAvailable",
        description = "Get Inventory Availability for a Product",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityOnHandTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "availableToPromiseTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", description = "Same as useEntityCache (SCIPIO)"),
            @Attribute(name = "useEntityCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, entity cache when looking up records, where possible (SCIPIO)"),
            @Attribute(name = "useInventoryCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, use ProductFacility.lastInventoryCount or other inventory cache. Current default: false; legacy default: false (SCIPIO)\n                WARNING: statusId cannot be used to calculate cache and if set no caching will occur")
        }
    )
    public interface GetProductInventoryAvailable {}

    /**
     * Get Inventory Availability for a Product constrained by a facilityId
     */
    @Service(
        name = "getInventoryAvailableByFacility",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "getProductInventoryAvailable",
        description = "Get Inventory Availability for a Product constrained by a facilityId",
        useTransaction = "false",
        log = "quiet",
        logEca = "quiet",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lotId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityOnHandTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "availableToPromiseTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", description = "Same as useEntityCache"),
            @Attribute(name = "useEntityCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, entity cache when looking up records, where possible (SCIPIO)"),
            @Attribute(name = "useInventoryCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, use ProductFacility.lastInventoryCount or other inventory cache. Current default: false; legacy default: false (SCIPIO)\n                WARNING: statusId/lotId cannot be used to calculate cache and if set no caching will occur")
        }
    )
    public interface GetInventoryAvailableByFacility {}

    /**
     * Get Inventory Availability for a Product constrained by a facility and location
     */
    @Service(
        name = "getInventoryAvailableByLocation",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "getProductInventoryAvailable",
        description = "Get Inventory Availability for a Product constrained by a facility and location",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "locationSeqId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityOnHandTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "availableToPromiseTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", description = "Same as useEntityCache"),
            @Attribute(name = "useEntityCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, entity cache when looking up records, where possible (SCIPIO)")
        }
    )
    public interface GetInventoryAvailableByLocation {}

    /**
     * Get Inventory Availability for a Product constrained by a containerId
     */
    @Service(
        name = "getInventoryAvailableByContainer",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "getProductInventoryAvailable",
        description = "Get Inventory Availability for a Product constrained by a containerId",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "containerId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityOnHandTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "availableToPromiseTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", description = "Same as useEntityCache"),
            @Attribute(name = "useEntityCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, entity cache when looking up records, where possible (SCIPIO)")
        }
    )
    public interface GetInventoryAvailableByContainer {}

    /**
     * Get Inventory Availability for an InventoryItem
     */
    @Service(
        name = "getInventoryAvailableByItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "getProductInventoryAvailable",
        description = "Get Inventory Availability for an InventoryItem",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "quantityOnHandTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "availableToPromiseTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", description = "Same as useEntityCache"),
            @Attribute(name = "useEntityCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, entity cache when looking up records, where possible (SCIPIO)")
        }
    )
    public interface GetInventoryAvailableByItem {}

    /**
     *              Get Inventory Availability for a product based on List of associated products,             usually marketing package components.         
     */
    @Service(
        name = "getProductInventoryAvailableFromAssocProducts",
        engine = "java",
        location = "org.ofbiz.product.inventory.InventoryServices",
        invoke = "getProductInventoryAvailableFromAssocProducts",
        description = "\n            Get Inventory Availability for a product based on List of associated products,\n            usually marketing package components.\n        ",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "assocProducts", type = "List", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityOnHandTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "availableToPromiseTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "useEntityCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, entity cache when looking up records, where possible (SCIPIO)"),
            @Attribute(name = "useInventoryCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, use ProductFacility.lastInventoryCount or other inventory cache. Current default: false; legacy default: false (SCIPIO)\n                WARNING: statusId/lotId cannot be used to calculate cache and if set no caching will occur")
        }
    )
    public interface GetProductInventoryAvailableFromAssocProducts {}

    /**
     * Get Inventory Availability for a Product by a Supplier
     */
    @Service(
        name = "getProductInventoryAvailableBySupplier",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "getProductInventoryAvailable",
        description = "Get Inventory Availability for a Product by a Supplier",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityOnHandTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "availableToPromiseTotal", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "useCache", type = "Boolean", mode = "IN", optional = "true", description = "Same as useEntityCache"),
            @Attribute(name = "useEntityCache", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false", description = "If true, entity cache when looking up records, where possible (SCIPIO)")
        }
    )
    public interface GetProductInventoryAvailableBySupplier {}

    /**
     * Count Inventory On Hand for a Product constrained by a facilityId at a given date.
     */
    @Service(
        name = "countProductInventoryOnHand",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "countProductInventoryOnHand",
        description = "Count Inventory On Hand for a Product constrained by a facilityId at a given date.",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryCountDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "quantityOnHandTotal", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface CountProductInventoryOnHand {}

    /**
     * Count Inventory Shipped for Sales Orders for a Product constrained by a facilityId in a given date range.
     */
    @Service(
        name = "countProductInventoryShippedForSales",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "countProductInventoryShippedForSales",
        description = "Count Inventory Shipped for Sales Orders for a Product constrained by a facilityId in a given date range.",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "quantityOnHandTotal", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface CountProductInventoryShippedForSales {}

    /**
     * Get ATP/QOH Availability for a list of OrderItems by summing over all facilities.  If the item is a MARKETING_PKG_AUTO/PICK, then put its quantity available from components             in the mktgPkgATPMap and mktgPkgQOHMap.
     */
    @Service(
        name = "getProductInventorySummaryForItems",
        engine = "java",
        location = "org.ofbiz.product.inventory.InventoryServices",
        invoke = "getProductInventorySummaryForItems",
        description = "Get ATP/QOH Availability for a list of OrderItems by summing over all facilities.  If the item is a MARKETING_PKG_AUTO/PICK, then put its quantity available from components\n            in the mktgPkgATPMap and mktgPkgQOHMap.",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "orderItems", type = "List", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityOnHandMap", type = "Map", mode = "OUT"),
            @Attribute(name = "availableToPromiseMap", type = "Map", mode = "OUT"),
            @Attribute(name = "mktgPkgQOHMap", type = "Map", mode = "OUT"),
            @Attribute(name = "mktgPkgATPMap", type = "Map", mode = "OUT")
        }
    )
    public interface GetProductInventorySummaryForItems {}

    /**
     * Get ATP/QOH Availability for a list of OrderItems by summing over all facilities.  If the item is a MARKETING_PKG_AUTO/PICK, then put its quantity available from components             in the mktgPkgATPMap and mktgPkgQOHMap.
     */
    @Service(
        name = "getProductInventoryAndFacilitySummary",
        engine = "java",
        location = "org.ofbiz.product.inventory.InventoryServices",
        invoke = "getProductInventoryAndFacilitySummary",
        description = "Get ATP/QOH Availability for a list of OrderItems by summing over all facilities.  If the item is a MARKETING_PKG_AUTO/PICK, then put its quantity available from components\n            in the mktgPkgATPMap and mktgPkgQOHMap.",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "checkTime", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "minimumStock", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityUomId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "totalQuantityOnHand", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "totalAvailableToPromise", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "quantityOnOrder", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "offsetQOHQtyAvailable", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "offsetATPQtyAvailable", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "defaultPrice", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "listPrice", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "wholeSalePrice", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "usageQuantity", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface GetProductInventoryAndFacilitySummary {}

    /**
     * Batch service for checking and sending backorder notifications.  Will also set an autoCancelDate for sales orders to 30 days (hard coded)             beyond the OISGIR's promisedDatetime, which is in turn set by reserveProductInventory service using ProductFacility.daysToShip or 30 days by default.
     */
    @Service(
        name = "checkInventoryAvailability",
        engine = "java",
        location = "org.ofbiz.product.inventory.InventoryServices",
        invoke = "checkInventoryAvailability",
        description = "Batch service for checking and sending backorder notifications.  Will also set an autoCancelDate for sales orders to 30 days (hard coded)\n            beyond the OISGIR's promisedDatetime, which is in turn set by reserveProductInventory service using ProductFacility.daysToShip or 30 days by default."
    )
    public interface CheckInventoryAvailability {}

    /**
     * Balance inventory items based on the new item specified which will have available inventory that back-order (negative ATP) reservations can be reassigned to.
     */
    @Service(
        name = "balanceInventoryItems",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "balanceInventoryItems",
        description = "Balance inventory items based on the new item specified which will have available inventory that back-order (negative ATP) reservations can be reassigned to.",
        auth = "true",
        transactionTimeout = "600",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "priorityOrderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "priorityOrderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "noLongerOnBackOrderIdSet", type = "Set", mode = "OUT", optional = "true")
        }
    )
    public interface BalanceInventoryItems {}

    /**
     * Balance inventory reservations for a given product/facility, considering all the reservations with promised date greater than fromDate.
     */
    @Service(
        name = "reassignInventoryReservations",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "reassignInventoryReservations",
        description = "Balance inventory reservations for a given product/facility, considering all the reservations with promised date greater than fromDate.",
        auth = "true",
        transactionTimeout = "600",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "noLongerOnBackOrderIdSet", type = "Set", mode = "OUT", optional = "true"),
            @Attribute(name = "priority", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ReassignInventoryReservations {}

    /**
     * For each product with a negative reservation in the order, calls reassignInventoryReservations
     */
    @Service(
        name = "balanceOrderItemsWithNegativeReservations",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "balanceOrderItemsWithNegativeReservations",
        description = "For each product with a negative reservation in the order, calls reassignInventoryReservations",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface BalanceOrderItemsWithNegativeReservations {}

    @Service(
        name = "reserveAnInventoryItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryReserveServices.xml",
        invoke = "reserveAnInventoryItem",
        auth = "true",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "INOUT"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "reservedDatetime", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "requireInventory", type = "String", mode = "IN"),
            @Attribute(name = "serialNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceId", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "promisedDatetime", type = "Timestamp", mode = "IN"),
            @Attribute(name = "priority", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ReserveAnInventoryItem {}

    /**
     * Reserve Inventory for a Product.             If requireInventory is Y the quantity not reserved is returned, if N then a negative             availableToPromise will be used to track quantity ordered beyond what is in stock.         
     */
    @Service(
        name = "reserveProductInventory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryReserveServices.xml",
        invoke = "reserveProductInventory",
        description = "Reserve Inventory for a Product.\n            If requireInventory is Y the quantity not reserved is returned, if N then a negative\n            availableToPromise will be used to track quantity ordered beyond what is in stock.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "reservedDatetime", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "requireInventory", type = "String", mode = "IN"),
            @Attribute(name = "reserveOrderEnumId", type = "String", mode = "IN"),
            @Attribute(name = "sequenceId", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "lotId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "priority", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityNotReserved", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface ReserveProductInventory {}

    /**
     * Reserve Inventory for a Product By Facility             If requireInventory is Y the quantity not reserved is returned, if N then a negative             availableToPromise will be used to track quantity ordered beyond what is in stock.         
     */
    @Service(
        name = "reserveProductInventoryByFacility",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryReserveServices.xml",
        invoke = "reserveProductInventory",
        description = "Reserve Inventory for a Product By Facility\n            If requireInventory is Y the quantity not reserved is returned, if N then a negative\n            availableToPromise will be used to track quantity ordered beyond what is in stock.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "reservedDatetime", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "requireInventory", type = "String", mode = "IN"),
            @Attribute(name = "reserveOrderEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceId", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "quantityNotReserved", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "priority", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lotId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ReserveProductInventoryByFacility {}

    /**
     * Reserve Inventory for a Product By Container             If requireInventory is Y the quantity not reserved is returned, if N then a negative             availableToPromise will be used to track quantity ordered beyond what is in stock.         
     */
    @Service(
        name = "reserveProductInventoryByContainer",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryReserveServices.xml",
        invoke = "reserveProductInventory",
        description = "Reserve Inventory for a Product By Container\n            If requireInventory is Y the quantity not reserved is returned, if N then a negative\n            availableToPromise will be used to track quantity ordered beyond what is in stock.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "containerId", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "reservedDatetime", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "requireInventory", type = "String", mode = "IN"),
            @Attribute(name = "reserveOrderEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceId", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "quantityNotReserved", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface ReserveProductInventoryByContainer {}

    /**
     * Create OrderItemShipGrpInvRes or increment existing reserved quantity.
     */
    @Service(
        name = "reserveOrderItemInventory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryReserveServices.xml",
        invoke = "reserveOrderItemInventory",
        description = "Create OrderItemShipGrpInvRes or increment existing reserved quantity.",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderItemShipGrpInvRes", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "OrderItemShipGrpInvRes", mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdDatetime"})
        },
        attributes = {
            @Attribute(name = "priority", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "quantity", optional = "false")
        }
    )
    public interface ReserveOrderItemInventory {}

    /**
     *              Iterates through each OrderItemShipGrpInvRes on each OrderItem for the order             with the given orderId and cancels the reservation by changing the status             of the OrderItemShipGrpInvRes and incrementing the corresponding non-serialized             inventoryItem's availableToPromise quantity, or setting the status of the             corresponding serialized inventoryItem to available.         
     */
    @Service(
        name = "cancelOrderInventoryReservation",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryReserveServices.xml",
        invoke = "cancelOrderInventoryReservation",
        description = "\n            Iterates through each OrderItemShipGrpInvRes on each OrderItem for the order\n            with the given orderId and cancels the reservation by changing the status\n            of the OrderItemShipGrpInvRes and incrementing the corresponding non-serialized\n            inventoryItem's availableToPromise quantity, or setting the status of the\n            corresponding serialized inventoryItem to available.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CancelOrderInventoryReservation {}

    /**
     * Cancel a specific quantity for an order item
     */
    @Service(
        name = "cancelOrderItemInvResQty",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryReserveServices.xml",
        invoke = "cancelOrderItemInvResQty",
        description = "Cancel a specific quantity for an order item",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cancelQuantity", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface CancelOrderItemInvResQty {}

    /**
     * Cancels an inventory reservation
     */
    @Service(
        name = "cancelOrderItemShipGrpInvRes",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryReserveServices.xml",
        invoke = "cancelOrderItemShipGrpInvRes",
        description = "Cancels an inventory reservation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderItemShipGrpInvRes", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "cancelQuantity", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface CancelOrderItemShipGrpInvRes {}

    /**
     * Inventory Transfer Interface
     */
    @Service(
        name = "interfaceInventoryTransfer",
        engine = "interface",
        description = "Inventory Transfer Interface",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "locationSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "containerId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facilityIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "locationSeqIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "containerIdTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "itemIssuanceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sendDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "receiveDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface InterfaceInventoryTransfer {}

    /**
     * Create an inventory transfer.  Uses the prepareInventoryTransfer service; see comments there about transfer quantities and inventory items.
     */
    @Service(
        name = "createInventoryTransfer",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createInventoryTransfer",
        description = "Create an inventory transfer.  Uses the prepareInventoryTransfer service; see comments there about transfer quantities and inventory items.",
        auth = "true",
        implemented = {@Implements(service = "interfaceInventoryTransfer")},
        attributes = {
            @Attribute(name = "xferQty", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "inventoryTransferId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateInventoryTransfer {}

    /**
     * Update an inventory transfer record
     */
    @Service(
        name = "updateInventoryTransfer",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "updateInventoryTransfer",
        description = "Update an inventory transfer record",
        auth = "true",
        implemented = {@Implements(service = "interfaceInventoryTransfer")},
        attributes = {
            @Attribute(name = "inventoryTransferId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateInventoryTransfer {}

    /**
     * Prepares inventory item for transfer.  If the xferQty is less than quantityOnHandTotal of the inventory item, then the inventory                 item is "split," and an new inventory item will be created with xferQty and used for the inventory transfer.
     */
    @Service(
        name = "prepareInventoryTransfer",
        engine = "java",
        location = "org.ofbiz.product.inventory.InventoryServices",
        invoke = "prepareInventoryTransfer",
        description = "Prepares inventory item for transfer.  If the xferQty is less than quantityOnHandTotal of the inventory item, then the inventory\n                item is \"split,\" and an new inventory item will be created with xferQty and used for the inventory transfer.",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "INOUT"),
            @Attribute(name = "xferQty", type = "BigDecimal", mode = "IN")
        }
    )
    public interface PrepareInventoryTransfer {}

    /**
     * Completes the inventory transfer
     */
    @Service(
        name = "completeInventoryTransfer",
        engine = "java",
        location = "org.ofbiz.product.inventory.InventoryServices",
        invoke = "completeInventoryTransfer",
        description = "Completes the inventory transfer",
        attributes = {
            @Attribute(name = "inventoryTransferId", type = "String", mode = "IN"),
            @Attribute(name = "receiveDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CompleteInventoryTransfer {}

    /**
     * Cancel the inventory transfer
     */
    @Service(
        name = "cancelInventoryTransfer",
        engine = "java",
        location = "org.ofbiz.product.inventory.InventoryServices",
        invoke = "cancelInventoryTransfer",
        description = "Cancel the inventory transfer",
        attributes = {
            @Attribute(name = "inventoryTransferId", type = "String", mode = "IN")
        }
    )
    public interface CancelInventoryTransfer {}

    /**
     * Create inventory transfers for the given product and quantity. Return the units not available for transfers.
     */
    @Service(
        name = "createInventoryTransfersForProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createInventoryTransfersForProduct",
        description = "Create inventory transfers for the given product and quantity. Return the units not available for transfers.",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "containerId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "facilityIdTo", type = "String", mode = "IN"),
            @Attribute(name = "sendDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "reserveOrderEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityNotTransferred", type = "BigDecimal", mode = "OUT")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateInventoryTransfersForProduct {}

    /**
     *              Issues the Inventory for an Order that was Immediately Fulfilled, like in a POS environment.             Note that this skips the normal inventory reservation process, and the shipment process (no shipment is created).         
     */
    @Service(
        name = "issueImmediatelyFulfilledOrder",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryIssueServices.xml",
        invoke = "issueImmediatelyFulfilledOrder",
        description = "\n            Issues the Inventory for an Order that was Immediately Fulfilled, like in a POS environment.\n            Note that this skips the normal inventory reservation process, and the shipment process (no shipment is created).\n        ",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface IssueImmediatelyFulfilledOrder {}

    /**
     *              Issues the Inventory for an Order Item that was Immediately Fulfilled for more info see the issueImmediatelyFulfilledOrder service.         
     */
    @Service(
        name = "issueImmediatelyFulfilledOrderItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryIssueServices.xml",
        invoke = "issueImmediatelyFulfilledOrderItem",
        description = "\n            Issues the Inventory for an Order Item that was Immediately Fulfilled for more info see the issueImmediatelyFulfilledOrder service.\n        ",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "orderHeader", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "orderItem", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "productStore", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true")
        }
    )
    public interface IssueImmediatelyFulfilledOrderItem {}

    /**
     * Create an ProductFacility
     */
    @Service(
        name = "createProductFacility",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createProductFacility",
        description = "Create an ProductFacility",
        defaultEntityName = "ProductFacility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "checkProductFacilityRelatedPermission", mainAction = "CREATE")
    )
    public interface CreateProductFacility {}

    /**
     * Update an ProductFacility
     */
    @Service(
        name = "updateProductFacility",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "updateProductFacility",
        description = "Update an ProductFacility",
        defaultEntityName = "ProductFacility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "checkProductFacilityRelatedPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductFacility {}

    /**
     * Delete an ProductFacility
     */
    @Service(
        name = "deleteProductFacility",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "deleteProductFacility",
        description = "Delete an ProductFacility",
        defaultEntityName = "ProductFacility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "checkProductFacilityRelatedPermission", mainAction = "DELETE")
    )
    public interface DeleteProductFacility {}

    /**
     * Create an ProductFacilityLocation
     */
    @Service(
        name = "createProductFacilityLocation",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createProductFacilityLocation",
        description = "Create an ProductFacilityLocation",
        defaultEntityName = "ProductFacilityLocation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "checkProductFacilityRelatedPermission", mainAction = "CREATE")
    )
    public interface CreateProductFacilityLocation {}

    /**
     * Update an ProductFacilityLocation
     */
    @Service(
        name = "updateProductFacilityLocation",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "updateProductFacilityLocation",
        description = "Update an ProductFacilityLocation",
        defaultEntityName = "ProductFacilityLocation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "checkProductFacilityRelatedPermission", mainAction = "UPDATE")
    )
    public interface UpdateProductFacilityLocation {}

    /**
     * Delete an ProductFacilityLocation
     */
    @Service(
        name = "deleteProductFacilityLocation",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "deleteProductFacilityLocation",
        description = "Delete an ProductFacilityLocation",
        defaultEntityName = "ProductFacilityLocation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "checkProductFacilityRelatedPermission", mainAction = "DELETE")
    )
    public interface DeleteProductFacilityLocation {}

    /**
     * Create a Facility
     */
    @Service(
        name = "createFacility",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "createFacility",
        description = "Create a Facility",
        defaultEntityName = "Facility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "facilityTypeId", optional = "false"),
            @OverrideAttribute(name = "facilityName", optional = "false"),
            @OverrideAttribute(name = "ownerPartyId", optional = "false")
        }
    )
    public interface CreateFacility {}

    /**
     * Update a Facility
     */
    @Service(
        name = "updateFacility",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "updateFacility",
        description = "Update a Facility",
        defaultEntityName = "Facility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateFacility {}

    /**
     * Delete a Facility
     */
    @Service(
        name = "deleteFacility",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "deleteFacility",
        description = "Delete a Facility",
        defaultEntityName = "Facility",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteFacility {}

    /**
     * Create Facility Attribute
     */
    @Service(
        name = "createFacilityAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Facility Attribute",
        defaultEntityName = "FacilityAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "FacilityAttribute", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "FacilityAttribute", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateFacilityAttribute {}

    /**
     * Update Facility Attribute
     */
    @Service(
        name = "updateFacilityAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Facility Attribute",
        defaultEntityName = "FacilityAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "FacilityAttribute", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "FacilityAttribute", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFacilityAttribute {}

    /**
     * Delete Facility Attribute
     */
    @Service(
        name = "deleteFacilityAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Facility Attribute",
        defaultEntityName = "FacilityAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "FacilityAttribute", mode = "IN", include = "pk")
        }
    )
    public interface DeleteFacilityAttribute {}

    /**
     * Create a Facility Location
     */
    @Service(
        name = "createFacilityLocation",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "createFacilityLocation",
        description = "Create a Facility Location",
        defaultEntityName = "FacilityLocation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "locationSeqId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateFacilityLocation {}

    /**
     * Update a Facility Location
     */
    @Service(
        name = "updateFacilityLocation",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "updateFacilityLocation",
        description = "Update a Facility Location",
        defaultEntityName = "FacilityLocation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateFacilityLocation {}

    /**
     * Delete a Facility Location
     */
    @Service(
        name = "deleteFacilityLocation",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "deleteFacilityLocation",
        description = "Delete a Facility Location",
        defaultEntityName = "FacilityLocation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteFacilityLocation {}

    /**
     * Create a Facility Group
     */
    @Service(
        name = "createFacilityGroup",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "createFacilityGroup",
        description = "Create a Facility Group",
        defaultEntityName = "FacilityGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "facilityGroupName", optional = "false"),
            @OverrideAttribute(name = "facilityGroupTypeId", optional = "false")
        }
    )
    public interface CreateFacilityGroup {}

    /**
     * Update a Facility Group
     */
    @Service(
        name = "updateFacilityGroup",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "updateFacilityGroup",
        description = "Update a Facility Group",
        defaultEntityName = "FacilityGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateFacilityGroup {}

    /**
     * Delete a Facility Group
     */
    @Service(
        name = "deleteFacilityGroup",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "deleteFacilityGroup",
        description = "Delete a Facility Group",
        defaultEntityName = "FacilityGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteFacilityGroup {}

    /**
     * Create a FacilityContactMech
     */
    @Service(
        name = "createFacilityContactMech",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "createFacilityContactMech",
        description = "Create a FacilityContactMech",
        auth = "true",
        debug = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ContactMech", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "FacilityContactMech", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateFacilityContactMech {}

    /**
     * Update a FacilityContactMech
     */
    @Service(
        name = "updateFacilityContactMech",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "updateFacilityContactMech",
        description = "Update a FacilityContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "FacilityContactMech", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT"),
            @Attribute(name = "contactMechTypeId", type = "String", mode = "IN"),
            @Attribute(name = "infoString", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "newContactMechId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFacilityContactMech {}

    /**
     * Update a FacilityContactMech
     */
    @Service(
        name = "updateFacilityContactMechGiven",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "updateFacilityContactMech",
        description = "Update a FacilityContactMech",
        auth = "true",
        implemented = {@Implements(service = "updateFacilityContactMech")},
        attributes = {
            @Attribute(name = "facilityContactMech", type = "GenericValue", mode = "IN")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFacilityContactMechGiven {}

    /**
     * Delete (expire) a FacilityContactMech (SCIPIO: 2018-10-30: Now also expires FacilityContactMechPurpose records)
     */
    @Service(
        name = "deleteFacilityContactMech",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "deleteFacilityContactMechAndPurposes",
        description = "Delete (expire) a FacilityContactMech (SCIPIO: 2018-10-30: Now also expires FacilityContactMechPurpose records)",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteFacilityContactMech {}

    /**
     * Delete (expire) a FacilityContactMech only (SCIPIO: Not its purposes)
     */
    @Service(
        name = "deleteFacilityContactMechOnly",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "deleteFacilityContactMechOnly",
        description = "Delete (expire) a FacilityContactMech only (SCIPIO: Not its purposes)",
        auth = "true",
        implemented = {@Implements(service = "deleteFacilityContactMech")},
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteFacilityContactMechOnly {}

    /**
     * Create a Postal Address
     */
    @Service(
        name = "createFacilityPostalAddress",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "createFacilityPostalAddress",
        description = "Create a Postal Address",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "FacilityContactMech", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "PostalAddress", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "address1", optional = "false"),
            @OverrideAttribute(name = "city", optional = "false"),
            @OverrideAttribute(name = "postalCode", optional = "true")
        }
    )
    public interface CreateFacilityPostalAddress {}

    /**
     * Update a Postal Address
     */
    @Service(
        name = "updateFacilityPostalAddress",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "updateFacilityPostalAddress",
        description = "Update a Postal Address",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "FacilityContactMech", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "PostalAddress", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT"),
            @Attribute(name = "directions", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFacilityPostalAddress {}

    /**
     * Create a Telecommunications Number
     */
    @Service(
        name = "createFacilityTelecomNumber",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "createFacilityTelecomNumber",
        description = "Create a Telecommunications Number",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "FacilityContactMech", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "TelecomNumber", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateFacilityTelecomNumber {}

    /**
     * Update a Telecommunications Number
     */
    @Service(
        name = "updateFacilityTelecomNumber",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "updateFacilityTelecomNumber",
        description = "Update a Telecommunications Number",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "FacilityContactMech", mode = "IN", optional = "true"),
            @EntityAttributes(entityName = "TelecomNumber", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFacilityTelecomNumber {}

    /**
     * Create an Email Address
     */
    @Service(
        name = "createFacilityEmailAddress",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "createFacilityEmailAddress",
        description = "Create an Email Address",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ContactMech", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "FacilityContactMech", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface CreateFacilityEmailAddress {}

    /**
     * Update an Email Address
     */
    @Service(
        name = "updateFacilityEmailAddress",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "updateFacilityEmailAddress",
        description = "Update an Email Address",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "FacilityContactMech", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT"),
            @Attribute(name = "emailAddress", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateFacilityEmailAddress {}

    /**
     * Create a purpose for facility contact mech
     */
    @Service(
        name = "createFacilityContactMechPurpose",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "createFacilityContactMechPurpose",
        description = "Create a purpose for facility contact mech",
        entityAttributes = {
            @EntityAttributes(entityName = "FacilityContactMechPurpose", mode = "IN", include = "pk", excludeFields = {"fromDate"})
        },
        attributes = {
            @Attribute(name = "fromDate", type = "Timestamp", mode = "OUT")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "CREATE")
    )
    public interface CreateFacilityContactMechPurpose {}

    /**
     * Delete a purpose for facility contact mech
     */
    @Service(
        name = "deleteFacilityContactMechPurpose",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/FacilityContactMechServices.xml",
        invoke = "deleteFacilityContactMechPurpose",
        description = "Delete a purpose for facility contact mech",
        entityAttributes = {
            @EntityAttributes(entityName = "FacilityContactMechPurpose", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteFacilityContactMechPurpose {}

    /**
     * Ensures Party ContactMech has the requested purposes (SCIPIO)
     */
    @Service(
        name = "ensureFacilityContactMechPurposes",
        engine = "java",
        location = "org.ofbiz.party.contact.ContactMechServices",
        invoke = "ensureFacilityContactMechPurposes",
        description = "Ensures Party ContactMech has the requested purposes (SCIPIO)",
        auth = "true",
        implemented = {@Implements(service = "ensureContactMechPurposesInterface")},
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface EnsureFacilityContactMechPurposes {}

    /**
     * Add ContactMech To Facility
     */
    @Service(
        name = "addContactMechToFacility",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "addContactMechToFacility",
        description = "Add ContactMech To Facility",
        defaultEntityName = "FacilityContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface AddContactMechToFacility {}

    /**
     * Remove ContactMech From Facility
     */
    @Service(
        name = "removeContactMechFromFacility",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "removeContactMechFromFacility",
        description = "Remove ContactMech From Facility",
        defaultEntityName = "FacilityContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveContactMechFromFacility {}

    /**
     * Add Facility To FacilityGroup
     */
    @Service(
        name = "addFacilityToGroup",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "addFacilityToGroup",
        description = "Add Facility To FacilityGroup",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityGroupId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface AddFacilityToGroup {}

    /**
     * Update Facility -> Group Member
     */
    @Service(
        name = "updateFacilityToGroup",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "updateFacilityToGroup",
        description = "Update Facility -> Group Member",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityGroupId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateFacilityToGroup {}

    /**
     * Remove Facility From FacilityGroup
     */
    @Service(
        name = "removeFacilityFromGroup",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "removeFacilityFromGroup",
        description = "Remove Facility From FacilityGroup",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityGroupId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveFacilityFromGroup {}

    /**
     * Add FacilityGroup To FacilityGroup
     */
    @Service(
        name = "addFacilityGroupToGroup",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "addFacilityGroupToGroup",
        description = "Add FacilityGroup To FacilityGroup",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityGroupId", type = "String", mode = "IN"),
            @Attribute(name = "parentFacilityGroupId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface AddFacilityGroupToGroup {}

    /**
     * Update FacilityGroup To FacilityGroup Rollup
     */
    @Service(
        name = "updateFacilityGroupToGroup",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "updateFacilityGroupToGroup",
        description = "Update FacilityGroup To FacilityGroup Rollup",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityGroupId", type = "String", mode = "IN"),
            @Attribute(name = "parentFacilityGroupId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceNum", type = "Long", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateFacilityGroupToGroup {}

    /**
     * Remove FacilityGroup From FacilityGroup
     */
    @Service(
        name = "removeFacilityGroupFromGroup",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "removeFacilityGroupFromGroup",
        description = "Remove FacilityGroup From FacilityGroup",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityGroupId", type = "String", mode = "IN"),
            @Attribute(name = "parentFacilityGroupId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveFacilityGroupFromGroup {}

    /**
     * Create a FacilityParty record
     */
    @Service(
        name = "addPartyToFacility",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "addPartyToFacility",
        description = "Create a FacilityParty record",
        defaultEntityName = "FacilityParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface AddPartyToFacility {}

    /**
     * Add Party To FacilityGroup
     */
    @Service(
        name = "addPartyToFacilityGroup",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "addPartyToFacilityGroup",
        description = "Add Party To FacilityGroup",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityGroupId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface AddPartyToFacilityGroup {}

    /**
     * Remove Party From Facility
     */
    @Service(
        name = "removePartyFromFacility",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "removePartyFromFacility",
        description = "Remove Party From Facility",
        defaultEntityName = "FacilityParty",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface RemovePartyFromFacility {}

    /**
     * Remove Party From FacilityGroup
     */
    @Service(
        name = "removePartyFromFacilityGroup",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "removePartyFromFacilityGroup",
        description = "Remove Party From FacilityGroup",
        auth = "true",
        attributes = {
            @Attribute(name = "facilityGroupId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface RemovePartyFromFacilityGroup {}

    /**
     * Create a Facility Content
     */
    @Service(
        name = "createFacilityContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "createFacilityContent",
        description = "Create a Facility Content",
        defaultEntityName = "FacilityContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateFacilityContent {}

    /**
     * Delete Content From Facility
     */
    @Service(
        name = "deleteFacilityContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/storage/StorageServices.xml",
        invoke = "deleteFacilityContent",
        description = "Delete Content From Facility",
        defaultEntityName = "FacilityContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteFacilityContent {}

    /**
     *              Find all Stock Moves that need to be done.             This service differs from the inventory transfer services in that it does not require             sufficient availableToPromise to move the stock, in fact it is generally triggered because             some promised inventory is in a bulk location and needs to be moved to a pick location.         
     */
    @Service(
        name = "findStockMovesNeeded",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/StockMoveServices.xml",
        invoke = "findStockMovesNeeded",
        description = "\n            Find all Stock Moves that need to be done.\n            This service differs from the inventory transfer services in that it does not require\n            sufficient availableToPromise to move the stock, in fact it is generally triggered because\n            some promised inventory is in a bulk location and needs to be moved to a pick location.\n        ",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "moveByOisgirInfoList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "stockMoveHandled", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "warningMessageList", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "VIEW")
    )
    public interface FindStockMovesNeeded {}

    /**
     *              Find all Stock Moves that should be done based on minimum quantities on each Pick/Primary ProductFacilityLocation.         
     */
    @Service(
        name = "findStockMovesRecommended",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/StockMoveServices.xml",
        invoke = "findStockMovesRecommended",
        description = "\n            Find all Stock Moves that should be done based on minimum quantities on each Pick/Primary ProductFacilityLocation.\n        ",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "stockMoveHandled", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "moveByPflInfoList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "warningMessageList", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "VIEW")
    )
    public interface FindStockMovesRecommended {}

    /**
     *              Process a Physical Stock Move from one FacilityLocation to another, in the same Facility.             This service will not only move quantities from one InventoryItem to another but it will             also reassign any existing OrderItemShipGrpInvRes records to the new InventoryItem.         
     */
    @Service(
        name = "processPhysicalStockMove",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/StockMoveServices.xml",
        invoke = "processPhysicalStockMove",
        description = "\n            Process a Physical Stock Move from one FacilityLocation to another, in the same Facility.\n            This service will not only move quantities from one InventoryItem to another but it will\n            also reassign any existing OrderItemShipGrpInvRes records to the new InventoryItem.\n        ",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "locationSeqId", type = "String", mode = "IN"),
            @Attribute(name = "targetLocationSeqId", type = "String", mode = "IN"),
            @Attribute(name = "quantityMoved", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "warningMessageList", type = "List", mode = "INOUT", optional = "true")
        }
    )
    public interface ProcessPhysicalStockMove {}

    /**
     * Create an InventoryItemLabelType
     */
    @Service(
        name = "createInventoryItemLabelType",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createInventoryItemLabelType",
        description = "Create an InventoryItemLabelType",
        defaultEntityName = "InventoryItemLabelType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateInventoryItemLabelType {}

    /**
     * Update an InventoryItemLabelType
     */
    @Service(
        name = "updateInventoryItemLabelType",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "updateInventoryItemLabelType",
        description = "Update an InventoryItemLabelType",
        defaultEntityName = "InventoryItemLabelType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateInventoryItemLabelType {}

    /**
     * Delete an InventoryItemLabelType
     */
    @Service(
        name = "deleteInventoryItemLabelType",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "deleteInventoryItemLabelType",
        description = "Delete an InventoryItemLabelType",
        defaultEntityName = "InventoryItemLabelType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteInventoryItemLabelType {}

    /**
     * Create an InventoryItemLabel
     */
    @Service(
        name = "createInventoryItemLabel",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createInventoryItemLabel",
        description = "Create an InventoryItemLabel",
        defaultEntityName = "InventoryItemLabel",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "inventoryItemLabelTypeId", optional = "false")
        }
    )
    public interface CreateInventoryItemLabel {}

    /**
     * Update an InventoryItemLabel
     */
    @Service(
        name = "updateInventoryItemLabel",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "updateInventoryItemLabel",
        description = "Update an InventoryItemLabel",
        defaultEntityName = "InventoryItemLabel",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"inventoryItemLabelTypeId"})
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateInventoryItemLabel {}

    /**
     * Delete an InventoryItemLabel
     */
    @Service(
        name = "deleteInventoryItemLabel",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "deleteInventoryItemLabel",
        description = "Delete an InventoryItemLabel",
        defaultEntityName = "InventoryItemLabel",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteInventoryItemLabel {}

    /**
     * Create an InventoryItemLabelAppl
     */
    @Service(
        name = "createInventoryItemLabelAppl",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createInventoryItemLabelAppl",
        description = "Create an InventoryItemLabelAppl",
        defaultEntityName = "InventoryItemLabelAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", excludeFields = {"inventoryItemLabelTypeId"}),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "inventoryItemLabelId", optional = "false")
        }
    )
    public interface CreateInventoryItemLabelAppl {}

    /**
     * Update an InventoryItemLabelAppl
     */
    @Service(
        name = "updateInventoryItemLabelAppl",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "updateInventoryItemLabelAppl",
        description = "Update an InventoryItemLabelAppl",
        defaultEntityName = "InventoryItemLabelAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"inventoryItemId"})
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateInventoryItemLabelAppl {}

    /**
     * Delete an InventoryItemLabelAppl
     */
    @Service(
        name = "deleteInventoryItemLabelAppl",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "deleteInventoryItemLabelAppl",
        description = "Delete an InventoryItemLabelAppl",
        defaultEntityName = "InventoryItemLabelAppl",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteInventoryItemLabelAppl {}

    /**
     * set order priority
     */
    @Service(
        name = "setOrderReservationPriority",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "setOrderReservationPriority",
        description = "set order priority",
        auth = "true",
        attributes = {
            @Attribute(name = "priority", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface SetOrderReservationPriority {}

    /**
     * Find Product's inventory locations from facility
     */
    @Service(
        name = "findProductInventorylocations",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/StockMoveServices.xml",
        invoke = "findProductInventorylocations",
        description = "Find Product's inventory locations from facility",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "LocationList", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "VIEW")
    )
    public interface FindProductInventorylocations {}

    /**
     * Service which run as EECA (on InventoryItemDetail entity) and updates lastInventoryCount for products available in facility in ProductFacility entity
     */
    @Service(
        name = "setLastInventoryCount",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "setLastInventoryCount",
        description = "Service which run as EECA (on InventoryItemDetail entity) and updates lastInventoryCount for products available in facility in ProductFacility entity",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemDetail", type = "GenericValue", mode = "IN", optional = "true", description = "Optional InventoryItemDetail record to help implement entity cache configured in inventory.properties (SCIPIO)"),
            @Attribute(name = "lastInvMode", type = "String", mode = "IN", optional = "true", description = "Mode for setting last inventory count, or type/source of the call. Values: AUTO (default), MANUAL (SCIPIO)"),
            @Attribute(name = "productId", type = "String", mode = "OUT", optional = "true", description = "productId of the inventory item, for SECA and reuse (SCIPIO)")
        }
    )
    public interface SetLastInventoryCount {}

    /**
     * Service which run as EECA (on InventoryItemDetail entity) and updates lastInventoryCount for products available in facility in ProductFacility entity             - runs for specified productId
     */
    @Service(
        name = "setProductLastInventoryCount",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "setProductLastInventoryCount",
        description = "Service which run as EECA (on InventoryItemDetail entity) and updates lastInventoryCount for products available in facility in ProductFacility entity\n            - runs for specified productId",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "lastInvMode", type = "String", mode = "IN", optional = "true", description = "Mode for setting last inventory count, or type/source of the call. Values: AUTO (default), MANUAL (SCIPIO)")
        }
    )
    public interface SetProductLastInventoryCount {}

    /**
     * Service which run as EECA (on InventoryItemDetail entity) and updates lastInventoryCount for products available in facility in ProductFacility entity             - runs for all products in the system (SCIPIO)
     */
    @Service(
        name = "setAllProductsLastInventoryCount",
        engine = "java",
        location = "org.ofbiz.product.inventory.InventoryServices",
        invoke = "setAllProductsLastInventoryCount",
        description = "Service which run as EECA (on InventoryItemDetail entity) and updates lastInventoryCount for products available in facility in ProductFacility entity\n            - runs for all products in the system (SCIPIO)",
        transactionTimeout = "14400",
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "ADMIN")
    )
    public interface SetAllProductsLastInventoryCount {}

    /**
     * Checks all ProductFacility records for lastInventoryCount and those lastInvStamp greater than a configured or passed time             and who were last updated in a non-manual way (lastInvMode AUTO) are recounted - used to trigger delayed recounts to automatically correct             inventory cache amounts (SCIPIO)
     */
    @Service(
        name = "updateDirtyLastInventoryCounts",
        engine = "java",
        location = "org.ofbiz.product.inventory.InventoryServices",
        invoke = "updateDirtyLastInventoryCounts",
        description = "Checks all ProductFacility records for lastInventoryCount and those lastInvStamp greater than a configured or passed time\n            and who were last updated in a non-manual way (lastInvMode AUTO) are recounted - used to trigger delayed recounts to automatically correct\n            inventory cache amounts (SCIPIO)",
        transactionTimeout = "14400",
        attributes = {
            @Attribute(name = "expiryTime", type = "Long", mode = "IN", optional = "true", description = "Expiry time in milliseconds"),
            @Attribute(name = "sepTrans", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true")
        }
    )
    public interface UpdateDirtyLastInventoryCounts {}

    /**
     * Create or update GeoPoint assigned to facility
     */
    @Service(
        name = "createUpdateFacilityGeoPoint",
        engine = "simple",
        location = "component://product/script/org/ofbiz/product/inventory/InventoryServices.xml",
        invoke = "createUpdateFacilityGeoPoint",
        description = "Create or update GeoPoint assigned to facility",
        defaultEntityName = "GeoPoint",
        entityAttributes = {
            @EntityAttributes(mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "dataSourceId", optional = "false"),
            @OverrideAttribute(name = "latitude", optional = "false"),
            @OverrideAttribute(name = "longitude", optional = "false")
        }
    )
    public interface CreateUpdateFacilityGeoPoint {}

    /**
     * Create a new Container Record
     */
    @Service(
        name = "createContainer",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Container Record",
        defaultEntityName = "Container",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateContainer {}

    /**
     * Update a Container
     */
    @Service(
        name = "updateContainer",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Container",
        defaultEntityName = "Container",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateContainer {}

    /**
     * Delete an existing Container Record
     */
    @Service(
        name = "deleteContainer",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing Container Record",
        defaultEntityName = "Container",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteContainer {}

    /**
     * Create a new ContainerType Record
     */
    @Service(
        name = "createContainerType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new ContainerType Record",
        defaultEntityName = "ContainerType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateContainerType {}

    /**
     * Update a ContainerType
     */
    @Service(
        name = "updateContainerType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ContainerType",
        defaultEntityName = "ContainerType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateContainerType {}

    /**
     * Delete an existing ContainerType Record
     */
    @Service(
        name = "deleteContainerType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing ContainerType Record",
        defaultEntityName = "ContainerType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteContainerType {}

    /**
     * Create a FacilityGroupType
     */
    @Service(
        name = "createFacilityGroupType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FacilityGroupType",
        defaultEntityName = "FacilityGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateFacilityGroupType {}

    /**
     * Update a FacilityGroupType
     */
    @Service(
        name = "updateFacilityGroupType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FacilityGroupType",
        defaultEntityName = "FacilityGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFacilityGroupType {}

    /**
     * Delete a FacilityGroupType
     */
    @Service(
        name = "deleteFacilityGroupType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FacilityGroupType",
        defaultEntityName = "FacilityGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFacilityGroupType {}

    /**
     * Create a FacilityTypeAttr
     */
    @Service(
        name = "createFacilityTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FacilityTypeAttr",
        defaultEntityName = "FacilityTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateFacilityTypeAttr {}

    /**
     * Update a FacilityTypeAttr
     */
    @Service(
        name = "updateFacilityTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FacilityTypeAttr",
        defaultEntityName = "FacilityTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFacilityTypeAttr {}

    /**
     * Remove a FacilityTypeAttr
     */
    @Service(
        name = "removeFacilityTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Remove a FacilityTypeAttr",
        defaultEntityName = "FacilityTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveFacilityTypeAttr {}

    /**
     * Create a FacilityType record
     */
    @Service(
        name = "createFacilityType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FacilityType record",
        defaultEntityName = "FacilityType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateFacilityType {}

    /**
     * Update a FacilityType record
     */
    @Service(
        name = "updateFacilityType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a FacilityType record",
        defaultEntityName = "FacilityType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateFacilityType {}

    /**
     * Delete a FacilityType record
     */
    @Service(
        name = "deleteFacilityType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FacilityType record",
        defaultEntityName = "FacilityType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFacilityType {}

    /**
     * Create a FacilityCarrierShipment record
     */
    @Service(
        name = "createFacilityCarrierShipment",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FacilityCarrierShipment record",
        defaultEntityName = "FacilityCarrierShipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateFacilityCarrierShipment {}

    /**
     * Delete a FacilityCarrierShipment record
     */
    @Service(
        name = "deleteFacilityCarrierShipment",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a FacilityCarrierShipment record",
        defaultEntityName = "FacilityCarrierShipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteFacilityCarrierShipment {}

    /**
     * Create a FacilityLocationGeoPoint record
     */
    @Service(
        name = "createFacilityLocationGeoPoint",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a FacilityLocationGeoPoint record",
        defaultEntityName = "FacilityLocationGeoPoint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateFacilityLocationGeoPoint {}

    /**
     * Expire a FacilityLocationGeoPoint record
     */
    @Service(
        name = "expireFacilityLocationGeoPoint",
        engine = "entity-auto",
        invoke = "expire",
        description = "Expire a FacilityLocationGeoPoint record",
        defaultEntityName = "FacilityLocationGeoPoint",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface ExpireFacilityLocationGeoPoint {}

    /**
     * Create InventoryItemType
     */
    @Service(
        name = "createInventoryItemType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create InventoryItemType",
        defaultEntityName = "InventoryItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateInventoryItemType {}

    /**
     * Update InventoryItemType
     */
    @Service(
        name = "updateInventoryItemType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update InventoryItemType",
        defaultEntityName = "InventoryItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInventoryItemType {}

    /**
     * Delete InventoryItemType
     */
    @Service(
        name = "deleteInventoryItemType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete InventoryItemType",
        defaultEntityName = "InventoryItemType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInventoryItemType {}

}

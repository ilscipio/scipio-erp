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
public class PicklistServices {

    /**
     * Convert a list of order IDs to a list of headers
     */
    @Service(
        name = "convertPickOrderIdListToHeaders",
        engine = "java",
        location = "org.ofbiz.shipment.picklist.PickListServices",
        invoke = "convertOrderIdListToHeaders",
        description = "Convert a list of order IDs to a list of headers",
        attributes = {
            @Attribute(name = "orderIdList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderHeaderList", type = "List", mode = "INOUT", optional = "true")
        }
    )
    public interface ConvertPickOrderIdListToHeaders {}

    /**
     * Gets Picklist Data
     */
    @Service(
        name = "findOrdersToPickMove",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "findOrdersToPickMove",
        description = "Gets Picklist Data",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentMethodTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "isRushOrder", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "maxNumberOfOrders", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "orderHeaderList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "pickMoveInfoList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "rushOrderInfo", type = "Map", mode = "OUT", optional = "true"),
            @Attribute(name = "groupByNoOfOrderItems", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "groupByWarehouseArea", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "groupByShippingMethod", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "VIEW")
    )
    public interface FindOrdersToPickMove {}

    /**
     * Create Picklist From Orders
     */
    @Service(
        name = "createPicklistFromOrders",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "createPicklistFromOrders",
        description = "Create Picklist From Orders",
        transactionTimeout = "600",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentMethodTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "maxNumberOfOrders", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "isRushOrder", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderIdList", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderHeaderList", type = "List", mode = "INOUT", optional = "true"),
            @Attribute(name = "picklistId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePicklistFromOrders {}

    /**
     * Print pick sheets for orders
     */
    @Service(
        name = "printPickSheets",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "printPickSheets",
        description = "Print pick sheets for orders",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "maxNumberOfOrdersToPrint", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "printGroupName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "groupByNoOfOrderItems", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "groupByWarehouseArea", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "groupByShippingMethod", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pickMoveInfoList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface PrintPickSheets {}

    /**
     * Create Picklist From Orders
     */
    @Service(
        name = "getPicklistDisplayInfo",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "getPicklistDisplayInfo",
        description = "Create Picklist From Orders",
        attributes = {
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "viewIndex", type = "Integer", mode = "INOUT", optional = "true"),
            @Attribute(name = "viewSize", type = "Integer", mode = "INOUT", optional = "true"),
            @Attribute(name = "highIndex", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "lowIndex", type = "Integer", mode = "OUT", optional = "true"),
            @Attribute(name = "picklistCount", type = "Long", mode = "OUT", optional = "true"),
            @Attribute(name = "picklistInfoList", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "VIEW")
    )
    public interface GetPicklistDisplayInfo {}

    /**
     * Get Pick And Pack Report Info
     */
    @Service(
        name = "getPickAndPackReportInfo",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "getPickAndPackReportInfo",
        description = "Get Pick And Pack Report Info",
        attributes = {
            @Attribute(name = "picklistId", type = "String", mode = "IN"),
            @Attribute(name = "picklistInfo", type = "Map", mode = "OUT"),
            @Attribute(name = "facilityLocationInfoList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "noLocationProductInfoList", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "VIEW")
    )
    public interface GetPickAndPackReportInfo {}

    /**
     * Create Picklist
     */
    @Service(
        name = "createPicklist",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "createPicklist",
        description = "Create Picklist",
        defaultEntityName = "Picklist",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"picklistDate", "createdByUserLogin", "lastModifiedByUserLogin"})
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePicklist {}

    /**
     * Update Picklist
     */
    @Service(
        name = "updatePicklist",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "updatePicklist",
        description = "Update Picklist",
        defaultEntityName = "Picklist",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"picklistDate", "createdByUserLogin", "lastModifiedByUserLogin"})
        },
        attributes = {
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePicklist {}

    /**
     * Delete Picklist
     */
    @Service(
        name = "deletePicklist",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "deletePicklist",
        description = "Delete Picklist",
        defaultEntityName = "Picklist",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "DELETE")
    )
    public interface DeletePicklist {}

    /**
     * Create PicklistBin
     */
    @Service(
        name = "createPicklistBin",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "createPicklistBin",
        description = "Create PicklistBin",
        defaultEntityName = "PicklistBin",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "CREATE")
    )
    public interface CreatePicklistBin {}

    /**
     * Update PicklistBin
     */
    @Service(
        name = "updatePicklistBin",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "updatePicklistBin",
        description = "Update PicklistBin",
        defaultEntityName = "PicklistBin",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePicklistBin {}

    /**
     * Delete PicklistBin
     */
    @Service(
        name = "deletePicklistBin",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "deletePicklistBin",
        description = "Delete PicklistBin",
        defaultEntityName = "PicklistBin",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "DELETE")
    )
    public interface DeletePicklistBin {}

    /**
     * Update Picklist based on Item Status
     */
    @Service(
        name = "checkPicklistBinItemStatuses",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "checkPicklistBinItemStatuses",
        description = "Update Picklist based on Item Status",
        defaultEntityName = "PicklistBin",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface CheckPicklistBinItemStatuses {}

    /**
     * Create PicklistItem
     */
    @Service(
        name = "createPicklistItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "createPicklistItem",
        description = "Create PicklistItem",
        defaultEntityName = "PicklistItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "itemStatusId", optional = "true")
        }
    )
    public interface CreatePicklistItem {}

    /**
     * Update PicklistItem
     */
    @Service(
        name = "updatePicklistItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "updatePicklistItem",
        description = "Update PicklistItem",
        defaultEntityName = "PicklistItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "oldItemStatusId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePicklistItem {}

    /**
     * Delete PicklistItem
     */
    @Service(
        name = "deletePicklistItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "deletePicklistItem",
        description = "Delete PicklistItem",
        defaultEntityName = "PicklistItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "DELETE")
    )
    public interface DeletePicklistItem {}

    /**
     * Edit PicklistItem
     */
    @Service(
        name = "editPicklistItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "editPicklistItem",
        description = "Edit PicklistItem",
        defaultEntityName = "PicklistItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "lotId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN"),
            @Attribute(name = "oldLotId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface EditPicklistItem {}

    /**
     * Update PicklistItem's Status to COMPLETE
     */
    @Service(
        name = "setPicklistItemToComplete",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "setPicklistItemToComplete",
        description = "Update PicklistItem's Status to COMPLETE",
        defaultEntityName = "PicklistItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface SetPicklistItemToComplete {}

    /**
     * If Picklist is Cancelled then cancel all the PicklistItems.
     */
    @Service(
        name = "cancelPicklistAndItems",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "cancelPicklistAndItems",
        description = "If Picklist is Cancelled then cancel all the PicklistItems.",
        auth = "true",
        attributes = {
            @Attribute(name = "picklistId", type = "String", mode = "IN")
        }
    )
    public interface CancelPicklistAndItems {}

    /**
     * Create PicklistRole
     */
    @Service(
        name = "createPicklistRole",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "createPicklistRole",
        description = "Create PicklistRole",
        defaultEntityName = "PicklistRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdByUserLogin", "lastModifiedByUserLogin"})
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreatePicklistRole {}

    /**
     * Update PicklistRole
     */
    @Service(
        name = "updatePicklistRole",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "updatePicklistRole",
        description = "Update PicklistRole",
        defaultEntityName = "PicklistRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdByUserLogin", "lastModifiedByUserLogin"})
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePicklistRole {}

    /**
     * Delete PicklistRole
     */
    @Service(
        name = "deletePicklistRole",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/picklist/PicklistServices.xml",
        invoke = "deletePicklistRole",
        description = "Delete PicklistRole",
        defaultEntityName = "PicklistRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityPermissionCheck", mainAction = "DELETE")
    )
    public interface DeletePicklistRole {}

}

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
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ShipmentServices {

    /**
     * Creates A CarrierShipmentMethod
     */
    @Service(
        name = "createCarrierShipmentMethod",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createCarrierShipmentMethod",
        description = "Creates A CarrierShipmentMethod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CarrierShipmentMethod", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "CarrierShipmentMethod", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCarrierShipmentMethod {}

    /**
     * Updates A CarrierShipmentMethod
     */
    @Service(
        name = "updateCarrierShipmentMethod",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "updateCarrierShipmentMethod",
        description = "Updates A CarrierShipmentMethod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CarrierShipmentMethod", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "CarrierShipmentMethod", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCarrierShipmentMethod {}

    /**
     * Deletes A CarrierShipmentMethod
     */
    @Service(
        name = "deleteCarrierShipmentMethod",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "deleteCarrierShipmentMethod",
        description = "Deletes A CarrierShipmentMethod",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CarrierShipmentMethod", mode = "IN", include = "pk")
        }
    )
    public interface DeleteCarrierShipmentMethod {}

    /**
     * Creates A ShipmentMethodType
     */
    @Service(
        name = "createShipmentMethodType",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createShipmentMethodType",
        description = "Creates A ShipmentMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentMethodType", mode = "IN")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "sequenceNum", optional = "true")
        }
    )
    public interface CreateShipmentMethodType {}

    /**
     * Updates A ShipmentMethodType
     */
    @Service(
        name = "updateShipmentMethodType",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "updateShipmentMethodType",
        description = "Updates A ShipmentMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentMethodType", mode = "IN")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "sequenceNum", optional = "true")
        }
    )
    public interface UpdateShipmentMethodType {}

    /**
     * Deletes A ShipmentMethodType
     */
    @Service(
        name = "deleteShipmentMethodType",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "deleteShipmentMethodType",
        description = "Deletes A ShipmentMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentMethodType", mode = "IN", include = "pk")
        }
    )
    public interface DeleteShipmentMethodType {}

    /**
     * Create a carrier PartyGroup
     */
    @Service(
        name = "createCarrier",
        engine = "group",
        description = "Create a carrier PartyGroup",
        invokes = {@GroupInvoke(name = "createPartyGroup", resultToContext = "true"), @GroupInvoke(name = "createPartyRole", resultToContext = "false")}
    )
    public interface CreateCarrier {}

    /**
     * Create Shipment, ShipmentItems and OrderShipment
     */
    @Service(
        name = "createOrderShipmentPlan",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createOrderShipmentPlan",
        description = "Create Shipment, ShipmentItems and OrderShipment",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateOrderShipmentPlan {}

    /**
     * Quick Ships An Entire Order Creating One Shipment Per Facility and Ship Group.  All approved order items are           automatically issued in full and put into one package.  The shipment is created in the INPUT status and then updated to           PACKED and SHIPPED.         
     */
    @Service(
        name = "quickShipEntireOrder",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "quickShipEntireOrder",
        description = "Quick Ships An Entire Order Creating One Shipment Per Facility and Ship Group.  All approved order items are\n          automatically issued in full and put into one package.  The shipment is created in the INPUT status and then updated to\n          PACKED and SHIPPED.\n        ",
        auth = "true",
        transactionTimeout = "600",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "originFacilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "setPackedOnly", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "eventDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentShipGroupFacilityList", type = "List", mode = "OUT")
        }
    )
    public interface QuickShipEntireOrder {}

    /**
     * Quick Ships An Order By Item
     */
    @Service(
        name = "quickShipOrderByItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "quickShipOrderByItem",
        description = "Quick Ships An Order By Item",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "itemShipList", type = "List", mode = "IN"),
            @Attribute(name = "originFacilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "setPackedOnly", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentId", type = "String", mode = "OUT")
        }
    )
    public interface QuickShipOrderByItem {}

    /**
     * Sends an order assuming a shipment already exists.
     */
    @Service(
        name = "orderSendShip",
        engine = "java",
        location = "com.ilscipio.scipio.shipment.shipment.ShipmentServices",
        invoke = "orderSendShip",
        description = "Sends an order assuming a shipment already exists.",
        auth = "true",
        transactionTimeout = "600",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "originFacilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "eventDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentShipGroupFacilityList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface OrderSendShip {}

    /**
     * Completes an order assuming a shipment already exists.
     */
    @Service(
        name = "orderCompleteShip",
        engine = "java",
        location = "com.ilscipio.scipio.shipment.shipment.ShipmentServices",
        invoke = "orderCompleteShip",
        description = "Completes an order assuming a shipment already exists.",
        auth = "true",
        transactionTimeout = "600",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "originFacilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "eventDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentShipGroupFacilityList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface OrderCompleteShip {}

    /**
     * The mirror of quickShipEntireOrder, this service automatically creates shipments for an entire purchase order.           All order items on each ship group is created as a Shipment.  All items on a Shipment are automatically issued to a Package.           The shipment's status is first set to CREATED and then set as SHIPPED.  The facilityId is used to set the destinationFacilityId           of the Shipment.         
     */
    @Service(
        name = "quickShipPurchaseOrder",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "quickShipPurchaseOrder",
        description = "The mirror of quickShipEntireOrder, this service automatically creates shipments for an entire purchase order.\n          All order items on each ship group is created as a Shipment.  All items on a Shipment are automatically issued to a Package.\n          The shipment's status is first set to CREATED and then set as SHIPPED.  The facilityId is used to set the destinationFacilityId\n          of the Shipment.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN")
        }
    )
    public interface QuickShipPurchaseOrder {}

    /**
     * Create a Return Shipment with information from ReturnHeader fields
     */
    @Service(
        name = "createShipmentForReturn",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createShipmentForReturn",
        description = "Create a Return Shipment with information from ReturnHeader fields",
        defaultEntityName = "ReturnHeader",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateShipmentForReturn {}

    /**
     * Create a Return Shipment and ShipmentItems with information from ReturnHeader and ReturnItems
     */
    @Service(
        name = "createShipmentAndItemsForReturn",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createShipmentAndItemsForReturn",
        description = "Create a Return Shipment and ShipmentItems with information from ReturnHeader and ReturnItems",
        defaultEntityName = "ReturnHeader",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateShipmentAndItemsForReturn {}

    /**
     * Create a Return Shipment and ShipmentItems with primaryReturnId
     */
    @Service(
        name = "createShipmentAndItemsForVendorReturn",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createShipmentAndItemsForVendorReturn",
        description = "Create a Return Shipment and ShipmentItems with primaryReturnId",
        defaultEntityName = "Shipment",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "OUT")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "primaryReturnId", optional = "false")
        }
    )
    public interface CreateShipmentAndItemsForVendorReturn {}

    /**
     * Create Shipment
     */
    @Service(
        name = "createShipment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createShipment",
        description = "Create Shipment",
        defaultEntityName = "Shipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        attributes = {
            @Attribute(name = "shipmentTypeId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateShipment {}

    /**
     * Update Shipment
     */
    @Service(
        name = "updateShipment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "updateShipment",
        description = "Update Shipment",
        defaultEntityName = "Shipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"shipmentTypeId", "createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        attributes = {
            @Attribute(name = "shipmentTypeId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "eventDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "oldPrimaryOrderId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "oldOriginFacilityId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "oldDestinationFacilityId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateShipment {}

    /**
     * Delete Shipment
     */
    @Service(
        name = "deleteShipment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "deleteShipment",
        description = "Delete Shipment",
        defaultEntityName = "Shipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteShipment {}

    /**
     * Create Shipment Status
     */
    @Service(
        name = "createShipmentStatus",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Shipment Status",
        defaultEntityName = "ShipmentStatus",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "statusDate", mode = "IN", optional = "true")
        }
    )
    public interface CreateShipmentStatus {}

    /**
     * Set Shipment Settings From Primary Order
     */
    @Service(
        name = "setShipmentSettingsFromPrimaryOrder",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "setShipmentSettingsFromPrimaryOrder",
        description = "Set Shipment Settings From Primary Order",
        auth = "true",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN")
        }
    )
    public interface SetShipmentSettingsFromPrimaryOrder {}

    /**
     * Set Shipment Settings From Facilities
     */
    @Service(
        name = "setShipmentSettingsFromFacilities",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "setShipmentSettingsFromFacilities",
        description = "Set Shipment Settings From Facilities",
        auth = "true",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN")
        }
    )
    public interface SetShipmentSettingsFromFacilities {}

    /**
     * Send Shipment Scheduled Notification
     */
    @Service(
        name = "sendShipmentScheduledNotification",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "sendShipmentScheduledNotification",
        description = "Send Shipment Scheduled Notification",
        auth = "true",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN")
        }
    )
    public interface SendShipmentScheduledNotification {}

    /**
     *              Release the purchase order's items assigned to the shipment but not             actually received; it is invoked as a seca when the purchase shipment             is marked as 'received'         
     */
    @Service(
        name = "balanceItemIssuancesForShipment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "balanceItemIssuancesForShipment",
        description = "\n            Release the purchase order's items assigned to the shipment but not\n            actually received; it is invoked as a seca when the purchase shipment\n            is marked as 'received'\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN")
        }
    )
    public interface BalanceItemIssuancesForShipment {}

    /**
     * Check Shipment Items and cancel Item Issuance and Order Shipment
     */
    @Service(
        name = "checkCancelItemIssuanceAndOrderShipmentFromShipment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "checkCancelItemIssuanceAndOrderShipmentFromShipment",
        description = "Check Shipment Items and cancel Item Issuance and Order Shipment",
        auth = "true",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface CheckCancelItemIssuanceAndOrderShipmentFromShipment {}

    /**
     * Creates a drop shipment for a ship group and calls updateShipment twice in succession to set             shipment status to PURCH_SHIP_SHIPPED and then to PURCH_SHIP_RECEIVED
     */
    @Service(
        name = "quickDropShipOrder",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "quickDropShipOrder",
        description = "Creates a drop shipment for a ship group and calls updateShipment twice in succession to set\n            shipment status to PURCH_SHIP_SHIPPED and then to PURCH_SHIP_RECEIVED",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentId", type = "String", mode = "OUT")
        }
    )
    public interface QuickDropShipOrder {}

    /**
     * Create ShipmentItem
     */
    @Service(
        name = "createShipmentItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createShipmentItem",
        description = "Create ShipmentItem",
        defaultEntityName = "ShipmentItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "shipmentItemSeqId", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateShipmentItem {}

    /**
     * Update ShipmentItem
     */
    @Service(
        name = "updateShipmentItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "updateShipmentItem",
        description = "Update ShipmentItem",
        defaultEntityName = "ShipmentItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateShipmentItem {}

    /**
     * Delete ShipmentItem
     */
    @Service(
        name = "deleteShipmentItem",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "deleteShipmentItem",
        description = "Delete ShipmentItem",
        defaultEntityName = "ShipmentItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteShipmentItem {}

    /**
     * Splits the specified ShipmentItem creating a new ShipmentItem with the given newItemQuantity.             NOTE that this does manage OrderShipment records, but NOTHING else, so it is only to be used for Shipment             Plan stuff BEFORE the items are issued, shipment packed, etc.
     */
    @Service(
        name = "splitShipmentItemByQuantity",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "splitShipmentItemByQuantity",
        description = "Splits the specified ShipmentItem creating a new ShipmentItem with the given newItemQuantity.\n            NOTE that this does manage OrderShipment records, but NOTHING else, so it is only to be used for Shipment\n            Plan stuff BEFORE the items are issued, shipment packed, etc.",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentItem", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "newItemQuantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "newShipmentItemSeqId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SplitShipmentItemByQuantity {}

    /**
     * Create ShipmentPackage
     */
    @Service(
        name = "createShipmentPackage",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createShipmentPackage",
        description = "Create ShipmentPackage",
        defaultEntityName = "ShipmentPackage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true", excludeFields = {"dateCreated"})
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "shipmentPackageSeqId", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateShipmentPackage {}

    /**
     * Update ShipmentPackage
     */
    @Service(
        name = "updateShipmentPackage",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "updateShipmentPackage",
        description = "Update ShipmentPackage",
        defaultEntityName = "ShipmentPackage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateShipmentPackage {}

    /**
     * Delete ShipmentPackage
     */
    @Service(
        name = "deleteShipmentPackage",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "deleteShipmentPackage",
        description = "Delete ShipmentPackage",
        defaultEntityName = "ShipmentPackage",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteShipmentPackage {}

    /**
     * Create ShipmentPackageContent
     */
    @Service(
        name = "createShipmentPackageContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createShipmentPackageContent",
        description = "Create ShipmentPackageContent",
        defaultEntityName = "ShipmentPackageContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "shipmentPackageSeqId", mode = "INOUT", optional = "false")
        }
    )
    public interface CreateShipmentPackageContent {}

    /**
     * Update ShipmentPackageContent
     */
    @Service(
        name = "updateShipmentPackageContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "updateShipmentPackageContent",
        description = "Update ShipmentPackageContent",
        defaultEntityName = "ShipmentPackageContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateShipmentPackageContent {}

    /**
     * Delete ShipmentPackageContent
     */
    @Service(
        name = "deleteShipmentPackageContent",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "deleteShipmentPackageContent",
        description = "Delete ShipmentPackageContent",
        defaultEntityName = "ShipmentPackageContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteShipmentPackageContent {}

    /**
     * Add Shipment Content To Package
     */
    @Service(
        name = "addShipmentContentToPackage",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "addShipmentContentToPackage",
        description = "Add Shipment Content To Package",
        defaultEntityName = "ShipmentPackageContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "quantity", mode = "IN", optional = "false"),
            @OverrideAttribute(name = "shipmentPackageSeqId", mode = "INOUT", optional = "false")
        }
    )
    public interface AddShipmentContentToPackage {}

    /**
     * Create ShipmentPackageRouteSeg
     */
    @Service(
        name = "createShipmentPackageRouteSeg",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createShipmentPackageRouteSeg",
        description = "Create ShipmentPackageRouteSeg",
        defaultEntityName = "ShipmentPackageRouteSeg",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateShipmentPackageRouteSeg {}

    /**
     * Update ShipmentPackageRouteSeg
     */
    @Service(
        name = "updateShipmentPackageRouteSeg",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "updateShipmentPackageRouteSeg",
        description = "Update ShipmentPackageRouteSeg",
        defaultEntityName = "ShipmentPackageRouteSeg",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateShipmentPackageRouteSeg {}

    /**
     * Delete ShipmentPackageRouteSeg
     */
    @Service(
        name = "deleteShipmentPackageRouteSeg",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "deleteShipmentPackageRouteSeg",
        description = "Delete ShipmentPackageRouteSeg",
        defaultEntityName = "ShipmentPackageRouteSeg",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteShipmentPackageRouteSeg {}

    /**
     * Create ShipmentContactMech
     */
    @Service(
        name = "createShipmentContactMech",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createShipmentContactMech",
        description = "Create ShipmentContactMech",
        defaultEntityName = "ShipmentContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateShipmentContactMech {}

    /**
     * Update ShipmentContactMech
     */
    @Service(
        name = "updateShipmentContactMech",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "updateShipmentContactMech",
        description = "Update ShipmentContactMech",
        defaultEntityName = "ShipmentContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateShipmentContactMech {}

    /**
     * Delete ShipmentContactMech
     */
    @Service(
        name = "deleteShipmentContactMech",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "deleteShipmentContactMech",
        description = "Delete ShipmentContactMech",
        defaultEntityName = "ShipmentContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteShipmentContactMech {}

    /**
     * Create ShipmentRouteSegment
     */
    @Service(
        name = "createShipmentRouteSegment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createShipmentRouteSegment",
        description = "Create ShipmentRouteSegment",
        defaultEntityName = "ShipmentRouteSegment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "shipmentRouteSegmentId", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateShipmentRouteSegment {}

    /**
     * Update ShipmentRouteSegment
     */
    @Service(
        name = "updateShipmentRouteSegment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "updateShipmentRouteSegment",
        description = "Update ShipmentRouteSegment",
        defaultEntityName = "ShipmentRouteSegment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateShipmentRouteSegment {}

    /**
     * Delete ShipmentRouteSegment
     */
    @Service(
        name = "deleteShipmentRouteSegment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "deleteShipmentRouteSegment",
        description = "Delete ShipmentRouteSegment",
        defaultEntityName = "ShipmentRouteSegment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteShipmentRouteSegment {}

    /**
     * Duplicates a shipment route segment and creates the new route segment in the NOT_STARTED status
     */
    @Service(
        name = "duplicateShipmentRouteSegment",
        engine = "java",
        location = "org.ofbiz.shipment.shipment.ShipmentServices",
        invoke = "duplicateShipmentRouteSegment",
        description = "Duplicates a shipment route segment and creates the new route segment in the NOT_STARTED status",
        defaultEntityName = "ShipmentRouteSegment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "newShipmentRouteSegmentId", type = "String", mode = "OUT")
        }
    )
    public interface DuplicateShipmentRouteSegment {}

    /**
     * Schedules a shipment route segment with the carrier and service level in the ShipmentRouteSegment entity.           Actual scheduling is done by an async service, and this does not return an error, so it can be called in a multi-form,           and one failed shipment scheduling does not cause other shipments to be rolled back.         
     */
    @Service(
        name = "quickScheduleShipmentRouteSegment",
        engine = "java",
        location = "org.ofbiz.shipment.shipment.ShipmentServices",
        invoke = "quickScheduleShipmentRouteSegment",
        description = "Schedules a shipment route segment with the carrier and service level in the ShipmentRouteSegment entity.\n          Actual scheduling is done by an async service, and this does not return an error, so it can be called in a multi-form,\n          and one failed shipment scheduling does not cause other shipments to be rolled back.\n        ",
        defaultEntityName = "ShipmentRouteSegment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface QuickScheduleShipmentRouteSegment {}

    /**
     * Create ItemIssuance
     */
    @Service(
        name = "createItemIssuance",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml",
        invoke = "createItemIssuance",
        description = "Create ItemIssuance",
        defaultEntityName = "ItemIssuance",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "affectAccounting", type = "Boolean", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateItemIssuance {}

    /**
     * Update ItemIssuance
     */
    @Service(
        name = "updateItemIssuance",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml",
        invoke = "updateItemIssuance",
        description = "Update ItemIssuance",
        defaultEntityName = "ItemIssuance",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateItemIssuance {}

    /**
     * Delete ItemIssuance
     */
    @Service(
        name = "deleteItemIssuance",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml",
        invoke = "deleteItemIssuance",
        description = "Delete ItemIssuance",
        defaultEntityName = "ItemIssuance",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteItemIssuance {}

    /**
     * Create ItemIssuanceRole
     */
    @Service(
        name = "createItemIssuanceRole",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml",
        invoke = "createItemIssuanceRole",
        description = "Create ItemIssuanceRole",
        defaultEntityName = "ItemIssuanceRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "Shipment", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateItemIssuanceRole {}

    /**
     * Delete ItemIssuanceRole
     */
    @Service(
        name = "deleteItemIssuanceRole",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml",
        invoke = "deleteItemIssuanceRole",
        description = "Delete ItemIssuanceRole",
        defaultEntityName = "ItemIssuanceRole",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "Shipment", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteItemIssuanceRole {}

    /**
     * Issue an OrderItem to a Shipment - only for non-sales orders
     */
    @Service(
        name = "issueOrderItemToShipment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml",
        invoke = "issueOrderItemToShipment",
        description = "Issue an OrderItem to a Shipment - only for non-sales orders",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "Shipment", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "OrderItemShipGroupAssoc", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "shipmentItemSeqId", type = "String", mode = "OUT")
        }
    )
    public interface IssueOrderItemToShipment {}

    /**
     * Add an OrderItemShipGrpInvRes to a Shipment - only for sales orders
     */
    @Service(
        name = "issueOrderItemShipGrpInvResToShipment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml",
        invoke = "issueOrderItemShipGrpInvResToShipment",
        description = "Add an OrderItemShipGrpInvRes to a Shipment - only for sales orders",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "Shipment", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "OrderItemShipGrpInvRes", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "eventDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentItemSeqId", type = "String", mode = "OUT"),
            @Attribute(name = "itemIssuanceId", type = "String", mode = "OUT")
        }
    )
    public interface IssueOrderItemShipGrpInvResToShipment {}

    /**
     * Issue an InventoryItem to a FixedAssetMaint - for conversion to use as supples/parts
     */
    @Service(
        name = "issueInventoryItemToFixedAssetMaint",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml",
        invoke = "issueInventoryItemToFixedAssetMaint",
        description = "Issue an InventoryItem to a FixedAssetMaint - for conversion to use as supples/parts",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "InventoryItem", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "FixedAssetMaint", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "itemIssuanceId", type = "String", mode = "OUT")
        }
    )
    public interface IssueInventoryItemToFixedAssetMaint {}

    /**
     * Return InventoryItem Issued to a FixedAssetMaint - for conversion to use as supples/parts
     */
    @Service(
        name = "returnInventoryItemIssuedToFixedAssetMaint",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml",
        invoke = "returnInventoryItemIssuedToFixedAssetMaint",
        description = "Return InventoryItem Issued to a FixedAssetMaint - for conversion to use as supples/parts",
        auth = "true",
        attributes = {
            @Attribute(name = "itemIssuanceId", type = "String", mode = "IN")
        }
    )
    public interface ReturnInventoryItemIssuedToFixedAssetMaint {}

    /**
     * Cancel an ItemIssuance from Sales Shipment
     */
    @Service(
        name = "cancelOrderItemIssuanceFromSalesShipment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml",
        invoke = "cancelOrderItemIssuanceFromSalesShipment",
        description = "Cancel an ItemIssuance from Sales Shipment",
        auth = "true",
        attributes = {
            @Attribute(name = "itemIssuanceId", type = "String", mode = "IN"),
            @Attribute(name = "cancelQuantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "canceledQuantity", type = "BigDecimal", mode = "OUT")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface CancelOrderItemIssuanceFromSalesShipment {}

    /**
     * Issue an InventoryItem to a Shipment
     */
    @Service(
        name = "issueInventoryItemToShipment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/issuance/IssuanceServices.xml",
        invoke = "issueInventoryItemToShipment",
        description = "Issue an InventoryItem to a Shipment",
        auth = "true",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "totalIssuedQty", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "itemIssuanceId", type = "String", mode = "OUT")
        }
    )
    public interface IssueInventoryItemToShipment {}

    /**
     * Verify Single Item
     */
    @Service(
        name = "verifySingleItem",
        engine = "java",
        location = "org.ofbiz.shipment.verify.VerifyPickServices",
        invoke = "verifySingleItem",
        description = "Verify Single Item",
        auth = "true",
        attributes = {
            @Attribute(name = "verifyPickSession", type = "org.ofbiz.shipment.verify.VerifyPickSession", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface VerifySingleItem {}

    /**
     * Verify Multiple Items
     */
    @Service(
        name = "verifyBulkItem",
        engine = "java",
        location = "org.ofbiz.shipment.verify.VerifyPickServices",
        invoke = "verifyBulkItem",
        description = "Verify Multiple Items",
        auth = "true",
        attributes = {
            @Attribute(name = "verifyPickSession", type = "org.ofbiz.shipment.verify.VerifyPickSession", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pickerPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "prd", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "geo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "qty", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "ite", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface VerifyBulkItem {}

    /**
     * Clear the current picking session
     */
    @Service(
        name = "cancelAllRows",
        engine = "java",
        location = "org.ofbiz.shipment.verify.VerifyPickServices",
        invoke = "cancelAllRows",
        description = "Clear the current picking session",
        auth = "true",
        attributes = {
            @Attribute(name = "verifyPickSession", type = "org.ofbiz.shipment.verify.VerifyPickSession", mode = "IN")
        }
    )
    public interface CancelAllRows {}

    /**
     * Complete the picking and set the shipment to PICKED
     */
    @Service(
        name = "completeVerifiedPick",
        engine = "java",
        location = "org.ofbiz.shipment.verify.VerifyPickServices",
        invoke = "completeVerifiedPick",
        description = "Complete the picking and set the shipment to PICKED",
        auth = "true",
        attributes = {
            @Attribute(name = "verifyPickSession", type = "org.ofbiz.shipment.verify.VerifyPickSession", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "INOUT"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pickerPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CompleteVerifiedPick {}

    /**
     * Set the weight, dimensions/shipmentBoxType of package in SESSION
     */
    @Service(
        name = "setPackageInfo",
        engine = "java",
        location = "org.ofbiz.shipment.weightPackage.WeightPackageServices",
        invoke = "setPackageInfo",
        description = "Set the weight, dimensions/shipmentBoxType of package in SESSION",
        auth = "true",
        attributes = {
            @Attribute(name = "weightPackageSession", type = "org.ofbiz.shipment.weightPackage.WeightPackageSession", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "packageWeight", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "packageLength", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "packageWidth", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "packageHeight", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentBoxTypeId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetPackageInfo {}

    /**
     * Update the weight, dimensions/shipmentBoxType of package
     */
    @Service(
        name = "updatePackedLine",
        engine = "java",
        location = "org.ofbiz.shipment.weightPackage.WeightPackageServices",
        invoke = "updatePackedLine",
        description = "Update the weight, dimensions/shipmentBoxType of package",
        auth = "true",
        attributes = {
            @Attribute(name = "weightPackageSession", type = "org.ofbiz.shipment.weightPackage.WeightPackageSession", mode = "IN"),
            @Attribute(name = "packageWeight", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "packageLength", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "packageWidth", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "packageHeight", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentBoxTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "weightPackageSeqId", type = "Integer", mode = "IN")
        }
    )
    public interface UpdatePackedLine {}

    /**
     * Delete the weight, dimensions/shipmentBoxType of package
     */
    @Service(
        name = "deletePackedLine",
        engine = "java",
        location = "org.ofbiz.shipment.weightPackage.WeightPackageServices",
        invoke = "deletePackedLine",
        description = "Delete the weight, dimensions/shipmentBoxType of package",
        auth = "true",
        attributes = {
            @Attribute(name = "weightPackageSession", type = "org.ofbiz.shipment.weightPackage.WeightPackageSession", mode = "IN"),
            @Attribute(name = "weightPackageSeqId", type = "Integer", mode = "IN")
        }
    )
    public interface DeletePackedLine {}

    /**
     * Complete the packging and set the shipment to packed
     */
    @Service(
        name = "completePackage",
        engine = "java",
        location = "org.ofbiz.shipment.weightPackage.WeightPackageServices",
        invoke = "completePackage",
        description = "Complete the packging and set the shipment to packed",
        auth = "true",
        attributes = {
            @Attribute(name = "weightPackageSession", type = "org.ofbiz.shipment.weightPackage.WeightPackageSession", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "dimensionUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "weightUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "estimatedShippingCost", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "newEstimatedShippingCost", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "showWarningForm", type = "Boolean", mode = "OUT", optional = "true")
        }
    )
    public interface CompletePackage {}

    /**
     * Complete the packaging set the shipment to packed
     */
    @Service(
        name = "completeShipment",
        engine = "java",
        location = "org.ofbiz.shipment.weightPackage.WeightPackageServices",
        invoke = "completeShipment",
        description = "Complete the packaging set the shipment to packed",
        auth = "true",
        attributes = {
            @Attribute(name = "weightPackageSession", type = "org.ofbiz.shipment.weightPackage.WeightPackageSession", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CompleteShipment {}

    /**
     * Save the package(s) information in ShipmentPackage entity from session
     */
    @Service(
        name = "savePackagesInfo",
        engine = "java",
        location = "org.ofbiz.shipment.weightPackage.WeightPackageServices",
        invoke = "savePackagesInfo",
        description = "Save the package(s) information in ShipmentPackage entity from session",
        auth = "true",
        attributes = {
            @Attribute(name = "weightPackageSession", type = "org.ofbiz.shipment.weightPackage.WeightPackageSession", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SavePackagesInfo {}

    /**
     * Pack Single Item
     */
    @Service(
        name = "packSingleItem",
        engine = "java",
        location = "org.ofbiz.shipment.packing.PackingServices",
        invoke = "addPackLine",
        description = "Pack Single Item",
        auth = "true",
        attributes = {
            @Attribute(name = "packingSession", type = "org.ofbiz.shipment.packing.PackingSession", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "weight", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "packageSeq", type = "Integer", mode = "IN"),
            @Attribute(name = "pickerPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "handlingInstructions", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PackSingleItem {}

    /**
     * Pack Multiple Items
     */
    @Service(
        name = "packBulkItems",
        engine = "java",
        location = "org.ofbiz.shipment.packing.PackingServices",
        invoke = "packBulk",
        description = "Pack Multiple Items",
        auth = "true",
        attributes = {
            @Attribute(name = "packingSession", type = "org.ofbiz.shipment.packing.PackingSession", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "updateQuantity", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "pickerPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "handlingInstructions", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "nextPackageSeq", type = "Integer", mode = "INOUT", optional = "true"),
            @Attribute(name = "prd", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "qty", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pkg", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "ite", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "wgt", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "numPackages", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface PackBulkItems {}

    /**
     * Increments the next package sequence
     */
    @Service(
        name = "setNextPackageSeq",
        engine = "java",
        location = "org.ofbiz.shipment.packing.PackingServices",
        invoke = "incrementPackageSeq",
        description = "Increments the next package sequence",
        auth = "true",
        attributes = {
            @Attribute(name = "packingSession", type = "org.ofbiz.shipment.packing.PackingSession", mode = "IN"),
            @Attribute(name = "nextPackageSeq", type = "Integer", mode = "OUT")
        }
    )
    public interface SetNextPackageSeq {}

    /**
     * Totals package weights and calls the calcShipmentCostEstimate via the PackingSession
     */
    @Service(
        name = "calcPackSessionAdditionalShippingCharge",
        engine = "java",
        location = "org.ofbiz.shipment.packing.PackingServices",
        invoke = "calcPackSessionAdditionalShippingCharge",
        description = "Totals package weights and calls the calcShipmentCostEstimate via the PackingSession",
        auth = "true",
        attributes = {
            @Attribute(name = "packingSession", type = "org.ofbiz.shipment.packing.PackingSession", mode = "IN"),
            @Attribute(name = "packageWeights", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "weightUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shippingContactMechId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentMethodTypeId", type = "String", mode = "IN"),
            @Attribute(name = "carrierPartyId", type = "String", mode = "IN"),
            @Attribute(name = "carrierRoleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "additionalShippingCharge", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface CalcPackSessionAdditionalShippingCharge {}

    /**
     * Clear the current packing session
     */
    @Service(
        name = "clearPackAll",
        engine = "java",
        location = "org.ofbiz.shipment.packing.PackingServices",
        invoke = "clearPackAll",
        description = "Clear the current packing session",
        auth = "true",
        attributes = {
            @Attribute(name = "packingSession", type = "org.ofbiz.shipment.packing.PackingSession", mode = "IN")
        }
    )
    public interface ClearPackAll {}

    /**
     * Clears the last package in the packing session
     */
    @Service(
        name = "clearLastPackage",
        engine = "java",
        location = "org.ofbiz.shipment.packing.PackingServices",
        invoke = "clearLastPackage",
        description = "Clears the last package in the packing session",
        auth = "true",
        attributes = {
            @Attribute(name = "packingSession", type = "org.ofbiz.shipment.packing.PackingSession", mode = "IN"),
            @Attribute(name = "nextPackageSeq", type = "Integer", mode = "OUT")
        }
    )
    public interface ClearLastPackage {}

    /**
     * Clear a single line from the current packing session
     */
    @Service(
        name = "clearPackLine",
        engine = "java",
        location = "org.ofbiz.shipment.packing.PackingServices",
        invoke = "clearPackLine",
        description = "Clear a single line from the current packing session",
        auth = "true",
        attributes = {
            @Attribute(name = "packingSession", type = "org.ofbiz.shipment.packing.PackingSession", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "packageSeqId", type = "Integer", mode = "IN")
        }
    )
    public interface ClearPackLine {}

    /**
     * Complete the packaging set the shipment to PACKED
     */
    @Service(
        name = "completePack",
        engine = "java",
        location = "org.ofbiz.shipment.packing.PackingServices",
        invoke = "completePack",
        description = "Complete the packaging set the shipment to PACKED",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "packingSession", type = "org.ofbiz.shipment.packing.PackingSession", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "INOUT"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "handlingInstructions", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pickerPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "additionalShippingCharge", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "forceComplete", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "packageWeights", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "dimensionUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "weightUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentId", type = "String", mode = "INOUT"),
            @Attribute(name = "boxTypes", type = "Map", mode = "IN", optional = "true")
        }
    )
    public interface CompletePack {}

    /**
     * Add an OrderShipment and a ShipmentItem - only for sales orders
     */
    @Service(
        name = "addOrderShipmentToShipment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "addOrderShipmentToShipment",
        description = "Add an OrderShipment and a ShipmentItem - only for sales orders",
        defaultEntityName = "OrderShipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "shipmentItemSeqId", mode = "INOUT", optional = "true")
        }
    )
    public interface AddOrderShipmentToShipment {}

    /**
     * Delete an OrderShipment and updates the ShipmentItem
     */
    @Service(
        name = "removeOrderShipmentFromShipment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "removeOrderShipmentFromShipment",
        description = "Delete an OrderShipment and updates the ShipmentItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderShipment", mode = "IN", include = "pk")
        }
    )
    public interface RemoveOrderShipmentFromShipment {}

    /**
     * get the order item quantity still not put in shipments
     */
    @Service(
        name = "getQuantityForShipment",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "getQuantityForShipment",
        description = "get the order item quantity still not put in shipments",
        defaultEntityName = "OrderItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "remainingQuantity", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetQuantityForShipment {}

    /**
     * ShipmentReceipt Interface
     */
    @Service(
        name = "interfaceShipmentReceipt",
        engine = "interface",
        description = "ShipmentReceipt Interface",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentReceipt", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "inventoryItemDetailSeqId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "inventoryItemId", optional = "false"),
            @OverrideAttribute(name = "productId", optional = "false"),
            @OverrideAttribute(name = "quantityAccepted", optional = "false"),
            @OverrideAttribute(name = "quantityRejected", optional = "false")
        }
    )
    public interface InterfaceShipmentReceipt {}

    /**
     * Creates a ShipmentReceipt Record
     */
    @Service(
        name = "createShipmentReceipt",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/receipt/ShipmentReceiptServices.xml",
        invoke = "createShipmentReceipt",
        description = "Creates a ShipmentReceipt Record",
        auth = "true",
        implemented = {@Implements(service = "interfaceShipmentReceipt")},
        attributes = {
            @Attribute(name = "receiptId", type = "String", mode = "OUT"),
            @Attribute(name = "affectAccounting", type = "Boolean", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateShipmentReceipt {}

    /**
     *            Whenever a ShipmentReceipt is generated, check the Shipment associated           with it to see if all items were received. If so, change its status to           PURCH_SHIP_RECEIVED. The check is accomplished by counting the           products shipped (from ShipmentAndItem) and matching them with the           products received (from ShipmentReceipt).         
     */
    @Service(
        name = "updatePurchaseShipmentFromReceipt",
        engine = "java",
        location = "org.ofbiz.shipment.shipment.ShipmentServices",
        invoke = "updatePurchaseShipmentFromReceipt",
        description = "\n          Whenever a ShipmentReceipt is generated, check the Shipment associated\n          with it to see if all items were received. If so, change its status to\n          PURCH_SHIP_RECEIVED. The check is accomplished by counting the\n          products shipped (from ShipmentAndItem) and matching them with the\n          products received (from ShipmentReceipt).\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN")
        }
    )
    public interface UpdatePurchaseShipmentFromReceipt {}

    /**
     * Receive Inventory In Warehouse
     */
    @Service(
        name = "receiveInventoryProduct",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/receipt/ShipmentReceiptServices.xml",
        invoke = "receiveInventoryProduct",
        description = "Receive Inventory In Warehouse",
        auth = "true",
        transactionTimeout = "600",
        entityAttributes = {
            @EntityAttributes(entityName = "InventoryItem", mode = "IN", include = "nonpk", optional = "true", excludeFields = {"availableToPromiseTotal", "quantityOnHandTotal"}),
            @EntityAttributes(entityName = "InventoryItemDetail", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "ShipmentReceipt", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "inventoryItemDetailSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "priorityOrderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "priorityOrderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currentInventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "orderCurrencyUnitPrice", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "quantityAccepted", optional = "false"),
            @OverrideAttribute(name = "quantityRejected", optional = "false"),
            @OverrideAttribute(name = "inventoryItemTypeId", optional = "false"),
            @OverrideAttribute(name = "productId", optional = "false"),
            @OverrideAttribute(name = "facilityId", optional = "false")
        }
    )
    public interface ReceiveInventoryProduct {}

    /**
     * Issues order item quantity specified to the shipment, then receives inventory for that item and quantity
     */
    @Service(
        name = "issueOrderItemToShipmentAndReceiveAgainstPO",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/receipt/ShipmentReceiptServices.xml",
        invoke = "issueOrderItemToShipmentAndReceiveAgainstPO",
        description = "Issues order item quantity specified to the shipment, then receives inventory for that item and quantity",
        auth = "true",
        transactionTimeout = "600",
        implemented = {@Implements(service = "issueOrderItemToShipment"), @Implements(service = "receiveInventoryProduct")}
    )
    public interface IssueOrderItemToShipmentAndReceiveAgainstPO {}

    @Service(
        name = "quickReceiveReturn",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/receipt/ShipmentReceiptServices.xml",
        invoke = "quickReceiveReturn",
        auth = "true",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "inventoryItemTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface QuickReceiveReturn {}

    /**
     * Interface for ShipmentReceiptRole
     */
    @Service(
        name = "interfaceShipmentReceiptRole",
        engine = "interface",
        description = "Interface for ShipmentReceiptRole",
        attributes = {
            @Attribute(name = "receiptId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN")
        }
    )
    public interface InterfaceShipmentReceiptRole {}

    /**
     * Create a ShipmentReceipt Role entry
     */
    @Service(
        name = "createShipmentReceiptRole",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/receipt/ShipmentReceiptServices.xml",
        invoke = "createShipmentReceiptRole",
        description = "Create a ShipmentReceipt Role entry",
        auth = "true",
        implemented = {@Implements(service = "interfaceShipmentReceiptRole")},
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateShipmentReceiptRole {}

    /**
     * Remove a ShipmentReceipt Role entry
     */
    @Service(
        name = "removeShipmentReceiptRole",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/receipt/ShipmentReceiptServices.xml",
        invoke = "removeShipmentReceiptRole",
        description = "Remove a ShipmentReceipt Role entry",
        auth = "true",
        implemented = {@Implements(service = "interfaceShipmentReceiptRole")},
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface RemoveShipmentReceiptRole {}

    /**
     * Create Shipment Estimate
     */
    @Service(
        name = "createShipmentEstimate",
        engine = "java",
        location = "org.ofbiz.shipment.shipment.ShipmentServices",
        invoke = "createShipmentEstimate",
        description = "Create Shipment Estimate",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreShipMethId", type = "String", mode = "IN"),
            @Attribute(name = "toGeo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromGeo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "flatPercent", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "flatPrice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "flatItemPrice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "shippingPricePercent", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "productFeatureGroupId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "featurePercent", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "featurePrice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "oversizeUnit", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "oversizePrice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "weightBreakId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "wmin", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "wmax", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "wprice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "wuom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityBreakId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "qmin", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "qmax", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "qprice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "quom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "priceBreakId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "pmin", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "pmax", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "pprice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "puom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentCostEstimateId", type = "String", mode = "OUT")
        }
    )
    public interface CreateShipmentEstimate {}

    /**
     * Remove Shipment Estimate
     */
    @Service(
        name = "removeShipmentEstimate",
        engine = "java",
        location = "org.ofbiz.shipment.shipment.ShipmentServices",
        invoke = "removeShipmentEstimate",
        description = "Remove Shipment Estimate",
        auth = "true",
        attributes = {
            @Attribute(name = "shipmentCostEstimateId", type = "String", mode = "IN")
        }
    )
    public interface RemoveShipmentEstimate {}

    /**
     * Interface for shipment estimate calc service
     */
    @Service(
        name = "calcShipmentEstimateInterface",
        engine = "interface",
        description = "Interface for shipment estimate calc service",
        attributes = {
            @Attribute(name = "serviceConfigProps", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "initialEstimateAmt", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "shippingContactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shippingOriginContactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shippingPostalCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shippingCountryCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentMethodTypeId", type = "String", mode = "IN"),
            @Attribute(name = "carrierPartyId", type = "String", mode = "IN"),
            @Attribute(name = "carrierRoleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "productStoreShipMethId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "shippableItemInfo", type = "List", mode = "IN"),
            @Attribute(name = "shippableWeight", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "shippableQuantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "shippableTotal", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentCustomMethodId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentGatewayConfigId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shippingEstimateAmount", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface CalcShipmentEstimateInterface {}

    /**
     * Generic Shipment Cost Estimate Calc Service - Use ShipmentCostEstimate Entities
     */
    @Service(
        name = "calcShipmentCostEstimate",
        engine = "java",
        location = "org.ofbiz.shipment.shipment.ShipmentServices",
        invoke = "calcShipmentCostEstimate",
        description = "Generic Shipment Cost Estimate Calc Service - Use ShipmentCostEstimate Entities",
        useTransaction = "false",
        implemented = {@Implements(service = "calcShipmentEstimateInterface")},
        overrideAttributes = {
            @OverrideAttribute(name = "shippingEstimateAmount", optional = "true")
        }
    )
    public interface CalcShipmentCostEstimate {}

    /**
     * Cancel Received Items against a purchase order if received something incorrectly
     */
    @Service(
        name = "cancelReceivedItems",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/receipt/ShipmentReceiptServices.xml",
        invoke = "cancelReceivedItems",
        description = "Cancel Received Items against a purchase order if received something incorrectly",
        auth = "true",
        attributes = {
            @Attribute(name = "receiptId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CancelReceivedItems {}

    /**
     * Create a QuantityBreak
     */
    @Service(
        name = "createQuantityBreak",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "createQuantityBreak",
        description = "Create a QuantityBreak",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "QuantityBreak", mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "CREATE")
    )
    public interface CreateQuantityBreak {}

    /**
     * Update a QuantityBreak
     */
    @Service(
        name = "updateQuantityBreak",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "updateQuantityBreak",
        description = "Update a QuantityBreak",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "QuantityBreak", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "QuantityBreak", mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "UPDATE")
    )
    public interface UpdateQuantityBreak {}

    /**
     * Delete a QuantityBreak
     */
    @Service(
        name = "deleteQuantityBreak",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "deleteQuantityBreak",
        description = "Delete a QuantityBreak",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "QuantityBreak", mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "facilityGenericPermission", mainAction = "DELETE")
    )
    public interface DeleteQuantityBreak {}

    /**
     * Calculates the total value of a shipment package by totalling the results of the getOrderItemInvoicedAmountAndQuantity             service for the orderItem related to each ShipmentPackageContent, prorated by the quantity of the orderItem issued to the             ShipmentPackageContent. Value is converted according to the incoming currencyUomId.
     */
    @Service(
        name = "getShipmentPackageValueFromOrders",
        engine = "java",
        location = "org.ofbiz.shipment.shipment.ShipmentServices",
        invoke = "getShipmentPackageValueFromOrders",
        description = "Calculates the total value of a shipment package by totalling the results of the getOrderItemInvoicedAmountAndQuantity\n            service for the orderItem related to each ShipmentPackageContent, prorated by the quantity of the orderItem issued to the\n            ShipmentPackageContent. Value is converted according to the incoming currencyUomId.",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentPackageSeqId", type = "String", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN"),
            @Attribute(name = "packageValue", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface GetShipmentPackageValueFromOrders {}

    @Service(
        name = "issueSerializedInvToShipmentPackageAndSetTracking",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "issueSerializedInvToShipmentPackageAndSetTracking",
        auth = "true",
        attributes = {
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "reservedDatetime", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "requireInventory", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "reserveOrderEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sequenceId", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "serialNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "trackingNum", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "originFacilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quantityNotReserved", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "promisedDatetime", type = "Timestamp", mode = "IN"),
            @Attribute(name = "shipmentPackageSeqId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface IssueSerializedInvToShipmentPackageAndSetTracking {}

    /**
     * Move a shipment into Packed status and then to Shipped status
     */
    @Service(
        name = "setShipmentStatusPackedAndShipped",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/shipment/ShipmentServices.xml",
        invoke = "setShipmentStatusPackedAndShipped",
        description = "Move a shipment into Packed status and then to Shipped status",
        auth = "true",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN")
        }
    )
    public interface SetShipmentStatusPackedAndShipped {}

    /**
     * Send a notification on Shipment Complete
     */
    @Service(
        name = "sendShipmentCompleteNotification",
        engine = "java",
        location = "org.ofbiz.shipment.shipment.ShipmentServices",
        invoke = "sendShipmentCompleteNotification",
        description = "Send a notification on Shipment Complete",
        auth = "true",
        requireNewTransaction = "true",
        maxRetry = "3",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN"),
            @Attribute(name = "sendTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "screenUri", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "body", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "subject", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "messageWrapper", type = "org.ofbiz.service.mail.MimeMessageWrapper", mode = "OUT", optional = "true"),
            @Attribute(name = "communicationEventId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SendShipmentCompleteNotification {}

    /**
     * Update issuance, shipment and order items if quantity received is higher than quantity on purchase order
     */
    @Service(
        name = "updateIssuanceShipmentAndPoOnReceiveInventory",
        engine = "simple",
        location = "component://product/script/org/ofbiz/shipment/receipt/ShipmentReceiptServices.xml",
        invoke = "updateIssuanceShipmentAndPoOnReceiveInventory",
        description = "Update issuance, shipment and order items if quantity received is higher than quantity on purchase order",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "quantityAccepted", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "shipmentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "unitCost", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderCurrencyUnitPrice", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateIssuanceShipmentAndPoOnReceiveInventory {}

    /**
     * Create a new Carrier Shipment Box Type Record
     */
    @Service(
        name = "createCarrierShipmentBoxType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a new Carrier Shipment Box Type Record",
        defaultEntityName = "CarrierShipmentBoxType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateCarrierShipmentBoxType {}

    /**
     * Update a Carrier Shipment Box Type
     */
    @Service(
        name = "updateCarrierShipmentBoxType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Carrier Shipment Box Type",
        defaultEntityName = "CarrierShipmentBoxType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateCarrierShipmentBoxType {}

    /**
     * Delete an existing Carrier Shipment Box Type Record
     */
    @Service(
        name = "deleteCarrierShipmentBoxType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete an existing Carrier Shipment Box Type Record",
        defaultEntityName = "CarrierShipmentBoxType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCarrierShipmentBoxType {}

    /**
     * Create a Delivery record
     */
    @Service(
        name = "createDelivery",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Delivery record",
        defaultEntityName = "Delivery",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateDelivery {}

    /**
     * Update a Delivery record
     */
    @Service(
        name = "updateDelivery",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Delivery record",
        defaultEntityName = "Delivery",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateDelivery {}

    /**
     * Delete a Delivery record
     */
    @Service(
        name = "deleteDelivery",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Delivery record",
        defaultEntityName = "Delivery",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteDelivery {}

    /**
     * Create a RejectionReason record
     */
    @Service(
        name = "createRejectionReason",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a RejectionReason record",
        defaultEntityName = "RejectionReason",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateRejectionReason {}

    /**
     * Update a RejectionReason record
     */
    @Service(
        name = "updateRejectionReason",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a RejectionReason record",
        defaultEntityName = "RejectionReason",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateRejectionReason {}

    /**
     * Delete a RejectionReason record
     */
    @Service(
        name = "deleteRejectionReason",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a RejectionReason record",
        defaultEntityName = "RejectionReason",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteRejectionReason {}

    /**
     * Create a ShipmentItemFeature
     */
    @Service(
        name = "createShipmentItemFeature",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ShipmentItemFeature",
        defaultEntityName = "ShipmentItemFeature",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateShipmentItemFeature {}

    /**
     * Delete a ShipmentItemFeature
     */
    @Service(
        name = "deleteShipmentItemFeature",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ShipmentItemFeature",
        defaultEntityName = "ShipmentItemFeature",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShipmentItemFeature {}

    /**
     * Create a ShipmentAttribute record
     */
    @Service(
        name = "createShipmentAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ShipmentAttribute record",
        defaultEntityName = "ShipmentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateShipmentAttribute {}

    /**
     * Update a ShipmentAttribute record
     */
    @Service(
        name = "updateShipmentAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ShipmentAttribute record",
        defaultEntityName = "ShipmentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentAttribute {}

    /**
     * Delete a ShipmentAttribute record
     */
    @Service(
        name = "deleteShipmentAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ShipmentAttribute record",
        defaultEntityName = "ShipmentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShipmentAttribute {}

    /**
     * Create a ShipmentBoxType record
     */
    @Service(
        name = "createShipmentBoxType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ShipmentBoxType record",
        defaultEntityName = "ShipmentBoxType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateShipmentBoxType {}

    /**
     * Update a ShipmentBoxType record
     */
    @Service(
        name = "updateShipmentBoxType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ShipmentBoxType record",
        defaultEntityName = "ShipmentBoxType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentBoxType {}

    /**
     * Delete a ShipmentBoxType record
     */
    @Service(
        name = "deleteShipmentBoxType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ShipmentBoxType record",
        defaultEntityName = "ShipmentBoxType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShipmentBoxType {}

    /**
     * Create a ShipmentContactMechType record
     */
    @Service(
        name = "createShipmentContactMechType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ShipmentContactMechType record",
        defaultEntityName = "ShipmentContactMechType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateShipmentContactMechType {}

    /**
     * Update a ShipmentContactMechType record
     */
    @Service(
        name = "updateShipmentContactMechType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ShipmentContactMechType record",
        defaultEntityName = "ShipmentContactMechType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentContactMechType {}

    /**
     * Delete a ShipmentContactMechType record
     */
    @Service(
        name = "deleteShipmentContactMechType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ShipmentContactMechType record",
        defaultEntityName = "ShipmentContactMechType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShipmentContactMechType {}

    /**
     * Create a ShipmentCostEstimate record
     */
    @Service(
        name = "createShipmentCostEstimate",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ShipmentCostEstimate record",
        defaultEntityName = "ShipmentCostEstimate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateShipmentCostEstimate {}

    /**
     * Update a ShipmentCostEstimate record
     */
    @Service(
        name = "updateShipmentCostEstimate",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ShipmentCostEstimate record",
        defaultEntityName = "ShipmentCostEstimate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentCostEstimate {}

    /**
     * Delete a ShipmentCostEstimate record
     */
    @Service(
        name = "deleteShipmentCostEstimate",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ShipmentCostEstimate record",
        defaultEntityName = "ShipmentCostEstimate",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShipmentCostEstimate {}

    /**
     * Update a ShipmentReceipt record
     */
    @Service(
        name = "updateShipmentReceipt",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ShipmentReceipt record",
        defaultEntityName = "ShipmentReceipt",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentReceipt {}

    /**
     * Delete a ShipmentReceipt record
     */
    @Service(
        name = "deleteShipmentReceipt",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ShipmentReceipt record",
        defaultEntityName = "ShipmentReceipt",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShipmentReceipt {}

    /**
     * Create a ShipmentTypeAttr Record
     */
    @Service(
        name = "createShipmentTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ShipmentTypeAttr Record",
        defaultEntityName = "ShipmentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateShipmentTypeAttr {}

    /**
     * Update a ShipmentTypeAttr Record
     */
    @Service(
        name = "updateShipmentTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ShipmentTypeAttr Record",
        defaultEntityName = "ShipmentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentTypeAttr {}

    /**
     * Delete a ShipmentTypeAttr Record
     */
    @Service(
        name = "deleteShipmentTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ShipmentTypeAttr Record",
        defaultEntityName = "ShipmentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShipmentTypeAttr {}

    /**
     * Create a ShippingDocument
     */
    @Service(
        name = "createShippingDocument",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ShippingDocument",
        defaultEntityName = "ShippingDocument",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateShippingDocument {}

    /**
     * Update a ShippingDocument
     */
    @Service(
        name = "updateShippingDocument",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ShippingDocument",
        defaultEntityName = "ShippingDocument",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShippingDocument {}

    /**
     * Delete a ShippingDocument
     */
    @Service(
        name = "deleteShippingDocument",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ShippingDocument",
        defaultEntityName = "ShippingDocument",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShippingDocument {}

    /**
     * Create a ShipmentItemBilling
     */
    @Service(
        name = "createShipmentItemBilling",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ShipmentItemBilling",
        defaultEntityName = "ShipmentItemBilling",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateShipmentItemBilling {}

    /**
     * Update a ShipmentItemBilling
     */
    @Service(
        name = "updateShipmentItemBilling",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ShipmentItemBilling",
        defaultEntityName = "ShipmentItemBilling",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateShipmentItemBilling {}

    /**
     * Delete a ShipmentItemBilling
     */
    @Service(
        name = "deleteShipmentItemBilling",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ShipmentItemBilling",
        defaultEntityName = "ShipmentItemBilling",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteShipmentItemBilling {}

}

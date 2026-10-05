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
package com.ilscipio.scipio.product.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Auto-generated annotation-based entity definitions.
 *
 * <p>Generated from entitymodel.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ShipmentEntities {

    /**
     * Item Issuance
     */
    @Entity(
        name = "ItemIssuance",
        packageName = "org.ofbiz.shipment.issuance",
        title = "Item Issuance",
        fields = {
            @Field(name = "itemIssuanceId", type = "id-ne"),
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "shipGroupSeqId", type = "id-ne"),
            @Field(name = "inventoryItemId", type = "id"),
            @Field(name = "shipmentId", type = "id"),
            @Field(name = "shipmentItemSeqId", type = "id"),
            @Field(name = "fixedAssetId", type = "id"),
            @Field(name = "maintHistSeqId", type = "id"),
            @Field(name = "issuedDateTime", type = "date-time"),
            @Field(name = "issuedByUserLoginId", type = "id-vlong"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "cancelQuantity", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "itemIssuanceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "ITEM_ISS_INVITM",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGrpInvRes",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId"),
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Shipment",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentItem",
                fkName = "ITEM_ISS_SHITM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAssetMaint",
                fkName = "ITEM_ISS_FAMNT",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId"),
                    @KeyMap(fieldName = "maintHistSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderHeader",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "ITEM_ISS_ORITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "IssuedBy",
                fkName = "ITEM_ISS_IBUL",
                keyMaps = {
                    @KeyMap(fieldName = "issuedByUserLoginId", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface ItemIssuanceEntity {}

    /**
     * Item Issuance Role
     */
    @Entity(
        name = "ItemIssuanceRole",
        packageName = "org.ofbiz.shipment.issuance",
        title = "Item Issuance Role",
        fields = {
            @Field(name = "itemIssuanceId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "itemIssuanceId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ItemIssuance",
                fkName = "ITEM_ISSRL_ITMIS",
                keyMaps = {
                    @KeyMap(fieldName = "itemIssuanceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "ITEM_ISSRL_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "ITEM_ISSRL_PTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface ItemIssuanceRoleEntity {}

    /**
     * Picklist
     */
    @Entity(
        name = "Picklist",
        packageName = "org.ofbiz.shipment.picklist",
        title = "Picklist",
        fields = {
            @Field(name = "picklistId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "shipmentMethodTypeId", type = "id-ne"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "picklistDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "picklistId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "PICKLST_FLTY",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentMethodType",
                fkName = "PICKLST_SMTP",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "PICKLST_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "StatusValidChangeToDetail",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface PicklistEntity {}

    /**
     * Picklist
     */
    @Entity(
        name = "PicklistBin",
        packageName = "org.ofbiz.shipment.picklist",
        title = "Picklist",
        fields = {
            @Field(name = "picklistBinId", type = "id-ne"),
            @Field(name = "picklistId", type = "id-ne"),
            @Field(name = "binLocationNumber", type = "numeric"),
            @Field(name = "primaryOrderId", type = "id"),
            @Field(name = "primaryShipGroupSeqId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "picklistBinId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Picklist",
                fkName = "PCKLST_BIN_PKLT",
                keyMaps = {
                    @KeyMap(fieldName = "picklistId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItemShipGroup",
                title = "Primary",
                fkName = "PCKLST_BIN_OISG",
                keyMaps = {
                    @KeyMap(fieldName = "primaryOrderId", relFieldName = "orderId"),
                    @KeyMap(fieldName = "primaryShipGroupSeqId", relFieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderHeader",
                title = "Primary",
                keyMaps = {
                    @KeyMap(fieldName = "primaryOrderId", relFieldName = "orderId")
                }
            )
        }
    )
    public interface PicklistBinEntity {}

    /**
     * Picklist
     */
    @Entity(
        name = "PicklistItem",
        packageName = "org.ofbiz.shipment.picklist",
        title = "Picklist",
        fields = {
            @Field(name = "picklistBinId", type = "id-ne"),
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "shipGroupSeqId", type = "id-ne"),
            @Field(name = "inventoryItemId", type = "id-ne"),
            @Field(name = "itemStatusId", type = "id-ne"),
            @Field(name = "quantity", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "picklistBinId"),
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "shipGroupSeqId"),
            @PrimaryKey(field = "inventoryItemId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PicklistBin",
                fkName = "PCKLST_ITM_BIN",
                keyMaps = {
                    @KeyMap(fieldName = "picklistBinId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItemShipGroup",
                fkName = "PCKLST_ITM_OISG",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "PCKLST_ITM_ODIT",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderHeader",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "PICKLST_ITM_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "itemStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "PCKLST_ITM_INV",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "InventoryItemAndLocation",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGrpInvRes",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId"),
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ItemIssuance",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId"),
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface PicklistItemEntity {}

    /**
     * Picklist Role
     */
    @Entity(
        name = "PicklistRole",
        packageName = "org.ofbiz.shipment.picklist",
        title = "Picklist Role",
        fields = {
            @Field(name = "picklistId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "picklistId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Picklist",
                fkName = "PCKLST_RLE_PKLT",
                keyMaps = {
                    @KeyMap(fieldName = "picklistId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "PCKLST_RLE_PRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyNameView",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "PCKLST_RLE_CBUL",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "LastModifiedBy",
                fkName = "PCKLST_RLE_LMUL",
                keyMaps = {
                    @KeyMap(fieldName = "lastModifiedByUserLogin", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface PicklistRoleEntity {}

    /**
     * Picklist Status History
     */
    @Entity(
        name = "PicklistStatusHistory",
        packageName = "org.ofbiz.shipment.picklist",
        title = "Picklist Status History",
        fields = {
            @Field(name = "picklistId", type = "id-ne"),
            @Field(name = "changeDate", type = "date-time"),
            @Field(name = "changeUserLoginId", type = "id-vlong"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "statusIdTo", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "picklistId"),
            @PrimaryKey(field = "changeDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Picklist",
                fkName = "PCKLST_STHST_PKLT",
                keyMaps = {
                    @KeyMap(fieldName = "picklistId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "Change",
                fkName = "PCKLST_STHST_CUL",
                keyMaps = {
                    @KeyMap(fieldName = "changeUserLoginId", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "PCKLST_STHST_FSI",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "To",
                fkName = "PCKLST_STHST_TSI",
                keyMaps = {
                    @KeyMap(fieldName = "statusIdTo", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusValidChange",
                fkName = "PCKLST_STHST_SVC",
                keyMaps = {
                    @KeyMap(fieldName = "statusId"),
                    @KeyMap(fieldName = "statusIdTo")
                }
            )
        }
    )
    public interface PicklistStatusHistoryEntity {}

    /**
     * Rejection Reason
     */
    @Entity(
        name = "RejectionReason",
        packageName = "org.ofbiz.shipment.receipt",
        title = "Rejection Reason",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "rejectionId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "rejectionId")
        }
    )
    public interface RejectionReasonEntity {}

    /**
     * Shipment Receipt
     */
    @Entity(
        name = "ShipmentReceipt",
        packageName = "org.ofbiz.shipment.receipt",
        title = "Shipment Receipt",
        fields = {
            @Field(name = "receiptId", type = "id-ne"),
            @Field(name = "inventoryItemId", type = "id"),
            @Field(name = "productId", type = "id"),
            @Field(name = "shipmentId", type = "id"),
            @Field(name = "shipmentItemSeqId", type = "id"),
            @Field(name = "shipmentPackageSeqId", type = "id"),
            @Field(name = "orderId", type = "id"),
            @Field(name = "orderItemSeqId", type = "id"),
            @Field(name = "returnId", type = "id"),
            @Field(name = "returnItemSeqId", type = "id"),
            @Field(name = "rejectionId", type = "id"),
            @Field(name = "receivedByUserLoginId", type = "id-vlong"),
            @Field(name = "datetimeReceived", type = "date-time"),
            @Field(name = "itemDescription", type = "description"),
            @Field(name = "quantityAccepted", type = "fixed-point"),
            @Field(name = "quantityRejected", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "receiptId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "SHP_RCPT_INVITM",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "SHP_RCPT_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentPackage",
                fkName = "SHP_RCPT_SHPKG",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentPackageSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderHeader",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "SHP_RCPT_ORDITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RejectionReason",
                fkName = "SHP_RCPT_REJRSN",
                keyMaps = {
                    @KeyMap(fieldName = "rejectionId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "SHP_RCPT_USERLGN",
                keyMaps = {
                    @KeyMap(fieldName = "receivedByUserLoginId", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Shipment",
                fkName = "SHP_RCPT_SHIPMT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ShipmentItem",
                fkName = "SHP_RCPT_SHIPIT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnItem",
                fkName = "SHP_RCPT_RETINVITM",
                keyMaps = {
                    @KeyMap(fieldName = "returnId"),
                    @KeyMap(fieldName = "returnItemSeqId")
                }
            )
        }
    )
    public interface ShipmentReceiptEntity {}

    /**
     * Shipment Receipt Role
     */
    @Entity(
        name = "ShipmentReceiptRole",
        packageName = "org.ofbiz.shipment.receipt",
        title = "Shipment Receipt Role",
        fields = {
            @Field(name = "receiptId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "receiptId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentReceipt",
                fkName = "SHP_RCPTRL_RCPT",
                keyMaps = {
                    @KeyMap(fieldName = "receiptId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "SHP_RCPTRL_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "SHP_RCPTRL_PTRL",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface ShipmentReceiptRoleEntity {}

    /**
     * Carrier Shipment Method
     */
    @Entity(
        name = "CarrierShipmentMethod",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Carrier Shipment Method",
        fields = {
            @Field(name = "shipmentMethodTypeId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "sequenceNumber", type = "numeric"),
            @Field(name = "carrierServiceCode", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentMethodTypeId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentMethodType",
                fkName = "CARR_SHMETH_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "CARR_SHMETH_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "CARR_SHMETH_PROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface CarrierShipmentMethodEntity {}

    /**
     * Carrier Shipment Method
     */
    @Entity(
        name = "CarrierShipmentBoxType",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Carrier Shipment Method",
        fields = {
            @Field(name = "shipmentBoxTypeId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "packagingTypeCode", type = "id"),
            @Field(name = "oversizeCode", type = "very-short")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentBoxTypeId"),
            @PrimaryKey(field = "partyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentBoxType",
                fkName = "CARR_SHBX_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentBoxTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "CARR_SHBX_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            )
        }
    )
    public interface CarrierShipmentBoxTypeEntity {}

    /**
     * Delivery
     */
    @Entity(
        name = "Delivery",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Delivery",
        fields = {
            @Field(name = "deliveryId", type = "id-ne"),
            @Field(name = "originFacilityId", type = "id"),
            @Field(name = "destFacilityId", type = "id"),
            @Field(name = "actualStartDate", type = "date-time"),
            @Field(name = "actualArrivalDate", type = "date-time"),
            @Field(name = "estimatedStartDate", type = "date-time"),
            @Field(name = "estimatedArrivalDate", type = "date-time"),
            @Field(name = "fixedAssetId", type = "id"),
            @Field(name = "startMileage", type = "fixed-point"),
            @Field(name = "endMileage", type = "fixed-point"),
            @Field(name = "fuelUsed", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "deliveryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "DELIV_FXAS",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                title = "Origin",
                fkName = "DELIV_OFAC",
                keyMaps = {
                    @KeyMap(fieldName = "originFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                title = "Dest",
                fkName = "DELIV_DFAC",
                keyMaps = {
                    @KeyMap(fieldName = "destFacilityId", relFieldName = "facilityId")
                }
            )
        }
    )
    public interface DeliveryEntity {}

    /**
     * Shipment
     */
    @Entity(
        name = "Shipment",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment",
        fields = {
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "shipmentTypeId", type = "id"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "primaryOrderId", type = "id"),
            @Field(name = "primaryReturnId", type = "id"),
            @Field(name = "primaryShipGroupSeqId", type = "id"),
            @Field(name = "picklistBinId", type = "id"),
            @Field(name = "estimatedReadyDate", type = "date-time"),
            @Field(name = "estimatedShipDate", type = "date-time"),
            @Field(name = "estimatedShipWorkEffId", type = "id"),
            @Field(name = "estimatedArrivalDate", type = "date-time"),
            @Field(name = "estimatedArrivalWorkEffId", type = "id"),
            @Field(name = "latestCancelDate", type = "date-time"),
            @Field(name = "estimatedShipCost", type = "currency-amount"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "handlingInstructions", type = "long-varchar"),
            @Field(name = "originFacilityId", type = "id"),
            @Field(name = "destinationFacilityId", type = "id"),
            @Field(name = "originContactMechId", type = "id"),
            @Field(name = "originTelecomNumberId", type = "id"),
            @Field(name = "destinationContactMechId", type = "id"),
            @Field(name = "destinationTelecomNumberId", type = "id"),
            @Field(name = "partyIdTo", type = "id"),
            @Field(name = "partyIdFrom", type = "id"),
            @Field(name = "additionalShippingCharge", type = "currency-amount"),
            @Field(name = "addtlShippingChargeDesc", type = "long-varchar"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentType",
                fkName = "SHPMNT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "SHPMNT_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                title = "EstimatedShip",
                fkName = "SHPMNT_ESHWEFF",
                keyMaps = {
                    @KeyMap(fieldName = "estimatedShipWorkEffId", relFieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                title = "EstimatedArrival",
                fkName = "SHPMNT_EARRWEFF",
                keyMaps = {
                    @KeyMap(fieldName = "estimatedArrivalWorkEffId", relFieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "SHPMNT_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                title = "Origin",
                fkName = "SHPMNT_OFAC",
                keyMaps = {
                    @KeyMap(fieldName = "originFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                title = "Destination",
                fkName = "SHPMNT_DFAC",
                keyMaps = {
                    @KeyMap(fieldName = "destinationFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContactMech",
                title = "Origin",
                keyMaps = {
                    @KeyMap(fieldName = "originContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContactMech",
                title = "Dest",
                keyMaps = {
                    @KeyMap(fieldName = "destinationContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PostalAddress",
                title = "Origin",
                fkName = "SHPMNT_OPAD",
                keyMaps = {
                    @KeyMap(fieldName = "originContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TelecomNumber",
                title = "Origin",
                fkName = "SHPMNT_OTCN",
                keyMaps = {
                    @KeyMap(fieldName = "originTelecomNumberId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PostalAddress",
                title = "Destination",
                fkName = "SHPMNT_DPAD",
                keyMaps = {
                    @KeyMap(fieldName = "destinationContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TelecomNumber",
                title = "Destination",
                fkName = "SHPMNT_DTCN",
                keyMaps = {
                    @KeyMap(fieldName = "destinationTelecomNumberId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                title = "Primary",
                fkName = "SHPMNT_PODR",
                keyMaps = {
                    @KeyMap(fieldName = "primaryOrderId", relFieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnHeader",
                title = "Primary",
                fkName = "SHPMNT_PRTNHDR",
                keyMaps = {
                    @KeyMap(fieldName = "primaryReturnId", relFieldName = "returnId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PicklistBin",
                fkName = "SHPMNT_PKLSTBIN",
                keyMaps = {
                    @KeyMap(fieldName = "picklistBinId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGroup",
                title = "Primary",
                keyMaps = {
                    @KeyMap(fieldName = "primaryOrderId", relFieldName = "orderId"),
                    @KeyMap(fieldName = "primaryShipGroupSeqId", relFieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ShipmentTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "To",
                fkName = "SHPMNT_PRTYTO",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Person",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyGroup",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdTo", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "From",
                fkName = "SHPMNT_PRTYFM",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Person",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyGroup",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "partyIdFrom", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ShipmentManifestView",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            )
        }
    )
    public interface ShipmentEntity {}

    /**
     * Shipment Attribute
     */
    @Entity(
        name = "ShipmentAttribute",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Attribute",
        fields = {
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Shipment",
                fkName = "SHPMNT_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ShipmentTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface ShipmentAttributeEntity {}

    /**
     * Shipment Contact Mechanism Type
     */
    @Entity(
        name = "ShipmentBoxType",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Contact Mechanism Type",
        fields = {
            @Field(name = "shipmentBoxTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "dimensionUomId", type = "id"),
            @Field(name = "boxLength", type = "fixed-point"),
            @Field(name = "boxWidth", type = "fixed-point"),
            @Field(name = "boxHeight", type = "fixed-point"),
            @Field(name = "weightUomId", type = "id"),
            @Field(name = "boxWeight", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentBoxTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Dimension",
                fkName = "SHMT_BXTP_DUOM",
                keyMaps = {
                    @KeyMap(fieldName = "dimensionUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Weight",
                fkName = "SHMT_BXTP_WUOM",
                keyMaps = {
                    @KeyMap(fieldName = "weightUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface ShipmentBoxTypeEntity {}

    /**
     * Shipment Contact Mechanism
     */
    @Entity(
        name = "ShipmentContactMech",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Contact Mechanism",
        fields = {
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "shipmentContactMechTypeId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentId"),
            @PrimaryKey(field = "shipmentContactMechTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Shipment",
                fkName = "SHPMT_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "SHPMT_CMECH_CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentContactMechType",
                fkName = "SHPMT_CMECH_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentContactMechTypeId")
                }
            )
        }
    )
    public interface ShipmentContactMechEntity {}

    /**
     * Shipment Contact Mechanism Type
     */
    @Entity(
        name = "ShipmentContactMechType",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Contact Mechanism Type",
        fields = {
            @Field(name = "shipmentContactMechTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentContactMechTypeId")
        }
    )
    public interface ShipmentContactMechTypeEntity {}

    /**
     * Shipment Cost Estimate
     */
    @Entity(
        name = "ShipmentCostEstimate",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Cost Estimate",
        fields = {
            @Field(name = "shipmentCostEstimateId", type = "id-ne"),
            @Field(name = "shipmentMethodTypeId", type = "id"),
            @Field(name = "carrierPartyId", type = "id"),
            @Field(name = "carrierRoleTypeId", type = "id"),
            @Field(name = "productStoreShipMethId", type = "id"),
            @Field(name = "productStoreId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "roleTypeId", type = "id"),
            @Field(name = "geoIdTo", type = "id"),
            @Field(name = "geoIdFrom", type = "id"),
            @Field(name = "weightBreakId", type = "id"),
            @Field(name = "weightUomId", type = "id"),
            @Field(name = "weightUnitPrice", type = "currency-amount"),
            @Field(name = "quantityBreakId", type = "id"),
            @Field(name = "quantityUomId", type = "id"),
            @Field(name = "quantityUnitPrice", type = "currency-amount"),
            @Field(name = "priceBreakId", type = "id"),
            @Field(name = "priceUomId", type = "id"),
            @Field(name = "priceUnitPrice", type = "currency-amount"),
            @Field(name = "orderFlatPrice", type = "currency-amount"),
            @Field(name = "orderPricePercent", type = "fixed-point"),
            @Field(name = "orderItemFlatPrice", type = "currency-amount"),
            @Field(name = "shippingPricePercent", type = "fixed-point"),
            @Field(name = "productFeatureGroupId", type = "id"),
            @Field(name = "oversizeUnit", type = "fixed-point"),
            @Field(name = "oversizePrice", type = "currency-amount"),
            @Field(name = "featurePercent", type = "fixed-point"),
            @Field(name = "featurePrice", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentCostEstimateId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CarrierShipmentMethod",
                fkName = "SHPMNT_CE_CSHMTH",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId"),
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId"),
                    @KeyMap(fieldName = "carrierRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStoreShipmentMeth",
                fkName = "SHPMNT_PS_SH_METH",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreShipMethId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "SHPMNT_CE_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RoleType",
                fkName = "SHPMNT_CE_ROLET",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyRole",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Weight",
                fkName = "SHPMNT_CE_WUOM",
                keyMaps = {
                    @KeyMap(fieldName = "weightUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Quantity",
                fkName = "SHPMNT_CE_QUOM",
                keyMaps = {
                    @KeyMap(fieldName = "quantityUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Price",
                fkName = "SHPMNT_CE_PUOM",
                keyMaps = {
                    @KeyMap(fieldName = "priceUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "To",
                fkName = "SHPMNT_CE_TGEO",
                keyMaps = {
                    @KeyMap(fieldName = "geoIdTo", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "From",
                fkName = "SHPMNT_CE_FGEO",
                keyMaps = {
                    @KeyMap(fieldName = "geoIdFrom", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "QuantityBreak",
                title = "Weight",
                fkName = "SHPMNT_CE_WHT_QB",
                keyMaps = {
                    @KeyMap(fieldName = "weightBreakId", relFieldName = "quantityBreakId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "QuantityBreak",
                title = "Quantity",
                fkName = "SHPMNT_CE_QNT_QB",
                keyMaps = {
                    @KeyMap(fieldName = "quantityBreakId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "QuantityBreak",
                title = "Price",
                fkName = "SHPMNT_CE_PRC_QB",
                keyMaps = {
                    @KeyMap(fieldName = "priceBreakId", relFieldName = "quantityBreakId")
                }
            )
        }
    )
    public interface ShipmentCostEstimateEntity {}

    /**
     * Shipment Gateway Config Type
     */
    @Entity(
        name = "ShipmentGatewayConfigType",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Gateway Config Type",
        fields = {
            @Field(name = "shipmentGatewayConfTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentGatewayConfTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentGatewayConfigType",
                title = "Parent",
                fkName = "SGCT_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "shipmentGatewayConfTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ShipmentGatewayConfigType",
                title = "Sibling",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId")
                }
            )
        }
    )
    public interface ShipmentGatewayConfigTypeEntity {}

    /**
     * Shipment Gateway Config
     */
    @Entity(
        name = "ShipmentGatewayConfig",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Gateway Config",
        fields = {
            @Field(name = "shipmentGatewayConfigId", type = "id-ne"),
            @Field(name = "shipmentGatewayConfTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentGatewayConfigType",
                fkName = "SGC_SGCT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentGatewayConfTypeId", relFieldName = "shipmentGatewayConfTypeId")
                }
            )
        }
    )
    public interface ShipmentGatewayConfigEntity {}

    /**
     * DHL Shipment Gateway Configuration
     */
    @Entity(
        name = "ShipmentGatewayDhl",
        packageName = "org.ofbiz.shipment.shipment",
        title = "DHL Shipment Gateway Configuration",
        fields = {
            @Field(name = "shipmentGatewayConfigId", type = "id-ne"),
            @Field(name = "connectUrl", type = "value", description = "DHL Connection URL"),
            @Field(name = "connectTimeout", type = "numeric", description = "Timeout in seconds"),
            @Field(name = "headVersion", type = "short-varchar", description = "Head version attribute"),
            @Field(name = "headAction", type = "value", description = "Head action attribute"),
            @Field(name = "accessUserId", type = "value", description = "Your DHL ShipIT User Id", encrypt = "true"),
            @Field(name = "accessPassword", type = "value", description = "Your DHL ShipIT Access Password", encrypt = "true"),
            @Field(name = "accessAccountNbr", type = "value", description = "Your DHL ShipIT Account Number", encrypt = "true"),
            @Field(name = "accessShippingKey", type = "value", description = "Your DHL ShipIT Shipping Key", encrypt = "true"),
            @Field(name = "labelImageFormat", type = "short-varchar", description = "Label image format"),
            @Field(name = "rateEstimateTemplate", type = "value", description = "API Schema Templates")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentGatewayConfig",
                fkName = "SGDHL_SGC",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentGatewayConfigId")
                }
            )
        }
    )
    public interface ShipmentGatewayDhlEntity {}

    /**
     * Fedex Shipment Gateway Configuration
     */
    @Entity(
        name = "ShipmentGatewayFedex",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Fedex Shipment Gateway Configuration",
        fields = {
            @Field(name = "shipmentGatewayConfigId", type = "id-ne"),
            @Field(name = "connectUrl", type = "value", description = "Fedex Connection URL"),
            @Field(name = "connectSoapUrl", type = "value", description = "Fedex Soap Connection URL"),
            @Field(name = "connectTimeout", type = "numeric", description = "Timeout in seconds"),
            @Field(name = "accessAccountNbr", type = "value", description = "Your Fedex account number", encrypt = "true"),
            @Field(name = "accessMeterNumber", type = "value", description = "Your Fedex meter number", encrypt = "true"),
            @Field(name = "accessUserKey", type = "value", description = "Your Fedex user credential key", encrypt = "true"),
            @Field(name = "accessUserPwd", type = "value", description = "Your Fedex user credential password", encrypt = "true"),
            @Field(name = "labelImageType", type = "short-varchar", description = "Label image type"),
            @Field(name = "defaultDropoffType", type = "value", description = "Default dropoff type"),
            @Field(name = "defaultPackagingType", type = "value", description = "Default packaging type"),
            @Field(name = "templateShipment", type = "value", description = "Shipment Template location"),
            @Field(name = "templateSubscription", type = "value", description = "Subscription Template location"),
            @Field(name = "rateEstimateTemplate", type = "value", description = "FedEx API Rate Estimate Template")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentGatewayConfig",
                fkName = "SGFED_SGC",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentGatewayConfigId")
                }
            )
        }
    )
    public interface ShipmentGatewayFedexEntity {}

    /**
     * UPS Shipment Gateway Configuration
     */
    @Entity(
        name = "ShipmentGatewayUps",
        packageName = "org.ofbiz.shipment.shipment",
        title = "UPS Shipment Gateway Configuration",
        fields = {
            @Field(name = "shipmentGatewayConfigId", type = "id-ne"),
            @Field(name = "connectUrl", type = "value", description = "UPS Connection URL"),
            @Field(name = "connectTimeout", type = "numeric", description = "Timeout in seconds"),
            @Field(name = "shipperNumber", type = "value", description = "UPS Shipper Number"),
            @Field(name = "billShipperAccountNumber", type = "value", description = "UPS Bill Shipper Account Number"),
            @Field(name = "accessLicenseNumber", type = "value", description = "UPS XPCI Access License Number", encrypt = "true"),
            @Field(name = "accessUserId", type = "value", description = "UPS XPCI Access User ID", encrypt = "true"),
            @Field(name = "accessPassword", type = "value", description = "UPS XPCI Access Password", encrypt = "true"),
            @Field(name = "saveCertInfo", type = "short-varchar", description = "Setting to save files needed for UPS certification (true|false)"),
            @Field(name = "saveCertPath", type = "value", description = "UPS file certificate path"),
            @Field(name = "shipperPickupType", type = "short-varchar", description = "Shipper Default Pickup Type"),
            @Field(name = "customerClassification", type = "short-varchar", description = "Customer Classification"),
            @Field(name = "maxEstimateWeight", type = "fixed-point", description = "Estimate split into packages"),
            @Field(name = "minEstimateWeight", type = "fixed-point", description = "Minimum weight for a package"),
            @Field(name = "codAllowCod", type = "value", description = "All shipment package items are from orders which have been fully paid via EXT_COD"),
            @Field(name = "codSurchargeAmount", type = "fixed-point", description = "Surcharge amount"),
            @Field(name = "codSurchargeCurrencyUomId", type = "short-varchar", description = "Surcharge currency"),
            @Field(name = "codSurchargeApplyToPackage", type = "short-varchar", description = "Surcharge amount will be applied to each shipment package"),
            @Field(name = "codFundsCode", type = "short-varchar", description = "The code that indicates the type of funds used for the COD payment"),
            @Field(name = "defaultReturnLabelMemo", type = "value", description = "Return label email memo"),
            @Field(name = "defaultReturnLabelSubject", type = "value", description = "Return label subject")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentGatewayConfig",
                fkName = "SGUPS_SGC",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentGatewayConfigId")
                }
            )
        }
    )
    public interface ShipmentGatewayUpsEntity {}

    /**
     * USPS Shipment Gateway Configuration
     */
    @Entity(
        name = "ShipmentGatewayUsps",
        packageName = "org.ofbiz.shipment.shipment",
        title = "USPS Shipment Gateway Configuration",
        fields = {
            @Field(name = "shipmentGatewayConfigId", type = "id-ne"),
            @Field(name = "connectUrl", type = "value", description = "USPS Connection URL"),
            @Field(name = "connectUrlLabels", type = "value", description = "USPS Connection URL for Labels"),
            @Field(name = "connectTimeout", type = "numeric", description = "Timeout in seconds"),
            @Field(name = "accessUserId", type = "value", description = "USPS Access User ID", encrypt = "true"),
            @Field(name = "accessPassword", type = "value", description = "USPS Access Password", encrypt = "true"),
            @Field(name = "maxEstimateWeight", type = "numeric", description = "Estimate split into packages"),
            @Field(name = "test", type = "short-varchar", description = "Test/Production mode")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentGatewayConfigId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentGatewayConfig",
                fkName = "SGUSPS_SGC",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentGatewayConfigId")
                }
            )
        }
    )
    public interface ShipmentGatewayUspsEntity {}

    /**
     * Shipment Item
     */
    @Entity(
        name = "ShipmentItem",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Item",
        fields = {
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "shipmentItemSeqId", type = "id-ne"),
            @Field(name = "productId", type = "id"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "shipmentContentDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentId"),
            @PrimaryKey(field = "shipmentItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Shipment",
                fkName = "SHPMNT_ITM_SHPMT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "SHPMNT_ITM_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ShipmentItemEntity {}

    /**
     * Shipment Item Billing
     */
    @Entity(
        name = "ShipmentItemBilling",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Item Billing",
        fields = {
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "shipmentItemSeqId", type = "id-ne"),
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "invoiceItemSeqId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentId"),
            @PrimaryKey(field = "shipmentItemSeqId"),
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "invoiceItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Shipment",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentItem",
                fkName = "SHPMNT_ITBL_SPIM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Invoice",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItem",
                fkName = "SHPMNT_ITBL_INIM",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            )
        }
    )
    public interface ShipmentItemBillingEntity {}

    /**
     * Shipment Item Feature
     */
    @Entity(
        name = "ShipmentItemFeature",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Item Feature",
        fields = {
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "shipmentItemSeqId", type = "id-ne"),
            @Field(name = "productFeatureId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentId"),
            @PrimaryKey(field = "shipmentItemSeqId"),
            @PrimaryKey(field = "productFeatureId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Shipment",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentItem",
                fkName = "SHPMNT_ITFT_SPIM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                fkName = "SHPMNT_ITFT_FEAT",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            )
        }
    )
    public interface ShipmentItemFeatureEntity {}

    /**
     * Shipment Method Type
     */
    @Entity(
        name = "ShipmentMethodType",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Method Type",
        fields = {
            @Field(name = "shipmentMethodTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "sequenceNum", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentMethodTypeId")
        }
    )
    public interface ShipmentMethodTypeEntity {}

    /**
     * Shipment Package
     */
    @Entity(
        name = "ShipmentPackage",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Package",
        fields = {
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "shipmentPackageSeqId", type = "id-ne"),
            @Field(name = "shipmentBoxTypeId", type = "id"),
            @Field(name = "dateCreated", type = "date-time"),
            @Field(name = "boxLength", type = "fixed-point", description = "This field store the length of package; if a shipmentBoxTypeId is specified then this overrides the dimension specified there; this field is meant to be used when there is no applicable ShipmentBoxType"),
            @Field(name = "boxHeight", type = "fixed-point", description = "This field store the height of package; if a shipmentBoxTypeId is specified then this overrides the dimension specified there; this field is meant to be used when there is no applicable ShipmentBoxType"),
            @Field(name = "boxWidth", type = "fixed-point", description = "This field store the width of package; if a shipmentBoxTypeId is specified then this overrides the dimension specified there; this field is meant to be used when there is no applicable ShipmentBoxType"),
            @Field(name = "dimensionUomId", type = "id", description = "This field store the unit of measurement of dimension (length, width and height)"),
            @Field(name = "weight", type = "fixed-point"),
            @Field(name = "weightUomId", type = "id"),
            @Field(name = "insuredValue", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentId"),
            @PrimaryKey(field = "shipmentPackageSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Shipment",
                fkName = "SHPKG_SHPMNT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentBoxType",
                fkName = "SHPKG_BXTYP",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentBoxTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CarrierShipmentBoxType",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentBoxTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Dimension",
                fkName = "SHPKG_DUOM",
                keyMaps = {
                    @KeyMap(fieldName = "dimensionUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Weight",
                fkName = "SHPKG_WUOM",
                keyMaps = {
                    @KeyMap(fieldName = "weightUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface ShipmentPackageEntity {}

    /**
     * Shipment Package Content
     */
    @Entity(
        name = "ShipmentPackageContent",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Package Content",
        fields = {
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "shipmentPackageSeqId", type = "id-ne"),
            @Field(name = "shipmentItemSeqId", type = "id-ne"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "subProductId", type = "id"),
            @Field(name = "subProductQuantity", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentId"),
            @PrimaryKey(field = "shipmentPackageSeqId"),
            @PrimaryKey(field = "shipmentItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentPackage",
                fkName = "PCK_CNTNT_SHPKG",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentPackageSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Shipment",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentItem",
                fkName = "PCK_CNTNT_SHITM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                title = "Sub",
                fkName = "PCK_CNTNT_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "subProductId", relFieldName = "productId")
                }
            )
        }
    )
    public interface ShipmentPackageContentEntity {}

    /**
     * Shipment Package Route Segment
     */
    @Entity(
        name = "ShipmentPackageRouteSeg",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Package Route Segment",
        fields = {
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "shipmentPackageSeqId", type = "id-ne"),
            @Field(name = "shipmentRouteSegmentId", type = "id-ne"),
            @Field(name = "trackingCode", type = "short-varchar"),
            @Field(name = "boxNumber", type = "short-varchar"),
            @Field(name = "labelImage", type = "byte-array"),
            @Field(name = "labelIntlSignImage", type = "byte-array"),
            @Field(name = "labelHtml", type = "very-long"),
            @Field(name = "labelPrinted", type = "indicator"),
            @Field(name = "internationalInvoice", type = "byte-array"),
            @Field(name = "packageTransportCost", type = "currency-amount"),
            @Field(name = "packageServiceCost", type = "currency-amount"),
            @Field(name = "packageOtherCost", type = "currency-amount"),
            @Field(name = "codAmount", type = "currency-amount"),
            @Field(name = "insuredAmount", type = "currency-amount"),
            @Field(name = "currencyUomId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentId"),
            @PrimaryKey(field = "shipmentPackageSeqId"),
            @PrimaryKey(field = "shipmentRouteSegmentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentPackage",
                fkName = "SHPKRTSG_SHPKG",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentPackageSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Shipment",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentRouteSegment",
                fkName = "SHPKRTSG_RTSG",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentRouteSegmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "SHPKRTSG_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface ShipmentPackageRouteSegEntity {}

    /**
     * Shipment Route Segment
     */
    @Entity(
        name = "ShipmentRouteSegment",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Route Segment",
        fields = {
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "shipmentRouteSegmentId", type = "id-ne"),
            @Field(name = "deliveryId", type = "id"),
            @Field(name = "originFacilityId", type = "id"),
            @Field(name = "destFacilityId", type = "id"),
            @Field(name = "originContactMechId", type = "id"),
            @Field(name = "originTelecomNumberId", type = "id"),
            @Field(name = "destContactMechId", type = "id"),
            @Field(name = "destTelecomNumberId", type = "id"),
            @Field(name = "carrierPartyId", type = "id"),
            @Field(name = "shipmentMethodTypeId", type = "id"),
            @Field(name = "carrierServiceStatusId", type = "id"),
            @Field(name = "carrierDeliveryZone", type = "short-varchar"),
            @Field(name = "carrierRestrictionCodes", type = "short-varchar"),
            @Field(name = "carrierRestrictionDesc", type = "very-long"),
            @Field(name = "billingWeight", type = "fixed-point"),
            @Field(name = "billingWeightUomId", type = "id"),
            @Field(name = "actualTransportCost", type = "currency-amount"),
            @Field(name = "actualServiceCost", type = "currency-amount"),
            @Field(name = "actualOtherCost", type = "currency-amount"),
            @Field(name = "actualCost", type = "currency-amount"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "actualStartDate", type = "date-time"),
            @Field(name = "actualArrivalDate", type = "date-time"),
            @Field(name = "estimatedStartDate", type = "date-time"),
            @Field(name = "estimatedArrivalDate", type = "date-time"),
            @Field(name = "trackingIdNumber", type = "short-varchar"),
            @Field(name = "trackingDigest", type = "very-long"),
            @Field(name = "updatedByUserLoginId", type = "id-vlong"),
            @Field(name = "lastUpdatedDate", type = "date-time"),
            @Field(name = "homeDeliveryType", type = "id"),
            @Field(name = "homeDeliveryDate", type = "date-time"),
            @Field(name = "thirdPartyAccountNumber", type = "id"),
            @Field(name = "thirdPartyPostalCode", type = "id"),
            @Field(name = "thirdPartyCountryGeoCode", type = "id"),
            @Field(name = "upsHighValueReport", type = "byte-array")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentId"),
            @PrimaryKey(field = "shipmentRouteSegmentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Shipment",
                fkName = "SHPMT_RTSEG_SHPMT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Delivery",
                fkName = "SHPMT_RTSEG_DEL",
                keyMaps = {
                    @KeyMap(fieldName = "deliveryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Carrier",
                fkName = "SHPMT_RTSEG_CPTY",
                keyMaps = {
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Person",
                title = "Carrier",
                keyMaps = {
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PartyGroup",
                title = "Carrier",
                keyMaps = {
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentMethodType",
                fkName = "SHPMT_RTSEG_SHMT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                title = "Origin",
                fkName = "SHPMT_RTSEG_OFAC",
                keyMaps = {
                    @KeyMap(fieldName = "originFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                title = "Dest",
                fkName = "SHPMT_RTSEG_DFAC",
                keyMaps = {
                    @KeyMap(fieldName = "destFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContactMech",
                title = "Origin",
                keyMaps = {
                    @KeyMap(fieldName = "originContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContactMech",
                title = "Dest",
                keyMaps = {
                    @KeyMap(fieldName = "destContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PostalAddress",
                title = "Origin",
                fkName = "SHPMT_RTSEG_OPAD",
                keyMaps = {
                    @KeyMap(fieldName = "originContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TelecomNumber",
                title = "Origin",
                fkName = "SHPMT_RTSEG_OTCN",
                keyMaps = {
                    @KeyMap(fieldName = "originTelecomNumberId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PostalAddress",
                title = "Dest",
                fkName = "SHPMT_RTSEG_DPAD",
                keyMaps = {
                    @KeyMap(fieldName = "destContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TelecomNumber",
                title = "Dest",
                fkName = "SHPMT_RTSEG_DTCN",
                keyMaps = {
                    @KeyMap(fieldName = "destTelecomNumberId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "CarrierService",
                fkName = "SHPKRTSG_CSSTS",
                keyMaps = {
                    @KeyMap(fieldName = "carrierServiceStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "SHPMT_RTSEG_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "BillingWeight",
                fkName = "SHPKRTSG_BWUOM",
                keyMaps = {
                    @KeyMap(fieldName = "billingWeightUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface ShipmentRouteSegmentEntity {}

    /**
     * Shipment Status
     */
    @Entity(
        name = "ShipmentStatus",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Status",
        fields = {
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "statusDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "statusId"),
            @PrimaryKey(field = "shipmentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "SHPMNT_STTS_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Shipment",
                fkName = "SHPMNT_STTS_SHMT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            )
        }
    )
    public interface ShipmentStatusEntity {}

    /**
     * Shipment Type
     */
    @Entity(
        name = "ShipmentType",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Type",
        defaultResourceName = "ProductEntityLabels",
        fields = {
            @Field(name = "shipmentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentType",
                title = "Parent",
                fkName = "SHPMNT_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "shipmentTypeId")
                }
            )
        }
    )
    public interface ShipmentTypeEntity {}

    /**
     * Shipment Type Attribute
     */
    @Entity(
        name = "ShipmentTypeAttr",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Type Attribute",
        fields = {
            @Field(name = "shipmentTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "shipmentTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentType",
                fkName = "SHPMNT_TYPATR",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ShipmentAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Shipment",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentTypeId")
                }
            )
        }
    )
    public interface ShipmentTypeAttrEntity {}

    /**
     * Shipping Document
     */
    @Entity(
        name = "ShippingDocument",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipping Document",
        fields = {
            @Field(name = "documentId", type = "id-ne"),
            @Field(name = "shipmentId", type = "id"),
            @Field(name = "shipmentItemSeqId", type = "id"),
            @Field(name = "shipmentPackageSeqId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "documentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Document",
                fkName = "SHPNG_DOC_DOC",
                keyMaps = {
                    @KeyMap(fieldName = "documentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentItem",
                fkName = "SHPNG_DOC_SMITM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentPackage",
                fkName = "SHPNG_DOC_SHPKG",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentPackageSeqId")
                }
            )
        }
    )
    public interface ShippingDocumentEntity {}

    @ViewEntity(
        name = "ItemIssuanceAndInventoryItem",
        packageName = "org.ofbiz.shipment.issuance",
        members = {
            @MemberEntity(entityAlias = "IISS", entityName = "ItemIssuance"),
            @MemberEntity(entityAlias = "IITM", entityName = "InventoryItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "IISS")
        },
        aliases = {
            @Alias(name = "inventoryItemId", entityAlias = "IITM"),
            @Alias(name = "inventoryItemTypeId", entityAlias = "IITM"),
            @Alias(name = "productId", entityAlias = "IITM"),
            @Alias(name = "partyId", entityAlias = "IITM"),
            @Alias(name = "ownerPartyId", entityAlias = "IITM"),
            @Alias(name = "statusId", entityAlias = "IITM"),
            @Alias(name = "datetimeReceived", entityAlias = "IITM"),
            @Alias(name = "datetimeManufactured", entityAlias = "IITM"),
            @Alias(name = "expireDate", entityAlias = "IITM"),
            @Alias(name = "facilityId", entityAlias = "IITM"),
            @Alias(name = "containerId", entityAlias = "IITM"),
            @Alias(name = "lotId", entityAlias = "IITM"),
            @Alias(name = "uomId", entityAlias = "IITM"),
            @Alias(name = "binNumber", entityAlias = "IITM"),
            @Alias(name = "locationSeqId", entityAlias = "IITM"),
            @Alias(name = "comments", entityAlias = "IITM"),
            @Alias(name = "quantityOnHandTotal", entityAlias = "IITM"),
            @Alias(name = "availableToPromiseTotal", entityAlias = "IITM"),
            @Alias(name = "accountingQuantityTotal", entityAlias = "IITM"),
            @Alias(name = "oldQuantityOnHand", entityAlias = "IITM"),
            @Alias(name = "oldAvailableToPromise", entityAlias = "IITM"),
            @Alias(name = "serialNumber", entityAlias = "IITM"),
            @Alias(name = "softIdentifier", entityAlias = "IITM"),
            @Alias(name = "activationNumber", entityAlias = "IITM"),
            @Alias(name = "activationValidThru", entityAlias = "IITM"),
            @Alias(name = "unitCost", entityAlias = "IITM"),
            @Alias(name = "currencyUomId", entityAlias = "IITM"),
            @Alias(name = "inventoryItemFixedAssetId", entityAlias = "IITM", field = "fixedAssetId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "IISS",
                relEntityAlias = "IITM",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface ItemIssuanceAndInventoryItemView {}

    /**
     * Picklist and PicklistBin and PicklistItem View
     */
    @ViewEntity(
        name = "PicklistAndBinAndItem",
        packageName = "org.ofbiz.shipment.picklist",
        title = "Picklist and PicklistBin and PicklistItem View",
        members = {
            @MemberEntity(entityAlias = "PL", entityName = "Picklist"),
            @MemberEntity(entityAlias = "PLB", entityName = "PicklistBin"),
            @MemberEntity(entityAlias = "PLI", entityName = "PicklistItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PL"),
            @AliasAll(entityAlias = "PLB"),
            @AliasAll(entityAlias = "PLI")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PL",
                relEntityAlias = "PLB",
                keyMaps = {
                    @KeyMap(fieldName = "picklistId")
                }
            ),
            @ViewLink(
                entityAlias = "PLB",
                relEntityAlias = "PLI",
                keyMaps = {
                    @KeyMap(fieldName = "picklistBinId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        }
    )
    public interface PicklistAndBinAndItemView {}

    /**
     * Picklist Item and Bin View
     */
    @ViewEntity(
        name = "PicklistItemAndBin",
        packageName = "org.ofbiz.shipment.picklist",
        title = "Picklist Item and Bin View",
        members = {
            @MemberEntity(entityAlias = "PB", entityName = "PicklistBin"),
            @MemberEntity(entityAlias = "PIM", entityName = "PicklistItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "PB"),
            @AliasAll(entityAlias = "PIM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PB",
                relEntityAlias = "PIM",
                keyMaps = {
                    @KeyMap(fieldName = "picklistBinId")
                }
            )
        }
    )
    public interface PicklistItemAndBinView {}

    /**
     * ShipmentReceipt And Inventory Item View
     */
    @ViewEntity(
        name = "ShipmentReceiptAndItem",
        packageName = "org.ofbiz.shipment.shipment",
        title = "ShipmentReceipt And Inventory Item View",
        members = {
            @MemberEntity(entityAlias = "SR", entityName = "ShipmentReceipt"),
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "SR")
        },
        aliases = {
            @Alias(name = "facilityId", entityAlias = "II"),
            @Alias(name = "locationSeqId", entityAlias = "II"),
            @Alias(name = "quantityOnHandTotal", entityAlias = "II"),
            @Alias(name = "availableToPromiseTotal", entityAlias = "II"),
            @Alias(name = "unitCost", entityAlias = "II"),
            @Alias(name = "lotId", entityAlias = "II")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SR",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface ShipmentReceiptAndItemView {}

    /**
     * Carrier And Shipment Method Type View
     */
    @ViewEntity(
        name = "CarrierAndShipmentMethod",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Carrier And Shipment Method Type View",
        members = {
            @MemberEntity(entityAlias = "CS", entityName = "CarrierShipmentMethod"),
            @MemberEntity(entityAlias = "SM", entityName = "ShipmentMethodType")
        },
        aliases = {
            @Alias(name = "shipmentMethodTypeId", entityAlias = "CS"),
            @Alias(name = "partyId", entityAlias = "CS"),
            @Alias(name = "roleTypeId", entityAlias = "CS"),
            @Alias(name = "sequenceNumber", entityAlias = "CS"),
            @Alias(name = "description", entityAlias = "SM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CS",
                relEntityAlias = "SM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId")
                }
            )
        }
    )
    public interface CarrierAndShipmentMethodView {}

    /**
     * Order Shipment Information View
     * This view is meant for getting all tracking information for all shipments associated with an order, it does not include information to determine which packages in a given shipment correspond to that order, or to determine what information applies to each line item of the order.
     */
    @ViewEntity(
        name = "OrderShipmentInfoSummary",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Order Shipment Information View",
        description = "This view is meant for getting all tracking information for all shipments associated with an order, it does not include information to determine which packages in a given shipment correspond to that order, or to determine what information applies to each line item of the order.",
        members = {
            @MemberEntity(entityAlias = "II", entityName = "ItemIssuance"),
            @MemberEntity(entityAlias = "SRS", entityName = "ShipmentRouteSegment"),
            @MemberEntity(entityAlias = "SPRS", entityName = "ShipmentPackageRouteSeg")
        },
        aliases = {
            @Alias(name = "orderId", entityAlias = "II"),
            @Alias(name = "orderItemSeqId", entityAlias = "II"),
            @Alias(name = "shipmentId", entityAlias = "II"),
            @Alias(name = "shipGroupSeqId", entityAlias = "II"),
            @Alias(name = "shipmentRouteSegmentId", entityAlias = "SRS"),
            @Alias(name = "carrierPartyId", entityAlias = "SRS"),
            @Alias(name = "actualStartDate", entityAlias = "SRS"),
            @Alias(name = "shipmentMethodTypeId", entityAlias = "SRS"),
            @Alias(name = "shipmentPackageSeqId", entityAlias = "SPRS"),
            @Alias(name = "trackingCode", entityAlias = "SPRS"),
            @Alias(name = "boxNumber", entityAlias = "SPRS")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "II",
                relEntityAlias = "SRS",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @ViewLink(
                entityAlias = "SRS",
                relEntityAlias = "SPRS",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentRouteSegmentId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Shipment",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            )
        }
    )
    public interface OrderShipmentInfoSummaryView {}

    /**
     * Shipment and Item View
     */
    @ViewEntity(
        name = "ShipmentAndItem",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment and Item View",
        members = {
            @MemberEntity(entityAlias = "SH", entityName = "Shipment"),
            @MemberEntity(entityAlias = "SITM", entityName = "ShipmentItem")
        },
        aliases = {
            @Alias(name = "shipmentId", entityAlias = "SH"),
            @Alias(name = "shipmentTypeId", entityAlias = "SH"),
            @Alias(name = "statusId", entityAlias = "SH"),
            @Alias(name = "primaryOrderId", entityAlias = "SH"),
            @Alias(name = "estimatedReadyDate", entityAlias = "SH"),
            @Alias(name = "estimatedShipDate", entityAlias = "SH"),
            @Alias(name = "estimatedArrivalDate", entityAlias = "SH"),
            @Alias(name = "latestCancelDate", entityAlias = "SH"),
            @Alias(name = "estimatedShipCost", entityAlias = "SH"),
            @Alias(name = "handlingInstructions", entityAlias = "SH"),
            @Alias(name = "originFacilityId", entityAlias = "SH"),
            @Alias(name = "destinationFacilityId", entityAlias = "SH"),
            @Alias(name = "originContactMechId", entityAlias = "SH"),
            @Alias(name = "destinationContactMechId", entityAlias = "SH"),
            @Alias(name = "partyIdTo", entityAlias = "SH"),
            @Alias(name = "partyIdFrom", entityAlias = "SH"),
            @Alias(name = "shipmentItemSeqId", entityAlias = "SITM"),
            @Alias(name = "productId", entityAlias = "SITM"),
            @Alias(name = "quantity", entityAlias = "SITM"),
            @Alias(name = "shipmentContentDescription", entityAlias = "SITM")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SH",
                relEntityAlias = "SITM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Shipment",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ShipmentItem",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentItemSeqId")
                }
            )
        }
    )
    public interface ShipmentAndItemView {}

    /**
     * Shipment Manifest View
     */
    @ViewEntity(
        name = "ShipmentManifestView",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Manifest View",
        members = {
            @MemberEntity(entityAlias = "SITM", entityName = "ShipmentItem"),
            @MemberEntity(entityAlias = "PROD", entityName = "Product"),
            @MemberEntity(entityAlias = "ITMI", entityName = "ItemIssuance"),
            @MemberEntity(entityAlias = "SPCT", entityName = "ShipmentPackageContent"),
            @MemberEntity(entityAlias = "SPKG", entityName = "ShipmentPackage"),
            @MemberEntity(entityAlias = "WTUOM", entityName = "Uom"),
            @MemberEntity(entityAlias = "SPRS", entityName = "ShipmentPackageRouteSeg"),
            @MemberEntity(entityAlias = "SRTS", entityName = "ShipmentRouteSegment"),
            @MemberEntity(entityAlias = "OFAC", entityName = "Facility"),
            @MemberEntity(entityAlias = "DFAC", entityName = "Facility"),
            @MemberEntity(entityAlias = "OPAD", entityName = "PostalAddress"),
            @MemberEntity(entityAlias = "OTCN", entityName = "TelecomNumber"),
            @MemberEntity(entityAlias = "DPAD", entityName = "PostalAddress"),
            @MemberEntity(entityAlias = "DTCN", entityName = "TelecomNumber"),
            @MemberEntity(entityAlias = "CPER", entityName = "Person"),
            @MemberEntity(entityAlias = "CPGP", entityName = "PartyGroup"),
            @MemberEntity(entityAlias = "SHMT", entityName = "ShipmentMethodType")
        },
        aliases = {
            @Alias(name = "shipmentId", entityAlias = "SITM"),
            @Alias(name = "shipmentItemSeqId", entityAlias = "SITM"),
            @Alias(name = "productId", entityAlias = "SITM"),
            @Alias(name = "quantity", entityAlias = "SITM"),
            @Alias(name = "shipmentContentDescription", entityAlias = "SITM"),
            @Alias(name = "internalName", entityAlias = "PROD"),
            @Alias(name = "itemIssuanceId", entityAlias = "ITMI"),
            @Alias(name = "orderId", entityAlias = "ITMI"),
            @Alias(name = "orderItemSeqId", entityAlias = "ITMI"),
            @Alias(name = "inventoryItemId", entityAlias = "ITMI"),
            @Alias(name = "issuedDateTime", entityAlias = "ITMI"),
            @Alias(name = "issuedByUserLoginId", entityAlias = "ITMI"),
            @Alias(name = "issuedQuantity", entityAlias = "ITMI", field = "quantity"),
            @Alias(name = "packageQuantity", entityAlias = "SPCT", field = "quantity"),
            @Alias(name = "shipmentPackageSeqId", entityAlias = "SPKG"),
            @Alias(name = "packageDateCreated", entityAlias = "SPKG", field = "dateCreated"),
            @Alias(name = "weight", entityAlias = "SPKG"),
            @Alias(name = "weightUomAbbreviation", entityAlias = "WTUOM", field = "abbreviation"),
            @Alias(name = "weightUomDescription", entityAlias = "WTUOM", field = "description"),
            @Alias(name = "trackingCode", entityAlias = "SPRS"),
            @Alias(name = "boxNumber", entityAlias = "SPRS"),
            @Alias(name = "shipmentRouteSegmentId", entityAlias = "SRTS"),
            @Alias(name = "deliveryId", entityAlias = "SRTS"),
            @Alias(name = "originFacilityId", entityAlias = "SRTS"),
            @Alias(name = "destFacilityId", entityAlias = "SRTS"),
            @Alias(name = "originContactMechId", entityAlias = "SRTS"),
            @Alias(name = "originTelecomNumberId", entityAlias = "SRTS"),
            @Alias(name = "destContactMechId", entityAlias = "SRTS"),
            @Alias(name = "destTelecomNumberId", entityAlias = "SRTS"),
            @Alias(name = "carrierPartyId", entityAlias = "SRTS"),
            @Alias(name = "shipmentMethodTypeId", entityAlias = "SRTS"),
            @Alias(name = "actualCost", entityAlias = "SRTS"),
            @Alias(name = "actualStartDate", entityAlias = "SRTS"),
            @Alias(name = "actualArrivalDate", entityAlias = "SRTS"),
            @Alias(name = "estimatedStartDate", entityAlias = "SRTS"),
            @Alias(name = "estimatedArrivalDate", entityAlias = "SRTS"),
            @Alias(name = "originFacilityName", entityAlias = "OFAC", field = "facilityName"),
            @Alias(name = "destFacilityName", entityAlias = "DFAC", field = "facilityName"),
            @Alias(name = "originToName", entityAlias = "OPAD", field = "toName"),
            @Alias(name = "originAttnName", entityAlias = "OPAD", field = "attnName"),
            @Alias(name = "originAddress1", entityAlias = "OPAD", field = "address1"),
            @Alias(name = "originAddress2", entityAlias = "OPAD", field = "address2"),
            @Alias(name = "originDirections", entityAlias = "OPAD", field = "directions"),
            @Alias(name = "originCity", entityAlias = "OPAD", field = "city"),
            @Alias(name = "originPostalCode", entityAlias = "OPAD", field = "postalCode"),
            @Alias(name = "originCountryGeoId", entityAlias = "OPAD", field = "countryGeoId"),
            @Alias(name = "originStateProvinceGeoId", entityAlias = "OPAD", field = "stateProvinceGeoId"),
            @Alias(name = "originPostalCodeGeoId", entityAlias = "OPAD", field = "postalCodeGeoId"),
            @Alias(name = "originCountryCode", entityAlias = "OTCN", field = "countryCode"),
            @Alias(name = "originAreaCode", entityAlias = "OTCN", field = "areaCode"),
            @Alias(name = "originContactNumber", entityAlias = "OTCN", field = "contactNumber"),
            @Alias(name = "destToName", entityAlias = "DPAD", field = "toName"),
            @Alias(name = "destAttnName", entityAlias = "DPAD", field = "attnName"),
            @Alias(name = "destAddress1", entityAlias = "DPAD", field = "address1"),
            @Alias(name = "destAddress2", entityAlias = "DPAD", field = "address2"),
            @Alias(name = "destDirections", entityAlias = "DPAD", field = "directions"),
            @Alias(name = "destCity", entityAlias = "DPAD", field = "city"),
            @Alias(name = "destPostalCode", entityAlias = "DPAD", field = "postalCode"),
            @Alias(name = "destCountryGeoId", entityAlias = "DPAD", field = "countryGeoId"),
            @Alias(name = "destStateProvinceGeoId", entityAlias = "DPAD", field = "stateProvinceGeoId"),
            @Alias(name = "destPostalCodeGeoId", entityAlias = "DPAD", field = "postalCodeGeoId"),
            @Alias(name = "destCountryCode", entityAlias = "DTCN", field = "countryCode"),
            @Alias(name = "destAreaCode", entityAlias = "DTCN", field = "areaCode"),
            @Alias(name = "destContactNumber", entityAlias = "DTCN", field = "contactNumber"),
            @Alias(name = "carrierFirstName", entityAlias = "CPER", field = "firstName"),
            @Alias(name = "carrierLastName", entityAlias = "CPER", field = "lastName"),
            @Alias(name = "carrierGroupName", entityAlias = "CPGP", field = "groupName"),
            @Alias(name = "shipmentMethodDescription", entityAlias = "SHMT", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SITM",
                relEntityAlias = "ITMI",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentItemSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "SITM",
                relEntityAlias = "PROD",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @ViewLink(
                entityAlias = "SITM",
                relEntityAlias = "SPCT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentItemSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "SPCT",
                relEntityAlias = "SPKG",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentPackageSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "SPKG",
                relEntityAlias = "WTUOM",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "weightUomId", relFieldName = "uomId")
                }
            ),
            @ViewLink(
                entityAlias = "SPKG",
                relEntityAlias = "SPRS",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentPackageSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "SPRS",
                relEntityAlias = "SRTS",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentRouteSegmentId")
                }
            ),
            @ViewLink(
                entityAlias = "SRTS",
                relEntityAlias = "OFAC",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "originFacilityId", relFieldName = "facilityId")
                }
            ),
            @ViewLink(
                entityAlias = "SRTS",
                relEntityAlias = "DFAC",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "destFacilityId", relFieldName = "facilityId")
                }
            ),
            @ViewLink(
                entityAlias = "SRTS",
                relEntityAlias = "OPAD",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "originContactMechId", relFieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "SRTS",
                relEntityAlias = "OTCN",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "originTelecomNumberId", relFieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "SRTS",
                relEntityAlias = "DPAD",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "destContactMechId", relFieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "SRTS",
                relEntityAlias = "DTCN",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "destTelecomNumberId", relFieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "SRTS",
                relEntityAlias = "CPER",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "SRTS",
                relEntityAlias = "CPGP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "SRTS",
                relEntityAlias = "SHMT",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId", relFieldName = "shipmentMethodTypeId")
                }
            )
        }
    )
    public interface ShipmentManifestViewView {}

    /**
     * Shipment Package Route Detail View
     * View to list information about individual packages with route information, for getting the carrier labels for a package.
     */
    @ViewEntity(
        name = "ShipmentPackageRouteDetail",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Package Route Detail View",
        description = "View to list information about individual packages with route information, for getting the carrier labels for a package.",
        members = {
            @MemberEntity(entityAlias = "SPRS", entityName = "ShipmentPackageRouteSeg"),
            @MemberEntity(entityAlias = "SRS", entityName = "ShipmentRouteSegment"),
            @MemberEntity(entityAlias = "S", entityName = "Shipment")
        },
        aliases = {
            @Alias(name = "shipmentId", entityAlias = "SPRS"),
            @Alias(name = "shipmentPackageSeqId", entityAlias = "SPRS"),
            @Alias(name = "shipmentRouteSegmentId", entityAlias = "SPRS"),
            @Alias(name = "labelPrinted", entityAlias = "SPRS"),
            @Alias(name = "trackingCode", entityAlias = "SPRS"),
            @Alias(name = "carrierPartyId", entityAlias = "SRS"),
            @Alias(name = "carrierServiceStatusId", entityAlias = "SRS"),
            @Alias(name = "shipmentMethodTypeId", entityAlias = "SRS"),
            @Alias(name = "statusId", entityAlias = "S"),
            @Alias(name = "primaryOrderId", entityAlias = "S")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SPRS",
                relEntityAlias = "SRS",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentRouteSegmentId")
                }
            ),
            @ViewLink(
                entityAlias = "SPRS",
                relEntityAlias = "S",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            )
        }
    )
    public interface ShipmentPackageRouteDetailView {}

    /**
     * Shipment Route Segment Detail View
     * View to list a shipment route segment with extra shipment information, for scheduling shipment route segments
     */
    @ViewEntity(
        name = "ShipmentRouteSegmentDetail",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Route Segment Detail View",
        description = "View to list a shipment route segment with extra shipment information, for scheduling shipment route segments",
        members = {
            @MemberEntity(entityAlias = "SRS", entityName = "ShipmentRouteSegment"),
            @MemberEntity(entityAlias = "S", entityName = "Shipment")
        },
        aliases = {
            @Alias(name = "shipmentId", entityAlias = "SRS"),
            @Alias(name = "shipmentRouteSegmentId", entityAlias = "SRS"),
            @Alias(name = "originFacilityId", entityAlias = "SRS"),
            @Alias(name = "carrierPartyId", entityAlias = "SRS"),
            @Alias(name = "carrierServiceStatusId", entityAlias = "SRS"),
            @Alias(name = "shipmentMethodTypeId", entityAlias = "SRS"),
            @Alias(name = "billingWeight", entityAlias = "SRS"),
            @Alias(name = "billingWeightUomId", entityAlias = "SRS"),
            @Alias(name = "statusId", entityAlias = "S"),
            @Alias(name = "primaryOrderId", entityAlias = "S")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SRS",
                relEntityAlias = "S",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            )
        }
    )
    public interface ShipmentRouteSegmentDetailView {}

    /**
     * Shipment Route Segment Detail View
     * View to report ShipmentPackageContent quantity vs. OrderItem quantity via         ItemIssuance
     */
    @ViewEntity(
        name = "PackedQtyVsOrderItemQuantity",
        packageName = "org.ofbiz.shipment.shipment",
        title = "Shipment Route Segment Detail View",
        description = "View to report ShipmentPackageContent quantity vs. OrderItem quantity via\n        ItemIssuance",
        members = {
            @MemberEntity(entityAlias = "SPC", entityName = "ShipmentPackageContent"),
            @MemberEntity(entityAlias = "II", entityName = "ItemIssuance"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem")
        },
        aliases = {
            @Alias(name = "shipmentId", entityAlias = "SPC"),
            @Alias(name = "shipmentPackageSeqId", entityAlias = "SPC"),
            @Alias(name = "packedQuantity", entityAlias = "SPC", field = "quantity"),
            @Alias(name = "issuedQuantity", entityAlias = "II", field = "quantity"),
            @Alias(name = "orderId", entityAlias = "OI"),
            @Alias(name = "orderItemSeqId", entityAlias = "OI"),
            @Alias(name = "orderedQuantity", entityAlias = "OI", field = "quantity")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "SPC",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentItemSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "II",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface PackedQtyVsOrderItemQuantityView {}

}

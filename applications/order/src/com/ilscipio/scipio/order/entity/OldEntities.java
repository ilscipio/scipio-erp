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
package com.ilscipio.scipio.order.entity;

import com.ilscipio.scipio.entity.def.*;

/**
 * Auto-generated annotation-based entity definitions.
 *
 * <p>Generated from entitymodel.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OldEntities {

    /**
     * The OLD Order Item Association Entity (replaced by OrderItemAssoc)
     */
    @Entity(
        name = "OldOrderItemAssociation",
        packageName = "org.ofbiz.order.order",
        tableName = "ORDER_ITEM_ASSOCIATION",
        title = "The OLD Order Item Association Entity (replaced by OrderItemAssoc)",
        neverCache = true,
        fields = {
            @Field(name = "salesOrderId", type = "id-ne"),
            @Field(name = "soItemSeqId", type = "id-ne"),
            @Field(name = "purchaseOrderId", type = "id-ne"),
            @Field(name = "poItemSeqId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "salesOrderId"),
            @PrimaryKey(field = "soItemSeqId"),
            @PrimaryKey(field = "purchaseOrderId"),
            @PrimaryKey(field = "poItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                title = "Sales",
                fkName = "ORDER_ITASSC_SOHD",
                keyMaps = {
                    @KeyMap(fieldName = "salesOrderId", relFieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItem",
                title = "Sales",
                keyMaps = {
                    @KeyMap(fieldName = "salesOrderId", relFieldName = "orderId"),
                    @KeyMap(fieldName = "soItemSeqId", relFieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                title = "Purchase",
                fkName = "ORDER_ITASSC_POHD",
                keyMaps = {
                    @KeyMap(fieldName = "purchaseOrderId", relFieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItem",
                title = "Purchase",
                keyMaps = {
                    @KeyMap(fieldName = "purchaseOrderId", relFieldName = "orderId"),
                    @KeyMap(fieldName = "poItemSeqId", relFieldName = "orderItemSeqId")
                }
            )
        }
    )
    public interface OldOrderItemAssociationEntity {}

    /**
     * The OLD Order Item Inventory Reservation
     */
    @Entity(
        name = "OldOrderItemInventoryRes",
        packageName = "org.ofbiz.order.order",
        tableName = "ORDER_ITEM_INVENTORY_RES",
        title = "The OLD Order Item Inventory Reservation",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "inventoryItemId", type = "id-ne"),
            @Field(name = "reserveOrderEnumId", type = "id-ne"),
            @Field(name = "quantity", type = "floating-point"),
            @Field(name = "quantityNotAvailable", type = "floating-point"),
            @Field(name = "reservedDatetime", type = "date-time"),
            @Field(name = "createdDatetime", type = "date-time"),
            @Field(name = "promisedDatetime", type = "date-time"),
            @Field(name = "currentPromisedDate", type = "date-time"),
            @Field(name = "pickStartDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "inventoryItemId")
        },
        relations = {
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
                fkName = "OLDODR_ITIR_OITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "OLDODR_ITIR_INVITM",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface OldOrderItemInventoryResEntity {}

    /**
     * The Order Shipment Preference Entity (Deprecated)
     */
    @Entity(
        name = "OldOrderShipmentPreference",
        packageName = "org.ofbiz.order.order",
        tableName = "ORDER_SHIPMENT_PREFERENCE",
        title = "The Order Shipment Preference Entity (Deprecated)",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "shipmentMethodTypeId", type = "id"),
            @Field(name = "carrierPartyId", type = "id"),
            @Field(name = "carrierRoleTypeId", type = "id"),
            @Field(name = "trackingNumber", type = "short-varchar"),
            @Field(name = "shippingInstructions", type = "long-varchar"),
            @Field(name = "maySplit", type = "indicator"),
            @Field(name = "giftMessage", type = "long-varchar"),
            @Field(name = "isGift", type = "indicator"),
            @Field(name = "shipAfterDate", type = "date-time"),
            @Field(name = "shipBeforeDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CarrierShipmentMethod",
                fkName = "ORDER_SHPREF_CSHM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId"),
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId"),
                    @KeyMap(fieldName = "carrierRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_SHPREF_OHDR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Carrier",
                fkName = "ORDER_SHPREF_CPRTY",
                keyMaps = {
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                title = "Carrier",
                fkName = "ORDER_SHPREF_CPRLE",
                keyMaps = {
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId"),
                    @KeyMap(fieldName = "carrierRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentMethodType",
                fkName = "ORDER_SHPREF_SHMTP",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId")
                }
            )
        }
    )
    public interface OldOrderShipmentPreferenceEntity {}

    /**
     * Old Customer Request Role
     */
    @Entity(
        name = "OldCustRequestRole",
        packageName = "org.ofbiz.order.request",
        tableName = "CUST_REQUEST_ROLE",
        title = "Old Customer Request Role",
        fields = {
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequest",
                fkName = "CUSTREQ_RL_CRQST",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "CUSTREQ_RL_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
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
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "CUSTREQ_RL_PROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface OldCustRequestRoleEntity {}

}

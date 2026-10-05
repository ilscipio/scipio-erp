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
public class ViewEntities {

    /**
     * Order Header And Order Items View
     */
    @ViewEntity(
        name = "OrderHeaderAndItems",
        packageName = "org.ofbiz.order.order",
        title = "Order Header And Order Items View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem")
        },
        aliases = {
            @Alias(name = "orderId", entityAlias = "OH"),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "orderStatusId", entityAlias = "OH", field = "statusId"),
            @Alias(name = "grandTotal", entityAlias = "OH"),
            @Alias(name = "productStoreId", entityAlias = "OH"),
            @Alias(name = "orderTypeId", entityAlias = "OH"),
            @Alias(name = "currencyUom", entityAlias = "OH"),
            @Alias(name = "orderItemSeqId", entityAlias = "OI"),
            @Alias(name = "productId", entityAlias = "OI"),
            @Alias(name = "quantity", entityAlias = "OI"),
            @Alias(name = "cancelQuantity", entityAlias = "OI"),
            @Alias(name = "unitPrice", entityAlias = "OI"),
            @Alias(name = "unitListPrice", entityAlias = "OI"),
            @Alias(name = "itemDescription", entityAlias = "OI"),
            @Alias(name = "itemStatusId", entityAlias = "OI", field = "statusId"),
            @Alias(name = "estimatedShipDate", entityAlias = "OI"),
            @Alias(name = "estimatedDeliveryDate", entityAlias = "OI"),
            @Alias(name = "shipBeforeDate", entityAlias = "OI"),
            @Alias(name = "shipAfterDate", entityAlias = "OI"),
            @Alias(name = "orderItemTypeId", entityAlias = "OI")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface OrderHeaderAndItemsView {}

    /**
     * OrderHeader, OrderItemShipGrpInvRes, OrderItemShipGroup, InventoryItem and FacilityLocation View
     */
    @ViewEntity(
        name = "OrderHeaderAndItemFacilityLocation",
        packageName = "org.ofbiz.order.order",
        title = "OrderHeader, OrderItemShipGrpInvRes, OrderItemShipGroup, InventoryItem and FacilityLocation View",
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OISGIR", entityName = "OrderItemShipGrpInvRes"),
            @MemberEntity(entityAlias = "OISG", entityName = "OrderItemShipGroup"),
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem"),
            @MemberEntity(entityAlias = "FL", entityName = "FacilityLocation")
        },
        aliases = {
            @Alias(name = "orderId", entityAlias = "OH"),
            @Alias(name = "orderItemSeqId", entityAlias = "OISGIR"),
            @Alias(name = "inventoryItemId", entityAlias = "OISGIR"),
            @Alias(name = "shipGroupSeqId", entityAlias = "OISG"),
            @Alias(name = "shipmentMethodTypeId", entityAlias = "OISG"),
            @Alias(name = "carrierPartyId", entityAlias = "OISG"),
            @Alias(name = "productId", entityAlias = "II"),
            @Alias(name = "facilityId", entityAlias = "II"),
            @Alias(name = "locationSeqId", entityAlias = "II"),
            @Alias(name = "locationTypeEnumId", entityAlias = "FL"),
            @Alias(name = "areaId", entityAlias = "FL")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OISGIR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OISGIR",
                relEntityAlias = "OISG",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "OISGIR",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @ViewLink(
                entityAlias = "II",
                relEntityAlias = "FL",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            )
        }
    )
    public interface OrderHeaderAndItemFacilityLocationView {}

    /**
     * Order Header And Payment Preference View
     */
    @ViewEntity(
        name = "OrderHeaderAndPaymentPref",
        packageName = "org.ofbiz.order.order",
        title = "Order Header And Payment Preference View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "PF", entityName = "OrderPaymentPreference"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader")
        },
        aliases = {
            @Alias(name = "orderId", entityAlias = "OH"),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "originFacilityId", entityAlias = "OH"),
            @Alias(name = "productStoreId", entityAlias = "OH"),
            @Alias(name = "terminalId", entityAlias = "OH"),
            @Alias(name = "webSiteId", entityAlias = "OH"),
            @Alias(name = "currencyUom", entityAlias = "OH"),
            @Alias(name = "orderPaymentPreferenceId", entityAlias = "PF"),
            @Alias(name = "paymentMethodTypeId", entityAlias = "PF"),
            @Alias(name = "orderStatusId", entityAlias = "OH", field = "statusId"),
            @Alias(name = "paymentStatusId", entityAlias = "PF", field = "statusId"),
            @Alias(name = "maxAmount", entityAlias = "PF")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "PF",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
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
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderPaymentPreference",
                keyMaps = {
                    @KeyMap(fieldName = "orderPaymentPreferenceId")
                }
            )
        }
    )
    public interface OrderHeaderAndPaymentPrefView {}

    /**
     * Payment Preference and Payment View
     */
    @ViewEntity(
        name = "OrderPaymentPrefAndPayment",
        packageName = "org.ofbiz.order.order",
        title = "Payment Preference and Payment View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "PF", entityName = "OrderPaymentPreference"),
            @MemberEntity(entityAlias = "PMT", entityName = "Payment")
        },
        aliases = {
            @Alias(name = "orderPaymentPreferenceId", entityAlias = "PF"),
            @Alias(name = "orderId", entityAlias = "PF"),
            @Alias(name = "statusId", entityAlias = "PF"),
            @Alias(name = "paymentId", entityAlias = "PMT"),
            @Alias(name = "paymentTypeId", entityAlias = "PMT"),
            @Alias(name = "amount", entityAlias = "PMT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "PF",
                relEntityAlias = "PMT",
                keyMaps = {
                    @KeyMap(fieldName = "orderPaymentPreferenceId", relFieldName = "paymentPreferenceId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderHeader",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface OrderPaymentPrefAndPaymentView {}

    /**
     * Order Header And Roles View
     */
    @ViewEntity(
        name = "OrderHeaderAndRoleSummary",
        packageName = "org.ofbiz.order.order",
        title = "Order Header And Roles View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "ORLE", entityName = "OrderRole"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader")
        },
        aliases = {
            @Alias(name = "partyId", entityAlias = "ORLE", groupBy = true),
            @Alias(name = "roleTypeId", entityAlias = "ORLE", groupBy = true),
            @Alias(name = "orderId", entityAlias = "OH"),
            @Alias(name = "orderTypeId", entityAlias = "OH", groupBy = true),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "statusId", entityAlias = "OH", groupBy = true),
            @Alias(name = "totalGrandAmount", entityAlias = "OH", field = "grandTotal", function = AggregateFunction.SUM),
            @Alias(name = "totalSubRemainingAmount", entityAlias = "OH", field = "remainingSubTotal", function = AggregateFunction.SUM),
            @Alias(name = "totalOrders", entityAlias = "OH", field = "orderId", function = AggregateFunction.COUNT)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "ORLE",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface OrderHeaderAndRoleSummaryView {}

    /**
     * Order Header And Roles View
     */
    @ViewEntity(
        name = "OrderHeaderAndRoles",
        packageName = "org.ofbiz.order.order",
        title = "Order Header And Roles View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "ORLE", entityName = "OrderRole"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "ORLE"),
            @AliasAll(entityAlias = "OH")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "ORLE",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderType",
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Facility",
                title = "Origin",
                keyMaps = {
                    @KeyMap(fieldName = "originFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAdjustment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemBilling",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemRole",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderRole",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderShipment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderStatus",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderTerm",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkOrderItemFulfillment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductOrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "BillingAccount",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderPaymentPreference",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderHeaderNoteView",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "UserLogin",
                keyMaps = {
                    @KeyMap(fieldName = "createdBy", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                title = "Sync",
                keyMaps = {
                    @KeyMap(fieldName = "syncStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderRequirementCommitment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ShipmentReceipt",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface OrderHeaderAndRolesView {}

    /**
     * Order Header And Roles View
     */
    @ViewEntity(
        name = "OrderHeaderItemAndInv",
        packageName = "org.ofbiz.order.order",
        title = "Order Header And Roles View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "IV", entityName = "OrderItemShipGrpInvRes")
        },
        aliases = {
            @Alias(name = "orderId", entityAlias = "OH"),
            @Alias(name = "orderTypeId", entityAlias = "OH"),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "entryDate", entityAlias = "OH"),
            @Alias(name = "visitId", entityAlias = "OH"),
            @Alias(name = "statusId", entityAlias = "OH"),
            @Alias(name = "createdBy", entityAlias = "OH"),
            @Alias(name = "firstAttemptOrderId", entityAlias = "OH"),
            @Alias(name = "currencyUom", entityAlias = "OH"),
            @Alias(name = "syncStatusId", entityAlias = "OH"),
            @Alias(name = "billingAccountId", entityAlias = "OH"),
            @Alias(name = "originFacilityId", entityAlias = "OH"),
            @Alias(name = "productStoreId", entityAlias = "OH"),
            @Alias(name = "webSiteId", entityAlias = "OH"),
            @Alias(name = "grandTotal", entityAlias = "OH"),
            @Alias(name = "remainingSubTotal", entityAlias = "OH"),
            @Alias(name = "productId", entityAlias = "OI"),
            @Alias(name = "quantity", entityAlias = "OI"),
            @Alias(name = "unitPrice", entityAlias = "OI"),
            @Alias(name = "unitListPrice", entityAlias = "OI"),
            @Alias(name = "estimatedShipDate", entityAlias = "OI"),
            @Alias(name = "autoCancelDate", entityAlias = "OI"),
            @Alias(name = "correspondingPoId", entityAlias = "OI"),
            @Alias(name = "quantityNotAvailable", entityAlias = "IV")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "IV",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderType",
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Facility",
                title = "Origin",
                keyMaps = {
                    @KeyMap(fieldName = "originFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAdjustment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemBilling",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemRole",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderRole",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderShipment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderStatus",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderTerm",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkOrderItemFulfillment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductOrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "BillingAccount",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderPaymentPreference",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderHeaderNoteView",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "UserLogin",
                keyMaps = {
                    @KeyMap(fieldName = "createdBy", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                title = "Sync",
                keyMaps = {
                    @KeyMap(fieldName = "syncStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderRequirementCommitment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ShipmentReceipt",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface OrderHeaderItemAndInvView {}

    /**
     * Order Header And Roles View
     */
    @ViewEntity(
        name = "OrderHeaderItemAndInvRoles",
        packageName = "org.ofbiz.order.order",
        title = "Order Header And Roles View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OT", entityName = "OrderRole"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "IV", entityName = "OrderItemShipGrpInvRes")
        },
        aliases = {
            @Alias(name = "orderId", entityAlias = "OH"),
            @Alias(name = "partyId", entityAlias = "OT"),
            @Alias(name = "roleTypeId", entityAlias = "OT"),
            @Alias(name = "orderTypeId", entityAlias = "OH"),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "entryDate", entityAlias = "OH"),
            @Alias(name = "visitId", entityAlias = "OH"),
            @Alias(name = "statusId", entityAlias = "OH"),
            @Alias(name = "createdBy", entityAlias = "OH"),
            @Alias(name = "firstAttemptOrderId", entityAlias = "OH"),
            @Alias(name = "currencyUom", entityAlias = "OH"),
            @Alias(name = "syncStatusId", entityAlias = "OH"),
            @Alias(name = "billingAccountId", entityAlias = "OH"),
            @Alias(name = "originFacilityId", entityAlias = "OH"),
            @Alias(name = "productStoreId", entityAlias = "OH"),
            @Alias(name = "webSiteId", entityAlias = "OH"),
            @Alias(name = "grandTotal", entityAlias = "OH"),
            @Alias(name = "remainingSubTotal", entityAlias = "OH"),
            @Alias(name = "productId", entityAlias = "OI"),
            @Alias(name = "quantity", entityAlias = "OI"),
            @Alias(name = "unitPrice", entityAlias = "OI"),
            @Alias(name = "unitListPrice", entityAlias = "OI"),
            @Alias(name = "estimatedShipDate", entityAlias = "OI"),
            @Alias(name = "autoCancelDate", entityAlias = "OI"),
            @Alias(name = "correspondingPoId", entityAlias = "OI"),
            @Alias(name = "quantityNotAvailable", entityAlias = "IV")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OT",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "IV",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderType",
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Facility",
                title = "Origin",
                keyMaps = {
                    @KeyMap(fieldName = "originFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAdjustment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemBilling",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemRole",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderRole",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderShipment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderStatus",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderTerm",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkOrderItemFulfillment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductOrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "BillingAccount",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderPaymentPreference",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderHeaderNoteView",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "UserLogin",
                keyMaps = {
                    @KeyMap(fieldName = "createdBy", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                title = "Sync",
                keyMaps = {
                    @KeyMap(fieldName = "syncStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderRequirementCommitment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ShipmentReceipt",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface OrderHeaderItemAndInvRolesView {}

    /**
     * Order Header And Roles View
     */
    @ViewEntity(
        name = "OrderHeaderItemAndRoles",
        packageName = "org.ofbiz.order.order",
        title = "Order Header And Roles View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OT", entityName = "OrderRole"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem")
        },
        aliases = {
            @Alias(name = "orderName", entityAlias = "OH"),
            @Alias(name = "orderId", entityAlias = "OH"),
            @Alias(name = "partyId", entityAlias = "OT"),
            @Alias(name = "roleTypeId", entityAlias = "OT"),
            @Alias(name = "orderTypeId", entityAlias = "OH"),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "entryDate", entityAlias = "OH"),
            @Alias(name = "visitId", entityAlias = "OH"),
            @Alias(name = "statusId", entityAlias = "OH"),
            @Alias(name = "createdBy", entityAlias = "OH"),
            @Alias(name = "firstAttemptOrderId", entityAlias = "OH"),
            @Alias(name = "currencyUom", entityAlias = "OH"),
            @Alias(name = "syncStatusId", entityAlias = "OH"),
            @Alias(name = "billingAccountId", entityAlias = "OH"),
            @Alias(name = "originFacilityId", entityAlias = "OH"),
            @Alias(name = "productStoreId", entityAlias = "OH"),
            @Alias(name = "webSiteId", entityAlias = "OH"),
            @Alias(name = "grandTotal", entityAlias = "OH"),
            @Alias(name = "remainingSubTotal", entityAlias = "OH"),
            @Alias(name = "productId", entityAlias = "OI"),
            @Alias(name = "quantity", entityAlias = "OI"),
            @Alias(name = "unitPrice", entityAlias = "OI"),
            @Alias(name = "unitListPrice", entityAlias = "OI"),
            @Alias(name = "estimatedShipDate", entityAlias = "OI"),
            @Alias(name = "autoCancelDate", entityAlias = "OI"),
            @Alias(name = "correspondingPoId", entityAlias = "OI"),
            @Alias(name = "orderItemTypeId", entityAlias = "OI"),
            @Alias(name = "itemDescription", entityAlias = "OI"),
            @Alias(name = "orderItemSeqId", entityAlias = "OI")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OT",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderType",
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Facility",
                title = "Origin",
                keyMaps = {
                    @KeyMap(fieldName = "originFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAdjustment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemBilling",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemRole",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderRole",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderShipment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderStatus",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderTerm",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "WorkOrderItemFulfillment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductOrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "BillingAccount",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderPaymentPreference",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderHeaderNoteView",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "UserLogin",
                keyMaps = {
                    @KeyMap(fieldName = "createdBy", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                title = "Sync",
                keyMaps = {
                    @KeyMap(fieldName = "syncStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderRequirementCommitment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ShipmentReceipt",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface OrderHeaderItemAndRolesView {}

    /**
     * Order Header Note View
     */
    @ViewEntity(
        name = "OrderHeaderNoteView",
        packageName = "org.ofbiz.order.order",
        title = "Order Header Note View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OD", entityName = "OrderHeaderNote"),
            @MemberEntity(entityAlias = "ND", entityName = "NoteData")
        },
        aliases = {
            @Alias(name = "orderId", entityAlias = "OD"),
            @Alias(name = "internalNote", entityAlias = "OD"),
            @Alias(name = "noteId", entityAlias = "ND"),
            @Alias(name = "noteName", entityAlias = "ND"),
            @Alias(name = "noteInfo", entityAlias = "ND"),
            @Alias(name = "noteDateTime", entityAlias = "ND"),
            @Alias(name = "noteParty", entityAlias = "ND")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OD",
                relEntityAlias = "ND",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        }
    )
    public interface OrderHeaderNoteViewView {}

    /**
     * Order Header Note View Full
     */
    @ViewEntity(
        name = "OrderHeaderNoteViewFull",
        packageName = "org.ofbiz.order.order",
        title = "Order Header Note View Full",
        members = {
            @MemberEntity(entityAlias = "OD", entityName = "OrderHeaderNote"),
            @MemberEntity(entityAlias = "ND", entityName = "NoteData")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OD"),
            @AliasAll(entityAlias = "ND")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OD",
                relEntityAlias = "ND",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        }
    )
    public interface OrderHeaderNoteViewFullView {}

    /**
     * OrderItem And ProductContent Info View
     */
    @ViewEntity(
        name = "OrderItemAndProductContentInfo",
        packageName = "org.ofbiz.order.order",
        title = "OrderItem And ProductContent Info View",
        members = {
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "PC", entityName = "ProductContent"),
            @MemberEntity(entityAlias = "CNT", entityName = "Content"),
            @MemberEntity(entityAlias = "PDCT", entityName = "Product")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OI", excludes = {"comments"}),
            @AliasAll(entityAlias = "PC"),
            @AliasAll(entityAlias = "CNT", excludes = {"statusId"}),
            @AliasAll(entityAlias = "PDCT", excludes = {"description", "createdDate", "createdByUserLogin", "lastModifiedDate", "lastModifiedByUserLogin"})
        },
        aliases = {
            @Alias(name = "contentStatusId", entityAlias = "CNT", field = "statusId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "PC",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @ViewLink(
                entityAlias = "PC",
                relEntityAlias = "CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "PDCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface OrderItemAndProductContentInfoView {}

    /**
     * OrderHeader, OrderItem And ShipGroups View
     */
    @ViewEntity(
        name = "OrderHeaderItemAndShipGroup",
        packageName = "org.ofbiz.order.order",
        title = "OrderHeader, OrderItem And ShipGroups View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "OISGA", entityName = "OrderItemShipGroupAssoc"),
            @MemberEntity(entityAlias = "OISG", entityName = "OrderItemShipGroup"),
            @MemberEntity(entityAlias = "OISGIR", entityName = "OrderItemShipGrpInvRes"),
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OH"),
            @AliasAll(entityAlias = "OISGA"),
            @AliasAll(entityAlias = "OISG", excludes = {"facilityId"}),
            @AliasAll(entityAlias = "OI", excludes = {"quantity", "cancelQuantity", "shipAfterDate", "shipBeforeDate", "estimatedShipDate", "estimatedDeliveryDate", "statusId", "externalId", "syncStatusId"})
        },
        aliases = {
            @Alias(name = "oiQuantity", entityAlias = "OI", field = "quantity"),
            @Alias(name = "oiCancelQuantity", entityAlias = "OI", field = "cancelQuantity"),
            @Alias(name = "oiShipAfterDate", entityAlias = "OI", field = "shipAfterDate"),
            @Alias(name = "oiShipBeforeDate", entityAlias = "OI", field = "shipBeforeDate"),
            @Alias(name = "oiEstimatedShipDate", entityAlias = "OI", field = "estimatedShipDate"),
            @Alias(name = "oiEstimatedDeliveryDate", entityAlias = "OI", field = "estimatedDeliveryDate"),
            @Alias(name = "oiStatusId", entityAlias = "OI", field = "statusId"),
            @Alias(name = "oiExternalId", entityAlias = "OI", field = "externalId"),
            @Alias(name = "oiSyncStatusId", entityAlias = "OI", field = "syncStatusId"),
            @Alias(name = "reservedQuantity", entityAlias = "OISGIR", field = "quantity"),
            @Alias(name = "facilityId", entityAlias = "II")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "OISGA",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "OISGA",
                relEntityAlias = "OISG",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "OISGA",
                relEntityAlias = "OISGIR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "OISGIR",
                relEntityAlias = "II",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        }
    )
    public interface OrderHeaderItemAndShipGroupView {}

    /**
     * OrderItem And ShipGroupAssoc View
     */
    @ViewEntity(
        name = "OrderItemAndShipGroupAssoc",
        packageName = "org.ofbiz.order.order",
        title = "OrderItem And ShipGroupAssoc View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "OISGA", entityName = "OrderItemShipGroupAssoc")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OI", excludes = {"quantity", "cancelQuantity"}),
            @AliasAll(entityAlias = "OISGA")
        },
        aliases = {
            @Alias(name = "orderItemQuantity", entityAlias = "OI", field = "quantity"),
            @Alias(name = "orderItemCancelQuantity", entityAlias = "OI", field = "cancelQuantity")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "OISGA",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ItemIssuance",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemShipGrpInvRes",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemShipGrpInvResAndItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemBilling",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAdjustment",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        }
    )
    public interface OrderItemAndShipGroupAssocView {}

    /**
     * Order Item and Inventory Reservation View
     */
    @ViewEntity(
        name = "OrderItemAndShipGrpInvResAndItem",
        packageName = "org.ofbiz.order.order",
        title = "Order Item and Inventory Reservation View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "OISGIR", entityName = "OrderItemShipGrpInvRes"),
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OI"),
            @AliasAll(entityAlias = "OISGIR", excludes = {"quantity"}),
            @AliasAll(entityAlias = "II", excludes = {"productId", "statusId", "comments"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "OISGIR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "OISGIR",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface OrderItemAndShipGrpInvResAndItemView {}

    /**
     * Order Item Inventory Reservation and Inventory Item View
     */
    @ViewEntity(
        name = "OrderItemShipGrpInvResAndItem",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Inventory Reservation and Inventory Item View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OISGIR", entityName = "OrderItemShipGrpInvRes"),
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OISGIR"),
            @AliasAll(entityAlias = "II")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OISGIR",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface OrderItemShipGrpInvResAndItemView {}

    /**
     * Order Item Inventory Reservation and Inventory Item and FacilityLocation View
     */
    @ViewEntity(
        name = "OrderItemShipGrpInvResAndItemLocation",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Inventory Reservation and Inventory Item and FacilityLocation View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OISGIR", entityName = "OrderItemShipGrpInvRes"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem"),
            @MemberEntity(entityAlias = "FL", entityName = "FacilityLocation")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OISGIR"),
            @AliasAll(entityAlias = "II"),
            @AliasAll(entityAlias = "FL")
        },
        aliases = {
            @Alias(name = "orderItemStatusId", entityAlias = "OI", field = "statusId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OISGIR",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            ),
            @ViewLink(
                entityAlias = "II",
                relEntityAlias = "FL",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "OISGIR",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FacilityLocation",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId"),
                    @KeyMap(fieldName = "locationSeqId")
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
            )
        }
    )
    public interface OrderItemShipGrpInvResAndItemLocationView {}

    /**
     * Sum item issuance quantity to use directly in OrderItemQuantityReportGroupByItem and OrderItemQuantityReportGroupByProduct entities
     */
    @ViewEntity(
        name = "ItemIssuanceQuantitySum",
        packageName = "org.ofbiz.order.order",
        title = "Sum item issuance quantity to use directly in OrderItemQuantityReportGroupByItem and OrderItemQuantityReportGroupByProduct entities",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "II", entityName = "ItemIssuance")
        },
        aliases = {
            @Alias(name = "issuedQuantitySum", entityAlias = "II", field = "quantity", function = AggregateFunction.SUM),
            @Alias(name = "orderId", entityAlias = "II", field = "orderId", groupBy = true),
            @Alias(name = "orderItemSeqId", entityAlias = "II", field = "orderItemSeqId", groupBy = true)
        }
    )
    public interface ItemIssuanceQuantitySumView {}

    /**
     * Reports quantity ordered, issued and open by item for OrderItems.
     */
    @ViewEntity(
        name = "OrderItemQuantityReportGroupByItem",
        packageName = "org.ofbiz.order.order",
        title = "Reports quantity ordered, issued and open by item for OrderItems.",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "II", entityName = "ItemIssuanceQuantitySum")
        },
        aliases = {
            @Alias(name = "productStoreId", entityAlias = "OH"),
            @Alias(name = "orderId", entityAlias = "OH", groupBy = true),
            @Alias(name = "orderTypeId", entityAlias = "OH"),
            @Alias(name = "orderStatusId", entityAlias = "OH", field = "statusId"),
            @Alias(name = "orderDate", entityAlias = "OH", groupBy = true),
            @Alias(name = "orderItemSeqId", entityAlias = "OI", groupBy = true),
            @Alias(name = "orderItemStatusId", entityAlias = "OI", field = "statusId"),
            @Alias(name = "productId", entityAlias = "OI", groupBy = true),
            @Alias(name = "itemDescription", entityAlias = "OI", groupBy = true),
            @Alias(name = "shipBeforeDate", entityAlias = "OI", groupBy = true),
            @Alias(name = "shipAfterDate", entityAlias = "OI", groupBy = true),
            @Alias(name = "quantityOrdered", entityAlias = "OI", function = AggregateFunction.SUM,
                complexAlias = @ComplexAlias(operator = "-", fields = {
                    @ComplexAliasField(entityAlias = "OI", field = "quantity", defaultValue = "0"),
                    @ComplexAliasField(entityAlias = "OI", field = "cancelQuantity", defaultValue = "0")
                })),
            @Alias(name = "quantityIssued", entityAlias = "II", field = "issuedQuantitySum", function = AggregateFunction.MIN),
            @Alias(name = "quantityOpen", entityAlias = "OI", function = AggregateFunction.SUM,
                complexAlias = @ComplexAlias(operator = "-", fields = {
                    @ComplexAliasField(entityAlias = "OI", field = "quantity", defaultValue = "0"),
                    @ComplexAliasField(entityAlias = "OI", field = "cancelQuantity", defaultValue = "0"),
                    @ComplexAliasField(entityAlias = "II", field = "issuedQuantitySum", defaultValue = "0")
                }))
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "II",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        }
    )
    public interface OrderItemQuantityReportGroupByItemView {}

    /**
     * Reports quantity ordered, issued and open by product for OrderItems.
     */
    @ViewEntity(
        name = "OrderItemQuantityReportGroupByProduct",
        packageName = "org.ofbiz.order.order",
        title = "Reports quantity ordered, issued and open by product for OrderItems.",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "II", entityName = "ItemIssuanceQuantitySum")
        },
        aliases = {
            @Alias(name = "orderTypeId", entityAlias = "OH", groupBy = true),
            @Alias(name = "orderStatusId", entityAlias = "OH", field = "statusId"),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "orderItemStatusId", entityAlias = "OI", field = "statusId"),
            @Alias(name = "productId", entityAlias = "OI", groupBy = true),
            @Alias(name = "quantityOrdered", entityAlias = "OI", function = AggregateFunction.SUM,
                complexAlias = @ComplexAlias(operator = "-", fields = {
                    @ComplexAliasField(entityAlias = "OI", field = "quantity", defaultValue = "0"),
                    @ComplexAliasField(entityAlias = "OI", field = "cancelQuantity", defaultValue = "0")
                })),
            @Alias(name = "quantityIssued", entityAlias = "II", field = "issuedQuantitySum", function = AggregateFunction.MIN),
            @Alias(name = "quantityOpen", entityAlias = "OI", function = AggregateFunction.SUM,
                complexAlias = @ComplexAlias(operator = "-", fields = {
                    @ComplexAliasField(entityAlias = "OI", field = "quantity", defaultValue = "0"),
                    @ComplexAliasField(entityAlias = "OI", field = "cancelQuantity", defaultValue = "0"),
                    @ComplexAliasField(entityAlias = "II", field = "issuedQuantitySum", defaultValue = "0")
                }))
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "II",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        }
    )
    public interface OrderItemQuantityReportGroupByProductView {}

    /**
     * Order Purchase Payment Summary View
     */
    @ViewEntity(
        name = "OrderPurchasePaymentSummary",
        packageName = "org.ofbiz.order.order",
        title = "Order Purchase Payment Summary View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OPP", entityName = "OrderPaymentPreference"),
            @MemberEntity(entityAlias = "PMT", entityName = "PaymentMethodType")
        },
        aliases = {
            @Alias(name = "webSiteId", entityAlias = "OH", groupBy = true),
            @Alias(name = "productStoreId", entityAlias = "OH", groupBy = true),
            @Alias(name = "originFacilityId", entityAlias = "OH", groupBy = true),
            @Alias(name = "terminalId", entityAlias = "OH", groupBy = true),
            @Alias(name = "statusId", entityAlias = "OH", groupBy = true),
            @Alias(name = "paymentMethodTypeId", entityAlias = "OPP", groupBy = true),
            @Alias(name = "description", entityAlias = "PMT", groupBy = true),
            @Alias(name = "maxAmount", entityAlias = "OPP", function = AggregateFunction.SUM),
            @Alias(name = "orderId", entityAlias = "OH", groupBy = true),
            @Alias(name = "orderTypeId", entityAlias = "OH", groupBy = true),
            @Alias(name = "orderDate", entityAlias = "OH", groupBy = true),
            @Alias(name = "billingAccountId", entityAlias = "OH", groupBy = true),
            @Alias(name = "preferenceStatusId", entityAlias = "OPP", field = "statusId", groupBy = true)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OPP",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OPP",
                relEntityAlias = "PMT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            )
        }
    )
    public interface OrderPurchasePaymentSummaryView {}

    /**
     * Order Purchase Product Summary View
     */
    @ViewEntity(
        name = "OrderPurchaseProductSummary",
        packageName = "org.ofbiz.order.order",
        title = "Order Purchase Product Summary View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "PR", entityName = "Product")
        },
        aliases = {
            @Alias(name = "webSiteId", entityAlias = "OH", groupBy = true),
            @Alias(name = "productStoreId", entityAlias = "OH", groupBy = true),
            @Alias(name = "originFacilityId", entityAlias = "OH", groupBy = true),
            @Alias(name = "terminalId", entityAlias = "OH", groupBy = true),
            @Alias(name = "statusId", entityAlias = "OH", groupBy = true),
            @Alias(name = "productId", entityAlias = "PR", groupBy = true),
            @Alias(name = "internalName", entityAlias = "PR", groupBy = true),
            @Alias(name = "quantity", entityAlias = "OI", function = AggregateFunction.SUM),
            @Alias(name = "cancelQuantity", entityAlias = "OI", function = AggregateFunction.SUM),
            @Alias(name = "unitPrice", entityAlias = "OI", function = AggregateFunction.AVG),
            @Alias(name = "unitListPrice", entityAlias = "OI", function = AggregateFunction.AVG),
            @Alias(name = "orderId", entityAlias = "OH"),
            @Alias(name = "orderTypeId", entityAlias = "OH"),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "itemStatusId", entityAlias = "OI", field = "statusId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface OrderPurchaseProductSummaryView {}

    /**
     * Order Report View
     */
    @ViewEntity(
        name = "OrderReportView",
        packageName = "org.ofbiz.order.order",
        title = "Order Report View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OPP", entityName = "OrderPaymentPreference"),
            @MemberEntity(entityAlias = "PMT", entityName = "PaymentMethodType"),
            @MemberEntity(entityAlias = "OS", entityName = "StatusItem"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "OIS", entityName = "StatusItem")
        },
        aliases = {
            @Alias(name = "groupName", entityAlias = "OS", field = "description"),
            @Alias(name = "orderId", entityAlias = "OH"),
            @Alias(name = "orderTypeId", entityAlias = "OH"),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "orderStatus", entityAlias = "OS", field = "description"),
            @Alias(name = "paymentMethod", entityAlias = "PMT", field = "description"),
            @Alias(name = "visitId", entityAlias = "OH"),
            @Alias(name = "currencyUom", entityAlias = "OH"),
            @Alias(name = "originFacilityId", entityAlias = "OH"),
            @Alias(name = "webSiteId", entityAlias = "OH"),
            @Alias(name = "grandTotal", entityAlias = "OH"),
            @Alias(name = "productId", entityAlias = "OI"),
            @Alias(name = "itemDescription", entityAlias = "OI"),
            @Alias(name = "itemStatus", entityAlias = "OIS", field = "description"),
            @Alias(name = "quantity", entityAlias = "OI"),
            @Alias(name = "unitPrice", entityAlias = "OI")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OPP",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OPP",
                relEntityAlias = "PMT",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "OIS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface OrderReportViewView {}

    /**
     * OrderRole And OrderItem And ProductContent Info View
     */
    @ViewEntity(
        name = "OrderRoleAndProductContentInfo",
        packageName = "org.ofbiz.order.order",
        title = "OrderRole And OrderItem And ProductContent Info View",
        members = {
            @MemberEntity(entityAlias = "ORLE", entityName = "OrderRole"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "PC", entityName = "ProductContent"),
            @MemberEntity(entityAlias = "CNT", entityName = "Content"),
            @MemberEntity(entityAlias = "PDCT", entityName = "Product")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "ORLE"),
            @AliasAll(entityAlias = "OI"),
            @AliasAll(entityAlias = "PC"),
            @AliasAll(entityAlias = "CNT", excludes = {"statusId"})
        },
        aliases = {
            @Alias(name = "productName", entityAlias = "PDCT"),
            @Alias(name = "contentStatusId", entityAlias = "CNT", field = "statusId")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "ORLE",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "PC",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @ViewLink(
                entityAlias = "PC",
                relEntityAlias = "CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "PDCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface OrderRoleAndProductContentInfoView {}

    /**
     * Order WorkEffort Task List
     */
    @ViewEntity(
        name = "OrderTaskList",
        packageName = "org.ofbiz.order.order",
        title = "Order WorkEffort Task List",
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OHR", entityName = "OrderRole"),
            @MemberEntity(entityAlias = "PS", entityName = "Person"),
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "WEPA", entityName = "WorkEffortPartyAssignment")
        },
        aliases = {
            @Alias(name = "orderId", entityAlias = "OH"),
            @Alias(name = "orderTypeId", entityAlias = "OH"),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "entryDate", entityAlias = "OH"),
            @Alias(name = "grandTotal", entityAlias = "OH"),
            @Alias(name = "orderRoleTypeId", entityAlias = "OHR", field = "roleTypeId"),
            @Alias(name = "customerPartyId", entityAlias = "OHR", field = "partyId"),
            @Alias(name = "customerFirstName", entityAlias = "PS", field = "firstName"),
            @Alias(name = "customerLastName", entityAlias = "PS", field = "lastName"),
            @Alias(name = "workEffortId", entityAlias = "WE"),
            @Alias(name = "workEffortTypeId", entityAlias = "WE"),
            @Alias(name = "currentStatusId", entityAlias = "WE"),
            @Alias(name = "lastStatusUpdate", entityAlias = "WE"),
            @Alias(name = "priority", entityAlias = "WE"),
            @Alias(name = "workEffortName", entityAlias = "WE"),
            @Alias(name = "description", entityAlias = "WE"),
            @Alias(name = "createdDate", entityAlias = "WE"),
            @Alias(name = "createdByUserLogin", entityAlias = "WE"),
            @Alias(name = "lastModifiedDate", entityAlias = "WE"),
            @Alias(name = "lastModifiedByUserLogin", entityAlias = "WE"),
            @Alias(name = "estimatedStartDate", entityAlias = "WE"),
            @Alias(name = "estimatedCompletionDate", entityAlias = "WE"),
            @Alias(name = "actualStartDate", entityAlias = "WE"),
            @Alias(name = "actualCompletionDate", entityAlias = "WE"),
            @Alias(name = "infoUrl", entityAlias = "WE"),
            @Alias(name = "wepaPartyId", entityAlias = "WEPA", field = "partyId"),
            @Alias(name = "roleTypeId", entityAlias = "WEPA"),
            @Alias(name = "fromDate", entityAlias = "WEPA"),
            @Alias(name = "thruDate", entityAlias = "WEPA"),
            @Alias(name = "statusId", entityAlias = "WEPA"),
            @Alias(name = "statusDateTime", entityAlias = "WEPA")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OHR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OHR",
                relEntityAlias = "PS",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "WE",
                keyMaps = {
                    @KeyMap(fieldName = "orderId", relFieldName = "sourceReferenceId")
                }
            ),
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "WEPA",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface OrderTaskListView {}

    /**
     * Order Header And Ship Groups
     */
    @ViewEntity(
        name = "OrderHeaderAndShipGroups",
        packageName = "org.ofbiz.order.order",
        title = "Order Header And Ship Groups",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OISG", entityName = "OrderItemShipGroup"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "ORR", entityName = "OrderRole")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OISG"),
            @AliasAll(entityAlias = "PA"),
            @AliasAll(entityAlias = "ORR"),
            @AliasAll(entityAlias = "OH")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OISG",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OISG",
                relEntityAlias = "PA",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "ORR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface OrderHeaderAndShipGroupsView {}

    /**
     * Order Header And Ship Groups By Product
     */
    @ViewEntity(
        name = "OrderHeaderAndShipGroupsByProduct",
        packageName = "org.ofbiz.order.order",
        title = "Order Header And Ship Groups By Product",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OISG", entityName = "OrderItemShipGroup"),
            @MemberEntity(entityAlias = "PA", entityName = "PostalAddress"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "ORR", entityName = "OrderRole"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "PR", entityName = "Product")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OISG"),
            @AliasAll(entityAlias = "PA"),
            @AliasAll(entityAlias = "ORR"),
            @AliasAll(entityAlias = "OH")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "PR", field = "productId"),
            @Alias(name = "brandName", entityAlias = "PR", field = "brandName"),
            @Alias(name = "internalName", entityAlias = "PR", field = "internalName")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OISG",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OISG",
                relEntityAlias = "PA",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "ORR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface OrderHeaderAndShipGroupsByProductView {}

    /**
     * Order Report Group By Product View Entity with extra details for sales order reporting
     */
    @ViewEntity(
        name = "OrderReportSalesGroupByProduct",
        packageName = "org.ofbiz.order.order",
        title = "Order Report Group By Product View Entity with extra details for sales order reporting",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "RL", entityName = "OrderRole"),
            @MemberEntity(entityAlias = "PR", entityName = "Product"),
            @MemberEntity(entityAlias = "PS", entityName = "ProductStore")
        },
        aliases = {
            @Alias(name = "productStoreId", entityAlias = "OH", groupBy = true),
            @Alias(name = "storeName", entityAlias = "PS", groupBy = true),
            @Alias(name = "productId", entityAlias = "OI", groupBy = true),
            @Alias(name = "internalName", entityAlias = "PR", groupBy = true),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "orderStatusId", entityAlias = "OH", field = "statusId"),
            @Alias(name = "orderItemStatusId", entityAlias = "OI", field = "statusId"),
            @Alias(name = "orderTypeId", entityAlias = "OH"),
            @Alias(name = "partyId", entityAlias = "RL"),
            @Alias(name = "roleTypeId", entityAlias = "RL"),
            @Alias(name = "quantityOrdered", entityAlias = "OI", function = AggregateFunction.SUM,
                complexAlias = @ComplexAlias(operator = "-", fields = {
                    @ComplexAliasField(entityAlias = "OI", field = "quantity", defaultValue = "0"),
                    @ComplexAliasField(entityAlias = "OI", field = "cancelQuantity", defaultValue = "0")
                })),
            @Alias(name = "unitPrice", entityAlias = "OI", function = AggregateFunction.SUM)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "RL",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "PS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            )
        }
    )
    public interface OrderReportSalesGroupByProductView {}

    /**
     * Order Report Group By Product View Entity with extra details for purchase order reporting
     */
    @ViewEntity(
        name = "OrderReportPurchasesGroupByProduct",
        packageName = "org.ofbiz.order.order",
        title = "Order Report Group By Product View Entity with extra details for purchase order reporting",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "RT", entityName = "OrderRole"),
            @MemberEntity(entityAlias = "RF", entityName = "OrderRole"),
            @MemberEntity(entityAlias = "PR", entityName = "Product")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "OI", groupBy = true),
            @Alias(name = "internalName", entityAlias = "PR", groupBy = true),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "orderStatusId", entityAlias = "OH", field = "statusId"),
            @Alias(name = "orderItemStatusId", entityAlias = "OI", field = "statusId"),
            @Alias(name = "orderTypeId", entityAlias = "OH"),
            @Alias(name = "toPartyId", entityAlias = "RT", field = "partyId"),
            @Alias(name = "toRoleTypeId", entityAlias = "RT", field = "roleTypeId"),
            @Alias(name = "fromPartyId", entityAlias = "RF", field = "partyId"),
            @Alias(name = "fromRoleTypeId", entityAlias = "RF", field = "roleTypeId"),
            @Alias(name = "quantity", entityAlias = "OI", function = AggregateFunction.SUM),
            @Alias(name = "unitPrice", entityAlias = "OI", function = AggregateFunction.SUM)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "RT",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "RF",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface OrderReportPurchasesGroupByProductView {}

    /**
     * Basic product quantity and price report with ability to select based on order type, status, date and order item status
     */
    @ViewEntity(
        name = "OrderReportGroupByProduct",
        packageName = "org.ofbiz.order.order",
        title = "Basic product quantity and price report with ability to select based on order type, status, date and order item status",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem")
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "OI", groupBy = true),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "orderStatusId", entityAlias = "OH", field = "statusId"),
            @Alias(name = "orderItemStatusId", entityAlias = "OI", field = "statusId"),
            @Alias(name = "orderTypeId", entityAlias = "OH"),
            @Alias(name = "quantity", entityAlias = "OI", function = AggregateFunction.SUM),
            @Alias(name = "unitPrice", entityAlias = "OI", function = AggregateFunction.SUM)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OH",
                relEntityAlias = "OI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface OrderReportGroupByProductView {}

    /**
     * Customer Request And Role View
     * See CustRequest for descriptions of date fields
     */
    @ViewEntity(
        name = "CustRequestAndRole",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request And Role View",
        description = "See CustRequest for descriptions of date fields",
        members = {
            @MemberEntity(entityAlias = "CR", entityName = "CustRequest"),
            @MemberEntity(entityAlias = "CRP", entityName = "CustRequestParty")
        },
        aliases = {
            @Alias(name = "custRequestId", entityAlias = "CR"),
            @Alias(name = "custRequestTypeId", entityAlias = "CR"),
            @Alias(name = "statusId", entityAlias = "CR"),
            @Alias(name = "fromPartyId", entityAlias = "CR"),
            @Alias(name = "priority", entityAlias = "CR"),
            @Alias(name = "custRequestDate", entityAlias = "CR"),
            @Alias(name = "responseRequiredDate", entityAlias = "CR"),
            @Alias(name = "custRequestName", entityAlias = "CR"),
            @Alias(name = "description", entityAlias = "CR"),
            @Alias(name = "createdDate", entityAlias = "CR"),
            @Alias(name = "lastModifiedDate", entityAlias = "CR"),
            @Alias(name = "lastModifiedByUserLogin", entityAlias = "CR"),
            @Alias(name = "partyId", entityAlias = "CRP"),
            @Alias(name = "roleTypeId", entityAlias = "CRP"),
            @Alias(name = "fromDate", entityAlias = "CRP"),
            @Alias(name = "thruDate", entityAlias = "CRP")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CR",
                relEntityAlias = "CRP",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CustRequest",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CustRequestParty",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Person",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface CustRequestAndRoleView {}

    /**
     * Customer Request And Role View
     */
    @ViewEntity(
        name = "CustReqAndTypeAndPartyRel",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request And Role View",
        members = {
            @MemberEntity(entityAlias = "CR", entityName = "CustRequest"),
            @MemberEntity(entityAlias = "CRT", entityName = "CustRequestType"),
            @MemberEntity(entityAlias = "PR", entityName = "PartyRelationship")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CR")
        },
        aliases = {
            @Alias(name = "typeDescription", entityAlias = "CRT", field = "description"),
            @Alias(name = "partyIdFrom", entityAlias = "PR"),
            @Alias(name = "roleTypeIdFrom", entityAlias = "PR"),
            @Alias(name = "partyIdTo", entityAlias = "PR"),
            @Alias(name = "roleTypeIdTo", entityAlias = "PR"),
            @Alias(name = "fromDate", entityAlias = "PR"),
            @Alias(name = "thruDate", entityAlias = "PR"),
            @Alias(name = "relStatusId", entityAlias = "PR", field = "statusId"),
            @Alias(name = "partyRelationshipTypeId", entityAlias = "PR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CR",
                relEntityAlias = "CRT",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestTypeId")
                }
            ),
            @ViewLink(
                entityAlias = "CRT",
                relEntityAlias = "PR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "partyId", relFieldName = "partyIdFrom")
                }
            )
        }
    )
    public interface CustReqAndTypeAndPartyRelView {}

    /**
     * Customer Request And CommunicationEvent
     */
    @ViewEntity(
        name = "CustRequestAndCommEvent",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request And CommunicationEvent",
        members = {
            @MemberEntity(entityAlias = "CR", entityName = "CustRequest"),
            @MemberEntity(entityAlias = "CRC", entityName = "CustRequestCommEvent")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CR")
        },
        aliases = {
            @Alias(name = "communicationEventId", entityAlias = "CRC")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CR",
                relEntityAlias = "CRC",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CustRequest",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CommunicationEvent",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
        }
    )
    public interface CustRequestAndCommEventView {}

    /**
     * Customer Request And WorkEffort
     */
    @ViewEntity(
        name = "CustRequestAndWorkEffort",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request And WorkEffort",
        members = {
            @MemberEntity(entityAlias = "CRW", entityName = "CustRequestWorkEffort"),
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CRW", excludes = {"workEffortId"}),
            @AliasAll(entityAlias = "WE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CRW",
                relEntityAlias = "WE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CustRequest",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface CustRequestAndWorkEffortView {}

    /**
     * Customer Request And Note
     */
    @ViewEntity(
        name = "CustRequestAndNote",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request And Note",
        members = {
            @MemberEntity(entityAlias = "CRN", entityName = "CustRequestNote"),
            @MemberEntity(entityAlias = "ND", entityName = "NoteData")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CRN", excludes = {"noteId"}),
            @AliasAll(entityAlias = "ND")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CRN",
                relEntityAlias = "ND",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CustRequest",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            )
        }
    )
    public interface CustRequestAndNoteView {}

    /**
     * Customer Request And WorkEffort
     */
    @ViewEntity(
        name = "CustRequestInfoAndWorkEffortAndPartyRel",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request And WorkEffort",
        members = {
            @MemberEntity(entityAlias = "CR", entityName = "CustRequest"),
            @MemberEntity(entityAlias = "CRW", entityName = "CustRequestWorkEffort"),
            @MemberEntity(entityAlias = "PR", entityName = "PartyRelationship")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CR"),
            @AliasAll(entityAlias = "CRW", excludes = {"custRequestId"})
        },
        aliases = {
            @Alias(name = "partyIdFrom", entityAlias = "PR"),
            @Alias(name = "roleTypeIdFrom", entityAlias = "PR"),
            @Alias(name = "partyIdTo", entityAlias = "PR"),
            @Alias(name = "roleTypeIdTo", entityAlias = "PR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CR",
                relEntityAlias = "CRW",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @ViewLink(
                entityAlias = "CR",
                relEntityAlias = "PR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "fromPartyId", relFieldName = "partyIdTo")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface CustRequestInfoAndWorkEffortAndPartyRelView {}

    /**
     * Customer Request and Note View
     */
    @ViewEntity(
        name = "CustRequestNoteView",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request and Note View",
        members = {
            @MemberEntity(entityAlias = "PR", entityName = "Person"),
            @MemberEntity(entityAlias = "CR", entityName = "CustRequestNote"),
            @MemberEntity(entityAlias = "ND", entityName = "NoteData")
        },
        aliases = {
            @Alias(name = "custRequestId", entityAlias = "CR"),
            @Alias(name = "noteId", entityAlias = "ND"),
            @Alias(name = "noteName", entityAlias = "ND"),
            @Alias(name = "noteInfo", entityAlias = "ND"),
            @Alias(name = "noteDateTime", entityAlias = "ND"),
            @Alias(name = "noteParty", entityAlias = "PR", field = "partyId"),
            @Alias(name = "firstName", entityAlias = "PR"),
            @Alias(name = "lastName", entityAlias = "PR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CR",
                relEntityAlias = "ND",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            ),
            @ViewLink(
                entityAlias = "ND",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "noteParty", relFieldName = "partyId")
                }
            )
        }
    )
    public interface CustRequestNoteViewView {}

    /**
     * Customer Request Item and Note View
     */
    @ViewEntity(
        name = "CustRequestItemNoteView",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request Item and Note View",
        members = {
            @MemberEntity(entityAlias = "PR", entityName = "Person"),
            @MemberEntity(entityAlias = "CR", entityName = "CustRequestItemNote"),
            @MemberEntity(entityAlias = "ND", entityName = "NoteData")
        },
        aliases = {
            @Alias(name = "custRequestId", entityAlias = "CR"),
            @Alias(name = "custRequestItemSeqId", entityAlias = "CR"),
            @Alias(name = "noteId", entityAlias = "ND"),
            @Alias(name = "noteName", entityAlias = "ND"),
            @Alias(name = "noteInfo", entityAlias = "ND"),
            @Alias(name = "noteDateTime", entityAlias = "ND"),
            @Alias(name = "partyId", entityAlias = "PR"),
            @Alias(name = "firstName", entityAlias = "PR"),
            @Alias(name = "lastName", entityAlias = "PR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CR",
                relEntityAlias = "ND",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            ),
            @ViewLink(
                entityAlias = "ND",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "noteParty", relFieldName = "partyId")
                }
            )
        }
    )
    public interface CustRequestItemNoteViewView {}

    /**
     * Quote Note View
     */
    @ViewEntity(
        name = "QuoteNoteView",
        packageName = "org.ofbiz.order.quote",
        title = "Quote Note View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "QD", entityName = "QuoteNote"),
            @MemberEntity(entityAlias = "ND", entityName = "NoteData")
        },
        aliases = {
            @Alias(name = "quoteId", entityAlias = "QD"),
            @Alias(name = "noteId", entityAlias = "ND"),
            @Alias(name = "noteName", entityAlias = "ND"),
            @Alias(name = "noteInfo", entityAlias = "ND"),
            @Alias(name = "noteDateTime", entityAlias = "ND"),
            @Alias(name = "noteParty", entityAlias = "ND")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "QD",
                relEntityAlias = "ND",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        }
    )
    public interface QuoteNoteViewView {}

    /**
     * Quote And Workeffort
     * Shows workefforts for a Quote
     */
    @ViewEntity(
        name = "QuoteWorkEffortView",
        packageName = "org.ofbiz.order.quote",
        title = "Quote And Workeffort",
        description = "Shows workefforts for a Quote",
        members = {
            @MemberEntity(entityAlias = "QWE", entityName = "QuoteWorkEffort"),
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "SI", entityName = "StatusItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "QWE"),
            @AliasAll(entityAlias = "WE")
        },
        aliases = {
            @Alias(name = "statusItemDescription", entityAlias = "SI", field = "description")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "QWE",
                relEntityAlias = "WE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "SI",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "currentStatusId", relFieldName = "statusId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "WorkEffort",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Quote",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "StatusItem",
                keyMaps = {
                    @KeyMap(fieldName = "currentStatusId", relFieldName = "statusId")
                }
            )
        }
    )
    public interface QuoteWorkEffortViewView {}

    /**
     * Requirement And Role View
     */
    @ViewEntity(
        name = "RequirementAndRole",
        packageName = "org.ofbiz.order.request",
        title = "Requirement And Role View",
        members = {
            @MemberEntity(entityAlias = "RQ", entityName = "Requirement"),
            @MemberEntity(entityAlias = "RQR", entityName = "RequirementRole")
        },
        aliases = {
            @Alias(name = "requirementId", entityAlias = "RQ"),
            @Alias(name = "requirementTypeId", entityAlias = "RQ"),
            @Alias(name = "statusId", entityAlias = "RQ"),
            @Alias(name = "facilityId", entityAlias = "RQ"),
            @Alias(name = "deliverableId", entityAlias = "RQ"),
            @Alias(name = "fixedAssetId", entityAlias = "RQ"),
            @Alias(name = "productId", entityAlias = "RQ"),
            @Alias(name = "description", entityAlias = "RQ"),
            @Alias(name = "requirementStartDate", entityAlias = "RQ"),
            @Alias(name = "requiredByDate", entityAlias = "RQ"),
            @Alias(name = "estimatedBudget", entityAlias = "RQ"),
            @Alias(name = "quantity", entityAlias = "RQ"),
            @Alias(name = "reason", entityAlias = "RQ"),
            @Alias(name = "lastModifiedDate", entityAlias = "RQ"),
            @Alias(name = "lastModifiedByUserLogin", entityAlias = "RQ"),
            @Alias(name = "partyId", entityAlias = "RQR"),
            @Alias(name = "roleTypeId", entityAlias = "RQR"),
            @Alias(name = "fromDate", entityAlias = "RQR"),
            @Alias(name = "thruDate", entityAlias = "RQR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "RQ",
                relEntityAlias = "RQR",
                relOptional = true,
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Requirement",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "RequirementRole",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId"),
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId"),
                    @KeyMap(fieldName = "fromDate")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Party",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Person",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "RoleType",
                keyMaps = {
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface RequirementAndRoleView {}

    /**
     * Requirement Customer Request View
     */
    @ViewEntity(
        name = "RequirementCustRequestView",
        packageName = "org.ofbiz.order.request",
        title = "Requirement Customer Request View",
        members = {
            @MemberEntity(entityAlias = "RR", entityName = "RequirementCustRequest"),
            @MemberEntity(entityAlias = "RQ", entityName = "Requirement"),
            @MemberEntity(entityAlias = "RI", entityName = "CustRequestItem")
        },
        aliases = {
            @Alias(name = "custRequestId", entityAlias = "RR"),
            @Alias(name = "custRequestItemSeqId", entityAlias = "RR"),
            @Alias(name = "requirementId", entityAlias = "RR"),
            @Alias(name = "requirementTypeId", entityAlias = "RQ"),
            @Alias(name = "description", entityAlias = "RQ"),
            @Alias(name = "productId", entityAlias = "RQ"),
            @Alias(name = "estimatedBudget", entityAlias = "RQ"),
            @Alias(name = "quantity", entityAlias = "RQ"),
            @Alias(name = "requirementStartDate", entityAlias = "RQ"),
            @Alias(name = "requiredByDate", entityAlias = "RQ"),
            @Alias(name = "statusId", entityAlias = "RI"),
            @Alias(name = "priority", entityAlias = "RI"),
            @Alias(name = "maximumAmount", entityAlias = "RI")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "RR",
                relEntityAlias = "RQ",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            ),
            @ViewLink(
                entityAlias = "RR",
                relEntityAlias = "RI",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId"),
                    @KeyMap(fieldName = "custRequestItemSeqId")
                }
            )
        }
    )
    public interface RequirementCustRequestViewView {}

    /**
     * Sum of Requirement entity quantities, grouped by productId, facilityId, statusId
     */
    @ViewEntity(
        name = "RequirementByProductFacility",
        packageName = "org.ofbiz.order.request",
        title = "Sum of Requirement entity quantities, grouped by productId, facilityId, statusId",
        members = {
            @MemberEntity(entityAlias = "RQ", entityName = "Requirement")
        },
        aliases = {
            @Alias(name = "statusId", entityAlias = "RQ", groupBy = true),
            @Alias(name = "productId", entityAlias = "RQ", groupBy = true),
            @Alias(name = "facilityId", entityAlias = "RQ", groupBy = true),
            @Alias(name = "quantity", entityAlias = "RQ", function = AggregateFunction.SUM)
        }
    )
    public interface RequirementByProductFacilityView {}

    /**
     * A join on Requirement and RequirementRole to count number of distinct products required from a supplier party
     */
    @ViewEntity(
        name = "RequirementPartyProductCount",
        packageName = "org.ofbiz.order.request",
        title = "A join on Requirement and RequirementRole to count number of distinct products required from a supplier party",
        members = {
            @MemberEntity(entityAlias = "RQ", entityName = "Requirement"),
            @MemberEntity(entityAlias = "RQR", entityName = "RequirementRole")
        },
        aliases = {
            @Alias(name = "requirementTypeId", entityAlias = "RQ"),
            @Alias(name = "statusId", entityAlias = "RQ"),
            @Alias(name = "productId", entityAlias = "RQ", function = AggregateFunction.COUNT_DISTINCT),
            @Alias(name = "partyId", entityAlias = "RQR", groupBy = true),
            @Alias(name = "roleTypeId", entityAlias = "RQR"),
            @Alias(name = "fromDate", entityAlias = "RQR"),
            @Alias(name = "thruDate", entityAlias = "RQR")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "RQ",
                relEntityAlias = "RQR",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            )
        }
    )
    public interface RequirementPartyProductCountView {}

    /**
     * Customer Request And Content View
     * Show Content of CustRequest
     */
    @ViewEntity(
        name = "CustRequestAndContent",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request And Content View",
        description = "Show Content of CustRequest",
        members = {
            @MemberEntity(entityAlias = "CRC", entityName = "CustRequestContent"),
            @MemberEntity(entityAlias = "CT", entityName = "Content")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CRC", excludes = {"contentId"}),
            @AliasAll(entityAlias = "CT")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CRC",
                relEntityAlias = "CT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface CustRequestAndContentView {}

    /**
     * Communication Event And Order View
     */
    @ViewEntity(
        name = "CommunicationEventAndOrder",
        packageName = "org.ofbiz.order.communication",
        title = "Communication Event And Order View",
        members = {
            @MemberEntity(entityAlias = "CO", entityName = "CommunicationEventOrder"),
            @MemberEntity(entityAlias = "CE", entityName = "CommunicationEvent")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CO"),
            @AliasAll(entityAlias = "CE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CO",
                relEntityAlias = "CE",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
        }
    )
    public interface CommunicationEventAndOrderView {}

    /**
     * Communication Event And Customer request
     */
    @ViewEntity(
        name = "CommunicationEventAndCustRequest",
        packageName = "org.ofbiz.order.communication",
        title = "Communication Event And Customer request",
        members = {
            @MemberEntity(entityAlias = "CR", entityName = "CustRequestCommEvent"),
            @MemberEntity(entityAlias = "CE", entityName = "CommunicationEvent")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "CR"),
            @AliasAll(entityAlias = "CE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "CR",
                relEntityAlias = "CE",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
        }
    )
    public interface CommunicationEventAndCustRequestView {}

    /**
     * Order Item and Inventory Reservation View
     */
    @ViewEntity(
        name = "OrderItemAndShipGrpInvResAndItemSum",
        packageName = "org.ofbiz.order.order",
        title = "Order Item and Inventory Reservation View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "OISGIR", entityName = "OrderItemShipGrpInvRes"),
            @MemberEntity(entityAlias = "II", entityName = "InventoryItem")
        },
        aliases = {
            @Alias(name = "orderId", entityAlias = "OI", groupBy = true),
            @Alias(name = "orderItemSeqId", entityAlias = "OI", groupBy = true),
            @Alias(name = "productId", entityAlias = "OI", groupBy = true),
            @Alias(name = "inventoryProductId", entityAlias = "II", field = "productId", groupBy = true),
            @Alias(name = "shipGroupSeqId", entityAlias = "OISGIR", groupBy = true),
            @Alias(name = "quantityOrdered", entityAlias = "OI", function = AggregateFunction.SUM,
                complexAlias = @ComplexAlias(operator = "-", fields = {
                    @ComplexAliasField(entityAlias = "OI", field = "quantity", defaultValue = "0"),
                    @ComplexAliasField(entityAlias = "OI", field = "cancelQuantity", defaultValue = "0")
                })),
            @Alias(name = "totQuantityReserved", entityAlias = "OISGIR", field = "quantity", function = AggregateFunction.SUM),
            @Alias(name = "totQuantityNotAvailable", entityAlias = "OISGIR", field = "quantityNotAvailable", function = AggregateFunction.SUM),
            @Alias(name = "totQuantityAvailable", entityAlias = "OISGIR", function = AggregateFunction.SUM,
                complexAlias = @ComplexAlias(operator = "-", fields = {
                    @ComplexAliasField(entityAlias = "OISGIR", field = "quantity", defaultValue = "0"),
                    @ComplexAliasField(entityAlias = "OISGIR", field = "quantityNotAvailable", defaultValue = "0")
                }))
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "OISGIR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @ViewLink(
                entityAlias = "OISGIR",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface OrderItemAndShipGrpInvResAndItemSumView {}

    /**
     * Order Header And Work Effort View
     */
    @ViewEntity(
        name = "OrderHeaderAndWorkEffort",
        packageName = "org.ofbiz.order.order",
        title = "Order Header And Work Effort View",
        members = {
            @MemberEntity(entityAlias = "WE", entityName = "WorkEffort"),
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OHWE", entityName = "OrderHeaderWorkEffort")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "WE"),
            @AliasAll(entityAlias = "OH", excludes = {"priority"}),
            @AliasAll(entityAlias = "OHWE")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "WE",
                relEntityAlias = "OHWE",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @ViewLink(
                entityAlias = "OHWE",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface OrderHeaderAndWorkEffortView {}

    /**
     * OrderItemBilling and Invoice and InvoiceItem View
     */
    @ViewEntity(
        name = "OrderItemBillingAndInvoiceAndItem",
        packageName = "org.ofbiz.order.order",
        title = "OrderItemBilling and Invoice and InvoiceItem View",
        members = {
            @MemberEntity(entityAlias = "OIB", entityName = "OrderItemBilling"),
            @MemberEntity(entityAlias = "INV", entityName = "Invoice"),
            @MemberEntity(entityAlias = "II", entityName = "InvoiceItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OIB")
        },
        aliases = {
            @Alias(name = "statusId", entityAlias = "INV")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OIB",
                relEntityAlias = "INV",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @ViewLink(
                entityAlias = "OIB",
                relEntityAlias = "II",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            )
        },
        relations = {
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
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            )
        }
    )
    public interface OrderItemBillingAndInvoiceAndItemView {}

    /**
     * OrderItem And Product View
     */
    @ViewEntity(
        name = "OrderItemAndProduct",
        packageName = "org.ofbiz.order.order",
        title = "OrderItem And Product View",
        neverCache = true,
        members = {
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem"),
            @MemberEntity(entityAlias = "PR", entityName = "Product")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OI"),
            @AliasAll(entityAlias = "PR", excludes = {"comments"})
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "PR",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Product",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface OrderItemAndProductView {}

    /**
     * Order Statistics
     */
    @ViewEntity(
        name = "OrderStats",
        packageName = "com.ilscipio.scipio.ce.order.stats",
        title = "Order Statistics",
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader")
        },
        aliases = {
            @Alias(name = "productStoreId", entityAlias = "OH", groupBy = true),
            @Alias(name = "webSiteId", entityAlias = "OH", groupBy = true),
            @Alias(name = "terminalId", entityAlias = "OH", groupBy = true),
            @Alias(name = "salesChannelEnumId", entityAlias = "OH", groupBy = true),
            @Alias(name = "orderTypeId", entityAlias = "OH", groupBy = true),
            @Alias(name = "orderDate", entityAlias = "OH"),
            @Alias(name = "statusId", entityAlias = "OH", groupBy = true),
            @Alias(name = "totalGrandAmount", entityAlias = "OH", field = "grandTotal", function = AggregateFunction.SUM),
            @Alias(name = "totalSubRemainingAmount", entityAlias = "OH", field = "remainingSubTotal", function = AggregateFunction.SUM),
            @Alias(name = "totalOrders", entityAlias = "OH", field = "orderId", function = AggregateFunction.COUNT)
        }
    )
    public interface OrderStatsView {}

    /**
     * Return item and reason stats
     */
    @ViewEntity(
        name = "ReturnItemStats",
        packageName = "com.ilscipio.scipio.ce.order.stats",
        title = "Return item and reason stats",
        members = {
            @MemberEntity(entityAlias = "RI", entityName = "ReturnItem")
        },
        aliases = {
            @Alias(name = "statusId", entityAlias = "RI", groupBy = true),
            @Alias(name = "returnTypeId", entityAlias = "RI", groupBy = true),
            @Alias(name = "returnReasonId", entityAlias = "RI", groupBy = true),
            @Alias(name = "returnItemTypeId", entityAlias = "RI", groupBy = true),
            @Alias(name = "totalReturnValue", entityAlias = "RI", field = "returnPrice", function = AggregateFunction.SUM),
            @Alias(name = "totalQuantity", entityAlias = "RI", field = "returnQuantity", function = AggregateFunction.SUM),
            @Alias(name = "lastUpdatedStamp", entityAlias = "RI", groupBy = true),
            @Alias(name = "day", entityAlias = "RI", field = "lastUpdatedStamp", groupBy = true, function = AggregateFunction.EXTRACT_DAY),
            @Alias(name = "month", entityAlias = "RI", field = "lastUpdatedStamp", groupBy = true, function = AggregateFunction.EXTRACT_MONTH),
            @Alias(name = "year", entityAlias = "RI", field = "lastUpdatedStamp", groupBy = true, function = AggregateFunction.EXTRACT_YEAR)
        }
    )
    public interface ReturnItemStatsView {}

    /**
     * Best Selling Products By Quantity Ordered
     */
    @ViewEntity(
        name = "BestSellingProductsByQuantityOrdered",
        packageName = "com.ilscipio.scipio.ce.order.stats",
        title = "Best Selling Products By Quantity Ordered",
        neverCache = true,
        aliasColumns = "true",
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OH", select = "false"),
            @AliasAll(entityAlias = "OI", select = "false", excludes = {"productId"})
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "OI", groupBy = true),
            @Alias(name = "quantityOrdered", entityAlias = "OI", function = AggregateFunction.SUM,
                complexAlias = @ComplexAlias(operator = "-", fields = {
                    @ComplexAliasField(entityAlias = "OI", field = "quantity", defaultValue = "0"),
                    @ComplexAliasField(entityAlias = "OI", field = "cancelQuantity", defaultValue = "0")
                }))
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface BestSellingProductsByQuantityOrderedView {}

    /**
     * Best Selling Products By Sales Total
     */
    @ViewEntity(
        name = "BestSellingProductsBySalesTotal",
        packageName = "com.ilscipio.scipio.ce.order.stats",
        title = "Best Selling Products By Sales Total",
        neverCache = true,
        aliasColumns = "true",
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OH", select = "false"),
            @AliasAll(entityAlias = "OI", select = "false", excludes = {"productId"})
        },
        aliases = {
            @Alias(name = "orderDate", entityAlias = "OH", select = "false"),
            @Alias(name = "productId", entityAlias = "OI", groupBy = true),
            @Alias(name = "salesTotal", entityAlias = "OI", function = AggregateFunction.SUM,
                complexAlias = @ComplexAlias(operator = "*",
                    fields = { @ComplexAliasField(entityAlias = "OI", field = "unitPrice", defaultValue = "0") },
                    nested = { @NestedComplexAlias(operator = "-", fields = {
                        @ComplexAliasField(entityAlias = "OI", field = "quantity", defaultValue = "0"),
                        @ComplexAliasField(entityAlias = "OI", field = "cancelQuantity", defaultValue = "0")
                    }) }
                ))
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface BestSellingProductsBySalesTotalView {}

    /**
     * Best Selling Products By Order Item Count
     */
    @ViewEntity(
        name = "BestSellingProductsByOrderItemCount",
        packageName = "com.ilscipio.scipio.ce.order.stats",
        title = "Best Selling Products By Order Item Count",
        neverCache = true,
        aliasColumns = "true",
        members = {
            @MemberEntity(entityAlias = "OH", entityName = "OrderHeader"),
            @MemberEntity(entityAlias = "OI", entityName = "OrderItem")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OH", select = "false"),
            @AliasAll(entityAlias = "OI", select = "false", excludes = {"productId"})
        },
        aliases = {
            @Alias(name = "productId", entityAlias = "OI", groupBy = true),
            @Alias(name = "orderItemCount", entityAlias = "OI", field = "orderItemSeqId", function = AggregateFunction.COUNT)
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OI",
                relEntityAlias = "OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface BestSellingProductsByOrderItemCountView {}

}

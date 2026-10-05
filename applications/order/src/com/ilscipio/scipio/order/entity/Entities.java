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
public class Entities {

    /**
     * Order Adjustment
     * Note that both includeInTax and includeInShipping should default to true, except in the case where this adjustment is a tax or shipping adjustment then should be ignored.
     */
    @Entity(
        name = "OrderAdjustment",
        packageName = "org.ofbiz.order.order",
        title = "Order Adjustment",
        description = "Note that both includeInTax and includeInShipping should default to true, except in the case where this adjustment is a tax or shipping adjustment then should be ignored.",
        neverCache = true,
        fields = {
            @Field(name = "orderAdjustmentId", type = "id-ne"),
            @Field(name = "orderAdjustmentTypeId", type = "id"),
            @Field(name = "orderId", type = "id"),
            @Field(name = "orderItemSeqId", type = "id"),
            @Field(name = "shipGroupSeqId", type = "id"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "description", type = "description"),
            @Field(name = "amount", type = "currency-precise"),
            @Field(name = "recurringAmount", type = "currency-precise"),
            @Field(name = "amountAlreadyIncluded", type = "currency-precise", description = "The amount here is already represented in the price, such as VAT taxes."),
            @Field(name = "productPromoId", type = "id"),
            @Field(name = "productPromoRuleId", type = "id"),
            @Field(name = "productPromoActionSeqId", type = "id"),
            @Field(name = "productFeatureId", type = "id"),
            @Field(name = "correspondingProductId", type = "id"),
            @Field(name = "taxAuthorityRateSeqId", type = "id-ne"),
            @Field(name = "sourceReferenceId", type = "id-long"),
            @Field(name = "sourcePercentage", type = "fixed-point", description = "for tax entries this is the tax percentage"),
            @Field(name = "customerReferenceId", type = "id-long", description = "for tax entries this is partyTaxId"),
            @Field(name = "primaryGeoId", type = "id", description = "for tax entries this is the primary jurisdiction Geo (the smallest or most local Geo that this tax is for, usually a state/province, perhaps a county or a city)"),
            @Field(name = "secondaryGeoId", type = "id", description = "for tax entries this is the secondary jurisdiction Geo (usually a country, or other Geo that the primary is within)"),
            @Field(name = "exemptAmount", type = "currency-precise", description = "an amount that would normally apply, but not to this order; for tax exemption represents the what the tax would have been"),
            @Field(name = "taxAuthGeoId", type = "id", description = "these taxAuth fields deprecate the primaryGeoId and secondaryGeoId fields and will be used with the newer tax calc stuff"),
            @Field(name = "taxAuthPartyId", type = "id"),
            @Field(name = "overrideGlAccountId", type = "id", description = "used to specify the override or actual glAccountId used for the adjustment, avoids problems if configuration changes after initial posting, etc"),
            @Field(name = "includeInTax", type = "indicator"),
            @Field(name = "includeInShipping", type = "indicator"),
            @Field(name = "isManual", type = "indicator"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong"),
            @Field(name = "originalAdjustmentId", type = "id", description = "specifies relation to source OrderAdjustment - eg. for tax on shipping charges"),
            @Field(name = "oldAmountPerQuantity", type = "currency-amount", colName = "AMOUNT_PER_QUANTITY"),
            @Field(name = "oldPercentage", type = "floating-point", colName = "PERCENTAGE")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderAdjustmentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderAdjustmentType",
                fkName = "ORDER_ADJ_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "orderAdjustmentTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAdjustmentTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "orderAdjustmentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_ADJ_OHEAD",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "ORDER_ADJ_USERL",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
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
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGroup",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGroupAssoc",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "ORDER_ADJ_PROMO",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductPromoRule",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId"),
                    @KeyMap(fieldName = "productPromoRuleId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductPromoAction",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId"),
                    @KeyMap(fieldName = "productPromoRuleId"),
                    @KeyMap(fieldName = "productPromoActionSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "Primary",
                fkName = "ORDER_ADJ_PRGEO",
                keyMaps = {
                    @KeyMap(fieldName = "primaryGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "Secondary",
                fkName = "ORDER_ADJ_SCGEO",
                keyMaps = {
                    @KeyMap(fieldName = "secondaryGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthority",
                fkName = "ORDER_ADJ_TXA",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId"),
                    @KeyMap(fieldName = "taxAuthPartyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "Override",
                fkName = "ORDER_ADJ_OGLA",
                keyMaps = {
                    @KeyMap(fieldName = "overrideGlAccountId", relFieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthorityRateProduct",
                fkName = "ORDER_ADJ_TARP",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthorityRateSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderAdjustment",
                fkName = "ORDER_ADJ_OA",
                keyMaps = {
                    @KeyMap(fieldName = "originalAdjustmentId", relFieldName = "orderAdjustmentId")
                }
            )
        }
    )
    public interface OrderAdjustmentEntity {}

    /**
     * Order Adjustment Attribute
     */
    @Entity(
        name = "OrderAdjustmentAttribute",
        packageName = "org.ofbiz.order.order",
        title = "Order Adjustment Attribute",
        fields = {
            @Field(name = "orderAdjustmentId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderAdjustmentId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderAdjustment",
                fkName = "ORDER_ADJ_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "orderAdjustmentId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAdjustmentTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface OrderAdjustmentAttributeEntity {}

    /**
     * Order Adjustment Type
     */
    @Entity(
        name = "OrderAdjustmentType",
        packageName = "org.ofbiz.order.order",
        title = "Order Adjustment Type",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "orderAdjustmentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderAdjustmentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderAdjustmentType",
                title = "Parent",
                fkName = "ORDER_ADJ_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "orderAdjustmentTypeId")
                }
            )
        }
    )
    public interface OrderAdjustmentTypeEntity {}

    /**
     * Order Adjustment Billing
     */
    @Entity(
        name = "OrderAdjustmentBilling",
        packageName = "org.ofbiz.order.order",
        title = "Order Adjustment Billing",
        neverCache = true,
        fields = {
            @Field(name = "orderAdjustmentId", type = "id-ne"),
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "invoiceItemSeqId", type = "id-ne"),
            @Field(name = "amount", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderAdjustmentId"),
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "invoiceItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderAdjustment",
                fkName = "ORDER_ADJBLNG_OA",
                keyMaps = {
                    @KeyMap(fieldName = "orderAdjustmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Invoice",
                fkName = "ORDER_ADJBLNG_INV",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItem",
                fkName = "ORDER_ADJBLNG_IITM",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            )
        }
    )
    public interface OrderAdjustmentBillingEntity {}

    /**
     * Order Adjustment Type Attribute
     */
    @Entity(
        name = "OrderAdjustmentTypeAttr",
        packageName = "org.ofbiz.order.order",
        title = "Order Adjustment Type Attribute",
        fields = {
            @Field(name = "orderAdjustmentTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderAdjustmentTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderAdjustmentType",
                fkName = "ORDER_ADJ_TYPATTR",
                keyMaps = {
                    @KeyMap(fieldName = "orderAdjustmentTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAdjustmentAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAdjustment",
                keyMaps = {
                    @KeyMap(fieldName = "orderAdjustmentTypeId")
                }
            )
        }
    )
    public interface OrderAdjustmentTypeAttrEntity {}

    /**
     * Order Attribute
     */
    @Entity(
        name = "OrderAttribute",
        packageName = "org.ofbiz.order.order",
        title = "Order Attribute",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_ATTR_HDR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface OrderAttributeEntity {}

    /**
     * Order Blacklist
     */
    @Entity(
        name = "OrderBlacklist",
        packageName = "org.ofbiz.order.order",
        title = "Order Blacklist",
        fields = {
            @Field(name = "blacklistString", type = "long-varchar"),
            @Field(name = "orderBlacklistTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "blacklistString"),
            @PrimaryKey(field = "orderBlacklistTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderBlacklistType",
                fkName = "ORDER_BKL_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "orderBlacklistTypeId")
                }
            )
        }
    )
    public interface OrderBlacklistEntity {}

    /**
     * Order Blacklist Type
     */
    @Entity(
        name = "OrderBlacklistType",
        packageName = "org.ofbiz.order.order",
        title = "Order Blacklist Type",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "orderBlacklistTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderBlacklistTypeId")
        }
    )
    public interface OrderBlacklistTypeEntity {}

    /**
     * Communication Event Order
     */
    @Entity(
        name = "CommunicationEventOrder",
        packageName = "org.ofbiz.order.order",
        title = "Communication Event Order",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "communicationEventId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "communicationEventId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "COMEV_ORDER_ORDER",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEvent",
                fkName = "COMEV_ORDER_CMEV",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
        }
    )
    public interface CommunicationEventOrderEntity {}

    /**
     * Order Contact Mechanism
     */
    @Entity(
        name = "OrderContactMech",
        packageName = "org.ofbiz.order.order",
        title = "Order Contact Mechanism",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "contactMechPurposeTypeId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "contactMechPurposeTypeId"),
            @PrimaryKey(field = "contactMechId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_CMECH_HDR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "ORDER_CMECH_CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechPurposeType",
                fkName = "ORDER_CMECH_CMPT",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechPurposeTypeId")
                }
            )
        }
    )
    public interface OrderContactMechEntity {}

    /**
     * Order Data Object
     */
    @Entity(
        name = "OrderContent",
        packageName = "org.ofbiz.order.order",
        title = "Order Data Object",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "orderContentTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "orderContentTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORD_CNT_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "ORD_CNT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderContentType",
                fkName = "ORD_CNT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "orderContentTypeId")
                }
            )
        }
    )
    public interface OrderContentEntity {}

    /**
     * Order Content Type
     */
    @Entity(
        name = "OrderContentType",
        packageName = "org.ofbiz.order.order",
        title = "Order Content Type",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "orderContentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderContentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderContentType",
                title = "Parent",
                fkName = "ORDCT_TYP_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "orderContentTypeId")
                }
            )
        }
    )
    public interface OrderContentTypeEntity {}

    /**
     * The Order Delivery Schedule
     */
    @Entity(
        name = "OrderDeliverySchedule",
        packageName = "org.ofbiz.order.order",
        title = "The Order Delivery Schedule",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "estimatedReadyDate", type = "date-time"),
            @Field(name = "cartons", type = "numeric"),
            @Field(name = "skidsPallets", type = "numeric"),
            @Field(name = "unitsPieces", type = "fixed-point"),
            @Field(name = "totalCubicSize", type = "fixed-point"),
            @Field(name = "totalCubicUomId", type = "id"),
            @Field(name = "totalWeight", type = "fixed-point"),
            @Field(name = "totalWeightUomId", type = "id"),
            @Field(name = "statusId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_DELSCH_OHDR",
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
                relEntityName = "Uom",
                title = "TotalCubic",
                fkName = "ORDER_DELSCH_TCUOM",
                keyMaps = {
                    @KeyMap(fieldName = "totalCubicUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "TotalWeight",
                fkName = "ORDER_DELSCH_TWUOM",
                keyMaps = {
                    @KeyMap(fieldName = "totalWeightUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "ORDER_DELSCH_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface OrderDeliveryScheduleEntity {}

    /**
     * Order Header
     */
    @Entity(
        name = "OrderHeader",
        packageName = "org.ofbiz.order.order",
        title = "Order Header",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderTypeId", type = "id"),
            @Field(name = "orderName", type = "name"),
            @Field(name = "externalId", type = "id"),
            @Field(name = "salesChannelEnumId", type = "id"),
            @Field(name = "orderDate", type = "date-time"),
            @Field(name = "priority", type = "indicator", description = "Sets priority for Inventory Reservation"),
            @Field(name = "entryDate", type = "date-time"),
            @Field(name = "pickSheetPrintedDate", type = "date-time", description = "This will be set to a date when pick sheet of the order is printed"),
            @Field(name = "visitId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "createdBy", type = "id-vlong"),
            @Field(name = "firstAttemptOrderId", type = "id"),
            @Field(name = "currencyUom", type = "id"),
            @Field(name = "syncStatusId", type = "id"),
            @Field(name = "billingAccountId", type = "id"),
            @Field(name = "originFacilityId", type = "id"),
            @Field(name = "webSiteId", type = "id"),
            @Field(name = "productStoreId", type = "id"),
            @Field(name = "terminalId", type = "id-long"),
            @Field(name = "transactionId", type = "id-long"),
            @Field(name = "autoOrderShoppingListId", type = "id"),
            @Field(name = "needsInventoryIssuance", type = "indicator"),
            @Field(name = "isRushOrder", type = "indicator"),
            @Field(name = "internalCode", type = "id-long"),
            @Field(name = "remainingSubTotal", type = "currency-amount"),
            @Field(name = "grandTotal", type = "currency-amount"),
            @Field(name = "isViewed", type = "indicator"),
            @Field(name = "invoicePerShipment", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderType",
                fkName = "ORDER_HDR_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "SalesChannel",
                fkName = "ORDER_HDR_SCENUM",
                keyMaps = {
                    @KeyMap(fieldName = "salesChannelEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                title = "Origin",
                fkName = "ORDER_HDR_OFAC",
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
                type = RelationType.ONE,
                relEntityName = "BillingAccount",
                fkName = "ORDER_HDR_BACCT",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "ORDER_HDR_PDSTR",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShoppingList",
                title = "AutoOrder",
                fkName = "ORDER_HDR_AOSHLST",
                keyMaps = {
                    @KeyMap(fieldName = "autoOrderShoppingListId", relFieldName = "shoppingListId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "CreatedBy",
                fkName = "ORDER_HDR_CBUL",
                keyMaps = {
                    @KeyMap(fieldName = "createdBy", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "ORDER_HDR_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "Sync",
                fkName = "ORDER_HDR_SYST",
                keyMaps = {
                    @KeyMap(fieldName = "syncStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "ORDER_HDR_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUom", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WebSite",
                fkName = "ORDER_HDR_WS",
                keyMaps = {
                    @KeyMap(fieldName = "webSiteId")
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
                type = RelationType.MANY,
                relEntityName = "OrderItemAndShipGroupAssoc",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        },
        indexes = {
            @Index(
                name = "ORDEREXT_ID_IDX",
                fields = {
                    @IndexField(name = "externalId")
                }
            )
        }
    )
    public interface OrderHeaderEntity {}

    /**
     * Order Header Note
     */
    @Entity(
        name = "OrderHeaderNote",
        packageName = "org.ofbiz.order.order",
        title = "Order Header Note",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "noteId", type = "id-ne"),
            @Field(name = "internalNote", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "noteId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_HDRNT_HDR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "NoteData",
                fkName = "ORDER_HDRNT_NOTE",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        }
    )
    public interface OrderHeaderNoteEntity {}

    /**
     * OrderHeader WorkEffort
     */
    @Entity(
        name = "OrderHeaderWorkEffort",
        packageName = "org.ofbiz.order.order",
        title = "OrderHeader WorkEffort",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "workEffortId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "workEffortId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDERHDWE_OH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "ORDERHDWE_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface OrderHeaderWorkEffortEntity {}

    /**
     * Order Item
     */
    @Entity(
        name = "OrderItem",
        packageName = "org.ofbiz.order.order",
        title = "Order Item",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "externalId", type = "id"),
            @Field(name = "orderItemTypeId", type = "id-ne"),
            @Field(name = "orderItemGroupSeqId", type = "id-ne"),
            @Field(name = "isItemGroupPrimary", type = "indicator"),
            @Field(name = "fromInventoryItemId", type = "id"),
            @Field(name = "budgetId", type = "id"),
            @Field(name = "budgetItemSeqId", type = "id"),
            @Field(name = "productId", type = "id"),
            @Field(name = "supplierProductId", type = "id-long"),
            @Field(name = "productFeatureId", type = "id"),
            @Field(name = "prodCatalogId", type = "id"),
            @Field(name = "productCategoryId", type = "id"),
            @Field(name = "isPromo", type = "indicator"),
            @Field(name = "quoteId", type = "id"),
            @Field(name = "quoteItemSeqId", type = "id"),
            @Field(name = "shoppingListId", type = "id"),
            @Field(name = "shoppingListItemSeqId", type = "id"),
            @Field(name = "subscriptionId", type = "id"),
            @Field(name = "deploymentId", type = "id"),
            @Field(name = "quantity", type = "fixed-point", enableAuditLog = true),
            @Field(name = "cancelQuantity", type = "fixed-point"),
            @Field(name = "selectedAmount", type = "fixed-point"),
            @Field(name = "unitPrice", type = "currency-precise", enableAuditLog = true),
            @Field(name = "unitListPrice", type = "currency-precise"),
            @Field(name = "unitAverageCost", type = "currency-amount"),
            @Field(name = "unitRecurringPrice", type = "currency-amount"),
            @Field(name = "isModifiedPrice", type = "indicator"),
            @Field(name = "recurringFreqUomId", type = "id"),
            @Field(name = "itemDescription", type = "description"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "correspondingPoId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "syncStatusId", type = "id"),
            @Field(name = "estimatedShipDate", type = "date-time"),
            @Field(name = "estimatedDeliveryDate", type = "date-time"),
            @Field(name = "autoCancelDate", type = "date-time"),
            @Field(name = "dontCancelSetDate", type = "date-time"),
            @Field(name = "dontCancelSetUserLogin", type = "id-vlong"),
            @Field(name = "shipBeforeDate", type = "date-time"),
            @Field(name = "shipAfterDate", type = "date-time"),
            @Field(name = "cancelBackOrderDate", type = "date-time", description = "Used to cancel all orders from suppliers when its in past"),
            @Field(name = "overrideGlAccountId", type = "id", description = "Used to specify the override or actual glAccountId used for the adjustment, avoids problems if configuration changes after initial posting, etc."),
            @Field(name = "salesOpportunityId", type = "id-ne"),
            @Field(name = "changeByUserLoginId", type = "id-vlong", enableAuditLog = true)
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_ITEM_HDR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItemType",
                fkName = "ORDER_ITEM_ORTYP",
                keyMaps = {
                    @KeyMap(fieldName = "orderItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItemGroup",
                fkName = "ORDER_ITEM_ITGRP",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "orderItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "ORDER_ITEM_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                title = "From",
                fkName = "ORDER_ITEM_FMINV",
                keyMaps = {
                    @KeyMap(fieldName = "fromInventoryItemId", relFieldName = "inventoryItemId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "RecurringFreq",
                fkName = "ORDER_ITEM_RFUOM",
                keyMaps = {
                    @KeyMap(fieldName = "recurringFreqUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "ORDER_ITEM_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductFacilityLocation",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "StatusValidChange",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "Sync",
                fkName = "ORDER_ITEM_SYST",
                keyMaps = {
                    @KeyMap(fieldName = "syncStatusId", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "DontCancelSet",
                fkName = "ORDER_ITEM_DCUL",
                keyMaps = {
                    @KeyMap(fieldName = "dontCancelSetUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "QuoteItem",
                fkName = "ORDER_ITEM_QUIT",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId"),
                    @KeyMap(fieldName = "quoteItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ShoppingListItem",
                fkName = "ORDER_ITEM_SLI",
                keyMaps = {
                    @KeyMap(fieldName = "shoppingListId"),
                    @KeyMap(fieldName = "shoppingListItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "Override",
                fkName = "ORDER_ITEM_OGLA",
                keyMaps = {
                    @KeyMap(fieldName = "overrideGlAccountId", relFieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SalesOpportunity",
                fkName = "ORDER_ITEM_SLSOPP",
                keyMaps = {
                    @KeyMap(fieldName = "salesOpportunityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "ChangeBy",
                fkName = "ORDER_ITEM_USRLGN",
                keyMaps = {
                    @KeyMap(fieldName = "changeByUserLoginId", relFieldName = "userLoginId")
                }
            )
        },
        indexes = {
            @Index(
                name = "ORDITMEXT_ID_IDX",
                fields = {
                    @IndexField(name = "externalId")
                }
            )
        }
    )
    public interface OrderItemEntity {}

    /**
     * Order Item Assoc
     */
    @Entity(
        name = "OrderItemAssoc",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Assoc",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "shipGroupSeqId", type = "id-ne"),
            @Field(name = "toOrderId", type = "id-ne"),
            @Field(name = "toOrderItemSeqId", type = "id-ne"),
            @Field(name = "toShipGroupSeqId", type = "id-ne"),
            @Field(name = "orderItemAssocTypeId", type = "id-ne"),
            @Field(name = "quantity", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "shipGroupSeqId"),
            @PrimaryKey(field = "toOrderId"),
            @PrimaryKey(field = "toOrderItemSeqId"),
            @PrimaryKey(field = "toShipGroupSeqId"),
            @PrimaryKey(field = "orderItemAssocTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItemAssocType",
                fkName = "ORDER_ITASS_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "orderItemAssocTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                title = "From",
                fkName = "ORDER_ITASS_FRHD",
                keyMaps = {
                    @KeyMap(fieldName = "orderId", relFieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItem",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGroupAssoc",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGroup",
                title = "From",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                title = "To",
                fkName = "ORDER_ITASS_TOHD",
                keyMaps = {
                    @KeyMap(fieldName = "toOrderId", relFieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItem",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "toOrderId", relFieldName = "orderId"),
                    @KeyMap(fieldName = "toOrderItemSeqId", relFieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGroupAssoc",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "toOrderId", relFieldName = "orderId"),
                    @KeyMap(fieldName = "toOrderItemSeqId", relFieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "toShipGroupSeqId", relFieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGroup",
                title = "To",
                keyMaps = {
                    @KeyMap(fieldName = "toOrderId", relFieldName = "orderId"),
                    @KeyMap(fieldName = "toShipGroupSeqId", relFieldName = "shipGroupSeqId")
                }
            )
        }
    )
    public interface OrderItemAssocEntity {}

    /**
     * Order Item Assoc Type
     */
    @Entity(
        name = "OrderItemAssocType",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Assoc Type",
        fields = {
            @Field(name = "orderItemAssocTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderItemAssocTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItemAssocType",
                title = "Parent",
                fkName = "ORDER_ITAS_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "orderItemAssocTypeId")
                }
            )
        }
    )
    public interface OrderItemAssocTypeEntity {}

    /**
     * Order Item Attribute
     */
    @Entity(
        name = "OrderItemAttribute",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Attribute",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "ORDER_ITEM_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface OrderItemAttributeEntity {}

    /**
     * Order Item Billing
     */
    @Entity(
        name = "OrderItemBilling",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Billing",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "invoiceItemSeqId", type = "id-ne"),
            @Field(name = "itemIssuanceId", type = "id"),
            @Field(name = "shipmentReceiptId", type = "id"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "amount", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "invoiceItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_ITBLNG_OHDR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "ORDER_ITBLNG_OITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Invoice",
                fkName = "ORDER_ITBLNG_INV",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItem",
                fkName = "ORDER_ITBLNG_IITM",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentReceipt",
                fkName = "ORDER_ITBL_SHIPRCP",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentReceiptId", relFieldName = "receiptId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ItemIssuance",
                fkName = "ORDER_ITBLNG_IISS",
                keyMaps = {
                    @KeyMap(fieldName = "itemIssuanceId")
                }
            )
        }
    )
    public interface OrderItemBillingEntity {}

    /**
     * Order Item Change
     */
    @Entity(
        name = "OrderItemChange",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Change",
        neverCache = true,
        fields = {
            @Field(name = "orderItemChangeId", type = "id-ne"),
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "changeTypeEnumId", type = "id"),
            @Field(name = "changeDatetime", type = "date-time"),
            @Field(name = "changeUserLogin", type = "id-vlong"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "cancelQuantity", type = "fixed-point"),
            @Field(name = "unitPrice", type = "currency-amount"),
            @Field(name = "itemDescription", type = "description"),
            @Field(name = "reasonEnumId", type = "id"),
            @Field(name = "changeComments", type = "comment")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderItemChangeId")
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
                fkName = "ORDER_ITCH_OITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "ORDER_ITCH_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "changeTypeEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "Reason",
                fkName = "ORDER_ITCH_REAS",
                keyMaps = {
                    @KeyMap(fieldName = "reasonEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "ORDER_ITCH_USER",
                keyMaps = {
                    @KeyMap(fieldName = "changeUserLogin", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface OrderItemChangeEntity {}

    /**
     * Order Item Contact Mechanism
     */
    @Entity(
        name = "OrderItemContactMech",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Contact Mechanism",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "contactMechPurposeTypeId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "contactMechPurposeTypeId")
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
                fkName = "ORDER_ITCM_OITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "ORDER_ITCM_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechPurposeType",
                fkName = "ORDER_ITCM_CMPT",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechPurposeTypeId")
                }
            )
        }
    )
    public interface OrderItemContactMechEntity {}

    /**
     * Order Item Group
     */
    @Entity(
        name = "OrderItemGroup",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Group",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemGroupSeqId", type = "id-ne"),
            @Field(name = "parentGroupSeqId", type = "id"),
            @Field(name = "groupName", type = "name")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemGroupSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDERITMGRP_HDR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItemGroup",
                title = "Parent",
                fkName = "ORDERITMGRP_PGRP",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "parentGroupSeqId", relFieldName = "orderItemGroupSeqId")
                }
            )
        }
    )
    public interface OrderItemGroupEntity {}

    /**
     * Order Item Group Order
     */
    @Entity(
        name = "OrderItemGroupOrder",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Group Order",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "groupOrderId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "groupOrderId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "OIGO_ORDER_ITEM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductGroupOrder",
                fkName = "OIGO_GROUP_ORDER",
                keyMaps = {
                    @KeyMap(fieldName = "groupOrderId")
                }
            )
        }
    )
    public interface OrderItemGroupOrderEntity {}

    /**
     * Order Item Price Info
     */
    @Entity(
        name = "OrderItemPriceInfo",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Price Info",
        neverCache = true,
        fields = {
            @Field(name = "orderItemPriceInfoId", type = "id-ne"),
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "productPriceRuleId", type = "id"),
            @Field(name = "productPriceActionSeqId", type = "id"),
            @Field(name = "modifyAmount", type = "currency-precise"),
            @Field(name = "description", type = "description"),
            @Field(name = "rateCode", type = "short-varchar")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderItemPriceInfoId")
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
                fkName = "ORDER_OIPI_OITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductPriceRule",
                keyMaps = {
                    @KeyMap(fieldName = "productPriceRuleId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPriceAction",
                fkName = "ORDER_OIPI_PRAI",
                keyMaps = {
                    @KeyMap(fieldName = "productPriceRuleId"),
                    @KeyMap(fieldName = "productPriceActionSeqId")
                }
            )
        }
    )
    public interface OrderItemPriceInfoEntity {}

    /**
     * Order Item Role
     */
    @Entity(
        name = "OrderItemRole",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Role",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_ITRL_OHDR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "ORDER_ITRL_OITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "ORDER_ITRL_PARTY",
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
                fkName = "ORDER_ITRL_PTRLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface OrderItemRoleEntity {}

    /**
     * Order Item Ship Group
     */
    @Entity(
        name = "OrderItemShipGroup",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Ship Group",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "shipGroupSeqId", type = "id-ne"),
            @Field(name = "shipmentMethodTypeId", type = "id", enableAuditLog = true),
            @Field(name = "supplierPartyId", type = "id"),
            @Field(name = "vendorPartyId", type = "id", description = "For use with multi-vendor stores, order will be split so that each ship group is associated with only one vendor (only if applicable)"),
            @Field(name = "carrierPartyId", type = "id", enableAuditLog = true),
            @Field(name = "carrierRoleTypeId", type = "id"),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "contactMechId", type = "id"),
            @Field(name = "telecomContactMechId", type = "id"),
            @Field(name = "trackingNumber", type = "short-varchar"),
            @Field(name = "shippingInstructions", type = "long-varchar"),
            @Field(name = "maySplit", type = "indicator"),
            @Field(name = "giftMessage", type = "long-varchar"),
            @Field(name = "isGift", type = "indicator"),
            @Field(name = "shipAfterDate", type = "date-time"),
            @Field(name = "shipByDate", type = "date-time"),
            @Field(name = "estimatedShipDate", type = "date-time"),
            @Field(name = "estimatedDeliveryDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "shipGroupSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_ITSG_ORDH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Supplier",
                fkName = "ORDER_ITSG_SPRTY",
                keyMaps = {
                    @KeyMap(fieldName = "supplierPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Vendor",
                fkName = "ORDER_ITSG_VPRTY",
                keyMaps = {
                    @KeyMap(fieldName = "vendorPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CarrierShipmentMethod",
                fkName = "ORDER_ITSG_CSHM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId"),
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId"),
                    @KeyMap(fieldName = "carrierRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "Carrier",
                fkName = "ORDER_ITSG_CPRTY",
                keyMaps = {
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                title = "Carrier",
                fkName = "ORDER_ITSG_CPRLE",
                keyMaps = {
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId"),
                    @KeyMap(fieldName = "carrierRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "ORDER_ITSG_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentMethodType",
                fkName = "ORDER_ITSG_SHMTP",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "ORDER_ITSG_CNTM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PostalAddress",
                fkName = "ORDER_ITSG_PADR",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                title = "Telecom",
                fkName = "ORDER_ITSG_TCNT",
                keyMaps = {
                    @KeyMap(fieldName = "telecomContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TelecomNumber",
                title = "Telecom",
                fkName = "ORDER_ITSG_TCNB",
                keyMaps = {
                    @KeyMap(fieldName = "telecomContactMechId", relFieldName = "contactMechId")
                }
            )
        }
    )
    public interface OrderItemShipGroupEntity {}

    /**
     * Order Item Package Association
     */
    @Entity(
        name = "OrderItemShipGroupAssoc",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Package Association",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "shipGroupSeqId", type = "id-ne"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "cancelQuantity", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "shipGroupSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_ISGA_ORDH",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "ORDER_ISGA_ORDI",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItemShipGroup",
                fkName = "ORDER_ISGA_OISG",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            )
        }
    )
    public interface OrderItemShipGroupAssocEntity {}

    /**
     * Order Item Inventory Reservation
     */
    @Entity(
        name = "OrderItemShipGrpInvRes",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Inventory Reservation",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "shipGroupSeqId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "inventoryItemId", type = "id-ne"),
            @Field(name = "reserveOrderEnumId", type = "id-ne"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "quantityNotAvailable", type = "fixed-point"),
            @Field(name = "reservedDatetime", type = "date-time"),
            @Field(name = "createdDatetime", type = "date-time"),
            @Field(name = "promisedDatetime", type = "date-time"),
            @Field(name = "currentPromisedDate", type = "date-time"),
            @Field(name = "priority", type = "indicator", description = "Sets priority for Inventory Reservation"),
            @Field(name = "sequenceId", type = "numeric"),
            @Field(name = "oldPickStartDate", type = "date-time", colName = "PICK_START_DATE")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "shipGroupSeqId"),
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
                fkName = "ORDER_ITIR_OITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGroup",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGroupAssoc",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InventoryItem",
                fkName = "ORDER_ITIR_INVITM",
                keyMaps = {
                    @KeyMap(fieldName = "inventoryItemId")
                }
            )
        }
    )
    public interface OrderItemShipGrpInvResEntity {}

    /**
     * Order Item Type
     */
    @Entity(
        name = "OrderItemType",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Type",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "orderItemTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderItemTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItemType",
                title = "Parent",
                fkName = "ORDER_ITEM_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "orderItemTypeId")
                }
            )
        }
    )
    public interface OrderItemTypeEntity {}

    /**
     * Order Item Type Attribute
     */
    @Entity(
        name = "OrderItemTypeAttr",
        packageName = "org.ofbiz.order.order",
        title = "Order Item Type Attribute",
        fields = {
            @Field(name = "orderItemTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderItemTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItemType",
                fkName = "ORDER_ITEM_TYPATR",
                keyMaps = {
                    @KeyMap(fieldName = "orderItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderItemTypeId")
                }
            )
        }
    )
    public interface OrderItemTypeAttrEntity {}

    /**
     * Order Notification
     */
    @Entity(
        name = "OrderNotification",
        packageName = "org.ofbiz.order.order",
        title = "Order Notification",
        neverCache = true,
        fields = {
            @Field(name = "orderNotificationId", type = "id-ne"),
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "emailType", type = "id-ne"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "notificationDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderNotificationId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORD_NOTIFY_ORDHDR",
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
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                fkName = "ORD_NOTIFY_ENUM",
                keyMaps = {
                    @KeyMap(fieldName = "emailType", relFieldName = "enumId")
                }
            )
        }
    )
    public interface OrderNotificationEntity {}

    /**
     * The Order Payment Preference
     */
    @Entity(
        name = "OrderPaymentPreference",
        packageName = "org.ofbiz.order.order",
        title = "The Order Payment Preference",
        neverCache = true,
        fields = {
            @Field(name = "orderPaymentPreferenceId", type = "id-ne"),
            @Field(name = "orderId", type = "id"),
            @Field(name = "orderItemSeqId", type = "id"),
            @Field(name = "shipGroupSeqId", type = "id"),
            @Field(name = "productPricePurposeId", type = "id"),
            @Field(name = "paymentMethodTypeId", type = "id"),
            @Field(name = "paymentMethodId", type = "id"),
            @Field(name = "finAccountId", type = "id", description = "For paying with a fin account instead of payment method on file"),
            @Field(name = "securityCode", type = "long-varchar", description = "NOTE: THIS SHOULD NEVER BE PERSISTED OUTSIDE THE SCOPE OF A SINGLE TRANSACTION,\n              TYPICALLY ONLY FOR AUTHORIZATION PURPOSES, SHOULD BE REMOVED IMMEDIATELY FOLLOWING USE;\n              this is the 3 digit on back (for Visa, MC, etc) or 4 digit on front (Amex, etc) card\n              verification code; also note that this field is longer than needed to accommodate encryption.\n          ", encrypt = "true"),
            @Field(name = "track2", type = "long-varchar", description = "NOTE: THIS SHOULD NEVER BE PERSISTED OUTSIDE THE SCOPE OF A SINGLE TRANSACTION,\n              TYPICALLY ONLY FOR AUTHORIZATION PURPOSES, SHOULD BE REMOVED IMMEDIATELY FOLLOWING USE;\n              this is raw track2 data, exactly as read by the magnetic swipe reader;\n              also note that this field is longer than needed to accommodate encryption.\n          ", encrypt = "true"),
            @Field(name = "presentFlag", type = "indicator"),
            @Field(name = "swipedFlag", type = "indicator"),
            @Field(name = "overflowFlag", type = "indicator"),
            @Field(name = "maxAmount", type = "currency-amount"),
            @Field(name = "processAttempt", type = "numeric"),
            @Field(name = "billingPostalCode", type = "short-varchar"),
            @Field(name = "manualAuthCode", type = "short-varchar"),
            @Field(name = "manualRefNum", type = "short-varchar"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "needsNsfRetry", type = "indicator"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderPaymentPreferenceId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_PMPRF_OHDR",
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
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGroup",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPricePurpose",
                fkName = "ORDER_PMPRF_PPRP",
                keyMaps = {
                    @KeyMap(fieldName = "productPricePurposeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethodType",
                fkName = "ORDER_PMPRF_PMTP",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethod",
                fkName = "ORDER_PMPRF_PMETH",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccount",
                fkName = "ORDER_PMPRF_FINACT",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "ORDER_PMPRF_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "ORDER_PMPRF_USRL",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CreditCard",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "EftAccount",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "GiftCard",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            )
        },
        indexes = {
            @Index(
                name = "NSF_RETRY_CHECK",
                fields = {
                    @IndexField(name = "needsNsfRetry")
                }
            )
        }
    )
    public interface OrderPaymentPreferenceEntity {}

    /**
     * Order Product Promo Code
     */
    @Entity(
        name = "OrderProductPromoCode",
        packageName = "org.ofbiz.order.order",
        title = "Order Product Promo Code",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "productPromoCodeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "productPromoCodeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_PPCD_ORD",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromoCode",
                fkName = "ORDER_PPCD_PPC",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoCodeId")
                }
            )
        }
    )
    public interface OrderProductPromoCodeEntity {}

    /**
     * Order Role
     */
    @Entity(
        name = "OrderRole",
        packageName = "org.ofbiz.order.order",
        title = "Order Role",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_ROLE_OHDR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "ORDER_ROLE_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "ORDER_ROLE_PROLE",
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
                type = RelationType.MANY,
                relEntityName = "OrderItem",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            )
        }
    )
    public interface OrderRoleEntity {}

    /**
     * Order Shipment
     */
    @Entity(
        name = "OrderShipment",
        packageName = "org.ofbiz.order.order",
        title = "Order Shipment",
        neverCache = true,
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "shipGroupSeqId", type = "id-ne"),
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "shipmentItemSeqId", type = "id-ne"),
            @Field(name = "quantity", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "shipGroupSeqId"),
            @PrimaryKey(field = "shipmentId"),
            @PrimaryKey(field = "shipmentItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_SHPMT_OHDR",
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
                relEntityName = "Shipment",
                fkName = "ORDER_SHPMT_SHPMT",
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
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGroupAssoc",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            )
        }
    )
    public interface OrderShipmentEntity {}

    /**
     * Order Status
     */
    @Entity(
        name = "OrderStatus",
        packageName = "org.ofbiz.order.order",
        title = "Order Status",
        neverCache = true,
        fields = {
            @Field(name = "orderStatusId", type = "id-ne"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "orderId", type = "id"),
            @Field(name = "orderItemSeqId", type = "id"),
            @Field(name = "orderPaymentPreferenceId", type = "id"),
            @Field(name = "statusDatetime", type = "date-time"),
            @Field(name = "statusUserLogin", type = "id-vlong"),
            @Field(name = "changeReason", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderStatusId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "ORDER_STTS_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_STTS_OHDR",
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
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderPaymentPreference",
                keyMaps = {
                    @KeyMap(fieldName = "orderPaymentPreferenceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "ORDER_STTS_USER",
                keyMaps = {
                    @KeyMap(fieldName = "statusUserLogin", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface OrderStatusEntity {}

    /**
     * Order Summary Entry
     */
    @Entity(
        name = "OrderSummaryEntry",
        packageName = "org.ofbiz.order.order",
        title = "Order Summary Entry",
        fields = {
            @Field(name = "entryDate", type = "date"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "facilityId", type = "id-ne"),
            @Field(name = "totalQuantity", type = "fixed-point"),
            @Field(name = "grossSales", type = "currency-amount"),
            @Field(name = "productCost", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "entryDate"),
            @PrimaryKey(field = "productId"),
            @PrimaryKey(field = "facilityId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "ORDER_SMENT_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "ORDER_SMENT_FAC",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            )
        }
    )
    public interface OrderSummaryEntryEntity {}

    /**
     * Order Term
     */
    @Entity(
        name = "OrderTerm",
        packageName = "org.ofbiz.order.order",
        title = "Order Term",
        fields = {
            @Field(name = "termTypeId", type = "id-ne"),
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "termValue", type = "currency-amount"),
            @Field(name = "termDays", type = "numeric"),
            @Field(name = "textValue", type = "description"),
            @Field(name = "description", type = "description"),
            @Field(name = "uomId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "termTypeId"),
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "ORDER_TERM_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDER_TERM_OHDR",
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
                relEntityName = "TermType",
                fkName = "ORDER_TERM_TTYPE",
                keyMaps = {
                    @KeyMap(fieldName = "termTypeId")
                }
            )
        }
    )
    public interface OrderTermEntity {}

    /**
     * Order Term Attribute
     */
    @Entity(
        name = "OrderTermAttribute",
        packageName = "org.ofbiz.order.order",
        title = "Order Term Attribute",
        fields = {
            @Field(name = "termTypeId", type = "id-ne"),
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "termTypeId"),
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderTerm",
                fkName = "ORDER_TATTR_OTRM",
                keyMaps = {
                    @KeyMap(fieldName = "termTypeId"),
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        }
    )
    public interface OrderTermAttributeEntity {}

    /**
     * Order Type
     */
    @Entity(
        name = "OrderType",
        packageName = "org.ofbiz.order.order",
        title = "Order Type",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "orderTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderType",
                title = "Parent",
                fkName = "ORDER_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "orderTypeId")
                }
            )
        }
    )
    public interface OrderTypeEntity {}

    /**
     * Order Type Attribute
     */
    @Entity(
        name = "OrderTypeAttr",
        packageName = "org.ofbiz.order.order",
        title = "Order Type Attribute",
        fields = {
            @Field(name = "orderTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderType",
                fkName = "ORDER_TPAT_ORTYP",
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderHeader",
                keyMaps = {
                    @KeyMap(fieldName = "orderTypeId")
                }
            )
        }
    )
    public interface OrderTypeAttrEntity {}

    /**
     * Product Order Item
     */
    @Entity(
        name = "ProductOrderItem",
        packageName = "org.ofbiz.order.order",
        title = "Product Order Item",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "engagementId", type = "id-ne"),
            @Field(name = "engagementItemSeqId", type = "id-ne"),
            @Field(name = "productId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "engagementId"),
            @PrimaryKey(field = "engagementItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "PROD_OITEM_OHDR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "PROD_OITEM_OITEM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "PROD_OITEM_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                title = "Engagement",
                fkName = "PROD_OITEM_ENOHDR",
                keyMaps = {
                    @KeyMap(fieldName = "engagementId", relFieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                title = "Engagement",
                fkName = "PROD_OITEM_ENOITM",
                keyMaps = {
                    @KeyMap(fieldName = "engagementId", relFieldName = "orderId"),
                    @KeyMap(fieldName = "engagementItemSeqId", relFieldName = "orderItemSeqId")
                }
            )
        }
    )
    public interface ProductOrderItemEntity {}

    /**
     * Work Order Item Fulfillment
     */
    @Entity(
        name = "WorkOrderItemFulfillment",
        packageName = "org.ofbiz.order.order",
        title = "Work Order Item Fulfillment",
        fields = {
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "shipGroupSeqId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "workEffortId"),
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "WORDER_ITFMT_OHDR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "WORDER_ITFMT_OITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WORDER_ITFMT_WEFRT",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "OrderItemShipGroupAssoc",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId"),
                    @KeyMap(fieldName = "shipGroupSeqId")
                }
            )
        }
    )
    public interface WorkOrderItemFulfillmentEntity {}

    /**
     * Quote
     */
    @Entity(
        name = "Quote",
        packageName = "org.ofbiz.order.quote",
        title = "Quote",
        fields = {
            @Field(name = "quoteId", type = "id-ne"),
            @Field(name = "quoteTypeId", type = "id-ne"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "issueDate", type = "date-time"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "productStoreId", type = "id"),
            @Field(name = "salesChannelEnumId", type = "id"),
            @Field(name = "validFromDate", type = "date-time"),
            @Field(name = "validThruDate", type = "date-time"),
            @Field(name = "quoteName", type = "name"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "quoteId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "QuoteType",
                fkName = "QUOTE_QTTYP",
                keyMaps = {
                    @KeyMap(fieldName = "quoteTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "QuoteTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "quoteTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "QUOTE_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "QUOTE_STATUS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "QUOTE_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "QUOTE_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "SalesChannel",
                fkName = "QUOTE_CHANNEL",
                keyMaps = {
                    @KeyMap(fieldName = "salesChannelEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "QuoteNoteView",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            )
        }
    )
    public interface QuoteEntity {}

    /**
     * Quote Attribute
     */
    @Entity(
        name = "QuoteAttribute",
        packageName = "org.ofbiz.order.quote",
        title = "Quote Attribute",
        fields = {
            @Field(name = "quoteId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "quoteId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Quote",
                fkName = "QUOTE_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "QuoteTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface QuoteAttributeEntity {}

    /**
     * Quote Coefficient
     */
    @Entity(
        name = "QuoteCoefficient",
        packageName = "org.ofbiz.order.quote",
        title = "Quote Coefficient",
        fields = {
            @Field(name = "quoteId", type = "id-ne"),
            @Field(name = "coeffName", type = "id-long-ne"),
            @Field(name = "coeffValue", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "quoteId"),
            @PrimaryKey(field = "coeffName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Quote",
                fkName = "QUOTE_COEFF",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            )
        }
    )
    public interface QuoteCoefficientEntity {}

    /**
     * Quote Item
     */
    @Entity(
        name = "QuoteItem",
        packageName = "org.ofbiz.order.quote",
        title = "Quote Item",
        fields = {
            @Field(name = "quoteId", type = "id-ne"),
            @Field(name = "quoteItemSeqId", type = "id-ne"),
            @Field(name = "productId", type = "id"),
            @Field(name = "productFeatureId", type = "id"),
            @Field(name = "deliverableTypeId", type = "id"),
            @Field(name = "skillTypeId", type = "id"),
            @Field(name = "uomId", type = "id"),
            @Field(name = "workEffortId", type = "id"),
            @Field(name = "custRequestId", type = "id"),
            @Field(name = "custRequestItemSeqId", type = "id"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "selectedAmount", type = "fixed-point"),
            @Field(name = "quoteUnitPrice", type = "currency-amount"),
            @Field(name = "reservStart", type = "date-time"),
            @Field(name = "reservLength", type = "fixed-point"),
            @Field(name = "reservPersons", type = "fixed-point"),
            @Field(name = "configId", type = "id"),
            @Field(name = "estimatedDeliveryDate", type = "date-time"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "isPromo", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "quoteId"),
            @PrimaryKey(field = "quoteItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Quote",
                fkName = "QUOTE_ITM_QTE",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "QUOTE_ITM_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                fkName = "QUOTE_ITM_PFEAT",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "DeliverableType",
                fkName = "QUOTE_ITM_DELT",
                keyMaps = {
                    @KeyMap(fieldName = "deliverableTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SkillType",
                fkName = "QUOTE_ITM_SKLT",
                keyMaps = {
                    @KeyMap(fieldName = "skillTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "QUOTE_ITM_UOM",
                keyMaps = {
                    @KeyMap(fieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "QUOTE_ITM_WKEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequest",
                fkName = "QUOTE_ITM_CSRQ",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequestItem",
                fkName = "QUOTE_ITM_CSRITM",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId"),
                    @KeyMap(fieldName = "custRequestItemSeqId")
                }
            )
        }
    )
    public interface QuoteItemEntity {}

    /**
     * Quote Note
     */
    @Entity(
        name = "QuoteNote",
        packageName = "org.ofbiz.order.quote",
        title = "Quote Note",
        neverCache = true,
        fields = {
            @Field(name = "quoteId", type = "id-ne"),
            @Field(name = "noteId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "quoteId"),
            @PrimaryKey(field = "noteId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Quote",
                fkName = "QUOTE_NT_QTE",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "NoteData",
                fkName = "QUOTE_NT_NOTE",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        }
    )
    public interface QuoteNoteEntity {}

    /**
     * Quote Role
     */
    @Entity(
        name = "QuoteRole",
        packageName = "org.ofbiz.order.quote",
        title = "Quote Role",
        fields = {
            @Field(name = "quoteId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "quoteId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Quote",
                fkName = "QUOTE_RL_QUOTE",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "QUOTE_RL_PARTY",
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
                fkName = "QUOTE_RL_PROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface QuoteRoleEntity {}

    /**
     * Quote Term
     */
    @Entity(
        name = "QuoteTerm",
        packageName = "org.ofbiz.order.quote",
        title = "Quote Term",
        fields = {
            @Field(name = "termTypeId", type = "id-ne"),
            @Field(name = "quoteId", type = "id-ne"),
            @Field(name = "quoteItemSeqId", type = "id-ne"),
            @Field(name = "termValue", type = "numeric"),
            @Field(name = "uomId", type = "id"),
            @Field(name = "termDays", type = "numeric"),
            @Field(name = "textValue", type = "description"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "termTypeId"),
            @PrimaryKey(field = "quoteId"),
            @PrimaryKey(field = "quoteItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Quote",
                fkName = "QUOTE_TERM_QTE",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "QuoteItem",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId"),
                    @KeyMap(fieldName = "quoteItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TermType",
                fkName = "QUOTE_TERM_TTYPE",
                keyMaps = {
                    @KeyMap(fieldName = "termTypeId")
                }
            )
        }
    )
    public interface QuoteTermEntity {}

    /**
     * Quote Term Attribute
     */
    @Entity(
        name = "QuoteTermAttribute",
        packageName = "org.ofbiz.order.quote",
        title = "Quote Term Attribute",
        fields = {
            @Field(name = "termTypeId", type = "id-ne"),
            @Field(name = "quoteId", type = "id-ne"),
            @Field(name = "quoteItemSeqId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "termTypeId"),
            @PrimaryKey(field = "quoteId"),
            @PrimaryKey(field = "quoteItemSeqId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "QuoteTerm",
                fkName = "QUOTE_TERM_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "termTypeId"),
                    @KeyMap(fieldName = "quoteId"),
                    @KeyMap(fieldName = "quoteItemSeqId")
                }
            )
        }
    )
    public interface QuoteTermAttributeEntity {}

    /**
     * Quote Type
     */
    @Entity(
        name = "QuoteType",
        packageName = "org.ofbiz.order.quote",
        title = "Quote Type",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "quoteTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "quoteTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "QuoteType",
                title = "Parent",
                fkName = "QUOTE_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "quoteTypeId")
                }
            )
        }
    )
    public interface QuoteTypeEntity {}

    /**
     * Quote Type Attribute
     */
    @Entity(
        name = "QuoteTypeAttr",
        packageName = "org.ofbiz.order.quote",
        title = "Quote Type Attribute",
        fields = {
            @Field(name = "quoteTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "quoteTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "QuoteType",
                fkName = "QUOTE_TPAT_QTYP",
                keyMaps = {
                    @KeyMap(fieldName = "quoteTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "QuoteAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Quote",
                keyMaps = {
                    @KeyMap(fieldName = "quoteTypeId")
                }
            )
        }
    )
    public interface QuoteTypeAttrEntity {}

    /**
     * Quote Work Effort
     */
    @Entity(
        name = "QuoteWorkEffort",
        packageName = "org.ofbiz.order.quote",
        title = "Quote Work Effort",
        fields = {
            @Field(name = "quoteId", type = "id-ne"),
            @Field(name = "workEffortId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "quoteId"),
            @PrimaryKey(field = "workEffortId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Quote",
                fkName = "QUOTE_WE_QUOTE",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "QUOTE_WE_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface QuoteWorkEffortEntity {}

    /**
     * Quote Adjustment
     * Note that both includeInTax and includeInShipping should default to true, except in the case where this adjustment is a tax or shipping adjustment then should be ignored.
     */
    @Entity(
        name = "QuoteAdjustment",
        packageName = "org.ofbiz.order.quote",
        title = "Quote Adjustment",
        description = "Note that both includeInTax and includeInShipping should default to true, except in the case where this adjustment is a tax or shipping adjustment then should be ignored.",
        neverCache = true,
        fields = {
            @Field(name = "quoteAdjustmentId", type = "id-ne"),
            @Field(name = "quoteAdjustmentTypeId", type = "id"),
            @Field(name = "quoteId", type = "id"),
            @Field(name = "quoteItemSeqId", type = "id"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "description", type = "description"),
            @Field(name = "amount", type = "currency-amount"),
            @Field(name = "productPromoId", type = "id"),
            @Field(name = "productPromoRuleId", type = "id"),
            @Field(name = "productPromoActionSeqId", type = "id"),
            @Field(name = "productFeatureId", type = "id"),
            @Field(name = "correspondingProductId", type = "id"),
            @Field(name = "sourceReferenceId", type = "id-long"),
            @Field(name = "sourcePercentage", type = "fixed-point"),
            @Field(name = "customerReferenceId", type = "id-long"),
            @Field(name = "primaryGeoId", type = "id"),
            @Field(name = "secondaryGeoId", type = "id"),
            @Field(name = "exemptAmount", type = "currency-amount"),
            @Field(name = "taxAuthGeoId", type = "id"),
            @Field(name = "taxAuthPartyId", type = "id"),
            @Field(name = "overrideGlAccountId", type = "id"),
            @Field(name = "includeInTax", type = "indicator"),
            @Field(name = "includeInShipping", type = "indicator"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "quoteAdjustmentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderAdjustmentType",
                fkName = "QUOTE_ADJ_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "quoteAdjustmentTypeId", relFieldName = "orderAdjustmentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Quote",
                fkName = "QUOTE_ADJ_OHEAD",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "QUOTE_ADJ_USERL",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "QuoteItem",
                keyMaps = {
                    @KeyMap(fieldName = "quoteId"),
                    @KeyMap(fieldName = "quoteItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "QUOTE_ADJ_PROMO",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductPromoRule",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId"),
                    @KeyMap(fieldName = "productPromoRuleId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductPromoAction",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId"),
                    @KeyMap(fieldName = "productPromoRuleId"),
                    @KeyMap(fieldName = "productPromoActionSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "Primary",
                fkName = "QUOTE_ADJ_PRGEO",
                keyMaps = {
                    @KeyMap(fieldName = "primaryGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "Secondary",
                fkName = "QUOTE_ADJ_SCGEO",
                keyMaps = {
                    @KeyMap(fieldName = "secondaryGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthority",
                fkName = "QUOTE_ADJ_TXA",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId"),
                    @KeyMap(fieldName = "taxAuthPartyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "Override",
                fkName = "QUOTE_ADJ_OGLA",
                keyMaps = {
                    @KeyMap(fieldName = "overrideGlAccountId", relFieldName = "glAccountId")
                }
            )
        }
    )
    public interface QuoteAdjustmentEntity {}

    /**
     * Customer Request
     */
    @Entity(
        name = "CustRequest",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request",
        fields = {
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "custRequestTypeId", type = "id"),
            @Field(name = "custRequestCategoryId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "fromPartyId", type = "id"),
            @Field(name = "priority", type = "numeric"),
            @Field(name = "custRequestDate", type = "date-time", description = "\n          When the customer (or whoever) submitted the request, maybe out of OFBiz : comming by mail, email, etc.\n        "),
            @Field(name = "responseRequiredDate", type = "date-time", description = "\n          responseRequiredDate is the time the customer needs a response.\n        "),
            @Field(name = "custRequestName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "maximumAmountUomId", type = "id"),
            @Field(name = "productStoreId", type = "id"),
            @Field(name = "salesChannelEnumId", type = "id"),
            @Field(name = "fulfillContactMechId", type = "id", description = "\n          Field to support a location of a cust request--ie, product literature sent to an address, service call at a localtion, etc.\n        "),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "openDateTime", type = "date-time", description = "\n          Used when the customer service person, or anyone assigned to handle the incoming request, opens it for action.\n          You  cantake the customer requestdate and openDateTime to see the efficiency of the customer service people.\n        "),
            @Field(name = "closedDateTime", type = "date-time", description = "\n          Used when the customer service person, or anyone assigned to handle the incoming request, closes it as resolution.\n          In some customer response systems, the openDateTime and closedDateTime can happen more than once as the customer is not satified with the resolution.\n        "),
            @Field(name = "internalComment", type = "comment"),
            @Field(name = "reason", type = "description"),
            @Field(name = "createdDate", type = "date-time", description = "\n          When it is actually stored in the system.\n        "),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time", description = "\n          Last modified date can be till the closedDateTime which is when the customer service people, or anyone assigned to handle the incoming request, says it is resolved.\n          This gives when the last action was done to see if the steps to resolve the request are happening in a timely manner.\n        "),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequestType",
                fkName = "CUST_REQ_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequestCategory",
                fkName = "CUST_REQ_CAT",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestCategoryId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "CUST_REQ_STATUS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "From",
                fkName = "CUST_REQ_FRMPTY",
                keyMaps = {
                    @KeyMap(fieldName = "fromPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "MaximumAmount",
                fkName = "CUST_REQ_AUOM",
                keyMaps = {
                    @KeyMap(fieldName = "maximumAmountUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "CUST_REQ_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Enumeration",
                title = "SalesChannel",
                fkName = "CUST_REQ_CHANNEL",
                keyMaps = {
                    @KeyMap(fieldName = "salesChannelEnumId", relFieldName = "enumId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CustRequestTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                title = "Fulfill",
                fkName = "CUST_REQ_FULCM",
                keyMaps = {
                    @KeyMap(fieldName = "fulfillContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "CUST_REQ_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface CustRequestEntity {}

    /**
     * Customer Request Attribute
     */
    @Entity(
        name = "CustRequestAttribute",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request Attribute",
        fields = {
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequest",
                fkName = "CUST_REQ_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CustRequestTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface CustRequestAttributeEntity {}

    /**
     * Customer Category Type
     */
    @Entity(
        name = "CustRequestCategory",
        packageName = "org.ofbiz.order.request",
        title = "Customer Category Type",
        fields = {
            @Field(name = "custRequestCategoryId", type = "id-ne"),
            @Field(name = "custRequestTypeId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestCategoryId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequestType",
                fkName = "CUST_RQCT_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestTypeId")
                }
            )
        }
    )
    public interface CustRequestCategoryEntity {}

    /**
     * Customer Request Communication Event
     */
    @Entity(
        name = "CustRequestCommEvent",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request Communication Event",
        fields = {
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "communicationEventId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestId"),
            @PrimaryKey(field = "communicationEventId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequest",
                fkName = "CUSTREQ_CEV_CRQ",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CommunicationEvent",
                fkName = "CUSTREQ_CEV_CEV",
                keyMaps = {
                    @KeyMap(fieldName = "communicationEventId")
                }
            )
        }
    )
    public interface CustRequestCommEventEntity {}

    /**
     * Customer Request Content
     */
    @Entity(
        name = "CustRequestContent",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request Content",
        fields = {
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "contentId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestId"),
            @PrimaryKey(field = "contentId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequest",
                fkName = "CUSTREQ_CNT_CUSTRQ",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Content",
                fkName = "CUSTREQ_CNT_CNT",
                keyMaps = {
                    @KeyMap(fieldName = "contentId")
                }
            )
        }
    )
    public interface CustRequestContentEntity {}

    /**
     * Customer Request Item
     */
    @Entity(
        name = "CustRequestItem",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request Item",
        fields = {
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "custRequestItemSeqId", type = "id-ne"),
            @Field(name = "custRequestResolutionId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "priority", type = "numeric"),
            @Field(name = "sequenceNum", type = "numeric"),
            @Field(name = "requiredByDate", type = "date-time"),
            @Field(name = "productId", type = "id"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "selectedAmount", type = "fixed-point"),
            @Field(name = "maximumAmount", type = "currency-amount"),
            @Field(name = "reservStart", type = "date-time"),
            @Field(name = "reservLength", type = "fixed-point"),
            @Field(name = "reservPersons", type = "fixed-point"),
            @Field(name = "configId", type = "id"),
            @Field(name = "description", type = "description"),
            @Field(name = "story", type = "very-long")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestId"),
            @PrimaryKey(field = "custRequestItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequest",
                fkName = "CUST_REQITM_CREQ",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "CUST_REQITM_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequestResolution",
                fkName = "CUST_REQITM_RES",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestResolutionId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "CUST_REQITM_PRD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface CustRequestItemEntity {}

    /**
     * Customer Request Note
     */
    @Entity(
        name = "CustRequestNote",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request Note",
        fields = {
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "noteId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestId"),
            @PrimaryKey(field = "noteId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequest",
                fkName = "CRQ_CR",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "NoteData",
                fkName = "CRQ_NOTE",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        }
    )
    public interface CustRequestNoteEntity {}

    /**
     * Customer Request Item Note
     */
    @Entity(
        name = "CustRequestItemNote",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request Item Note",
        fields = {
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "custRequestItemSeqId", type = "id-ne"),
            @Field(name = "noteId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestId"),
            @PrimaryKey(field = "custRequestItemSeqId"),
            @PrimaryKey(field = "noteId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequestItem",
                fkName = "CUST_REQ_ITNT",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId"),
                    @KeyMap(fieldName = "custRequestItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "NoteData",
                fkName = "CUST_REQ_NOTE",
                keyMaps = {
                    @KeyMap(fieldName = "noteId")
                }
            )
        }
    )
    public interface CustRequestItemNoteEntity {}

    /**
     * Cust Request Item Work Effort
     */
    @Entity(
        name = "CustRequestItemWorkEffort",
        packageName = "org.ofbiz.order.request",
        title = "Cust Request Item Work Effort",
        fields = {
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "custRequestItemSeqId", type = "id-ne"),
            @Field(name = "workEffortId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestId"),
            @PrimaryKey(field = "custRequestItemSeqId"),
            @PrimaryKey(field = "workEffortId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequestItem",
                fkName = "WORK_REQFL_CSTRQ",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId"),
                    @KeyMap(fieldName = "custRequestItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CustRequest",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "CUST_REQ_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface CustRequestItemWorkEffortEntity {}

    /**
     * Customer Request Resolution
     */
    @Entity(
        name = "CustRequestResolution",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request Resolution",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "custRequestResolutionId", type = "id-ne"),
            @Field(name = "custRequestTypeId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestResolutionId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequestType",
                fkName = "CUST_RQRS_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestTypeId")
                }
            )
        }
    )
    public interface CustRequestResolutionEntity {}

    /**
     * Customer Request Role
     */
    @Entity(
        name = "CustRequestParty",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request Role",
        fields = {
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequest",
                fkName = "CREQ_RL_CRQST",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "CREQ_RL_PARTY",
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
                fkName = "CREQ_RL_PROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface CustRequestPartyEntity {}

    /**
     * Customer Request Status
     */
    @Entity(
        name = "CustRequestStatus",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request Status",
        fields = {
            @Field(name = "custRequestStatusId", type = "id-ne"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "custRequestId", type = "id"),
            @Field(name = "custRequestItemSeqId", type = "id"),
            @Field(name = "statusDatetime", type = "date-time"),
            @Field(name = "changeByUserLoginId", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestStatusId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "CUST_REQST_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequest",
                fkName = "CUST_REQ_STRQ",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "CustRequestItem",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId"),
                    @KeyMap(fieldName = "custRequestItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "ChangeBy",
                fkName = "CUST_RQSTTS_USRLGN",
                keyMaps = {
                    @KeyMap(fieldName = "changeByUserLoginId", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface CustRequestStatusEntity {}

    /**
     * Customer Request Type
     */
    @Entity(
        name = "CustRequestType",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request Type",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "custRequestTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description"),
            @Field(name = "partyId", type = "id-ne", description = "party or party group(via partyRelationShip entity) responsible for responding to the communication request of this particular type")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequestType",
                title = "Parent",
                fkName = "CUST_REQ_TYPE_PAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "custRequestTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "CUST_PTY_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "PartyRelationship",
                fkName = "CUST_PTY_RELAT",
                keyMaps = {
                    @KeyMap(fieldName = "partyId", relFieldName = "partyIdFrom")
                }
            )
        }
    )
    public interface CustRequestTypeEntity {}

    /**
     * Customer Request Type Attribute
     */
    @Entity(
        name = "CustRequestTypeAttr",
        packageName = "org.ofbiz.order.request",
        title = "Customer Request Type Attribute",
        fields = {
            @Field(name = "custRequestTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequestType",
                fkName = "CUST_REQ_TYPE_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CustRequestAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "CustRequest",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestTypeId")
                }
            )
        }
    )
    public interface CustRequestTypeAttrEntity {}

    /**
     * Cust Request Work Effort
     */
    @Entity(
        name = "CustRequestWorkEffort",
        packageName = "org.ofbiz.order.request",
        title = "Cust Request Work Effort",
        fields = {
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "workEffortId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestId"),
            @PrimaryKey(field = "workEffortId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequest",
                fkName = "CSTREQ_WF_CREQ",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "CSTREQ_WF_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface CustRequestWorkEffortEntity {}

    /**
     * Responding Party
     */
    @Entity(
        name = "RespondingParty",
        packageName = "org.ofbiz.order.request",
        title = "Responding Party",
        fields = {
            @Field(name = "respondingPartySeqId", type = "id-ne"),
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id"),
            @Field(name = "dateSent", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "respondingPartySeqId"),
            @PrimaryKey(field = "custRequestId"),
            @PrimaryKey(field = "partyId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CustRequest",
                fkName = "RESP_PTY_CSREQ",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "RESP_PTY_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "RESP_PTY_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface RespondingPartyEntity {}

    /**
     * Desired Feature
     */
    @Entity(
        name = "DesiredFeature",
        packageName = "org.ofbiz.order.requirement",
        title = "Desired Feature",
        fields = {
            @Field(name = "desiredFeatureId", type = "id-ne"),
            @Field(name = "requirementId", type = "id-ne"),
            @Field(name = "productFeatureId", type = "id"),
            @Field(name = "optionalInd", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "desiredFeatureId"),
            @PrimaryKey(field = "requirementId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Requirement",
                fkName = "DES_FEAT_REQ",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductFeature",
                fkName = "DES_FEAT_PFEAT",
                keyMaps = {
                    @KeyMap(fieldName = "productFeatureId")
                }
            )
        }
    )
    public interface DesiredFeatureEntity {}

    /**
     * Order Requirement Commitment
     */
    @Entity(
        name = "OrderRequirementCommitment",
        packageName = "org.ofbiz.order.requirement",
        title = "Order Requirement Commitment",
        fields = {
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id-ne"),
            @Field(name = "requirementId", type = "id-ne"),
            @Field(name = "quantity", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "orderId"),
            @PrimaryKey(field = "orderItemSeqId"),
            @PrimaryKey(field = "requirementId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "ORDREQ_CMT_ORD",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "ORDREQ_CMT_OITM",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Requirement",
                fkName = "ORDREQ_CMT_REQ",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            )
        }
    )
    public interface OrderRequirementCommitmentEntity {}

    /**
     * Requirement
     */
    @Entity(
        name = "Requirement",
        packageName = "org.ofbiz.order.requirement",
        title = "Requirement",
        fields = {
            @Field(name = "requirementId", type = "id-ne"),
            @Field(name = "requirementTypeId", type = "id"),
            @Field(name = "facilityId", type = "id"),
            @Field(name = "deliverableId", type = "id"),
            @Field(name = "fixedAssetId", type = "id"),
            @Field(name = "productId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "description", type = "description"),
            @Field(name = "requirementStartDate", type = "date-time"),
            @Field(name = "requiredByDate", type = "date-time"),
            @Field(name = "estimatedBudget", type = "currency-amount"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "useCase", type = "very-long"),
            @Field(name = "reason", type = "long-varchar"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "requirementId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RequirementType",
                fkName = "REQ_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "requirementTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "RequirementTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "requirementTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "REQ_FACILITY",
                keyMaps = {
                    @KeyMap(fieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Deliverable",
                fkName = "REQ_DELIVERABLE",
                keyMaps = {
                    @KeyMap(fieldName = "deliverableId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FixedAsset",
                fkName = "REQ_FIXED_ASSET",
                keyMaps = {
                    @KeyMap(fieldName = "fixedAssetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "REQ_PRODUCT",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "REQ_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface RequirementEntity {}

    /**
     * Requirement Attribute
     */
    @Entity(
        name = "RequirementAttribute",
        packageName = "org.ofbiz.order.requirement",
        title = "Requirement Attribute",
        fields = {
            @Field(name = "requirementId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "attrValue", type = "value"),
            @Field(name = "attrDescription", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "requirementId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Requirement",
                fkName = "REQ_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "RequirementTypeAttr",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            )
        }
    )
    public interface RequirementAttributeEntity {}

    /**
     * Requirement Budget Allocation
     */
    @Entity(
        name = "RequirementBudgetAllocation",
        packageName = "org.ofbiz.order.requirement",
        title = "Requirement Budget Allocation",
        fields = {
            @Field(name = "budgetId", type = "id-ne"),
            @Field(name = "budgetItemSeqId", type = "id-ne"),
            @Field(name = "requirementId", type = "id-ne"),
            @Field(name = "amount", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "budgetId"),
            @PrimaryKey(field = "budgetItemSeqId"),
            @PrimaryKey(field = "requirementId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Budget",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BudgetItem",
                fkName = "REQ_BDGTAL_BITM",
                keyMaps = {
                    @KeyMap(fieldName = "budgetId"),
                    @KeyMap(fieldName = "budgetItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Requirement",
                fkName = "REQ_BDGTAL_REQ",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            )
        }
    )
    public interface RequirementBudgetAllocationEntity {}

    /**
     * Requirement Customer Request
     */
    @Entity(
        name = "RequirementCustRequest",
        packageName = "org.ofbiz.order.requirement",
        title = "Requirement Customer Request",
        fields = {
            @Field(name = "custRequestId", type = "id-ne"),
            @Field(name = "custRequestItemSeqId", type = "id-ne"),
            @Field(name = "requirementId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "custRequestId"),
            @PrimaryKey(field = "custRequestItemSeqId"),
            @PrimaryKey(field = "requirementId")
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
                type = RelationType.ONE,
                relEntityName = "CustRequestItem",
                fkName = "REQ_CSREQ_CRITM",
                keyMaps = {
                    @KeyMap(fieldName = "custRequestId"),
                    @KeyMap(fieldName = "custRequestItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Requirement",
                fkName = "REQ_CSREQ_REQ",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            )
        }
    )
    public interface RequirementCustRequestEntity {}

    /**
     * Requirement Role
     */
    @Entity(
        name = "RequirementRole",
        packageName = "org.ofbiz.order.requirement",
        title = "Requirement Role",
        fields = {
            @Field(name = "requirementId", type = "id-ne"),
            @Field(name = "partyId", type = "id-ne"),
            @Field(name = "roleTypeId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "requirementId"),
            @PrimaryKey(field = "partyId"),
            @PrimaryKey(field = "roleTypeId"),
            @PrimaryKey(field = "fromDate")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Requirement",
                fkName = "REQ_ROLE_REQ",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "REQ_ROLE_PRTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PartyRole",
                fkName = "REQ_ROLE_PROLE",
                keyMaps = {
                    @KeyMap(fieldName = "partyId"),
                    @KeyMap(fieldName = "roleTypeId")
                }
            )
        }
    )
    public interface RequirementRoleEntity {}

    /**
     * Requirement Status
     */
    @Entity(
        name = "RequirementStatus",
        packageName = "org.ofbiz.order.requirement",
        title = "Requirement Status",
        fields = {
            @Field(name = "requirementId", type = "id-ne"),
            @Field(name = "statusId", type = "id-ne"),
            @Field(name = "statusDate", type = "date-time"),
            @Field(name = "changeByUserLoginId", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "requirementId"),
            @PrimaryKey(field = "statusId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Requirement",
                fkName = "REQ_STTS_REQ",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "REQ_STTS_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "ChangeBy",
                fkName = "REQ_STTS_USRLGN",
                keyMaps = {
                    @KeyMap(fieldName = "changeByUserLoginId", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface RequirementStatusEntity {}

    /**
     * Requirement Type
     */
    @Entity(
        name = "RequirementType",
        packageName = "org.ofbiz.order.requirement",
        title = "Requirement Type",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "requirementTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "requirementTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RequirementType",
                title = "Parent",
                fkName = "REQ_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "requirementTypeId")
                }
            )
        }
    )
    public interface RequirementTypeEntity {}

    /**
     * Requirement Type Attribute
     */
    @Entity(
        name = "RequirementTypeAttr",
        packageName = "org.ofbiz.order.requirement",
        title = "Requirement Type Attribute",
        fields = {
            @Field(name = "requirementTypeId", type = "id-ne"),
            @Field(name = "attrName", type = "id-long-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "requirementTypeId"),
            @PrimaryKey(field = "attrName")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RequirementType",
                fkName = "REQ_TYPE_ATTR",
                keyMaps = {
                    @KeyMap(fieldName = "requirementTypeId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "RequirementAttribute",
                keyMaps = {
                    @KeyMap(fieldName = "attrName")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "Requirement",
                keyMaps = {
                    @KeyMap(fieldName = "requirementTypeId")
                }
            )
        }
    )
    public interface RequirementTypeAttrEntity {}

    /**
     * Work Requirement Fulfillment Type
     */
    @Entity(
        name = "WorkReqFulfType",
        packageName = "org.ofbiz.order.requirement",
        title = "Work Requirement Fulfillment Type",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "workReqFulfTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "workReqFulfTypeId")
        }
    )
    public interface WorkReqFulfTypeEntity {}

    /**
     * Work Requirement Fulfillment
     */
    @Entity(
        name = "WorkRequirementFulfillment",
        packageName = "org.ofbiz.order.requirement",
        title = "Work Requirement Fulfillment",
        fields = {
            @Field(name = "requirementId", type = "id-ne"),
            @Field(name = "workEffortId", type = "id-ne"),
            @Field(name = "workReqFulfTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "requirementId"),
            @PrimaryKey(field = "workEffortId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Requirement",
                fkName = "WORK_REQFL_REQ",
                keyMaps = {
                    @KeyMap(fieldName = "requirementId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "WORK_REQFL_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkReqFulfType",
                fkName = "WORK_REQFL_WRFT",
                keyMaps = {
                    @KeyMap(fieldName = "workReqFulfTypeId")
                }
            )
        }
    )
    public interface WorkRequirementFulfillmentEntity {}

    /**
     * Return Adjustment
     * Tax, shipping, and promotional adjustments which are carried over from the order to the return.         Note that both includeInTax and includeInShipping should default to true, except in the case where this adjustment is a tax or shipping adjustment         then should be ignored.
     */
    @Entity(
        name = "ReturnAdjustment",
        packageName = "org.ofbiz.order.return",
        title = "Return Adjustment",
        description = "Tax, shipping, and promotional adjustments which are carried over from the order to the return.\n        Note that both includeInTax and includeInShipping should default to true, except in the case where this adjustment is a tax or shipping adjustment\n        then should be ignored.",
        neverCache = true,
        fields = {
            @Field(name = "returnAdjustmentId", type = "id-ne"),
            @Field(name = "returnAdjustmentTypeId", type = "id"),
            @Field(name = "returnId", type = "id"),
            @Field(name = "returnItemSeqId", type = "id"),
            @Field(name = "shipGroupSeqId", type = "id"),
            @Field(name = "comments", type = "comment"),
            @Field(name = "description", type = "description"),
            @Field(name = "returnTypeId", type = "id", description = "actually used for disbursement type: store credit, cash refund, exchange"),
            @Field(name = "orderAdjustmentId", type = "id"),
            @Field(name = "amount", type = "currency-precise"),
            @Field(name = "productPromoId", type = "id"),
            @Field(name = "productPromoRuleId", type = "id"),
            @Field(name = "productPromoActionSeqId", type = "id"),
            @Field(name = "productFeatureId", type = "id"),
            @Field(name = "correspondingProductId", type = "id"),
            @Field(name = "taxAuthorityRateSeqId", type = "id-ne"),
            @Field(name = "sourceReferenceId", type = "id-long"),
            @Field(name = "sourcePercentage", type = "fixed-point", description = "for tax entries this is the tax percentage"),
            @Field(name = "customerReferenceId", type = "id-long", description = "for tax entries this is partyTaxId"),
            @Field(name = "primaryGeoId", type = "id", description = "for tax entries this is the primary jurisdiction Geo (the smallest or most local Geo that this tax is for, usually a state/province, perhaps a county or a city)"),
            @Field(name = "secondaryGeoId", type = "id", description = "for tax entries this is the secondary jurisdiction Geo (usually a country, or other Geo that the primary is within)"),
            @Field(name = "exemptAmount", type = "currency-amount", description = "an amount that would normally apply, but not to this order; for tax exemption represents the what the tax would have been"),
            @Field(name = "taxAuthGeoId", type = "id", description = "these taxAuth fields deprecate the primaryGeoId and secondaryGeoId fields and will be used with the newer tax calc stuff"),
            @Field(name = "taxAuthPartyId", type = "id"),
            @Field(name = "overrideGlAccountId", type = "id", description = "used to specify the override or actual glAccountId used for the adjustment, avoids problems if configuration changes after initial posting, etc"),
            @Field(name = "includeInTax", type = "indicator"),
            @Field(name = "includeInShipping", type = "indicator"),
            @Field(name = "createdDate", type = "date-time"),
            @Field(name = "createdByUserLogin", type = "id-vlong"),
            @Field(name = "lastModifiedDate", type = "date-time"),
            @Field(name = "lastModifiedByUserLogin", type = "id-vlong")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnAdjustmentId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnAdjustmentType",
                fkName = "RETURN_ADJ_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "returnAdjustmentTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnHeader",
                fkName = "RETURN_ADJ_RHEAD",
                keyMaps = {
                    @KeyMap(fieldName = "returnId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                fkName = "RETURN_ADJ_USERL",
                keyMaps = {
                    @KeyMap(fieldName = "createdByUserLogin", relFieldName = "userLoginId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ReturnItem",
                keyMaps = {
                    @KeyMap(fieldName = "returnId"),
                    @KeyMap(fieldName = "returnItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromo",
                fkName = "RETURN_ADJ_PROMO",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductPromoRule",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId"),
                    @KeyMap(fieldName = "productPromoRuleId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ProductPromoAction",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoId"),
                    @KeyMap(fieldName = "productPromoRuleId"),
                    @KeyMap(fieldName = "productPromoActionSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "Primary",
                fkName = "RETURN_ADJ_PRGEO",
                keyMaps = {
                    @KeyMap(fieldName = "primaryGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Geo",
                title = "Secondary",
                fkName = "RETURN_ADJ_SCGEO",
                keyMaps = {
                    @KeyMap(fieldName = "secondaryGeoId", relFieldName = "geoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthority",
                fkName = "RETURN_ADJ_TXA",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthGeoId"),
                    @KeyMap(fieldName = "taxAuthPartyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "GlAccount",
                title = "Override",
                fkName = "RETURN_ADJ_OGLA",
                keyMaps = {
                    @KeyMap(fieldName = "overrideGlAccountId", relFieldName = "glAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnType",
                fkName = "RET_ADJ_RTN_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "returnTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderAdjustment",
                fkName = "RETURN_ADJ_ORDADJ",
                keyMaps = {
                    @KeyMap(fieldName = "orderAdjustmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "TaxAuthorityRateProduct",
                fkName = "RETURN_ADJ_TARP",
                keyMaps = {
                    @KeyMap(fieldName = "taxAuthorityRateSeqId")
                }
            )
        }
    )
    public interface ReturnAdjustmentEntity {}

    /**
     * Return Adjustment Type
     */
    @Entity(
        name = "ReturnAdjustmentType",
        packageName = "org.ofbiz.order.return",
        title = "Return Adjustment Type",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "returnAdjustmentTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id-ne"),
            @Field(name = "hasTable", type = "indicator"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnAdjustmentTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnAdjustmentType",
                title = "Parent",
                fkName = "RETURN_ADJ_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "returnAdjustmentTypeId")
                }
            )
        }
    )
    public interface ReturnAdjustmentTypeEntity {}

    /**
     * Return
     */
    @Entity(
        name = "ReturnHeader",
        packageName = "org.ofbiz.order.return",
        title = "Return",
        fields = {
            @Field(name = "returnId", type = "id-ne"),
            @Field(name = "returnHeaderTypeId", type = "id-ne"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "createdBy", type = "id-vlong"),
            @Field(name = "fromPartyId", type = "id"),
            @Field(name = "toPartyId", type = "id"),
            @Field(name = "paymentMethodId", type = "id"),
            @Field(name = "finAccountId", type = "id"),
            @Field(name = "billingAccountId", type = "id"),
            @Field(name = "entryDate", type = "date-time"),
            @Field(name = "originContactMechId", type = "id"),
            @Field(name = "destinationFacilityId", type = "id"),
            @Field(name = "needsInventoryReceive", type = "indicator"),
            @Field(name = "currencyUomId", type = "id-ne"),
            @Field(name = "supplierRmaId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnHeaderType",
                fkName = "RTN_HEAD_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "returnHeaderTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "RTN_FROM_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "fromPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                title = "To",
                fkName = "RTN_TO_PARTY",
                keyMaps = {
                    @KeyMap(fieldName = "toPartyId", relFieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BillingAccount",
                fkName = "RTN_TO_BACT",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccount",
                fkName = "RTN_TO_FACT",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethod",
                fkName = "RTN_TO_PAYMETH",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Facility",
                fkName = "RTN_TO_FACILITY",
                keyMaps = {
                    @KeyMap(fieldName = "destinationFacilityId", relFieldName = "facilityId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "RTN_FROM_CTM",
                keyMaps = {
                    @KeyMap(fieldName = "originContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PostalAddress",
                keyMaps = {
                    @KeyMap(fieldName = "originContactMechId", relFieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "RTN_STTS_ITEM",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                fkName = "RTN_HDR_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "UserLogin",
                keyMaps = {
                    @KeyMap(fieldName = "createdBy", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface ReturnHeaderEntity {}

    /**
     * Return Header Type
     */
    @Entity(
        name = "ReturnHeaderType",
        packageName = "org.ofbiz.order.return",
        title = "Return Header Type",
        fields = {
            @Field(name = "returnHeaderTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnHeaderTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnHeaderType",
                title = "Parent",
                fkName = "RTHEAD_TYPE_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "returnHeaderTypeId")
                }
            )
        }
    )
    public interface ReturnHeaderTypeEntity {}

    /**
     * Return Item
     */
    @Entity(
        name = "ReturnItem",
        packageName = "org.ofbiz.order.return",
        title = "Return Item",
        fields = {
            @Field(name = "returnId", type = "id-ne"),
            @Field(name = "returnItemSeqId", type = "id-ne"),
            @Field(name = "returnReasonId", type = "id", description = "why item is returned: did not like, wrong item, damaged, etc. etc.", enableAuditLog = true),
            @Field(name = "returnTypeId", type = "id", description = "actually used for disbursement type: store credit, cash refund, exchange", enableAuditLog = true),
            @Field(name = "returnItemTypeId", type = "id", description = "what is returned: a product, a service, etc"),
            @Field(name = "productId", type = "id", description = "we need this field to be able to figure out net sales of a product"),
            @Field(name = "description", type = "description"),
            @Field(name = "orderId", type = "id-ne"),
            @Field(name = "orderItemSeqId", type = "id"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "expectedItemStatus", type = "id"),
            @Field(name = "returnQuantity", type = "fixed-point", description = "promised by the customer", enableAuditLog = true),
            @Field(name = "receivedQuantity", type = "fixed-point", description = "actually received from the customer", enableAuditLog = true),
            @Field(name = "returnPrice", type = "currency-amount", enableAuditLog = true),
            @Field(name = "returnItemResponseId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnId"),
            @PrimaryKey(field = "returnItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnHeader",
                fkName = "RTN_ITEM_RTN",
                keyMaps = {
                    @KeyMap(fieldName = "returnId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnReason",
                fkName = "RTN_ITEM_REASON",
                keyMaps = {
                    @KeyMap(fieldName = "returnReasonId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnType",
                fkName = "RTN_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "returnTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnItemType",
                fkName = "RTN_ITEM_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "returnItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnItemResponse",
                fkName = "RTN_ITEM_RESP",
                keyMaps = {
                    @KeyMap(fieldName = "returnItemResponseId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                fkName = "RTN_ITEM_ODR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderItem",
                fkName = "RTN_ITEM_ODRIT",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "RTN_ITEM_STTSIT",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                title = "Inventory",
                fkName = "RTN_ITEM_ITSTT",
                keyMaps = {
                    @KeyMap(fieldName = "expectedItemStatus", relFieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "RTN_ITEM_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderItemShipGrpInvRes",
                fkName = "RTN_ITEM_OISGIR",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "orderItemSeqId")
                }
            )
        },
        indexes = {
            @Index(
                name = "RTN_ITM_BYORDITM",
                fields = {
                    @IndexField(name = "orderId"),
                    @IndexField(name = "orderItemSeqId")
                }
            ),
            @Index(
                name = "RTN_ITM_LSTUPDATED",
                fields = {
                    @IndexField(name = "statusId"),
                    @IndexField(name = "returnTypeId"),
                    @IndexField(name = "returnReasonId"),
                    @IndexField(name = "returnItemTypeId"),
                    @IndexField(name = "returnPrice"),
                    @IndexField(name = "returnQuantity")
                }
            )
        }
    )
    public interface ReturnItemEntity {}

    /**
     * The Return Item Response
     * Records what was done with a return: whether a replacement order, a payment, or a billing account credit was issued
     */
    @Entity(
        name = "ReturnItemResponse",
        packageName = "org.ofbiz.order.return",
        title = "The Return Item Response",
        description = "Records what was done with a return: whether a replacement order, a payment, or a billing account credit was issued",
        neverCache = true,
        fields = {
            @Field(name = "returnItemResponseId", type = "id-ne"),
            @Field(name = "orderPaymentPreferenceId", type = "id"),
            @Field(name = "replacementOrderId", type = "id"),
            @Field(name = "paymentId", type = "id"),
            @Field(name = "billingAccountId", type = "id"),
            @Field(name = "finAccountTransId", type = "id"),
            @Field(name = "responseAmount", type = "currency-amount"),
            @Field(name = "responseDate", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnItemResponseId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderPaymentPreference",
                fkName = "RTN_PAY_ORDPAYPF",
                keyMaps = {
                    @KeyMap(fieldName = "orderPaymentPreferenceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "OrderHeader",
                title = "Replacement",
                fkName = "RTN_RESP_NEWORD",
                keyMaps = {
                    @KeyMap(fieldName = "replacementOrderId", relFieldName = "orderId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Payment",
                fkName = "RTN_PAY_PAYMENT",
                keyMaps = {
                    @KeyMap(fieldName = "paymentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "BillingAccount",
                fkName = "RTN_PAY_BACT",
                keyMaps = {
                    @KeyMap(fieldName = "billingAccountId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "FinAccountTrans",
                fkName = "RTN_PAY_FINACTTX",
                keyMaps = {
                    @KeyMap(fieldName = "finAccountTransId")
                }
            )
        }
    )
    public interface ReturnItemResponseEntity {}

    /**
     * Return Item Type
     * ReturnItemType records the type of a ReturnItem
     */
    @Entity(
        name = "ReturnItemType",
        packageName = "org.ofbiz.order.return",
        title = "Return Item Type",
        description = "ReturnItemType records the type of a ReturnItem",
        fields = {
            @Field(name = "returnItemTypeId", type = "id-ne"),
            @Field(name = "parentTypeId", type = "id"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnItemTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnItemType",
                title = "Parent",
                fkName = "RETURN_ITEM_TYPPAR",
                keyMaps = {
                    @KeyMap(fieldName = "parentTypeId", relFieldName = "returnItemTypeId")
                }
            )
        }
    )
    public interface ReturnItemTypeEntity {}

    /**
     * Return Item Type Map
     * Mapping between productTypeId and returnItemTypeId for product order items, orderItemTypeId and returnItemTypeId for other           order items, or orderAdjustmentTypeId and returnAdjustmentTypeId.  Separate mappings for different types of returns (customer vs. vendor)
     */
    @Entity(
        name = "ReturnItemTypeMap",
        packageName = "org.ofbiz.order.return",
        title = "Return Item Type Map",
        description = "Mapping between productTypeId and returnItemTypeId for product order items, orderItemTypeId and returnItemTypeId for other\n          order items, or orderAdjustmentTypeId and returnAdjustmentTypeId.  Separate mappings for different types of returns (customer vs. vendor)",
        fields = {
            @Field(name = "returnItemMapKey", type = "id-ne"),
            @Field(name = "returnHeaderTypeId", type = "id-ne"),
            @Field(name = "returnItemTypeId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnItemMapKey"),
            @PrimaryKey(field = "returnHeaderTypeId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ReturnItemType",
                keyMaps = {
                    @KeyMap(fieldName = "returnItemTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnHeaderType",
                fkName = "RETITMMAP_RETTYP",
                keyMaps = {
                    @KeyMap(fieldName = "returnHeaderTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ReturnAdjustmentType",
                keyMaps = {
                    @KeyMap(fieldName = "returnItemTypeId", relFieldName = "returnAdjustmentTypeId")
                }
            )
        }
    )
    public interface ReturnItemTypeMapEntity {}

    /**
     * Return Reason
     */
    @Entity(
        name = "ReturnReason",
        packageName = "org.ofbiz.order.return",
        title = "Return Reason",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "returnReasonId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "sequenceId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnReasonId")
        }
    )
    public interface ReturnReasonEntity {}

    /**
     * Return Status History
     */
    @Entity(
        name = "ReturnStatus",
        packageName = "org.ofbiz.order.return",
        title = "Return Status History",
        fields = {
            @Field(name = "returnStatusId", type = "id-ne"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "returnId", type = "id"),
            @Field(name = "returnItemSeqId", type = "id"),
            @Field(name = "changeByUserLoginId", type = "id-vlong"),
            @Field(name = "statusDatetime", type = "date-time")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnStatusId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "RTN_STTS_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnHeader",
                fkName = "RTN_STTS_RTN",
                keyMaps = {
                    @KeyMap(fieldName = "returnId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ReturnItem",
                keyMaps = {
                    @KeyMap(fieldName = "returnId"),
                    @KeyMap(fieldName = "returnItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "UserLogin",
                title = "ChangeBy",
                fkName = "RTN_STTS_USRLGN",
                keyMaps = {
                    @KeyMap(fieldName = "changeByUserLoginId", relFieldName = "userLoginId")
                }
            )
        }
    )
    public interface ReturnStatusEntity {}

    /**
     * Return Type
     */
    @Entity(
        name = "ReturnType",
        packageName = "org.ofbiz.order.return",
        title = "Return Type",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "returnTypeId", type = "id-ne"),
            @Field(name = "description", type = "description"),
            @Field(name = "sequenceId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnTypeId")
        }
    )
    public interface ReturnTypeEntity {}

    /**
     * Records the quantity and amount returned to an inventory item from a return item.
     */
    @Entity(
        name = "ReturnItemBilling",
        packageName = "org.ofbiz.order.return",
        title = "Records the quantity and amount returned to an inventory item from a return item.",
        neverCache = true,
        fields = {
            @Field(name = "returnId", type = "id-ne"),
            @Field(name = "returnItemSeqId", type = "id-ne"),
            @Field(name = "invoiceId", type = "id-ne"),
            @Field(name = "invoiceItemSeqId", type = "id-ne"),
            @Field(name = "shipmentReceiptId", type = "id"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "amount", type = "currency-amount")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnId"),
            @PrimaryKey(field = "returnItemSeqId"),
            @PrimaryKey(field = "invoiceId"),
            @PrimaryKey(field = "invoiceItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnHeader",
                fkName = "RTN_ITBLNG_RHDR",
                keyMaps = {
                    @KeyMap(fieldName = "returnId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnItem",
                fkName = "RTN_ITBLNG_RITM",
                keyMaps = {
                    @KeyMap(fieldName = "returnId"),
                    @KeyMap(fieldName = "returnItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "Invoice",
                fkName = "RITBL_INVOICE",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "InvoiceItem",
                fkName = "RETURN_ITBLNG_IITM",
                keyMaps = {
                    @KeyMap(fieldName = "invoiceId"),
                    @KeyMap(fieldName = "invoiceItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentReceipt",
                fkName = "RITBL_SHIPRCPT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentReceiptId", relFieldName = "receiptId")
                }
            )
        }
    )
    public interface ReturnItemBillingEntity {}

    /**
     * Return Item And Shipment Association
     */
    @Entity(
        name = "ReturnItemShipment",
        packageName = "org.ofbiz.order.return",
        title = "Return Item And Shipment Association",
        fields = {
            @Field(name = "returnId", type = "id-ne"),
            @Field(name = "returnItemSeqId", type = "id-ne"),
            @Field(name = "shipmentId", type = "id-ne"),
            @Field(name = "shipmentItemSeqId", type = "id-ne"),
            @Field(name = "quantity", type = "fixed-point")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnId"),
            @PrimaryKey(field = "returnItemSeqId"),
            @PrimaryKey(field = "shipmentId"),
            @PrimaryKey(field = "shipmentItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnHeader",
                fkName = "RIT_SHPMT_RHDR",
                keyMaps = {
                    @KeyMap(fieldName = "returnId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnItem",
                fkName = "RIT_SHPMT_RITM",
                keyMaps = {
                    @KeyMap(fieldName = "returnId"),
                    @KeyMap(fieldName = "returnItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Shipment",
                fkName = "RIT_SHPMT_SHPMT",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShipmentItem",
                fkName = "RIT_SHPMT_SHPITM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentId"),
                    @KeyMap(fieldName = "shipmentItemSeqId")
                }
            )
        }
    )
    public interface ReturnItemShipmentEntity {}

    /**
     * Retrun Contact Mechanism
     */
    @Entity(
        name = "ReturnContactMech",
        packageName = "org.ofbiz.order.return",
        title = "Retrun Contact Mechanism",
        neverCache = true,
        fields = {
            @Field(name = "returnId", type = "id-ne"),
            @Field(name = "contactMechPurposeTypeId", type = "id-ne"),
            @Field(name = "contactMechId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "returnId"),
            @PrimaryKey(field = "contactMechPurposeTypeId"),
            @PrimaryKey(field = "contactMechId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ReturnHeader",
                fkName = "RETURN_CMECH_HDR",
                keyMaps = {
                    @KeyMap(fieldName = "returnId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "RETURN_CMECH_CM",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMechPurposeType",
                fkName = "RETURN_CMECH_CMPT",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechPurposeTypeId")
                }
            )
        }
    )
    public interface ReturnContactMechEntity {}

    /**
     * Cart Abandoned
     */
    @Entity(
        name = "CartAbandoned",
        packageName = "org.ofbiz.order.shoppingcart",
        title = "Cart Abandoned",
        neverCache = true,
        fields = {
            @Field(name = "visitId", type = "id-ne"),
            @Field(name = "fromDate", type = "date-time"),
            @Field(name = "thruDate", type = "date-time"),
            @Field(name = "productStoreId", type = "id-ne"),
            @Field(name = "webSiteId", type = "id-ne"),
            @Field(name = "currencyUomId", type = "id"),
            @Field(name = "locale", type = "very-short")
        },
        primaryKeys = {
            @PrimaryKey(field = "visitId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Visit",
                fkName = "CART_AB_VST",
                keyMaps = {
                    @KeyMap(fieldName = "visitId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WebSite",
                fkName = "CART_AB_WS",
                keyMaps = {
                    @KeyMap(fieldName = "webSiteId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "CART_AB_PRDSTR",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Uom",
                title = "Currency",
                fkName = "CART_AB_CUOM",
                keyMaps = {
                    @KeyMap(fieldName = "currencyUomId", relFieldName = "uomId")
                }
            )
        }
    )
    public interface CartAbandonedEntity {}

    /**
     * Cart Abandoned Status
     */
    @Entity(
        name = "CartAbandonedStatus",
        packageName = "org.ofbiz.order.shoppingcart",
        title = "Cart Abandoned Status",
        neverCache = true,
        fields = {
            @Field(name = "visitId", type = "id-ne"),
            @Field(name = "statusId", type = "id"),
            @Field(name = "visitHash", type = "long-varchar"),
            @Field(name = "reminderRetrySeq", type = "numeric")
        },
        primaryKeys = {
            @PrimaryKey(field = "visitId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "CART_AB_STTS_STI",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CartAbandoned",
                fkName = "CART_AB_STTS",
                keyMaps = {
                    @KeyMap(fieldName = "visitId")
                }
            )
        }
    )
    public interface CartAbandonedStatusEntity {}

    /**
     * Cart Abandoned Line
     */
    @Entity(
        name = "CartAbandonedLine",
        packageName = "org.ofbiz.order.shoppingcart",
        title = "Cart Abandoned Line",
        neverCache = true,
        fields = {
            @Field(name = "visitId", type = "id-ne"),
            @Field(name = "cartAbandonedLineSeqId", type = "id-ne"),
            @Field(name = "productId", type = "id-ne"),
            @Field(name = "prodCatalogId", type = "id-ne"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "reservStart", type = "date-time"),
            @Field(name = "reservLength", type = "fixed-point"),
            @Field(name = "reservPersons", type = "fixed-point"),
            @Field(name = "unitPrice", type = "currency-amount"),
            @Field(name = "reserv2ndPPPerc", type = "fixed-point"),
            @Field(name = "reservNthPPPerc", type = "fixed-point"),
            @Field(name = "configId", type = "id"),
            @Field(name = "totalWithAdjustments", type = "currency-amount"),
            @Field(name = "wasReserved", type = "indicator")
        },
        primaryKeys = {
            @PrimaryKey(field = "visitId"),
            @PrimaryKey(field = "cartAbandonedLineSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "CART_ABLN_PRD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProdCatalog",
                fkName = "CART_ABLN_PRDCAT",
                keyMaps = {
                    @KeyMap(fieldName = "prodCatalogId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CartAbandoned",
                fkName = "CART_ABNDND",
                keyMaps = {
                    @KeyMap(fieldName = "visitId")
                }
            )
        }
    )
    public interface CartAbandonedLineEntity {}

    /**
     * Shopping List
     */
    @Entity(
        name = "ShoppingList",
        packageName = "org.ofbiz.order.shoppinglist",
        title = "Shopping List",
        fields = {
            @Field(name = "shoppingListId", type = "id-ne"),
            @Field(name = "shoppingListTypeId", type = "id"),
            @Field(name = "parentShoppingListId", type = "id"),
            @Field(name = "productStoreId", type = "id"),
            @Field(name = "visitorId", type = "id"),
            @Field(name = "partyId", type = "id"),
            @Field(name = "listName", type = "name"),
            @Field(name = "description", type = "description"),
            @Field(name = "isPublic", type = "indicator"),
            @Field(name = "isActive", type = "indicator"),
            @Field(name = "currencyUom", type = "id"),
            @Field(name = "shipmentMethodTypeId", type = "id"),
            @Field(name = "carrierPartyId", type = "id"),
            @Field(name = "carrierRoleTypeId", type = "id"),
            @Field(name = "contactMechId", type = "id"),
            @Field(name = "paymentMethodId", type = "id"),
            @Field(name = "recurrenceInfoId", type = "id"),
            @Field(name = "lastOrderedDate", type = "date-time"),
            @Field(name = "lastAdminModified", type = "date-time"),
            @Field(name = "productPromoCodeId", type = "id"),
            @Field(name = "shoppingListAuthToken", type = "long-varchar", description = "Token used to authenticate anonymous shopping list cookie (SCIPIO)."),
            @Field(name = "userAddr", type = "description", description = "For guest lists, this is address information of the user, or simply the I.P. address (SCIPIO)."),
            @Field(name = "isUserDefault", type = "indicator", description = "Optional flag to indicate the default list to use (SCIPIO), per-user (partyId or device), per-shoppingListTypeId.")
        },
        primaryKeys = {
            @PrimaryKey(field = "shoppingListId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShoppingList",
                title = "Parent",
                fkName = "SHLIST_PARENT",
                keyMaps = {
                    @KeyMap(fieldName = "parentShoppingListId", relFieldName = "shoppingListId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ShoppingList",
                title = "Sibling",
                keyMaps = {
                    @KeyMap(fieldName = "parentShoppingListId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShoppingListType",
                fkName = "SHLIST_TYPE",
                keyMaps = {
                    @KeyMap(fieldName = "shoppingListTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductStore",
                fkName = "SHLIST_PRDS",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Party",
                fkName = "SHLIST_PTY",
                keyMaps = {
                    @KeyMap(fieldName = "partyId")
                }
            ),
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductStoreShipmentMeth",
                fkName = "SHLIST_PSSM",
                keyMaps = {
                    @KeyMap(fieldName = "productStoreId"),
                    @KeyMap(fieldName = "shipmentMethodTypeId"),
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId"),
                    @KeyMap(fieldName = "carrierRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "CarrierShipmentMethod",
                fkName = "SHLIST_CSSM",
                keyMaps = {
                    @KeyMap(fieldName = "shipmentMethodTypeId"),
                    @KeyMap(fieldName = "carrierPartyId", relFieldName = "partyId"),
                    @KeyMap(fieldName = "carrierRoleTypeId", relFieldName = "roleTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ContactMech",
                fkName = "SHLIST_CMECH",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PostalAddress",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "PaymentMethod",
                fkName = "SHLIST_PYMETH",
                keyMaps = {
                    @KeyMap(fieldName = "paymentMethodId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "RecurrenceInfo",
                fkName = "SHLIST_RECINFO",
                keyMaps = {
                    @KeyMap(fieldName = "recurrenceInfoId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ProductPromoCode",
                fkName = "SHLIST_PRMCD",
                keyMaps = {
                    @KeyMap(fieldName = "productPromoCodeId")
                }
            )
        }
    )
    public interface ShoppingListEntity {}

    /**
     * Shopping List Item
     */
    @Entity(
        name = "ShoppingListItem",
        packageName = "org.ofbiz.order.shoppinglist",
        title = "Shopping List Item",
        fields = {
            @Field(name = "shoppingListId", type = "id-ne"),
            @Field(name = "shoppingListItemSeqId", type = "id-ne"),
            @Field(name = "productId", type = "id"),
            @Field(name = "quantity", type = "fixed-point"),
            @Field(name = "modifiedPrice", type = "currency-precise"),
            @Field(name = "reservStart", type = "date-time"),
            @Field(name = "reservLength", type = "fixed-point"),
            @Field(name = "reservPersons", type = "fixed-point"),
            @Field(name = "quantityPurchased", type = "fixed-point"),
            @Field(name = "configId", type = "id")
        },
        primaryKeys = {
            @PrimaryKey(field = "shoppingListId"),
            @PrimaryKey(field = "shoppingListItemSeqId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShoppingList",
                fkName = "SHLIST_ITEM_LIST",
                keyMaps = {
                    @KeyMap(fieldName = "shoppingListId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "Product",
                fkName = "SHLIST_ITEM_PROD",
                keyMaps = {
                    @KeyMap(fieldName = "productId")
                }
            )
        }
    )
    public interface ShoppingListItemEntity {}

    /**
     * Shopping List Item
     */
    @Entity(
        name = "ShoppingListItemSurvey",
        packageName = "org.ofbiz.order.shoppinglist",
        title = "Shopping List Item",
        fields = {
            @Field(name = "shoppingListId", type = "id-ne"),
            @Field(name = "shoppingListItemSeqId", type = "id-ne"),
            @Field(name = "surveyResponseId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "shoppingListId"),
            @PrimaryKey(field = "shoppingListItemSeqId"),
            @PrimaryKey(field = "surveyResponseId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShoppingList",
                fkName = "SHLIST_ITSUR_LIST",
                keyMaps = {
                    @KeyMap(fieldName = "shoppingListId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShoppingListItem",
                fkName = "SHLIST_ITSUR_ITEM",
                keyMaps = {
                    @KeyMap(fieldName = "shoppingListId"),
                    @KeyMap(fieldName = "shoppingListItemSeqId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "SurveyResponse",
                fkName = "SHLIST_ITSUR_RESP",
                keyMaps = {
                    @KeyMap(fieldName = "surveyResponseId")
                }
            )
        }
    )
    public interface ShoppingListItemSurveyEntity {}

    /**
     * Shopping List Type
     */
    @Entity(
        name = "ShoppingListType",
        packageName = "org.ofbiz.order.shoppinglist",
        title = "Shopping List Type",
        defaultResourceName = "OrderEntityLabels",
        fields = {
            @Field(name = "shoppingListTypeId", type = "id-ne"),
            @Field(name = "description", type = "description")
        },
        primaryKeys = {
            @PrimaryKey(field = "shoppingListTypeId")
        }
    )
    public interface ShoppingListTypeEntity {}

    /**
     * ShoppingList WorkEffort
     */
    @Entity(
        name = "ShoppingListWorkEffort",
        packageName = "org.ofbiz.order.shoppinglist",
        title = "ShoppingList WorkEffort",
        fields = {
            @Field(name = "shoppingListId", type = "id-ne"),
            @Field(name = "workEffortId", type = "id-ne")
        },
        primaryKeys = {
            @PrimaryKey(field = "shoppingListId"),
            @PrimaryKey(field = "workEffortId")
        },
        relations = {
            @Relation(
                type = RelationType.ONE,
                relEntityName = "ShoppingList",
                fkName = "SHLISTWE_SHLST",
                keyMaps = {
                    @KeyMap(fieldName = "shoppingListId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "WorkEffort",
                fkName = "SHLISTWE_WEFF",
                keyMaps = {
                    @KeyMap(fieldName = "workEffortId")
                }
            )
        }
    )
    public interface ShoppingListWorkEffortEntity {}

    /**
     * Order Contact Detail View
     */
    @ViewEntity(
        name = "OrderAndContactMech",
        packageName = "org.ofbiz.order.order",
        title = "Order Contact Detail View",
        members = {
            @MemberEntity(entityAlias = "OCM", entityName = "OrderContactMech"),
            @MemberEntity(entityAlias = "CMD", entityName = "ContactMechDetail")
        },
        aliasAlls = {
            @AliasAll(entityAlias = "OCM"),
            @AliasAll(entityAlias = "CMD")
        },
        viewLinks = {
            @ViewLink(
                entityAlias = "OCM",
                relEntityAlias = "CMD",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "OrderContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "orderId"),
                    @KeyMap(fieldName = "contactMechPurposeTypeId"),
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContactMech",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "ContactMechPurposeType",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechPurposeTypeId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "PostalAddress",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "TelecomNumber",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            ),
            @Relation(
                type = RelationType.ONE_NOFK,
                relEntityName = "FtpAddress",
                keyMaps = {
                    @KeyMap(fieldName = "contactMechId")
                }
            )
        }
    )
    public interface OrderAndContactMechView {}

    @ExtendEntity(
        name = "OrderHeader",
        fields = {
            @Field(name = "marketplaceId", type = "id-ne"),
            @Field(name = "marketplaceStatusId", type = "id")
        },
        relations = {
            @Relation(
                type = RelationType.MANY,
                relEntityName = "ProductStoreMarketplace",
                fkName = "ORDER_HDR_MRKTPLC",
                keyMaps = {
                    @KeyMap(fieldName = "marketplaceId"),
                    @KeyMap(fieldName = "productStoreId")
                }
            ),
            @Relation(
                type = RelationType.ONE,
                relEntityName = "StatusItem",
                fkName = "ORDER_MRKTPL_STATUS",
                keyMaps = {
                    @KeyMap(fieldName = "statusId")
                }
            )
        }
    )
    public interface OrderHeaderExtension {}

}

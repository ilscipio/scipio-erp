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
package com.ilscipio.scipio.order.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Services {

    @Service(
        name = "orderNotificationInterface",
        engine = "interface",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "emailType", type = "String", mode = "OUT"),
            @Attribute(name = "screenUri", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "body", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "sendTo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sendCc", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "sendBcc", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "note", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "temporaryAnonymousUserLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "messageWrapper", type = "org.ofbiz.service.mail.MimeMessageWrapper", mode = "OUT", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "subject", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "communicationEventId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface OrderNotificationInterface {}

    /**
     * Send a order confirmation
     */
    @Service(
        name = "sendOrderConfirmation",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "sendOrderConfirmNotification",
        description = "Send a order confirmation",
        requireNewTransaction = "true",
        maxRetry = "3",
        implemented = {@Implements(service = "orderNotificationInterface")}
    )
    public interface SendOrderConfirmation {}

    /**
     * Send a order notification
     */
    @Service(
        name = "sendOrderChangeNotification",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "sendOrderChangeNotification",
        description = "Send a order notification",
        requireNewTransaction = "true",
        maxRetry = "3",
        implemented = {@Implements(service = "orderNotificationInterface")}
    )
    public interface SendOrderChangeNotification {}

    /**
     * Send a order notification
     */
    @Service(
        name = "sendOrderCompleteNotification",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "sendOrderCompleteNotification",
        description = "Send a order notification",
        requireNewTransaction = "true",
        maxRetry = "3",
        implemented = {@Implements(service = "orderNotificationInterface")}
    )
    public interface SendOrderCompleteNotification {}

    /**
     * Send a order notification
     */
    @Service(
        name = "sendOrderBackorderNotification",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "sendOrderBackorderNotification",
        description = "Send a order notification",
        requireNewTransaction = "true",
        maxRetry = "3",
        implemented = {@Implements(service = "orderNotificationInterface")}
    )
    public interface SendOrderBackorderNotification {}

    /**
     * Send a order notification
     */
    @Service(
        name = "sendOrderPayRetryNotification",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "sendOrderPayRetryNotification",
        description = "Send a order notification",
        requireNewTransaction = "true",
        maxRetry = "3",
        implemented = {@Implements(service = "orderNotificationInterface")}
    )
    public interface SendOrderPayRetryNotification {}

    /**
     * Send a order notification
     */
    @Service(
        name = "sendOrderPayChangeNotification",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "sendOrderPayChangeNotification",
        description = "Send a order notification",
        requireNewTransaction = "true",
        maxRetry = "3",
        implemented = {@Implements(service = "orderNotificationInterface")}
    )
    public interface SendOrderPayChangeNotification {}

    /**
     * Send a order notification
     */
    @Service(
        name = "sendOrderPayCompleteNotification",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "sendOrderPayCompleteNotification",
        description = "Send a order notification",
        requireNewTransaction = "true",
        maxRetry = "3",
        implemented = {@Implements(service = "orderNotificationInterface")}
    )
    public interface SendOrderPayCompleteNotification {}

    /**
     * Limit Service for order processing workflow; sends activitiy notifications
     */
    @Service(
        name = "sendProcessNotification",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "sendProcessNotification",
        description = "Limit Service for order processing workflow; sends activitiy notifications",
        requireNewTransaction = "true",
        maxRetry = "3",
        attributes = {
            @Attribute(name = "workEffortId", type = "String", mode = "IN"),
            @Attribute(name = "adminEmailList", type = "String", mode = "IN"),
            @Attribute(name = "assignedPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "assignedRoleTypeId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SendProcessNotification {}

    /**
     * Logs when a notification was sent
     */
    @Service(
        name = "createOrderNotificationLog",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderSimpleMethods.xml",
        invoke = "createNotificationLog",
        description = "Logs when a notification was sent",
        defaultEntityName = "OrderNotification",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk")
        },
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "emailType", type = "String", mode = "IN"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateOrderNotificationLog {}

    /**
     * Creates order entities
     */
    @Service(
        name = "storeOrder",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "createOrder",
        description = "Creates order entities",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "placingCustomerPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToCustomerPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipToCustomerPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "endUserCustomerPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billFromVendorPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipFromVendorPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "supplierAgentPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "visitId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "affiliateId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "originFacilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "transactionId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "terminalId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "workEffortId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "autoOrderShoppingListId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webSiteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "externalId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "internalCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "distributorId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderTypeId", type = "String", mode = "INOUT"),
            @Attribute(name = "salesChannelEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemGroups", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderItems", type = "List", mode = "IN"),
            @Attribute(name = "orderTerms", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "workEfforts", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderAdjustments", type = "List", mode = "IN"),
            @Attribute(name = "billingAccountId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shippingAmount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "firstAttemptOrderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currencyUom", type = "String", mode = "IN"),
            @Attribute(name = "grandTotal", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "taxAmount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "orderDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemShipGroupInfo", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemAttributes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderAttributes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderPaymentInfo", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderContactMechs", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemContactMechs", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemPriceInfos", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderProductPromoUses", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderProductPromoCodes", type = "Set", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemSurveyResponses", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "trackingCodeOrders", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderAdditionalPartyRoleMap", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemAssociations", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderInternalNotes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderNotes", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "supplierPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "originOrderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "marketplaceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "OUT")
        }
    )
    public interface StoreOrder {}

    @Service(
        name = "callProcessOrderPayments",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "callProcessOrderPayments",
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "manualHold", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface CallProcessOrderPayments {}

    @Service(
        name = "createOrderFromShoppingCart",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "createOrderFromShoppingCart",
        attributes = {
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "INOUT"),
            @Attribute(name = "orderId", type = "String", mode = "OUT")
        }
    )
    public interface CreateOrderFromShoppingCart {}

    @Service(
        name = "createSimpleNonProductSalesOrder",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "createSimpleNonProductSalesOrder",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "currency", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "itemMap", type = "Map", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "OUT")
        }
    )
    public interface CreateSimpleNonProductSalesOrder {}

    /**
     * Create a new order item billing record
     */
    @Service(
        name = "createOrderItemBilling",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderSimpleMethods.xml",
        invoke = "createOrderItemBilling",
        description = "Create a new order item billing record",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "itemIssuanceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentReceiptId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateOrderItemBilling {}

    /**
     * Permission service for the creation and editing of order adjustments
     */
    @Service(
        name = "orderAdjustmentPermissionCheck",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderSimpleMethods.xml",
        invoke = "orderAdjustmentPermissionCheck",
        description = "Permission service for the creation and editing of order adjustments",
        implemented = {@Implements(service = "permissionInterface")}
    )
    public interface OrderAdjustmentPermissionCheck {}

    /**
     * Creates a new order adjustment record
     */
    @Service(
        name = "createOrderAdjustment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderSimpleMethods.xml",
        invoke = "createOrderAdjustment",
        description = "Creates a new order adjustment record",
        defaultEntityName = "OrderAdjustment",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "orderAdjustmentPermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "orderAdjustmentTypeId", optional = "false"),
            @OverrideAttribute(name = "orderId", optional = "false")
        }
    )
    public interface CreateOrderAdjustment {}

    /**
     * Update an order adjustment record
     */
    @Service(
        name = "updateOrderAdjustment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderSimpleMethods.xml",
        invoke = "updateOrderAdjustment",
        description = "Update an order adjustment record",
        defaultEntityName = "OrderAdjustment",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "orderId", optional = "false")
        }
    )
    public interface UpdateOrderAdjustment {}

    /**
     * Delete an order adjustment record
     */
    @Service(
        name = "deleteOrderAdjustment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderSimpleMethods.xml",
        invoke = "deleteOrderAdjustment",
        description = "Delete an order adjustment record",
        defaultEntityName = "OrderAdjustment",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "productPromoCodeId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface DeleteOrderAdjustment {}

    /**
     * Create a new order adjustment billing record
     */
    @Service(
        name = "createOrderAdjustmentBilling",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderSimpleMethods.xml",
        invoke = "createOrderAdjustmentBilling",
        description = "Create a new order adjustment billing record",
        attributes = {
            @Attribute(name = "orderAdjustmentId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface CreateOrderAdjustmentBilling {}

    /**
     * Creates a payment using the order payment preference
     */
    @Service(
        name = "createPaymentFromPreference",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "createPaymentFromPreference",
        description = "Creates a payment using the order payment preference",
        attributes = {
            @Attribute(name = "orderPaymentPreferenceId", type = "String", mode = "IN"),
            @Attribute(name = "paymentFromId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentRefNum", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "eventDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "paymentId", type = "String", mode = "OUT")
        }
    )
    public interface CreatePaymentFromPreference {}

    /**
     * Sets the tracking number on a shipment preference
     */
    @Service(
        name = "updateTrackingNumber",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "updateTrackingNumber",
        description = "Sets the tracking number on a shipment preference",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "trackingNumber", type = "String", mode = "IN")
        }
    )
    public interface UpdateTrackingNumber {}

    /**
     * Reset the grandTotal of an existing order
     */
    @Service(
        name = "resetGrandTotal",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "resetGrandTotal",
        description = "Reset the grandTotal of an existing order",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface ResetGrandTotal {}

    /**
     * Find all OrderHeaders with no grandTotal and call resetGrandTotal
     */
    @Service(
        name = "setEmptyGrandTotals",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "setEmptyGrandTotals",
        description = "Find all OrderHeaders with no grandTotal and call resetGrandTotal",
        attributes = {
            @Attribute(name = "forceAll", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface SetEmptyGrandTotals {}

    /**
     * Adjust the order shipping amount
     */
    @Service(
        name = "recalcShippingTotal",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "recalcOrderShipping",
        description = "Adjust the order shipping amount",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface RecalcShippingTotal {}

    /**
     * Adjust the order tax amount
     */
    @Service(
        name = "recalcTaxTotal",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "recalcOrderTax",
        description = "Adjust the order tax amount",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface RecalcTaxTotal {}

    /**
     * Change the status of an existing order
     */
    @Service(
        name = "changeOrderStatus",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "setOrderStatus",
        description = "Change the status of an existing order",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "setItemStatus", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT"),
            @Attribute(name = "orderStatusId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "orderTypeId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "needsInventoryIssuance", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "grandTotal", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "changeReason", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ChangeOrderStatus {}

    /**
     * Change the status of an existing order item.  If no orderItemSeqId is specified, the status of all order items will be changed.
     */
    @Service(
        name = "changeOrderItemStatus",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "setItemStatus",
        description = "Change the status of an existing order item.  If no orderItemSeqId is specified, the status of all order items will be changed.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromStatusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "statusDateTime", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "changeReason", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "checkOutPaymentId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ChangeOrderItemStatus {}

    /**
     * Cancel an Order Item Quantity
     */
    @Service(
        name = "cancelOrderItem",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "cancelOrderItem",
        description = "Cancel an Order Item Quantity",
        auth = "true",
        transactionTimeout = "3000",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cancelQuantity", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "itemReasonMap", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "itemCommentMap", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "itemQtyMap", type = "Map", mode = "IN", optional = "true")
        }
    )
    public interface CancelOrderItem {}

    /**
     * Cancel an Order Item Quantity. This is equal to cancelOrderItem but no ECAs are attached to this service.
     */
    @Service(
        name = "cancelOrderItemNoActions",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "cancelOrderItem",
        description = "Cancel an Order Item Quantity. This is equal to cancelOrderItem but no ECAs are attached to this service.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cancelQuantity", type = "BigDecimal", mode = "IN", optional = "true")
        }
    )
    public interface CancelOrderItemNoActions {}

    /**
     * Update the quantities/prices for an existing order
     */
    @Service(
        name = "updateOrderItems",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "updateApprovedOrderItems",
        description = "Update the quantities/prices for an existing order",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "INOUT"),
            @Attribute(name = "supplierPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "calcTax", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "itemDescriptionMap", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "itemQtyMap", type = "Map", mode = "IN"),
            @Attribute(name = "itemPriceMap", type = "Map", mode = "IN"),
            @Attribute(name = "overridePriceMap", type = "Map", mode = "IN"),
            @Attribute(name = "itemReasonMap", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "itemCommentMap", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "itemAttributesMap", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "itemShipDateMap", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "itemDeliveryDateMap", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "OUT")
        }
    )
    public interface UpdateOrderItems {}

    /**
     * Load an existing shopping cart
     */
    @Service(
        name = "loadCartForUpdate",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "loadCartForUpdate",
        description = "Load an existing shopping cart",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "INOUT"),
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "OUT")
        }
    )
    public interface LoadCartForUpdate {}

    /**
     * Update the quantities/prices for an existing order
     */
    @Service(
        name = "saveUpdatedCartToOrder",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "saveUpdatedCartToOrder",
        description = "Update the quantities/prices for an existing order",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "INOUT"),
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN"),
            @Attribute(name = "calcTax", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "deleteItems", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "changeMap", type = "Map", mode = "IN")
        }
    )
    public interface SaveUpdatedCartToOrder {}

    /**
     * Append an item to an existing order
     */
    @Service(
        name = "appendOrderItem",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "addItemToApprovedOrder",
        description = "Append an item to an existing order",
        auth = "true",
        transactionTimeout = "7200",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "INOUT"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "prodCatalogId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "basePrice", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "overridePrice", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "reasonEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItemTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "changeComments", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "itemDesiredDeliveryDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "itemAttributesMap", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "calcTax", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true"),
            @Attribute(name = "shoppingCart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "OUT")
        }
    )
    public interface AppendOrderItem {}

    /**
     * Remove all existing order adjustments, recalc them and persist in OrderAdjustment.
     */
    @Service(
        name = "recreateOrderAdjustments",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "recreateOrderAdjustments",
        description = "Remove all existing order adjustments, recalc them and persist in OrderAdjustment.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "orderAdjustmentPermissionCheck", mainAction = "UPDATE")
    )
    public interface RecreateOrderAdjustments {}

    /**
     * Process payments for an order
     */
    @Service(
        name = "processOrderPayments",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "processOrderPayments",
        description = "Process payments for an order",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface ProcessOrderPayments {}

    @Service(
        name = "updateOrderPaymentPreference",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "updateOrderPaymentPreference",
        defaultEntityName = "OrderPaymentPreference",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "checkOutPaymentId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateOrderPaymentPreference {}

    /**
     * Check the status of all items and cancel/approve/complete the order if we can
     */
    @Service(
        name = "checkOrderItemStatus",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "checkItemStatus",
        description = "Check the status of all items and cancel/approve/complete the order if we can",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface CheckOrderItemStatus {}

    /**
     * Updates the (purchase) order/order item status based on receipt
     */
    @Service(
        name = "updateOrderStatusFromReceipt",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderSimpleMethods.xml",
        invoke = "updateOrderStatusFromReceipt",
        description = "Updates the (purchase) order/order item status based on receipt",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "currentStatusId", type = "String", mode = "OUT")
        }
    )
    public interface UpdateOrderStatusFromReceipt {}

    /**
     * Batch service which automatically cancels sales order and/or sales order items.                 Sales orders : These will be cancelled if the order status equals CREATED and it has been                   either 30 days or ProductStore.daysCancelNoPay since the order was created. A value of 0 for                   ProductStore.daysCancelNoPay means do not auto-cancel.                 Sales order items : This is only for orders on the APPROVED status. Items will be cancelled if the                   item is flagged with an autoCancelDate and does not have a dontCancelDate and dontCancelUserLogin                   associated with it, and it is past the autoCancelDate.
     */
    @Service(
        name = "autoCancelOrderItems",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "cancelFlaggedSalesOrders",
        description = "Batch service which automatically cancels sales order and/or sales order items.\n                Sales orders : These will be cancelled if the order status equals CREATED and it has been\n                  either 30 days or ProductStore.daysCancelNoPay since the order was created. A value of 0 for\n                  ProductStore.daysCancelNoPay means do not auto-cancel.\n                Sales order items : This is only for orders on the APPROVED status. Items will be cancelled if the\n                  item is flagged with an autoCancelDate and does not have a dontCancelDate and dontCancelUserLogin\n                  associated with it, and it is past the autoCancelDate."
    )
    public interface AutoCancelOrderItems {}

    /**
     * Set the Allow Split Flag To 'Y' (true)
     */
    @Service(
        name = "setAllowOrderSplit",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "allowOrderSplit",
        description = "Set the Allow Split Flag To 'Y' (true)",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN")
        }
    )
    public interface SetAllowOrderSplit {}

    /**
     * Compute and return the OrderItemShipGroup estimated ship date based on the associated items.
     */
    @Service(
        name = "getOrderItemShipGroupEstimatedShipDate",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "getOrderItemShipGroupEstimatedShipDate",
        description = "Compute and return the OrderItemShipGroup estimated ship date based on the associated items.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "estimatedShipDate", type = "Timestamp", mode = "OUT", optional = "true")
        }
    )
    public interface GetOrderItemShipGroupEstimatedShipDate {}

    /**
     * Adds a RoleType to an order
     */
    @Service(
        name = "addOrderRole",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "addRoleType",
        description = "Adds a RoleType to an order",
        attributes = {
            @Attribute(name = "removeOld", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN")
        }
    )
    public interface AddOrderRole {}

    /**
     * Removes a RoleType from an order
     */
    @Service(
        name = "removeOrderRole",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "removeRoleType",
        description = "Removes a RoleType from an order",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN")
        }
    )
    public interface RemoveOrderRole {}

    /**
     * Creates an order payment preference
     */
    @Service(
        name = "createOrderPaymentPreference",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "createPaymentPreference",
        description = "Creates an order payment preference",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "paymentMethodTypeId", type = "String", mode = "IN"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "maxAmount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "orderPaymentPreferenceId", type = "String", mode = "OUT")
        }
    )
    public interface CreateOrderPaymentPreference {}

    /**
     * Create a note item and associate with a order header
     */
    @Service(
        name = "createOrderNote",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "createOrderNote",
        description = "Create a note item and associate with a order header",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "note", type = "String", mode = "IN", allowHtml = "any"),
            @Attribute(name = "internalNote", type = "String", mode = "IN"),
            @Attribute(name = "noteName", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateOrderNote {}

    /**
     * Toggle Order Note and make it either Public or Private
     */
    @Service(
        name = "updateOrderNote",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "updateOrderNote",
        description = "Toggle Order Note and make it either Public or Private",
        defaultEntityName = "OrderHeaderNote",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "noteId", type = "String", mode = "IN"),
            @Attribute(name = "internalNote", type = "String", mode = "IN")
        }
    )
    public interface UpdateOrderNote {}

    /**
     * Check an order for digital items and invoice/capture + fulfill the items
     */
    @Service(
        name = "checkDigitalItemFulfillment",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "checkDigitalItemFulfillment",
        description = "Check an order for digital items and invoice/capture + fulfill the items",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface CheckDigitalItemFulfillment {}

    /**
     * Order Fulfillment
     */
    @Service(
        name = "fulfillDigitalItems",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "fulfillDigitalItems",
        description = "Order Fulfillment",
        requireNewTransaction = "true",
        transactionTimeout = "300",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItems", type = "java.util.List", mode = "IN")
        }
    )
    public interface FulfillDigitalItems {}

    @Service(
        name = "itemFulfillmentInterface",
        engine = "interface",
        entityAttributes = {
            @EntityAttributes(entityName = "ProductContent", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "orderItem", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        }
    )
    public interface ItemFulfillmentInterface {}

    /**
     * Check an order for service items and invoice the items
     */
    @Service(
        name = "invoiceServiceItems",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "invoiceServiceItems",
        description = "Check an order for service items and invoice the items",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface InvoiceServiceItems {}

    /**
     * Get Ordered Summary Information
     */
    @Service(
        name = "getOrderedSummaryInformation",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "getOrderedSummaryInformation",
        description = "Get Ordered Summary Information",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "roleTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "monthsToInclude", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "totalOrders", type = "Long", mode = "OUT"),
            @Attribute(name = "totalGrandAmount", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "totalSubRemainingAmount", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetOrderedSummaryInformation {}

    /**
     * Get basic order header information.
     */
    @Service(
        name = "getOrderHeaderInformation",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "getOrderHeaderInformation",
        description = "Get basic order header information.",
        validate = "false",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderHeader", mode = "INOUT", include = "pk"),
            @EntityAttributes(entityName = "OrderHeader", mode = "OUT", include = "nonpk", optional = "true")
        }
    )
    public interface GetOrderHeaderInformation {}

    /**
     * Get the total shipping for an order
     */
    @Service(
        name = "getOrderShippingAmount",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "getOrderShippingAmount",
        description = "Get the total shipping for an order",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shippingAmount", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetOrderShippingAmount {}

    /**
     * Gets the order status
     */
    @Service(
        name = "getOrderStatus",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "getOrderStatus",
        description = "Gets the order status",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "OUT")
        }
    )
    public interface GetOrderStatus {}

    /**
     * Creates a delivery schedule for the specified order
     */
    @Service(
        name = "createOrderDeliverySchedule",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderDeliveryServices.xml",
        invoke = "createOrderDeliverySchedule",
        description = "Creates a delivery schedule for the specified order",
        defaultEntityName = "OrderDeliverySchedule",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "orderItemSeqId", optional = "true")
        }
    )
    public interface CreateOrderDeliverySchedule {}

    /**
     * Update an existing delivery schedule for a specified purchase order
     */
    @Service(
        name = "updateOrderDeliverySchedule",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderDeliveryServices.xml",
        invoke = "updateOrderDeliverySchedule",
        description = "Update an existing delivery schedule for a specified purchase order",
        defaultEntityName = "OrderDeliverySchedule",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderDeliverySchedule {}

    /**
     * Send Order Delivery Schedule Notification
     */
    @Service(
        name = "sendOrderDeliveryScheduleNotification",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderDeliveryServices.xml",
        invoke = "sendOrderDeliveryScheduleNotification",
        description = "Send Order Delivery Schedule Notification",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SendOrderDeliveryScheduleNotification {}

    /**
     * Check Supplier Related Order Permission
     */
    @Service(
        name = "checkSupplierRelatedOrderPermission",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderDeliveryServices.xml",
        invoke = "checkSupplierRelatedOrderPermissionService",
        description = "Check Supplier Related Order Permission",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "checkAction", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "callingMethodName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "hasSupplierRelatedPermission", type = "String", mode = "OUT")
        }
    )
    public interface CheckSupplierRelatedOrderPermission {}

    /**
     * Given an orderId, this service will look through all its OrderItems and for each shoppingListItemId               and shoppingListItemSeqId, update the quantity purchased in the ShoppingListItem entity.  Used for               tracking how many of shopping list items are purchased.  This service is mounted as a seca on storeOrder.
     */
    @Service(
        name = "updateShoppingListQuantitiesFromOrder",
        engine = "java",
        location = "org.ofbiz.order.shoppinglist.ShoppingListServices",
        invoke = "updateShoppingListQuantitiesFromOrder",
        description = "Given an orderId, this service will look through all its OrderItems and for each shoppingListItemId\n              and shoppingListItemSeqId, update the quantity purchased in the ShoppingListItem entity.  Used for\n              tracking how many of shopping list items are purchased.  This service is mounted as a seca on storeOrder.",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface UpdateShoppingListQuantitiesFromOrder {}

    @Service(
        name = "shoppingCartRemoteTest",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "shoppingCartRemoteTest",
        attributes = {
            @Attribute(name = "cart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN")
        }
    )
    public interface ShoppingCartRemoteTest {}

    @Service(
        name = "shoppingCartTest",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "shoppingCartTest"
    )
    public interface ShoppingCartTest {}

    /**
     * Create OrderShipment
     */
    @Service(
        name = "createOrderShipment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "createOrderShipment",
        description = "Create OrderShipment",
        defaultEntityName = "OrderShipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderShipment {}

    /**
     * Update OrderShipment
     */
    @Service(
        name = "updateOrderShipment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "updateOrderShipment",
        description = "Update OrderShipment",
        defaultEntityName = "OrderShipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderShipment {}

    /**
     * Delete OrderShipment
     */
    @Service(
        name = "deleteOrderShipment",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "deleteOrderShipment",
        description = "Delete OrderShipment",
        defaultEntityName = "OrderShipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderShipment {}

    /**
     * Interface for Mass Order Change Services
     */
    @Service(
        name = "massOrderChangeInterface",
        engine = "interface",
        description = "Interface for Mass Order Change Services",
        attributes = {
            @Attribute(name = "orderIdList", type = "List", mode = "IN")
        }
    )
    public interface MassOrderChangeInterface {}

    @Service(
        name = "massPickOrders",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "massPickOrders",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "massOrderChangeInterface")}
    )
    public interface MassPickOrders {}

    @Service(
        name = "massChangeOrderApproved",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "massChangeApproved",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "massOrderChangeInterface")}
    )
    public interface MassChangeOrderApproved {}

    @Service(
        name = "massProcessOrders",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "massProcessOrders",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "massOrderChangeInterface")}
    )
    public interface MassProcessOrders {}

    @Service(
        name = "massHoldOrders",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "massHoldOrders",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "massOrderChangeInterface")}
    )
    public interface MassHoldOrders {}

    @Service(
        name = "massCancelOrders",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "massCancelOrders",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "massOrderChangeInterface")}
    )
    public interface MassCancelOrders {}

    @Service(
        name = "massCancelRemainingPurchaseOrderItems",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "massCancelRemainingPurchaseOrderItems",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "massOrderChangeInterface")}
    )
    public interface MassCancelRemainingPurchaseOrderItems {}

    @Service(
        name = "massRejectOrders",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "massRejectOrders",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "massOrderChangeInterface")}
    )
    public interface MassRejectOrders {}

    @Service(
        name = "massQuickShipOrders",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "massQuickShipOrders",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "massOrderChangeInterface")}
    )
    public interface MassQuickShipOrders {}

    @Service(
        name = "massPrintOrders",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "massPrintOrders",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "massOrderChangeInterface")},
        attributes = {
            @Attribute(name = "screenLocation", type = "String", mode = "IN"),
            @Attribute(name = "printerName", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface MassPrintOrders {}

    @Service(
        name = "massCreateFileForOrders",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "massCreateFileForOrders",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "massOrderChangeInterface")},
        attributes = {
            @Attribute(name = "screenLocation", type = "String", mode = "IN")
        }
    )
    public interface MassCreateFileForOrders {}

    /**
     * Get the Next Order ID According to Settings on the PartyAcctgPreference Entity for the given Party
     */
    @Service(
        name = "getNextOrderId",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "getNextOrderId",
        description = "Get the Next Order ID According to Settings on the PartyAcctgPreference Entity for the given Party",
        implemented = {@Implements(service = "storeOrder", optional = "true")},
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "OUT")
        }
    )
    public interface GetNextOrderId {}

    @Service(
        name = "orderSequence_enforced",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "orderSequence_enforced",
        implemented = {@Implements(service = "getNextOrderId")},
        attributes = {
            @Attribute(name = "partyAcctgPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "orderId", type = "Long", mode = "OUT")
        }
    )
    public interface OrderSequenceEnforced {}

    /**
     * Create OrderHeader
     */
    @Service(
        name = "createOrderHeader",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "createOrderHeader",
        description = "Create OrderHeader",
        defaultEntityName = "OrderHeader",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderHeader {}

    /**
     * Update OrderHeader
     */
    @Service(
        name = "updateOrderHeader",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "updateOrderHeader",
        description = "Update OrderHeader",
        defaultEntityName = "OrderHeader",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderHeader {}

    /**
     * Create a Communication Event Order
     */
    @Service(
        name = "createCommunicationEventOrder",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/communication/CommunicationEventServices.xml",
        invoke = "createCommunicationEventOrder",
        description = "Create a Communication Event Order",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEventOrder", mode = "IN", include = "pk")
        }
    )
    public interface CreateCommunicationEventOrder {}

    /**
     * Remove a Communication Event Order
     */
    @Service(
        name = "removeCommunicationEventOrder",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/communication/CommunicationEventServices.xml",
        invoke = "removeCommunicationEventOrder",
        description = "Remove a Communication Event Order",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "CommunicationEventOrder", mode = "IN", include = "pk")
        }
    )
    public interface RemoveCommunicationEventOrder {}

    /**
     * Creates a new OrderItemShipGroup.
     */
    @Service(
        name = "createOrderItemShipGroup",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "createOrderItemShipGroup",
        description = "Creates a new OrderItemShipGroup.",
        defaultEntityName = "OrderItemShipGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateOrderItemShipGroup {}

    /**
     * Updates OrderItemShipGroup.  The shipmentMethod field is of the format ${shipmentMethodTypeId}@${carrierPartyId}
     */
    @Service(
        name = "updateOrderItemShipGroup",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "updateOrderItemShipGroup",
        description = "Updates OrderItemShipGroup.  The shipmentMethod field is of the format ${shipmentMethodTypeId}@${carrierPartyId}",
        defaultEntityName = "OrderItemShipGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "oldContactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentMethod", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateOrderItemShipGroup {}

    /**
     * Create Order Contact Mech
     */
    @Service(
        name = "createOrderContactMech",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "createOrderContactMech",
        description = "Create Order Contact Mech",
        defaultEntityName = "OrderContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface CreateOrderContactMech {}

    /**
     * Update Order Contact Mech
     */
    @Service(
        name = "updateOrderContactMech",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "updateOrderContactMech",
        description = "Update Order Contact Mech",
        defaultEntityName = "OrderContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "oldContactMechId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateOrderContactMech {}

    /**
     * Remove Order Contact Mech
     */
    @Service(
        name = "removeOrderContactMech",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "removeOrderContactMech",
        description = "Remove Order Contact Mech",
        defaultEntityName = "OrderContactMech",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveOrderContactMech {}

    /**
     * Create an Order Term
     */
    @Service(
        name = "createOrderTerm",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "createOrderTerm",
        description = "Create an Order Term",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderTerm", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "OrderTerm", mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "orderItemSeqId", optional = "true")
        }
    )
    public interface CreateOrderTerm {}

    /**
     * Update an Order Term
     */
    @Service(
        name = "updateOrderTerm",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "updateOrderTerm",
        description = "Update an Order Term",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderTerm", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "OrderTerm", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderTerm {}

    /**
     * Remove an Order Term
     */
    @Service(
        name = "removeOrderTerm",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "removeOrderTerm",
        description = "Remove an Order Term",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderTerm", mode = "IN", include = "pk")
        }
    )
    public interface RemoveOrderTerm {}

    /**
     * If the order is a sales order, create purchase orders (drop shipments) for each ship group associated to a supplier
     */
    @Service(
        name = "checkCreateDropShipPurchaseOrders",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "checkCreateDropShipPurchaseOrders",
        description = "If the order is a sales order, create purchase orders (drop shipments) for each ship group associated to a supplier",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface CheckCreateDropShipPurchaseOrders {}

    /**
     * Add Payment Method to Order.From this servicewe will call the createOrderPaymentPreference service to create OrderPaymentPreference
     */
    @Service(
        name = "addPaymentMethodToOrder",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "addPaymentMethodToOrder",
        description = "Add Payment Method to Order.From this servicewe will call the createOrderPaymentPreference service to create OrderPaymentPreference",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN"),
            @Attribute(name = "maxAmount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "orderPaymentPreferenceId", type = "String", mode = "OUT")
        }
    )
    public interface AddPaymentMethodToOrder {}

    /**
     * Completes a purchase order by cancelling remaining (unreceived) item quantities and generating new product requirements             from those quantities
     */
    @Service(
        name = "completePurchaseOrder",
        engine = "group",
        description = "Completes a purchase order by cancelling remaining (unreceived) item quantities and generating new product requirements\n            from those quantities",
        auth = "true",
        invokes = {@GroupInvoke(name = "cancelRemainingPurchaseOrderItems", resultToContext = "true"), @GroupInvoke(name = "generateReqsFromCancelledPOItems", resultToContext = "true"), @GroupInvoke(name = "checkOrderItemStatus", resultToContext = "true")}
    )
    public interface CompletePurchaseOrder {}

    /**
     * Cancels remaining (unreceived) quantities for items of an order. Does not consider received-but-rejected quantities.
     */
    @Service(
        name = "cancelRemainingPurchaseOrderItems",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "cancelRemainingPurchaseOrderItems",
        description = "Cancels remaining (unreceived) quantities for items of an order. Does not consider received-but-rejected quantities.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface CancelRemainingPurchaseOrderItems {}

    /**
     * Generates a product requirement for the total cancelled quantity over all order items for each product
     */
    @Service(
        name = "generateReqsFromCancelledPOItems",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "generateReqsFromCancelledPOItems",
        description = "Generates a product requirement for the total cancelled quantity over all order items for each product",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "facilityId", type = "String", mode = "IN")
        }
    )
    public interface GenerateReqsFromCancelledPOItems {}

    /**
     * Determines the total amount invoiced for a given order item over all invoices by totalling the item             subtotal (via OrderItemBilling), any adjustments for that item (via OrderAdjustmentBilling), and the item's             share of any order-level adjustments (that calculated by applying the percentage of the items total that the item represents             to the order-level adjustments total (also via OrderAdjustmentBilling). Also returns the quantity invoiced for the item over             all invoices, to aid in prorating.
     */
    @Service(
        name = "getOrderItemInvoicedAmountAndQuantity",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "getOrderItemInvoicedAmountAndQuantity",
        description = "Determines the total amount invoiced for a given order item over all invoices by totalling the item\n            subtotal (via OrderItemBilling), any adjustments for that item (via OrderAdjustmentBilling), and the item's\n            share of any order-level adjustments (that calculated by applying the percentage of the items total that the item represents\n            to the order-level adjustments total (also via OrderAdjustmentBilling). Also returns the quantity invoiced for the item over\n            all invoices, to aid in prorating.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "invoicedAmount", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "invoicedQuantity", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface GetOrderItemInvoicedAmountAndQuantity {}

    /**
     * Common order search fields
     */
    @Service(
        name = "findOrdersCommon",
        engine = "interface",
        description = "Common order search fields",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderTypeId", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderStatusId", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "orderWebSiteId", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "salesChannelEnumId", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "createdBy", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "terminalId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "transactionId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "externalId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "internalCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useEntryDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "minDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "maxDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "hasBackOrders", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLoginId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "roleTypeId", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "correspondingPoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subscriptionId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "budgetId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "quoteId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "goodIdentificationTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "goodIdentificationIdValue", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billingAccountId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "finAccountId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "accountNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentStatusId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "inventoryItemId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "softIdentifier", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "serialNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterInventoryProblems", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterPOsWithRejectedItems", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterPOsOpenPastTheirETA", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterPartiallyReceivedPOs", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "isViewed", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentMethod", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "gatewayAvsResult", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "gatewayScoreResult", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "countryGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "includeCountry", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "filterInventoryProblemsList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "filterPOsWithRejectedItemsList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "filterPOsOpenPastTheirETAList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "filterPartiallyReceivedPOsList", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "paramList", type = "String", mode = "OUT"),
            @Attribute(name = "orderList", type = "List", mode = "OUT"),
            @Attribute(name = "orderListSize", type = "Integer", mode = "OUT")
        }
    )
    public interface FindOrdersCommon {}

    /**
     * Uses dynamic view entity to find orders; returns a list of Order (OrderHeader) objects
     */
    @Service(
        name = "findOrders",
        engine = "java",
        location = "org.ofbiz.order.order.OrderLookupServices",
        invoke = "findOrders",
        description = "Uses dynamic view entity to find orders; returns a list of Order (OrderHeader) objects",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "findOrdersCommon")},
        attributes = {
            @Attribute(name = "viewIndex", type = "Integer", mode = "INOUT"),
            @Attribute(name = "viewSize", type = "Integer", mode = "INOUT"),
            @Attribute(name = "showAll", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "highIndex", type = "Integer", mode = "OUT"),
            @Attribute(name = "lowIndex", type = "Integer", mode = "OUT")
        }
    )
    public interface FindOrders {}

    /**
     * Uses dynamic view entity to find orders; returns a list of Order (OrderHeader) objects.             SCIPIO: Version of findOrders with extra functions for internal calls (not meant for receiving request parameters directly).
     */
    @Service(
        name = "findOrdersInternal",
        engine = "java",
        location = "org.ofbiz.order.order.OrderLookupServices",
        invoke = "findOrdersInternal",
        description = "Uses dynamic view entity to find orders; returns a list of Order (OrderHeader) objects.\n            SCIPIO: Version of findOrders with extra functions for internal calls (not meant for receiving request parameters directly).",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "findOrders")},
        attributes = {
            @Attribute(name = "errorAsFailure", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface FindOrdersInternal {}

    /**
     * Uses dynamic view entity to find orders; returns a full list of Order (OrderHeader) objects (no limitation set by viewIndex/viewSize).             SCIPIO: Version of findOrders that does not limit to view size and returns all.
     */
    @Service(
        name = "findOrdersFull",
        engine = "java",
        location = "org.ofbiz.order.order.OrderLookupServices",
        invoke = "findOrdersFull",
        description = "Uses dynamic view entity to find orders; returns a full list of Order (OrderHeader) objects (no limitation set by viewIndex/viewSize).\n            SCIPIO: Version of findOrders that does not limit to view size and returns all.",
        auth = "true",
        transactionTimeout = "300",
        implemented = {@Implements(service = "findOrdersCommon")},
        attributes = {
            @Attribute(name = "errorAsFailure", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "showAll", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface FindOrdersFull {}

    /**
     * Check if an Order is on Back Order
     */
    @Service(
        name = "checkOrderIsOnBackOrder",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "checkOrderIsOnBackOrder",
        description = "Check if an Order is on Back Order",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "isBackOrder", type = "Boolean", mode = "OUT")
        }
    )
    public interface CheckOrderIsOnBackOrder {}

    /**
     * Bulk create test sales orders. Note that default-values depend on demo data in [Scipio: shop]/data.
     */
    @Service(
        name = "createTestSalesOrders",
        engine = "java",
        location = "org.ofbiz.order.test.OrderTestServices",
        invoke = "createTestSalesOrders",
        description = "Bulk create test sales orders. Note that default-values depend on demo data in [Scipio: shop]/data.",
        auth = "true",
        transactionTimeout = "300",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN", optional = "true", defaultValue = "100"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true", defaultValue = "ScipioShop"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true", defaultValue = "USD"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true", defaultValue = "DemoCustomer"),
            @Attribute(name = "numberOfOrders", type = "Integer", mode = "IN", optional = "true", defaultValue = "10"),
            @Attribute(name = "shipOrder", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "numberOfProductsPerOrder", type = "Integer", mode = "IN", optional = "true", defaultValue = "5"),
            @Attribute(name = "salesChannel", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateTestSalesOrders {}

    /**
     * Bulk create test sales orders. Note that default-values depend on demo data in [Scipio: shop]/data.
     */
    @Service(
        name = "createTestSalesOrderSingle",
        engine = "java",
        location = "org.ofbiz.order.test.OrderTestServices",
        invoke = "createTestSalesOrderSingle",
        description = "Bulk create test sales orders. Note that default-values depend on demo data in [Scipio: shop]/data.",
        auth = "true",
        transactionTimeout = "300",
        attributes = {
            @Attribute(name = "productCategoryId", type = "String", mode = "IN", optional = "true", defaultValue = "100"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true", defaultValue = "ScipioShop"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true", defaultValue = "USD"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true", defaultValue = "DemoCustomer"),
            @Attribute(name = "numberOfProductsPerOrder", type = "Integer", mode = "IN", optional = "true", defaultValue = "5"),
            @Attribute(name = "salesChannel", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipOrder", type = "Boolean", mode = "IN", optional = "true", defaultValue = "false"),
            @Attribute(name = "orderId", type = "String", mode = "OUT")
        }
    )
    public interface CreateTestSalesOrderSingle {}

    /**
     * Change the payment status of an existing order
     */
    @Service(
        name = "changeOrderPaymentStatus",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "setOrderPaymentStatus",
        description = "Change the payment status of an existing order",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreferenceId", type = "String", mode = "IN"),
            @Attribute(name = "changeReason", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ChangeOrderPaymentStatus {}

    /**
     * Creates a new OrderItemChange record
     */
    @Service(
        name = "createOrderItemChange",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "createOrderItemChange",
        description = "Creates a new OrderItemChange record",
        defaultEntityName = "OrderItemChange",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "orderId", optional = "false"),
            @OverrideAttribute(name = "orderItemSeqId", optional = "false"),
            @OverrideAttribute(name = "changeTypeEnumId", optional = "false")
        }
    )
    public interface CreateOrderItemChange {}

    /**
     * A service designed to be automatically run by job scheduler to create orders from subscriptions which need to be extended.             This is done by looking for all subscriptions which are active and where the automaticExtend flag is set to "Y"
     */
    @Service(
        name = "runSubscriptionAutoReorders",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "runSubscriptionAutoReorders",
        description = "A service designed to be automatically run by job scheduler to create orders from subscriptions which need to be extended.\n            This is done by looking for all subscriptions which are active and where the automaticExtend flag is set to \"Y\"",
        auth = "true",
        useTransaction = "false"
    )
    public interface RunSubscriptionAutoReorders {}

    /**
     * Creates new shipping address and update existing address
     */
    @Service(
        name = "createUpdateShippingAddress",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "createUpdateShippingAddress",
        description = "Creates new shipping address and update existing address",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "setDefaultShipping", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "keepAddressBook", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "shipToAttnName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipToName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipToAddress1", type = "String", mode = "IN"),
            @Attribute(name = "shipToAddress2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipToCity", type = "String", mode = "IN"),
            @Attribute(name = "shipToStateProvinceGeoId", type = "String", mode = "IN"),
            @Attribute(name = "shipToPostalCode", type = "String", mode = "IN"),
            @Attribute(name = "shipToCountryGeoId", type = "String", mode = "IN"),
            @Attribute(name = "shipToContactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT"),
            @Attribute(name = "billToContactMechId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateUpdateShippingAddress {}

    /**
     * Creates new billing address and update existing address
     */
    @Service(
        name = "createUpdateBillingAddress",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "createUpdateBillingAddress",
        description = "Creates new billing address and update existing address",
        auth = "true",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "setDefaultBilling", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "keepAddressBook", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "useShippingAddressForBilling", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "userLogin", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "billToAttnName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToAddress1", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToAddress2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToCity", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToStateProvinceGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToPostalCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToCountryGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipToContactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billToContactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "OUT")
        }
    )
    public interface CreateUpdateBillingAddress {}

    /**
     * Create/Update credit card
     */
    @Service(
        name = "createUpdateCreditCard",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "createUpdateCreditCard",
        description = "Create/Update credit card",
        defaultEntityName = "CreditCard",
        auth = "true",
        attributes = {
            @Attribute(name = "expMonth", type = "String", mode = "IN"),
            @Attribute(name = "expYear", type = "String", mode = "IN"),
            @Attribute(name = "cardType", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "companyNameOnCard", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "titleOnCard", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "firstNameOnCard", type = "String", mode = "IN"),
            @Attribute(name = "middleNameOnCard", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lastNameOnCard", type = "String", mode = "IN"),
            @Attribute(name = "suffixOnCard", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "OUT")
        }
    )
    public interface CreateUpdateCreditCard {}

    /**
     * Sets unit price as last price for product
     */
    @Service(
        name = "setUnitPriceAsLastPrice",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "setUnitPriceAsLastPrice",
        description = "Sets unit price as last price for product",
        attributes = {
            @Attribute(name = "supplierPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderItems", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "facilityId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "unitCost", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "itemPriceMap", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "overridePriceMap", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "orderCurrencyUnitPrice", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetUnitPriceAsLastPrice {}

    /**
     * Cancels those back orders from suppliers whose cancel back order date (cancelBackOrderDate) has passed the current date
     */
    @Service(
        name = "cancelAllBackOrders",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "cancelAllBackOrders",
        description = "Cancels those back orders from suppliers whose cancel back order date (cancelBackOrderDate) has passed the current date",
        auth = "true"
    )
    public interface CancelAllBackOrders {}

    /**
     * Compare order's shipping amount and new shipping amount(based on weight and dimension of packages).If new shipping amount is more then or less than default percentage (defined in shipment.properties) of Order's shipping amount, then shipping method and shipping charges are updated. And if new shipping amount is not more then or less than default percentage (defined in shipment.properties)% of Order's shipping amount then only shipping method is updated.Also updates record in ShipmentRouteSegment entity
     */
    @Service(
        name = "updateShippingMethodAndCharges",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "updateShippingMethodAndCharges",
        description = "Compare order's shipping amount and new shipping amount(based on weight and dimension of packages).If new shipping amount is more then or less than default percentage (defined in shipment.properties) of Order's shipping amount, then shipping method and shipping charges are updated. And if new shipping amount is not more then or less than default percentage (defined in shipment.properties)% of Order's shipping amount then only shipping method is updated.Also updates record in ShipmentRouteSegment entity",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "oldContactMechId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentMethodAndAmount", type = "String", mode = "IN"),
            @Attribute(name = "contactMechPurposeTypeId", type = "String", mode = "IN"),
            @Attribute(name = "orderAdjustmentId", type = "String", mode = "IN"),
            @Attribute(name = "shippingAmount", type = "String", mode = "IN"),
            @Attribute(name = "shipmentId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentRouteSegmentId", type = "String", mode = "IN")
        }
    )
    public interface UpdateShippingMethodAndCharges {}

    /**
     * Set the shipping instructions for an order
     */
    @Service(
        name = "setShippingInstructions",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "setShippingInstructions",
        description = "Set the shipping instructions for an order",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "shippingInstructions", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetShippingInstructions {}

    /**
     * Set Gift message for an order
     */
    @Service(
        name = "setGiftMessage",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "setGiftMessage",
        description = "Set Gift message for an order",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN"),
            @Attribute(name = "giftMessage", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SetGiftMessage {}

    /**
     *              Cycles through all newly created sales orders and creates ProductAssoc records (of type ALSO_BOUGHT) for products              that were purchased together.  If a ProductAssoc record already exists then the quantity field is incremented by one.             Newly created orders are determined by looking for orders that were created after the JobSandbox.startDateTime of the             previous async execution of this service, alternatively the service can be supplied with a orderEntryFromDateTime              parameter which will process all orders placed after that date/time or as a final option processAllOrders can be set             to true to force a calculation of all orders ever placed with orderEntryFromDateTime being ignored.             SCIPIO: 2020-03-10: This service is now under a fail semaphore and will fail if already running;             this service is by default and usually run as a job every 24h (see OrderScheduledServices.xml)             and if an execution tries to run during another, it will fail but should simply pick up at the next execution some time later.         
     */
    @Service(
        name = "createAlsoBoughtProductAssocs",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "createAlsoBoughtProductAssocs",
        description = "\n            Cycles through all newly created sales orders and creates ProductAssoc records (of type ALSO_BOUGHT) for products \n            that were purchased together.  If a ProductAssoc record already exists then the quantity field is incremented by one.\n            Newly created orders are determined by looking for orders that were created after the JobSandbox.startDateTime of the\n            previous async execution of this service, alternatively the service can be supplied with a orderEntryFromDateTime \n            parameter which will process all orders placed after that date/time or as a final option processAllOrders can be set\n            to true to force a calculation of all orders ever placed with orderEntryFromDateTime being ignored.\n            SCIPIO: 2020-03-10: This service is now under a fail semaphore and will fail if already running;\n            this service is by default and usually run as a job every 24h (see OrderScheduledServices.xml)\n            and if an execution tries to run during another, it will fail but should simply pick up at the next execution some time later.\n        ",
        auth = "true",
        semaphore = "fail",
        log = "quiet",
        attributes = {
            @Attribute(name = "orderEntryFromDateTime", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "processAllOrders", type = "Boolean", mode = "IN", optional = "true")
        }
    )
    public interface CreateAlsoBoughtProductAssocs {}

    /**
     *              Cycles through all newly created sales orders and creates ProductAssoc records (of type ALSO_BOUGHT) for products             that were purchased together.  If a ProductAssoc record already exists then the quantity field is incremented by one.             Newly created orders are determined by looking for orders that were created after the JobSandbox.startDateTime of the             previous async execution of this service, alternatively the service can be supplied with a orderEntryFromDateTime             parameter which will process all orders placed after that date/time or as a final option processAllOrders can be set             to true to force a calculation of all orders ever placed with orderEntryFromDateTime being ignored.             SCIPIO: 2020-03-10: This is simply the original implementation of createAlsoBoughtProductAssocs without semaphore,             in case its execution must be forced for some reason.         
     */
    @Service(
        name = "createAlsoBoughtProductAssocsAlways",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "createAlsoBoughtProductAssocs",
        description = "\n            Cycles through all newly created sales orders and creates ProductAssoc records (of type ALSO_BOUGHT) for products\n            that were purchased together.  If a ProductAssoc record already exists then the quantity field is incremented by one.\n            Newly created orders are determined by looking for orders that were created after the JobSandbox.startDateTime of the\n            previous async execution of this service, alternatively the service can be supplied with a orderEntryFromDateTime\n            parameter which will process all orders placed after that date/time or as a final option processAllOrders can be set\n            to true to force a calculation of all orders ever placed with orderEntryFromDateTime being ignored.\n            SCIPIO: 2020-03-10: This is simply the original implementation of createAlsoBoughtProductAssocs without semaphore,\n            in case its execution must be forced for some reason.\n        ",
        auth = "true",
        log = "quiet",
        implemented = {@Implements(service = "createAlsoBoughtProductAssocs")}
    )
    public interface CreateAlsoBoughtProductAssocsAlways {}

    /**
     *              Creates ProductAssoc records (of type ALSO_BOUGHT) for products that were purchased together in the Order.  If a ProductAssoc record already exists then the quantity field is incremented by one.  If a variant product has             been ordered then the association is made to its parent product.         
     */
    @Service(
        name = "createAlsoBoughtProductAssocsForOrder",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "createAlsoBoughtProductAssocsForOrder",
        description = "\n            Creates ProductAssoc records (of type ALSO_BOUGHT) for products that were purchased together in the Order.  If a ProductAssoc record already exists then the quantity field is incremented by one.  If a variant product has\n            been ordered then the association is made to its parent product.\n        ",
        auth = "true",
        log = "quiet",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN")
        }
    )
    public interface CreateAlsoBoughtProductAssocsForOrder {}

    /**
     *              Calculate ATP and QOH According For each facility         
     */
    @Service(
        name = "productAvailabalityByFacility",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "productAvailabalityByFacility",
        description = "\n            Calculate ATP and QOH According For each facility\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "ownerPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "availabalityList", type = "List", mode = "OUT")
        }
    )
    public interface ProductAvailabalityByFacility {}

    /**
     * Creates a new OrderItemShipGroup with maySplit and isGift filled.
     */
    @Service(
        name = "addOrderItemShipGroup",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "addOrderItemShipGroup",
        description = "Creates a new OrderItemShipGroup with maySplit and isGift filled.",
        defaultEntityName = "OrderItemShipGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "shipGroupSeqId", mode = "INOUT", optional = "true")
        }
    )
    public interface AddOrderItemShipGroup {}

    /**
     * delete Order Item Ship Group 
     */
    @Service(
        name = "deleteOrderItemShipGroup",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "deleteOrderItemShipGroup",
        description = "delete Order Item Ship Group ",
        defaultEntityName = "OrderItemShipGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderItemShipGroup {}

    /**
     * add Order Item Ship Group Assoc and if order item ship group not exit, create it before
     */
    @Service(
        name = "addOrderItemShipGroupAssoc",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "addOrderItemShipGroupAssoc",
        description = "add Order Item Ship Group Assoc and if order item ship group not exit, create it before",
        defaultEntityName = "OrderItemShipGroupAssoc",
        auth = "true",
        implemented = {@Implements(service = "addOrderItemShipGroup", optional = "true")},
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface AddOrderItemShipGroupAssoc {}

    /**
     * update OrderItem from OISG, totalQuantity is used only if controller is a multi services 
     */
    @Service(
        name = "updateOrderItemShipGroupAssoc",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "updateOrderItemShipGroupAssoc",
        description = "update OrderItem from OISG, totalQuantity is used only if controller is a multi services ",
        defaultEntityName = "OrderItemShipGroupAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "totalQuantity", type = "BigDecimal", mode = "INOUT", optional = "true"),
            @Attribute(name = "rowCount", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "rowNumber", type = "Integer", mode = "INOUT", optional = "true")
        }
    )
    public interface UpdateOrderItemShipGroupAssoc {}

    /**
     * delete Order Item Ship Group Assoc
     */
    @Service(
        name = "deleteOrderItemShipGroupAssoc",
        engine = "entity-auto",
        invoke = "delete",
        description = "delete Order Item Ship Group Assoc",
        defaultEntityName = "OrderItemShipGroupAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderItemShipGroupAssoc {}

    /**
     *              Create a new Quate term.         
     */
    @Service(
        name = "createQuoteTerm",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "createQuoteTerm",
        description = "\n            Create a new Quate term.\n        ",
        defaultEntityName = "QuoteTerm",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "quoteItemSeqId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateQuoteTerm {}

    /**
     *              Edit the Quate term.         
     */
    @Service(
        name = "updateQuoteTerm",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "updateQuoteTerm",
        description = "\n            Edit the Quate term.\n        ",
        defaultEntityName = "QuoteTerm",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateQuoteTerm {}

    /**
     *              delete the Quate term.         
     */
    @Service(
        name = "deleteQuoteTerm",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/quote/QuoteServices.xml",
        invoke = "deleteQuoteTerm",
        description = "\n            delete the Quate term.\n        ",
        defaultEntityName = "QuoteTerm",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteQuoteTerm {}

    /**
     * Create Order Payment Application
     */
    @Service(
        name = "createOrderPaymentApplication",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "createOrderPaymentApplication",
        description = "Create Order Payment Application",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentId", type = "String", mode = "IN")
        }
    )
    public interface CreateOrderPaymentApplication {}

    /**
     * Create Test Order Rental of an asset which is shipped from and returned to inventory
     */
    @Service(
        name = "createTestOrderRentalProduct",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/test/ShoppingCartTests.xml",
        invoke = "testCreateOrderRentalProduct",
        description = "Create Test Order Rental of an asset which is shipped from and returned to inventory",
        auth = "true"
    )
    public interface CreateTestOrderRentalProduct {}

    /**
     * Create an order using a shopping cart - only used internally in ShoppingCartTests.xml for test purpose
     */
    @Service(
        name = "testCreateShoppinCartAndOrder",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/test/ShoppingCartTests.xml",
        invoke = "testCreateShoppinCartAndOrder",
        description = "Create an order using a shopping cart - only used internally in ShoppingCartTests.xml for test purpose",
        auth = "true",
        attributes = {
            @Attribute(name = "orderMap", type = "Map", mode = "OUT")
        }
    )
    public interface TestCreateShoppinCartAndOrder {}

    /**
     * Create Order Item Attribute
     */
    @Service(
        name = "createOrderItemAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Order Item Attribute",
        defaultEntityName = "OrderItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderItemAttribute", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "OrderItemAttribute", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderItemAttribute {}

    /**
     * Update Order Item Attribute
     */
    @Service(
        name = "updateOrderItemAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Order Item Attribute",
        defaultEntityName = "OrderItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderItemAttribute", mode = "IN", include = "pk"),
            @EntityAttributes(entityName = "OrderItemAttribute", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderItemAttribute {}

    /**
     * Delete Order Item Attribute
     */
    @Service(
        name = "deleteOrderItemAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Order Item Attribute",
        defaultEntityName = "OrderItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderItemAttribute", mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderItemAttribute {}

    /**
     * Create Order Item Group Order
     */
    @Service(
        name = "createOrderItemGroupOrder",
        engine = "entity-auto",
        invoke = "create",
        description = "Create Order Item Group Order",
        defaultEntityName = "OrderItemGroupOrder",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "OrderItemGroupOrder", mode = "IN", include = "pk")
        }
    )
    public interface CreateOrderItemGroupOrder {}

    /**
     * count Product Quantity Ordered
     */
    @Service(
        name = "countProductQuantityOrdered",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "countProductQuantityOrdered",
        description = "count Product Quantity Ordered",
        auth = "true",
        attributes = {
            @Attribute(name = "productId", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN")
        }
    )
    public interface CountProductQuantityOrdered {}

    /**
     * Move order items between ship groups
     */
    @Service(
        name = "MoveItemBetweenShipGroups",
        engine = "simple",
        location = "component://order/script/org/ofbiz/order/order/OrderServices.xml",
        invoke = "MoveItemBetweenShipGroups",
        description = "Move order items between ship groups",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderItemSeqId", type = "String", mode = "IN"),
            @Attribute(name = "fromGroupIndex", type = "String", mode = "IN"),
            @Attribute(name = "toGroupIndex", type = "String", mode = "IN"),
            @Attribute(name = "quantity", type = "BigDecimal", mode = "IN")
        }
    )
    public interface MoveItemBetweenShipGroups {}

    /**
     * Update Shipping Information on Order View
     */
    @Service(
        name = "updateShipGroupShipInfo",
        engine = "java",
        location = "org.ofbiz.order.order.OrderServices",
        invoke = "updateShipGroupShipInfo",
        description = "Update Shipping Information on Order View",
        auth = "true",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "oldContactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipGroupSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentMethod", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateShipGroupShipInfo {}

    /**
     * Create a OrderContent
     */
    @Service(
        name = "createOrderContent",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a OrderContent",
        defaultEntityName = "OrderContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateOrderContent {}

    /**
     * Expire a OrderContent
     */
    @Service(
        name = "expireOrderContent",
        engine = "entity-auto",
        invoke = "expire",
        description = "Expire a OrderContent",
        defaultEntityName = "OrderContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface ExpireOrderContent {}

    /**
     * Create a OrderAdjustmentTypeAttr
     */
    @Service(
        name = "createOrderAdjustmentTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a OrderAdjustmentTypeAttr",
        defaultEntityName = "OrderAdjustmentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateOrderAdjustmentTypeAttr {}

    /**
     * Update a OrderAdjustmentTypeAttr
     */
    @Service(
        name = "updateOrderAdjustmentTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a OrderAdjustmentTypeAttr",
        defaultEntityName = "OrderAdjustmentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateOrderAdjustmentTypeAttr {}

    /**
     * Delete a OrderAdjustmentTypeAttr
     */
    @Service(
        name = "deleteOrderAdjustmentTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a OrderAdjustmentTypeAttr",
        defaultEntityName = "OrderAdjustmentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteOrderAdjustmentTypeAttr {}

    /**
     * Create a ProductOrderItem
     */
    @Service(
        name = "createProductOrderItem",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a ProductOrderItem",
        defaultEntityName = "ProductOrderItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateProductOrderItem {}

    /**
     * Update a ProductOrderItem
     */
    @Service(
        name = "updateProductOrderItem",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a ProductOrderItem",
        defaultEntityName = "ProductOrderItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateProductOrderItem {}

    /**
     * Delete a ProductOrderItem
     */
    @Service(
        name = "deleteProductOrderItem",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ProductOrderItem",
        defaultEntityName = "ProductOrderItem",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteProductOrderItem {}

}

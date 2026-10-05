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
package com.ilscipio.scipio.order.mcp;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;
import java.util.LinkedHashMap;
import java.util.Map;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.order.shoppingcart.ShoppingCart;

import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpResource;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.protocol.JsonRpc;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.tool.DocumentTools;

/**
 * SCIPIO: 4.0.0: MCP server profile for the order component: sales/purchase orders, quotes and returns.
 */
@McpServer(name = "order", title = "Scipio Orders", component = "order",
        description = "Order management: sales/purchase orders, quotes, requirements and returns.",
        featuredServices = {"storeOrder", "createOrderAdjustment", "changeOrderStatus", "cancelOrderItem",
                "createOrderPaymentPreference", "createOrderNote", "createQuote", "createReturnHeader"},
        entities = {"OrderHeader", "OrderItem", "OrderStatus", "OrderAdjustment", "OrderRole", "OrderContactMech",
                "Quote", "QuoteItem", "ReturnHeader", "ReturnItem", "OrderItemShipGroup", "OrderNote"},
        serviceTools = {
            @McpServiceTool(service = "cancelOrderItem",
                    topic = "order",
                    name = "item_cancel",
                    description = "Cancel one order item.",
                    readOnly = false,
                    destructive = "true",
                    requiresConfirmation = true,
                    order = 90),
            @McpServiceTool(service = "createOrderAdjustment",
                    topic = "order",
                    name = "adjustment_add",
                    description = "Add an adjustment (discount, fee, tax) to an order.",
                    readOnly = false,
                    destructive = "false",
                    order = 91),
            @McpServiceTool(service = "changeOrderStatus", topic = "order", name = "set_status",
                    description = "Change the order status, e.g. ORDER_APPROVED, ORDER_CANCELLED, ORDER_COMPLETED.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 40),
            @McpServiceTool(service = "quickShipEntireOrder", topic = "order", name = "ship",
                    description = "Create, pack and ship a shipment for every item of an approved order.", readOnly = false, destructive = "true", requiresConfirmation = true, order = 50),
            @McpServiceTool(service = "createInvoiceForOrderAllItems", topic = "order", name = "invoice",
                    description = "Create a sales invoice for every item of an order.", readOnly = false, requiresConfirmation = true, order = 60),
            @McpServiceTool(service = "createOrderNote", topic = "order", name = "note_add",
                    description = "Attach a note to an order.", readOnly = false, order = 70),
            @McpServiceTool(service = "sendOrderMessage", topic = "order", name = "message_send",
                    description = "Email the customer of an order; stored as a communication event linked to the order. Marketplace orders: channel_message_not_supported.",
                    readOnly = false, destructive = "false", requiresConfirmation = true, order = 78),
            @McpServiceTool(service = "appendOrderItem", topic = "order", name = "item_add",
                    description = "Append an item to an approved order.",
                    readOnly = false, destructive = "false", order = 41),
            @McpServiceTool(service = "addPaymentMethodToOrder", topic = "order", name = "payment_add",
                    description = "Attach a stored payment method to an order with a max amount.",
                    readOnly = false, destructive = "false", order = 42),
            @McpServiceTool(service = "updateShipGroupShipInfo", topic = "order", name = "ship_group_update",
                    description = "Update the shipping address and method of an order ship group.",
                    readOnly = false, destructive = "false", order = 43),
            @McpServiceTool(service = "updateShippingMethodAndCharges", topic = "order", name = "shipping_method_set",
                    description = "Update the shipping method and recompute charges for a ship group.",
                    readOnly = false, destructive = "false", order = 44),
            @McpServiceTool(service = "createRequirement", topic = "purchase_order", name = "requirement_create",
                    description = "Create a new purchasing requirement for a product and facility.",
                    readOnly = false, destructive = "false", order = 45),
            @McpServiceTool(service = "autoAssignRequirementToSupplier", topic = "purchase_order", name = "requirement_assign_supplier",
                    description = "Auto-assign a purchasing requirement to its primary supplier.",
                    readOnly = false, destructive = "false", order = 46),
            @McpServiceTool(service = "createCustRequest", topic = "quote", name = "request_create",
                    description = "Create a customer request.",
                    readOnly = false, destructive = "false", order = 47),
            @McpServiceTool(service = "createPicklistFromOrders", topic = "order", name = "pick_list_create",
                    description = "Create a pick list from approved orders at a facility.",
                    readOnly = false, destructive = "false", order = 48),
            @McpServiceTool(service = "changeOrderStatus", topic = "order", name = "approve",
                    description = "Approve an order: sets its status to ORDER_APPROVED.", readOnly = false, destructive = "true", requiresConfirmation = true,
                    fixed = {"statusId=ORDER_APPROVED", "setItemStatus=Y"}, order = 71),
            @McpServiceTool(service = "changeOrderItemStatus", topic = "order", name = "item_set_status",
                    description = "Change the status of one order item, or all items.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 72),
            @McpServiceTool(service = "approveRequirement", topic = "purchase_order", name = "requirement_approve",
                    description = "Approve a purchasing requirement.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 73),
            @McpServiceTool(service = "setCustRequestStatus", topic = "quote", name = "request_set_status",
                    description = "Change the status of a customer request.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 74),
            @McpServiceTool(service = "updateReturnHeader", topic = "order", name = "return_set_status",
                    description = "Change the status of a return, e.g. RETURN_ACCEPTED, RETURN_RECEIVED, RETURN_COMPLETED.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 76),
            @McpServiceTool(service = "updateQuote", topic = "quote", name = "set_status",
                    description = "Change the status of a quote, e.g. QUO_CREATED, QUO_APPROVED, QUO_ORDERED.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 77),
            @McpServiceTool(service = "captureOrderPayments", topic = "order", name = "payment_capture",
                    description = "Capture (settle) pre-authorized payments for an order.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 80),
            @McpServiceTool(service = "refundOrderPaymentPreference", topic = "order", name = "payment_refund",
                    description = "Refund one order payment preference.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 81),
            @McpServiceTool(service = "releaseOrderPaymentPreference", topic = "order", name = "payment_release",
                    description = "Release the payment authorization of one order payment preference.",
                    readOnly = false, destructive = "true", requiresConfirmation = true, order = 82),
            // SCIPIO: 4.0.0: card payment through the hub (W1-10d); the desk calls these, the store never calls Stripe
            @McpServiceTool(service = "recordHubPayment", topic = "order", name = "payment_hub_record",
                    description = "Record a card payment that the hub took on Stripe (EXT_STRIPE_HUB): succeeded approves the order, failed adds a note. Idempotent by externalPaymentId.",
                    readOnly = false, destructive = "false", requiresConfirmation = false, order = 83),
            @McpServiceTool(service = "getHubPaymentInfo", topic = "order", name = "payment_hub_info",
                    description = "Read the payment type, the Stripe PaymentIntent, the captured and the refunded amount of one order payment preference.",
                    readOnly = true, destructive = "false", requiresConfirmation = false, order = 84),
            @McpServiceTool(service = "recordHubPaymentRefund", topic = "order", name = "payment_hub_refund_record",
                    description = "Record a refund that the hub made on Stripe for an EXT_STRIPE_HUB payment. Idempotent by externalRefundId.",
                    readOnly = false, destructive = "false", requiresConfirmation = false, order = 85)
        },
        topics = {
            @McpTopic(name = "order", title = "Orders", order = 10, featured = true,
                    description = "Orders: find, create, approve, ship, invoice, pay, and return."),
            @McpTopic(name = "purchase_order", title = "Purchase orders", order = 20,
                    description = "Purchase orders: create, complete, send, and manage requirements."),
            @McpTopic(name = "quote", title = "Quotes", order = 30,
                    description = "Quotes and customer requests: create and change status.")
        })
public final class OrderMcp {

    private OrderMcp() {}

    @McpTool(topic = "order", name = "find", description = "Find orders by id, status, party, order type or order date range.", readOnly = true, order = 10)
    public static Object findOrders(McpCallContext ctx,
            @McpParam(name = "orderId", description = "Exact order id", required = false) String orderId,
            @McpParam(name = "statusId", description = "e.g. ORDER_APPROVED", required = false) String statusId,
            @McpParam(name = "partyId", description = "Party id on any order role", required = false) String partyId,
            @McpParam(name = "orderTypeId", description = "e.g. SALES_ORDER, PURCHASE_ORDER", required = false) String orderTypeId,
            @McpParam(name = "fromDate", description = "Only orders placed on/after this date", required = false) Timestamp fromDate,
            @McpParam(name = "thruDate", description = "Only orders placed on/before this date", required = false) Timestamp thruDate,
            @McpParam(name = "limit", description = "Max rows to return", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conditions = new ArrayList<>();
            if (orderId != null) conditions.add(EntityCondition.makeCondition("orderId", orderId));
            if (statusId != null) conditions.add(EntityCondition.makeCondition("statusId", statusId));
            if (orderTypeId != null) conditions.add(EntityCondition.makeCondition("orderTypeId", orderTypeId));
            if (fromDate != null) conditions.add(EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate));
            if (thruDate != null) conditions.add(EntityCondition.makeCondition("orderDate", EntityOperator.LESS_THAN_EQUAL_TO, thruDate));
            if (partyId != null) {
                List<String> orderIds = new ArrayList<>();
                for (GenericValue role : EntityQuery.use(delegator).from("OrderRole").where("partyId", partyId).queryList()) {
                    orderIds.add(role.getString("orderId"));
                }
                if (orderIds.isEmpty()) return new ArrayList<>();
                conditions.add(EntityCondition.makeCondition("orderId", EntityOperator.IN, orderIds));
            }
            List<GenericValue> orders = EntityQuery.use(delegator).from("OrderHeader").where(conditions)
                    .orderBy("-orderDate").maxRows(ctx.limit(limit)).queryList();
            List<Map<String, Object>> result = new ArrayList<>();
            for (GenericValue order : orders) {
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("orderId", order.getString("orderId"));
                row.put("orderTypeId", order.getString("orderTypeId"));
                row.put("statusId", order.getString("statusId"));
                row.put("orderDate", ResultConverter.toJson(order.getTimestamp("orderDate")));
                row.put("grandTotal", ResultConverter.toJson(order.getBigDecimal("grandTotal")));
                row.put("currencyUom", order.getString("currencyUom"));
                GenericValue billTo = EntityQuery.use(delegator).from("OrderRole")
                        .where("orderId", order.getString("orderId"), "roleTypeId", "BILL_TO_CUSTOMER").queryFirst();
                row.put("billToPartyId", billTo != null ? billTo.getString("partyId") : null);
                result.add(row);
            }
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Order search failed: " + e.getMessage());
        }
    }

    /** The bill-to customer of an order: party id, name, e-mail and the city and country of the shipping address; null without one. */
    private static Map<String, Object> customerOf(Delegator delegator, OrderReadHelper helper) throws GenericEntityException {
        GenericValue party = helper.getBillToParty();
        if (party == null) {
            party = helper.getPlacingParty();
        }
        if (party == null) {
            return null;
        }
        Map<String, Object> c = new LinkedHashMap<>();
        String partyId = party.getString("partyId");
        c.put("partyId", partyId);
        c.put("name", org.ofbiz.party.party.PartyHelper.getPartyName(delegator, partyId, false));
        c.put("email", helper.getOrderEmailString());
        List<GenericValue> addresses = helper.getShippingLocations();
        if (UtilValidate.isNotEmpty(addresses)) {
            GenericValue a = addresses.get(0);
            c.put("city", a.getString("city"));
            c.put("postalCode", a.getString("postalCode"));
            c.put("countryGeoId", a.getString("countryGeoId"));
        }
        return c;
    }

    @McpTool(topic = "order", name = "get", description = "Get full order detail: header, items, status history, roles, ship groups, payment preferences, shipments and the customer (name, e-mail, city).", readOnly = true, order = 20)
    public static Object getOrder(McpCallContext ctx,
            @McpParam(name = "orderId", description = "Order id", required = true) String orderId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue orderHeader = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
            if (orderHeader == null) {
                throw new McpToolException("Order not found: " + orderId);
            }
            OrderReadHelper helper = new OrderReadHelper(ctx.getDispatcher(), ctx.getLocale(), orderHeader);
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("header", ResultConverter.toJson(orderHeader));
            result.put("items", ResultConverter.toJson(helper.getOrderItems()));
            result.put("adjustments", ResultConverter.toJson(helper.getAdjustments()));
            result.put("statusHistory", ResultConverter.toJson(helper.getOrderStatuses()));
            result.put("shipGroups", ResultConverter.toJson(helper.getOrderItemShipGroups()));
            result.put("roles", ResultConverter.toJson(
                    EntityQuery.use(delegator).from("OrderRole").where("orderId", orderId).queryList()));
            result.put("contactMechs", ResultConverter.toJson(
                    EntityQuery.use(delegator).from("OrderContactMech").where("orderId", orderId).queryList()));
            result.put("notes", ResultConverter.toJson(
                    EntityQuery.use(delegator).from("OrderHeaderNoteView").where("orderId", orderId).queryList()));
            // SCIPIO: 4.0.0: what a screen of the order needs for its actions: the payment preferences (refund), the shipments
            // (label, pack) and the customer (name, e-mail, city)
            result.put("paymentPreferences", ResultConverter.toJson(EntityQuery.use(delegator).from("OrderPaymentPreference")
                    .where("orderId", orderId).orderBy("orderPaymentPreferenceId").queryList()));
            result.put("shipments", ResultConverter.toJson(EntityQuery.use(delegator).from("Shipment")
                    .where("primaryOrderId", orderId).orderBy("shipmentId").queryList()));
            result.put("customer", customerOf(delegator, helper));
            List<GenericValue> messages = new ArrayList<>();
            for (GenericValue link : EntityQuery.use(delegator).from("CommunicationEventOrder").where("orderId", orderId).queryList()) {
                GenericValue ev = EntityQuery.use(delegator).from("CommunicationEvent")
                        .where("communicationEventId", link.getString("communicationEventId")).queryOne();
                if (ev != null) {
                    messages.add(ev);
                }
            }
            messages.sort(java.util.Comparator.comparing((GenericValue ev) -> String.valueOf(ev.get("entryDate"))));
            result.put("messages", ResultConverter.toJson(messages));
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load order " + orderId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "order", name = "create", description = "Create a sales order for a customer party in a product store.", readOnly = false, destructive = "false", requiresConfirmation = true, order = 30)
    @SuppressWarnings("unchecked")
    public static Object createOrder(McpCallContext ctx,
            @McpParam(name = "partyId", description = "Customer party id (bill-to and ship-to)", required = true) String partyId,
            @McpParam(name = "productStoreId", description = "Product store id, e.g. ScipioShop", required = true) String productStoreId,
            @McpParam(name = "items", description = "Array of {productId, quantity}", required = true, type = "array") List<Object> items,
            @McpParam(name = "currencyUomId", description = "Currency, default: the store currency", required = false) String currencyUomId,
            @McpParam(name = "webSiteId", description = "Web site id, default: the store's first web site", required = false) String webSiteId,
            @McpParam(name = "shippingContactMechId", description = "Postal address contact mech id; default: the party's SHIPPING_LOCATION", required = false) String shippingContactMechId,
            @McpParam(name = "shipmentMethodTypeId", description = "e.g. STANDARD, NEXT_DAY; default: the store's first method", required = false) String shipmentMethodTypeId,
            @McpParam(name = "carrierPartyId", description = "Carrier party id, e.g. _NA_, UPS", required = false) String carrierPartyId,
            @McpParam(name = "paymentMethodTypeId", description = "Payment method type, default EXT_OFFLINE", required = false) String paymentMethodTypeId,
            @McpParam(name = "paymentMethodId", description = "Stored payment method id of the customer; overrides paymentMethodTypeId", required = false) String paymentMethodId) throws McpToolException {
        List<Map<String, Object>> rows = new ArrayList<>();
        if (items != null) {
            for (Object o : items) {
                if (!(o instanceof Map)) throw new McpToolException("Each item must be an object {productId, quantity}");
                rows.add((Map<String, Object>) o);
            }
        }
        ShoppingCart cart = OrderAgentHelper.newSalesCart(ctx, productStoreId, webSiteId, currencyUomId);
        OrderAgentHelper.addItems(ctx, cart, rows);
        return OrderAgentHelper.placeOrder(ctx, cart, partyId, shippingContactMechId, shipmentMethodTypeId, carrierPartyId,
                paymentMethodTypeId, paymentMethodId);
    }

    @McpTool(topic = "purchase_order", name = "create", description = "Create a draft purchase order for a supplier from a list of items.", readOnly = false, destructive = "false", requiresConfirmation = false, order = 36)
    @SuppressWarnings("unchecked")
    public static Object createPurchaseOrder(McpCallContext ctx,
            @McpParam(name = "supplierPartyId", description = "Supplier party id (bill-from vendor)", required = true) String supplierPartyId,
            @McpParam(name = "facilityId", description = "Facility to receive the goods, optional", required = false) String facilityId,
            @McpParam(name = "items", description = "Array of {productId, quantity, price, supplierProductId}", required = true, type = "array") List<Object> items,
            @McpParam(name = "currencyUomId", description = "Currency, default: the store currency", required = false) String currencyUomId,
            @McpParam(name = "productStoreId", description = "Product store id; default: the caller's store", required = false) String productStoreId,
            @McpParam(name = "orderName", description = "Optional order name", required = false) String orderName,
            @McpParam(name = "shipByDate", description = "Requested ship-by date for the items", required = false) Timestamp shipByDate) throws McpToolException {
        if (UtilValidate.isEmpty(productStoreId)) productStoreId = ctx.getProductStoreId();
        if (UtilValidate.isEmpty(productStoreId)) {
            try {
                GenericValue firstStore = EntityQuery.use(ctx.getDelegator()).from("ProductStore").orderBy("productStoreId").queryFirst();
                if (firstStore == null) throw new McpToolException("No product store is configured");
                productStoreId = firstStore.getString("productStoreId");
            } catch (GenericEntityException e) {
                throw new McpToolException("Could not find a product store: " + e.getMessage());
            }
        }
        List<Map<String, Object>> rows = new ArrayList<>();
        if (items != null) {
            for (Object o : items) {
                if (!(o instanceof Map)) throw new McpToolException("Each item must be an object {productId, quantity}");
                rows.add((Map<String, Object>) o);
            }
        }
        ShoppingCart cart = OrderAgentHelper.newPurchaseCart(ctx, productStoreId, currencyUomId, supplierPartyId, facilityId);
        if (orderName != null) cart.setOrderName(orderName);
        if (shipByDate != null) cart.setDefaultShipBeforeDate(shipByDate);
        OrderAgentHelper.addPurchaseItems(ctx, cart, rows);
        return OrderAgentHelper.placePurchaseOrder(ctx, cart, supplierPartyId);
    }

    @McpTool(topic = "purchase_order", name = "complete", description = "Complete a purchase order and cancel remaining unreceived items.",
            readOnly = false, destructive = "true", requiresConfirmation = true, order = 75)
    public static Object completePurchaseOrder(McpCallContext ctx,
            @McpParam(name = "orderId", description = "Purchase order id", required = true) String orderId) throws McpToolException {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("orderId", orderId);
        return ResultConverter.toJsonMap(ctx.runService("completePurchaseOrder", params));
    }

    @McpTool(topic = "purchase_order", name = "send", description = "Email a purchase order as a PDF to its supplier.",
            readOnly = false, destructive = "false", requiresConfirmation = true, permission = "MCP_MAIL_SEND", order = 90)
    public static Object sendPurchaseOrder(McpCallContext ctx,
            @McpParam(name = "orderId", description = "Purchase order id", required = true) String orderId,
            @McpParam(name = "sendTo", description = "Recipient email address; default: the supplier's primary email", required = false) String sendTo,
            @McpParam(name = "text", description = "Body text; default: a standard purchase order message", required = false) String text) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue orderHeader = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
            if (orderHeader == null) throw new McpToolException("Order not found: " + orderId);
            if (!"PURCHASE_ORDER".equals(orderHeader.getString("orderTypeId"))) {
                throw new McpToolException("Order " + orderId + " is not a purchase order");
            }
            GenericValue supplierRole = EntityQuery.use(delegator).from("OrderRole")
                    .where("orderId", orderId, "roleTypeId", "BILL_FROM_VENDOR").queryFirst();
            if (supplierRole == null) throw new McpToolException("Order " + orderId + " has no supplier (BILL_FROM_VENDOR) role");
            Map<String, Object> args = new LinkedHashMap<>();
            args.put("templateId", "MCP_PURCHASE_ORDER");
            args.put("partyIdTo", supplierRole.getString("partyId"));
            if (sendTo != null) args.put("sendTo", sendTo);
            Map<String, Object> bodyParameters = new LinkedHashMap<>();
            bodyParameters.put("orderId", orderId);
            args.put("bodyParameters", bodyParameters);
            args.put("attachmentType", "order");
            args.put("attachmentId", orderId);
            args.put("text", text != null ? text : "Please find attached purchase order " + orderId + ".");
            return ResultConverter.toJsonMap(DocumentTools.send(ctx, args));
        } catch (GenericEntityException e) {
            throw new McpToolException("Could not send purchase order " + orderId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "order", name = "return_create", description = "Create a customer return for an order.", readOnly = false, destructive = "false", requiresConfirmation = false, order = 49)
    @SuppressWarnings("unchecked")
    public static Object createReturn(McpCallContext ctx,
            @McpParam(name = "orderId", description = "Order id being returned", required = true) String orderId,
            @McpParam(name = "items", description = "Array of {orderItemSeqId, returnQuantity, returnReasonId, returnTypeId, returnPrice}",
                    required = true, type = "array") List<Object> items,
            @McpParam(name = "returnHeaderTypeId", description = "Default CUSTOMER_RETURN", required = false) String returnHeaderTypeId,
            @McpParam(name = "comments", description = "Optional comment attached as an order note", required = false) String comments) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue orderHeader = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
            if (orderHeader == null) throw new McpToolException("Order not found: " + orderId);
            GenericValue billTo = EntityQuery.use(delegator).from("OrderRole")
                    .where("orderId", orderId, "roleTypeId", "BILL_TO_CUSTOMER").queryFirst();
            if (billTo == null) throw new McpToolException("Order " + orderId + " has no BILL_TO_CUSTOMER party");
            GenericValue billFrom = EntityQuery.use(delegator).from("OrderRole")
                    .where("orderId", orderId, "roleTypeId", "BILL_FROM_VENDOR").queryFirst();

            Map<String, Object> headerParams = new LinkedHashMap<>();
            headerParams.put("returnHeaderTypeId", returnHeaderTypeId != null ? returnHeaderTypeId : "CUSTOMER_RETURN");
            headerParams.put("fromPartyId", billTo.getString("partyId"));
            if (billFrom != null) headerParams.put("toPartyId", billFrom.getString("partyId"));
            headerParams.put("statusId", "RETURN_REQUESTED");
            headerParams.put("currencyUomId", orderHeader.getString("currencyUom"));
            Map<String, Object> headerRes = ctx.runService("createReturnHeader", headerParams);
            String returnId = (String) headerRes.get("returnId");

            List<Map<String, Object>> rows = new ArrayList<>();
            if (items != null) {
                for (Object o : items) {
                    if (!(o instanceof Map)) throw new McpToolException("Each item must be an object {orderItemSeqId, returnQuantity}");
                    rows.add((Map<String, Object>) o);
                }
            }
            if (rows.isEmpty()) throw new McpToolException("At least one item {orderItemSeqId, returnQuantity} is required");
            List<Map<String, Object>> resultItems = new ArrayList<>();
            for (Map<String, Object> row : rows) {
                Object seqId = row.get("orderItemSeqId");
                if (!(seqId instanceof String)) throw new McpToolException("Each item needs an orderItemSeqId");
                BigDecimal returnQuantity;
                try {
                    Object qty = row.get("returnQuantity");
                    returnQuantity = qty == null ? BigDecimal.ONE : new BigDecimal(String.valueOf(qty));
                } catch (NumberFormatException e) {
                    throw new McpToolException("Invalid returnQuantity for item " + seqId);
                }
                Map<String, Object> itemParams = new LinkedHashMap<>();
                itemParams.put("returnId", returnId);
                itemParams.put("orderId", orderId);
                itemParams.put("orderItemSeqId", seqId);
                itemParams.put("returnQuantity", returnQuantity);
                itemParams.put("returnReasonId", row.get("returnReasonId") != null ? row.get("returnReasonId") : "RTN_NOT_WANT");
                itemParams.put("returnTypeId", row.get("returnTypeId") != null ? row.get("returnTypeId") : "RTN_REFUND");
                itemParams.put("returnItemTypeId", row.get("returnItemTypeId") != null ? row.get("returnItemTypeId") : "RET_FPROD_ITEM");
                if (row.get("returnPrice") != null) {
                    try {
                        itemParams.put("returnPrice", new BigDecimal(String.valueOf(row.get("returnPrice"))));
                    } catch (NumberFormatException e) {
                        throw new McpToolException("Invalid returnPrice for item " + seqId);
                    }
                }
                Map<String, Object> itemRes = ctx.runService("createReturnItem", itemParams);
                Map<String, Object> resultRow = new LinkedHashMap<>();
                resultRow.put("orderItemSeqId", seqId);
                resultRow.put("returnItemSeqId", itemRes.get("returnItemSeqId"));
                resultItems.add(resultRow);
            }
            if (UtilValidate.isNotEmpty(comments)) {
                Map<String, Object> noteParams = new LinkedHashMap<>();
                noteParams.put("orderId", orderId);
                noteParams.put("note", "Return " + returnId + ": " + comments);
                noteParams.put("internalNote", "N");
                ctx.runService("createOrderNote", noteParams);
            }
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("returnId", returnId);
            out.put("statusId", "RETURN_REQUESTED");
            out.put("items", resultItems);
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Return creation failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "quote", name = "create", description = "Create a sales quote for a party from a list of items.",
            readOnly = false, destructive = "false", requiresConfirmation = false, order = 51)
    @SuppressWarnings("unchecked")
    public static Object createQuote(McpCallContext ctx,
            @McpParam(name = "partyId", description = "Customer party id", required = true) String partyId,
            @McpParam(name = "productStoreId", description = "Product store id, optional", required = false) String productStoreId,
            @McpParam(name = "currencyUomId", description = "Currency, optional", required = false) String currencyUomId,
            @McpParam(name = "quoteName", description = "Optional quote name", required = false) String quoteName,
            @McpParam(name = "validThruDate", description = "Optional validity end date", required = false) Timestamp validThruDate,
            @McpParam(name = "items", description = "Array of {productId, quantity, quoteUnitPrice, comments}", required = true, type = "array") List<Object> items) throws McpToolException {
        Map<String, Object> quoteParams = new LinkedHashMap<>();
        quoteParams.put("quoteTypeId", "PRODUCT_QUOTE");
        quoteParams.put("partyId", partyId);
        quoteParams.put("statusId", "QUO_CREATED");
        quoteParams.put("issueDate", new Timestamp(System.currentTimeMillis()));
        if (productStoreId != null) quoteParams.put("productStoreId", productStoreId);
        if (currencyUomId != null) quoteParams.put("currencyUomId", currencyUomId);
        if (quoteName != null) quoteParams.put("quoteName", quoteName);
        if (validThruDate != null) quoteParams.put("validThruDate", validThruDate);
        Map<String, Object> quoteRes = ctx.runService("createQuote", quoteParams);
        String quoteId = (String) quoteRes.get("quoteId");

        List<Map<String, Object>> resultItems = new ArrayList<>();
        if (items != null) {
            for (Object o : items) {
                if (!(o instanceof Map)) throw new McpToolException("Each item must be an object {productId, quantity}");
                Map<String, Object> row = (Map<String, Object>) o;
                Object productId = row.get("productId");
                if (!(productId instanceof String)) throw new McpToolException("Each item needs a productId");
                BigDecimal quantity;
                try {
                    Object qty = row.get("quantity");
                    quantity = qty == null ? BigDecimal.ONE : new BigDecimal(String.valueOf(qty));
                } catch (NumberFormatException e) {
                    throw new McpToolException("Invalid quantity for product " + productId);
                }
                Map<String, Object> itemParams = new LinkedHashMap<>();
                itemParams.put("quoteId", quoteId);
                itemParams.put("productId", productId);
                itemParams.put("quantity", quantity);
                if (row.get("quoteUnitPrice") != null) {
                    try {
                        itemParams.put("quoteUnitPrice", new BigDecimal(String.valueOf(row.get("quoteUnitPrice"))));
                    } catch (NumberFormatException e) {
                        throw new McpToolException("Invalid quoteUnitPrice for product " + productId);
                    }
                }
                if (row.get("comments") != null) itemParams.put("comments", row.get("comments"));
                Map<String, Object> itemRes = ctx.runService("createQuoteItem", itemParams);
                Map<String, Object> resultRow = new LinkedHashMap<>();
                resultRow.put("productId", productId);
                resultRow.put("quoteItemSeqId", itemRes.get("quoteItemSeqId"));
                resultItems.add(resultRow);
            }
        }
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("quoteId", quoteId);
        out.put("items", resultItems);
        return out;
    }

    @McpResource(uri = "scipio://order/{orderId}", name = "Order", description = "One order as JSON: header, items, adjustments, status history, roles, ship groups.",
            mimeType = "application/json")
    public static String orderResource(McpCallContext ctx, Map<String, String> uriParams) throws McpToolException {
        return JsonRpc.writePretty(getOrder(ctx, uriParams.get("orderId")));
    }
}

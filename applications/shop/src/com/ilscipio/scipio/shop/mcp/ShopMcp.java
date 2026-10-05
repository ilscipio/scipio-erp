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
package com.ilscipio.scipio.shop.mcp;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.order.shoppingcart.CartItemModifyException;
import org.ofbiz.order.shoppingcart.ItemNotFoundException;
import org.ofbiz.order.shoppingcart.CheckOutHelper;
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.party.contact.ContactHelper;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.order.shoppingcart.ShoppingCartItem;

import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.def.McpAccess;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.protocol.McpSession;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;

/**
 * SCIPIO: 4.0.0: MCP server profile for the shop storefront: public product/category browsing plus an
 * authenticated customer's own cart and orders. Entities are intentionally not exposed: customers must not
 * read raw entity data through this server.
 */
@McpServer(name = "shop", title = "Scipio Shop", component = "shop", allowAnonymous = true,
        description = "Storefront: public catalog search plus a signed-in customer's own cart and orders.",
        topics = {
            @McpTopic(name = "shop_catalog", title = "Catalog", order = 10, featured = true,
                    description = "Products and categories: search, get detail, review."),
            @McpTopic(name = "shop_cart", title = "Cart", order = 20, featured = true,
                    description = "Shopping cart: view, add, remove, checkout."),
            @McpTopic(name = "shop_account", title = "Account", order = 30,
                    description = "Customer account: orders, profile, contacts, requests, quotes.")
        })
public final class ShopMcp {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final String CART_ATTR = "shoppingCart";

    private ShopMcp() {}

    @McpTool(topic = "shop_catalog", name = "search", description = "Search products by keyword; returns id, name and price.",
            access = McpAccess.PUBLIC, readOnly = true)
    public static Object searchProducts(McpCallContext ctx,
            @McpParam(name = "keyword", description = "Search keyword matched against the product name", required = true) String keyword,
            @McpParam(name = "limit", description = "Max rows to return", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            EntityCondition cond = EntityCondition.makeCondition(EntityOperator.OR,
                    EntityCondition.makeCondition(org.ofbiz.entity.condition.EntityFunction.UPPER_FIELD("productName"), EntityOperator.LIKE, "%" + keyword.toUpperCase() + "%"),
                    EntityCondition.makeCondition(org.ofbiz.entity.condition.EntityFunction.UPPER_FIELD("internalName"), EntityOperator.LIKE, "%" + keyword.toUpperCase() + "%"));
            List<GenericValue> products = EntityQuery.use(delegator).from("Product").where(cond)
                    .maxRows(ctx.limit(limit)).queryList();
            List<Map<String, Object>> result = new ArrayList<>();
            for (GenericValue product : products) {
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("productId", product.getString("productId"));
                row.put("productName", product.getString("productName"));
                row.put("price", price(ctx, product));
                result.add(row);
            }
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Product search failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "shop_catalog", name = "get", description = "Get product detail with price and availability.",
            access = McpAccess.PUBLIC, readOnly = true)
    public static Object getProduct(McpCallContext ctx,
            @McpParam(name = "productId", description = "Product id", required = true) String productId,
            @McpParam(name = "facilityId", description = "Facility id to check availability", required = false) String facilityId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).queryOne();
            if (product == null) {
                throw new McpToolException("Product not found: " + productId);
            }
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("productId", product.getString("productId"));
            result.put("productName", product.getString("productName"));
            result.put("description", product.getString("description"));
            result.put("price", price(ctx, product));
            if (facilityId != null) {
                Map<String, Object> invParams = new LinkedHashMap<>();
                invParams.put("productId", productId);
                invParams.put("facilityId", facilityId);
                result.put("inventory", ResultConverter.toJsonMap(ctx.runService("getInventoryAvailableByFacility", invParams)));
            }
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load product " + productId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "shop_catalog", name = "categories", description = "List categories under a parent, or top-level when omitted.",
            access = McpAccess.PUBLIC, readOnly = true)
    public static Object listCategories(McpCallContext ctx,
            @McpParam(name = "parentCategoryId", description = "Parent category id; omit for top-level categories", required = false) String parentCategoryId,
            @McpParam(name = "limit", description = "Max rows to return", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conditions = new ArrayList<>();
            if (parentCategoryId != null) conditions.add(EntityCondition.makeCondition("parentProductCategoryId", parentCategoryId));
            return ResultConverter.toJson(EntityQuery.use(delegator).from("ProductCategoryRollup")
                    .where(conditions).maxRows(ctx.limit(limit)).queryList());
        } catch (GenericEntityException e) {
            throw new McpToolException("Category listing failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "shop_cart", name = "get", description = "Get the current session's shopping cart.", readOnly = true)
    public static Object getCart(McpCallContext ctx) throws McpToolException {
        return cartToMap(requireCart(ctx, false));
    }

    @McpTool(topic = "shop_cart", name = "add", description = "Add a product to the cart, or increase its quantity.",
            readOnly = false)
    public static Object addToCart(McpCallContext ctx,
            @McpParam(name = "productId", description = "Product id", required = true) String productId,
            @McpParam(name = "quantity", description = "Quantity to add", required = true) BigDecimal quantity) throws McpToolException {
        ShoppingCart cart = requireCart(ctx, true);
        try {
            cart.addOrIncreaseItem(productId, null, quantity, null, null, null, null, null, null, null, null, null,
                    null, null, null, ctx.getDispatcher());
        } catch (CartItemModifyException | ItemNotFoundException e) {
            throw new McpToolException("Could not add product " + productId + " to the cart: " + e.getMessage());
        }
        return cartToMap(cart);
    }

    @McpTool(topic = "shop_cart", name = "remove", description = "Remove an item from the cart by its index.",
            readOnly = false)
    public static Object removeCartItem(McpCallContext ctx,
            @McpParam(name = "cartIndex", description = "Zero-based index of the cart item", required = true) Integer cartIndex) throws McpToolException {
        ShoppingCart cart = requireCart(ctx, false);
        try {
            cart.removeCartItem(cartIndex, ctx.getDispatcher());
        } catch (CartItemModifyException e) {
            throw new McpToolException("Could not remove cart item " + cartIndex + ": " + e.getMessage());
        }
        return cartToMap(cart);
    }

    @McpTool(topic = "shop_account", name = "orders", description = "List orders placed by the signed-in customer.", readOnly = true)
    public static Object getMyOrders(McpCallContext ctx,
            @McpParam(name = "limit", description = "Max rows to return", required = false) Integer limit) throws McpToolException {
        String partyId = requirePartyId(ctx);
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conditions = UtilMisc.toList(
                    EntityCondition.makeCondition("partyId", partyId),
                    EntityCondition.makeCondition("roleTypeId", EntityOperator.IN,
                            UtilMisc.toList("BILL_TO_CUSTOMER", "PLACING_CUSTOMER")));
            List<GenericValue> roles = EntityQuery.use(delegator).from("OrderRole")
                    .where(conditions).maxRows(ctx.limit(limit)).queryList();
            List<Map<String, Object>> result = new ArrayList<>();
            for (GenericValue role : roles) {
                GenericValue order = EntityQuery.use(delegator).from("OrderHeader")
                        .where("orderId", role.getString("orderId")).queryOne();
                if (order == null) continue;
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("orderId", order.getString("orderId"));
                row.put("statusId", order.getString("statusId"));
                row.put("orderDate", ResultConverter.toJson(order.getTimestamp("orderDate")));
                row.put("grandTotal", ResultConverter.toJson(order.getBigDecimal("grandTotal")));
                result.add(row);
            }
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Order lookup failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "shop_account", name = "order_get", description = "Get full detail for one of the customer's own orders.", readOnly = true)
    public static Object getOrder(McpCallContext ctx,
            @McpParam(name = "orderId", description = "Order id", required = true) String orderId) throws McpToolException {
        String partyId = requirePartyId(ctx);
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue role = EntityQuery.use(delegator).from("OrderRole")
                    .where("orderId", orderId, "partyId", partyId).queryFirst();
            if (role == null) {
                throw McpToolException.denied("Order " + orderId + " does not belong to this customer");
            }
            GenericValue orderHeader = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
            OrderReadHelper helper = new OrderReadHelper(ctx.getDispatcher(), ctx.getLocale(), orderHeader);
            Map<String, Object> result = new LinkedHashMap<>();
            result.put("header", ResultConverter.toJson(orderHeader));
            result.put("items", ResultConverter.toJson(helper.getOrderItems()));
            result.put("statusHistory", ResultConverter.toJson(helper.getOrderStatuses()));
            return result;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load order " + orderId + ": " + e.getMessage());
        }
    }

    @McpTool(topic = "shop_cart", name = "checkout", description = "Place an order from the session cart.",
            readOnly = false, requiresConfirmation = true)
    public static Object checkout(McpCallContext ctx,
            @McpParam(name = "shippingContactMechId", description = "Postal address contact mech id; default: the customer's SHIPPING_LOCATION address", required = false) String shippingContactMechId,
            @McpParam(name = "shipmentMethodTypeId", description = "e.g. STANDARD, NEXT_DAY; default: the store's first method", required = false) String shipmentMethodTypeId,
            @McpParam(name = "carrierPartyId", description = "Carrier id, e.g. _NA_, UPS; default from the store", required = false) String carrierPartyId,
            @McpParam(name = "paymentMethodTypeId", description = "Payment method type, default EXT_OFFLINE", required = false) String paymentMethodTypeId,
            @McpParam(name = "paymentMethodId", description = "Stored payment method id; overrides paymentMethodTypeId", required = false) String paymentMethodId) throws McpToolException {
        String partyId = requirePartyId(ctx);
        ShoppingCart cart = requireCart(ctx, false);
        if (cart.size() == 0) {
            throw new McpToolException("Cart is empty");
        }
        Delegator delegator = ctx.getDelegator();
        try {
            cart.setUserLogin(ctx.getUserLogin(), ctx.getDispatcher());
            cart.setOrderPartyId(partyId);
            if (UtilValidate.isEmpty(shippingContactMechId)) {
                GenericValue party = EntityQuery.use(delegator).from("Party").where("partyId", partyId).queryOne();
                java.util.Collection<GenericValue> addresses = party != null ? ContactHelper.getContactMech(party, "SHIPPING_LOCATION", "POSTAL_ADDRESS", false) : java.util.Collections.<GenericValue>emptyList();
                if (addresses.isEmpty() && party != null) {
                    addresses = ContactHelper.getContactMech(party, null, "POSTAL_ADDRESS", false);
                }
                if (addresses.isEmpty()) {
                    throw new McpToolException("The customer has no postal address; create one first (service createPartyPostalAddress)");
                }
                shippingContactMechId = addresses.iterator().next().getString("contactMechId");
            }
            cart.setAllShippingContactMechId(shippingContactMechId);
            if (UtilValidate.isEmpty(shipmentMethodTypeId)) {
                GenericValue meth = EntityQuery.use(delegator).from("ProductStoreShipmentMeth")
                        .where("productStoreId", ctx.getProductStoreId()).orderBy("sequenceNumber").queryFirst();
                if (meth != null) {
                    shipmentMethodTypeId = meth.getString("shipmentMethodTypeId");
                    if (UtilValidate.isEmpty(carrierPartyId)) carrierPartyId = meth.getString("partyId");
                } else {
                    shipmentMethodTypeId = "NO_SHIPPING";
                }
            }
            cart.setAllShipmentMethodTypeId(shipmentMethodTypeId);
            cart.setAllCarrierPartyId(UtilValidate.isNotEmpty(carrierPartyId) ? carrierPartyId : "_NA_");
            cart.clearPayments();
            cart.addPayment(UtilValidate.isNotEmpty(paymentMethodId) ? paymentMethodId
                    : (UtilValidate.isNotEmpty(paymentMethodTypeId) ? paymentMethodTypeId : "EXT_OFFLINE"));
            CheckOutHelper helper = new CheckOutHelper(ctx.getDispatcher(), delegator, cart);
            try {
                helper.calcAndAddTax();
            } catch (Exception e) {
                Debug.logWarning("MCP shop_checkout: tax calculation skipped: " + e.getMessage(), module);
            }
            com.ilscipio.scipio.order.mcp.OrderAgentHelper.checkSpendCap(ctx, cart.getGrandTotal(), cart.getCurrency());
            Map<String, Object> res = helper.createOrder(ctx.getUserLogin(), null, null, null, false, null, ctx.getWebSiteId());
            if (ServiceUtil.isError(res)) {
                throw new McpToolException("Order creation failed: " + ServiceUtil.getErrorMessage(res));
            }
            String orderId = (String) res.get("orderId");
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("orderId", orderId);
            out.put("grandTotal", cart.getGrandTotal().setScale(2, java.math.RoundingMode.HALF_UP));
            out.put("currencyUomId", cart.getCurrency());
            out.put("shipmentMethodTypeId", shipmentMethodTypeId);
            out.put("shippingContactMechId", shippingContactMechId);
            try {
                GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", ctx.getProductStoreId()).cache().queryOne();
                Map<String, Object> pay = helper.processPayment(orderId, cart.getGrandTotal(), cart.getCurrency(), store, ctx.getUserLogin(), false, false);
                out.put("paymentProcessed", !ServiceUtil.isError(pay));
            } catch (Exception e) {
                out.put("paymentProcessed", false);
                out.put("paymentMessage", e.getMessage());
            }
            GenericValue header = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
            if (header != null) out.put("statusId", header.getString("statusId"));
            ctx.getSession().removeAttribute(CART_ATTR);
            return out;
        } catch (McpToolException e) {
            throw e;
        } catch (Exception e) {
            throw new McpToolException("Checkout failed: " + e.getMessage());
        }
    }

    @McpTool(topic = "shop_account", name = "profile_update", description = "Update the signed-in customer's profile fields.",
            readOnly = false, destructive = "false", order = 40)
    public static Object updateProfile(McpCallContext ctx,
            @McpParam(name = "firstName", required = false) String firstName,
            @McpParam(name = "lastName", required = false) String lastName,
            @McpParam(name = "gender", required = false) String gender,
            @McpParam(name = "birthDate", description = "Format yyyy-MM-dd", required = false) String birthDate,
            @McpParam(name = "comments", required = false) String comments) throws McpToolException {
        String partyId = requirePartyId(ctx);
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue person = EntityQuery.use(delegator).from("Person").where("partyId", partyId).queryOne();
            if (person == null) {
                throw new McpToolException("No person profile found for this customer");
            }
            Map<String, Object> params = new LinkedHashMap<>();
            params.put("partyId", partyId);
            params.put("firstName", firstName != null ? firstName : person.getString("firstName"));
            params.put("lastName", lastName != null ? lastName : person.getString("lastName"));
            if (gender != null) params.put("gender", gender);
            if (birthDate != null) {
                try {
                    params.put("birthDate", java.sql.Date.valueOf(birthDate));
                } catch (IllegalArgumentException e) {
                    throw new McpToolException("Invalid birthDate; use format yyyy-MM-dd");
                }
            }
            if (comments != null) params.put("comments", comments);
            Map<String, Object> res = ctx.runService("updatePerson", params);
            return ResultConverter.toJsonMap(res);
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to update profile: " + e.getMessage());
        }
    }

    @McpTool(topic = "shop_account", name = "address_set", description = "Create or update the signed-in customer's postal address.",
            readOnly = false, destructive = "false", order = 41)
    public static Object setAddress(McpCallContext ctx,
            @McpParam(name = "toName", required = false) String toName,
            @McpParam(name = "address1", required = true) String address1,
            @McpParam(name = "address2", required = false) String address2,
            @McpParam(name = "city", required = true) String city,
            @McpParam(name = "postalCode", required = true) String postalCode,
            @McpParam(name = "stateProvinceGeoId", required = false) String stateProvinceGeoId,
            @McpParam(name = "countryGeoId", required = false) String countryGeoId,
            @McpParam(name = "contactMechId", description = "Existing address id to update; omit to create new", required = false) String contactMechId,
            @McpParam(name = "setShipping", description = "Default true", required = false) Boolean setShipping,
            @McpParam(name = "setBilling", description = "Default false", required = false) Boolean setBilling) throws McpToolException {
        String partyId = requirePartyId(ctx);
        boolean shipping = (setShipping == null) || setShipping;
        boolean billing = (setBilling != null) && setBilling;
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("partyId", partyId);
        if (toName != null) params.put("toName", toName);
        params.put("address1", address1);
        if (address2 != null) params.put("address2", address2);
        params.put("city", city);
        params.put("postalCode", postalCode);
        if (stateProvinceGeoId != null) params.put("stateProvinceGeoId", stateProvinceGeoId);
        if (countryGeoId != null) params.put("countryGeoId", countryGeoId);
        String resultContactMechId;
        if (UtilValidate.isNotEmpty(contactMechId)) {
            params.put("contactMechId", contactMechId);
            ctx.runService("updatePartyPostalAddress", params);
            resultContactMechId = contactMechId;
        } else {
            Map<String, Object> res = ctx.runService("createPartyPostalAddress", params);
            resultContactMechId = (String) res.get("contactMechId");
        }
        if (shipping) ensureContactMechPurpose(ctx, partyId, resultContactMechId, "SHIPPING_LOCATION");
        if (billing) ensureContactMechPurpose(ctx, partyId, resultContactMechId, "BILLING_LOCATION");
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("contactMechId", resultContactMechId);
        out.put("shipping", shipping);
        out.put("billing", billing);
        return out;
    }

    @McpTool(topic = "shop_account", name = "email_set", description = "Create or update the signed-in customer's email address.",
            readOnly = false, destructive = "false", order = 42)
    public static Object setEmail(McpCallContext ctx,
            @McpParam(name = "emailAddress", required = true) String emailAddress,
            @McpParam(name = "contactMechId", required = false) String contactMechId) throws McpToolException {
        String partyId = requirePartyId(ctx);
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("partyId", partyId);
        params.put("emailAddress", emailAddress);
        String resultContactMechId;
        if (UtilValidate.isNotEmpty(contactMechId)) {
            params.put("contactMechId", contactMechId);
            ctx.runService("updatePartyEmailAddress", params);
            resultContactMechId = contactMechId;
        } else {
            Map<String, Object> res = ctx.runService("createPartyEmailAddress", params);
            resultContactMechId = (String) res.get("contactMechId");
        }
        ensureContactMechPurpose(ctx, partyId, resultContactMechId, "PRIMARY_EMAIL");
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("contactMechId", resultContactMechId);
        return out;
    }

    @McpTool(topic = "shop_account", name = "phone_set", description = "Create or update the signed-in customer's phone number.", readOnly = false, destructive = "false", order = 43)
    public static Object setPhone(McpCallContext ctx,
            @McpParam(name = "countryCode", required = false) String countryCode,
            @McpParam(name = "areaCode", required = false) String areaCode,
            @McpParam(name = "contactNumber", required = true) String contactNumber,
            @McpParam(name = "contactMechId", required = false) String contactMechId) throws McpToolException {
        String partyId = requirePartyId(ctx);
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("partyId", partyId);
        if (countryCode != null) params.put("countryCode", countryCode);
        if (areaCode != null) params.put("areaCode", areaCode);
        params.put("contactNumber", contactNumber);
        String resultContactMechId;
        if (UtilValidate.isNotEmpty(contactMechId)) {
            params.put("contactMechId", contactMechId);
            ctx.runService("updatePartyTelecomNumber", params);
            resultContactMechId = contactMechId;
        } else {
            Map<String, Object> res = ctx.runService("createPartyTelecomNumber", params);
            resultContactMechId = (String) res.get("contactMechId");
        }
        ensureContactMechPurpose(ctx, partyId, resultContactMechId, "PRIMARY_PHONE");
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("contactMechId", resultContactMechId);
        return out;
    }

    @McpTool(topic = "shop_catalog", name = "review_create", description = "Post a product review as the signed-in customer.",
            readOnly = false, destructive = "false", order = 44)
    public static Object createReview(McpCallContext ctx,
            @McpParam(name = "productId", required = true) String productId,
            @McpParam(name = "productRating", description = "1 to 5", required = true) BigDecimal productRating,
            @McpParam(name = "productReview", description = "Review text", required = true) String productReview,
            @McpParam(name = "postedAnonymous", description = "Default false", required = false) Boolean postedAnonymous) throws McpToolException {
        requirePartyId(ctx);
        if (productRating.compareTo(BigDecimal.ONE) < 0 || productRating.compareTo(new BigDecimal("5")) > 0) {
            throw new McpToolException("productRating must be between 1 and 5");
        }
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("productId", productId);
        params.put("productStoreId", ctx.getProductStoreId());
        params.put("userLoginId", ctx.getUserLoginId());
        params.put("productRating", productRating);
        params.put("productReview", productReview);
        params.put("postedAnonymous", (postedAnonymous != null && postedAnonymous) ? "Y" : "N");
        Map<String, Object> res = ctx.runService("createProductReview", params);
        return ResultConverter.toJsonMap(res);
    }

    @McpTool(topic = "shop_account", name = "contact_list_signup", description = "Sign the customer up for a marketing contact list.",
            readOnly = false, destructive = "false", order = 45)
    public static Object contactListSignup(McpCallContext ctx,
            @McpParam(name = "contactListId", required = true) String contactListId,
            @McpParam(name = "emailAddress", required = false) String emailAddress) throws McpToolException {
        String partyId = requirePartyId(ctx);
        String email = UtilValidate.isNotEmpty(emailAddress) ? emailAddress : primaryEmail(ctx, partyId);
        if (UtilValidate.isEmpty(email)) {
            throw new McpToolException("No email address available; pass emailAddress or set one first with shop_email_set");
        }
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("contactListId", contactListId);
        params.put("email", email);
        params.put("partyId", partyId);
        Map<String, Object> res = ctx.runService("signUpForContactList", params);
        return ResultConverter.toJsonMap(res);
    }

    @McpTool(topic = "shop_account", name = "contact_list_unsubscribe", description = "Unsubscribe the customer from a marketing contact list.", readOnly = false, destructive = "false", order = 46)
    public static Object contactListUnsubscribe(McpCallContext ctx,
            @McpParam(name = "contactListId", required = true) String contactListId) throws McpToolException {
        String partyId = requirePartyId(ctx);
        String email = primaryEmail(ctx, partyId);
        if (UtilValidate.isEmpty(email)) {
            throw new McpToolException("No email address on file for this customer");
        }
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("contactListId", contactListId);
        params.put("email", email);
        params.put("partyId", partyId);
        Map<String, Object> res = ctx.runService("unsubscribeContactListParty", params);
        return ResultConverter.toJsonMap(res);
    }

    @McpTool(topic = "shop_account", name = "quote_from_list", description = "Create a quote from one of the customer's shopping lists.", readOnly = false, destructive = "false", order = 47)
    public static Object quoteFromList(McpCallContext ctx,
            @McpParam(name = "shoppingListId", required = true) String shoppingListId) throws McpToolException {
        String partyId = requirePartyId(ctx);
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue list = EntityQuery.use(delegator).from("ShoppingList").where("shoppingListId", shoppingListId).queryOne();
            if (list == null || !partyId.equals(list.getString("partyId"))) {
                throw McpToolException.denied("Shopping list " + shoppingListId + " does not belong to this customer");
            }
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load shopping list " + shoppingListId + ": " + e.getMessage());
        }
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("shoppingListId", shoppingListId);
        Map<String, Object> res = ctx.runService("createQuoteFromShoppingList", params);
        return ResultConverter.toJsonMap(res);
    }

    @McpTool(topic = "shop_account", name = "request_create", description = "Create a customer request, from a shopping list or given details.", readOnly = false, destructive = "false", order = 48)
    public static Object createRequest(McpCallContext ctx,
            @McpParam(name = "shoppingListId", required = false) String shoppingListId,
            @McpParam(name = "custRequestName", required = false) String custRequestName,
            @McpParam(name = "description", required = false) String description) throws McpToolException {
        String partyId = requirePartyId(ctx);
        Delegator delegator = ctx.getDelegator();
        Map<String, Object> params = new LinkedHashMap<>();
        if (UtilValidate.isNotEmpty(shoppingListId)) {
            try {
                GenericValue list = EntityQuery.use(delegator).from("ShoppingList").where("shoppingListId", shoppingListId).queryOne();
                if (list == null || !partyId.equals(list.getString("partyId"))) {
                    throw McpToolException.denied("Shopping list " + shoppingListId + " does not belong to this customer");
                }
            } catch (GenericEntityException e) {
                throw new McpToolException("Failed to load shopping list " + shoppingListId + ": " + e.getMessage());
            }
            params.put("shoppingListId", shoppingListId);
            Map<String, Object> res = ctx.runService("createCustRequestFromShoppingList", params);
            return ResultConverter.toJsonMap(res);
        }
        if (UtilValidate.isEmpty(custRequestName)) {
            throw new McpToolException("custRequestName is required when shoppingListId is not given");
        }
        params.put("fromPartyId", partyId);
        params.put("custRequestTypeId", "RF_INFO");
        params.put("custRequestName", custRequestName);
        if (description != null) params.put("description", description);
        Map<String, Object> res = ctx.runService("createCustRequest", params);
        return ResultConverter.toJsonMap(res);
    }

    @McpTool(topic = "shop_account", name = "order_confirmation_resend", description = "Resend the order confirmation email for a customer order.",
            readOnly = false, destructive = "true", requiresConfirmation = true, order = 80)
    public static Object resendOrderConfirmation(McpCallContext ctx,
            @McpParam(name = "orderId", required = true) String orderId) throws McpToolException {
        String partyId = requirePartyId(ctx);
        try {
            List<EntityCondition> conditions = UtilMisc.toList(
                    EntityCondition.makeCondition("orderId", orderId),
                    EntityCondition.makeCondition("partyId", partyId),
                    EntityCondition.makeCondition("roleTypeId", EntityOperator.IN,
                            UtilMisc.toList("BILL_TO_CUSTOMER", "PLACING_CUSTOMER")));
            GenericValue role = EntityQuery.use(ctx.getDelegator()).from("OrderRole").where(conditions).queryFirst();
            if (role == null) {
                throw McpToolException.denied("Order " + orderId + " does not belong to this customer");
            }
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to load order " + orderId + ": " + e.getMessage());
        }
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("orderId", orderId);
        Map<String, Object> res = ctx.runService("sendOrderConfirmation", params);
        return ResultConverter.toJsonMap(res);
    }

    private static void ensureContactMechPurpose(McpCallContext ctx, String partyId, String contactMechId, String purposeTypeId) throws McpToolException {
        try {
            GenericValue existing = EntityQuery.use(ctx.getDelegator()).from("PartyContactMechPurpose")
                    .where("partyId", partyId, "contactMechId", contactMechId, "contactMechPurposeTypeId", purposeTypeId)
                    .filterByDate().queryFirst();
            if (existing != null) return;
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to check contact mech purpose: " + e.getMessage());
        }
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("partyId", partyId);
        params.put("contactMechId", contactMechId);
        params.put("contactMechPurposeTypeId", purposeTypeId);
        ctx.runService("createPartyContactMechPurpose", params);
    }

    private static String primaryEmail(McpCallContext ctx, String partyId) throws McpToolException {
        try {
            GenericValue party = EntityQuery.use(ctx.getDelegator()).from("Party").where("partyId", partyId).queryOne();
            if (party == null) return null;
            java.util.Collection<GenericValue> emails = ContactHelper.getContactMech(party, "PRIMARY_EMAIL", "EMAIL_ADDRESS", false);
            if (emails.isEmpty()) return null;
            return emails.iterator().next().getString("infoString");
        } catch (GenericEntityException e) {
            throw new McpToolException("Failed to look up primary email: " + e.getMessage());
        }
    }

    private static String requirePartyId(McpCallContext ctx) throws McpToolException {
        String partyId = ctx.getPartyId();
        if (partyId == null) {
            throw McpToolException.denied("A signed-in customer is required");
        }
        return partyId;
    }

    private static BigDecimal price(McpCallContext ctx, GenericValue product) throws McpToolException {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("product", product);
        params.put("productStoreId", ctx.getProductStoreId());
        params.put("currencyUomId", ctx.getCurrencyUomId());
        Map<String, Object> priceResult = ctx.runService("calculateProductPrice", params);
        BigDecimal p = (BigDecimal) priceResult.get("price");
        return p != null ? p.setScale(2, java.math.RoundingMode.HALF_UP) : null;
    }

    private static ShoppingCart requireCart(McpCallContext ctx, boolean createIfMissing) throws McpToolException {
        McpSession session = ctx.getSession();
        if (session == null) {
            throw new McpToolException("initialize a session first");
        }
        ShoppingCart cart = session.getAttribute(CART_ATTR, ShoppingCart.class);
        if (cart == null) {
            if (!createIfMissing) {
                throw new McpToolException("No cart in this session yet; add a product first");
            }
            cart = new ShoppingCart(ctx.getDelegator(), ctx.getProductStoreId(), ctx.getWebSiteId(), ctx.getLocale(),
                    ctx.getCurrencyUomId());
            session.setAttribute(CART_ATTR, cart);
        }
        return cart;
    }

    private static Map<String, Object> cartToMap(ShoppingCart cart) {
        Map<String, Object> result = new LinkedHashMap<>();
        List<Map<String, Object>> items = new ArrayList<>();
        int i = 0;
        for (ShoppingCartItem item : cart.items()) {
            Map<String, Object> row = new LinkedHashMap<>();
            row.put("cartIndex", i++);
            row.put("productId", item.getProductId());
            row.put("productName", item.getName());
            row.put("quantity", item.getQuantity());
            row.put("subTotal", item.getItemSubTotal());
            items.add(row);
        }
        result.put("items", items);
        result.put("subTotal", cart.getSubTotal().setScale(2, java.math.RoundingMode.HALF_UP));
        result.put("grandTotal", cart.getGrandTotal().setScale(2, java.math.RoundingMode.HALF_UP));
        return result;
    }
}

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
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.order.shoppingcart.CartItemModifyException;
import org.ofbiz.order.shoppingcart.CheckOutHelper;
import org.ofbiz.order.shoppingcart.ItemNotFoundException;
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.order.shoppingcart.ShoppingCartItem;
import org.ofbiz.party.contact.ContactHelper;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.security.McpPrincipal;

/**
 * SCIPIO: 4.0.0: Shared order placement logic for agent tools (back-office {@code order_create} and the shop
 * assistant): cart creation, item lines, shipping and payment defaults, the token spend cap, and order creation.
 */
public final class OrderAgentHelper {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private OrderAgentHelper() {}

    /** Denies the call when the token carries a spend cap ({@code McpAccessToken.maxOrderAmount}) below the total. */
    public static void checkSpendCap(McpCallContext ctx, BigDecimal grandTotal, String currencyUomId) throws McpToolException {
        McpPrincipal p = ctx.getPrincipal();
        BigDecimal cap = p != null ? p.getMaxOrderAmount() : null;
        if (cap != null && grandTotal != null && grandTotal.compareTo(cap) > 0) {
            throw McpToolException.denied("Order total " + grandTotal.setScale(2, RoundingMode.HALF_UP) + " " + currencyUomId
                    + " exceeds this token's spend cap of " + cap.setScale(2, RoundingMode.HALF_UP));
        }
    }

    /** A new sales order cart for a store; currency and web site default from the store. */
    public static ShoppingCart newSalesCart(McpCallContext ctx, String productStoreId, String webSiteId, String currencyUomId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", productStoreId).cache().queryOne();
            if (store == null) throw new McpToolException("Product store not found: " + productStoreId);
            if (UtilValidate.isEmpty(currencyUomId)) currencyUomId = store.getString("defaultCurrencyUomId");
            if (UtilValidate.isEmpty(webSiteId)) {
                GenericValue ws = EntityQuery.use(delegator).from("WebSite").where("productStoreId", productStoreId).queryFirst();
                if (ws != null) webSiteId = ws.getString("webSiteId");
            }
            ShoppingCart cart = new ShoppingCart(delegator, productStoreId, webSiteId, ctx.getLocale(), currencyUomId);
            cart.setOrderType("SALES_ORDER");
            return cart;
        } catch (GenericEntityException e) {
            throw new McpToolException("Could not prepare a cart for store " + productStoreId + ": " + e.getMessage());
        }
    }

    /** Adds {@code {productId, quantity}} rows to the cart. */
    public static void addItems(McpCallContext ctx, ShoppingCart cart, List<Map<String, Object>> items) throws McpToolException {
        if (items == null || items.isEmpty()) throw new McpToolException("At least one item {productId, quantity} is required");
        for (Map<String, Object> item : items) {
            Object pid = item.get("productId");
            Object qty = item.get("quantity");
            if (!(pid instanceof String) || ((String) pid).isEmpty()) throw new McpToolException("Each item needs a productId");
            BigDecimal quantity;
            try {
                quantity = qty == null ? BigDecimal.ONE : new BigDecimal(String.valueOf(qty));
            } catch (NumberFormatException e) {
                throw new McpToolException("Invalid quantity " + qty + " for product " + pid);
            }
            try {
                cart.addOrIncreaseItem((String) pid, null, quantity, null, null, null, null, null, null, null, null, null,
                        null, null, null, ctx.getDispatcher());
            } catch (CartItemModifyException | ItemNotFoundException e) {
                throw new McpToolException("Could not add product " + pid + ": " + e.getMessage());
            }
        }
    }

    /**
     * Applies party, shipping and payment defaults, calculates tax, checks the spend cap and creates the order.
     * Returns orderId, totals, the resolved shipping choices and the payment result.
     */
    public static Map<String, Object> placeOrder(McpCallContext ctx, ShoppingCart cart, String partyId, String shippingContactMechId,
                                                 String shipmentMethodTypeId, String carrierPartyId, String paymentMethodTypeId,
                                                 String paymentMethodId) throws McpToolException {
        if (cart.size() == 0) throw new McpToolException("Cart is empty");
        Delegator delegator = ctx.getDelegator();
        try {
            cart.setUserLogin(ctx.getUserLogin(), ctx.getDispatcher());
            cart.setOrderPartyId(partyId);
            if (UtilValidate.isEmpty(shippingContactMechId)) {
                GenericValue party = EntityQuery.use(delegator).from("Party").where("partyId", partyId).queryOne();
                Collection<GenericValue> addresses = party != null ? ContactHelper.getContactMech(party, "SHIPPING_LOCATION", "POSTAL_ADDRESS", false) : Collections.<GenericValue>emptyList();
                if (addresses.isEmpty() && party != null) {
                    addresses = ContactHelper.getContactMech(party, null, "POSTAL_ADDRESS", false);
                }
                if (addresses.isEmpty()) {
                    throw new McpToolException("Party " + partyId + " has no postal address; add one first (party tool contact_mech_add)");
                }
                shippingContactMechId = addresses.iterator().next().getString("contactMechId");
            }
            cart.setAllShippingContactMechId(shippingContactMechId);
            if (UtilValidate.isEmpty(shipmentMethodTypeId)) {
                GenericValue meth = EntityQuery.use(delegator).from("ProductStoreShipmentMeth")
                        .where("productStoreId", cart.getProductStoreId()).orderBy("sequenceNumber").queryFirst();
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
                Debug.logWarning("MCP order placement: tax calculation skipped: " + e.getMessage(), module);
            }
            checkSpendCap(ctx, cart.getGrandTotal(), cart.getCurrency());
            Map<String, Object> res = helper.createOrder(ctx.getUserLogin(), null, null, null, false, null, cart.getWebSiteId());
            if (ServiceUtil.isError(res)) {
                throw new McpToolException("Order creation failed: " + ServiceUtil.getErrorMessage(res));
            }
            String orderId = (String) res.get("orderId");
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("orderId", orderId);
            out.put("grandTotal", cart.getGrandTotal().setScale(2, RoundingMode.HALF_UP));
            out.put("currencyUomId", cart.getCurrency());
            out.put("shipmentMethodTypeId", shipmentMethodTypeId);
            out.put("shippingContactMechId", shippingContactMechId);
            try {
                GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", cart.getProductStoreId()).cache().queryOne();
                Map<String, Object> pay = helper.processPayment(orderId, cart.getGrandTotal(), cart.getCurrency(), store, ctx.getUserLogin(), false, false);
                out.put("paymentProcessed", !ServiceUtil.isError(pay));
            } catch (Exception e) {
                out.put("paymentProcessed", false);
                out.put("paymentMessage", e.getMessage());
            }
            GenericValue header = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
            if (header != null) out.put("statusId", header.getString("statusId"));
            return out;
        } catch (McpToolException e) {
            throw e;
        } catch (Exception e) {
            throw new McpToolException("Order placement failed: " + e.getMessage());
        }
    }

    /**
     * A new purchase order cart for a store and supplier; currency defaults from the store, the bill-to company
     * defaults from the store's {@code payToPartyId}.
     */
    public static ShoppingCart newPurchaseCart(McpCallContext ctx, String productStoreId, String currencyUomId,
                                                String supplierPartyId, String facilityId) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", productStoreId).cache().queryOne();
            if (store == null) throw new McpToolException("Product store not found: " + productStoreId);
            if (UtilValidate.isEmpty(currencyUomId)) currencyUomId = store.getString("defaultCurrencyUomId");
            String webSiteId = null;
            GenericValue ws = EntityQuery.use(delegator).from("WebSite").where("productStoreId", productStoreId).queryFirst();
            if (ws != null) webSiteId = ws.getString("webSiteId");
            String companyPartyId = store.getString("payToPartyId");
            if (UtilValidate.isEmpty(companyPartyId)) {
                throw new McpToolException("Product store " + productStoreId + " has no payToPartyId; cannot bill a purchase order to it");
            }
            ShoppingCart cart = new ShoppingCart(delegator, productStoreId, webSiteId, ctx.getLocale(), currencyUomId);
            cart.setOrderType("PURCHASE_ORDER");
            if (UtilValidate.isNotEmpty(facilityId)) cart.setFacilityId(facilityId);
            cart.setBillToCustomerPartyId(companyPartyId);
            cart.setBillFromVendorPartyId(supplierPartyId);
            cart.setOrderPartyId(supplierPartyId);
            return cart;
        } catch (GenericEntityException e) {
            throw new McpToolException("Could not prepare a purchase cart for store " + productStoreId + ": " + e.getMessage());
        }
    }

    /**
     * Adds {@code {productId, quantity, price}} rows to a purchase order cart. The price, when given, overrides the
     * SupplierProduct price that the cart would otherwise resolve automatically.
     */
    public static void addPurchaseItems(McpCallContext ctx, ShoppingCart cart, List<Map<String, Object>> items) throws McpToolException {
        if (items == null || items.isEmpty()) throw new McpToolException("At least one item {productId, quantity} is required");
        for (Map<String, Object> item : items) {
            Object pid = item.get("productId");
            Object qty = item.get("quantity");
            if (!(pid instanceof String) || ((String) pid).isEmpty()) throw new McpToolException("Each item needs a productId");
            BigDecimal quantity;
            try {
                quantity = qty == null ? BigDecimal.ONE : new BigDecimal(String.valueOf(qty));
            } catch (NumberFormatException e) {
                throw new McpToolException("Invalid quantity " + qty + " for product " + pid);
            }
            BigDecimal price = null;
            Object priceObj = item.get("price");
            if (priceObj != null) {
                try {
                    price = new BigDecimal(String.valueOf(priceObj));
                } catch (NumberFormatException e) {
                    throw new McpToolException("Invalid price " + priceObj + " for product " + pid);
                }
            }
            try {
                int idx = cart.addOrIncreaseItem((String) pid, null, quantity, null, null, null, null, null, null, null, null, null,
                        null, null, null, ctx.getDispatcher());
                if (price != null) {
                    ShoppingCartItem cartItem = cart.findCartItem(idx);
                    if (cartItem != null) {
                        cartItem.setBasePrice(price);
                        cartItem.setIsModifiedPrice(true);
                    }
                }
            } catch (CartItemModifyException | ItemNotFoundException e) {
                throw new McpToolException("Could not add product " + pid + ": " + e.getMessage());
            }
        }
    }

    /**
     * Checks the spend cap and creates a draft purchase order (status ORDER_CREATED) from the cart. Returns orderId,
     * totals, the supplier and the resolved items.
     */
    public static Map<String, Object> placePurchaseOrder(McpCallContext ctx, ShoppingCart cart, String supplierPartyId) throws McpToolException {
        if (cart.size() == 0) throw new McpToolException("Cart is empty");
        Delegator delegator = ctx.getDelegator();
        try {
            cart.setUserLogin(ctx.getUserLogin(), ctx.getDispatcher());
            checkSpendCap(ctx, cart.getGrandTotal(), cart.getCurrency());
            CheckOutHelper helper = new CheckOutHelper(ctx.getDispatcher(), delegator, cart);
            Map<String, Object> res = helper.createOrder(ctx.getUserLogin(), null, null, null, false, null, cart.getWebSiteId());
            if (ServiceUtil.isError(res)) {
                throw new McpToolException("Purchase order creation failed: " + ServiceUtil.getErrorMessage(res));
            }
            String orderId = (String) res.get("orderId");
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("orderId", orderId);
            out.put("grandTotal", cart.getGrandTotal().setScale(2, RoundingMode.HALF_UP));
            out.put("currencyUomId", cart.getCurrency());
            out.put("supplierPartyId", supplierPartyId);
            GenericValue header = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
            if (header != null) out.put("statusId", header.getString("statusId"));
            List<Map<String, Object>> items = new ArrayList<>();
            for (int i = 0; i < cart.size(); i++) {
                ShoppingCartItem item = cart.findCartItem(i);
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("productId", item.getProductId());
                row.put("quantity", item.getQuantity());
                row.put("price", item.getBasePrice());
                items.add(row);
            }
            out.put("items", items);
            return out;
        } catch (McpToolException e) {
            throw e;
        } catch (Exception e) {
            throw new McpToolException("Purchase order placement failed: " + e.getMessage());
        }
    }
}

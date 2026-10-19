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
package com.ilscipio.scipio.acp.checkout;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
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
import org.ofbiz.order.shoppingcart.product.ProductPromoWorker;
import org.ofbiz.order.shoppingcart.shipping.ShippingEvents;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.acp.AcpConfig;
import com.ilscipio.scipio.acp.AcpException;
import com.ilscipio.scipio.acp.AcpJson;
import com.ilscipio.scipio.acp.payment.AcpPaymentHandler;

/**
 * SCIPIO: 4.0.0: Prices the state of an ACP checkout session with a fresh {@link ShoppingCart}: catalog prices,
 * stock rules, promotions and codes, shipping estimates per store shipment method, and tax. Renders the session JSON.
 */
public final class AcpPricing {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final Set<String> TAX_TYPES = Set.of("SALES_TAX", "VAT_TAX");

    private AcpPricing() {}

    /** The priced session. */
    public static final class Result {
        public ShoppingCart cart;
        public String status;
        public String currency;
        public BigDecimal total = BigDecimal.ZERO;
        public long totalMinor;
        public String shipContactMechId;
        public List<Map<String, Object>> lineItems = new ArrayList<>();
        public List<Map<String, Object>> totals = new ArrayList<>();
        public List<Map<String, Object>> fulfillmentOptions = new ArrayList<>();
        public List<Map<String, Object>> selected = new ArrayList<>();
        public List<Map<String, Object>> messages = new ArrayList<>();
        public Map<String, Object> discounts;
        boolean blocking;
    }

    public static Result price(AcpContext ctx, GenericValue row, Map<String, Object> state) throws AcpException {
        Delegator delegator = ctx.getDelegator();
        LocalDispatcher dispatcher = ctx.getDispatcher();
        Result r = new Result();
        r.currency = row.getString("currencyUomId");
        try {
            GenericValue system = ctx.getSystemLogin();
            ShoppingCart cart = new ShoppingCart(delegator, ctx.getProductStoreId(), ctx.getWebSiteId(), locale(ctx, state), r.currency);
            cart.setOrderType("SALES_ORDER");
            cart.setUserLogin(system, dispatcher);
            cart.setChannelType("ACP_SALES_CHANNEL");
            r.cart = cart;

            // items
            List<Object> items = AcpJson.list(state.get("items"));
            Map<String, Map<String, Object>> failed = new LinkedHashMap<>();
            int i = 0;
            for (Object o : items) {
                Map<String, Object> it = AcpJson.map(o);
                String productId = AcpJson.str(it, "id");
                BigDecimal qty = new BigDecimal(AcpJson.str(it, "quantity"));
                String param = "$.line_items[" + i + "]";
                GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache().queryOne();
                if (product == null) {
                    r.messages.add(error("invalid", param, "Item " + productId + " does not exist.", "requires_buyer_input"));
                    r.blocking = true;
                } else if ("Y".equals(product.getString("isVirtual"))) {
                    r.messages.add(error("invalid", param, "Item " + productId + " has variants; send the id of one variant.", "requires_buyer_input"));
                    r.blocking = true;
                    failed.put(productId, UtilMisc.toMap("product", product, "quantity", qty));
                } else {
                    try {
                        cart.addOrIncreaseItem(productId, null, qty, null, null, null, null, null, null, null, null, null, null, null, null, dispatcher);
                    } catch (CartItemModifyException e) {
                        r.messages.add(error("out_of_stock", param, "Item " + productId + " is not available in this quantity.", "requires_buyer_input"));
                        r.blocking = true;
                        failed.put(productId, UtilMisc.toMap("product", product, "quantity", qty));
                    } catch (ItemNotFoundException e) {
                        r.messages.add(error("invalid", param, "Item " + productId + " cannot be sold.", "requires_buyer_input"));
                        r.blocking = true;
                    }
                }
                i++;
            }

            // address
            Map<String, Object> fd = AcpJson.map(state.get("fulfillment_details"));
            Map<String, Object> address = fd != null ? AcpJson.map(fd.get("address")) : null;
            if (address != null) {
                String missing = AcpParties.missingAddressField(address);
                if (missing != null) {
                    r.messages.add(error("missing", "$.fulfillment_details.address." + missing, "The address needs " + missing + ".", "requires_buyer_input"));
                    r.blocking = true;
                } else {
                    try {
                        r.shipContactMechId = AcpParties.ensureAddress(ctx, row, state, fd);
                    } catch (AcpException e) {
                        r.messages.add(error("invalid", e.getParam(), e.getMessage(), "requires_buyer_input"));
                        r.blocking = true;
                    }
                }
            }

            // fulfillment
            boolean shippable = cart.size() > 0 && cart.shippingApplies();
            if (r.shipContactMechId != null) {
                cart.setAllShippingContactMechId(r.shipContactMechId);
            }
            if (!shippable) {
                cart.setAllShipmentMethodTypeId("NO_SHIPPING");
                cart.setAllCarrierPartyId("_NA_");
                if (cart.size() > 0) {
                    Map<String, Object> opt = option("digital", "digital", "Digital delivery", null, 0L, r.currency);
                    r.fulfillmentOptions.add(opt);
                    r.selected.add(UtilMisc.toMap("type", "digital", "option_id", "digital", "item_ids", itemIds(cart)));
                }
            } else if (r.shipContactMechId == null) {
                if (address == null) {
                    r.messages.add(error("missing", "$.fulfillment_details.address", "A shipping address is required.", "requires_buyer_input"));
                    r.blocking = true;
                }
            } else {
                shippingOptions(ctx, cart, r);
                if (r.fulfillmentOptions.isEmpty()) {
                    r.messages.add(error("region_restricted", "$.fulfillment_details.address", "The store does not ship to this address.", "requires_buyer_input"));
                    r.blocking = true;
                } else {
                    Map<String, Object> chosen = chooseOption(state, r.fulfillmentOptions);
                    String[] key = ((String) chosen.get("id")).split("@", 2);
                    cart.setShipmentMethodTypeId(0, key[0]);
                    cart.setCarrierPartyId(0, key.length > 1 ? key[1] : "_NA_");
                    cart.setItemShipGroupEstimate(AcpJson.fromMinor(firstTotal(chosen), r.currency), 0);
                    r.selected.add(UtilMisc.toMap("type", "shipping", "option_id", chosen.get("id"), "item_ids", itemIds(cart)));
                }
            }

            // discounts
            r.discounts = discounts(ctx, cart, AcpJson.list(state.get("codes")), r);

            // tax
            if (r.shipContactMechId != null && cart.size() > 0) {
                try {
                    new CheckOutHelper(dispatcher, delegator, cart).calcAndAddTax();
                } catch (Exception e) {
                    Debug.logWarning("ACP: tax not calculated for checkout session " + row.getString("checkoutSessionId") + ": " + e.getMessage(), module);
                    r.messages.add(error("invalid", "$.totals", "Tax could not be calculated; try again later.", "recoverable"));
                    r.blocking = true;
                }
            }

            // buyer
            Map<String, Object> buyer = AcpJson.map(state.get("buyer"));
            String email = buyer != null ? AcpJson.str(buyer, "email") : null;
            if (UtilValidate.isEmpty(email)) {
                r.messages.add(error("missing", "$.buyer.email", "The buyer e-mail address is required.", "requires_buyer_input"));
                r.blocking = true;
            } else if (!UtilValidate.isEmail(email)) {
                r.messages.add(error("invalid", "$.buyer.email", "The buyer e-mail address is not valid.", "requires_buyer_input"));
                r.blocking = true;
            }
            if (cart.size() == 0) {
                r.blocking = true;
            }

            renderItems(ctx, cart, failed, r);
            renderTotals(cart, r);
            r.status = r.blocking ? "not_ready_for_payment" : "ready_for_payment";
            return r;
        } catch (GenericEntityException | CartItemModifyException e) {
            Debug.logError(e, "ACP: pricing failed for checkout session " + row.getString("checkoutSessionId") + ": " + e.getMessage(), module);
            throw AcpException.processing("The checkout session could not be priced.");
        }
    }

    // ---------------------------------------------------------------- shipping

    private static void shippingOptions(AcpContext ctx, ShoppingCart cart, Result r) throws GenericEntityException {
        List<GenericValue> methods = EntityQuery.use(ctx.getDelegator()).from("ProductStoreShipmentMeth")
                .where("productStoreId", ctx.getProductStoreId()).orderBy("sequenceNumber").cache().queryList();
        Set<String> seen = new LinkedHashSet<>();
        for (GenericValue m : methods) {
            String type = m.getString("shipmentMethodTypeId");
            String carrier = UtilValidate.isNotEmpty(m.getString("partyId")) ? m.getString("partyId") : "_NA_";
            if ("NO_SHIPPING".equals(type) || !seen.add(type + "@" + carrier)) continue;
            cart.setShipmentMethodTypeId(0, type);
            cart.setCarrierPartyId(0, carrier);
            cart.setProductStoreShipMethId(0, m.getString("productStoreShipMethId"));
            Map<String, Object> est;
            try {
                est = ShippingEvents.getShipGroupEstimate(ctx.getDispatcher(), ctx.getDelegator(), cart, 0);
            } catch (RuntimeException e) {
                continue;
            }
            if (est == null || ServiceUtil.isError(est) || est.get("shippingTotal") == null) continue;
            BigDecimal amount = (BigDecimal) est.get("shippingTotal");
            GenericValue mt = EntityQuery.use(ctx.getDelegator()).from("ShipmentMethodType").where("shipmentMethodTypeId", type).cache().queryOne();
            String title = mt != null && UtilValidate.isNotEmpty(mt.getString("description")) ? mt.getString("description") : type;
            String carrierName = "_NA_".equals(carrier) ? null : carrier;
            r.fulfillmentOptions.add(option("shipping", type + "@" + carrier, title, carrierName, AcpJson.toMinor(amount, r.currency), r.currency));
        }
        cart.setProductStoreShipMethId(0, null);
    }

    private static Map<String, Object> chooseOption(Map<String, Object> state, List<Map<String, Object>> options) {
        for (Object o : AcpJson.list(state.get("selected"))) {
            String id = AcpJson.str(AcpJson.map(o), "option_id");
            for (Map<String, Object> opt : options) {
                if (opt.get("id").equals(id)) return opt;
            }
        }
        Map<String, Object> best = options.get(0);
        for (Map<String, Object> opt : options) {
            if (firstTotal(opt) < firstTotal(best)) best = opt;
        }
        return best;
    }

    private static long firstTotal(Map<String, Object> opt) {
        for (Object t : AcpJson.list(opt.get("totals"))) {
            Map<String, Object> m = AcpJson.map(t);
            if ("total".equals(m.get("type"))) return ((Number) m.get("amount")).longValue();
        }
        return 0L;
    }

    static Map<String, Object> option(String type, String id, String title, String carrier, long amount, String currency) {
        Map<String, Object> o = new LinkedHashMap<>();
        o.put("type", type);
        o.put("id", id);
        o.put("title", title);
        if (carrier != null) o.put("carrier", carrier);
        List<Map<String, Object>> totals = new ArrayList<>();
        totals.add(total("fulfillment", "Shipping", amount));
        totals.add(total("total", "Total", amount));
        o.put("totals", totals);
        return o;
    }

    private static List<String> itemIds(ShoppingCart cart) {
        List<String> ids = new ArrayList<>();
        for (ShoppingCartItem item : cart.items()) {
            ids.add(lineId(item.getProductId()));
        }
        return ids;
    }

    // ---------------------------------------------------------------- discounts

    private static Map<String, Object> discounts(AcpContext ctx, ShoppingCart cart, List<Object> codes, Result r) throws GenericEntityException {
        Delegator delegator = ctx.getDelegator();
        List<String> accepted = new ArrayList<>();
        List<Map<String, Object>> rejected = new ArrayList<>();
        for (Object c : codes) {
            String code = c.toString();
            String err = cart.size() > 0 ? cart.addProductPromoCode(code, ctx.getDispatcher()) : "The cart is empty.";
            if (err != null) {
                rejected.add(UtilMisc.toMap("code", code, "reason", "discount_code_invalid", "message", err));
                Map<String, Object> w = new LinkedHashMap<>();
                w.put("type", "warning");
                w.put("code", "discount_code_invalid");
                w.put("param", "$.discounts.codes");
                w.put("content_type", "plain");
                w.put("content", "Code " + code + ": " + err);
                r.messages.add(w);
            } else {
                accepted.add(code);
            }
        }
        if (cart.size() > 0) {
            ProductPromoWorker.doPromotions(cart, ctx.getDispatcher());
        }
        // the applied promotions, order and item level
        Map<String, BigDecimal> byPromo = new LinkedHashMap<>();
        for (GenericValue adj : cart.getAdjustments()) {
            addPromo(byPromo, adj);
        }
        for (ShoppingCartItem item : cart.items()) {
            for (GenericValue adj : item.getAdjustments()) {
                addPromo(byPromo, adj);
            }
        }
        List<Map<String, Object>> applied = new ArrayList<>();
        for (Map.Entry<String, BigDecimal> e : byPromo.entrySet()) {
            GenericValue promo = EntityQuery.use(delegator).from("ProductPromo").where("productPromoId", e.getKey()).cache().queryOne();
            String name = promo != null && UtilValidate.isNotEmpty(promo.getString("promoName")) ? promo.getString("promoName") : e.getKey();
            String code = null;
            for (String c : accepted) {
                GenericValue pc = EntityQuery.use(delegator).from("ProductPromoCode").where("productPromoCodeId", c).cache().queryOne();
                if (pc != null && e.getKey().equals(pc.getString("productPromoId"))) code = c;
            }
            Map<String, Object> a = new LinkedHashMap<>();
            a.put("id", e.getKey());
            if (code != null) a.put("code", code);
            a.put("coupon", UtilMisc.toMap("id", e.getKey(), "name", name));
            a.put("amount", AcpJson.toMinor(e.getValue().negate(), r.currency));
            a.put("automatic", code == null);
            applied.add(a);
        }
        Map<String, Object> d = new LinkedHashMap<>();
        d.put("codes", accepted);
        d.put("applied", applied);
        d.put("rejected", rejected);
        return d;
    }

    private static void addPromo(Map<String, BigDecimal> byPromo, GenericValue adj) {
        if ("PROMOTION_ADJUSTMENT".equals(adj.getString("orderAdjustmentTypeId")) && adj.getString("productPromoId") != null
                && adj.getBigDecimal("amount") != null) {
            byPromo.merge(adj.getString("productPromoId"), adj.getBigDecimal("amount"), BigDecimal::add);
        }
    }

    // ---------------------------------------------------------------- rendering

    private static void renderItems(AcpContext ctx, ShoppingCart cart, Map<String, Map<String, Object>> failed, Result r) {
        String cur = r.currency;
        for (ShoppingCartItem item : cart.items()) {
            BigDecimal base = item.getBasePrice() != null ? item.getBasePrice() : BigDecimal.ZERO;
            BigDecimal qty = item.getQuantity();
            BigDecimal baseAmount = base.multiply(qty);
            BigDecimal promo = BigDecimal.ZERO;
            BigDecimal tax = BigDecimal.ZERO;
            for (GenericValue adj : item.getAdjustments()) {
                BigDecimal amt = adj.getBigDecimal("amount") != null ? adj.getBigDecimal("amount") : BigDecimal.ZERO;
                if ("PROMOTION_ADJUSTMENT".equals(adj.getString("orderAdjustmentTypeId"))) promo = promo.add(amt);
                if (TAX_TYPES.contains(adj.getString("orderAdjustmentTypeId"))) tax = tax.add(amt);
            }
            BigDecimal sub = item.getItemSubTotal();
            Map<String, Object> li = lineItem(ctx, item.getProduct(), item.getName(), qty, base, cur);
            li.put("availability_status", "in_stock");
            List<Map<String, Object>> t = new ArrayList<>();
            t.add(total("items_base_amount", "Base Amount", AcpJson.toMinor(baseAmount, cur)));
            t.add(total("items_discount", "Discount", AcpJson.toMinor(promo.negate().max(BigDecimal.ZERO), cur)));
            t.add(total("subtotal", "Subtotal", AcpJson.toMinor(sub, cur)));
            t.add(total("tax", "Tax", AcpJson.toMinor(tax, cur)));
            t.add(total("total", "Total", AcpJson.toMinor(sub.add(tax), cur)));
            li.put("totals", t);
            r.lineItems.add(li);
        }
        for (Map<String, Object> f : failed.values()) {
            GenericValue product = (GenericValue) f.get("product");
            Map<String, Object> li = lineItem(ctx, product, product.getString("productName"), (BigDecimal) f.get("quantity"), null, cur);
            li.put("availability_status", "out_of_stock");
            List<Map<String, Object>> t = new ArrayList<>();
            t.add(total("subtotal", "Subtotal", 0L));
            t.add(total("total", "Total", 0L));
            li.put("totals", t);
            r.lineItems.add(li);
        }
    }

    private static Map<String, Object> lineItem(AcpContext ctx, GenericValue product, String name, BigDecimal qty, BigDecimal unit, String cur) {
        String productId = product.getString("productId");
        Map<String, Object> li = new LinkedHashMap<>();
        li.put("id", lineId(productId));
        Map<String, Object> item = new LinkedHashMap<>();
        item.put("id", productId);
        if (unit != null) item.put("unit_amount", AcpJson.toMinor(unit, cur));
        li.put("item", item);
        li.put("quantity", qty.intValue());
        li.put("name", UtilValidate.isNotEmpty(name) ? name : productId);
        String desc = product.getString("description");
        if (UtilValidate.isNotEmpty(desc)) li.put("description", desc);
        String img = UtilValidate.isNotEmpty(product.getString("largeImageUrl")) ? product.getString("largeImageUrl") : product.getString("mediumImageUrl");
        if (UtilValidate.isNotEmpty(img)) li.put("images", List.of(absoluteImage(ctx, img)));
        if (unit != null) li.put("unit_amount", AcpJson.toMinor(unit, cur));
        li.put("product_id", productId);
        li.put("sku", productId);
        if ("Y".equals(product.getString("isVariant"))) {
            try {
                GenericValue assoc = EntityQuery.use(ctx.getDelegator()).from("ProductAssoc")
                        .where("productIdTo", productId, "productAssocTypeId", "PRODUCT_VARIANT").filterByDate().cache().queryFirst();
                if (assoc != null) li.put("parent_id", assoc.getString("productId"));
            } catch (GenericEntityException e) {
                Debug.logWarning("ACP: no parent of variant " + productId + ": " + e.getMessage(), module);
            }
        }
        return li;
    }

    static String absoluteImage(AcpContext ctx, String img) {
        if (img.startsWith("http://") || img.startsWith("https://")) return img;
        String base = ctx.getBaseUrl();
        int slash = base.indexOf('/', base.indexOf("//") + 2);
        String host = slash > 0 ? base.substring(0, slash) : base;
        return host + (img.startsWith("/") ? img : "/" + img);
    }

    private static void renderTotals(ShoppingCart cart, Result r) {
        String cur = r.currency;
        BigDecimal base = BigDecimal.ZERO;
        BigDecimal itemPromo = BigDecimal.ZERO;
        for (ShoppingCartItem item : cart.items()) {
            base = base.add((item.getBasePrice() != null ? item.getBasePrice() : BigDecimal.ZERO).multiply(item.getQuantity()));
            for (GenericValue adj : item.getAdjustments()) {
                if ("PROMOTION_ADJUSTMENT".equals(adj.getString("orderAdjustmentTypeId")) && adj.getBigDecimal("amount") != null) {
                    itemPromo = itemPromo.add(adj.getBigDecimal("amount"));
                }
            }
        }
        BigDecimal orderPromo = BigDecimal.ZERO;
        for (GenericValue adj : cart.getAdjustments()) {
            if ("PROMOTION_ADJUSTMENT".equals(adj.getString("orderAdjustmentTypeId")) && adj.getBigDecimal("amount") != null) {
                orderPromo = orderPromo.add(adj.getBigDecimal("amount"));
            }
        }
        BigDecimal grand = cart.size() > 0 ? cart.getGrandTotal() : BigDecimal.ZERO;
        r.total = grand;
        r.totalMinor = AcpJson.toMinor(grand, cur);
        r.totals.add(total("items_base_amount", "Base Amount", AcpJson.toMinor(base, cur)));
        r.totals.add(total("items_discount", "Item Discounts", AcpJson.toMinor(itemPromo.negate().max(BigDecimal.ZERO), cur)));
        r.totals.add(total("subtotal", "Subtotal", AcpJson.toMinor(cart.getSubTotal(), cur)));
        r.totals.add(total("discount", "Discount", AcpJson.toMinor(orderPromo.negate().max(BigDecimal.ZERO), cur)));
        r.totals.add(total("fulfillment", "Shipping", AcpJson.toMinor(cart.getTotalShipping(), cur)));
        r.totals.add(total("tax", "Tax", AcpJson.toMinor(cart.getTotalSalesTax(), cur)));
        r.totals.add(total("total", "Total", r.totalMinor));
    }

    /** The CheckoutSession JSON. */
    static Map<String, Object> toSession(AcpContext ctx, GenericValue row, Map<String, Object> state, Result r) {
        Map<String, Object> s = new LinkedHashMap<>();
        s.put("id", row.getString("checkoutSessionId"));
        s.put("protocol", UtilMisc.toMap("version", AcpConfig.apiVersion(ctx.getDelegator())));
        s.put("capabilities", capabilities(ctx));
        Map<String, Object> buyer = AcpJson.map(state.get("buyer"));
        if (buyer != null) s.put("buyer", buyer);
        String status = row.getString("status");
        s.put("status", UtilValidate.isNotEmpty(status) && (AcpCheckout.isFinal(row) || AcpCheckout.IN_PROGRESS.equals(status)) ? status : r.status);
        s.put("currency", r.currency.toLowerCase(Locale.ROOT));
        if (state.get("locale") != null) s.put("locale", state.get("locale"));
        s.put("line_items", r.lineItems);
        if (state.get("fulfillment_details") != null) s.put("fulfillment_details", state.get("fulfillment_details"));
        s.put("fulfillment_options", r.fulfillmentOptions);
        s.put("selected_fulfillment_options", r.selected);
        s.put("totals", r.totals);
        s.put("messages", r.messages);
        s.put("links", links(ctx));
        if (r.discounts != null) s.put("discounts", r.discounts);
        s.put("created_at", AcpCheckout.iso(row.getTimestamp("createdDate")));
        s.put("updated_at", AcpCheckout.iso(row.getTimestamp("lastUpdatedStamp") != null ? row.getTimestamp("lastUpdatedStamp") : UtilDateTime.nowTimestamp()));
        s.put("expires_at", AcpCheckout.iso(row.getTimestamp("expiresDate")));
        s.put("continue_url", ctx.url("/control/main"));
        return s;
    }

    static Map<String, Object> capabilities(AcpContext ctx) {
        Map<String, Object> c = new LinkedHashMap<>();
        AcpPaymentHandler handler = AcpPaymentHandler.forStore(ctx);
        List<Map<String, Object>> handlers = new ArrayList<>();
        if (handler != null) handlers.add(AcpPaymentHandler.describe(ctx, handler));
        c.put("payment", UtilMisc.toMap("handlers", handlers));
        Map<String, Object> interventions = new LinkedHashMap<>();
        interventions.put("supported", List.of());
        interventions.put("required", List.of());
        interventions.put("enforcement", "optional");
        c.put("interventions", interventions);
        c.put("extensions", List.of("discount"));
        return c;
    }

    static List<Map<String, Object>> links(AcpContext ctx) {
        List<Map<String, Object>> links = new ArrayList<>();
        for (String type : List.of("terms_of_use", "privacy_policy", "return_policy", "shipping_policy")) {
            String url = ctx.url(AcpConfig.get(ctx.getDelegator(), ctx.getProductStoreId(), "link." + type, null));
            if (url != null) links.add(UtilMisc.toMap("type", type, "url", url));
        }
        return links;
    }

    static Map<String, Object> total(String type, String text, long amount) {
        Map<String, Object> t = new LinkedHashMap<>();
        t.put("type", type);
        t.put("display_text", text);
        t.put("amount", amount);
        return t;
    }

    static Map<String, Object> error(String code, String param, String content, String resolution) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("type", "error");
        m.put("code", code);
        if (param != null) m.put("param", param);
        m.put("severity", "high");
        if (resolution != null) m.put("resolution", resolution);
        m.put("content_type", "plain");
        m.put("content", content);
        return m;
    }

    static Map<String, Object> info(String param, String content) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("type", "info");
        if (param != null) m.put("param", param);
        m.put("content_type", "plain");
        m.put("content", content);
        return m;
    }

    static String lineId(String productId) {
        return "li_" + productId;
    }

    private static Locale locale(AcpContext ctx, Map<String, Object> state) {
        String l = AcpJson.str(state, "locale");
        return UtilValidate.isNotEmpty(l) ? Locale.forLanguageTag(l.replace('_', '-')) : ctx.getLocale();
    }
}

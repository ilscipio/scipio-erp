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
package com.ilscipio.scipio.channel.store;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.order.shoppingcart.CheckOutHelper;
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.order.shoppingcart.ShoppingCartItem;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.channel.core.ChannelSetting;
import com.ilscipio.scipio.channel.core.IncomingOrder;
import com.ilscipio.scipio.channel.core.OrderIntake;

/**
 * Makes the store order of a channel order through the order services (blueprint 7.3: "channel orders through the order
 * services with the external id"). Steps: a buyer party with the ship-to address and e-mail (one party for each channel
 * order, so that erasing the buyer data of one order touches no other order), a cart of the store of the channel with the
 * channel price and the external id, the channel tax as an adjustment, the order, the channel attributes, and the approval
 * of a paid order.
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08). Not covered by a unit test: it needs a running store database (open point in
 * docs/wp/W1-08.md).</p>
 */
public final class EntityOrderCreator implements OrderIntake.OrderCreator {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final Delegator delegator;
    private final LocalDispatcher dispatcher;
    private final GenericValue userLogin;

    public EntityOrderCreator(Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin) {
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.userLogin = userLogin;
    }

    @Override
    public String create(ChannelSetting setting, IncomingOrder order, java.util.List<OrderIntake.ResolvedLine> lines)
            throws Exception {
        String partyId = createBuyer(order);

        ShoppingCart cart = new ShoppingCart(delegator, setting.productStoreId, null, Locale.ENGLISH, setting.currencyUomId);
        cart.setOrderType("SALES_ORDER");
        cart.setUserLogin(userLogin, dispatcher);
        cart.setOrderPartyId(partyId);
        cart.setExternalId(order.externalOrderId);
        cart.setChannelType(UtilValidate.isNotEmpty(setting.salesChannelEnumId) ? setting.salesChannelEnumId : "UNKNWN_SALES_CHANNEL");
        cart.setOrderDate(Timestamp.from(order.placedAt));
        cart.setOrderName(setting.channelId + " " + order.externalOrderId);
        cart.setOrderAttribute("channelId", setting.channelId);
        cart.setOrderAttribute("channelTaxCollected", order.taxCollectedByChannel ? "Y" : "N");
        cart.setOrderAttribute("channelPricesIncludeTax", setting.pricesIncludeTax ? "Y" : "N");
        if (UtilValidate.isNotEmpty(order.buyerNote)) {
            cart.addOrderNote(order.buyerNote);
        }

        for (OrderIntake.ResolvedLine rl : lines) {
            // each line is its own item (two lines of one SKU can have two prices); the channel already sold the goods, so no inventory check
            int idx = cart.addItemToEnd(rl.productId, null, BigDecimal.valueOf(rl.line.quantity), rl.line.unitPrice, null, null,
                    null, null, null, null, null, dispatcher, Boolean.FALSE, Boolean.FALSE, Boolean.TRUE, Boolean.FALSE);
            ShoppingCartItem item = cart.findCartItem(idx);
            // the price of the channel order wins over the catalog price
            item.setBasePrice(rl.line.unitPrice);
            item.setDisplayPrice(rl.line.unitPrice);
            item.setIsModifiedPrice(true);
        }

        // prices without tax: the tax that the channel collected is an adjustment. Prices with tax: the tax is inside.
        if (order.taxCollectedByChannel && !setting.pricesIncludeTax && order.tax.signum() > 0) {
            cart.addAdjustment(adjustment("SALES_TAX", order.tax, "Tax collected by " + setting.channelId));
        }
        if (order.shipping.signum() > 0) {
            cart.addAdjustment(adjustment("SHIPPING_CHARGES", order.shipping, "Shipping paid to " + setting.channelId));
        }

        String contactMechId = createAddress(partyId, order);
        if (contactMechId != null) {
            cart.setAllShippingContactMechId(contactMechId);
        }
        cart.setAllShipmentMethodTypeId("NO_SHIPPING");
        cart.setAllCarrierPartyId("_NA_");
        cart.addPayment("EXT_OFFLINE");

        CheckOutHelper helper = new CheckOutHelper(dispatcher, delegator, cart);
        Map<String, Object> res = helper.createOrder(userLogin, null, null, null, false, null, null);
        if (ServiceUtil.isError(res)) {
            throw new IllegalStateException(ServiceUtil.getErrorMessage(res));
        }
        String orderId = (String) res.get("orderId");
        if ("PAID".equals(order.status) || "SHIPPED".equals(order.status) || "PARTLY_SHIPPED".equals(order.status)) {
            Map<String, Object> st = dispatcher.runSync("changeOrderStatus", UtilMisc.<String, Object>toMap("orderId", orderId,
                    "statusId", "ORDER_APPROVED", "userLogin", userLogin));
            if (ServiceUtil.isError(st)) {
                Debug.logWarning("Channel order " + orderId + " stays created: " + ServiceUtil.getErrorMessage(st), module);
            }
        }
        return orderId;
    }

    @Override
    public void cancel(String orderId) throws Exception {
        Map<String, Object> st = dispatcher.runSync("changeOrderStatus", UtilMisc.<String, Object>toMap("orderId", orderId,
                "statusId", "ORDER_CANCELLED", "userLogin", userLogin));
        if (ServiceUtil.isError(st)) {
            throw new IllegalStateException(ServiceUtil.getErrorMessage(st));
        }
    }

    private GenericValue adjustment(String type, BigDecimal amount, String description) {
        GenericValue adj = delegator.makeValue("OrderAdjustment");
        adj.set("orderAdjustmentTypeId", type);
        adj.set("amount", amount);
        adj.set("description", description);
        return adj;
    }

    private String createBuyer(IncomingOrder order) throws GenericServiceException {
        String name = UtilValidate.isNotEmpty(order.buyerName) ? order.buyerName.trim() : "Channel buyer";
        int sp = name.lastIndexOf(' ');
        String first = sp > 0 ? name.substring(0, sp) : name;
        String last = sp > 0 ? name.substring(sp + 1) : "-";
        Map<String, Object> res = dispatcher.runSync("createPerson", UtilMisc.<String, Object>toMap("firstName", first,
                "lastName", last, "userLogin", userLogin));
        if (ServiceUtil.isError(res)) {
            throw new GenericServiceException(ServiceUtil.getErrorMessage(res));
        }
        String partyId = (String) res.get("partyId");
        Map<String, Object> role = dispatcher.runSync("createPartyRole", UtilMisc.<String, Object>toMap("partyId", partyId,
                "roleTypeId", "CUSTOMER", "userLogin", userLogin));
        if (ServiceUtil.isError(role)) {
            throw new GenericServiceException(ServiceUtil.getErrorMessage(role));
        }
        if (UtilValidate.isNotEmpty(order.buyerEmail)) {
            Map<String, Object> mail = dispatcher.runSync("createPartyEmailAddress", UtilMisc.<String, Object>toMap(
                    "partyId", partyId, "emailAddress", order.buyerEmail, "contactMechPurposeTypeId", "PRIMARY_EMAIL",
                    "userLogin", userLogin));
            if (ServiceUtil.isError(mail)) {
                Debug.logWarning("Channel buyer has no e-mail on the party: " + ServiceUtil.getErrorMessage(mail), module);
            }
        }
        return partyId;
    }

    private String createAddress(String partyId, IncomingOrder order) throws GenericServiceException, GenericEntityException {
        Map<String, String> a = order.shipTo;
        if (a.isEmpty() || UtilValidate.isEmpty(a.get("line1"))) {
            return null;
        }
        String countryGeoId = null;
        if (UtilValidate.isNotEmpty(a.get("countryCode"))) {
            GenericValue geo = EntityQuery.use(delegator).from("Geo").where("geoTypeId", "COUNTRY", "geoCode", a.get("countryCode"))
                    .cache().queryFirst();
            countryGeoId = geo != null ? geo.getString("geoId") : null;
        }
        Map<String, Object> in = UtilMisc.<String, Object>toMap("partyId", partyId, "toName", a.get("name"),
                "address1", a.get("line1"), "address2", a.get("line2"), "city", a.get("city"),
                "postalCode", UtilValidate.isNotEmpty(a.get("postalCode")) ? a.get("postalCode") : "-",
                "countryGeoId", countryGeoId, "contactMechPurposeTypeId", "SHIPPING_LOCATION", "userLogin", userLogin);
        Map<String, Object> res = dispatcher.runSync("createPartyPostalAddress", in);
        if (ServiceUtil.isError(res)) {
            throw new GenericServiceException(ServiceUtil.getErrorMessage(res));
        }
        return (String) res.get("contactMechId");
    }
}

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

import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;
import java.util.regex.Pattern;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.Service;

/**
 * Service sendOrderMessage: an email to the customer of an order. The mail goes through sendMail; the SECA chain of
 * sendMail stores it as a CommunicationEvent that is linked to the order (orderId) and to the customer (partyId).
 * The MCP action is order/order message_send (W1-08c).
 *
 * <p>SCIPIO: 4.0.0: Added (W1-08c).</p>
 */
public class OrderMessageServices {
    public static final String ERR_CHANNEL = "channel_message_not_supported";
    public static final String ERR_NO_RECIPIENT = "no_customer_email";
    public static final String ERR_NO_SENDER = "no_store_sender_address";
    private static final Pattern EMAIL = Pattern.compile("[^\\s@]+@[^\\s@]+\\.[^\\s@]+");

    @Service(
        name = "sendOrderMessage",
        engine = "java",
        location = "com.ilscipio.scipio.order.service.OrderMessageServices",
        invoke = "sendOrderMessage",
        description = "Sends an email to the customer of a sales order. The email is stored as a CommunicationEvent linked to the order. "
                + "Needs the permission ORDERMGR_UPDATE. Orders from a marketplace channel are refused with channel_message_not_supported.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "subject", type = "String", mode = "IN", optional = "false", allowHtml = "any"),
            @Attribute(name = "body", type = "String", mode = "IN", optional = "false", allowHtml = "any"),
            @Attribute(name = "communicationEventId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "errorCode", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SendOrderMessage {}

    /** True when the sales channel id names a marketplace (eBay, Amazon). Pure, for tests. */
    public static boolean isMarketplaceChannelId(String salesChannelEnumId) {
        if (salesChannelEnumId == null) {
            return false;
        }
        String s = salesChannelEnumId.toUpperCase(Locale.ROOT);
        return s.contains("EBAY") || s.contains("AMAZON") || s.contains("AMZN");
    }

    public static boolean isEmail(String s) {
        return s != null && EMAIL.matcher(s.trim()).matches();
    }

    private static Map<String, Object> fail(String code, String text) {
        Map<String, Object> r = ServiceUtil.returnError(code + ": " + text);
        r.put("errorCode", code);
        return r;
    }

    public static Map<String, Object> sendOrderMessage(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (!dctx.getSecurity().hasEntityPermission("ORDERMGR", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError("Permission ORDERMGR_UPDATE is required.");
        }
        String orderId = (String) context.get("orderId");
        String subject = (String) context.get("subject");
        String body = (String) context.get("body");
        if (UtilValidate.isEmpty(subject) || UtilValidate.isEmpty(body)) {
            return ServiceUtil.returnError("subject and body are required.");
        }
        try {
            GenericValue order = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
            if (order == null) {
                return ServiceUtil.returnError("Order not found: " + orderId);
            }
            if (isMarketplaceChannelId(order.getString("salesChannelEnumId")) || hasChannelRef(delegator, orderId)) {
                return fail(ERR_CHANNEL, "order " + orderId + " comes from a marketplace channel; a message goes through the marketplace.");
            }
            String partyId = customerPartyId(delegator, orderId);
            String sendTo = customerEmail(delegator, orderId, partyId);
            if (!isEmail(sendTo)) {
                return fail(ERR_NO_RECIPIENT, "order " + orderId + " has no customer email address.");
            }
            String sendFrom = storeSender(delegator, order.getString("productStoreId"));
            if (!isEmail(sendFrom)) {
                return fail(ERR_NO_SENDER, "product store " + order.getString("productStoreId") + " has no sender address (ProductStoreEmailSetting.fromAddress).");
            }
            Map<String, Object> mail = new LinkedHashMap<>();
            mail.put("sendTo", sendTo.trim());
            mail.put("sendFrom", sendFrom.trim());
            mail.put("subject", subject);
            mail.put("body", body);
            mail.put("contentType", "text/plain");
            mail.put("orderId", orderId);
            if (partyId != null) {
                mail.put("partyId", partyId);
            }
            mail.put("userLogin", userLogin);
            mail.put("locale", context.get("locale"));
            Map<String, Object> res = dispatcher.runSync("sendMail", mail);
            if (ServiceUtil.isError(res)) {
                return ServiceUtil.returnError("Mail failed: " + ServiceUtil.getErrorMessage(res));
            }
            String commId = (String) res.get("communicationEventId");
            if (commId == null) {
                return ServiceUtil.returnError("The mail went out, but no CommunicationEvent was stored (the customer has no party id?).");
            }
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.put("communicationEventId", commId);
            return out;
        } catch (GenericEntityException | GenericServiceException e) {
            return ServiceUtil.returnError("Message failed: " + e.getMessage());
        }
    }

    private static boolean hasChannelRef(Delegator delegator, String orderId) throws GenericEntityException {
        if (delegator.getModelReader().getModelEntityNoCheck("ChannelOrderRef") == null) {
            return false;
        }
        return EntityQuery.use(delegator).from("ChannelOrderRef").where("orderId", orderId).queryCount() > 0;
    }

    private static String customerPartyId(Delegator delegator, String orderId) throws GenericEntityException {
        for (String role : new String[] {"PLACING_CUSTOMER", "BILL_TO_CUSTOMER", "END_USER_CUSTOMER"}) {
            GenericValue r = EntityQuery.use(delegator).from("OrderRole").where("orderId", orderId, "roleTypeId", role).queryFirst();
            if (r != null) {
                return r.getString("partyId");
            }
        }
        return null;
    }

    private static String customerEmail(Delegator delegator, String orderId, String partyId) throws GenericEntityException {
        GenericValue ocm = EntityQuery.use(delegator).from("OrderContactMech").where("orderId", orderId, "contactMechPurposeTypeId", "ORDER_EMAIL").queryFirst();
        if (ocm != null) {
            GenericValue cm = EntityQuery.use(delegator).from("ContactMech").where("contactMechId", ocm.getString("contactMechId")).queryOne();
            if (cm != null && UtilValidate.isNotEmpty(cm.getString("infoString"))) {
                return cm.getString("infoString");
            }
        }
        if (partyId != null) {
            GenericValue pcm = EntityQuery.use(delegator).from("PartyContactMechPurpose")
                    .where("partyId", partyId, "contactMechPurposeTypeId", "PRIMARY_EMAIL").filterByDate().queryFirst();
            if (pcm != null) {
                GenericValue cm = EntityQuery.use(delegator).from("ContactMech").where("contactMechId", pcm.getString("contactMechId")).queryOne();
                if (cm != null) {
                    return cm.getString("infoString");
                }
            }
        }
        return null;
    }

    /** Email types read for the sender address, in order (PRDS_ODR_CONFIRM is the real type id of the order confirmation). */
    public static final String[] STORE_SENDER_EMAIL_TYPES = {"PRDS_ODR_CHANGE", "PRDS_ODR_CONFIRM"};

    private static String storeSender(Delegator delegator, String productStoreId) throws GenericEntityException {
        if (productStoreId == null) {
            return null;
        }
        for (String type : STORE_SENDER_EMAIL_TYPES) {
            GenericValue s = EntityQuery.use(delegator).from("ProductStoreEmailSetting")
                    .where("productStoreId", productStoreId, "emailType", type).queryOne();
            if (s != null && UtilValidate.isNotEmpty(s.getString("fromAddress"))) {
                return s.getString("fromAddress");
            }
        }
        GenericValue any = EntityQuery.use(delegator).from("ProductStoreEmailSetting").where("productStoreId", productStoreId).queryFirst();
        return any != null ? any.getString("fromAddress") : null;
    }
}

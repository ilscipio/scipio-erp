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
package com.ilscipio.scipio.compliance.service;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.compliance.LegalDocumentWorker;

/**
 * EU withdrawal function (Directive 2011/83/EU Art. 11a as amended by Directive (EU) 2023/2673): a consumer
 * withdraws from a distance contract online in two steps; the trader confirms receipt, with the content and the
 * time, on a durable medium (the e-mail).
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class WithdrawalServiceImpl {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String BODY_SCREEN = "component://compliance/widget/ComplianceEmailScreens.xml#WithdrawalConfirmationEmail";

    private WithdrawalServiceImpl() {}

    /**
     * Checks an order for a withdrawal: the order exists in the store, and the e-mail address (or the logged-in
     * customer) matches it. OUT: orderHeader, items (list of maps: orderItemSeqId, productId, description,
     * quantity, unitPrice).
     */
    public static Map<String, Object> checkWithdrawalOrder(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        String orderId = UtilValidate.isNotEmpty((String) context.get("orderId")) ? ((String) context.get("orderId")).trim() : null;
        String email = (String) context.get("emailAddress");
        String partyId = (String) context.get("partyId");
        String productStoreId = (String) context.get("productStoreId");
        try {
            GenericValue order = orderId != null ? EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne() : null;
            boolean ok = order != null && "SALES_ORDER".equals(order.getString("orderTypeId"))
                    && (UtilValidate.isEmpty(productStoreId) || productStoreId.equals(order.getString("productStoreId")));
            if (ok) {
                boolean owner = UtilValidate.isNotEmpty(partyId) && EntityQuery.use(delegator).from("OrderRole")
                        .where("orderId", orderId, "partyId", partyId, "roleTypeId", "PLACING_CUSTOMER").queryCount() > 0;
                ok = owner || emailMatches(delegator, order, email);
            }
            if (!ok) {
                // one answer for all cases: do not reveal which orders exist (success, so no rollback and no error log)
                Map<String, Object> result = ServiceUtil.returnSuccess();
                result.put("matched", Boolean.FALSE);
                return result;
            }
            List<Map<String, Object>> items = new ArrayList<>();
            for (GenericValue oi : EntityQuery.use(delegator).from("OrderItem").where("orderId", orderId).orderBy("orderItemSeqId").queryList()) {
                if ("ITEM_CANCELLED".equals(oi.getString("statusId")) || "ITEM_REJECTED".equals(oi.getString("statusId"))) {
                    continue;
                }
                BigDecimal qty = oi.getBigDecimal("quantity") != null ? oi.getBigDecimal("quantity") : BigDecimal.ZERO;
                if (oi.getBigDecimal("cancelQuantity") != null) {
                    qty = qty.subtract(oi.getBigDecimal("cancelQuantity"));
                }
                if (qty.signum() <= 0) {
                    continue;
                }
                Map<String, Object> m = new LinkedHashMap<>();
                m.put("orderItemSeqId", oi.getString("orderItemSeqId"));
                m.put("productId", oi.getString("productId"));
                m.put("description", UtilValidate.isNotEmpty(oi.getString("itemDescription")) ? oi.getString("itemDescription") : oi.getString("productId"));
                m.put("quantity", qty);
                m.put("unitPrice", oi.getBigDecimal("unitPrice"));
                m.put("statusId", oi.getString("statusId"));
                items.add(m);
            }
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("matched", Boolean.TRUE);
            result.put("orderHeader", order);
            result.put("items", items);
            return result;
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    private static boolean emailMatches(Delegator delegator, GenericValue order, String email) throws GenericEntityException {
        if (UtilValidate.isEmpty(email)) {
            return false;
        }
        String e = email.trim();
        for (GenericValue ocm : EntityQuery.use(delegator).from("OrderContactMech").where("orderId", order.getString("orderId"), "contactMechPurposeTypeId", "ORDER_EMAIL").queryList()) {
            GenericValue cm = EntityQuery.use(delegator).from("ContactMech").where("contactMechId", ocm.getString("contactMechId")).queryOne();
            if (cm != null && e.equalsIgnoreCase(cm.getString("infoString"))) {
                return true;
            }
        }
        return false;
    }

    /**
     * Records a withdrawal: a customer return with reason RTN_WITHDRAWAL for the chosen items (all when none are
     * given), and the confirmation e-mail with the statement, the items and the time of receipt. Items that cannot
     * be returned yet (not shipped) stay in the confirmation for the merchant to cancel.
     * OUT: returnId, receivedDate, withdrawnItems, pendingItems, emailSent.
     */
    public static Map<String, Object> createWithdrawal(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        Map<String, Object> check = checkWithdrawalOrder(dctx, context);
        if (ServiceUtil.isError(check)) {
            return check;
        }
        if (!Boolean.TRUE.equals(check.get("matched"))) {
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("matched", Boolean.FALSE);
            return result;
        }
        GenericValue order = (GenericValue) check.get("orderHeader");
        @SuppressWarnings("unchecked")
        List<Map<String, Object>> items = (List<Map<String, Object>>) check.get("items");
        @SuppressWarnings("unchecked")
        List<String> chosen = (List<String>) context.get("orderItemSeqIds");
        String orderId = order.getString("orderId");
        Timestamp receivedDate = UtilDateTime.nowTimestamp();
        try {
            GenericValue system = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").cache().queryOne();
            GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", order.getString("productStoreId")).cache().queryOne();
            GenericValue placing = EntityQuery.use(delegator).from("OrderRole").where("orderId", orderId, "roleTypeId", "PLACING_CUSTOMER").queryFirst();
            Map<String, Object> hdr = new HashMap<>();
            hdr.put("userLogin", system);
            hdr.put("returnHeaderTypeId", "CUSTOMER_RETURN");
            hdr.put("fromPartyId", placing != null ? placing.getString("partyId") : null);
            hdr.put("toPartyId", store != null ? store.getString("payToPartyId") : null);
            hdr.put("destinationFacilityId", store != null && store.getString("inventoryFacilityId") != null ? store.getString("inventoryFacilityId") : order.getString("originFacilityId"));
            hdr.put("currencyUomId", order.getString("currencyUom"));
            hdr.put("entryDate", receivedDate);
            Map<String, Object> hdrRes = dispatcher.runSync("createReturnHeader", hdr);
            if (ServiceUtil.isError(hdrRes)) {
                return ServiceUtil.returnError("Could not record the withdrawal: " + ServiceUtil.getErrorMessage(hdrRes));
            }
            String returnId = (String) hdrRes.get("returnId");
            List<Map<String, Object>> withdrawn = new ArrayList<>();
            List<Map<String, Object>> pending = new ArrayList<>();
            for (Map<String, Object> item : items) {
                if (UtilValidate.isNotEmpty(chosen) && !chosen.contains((String) item.get("orderItemSeqId"))) {
                    continue;
                }
                Map<String, Object> ri = new HashMap<>();
                ri.put("userLogin", system);
                ri.put("returnId", returnId);
                ri.put("returnReasonId", "RTN_WITHDRAWAL");
                ri.put("returnTypeId", "RTN_REFUND");
                ri.put("returnItemTypeId", "RET_FPROD_ITEM");
                ri.put("orderId", orderId);
                ri.put("orderItemSeqId", item.get("orderItemSeqId"));
                ri.put("productId", item.get("productId"));
                ri.put("description", item.get("description"));
                ri.put("returnQuantity", item.get("quantity"));
                ri.put("returnPrice", item.get("unitPrice"));
                // not yet shipped or already returned: record it in the confirmation for the merchant to cancel
                GenericValue orderItem = EntityQuery.use(delegator).from("OrderItem").where("orderId", orderId, "orderItemSeqId", item.get("orderItemSeqId")).queryOne();
                Map<String, Object> rq = dispatcher.runSync("getReturnableQuantity", UtilMisc.toMap("orderItem", orderItem, "userLogin", system));
                BigDecimal returnable = ServiceUtil.isError(rq) ? null : (BigDecimal) rq.get("returnableQuantity");
                if (returnable == null || returnable.signum() <= 0) {
                    pending.add(item);
                    continue;
                }
                ri.put("returnQuantity", returnable.min((BigDecimal) item.get("quantity")));
                if (rq.get("returnablePrice") != null) {
                    ri.put("returnPrice", rq.get("returnablePrice"));
                }
                Map<String, Object> riRes = dispatcher.runSync("createReturnItem", ri);
                (ServiceUtil.isError(riRes) ? pending : withdrawn).add(item);
            }
            // the confirmation e-mail goes out after the commit (sendConfirmation), so it can read the new return
            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("returnId", returnId);
            result.put("receivedDate", receivedDate);
            result.put("withdrawnItems", withdrawn);
            result.put("pendingItems", pending);
            return result;
        } catch (GenericEntityException | GenericServiceException e) {
            Debug.logError(e, "Could not record the withdrawal of order " + orderId, module);
            return ServiceUtil.returnError("Could not record the withdrawal: " + e.getMessage());
        }
    }

    /**
     * Sends the confirmation of receipt (content and time) for a recorded withdrawal. Call it after createWithdrawal
     * has committed. Returns true when the mail service succeeded.
     */
    public static boolean sendConfirmation(DispatchContext dctx, String orderId, String email, String customerName,
                                           String returnId, Timestamp receivedDate, List<Map<String, Object>> withdrawn,
                                           List<Map<String, Object>> pending, Locale locale) {
        Delegator delegator = dctx.getDelegator();
        try {
            GenericValue order = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
            if (order == null) {
                return false;
            }
            GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", order.getString("productStoreId")).cache().queryOne();
            String sendTo = email;
            if (UtilValidate.isEmpty(sendTo)) {
                GenericValue ocm = EntityQuery.use(delegator).from("OrderContactMech").where("orderId", order.getString("orderId"), "contactMechPurposeTypeId", "ORDER_EMAIL").queryFirst();
                GenericValue cm = ocm != null ? ocm.getRelatedOne("ContactMech", false) : null;
                sendTo = cm != null ? cm.getString("infoString") : null;
            }
            if (UtilValidate.isEmpty(sendTo)) {
                return false;
            }
            String storeId = order.getString("productStoreId");
            GenericValue setting = EntityQuery.use(delegator).from("ProductStoreEmailSetting").where("productStoreId", storeId, "emailType", "PRDS_ODR_WITHDRAWAL").cache().queryOne();
            if (setting == null) {
                setting = EntityQuery.use(delegator).from("ProductStoreEmailSetting").where("productStoreId", storeId, "emailType", "PRDS_ODR_CONFIRM").cache().queryOne();
            }
            GenericValue profile = LegalDocumentWorker.getProfile(delegator, storeId);
            String from = setting != null && UtilValidate.isNotEmpty(setting.getString("fromAddress")) ? setting.getString("fromAddress")
                    : (profile != null ? profile.getString("contactEmail") : null);
            if (UtilValidate.isEmpty(from)) {
                Debug.logWarning("No sender address for the withdrawal confirmation of order " + order.getString("orderId"), module);
                return false;
            }
            boolean de = locale != null && "de".equals(locale.getLanguage());
            Map<String, Object> bodyParameters = UtilMisc.toMap("orderId", order.getString("orderId"), "orderDate", order.getTimestamp("orderDate"),
                    "withdrawalRef", returnId, "receivedDate", receivedDate, "withdrawnItems", withdrawn, "pendingItems", pending,
                    "customerName", customerName, "storeName", store != null ? store.getString("storeName") : "", "locale", locale,
                    "productStoreId", storeId);
            Map<String, Object> mail = new HashMap<>();
            mail.put("sendTo", sendTo);
            mail.put("sendFrom", from);
            if (setting != null && UtilValidate.isNotEmpty(setting.getString("bccAddress"))) {
                mail.put("sendBcc", setting.getString("bccAddress"));
            }
            mail.put("subject", (de ? "Eingangsbestätigung Ihres Widerrufs, Bestellung " : "Confirmation of your withdrawal, order ") + order.getString("orderId"));
            mail.put("bodyScreenUri", setting != null && "PRDS_ODR_WITHDRAWAL".equals(setting.getString("emailType")) && UtilValidate.isNotEmpty(setting.getString("bodyScreenLocation"))
                    ? setting.getString("bodyScreenLocation") : BODY_SCREEN);
            mail.put("bodyParameters", bodyParameters);
            mail.put("webSiteId", order.getString("webSiteId"));
            mail.put("locale", locale);
            mail.put("userLogin", EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").cache().queryOne());
            Map<String, Object> res = dctx.getDispatcher().runSync("sendMailFromScreen", mail, -1, true);
            return !ServiceUtil.isError(res);
        } catch (GenericEntityException | GenericServiceException e) {
            Debug.logError(e, "Could not send the withdrawal confirmation of order " + orderId, module);
            return false;
        }
    }
}

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
package com.ilscipio.scipio.order.payment;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.LinkedHashSet;
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
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.order.order.OrderChangeHelper;
import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.Service;

/**
 * The store side of the card payment through the hub (W1-10d). The store never calls Stripe: the hub does, and the desk
 * gives the result to the store over MCP (topic order: {@code payment_hub_record}, {@code payment_hub_info},
 * {@code payment_hub_refund_record}). The MCP layer writes the McpAuditLog row.
 *
 * <p>Review fixes: each record needs the permission {@code HUBPAY_RECORD} (only the MCP login of the desk has it) and the MAC
 * of the desk over the recorded values, made with the checkout key of the store ({@link HubCheckout#recordMac}). A succeeded
 * payment that the order cannot take (paid by another PaymentIntent, cancelled, no open card payment) is an
 * {@code orphan_payment}: the pod writes a note and the desk refunds it. The record locks the preference rows of the order
 * (an UPDATE) before it reads them: two records at the same time do not both create.</p>
 *
 * <p>Each record is idempotent by the Stripe ID (the PaymentIntent, the refund): a repeat finds the gateway response row and
 * changes nothing.</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-10d).</p>
 */
public final class HubPaymentServices {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String ERR_NO_PAYMENT = "no_hub_payment";
    public static final String ERR_CURRENCY = "currency_mismatch";
    public static final String ERR_SHORT = "amount_short";
    public static final String ERR_REFUND_TOO_LARGE = "refund_exceeds_payment";
    public static final String ERR_NOT_PAID = "payment_not_received";
    public static final String ERR_MAC_MISSING = "mac_missing";
    public static final String ERR_MAC_BAD = "mac_mismatch";
    public static final String ERR_NO_KEY = "no_checkout_key";
    public static final String ERR_PERMISSION = "permission_denied";
    public static final String ERR_OTHER_PAYMENT = "other_payment";
    /** The result code of a succeeded payment that the order cannot take: the desk refunds it. */
    public static final String ORPHAN_PAYMENT = "orphan_payment";
    /** The default age (days) of an unpaid card order that the auto-cancel job cancels. */
    public static final int DEFAULT_CANCEL_DAYS = 3;

    private HubPaymentServices() {}

    @Service(
        name = "recordHubPayment",
        engine = "java",
        location = "com.ilscipio.scipio.order.payment.HubPaymentServices",
        invoke = "recordHubPayment",
        description = "Records the result of a card payment that the hub took on the connected account of the store (EXT_STRIPE_HUB). "
                + "paymentStatus succeeded: gateway response, payment received, order approved; an order that cannot take it answers "
                + "errorCode orphan_payment (the desk refunds it). paymentStatus failed: an order note. Idempotent by externalPaymentId. "
                + "Needs the permission HUBPAY_RECORD and the MAC of the desk (checkout key).",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "externalPaymentId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "amount", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "paymentStatus", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "failureCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mac", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "recorded", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "paymentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "errorCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "orphanReason", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface RecordHubPayment {}

    @Service(
        name = "getHubPaymentInfo",
        engine = "java",
        location = "com.ilscipio.scipio.order.payment.HubPaymentServices",
        invoke = "getHubPaymentInfo",
        description = "Reads one order payment preference for a refund through the hub (by orderPaymentPreferenceId, or by the Stripe "
                + "PaymentIntent externalPaymentId): the payment method type, the PaymentIntent, the captured and the refunded amount. "
                + "Needs the permission ORDERMGR_VIEW.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreferenceId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "externalPaymentId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "paymentMethodTypeId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "currencyUomId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "capturedAmount", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "refundedAmount", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "errorCode", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetHubPaymentInfo {}

    @Service(
        name = "recordHubPaymentRefund",
        engine = "java",
        location = "com.ilscipio.scipio.order.payment.HubPaymentServices",
        invoke = "recordHubPaymentRefund",
        description = "Records a refund that the hub made on Stripe for an EXT_STRIPE_HUB payment (from the app or from the Stripe "
                + "dashboard): gateway response, refund payment, the preference status PAYMENT_REFUNDED when all is refunded. "
                + "Idempotent by externalRefundId. Needs HUBPAY_RECORD and the MAC of the desk.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreferenceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "externalPaymentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "externalRefundId", type = "String", mode = "IN", optional = "false"),
            @Attribute(name = "reason", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mac", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "recorded", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "paymentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "errorCode", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface RecordHubPaymentRefund {}

    @Service(
        name = "cancelUnpaidHubOrders",
        engine = "java",
        location = "com.ilscipio.scipio.order.payment.HubPaymentServices",
        invoke = "cancelUnpaidHubOrders",
        description = "Cancels each order with a card payment through the hub (EXT_STRIPE_HUB) that stays unpaid for the given days "
                + "(default: SystemProperty hubcheckout.cancelDays, else 3): the order, its items and the stock reservations. "
                + "The daily job runs it. Needs ORDERMGR_UPDATE.",
        auth = "true",
        attributes = {
            @Attribute(name = "days", type = "Integer", mode = "IN", optional = "true"),
            @Attribute(name = "cancelledOrderIds", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface CancelUnpaidHubOrders {}

    // ---------------------------------------------------------------- pure checks (tests)

    /** Null when the paid amount covers the due amount in the order currency; else the error code. */
    public static String checkPayment(BigDecimal due, String orderCurrency, BigDecimal paid, String paidCurrency) {
        if (orderCurrency == null || paidCurrency == null || !orderCurrency.equalsIgnoreCase(paidCurrency)) {
            return ERR_CURRENCY;
        }
        if (paid == null || due == null || paid.compareTo(due) < 0) {
            return ERR_SHORT;
        }
        return null;
    }

    /** Null when a refund of {@code amount} fits into the captured amount; else the error code. */
    public static String checkRefund(BigDecimal captured, BigDecimal refundedBefore, BigDecimal amount) {
        if (captured == null || captured.signum() <= 0) {
            return ERR_NOT_PAID;
        }
        if (amount == null || amount.signum() <= 0 || refundedBefore.add(amount).compareTo(captured) > 0) {
            return ERR_REFUND_TOO_LARGE;
        }
        return null;
    }

    /** What a succeeded PaymentIntent does to the order. */
    public enum Outcome { RECORD, ALREADY_RECORDED, ORPHAN }

    /**
     * The outcome of a succeeded PaymentIntent (pure, for tests).
     *
     * @param orderStatusId  the order status, or null when the order does not exist
     * @param prefStatusId   the status of the open EXT_STRIPE_HUB preference, or null when the order has none
     * @param prefIntent     the PaymentIntent that paid the preference (manualRefNum), or null
     * @param sameIntentSeen a capture row with this PaymentIntent exists
     * @param paymentIntent  the PaymentIntent of the record
     */
    public static Outcome outcome(String orderStatusId, String prefStatusId, String prefIntent, boolean sameIntentSeen, String paymentIntent) {
        if (sameIntentSeen || (paymentIntent != null && paymentIntent.equals(prefIntent))) {
            return Outcome.ALREADY_RECORDED;
        }
        if (orderStatusId == null || "ORDER_CANCELLED".equals(orderStatusId) || "ORDER_REJECTED".equals(orderStatusId) || prefStatusId == null) {
            return Outcome.ORPHAN;
        }
        if ("PAYMENT_RECEIVED".equals(prefStatusId) || "PAYMENT_SETTLED".equals(prefStatusId) || "PAYMENT_REFUNDED".equals(prefStatusId)) {
            return Outcome.ORPHAN;
        }
        return Outcome.RECORD;
    }

    /** The reason text of an orphan payment for the note and the desk alert. */
    static String orphanReason(String orderStatusId, String prefStatusId) {
        if (orderStatusId == null) {
            return "no_order";
        }
        if ("ORDER_CANCELLED".equals(orderStatusId) || "ORDER_REJECTED".equals(orderStatusId)) {
            return "order_cancelled";
        }
        if (prefStatusId == null) {
            return "no_open_card_payment";
        }
        return "already_paid";
    }

    /** Null when the caller may record and the MAC fits; else the error result. */
    static Map<String, Object> checkRecordCall(DispatchContext dctx, GenericValue userLogin, String mac, String kind, String orderId,
            String paymentIntent, String amount, String currency, String ref) {
        if (userLogin == null || !dctx.getSecurity().hasPermission(HubCheckout.PERMISSION_RECORD, userLogin)) {
            return fail(ERR_PERMISSION, "Permission " + HubCheckout.PERMISSION_RECORD + " is required (the MCP login of the desk).");
        }
        if (UtilValidate.isEmpty(mac)) {
            return fail(ERR_MAC_MISSING, "The record has no MAC of the desk.");
        }
        String key = HubCheckout.checkoutKey(dctx.getDelegator());
        if (key == null) {
            return fail(ERR_NO_KEY, "The store has no checkout key (SystemProperty hubcheckout.key).");
        }
        if (!HubCheckout.macMatches(key, mac, kind, orderId, paymentIntent, amount, currency, ref)) {
            return fail(ERR_MAC_BAD, "The MAC of the record does not fit its values.");
        }
        return null;
    }

    // ---------------------------------------------------------------- services

    public static Map<String, Object> recordHubPayment(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String orderId = (String) context.get("orderId");
        String paymentIntent = (String) context.get("externalPaymentId");
        String status = ((String) context.get("paymentStatus")).trim().toLowerCase(Locale.ROOT);
        String amountText = (String) context.get("amount");
        String currency = ((String) context.get("currencyUomId")).trim().toUpperCase(Locale.ROOT);
        boolean succeeded = "succeeded".equals(status);
        Map<String, Object> refused = checkRecordCall(dctx, userLogin, (String) context.get("mac"),
                succeeded ? HubCheckout.KIND_PAYMENT : HubCheckout.KIND_PAYMENT_FAILED, orderId, paymentIntent, amountText, currency, null);
        if (refused != null) {
            return refused;
        }
        try {
            GenericValue order = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
            if (order != null) {
                lockPreferences(delegator, EntityCondition.makeCondition("orderId", orderId));
            }
            GenericValue pref = order == null ? null : HubCheckout.preference(delegator, orderId);
            String prefStatus = pref == null ? null : pref.getString("statusId");
            if (!succeeded) {
                if (pref == null) {
                    return fail(ERR_NO_PAYMENT, "Order " + orderId + " has no open " + HubCheckout.PAYMENT_METHOD_TYPE_ID + " payment.");
                }
                Map<String, Object> out = ServiceUtil.returnSuccess();
                out.put("recorded", Boolean.FALSE);
                if ("PAYMENT_RECEIVED".equals(prefStatus) || "PAYMENT_SETTLED".equals(prefStatus)) {
                    // the order is paid: a failed attempt of another PaymentIntent changes nothing and asks the shopper for nothing
                    return out;
                }
                String code = (String) context.get("failureCode");
                Map<String, Object> note = dispatcher.runSync("createOrderNote", UtilMisc.toMap("userLogin", userLogin, "orderId", orderId,
                        "internalNote", "Y", "note", "Card payment " + paymentIntent + " failed" + (UtilValidate.isEmpty(code) ? "" : " (" + code + ")")
                                + ". The shopper can pay again on the order page."));
                if (ServiceUtil.isError(note)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(note));
                }
                return out;
            }
            boolean seen = pref != null && EntityQuery.use(delegator).from("PaymentGatewayResponse").where("orderPaymentPreferenceId",
                    pref.getString("orderPaymentPreferenceId"), "referenceNum", paymentIntent, "transCodeEnumId", "PGT_CAPTURE").queryFirst() != null;
            Outcome outcome = outcome(order == null ? null : order.getString("statusId"), prefStatus,
                    pref == null ? null : pref.getString("manualRefNum"), seen, paymentIntent);
            if (outcome == Outcome.ALREADY_RECORDED) {
                Map<String, Object> out = ServiceUtil.returnSuccess("The payment is already recorded.");
                out.put("recorded", Boolean.FALSE);
                return out;
            }
            if (outcome == Outcome.ORPHAN) {
                String reason = orphanReason(order == null ? null : order.getString("statusId"), prefStatus);
                if (order != null) {
                    Map<String, Object> note = dispatcher.runSync("createOrderNote", UtilMisc.toMap("userLogin", userLogin, "orderId", orderId,
                            "internalNote", "Y", "note", "Card payment " + paymentIntent + " (" + amountText + " " + currency + ") arrived, but the order "
                                    + "cannot take it (" + reason + "). The platform refunds it to the shopper automatically."));
                    if (ServiceUtil.isError(note)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(note));
                    }
                }
                Debug.logWarning("recordHubPayment: orphan payment " + paymentIntent + " for order " + orderId + " (" + reason + ")", module);
                Map<String, Object> out = ServiceUtil.returnSuccess("The order cannot take the payment (" + reason + "); refund it.");
                out.put("recorded", Boolean.FALSE);
                out.put("errorCode", ORPHAN_PAYMENT);
                out.put("orphanReason", reason);
                return out;
            }
            BigDecimal paid = new BigDecimal(amountText.trim());
            BigDecimal due = pref.getBigDecimal("maxAmount") != null ? pref.getBigDecimal("maxAmount") : new OrderReadHelper(order).getOrderGrandTotal();
            String problem = checkPayment(due, order.getString("currencyUom"), paid, currency);
            if (problem != null) {
                return fail(problem, "The payment " + paymentIntent + " (" + paid + " " + currency + ") does not pay order " + orderId + " ("
                        + due + " " + order.getString("currencyUom") + ").");
            }
            Timestamp now = UtilDateTime.nowTimestamp();
            String responseId = delegator.getNextSeqId("PaymentGatewayResponse");
            delegator.create("PaymentGatewayResponse", UtilMisc.toMap("paymentGatewayResponseId", responseId,
                    "paymentServiceTypeEnumId", "PRDS_PAY_EXTERNAL", "orderPaymentPreferenceId", pref.getString("orderPaymentPreferenceId"),
                    "paymentMethodTypeId", HubCheckout.PAYMENT_METHOD_TYPE_ID, "transCodeEnumId", "PGT_CAPTURE", "amount", paid,
                    "currencyUomId", currency, "referenceNum", paymentIntent, "gatewayMessage", "Stripe (hub)", "transactionDate", now));
            pref.set("statusId", "PAYMENT_RECEIVED");
            pref.set("manualRefNum", paymentIntent);
            pref.store();
            GenericValue placing = new OrderReadHelper(order).getPlacingParty();
            GenericValue store = order.getRelatedOne("ProductStore", false);
            String paymentId = delegator.getNextSeqId("Payment");
            delegator.create("Payment", UtilMisc.toMap("paymentId", paymentId, "paymentTypeId", "CUSTOMER_PAYMENT",
                    "paymentMethodTypeId", HubCheckout.PAYMENT_METHOD_TYPE_ID, "paymentPreferenceId", pref.getString("orderPaymentPreferenceId"),
                    "paymentGatewayResponseId", responseId, "partyIdFrom", placing == null ? "_NA_" : placing.getString("partyId"),
                    "partyIdTo", store == null ? null : store.getString("payToPartyId"), "statusId", "PMNT_RECEIVED", "effectiveDate", now,
                    "paymentRefNum", paymentIntent, "amount", paid, "currencyUomId", currency, "comments", "Card payment through the hub"));
            if (!OrderChangeHelper.approveOrder(dispatcher, userLogin, orderId)) {
                return ServiceUtil.returnError("The payment is recorded, but order " + orderId + " is not approved.");
            }
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.put("recorded", Boolean.TRUE);
            out.put("paymentId", paymentId);
            return out;
        } catch (GenericEntityException | GenericServiceException | NumberFormatException e) {
            Debug.logError(e, "recordHubPayment: order " + orderId + ": " + e.getMessage(), module);
            return ServiceUtil.returnError("recordHubPayment: " + e.getMessage());
        }
    }

    public static Map<String, Object> getHubPaymentInfo(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (!dctx.getSecurity().hasEntityPermission("ORDERMGR", "_VIEW", userLogin)) {
            return ServiceUtil.returnError("Permission ORDERMGR_VIEW is required.");
        }
        String prefId = (String) context.get("orderPaymentPreferenceId");
        String intent = (String) context.get("externalPaymentId");
        if (UtilValidate.isEmpty(prefId) && UtilValidate.isEmpty(intent)) {
            return fail(ERR_NO_PAYMENT, "Give orderPaymentPreferenceId or externalPaymentId.");
        }
        try {
            GenericValue pref = findPreference(delegator, prefId, intent);
            if (pref == null) {
                return fail(ERR_NO_PAYMENT, "No order payment preference " + (prefId != null ? prefId : "for " + intent) + ".");
            }
            prefId = pref.getString("orderPaymentPreferenceId");
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.put("orderPaymentPreferenceId", prefId);
            out.put("orderId", pref.getString("orderId"));
            out.put("paymentMethodTypeId", pref.getString("paymentMethodTypeId"));
            out.put("statusId", pref.getString("statusId"));
            if (HubCheckout.PAYMENT_METHOD_TYPE_ID.equals(pref.getString("paymentMethodTypeId"))) {
                Totals t = totals(delegator, prefId);
                out.put("externalPaymentId", pref.getString("manualRefNum"));
                out.put("currencyUomId", t.currency);
                out.put("capturedAmount", t.captured.toPlainString());
                out.put("refundedAmount", t.refunded.toPlainString());
            }
            return out;
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError("getHubPaymentInfo: " + e.getMessage());
        }
    }

    public static Map<String, Object> recordHubPaymentRefund(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String prefId = (String) context.get("orderPaymentPreferenceId");
        String orderId = (String) context.get("orderId");
        String intent = (String) context.get("externalPaymentId");
        String refundId = (String) context.get("externalRefundId");
        String amountText = (String) context.get("amount");
        String currency = context.get("currencyUomId") == null ? null : ((String) context.get("currencyUomId")).trim().toUpperCase(Locale.ROOT);
        Map<String, Object> refused = checkRecordCall(dctx, userLogin, (String) context.get("mac"), HubCheckout.KIND_REFUND, orderId, intent,
                amountText, currency, refundId);
        if (refused != null) {
            return refused;
        }
        try {
            GenericValue pref = findPreference(delegator, prefId, intent);
            if (pref == null || !HubCheckout.PAYMENT_METHOD_TYPE_ID.equals(pref.getString("paymentMethodTypeId"))) {
                return fail(ERR_NO_PAYMENT, "Preference " + (prefId != null ? prefId : intent) + " is not a card payment through the hub.");
            }
            prefId = pref.getString("orderPaymentPreferenceId");
            // the MAC covers the order and the PaymentIntent: they must be the ones of the preference
            if (!pref.getString("orderId").equals(orderId) || intent == null || !intent.equals(pref.getString("manualRefNum"))) {
                return fail(ERR_OTHER_PAYMENT, "The refund belongs to another order or PaymentIntent than preference " + prefId + ".");
            }
            lockPreferences(delegator, EntityCondition.makeCondition("orderPaymentPreferenceId", prefId));
            GenericValue earlier = EntityQuery.use(delegator).from("PaymentGatewayResponse").where("orderPaymentPreferenceId", prefId,
                    "referenceNum", refundId, "transCodeEnumId", "PGT_REFUND").queryFirst();
            if (earlier != null) {
                Map<String, Object> out = ServiceUtil.returnSuccess("The refund is already recorded.");
                out.put("recorded", Boolean.FALSE);
                return out;
            }
            BigDecimal amount = new BigDecimal(amountText.trim());
            Totals t = totals(delegator, prefId);
            if (t.currency != null && currency != null && !t.currency.equalsIgnoreCase(currency)) {
                return fail(ERR_CURRENCY, "The refund is in " + currency + ", the payment in " + t.currency + ".");
            }
            String problem = checkRefund(t.captured, t.refunded, amount);
            if (problem != null) {
                return fail(problem, "A refund of " + amount + " does not fit: captured " + t.captured + ", refunded " + t.refunded + ".");
            }
            Timestamp now = UtilDateTime.nowTimestamp();
            String responseId = delegator.getNextSeqId("PaymentGatewayResponse");
            delegator.create("PaymentGatewayResponse", UtilMisc.toMap("paymentGatewayResponseId", responseId,
                    "paymentServiceTypeEnumId", "PRDS_PAY_REFUND", "orderPaymentPreferenceId", prefId,
                    "paymentMethodTypeId", HubCheckout.PAYMENT_METHOD_TYPE_ID, "transCodeEnumId", "PGT_REFUND", "amount", amount,
                    "currencyUomId", t.currency, "referenceNum", refundId, "gatewayMessage", "Stripe refund (hub)", "transactionDate", now));
            GenericValue order = pref.getRelatedOne("OrderHeader", false);
            GenericValue placing = order == null ? null : new OrderReadHelper(order).getPlacingParty();
            GenericValue store = order == null ? null : order.getRelatedOne("ProductStore", false);
            String paymentId = delegator.getNextSeqId("Payment");
            String reason = (String) context.get("reason");
            delegator.create("Payment", UtilMisc.toMap("paymentId", paymentId, "paymentTypeId", "CUSTOMER_REFUND",
                    "paymentMethodTypeId", HubCheckout.PAYMENT_METHOD_TYPE_ID, "paymentPreferenceId", prefId, "paymentGatewayResponseId", responseId,
                    "partyIdFrom", store == null ? null : store.getString("payToPartyId"), "partyIdTo", placing == null ? "_NA_" : placing.getString("partyId"),
                    "statusId", "PMNT_SENT", "effectiveDate", now, "paymentRefNum", refundId, "amount", amount, "currencyUomId", t.currency,
                    "comments", "Card refund through the hub" + (UtilValidate.isEmpty(reason) ? "" : ": " + reason)));
            if (t.refunded.add(amount).compareTo(t.captured) >= 0) {
                pref.set("statusId", "PAYMENT_REFUNDED");
                pref.store();
            }
            Map<String, Object> out = ServiceUtil.returnSuccess();
            out.put("recorded", Boolean.TRUE);
            out.put("paymentId", paymentId);
            return out;
        } catch (GenericEntityException | NumberFormatException e) {
            Debug.logError(e, "recordHubPaymentRefund: preference " + prefId + ": " + e.getMessage(), module);
            return ServiceUtil.returnError("recordHubPaymentRefund: " + e.getMessage());
        }
    }

    public static Map<String, Object> cancelUnpaidHubOrders(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (!dctx.getSecurity().hasEntityPermission("ORDERMGR", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError("Permission ORDERMGR_UPDATE is required.");
        }
        int days = context.get("days") instanceof Integer ? (Integer) context.get("days") : cancelDays(delegator);
        Timestamp cutoff = new Timestamp(System.currentTimeMillis() - days * 86_400_000L);
        java.util.List<String> cancelled = new java.util.ArrayList<>();
        try {
            Set<String> orderIds = new LinkedHashSet<>();
            for (GenericValue p : EntityQuery.use(delegator).from("OrderPaymentPreference").where("paymentMethodTypeId",
                    HubCheckout.PAYMENT_METHOD_TYPE_ID, "statusId", "PAYMENT_NOT_RECEIVED").queryList()) {
                orderIds.add(p.getString("orderId"));
            }
            for (String orderId : orderIds) {
                GenericValue order = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
                if (order == null || order.getTimestamp("orderDate") == null || !order.getTimestamp("orderDate").before(cutoff)
                        || !mayAutoCancel(order.getString("statusId"))) {
                    continue;
                }
                if (OrderChangeHelper.cancelOrder(dispatcher, userLogin, orderId)) {
                    dispatcher.runSync("createOrderNote", UtilMisc.toMap("userLogin", userLogin, "orderId", orderId, "internalNote", "Y",
                            "note", "Cancelled: the card payment did not arrive in " + days + " days. The stock is free again."));
                    cancelled.add(orderId);
                } else {
                    Debug.logWarning("cancelUnpaidHubOrders: order " + orderId + " could not be cancelled", module);
                }
            }
        } catch (GenericEntityException | GenericServiceException e) {
            Debug.logError(e, "cancelUnpaidHubOrders: " + e.getMessage(), module);
            return ServiceUtil.returnError("cancelUnpaidHubOrders: " + e.getMessage());
        }
        Map<String, Object> out = ServiceUtil.returnSuccess("Cancelled " + cancelled.size() + " unpaid card orders.");
        out.put("cancelledOrderIds", cancelled);
        return out;
    }

    /** An order that the auto-cancel job may cancel: not approved, not done (pure, for tests). */
    public static boolean mayAutoCancel(String orderStatusId) {
        return "ORDER_CREATED".equals(orderStatusId) || "ORDER_HOLD".equals(orderStatusId);
    }

    static int cancelDays(Delegator delegator) {
        String v = EntityUtilProperties.getPropertyValue(HubCheckout.RESOURCE, "cancelDays", String.valueOf(DEFAULT_CANCEL_DAYS), delegator);
        try {
            int d = Integer.parseInt(v.trim());
            return d > 0 ? d : DEFAULT_CANCEL_DAYS;
        } catch (NumberFormatException e) {
            return DEFAULT_CANCEL_DAYS;
        }
    }

    // ---------------------------------------------------------------- helpers

    /**
     * Locks the EXT_STRIPE_HUB preference rows of the condition until the transaction of the service ends: an UPDATE that
     * writes no new value (review finding 7f). A second record of the same order waits here and then reads the rows of the
     * first.
     */
    static void lockPreferences(Delegator delegator, EntityCondition condition) throws GenericEntityException {
        delegator.storeByCondition("OrderPaymentPreference", UtilMisc.toMap("paymentMethodTypeId", HubCheckout.PAYMENT_METHOD_TYPE_ID),
                EntityCondition.makeCondition(condition, EntityCondition.makeCondition("paymentMethodTypeId", HubCheckout.PAYMENT_METHOD_TYPE_ID)));
    }

    /** The preference by ID, else the EXT_STRIPE_HUB preference that the PaymentIntent paid. */
    static GenericValue findPreference(Delegator delegator, String prefId, String paymentIntent) throws GenericEntityException {
        if (UtilValidate.isNotEmpty(prefId)) {
            return EntityQuery.use(delegator).from("OrderPaymentPreference").where("orderPaymentPreferenceId", prefId).queryOne();
        }
        if (UtilValidate.isEmpty(paymentIntent)) {
            return null;
        }
        return EntityQuery.use(delegator).from("OrderPaymentPreference").where("paymentMethodTypeId", HubCheckout.PAYMENT_METHOD_TYPE_ID,
                "manualRefNum", paymentIntent).queryFirst();
    }

    static final class Totals {
        BigDecimal captured = BigDecimal.ZERO;
        BigDecimal refunded = BigDecimal.ZERO;
        String currency;
    }

    static Totals totals(Delegator delegator, String prefId) throws GenericEntityException {
        Totals t = new Totals();
        for (GenericValue r : EntityQuery.use(delegator).from("PaymentGatewayResponse").where("orderPaymentPreferenceId", prefId).queryList()) {
            BigDecimal a = r.getBigDecimal("amount") == null ? BigDecimal.ZERO : r.getBigDecimal("amount");
            if ("PGT_CAPTURE".equals(r.getString("transCodeEnumId"))) {
                t.captured = t.captured.add(a);
                t.currency = r.getString("currencyUomId");
            } else if ("PGT_REFUND".equals(r.getString("transCodeEnumId"))) {
                t.refunded = t.refunded.add(a);
            }
        }
        return t;
    }

    private static Map<String, Object> fail(String code, String text) {
        Map<String, Object> r = ServiceUtil.returnError(code + ": " + text);
        r.put("errorCode", code);
        return r;
    }
}

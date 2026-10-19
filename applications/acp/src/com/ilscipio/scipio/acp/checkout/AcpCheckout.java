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
import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.order.order.OrderChangeHelper;
import org.ofbiz.order.shoppingcart.CartItemModifyException;
import org.ofbiz.order.shoppingcart.CheckOutHelper;
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.acp.AcpConfig;
import com.ilscipio.scipio.acp.AcpException;
import com.ilscipio.scipio.acp.AcpJson;
import com.ilscipio.scipio.acp.payment.AcpPaymentHandler;
import com.ilscipio.scipio.acp.payment.AcpPaymentHandler.ChargeRequest;
import com.ilscipio.scipio.acp.payment.AcpPaymentHandler.ChargeResult;
import com.ilscipio.scipio.acp.payment.StripeSptHandler;
import com.ilscipio.scipio.acp.webhook.AcpWebhooks;

/**
 * SCIPIO: 4.0.0: The five operations of an ACP checkout session (create, get, update, complete, cancel). The REST
 * endpoint and the MCP binding both call this class. The session row keeps what the agent sent; {@link AcpPricing}
 * prices it again on every call with the cart, promotion, shipping and tax engines of the store.
 */
public final class AcpCheckout {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String ENTITY = "AcpCheckoutSession";

    public static final String COMPLETED = "completed";
    public static final String CANCELED = "canceled";
    public static final String EXPIRED = "expired";
    public static final String IN_PROGRESS = "complete_in_progress";

    private AcpCheckout() {}

    // ---------------------------------------------------------------- operations

    public static Map<String, Object> create(AcpContext ctx, Map<String, Object> body) throws AcpException {
        List<Object> items = AcpJson.list(body.get("line_items"));
        if (items.isEmpty()) {
            items = AcpJson.list(body.get("items")); // 2025 versions
        }
        if (items.isEmpty()) {
            throw AcpException.invalid("missing", "line_items needs at least one item.", "$.line_items");
        }
        String currency;
        try {
            currency = UtilValidate.isNotEmpty(AcpJson.str(body, "currency")) ? AcpJson.str(body, "currency").toUpperCase(Locale.ROOT)
                    : ctx.getDefaultCurrency();
        } catch (GenericEntityException e) {
            throw AcpException.processing("The store is not available.");
        }
        if (!currency.matches("[A-Z]{3}")) {
            throw AcpException.invalid("invalid", "currency must be an ISO 4217 code.", "$.currency");
        }
        Map<String, Object> state = new LinkedHashMap<>();
        state.put("items", normalizeItems(items, "$.line_items"));
        copyIfPresent(body, state, "buyer", "fulfillment_details", "locale", "timezone", "metadata", "order_notes", "affiliate_attribution");
        state.put("codes", discountCodes(body));
        state.put("agent_capabilities", body.get("capabilities"));
        state.put("base_url", ctx.getBaseUrl());

        String id = "cs_" + UUID.randomUUID().toString().replace("-", "");
        Timestamp now = UtilDateTime.nowTimestamp();
        GenericValue row = ctx.getDelegator().makeValue(ENTITY);
        row.set("checkoutSessionId", id);
        row.set("productStoreId", ctx.getProductStoreId());
        row.set("webSiteId", ctx.getWebSiteId());
        row.set("webappName", ctx.getWebappName());
        row.set("tokenId", ctx.getTokenId());
        row.set("userLoginId", ctx.getAgentUserLoginId());
        row.set("currencyUomId", currency);
        row.set("createdDate", now);
        row.set("expiresDate", new Timestamp(now.getTime() + AcpConfig.getInt(ctx.getDelegator(), "session.ttlMinutes", 360) * 60_000L));
        return priceAndStore(ctx, row, state, true);
    }

    public static Map<String, Object> get(AcpContext ctx, String id) throws AcpException {
        GenericValue row = load(ctx, id);
        Map<String, Object> state = state(row);
        // read only: a write here could undo a complete that runs at the same time
        return render(ctx, row, state);
    }

    public static Map<String, Object> update(AcpContext ctx, String id, Map<String, Object> body) throws AcpException {
        GenericValue row = load(ctx, id);
        requireOpen(row);
        Map<String, Object> state = state(row);
        if (body.containsKey("line_items") || body.containsKey("items")) {
            List<Object> items = AcpJson.list(body.containsKey("line_items") ? body.get("line_items") : body.get("items"));
            if (items.isEmpty()) {
                throw AcpException.invalid("missing", "line_items needs at least one item.", "$.line_items");
            }
            state.put("items", normalizeItems(items, "$.line_items"));
        }
        if (body.containsKey("buyer")) state.put("buyer", mergeMaps(AcpJson.map(state.get("buyer")), AcpJson.map(body.get("buyer"))));
        if (body.containsKey("fulfillment_details")) state.put("fulfillment_details", body.get("fulfillment_details"));
        if (body.containsKey("fulfillment_address")) state.put("fulfillment_details", UtilMisc.toMap("address", body.get("fulfillment_address"))); // 2025
        if (body.containsKey("selected_fulfillment_options")) state.put("selected", body.get("selected_fulfillment_options"));
        if (body.containsKey("fulfillment_option_id")) { // 2025
            state.put("selected", List.of(UtilMisc.toMap("type", "shipping", "option_id", body.get("fulfillment_option_id"))));
        }
        if (body.containsKey("discounts") || body.containsKey("coupons")) state.put("codes", discountCodes(body));
        copyIfPresent(body, state, "order_notes");
        return priceAndStore(ctx, row, state, false);
    }

    public static Map<String, Object> cancel(AcpContext ctx, String id, Map<String, Object> body) throws AcpException {
        GenericValue row = load(ctx, id);
        String status = row.getString("status");
        if (COMPLETED.equals(status) || IN_PROGRESS.equals(status)) {
            throw new AcpException(405, AcpException.INVALID_REQUEST, "invalid_state", "A completed checkout session cannot be canceled.", null);
        }
        Map<String, Object> state = state(row);
        if (!CANCELED.equals(status) && !EXPIRED.equals(status)) {
            String pending = AcpJson.str(state, "pending_order_id");
            if (pending != null) {
                // an attempt with an unknown payment outcome: never cancel an order that may be paid
                AcpPaymentHandler handler = AcpPaymentHandler.forStore(ctx);
                ChargeResult earlier = handler != null ? lookupPending(ctx, handler, state, pending) : null;
                if (earlier == null) {
                    throw AcpException.conflict("payment_outcome_unknown", "The payment of this checkout is still being checked; try again later.");
                }
                if (earlier.status == ChargeResult.Status.SUCCEEDED) {
                    finishPaid(ctx, handler, row, state, null, pending, earlier.reference, pendingTotal(state, row));
                    throw new AcpException(405, AcpException.INVALID_REQUEST, "invalid_state", "The payment was taken; the order is placed.", null);
                }
                state.remove("pending_order_id");
            }
            if (body != null && body.get("intent_trace") != null) state.put("intent_trace", body.get("intent_trace"));
            if (storeIfOpen(ctx, row, UtilMisc.toMap("status", CANCELED, "stateJson", AcpJson.write(state))) != 1) {
                throw AcpException.conflict("checkout_in_progress", "The checkout session changed; read it again.");
            }
            if (pending != null) {
                cancelOrder(ctx, pending, "The agent canceled the checkout session.");
            }
        }
        return render(ctx, row, state);
    }

    /**
     * Places the order and charges the shared payment token. Safe to retry: the order of an attempt whose payment
     * outcome is unknown stays open (pending_order_id) and a retry with the same token uses the same order and the
     * same PSP idempotency key; only a definite decline cancels that order and starts a new attempt.
     */
    public static Map<String, Object> complete(AcpContext ctx, String id, Map<String, Object> body) throws AcpException {
        Map<String, Object> paymentData = AcpJson.map(body.get("payment_data"));
        if (paymentData == null) {
            throw AcpException.invalid("missing", "payment_data is required.", "$.payment_data");
        }
        String spt = sharedPaymentToken(paymentData);
        if (UtilValidate.isEmpty(spt)) {
            throw AcpException.invalid("missing", "payment_data.instrument.credential.token is required.", "$.payment_data.instrument.credential.token");
        }
        String handlerId = AcpJson.str(paymentData, "handler_id");
        if (UtilValidate.isNotEmpty(handlerId) && !AcpPaymentHandler.HANDLER_ID.equals(handlerId)) {
            throw AcpException.invalid("invalid", "Unknown payment handler " + handlerId + ".", "$.payment_data.handler_id");
        }
        AcpPaymentHandler handler = AcpPaymentHandler.forStore(ctx);
        if (handler == null) {
            throw AcpException.unavailable("The store accepts no agent payments.");
        }

        // 1. price and claim the session: only one complete runs
        GenericValue row = load(ctx, id);
        if (COMPLETED.equals(row.getString("status"))) {
            return render(ctx, row, state(row));
        }
        requireOpen(row);
        Map<String, Object> state = state(row);
        if (body.containsKey("buyer")) state.put("buyer", mergeMaps(AcpJson.map(state.get("buyer")), AcpJson.map(body.get("buyer"))));
        if (paymentData.get("billing_address") != null) state.put("billing_address", paymentData.get("billing_address"));
        copyIfPresent(body, state, "order_notes", "marketing_consents", "affiliate_attribution");
        AcpPricing.Result priced = AcpPricing.price(ctx, row, state);
        Object quoted = state.get("quoted_total");
        // a retry of an attempt with an unknown outcome charges the total of its order, not a new price
        boolean pendingAttempt = AcpJson.str(state, "pending_order_id") != null;
        boolean priceChanged = !pendingAttempt && quoted != null && ((Number) quoted).longValue() != priced.totalMinor;
        if (!"ready_for_payment".equals(priced.status) || priceChanged) {
            state.put("quoted_total", priced.totalMinor);
            if (storeIfOpen(ctx, row, UtilMisc.toMap("status", priced.status, "stateJson", AcpJson.write(state))) != 1) {
                throw AcpException.conflict("checkout_in_progress", "The checkout session changed; read it again.");
            }
            row.set("status", priced.status);
            Map<String, Object> out = render(ctx, row, state, priced);
            if (priceChanged) {
                addMessage(out, warning("price_change", "$.totals", "The total changed to " + AcpJson.fromMinor(priced.totalMinor, priced.currency)
                        + " " + priced.currency + "; confirm the new total and complete again."));
            }
            return out;
        }
        state.put("claimed_at", System.currentTimeMillis());
        if (storeIfOpen(ctx, row, UtilMisc.toMap("status", IN_PROGRESS, "stateJson", AcpJson.write(state))) != 1) {
            throw AcpException.conflict("checkout_in_progress", "The checkout session is being completed.");
        }
        row.set("status", IN_PROGRESS);

        // 2. the order of this attempt (kept from an attempt with an unknown outcome)
        String orderId = AcpJson.str(state, "pending_order_id");
        String sptHash = AcpJson.sha256Hex(spt).substring(0, 16);
        long attempt = state.get("pay_attempt") instanceof Number ? ((Number) state.get("pay_attempt")).longValue() : 0L;
        try {
            if (orderId != null && !sptHash.equals(state.get("pending_spt"))) {
                // another token after an unknown outcome: never charge twice for one order
                ChargeResult earlier = lookupPending(ctx, handler, state, orderId);
                if (earlier != null && earlier.status == ChargeResult.Status.SUCCEEDED) {
                    return finishPaid(ctx, handler, row, state, priced, orderId, earlier.reference, pendingTotal(state, row));
                }
                if (earlier == null) {
                    reopenState(ctx, row, state);
                    return withMessage(render(ctx, row, state, priced), AcpPricing.error("invalid", "$.payment_data",
                            "The earlier payment of this checkout is still being checked; retry with the same payment token.", "recoverable"));
                }
                cancelOrder(ctx, orderId, "A new payment token replaces an attempt that did not pay.");
                orderId = null;
                attempt++;
            }
            if (orderId == null) {
                try {
                    orderId = createOrder(ctx, row, state, priced, handler);
                } catch (AcpException | RuntimeException | GenericEntityException | GenericServiceException | CartItemModifyException e) {
                    Debug.logError(e, "ACP: order not created for checkout session " + id + ": " + e.getMessage(), module);
                    reopenState(ctx, row, state);
                    throw AcpException.processing("The order could not be placed.");
                }
                state.put("pending_order_id", orderId);
                state.put("pending_amount", priced.totalMinor);
                state.put("pending_since", System.currentTimeMillis());
                state.put("pending_spt", sptHash);
                state.put("pay_attempt", attempt);
                storeState(row, state);
            }
        } catch (GenericEntityException e) {
            reopenState(ctx, row, state);
            throw AcpException.processing("The checkout session could not be saved.");
        }

        // 3. charge; the PSP idempotency key is the same for every retry of this attempt
        ChargeRequest cr = new ChargeRequest();
        cr.checkoutSessionId = id;
        cr.orderId = orderId;
        cr.idempotencyKey = "acp-" + id + "-" + attempt + "-" + sptHash;
        cr.amountMinor = ((Number) state.get("pending_amount")).longValue();
        cr.currency = row.getString("currencyUomId");
        cr.sharedPaymentToken = spt;
        cr.buyerEmail = AcpJson.str(AcpJson.map(state.get("buyer")), "email");
        ChargeResult charge;
        try {
            charge = handler.charge(ctx, cr);
        } catch (RuntimeException e) {
            Debug.logError(e, "ACP: charge failed for order " + orderId + ": " + e.getMessage(), module);
            charge = ChargeResult.of(ChargeResult.Status.ERROR, null, "processing_error", null);
        }
        if (charge.status == ChargeResult.Status.SUCCEEDED) {
            return finishPaid(ctx, handler, row, state, priced, orderId, charge.reference, pendingTotal(state, row));
        }
        if (charge.status == ChargeResult.Status.ERROR) {
            // unknown outcome: keep the order and the attempt, a retry with the same token is safe
            reopenState(ctx, row, state);
            return withMessage(render(ctx, row, state, priced), AcpPricing.error("invalid", "$.payment_data",
                    "The payment service did not answer; retry the complete with the same payment token.", "recoverable"));
        }
        // a definite decline or an authentication that ACP cannot give: cancel this attempt
        if (charge.status == ChargeResult.Status.ACTION_REQUIRED && handler instanceof StripeSptHandler) {
            ((StripeSptHandler) handler).cancel(ctx, charge.reference);
        }
        cancelOrder(ctx, orderId, "Agent payment " + AcpPaymentHandler.nz(charge.reference) + " failed: " + AcpPaymentHandler.nz(charge.code));
        state.remove("pending_order_id");
        state.remove("pending_amount");
        state.remove("pending_since");
        state.remove("pending_spt");
        state.put("pay_attempt", attempt + 1);
        reopenState(ctx, row, state);
        String code = charge.status == ChargeResult.Status.ACTION_REQUIRED ? "requires_3ds" : "payment_declined";
        String text = charge.status == ChargeResult.Status.ACTION_REQUIRED ? "The card needs 3-D Secure, which this store does not support for agents yet."
                : "The payment was declined" + (UtilValidate.isNotEmpty(charge.code) && !"payment_declined".equals(charge.code) ? " (" + charge.code + ")." : ".");
        return withMessage(render(ctx, row, state, priced), AcpPricing.error(code, "$.payment_data", text, "requires_buyer_input"));
    }

    /** The money is taken: the session completes whatever the bookkeeping does; a bookkeeping fault becomes an order note. */
    private static Map<String, Object> finishPaid(AcpContext ctx, AcpPaymentHandler handler, GenericValue row, Map<String, Object> state,
            AcpPricing.Result priced, String orderId, String reference, BigDecimal total) {
        String currency = row.getString("currencyUomId");
        if (handler.recordsPayment()) {
            try {
                recordPayment(ctx, orderId, handler.paymentMethodTypeId(), reference, total, currency);
            } catch (GenericEntityException | GenericServiceException | RuntimeException e) {
                Debug.logError(e, "ACP: payment " + reference + " of order " + orderId + " is taken but not recorded: " + e.getMessage(), module);
                try {
                    ctx.getDispatcher().runSync("createOrderNote", UtilMisc.toMap("userLogin", ctx.getSystemLogin(), "orderId", orderId,
                            "internalNote", "Y", "note", "Agent card payment " + reference + " (" + total + " " + currency
                                    + ") is captured, but the payment could not be recorded. Record it by hand."));
                } catch (GenericEntityException | GenericServiceException e2) {
                    Debug.logError(e2, module);
                }
            }
        }
        state.remove("pending_order_id");
        state.remove("pending_amount");
        state.remove("pending_since");
        state.remove("pending_spt");
        state.remove("claimed_at");
        state.put("permalink_url", permalink(ctx, orderId));
        state.put("order_number", orderId);
        row.set("status", COMPLETED);
        row.set("orderId", orderId);
        row.set("paymentRef", reference);
        row.set("totalAmount", total);
        row.set("stateJson", AcpJson.write(state));
        try {
            row.store();
        } catch (GenericEntityException e) {
            Debug.logError(e, "ACP: order " + orderId + " is paid, but checkout session " + row.getString("checkoutSessionId") + " is not saved", module);
        }
        AcpWebhooks.enqueueOrderEvent(ctx.getDelegator(), row, AcpWebhooks.ORDER_CREATE);
        try {
            ctx.getDispatcher().runAsync("acpDeliverWebhooks", UtilMisc.toMap("userLogin", ctx.getSystemLogin()), false);
        } catch (GenericServiceException | GenericEntityException e) {
            Debug.logWarning("ACP: webhook delivery not started: " + e.getMessage(), module);
        }
        if (priced == null) {
            try {
                return render(ctx, row, state);
            } catch (AcpException e) {
                return new LinkedHashMap<>();
            }
        }
        return render(ctx, row, state, priced);
    }

    /** The total of the pending order (major units). */
    private static BigDecimal pendingTotal(Map<String, Object> state, GenericValue row) {
        Object m = state.get("pending_amount");
        return m instanceof Number ? AcpJson.fromMinor(((Number) m).longValue(), row.getString("currencyUomId"))
                : row.getBigDecimal("totalAmount");
    }

    /** The outcome of the pending attempt; after 30 minutes without a known success it counts as not paid. */
    static ChargeResult lookupPending(AcpContext ctx, AcpPaymentHandler handler, Map<String, Object> state, String orderId) {
        ChargeResult r = handler.lookup(ctx, orderId);
        Object since = state.get("pending_since");
        if (r == null && since instanceof Number && System.currentTimeMillis() - ((Number) since).longValue() > 1_800_000L) {
            return ChargeResult.of(ChargeResult.Status.DECLINED, null, "no_payment_found", null);
        }
        return r;
    }

    /**
     * Expiry job: expires open sessions past expiresDate. A session with a pending attempt is settled first: a found
     * payment completes it, a sure non-payment cancels the order; an unknown outcome waits for the next run.
     */
    public static int expireSessions(Delegator delegator, LocalDispatcher dispatcher) throws GenericEntityException {
        int n = 0;
        for (GenericValue row : EntityQuery.use(delegator).from(ENTITY).where(EntityCondition.makeCondition(
                EntityCondition.makeCondition("expiresDate", EntityOperator.LESS_THAN, UtilDateTime.nowTimestamp()), EntityOperator.AND,
                EntityCondition.makeCondition("status", EntityOperator.IN, List.of("not_ready_for_payment", "ready_for_payment", "incomplete"))))
                .maxRows(500).queryList()) {
            try {
                Map<String, Object> state = state(row);
                AcpContext ctx = new AcpContext(delegator, dispatcher, row.getString("productStoreId"), row.getString("webSiteId"),
                        row.getString("webappName"), AcpJson.str(state, "base_url"), row.getString("tokenId"), row.getString("userLoginId"), null);
                String pending = AcpJson.str(state, "pending_order_id");
                if (pending != null) {
                    AcpPaymentHandler handler = AcpPaymentHandler.forStore(ctx);
                    ChargeResult r = handler != null ? lookupPending(ctx, handler, state, pending) : null;
                    if (r == null) continue;
                    if (r.status == ChargeResult.Status.SUCCEEDED) {
                        finishPaid(ctx, handler, row, state, null, pending, r.reference, pendingTotal(state, row));
                        continue;
                    }
                    cancelOrder(ctx, pending, "The agent checkout expired without a payment.");
                    state.remove("pending_order_id");
                }
                n += storeIfOpen(ctx, row, UtilMisc.toMap("status", EXPIRED, "stateJson", AcpJson.write(state)));
            } catch (AcpException | RuntimeException e) {
                Debug.logWarning("ACP: session " + row.getString("checkoutSessionId") + " not expired: " + e.getMessage(), module);
            }
        }
        return n;
    }

    private static Map<String, Object> withMessage(Map<String, Object> session, Map<String, Object> message) {
        addMessage(session, message);
        return session;
    }

    private static void addMessage(Map<String, Object> session, Map<String, Object> message) {
        List<Object> messages = new ArrayList<>(AcpJson.list(session.get("messages")));
        messages.add(message);
        session.put("messages", messages);
    }

    static Map<String, Object> warning(String code, String param, String content) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("type", "warning");
        m.put("code", code);
        m.put("param", param);
        m.put("content_type", "plain");
        m.put("content", content);
        return m;
    }

    // ---------------------------------------------------------------- order

    private static String createOrder(AcpContext ctx, GenericValue row, Map<String, Object> state, AcpPricing.Result priced,
            AcpPaymentHandler handler) throws GenericEntityException, GenericServiceException, CartItemModifyException, AcpException {
        LocalDispatcher dispatcher = ctx.getDispatcher();
        GenericValue system = ctx.getSystemLogin();
        Map<String, Object> buyer = AcpJson.map(state.get("buyer"));
        String partyId = AcpParties.createBuyer(ctx, buyer, AcpJson.map(state.get("fulfillment_details")));
        ShoppingCart cart = priced.cart;
        cart.setUserLogin(system, dispatcher);
        cart.setOrderPartyId(partyId);
        cart.setPlacingCustomerPartyId(partyId);
        cart.setBillToCustomerPartyId(partyId);
        cart.setShipToCustomerPartyId(partyId);
        cart.setEndUserCustomerPartyId(partyId);
        cart.setChannelType("ACP_SALES_CHANNEL");
        cart.setExternalId(row.getString("checkoutSessionId"));
        cart.setOrderAttribute("acpCheckoutSessionId", row.getString("checkoutSessionId"));
        cart.setOrderAttribute("acpAgent", AcpPaymentHandler.nz(row.getString("userLoginId")));
        String notes = AcpJson.str(state, "order_notes");
        if (UtilValidate.isNotEmpty(notes)) {
            cart.addOrderNote(notes.length() > 2000 ? notes.substring(0, 2000) : notes);
        }
        if (priced.shipContactMechId != null) {
            AcpParties.linkAddress(ctx, partyId, priced.shipContactMechId, "SHIPPING_LOCATION");
        }
        Map<String, Object> billing = AcpJson.map(state.get("billing_address"));
        if (billing != null) {
            AcpParties.addPostalAddress(ctx, partyId, billing, "BILLING_LOCATION");
        }
        cart.clearPayments();
        cart.addPaymentAmount(handler.paymentMethodTypeId(), priced.total, true);
        CheckOutHelper helper = new CheckOutHelper(dispatcher, ctx.getDelegator(), cart);
        Map<String, Object> res = helper.createOrder(system, null, null, null, false, null, ctx.getWebSiteId());
        if (ServiceUtil.isError(res)) {
            Debug.logError("ACP: createOrder for checkout session " + row.getString("checkoutSessionId") + ": " + ServiceUtil.getErrorMessage(res), module);
            throw AcpException.unprocessable("order_failed", "The order could not be placed.", null);
        }
        return (String) res.get("orderId");
    }

    /** Records a payment that the store account took (StripeSptHandler): gateway response, payment, preference received, order approved. */
    static void recordPayment(AcpContext ctx, String orderId, String paymentMethodTypeId, String reference, BigDecimal amount, String currency)
            throws GenericEntityException, GenericServiceException {
        Delegator delegator = ctx.getDelegator();
        GenericValue pref = EntityQuery.use(delegator).from("OrderPaymentPreference")
                .where("orderId", orderId, "paymentMethodTypeId", paymentMethodTypeId).queryFirst();
        if (pref == null) {
            throw new GenericEntityException("Order " + orderId + " has no " + paymentMethodTypeId + " payment preference");
        }
        Timestamp now = UtilDateTime.nowTimestamp();
        String responseId = delegator.getNextSeqId("PaymentGatewayResponse");
        delegator.create("PaymentGatewayResponse", UtilMisc.toMap("paymentGatewayResponseId", responseId,
                "paymentServiceTypeEnumId", "PRDS_PAY_EXTERNAL", "orderPaymentPreferenceId", pref.getString("orderPaymentPreferenceId"),
                "paymentMethodTypeId", paymentMethodTypeId, "transCodeEnumId", "PGT_CAPTURE", "amount", amount,
                "currencyUomId", currency, "referenceNum", reference, "gatewayMessage", "Stripe (ACP shared payment token)", "transactionDate", now));
        pref.set("statusId", "PAYMENT_RECEIVED");
        pref.set("manualRefNum", reference);
        pref.store();
        GenericValue order = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
        GenericValue store = order.getRelatedOne("ProductStore", false);
        GenericValue placing = EntityQuery.use(delegator).from("OrderRole").where("orderId", orderId, "roleTypeId", "PLACING_CUSTOMER").queryFirst();
        delegator.create("Payment", UtilMisc.toMap("paymentId", delegator.getNextSeqId("Payment"), "paymentTypeId", "CUSTOMER_PAYMENT",
                "paymentMethodTypeId", paymentMethodTypeId, "paymentPreferenceId", pref.getString("orderPaymentPreferenceId"),
                "paymentGatewayResponseId", responseId, "partyIdFrom", placing == null ? "_NA_" : placing.getString("partyId"),
                "partyIdTo", store == null ? null : store.getString("payToPartyId"), "statusId", "PMNT_RECEIVED", "effectiveDate", now,
                "paymentRefNum", reference, "amount", amount, "currencyUomId", currency, "comments", "Card payment through an AI agent (ACP)"));
        if (!OrderChangeHelper.approveOrder(ctx.getDispatcher(), ctx.getSystemLogin(), orderId)) {
            Debug.logWarning("ACP: payment " + reference + " is recorded, but order " + orderId + " is not approved", module);
        }
    }

    private static void cancelOrder(AcpContext ctx, String orderId, String note) {
        try {
            GenericValue system = ctx.getSystemLogin();
            ctx.getDispatcher().runSync("createOrderNote", UtilMisc.toMap("userLogin", system, "orderId", orderId, "internalNote", "Y", "note", note));
            Map<String, Object> st = ctx.getDispatcher().runSync("changeOrderStatus", UtilMisc.toMap("orderId", orderId,
                    "statusId", "ORDER_CANCELLED", "setItemStatus", "Y", "userLogin", system));
            if (ServiceUtil.isError(st)) {
                Debug.logWarning("ACP: order " + orderId + " stays open: " + ServiceUtil.getErrorMessage(st), module);
            }
        } catch (GenericEntityException | GenericServiceException e) {
            Debug.logError(e, "ACP: could not cancel order " + orderId + ": " + e.getMessage(), module);
        }
    }

    /** Back from complete_in_progress to ready_for_payment, with the state of the attempt. */
    private static void reopenState(AcpContext ctx, GenericValue row, Map<String, Object> state) {
        state.remove("claimed_at");
        row.set("status", "ready_for_payment");
        row.set("stateJson", AcpJson.write(state));
        try {
            ctx.getDelegator().storeByCondition(ENTITY, UtilMisc.toMap("status", "ready_for_payment", "stateJson", row.getString("stateJson")),
                    EntityCondition.makeCondition(EntityCondition.makeCondition("checkoutSessionId", row.getString("checkoutSessionId")),
                            EntityOperator.AND, EntityCondition.makeCondition("status", IN_PROGRESS)));
        } catch (GenericEntityException e) {
            Debug.logError(e, "ACP: could not reopen checkout session " + row.getString("checkoutSessionId"), module);
        }
    }

    private static void storeState(GenericValue row, Map<String, Object> state) throws GenericEntityException {
        row.set("stateJson", AcpJson.write(state));
        row.store();
    }

    /** Writes the fields only while the session is open; returns the number of rows written (0 or 1). */
    static int storeIfOpen(AcpContext ctx, GenericValue row, Map<String, Object> fields) throws AcpException {
        try {
            int n = ctx.getDelegator().storeByCondition(ENTITY, fields, EntityCondition.makeCondition(
                    EntityCondition.makeCondition("checkoutSessionId", row.getString("checkoutSessionId")), EntityOperator.AND,
                    EntityCondition.makeCondition("status", EntityOperator.NOT_IN, List.of(IN_PROGRESS, COMPLETED, CANCELED, EXPIRED))));
            if (n == 1) row.setAllFields(fields, true, null, null);
            return n;
        } catch (GenericEntityException e) {
            throw AcpException.processing("The checkout session could not be saved.");
        }
    }

    static String permalink(AcpContext ctx, String orderId) {
        return ctx.url("/control/orderstatus?orderId=" + orderId);
    }

    // ---------------------------------------------------------------- session row

    static GenericValue load(AcpContext ctx, String id) throws AcpException {
        if (UtilValidate.isEmpty(id) || id.length() > 60) {
            throw AcpException.notFound("Checkout session not found.");
        }
        GenericValue row;
        try {
            row = EntityQuery.use(ctx.getDelegator()).from(ENTITY).where("checkoutSessionId", id).queryOne();
        } catch (GenericEntityException e) {
            throw AcpException.processing("The checkout session could not be read.");
        }
        // a session is only visible to the store and the token that made it
        if (row == null || !java.util.Objects.equals(row.getString("productStoreId"), ctx.getProductStoreId())
                || (ctx.getTokenId() != null && !ctx.getTokenId().equals(row.getString("tokenId")))) {
            throw AcpException.notFound("Checkout session not found.");
        }
        if (!isFinal(row) && !IN_PROGRESS.equals(row.getString("status")) && row.getTimestamp("expiresDate") != null
                && row.getTimestamp("expiresDate").before(UtilDateTime.nowTimestamp())
                && (row.getString("stateJson") == null || !row.getString("stateJson").contains("\"pending_order_id\""))) {
            storeIfOpen(ctx, row, UtilMisc.toMap("status", EXPIRED));
        }
        if (IN_PROGRESS.equals(row.getString("status"))) {
            // a complete that died (restart) gives the session back after 10 minutes; its order stays pending for a retry
            Map<String, Object> st = state(row);
            Object claimed = st.get("claimed_at");
            if (claimed instanceof Number && System.currentTimeMillis() - ((Number) claimed).longValue() > 600_000L) {
                reopenState(ctx, row, st);
            }
        }
        return row;
    }

    static boolean isFinal(GenericValue row) {
        String s = row.getString("status");
        return COMPLETED.equals(s) || CANCELED.equals(s) || EXPIRED.equals(s);
    }

    private static void requireOpen(GenericValue row) throws AcpException {
        String s = row.getString("status");
        if (COMPLETED.equals(s) || CANCELED.equals(s) || EXPIRED.equals(s)) {
            throw AcpException.conflict("invalid_state", "The checkout session is " + s + ".");
        }
        if (IN_PROGRESS.equals(s)) {
            throw AcpException.conflict("checkout_in_progress", "The checkout session is being completed.");
        }
    }

    static Map<String, Object> state(GenericValue row) throws AcpException {
        try {
            return AcpJson.parse(row.getString("stateJson"));
        } catch (java.io.IOException e) {
            throw AcpException.processing("The checkout session is damaged.");
        }
    }


    private static Map<String, Object> priceAndStore(AcpContext ctx, GenericValue row, Map<String, Object> state, boolean create) throws AcpException {
        AcpPricing.Result priced = AcpPricing.price(ctx, row, state);
        state.put("quoted_total", priced.totalMinor);
        Map<String, Object> fields = UtilMisc.toMap("status", priced.status, "shipContactMechId", priced.shipContactMechId,
                "totalAmount", priced.total, "stateJson", AcpJson.write(state));
        if (create) {
            row.setAllFields(fields, true, null, null);
            try {
                row.create();
            } catch (GenericEntityException e) {
                throw AcpException.processing("The checkout session could not be saved.");
            }
        } else if (storeIfOpen(ctx, row, fields) != 1) {
            throw AcpException.conflict("checkout_in_progress", "The checkout session changed; read it again.");
        }
        return render(ctx, row, state, priced);
    }

    static Map<String, Object> render(AcpContext ctx, GenericValue row, Map<String, Object> state) throws AcpException {
        return render(ctx, row, state, AcpPricing.price(ctx, row, state));
    }

    static Map<String, Object> render(AcpContext ctx, GenericValue row, Map<String, Object> state, AcpPricing.Result priced) {
        Map<String, Object> out = AcpPricing.toSession(ctx, row, state, priced);
        if (COMPLETED.equals(row.getString("status")) && row.getString("orderId") != null) {
            Map<String, Object> order = new LinkedHashMap<>();
            order.put("type", "order");
            order.put("id", row.getString("orderId"));
            order.put("checkout_session_id", row.getString("checkoutSessionId"));
            order.put("order_number", row.getString("orderId"));
            order.put("permalink_url", AcpJson.str(state, "permalink_url"));
            out.put("order", order);
        }
        return out;
    }

    // ---------------------------------------------------------------- request parsing

    /** [{id, quantity}] with the quantities of repeated ids added up. Accepts quantity on the item (2025 versions). */
    static List<Map<String, Object>> normalizeItems(List<Object> items, String param) throws AcpException {
        Map<String, Long> q = new LinkedHashMap<>();
        int i = 0;
        for (Object o : items) {
            Map<String, Object> m = AcpJson.map(o);
            String id = m == null ? null : AcpJson.str(m, "id");
            if (UtilValidate.isEmpty(id)) {
                throw AcpException.invalid("missing", "Every line item needs an id.", param + "[" + i + "].id");
            }
            long n = 1;
            if (m.get("quantity") != null) {
                try {
                    n = new BigDecimal(m.get("quantity").toString()).longValueExact();
                } catch (ArithmeticException | NumberFormatException e) {
                    throw AcpException.invalid("invalid", "quantity must be a whole number.", param + "[" + i + "].quantity");
                }
                if (n < 1 || n > 10000) {
                    throw AcpException.invalid("invalid", "quantity must be between 1 and 10000.", param + "[" + i + "].quantity");
                }
            }
            q.merge(id, n, Long::sum);
            i++;
        }
        if (q.size() > 100) {
            throw AcpException.invalid("invalid", "At most 100 different items.", param);
        }
        List<Map<String, Object>> out = new ArrayList<>();
        for (Map.Entry<String, Long> e : q.entrySet()) {
            out.add(UtilMisc.toMap("id", e.getKey(), "quantity", e.getValue()));
        }
        return out;
    }

    static List<String> discountCodes(Map<String, Object> body) {
        List<String> codes = new ArrayList<>();
        Map<String, Object> d = AcpJson.map(body.get("discounts"));
        for (Object c : AcpJson.list(d != null ? d.get("codes") : null)) {
            if (c != null && !codes.contains(c.toString().trim())) codes.add(c.toString().trim());
        }
        for (Object c : AcpJson.list(body.get("coupons"))) {
            if (c != null && !codes.contains(c.toString().trim())) codes.add(c.toString().trim());
        }
        return codes;
    }

    static String sharedPaymentToken(Map<String, Object> paymentData) {
        Map<String, Object> instrument = AcpJson.map(paymentData.get("instrument"));
        Map<String, Object> credential = instrument != null ? AcpJson.map(instrument.get("credential")) : null;
        String token = credential != null ? AcpJson.str(credential, "token") : null;
        if (token == null) token = AcpJson.str(paymentData, "token"); // 2025 versions: {token, provider}
        return token == null ? null : token.trim();
    }

    private static void copyIfPresent(Map<String, Object> from, Map<String, Object> to, String... keys) {
        for (String k : keys) {
            if (from.get(k) != null) to.put(k, from.get(k));
        }
    }

    private static Map<String, Object> mergeMaps(Map<String, Object> a, Map<String, Object> b) {
        Map<String, Object> m = new LinkedHashMap<>();
        if (a != null) m.putAll(a);
        if (b != null) m.putAll(b);
        return m;
    }

    private static void rollback(boolean began, Exception e) {
        try {
            TransactionUtil.rollback(began, "ACP complete: " + e.getMessage(), e);
        } catch (org.ofbiz.entity.transaction.GenericTransactionException te) {
            Debug.logError(te, module);
        }
    }

    static String iso(Timestamp t) {
        return t == null ? null : Instant.ofEpochMilli(t.getTime()).toString();
    }
}

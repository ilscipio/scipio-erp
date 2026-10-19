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
package com.ilscipio.scipio.acp.webhook;

import java.io.IOException;
import java.math.BigDecimal;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.sql.Timestamp;
import java.time.Duration;
import java.time.Instant;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
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
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.order.order.OrderReadHelper;

import com.ilscipio.scipio.acp.AcpConfig;
import com.ilscipio.scipio.acp.AcpJson;

/**
 * SCIPIO: 4.0.0: Signed order events for the agent (ACP order webhooks). An event is a full Order object
 * ({@code {type: order_create | order_update, data: Order}}), sent with
 * {@code Merchant-Signature: t=<unix seconds>,v1=<hex HMAC-SHA256 of "<t>.<body>" with webhook.secret>}.
 * Events are written to AcpWebhookDelivery first; the job acpDeliverWebhooks sends them and retries with backoff.
 */
public final class AcpWebhooks {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String ORDER_CREATE = "order_create";
    public static final String ORDER_UPDATE = "order_update";
    public static final String ENTITY = "AcpWebhookDelivery";

    private static final HttpClient HTTP = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build();

    private AcpWebhooks() {}

    /** Writes one event for the session of the order; nothing when the store has no webhook url. */
    public static void enqueueOrderEvent(Delegator delegator, GenericValue session, String type) {
        String storeId = session.getString("productStoreId");
        String url = AcpConfig.get(delegator, storeId, "webhook.url", null);
        if (UtilValidate.isEmpty(url) || UtilValidate.isEmpty(session.getString("orderId"))) {
            return;
        }
        try {
            Map<String, Object> event = new LinkedHashMap<>();
            event.put("type", type);
            event.put("data", order(delegator, session));
            Timestamp now = UtilDateTime.nowTimestamp();
            delegator.create(ENTITY, UtilMisc.toMap("deliveryId", delegator.getNextSeqId(ENTITY), "checkoutSessionId",
                    session.getString("checkoutSessionId"), "orderId", session.getString("orderId"), "productStoreId", storeId,
                    "eventType", type, "payload", AcpJson.write(event), "status", "PENDING", "attempts", 0L, "nextAttemptDate", now,
                    "createdDate", now));
        } catch (GenericEntityException | RuntimeException e) {
            Debug.logError(e, "ACP: webhook event " + type + " for order " + session.getString("orderId") + " not written: " + e.getMessage(), module);
        }
    }

    /** Writes an order_update event when the order came from an ACP checkout session. */
    public static void orderChanged(Delegator delegator, String orderId) {
        if (UtilValidate.isEmpty(orderId)) return;
        try {
            GenericValue session = EntityQuery.use(delegator).from("AcpCheckoutSession").where("orderId", orderId).queryFirst();
            if (session != null) {
                enqueueOrderEvent(delegator, session, ORDER_UPDATE);
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "ACP: order " + orderId + " update not queued: " + e.getMessage(), module);
        }
    }

    /** Sends the due events. Returns the number sent. */
    public static int deliver(Delegator delegator, int max) throws GenericEntityException {
        List<GenericValue> due = EntityQuery.use(delegator).from(ENTITY).where(EntityCondition.makeCondition(
                EntityCondition.makeCondition("status", "PENDING"), EntityOperator.AND,
                EntityCondition.makeCondition("nextAttemptDate", EntityOperator.LESS_THAN_EQUAL_TO, UtilDateTime.nowTimestamp())))
                .orderBy("createdDate").maxRows(max).queryList();
        int sent = 0;
        for (GenericValue d : due) {
            // claim: another sender that read the same row finds it not due any more (10 minutes)
            int claimed = delegator.storeByCondition(ENTITY, UtilMisc.toMap("nextAttemptDate", new Timestamp(System.currentTimeMillis() + 600_000L)),
                    EntityCondition.makeCondition(EntityCondition.makeCondition("deliveryId", d.getString("deliveryId")), EntityOperator.AND,
                            EntityCondition.makeCondition(EntityCondition.makeCondition("status", "PENDING"), EntityOperator.AND,
                                    EntityCondition.makeCondition("nextAttemptDate", d.getTimestamp("nextAttemptDate")))));
            if (claimed != 1) continue;
            String storeId = d.getString("productStoreId");
            String url = AcpConfig.get(delegator, storeId, "webhook.url", null);
            String secret = AcpConfig.get(delegator, storeId, "webhook.secret", null);
            long attempts = d.getLong("attempts") != null ? d.getLong("attempts") : 0L;
            String error = null;
            if (UtilValidate.isEmpty(url) || UtilValidate.isEmpty(secret)) {
                error = "webhook.url or webhook.secret is not set";
            } else {
                error = post(url, secret, d.getString("payload"), "whd_" + d.getString("deliveryId"));
            }
            attempts++;
            d.set("attempts", attempts);
            if (error == null) {
                d.set("status", "SENT");
                d.set("sentDate", UtilDateTime.nowTimestamp());
                d.set("lastError", null);
                sent++;
            } else {
                d.set("lastError", error.length() > 250 ? error.substring(0, 250) : error);
                if (attempts >= AcpConfig.getInt(delegator, "webhook.maxAttempts", 8)) {
                    d.set("status", "PARKED");
                    Debug.logWarning("ACP: webhook " + d.getString("deliveryId") + " parked after " + attempts + " attempts: " + error, module);
                } else {
                    d.set("nextAttemptDate", new Timestamp(System.currentTimeMillis() + backoffMillis(attempts)));
                }
            }
            d.store();
        }
        return sent;
    }

    /** 1, 2, 4, 8 ... minutes, at most 6 hours. */
    static long backoffMillis(long attempts) {
        long minutes = 1L << Math.min(attempts - 1, 9);
        return Math.min(minutes, 360L) * 60_000L;
    }

    /** The Merchant-Signature header value. Pure, for tests. */
    public static String signature(String secret, long unixSeconds, String body) {
        return "t=" + unixSeconds + ",v1=" + AcpJson.hex(AcpJson.hmacSha256(secret, unixSeconds + "." + body));
    }

    /** requestId is the same for every attempt of one event, so that the agent can drop repeats. */
    private static String post(String url, String secret, String body, String requestId) {
        long t = Instant.now().getEpochSecond();
        try {
            HttpRequest r = HttpRequest.newBuilder(URI.create(url))
                    .timeout(Duration.ofSeconds(20))
                    .header("Content-Type", "application/json")
                    .header("Merchant-Signature", signature(secret, t, body))
                    .header("Timestamp", Instant.ofEpochSecond(t).toString())
                    .header("Request-Id", requestId)
                    .POST(HttpRequest.BodyPublishers.ofString(body, StandardCharsets.UTF_8))
                    .build();
            HttpResponse<String> res = HTTP.send(r, HttpResponse.BodyHandlers.ofString());
            return res.statusCode() >= 200 && res.statusCode() < 300 ? null : "HTTP " + res.statusCode();
        } catch (IOException | IllegalArgumentException e) {
            return e.getClass().getSimpleName() + ": " + e.getMessage();
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return "interrupted";
        }
    }

    // ---------------------------------------------------------------- the Order object

    public static Map<String, Object> order(Delegator delegator, GenericValue session) throws GenericEntityException {
        String orderId = session.getString("orderId");
        GenericValue header = EntityQuery.use(delegator).from("OrderHeader").where("orderId", orderId).queryOne();
        OrderReadHelper orh = new OrderReadHelper(header);
        String cur = header.getString("currencyUom");
        Map<String, Object> state;
        try {
            state = AcpJson.parse(session.getString("stateJson"));
        } catch (IOException e) {
            state = new LinkedHashMap<>();
        }
        Map<String, Object> o = new LinkedHashMap<>();
        o.put("type", "order");
        o.put("id", orderId);
        o.put("checkout_session_id", session.getString("checkoutSessionId"));
        o.put("order_number", orderId);
        o.put("permalink_url", AcpJson.str(state, "permalink_url"));
        o.put("status", status(header.getString("statusId")));

        List<Map<String, Object>> lines = new ArrayList<>();
        for (GenericValue item : orh.getOrderItems()) {
            BigDecimal qty = item.getBigDecimal("quantity") != null ? item.getBigDecimal("quantity") : BigDecimal.ZERO;
            BigDecimal cancelled = item.getBigDecimal("cancelQuantity") != null ? item.getBigDecimal("cancelQuantity") : BigDecimal.ZERO;
            BigDecimal shipped = orh.getItemShippedQuantity(item);
            Map<String, Object> li = new LinkedHashMap<>();
            li.put("id", item.getString("orderItemSeqId"));
            li.put("title", UtilValidate.isNotEmpty(item.getString("itemDescription")) ? item.getString("itemDescription") : item.getString("productId"));
            if (item.getString("productId") != null) li.put("product_id", item.getString("productId"));
            li.put("quantity", UtilMisc.toMap("ordered", qty.intValue(), "current", qty.subtract(cancelled).intValue(),
                    "fulfilled", shipped != null ? shipped.intValue() : 0));
            if (item.getBigDecimal("unitPrice") != null) li.put("unit_price", AcpJson.toMinor(item.getBigDecimal("unitPrice"), cur));
            li.put("subtotal", AcpJson.toMinor(OrderReadHelper.getOrderItemSubTotal(item, orh.getAdjustments()), cur));
            li.put("status", status(item.getString("statusId")));
            lines.add(li);
        }
        o.put("line_items", lines);

        List<Map<String, Object>> fulfillments = new ArrayList<>();
        for (GenericValue sh : EntityQuery.use(delegator).from("Shipment").where("primaryOrderId", orderId).queryList()) {
            Map<String, Object> f = new LinkedHashMap<>();
            f.put("id", sh.getString("shipmentId"));
            f.put("type", "shipping");
            f.put("status", shipmentStatus(sh.getString("statusId")));
            GenericValue seg = EntityQuery.use(delegator).from("ShipmentRouteSegment").where("shipmentId", sh.getString("shipmentId")).queryFirst();
            String tracking = seg != null ? seg.getString("trackingIdNumber") : null;
            if (UtilValidate.isEmpty(tracking)) {
                GenericValue pkg = EntityQuery.use(delegator).from("ShipmentPackageRouteSeg").where("shipmentId", sh.getString("shipmentId")).queryFirst();
                tracking = pkg != null ? pkg.getString("trackingCode") : null;
            }
            if (seg != null && UtilValidate.isNotEmpty(seg.getString("carrierPartyId")) && !"_NA_".equals(seg.getString("carrierPartyId"))) {
                f.put("carrier", seg.getString("carrierPartyId"));
            }
            if (UtilValidate.isNotEmpty(tracking)) f.put("tracking_number", tracking);
            fulfillments.add(f);
        }
        o.put("fulfillments", fulfillments);

        List<Map<String, Object>> adjustments = new ArrayList<>();
        for (GenericValue pgr : EntityQuery.use(delegator).from("PaymentGatewayResponse").where(EntityCondition.makeCondition(
                EntityCondition.makeCondition("orderPaymentPreferenceId", EntityOperator.IN, prefIds(delegator, orderId)), EntityOperator.AND,
                EntityCondition.makeCondition("transCodeEnumId", "PGT_REFUND"))).queryList()) {
            Map<String, Object> a = new LinkedHashMap<>();
            a.put("id", pgr.getString("paymentGatewayResponseId"));
            a.put("type", "refund");
            a.put("occurred_at", pgr.getTimestamp("transactionDate") != null ? Instant.ofEpochMilli(pgr.getTimestamp("transactionDate").getTime()).toString()
                    : Instant.now().toString());
            a.put("status", "completed");
            a.put("amount", AcpJson.toMinor(pgr.getBigDecimal("amount"), cur));
            a.put("currency", cur.toLowerCase(java.util.Locale.ROOT));
            adjustments.add(a);
        }
        o.put("adjustments", adjustments);

        List<Map<String, Object>> totals = new ArrayList<>();
        totals.add(total("subtotal", "Subtotal", AcpJson.toMinor(orh.getOrderItemsSubTotal(), cur)));
        totals.add(total("fulfillment", "Shipping", AcpJson.toMinor(orh.getShippingTotal(), cur)));
        totals.add(total("tax", "Tax", AcpJson.toMinor(orh.getTaxTotal(), cur)));
        totals.add(total("total", "Total", AcpJson.toMinor(orh.getOrderGrandTotal(), cur)));
        o.put("totals", totals);
        return o;
    }

    private static List<String> prefIds(Delegator delegator, String orderId) throws GenericEntityException {
        List<String> ids = new ArrayList<>();
        for (GenericValue p : EntityQuery.use(delegator).from("OrderPaymentPreference").where("orderId", orderId).queryList()) {
            ids.add(p.getString("orderPaymentPreferenceId"));
        }
        if (ids.isEmpty()) ids.add("_NONE_");
        return ids;
    }

    static String status(String statusId) {
        if (statusId == null) return "created";
        switch (statusId) {
            case "ORDER_CREATED": case "ITEM_CREATED": return "created";
            case "ORDER_APPROVED": case "ITEM_APPROVED": return "confirmed";
            case "ORDER_PROCESSING": return "processing";
            case "ORDER_HOLD": return "on_hold";
            case "ORDER_SENT": return "shipped";
            case "ORDER_COMPLETED": case "ITEM_COMPLETED": return "fulfilled";
            case "ORDER_CANCELLED": case "ITEM_CANCELLED": return "canceled";
            case "ORDER_REJECTED": case "ITEM_REJECTED": return "canceled";
            default: return statusId.toLowerCase(java.util.Locale.ROOT);
        }
    }

    static String shipmentStatus(String statusId) {
        if (statusId == null) return "pending";
        switch (statusId) {
            case "SHIPMENT_SHIPPED": return "shipped";
            case "SHIPMENT_DELIVERED": return "delivered";
            case "SHIPMENT_CANCELLED": return "canceled";
            default: return "pending";
        }
    }

    private static Map<String, Object> total(String type, String text, long amount) {
        Map<String, Object> t = new LinkedHashMap<>();
        t.put("type", type);
        t.put("display_text", text);
        t.put("amount", amount);
        return t;
    }
}

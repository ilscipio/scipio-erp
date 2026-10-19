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
package com.ilscipio.scipio.acp.payment;

import java.io.IOException;
import java.net.URI;
import java.net.URLEncoder;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.LinkedHashMap;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;

import com.ilscipio.scipio.acp.AcpConfig;
import com.ilscipio.scipio.acp.AcpJson;
import com.ilscipio.scipio.acp.checkout.AcpContext;

/**
 * SCIPIO: 4.0.0: Charges a Stripe Shared Payment Token (spt_...) on the Stripe account of the store: one
 * PaymentIntent with {@code payment_method_data[shared_payment_granted_token]} and {@code confirm=true}. Plain HTTPS,
 * no Stripe SDK. The key comes from {@code stripe.secretKey} (acp.properties, SystemProperty or ACP_STRIPE_SECRET_KEY).
 */
public class StripeSptHandler implements AcpPaymentHandler {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String PAYMENT_METHOD_TYPE_ID = "EXT_ACP_STRIPE";

    private static final HttpClient HTTP = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build();

    public static boolean isConfigured(AcpContext ctx) {
        return UtilValidate.isNotEmpty(secretKey(ctx));
    }

    static String secretKey(AcpContext ctx) {
        return AcpConfig.get(ctx.getDelegator(), ctx.getProductStoreId(), "stripe.secretKey", null);
    }

    @Override
    public String name() { return "stripe"; }

    @Override
    public String paymentMethodTypeId() { return PAYMENT_METHOD_TYPE_ID; }

    @Override
    public boolean recordsPayment() { return true; }

    @Override
    public Map<String, Object> config(AcpContext ctx) {
        Map<String, Object> c = new LinkedHashMap<>();
        String account = AcpConfig.get(ctx.getDelegator(), ctx.getProductStoreId(), "stripe.accountId", null);
        c.put("merchant_id", UtilValidate.isNotEmpty(account) ? account : ctx.getProductStoreId());
        c.put("psp", "stripe");
        String key = secretKey(ctx);
        c.put("environment", key != null && key.contains("_test_") ? "test" : "production");
        return c;
    }

    @Override
    public ChargeResult charge(AcpContext ctx, ChargeRequest req) {
        String key = secretKey(ctx);
        if (UtilValidate.isEmpty(key)) {
            return ChargeResult.of(ChargeResult.Status.ERROR, null, "not_configured", "The store has no Stripe key.");
        }
        Map<String, String> form = new LinkedHashMap<>();
        form.put("amount", String.valueOf(req.amountMinor));
        form.put("currency", req.currency.toLowerCase(Locale.ROOT));
        form.put("payment_method_data[shared_payment_granted_token]", req.sharedPaymentToken);
        form.put("confirm", "true");
        form.put("description", "Order " + req.orderId);
        form.put("metadata[order_id]", req.orderId);
        form.put("metadata[acp_checkout_session_id]", req.checkoutSessionId);
        form.put("metadata[product_store_id]", AcpPaymentHandler.nz(ctx.getProductStoreId()));
        if (UtilValidate.isNotEmpty(req.buyerEmail)) {
            form.put("receipt_email", req.buyerEmail);
        }
        try {
            Map<String, Object> pi = post(ctx, key, "/v1/payment_intents", form, req.idempotencyKey);
            return result(pi);
        } catch (IOException e) {
            Debug.logError(e, "ACP: Stripe call failed for order " + req.orderId + ": " + e.getMessage(), module);
            return ChargeResult.of(ChargeResult.Status.ERROR, null, "processing_error", "The payment service is not reachable.");
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return ChargeResult.of(ChargeResult.Status.ERROR, null, "processing_error", "The payment call was interrupted.");
        }
    }

    /** Maps a PaymentIntent (or a Stripe error answer) to a result. Pure, for tests. */
    static ChargeResult result(Map<String, Object> pi) {
        Map<String, Object> error = AcpJson.map(pi.get("error"));
        if (error != null) {
            String code = AcpJson.str(error, "decline_code");
            if (code == null) code = AcpJson.str(error, "code");
            Map<String, Object> failedPi = AcpJson.map(error.get("payment_intent"));
            String ref = failedPi != null ? AcpJson.str(failedPi, "id") : null;
            String type = AcpJson.str(error, "type");
            ChargeResult.Status st = isDefiniteRefusal(type, AcpJson.str(error, "code")) ? ChargeResult.Status.DECLINED : ChargeResult.Status.ERROR;
            return ChargeResult.of(st, ref, code != null ? code : "payment_declined", AcpJson.str(error, "message"));
        }
        String status = AcpJson.str(pi, "status");
        String id = AcpJson.str(pi, "id");
        if ("succeeded".equals(status) || "processing".equals(status)) {
            return ChargeResult.of(ChargeResult.Status.SUCCEEDED, id, status, null);
        }
        if ("requires_action".equals(status)) {
            return ChargeResult.of(ChargeResult.Status.ACTION_REQUIRED, id, "requires_3ds", "The card needs authentication.");
        }
        Map<String, Object> last = AcpJson.map(pi.get("last_payment_error"));
        String code = last != null ? AcpJson.str(last, "decline_code") : null;
        return ChargeResult.of(ChargeResult.Status.DECLINED, id, code != null ? code : "payment_declined",
                last != null ? AcpJson.str(last, "message") : "The payment was not completed (" + status + ").");
    }

    /** Finds a PaymentIntent of the order by its metadata (Stripe search): succeeded, or declined when none succeeded. */
    @Override
    public ChargeResult lookup(AcpContext ctx, String orderId) {
        ChargeResult local = AcpPaymentHandler.super.lookup(ctx, orderId);
        if (local != null) return local;
        String key = secretKey(ctx);
        if (UtilValidate.isEmpty(key)) return null;
        try {
            String base = AcpConfig.get(ctx.getDelegator(), ctx.getProductStoreId(), "stripe.apiBase", "https://api.stripe.com");
            String q = URLEncoder.encode("metadata['order_id']:'" + orderId.replace("'", "") + "'", StandardCharsets.UTF_8);
            HttpRequest r = HttpRequest.newBuilder(URI.create(stripSlash(base) + "/v1/payment_intents/search?query=" + q))
                    .timeout(Duration.ofSeconds(20)).header("Authorization", "Bearer " + key).GET().build();
            HttpResponse<String> res = HTTP.send(r, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8));
            if (res.statusCode() != 200) return null;
            for (Object o : AcpJson.list(AcpJson.parse(res.body()).get("data"))) {
                Map<String, Object> pi = AcpJson.map(o);
                String st = AcpJson.str(pi, "status");
                if ("succeeded".equals(st) || "processing".equals(st)) {
                    return ChargeResult.of(ChargeResult.Status.SUCCEEDED, AcpJson.str(pi, "id"), st, null);
                }
            }
            // Stripe search is eventually consistent (about a minute): no success is not yet a decline
            return null;
        } catch (IOException e) {
            return null;
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return null;
        }
    }

    /**
     * A Stripe error that surely took no money: a card error, or a request error that is not about concurrency or
     * idempotency (lock_timeout, idempotency_key_in_use, rate limits). Everything else leaves the outcome open.
     */
    static boolean isDefiniteRefusal(String type, String code) {
        if ("card_error".equals(type)) return true;
        if (!"invalid_request_error".equals(type)) return false;
        return code == null || !(code.equals("lock_timeout") || code.startsWith("idempotency") || code.startsWith("rate_limit"));
    }

    /** Cancels a PaymentIntent that waits for an action that ACP cannot give (3DS without delegate authentication). */
    public void cancel(AcpContext ctx, String paymentIntentId) {
        String key = secretKey(ctx);
        if (UtilValidate.isEmpty(key) || UtilValidate.isEmpty(paymentIntentId)) return;
        try {
            post(ctx, key, "/v1/payment_intents/" + URLEncoder.encode(paymentIntentId, StandardCharsets.UTF_8) + "/cancel",
                    new LinkedHashMap<>(), null);
        } catch (IOException e) {
            Debug.logWarning("ACP: could not cancel PaymentIntent " + paymentIntentId + ": " + e.getMessage(), module);
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
        }
    }

    static Map<String, Object> post(AcpContext ctx, String key, String path, Map<String, String> form, String idempotencyKey)
            throws IOException, InterruptedException {
        String base = AcpConfig.get(ctx.getDelegator(), ctx.getProductStoreId(), "stripe.apiBase", "https://api.stripe.com");
        String version = AcpConfig.get(ctx.getDelegator(), ctx.getProductStoreId(), "stripe.apiVersion", null);
        HttpRequest.Builder b = HttpRequest.newBuilder(URI.create(stripSlash(base) + path))
                .timeout(Duration.ofSeconds(40))
                .header("Authorization", "Bearer " + key)
                .header("Content-Type", "application/x-www-form-urlencoded")
                .POST(HttpRequest.BodyPublishers.ofString(encode(form)));
        if (UtilValidate.isNotEmpty(version)) b.header("Stripe-Version", version);
        if (idempotencyKey != null) b.header("Idempotency-Key", idempotencyKey);
        HttpResponse<String> res = HTTP.send(b.build(), HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8));
        Map<String, Object> body = AcpJson.parse(res.body());
        if (res.statusCode() >= 500) {
            throw new IOException("Stripe answered " + res.statusCode());
        }
        return body;
    }

    static String encode(Map<String, String> form) {
        StringBuilder sb = new StringBuilder();
        for (Map.Entry<String, String> e : form.entrySet()) {
            if (e.getValue() == null) continue;
            if (sb.length() > 0) sb.append('&');
            sb.append(URLEncoder.encode(e.getKey(), StandardCharsets.UTF_8)).append('=')
                    .append(URLEncoder.encode(e.getValue(), StandardCharsets.UTF_8));
        }
        return sb.toString();
    }

    private static String stripSlash(String s) {
        return s.endsWith("/") ? s.substring(0, s.length() - 1) : s;
    }
}

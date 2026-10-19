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
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.time.Duration;
import java.util.LinkedHashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.util.EntityUtilProperties;

import com.ilscipio.scipio.acp.AcpJson;
import com.ilscipio.scipio.acp.checkout.AcpContext;
import com.ilscipio.scipio.order.payment.HubCheckout;

/**
 * SCIPIO: 4.0.0: Charges a Stripe Shared Payment Token through the Scipio hub (hosted stores, EXT_STRIPE_HUB). The
 * store signs the claims of the order with its checkout key (as for the pay step) plus {@code kind=acp_spt} and the
 * SHA-256 of the token, and posts {@code {token, shared_payment_token}} to {@code <hub url>/<storeId>/spt}. The hub
 * confirms a PaymentIntent on the connected account; its Stripe webhook records the payment on the order later
 * (recordHubPayment), exactly as for a card payment on the order page.
 */
public class HubSptHandler implements AcpPaymentHandler {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String KIND = "acp_spt";

    private static final HttpClient HTTP = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build();

    public static boolean isConfigured(AcpContext ctx) {
        return AcpPaymentHandler.hubSettingsPresent(ctx);
    }

    @Override
    public String name() { return "hub"; }

    @Override
    public String paymentMethodTypeId() { return HubCheckout.PAYMENT_METHOD_TYPE_ID; }

    @Override
    public boolean recordsPayment() { return false; }

    @Override
    public Map<String, Object> config(AcpContext ctx) {
        Map<String, Object> c = new LinkedHashMap<>();
        c.put("merchant_id", EntityUtilProperties.getPropertyValue(HubCheckout.RESOURCE, "storeId", ctx.getDelegator()));
        c.put("psp", "stripe");
        return c;
    }

    /** The signed claims of one SPT charge. Pure apart from the clock, for tests. */
    public static Map<String, Object> claims(String storeId, ChargeRequest req, String origin, long nowEpochSeconds) {
        Map<String, Object> c = HubCheckout.claims(storeId, req.orderId, AcpJson.fromMinor(req.amountMinor, req.currency), req.currency,
                origin, req.buyerEmail, nowEpochSeconds);
        c.put("kind", KIND);
        c.put("session", req.checkoutSessionId);
        c.put("spt", AcpJson.sha256Hex(req.sharedPaymentToken));
        return c;
    }

    @Override
    public ChargeResult charge(AcpContext ctx, ChargeRequest req) {
        String storeId = EntityUtilProperties.getPropertyValue(HubCheckout.RESOURCE, "storeId", ctx.getDelegator());
        String url = EntityUtilProperties.getPropertyValue(HubCheckout.RESOURCE, "url", ctx.getDelegator());
        String key = HubCheckout.checkoutKey(ctx.getDelegator());
        if (UtilValidate.isEmpty(storeId) || UtilValidate.isEmpty(url) || key == null) {
            return ChargeResult.of(ChargeResult.Status.ERROR, null, "not_configured", "The store has no hub checkout settings.");
        }
        Map<String, Object> body = new LinkedHashMap<>();
        body.put("token", HubCheckout.sign(claims(storeId, req, ctx.getBaseUrl(), System.currentTimeMillis() / 1000), key));
        body.put("shared_payment_token", req.sharedPaymentToken);
        String target = (url.endsWith("/") ? url.substring(0, url.length() - 1) : url) + "/" + storeId + "/spt";
        try {
            HttpRequest r = HttpRequest.newBuilder(URI.create(target))
                    .timeout(Duration.ofSeconds(45))
                    .header("Content-Type", "application/json")
                    .header("Idempotency-Key", req.idempotencyKey)
                    .POST(HttpRequest.BodyPublishers.ofString(AcpJson.write(body), StandardCharsets.UTF_8))
                    .build();
            HttpResponse<String> res = HTTP.send(r, HttpResponse.BodyHandlers.ofString(StandardCharsets.UTF_8));
            Map<String, Object> out = AcpJson.parse(res.body());
            if (res.statusCode() >= 500) {
                return ChargeResult.of(ChargeResult.Status.ERROR, null, "processing_error", "The hub answered " + res.statusCode() + ".");
            }
            return result(res.statusCode(), out);
        } catch (IOException e) {
            Debug.logError(e, "ACP: hub call failed for order " + req.orderId + ": " + e.getMessage(), module);
            return ChargeResult.of(ChargeResult.Status.ERROR, null, "processing_error", "The payment service is not reachable.");
        } catch (InterruptedException e) {
            Thread.currentThread().interrupt();
            return ChargeResult.of(ChargeResult.Status.ERROR, null, "processing_error", "The payment call was interrupted.");
        }
    }

    /** Maps the hub answer {status, payment_intent, failure_code, message}. Pure, for tests. */
    static ChargeResult result(int httpStatus, Map<String, Object> out) {
        String status = AcpJson.str(out, "status");
        String pi = AcpJson.str(out, "payment_intent");
        String code = AcpJson.str(out, "failure_code");
        String message = AcpJson.str(out, "message");
        if (httpStatus >= 400 && status == null) {
            // the hub refused before any charge (token, limits, account)
            return ChargeResult.of(ChargeResult.Status.DECLINED, null, code != null ? code : "hub_refused",
                    message != null ? message : "The hub refused the payment (" + httpStatus + ").");
        }
        if ("succeeded".equals(status) || "processing".equals(status)) {
            return ChargeResult.of(ChargeResult.Status.SUCCEEDED, pi, status, null);
        }
        if ("requires_action".equals(status)) {
            return ChargeResult.of(ChargeResult.Status.ACTION_REQUIRED, pi, "requires_3ds", "The card needs authentication.");
        }
        return ChargeResult.of(ChargeResult.Status.DECLINED, pi, code != null ? code : "payment_declined",
                message != null ? message : "The payment was declined.");
    }
}

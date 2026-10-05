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
import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.util.Base64;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.order.order.OrderReadHelper;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Card payment through the hub (payment method type {@code EXT_STRIPE_HUB}, work package W1-10d).
 *
 * <p>A hosted store pod holds no platform secret. After the order is placed, the pay step of the order page carries a
 * checkout token: the order, the amount, the currency, the store and the origin, signed with HMAC-SHA256 and the
 * checkout key of this store. The browser posts the token to the hub ({@code POST /hub/checkout/{store}/intent}). The hub
 * checks the signature with its copy of the key, creates the PaymentIntent on the connected Stripe account of the store and
 * gives the client secret to Stripe.js. The Stripe webhook reaches the hub; the desk records the payment in the store over
 * MCP ({@code order/order payment_hub_record}, {@link HubPaymentServices}).</p>
 *
 * <p>Token: {@code v1.<base64url(JSON claims)>.<base64url(HMAC-SHA256(key, "v1." + base64url(JSON claims)))>}. The key is
 * the UTF-8 text of the checkout key. Claims: {@code store}, {@code orderId}, {@code amount} (plain decimal), {@code currency},
 * {@code origin}, {@code exp} (epoch seconds), optional {@code email}.</p>
 *
 * <p>Settings (SystemProperty of the store database, resource {@code hubcheckout}; the desk writes them when it makes the
 * store): {@code storeId}, {@code key}, {@code url} (the base address of the hub route, without the store).</p>
 *
 * <p>SCIPIO: 4.0.0: Added (W1-10d).</p>
 */
public final class HubCheckout {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String PAYMENT_METHOD_TYPE_ID = "EXT_STRIPE_HUB";
    /** The answer of checkExternalPayment for this type (EXT_ removed, lower case). */
    public static final String EXTERNAL_TYPE = "stripe_hub";
    public static final String RESOURCE = "hubcheckout";
    /** The session attribute with the order IDs that this session placed with this type. */
    public static final String SESSION_ORDERS = "scpHubCheckoutOrderIds";
    /** The lifetime of a token. The hub refuses a token that is older. */
    public static final long TTL_SECONDS = 3600;
    /** The permission of the record services: only the MCP login of the desk has it (group HUBPAY_DESK), not the owner. */
    public static final String PERMISSION_RECORD = "HUBPAY_RECORD";
    /** The kinds of the record MAC. */
    public static final String KIND_PAYMENT = "payment";
    public static final String KIND_PAYMENT_FAILED = "payment_failed";
    public static final String KIND_REFUND = "refund";
    /** The request parameter of the signed pay link in the order mail. */
    public static final String PAY_LINK_PARAM = "pay";

    private static final String VERSION = "v1";
    private static final ObjectMapper JSON = new ObjectMapper();
    private static final Base64.Encoder B64 = Base64.getUrlEncoder().withoutPadding();
    private static final Base64.Decoder B64D = Base64.getUrlDecoder();

    private HubCheckout() {}

    // ---------------------------------------------------------------- token

    /** Signs the claims. The order of the claims is kept. */
    public static String sign(Map<String, Object> claims, String key) {
        if (UtilValidate.isEmpty(key)) {
            throw new IllegalArgumentException("No checkout key");
        }
        try {
            String payload = B64.encodeToString(JSON.writeValueAsBytes(claims));
            String signed = VERSION + "." + payload;
            return signed + "." + B64.encodeToString(hmac(key, signed));
        } catch (java.io.IOException e) {
            throw new IllegalArgumentException("The claims are not JSON: " + e.getMessage(), e);
        }
    }

    /**
     * The claims of a token that this key signed. Throws {@link IllegalArgumentException} with the code {@code malformed} or
     * {@code bad_signature}. The expiry is the work of the caller (it has the clock).
     */
    public static Map<String, Object> verify(String token, String key) {
        String[] parts = token == null ? new String[0] : token.trim().split("\\.");
        if (parts.length != 3 || !VERSION.equals(parts[0])) {
            throw new IllegalArgumentException("malformed");
        }
        byte[] given;
        try {
            given = B64D.decode(parts[2]);
        } catch (IllegalArgumentException e) {
            throw new IllegalArgumentException("malformed");
        }
        if (!MessageDigest.isEqual(hmac(key, parts[0] + "." + parts[1]), given)) {
            throw new IllegalArgumentException("bad_signature");
        }
        try {
            return JSON.readValue(B64D.decode(parts[1]), new TypeReference<LinkedHashMap<String, Object>>() {});
        } catch (java.io.IOException | IllegalArgumentException e) {
            throw new IllegalArgumentException("malformed");
        }
    }

    private static byte[] hmac(String key, String text) {
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            mac.init(new SecretKeySpec(key.getBytes(StandardCharsets.UTF_8), "HmacSHA256"));
            return mac.doFinal(text.getBytes(StandardCharsets.US_ASCII));
        } catch (GeneralSecurityException e) {
            throw new IllegalStateException("HmacSHA256 is not available", e);
        }
    }

    // ---------------------------------------------------------------- the record MAC of the desk (review finding 3)

    /**
     * The MAC that the desk sends with each record ({@code payment_hub_record}, {@code payment_hub_refund_record}): base64url of
     * HMAC-SHA256 with the checkout key over {@code "hubpay.v1\n" kind "\n" orderId "\n" paymentIntentId "\n" amount "\n"
     * currency "\n" ref} (UTF-8). The amount is the plain decimal without trailing zeros, the currency is upper case, ref is
     * the refund ID of a refund (else empty). The desk has the same function (StoreCheckout.recordMac).
     */
    public static String recordMac(String key, String kind, String orderId, String paymentIntentId, String amount, String currency, String ref) {
        if (UtilValidate.isEmpty(key)) {
            throw new IllegalArgumentException("No checkout key");
        }
        String text = "hubpay.v1\n" + nz(kind) + "\n" + nz(orderId) + "\n" + nz(paymentIntentId) + "\n" + plainAmount(amount) + "\n"
                + nz(currency).toUpperCase(java.util.Locale.ROOT) + "\n" + nz(ref);
        return B64.encodeToString(hmacUtf8(key, text));
    }

    /** True when the given MAC is the MAC of these values (constant-time compare). False without a key or a MAC. */
    public static boolean macMatches(String key, String given, String kind, String orderId, String paymentIntentId, String amount,
            String currency, String ref) {
        if (UtilValidate.isEmpty(key) || UtilValidate.isEmpty(given)) {
            return false;
        }
        byte[] want = recordMac(key, kind, orderId, paymentIntentId, amount, currency, ref).getBytes(StandardCharsets.US_ASCII);
        return MessageDigest.isEqual(want, given.trim().getBytes(StandardCharsets.US_ASCII));
    }

    static String plainAmount(String amount) {
        try {
            return new BigDecimal(nz(amount).trim()).stripTrailingZeros().toPlainString();
        } catch (NumberFormatException e) {
            return nz(amount).trim();
        }
    }

    private static String nz(String s) {
        return s == null ? "" : s;
    }

    private static byte[] hmacUtf8(String key, String text) {
        try {
            Mac mac = Mac.getInstance("HmacSHA256");
            mac.init(new SecretKeySpec(key.getBytes(StandardCharsets.UTF_8), "HmacSHA256"));
            return mac.doFinal(text.getBytes(StandardCharsets.UTF_8));
        } catch (GeneralSecurityException e) {
            throw new IllegalStateException("HmacSHA256 is not available", e);
        }
    }

    /** The checkout key of this store database, or null. A test replaces the lookup. */
    static volatile java.util.function.Function<Delegator, String> keyLookup =
            d -> EntityUtilProperties.getPropertyValue(RESOURCE, "key", d);

    public static String checkoutKey(Delegator delegator) {
        String k = keyLookup.apply(delegator);
        return UtilValidate.isEmpty(k) ? null : k.trim();
    }

    // ---------------------------------------------------------------- the signed pay link (review finding 7e)

    /** The signature of the pay link of one order: base64url HMAC-SHA256 with the checkout key over "hubpay.paylink.v1\n" + orderId. */
    public static String payLinkSignature(String key, String orderId) {
        return B64.encodeToString(hmacUtf8(key, "hubpay.paylink.v1\n" + nz(orderId)));
    }

    /** True when the request carries the valid pay link signature of the order (a guest who lost the session). */
    public static boolean payLinkValid(HttpServletRequest request, Delegator delegator, String orderId) {
        String sig = request == null ? null : request.getParameter(PAY_LINK_PARAM);
        String key = delegator == null || UtilValidate.isEmpty(sig) || UtilValidate.isEmpty(orderId) ? null : checkoutKey(delegator);
        if (key == null) {
            return false;
        }
        return MessageDigest.isEqual(payLinkSignature(key, orderId).getBytes(StandardCharsets.US_ASCII),
                sig.trim().getBytes(StandardCharsets.US_ASCII));
    }

    /**
     * The pay link of the order mail: {@code <base>/control/ordercomplete?orderId=..&pay=..}, or null when the order has no
     * unpaid card payment through the hub or the store has no key. {@code base} is the secure shop address with the webapp.
     */
    public static String payLink(Delegator delegator, GenericValue orderHeader, String base) {
        if (delegator == null || orderHeader == null || UtilValidate.isEmpty(base)) {
            return null;
        }
        try {
            String orderId = orderHeader.getString("orderId");
            GenericValue pref = preference(delegator, orderId);
            String key = checkoutKey(delegator);
            if (pref == null || key == null || !"PAYMENT_NOT_RECEIVED".equals(pref.getString("statusId"))) {
                return null;
            }
            return stripSlash(base) + "/control/ordercomplete?orderId=" + java.net.URLEncoder.encode(orderId, "UTF-8") + "&" + PAY_LINK_PARAM + "="
                    + payLinkSignature(key, orderId);
        } catch (GenericEntityException | java.io.UnsupportedEncodingException | RuntimeException e) {
            Debug.logWarning("Hub checkout: no pay link for order " + orderHeader.getString("orderId") + ": " + e.getMessage(), module);
            return null;
        }
    }

    /** The claims of one order. Pure, for tests. */
    public static Map<String, Object> claims(String storeId, String orderId, BigDecimal amount, String currency, String origin,
            String email, long nowEpochSeconds) {
        Map<String, Object> c = new LinkedHashMap<>();
        c.put("store", storeId);
        c.put("orderId", orderId);
        c.put("amount", amount.stripTrailingZeros().toPlainString());
        c.put("currency", currency.toUpperCase(java.util.Locale.ROOT));
        c.put("origin", origin);
        if (UtilValidate.isNotEmpty(email)) {
            c.put("email", email);
        }
        c.put("exp", nowEpochSeconds + TTL_SECONDS);
        return c;
    }

    // ---------------------------------------------------------------- the pay step of the order page

    /** Called by checkExternalPayment: this session placed the order and may pay it. */
    @SuppressWarnings("unchecked")
    public static void rememberOrder(HttpServletRequest request, String orderId) {
        if (orderId == null) {
            return;
        }
        HttpSession session = request.getSession();
        synchronized (session) {
            Set<String> ids = (Set<String>) session.getAttribute(SESSION_ORDERS);
            Set<String> next = ids == null ? new HashSet<>() : new HashSet<>(ids);
            next.add(orderId);
            session.setAttribute(SESSION_ORDERS, Collections.unmodifiableSet(next));
        }
    }

    /**
     * The pay step of an order for the template (shop order/hubpay.ftl). The key {@code state}: {@code none} (not this
     * payment type, or not the order of this visitor), {@code not_configured}, {@code paid}, {@code confirming} (the
     * shopper is back from Stripe and the webhook did not arrive yet), or {@code due} with {@code token}, {@code intentUrl},
     * {@code returnUrl}, {@code amount} and {@code currency}.
     */
    public static Map<String, Object> payStep(HttpServletRequest request, GenericValue orderHeader) {
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("state", "none");
        if (orderHeader == null) {
            return out;
        }
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        String orderId = orderHeader.getString("orderId");
        try {
            GenericValue pref = preference(delegator, orderId);
            if (pref == null || !mayPay(request, orderHeader)) {
                return out;
            }
            String status = pref.getString("statusId");
            if ("PAYMENT_RECEIVED".equals(status) || "PAYMENT_SETTLED".equals(status)) {
                out.put("state", "paid");
                return out;
            }
            String redirect = request.getParameter("redirect_status");
            if (request.getParameter("payment_intent") != null && ("succeeded".equals(redirect) || "processing".equals(redirect))) {
                out.put("state", "confirming");
                return out;
            }
            String storeId = EntityUtilProperties.getPropertyValue(RESOURCE, "storeId", delegator);
            String key = checkoutKey(delegator);
            String url = EntityUtilProperties.getPropertyValue(RESOURCE, "url", delegator);
            if (UtilValidate.isEmpty(storeId) || UtilValidate.isEmpty(key) || UtilValidate.isEmpty(url)) {
                out.put("state", "not_configured");
                return out;
            }
            BigDecimal amount = pref.getBigDecimal("maxAmount");
            if (amount == null) {
                amount = new OrderReadHelper(orderHeader).getOrderGrandTotal();
            }
            String currency = orderHeader.getString("currencyUom");
            String origin = origin(request);
            String email = null;
            try {
                email = new OrderReadHelper(orderHeader).getOrderEmailString();
            } catch (RuntimeException e) {
                // no receipt mail from Stripe; the store sends its own confirmation
            }
            if (email != null && email.contains(",")) {
                email = email.substring(0, email.indexOf(',')).trim();
            }
            Map<String, Object> claims = claims(storeId, orderId, amount, currency, origin, email, System.currentTimeMillis() / 1000);
            out.put("state", "due");
            out.put("token", sign(claims, key));
            out.put("intentUrl", stripSlash(url) + "/" + storeId + "/intent");
            out.put("returnUrl", origin + request.getContextPath() + "/control/ordercomplete?orderId=" + orderId);
            out.put("amount", amount);
            out.put("currency", currency);
            return out;
        } catch (GenericEntityException | RuntimeException e) {
            Debug.logError(e, "Hub checkout: no pay step for order " + orderId + ": " + e.getMessage(), module);
            out.put("state", "error");
            return out;
        }
    }

    /** The domains of the pay step for the Content-Security-Policy of the payment pages (Stripe.js and the hub). */
    public static List<String> scriptDomains(Delegator delegator) {
        String url = EntityUtilProperties.getPropertyValue(RESOURCE, "url", delegator);
        if (UtilValidate.isEmpty(url)) {
            return List.of();
        }
        String host = java.net.URI.create(url.trim()).getHost();
        return host == null ? List.of("js.stripe.com", "api.stripe.com", "hooks.stripe.com")
                : List.of("js.stripe.com", "api.stripe.com", "hooks.stripe.com", host);
    }

    /** The open EXT_STRIPE_HUB preference of the order, or null. */
    static GenericValue preference(Delegator delegator, String orderId) throws GenericEntityException {
        for (GenericValue p : EntityQuery.use(delegator).from("OrderPaymentPreference").where("orderId", orderId,
                "paymentMethodTypeId", PAYMENT_METHOD_TYPE_ID).orderBy("orderPaymentPreferenceId").queryList()) {
            if (!"PAYMENT_CANCELLED".equals(p.getString("statusId")) && !"PAYMENT_DECLINED".equals(p.getString("statusId"))) {
                return p;
            }
        }
        return null;
    }

    /** This session placed the order, or the logged-in party is the placing customer. */
    @SuppressWarnings("unchecked")
    static boolean mayPay(HttpServletRequest request, GenericValue orderHeader) {
        HttpSession session = request.getParameter(PAY_LINK_PARAM) != null ? request.getSession() : request.getSession(false);
        if (session == null) {
            return false;
        }
        Set<String> ids = (Set<String>) session.getAttribute(SESSION_ORDERS);
        if (ids != null && ids.contains(orderHeader.getString("orderId"))) {
            return true;
        }
        // the signed pay link of the order mail: a guest who lost the session pays again (review finding 7e)
        if (payLinkValid(request, (Delegator) request.getAttribute("delegator"), orderHeader.getString("orderId"))) {
            rememberOrder(request, orderHeader.getString("orderId"));
            return true;
        }
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        if (userLogin == null || userLogin.getString("partyId") == null) {
            return false;
        }
        GenericValue placing = new OrderReadHelper(orderHeader).getPlacingParty();
        return placing != null && userLogin.getString("partyId").equals(placing.getString("partyId"));
    }

    /** The origin of the request as the browser sees it: scheme, host and a port that is not the default. */
    static String origin(HttpServletRequest request) {
        // behind the Ingress (TLS ends there): the forwarded scheme and host are what the browser sees
        String fwdProto = first(request.getHeader("X-Forwarded-Proto"));
        String fwdHost = first(request.getHeader("X-Forwarded-Host"));
        if (fwdProto != null && fwdHost != null && fwdHost.matches("[A-Za-z0-9.\\-]+(:[0-9]{1,5})?")) {
            return fwdProto.toLowerCase(java.util.Locale.ROOT) + "://" + fwdHost.toLowerCase(java.util.Locale.ROOT);
        }
        String scheme = request.getScheme();
        int port = request.getServerPort();
        boolean standard = port <= 0 || ("https".equals(scheme) && port == 443) || ("http".equals(scheme) && port == 80);
        return scheme + "://" + request.getServerName() + (standard ? "" : ":" + port);
    }

    private static String first(String header) {
        if (header == null || header.trim().isEmpty()) {
            return null;
        }
        String v = header.split(",")[0].trim();
        return v.isEmpty() ? null : v;
    }

    private static String stripSlash(String s) {
        String t = s.trim();
        return t.endsWith("/") ? t.substring(0, t.length() - 1) : t;
    }
}

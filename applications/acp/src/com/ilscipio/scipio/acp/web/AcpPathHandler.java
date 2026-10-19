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
package com.ilscipio.scipio.acp.web;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.time.Instant;
import java.time.format.DateTimeParseException;
import java.util.Base64;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.UUID;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.security.Security;
import org.ofbiz.security.SecurityConfigurationException;
import org.ofbiz.security.SecurityFactory;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.ExtWebappInfo;
import org.ofbiz.webapp.control.RequestLinkUtil;
import org.ofbiz.webapp.control.WebappPathHandler;
import org.ofbiz.webapp.control.WebappPathHandlerDef;
import org.ofbiz.webapp.website.WebSiteWorker;

import com.ilscipio.scipio.acp.AcpConfig;
import com.ilscipio.scipio.acp.AcpException;
import com.ilscipio.scipio.acp.AcpJson;
import com.ilscipio.scipio.acp.checkout.AcpCheckout;
import com.ilscipio.scipio.acp.checkout.AcpContext;
import com.ilscipio.scipio.acp.feed.AcpFeed;
import com.ilscipio.scipio.mcp.security.McpAuthException;
import com.ilscipio.scipio.mcp.security.McpAuthenticator;
import com.ilscipio.scipio.mcp.security.McpPrincipal;
import com.ilscipio.scipio.mcp.security.McpRateLimiter;
import com.ilscipio.scipio.mcp.security.McpTokenRoutes;
import com.ilscipio.scipio.mcp.web.McpRequest;

/**
 * SCIPIO: 4.0.0: The ACP endpoint of every store webapp, {@code /<webapp>/acp/...}, through the ContextFilter
 * path-handler hook. Never touches the HttpSession.
 * <ul>
 * <li>POST checkout_sessions, GET|POST checkout_sessions/{id}, POST checkout_sessions/{id}/complete|cancel (REST)</li>
 * <li>POST mcp (the MCP binding: tools create/get/update/complete/cancel_checkout_session)</li>
 * <li>GET feed/products.jsonl, GET feed/metadata.json (the product feed)</li>
 * </ul>
 * Auth: an MCP access token (Bearer) whose login has the permission ACP_CHECKOUT.
 */
@WebappPathHandlerDef
public final class AcpPathHandler implements WebappPathHandler {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String SEGMENT = "acp";

    @Override
    public String getPathSegment() {
        return SEGMENT;
    }

    @Override
    public String getTenantId(HttpServletRequest request, Delegator baseDelegator) {
        return McpTokenRoutes.getTenantId(request, baseDelegator);
    }

    @Override
    public boolean handle(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        if (delegator == null || dispatcher == null || !AcpConfig.isEnabled(delegator)) return false;
        Store store = Store.of(request, delegator);
        if (store == null) return false;

        String requestId = UtilValidate.isNotEmpty(request.getHeader("Request-Id")) ? request.getHeader("Request-Id") : UUID.randomUUID().toString();
        response.setHeader("Request-Id", requestId);
        response.setHeader("Cache-Control", "no-store");
        String path = RequestLinkUtil.getServletAndPathInfo(request);
        String sub = path.length() > SEGMENT.length() + 1 ? path.substring(SEGMENT.length() + 1) : "";
        while (sub.startsWith("/")) sub = sub.substring(1);
        String method = request.getMethod();

        Security security;
        try {
            security = SecurityFactory.getInstance(delegator);
        } catch (SecurityConfigurationException e) {
            sendError(response, AcpException.unavailable("Security layer not ready."));
            return true;
        }
        McpRequest mreq = new McpRequest(request, request.getServletContext(), store.webappInfo, delegator, dispatcher, security, requestId);
        McpPrincipal principal;
        try {
            principal = McpAuthenticator.authenticate(mreq);
        } catch (McpAuthException e) {
            if (!McpRateLimiter.get().tryAcquire("auth:" + request.getRemoteAddr(), com.ilscipio.scipio.mcp.security.McpConfig.getFailedAuthPerMinute())) {
                response.setHeader("Retry-After", "60");
                sendError(response, new AcpException(429, AcpException.INVALID_REQUEST, "rate_limited", "Too many failed logins.", null));
                return true;
            }
            sendError(response, new AcpException(e.getStatus(), AcpException.INVALID_REQUEST, e.getStatus() == 403 ? "forbidden" : "unauthorized",
                    e.getMessage(), null));
            return true;
        }
        if (principal == null) {
            response.setHeader("WWW-Authenticate", "Bearer");
            sendError(response, new AcpException(401, AcpException.INVALID_REQUEST, "unauthorized", "Bearer token required.", null));
            return true;
        }
        String permission = AcpConfig.get(delegator, "permission", "ACP_CHECKOUT");
        if (!security.hasPermission(permission, principal.getUserLogin()) || !principal.isWebappAllowed(store.webappName)) {
            sendError(response, new AcpException(403, AcpException.INVALID_REQUEST, "forbidden", "The token may not buy in this store.", null));
            return true;
        }
        if (!McpRateLimiter.get().tryAcquire("acp:" + principal.getTokenId(), AcpConfig.getInt(delegator, "rateLimit.perMinute", 120))) {
            response.setHeader("Retry-After", "60");
            sendError(response, new AcpException(429, AcpException.INVALID_REQUEST, "rate_limited", "Too many requests.", null));
            return true;
        }
        String body = "POST".equals(method) ? readBody(request) : "";
        AcpContext ctx = new AcpContext(delegator, dispatcher, store.productStoreId, store.webSiteId, store.webappName, baseUrl(request, store.productStoreId),
                principal.getTokenId(), principal.getUserLoginId(), request.getLocale());

        try {
            if (sub.equals("mcp")) {
                if (!"POST".equals(method)) {
                    response.setHeader("Allow", "POST");
                    sendError(response, new AcpException(405, AcpException.INVALID_REQUEST, "method_not_allowed", "Use POST.", null));
                    return true;
                }
                AcpMcpBinding.handle(ctx, request, response, body);
                return true;
            }
            if (sub.startsWith("feed/")) {
                serveFeed(ctx, response, sub.substring(5), method);
                return true;
            }
            if (!sub.equals("checkout_sessions") && !sub.startsWith("checkout_sessions/")) {
                sendError(response, AcpException.notFound("Not found."));
                return true;
            }
            checkVersion(delegator, request.getHeader("API-Version"));
            checkSignature(delegator, store.productStoreId, request, body);
            String[] parts = sub.split("/");
            if ("GET".equals(method)) {
                if (parts.length != 2) throw AcpException.notFound("Not found.");
                sendJson(response, 200, AcpJson.write(AcpCheckout.get(ctx, parts[1])));
                return true;
            }
            if (!"POST".equals(method)) {
                response.setHeader("Allow", "GET, POST");
                throw new AcpException(405, AcpException.INVALID_REQUEST, "method_not_allowed", "Use GET or POST.", null);
            }
            String key = request.getHeader("Idempotency-Key");
            if (UtilValidate.isEmpty(key)) {
                throw AcpException.invalid("idempotency_key_required", "The Idempotency-Key header is required.", "Idempotency-Key");
            }
            response.setHeader("Idempotency-Key", key);
            Map<String, Object> json;
            try {
                json = AcpJson.parse(body);
            } catch (IOException e) {
                throw AcpException.invalid("invalid", "The body is not a JSON object.", "$");
            }
            String hash = AcpJson.sha256Hex(method + " " + sub + "\n" + body);
            AcpIdempotency.Stored stored = AcpIdempotency.begin(delegator, principal.getTokenId(), key, hash);
            if (stored != null) {
                response.setHeader("Idempotent-Replayed", "true");
                sendJson(response, stored.status, stored.body);
                return true;
            }
            int status = 200;
            String out;
            try {
                out = AcpJson.write(dispatch(ctx, parts, json));
                if (parts.length == 1) status = 201;
            } catch (AcpException e) {
                AcpIdempotency.finish(delegator, principal.getTokenId(), key, e.getStatus(), AcpJson.write(e.toBody()));
                throw e;
            } catch (RuntimeException e) {
                AcpIdempotency.finish(delegator, principal.getTokenId(), key, 500, null);
                throw e;
            }
            AcpIdempotency.finish(delegator, principal.getTokenId(), key, status, out);
            sendJson(response, status, out);
            return true;
        } catch (AcpException e) {
            sendError(response, e);
            return true;
        } catch (RuntimeException e) {
            Debug.logError(e, "ACP: " + method + " " + sub + " failed: " + e.getMessage(), module);
            sendError(response, AcpException.processing("Unexpected error."));
            return true;
        }
    }

    /** create | update | complete | cancel. */
    static Map<String, Object> dispatch(AcpContext ctx, String[] parts, Map<String, Object> json) throws AcpException {
        if (parts.length == 1) return AcpCheckout.create(ctx, json);
        if (parts.length == 2) return AcpCheckout.update(ctx, parts[1], json);
        if (parts.length == 3 && "complete".equals(parts[2])) return AcpCheckout.complete(ctx, parts[1], json);
        if (parts.length == 3 && "cancel".equals(parts[2])) return AcpCheckout.cancel(ctx, parts[1], json);
        throw AcpException.notFound("Not found.");
    }

    static void checkVersion(Delegator delegator, String version) throws AcpException {
        if (UtilValidate.isEmpty(version)) {
            throw AcpException.invalid("missing", "The API-Version header is required.", "API-Version");
        }
        List<String> supported = AcpConfig.supportedVersions(delegator);
        if (!supported.contains(version.trim())) {
            AcpException e = new AcpException(400, AcpException.INVALID_REQUEST, "unsupported_api_version",
                    "API-Version " + version + " is not supported; use one of " + supported + ".", "API-Version");
            throw e;
        }
    }

    /** When signature.secret is set: Signature = base64(HMAC-SHA256(secret, Timestamp + "." + body)), Timestamp RFC 3339, max skew. */
    static void checkSignature(Delegator delegator, String productStoreId, HttpServletRequest request, String body) throws AcpException {
        String secret = AcpConfig.get(delegator, productStoreId, "signature.secret", null);
        if (UtilValidate.isEmpty(secret)) return;
        verifySignature(secret, request.getHeader("Signature"), request.getHeader("Timestamp"), body,
                AcpConfig.getInt(delegator, "signature.maxSkewSeconds", 300), Instant.now());
    }

    /** Pure, for tests. */
    static void verifySignature(String secret, String signature, String timestamp, String body, int maxSkewSeconds, Instant now) throws AcpException {
        if (UtilValidate.isEmpty(signature) || UtilValidate.isEmpty(timestamp)) {
            throw new AcpException(401, AcpException.INVALID_REQUEST, "invalid_signature", "Signature and Timestamp are required.", "Signature");
        }
        Instant t;
        try {
            t = Instant.parse(timestamp.trim());
        } catch (DateTimeParseException e) {
            throw new AcpException(401, AcpException.INVALID_REQUEST, "invalid_signature", "Timestamp is not RFC 3339.", "Timestamp");
        }
        if (Math.abs(now.getEpochSecond() - t.getEpochSecond()) > maxSkewSeconds) {
            throw new AcpException(401, AcpException.INVALID_REQUEST, "invalid_signature", "Timestamp is too old.", "Timestamp");
        }
        String expected = Base64.getEncoder().encodeToString(AcpJson.hmacSha256(secret, timestamp.trim() + "." + (body == null ? "" : body)));
        String given = signature.trim();
        if (given.startsWith("v1=")) given = given.substring(3);
        if (!AcpJson.constantTimeEquals(expected, given)
                && !AcpJson.constantTimeEquals(expected.replace('+', '-').replace('/', '_').replace("=", ""), given)) {
            throw new AcpException(401, AcpException.INVALID_REQUEST, "invalid_signature", "The signature does not match.", "Signature");
        }
    }

    private static void serveFeed(AcpContext ctx, HttpServletResponse response, String file, String method) throws IOException, AcpException {
        if (!"GET".equals(method)) {
            response.setHeader("Allow", "GET");
            throw new AcpException(405, AcpException.INVALID_REQUEST, "method_not_allowed", "Use GET.", null);
        }
        if (!"products.jsonl".equals(file) && !"metadata.json".equals(file)) {
            throw AcpException.notFound("Not found.");
        }
        try {
            AcpFeed.ensureFresh(ctx, 24L * 3600_000L);
        } catch (GenericEntityException e) {
            Debug.logError(e, "ACP: feed failed for store " + ctx.getProductStoreId(), module);
            throw AcpException.processing("The feed could not be built.");
        }
        File f = "metadata.json".equals(file) ? AcpFeed.metadataFile(ctx.getDelegator(), ctx.getProductStoreId())
                : AcpFeed.productsFile(ctx.getDelegator(), ctx.getProductStoreId());
        response.setStatus(200);
        response.setContentType("metadata.json".equals(file) ? "application/json;charset=UTF-8" : "application/jsonl;charset=UTF-8");
        response.setContentLengthLong(f.length());
        try (OutputStream os = response.getOutputStream()) {
            Files.copy(f.toPath(), os);
        }
    }

    // ---------------------------------------------------------------- helpers

    static String readBody(HttpServletRequest request) throws IOException {
        byte[] b = request.getInputStream().readNBytes(2_000_001);
        if (b.length > 2_000_000) throw new IOException("Body too large");
        if (b.length == 0) {
            // a filter before ContextFilter (SEO, CMS) parsed the JSON body and kept its exact text
            Object text = request.getAttribute(org.ofbiz.webapp.event.RequestBodyMapHandlerFactory.REQUEST_BODY_TEXT_ATTR);
            if (text instanceof String) return (String) text;
        }
        return new String(b, StandardCharsets.UTF_8);
    }

    /**
     * The address of the store webapp. Client headers (X-Forwarded-*) are not read: a proxy sets the scheme and host
     * through the Tomcat RemoteIpValve, or the setting baseUrl[.&lt;productStoreId&gt;] fixes the address.
     */
    static String baseUrl(HttpServletRequest request) {
        return baseUrl(request, null);
    }

    static String baseUrl(HttpServletRequest request, String productStoreId) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        String fixed = delegator != null ? AcpConfig.get(delegator, productStoreId, "baseUrl", null) : null;
        if (UtilValidate.isNotEmpty(fixed)) {
            return fixed.endsWith("/") ? fixed.substring(0, fixed.length() - 1) : fixed;
        }
        String scheme = request.getScheme();
        int port = request.getServerPort();
        boolean dflt = ("https".equals(scheme) && port == 443) || ("http".equals(scheme) && port == 80);
        return scheme + "://" + request.getServerName() + (dflt ? "" : ":" + port) + request.getContextPath();
    }

    static void sendJson(HttpServletResponse response, int status, String json) throws IOException {
        response.setStatus(status);
        response.setContentType("application/json;charset=UTF-8");
        byte[] b = (json == null ? "{}" : json).getBytes(StandardCharsets.UTF_8);
        response.setContentLength(b.length);
        try (OutputStream os = response.getOutputStream()) {
            os.write(b);
        }
    }

    static void sendError(HttpServletResponse response, AcpException e) throws IOException {
        if (e.getStatus() == 409) response.setHeader("Retry-After", "2");
        sendJson(response, e.getStatus(), AcpJson.write(e.toBody()));
    }

    /** The store of the webapp: web site, product store and webapp name; null for a webapp without a store. */
    static final class Store {
        String webSiteId;
        String productStoreId;
        String webappName;
        org.ofbiz.base.component.ComponentConfig.WebappInfo webappInfo;

        static Store of(HttpServletRequest request, Delegator delegator) {
            ServletContext sc = request.getServletContext();
            Store s = new Store();
            try {
                ExtWebappInfo ext = ExtWebappInfo.fromServletContext(sc);
                s.webappInfo = ext != null ? ext.getWebappInfo() : null;
            } catch (RuntimeException e) {
                s.webappInfo = null;
            }
            String ctxPath = s.webappInfo != null ? s.webappInfo.getContextRoot() : request.getContextPath();
            s.webappName = ctxPath != null && ctxPath.startsWith("/") ? ctxPath.substring(1) : String.valueOf(ctxPath);
            if (AcpConfig.getList(delegator, "excludedWebapps").contains(s.webappName)) return null;
            s.webSiteId = WebSiteWorker.getWebSiteId(sc);
            if (UtilValidate.isEmpty(s.webSiteId)) return null;
            try {
                GenericValue ws = EntityQuery.use(delegator).from("WebSite").where("webSiteId", s.webSiteId).cache().queryOne();
                s.productStoreId = ws != null ? ws.getString("productStoreId") : null;
            } catch (GenericEntityException e) {
                return null;
            }
            return UtilValidate.isEmpty(s.productStoreId) ? null : s;
        }
    }

    static Map<String, Object> discovery(Delegator delegator, String apiBaseUrl, String productStoreId) {
        Map<String, Object> d = new LinkedHashMap<>();
        Map<String, Object> protocol = new LinkedHashMap<>();
        protocol.put("name", "acp");
        protocol.put("version", AcpConfig.apiVersion(delegator));
        protocol.put("supported_versions", AcpConfig.supportedVersions(delegator));
        protocol.put("documentation_url", "https://www.scipioerp.com/community/developer/agents/agentic-commerce/");
        d.put("protocol", protocol);
        d.put("api_base_url", apiBaseUrl);
        d.put("transports", List.of("rest", "mcp"));
        Map<String, Object> caps = new LinkedHashMap<>();
        caps.put("services", List.of("checkout", "orders"));
        caps.put("extensions", List.of(Map.of("name", "discount", "spec", "https://agenticcommerce.dev/specs/discount",
                "schema", "https://agenticcommerce.dev/schemas/discount.json")));
        caps.put("intervention_types", List.of());
        String currency = null;
        try {
            GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", productStoreId).cache().queryOne();
            currency = store != null ? store.getString("defaultCurrencyUomId") : null;
        } catch (GenericEntityException e) {
            currency = null;
        }
        if (currency != null) caps.put("supported_currencies", List.of(currency.toLowerCase(Locale.ROOT)));
        d.put("capabilities", caps);
        return d;
    }
}

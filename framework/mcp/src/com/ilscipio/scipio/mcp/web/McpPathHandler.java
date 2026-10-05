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
package com.ilscipio.scipio.mcp.web;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.security.Security;
import org.ofbiz.security.SecurityConfigurationException;
import org.ofbiz.security.SecurityFactory;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.ExtWebappInfo;
import org.ofbiz.webapp.control.WebappPathHandler;
import org.ofbiz.webapp.control.WebappPathHandlerDef;

import com.ilscipio.scipio.mcp.protocol.JsonRpc;
import com.ilscipio.scipio.mcp.protocol.JsonRpc.JsonRpcException;
import com.ilscipio.scipio.mcp.protocol.McpProtocol;
import com.ilscipio.scipio.mcp.protocol.McpSession;
import com.ilscipio.scipio.mcp.protocol.McpSessionRegistry;
import com.ilscipio.scipio.mcp.registry.McpRegistry;
import com.ilscipio.scipio.mcp.registry.McpServerDef;
import com.ilscipio.scipio.mcp.security.McpAuthException;
import com.ilscipio.scipio.mcp.security.McpAuthenticator;
import com.ilscipio.scipio.mcp.security.McpConfig;
import com.ilscipio.scipio.mcp.security.McpPolicy;
import com.ilscipio.scipio.mcp.security.McpPrincipal;
import com.ilscipio.scipio.mcp.security.McpRateLimiter;
import com.ilscipio.scipio.mcp.security.McpTokenRoutes;

/**
 * SCIPIO: 4.0.0: MCP Streamable HTTP endpoint served at {@code /<webapp>/mcp} in every webapp through the
 * ContextFilter path-handler hook. Never touches the HttpSession.
 */
@WebappPathHandlerDef
public final class McpPathHandler implements WebappPathHandler {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String HDR_SESSION = "Mcp-Session-Id";
    public static final String HDR_VERSION = "MCP-Protocol-Version";

    @Override
    public String getPathSegment() {
        return McpConfig.getPathSegment();
    }

    /** SCIPIO: 4.0.0: Pooled runtime: the store of the bearer token (McpTokenRoute), read before authentication (G2). */
    @Override
    public String getTenantId(HttpServletRequest request, Delegator baseDelegator) {
        return McpConfig.isEnabled() ? McpTokenRoutes.getTenantId(request, baseDelegator) : null;
    }

    @Override
    public boolean handle(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        if (!McpConfig.isEnabled()) return false;
        ServletContext sc = request.getServletContext();
        WebappInfo webappInfo = null;
        try {
            ExtWebappInfo ext = ExtWebappInfo.fromServletContext(sc);
            webappInfo = ext != null ? ext.getWebappInfo() : null;
        } catch (RuntimeException e) {
            Debug.logWarning("MCP: no webapp info for context " + request.getContextPath() + ": " + e.getMessage(), module);
        }
        String ctxPath = webappInfo != null ? webappInfo.getContextRoot() : request.getContextPath();
        String webappName = ctxPath != null && ctxPath.startsWith("/") ? ctxPath.substring(1) : String.valueOf(ctxPath);
        if (McpConfig.getExcludedWebapps().contains(webappName)) return false;

        String extra = request.getPathInfo();
        if (UtilValidate.isNotEmpty(extra) && !"/".equals(extra)) {
            sendJson(response, 404, JsonRpc.error(null, JsonRpc.INVALID_REQUEST, "Not found", null));
            return true;
        }
        response.setHeader("Cache-Control", "no-store");
        String method = request.getMethod();
        if ("GET".equals(method)) {
            response.setHeader("Allow", "POST, DELETE");
            sendJson(response, 405, JsonRpc.error(null, JsonRpc.INVALID_REQUEST, "Server-initiated streams are not supported; use POST", null));
            return true;
        }
        if (!"POST".equals(method) && !"DELETE".equals(method)) {
            response.setHeader("Allow", "POST, DELETE");
            sendJson(response, 405, JsonRpc.error(null, JsonRpc.INVALID_REQUEST, "Method not allowed", null));
            return true;
        }
        if (!checkTransport(request, response)) return true;

        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        if (delegator == null || dispatcher == null) {
            sendJson(response, 503, JsonRpc.error(null, JsonRpc.INTERNAL_ERROR, "Service layer not ready", null));
            return true;
        }
        Security security;
        try {
            security = SecurityFactory.getInstance(delegator);
        } catch (SecurityConfigurationException e) {
            Debug.logError(e, "MCP: security unavailable", module);
            sendJson(response, 503, JsonRpc.error(null, JsonRpc.INTERNAL_ERROR, "Security layer not ready", null));
            return true;
        }
        McpRequest req = new McpRequest(request, sc, webappInfo, delegator, dispatcher, security, UUID.randomUUID().toString());
        McpRegistry registry = McpRegistry.get(dispatcher);
        McpServerDef server = registry.resolve(req.getWebappName(), req.getComponentName());
        req.setServer(server);

        // Authentication
        String ipKey = "ip:" + req.getRemoteAddr();
        McpPrincipal principal;
        try {
            principal = McpAuthenticator.authenticate(req);
        } catch (McpAuthException e) {
            if (!McpRateLimiter.get().tryAcquire("auth:" + req.getRemoteAddr(), McpConfig.getFailedAuthPerMinute())) {
                response.setHeader("Retry-After", "60");
                sendJson(response, 429, JsonRpc.error(null, JsonRpc.SERVER_ERROR, "Too many failed authentication attempts", null));
                return true;
            }
            sendAuthError(response, e);
            return true;
        }
        req.setPrincipal(principal);
        if (principal == null && !server.isAllowAnonymous()) {
            sendAuthError(response, McpAuthException.unauthorized("Authentication required"));
            return true;
        }
        try {
            McpPolicy.checkServerAccess(req);
        } catch (McpAuthException e) {
            sendAuthError(response, e);
            return true;
        }

        // Rate limits
        // SCIPIO: 4.0.0: pooled runtime: buckets per store; stores copied from one template share token ids (G6)
        String rateKey = delegator.getDelegatorName() + "|" + (principal != null ? "tok:" + principal.getTokenId() : ipKey);
        String login = principal != null ? principal.getUserLoginId() : null;
        int perMinute = principal != null ? McpConfig.getRateLimitPerMinute(login) : McpConfig.getAnonymousRateLimitPerMinute();
        if (!McpRateLimiter.get().tryAcquire(rateKey, perMinute)) {
            response.setHeader("Retry-After", "60");
            sendJson(response, 429, JsonRpc.error(null, JsonRpc.SERVER_ERROR, "Rate limit exceeded", null));
            return true;
        }
        if (!McpRateLimiter.get().tryAcquireSlot(rateKey, McpConfig.getMaxConcurrent(login))) {
            response.setHeader("Retry-After", "5");
            sendJson(response, 429, JsonRpc.error(null, JsonRpc.SERVER_ERROR, "Too many concurrent requests", null));
            return true;
        }
        try {
            if ("DELETE".equals(method)) {
                handleDelete(request, response, req);
            } else {
                handlePost(request, response, req, registry);
            }
        } finally {
            McpRateLimiter.get().release(rateKey);
        }
        return true;
    }

    private void handleDelete(HttpServletRequest request, HttpServletResponse response, McpRequest req) throws IOException {
        String sid = request.getHeader(HDR_SESSION);
        McpSession session = McpSessionRegistry.get().find(sid);
        if (session == null || !sessionMatches(session, req)) {
            sendJson(response, 404, JsonRpc.error(null, JsonRpc.SERVER_ERROR, "Session not found", null));
            return;
        }
        McpSessionRegistry.get().remove(sid);
        response.setStatus(204);
    }

    private void handlePost(HttpServletRequest request, HttpServletResponse response, McpRequest req, McpRegistry registry) throws IOException {
        String contentType = request.getContentType();
        if (contentType == null || !contentType.toLowerCase().contains("application/json")) {
            sendJson(response, 415, JsonRpc.error(null, JsonRpc.INVALID_REQUEST, "Content-Type must be application/json", null));
            return;
        }
        String versionHeader = request.getHeader(HDR_VERSION);
        if (UtilValidate.isNotEmpty(versionHeader) && !McpProtocol.isSupportedVersion(versionHeader)) {
            sendJson(response, 400, JsonRpc.error(null, JsonRpc.INVALID_REQUEST, "Unsupported MCP-Protocol-Version " + versionHeader + "; supported: " + McpProtocol.SUPPORTED_VERSIONS, null));
            return;
        }
        String body = readBody(request, McpConfig.getRequestMaxBytes());
        if (body == null) {
            sendJson(response, 413, JsonRpc.error(null, JsonRpc.INVALID_REQUEST, "Request body too large", null));
            return;
        }
        JsonRpc.Parsed parsed;
        try {
            parsed = JsonRpc.parse(body);
        } catch (JsonRpcException e) {
            sendJson(response, 400, JsonRpc.error(null, e.getCode(), e.getMessage(), null));
            return;
        }

        // Session binding
        String sid = request.getHeader(HDR_SESSION);
        McpSession session = null;
        if (UtilValidate.isNotEmpty(sid)) {
            session = McpSessionRegistry.get().find(sid);
            if (session == null || !sessionMatches(session, req)) {
                sendJson(response, 404, JsonRpc.error(null, JsonRpc.SERVER_ERROR, "Session not found; re-initialize", null));
                return;
            }
            req.setSession(session);
        }
        String version = UtilValidate.isNotEmpty(versionHeader) ? versionHeader
                : (session != null ? session.getProtocolVersion() : McpProtocol.V_2025_03_26);
        req.setProtocolVersion(version);
        if (parsed.batch) {
            if (McpProtocol.V_2025_06_18.equals(version)) {
                sendJson(response, 400, JsonRpc.error(null, JsonRpc.INVALID_REQUEST, "Batches are not supported in protocol " + version, null));
                return;
            }
            if (parsed.requests.size() > McpConfig.getBatchMaxSize()) {
                sendJson(response, 400, JsonRpc.error(null, JsonRpc.INVALID_REQUEST, "Batch exceeds " + McpConfig.getBatchMaxSize() + " requests", null));
                return;
            }
        }

        McpProtocol protocol = new McpProtocol(registry);
        List<Map<String, Object>> responses = new ArrayList<>();
        boolean initialized = false;
        for (JsonRpc.Request rpc : parsed.requests) {
            if (rpc.isInvalid()) {
                if (!rpc.notification) responses.add(JsonRpc.error(rpc.id, JsonRpc.INVALID_REQUEST, rpc.invalidReason, null));
                continue;
            }
            try {
                Object result = protocol.handle(req, rpc);
                if ("initialize".equals(rpc.method)) initialized = true;
                if (!rpc.notification) responses.add(JsonRpc.result(rpc.id, result));
            } catch (JsonRpcException e) {
                if (!rpc.notification) responses.add(JsonRpc.error(rpc.id, e.getCode(), e.getMessage(), e.getData()));
            } catch (RuntimeException e) {
                Debug.logError(e, "MCP: internal error handling " + rpc.method, module);
                if (!rpc.notification) responses.add(JsonRpc.error(rpc.id, JsonRpc.INTERNAL_ERROR, "Internal error", null));
            }
        }
        response.setHeader(HDR_VERSION, req.getProtocolVersion());
        if (initialized && req.getSession() != null) {
            response.setHeader(HDR_SESSION, req.getSession().getId());
        }
        if (responses.isEmpty()) {
            response.setStatus(202);
            return;
        }
        sendJson(response, 200, parsed.batch ? responses : responses.get(0));
    }

    private static boolean sessionMatches(McpSession session, McpRequest req) {
        String tokenId = req.getTokenId() != null ? req.getTokenId() : "";
        // SCIPIO: 4.0.0: pooled runtime: a session id of store A is unknown on store B (G6)
        return session.getTokenId().equals(tokenId) && session.getWebappName().equals(req.getWebappName())
                && session.getDelegatorName().equals(req.getDelegator().getDelegatorName());
    }

    private static boolean checkTransport(HttpServletRequest request, HttpServletResponse response) throws IOException {
        if (!request.isSecure() && !McpConfig.isAllowInsecure()) {
            // X-Forwarded-Proto is trusted only from a configured reverse proxy address (mcp.trustedProxies)
            String forwarded = request.getHeader("X-Forwarded-Proto");
            List<String> proxies = McpConfig.getTrustedProxies();
            boolean fromProxy = !proxies.isEmpty()
                    && McpAuthenticator.isRemoteAddrAllowed(String.join(",", proxies), request.getRemoteAddr());
            if (!fromProxy || forwarded == null || !forwarded.equalsIgnoreCase("https")) {
                sendJson(response, 403, JsonRpc.error(null, JsonRpc.INVALID_REQUEST, "HTTPS required", null));
                return false;
            }
        }
        String origin = request.getHeader("Origin");
        if (UtilValidate.isNotEmpty(origin) && !McpConfig.getAllowedOrigins().contains(origin)) {
            sendJson(response, 403, JsonRpc.error(null, JsonRpc.INVALID_REQUEST, "Origin not allowed", null));
            return false;
        }
        List<String> hosts = McpConfig.getAllowedHosts();
        if (!hosts.isEmpty()) {
            String host = request.getHeader("Host");
            if (host == null || !hosts.contains(host)) {
                sendJson(response, 403, JsonRpc.error(null, JsonRpc.INVALID_REQUEST, "Host not allowed", null));
                return false;
            }
        }
        return true;
    }

    private static void sendAuthError(HttpServletResponse response, McpAuthException e) throws IOException {
        if (e.getStatus() == 401) {
            response.setHeader("WWW-Authenticate", "Bearer realm=\"scipio-mcp\"");
        }
        sendJson(response, e.getStatus(), JsonRpc.error(null, JsonRpc.SERVER_ERROR, e.getMessage(), null));
    }

    private static void sendJson(HttpServletResponse response, int status, Object body) throws IOException {
        response.setStatus(status);
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        byte[] bytes = JsonRpc.write(body).getBytes(StandardCharsets.UTF_8);
        response.setContentLength(bytes.length);
        response.getOutputStream().write(bytes);
        response.getOutputStream().flush();
    }

    /**
     * Reads the body up to maxBytes; returns null when the limit is exceeded.
     * Filters that run before ContextFilter (SEO, CMS) may already have parsed an application/json body through
     * {@code UtilHttp.getParameterMap}; in that case the parsed map is cached as the {@code requestBodyMap}
     * attribute and is used instead of the consumed stream (single requests only).
     */
    private static String readBody(HttpServletRequest request, int maxBytes) throws IOException {
        if (request.getContentLengthLong() > maxBytes) return null;
        String body = readStream(request, maxBytes);
        if (body != null && body.trim().isEmpty()) {
            Object cached = request.getAttribute("requestBodyMap");
            if (cached instanceof Map && !((Map<?, ?>) cached).isEmpty()) {
                return JsonRpc.write(cached);
            }
        }
        return body;
    }

    private static String readStream(HttpServletRequest request, int maxBytes) throws IOException {
        try (InputStream in = request.getInputStream()) {
            java.io.ByteArrayOutputStream buf = new java.io.ByteArrayOutputStream(Math.min(8192, maxBytes));
            byte[] chunk = new byte[8192];
            int n;
            long total = 0;
            while ((n = in.read(chunk)) > 0) {
                total += n;
                if (total > maxBytes) return null;
                buf.write(chunk, 0, n);
            }
            return new String(buf.toByteArray(), StandardCharsets.UTF_8);
        }
    }
}

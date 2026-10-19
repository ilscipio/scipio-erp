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

import java.io.IOException;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilValidate;

import com.ilscipio.scipio.acp.AcpConfig;
import com.ilscipio.scipio.acp.AcpException;
import com.ilscipio.scipio.acp.AcpJson;
import com.ilscipio.scipio.acp.checkout.AcpCheckout;
import com.ilscipio.scipio.acp.checkout.AcpContext;

/**
 * SCIPIO: 4.0.0: The ACP MCP binding (docs/mcp-binding.md of the spec) at {@code /<webapp>/acp/mcp}: JSON-RPC 2.0 over
 * Streamable HTTP (JSON answers), methods initialize, ping, tools/list and tools/call. A tool takes
 * {@code {meta: {api_version, idempotency_key?, ...}, id?, payload?}}; the result is the CheckoutSession itself, an
 * ACP error is JSON-RPC error -32000 with the ACP error in data.
 */
public final class AcpMcpBinding {

    public static final String PROTOCOL_VERSION = "2025-11-25";
    static final int ACP_ERROR = -32000;
    static final int INVALID_PARAMS = -32602;
    static final int METHOD_NOT_FOUND = -32601;
    static final int PARSE_ERROR = -32700;
    static final int INVALID_REQUEST = -32600;

    private AcpMcpBinding() {}

    static void handle(AcpContext ctx, HttpServletRequest request, HttpServletResponse response, String body) throws IOException {
        Map<String, Object> req;
        try {
            req = AcpJson.parse(body);
        } catch (IOException e) {
            AcpPathHandler.sendJson(response, 200, AcpJson.write(error(null, PARSE_ERROR, "Parse error", null)));
            return;
        }
        Object id = req.get("id");
        String method = AcpJson.str(req, "method");
        if (!"2.0".equals(AcpJson.str(req, "jsonrpc")) || method == null) {
            AcpPathHandler.sendJson(response, 200, AcpJson.write(error(id, INVALID_REQUEST, "Invalid request", null)));
            return;
        }
        if (id == null) { // notification
            response.setStatus(202);
            return;
        }
        Map<String, Object> params = AcpJson.map(req.get("params"));
        Map<String, Object> out;
        switch (method) {
            case "initialize":
                out = result(id, initialize(params));
                break;
            case "ping":
                out = result(id, new LinkedHashMap<>());
                break;
            case "tools/list":
                out = result(id, Map.of("tools", tools()));
                break;
            case "tools/call":
                out = call(ctx, request, id, params, body);
                break;
            default:
                out = error(id, METHOD_NOT_FOUND, "Method not found: " + method, null);
        }
        AcpPathHandler.sendJson(response, 200, AcpJson.write(out));
    }

    private static Map<String, Object> initialize(Map<String, Object> params) {
        Map<String, Object> r = new LinkedHashMap<>();
        String asked = params != null ? AcpJson.str(params, "protocolVersion") : null;
        r.put("protocolVersion", UtilValidate.isNotEmpty(asked) ? asked : PROTOCOL_VERSION);
        r.put("capabilities", Map.of("tools", Map.of("listChanged", Boolean.FALSE)));
        r.put("serverInfo", Map.of("name", "scipio-acp", "title", "Scipio Agentic Commerce", "version", "4.0.0"));
        r.put("instructions", "Agentic Commerce Protocol checkout: create_checkout_session, update_checkout_session, "
                + "complete_checkout_session (payment_data with a Stripe shared payment token), get and cancel.");
        return r;
    }

    static Map<String, Object> call(AcpContext ctx, HttpServletRequest request, Object rpcId, Map<String, Object> params, String rawBody) {
        String name = params != null ? AcpJson.str(params, "name") : null;
        Map<String, Object> args = params != null ? AcpJson.map(params.get("arguments")) : null;
        if (name == null || args == null) {
            return error(rpcId, INVALID_PARAMS, "params.name and params.arguments are required", null);
        }
        Map<String, Object> meta = AcpJson.map(args.get("meta"));
        if (meta == null || UtilValidate.isEmpty(AcpJson.str(meta, "api_version"))) {
            return error(rpcId, INVALID_PARAMS, "arguments.meta.api_version is required", null);
        }
        String sessionId = AcpJson.str(args, "id");
        Map<String, Object> payload = AcpJson.map(args.get("payload"));
        boolean needsId = !"create_checkout_session".equals(name);
        boolean needsPayload = "create_checkout_session".equals(name) || "update_checkout_session".equals(name)
                || "complete_checkout_session".equals(name);
        if (needsId && UtilValidate.isEmpty(sessionId)) return error(rpcId, INVALID_PARAMS, "arguments.id is required", null);
        if (needsPayload && payload == null) return error(rpcId, INVALID_PARAMS, "arguments.payload is required", null);
        if (payload == null) payload = new LinkedHashMap<>();
        String idemKey = AcpJson.str(meta, "idempotency_key");
        try {
            AcpPathHandler.checkVersion(ctx.getDelegator(), AcpJson.str(meta, "api_version"));
            if (UtilValidate.isNotEmpty(AcpConfig.get(ctx.getDelegator(), ctx.getProductStoreId(), "signature.secret", null))) {
                AcpPathHandler.verifySignature(AcpConfig.get(ctx.getDelegator(), ctx.getProductStoreId(), "signature.secret", null),
                        // the signature covers the exact JSON-RPC body (tool name, id and payload); the HTTP headers win over meta
                        firstNonEmpty(request.getHeader("Signature"), AcpJson.str(meta, "signature")),
                        firstNonEmpty(request.getHeader("Timestamp"), AcpJson.str(meta, "timestamp")), rawBody,
                        AcpConfig.getInt(ctx.getDelegator(), "signature.maxSkewSeconds", 300), java.time.Instant.now());
            }
            boolean write = !"get_checkout_session".equals(name);
            if (write && UtilValidate.isNotEmpty(idemKey)) {
                String hash = AcpJson.sha256Hex("MCP " + name + " " + sessionId + "\n" + AcpJson.write(payload));
                AcpIdempotency.Stored stored = AcpIdempotency.begin(ctx.getDelegator(), ctx.getTokenId(), idemKey, hash);
                if (stored != null) {
                    Map<String, Object> replay = AcpJson.parse(stored.body);
                    return stored.status < 400 ? result(rpcId, replay) : error(rpcId, ACP_ERROR, AcpJson.str(replay, "message"), replay);
                }
                try {
                    Map<String, Object> session = run(ctx, name, sessionId, payload);
                    AcpIdempotency.finish(ctx.getDelegator(), ctx.getTokenId(), idemKey, 200, AcpJson.write(session));
                    return result(rpcId, session);
                } catch (AcpException e) {
                    AcpIdempotency.finish(ctx.getDelegator(), ctx.getTokenId(), idemKey, e.getStatus(), AcpJson.write(e.toBody()));
                    throw e;
                } catch (RuntimeException e) {
                    AcpIdempotency.finish(ctx.getDelegator(), ctx.getTokenId(), idemKey, 500, null);
                    throw e;
                }
            }
            return result(rpcId, run(ctx, name, sessionId, payload));
        } catch (AcpException e) {
            return error(rpcId, ACP_ERROR, e.getMessage(), e.toBody());
        } catch (IOException e) {
            return error(rpcId, ACP_ERROR, "Stored answer unreadable", AcpException.processing("Stored answer unreadable").toBody());
        } catch (RuntimeException e) {
            return error(rpcId, ACP_ERROR, "Unexpected error", AcpException.processing("Unexpected error.").toBody());
        }
    }

    private static String firstNonEmpty(String a, String b) {
        return UtilValidate.isNotEmpty(a) ? a : b;
    }

    private static Map<String, Object> run(AcpContext ctx, String name, String id, Map<String, Object> payload) throws AcpException {
        switch (name) {
            case "create_checkout_session": return AcpCheckout.create(ctx, payload);
            case "get_checkout_session": return AcpCheckout.get(ctx, id);
            case "update_checkout_session": return AcpCheckout.update(ctx, id, payload);
            case "complete_checkout_session": return AcpCheckout.complete(ctx, id, payload);
            case "cancel_checkout_session": return AcpCheckout.cancel(ctx, id, payload);
            default: throw AcpException.notFound("Unknown tool " + name + ".");
        }
    }

    static List<Map<String, Object>> tools() {
        List<Map<String, Object>> t = new ArrayList<>();
        t.add(tool("create_checkout_session", "Create a checkout session for items of this store.", false, true, true));
        t.add(tool("get_checkout_session", "Retrieve the current state of a checkout session.", true, false, false));
        t.add(tool("update_checkout_session", "Update items, buyer, fulfillment details, the selected fulfillment option or discount codes.", true, true, true));
        t.add(tool("complete_checkout_session", "Complete the checkout with payment_data (a Stripe shared payment token) and place the order.", true, true, true));
        t.add(tool("cancel_checkout_session", "Cancel a checkout session.", true, false, true));
        return t;
    }

    private static Map<String, Object> tool(String name, String description, boolean id, boolean payloadRequired, boolean payload) {
        Map<String, Object> props = new LinkedHashMap<>();
        Map<String, Object> metaProps = new LinkedHashMap<>();
        metaProps.put("api_version", Map.of("type", "string", "description", "ACP API version, e.g. 2026-04-17"));
        metaProps.put("idempotency_key", Map.of("type", "string"));
        metaProps.put("request_id", Map.of("type", "string"));
        props.put("meta", Map.of("type", "object", "properties", metaProps, "required", List.of("api_version"), "additionalProperties", Boolean.TRUE));
        List<String> required = new ArrayList<>(List.of("meta"));
        if (id) {
            props.put("id", Map.of("type", "string", "description", "Checkout session id"));
            required.add("id");
        }
        if (payload) {
            props.put("payload", Map.of("type", "object", "description", "The ACP request body of the matching REST call"));
            if (payloadRequired) required.add("payload");
        }
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", props);
        schema.put("required", required);
        Map<String, Object> tool = new LinkedHashMap<>();
        tool.put("name", name);
        tool.put("description", description);
        tool.put("inputSchema", schema);
        return tool;
    }

    static Map<String, Object> result(Object id, Object result) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("jsonrpc", "2.0");
        m.put("id", id);
        m.put("result", result);
        return m;
    }

    static Map<String, Object> error(Object id, int code, String message, Object data) {
        Map<String, Object> e = new LinkedHashMap<>();
        e.put("code", code);
        e.put("message", message);
        if (data != null) e.put("data", data);
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("jsonrpc", "2.0");
        m.put("id", id);
        m.put("error", e);
        return m;
    }
}

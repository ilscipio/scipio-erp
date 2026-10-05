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
package com.ilscipio.scipio.mcp.protocol;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.service.ModelService;

import com.ilscipio.scipio.mcp.catalog.JsonSchemaValidator;
import com.ilscipio.scipio.mcp.protocol.JsonRpc.JsonRpcException;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpPromptDef;
import com.ilscipio.scipio.mcp.registry.McpRegistry;
import com.ilscipio.scipio.mcp.registry.McpResourceDef;
import com.ilscipio.scipio.mcp.registry.McpResult;
import com.ilscipio.scipio.mcp.registry.McpServerDef;
import com.ilscipio.scipio.mcp.registry.McpToolDef;
import com.ilscipio.scipio.mcp.registry.McpTopicTool;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.security.McpAudit;
import com.ilscipio.scipio.mcp.security.McpConfig;
import com.ilscipio.scipio.mcp.security.McpPolicy;
import com.ilscipio.scipio.mcp.security.McpRedactor;
import com.ilscipio.scipio.mcp.security.McpUsageTracker;
import com.ilscipio.scipio.mcp.web.McpRequest;

/**
 * SCIPIO: 4.0.0: MCP method dispatch (initialize, tools, resources, prompts) on top of JSON-RPC.
 */
public final class McpProtocol {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String V_2025_06_18 = "2025-06-18";
    public static final String V_2025_03_26 = "2025-03-26";
    public static final List<String> SUPPORTED_VERSIONS = Collections.unmodifiableList(Arrays.asList(V_2025_06_18, V_2025_03_26));
    public static final String SERVER_VERSION = "4.0.0";
    private static final int PAGE_SIZE = 100;

    private final McpRegistry registry;
    private final McpRedactor redactor;

    public McpProtocol(McpRegistry registry) {
        this.registry = registry;
        this.redactor = McpRedactor.fromConfig();
    }

    public static boolean isSupportedVersion(String v) {
        return v != null && SUPPORTED_VERSIONS.contains(v);
    }

    /** Handles one request; returns the JSON-RPC result object (null for notifications). */
    public Object handle(McpRequest req, JsonRpc.Request rpc) throws JsonRpcException {
        String method = rpc.method;
        if (method.startsWith("notifications/")) {
            return null;
        }
        switch (method) {
            case "initialize": return initialize(req, rpc.params);
            case "ping": return new LinkedHashMap<String, Object>();
            case "tools/list": return toolsList(req, rpc.params);
            case "tools/call": return toolsCall(req, rpc.params);
            case "resources/list": return resourcesList(req, rpc.params);
            case "resources/templates/list": return resourceTemplatesList(req);
            case "resources/read": return resourcesRead(req, rpc.params);
            case "prompts/list": return promptsList(req);
            case "prompts/get": return promptsGet(req, rpc.params);
            case "logging/setLevel": return new LinkedHashMap<String, Object>();
            default: throw new JsonRpcException(JsonRpc.METHOD_NOT_FOUND, "Method not found: " + method);
        }
    }

    // ---- initialize ----

    private Object initialize(McpRequest req, Map<String, Object> params) throws JsonRpcException {
        String requested = params.get("protocolVersion") instanceof String ? (String) params.get("protocolVersion") : null;
        String version = isSupportedVersion(requested) ? requested : V_2025_06_18;
        McpSession session = McpSessionRegistry.get().create(req.getTokenId(), req.getWebappName(), version,
                req.getDelegator().getDelegatorName()); // SCIPIO: 4.0.0: pooled runtime: the session belongs to this store (G6)
        if (session == null) {
            throw new JsonRpcException(JsonRpc.SERVER_ERROR, "Too many MCP sessions for this store; retry later");
        }
        req.setSession(session);
        req.setProtocolVersion(version);
        McpServerDef server = req.getServer();

        Map<String, Object> capabilities = new LinkedHashMap<>();
        capabilities.put("tools", Collections.singletonMap("listChanged", false));
        Map<String, Object> resources = new LinkedHashMap<>();
        resources.put("subscribe", false);
        resources.put("listChanged", false);
        capabilities.put("resources", resources);
        capabilities.put("prompts", Collections.singletonMap("listChanged", false));
        capabilities.put("logging", new LinkedHashMap<String, Object>());

        Map<String, Object> serverInfo = new LinkedHashMap<>();
        serverInfo.put("name", "scipio-" + server.getName());
        serverInfo.put("title", server.getTitle());
        serverInfo.put("version", SERVER_VERSION);

        Map<String, Object> result = new LinkedHashMap<>();
        result.put("protocolVersion", version);
        result.put("capabilities", capabilities);
        result.put("serverInfo", serverInfo);
        result.put("instructions", instructions(req, server));
        return result;
    }

    private static String instructions(McpRequest req, McpServerDef server) {
        StringBuilder sb = new StringBuilder();
        if (UtilValidate.isNotEmpty(server.getDescription())) sb.append(server.getDescription()).append('\n');
        sb.append("This is the Scipio ERP MCP endpoint for webapp '").append(req.getWebappName()).append("'.\n");
        sb.append("Start with scipio_whoami. Most tools take an action argument; the tool description lists the actions. ");
        sb.append("Prefer the application tools listed first. Read the matching skill (scipio_apps skill_get) before multi-step tasks. ");
        sb.append("Use scipio_service (search, describe, call) when no application tool fits. ");
        sb.append("Pass idempotencyKey on write calls you may retry. Every call runs with the permissions of the token's user and is audited.");
        return sb.toString();
    }

    // ---- tools ----

    private List<McpToolDef> visibleTools(McpRequest req) {
        List<McpToolDef> all = registry.getTools(req.getServer());
        if (!req.isAnonymous()) return all;
        List<McpToolDef> out = new ArrayList<>();
        for (McpToolDef t : all) if (t.isPublicAccess()) out.add(t);
        return out;
    }

    private Object toolsList(McpRequest req, Map<String, Object> params) throws JsonRpcException {
        List<McpToolDef> tools = visibleTools(req);
        int offset = cursorOffset(params.get("cursor"));
        List<Map<String, Object>> out = new ArrayList<>();
        int end = Math.min(tools.size(), offset + PAGE_SIZE);
        for (int i = offset; i < end; i++) out.add(toolJson(tools.get(i)));
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("tools", out);
        if (end < tools.size()) result.put("nextCursor", cursor(end));
        return result;
    }

    public static Map<String, Object> toolJson(McpToolDef t) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("name", t.getName());
        if (UtilValidate.isNotEmpty(t.getTitle())) m.put("title", t.getTitle());
        m.put("description", t.getDescription());
        m.put("inputSchema", t.getInputSchema());
        if (t.getOutputSchema() != null) m.put("outputSchema", t.getOutputSchema());
        Map<String, Object> ann = new LinkedHashMap<>();
        ann.put("title", UtilValidate.isNotEmpty(t.getTitle()) ? t.getTitle() : t.getName());
        ann.put("readOnlyHint", t.isReadOnly());
        ann.put("destructiveHint", t.isDestructive());
        ann.put("idempotentHint", t.isIdempotent());
        ann.put("openWorldHint", false);
        m.put("annotations", ann);
        Map<String, Object> meta = new LinkedHashMap<>();
        meta.put("featured", t.isFeatured());
        meta.put("requiresConfirmation", t.isRequiresConfirmation());
        if (t.getServiceName() != null) meta.put("scipio.service", t.getServiceName());
        if (UtilValidate.isNotEmpty(t.getPermission())) meta.put("scipio.permission", t.getPermission());
        if (!t.getTags().isEmpty()) meta.put("scipio.tags", new ArrayList<>(t.getTags()));
        if (t.isComposite()) {
            Map<String, Object> actions = new LinkedHashMap<>();
            for (Map.Entry<String, McpToolDef> e : t.getActions().entrySet()) {
                McpToolDef a = e.getValue();
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("readOnly", a.isReadOnly());
                row.put("destructive", a.isDestructive());
                if (a.isRequiresConfirmation()) row.put("requiresConfirmation", true);
                if (a.isPublicAccess()) row.put("public", true);
                if (a.getServiceName() != null) row.put("service", a.getServiceName());
                if (UtilValidate.isNotEmpty(a.getPermission())) row.put("permission", a.getPermission());
                actions.put(e.getKey(), row);
            }
            meta.put("scipio.actions", actions);
        }
        m.put("_meta", meta);
        return m;
    }

    @SuppressWarnings("unchecked")
    private Object toolsCall(McpRequest req, Map<String, Object> params) throws JsonRpcException {
        String name = params.get("name") instanceof String ? (String) params.get("name") : null;
        if (UtilValidate.isEmpty(name)) throw new JsonRpcException(JsonRpc.INVALID_PARAMS, "Missing tool name");
        McpToolDef tool = registry.findTool(req.getServer(), name);
        if (tool == null || (req.isAnonymous() && !tool.isPublicAccess())) {
            throw new JsonRpcException(JsonRpc.INVALID_PARAMS, "Unknown tool: " + name);
        }
        Map<String, Object> args = params.get("arguments") instanceof Map ? new LinkedHashMap<>((Map<String, Object>) params.get("arguments")) : new LinkedHashMap<>();
        String idempotencyKey = null;
        Object ik = args.remove("idempotencyKey");
        if (ik instanceof String) idempotencyKey = (String) ik;
        Object meta = params.get("_meta");
        if (meta instanceof Map && ((Map<String, Object>) meta).get("idempotencyKey") instanceof String) {
            idempotencyKey = (String) ((Map<String, Object>) meta).get("idempotencyKey");
        }
        if (idempotencyKey != null && idempotencyKey.length() > 200) idempotencyKey = idempotencyKey.substring(0, 200);

        long start = System.currentTimeMillis();
        McpAudit.Entry audit = new McpAudit.Entry();
        audit.tokenId = req.getTokenId();
        audit.userLoginId = req.getUserLoginId();
        audit.webappName = req.getWebappName();
        audit.serverName = req.getServer().getName();
        audit.method = "tools/call";
        audit.toolName = tool.isComposite() && args.get(McpTopicTool.ACTION) instanceof String ? name + ":" + args.get(McpTopicTool.ACTION) : name;
        audit.args = args;
        audit.remoteAddr = req.getRemoteAddr();
        audit.requestId = req.getRequestId();
        audit.idempotencyKey = idempotencyKey;

        ModelService backing = tool.getServiceName() != null ? req.getDispatcher().getDispatchContext().getModelServiceOrNull(tool.getServiceName()) : null;
        McpPolicy.Decision decision = McpPolicy.checkTool(req, tool, backing);
        if (!decision.allowed) {
            audit.status = McpAudit.DENIED;
            audit.errorMessage = decision.reason;
            return finish(req, tool, audit, start, errorResult("Denied: " + decision.reason), true);
        }
        if (idempotencyKey != null && !req.isAnonymous()) {
            Map<String, Object> replay = McpAudit.findIdempotentResult(req.getDelegator(), req.getTokenId(), idempotencyKey);
            if (replay != null) {
                Debug.logInfo("[MCP] idempotent replay tool=" + name + " key=" + idempotencyKey + " token=" + req.getTokenId(), McpAudit.AUDIT_MODULE);
                replay.put("_meta", Collections.singletonMap("idempotentReplay", true));
                audit.status = McpAudit.REPLAY;
                audit.durationMs = System.currentTimeMillis() - start;
                McpAudit.record(req.getDelegator(), redactor, audit);
                return replay;
            }
        }
        List<String> problems = JsonSchemaValidator.validate(tool.getInputSchema(), args);
        if (!problems.isEmpty()) {
            audit.status = McpAudit.ERROR;
            audit.errorMessage = "Invalid arguments: " + String.join("; ", problems);
            return finish(req, tool, audit, start, errorResult(audit.errorMessage), true);
        }
        McpCallContext ctx = new McpCallContext(req);
        Map<String, Object> result;
        boolean error;
        try {
            McpResult r = tool.getExecutor().execute(ctx, args);
            result = toCallResult(r);
            error = r.isError();
            audit.status = error ? McpAudit.ERROR : McpAudit.OK;
            if (error) audit.errorMessage = r.getText();
            else audit.result = result;
        } catch (McpToolException e) {
            error = true;
            audit.status = e.isDenied() ? McpAudit.DENIED : McpAudit.ERROR;
            audit.errorMessage = e.getMessage();
            result = errorResult((e.isDenied() ? "Denied: " : "") + e.getMessage());
        } catch (Throwable t) {
            error = true;
            Debug.logError(t, "MCP: tool " + name + " failed", module);
            audit.status = McpAudit.ERROR;
            audit.errorMessage = t.getClass().getSimpleName() + ": " + t.getMessage();
            result = errorResult("Tool " + name + " failed: " + safe(t));
        }
        return finish(req, tool, audit, start, result, error);
    }

    private Map<String, Object> finish(McpRequest req, McpToolDef tool, McpAudit.Entry audit, long start, Map<String, Object> result, boolean error) {
        audit.durationMs = System.currentTimeMillis() - start;
        McpAudit.record(req.getDelegator(), redactor, audit);
        McpUsageTracker.get().record(req.getDelegator(), req.getServer().getName(), audit.toolName, error);
        return result;
    }

    private Map<String, Object> toCallResult(McpResult r) {
        Map<String, Object> out = new LinkedHashMap<>();
        List<Map<String, Object>> content = new ArrayList<>();
        Object structured = r.getStructured() != null ? redactor.redact(r.getStructured()) : null;
        String text;
        if (r.getText() != null) {
            text = McpRedactor.stripControl(r.getText());
        } else {
            text = JsonRpc.writePretty(structured != null ? structured : new LinkedHashMap<String, Object>());
        }
        int max = McpConfig.getResultMaxChars();
        boolean truncated = text.length() > max;
        text = McpRedactor.truncate(text, max);
        content.add(textContent(text));
        if (r.hasBlob()) {
            Map<String, Object> resource = new LinkedHashMap<>();
            resource.put("uri", "scipio://document/" + r.getBlobName());
            resource.put("mimeType", r.getBlobMimeType());
            resource.put("blob", Base64.getEncoder().encodeToString(r.getBlob()));
            Map<String, Object> c = new LinkedHashMap<>();
            c.put("type", "resource");
            c.put("resource", resource);
            content.add(c);
        }
        out.put("content", content);
        if (structured instanceof Map && !truncated) {
            out.put("structuredContent", structured);
        } else if (structured != null && !truncated) {
            out.put("structuredContent", Collections.singletonMap("result", structured));
        }
        if (r.isError()) out.put("isError", true);
        if (truncated) out.put("_meta", Collections.singletonMap("truncated", true));
        return out;
    }

    private static Map<String, Object> errorResult(String message) {
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("content", Collections.singletonList(textContent(message)));
        out.put("isError", true);
        return out;
    }

    private static Map<String, Object> textContent(String text) {
        Map<String, Object> c = new LinkedHashMap<>();
        c.put("type", "text");
        c.put("text", text);
        return c;
    }

    private static String safe(Throwable t) {
        String m = t.getMessage();
        if (m == null) return t.getClass().getSimpleName();
        int nl = m.indexOf('\n');
        if (nl > 0) m = m.substring(0, nl);
        return m.length() > 400 ? m.substring(0, 400) : m;
    }

    // ---- resources ----

    private Object resourcesList(McpRequest req, Map<String, Object> params) throws JsonRpcException {
        List<Map<String, Object>> out = new ArrayList<>();
        for (McpResourceDef r : registry.getResources(req.getServer())) {
            if (r.isTemplate()) continue;
            Map<String, Object> m = new LinkedHashMap<>();
            m.put("uri", r.getUri());
            m.put("name", r.getName());
            if (!r.getDescription().isEmpty()) m.put("description", r.getDescription());
            m.put("mimeType", r.getMimeType());
            out.add(m);
        }
        int offset = cursorOffset(params.get("cursor"));
        int end = Math.min(out.size(), offset + PAGE_SIZE);
        Map<String, Object> result = new LinkedHashMap<>();
        result.put("resources", new ArrayList<>(out.subList(Math.min(offset, out.size()), end)));
        if (end < out.size()) result.put("nextCursor", cursor(end));
        return result;
    }

    private Object resourceTemplatesList(McpRequest req) {
        List<Map<String, Object>> out = new ArrayList<>();
        for (McpResourceDef r : registry.getResources(req.getServer())) {
            if (!r.isTemplate()) continue;
            Map<String, Object> m = new LinkedHashMap<>();
            m.put("uriTemplate", r.getUri());
            m.put("name", r.getName());
            if (!r.getDescription().isEmpty()) m.put("description", r.getDescription());
            m.put("mimeType", r.getMimeType());
            out.add(m);
        }
        return Collections.singletonMap("resourceTemplates", out);
    }

    private Object resourcesRead(McpRequest req, Map<String, Object> params) throws JsonRpcException {
        String uri = params.get("uri") instanceof String ? (String) params.get("uri") : null;
        if (UtilValidate.isEmpty(uri)) throw new JsonRpcException(JsonRpc.INVALID_PARAMS, "Missing uri");
        McpCallContext ctx = new McpCallContext(req);
        for (McpResourceDef r : registry.getResources(req.getServer())) {
            Map<String, String> p = r.match(uri);
            if (p == null) continue;
            try {
                String text = r.getReader().read(ctx, uri, p);
                Map<String, Object> c = new LinkedHashMap<>();
                c.put("uri", uri);
                c.put("mimeType", r.getMimeType());
                c.put("text", McpRedactor.truncate(McpRedactor.stripControl(text), McpConfig.getResultMaxChars()));
                return Collections.singletonMap("contents", Collections.singletonList(c));
            } catch (Exception e) {
                throw new JsonRpcException(JsonRpc.SERVER_ERROR, "Resource read failed: " + safe(e));
            }
        }
        throw new JsonRpcException(-32002, "Resource not found: " + uri);
    }

    // ---- prompts ----

    private Object promptsList(McpRequest req) {
        List<Map<String, Object>> out = new ArrayList<>();
        for (McpPromptDef p : registry.getPrompts(req.getServer())) {
            Map<String, Object> m = new LinkedHashMap<>();
            m.put("name", p.getName());
            if (!p.getDescription().isEmpty()) m.put("description", p.getDescription());
            List<Map<String, Object>> args = new ArrayList<>();
            for (McpPromptDef.Arg a : p.getArguments()) {
                Map<String, Object> am = new LinkedHashMap<>();
                am.put("name", a.name);
                if (!a.description.isEmpty()) am.put("description", a.description);
                am.put("required", a.required);
                args.add(am);
            }
            m.put("arguments", args);
            out.add(m);
        }
        return Collections.singletonMap("prompts", out);
    }

    @SuppressWarnings("unchecked")
    private Object promptsGet(McpRequest req, Map<String, Object> params) throws JsonRpcException {
        String name = params.get("name") instanceof String ? (String) params.get("name") : null;
        if (UtilValidate.isEmpty(name)) throw new JsonRpcException(JsonRpc.INVALID_PARAMS, "Missing prompt name");
        Map<String, String> arguments = new LinkedHashMap<>();
        if (params.get("arguments") instanceof Map) {
            for (Map.Entry<String, Object> e : ((Map<String, Object>) params.get("arguments")).entrySet()) {
                arguments.put(e.getKey(), e.getValue() != null ? String.valueOf(e.getValue()) : null);
            }
        }
        for (McpPromptDef p : registry.getPrompts(req.getServer())) {
            if (!p.getName().equals(name)) continue;
            for (McpPromptDef.Arg a : p.getArguments()) {
                if (a.required && UtilValidate.isEmpty(arguments.get(a.name))) {
                    throw new JsonRpcException(JsonRpc.INVALID_PARAMS, "Missing prompt argument: " + a.name);
                }
            }
            try {
                String text = p.getRenderer().render(new McpCallContext(req), arguments);
                Map<String, Object> message = new LinkedHashMap<>();
                message.put("role", "user");
                message.put("content", textContent(McpRedactor.stripControl(text)));
                Map<String, Object> result = new LinkedHashMap<>();
                result.put("description", p.getDescription());
                result.put("messages", Collections.singletonList(message));
                return result;
            } catch (Exception e) {
                throw new JsonRpcException(JsonRpc.SERVER_ERROR, "Prompt failed: " + safe(e));
            }
        }
        throw new JsonRpcException(JsonRpc.INVALID_PARAMS, "Unknown prompt: " + name);
    }

    // ---- cursors ----

    private static String cursor(int offset) {
        return Base64.getUrlEncoder().withoutPadding().encodeToString(("o:" + offset).getBytes(java.nio.charset.StandardCharsets.UTF_8));
    }

    private static int cursorOffset(Object cursor) throws JsonRpcException {
        if (!(cursor instanceof String) || ((String) cursor).isEmpty()) return 0;
        try {
            String s = new String(Base64.getUrlDecoder().decode((String) cursor), java.nio.charset.StandardCharsets.UTF_8);
            if (!s.startsWith("o:")) throw new IllegalArgumentException();
            return Math.max(0, Integer.parseInt(s.substring(2)));
        } catch (RuntimeException e) {
            throw new JsonRpcException(JsonRpc.INVALID_PARAMS, "Invalid cursor");
        }
    }
}

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
package com.ilscipio.scipio.mcp.catalog;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceValidationException;

import com.ilscipio.scipio.mcp.registry.McpAnnotationReader;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpResult;
import com.ilscipio.scipio.mcp.registry.McpServerDef;
import com.ilscipio.scipio.mcp.registry.McpToolDef;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.registry.McpToolProvider;
import com.ilscipio.scipio.mcp.registry.McpTopicTool;
import com.ilscipio.scipio.mcp.security.McpConfig;
import com.ilscipio.scipio.mcp.security.McpPolicy;
import com.ilscipio.scipio.mcp.security.McpUsageTracker;
import com.ilscipio.scipio.mcp.skill.SkillProvider;
import com.ilscipio.scipio.mcp.tool.DocumentTools;

/**
 * SCIPIO: 4.0.0: The core tools every server shares: identity, app discovery, service catalog and gateway,
 * entity discovery and guarded entity access.
 */
public final class CoreToolProvider implements McpToolProvider {

    @Override
    public List<McpToolDef> getTools(McpServerDef server) {
        List<McpToolDef> tools = new ArrayList<>();
        tools.add(whoami());
        tools.add(McpTopicTool.build("scipio_apps", "Applications", "Applications, their tools and the agent skills.", 10, true,
                Arrays.asList(listApps(), appTools(), callAppTool(), SkillProvider.skillList(), SkillProvider.skillGet())));
        tools.add(McpTopicTool.build("scipio_service", "Services", "Scipio services: search, describe, call.", 20, false,
                Arrays.asList(searchServices(), describeService(), callService())));
        tools.add(McpTopicTool.build("scipio_entity", "Entities", "Entity (table) access: list, describe, find, store, remove.", 30, false,
                Arrays.asList(listEntities(), describeEntity(), findEntity(), storeEntity(), removeEntity())));
        tools.add(DocumentTools.documentTool());
        return tools;
    }

    // ---- schema helpers ----

    static Map<String, Object> prop(String type, String description) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("type", type);
        m.put("description", description);
        return m;
    }

    static Map<String, Object> arrayProp(String description, Map<String, Object> items) {
        Map<String, Object> m = prop("array", description);
        m.put("items", items);
        return m;
    }

    static Map<String, Object> schema(Map<String, Object> props, String... required) {
        Map<String, Object> s = McpToolDef.emptyObjectSchema();
        s.put("properties", props);
        if (required.length > 0) s.put("required", Arrays.asList(required));
        return s;
    }

    static String str(Map<String, Object> args, String key) {
        Object v = args.get(key);
        return v != null ? String.valueOf(v).trim() : null;
    }

    static Integer integer(Map<String, Object> args, String key) {
        Object v = args.get(key);
        if (v instanceof Number) return ((Number) v).intValue();
        if (v instanceof String && !((String) v).isEmpty()) return Integer.valueOf((String) v);
        return null;
    }

    @SuppressWarnings("unchecked")
    static List<String> strings(Map<String, Object> args, String key) {
        Object v = args.get(key);
        if (v instanceof List) {
            List<String> out = new ArrayList<>();
            for (Object o : (List<Object>) v) out.add(String.valueOf(o));
            return out;
        }
        if (v instanceof String && !((String) v).isEmpty()) return Arrays.asList(((String) v).split("\\s*,\\s*"));
        return null;
    }

    @SuppressWarnings("unchecked")
    static Map<String, Object> map(Map<String, Object> args, String key) {
        Object v = args.get(key);
        return v instanceof Map ? (Map<String, Object>) v : null;
    }

    static String componentFilter(McpCallContext ctx, Map<String, Object> args) {
        String app = str(args, "application");
        if (UtilValidate.isNotEmpty(app)) return "*".equals(app) ? null : app;
        return ctx.getServer().isHub() ? null : (UtilValidate.isNotEmpty(ctx.getServer().getComponent()) ? ctx.getServer().getComponent() : null);
    }

    // ---- tools ----

    McpToolDef whoami() {
        return McpToolDef.builder("scipio_whoami").title("Who am I")
                .description("Acting user, token limits, current server and store context.")
                .inputSchema(schema(new LinkedHashMap<>())).readOnly(true).publicAccess(true).source("CoreToolProvider")
                .executor((ctx, args) -> {
                    Map<String, Object> out = new LinkedHashMap<>();
                    out.put("anonymous", ctx.isAnonymous());
                    if (!ctx.isAnonymous()) {
                        out.put("userLoginId", ctx.getUserLoginId());
                        out.put("partyId", ctx.getPartyId());
                        out.put("tokenId", ctx.getPrincipal().getTokenId());
                        out.put("tokenName", ctx.getPrincipal().getToken().getString("tokenName"));
                        out.put("readOnlyToken", ctx.getPrincipal().isReadOnly());
                        List<String> perms = new ArrayList<>();
                        for (String p : Arrays.asList("MCP_ACCESS", "MCP_GATEWAY", "MCP_ENTITY_READ", "MCP_ENTITY_WRITE", "MCP_ADMIN")) {
                            if (ctx.hasPermission(p)) perms.add(p);
                        }
                        for (String base : ctx.getRequest().getBasePermissions()) {
                            for (String a : Arrays.asList("_VIEW", "_CREATE", "_UPDATE", "_DELETE", "_ADMIN")) {
                                if (ctx.getSecurity().hasEntityPermission(base, a, ctx.getUserLogin())) perms.add(base + a);
                            }
                        }
                        out.put("permissions", perms);
                    }
                    out.put("webapp", ctx.getWebappName());
                    out.put("server", ctx.getServer().getName());
                    out.put("component", ctx.getServer().getComponent());
                    out.put("hub", ctx.getServer().isHub());
                    if (ctx.getWebSiteId() != null) out.put("webSiteId", ctx.getWebSiteId());
                    if (ctx.getProductStoreId() != null) out.put("productStoreId", ctx.getProductStoreId());
                    if (ctx.getCurrencyUomId() != null) out.put("currencyUomId", ctx.getCurrencyUomId());
                    out.put("locale", ctx.getLocale().toLanguageTag());
                    out.put("timeZone", ctx.getTimeZone().getID());
                    return McpResult.ok(out);
                }).build();
    }

    McpToolDef listApps() {
        return McpToolDef.builder("list").title("List applications")
                .description("List every application with its MCP endpoint, server and base permission.")
                .inputSchema(schema(new LinkedHashMap<>())).readOnly(true).source("CoreToolProvider")
                .executor((ctx, args) -> {
                    List<Map<String, Object>> apps = new ArrayList<>();
                    List<String> excluded = McpConfig.getExcludedWebapps();
                    com.ilscipio.scipio.mcp.registry.McpRegistry registry = com.ilscipio.scipio.mcp.registry.McpRegistry.get(ctx.getDispatcher());
                    for (ComponentConfig.WebappInfo wi : ComponentConfig.getAllWebappResourceInfos()) {
                        String ctxRoot = wi.getContextRoot();
                        String name = ctxRoot != null && ctxRoot.startsWith("/") ? ctxRoot.substring(1) : ctxRoot;
                        if (UtilValidate.isEmpty(name) || excluded.contains(name)) continue;
                        Map<String, Object> row = new LinkedHashMap<>();
                        row.put("webapp", name);
                        row.put("title", wi.getTitle());
                        row.put("component", wi.getComponentConfig().getComponentName());
                        row.put("mcpPath", "/" + name + "/" + McpConfig.getPathSegment());
                        McpServerDef s = registry.resolve(name, wi.getComponentConfig().getComponentName());
                        row.put("server", s.getName());
                        row.put("profile", !s.isSynthetic());
                        row.put("mcpUrl", baseUrl(ctx) + "/" + name + "/" + McpConfig.getPathSegment());
                        row.put("basePermission", Arrays.asList(wi.getBasePermission()));
                        List<String> core = new ArrayList<>();
                        for (McpToolDef t : s.getTools()) core.add(t.getName());
                        row.put("coreTools", core);
                        apps.add(row);
                    }
                    Map<String, Object> out = new LinkedHashMap<>();
                    out.put("apps", apps);
                    return McpResult.ok(out);
                }).build();
    }

    static String baseUrl(McpCallContext ctx) {
        javax.servlet.http.HttpServletRequest r = ctx.getRequest().getHttpRequest();
        String scheme = r.isSecure() ? "https" : (r.getHeader("X-Forwarded-Proto") != null ? r.getHeader("X-Forwarded-Proto") : r.getScheme());
        String host = r.getHeader("Host");
        if (host == null || host.isEmpty()) host = r.getServerName() + ":" + r.getServerPort();
        return scheme + "://" + host;
    }

    /** Compact tool row for app tool listings. */
    @SuppressWarnings("unchecked")
    static Map<String, Object> appToolRow(McpToolDef t) {
        Map<String, Object> row = new LinkedHashMap<>();
        row.put("name", t.getName());
        row.put("description", t.getDescription());
        row.put("featured", t.isFeatured());
        row.put("readOnly", t.isReadOnly());
        row.put("destructive", t.isDestructive());
        if (t.getServiceName() != null) row.put("service", t.getServiceName());
        if (t.isPublicAccess()) row.put("public", true);
        if (t.isRequiresConfirmation()) row.put("requiresConfirmation", true);
        if (t.isComposite()) row.put("actions", new ArrayList<>(t.getActions().keySet()));
        Object props = t.getInputSchema().get("properties");
        if (props instanceof Map) row.put("parameters", new ArrayList<>(((Map<String, Object>) props).keySet()));
        Object req = t.getInputSchema().get("required");
        if (req instanceof List) row.put("required", req);
        return row;
    }

    McpToolDef appTools() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("application", prop("string", "Server, webapp or component name, e.g. order, catalog, party. Omit for all."));
        props.put("featuredOnly", prop("boolean", "Only featured tools (default false)"));
        return McpToolDef.builder("tools").title("List application tools")
                .description("List the tools and actions of one application, or of every application.")
                .inputSchema(schema(props)).readOnly(true).source("CoreToolProvider")
                .executor((ctx, args) -> {
                    com.ilscipio.scipio.mcp.registry.McpRegistry registry = com.ilscipio.scipio.mcp.registry.McpRegistry.get(ctx.getDispatcher());
                    String app = str(args, "application");
                    boolean featuredOnly = Boolean.TRUE.equals(args.get("featuredOnly"));
                    List<McpServerDef> servers = new ArrayList<>();
                    if (UtilValidate.isNotEmpty(app)) {
                        McpServerDef s = registry.findServer(app);
                        if (s == null) throw new McpToolException("Unknown application " + app + "; use action list");
                        servers.add(s);
                    } else {
                        servers.addAll(registry.getServers());
                    }
                    List<Map<String, Object>> out = new ArrayList<>();
                    for (McpServerDef s : servers) {
                        if (s.isHub()) continue;
                        com.ilscipio.scipio.mcp.registry.McpRegistry.WebappBinding binding = registry.webappBindingFor(s);
                        List<Map<String, Object>> rows = new ArrayList<>();
                        List<McpToolDef> sorted = new ArrayList<>(s.getTools());
                        sorted.sort((a, b) -> Boolean.compare(b.isFeatured(), a.isFeatured()));
                        for (McpToolDef t : sorted) {
                            if (featuredOnly && !t.isFeatured()) continue;
                            rows.add(appToolRow(t));
                        }
                        Map<String, Object> row = new LinkedHashMap<>();
                        row.put("application", s.getName());
                        row.put("title", s.getTitle());
                        row.put("component", s.getComponent());
                        row.put("webapp", binding.webappName);
                        row.put("mcpUrl", baseUrl(ctx) + "/" + binding.webappName + "/" + McpConfig.getPathSegment());
                        row.put("toolCount", rows.size());
                        row.put("tools", rows);
                        out.add(row);
                    }
                    Map<String, Object> result = new LinkedHashMap<>();
                    result.put("apps", out);
                    return McpResult.ok(result);
                }).build();
    }

    McpToolDef callAppTool() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("application", prop("string", "Server, webapp or component name"));
        props.put("tool", prop("string", "Tool name (see action tools)"));
        Map<String, Object> arguments = prop("object", "Arguments for that tool, including its action");
        arguments.put("additionalProperties", true);
        props.put("arguments", arguments);
        return McpToolDef.builder("call").title("Call application tool")
                .description("Run one application tool from here; the application permission rules apply.")
                .inputSchema(schema(props, "application", "tool")).readOnly(false).destructive(true).idempotent(false)
                .tags("gateway").source("CoreToolProvider")
                .executor((ctx, args) -> {
                    com.ilscipio.scipio.mcp.registry.McpRegistry registry = com.ilscipio.scipio.mcp.registry.McpRegistry.get(ctx.getDispatcher());
                    String app = str(args, "application");
                    String toolName = str(args, "tool");
                    McpServerDef target = registry.findServer(app);
                    if (target == null) throw new McpToolException("Unknown application " + app + "; use action list");
                    McpToolDef tool = null;
                    for (McpToolDef t : target.getTools()) {
                        if (t.getName().equals(toolName)) { tool = t; break; }
                    }
                    if (tool == null) throw new McpToolException("Unknown tool " + toolName + " in " + target.getName() + "; use action tools");
                    Map<String, Object> toolArgs = map(args, "arguments");
                    if (toolArgs == null) toolArgs = new LinkedHashMap<>();
                    com.ilscipio.scipio.mcp.registry.McpRegistry.WebappBinding binding = registry.webappBindingFor(target);
                    com.ilscipio.scipio.mcp.web.McpRequest derived = ctx.getRequest().forServer(target, binding.webappName, binding.basePermissions);
                    try {
                        McpPolicy.checkServerAccess(derived);
                    } catch (com.ilscipio.scipio.mcp.security.McpAuthException e) {
                        throw McpToolException.denied("Access to " + target.getName() + " denied: " + e.getMessage());
                    }
                    ModelService backing = tool.getServiceName() != null ? ctx.getDispatcher().getDispatchContext().getModelServiceOrNull(tool.getServiceName()) : null;
                    McpPolicy.Decision d = McpPolicy.checkTool(derived, tool, backing);
                    if (!d.allowed) throw McpToolException.denied(d.reason);
                    List<String> problems = JsonSchemaValidator.validate(tool.getInputSchema(), toolArgs);
                    if (!problems.isEmpty()) throw new McpToolException("Invalid arguments for " + toolName + ": " + String.join("; ", problems));
                    boolean error = true;
                    try {
                        McpResult r = tool.getExecutor().execute(new McpCallContext(derived), toolArgs);
                        error = r.isError();
                        return r;
                    } finally {
                        McpUsageTracker.get().record(ctx.getDelegator(), target.getName(), tool.getName(), error);
                    }
                }).build();
    }

    McpToolDef searchServices() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("query", prop("string", "Words from the service name or purpose; empty lists the most used"));
        props.put("application", prop("string", "Component filter, e.g. order, product; default this application; * for all"));
        props.put("limit", prop("integer", "Max rows (default 20, max 100)"));
        props.put("includeUncallable", prop("boolean", "Also list services this token may not call (default false)"));
        return McpToolDef.builder("search").title("Search services")
                .description("Find services by name or purpose; featured and most used first.")
                .inputSchema(schema(props)).readOnly(true).source("CoreToolProvider")
                .executor((ctx, args) -> {
                    ServiceCatalog catalog = ServiceCatalog.get(ctx.getDispatcher());
                    String component = componentFilter(ctx, args);
                    Integer limit = integer(args, "limit");
                    int max = limit != null && limit > 0 ? Math.min(limit, 100) : 20;
                    boolean includeUncallable = Boolean.TRUE.equals(args.get("includeUncallable"));
                    McpServerDef server = ctx.getServer();
                    Set<String> featured = new HashSet<>(server.getFeaturedServices());
                    List<ServiceCatalog.Hit> hits = catalog.search(str(args, "query"), component, featured,
                            e -> McpPolicy.isServiceVisible(server, e.name, e.component) || UtilValidate.isNotEmpty(str(args, "application")),
                            ctx.getDelegator(), includeUncallable ? max : max * 3);
                    List<Map<String, Object>> rows = new ArrayList<>();
                    for (ServiceCatalog.Hit h : hits) {
                        ModelService svc = ctx.getDispatcher().getDispatchContext().getModelServiceOrNull(h.entry.name);
                        McpPolicy.Decision d = svc != null ? McpPolicy.checkService(ctx.getRequest(), svc, false, false) : McpPolicy.Decision.deny("unknown");
                        if (!d.allowed && !includeUncallable) continue;
                        Map<String, Object> row = new LinkedHashMap<>();
                        row.put("name", h.entry.name);
                        row.put("description", h.entry.description);
                        row.put("component", h.entry.component);
                        row.put("featured", h.featured);
                        row.put("usedByUi", catalog.isUsedByUi(h.entry.name));
                        row.put("callCount", McpUsageTracker.get().getCallCount(ctx.getDelegator(), "scipio_service:" + h.entry.name));
                        row.put("readOnly", h.entry.readOnly);
                        row.put("callable", d.allowed);
                        if (!d.allowed) row.put("reason", d.reason);
                        if (h.entry.deprecated) row.put("deprecated", true);
                        rows.add(row);
                        if (rows.size() >= max) break;
                    }
                    Map<String, Object> out = new LinkedHashMap<>();
                    out.put("services", rows);
                    return McpResult.ok(out);
                }).build();
    }

    McpToolDef describeService() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("name", prop("string", "Service name, e.g. createOrder"));
        return McpToolDef.builder("describe").title("Describe service")
                .description("Input and output schema, permissions and metadata of one service.")
                .inputSchema(schema(props, "name")).readOnly(true).source("CoreToolProvider")
                .executor((ctx, args) -> {
                    String name = str(args, "name");
                    ModelService svc = ctx.getDispatcher().getDispatchContext().getModelServiceOrNull(name);
                    if (svc == null) throw new McpToolException("Unknown service " + name);
                    Map<String, Object> out = ServiceCatalog.get(ctx.getDispatcher()).describe(svc);
                    McpPolicy.Decision d = McpPolicy.checkService(ctx.getRequest(), svc, false, false);
                    out.put("callable", d.allowed);
                    if (!d.allowed) out.put("reason", d.reason);
                    return McpResult.ok(out);
                }).build();
    }

    McpToolDef callService() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("name", prop("string", "Service name"));
        Map<String, Object> paramsProp = prop("object", "Service input parameters (see action describe)");
        paramsProp.put("additionalProperties", true);
        props.put("params", paramsProp);
        props.put("dryRun", prop("boolean", "Validate the parameters only; do not run (default false)"));
        props.put("idempotencyKey", prop("string", "Client key; a repeated call with the same key returns the first result"));
        return McpToolDef.builder("call").title("Call service")
                .description("Run one service as the token user; needs MCP_GATEWAY. Prefer application tools.")
                .inputSchema(schema(props, "name")).readOnly(false).destructive(true).idempotent(false)
                .tags("gateway").source("CoreToolProvider")
                .executor((ctx, args) -> {
                    String name = str(args, "name");
                    ModelService svc = ctx.getDispatcher().getDispatchContext().getModelServiceOrNull(name);
                    if (svc == null) throw new McpToolException("Unknown service " + name);
                    McpPolicy.Decision d = McpPolicy.checkService(ctx.getRequest(), svc, false, false);
                    if (!d.allowed) throw McpToolException.denied(d.reason);
                    Map<String, Object> params = map(args, "params");
                    if (params == null) params = new LinkedHashMap<>();
                    List<String> problems = JsonSchemaValidator.validate(ServiceSchemaBuilder.inputSchema(svc, null, null), params);
                    if (!problems.isEmpty()) throw new McpToolException("Invalid parameters: " + String.join("; ", problems));
                    Map<String, Object> converted = ServiceSchemaBuilder.applyDefaults(svc, convertParams(ctx, svc, params));
                    if (Boolean.TRUE.equals(args.get("dryRun"))) {
                        try {
                            svc.validate(ctx.serviceContext(converted), ModelService.IN_PARAM, ctx.getLocale());
                        } catch (ServiceValidationException e) {
                            throw new McpToolException("Validation failed: " + e.getMessage());
                        }
                        Map<String, Object> out = new LinkedHashMap<>();
                        out.put("valid", true);
                        out.put("service", name);
                        return McpResult.ok(out);
                    }
                    boolean error = true;
                    try {
                        Map<String, Object> result = ctx.runService(name, converted);
                        error = false;
                        return McpResult.ok(McpAnnotationReader.serviceResultToJson(svc, result));
                    } finally {
                        McpUsageTracker.get().record(ctx.getDelegator(), ctx.getServer().getName(), "scipio_service:" + name, error);
                    }
                }).build();
    }

    static Map<String, Object> convertParams(McpCallContext ctx, ModelService svc, Map<String, Object> params) throws McpToolException {
        return McpAnnotationReader.convertServiceParams(svc, params);
    }

    McpToolDef listEntities() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("query", prop("string", "Text filter on entity name or description"));
        props.put("application", prop("string", "Component filter; default this application; * for all"));
        props.put("limit", prop("integer", "Max rows (default 50, max 500)"));
        return McpToolDef.builder("list").title("List entities")
                .description("List entity (table) names with their component.")
                .inputSchema(schema(props)).readOnly(true).source("CoreToolProvider")
                .executor((ctx, args) -> {
                    Map<String, Object> out = new LinkedHashMap<>();
                    out.put("entities", EntityCatalog.listEntities(ctx.getDelegator(), componentFilter(ctx, args), str(args, "query"), ctx.limit(integer(args, "limit"))));
                    return McpResult.ok(out);
                }).build();
    }

    McpToolDef describeEntity() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("entityName", prop("string", "Entity name, e.g. OrderHeader"));
        return McpToolDef.builder("describe").title("Describe entity")
                .description("Fields, primary keys and relations of one entity.")
                .inputSchema(schema(props, "entityName")).readOnly(true).source("CoreToolProvider")
                .executor((ctx, args) -> McpResult.ok(EntityCatalog.describe(ctx.getDelegator(), str(args, "entityName")))).build();
    }

    McpToolDef findEntity() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("entityName", prop("string", "Entity name"));
        Map<String, Object> cond = prop("object", "One condition");
        Map<String, Object> cprops = new LinkedHashMap<>();
        cprops.put("field", prop("string", "Field name"));
        Map<String, Object> op = prop("string", "Operator (default eq)");
        op.put("enum", Arrays.asList("eq", "ne", "lt", "le", "gt", "ge", "like", "in", "notIn", "isNull", "notNull"));
        cprops.put("op", op);
        cprops.put("value", new LinkedHashMap<String, Object>(Collections.singletonMap("description", "Value; array for in/notIn; % wildcards for like")));
        cond.put("properties", cprops);
        cond.put("required", Collections.singletonList("field"));
        props.put("conditions", arrayProp("Conditions combined with AND", cond));
        props.put("fields", arrayProp("Fields to return (default all)", prop("string", "Field name")));
        props.put("orderBy", arrayProp("Sort fields; prefix with - for descending", prop("string", "Field name")));
        props.put("limit", prop("integer", "Max rows (default 50, max 500)"));
        return McpToolDef.builder("find").title("Find entity records")
                .description("Read records of one entity with simple conditions; entities outside the allowlist need MCP_ENTITY_READ.")
                .inputSchema(schema(props, "entityName")).readOnly(true).source("CoreToolProvider")
                .executor((ctx, args) -> {
                    @SuppressWarnings("unchecked")
                    List<Map<String, Object>> conditions = args.get("conditions") instanceof List ? (List<Map<String, Object>>) args.get("conditions") : null;
                    List<Map<String, Object>> rows = EntityCatalog.find(ctx, str(args, "entityName"), conditions, strings(args, "fields"), strings(args, "orderBy"), ctx.limit(integer(args, "limit")));
                    Map<String, Object> out = new LinkedHashMap<>();
                    out.put("count", rows.size());
                    out.put("records", rows);
                    return McpResult.ok(out);
                }).build();
    }

    McpToolDef storeEntity() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("entityName", prop("string", "Entity name"));
        Map<String, Object> fields = prop("object", "Field values (primary key included)");
        fields.put("additionalProperties", true);
        props.put("fields", fields);
        props.put("create", prop("boolean", "true to create, false to update (default false)"));
        return McpToolDef.builder("store").title("Store entity record")
                .description("Create or update one record directly, without service logic; needs MCP_ENTITY_WRITE.")
                .inputSchema(schema(props, "entityName", "fields")).readOnly(false).destructive(true).idempotent(true)
                .requiresConfirmation(true).permission("MCP_ENTITY_WRITE").source("CoreToolProvider")
                .executor((ctx, args) -> McpResult.ok(EntityCatalog.store(ctx, str(args, "entityName"), map(args, "fields"), Boolean.TRUE.equals(args.get("create"))))).build();
    }

    McpToolDef removeEntity() {
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("entityName", prop("string", "Entity name"));
        Map<String, Object> pk = prop("object", "Primary key field values");
        pk.put("additionalProperties", true);
        props.put("pk", pk);
        return McpToolDef.builder("remove").title("Remove entity record")
                .description("Delete one record by primary key, without service logic; needs MCP_ENTITY_WRITE.")
                .inputSchema(schema(props, "entityName", "pk")).readOnly(false).destructive(true).idempotent(true)
                .requiresConfirmation(true).permission("MCP_ENTITY_WRITE").source("CoreToolProvider")
                .executor((ctx, args) -> McpResult.ok(EntityCatalog.remove(ctx, str(args, "entityName"), map(args, "pk")))).build();
    }
}

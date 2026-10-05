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
package com.ilscipio.scipio.mcp.service;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.mcp.catalog.ServiceCatalog;
import com.ilscipio.scipio.mcp.registry.McpRegistry;
import com.ilscipio.scipio.mcp.registry.McpServerDef;
import com.ilscipio.scipio.mcp.security.McpConfig;
import com.ilscipio.scipio.mcp.security.McpTokenRoutes;
import com.ilscipio.scipio.mcp.security.McpTokenUtil;
import com.ilscipio.scipio.service.def.Attribute;
import com.ilscipio.scipio.service.def.Service;

/**
 * SCIPIO: 4.0.0: Token management and catalog services (used by the Webtools pages and by scripts).
 */
public class McpServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String LOCATION = "com.ilscipio.scipio.mcp.service.McpServices";

    @Service(
        name = "createMcpAccessToken",
        engine = "java", location = LOCATION, invoke = "createMcpAccessToken",
        description = "Create an MCP access token for a user login. The raw token is returned once. Own tokens need MCP_ACCESS; other users' tokens need MCP_ADMIN.",
        auth = "true",
        attributes = {
            @Attribute(name = "userLoginId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "tokenName", type = "String", mode = "IN"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "webapps", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "readOnly", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expiresDate", type = "java.sql.Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "expiresInDays", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "noExpiry", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "remoteAddrAllow", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "maxOrderAmount", type = "java.math.BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "tokenId", type = "String", mode = "OUT"),
            @Attribute(name = "token", type = "String", mode = "OUT"),
            @Attribute(name = "tokenPrefix", type = "String", mode = "OUT")
        }
    )
    public interface CreateMcpAccessToken {}

    public static Map<String, Object> createMcpAccessToken(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Security security = dctx.getSecurity();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String caller = userLogin.getString("userLoginId");
        String target = (String) context.get("userLoginId");
        if (UtilValidate.isEmpty(target)) target = caller;
        boolean admin = security.hasPermission("MCP_ADMIN", userLogin);
        if (!target.equals(caller) && !admin) {
            return ServiceUtil.returnError("MCP_ADMIN is required to create tokens for other users");
        }
        if (!admin && !security.hasPermission("MCP_ACCESS", userLogin)) {
            return ServiceUtil.returnError("MCP_ACCESS is required to create a token");
        }
        try {
            GenericValue targetLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", target).queryOne();
            if (targetLogin == null) return ServiceUtil.returnError("User login not found: " + target);
            if ("N".equals(targetLogin.getString("enabled"))) return ServiceUtil.returnError("User login is disabled: " + target);
            if (McpConfig.getTokenDenyUsers().contains(target)) return ServiceUtil.returnError("User login " + target + " may not own a token");

            Timestamp expires = (Timestamp) context.get("expiresDate");
            Long days = (Long) context.get("expiresInDays");
            boolean noExpiry = "Y".equals(context.get("noExpiry"));
            long maxMillis = System.currentTimeMillis() + McpConfig.getTokenMaxExpiryDays() * 86_400_000L;
            if (noExpiry) {
                if (!admin || !McpConfig.isTokenAllowNoExpiry()) {
                    return ServiceUtil.returnError("A token without expiry needs MCP_ADMIN and mcp.token.allowNoExpiry=true");
                }
                expires = null;
            } else {
                if (expires == null) {
                    long d = days != null && days > 0 ? days : McpConfig.getTokenDefaultExpiryDays();
                    expires = new Timestamp(System.currentTimeMillis() + d * 86_400_000L);
                }
                if (expires.getTime() > maxMillis) {
                    return ServiceUtil.returnError("Expiry exceeds mcp.token.maxExpiryDays=" + McpConfig.getTokenMaxExpiryDays());
                }
            }
            McpTokenUtil.Generated g = McpTokenUtil.generate();
            GenericValue token = delegator.makeValue("McpAccessToken");
            token.set("tokenId", g.tokenId);
            token.set("userLoginId", target);
            token.set("tokenName", context.get("tokenName"));
            token.set("description", context.get("description"));
            token.set("tokenHash", g.hash);
            token.set("tokenPrefix", g.displayPrefix);
            token.set("webapps", UtilValidate.isNotEmpty((String) context.get("webapps")) ? context.get("webapps") : "*");
            token.set("readOnly", "Y".equals(context.get("readOnly")) ? "Y" : "N");
            token.set("expiresDate", expires);
            token.set("disabled", "N");
            token.set("remoteAddrAllow", context.get("remoteAddrAllow"));
            token.set("maxOrderAmount", context.get("maxOrderAmount"));
            token.set("createdByUserLogin", caller);
            token.set("createdDate", UtilDateTime.nowTimestamp());
            token.set("tenantId", delegator.getDelegatorTenantId()); // SCIPIO: 4.0.0: pooled runtime: store of the token (G5)
            token.create();
            McpTokenRoutes.register(delegator, g.tokenId); // SCIPIO: 4.0.0: pooled runtime: master route (G2)
            Debug.logInfo("[MCP] token created id=" + g.tokenId + " for user=" + target + " by=" + caller, "mcp.audit");
            Map<String, Object> result = ServiceUtil.returnSuccess("Token created. Copy it now; it is not shown again.");
            result.put("tokenId", g.tokenId);
            result.put("token", g.rawToken);
            result.put("tokenPrefix", g.displayPrefix);
            return result;
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError("Could not create token: " + e.getMessage());
        }
    }

    @Service(
        name = "revokeMcpAccessToken",
        engine = "java", location = LOCATION, invoke = "revokeMcpAccessToken",
        description = "Disable an MCP access token. Owner or MCP_ADMIN.",
        auth = "true",
        attributes = {
            @Attribute(name = "tokenId", type = "String", mode = "IN")
        }
    )
    public interface RevokeMcpAccessToken {}

    public static Map<String, Object> revokeMcpAccessToken(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String tokenId = (String) context.get("tokenId");
        try {
            GenericValue token = EntityQuery.use(delegator).from("McpAccessToken").where("tokenId", tokenId).queryOne();
            if (token == null) return ServiceUtil.returnError("Token not found: " + tokenId);
            boolean owner = userLogin.getString("userLoginId").equals(token.getString("userLoginId"));
            if (!owner && !dctx.getSecurity().hasPermission("MCP_ADMIN", userLogin)) {
                return ServiceUtil.returnError("MCP_ADMIN is required to revoke tokens of other users");
            }
            token.set("disabled", "Y");
            token.store();
            Debug.logInfo("[MCP] token revoked id=" + tokenId + " by=" + userLogin.getString("userLoginId"), "mcp.audit");
            return ServiceUtil.returnSuccess("Token revoked");
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError("Could not revoke token: " + e.getMessage());
        }
    }

    @Service(
        name = "mcpListServers",
        engine = "java", location = LOCATION, invoke = "mcpListServers",
        description = "List the MCP server profiles and their tools.",
        auth = "true",
        attributes = {
            @Attribute(name = "servers", type = "List", mode = "OUT")
        }
    )
    public interface McpListServers {}

    public static Map<String, Object> mcpListServers(DispatchContext dctx, Map<String, ? extends Object> context) {
        McpRegistry registry = McpRegistry.get(dctx.getDispatcher());
        List<Map<String, Object>> servers = new ArrayList<>();
        for (McpServerDef s : registry.getServers()) {
            Map<String, Object> row = new LinkedHashMap<>();
            row.put("name", s.getName());
            row.put("title", s.getTitle());
            row.put("description", s.getDescription());
            row.put("component", s.getComponent());
            row.put("webapps", new ArrayList<>(s.getWebapps()));
            row.put("hub", s.isHub());
            row.put("allowAnonymous", s.isAllowAnonymous());
            row.put("featuredServices", s.getFeaturedServices());
            row.put("entities", new ArrayList<>(s.getEntities()));
            row.put("sourceClass", s.getSourceClass());
            List<Map<String, Object>> tools = new ArrayList<>();
            for (com.ilscipio.scipio.mcp.registry.McpToolDef t : registry.getTools(s)) {
                Map<String, Object> tm = new LinkedHashMap<>();
                tm.put("name", t.getName());
                tm.put("description", t.getDescription());
                tm.put("readOnly", t.isReadOnly());
                tm.put("destructive", t.isDestructive());
                tm.put("featured", t.isFeatured());
                tm.put("publicAccess", t.isPublicAccess());
                tm.put("permission", t.getPermission());
                tm.put("service", t.getServiceName());
                tm.put("source", t.getSource());
                tools.add(tm);
            }
            row.put("tools", tools);
            servers.add(row);
        }
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("servers", servers);
        return result;
    }

    @Service(
        name = "mcpSearchServices",
        engine = "java", location = LOCATION, invoke = "mcpSearchServices",
        description = "Search the service catalog with the MCP ranking.",
        auth = "true",
        attributes = {
            @Attribute(name = "query", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "application", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "limit", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "services", type = "List", mode = "OUT")
        }
    )
    public interface McpSearchServices {}

    public static Map<String, Object> mcpSearchServices(DispatchContext dctx, Map<String, ? extends Object> context) {
        ServiceCatalog catalog = ServiceCatalog.get(dctx.getDispatcher());
        Long limit = (Long) context.get("limit");
        int max = limit != null && limit > 0 ? (int) Math.min(limit, 200) : 50;
        List<Map<String, Object>> rows = new ArrayList<>();
        for (ServiceCatalog.Hit h : catalog.search((String) context.get("query"), (String) context.get("application"), null, null, dctx.getDelegator(), max)) {
            Map<String, Object> row = new LinkedHashMap<>();
            row.put("name", h.entry.name);
            row.put("description", h.entry.description);
            row.put("component", h.entry.component);
            row.put("usedByUi", catalog.isUsedByUi(h.entry.name));
            row.put("readOnly", h.entry.readOnly);
            row.put("guarded", h.entry.guarded);
            row.put("score", h.score);
            rows.add(row);
        }
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("services", rows);
        return result;
    }

    @Service(
        name = "mcpDescribeService",
        engine = "java", location = LOCATION, invoke = "mcpDescribeService",
        description = "Describe a service with its JSON schemas.",
        auth = "true",
        attributes = {
            @Attribute(name = "serviceName", type = "String", mode = "IN"),
            @Attribute(name = "service", type = "Map", mode = "OUT")
        }
    )
    public interface McpDescribeService {}

    public static Map<String, Object> mcpDescribeService(DispatchContext dctx, Map<String, ? extends Object> context) {
        String name = (String) context.get("serviceName");
        ModelService svc = dctx.getModelServiceOrNull(name);
        if (svc == null) return ServiceUtil.returnError("Unknown service " + name);
        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("service", ServiceCatalog.get(dctx.getDispatcher()).describe(svc));
        return result;
    }
}

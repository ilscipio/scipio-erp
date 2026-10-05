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
package com.ilscipio.scipio.mcp;

import java.security.SecureRandom;
import java.util.Arrays;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpRegistry;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.skill.AgentInstallInfo;

/**
 * SCIPIO: 4.0.0: The discovery hub at {@code /admin/mcp} (and {@code /webtools/mcp}). It always exists and carries
 * the core tools with no component filter: list apps, search and call any service, list and read entities, skills.
 * Service calls through the hub need the base permission of the component that owns the service.
 */
@McpServer(name = "admin", title = "Scipio Admin Hub", component = "webtools", webapps = { "admin", "webtools" }, hub = true,
        description = "Discovery hub for the whole installation. scipio_apps lists every application, its endpoint and its tools; "
                + "scipio_service and scipio_entity are not limited to one application. scipio_admin returns client connection snippets.",
        topics = @McpTopic(name = "scipio_admin", title = "Administration", order = 10, featured = true,
                description = "Installation administration: connection info, registry reload, device tokens."))
public final class AdminMcp {

    private static final String ALPHABET = "ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz23456789";
    private static final SecureRandom RANDOM = new SecureRandom();

    private AdminMcp() {}

    @McpTool(topic = "scipio_admin", name = "install_info", description = "Endpoint URLs and client snippets (Claude Code, Cursor, VS Code, curl); never a token.",
            readOnly = true, order = 10)
    public static Object installInfo(McpCallContext ctx,
            @McpParam(name = "webapp", description = "Endpoint webapp, e.g. ordermgr; default admin", required = false) String webapp) {
        return AgentInstallInfo.build(AgentInstallInfo.baseUrl(ctx.getRequest().getHttpRequest()), webapp, null);
    }

    @McpTool(topic = "scipio_admin", name = "reload_registry", description = "Rebuild the tool registry, skills and service catalog without a restart.",
            readOnly = false, destructive = "false", permission = "MCP_ADMIN", requiresConfirmation = true, order = 20)
    public static Object reloadRegistry(McpCallContext ctx) throws McpToolException {
        ctx.requirePermission("MCP_ADMIN");
        McpRegistry.reloadAll();
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("reloaded", true);
        out.put("note", "Registry, skills, service catalog and policy caches rebuild on the next MCP request; re-run tools/list.");
        return out;
    }

    @McpTool(topic = "scipio_admin", name = "device_token_create", description = "Create a shop floor device login and token for one station; the raw token is returned once.",
            readOnly = false, destructive = "false", permission = "MCP_ADMIN", requiresConfirmation = true, order = 30)
    public static Object createDeviceToken(McpCallContext ctx,
            @McpParam(name = "stationName", description = "Station label, e.g. Frame jig 2", required = true) String stationName,
            @McpParam(name = "facilityId", description = "Plant of the station", required = false) String facilityId,
            @McpParam(name = "fixedAssetId", description = "Work center of the station", required = false) String fixedAssetId,
            @McpParam(name = "expiresInDays", description = "Token life in days", required = false) Integer expiresInDays,
            @McpParam(name = "groupId", description = "Security group; default SCIPIO_FLOOR", required = false) String groupId,
            @McpParam(name = "webapps", description = "Endpoints the token may use; default manufacturing", required = false) String webapps) throws McpToolException {
        ctx.requirePermission("MCP_ADMIN");
        if (UtilValidate.isEmpty(stationName)) throw new McpToolException("stationName is required");
        String group = UtilValidate.isNotEmpty(groupId) ? groupId : "SCIPIO_FLOOR";
        String apps = UtilValidate.isNotEmpty(webapps) ? webapps : "manufacturing";
        String slug = stationName.trim().toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9]+", "-").replaceAll("^-+|-+$", "");
        if (slug.isEmpty()) throw new McpToolException("stationName needs at least one letter or digit");
        if (slug.length() > 30) slug = slug.substring(0, 30);
        String userLoginId = uniqueLoginId(ctx, "floor-" + slug);
        String password = randomSecret(24);

        Map<String, Object> user = new LinkedHashMap<>();
        user.put("userLoginId", userLoginId);
        user.put("currentPassword", password);
        user.put("currentPasswordVerify", password);
        user.put("firstName", stationName.trim());
        user.put("lastName", "Floor device");
        user.put("description", "Shop floor device token" + (facilityId != null ? " at " + facilityId : ""));
        Map<String, Object> created = ctx.runService("createPersonAndUserLogin", user);
        String partyId = (String) created.get("partyId");

        Map<String, Object> grp = new LinkedHashMap<>();
        grp.put("userLoginId", userLoginId);
        grp.put("groupId", group);
        grp.put("fromDate", UtilDateTime.nowTimestamp());
        ctx.runService("addUserLoginToSecurityGroup", grp);

        Map<String, Object> tok = new LinkedHashMap<>();
        tok.put("userLoginId", userLoginId);
        tok.put("tokenName", "Floor " + stationName.trim());
        tok.put("webapps", apps);
        StringBuilder desc = new StringBuilder("Device token for station ").append(stationName.trim());
        if (facilityId != null) desc.append("; facilityId=").append(facilityId);
        if (fixedAssetId != null) desc.append("; fixedAssetId=").append(fixedAssetId);
        tok.put("description", desc.toString());
        if (expiresInDays != null) tok.put("expiresInDays", Long.valueOf(expiresInDays.longValue()));
        Map<String, Object> token = ctx.runService("createMcpAccessToken", tok);

        String base = AgentInstallInfo.baseUrl(ctx.getRequest().getHttpRequest());
        List<String> appList = Arrays.asList(apps.split("\\s*,\\s*"));
        String endpoint = base + "/" + appList.get(0) + "/mcp";
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("userLoginId", userLoginId);
        out.put("partyId", partyId);
        out.put("groupId", group);
        out.put("tokenId", token.get("tokenId"));
        out.put("tokenPrefix", token.get("tokenPrefix"));
        out.put("token", token.get("token"));
        out.put("webapps", apps);
        out.put("endpoint", endpoint);
        out.put("qrPayload", "scipio-mcp:" + endpoint + "?station=" + slug + "&token=" + token.get("token"));
        out.put("note", "Print qrPayload as a QR code on the station. The raw token is shown once; the device sends it as Authorization: Bearer.");
        return out;
    }

    private static String uniqueLoginId(McpCallContext ctx, String base) throws McpToolException {
        try {
            String id = base;
            for (int i = 2; i < 100; i++) {
                if (EntityQuery.use(ctx.getDelegator()).from("UserLogin").where("userLoginId", id).queryOne() == null) return id;
                id = base + "-" + i;
            }
        } catch (GenericEntityException e) {
            throw new McpToolException("User login lookup failed: " + e.getMessage());
        }
        throw new McpToolException("Too many device logins for " + base);
    }

    private static String randomSecret(int length) {
        StringBuilder sb = new StringBuilder(length);
        for (int i = 0; i < length; i++) sb.append(ALPHABET.charAt(RANDOM.nextInt(ALPHABET.length())));
        return sb.toString();
    }
}

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
package com.ilscipio.scipio.mcp.security;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Pattern;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;
import org.ofbiz.service.ModelService;

import com.ilscipio.scipio.mcp.catalog.ServiceCatalog;
import com.ilscipio.scipio.mcp.registry.McpServerDef;
import com.ilscipio.scipio.mcp.registry.McpToolDef;
import com.ilscipio.scipio.mcp.web.McpRequest;

/**
 * SCIPIO: 4.0.0: Authorization decisions. Permissions are the token user's permissions; this class adds the
 * MCP-specific gates (MCP_* permissions, webapp base permission, read-only tokens, deny lists).
 *
 * <p>Base permission rule (same as {@code LoginWorker.hasApplicationPermission}): the user must hold
 * {@code <base>_VIEW} for <b>every</b> base permission of the webapp. A write needs {@code <base>_UPDATE}
 * (or {@code _ADMIN}) on every application-specific base; the generic {@code OFBTOOLS} base only needs
 * {@code _VIEW} unless it is the only base.</p>
 *
 * <p>Gateway rule (one rule for every endpoint, hub included): a service call through the gateway needs the
 * base permission of the <b>component that owns the service</b>, not of the endpoint webapp. A service whose
 * component has no webapp base permission needs {@code MCP_ADMIN}, unless the component is listed in
 * {@code mcp.gateway.openComponents}; then the endpoint's own base permission applies.</p>
 */
public final class McpPolicy {

    /** Name prefixes that classify a service as read-only when no better signal exists. */
    private static final Pattern READ_ONLY_NAME = Pattern.compile(
            "^(get|find|list|search|lookup|count|calc|calculate|is|has|describe|check)[A-Z0-9_].*");

    /** Generic base permission shared by every back-office webapp; needs _VIEW only for writes. */
    public static final String GENERIC_BASE = "OFBTOOLS";

    /** Tag of the core tools that run another service or tool and apply the policy to that target themselves. */
    public static final String GATEWAY_TAG = "gateway";

    private static final Map<String, List<List<String>>> COMPONENT_BASES = new ConcurrentHashMap<>();

    /** Test seam: replaces the component-to-base-permission lookup (null = ComponentConfig). */
    static volatile java.util.function.Function<String, List<List<String>>> componentBasesSource;
    /** Test seam: replaces the service-to-component lookup (null = ServiceCatalog). */
    static volatile java.util.function.Function<ModelService, String> serviceComponentSource;

    private McpPolicy() {}

    public static final class Decision {
        public final boolean allowed;
        public final String reason;

        private Decision(boolean allowed, String reason) {
            this.allowed = allowed;
            this.reason = reason;
        }

        public static Decision allow() { return new Decision(true, null); }
        public static Decision deny(String reason) { return new Decision(false, reason); }
    }

    /** Gate for listing and calling anything in a server. Throws 403 for authenticated users without access. */
    public static void checkServerAccess(McpRequest req) throws McpAuthException {
        McpServerDef server = req.getServer();
        if (req.isAnonymous()) {
            if (!server.isAllowAnonymous()) throw McpAuthException.unauthorized("Authentication required");
            return;
        }
        McpPrincipal p = req.getPrincipal();
        if (!p.isWebappAllowed(req.getWebappName())) {
            throw McpAuthException.forbidden("Token not allowed for webapp " + req.getWebappName());
        }
        if (!hasPermission(req, "MCP_ACCESS")) {
            throw McpAuthException.forbidden("Token user lacks MCP_ACCESS");
        }
        if (server.isHub()) {
            if (!hasPermission(req, "MCP_ADMIN") && !hasPermission(req, GENERIC_BASE + "_VIEW")) {
                throw McpAuthException.forbidden("Hub requires MCP_ADMIN or OFBTOOLS_VIEW");
            }
        } else if (!req.getBasePermissions().isEmpty() && !hasBasePermission(req, req.getBasePermissions(), "_VIEW")) {
            throw McpAuthException.forbidden("Token user lacks " + String.join("/", req.getBasePermissions()) + "_VIEW");
        }
        if (UtilValidate.isNotEmpty(server.getRequiredPermission()) && !hasPermission(req, server.getRequiredPermission())) {
            throw McpAuthException.forbidden("Server requires " + server.getRequiredPermission());
        }
    }

    /** Tool-level decision before execution. */
    public static Decision checkTool(McpRequest req, McpToolDef tool, ModelService backingService) {
        if (req.isAnonymous()) {
            return tool.isPublicAccess() ? Decision.allow() : Decision.deny("Authentication required for " + tool.getName());
        }
        McpPrincipal p = req.getPrincipal();
        if (UtilValidate.isNotEmpty(tool.getPermission()) && !hasPermission(req, tool.getPermission())) {
            return Decision.deny("Permission " + tool.getPermission() + " required for " + tool.getName());
        }
        if (tool.getTags().contains(GATEWAY_TAG) || tool.isComposite()) {
            // scipio_service call / scipio_apps call decide per target through checkService / checkTool, and a
            // composite tool checks its resolved action; the endpoint-level write gate must not pre-empt a read.
            return Decision.allow();
        }
        if (p.isReadOnly() && !tool.isReadOnly()) {
            return Decision.deny("Read-only token cannot call " + tool.getName());
        }
        if (backingService != null) {
            // A tool that names its own permission (held, checked above) replaces the webapp base gate of the
            // backing service: a floor device with MANUFACTURING_FLOOR may declare a task without MANUFACTURING_UPDATE.
            return checkService(req, backingService, true, tool.isReadOnly(), UtilValidate.isNotEmpty(tool.getPermission()));
        }
        if (!tool.isReadOnly() && UtilValidate.isEmpty(tool.getPermission()) && !req.getServer().isHub()) {
            List<String> bases = req.getBasePermissions();
            if (bases.isEmpty()) {
                // A public-facing server (shop) owns its write tools; any other permission-less webapp fails closed.
                if (!req.getServer().isAllowAnonymous()) {
                    return Decision.deny("Tool " + tool.getName() + " needs an explicit permission: webapp "
                            + req.getWebappName() + " declares no base permission");
                }
            } else if (!hasBasePermission(req, bases, "_UPDATE")) {
                return Decision.deny("Tool " + tool.getName() + " requires " + String.join("/", bases) + "_UPDATE");
            }
        }
        return Decision.allow();
    }

    /**
     * Decision for a service call. {@code explicit} is true for tools declared in a profile (no MCP_GATEWAY needed).
     * {@code declaredReadOnly} is the tool annotation; it never widens the classification of the service itself.
     */
    public static Decision checkService(McpRequest req, ModelService svc, boolean explicit, boolean declaredReadOnly) {
        return checkService(req, svc, explicit, declaredReadOnly, false);
    }

    /**
     * As above; {@code toolPermissionHeld} is true when the calling tool declares an explicit permission that the
     * user holds. Deny lists, admin-only patterns and the read-only token rule still apply; the component base
     * permission gate does not.
     */
    public static Decision checkService(McpRequest req, ModelService svc, boolean explicit, boolean declaredReadOnly, boolean toolPermissionHeld) {
        String name = svc.name;
        if (McpConfig.anyGlobMatches(McpConfig.getServiceDenyPatterns(), name)) {
            return Decision.deny("Service " + name + " is blocked by policy");
        }
        McpServerDef server = req.getServer();
        if (McpConfig.anyGlobMatches(server.getServiceDeny(), name)) {
            return Decision.deny("Service " + name + " is blocked in server " + server.getName());
        }
        if (req.isAnonymous()) {
            return Decision.deny("Authentication required");
        }
        if (McpConfig.anyGlobMatches(McpConfig.getServiceAdminOnlyPatterns(), name) && !hasPermission(req, "MCP_ADMIN")) {
            return Decision.deny("Service " + name + " requires MCP_ADMIN");
        }
        if (!explicit && !hasPermission(req, "MCP_GATEWAY")) {
            return Decision.deny("MCP_GATEWAY permission required to call services directly");
        }
        boolean classifiedReadOnly = isReadOnlyService(svc);
        boolean readOnly = explicit ? (declaredReadOnly && classifiedReadOnly) : classifiedReadOnly;
        if (req.getPrincipal().isReadOnly() && !readOnly) {
            return Decision.deny("Read-only token cannot call " + name);
        }
        if (hasOwnPermissions(svc)) {
            return Decision.allow(); // the service enforces its permissions with the token user
        }
        if (toolPermissionHeld) {
            return Decision.allow(); // the tool's own permission stands in for the component base permission
        }
        if (!McpConfig.isAllowUnguarded() && !explicit) {
            return Decision.deny("Service " + name + " declares no permissions and unguarded calls are disabled");
        }
        String action = readOnly ? "_VIEW" : "_UPDATE";
        String component = serviceComponent(req, svc);
        List<List<String>> webappBases = componentBasePermissions(component);
        if (webappBases.isEmpty() && McpConfig.getOpenComponents().contains(component)) {
            webappBases = req.getBasePermissions().isEmpty() ? Collections.emptyList()
                    : Collections.singletonList(req.getBasePermissions());
        }
        if (webappBases.isEmpty()) {
            if (hasPermission(req, "MCP_ADMIN")) return Decision.allow();
            return Decision.deny("Service " + name + " belongs to component " + (UtilValidate.isNotEmpty(component) ? component : "(unknown)")
                    + " which has no webapp base permission; MCP_ADMIN required");
        }
        for (List<String> bases : webappBases) {
            if (hasBasePermission(req, bases, action)) return Decision.allow();
        }
        return Decision.deny("Service " + name + " requires " + describe(webappBases) + action + " (component " + component + ")");
    }

    private static String serviceComponent(McpRequest req, ModelService svc) {
        java.util.function.Function<ModelService, String> source = serviceComponentSource;
        if (source != null) return source.apply(svc);
        if (req.getDispatcher() == null) return "";
        return ServiceCatalog.get(req.getDispatcher()).componentOf(svc);
    }

    private static String describe(List<List<String>> webappBases) {
        List<String> parts = new ArrayList<>();
        for (List<String> bases : webappBases) parts.add(String.join("+", bases));
        return String.join(" or ", parts);
    }

    public static boolean hasOwnPermissions(ModelService svc) {
        return UtilValidate.isNotEmpty(svc.permissionServiceName) || (svc.permissionGroups != null && !svc.permissionGroups.isEmpty());
    }

    /** Best-effort read-only classification of a service. Entity-auto writes are always writes. */
    public static boolean isReadOnlyService(ModelService svc) {
        if ("entity-auto".equals(svc.engineName)) {
            String inv = svc.invoke != null ? svc.invoke : "";
            return !(inv.equals("create") || inv.equals("update") || inv.equals("delete") || inv.equals("expire"));
        }
        if (McpConfig.isReadOnlyHeuristic() && svc.name != null) {
            return READ_ONLY_NAME.matcher(svc.name).matches();
        }
        return false;
    }

    /**
     * Base permission sets of every webapp of a component (one list per webapp, {@code NONE} removed, empty
     * lists dropped). Cached per component until {@link #resetCaches()}.
     */
    public static List<List<String>> componentBasePermissions(String component) {
        if (UtilValidate.isEmpty(component)) return Collections.emptyList();
        java.util.function.Function<String, List<List<String>>> source = componentBasesSource;
        if (source != null) return source.apply(component);
        return COMPONENT_BASES.computeIfAbsent(component, c -> {
            List<List<String>> out = new ArrayList<>();
            for (ComponentConfig.WebappInfo wi : ComponentConfig.getAllWebappResourceInfos(c)) {
                List<String> perms = new ArrayList<>();
                String[] base = wi.getBasePermission();
                if (base != null) {
                    for (String p : base) {
                        if (p != null && !p.trim().isEmpty() && !"NONE".equalsIgnoreCase(p.trim())) perms.add(p.trim());
                    }
                }
                if (!perms.isEmpty() && !out.contains(perms)) out.add(Collections.unmodifiableList(perms));
            }
            return Collections.unmodifiableList(out);
        });
    }

    /** For tests and registry reloads. */
    public static void resetCaches() {
        COMPONENT_BASES.clear();
    }

    /**
     * True when the token user holds {@code <base><action>} (or {@code <base>_ADMIN}) for every base in the list.
     * The generic {@code OFBTOOLS} base only needs {@code _VIEW} unless it is the only base. An empty list is false.
     */
    public static boolean hasBasePermission(McpRequest req, List<String> bases, String action) {
        if (bases == null || bases.isEmpty()) return false;
        GenericValue ul = req.getUserLogin();
        if (ul == null) return false;
        Security security = req.getSecurity();
        for (String base : bases) {
            String a = action;
            if (GENERIC_BASE.equals(base) && bases.size() > 1) a = "_VIEW";
            if (!security.hasEntityPermission(base, a, ul)) return false;
        }
        return true;
    }

    /** Endpoint webapp variant of {@link #hasBasePermission(McpRequest, List, String)}. */
    public static boolean hasBasePermission(McpRequest req, String action) {
        return hasBasePermission(req, req.getBasePermissions(), action);
    }

    public static boolean hasPermission(McpRequest req, String permission) {
        GenericValue ul = req.getUserLogin();
        if (ul == null || UtilValidate.isEmpty(permission)) return false;
        Security security = req.getSecurity();
        int idx = permission.lastIndexOf('_');
        if (idx > 0 && security.hasEntityPermission(permission.substring(0, idx), permission.substring(idx), ul)) {
            return true;
        }
        return security.hasPermission(permission, ul);
    }

    /** Service visibility in a server: component services, allow patterns, or everything in the hub. */
    public static boolean isServiceVisible(McpServerDef server, String serviceName, String serviceComponent) {
        if (server.isHub()) return true;
        if (McpConfig.anyGlobMatches(server.getServiceDeny(), serviceName)) return false;
        if (UtilValidate.isNotEmpty(server.getComponent()) && server.getComponent().equals(serviceComponent)) return true;
        return McpConfig.anyGlobMatches(server.getServiceAllow(), serviceName);
    }

    public static boolean isEntityDenied(String entityName) {
        return McpConfig.anyGlobMatches(McpConfig.getEntityDenyPatterns(), entityName);
    }
}

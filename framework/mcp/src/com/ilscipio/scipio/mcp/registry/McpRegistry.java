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
package com.ilscipio.scipio.mcp.registry;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.service.LocalDispatcher;

import com.ilscipio.scipio.mcp.catalog.CoreToolProvider;
import com.ilscipio.scipio.mcp.security.McpConfig;
import com.ilscipio.scipio.mcp.skill.SkillProvider;

/**
 * SCIPIO: 4.0.0: Central registry of MCP servers and their tools. Built lazily on the first MCP request, when
 * every component and service model is loaded.
 */
public final class McpRegistry {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static volatile McpRegistry instance;

    private final Map<String, McpServerDef> byName = new LinkedHashMap<>();
    private final Map<String, McpServerDef> byWebapp = new LinkedHashMap<>();
    private final Map<String, McpServerDef> byComponent = new LinkedHashMap<>();
    private final Map<String, McpServerDef> synthesized = new ConcurrentHashMap<>();
    private final Map<String, List<McpToolDef>> toolCache = new ConcurrentHashMap<>();
    private final Map<String, List<McpResourceDef>> resourceCache = new ConcurrentHashMap<>();
    private final Map<String, List<McpPromptDef>> promptCache = new ConcurrentHashMap<>();
    private final Map<Class<?>, McpToolProvider> providerInstances = new ConcurrentHashMap<>();
    private final List<McpToolProvider> builtInProviders;

    private McpRegistry(LocalDispatcher dispatcher) {
        for (McpServerDef s : McpAnnotationReader.readAll(dispatcher.getDispatchContext())) {
            if (byName.putIfAbsent(s.getName(), s) != null) {
                Debug.logWarning("MCP: duplicate server name " + s.getName() + " (" + s.getSourceClass() + "); first kept", module);
                continue;
            }
            for (String w : s.getWebapps()) byWebapp.putIfAbsent(w, s);
            if (s.getWebapps().isEmpty() && UtilValidate.isNotEmpty(s.getComponent())) byComponent.putIfAbsent(s.getComponent(), s);
        }
        this.builtInProviders = Collections.unmodifiableList(java.util.Arrays.asList(new CoreToolProvider(), new SkillProvider()));
        Debug.logInfo("MCP: registry built with " + byName.size() + " server profiles: " + byName.keySet(), module);
        validateSkills();
    }

    /** Lints every SKILL.md against the real server and tool names; warnings show up in the log and on the skill. */
    private void validateSkills() {
        try {
            Set<String> toolNames = new HashSet<>();
            for (McpServerDef s : byName.values()) {
                for (McpToolDef t : getTools(s)) {
                    toolNames.add(t.getName());
                    toolNames.addAll(t.getActions().keySet());
                }
            }
            int warnings = com.ilscipio.scipio.mcp.skill.SkillRegistry.get().validate(new HashSet<>(byName.keySet()), toolNames);
            Debug.logInfo("MCP: skill validation finished with " + warnings + " warning(s)", module);
        } catch (RuntimeException e) {
            Debug.logWarning(e, "MCP: skill validation failed", module);
        }
    }

    public static McpRegistry get(LocalDispatcher dispatcher) {
        McpRegistry r = instance;
        if (r == null) {
            synchronized (McpRegistry.class) {
                r = instance;
                if (r == null) {
                    r = new McpRegistry(dispatcher);
                    instance = r;
                }
            }
        }
        return r;
    }

    /** Drops the registry; the next request rebuilds it. */
    public static void reset() {
        instance = null;
    }

    /**
     * Drops every agent-facing cache: server registry, skills, service catalog and the policy's component base
     * permission cache. The next MCP request rebuilds them. Used by the Webtools "Reload" action.
     */
    public static void reloadAll() {
        reset();
        com.ilscipio.scipio.mcp.skill.SkillRegistry.reset();
        com.ilscipio.scipio.mcp.catalog.ServiceCatalog.reset();
        com.ilscipio.scipio.mcp.security.McpPolicy.resetCaches();
        Debug.logInfo("MCP: registry, skills, service catalog and policy caches reset", module);
    }

    public List<McpServerDef> getServers() {
        return new ArrayList<>(byName.values());
    }

    public McpServerDef getServer(String name) {
        return byName.get(name);
    }

    /** Finds a declared server by server name, webapp name or component name; null when none matches. */
    public McpServerDef findServer(String key) {
        if (key == null || key.isEmpty()) return null;
        McpServerDef s = byName.get(key);
        if (s == null) s = byWebapp.get(key);
        if (s == null) s = byComponent.get(key);
        return s;
    }

    /** Webapp (context root) that serves a server, and its base permissions; used when the hub calls app tools. */
    public static final class WebappBinding {
        public final String webappName;
        public final List<String> basePermissions;
        WebappBinding(String webappName, List<String> basePermissions) {
            this.webappName = webappName;
            this.basePermissions = basePermissions;
        }
    }

    public WebappBinding webappBindingFor(McpServerDef server) {
        for (org.ofbiz.base.component.ComponentConfig.WebappInfo wi : org.ofbiz.base.component.ComponentConfig.getAllWebappResourceInfos()) {
            String root = wi.getContextRoot();
            String name = root != null && root.startsWith("/") ? root.substring(1) : root;
            if (name == null || name.isEmpty()) continue;
            boolean match = server.getWebapps().contains(name)
                    || (server.getWebapps().isEmpty() && UtilValidate.isNotEmpty(server.getComponent())
                        && server.getComponent().equals(wi.getComponentConfig().getComponentName()));
            if (!match) continue;
            List<String> perms = new ArrayList<>();
            String[] base = wi.getBasePermission();
            if (base != null) {
                for (String p : base) {
                    if (p != null && !p.trim().isEmpty() && !"NONE".equalsIgnoreCase(p.trim())) perms.add(p.trim());
                }
            }
            return new WebappBinding(name, Collections.unmodifiableList(perms));
        }
        return new WebappBinding(server.getName(), Collections.emptyList());
    }

    /** Server for a webapp: explicit webapp binding, then component binding, else a synthesized core-only server. */
    public McpServerDef resolve(String webappName, String componentName) {
        McpServerDef s = byWebapp.get(webappName);
        if (s != null) return s;
        if (UtilValidate.isNotEmpty(componentName)) {
            s = byComponent.get(componentName);
            if (s != null) return s;
        }
        return synthesized.computeIfAbsent(webappName, w -> McpServerDef.builder(w)
                .title("Scipio " + w).component(componentName != null ? componentName : "")
                .description("Core Scipio tools scoped to the " + (UtilValidate.isNotEmpty(componentName) ? componentName : w) + " application.")
                .synthetic(true).build());
    }

    /**
     * Tools of a server in a stable order: the profile's own tools first (by {@code order}, featured first, then
     * name), then the provider tools (core tools, skills) in the same order. Tools listed in
     * {@code mcp.tool.disable} are removed.
     */
    public List<McpToolDef> getTools(McpServerDef server) {
        return toolCache.computeIfAbsent(server.getName(), n -> {
            List<String> disabled = McpConfig.getDisabledTools();
            List<McpToolDef> own = new ArrayList<>();
            List<McpToolDef> provided = new ArrayList<>();
            Set<String> names = new HashSet<>();
            for (McpToolDef t : server.getTools()) add(own, names, t, server, disabled);
            for (McpToolProvider p : providers(server)) {
                for (McpToolDef t : p.getTools(server)) add(provided, names, t, server, disabled);
            }
            own.sort(TOOL_ORDER);
            provided.sort(TOOL_ORDER);
            List<McpToolDef> out = new ArrayList<>(own.size() + provided.size());
            out.addAll(own);
            out.addAll(provided);
            return Collections.unmodifiableList(out);
        });
    }

    private static final java.util.Comparator<McpToolDef> TOOL_ORDER = java.util.Comparator
            .comparingInt(McpToolDef::getOrder)
            .thenComparing(t -> !t.isFeatured())
            .thenComparing(McpToolDef::getName);

    public McpToolDef findTool(McpServerDef server, String name) {
        for (McpToolDef t : getTools(server)) {
            if (t.getName().equals(name)) return t;
        }
        return null;
    }

    public List<McpResourceDef> getResources(McpServerDef server) {
        return resourceCache.computeIfAbsent(server.getName(), n -> {
            List<McpResourceDef> out = new ArrayList<>(server.getResources());
            for (McpToolProvider p : providers(server)) out.addAll(p.getResources(server));
            return Collections.unmodifiableList(out);
        });
    }

    public List<McpPromptDef> getPrompts(McpServerDef server) {
        return promptCache.computeIfAbsent(server.getName(), n -> {
            List<McpPromptDef> out = new ArrayList<>(server.getPrompts());
            for (McpToolProvider p : providers(server)) out.addAll(p.getPrompts(server));
            return Collections.unmodifiableList(out);
        });
    }

    private void add(List<McpToolDef> out, Set<String> names, McpToolDef t, McpServerDef server, List<String> disabled) {
        if (disabled.contains(t.getName()) || disabled.contains(server.getName() + "." + t.getName())) {
            Debug.logInfo("MCP: tool " + t.getName() + " in server " + server.getName() + " disabled by mcp.tool.disable", module);
            return;
        }
        if (names.add(t.getName())) out.add(t);
        else Debug.logWarning("MCP: duplicate tool " + t.getName() + " in server " + server.getName() + " from " + t.getSource() + "; ignored", module);
    }

    private List<McpToolProvider> providers(McpServerDef server) {
        List<McpToolProvider> out = new ArrayList<>(builtInProviders);
        for (Class<? extends McpToolProvider> cls : server.getProviderClasses()) {
            McpToolProvider p = providerInstances.computeIfAbsent(cls, c -> {
                try {
                    return (McpToolProvider) c.getDeclaredConstructor().newInstance();
                } catch (ReflectiveOperationException e) {
                    Debug.logError(e, "MCP: could not instantiate provider " + c.getName(), module);
                    return null;
                }
            });
            if (p != null) out.add(p);
        }
        return out;
    }
}

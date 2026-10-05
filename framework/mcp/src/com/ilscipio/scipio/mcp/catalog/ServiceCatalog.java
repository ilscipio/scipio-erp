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

import java.net.URL;
import java.security.CodeSource;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelParam;
import org.ofbiz.service.ModelPermGroup;
import org.ofbiz.service.ModelPermission;
import org.ofbiz.service.ModelService;
import org.ofbiz.webapp.control.ConfigXMLReader;

import com.ilscipio.scipio.mcp.security.McpPolicy;
import com.ilscipio.scipio.mcp.security.McpUsageTracker;

/**
 * SCIPIO: 4.0.0: Index of every service with its owning component, read-only classification, UI usage and
 * search ranking. Built once per JVM on first use.
 */
public final class ServiceCatalog {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final Pattern COMPONENT_PATH = Pattern.compile("[/\\\\](applications|framework|addons|hot-deploy|specialpurpose|themes)[/\\\\]([A-Za-z0-9_-]+)[/\\\\]");
    private static final Pattern COMPONENT_URL = Pattern.compile("component://([A-Za-z0-9_-]+)/");
    private static final Pattern CAMEL = Pattern.compile("(?<=[a-z0-9])(?=[A-Z])|(?<=[A-Z])(?=[A-Z][a-z])|[_\\-\\s.]+");

    private static volatile ServiceCatalog instance;

    public static final class Entry {
        public final String name;
        public final String description;
        public final String component;
        public final String engine;
        public final boolean auth;
        public final boolean export;
        public final boolean deprecated;
        public final boolean readOnly;
        public final boolean guarded;
        public final String defaultEntity;
        final Set<String> nameTokens;
        final Set<String> descTokens;

        Entry(ModelService svc, String component) {
            this.name = svc.name;
            this.description = svc.description != null ? svc.description : "";
            this.component = component;
            this.engine = svc.engineName != null ? svc.engineName : "";
            this.auth = svc.auth;
            this.export = svc.export;
            this.deprecated = svc.deprecatedUseInstead != null;
            this.readOnly = McpPolicy.isReadOnlyService(svc);
            this.guarded = McpPolicy.hasOwnPermissions(svc);
            this.defaultEntity = svc.defaultEntityName;
            this.nameTokens = tokens(svc.name);
            this.descTokens = tokens(this.description);
        }
    }

    private final Map<String, Entry> entries;
    private final Set<String> usedByUi;
    private final Map<String, String> componentByLocation = new ConcurrentHashMap<>();

    private ServiceCatalog(DispatchContext dctx) {
        Map<String, Entry> map = new TreeMap<>();
        for (String name : dctx.getAllServiceNames()) {
            ModelService svc = dctx.getModelServiceOrNull(name);
            if (svc == null) continue;
            map.put(name, new Entry(svc, componentOf(svc)));
        }
        this.entries = Collections.unmodifiableMap(map);
        this.usedByUi = Collections.unmodifiableSet(collectUiServices());
        Debug.logInfo("MCP: service catalog built with " + entries.size() + " services, " + usedByUi.size() + " referenced by UI requests", module);
    }

    public static ServiceCatalog get(LocalDispatcher dispatcher) {
        ServiceCatalog c = instance;
        if (c == null) {
            synchronized (ServiceCatalog.class) {
                c = instance;
                if (c == null) {
                    c = new ServiceCatalog(dispatcher.getDispatchContext());
                    instance = c;
                }
            }
        }
        return c;
    }

    public static void reset() {
        instance = null;
    }

    public Entry get(String name) {
        return entries.get(name);
    }

    public Collection<Entry> all() {
        return entries.values();
    }

    public boolean isUsedByUi(String name) {
        return usedByUi.contains(name);
    }

    /** Component that defines the service, derived from its definition class or location. */
    public String componentOf(ModelService svc) {
        String loc = svc.definitionLocation != null ? svc.definitionLocation : svc.location;
        if (loc == null) return "";
        return componentByLocation.computeIfAbsent(loc, l -> {
            Matcher um = COMPONENT_URL.matcher(l);
            if (um.find()) return um.group(1);
            Matcher pm = COMPONENT_PATH.matcher(l);
            if (pm.find()) return pm.group(2);
            if (!l.contains("/") && !l.contains("\\") && l.contains(".")) {
                try {
                    Class<?> cls = Class.forName(l, false, Thread.currentThread().getContextClassLoader());
                    CodeSource cs = cls.getProtectionDomain().getCodeSource();
                    URL url = cs != null ? cs.getLocation() : null;
                    if (url != null) {
                        Matcher m = COMPONENT_PATH.matcher(url.getPath());
                        if (m.find()) return m.group(2);
                    }
                } catch (Throwable t) {
                    // fall through
                }
                String[] parts = l.split("\\.");
                if (parts.length > 3 && "com".equals(parts[0]) && "ilscipio".equals(parts[1]) && "scipio".equals(parts[2])) return parts[3];
                if (parts.length > 2 && "org".equals(parts[0]) && "ofbiz".equals(parts[1])) return parts[2];
            }
            return "";
        });
    }

    private static Set<String> collectUiServices() {
        Set<String> out = new HashSet<>();
        try {
            for (ComponentConfig.WebappInfo wi : ComponentConfig.getAllWebappResourceInfos()) {
                try {
                    ConfigXMLReader.ControllerConfig cc = ConfigXMLReader.getControllerConfig(wi, true);
                    if (cc == null) continue;
                    for (ConfigXMLReader.RequestMap rm : cc.getRequestMapMap().values()) {
                        if (rm.event != null && rm.event.invoke != null
                                && ("service".equals(rm.event.type) || "service-multi".equals(rm.event.type))) {
                            out.add(rm.event.invoke);
                        }
                    }
                } catch (Throwable t) {
                    // a broken controller must not break the catalog
                }
            }
        } catch (Throwable t) {
            Debug.logWarning(t, "MCP: could not scan controllers for UI service usage", module);
        }
        return out;
    }

    static Set<String> tokens(String s) {
        Set<String> out = new HashSet<>();
        if (UtilValidate.isEmpty(s)) return out;
        for (String t : CAMEL.split(s)) {
            t = t.toLowerCase(Locale.ROOT).replaceAll("[^a-z0-9]", "");
            if (t.length() > 1) out.add(t);
        }
        return out;
    }

    /** Scored search result row. */
    public static final class Hit {
        public final Entry entry;
        public final double score;
        public final boolean featured;

        Hit(Entry entry, double score, boolean featured) {
            this.entry = entry;
            this.score = score;
            this.featured = featured;
        }
    }

    public List<Hit> search(String query, String component, Set<String> featured, java.util.function.Predicate<Entry> visible,
                            Delegator delegator, int limit) {
        String q = query != null ? query.trim() : "";
        Set<String> qTokens = tokens(q);
        String qLower = q.toLowerCase(Locale.ROOT);
        List<Hit> hits = new ArrayList<>();
        McpUsageTracker usage = McpUsageTracker.get();
        for (Entry e : entries.values()) {
            if (UtilValidate.isNotEmpty(component) && !component.equals(e.component)) continue;
            if (visible != null && !visible.test(e)) continue;
            double score = 0;
            if (!q.isEmpty()) {
                String nameLower = e.name.toLowerCase(Locale.ROOT);
                if (nameLower.equals(qLower)) score += 100;
                else if (nameLower.startsWith(qLower)) score += 40;
                else if (nameLower.contains(qLower)) score += 25;
                for (String t : qTokens) {
                    if (e.nameTokens.contains(t)) score += 20;
                    else if (nameLower.contains(t)) score += 10;
                    if (e.descTokens.contains(t)) score += 5;
                }
                if (score <= 0) continue;
            }
            boolean isFeatured = featured != null && featured.contains(e.name);
            if (isFeatured) score += 30;
            if (usedByUi.contains(e.name)) score += 10;
            long calls = usage.getCallCount(delegator, "scipio_service:" + e.name);
            if (calls > 0) score += Math.log10(calls + 1) * 10;
            if (e.deprecated) score -= 20;
            hits.add(new Hit(e, score, isFeatured));
        }
        hits.sort((a, b) -> {
            int c = Double.compare(b.score, a.score);
            return c != 0 ? c : a.entry.name.compareTo(b.entry.name);
        });
        return hits.size() > limit ? new ArrayList<>(hits.subList(0, limit)) : hits;
    }

    /** Full description of a service for agents. */
    public Map<String, Object> describe(ModelService svc) {
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("name", svc.name);
        out.put("description", svc.description);
        out.put("component", componentOf(svc));
        out.put("engine", svc.engineName);
        out.put("auth", svc.auth);
        out.put("readOnly", McpPolicy.isReadOnlyService(svc));
        out.put("usedByUi", usedByUi.contains(svc.name));
        if (svc.defaultEntityName != null) out.put("defaultEntity", svc.defaultEntityName);
        if (svc.deprecatedUseInstead != null) out.put("deprecatedUseInstead", svc.deprecatedUseInstead);
        List<String> perms = new ArrayList<>();
        if (svc.permissionServiceName != null) perms.add("service:" + svc.permissionServiceName);
        if (svc.permissionGroups != null) {
            for (ModelPermGroup g : svc.permissionGroups) {
                for (ModelPermission p : g.permissions) {
                    if (p.permissionServiceName != null) perms.add("service:" + p.permissionServiceName);
                    else if (p.nameOrRole != null) perms.add(p.nameOrRole + (p.action != null ? p.action : ""));
                }
            }
        }
        out.put("permissions", perms);
        out.put("inputSchema", ServiceSchemaBuilder.inputSchema(svc, null, null));
        out.put("outputSchema", ServiceSchemaBuilder.outputSchema(svc));
        List<Map<String, Object>> params = new ArrayList<>();
        for (ModelParam p : svc.getModelParamList()) {
            if (p.internal || ServiceSchemaBuilder.HIDDEN_PARAMS.contains(p.name)) continue;
            Map<String, Object> pm = new LinkedHashMap<>();
            pm.put("name", p.name);
            pm.put("type", p.type);
            pm.put("mode", p.mode);
            pm.put("optional", p.optional);
            if (UtilValidate.isNotEmpty(p.description)) pm.put("description", p.description);
            params.add(pm);
        }
        out.put("parameters", params);
        return out;
    }
}

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
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * SCIPIO: 4.0.0: Immutable MCP server profile (from {@code @McpServer} or synthesized for a webapp without one).
 */
public final class McpServerDef {

    private final String name;
    private final String title;
    private final String description;
    private final String component;
    private final Set<String> webapps;
    private final List<String> featuredServices;
    private final List<String> serviceAllow;
    private final List<String> serviceDeny;
    private final Set<String> entities;
    private final boolean allowAnonymous;
    private final String requiredPermission;
    private final boolean hub;
    private final boolean synthetic;
    private final List<McpToolDef> tools;
    private final List<McpResourceDef> resources;
    private final List<McpPromptDef> prompts;
    private final List<Class<? extends McpToolProvider>> providerClasses;
    private final String sourceClass;

    private McpServerDef(Builder b) {
        this.name = b.name;
        this.title = b.title.isEmpty() ? b.name : b.title;
        this.description = b.description;
        this.component = b.component;
        this.webapps = Collections.unmodifiableSet(new LinkedHashSet<>(b.webapps));
        this.featuredServices = Collections.unmodifiableList(new ArrayList<>(b.featuredServices));
        this.serviceAllow = Collections.unmodifiableList(new ArrayList<>(b.serviceAllow));
        this.serviceDeny = Collections.unmodifiableList(new ArrayList<>(b.serviceDeny));
        this.entities = Collections.unmodifiableSet(new LinkedHashSet<>(b.entities));
        this.allowAnonymous = b.allowAnonymous;
        this.requiredPermission = b.requiredPermission;
        this.hub = b.hub;
        this.synthetic = b.synthetic;
        this.tools = Collections.unmodifiableList(new ArrayList<>(b.tools));
        this.resources = Collections.unmodifiableList(new ArrayList<>(b.resources));
        this.prompts = Collections.unmodifiableList(new ArrayList<>(b.prompts));
        this.providerClasses = Collections.unmodifiableList(new ArrayList<>(b.providerClasses));
        this.sourceClass = b.sourceClass;
    }

    public static Builder builder(String name) {
        return new Builder(name);
    }

    public String getName() { return name; }
    public String getTitle() { return title; }
    public String getDescription() { return description; }
    /** Component name that scopes the core tools; empty for the hub. */
    public String getComponent() { return component; }
    public Set<String> getWebapps() { return webapps; }
    public List<String> getFeaturedServices() { return featuredServices; }
    public List<String> getServiceAllow() { return serviceAllow; }
    public List<String> getServiceDeny() { return serviceDeny; }
    public Set<String> getEntities() { return entities; }
    public boolean isAllowAnonymous() { return allowAnonymous; }
    public String getRequiredPermission() { return requiredPermission; }
    public boolean isHub() { return hub; }
    /** True for servers synthesized for webapps without a profile. */
    public boolean isSynthetic() { return synthetic; }
    /** Tools declared directly on the profile class (not provider tools). */
    public List<McpToolDef> getTools() { return tools; }
    public List<McpResourceDef> getResources() { return resources; }
    public List<McpPromptDef> getPrompts() { return prompts; }
    public List<Class<? extends McpToolProvider>> getProviderClasses() { return providerClasses; }
    public String getSourceClass() { return sourceClass; }

    public static final class Builder {
        private final String name;
        private String title = "";
        private String description = "";
        private String component = "";
        private final List<String> webapps = new ArrayList<>();
        private final List<String> featuredServices = new ArrayList<>();
        private final List<String> serviceAllow = new ArrayList<>();
        private final List<String> serviceDeny = new ArrayList<>();
        private final List<String> entities = new ArrayList<>();
        private boolean allowAnonymous;
        private String requiredPermission = "";
        private boolean hub;
        private boolean synthetic;
        private final List<McpToolDef> tools = new ArrayList<>();
        private final List<McpResourceDef> resources = new ArrayList<>();
        private final List<McpPromptDef> prompts = new ArrayList<>();
        private final List<Class<? extends McpToolProvider>> providerClasses = new ArrayList<>();
        private String sourceClass = "";

        private Builder(String name) {
            this.name = name;
        }

        public Builder title(String v) { this.title = v != null ? v : ""; return this; }
        public Builder description(String v) { this.description = v != null ? v : ""; return this; }
        public Builder component(String v) { this.component = v != null ? v : ""; return this; }
        public Builder webapps(String... v) { addAll(webapps, v); return this; }
        public Builder featuredServices(String... v) { addAll(featuredServices, v); return this; }
        public Builder serviceAllow(String... v) { addAll(serviceAllow, v); return this; }
        public Builder serviceDeny(String... v) { addAll(serviceDeny, v); return this; }
        public Builder entities(String... v) { addAll(entities, v); return this; }
        public Builder allowAnonymous(boolean v) { this.allowAnonymous = v; return this; }
        public Builder requiredPermission(String v) { this.requiredPermission = v != null ? v : ""; return this; }
        public Builder hub(boolean v) { this.hub = v; return this; }
        public Builder synthetic(boolean v) { this.synthetic = v; return this; }
        public Builder tool(McpToolDef v) { tools.add(v); return this; }
        /** Tools added so far (read-only view for the reader). */
        public List<McpToolDef> buildToolsSnapshot() { return Collections.unmodifiableList(new ArrayList<>(tools)); }
        public Builder resource(McpResourceDef v) { resources.add(v); return this; }
        public Builder prompt(McpPromptDef v) { prompts.add(v); return this; }
        public Builder provider(Class<? extends McpToolProvider> v) { providerClasses.add(v); return this; }
        public Builder sourceClass(String v) { this.sourceClass = v != null ? v : ""; return this; }

        private static void addAll(List<String> target, String[] values) {
            if (values == null) return;
            for (String v : values) {
                if (v != null && !v.trim().isEmpty()) target.add(v.trim());
            }
        }

        public McpServerDef build() {
            if (name == null || name.isEmpty()) throw new IllegalArgumentException("server name required");
            return new McpServerDef(this);
        }
    }
}

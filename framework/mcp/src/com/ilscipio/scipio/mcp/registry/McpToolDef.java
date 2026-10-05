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

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * SCIPIO: 4.0.0: Immutable MCP tool definition plus its executor.
 */
public final class McpToolDef {

    @FunctionalInterface
    public interface Executor {
        McpResult execute(McpCallContext ctx, Map<String, Object> args) throws Exception;
    }

    private final String name;
    private final String title;
    private final String description;
    private final Map<String, Object> inputSchema;
    private final Map<String, Object> outputSchema;
    private final boolean readOnly;
    private final boolean destructive;
    private final boolean idempotent;
    private final boolean featured;
    private final int order;
    private final boolean publicAccess;
    private final boolean requiresConfirmation;
    private final String permission;
    private final Set<String> tags;
    private final String serviceName;
    private final String source;
    private final Executor executor;
    private final Map<String, McpToolDef> actions;

    /** Default sort key for tools that set none. */
    public static final int DEFAULT_ORDER = 100;

    private McpToolDef(Builder b) {
        this.name = b.name;
        this.title = b.title;
        this.description = b.description;
        this.inputSchema = b.inputSchema != null ? Collections.unmodifiableMap(b.inputSchema) : emptyObjectSchema();
        this.outputSchema = b.outputSchema != null ? Collections.unmodifiableMap(b.outputSchema) : null;
        this.readOnly = b.readOnly;
        this.destructive = b.destructive != null ? b.destructive : !b.readOnly;
        this.idempotent = b.idempotent != null ? b.idempotent : b.readOnly;
        this.featured = b.featured;
        this.order = b.order;
        this.publicAccess = b.publicAccess;
        this.requiresConfirmation = b.requiresConfirmation;
        this.permission = b.permission;
        this.tags = Collections.unmodifiableSet(new LinkedHashSet<>(b.tags));
        this.serviceName = b.serviceName;
        this.source = b.source;
        this.executor = b.executor;
        this.actions = b.actions != null ? Collections.unmodifiableMap(new LinkedHashMap<>(b.actions)) : Collections.emptyMap();
    }

    public static Map<String, Object> emptyObjectSchema() {
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", new LinkedHashMap<String, Object>());
        schema.put("additionalProperties", false);
        return schema;
    }

    public static Builder builder(String name) {
        return new Builder(name);
    }

    public String getName() { return name; }
    public String getTitle() { return title; }
    public String getDescription() { return description; }
    public Map<String, Object> getInputSchema() { return inputSchema; }
    public Map<String, Object> getOutputSchema() { return outputSchema; }
    public boolean isReadOnly() { return readOnly; }
    public boolean isDestructive() { return destructive; }
    public boolean isIdempotent() { return idempotent; }
    public boolean isFeatured() { return featured; }
    /** Sort key inside a server's tool list; lower first. */
    public int getOrder() { return order; }
    public boolean isPublicAccess() { return publicAccess; }
    public boolean isRequiresConfirmation() { return requiresConfirmation; }
    public String getPermission() { return permission; }
    public Set<String> getTags() { return tags; }
    /** Backing service name for service tools, else null. */
    public String getServiceName() { return serviceName; }
    /** Where the tool comes from (class name or provider), for diagnostics. */
    public String getSource() { return source; }
    public Executor getExecutor() { return executor; }
    /** Action name to action tool for a composite tool (see {@link McpTopicTool}); empty otherwise. */
    public Map<String, McpToolDef> getActions() { return actions; }
    public boolean isComposite() { return !actions.isEmpty(); }

    public static final class Builder {
        private final String name;
        private String title = "";
        private String description = "";
        private Map<String, Object> inputSchema;
        private Map<String, Object> outputSchema;
        private boolean readOnly;
        private Boolean destructive;
        private Boolean idempotent;
        private boolean featured;
        private int order = DEFAULT_ORDER;
        private boolean publicAccess;
        private boolean requiresConfirmation;
        private String permission = "";
        private Set<String> tags = new LinkedHashSet<>();
        private String serviceName;
        private String source = "";
        private Executor executor;
        private Map<String, McpToolDef> actions;

        private Builder(String name) {
            this.name = name;
        }

        public Builder title(String v) { this.title = v != null ? v : ""; return this; }
        public Builder description(String v) { this.description = v != null ? v : ""; return this; }
        public Builder inputSchema(Map<String, Object> v) { this.inputSchema = v; return this; }
        public Builder outputSchema(Map<String, Object> v) { this.outputSchema = v; return this; }
        public Builder readOnly(boolean v) { this.readOnly = v; return this; }
        public Builder destructive(Boolean v) { this.destructive = v; return this; }
        public Builder idempotent(Boolean v) { this.idempotent = v; return this; }
        public Builder featured(boolean v) { this.featured = v; return this; }
        public Builder order(int v) { this.order = v; return this; }
        public Builder publicAccess(boolean v) { this.publicAccess = v; return this; }
        public Builder requiresConfirmation(boolean v) { this.requiresConfirmation = v; return this; }
        public Builder permission(String v) { this.permission = v != null ? v : ""; return this; }
        public Builder tags(String... v) { if (v != null) { for (String t : v) { if (t != null && !t.isEmpty()) tags.add(t); } } return this; }
        public Builder serviceName(String v) { this.serviceName = v; return this; }
        public Builder source(String v) { this.source = v != null ? v : ""; return this; }
        public Builder executor(Executor v) { this.executor = v; return this; }
        public Builder actions(Map<String, McpToolDef> v) { this.actions = v; return this; }

        /** Parses "true"/"false"/"" tri-state annotation values. */
        public static Boolean triState(String v) {
            if (v == null || v.isEmpty()) return null;
            return Boolean.parseBoolean(v);
        }

        public McpToolDef build() {
            if (name == null || name.isEmpty()) throw new IllegalArgumentException("tool name required");
            if (executor == null) throw new IllegalArgumentException("tool executor required for " + name);
            return new McpToolDef(this);
        }
    }
}

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
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.service.ModelService;

import com.ilscipio.scipio.mcp.catalog.JsonSchemaValidator;
import com.ilscipio.scipio.mcp.security.McpConfig;
import com.ilscipio.scipio.mcp.security.McpPolicy;

/**
 * SCIPIO: 4.0.0: Builds one composite tool from several action tools. The composite takes an {@code action}
 * argument (enum of the action names) plus the union of the action parameters. At call time it resolves the
 * action, applies the policy of that action, validates the arguments against the action schema and runs it.
 */
public final class McpTopicTool {

    public static final String ACTION = "action";
    /** Tag on every composite tool; the endpoint-level policy gate defers to the per-action check. */
    public static final String COMPOSITE_TAG = "composite";

    private McpTopicTool() {}

    /**
     * @param members action tools in display order; each member's {@code name} is the action name
     */
    @SuppressWarnings("unchecked")
    public static McpToolDef build(String name, String title, String description, int order, boolean featured, List<McpToolDef> members) {
        List<String> disabled = McpConfig.getDisabledTools();
        Map<String, McpToolDef> actions = new LinkedHashMap<>();
        for (McpToolDef m : members) {
            if (disabled.contains(name + ":" + m.getName())) continue;
            if (actions.put(m.getName(), m) != null) {
                throw new IllegalArgumentException("duplicate action " + m.getName() + " in tool " + name);
            }
        }
        if (actions.isEmpty()) throw new IllegalArgumentException("tool " + name + " has no actions");
        int n = actions.size();

        // union schema
        Map<String, Map<String, Object>> merged = new LinkedHashMap<>();
        Map<String, Set<String>> usedBy = new LinkedHashMap<>();
        Map<String, Set<String>> requiredBy = new LinkedHashMap<>();
        boolean additional = false;
        boolean readOnly = true, destructive = false, idempotent = true, publicAccess = false, confirm = false;
        for (Map.Entry<String, McpToolDef> e : actions.entrySet()) {
            String action = e.getKey();
            McpToolDef d = e.getValue();
            Map<String, Object> s = d.getInputSchema();
            if (s.get("properties") instanceof Map) {
                for (Map.Entry<String, Object> p : ((Map<String, Object>) s.get("properties")).entrySet()) {
                    if (p.getValue() instanceof Map) {
                        Map<String, Object> ps = (Map<String, Object>) p.getValue();
                        Map<String, Object> have = merged.get(p.getKey());
                        if (have == null) {
                            merged.put(p.getKey(), new LinkedHashMap<>(ps));
                        } else if (ps.get("type") != null && have.get("type") != null && !have.get("type").equals(ps.get("type"))) {
                            // the same name with another type in another action: accept both; the action schema decides
                            Set<Object> types = new LinkedHashSet<>();
                            if (have.get("type") instanceof List) types.addAll((List<Object>) have.get("type")); else types.add(have.get("type"));
                            if (ps.get("type") instanceof List) types.addAll((List<Object>) ps.get("type")); else types.add(ps.get("type"));
                            have.put("type", new ArrayList<>(types));
                            have.remove("enum");
                        }
                        usedBy.computeIfAbsent(p.getKey(), k -> new LinkedHashSet<>()).add(action);
                    }
                }
            }
            if (s.get("required") instanceof List) {
                for (Object r : (List<Object>) s.get("required")) requiredBy.computeIfAbsent(String.valueOf(r), k -> new LinkedHashSet<>()).add(action);
            }
            if (Boolean.TRUE.equals(s.get("additionalProperties"))) additional = true;
            readOnly &= d.isReadOnly();
            destructive |= d.isDestructive();
            idempotent &= d.isIdempotent();
            publicAccess |= d.isPublicAccess();
            confirm |= d.isRequiresConfirmation();
        }
        Map<String, Object> props = new LinkedHashMap<>();
        Map<String, Object> actionProp = new LinkedHashMap<>();
        actionProp.put("type", "string");
        actionProp.put("enum", new ArrayList<>(actions.keySet()));
        actionProp.put("description", "Action to run");
        props.put(ACTION, actionProp);
        List<String> required = new ArrayList<>();
        required.add(ACTION);
        for (Map.Entry<String, Map<String, Object>> e : merged.entrySet()) {
            Map<String, Object> ps = e.getValue();
            Set<String> users = usedBy.get(e.getKey());
            Set<String> req = requiredBy.getOrDefault(e.getKey(), Collections.emptySet());
            String desc = ps.get("description") instanceof String ? ((String) ps.get("description")).trim() : "";
            if (req.size() == n) {
                required.add(e.getKey());
            } else if (users.size() == 1 && n > 1) {
                desc = "[" + users.iterator().next() + (req.isEmpty() ? "" : ", required") + "] " + desc;
            } else if (!req.isEmpty()) {
                desc = (desc + (desc.isEmpty() || desc.endsWith(".") ? " " : ". ") + "Required for: " + String.join(", ", req) + ".").trim();
            }
            if (!desc.isEmpty()) ps.put("description", desc.trim());
            props.put(e.getKey(), ps);
        }
        Map<String, Object> schema = new LinkedHashMap<>();
        schema.put("type", "object");
        schema.put("properties", props);
        schema.put("required", required);
        schema.put("additionalProperties", additional);

        // description: topic sentence + one line per action
        StringBuilder sb = new StringBuilder(description != null ? description.trim() : "");
        sb.append(sb.length() > 0 ? "\n" : "").append("Actions:");
        for (Map.Entry<String, McpToolDef> e : actions.entrySet()) {
            sb.append("\n- ").append(e.getKey()).append(": ").append(e.getValue().getDescription().trim());
        }

        final Map<String, McpToolDef> finalActions = Collections.unmodifiableMap(actions);
        return McpToolDef.builder(name)
                .title(title != null && !title.isEmpty() ? title : name).description(sb.toString()).inputSchema(schema)
                .readOnly(readOnly).destructive(destructive).idempotent(idempotent).publicAccess(publicAccess)
                .requiresConfirmation(confirm).featured(featured).order(order).tags(COMPOSITE_TAG)
                .source(members.get(0).getSource()).actions(finalActions)
                .executor((ctx, args) -> {
                    Object a = args.get(ACTION);
                    McpToolDef member = a instanceof String ? finalActions.get(a) : null;
                    if (member == null) {
                        throw new McpToolException("Unknown action " + a + " for " + name + "; use one of: " + String.join(", ", finalActions.keySet()));
                    }
                    ModelService backing = member.getServiceName() != null
                            ? ctx.getDispatcher().getDispatchContext().getModelServiceOrNull(member.getServiceName()) : null;
                    McpPolicy.Decision d = McpPolicy.checkTool(ctx.getRequest(), member, backing);
                    if (!d.allowed) throw McpToolException.denied(d.reason);
                    Map<String, Object> sub = new LinkedHashMap<>(args);
                    sub.remove(ACTION);
                    List<String> problems = JsonSchemaValidator.validate(member.getInputSchema(), sub);
                    if (!problems.isEmpty()) throw new McpToolException("action " + a + ": " + String.join("; ", problems));
                    return member.getExecutor().execute(ctx, sub);
                }).build();
    }
}

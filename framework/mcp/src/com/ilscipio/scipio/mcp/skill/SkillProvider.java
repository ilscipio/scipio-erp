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
package com.ilscipio.scipio.mcp.skill;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import com.ilscipio.scipio.mcp.registry.McpPromptDef;
import com.ilscipio.scipio.mcp.registry.McpResourceDef;
import com.ilscipio.scipio.mcp.registry.McpResult;
import com.ilscipio.scipio.mcp.registry.McpServerDef;
import com.ilscipio.scipio.mcp.registry.McpToolDef;
import com.ilscipio.scipio.mcp.registry.McpToolException;
import com.ilscipio.scipio.mcp.registry.McpToolProvider;

/**
 * SCIPIO: 4.0.0: Exposes skills as tools, resources ({@code scipio://skills/{name}}) and prompts in every server.
 */
public final class SkillProvider implements McpToolProvider {

    public static final String URI_PREFIX = "scipio://skills/";

    /** The skill actions join the {@code scipio_apps} tool built by {@link com.ilscipio.scipio.mcp.catalog.CoreToolProvider}. */
    @Override
    public List<McpToolDef> getTools(McpServerDef server) {
        return java.util.Collections.emptyList();
    }

    /** Action {@code skill_list} of {@code scipio_apps}. */
    public static McpToolDef skillList() {
        Map<String, Object> listSchema = McpToolDef.emptyObjectSchema();
        Map<String, Object> props = new LinkedHashMap<>();
        props.put("query", prop("string", "Optional text filter on name and description"));
        listSchema.put("properties", props);
        return McpToolDef.builder("skill_list")
                .title("List skills")
                .description("List the agent skills (SKILL.md) of this installation.")
                .inputSchema(listSchema).readOnly(true).publicAccess(true).source("SkillProvider")
                .executor((ctx, args) -> {
                    String q = args.get("query") instanceof String ? ((String) args.get("query")).toLowerCase(Locale.ROOT) : "";
                    List<Map<String, Object>> out = new ArrayList<>();
                    for (SkillRegistry.Skill s : SkillRegistry.get().all()) {
                        if (!q.isEmpty() && !s.name.toLowerCase(Locale.ROOT).contains(q) && !s.description.toLowerCase(Locale.ROOT).contains(q)) continue;
                        out.add(s.toMap());
                    }
                    Map<String, Object> result = new LinkedHashMap<>();
                    result.put("skills", out);
                    return McpResult.ok(result);
                }).build();
    }

    /** Action {@code skill_get} of {@code scipio_apps}. */
    public static McpToolDef skillGet() {
        Map<String, Object> getSchema = McpToolDef.emptyObjectSchema();
        Map<String, Object> gp = new LinkedHashMap<>();
        gp.put("name", prop("string", "Skill name"));
        getSchema.put("properties", gp);
        getSchema.put("required", java.util.Collections.singletonList("name"));
        return McpToolDef.builder("skill_get")
                .title("Get skill")
                .description("Full Markdown text of one skill.")
                .inputSchema(getSchema).readOnly(true).publicAccess(true).source("SkillProvider")
                .executor((ctx, args) -> {
                    SkillRegistry.Skill s = SkillRegistry.get().get(String.valueOf(args.get("name")));
                    if (s == null) throw new McpToolException("Unknown skill " + args.get("name"));
                    return McpResult.text(s.getContent());
                }).build();
    }

    @Override
    public List<McpResourceDef> getResources(McpServerDef server) {
        List<McpResourceDef> out = new ArrayList<>();
        out.add(new McpResourceDef(URI_PREFIX + "{name}", "skill", "Agent skill document by name", "text/markdown",
                (ctx, uri, params) -> {
                    SkillRegistry.Skill s = SkillRegistry.get().get(params.get("name"));
                    if (s == null) throw new McpToolException("Unknown skill " + params.get("name"));
                    return s.getContent();
                }));
        for (SkillRegistry.Skill s : SkillRegistry.get().all()) {
            out.add(new McpResourceDef(URI_PREFIX + s.name, s.name, s.description, "text/markdown",
                    (ctx, uri, params) -> s.getContent()));
        }
        return out;
    }

    @Override
    public List<McpPromptDef> getPrompts(McpServerDef server) {
        List<McpPromptDef> out = new ArrayList<>();
        for (SkillRegistry.Skill s : SkillRegistry.get().all()) {
            out.add(new McpPromptDef(s.name, s.description, null, (ctx, arguments) -> s.getContent()));
        }
        return out;
    }

    static Map<String, Object> prop(String type, String description) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("type", type);
        m.put("description", description);
        return m;
    }
}

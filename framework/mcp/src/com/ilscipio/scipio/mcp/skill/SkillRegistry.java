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

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.Debug;

/**
 * SCIPIO: 4.0.0: Finds Agent Skills ({@code skills/<name>/SKILL.md}) in every loaded component and validates them
 * against the tool registry (frontmatter present, server known, every referenced tool name real).
 */
public final class SkillRegistry {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static volatile SkillRegistry instance;

    /** A backticked snake_case token such as {@code order_find}; uppercase status ids and camelCase ids never match. */
    private static final Pattern TOOL_REF = Pattern.compile("`([a-z][a-z0-9]*(?:_[a-z0-9]+)+)`");

    public static final class Skill {
        public final String name;
        public final String description;
        public final String component;
        public final String server;
        public final Path path;
        private volatile String content;
        private volatile List<String> warnings = Collections.emptyList();

        Skill(String name, String description, String component, String server, Path path) {
            this.name = name;
            this.description = description;
            this.component = component;
            this.server = server;
            this.path = path;
        }

        public String getContent() {
            String c = content;
            if (c == null) {
                try {
                    c = new String(Files.readAllBytes(path), StandardCharsets.UTF_8);
                } catch (IOException e) {
                    c = "";
                }
                content = c;
            }
            return c;
        }

        /** Validation warnings from the last {@link SkillRegistry#validate(Set, Set)} run; empty when the skill is clean. */
        public List<String> getWarnings() { return warnings; }

        public Map<String, Object> toMap() {
            Map<String, Object> m = new LinkedHashMap<>();
            m.put("name", name);
            m.put("description", description);
            m.put("component", component);
            if (!server.isEmpty()) m.put("server", server);
            m.put("uri", "scipio://skills/" + name);
            if (!warnings.isEmpty()) m.put("warnings", new ArrayList<>(warnings));
            return m;
        }
    }

    private final Map<String, Skill> skills;

    private SkillRegistry() {
        Map<String, Skill> map = new LinkedHashMap<>();
        for (ComponentConfig cc : ComponentConfig.getAllComponents()) {
            Path dir = Paths.get(cc.getRootLocation(), "skills");
            if (!Files.isDirectory(dir)) continue;
            try (DirectoryStream<Path> ds = Files.newDirectoryStream(dir)) {
                for (Path skillDir : ds) {
                    Path md = skillDir.resolve("SKILL.md");
                    if (!Files.isRegularFile(md)) continue;
                    Skill s = parse(md, cc.getComponentName());
                    if (s != null && map.putIfAbsent(s.name, s) != null) {
                        Debug.logWarning("MCP: duplicate skill name " + s.name + " in " + md + "; first definition kept", module);
                    }
                }
            } catch (IOException e) {
                Debug.logWarning(e, "MCP: could not scan skills in " + dir, module);
            }
        }
        this.skills = Collections.unmodifiableMap(map);
        Debug.logInfo("MCP: skill registry built with " + skills.size() + " skills", module);
    }

    public static SkillRegistry get() {
        SkillRegistry r = instance;
        if (r == null) {
            synchronized (SkillRegistry.class) {
                r = instance;
                if (r == null) {
                    r = new SkillRegistry();
                    instance = r;
                }
            }
        }
        return r;
    }

    public static void reset() {
        instance = null;
    }

    public Skill get(String name) {
        return skills.get(name);
    }

    public List<Skill> all() {
        return new ArrayList<>(skills.values());
    }

    /**
     * Checks every skill: frontmatter fields present, {@code scipio-server} names a known server, and every
     * backticked tool-like name whose prefix belongs to a real tool family is a real tool. Warnings are stored on the
     * skill (shown by {@code scipio_list_skills} and the Webtools Skills page) and logged once.
     */
    public int validate(Set<String> serverNames, Set<String> toolNames) {
        Set<String> prefixes = new LinkedHashSet<>();
        for (String t : toolNames) {
            int us = t.indexOf('_');
            if (us > 0) prefixes.add(t.substring(0, us + 1));
        }
        int total = 0;
        for (Skill s : skills.values()) {
            List<String> w = new ArrayList<>();
            if (s.description.isEmpty()) w.add("frontmatter has no description");
            if (s.server.isEmpty()) w.add("frontmatter has no metadata.scipio-server");
            else if (!serverNames.contains(s.server)) w.add("unknown scipio-server " + s.server);
            String content = s.getContent();
            if (content.split("\r?\n").length < 20) w.add("skill body is very short");
            Set<String> unknown = new LinkedHashSet<>();
            Matcher m = TOOL_REF.matcher(content);
            while (m.find()) {
                String ref = m.group(1);
                if (toolNames.contains(ref)) continue;
                int us = ref.indexOf('_');
                if (us > 0 && prefixes.contains(ref.substring(0, us + 1))) unknown.add(ref);
            }
            for (String u : unknown) w.add("unknown tool name " + u);
            s.warnings = Collections.unmodifiableList(w);
            total += w.size();
            if (!w.isEmpty()) {
                Debug.logWarning("MCP: skill " + s.name + " (" + s.path + "): " + String.join("; ", w), module);
            }
        }
        return total;
    }

    static Skill parse(Path md, String component) {
        try {
            List<String> lines = Files.readAllLines(md, StandardCharsets.UTF_8);
            String name = null;
            String description = "";
            String server = "";
            if (!lines.isEmpty() && lines.get(0).trim().equals("---")) {
                for (int i = 1; i < lines.size(); i++) {
                    String line = lines.get(i);
                    if (line.trim().equals("---")) break;
                    String t = line.trim();
                    if (t.startsWith("name:")) name = unquote(t.substring(5));
                    else if (t.startsWith("description:")) description = unquote(t.substring(12));
                    else if (t.startsWith("scipio-server:") || t.startsWith("server:")) server = unquote(t.substring(t.indexOf(':') + 1));
                }
            }
            if (name == null || name.isEmpty()) name = md.getParent().getFileName().toString();
            return new Skill(name, description, component, server, md);
        } catch (IOException e) {
            Debug.logWarning(e, "MCP: could not read skill " + md, module);
            return null;
        }
    }

    private static String unquote(String s) {
        s = s.trim();
        if (s.length() >= 2 && ((s.startsWith("\"") && s.endsWith("\"")) || (s.startsWith("'") && s.endsWith("'")))) {
            s = s.substring(1, s.length() - 1);
        }
        return s;
    }
}

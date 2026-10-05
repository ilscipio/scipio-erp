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
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.stream.Stream;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * SCIPIO: 4.0.0: Packages every Agent Skill plus the MCP connection files into one Claude Code plugin zip, at run
 * time from the running server (Webtools "Download plugin", no Gradle needed). The layout matches the
 * {@code assembleAgentPlugin} Gradle task: {@code scipio-claude-plugin/.claude-plugin/plugin.json},
 * {@code .mcp.json}, {@code README.md}, {@code skills/<name>/SKILL.md}.
 */
public final class AgentPluginAssembler {

    public static final String ROOT = "scipio-claude-plugin";
    public static final String VERSION = "4.0.0";

    private AgentPluginAssembler() {}

    /**
     * Writes the plugin zip. {@code baseUrl} (for example {@code https://erp.example.com}) is written into
     * {@code .mcp.json}; when null the file uses the {@code SCIPIO_MCP_URL} environment variable instead. The token
     * always comes from the {@code SCIPIO_MCP_TOKEN} environment variable; it is never written into the zip.
     */
    public static void writeZip(OutputStream out, Collection<SkillRegistry.Skill> skills, String baseUrl) throws IOException {
        Set<String> written = new HashSet<>();
        try (ZipOutputStream zos = new ZipOutputStream(out, StandardCharsets.UTF_8)) {
            put(zos, ROOT + "/.claude-plugin/plugin.json", pluginJson());
            put(zos, ROOT + "/.mcp.json", mcpJson(baseUrl));
            put(zos, ROOT + "/README.md", readme(baseUrl));
            for (SkillRegistry.Skill skill : skills) {
                if (!written.add(skill.name)) continue;
                Path dir = skill.path.getParent();
                try (Stream<Path> files = Files.walk(dir)) {
                    for (Path f : (Iterable<Path>) files::iterator) {
                        if (!Files.isRegularFile(f)) continue;
                        String rel = dir.relativize(f).toString().replace('\\', '/');
                        put(zos, ROOT + "/skills/" + skill.name + "/" + rel, Files.readAllBytes(f));
                    }
                }
            }
        }
    }

    private static void put(ZipOutputStream zos, String name, String text) throws IOException {
        put(zos, name, text.getBytes(StandardCharsets.UTF_8));
    }

    private static void put(ZipOutputStream zos, String name, byte[] bytes) throws IOException {
        zos.putNextEntry(new ZipEntry(name));
        zos.write(bytes);
        zos.closeEntry();
    }

    public static String pluginJson() {
        return "{\n"
                + "  \"name\": \"scipio-erp\",\n"
                + "  \"version\": \"" + VERSION + "\",\n"
                + "  \"description\": \"Scipio ERP agent skills and MCP connection\",\n"
                + "  \"author\": {\n"
                + "    \"name\": \"ilscipio\"\n"
                + "  }\n"
                + "}\n";
    }

    public static String mcpJson(String baseUrl) {
        String url = (baseUrl != null && !baseUrl.isEmpty() ? baseUrl : "${SCIPIO_MCP_URL}") + "/admin/mcp";
        return "{\n"
                + "  \"mcpServers\": {\n"
                + "    \"scipio\": {\n"
                + "      \"type\": \"http\",\n"
                + "      \"url\": \"" + url + "\",\n"
                + "      \"headers\": {\n"
                + "        \"Authorization\": \"Bearer ${SCIPIO_MCP_TOKEN}\"\n"
                + "      }\n"
                + "    }\n"
                + "  }\n"
                + "}\n";
    }

    public static String readme(String baseUrl) {
        String url = baseUrl != null && !baseUrl.isEmpty() ? baseUrl : "https://your-scipio-host";
        return "# Scipio ERP agent plugin\n\n"
                + "This plugin bundles every Scipio ERP Agent Skill and the connection settings for the Scipio MCP server.\n\n"
                + "## Install\n\n"
                + "1. Create a token in Scipio: Webtools > Agent Access > Tokens (or ask an MCP_ADMIN user). Copy it once.\n"
                + "2. Export the token in the shell that runs your agent client:\n\n"
                + "   ```\n   export SCIPIO_MCP_TOKEN=\"<your token>\"\n"
                + (baseUrl == null || baseUrl.isEmpty() ? "   export SCIPIO_MCP_URL=\"" + url + "\"\n" : "")
                + "   ```\n\n"
                + "3. Install the plugin (skills plus the MCP connection):\n\n"
                + "   ```\n   claude plugin install ./" + ROOT + "\n   ```\n\n"
                + "   Or add only the MCP server:\n\n"
                + "   ```\n   claude mcp add --transport http scipio \"" + url + "/admin/mcp\" --header \"Authorization: Bearer $SCIPIO_MCP_TOKEN\"\n   ```\n\n"
                + "4. Restart the client and run `scipio_whoami` to confirm the connection and the token permissions.\n\n"
                + "## Contents\n\n"
                + "- `.claude-plugin/plugin.json`: the plugin manifest.\n"
                + "- `.mcp.json`: the Scipio MCP server connection (hub endpoint `/admin/mcp`).\n"
                + "- `skills/`: one folder per Agent Skill, copied from every component's own `skills/<name>/`.\n\n"
                + "Other clients (Cursor, VS Code, Claude Desktop): call `scipio_admin` with action `install_info` or open Webtools > Agent Access > Skills for ready-made snippets.\n";
    }
}

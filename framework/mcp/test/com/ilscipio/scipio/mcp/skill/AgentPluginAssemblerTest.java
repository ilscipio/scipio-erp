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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import org.junit.jupiter.api.Test;

public class AgentPluginAssemblerTest {

    private static Path skillDir(Path root, String name, String body) throws Exception {
        Path dir = root.resolve(name);
        Files.createDirectories(dir);
        Files.write(dir.resolve("SKILL.md"), ("---\nname: " + name + "\ndescription: test skill\nmetadata:\n  scipio-server: order\n---\n" + body).getBytes(StandardCharsets.UTF_8));
        Files.write(dir.resolve("extra.txt"), "x".getBytes(StandardCharsets.UTF_8));
        return dir.resolve("SKILL.md");
    }

    @Test
    public void zipHasManifestConnectionReadmeAndSkills() throws Exception {
        Path root = Files.createTempDirectory("skills");
        SkillRegistry.Skill a = SkillRegistry.parse(skillDir(root, "alpha-skill", "# a\n"), "order");
        SkillRegistry.Skill b = SkillRegistry.parse(skillDir(root, "beta-skill", "# b\n"), "party");
        ByteArrayOutputStream bytes = new ByteArrayOutputStream();
        AgentPluginAssembler.writeZip(bytes, Arrays.asList(a, b, a), "https://erp.example.com");
        Set<String> names = new LinkedHashSet<>();
        String mcpJson = null;
        try (ZipInputStream zin = new ZipInputStream(new ByteArrayInputStream(bytes.toByteArray()))) {
            ZipEntry e;
            while ((e = zin.getNextEntry()) != null) {
                names.add(e.getName());
                if (e.getName().endsWith("/.mcp.json")) {
                    mcpJson = new String(zin.readAllBytes(), StandardCharsets.UTF_8);
                }
            }
        }
        assertTrue(names.contains("scipio-claude-plugin/.claude-plugin/plugin.json"));
        assertTrue(names.contains("scipio-claude-plugin/.mcp.json"));
        assertTrue(names.contains("scipio-claude-plugin/README.md"));
        assertTrue(names.contains("scipio-claude-plugin/skills/alpha-skill/SKILL.md"));
        assertTrue(names.contains("scipio-claude-plugin/skills/alpha-skill/extra.txt"));
        assertTrue(names.contains("scipio-claude-plugin/skills/beta-skill/SKILL.md"));
        assertEquals(7, names.size(), "duplicate skill written once");
        assertTrue(mcpJson.contains("https://erp.example.com/admin/mcp"));
        assertTrue(mcpJson.contains("${SCIPIO_MCP_TOKEN}"));
        assertFalse(mcpJson.contains("scp_"));
        assertTrue(AgentPluginAssembler.mcpJson(null).contains("${SCIPIO_MCP_URL}/admin/mcp"));
    }

    @Test
    public void skillValidationFlagsUnknownToolsAndServers() throws Exception {
        Path root = Files.createTempDirectory("skills2");
        String body = "# s\n\nCall `order_find` then `order_bogus`. Status `ORDER_APPROVED` and `dryRun` are not tools.\n"
                + "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n11\n12\n13\n14\n15\n16\n17\n";
        SkillRegistry.Skill s = SkillRegistry.parse(skillDir(root, "gamma-skill", body), "order");
        Set<String> tools = new HashSet<>(Arrays.asList("order_find", "order_get", "scipio_whoami"));
        // validate() works on a registry instance; emulate its per-skill logic through a private registry built from the parsed skill
        java.lang.reflect.Constructor<SkillRegistry> ctor = SkillRegistry.class.getDeclaredConstructor();
        ctor.setAccessible(true);
        SkillRegistry reg = ctor.newInstance();
        java.lang.reflect.Field f = SkillRegistry.class.getDeclaredField("skills");
        f.setAccessible(true);
        f.set(reg, Collections.singletonMap(s.name, s));
        int warnings = reg.validate(new HashSet<>(Arrays.asList("order", "party")), tools);
        assertEquals(1, warnings);
        assertEquals("unknown tool name order_bogus", s.getWarnings().get(0));
        assertTrue(s.toMap().containsKey("warnings"));
    }
}

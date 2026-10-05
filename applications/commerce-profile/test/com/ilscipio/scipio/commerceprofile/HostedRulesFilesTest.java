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
package com.ilscipio.scipio.commerceprofile;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;

import com.ilscipio.scipio.cms.mcp.CmsMcp;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;

/**
 * Checks the data files of the profile: the security groups (seed data), the guard rules (secas.xml) against the
 * MCP tools that write code, and the component list of a store pod.
 */
public class HostedRulesFilesTest {

    /** Permissions that no store group may hold (blueprint section 5 and 09 section 6.3, rule 6). */
    private static final List<String> FORBIDDEN_FOR_STORES = Arrays.asList(
            "MCP_CODE_WRITE", "MCP_ENTITY_WRITE", "MCP_ENTITY_READ", "MCP_ADMIN", "CMS_CODE_VIEW", "CMS_CODE_UPDATE", "CMS_CODE_ADMIN", "HOSTED_OPS");

    private static Path component() {
        Path p = Paths.get("").toAbsolutePath();
        if (Files.exists(p.resolve("scipio-component.xml")) && p.getFileName().toString().equals("commerce-profile")) return p;
        Path q = p.resolve("applications").resolve("commerce-profile");
        return Files.exists(q) ? q : p;
    }

    private static String read(Path p) throws IOException {
        return new String(Files.readAllBytes(p), StandardCharsets.UTF_8);
    }

    private static Map<String, Set<String>> groupPermissions() throws IOException {
        String xml = read(component().resolve("data/CommerceProfileSecuritySeedData.xml"));
        Map<String, Set<String>> groups = new HashMap<>();
        Matcher g = Pattern.compile("<SecurityGroup\\s+groupId=\"(\\w+)\"").matcher(xml);
        while (g.find()) groups.put(g.group(1), new TreeSet<>());
        Matcher gp = Pattern.compile("<SecurityGroupPermission\\s+groupId=\"(\\w+)\"\\s+permissionId=\"(\\w+)\"").matcher(xml);
        while (gp.find()) {
            assertTrue(groups.containsKey(gp.group(1)), "permission for a group that the file does not define: " + gp.group(1));
            groups.get(gp.group(1)).add(gp.group(2));
        }
        return groups;
    }

    @Test
    public void storeGroupsHaveNoCodeOrServerPermission() throws IOException {
        Map<String, Set<String>> groups = groupPermissions();
        for (String id : Arrays.asList("TENANT_OWNER", "TENANT_STAFF", "TENANT_CONTENT")) {
            assertTrue(groups.containsKey(id), "missing group " + id);
            Set<String> perms = groups.get(id);
            assertFalse(perms.isEmpty(), id + " has no permission");
            assertTrue(perms.contains("MCP_ACCESS"), id + " needs MCP_ACCESS");
            for (String p : perms) {
                assertFalse(FORBIDDEN_FOR_STORES.contains(p), id + " holds " + p);
                assertFalse(p.startsWith("WEBTOOLS"), id + " holds " + p);
                assertFalse(p.startsWith("MANUFACTURING") || p.startsWith("HUMANRES"), id + " holds " + p);
                // No ADMIN permission, except the setup of the shop for the owner.
                assertFalse(p.endsWith("_ADMIN") && !(id.equals("TENANT_OWNER") && p.equals("SETUP_ADMIN")), id + " holds " + p);
            }
        }
        assertFalse(groups.containsKey("FULLADMIN"), "the profile does not touch FULLADMIN");
        // Content editors and staff do not use the gateway.
        assertFalse(groups.get("TENANT_CONTENT").contains("MCP_GATEWAY"));
        assertFalse(groups.get("TENANT_STAFF").contains("MCP_GATEWAY"));
    }

    @Test
    public void operatorGroupHoldsTheCodePermissions() throws IOException {
        Set<String> ops = groupPermissions().get("SCIPIO_OPS");
        assertTrue(ops.containsAll(Arrays.asList("HOSTED_OPS", "CMS_CODE_UPDATE", "MCP_CODE_WRITE")));
    }

    private static Map<String, String> secaRules() throws IOException {
        String xml = read(component().resolve("servicedef/secas.xml"));
        Map<String, String> rules = new HashMap<>();
        Matcher m = Pattern.compile("<eca service=\"(\\w+)\" event=\"auth\">\\s*<action service=\"(\\w+)\" mode=\"sync\" "
                + "ignore-error=\"false\" ignore-failure=\"false\"/>\\s*</eca>").matcher(xml);
        int count = 0;
        while (m.find()) {
            count++;
            assertFalse(rules.containsKey(m.group(1)), "two rules for " + m.group(1));
            rules.put(m.group(1), m.group(2));
        }
        assertEquals(count, xml.split("<eca ", -1).length - 1, "an eca element in secas.xml has an unexpected form");
        return rules;
    }

    @Test
    public void everyCmsCodeToolHasAGuardRule() throws IOException {
        Map<String, String> rules = secaRules();
        McpServer server = CmsMcp.class.getAnnotation(McpServer.class);
        List<String> codeServices = new ArrayList<>();
        for (McpServiceTool tool : server.serviceTools()) {
            if ("MCP_CODE_WRITE".equals(tool.permission())) codeServices.add(tool.service());
        }
        assertFalse(codeServices.isEmpty(), "CmsMcp has no MCP_CODE_WRITE tool: the test lost its input");
        for (String service : codeServices) {
            assertEquals("hostedGuardCmsCode", rules.get(service), "no guard for the MCP code tool of " + service);
        }
    }

    @Test
    public void guardRulesUseKnownGuards() throws IOException {
        Set<String> guards = new HashSet<>(Arrays.asList("hostedGuardCode", "hostedGuardCmsCode", "hostedGuardCmsView", "hostedGuardMail", "hostedGuardImport", "hostedGuardScreenFile"));
        Map<String, String> rules = secaRules();
        assertTrue(rules.size() > 50, "rules: " + rules.size());
        for (String guard : rules.values()) assertTrue(guards.contains(guard), guard);
        // Services that run code or load data must be guarded.
        for (String s : Arrays.asList("entityImport", "entityImportDir", "entityImportReaders", "createJobSandbox", "testGroovy", "createFile",
                "sendMail", "sendMailMultiPart", "sendMailFromScreen", "createFileFromScreen", "cmsImportXmlData", "cmsGetScriptTemplate")) {
            assertTrue(rules.containsKey(s), "no guard for " + s);
        }
    }

    @Test
    public void setupWizardFilesAreImportable() throws IOException {
        // The files that the setup wizard imports (SetupEvents.xml, SetupServices.java) must be on the allow list.
        String props = read(component().resolve("config/commerce-profile.properties"));
        for (String f : Arrays.asList("DemoGeneralChartOfAccounts.xml", "GlAccountData.xml", "ShippingData.xml", "ProductStoreData.xml")) {
            assertTrue(props.contains(f), f);
        }
        assertEquals("hostedGuardImport", secaRules().get("entityImport"));
    }

    private static List<String> loadList(Path file) throws IOException {
        List<String> out = new ArrayList<>();
        Matcher m = Pattern.compile("<load-component component-location=\"([\\w/-]+)\"").matcher(
                read(file).replaceAll("(?s)<!--.*?-->", ""));
        while (m.find()) out.add(m.group(1));
        return out;
    }

    @Test
    public void podComponentListIsTheFullListWithoutManufacturingAndHumanres() throws IOException {
        Path comp = component();
        List<String> full = loadList(comp.resolve("..").resolve("component-load.xml").normalize());
        List<String> pod = loadList(comp.resolve("profile/commerce-component-load.xml"));
        assertTrue(full.contains("manufacturing") && full.contains("humanres"), "the full list changed");
        List<String> expected = new ArrayList<>(full);
        expected.remove("manufacturing");
        expected.remove("humanres");
        expected.add("commerce-profile/pod-model");
        assertEquals(expected, pod);
        assertTrue(pod.contains("commerce-profile") && pod.contains("workeffort") && pod.contains("marketing"));
    }
}

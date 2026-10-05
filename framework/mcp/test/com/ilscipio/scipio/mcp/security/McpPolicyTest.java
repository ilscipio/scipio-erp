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
package com.ilscipio.scipio.mcp.security;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.ArgumentMatchers.eq;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;
import org.ofbiz.service.ModelService;

import com.ilscipio.scipio.mcp.registry.McpServerDef;
import com.ilscipio.scipio.mcp.registry.McpToolDef;
import com.ilscipio.scipio.mcp.web.McpRequest;

/**
 * Policy decisions that must fail closed: permission-less webapps, cross-component gateway calls, the hub,
 * the read-only AND rule and the trimmed read-only name heuristic.
 */
public class McpPolicyTest {

    private static final List<String> ORDER = Arrays.asList("OFBTOOLS", "ORDERMGR");
    private static final List<String> ACCOUNTING = Arrays.asList("OFBTOOLS", "ACCOUNTING");

    private GenericValue userLogin;
    private Set<String> granted;

    @BeforeEach
    public void setUp() {
        userLogin = mock(GenericValue.class);
        when(userLogin.getString("userLoginId")).thenReturn("agent");
        granted = new HashSet<>();
        McpPolicy.componentBasesSource = component -> {
            switch (component) {
                case "order": return Collections.singletonList(ORDER);
                case "accounting": return Collections.singletonList(ACCOUNTING);
                case "product": return Arrays.asList(Arrays.asList("OFBTOOLS", "CATALOG"), Arrays.asList("OFBTOOLS", "FACILITY"));
                default: return Collections.emptyList();
            }
        };
        McpPolicy.serviceComponentSource = svc -> {
            if (svc.name.contains("Invoice")) return "accounting";
            if (svc.name.contains("Order")) return "order";
            if (svc.name.contains("Product")) return "product";
            if (svc.name.contains("Country")) return "common";
            return "framework-thing";
        };
    }

    @AfterEach
    public void tearDown() {
        McpPolicy.componentBasesSource = null;
        McpPolicy.serviceComponentSource = null;
    }

    private Security security() {
        Security s = mock(Security.class);
        when(s.hasPermission(anyString(), any(GenericValue.class))).thenAnswer(inv -> granted.contains(inv.getArgument(0)));
        when(s.hasEntityPermission(anyString(), anyString(), any(GenericValue.class))).thenAnswer(inv -> {
            String base = inv.getArgument(0);
            String action = inv.getArgument(1);
            return granted.contains(base + action) || granted.contains(base + "_ADMIN");
        });
        return s;
    }

    private McpRequest request(McpServerDef server, String webapp, List<String> bases, boolean readOnlyToken) {
        HttpServletRequest http = mock(HttpServletRequest.class);
        when(http.getContextPath()).thenReturn("/" + webapp);
        when(http.getRemoteAddr()).thenReturn("127.0.0.1");
        McpRequest base = new McpRequest(http, null, null, null, null, security(), "req-1");
        GenericValue token = mock(GenericValue.class);
        when(token.getString("tokenId")).thenReturn("tok");
        when(token.getString("webapps")).thenReturn("*");
        when(token.getString("readOnly")).thenReturn(readOnlyToken ? "Y" : "N");
        base.setPrincipal(new McpPrincipal(token, userLogin));
        base.setServer(server);
        return base.forServer(server, webapp, bases);
    }

    private static ModelService service(String name, String engine, String invoke) {
        ModelService svc = new ModelService();
        svc.name = name;
        svc.engineName = engine;
        svc.invoke = invoke;
        return svc;
    }

    private static McpServerDef server(String name, String component, boolean hub, boolean anonymous) {
        return McpServerDef.builder(name).component(component).hub(hub).allowAnonymous(anonymous).build();
    }

    @Test
    public void gatewayUsesTheOwningComponentNotTheEndpoint() {
        granted.addAll(Arrays.asList("MCP_ACCESS", "MCP_GATEWAY", "OFBTOOLS_VIEW", "ORDERMGR_VIEW", "ORDERMGR_UPDATE"));
        McpRequest onOrder = request(server("order", "order", false, false), "ordermgr", ORDER, false);
        assertTrue(McpPolicy.checkService(onOrder, service("createOrderNote", "java", null), false, false).allowed);
        assertFalse(McpPolicy.checkService(onOrder, service("createInvoice", "java", null), false, false).allowed);
        assertFalse(McpPolicy.checkService(onOrder, service("findInvoices", "java", null), false, false).allowed);
        granted.add("ACCOUNTING_VIEW");
        assertTrue(McpPolicy.checkService(onOrder, service("findInvoices", "java", null), false, false).allowed);
        assertFalse(McpPolicy.checkService(onOrder, service("createInvoice", "java", null), false, false).allowed);
    }

    @Test
    public void permissionlessWebappFailsClosedOnTheGateway() {
        granted.addAll(Arrays.asList("MCP_ACCESS", "MCP_GATEWAY", "OFBTOOLS_VIEW", "ORDERMGR_VIEW"));
        McpRequest onShop = request(server("shop", "shop", false, true), "shop", Collections.emptyList(), false);
        assertFalse(McpPolicy.checkService(onShop, service("createInvoice", "java", null), false, false).allowed);
        assertFalse(McpPolicy.checkService(onShop, service("createOrderNote", "java", null), false, false).allowed);
        assertTrue(McpPolicy.checkService(onShop, service("getOrderHeader", "java", null), false, false).allowed);
    }

    @Test
    public void componentWithoutWebappNeedsAdminUnlessOpen() {
        granted.addAll(Arrays.asList("MCP_ACCESS", "MCP_GATEWAY", "OFBTOOLS_VIEW", "ORDERMGR_VIEW"));
        McpRequest onOrder = request(server("order", "order", false, false), "ordermgr", ORDER, false);
        assertFalse(McpPolicy.checkService(onOrder, service("getFrameworkThing", "java", null), false, false).allowed);
        // "common" is open by default: the endpoint base permission applies
        assertTrue(McpPolicy.checkService(onOrder, service("getCountryList", "java", null), false, false).allowed);
        granted.add("MCP_ADMIN");
        assertTrue(McpPolicy.checkService(onOrder, service("getFrameworkThing", "java", null), false, false).allowed);
    }

    @Test
    public void hubReadNeedsTheTargetComponentPermission() {
        granted.addAll(Arrays.asList("MCP_ACCESS", "MCP_GATEWAY", "OFBTOOLS_VIEW"));
        McpRequest onHub = request(server("admin", "webtools", true, false), "admin", Collections.singletonList("OFBTOOLS"), false);
        assertFalse(McpPolicy.checkService(onHub, service("findInvoices", "java", null), false, false).allowed);
        granted.add("ACCOUNTING_VIEW");
        assertTrue(McpPolicy.checkService(onHub, service("findInvoices", "java", null), false, false).allowed);
        assertFalse(McpPolicy.checkService(onHub, service("createInvoice", "java", null), false, false).allowed);
    }

    @Test
    public void everyBasePermissionIsRequired() {
        granted.addAll(Arrays.asList("MCP_ACCESS", "MCP_GATEWAY", "ORDERMGR_VIEW"));
        McpRequest onOrder = request(server("order", "order", false, false), "ordermgr", ORDER, false);
        assertFalse(McpPolicy.hasBasePermission(onOrder, ORDER, "_VIEW"));
        granted.add("OFBTOOLS_VIEW");
        assertTrue(McpPolicy.hasBasePermission(onOrder, ORDER, "_VIEW"));
        // a write needs _UPDATE on the application base only; OFBTOOLS stays at _VIEW
        assertFalse(McpPolicy.hasBasePermission(onOrder, ORDER, "_UPDATE"));
        granted.add("ORDERMGR_UPDATE");
        assertTrue(McpPolicy.hasBasePermission(onOrder, ORDER, "_UPDATE"));
        assertFalse(McpPolicy.hasBasePermission(onOrder, Collections.emptyList(), "_VIEW"));
    }

    @Test
    public void anyWebappOfTheComponentMaySatisfyTheRule() {
        granted.addAll(Arrays.asList("MCP_ACCESS", "MCP_GATEWAY", "OFBTOOLS_VIEW", "FACILITY_VIEW"));
        McpRequest onOrder = request(server("order", "order", false, false), "ordermgr", ORDER, false);
        assertTrue(McpPolicy.checkService(onOrder, service("getProductInventory", "java", null), false, false).allowed);
    }

    @Test
    public void readOnlyDeclarationNeverWidensAWriteService() {
        granted.addAll(Arrays.asList("MCP_ACCESS", "OFBTOOLS_VIEW", "ORDERMGR_VIEW"));
        McpRequest readOnlyToken = request(server("order", "order", false, false), "ordermgr", ORDER, true);
        ModelService write = service("createOrderNote", "entity-auto", "create");
        assertFalse(McpPolicy.checkService(readOnlyToken, write, true, true).allowed);
        ModelService read = service("getOrderHeader", "java", null);
        assertTrue(McpPolicy.checkService(readOnlyToken, read, true, true).allowed);
        // an explicit tool that declares itself a write stays a write
        assertFalse(McpPolicy.checkService(readOnlyToken, read, true, false).allowed);
    }

    @Test
    public void readOnlyHeuristicIsNarrow() {
        assertTrue(McpPolicy.isReadOnlyService(service("getOrderHeader", "java", null)));
        assertTrue(McpPolicy.isReadOnlyService(service("findInvoices", "java", null)));
        assertTrue(McpPolicy.isReadOnlyService(service("checkInventory", "java", null)));
        assertFalse(McpPolicy.isReadOnlyService(service("exportProducts", "java", null)));
        assertFalse(McpPolicy.isReadOnlyService(service("validateOrder", "java", null)));
        assertFalse(McpPolicy.isReadOnlyService(service("renderPage", "java", null)));
        assertFalse(McpPolicy.isReadOnlyService(service("queryAndUpdate", "java", null)));
        assertTrue(McpPolicy.isReadOnlyService(service("anything", "entity-auto", "find")));
        assertFalse(McpPolicy.isReadOnlyService(service("getSomething", "entity-auto", "delete")));
    }

    @Test
    public void compositeToolPassesTheEndpointGateAndChecksItsActions() {
        granted.addAll(Arrays.asList("MCP_ACCESS", "OFBTOOLS_VIEW", "ORDERMGR_VIEW"));
        McpToolDef find = McpToolDef.builder("find").readOnly(true).executor((c, a) -> null).build();
        McpToolDef create = McpToolDef.builder("create").readOnly(false).executor((c, a) -> null).build();
        McpToolDef order = com.ilscipio.scipio.mcp.registry.McpTopicTool.build("order", "Orders", "Orders.", 10, true, Arrays.asList(find, create));
        McpRequest readOnly = request(server("order", "order", false, false), "ordermgr", ORDER, true);
        assertTrue(McpPolicy.checkTool(readOnly, order, null).allowed);
        assertTrue(McpPolicy.checkTool(readOnly, find, null).allowed);
        assertFalse(McpPolicy.checkTool(readOnly, create, null).allowed);
        McpRequest full = request(server("order", "order", false, false), "ordermgr", ORDER, false);
        assertFalse(McpPolicy.checkTool(full, create, null).allowed);
        granted.add("ORDERMGR_UPDATE");
        assertTrue(McpPolicy.checkTool(full, create, null).allowed);
    }

    @Test
    public void handWrittenWriteToolFailsClosedWithoutBasePermission() {
        granted.addAll(Arrays.asList("MCP_ACCESS"));
        McpToolDef write = McpToolDef.builder("thing_update").readOnly(false).executor((c, a) -> null).build();
        McpRequest bare = request(server("bare", "bare", false, false), "bare", Collections.emptyList(), false);
        assertFalse(McpPolicy.checkTool(bare, write, null).allowed);
        McpRequest shop = request(server("shop", "shop", false, true), "shop", Collections.emptyList(), false);
        assertTrue(McpPolicy.checkTool(shop, write, null).allowed);
        McpRequest order = request(server("order", "order", false, false), "ordermgr", ORDER, false);
        assertFalse(McpPolicy.checkTool(order, write, null).allowed);
        granted.addAll(Arrays.asList("OFBTOOLS_VIEW", "ORDERMGR_UPDATE"));
        assertTrue(McpPolicy.checkTool(order, write, null).allowed);
    }

    @Test
    public void serverAccessChecksEveryBaseAndAllowsPublicWebapps() throws Exception {
        granted.addAll(Arrays.asList("MCP_ACCESS", "ORDERMGR_VIEW"));
        McpRequest order = request(server("order", "order", false, false), "ordermgr", ORDER, false);
        boolean denied = false;
        try {
            McpPolicy.checkServerAccess(order);
        } catch (McpAuthException e) {
            denied = true;
        }
        assertTrue(denied);
        granted.add("OFBTOOLS_VIEW");
        McpPolicy.checkServerAccess(order);
        McpPolicy.checkServerAccess(request(server("shop", "shop", false, true), "shop", Collections.emptyList(), false));
    }

    @Test
    public void toolPermissionReplacesTheBaseWriteGateOfTheBackingService() {
        // A floor device holds MANUFACTURING_FLOOR but not MANUFACTURING_UPDATE.
        granted.addAll(Arrays.asList("MCP_ACCESS", "OFBTOOLS_VIEW", "MANUFACTURING_VIEW", "MANUFACTURING_FLOOR"));
        McpPolicy.componentBasesSource = component -> "manufacturing".equals(component)
                ? Collections.singletonList(Arrays.asList("OFBTOOLS", "MANUFACTURING")) : Collections.emptyList();
        McpPolicy.serviceComponentSource = svc -> "manufacturing";
        McpRequest req = request(server("manufacturing", "manufacturing", false, false), "manufacturing",
                Arrays.asList("OFBTOOLS", "MANUFACTURING"), false);
        ModelService scan = service("recordProductionRunScan", "java", null);
        McpToolDef plain = McpToolDef.builder("production_run_scan").serviceName("recordProductionRunScan")
                .executor((c, a) -> null).build();
        assertFalse(McpPolicy.checkTool(req, plain, scan).allowed);
        McpToolDef floor = McpToolDef.builder("production_run_scan").serviceName("recordProductionRunScan")
                .permission("MANUFACTURING_FLOOR").executor((c, a) -> null).build();
        assertTrue(McpPolicy.checkTool(req, floor, scan).allowed);
        // The tool permission never lifts the deny list or the read-only token rule.
        assertFalse(McpPolicy.checkService(req, service("sendMailFromScreen", "java", null), true, false, true).allowed);
        McpRequest ro = request(server("manufacturing", "manufacturing", false, false), "manufacturing",
                Arrays.asList("OFBTOOLS", "MANUFACTURING"), true);
        assertFalse(McpPolicy.checkTool(ro, floor, scan).allowed);
        granted.clear();
        granted.addAll(Arrays.asList("MCP_ACCESS", "OFBTOOLS_VIEW", "MANUFACTURING_VIEW"));
        assertFalse(McpPolicy.checkTool(req, floor, scan).allowed);
    }
}

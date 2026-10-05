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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.when;

import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.junit.jupiter.api.Test;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;

import com.ilscipio.scipio.mcp.security.McpPrincipal;
import com.ilscipio.scipio.mcp.web.McpRequest;

public class McpTopicToolTest {

    private static Map<String, Object> prop(String type, String description) {
        Map<String, Object> m = new LinkedHashMap<>();
        m.put("type", type);
        m.put("description", description);
        return m;
    }

    private static McpToolDef action(String name, boolean readOnly, String[] params, String... required) {
        Map<String, Object> schema = McpToolDef.emptyObjectSchema();
        Map<String, Object> props = new LinkedHashMap<>();
        for (String p : params) props.put(p, prop("string", p + " value"));
        schema.put("properties", props);
        if (required.length > 0) schema.put("required", Arrays.asList(required));
        return McpToolDef.builder(name).description("Do " + name + ".").inputSchema(schema).readOnly(readOnly)
                .source("Test." + name).executor((ctx, args) -> McpResult.text(name + ":" + args)).build();
    }

    private static McpToolDef composite() {
        return McpTopicTool.build("invoice", "Invoices", "Invoices: find, read, create.", 10, true, Arrays.asList(
                action("find", true, new String[] { "invoiceId", "statusId", "limit" }),
                action("get", true, new String[] { "invoiceId" }, "invoiceId"),
                action("create", false, new String[] { "invoiceTypeId", "partyId", "statusId" }, "invoiceTypeId", "partyId")));
    }

    @Test
    @SuppressWarnings("unchecked")
    public void unionSchemaAndDescription() {
        McpToolDef t = composite();
        assertTrue(t.isComposite());
        assertEquals(3, t.getActions().size());
        assertFalse(t.isReadOnly());
        assertTrue(t.isDestructive());
        assertTrue(t.getTags().contains(McpTopicTool.COMPOSITE_TAG));
        Map<String, Object> props = (Map<String, Object>) t.getInputSchema().get("properties");
        assertEquals(Arrays.asList("action", "invoiceId", "statusId", "limit", "invoiceTypeId", "partyId"), new java.util.ArrayList<>(props.keySet()));
        assertEquals(Arrays.asList("find", "get", "create"), ((Map<String, Object>) props.get("action")).get("enum"));
        assertEquals(Collections.singletonList("action"), t.getInputSchema().get("required"));
        assertEquals("invoiceId value. Required for: get.", ((Map<String, Object>) props.get("invoiceId")).get("description"));
        assertEquals("[find] limit value", ((Map<String, Object>) props.get("limit")).get("description"));
        assertEquals("[create, required] invoiceTypeId value", ((Map<String, Object>) props.get("invoiceTypeId")).get("description"));
        assertEquals(Boolean.FALSE, t.getInputSchema().get("additionalProperties"));
        String d = t.getDescription();
        assertTrue(d.startsWith("Invoices: find, read, create.\nActions:\n- find: Do find.\n- get: Do get.\n- create: Do create."), d);
    }

    @Test
    public void unknownActionListsTheActions() {
        McpToolDef t = composite();
        Map<String, Object> args = new LinkedHashMap<>();
        args.put("action", "delete");
        McpToolException e = assertThrows(McpToolException.class, () -> t.getExecutor().execute(null, args));
        assertTrue(e.getMessage().contains("find, get, create"), e.getMessage());
    }

    @Test
    public void actionValidationAndPolicy() throws Exception {
        McpToolDef t = composite();
        McpCallContext ctx = new McpCallContext(request(true));
        Map<String, Object> args = new LinkedHashMap<>();
        args.put("action", "get");
        McpToolException missing = assertThrows(McpToolException.class, () -> t.getExecutor().execute(ctx, args));
        assertTrue(missing.getMessage().startsWith("action get: missing required argument: invoiceId"), missing.getMessage());
        args.put("invoiceId", "10000");
        McpResult r = t.getExecutor().execute(ctx, args);
        assertEquals("get:{invoiceId=10000}", r.getText());
        // a read-only token may not run the write action, even though the composite passed the endpoint gate
        Map<String, Object> write = new LinkedHashMap<>();
        write.put("action", "create");
        write.put("invoiceTypeId", "SALES_INVOICE");
        write.put("partyId", "DemoCustomer");
        McpToolException denied = assertThrows(McpToolException.class, () -> t.getExecutor().execute(ctx, write));
        assertTrue(denied.isDenied());
        assertTrue(denied.getMessage().contains("Read-only token"), denied.getMessage());
    }

    @Test
    public void disabledActionIsDropped() {
        McpToolDef t = composite();
        assertNotNull(t.getActions().get("find"));
        List<McpToolDef> one = Collections.singletonList(action("only", true, new String[0]));
        McpToolDef single = McpTopicTool.build("solo", "", "", 100, false, one);
        assertEquals("solo", single.getTitle());
        assertTrue(single.getDescription().startsWith("Actions:\n- only:"), single.getDescription());
    }

    private static McpRequest request(boolean readOnlyToken) {
        GenericValue userLogin = mock(GenericValue.class);
        when(userLogin.getString("userLoginId")).thenReturn("agent");
        Security s = mock(Security.class);
        when(s.hasPermission(anyString(), any(GenericValue.class))).thenReturn(true);
        when(s.hasEntityPermission(anyString(), anyString(), any(GenericValue.class))).thenReturn(true);
        HttpServletRequest http = mock(HttpServletRequest.class);
        when(http.getContextPath()).thenReturn("/accounting");
        when(http.getRemoteAddr()).thenReturn("127.0.0.1");
        McpRequest base = new McpRequest(http, null, null, null, null, s, "req-1");
        GenericValue token = mock(GenericValue.class);
        when(token.getString("tokenId")).thenReturn("tok");
        when(token.getString("webapps")).thenReturn("*");
        when(token.getString("readOnly")).thenReturn(readOnlyToken ? "Y" : "N");
        base.setPrincipal(new McpPrincipal(token, userLogin));
        McpServerDef server = McpServerDef.builder("accounting").component("accounting").build();
        base.setServer(server);
        return base.forServer(server, "accounting", Arrays.asList("OFBTOOLS", "ACCOUNTING"));
    }
}

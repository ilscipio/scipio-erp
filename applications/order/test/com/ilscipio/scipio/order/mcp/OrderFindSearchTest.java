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
package com.ilscipio.scipio.order.mcp;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;

/** R4-4: order/order find with the search text q and the paging offset (argument level). */
public class OrderFindSearchTest {

    @Test
    void searchTextIsCleanedAndBounded() {
        assertEquals("ada", OrderMcp.cleanSearch("  ada "));
        assertEquals("ada", OrderMcp.cleanSearch("%ada%"));
        assertNull(OrderMcp.cleanSearch("   "));
        assertNull(OrderMcp.cleanSearch(null));
        assertNull(OrderMcp.cleanSearch("x".repeat(OrderMcp.MAX_SEARCH_LENGTH + 1)));
        assertNotNull(OrderMcp.cleanSearch("x".repeat(OrderMcp.MAX_SEARCH_LENGTH)));
    }

    @Test
    void findHasQAndOffsetAsOptionalArguments() {
        Map<String, Boolean> params = new HashMap<>();
        for (Method m : OrderMcp.class.getDeclaredMethods()) {
            if (!m.getName().equals("findOrders")) continue;
            for (Parameter p : m.getParameters()) {
                McpParam mp = p.getAnnotation(McpParam.class);
                if (mp != null) params.put(mp.name(), mp.required());
            }
        }
        assertEquals(Boolean.FALSE, params.get("q"));
        assertEquals(Boolean.FALSE, params.get("offset"));
    }

    @Test
    void aTooLongSearchTextIsRefusedBeforeAnyQuery() {
        org.ofbiz.entity.GenericValue userLogin = mock(org.ofbiz.entity.GenericValue.class);
        org.ofbiz.security.Security sec = mock(org.ofbiz.security.Security.class);
        javax.servlet.http.HttpServletRequest http = mock(javax.servlet.http.HttpServletRequest.class);
        com.ilscipio.scipio.mcp.web.McpRequest req = new com.ilscipio.scipio.mcp.web.McpRequest(http, null, null, null, null, sec, "req-1");
        org.ofbiz.entity.GenericValue token = mock(org.ofbiz.entity.GenericValue.class);
        req.setPrincipal(new com.ilscipio.scipio.mcp.security.McpPrincipal(token, userLogin));
        McpCallContext ctx = new McpCallContext(req);
        McpToolException e = assertThrows(McpToolException.class, () -> OrderMcp.findOrders(ctx, null, null, null, null, null, null,
                null, "x".repeat(OrderMcp.MAX_SEARCH_LENGTH + 1), null, null, null));
        assertTrue(e.getMessage().startsWith("invalid_q"));
    }
}

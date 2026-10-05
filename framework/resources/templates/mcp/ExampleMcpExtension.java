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
package @component-package@.mcp;

import java.util.LinkedHashMap;
import java.util.Map;

import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpServerExtension;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;

/**
 * Template: extend an EXISTING MCP server (here {@code order}) from your own component.
 *
 * <p>Copy this file into your component's {@code src/.../mcp/} package, rename it, set {@code server} to the
 * server you extend, and add methods. The tools appear in that server's tool list (and on its endpoint) after
 * the next registry build; on a running server click "Reload agent registry" in Webtools. A tool name that the
 * server already defines is skipped with a warning; pick a name with your own prefix.</p>
 */
@McpServerExtension(server = "order",
        featuredServices = {"createOrderNote"},
        entities = {"OrderHeaderNoteView"},
        serviceTools = {
            @McpServiceTool(service = "createOrderNote", name = "example_order_note",
                    description = "Attach a note to an order (example extension).", readOnly = false, order = 200)
        })
public final class ExampleMcpExtension {

    private ExampleMcpExtension() {}

    @McpTool(name = "example_order_summary", description = "Example: one-line summary of an order.",
            readOnly = true, order = 210)
    public static Object summary(McpCallContext ctx,
            @McpParam(name = "orderId", description = "Order id", required = true) String orderId) throws McpToolException {
        try {
            GenericValue header = EntityQuery.use(ctx.getDelegator()).from("OrderHeader").where("orderId", orderId).queryOne();
            if (header == null) throw new McpToolException("Order not found: " + orderId);
            Map<String, Object> out = new LinkedHashMap<>();
            out.put("orderId", orderId);
            out.put("statusId", header.getString("statusId"));
            out.put("grandTotal", ResultConverter.toJson(header.getBigDecimal("grandTotal")));
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("Lookup failed: " + e.getMessage());
        }
    }
}

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
package @component-package@.@component-name@.mcp;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;

import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;

/**
 * MCP server profile for the @component-name@ component.
 *
 * <p>SCIPIO: 4.0.0: Added by createComponent template.</p>
 */
@McpServer(name = "@component-name@", title = "@component-resource-name@", component = "@component-name@",
        description = "@component-resource-name@ component: find @component-resource-name@ records.",
        entities = {"@component-resource-name@"},
        topics = {
            @McpTopic(name = "@component-name@", title = "@component-resource-name@", order = 10, featured = true,
                    description = "@component-resource-name@ records: find.")
        })
public final class @component-resource-name@Mcp {

    private @component-resource-name@Mcp() {}

    @McpTool(topic = "@component-name@", name = "find", description = "Find @component-resource-name@ records by id or name.", readOnly = true, order = 10)
    public static Object find@component-resource-name@(McpCallContext ctx,
            @McpParam(name = "id", description = "Record id", required = false) String id,
            @McpParam(name = "name", description = "Name to search for", required = false) String name,
            @McpParam(name = "limit", description = "Max rows to return", required = false) Integer limit) throws McpToolException {
        Delegator delegator = ctx.getDelegator();
        try {
            List<EntityCondition> conds = new ArrayList<>();
            if (id != null) conds.add(EntityCondition.makeCondition("id", id));
            if (name != null) conds.add(EntityCondition.makeCondition("name", name));
            List<Map<String, Object>> out = new ArrayList<>();
            for (GenericValue v : EntityQuery.use(delegator).from("@component-resource-name@").where(conds).maxRows(ctx.limit(limit)).queryList()) {
                Map<String, Object> row = new LinkedHashMap<>();
                row.put("id", v.getString("id"));
                row.put("name", v.getString("name"));
                out.add(row);
            }
            return out;
        } catch (GenericEntityException e) {
            throw new McpToolException("@component-resource-name@ search failed: " + e.getMessage());
        }
    }

}

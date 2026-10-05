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
package com.ilscipio.scipio.solr.mcp;

import java.util.LinkedHashMap;
import java.util.Map;

import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;

/**
 * SCIPIO: 4.0.0: MCP server profile for the solr component: product search index status, search and reindex.
 */
@McpServer(name = "solr", title = "Scipio Search (Solr)", component = "solr",
        description = "Product search index: run a keyword search, check status, and rebuild.",
        featuredServices = {"solrKeywordSearch", "solrProductsSearch", "checkSolrReady", "rebuildSolrIndex", "markSolrDataDirty"},
        serviceTools = {
            @McpServiceTool(service = "markSolrDataDirty",
                    topic = "solr",
                    name = "mark_dirty",
                    description = "Mark the search index as dirty so the next reindex updates it.",
                    readOnly = false,
                    destructive = "false",
                    order = 40)
        },
        topics = {
            @McpTopic(name = "solr", title = "Search Index", order = 10, featured = true,
                    description = "Search index: keyword search, status, reindex, mark dirty.")
        })
public final class SolrMcp {

    private SolrMcp() {}

    @McpTool(topic = "solr", name = "search", description = "Keyword search over the product index; returns matching products.", readOnly = true, order = 10)
    public static Object productSearch(McpCallContext ctx,
            @McpParam(name = "query", description = "Search text, e.g. 'red shirt'", required = true) String query,
            @McpParam(name = "queryFilter", description = "Optional Solr filter query, e.g. productStoreId:ScipioShop", required = false) String queryFilter,
            @McpParam(name = "sortBy", description = "Optional sort field", required = false) String sortBy,
            @McpParam(name = "limit", description = "Max rows to return", required = false) Integer limit) throws McpToolException {
        Map<String, Object> params = new LinkedHashMap<>();
        params.put("query", query);
        if (queryFilter != null) params.put("queryFilter", queryFilter);
        if (sortBy != null) params.put("sortBy", sortBy);
        params.put("viewSize", ctx.limit(limit));
        params.put("viewIndex", 0);
        return ResultConverter.toJsonMap(ctx.runService("solrKeywordSearch", params));
    }

    @McpTool(topic = "solr", name = "status", description = "Check whether the Solr index is reachable and ready.", readOnly = true, order = 20)
    public static Object status(McpCallContext ctx) throws McpToolException {
        return ResultConverter.toJsonMap(ctx.runService("checkSolrReady", new LinkedHashMap<>()));
    }

    @McpTool(topic = "solr", name = "reindex", description = "Rebuild the whole product search index.", readOnly = false, destructive = "true", requiresConfirmation = true, permission = "SOLRADM_ADMIN", order = 30)
    public static Object reindex(McpCallContext ctx,
            @McpParam(name = "onlyIfDirty", description = "Rebuild only when the index is marked dirty (default false)", required = false) Boolean onlyIfDirty) throws McpToolException {
        Map<String, Object> params = new LinkedHashMap<>();
        if (onlyIfDirty != null) params.put("onlyIfDirty", onlyIfDirty);
        return ResultConverter.toJsonMap(ctx.runService("rebuildSolrIndex", params));
    }
}

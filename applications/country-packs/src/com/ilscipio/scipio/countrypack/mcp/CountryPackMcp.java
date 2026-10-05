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
package com.ilscipio.scipio.countrypack.mcp;

import java.util.LinkedHashMap;
import java.util.Map;

import org.ofbiz.base.util.UtilValidate;

import com.ilscipio.scipio.countrypack.core.PackEngine;
import com.ilscipio.scipio.countrypack.service.CountryPackServiceImpl;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpResource;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.def.McpTopic;
import com.ilscipio.scipio.mcp.protocol.JsonRpc;
import com.ilscipio.scipio.mcp.registry.McpCallContext;
import com.ilscipio.scipio.mcp.registry.McpToolException;

/**
 * MCP server profile of the country packs: list the packs, apply a pack, read the tasks, finish a task. The resource
 * {@code scipio://setup/country-packs} is the pack part of the setup checklist (blueprint section 6).
 *
 * <p>SCIPIO: 4.0.0: Added (W1-03).</p>
 */
@McpServer(name = "country-packs", title = "Country packs", component = "country-packs",
        description = "Country packs: the legal texts, tasks and rules of the seller's home country and markets.",
        entities = {"CountryPackAssignment", "CountryPackTask"},
        topics = {
            @McpTopic(name = "country-packs", title = "Country packs", order = 45, featured = true,
                    description = "Set up a store for a country: legal texts from templates, registrations (LUCID ...), tax notes.")
        },
        serviceTools = {
            @McpServiceTool(service = "countryPackApply", topic = "country-packs", name = "apply",
                    description = "Apply a country pack to a store: creates the setup tasks and publishes the legal texts from the templates. "
                            + "The texts are marked TEMPLATE - not legal advice; the seller is liable and can change every word.",
                    readOnly = false, requiresConfirmation = true, order = 20),
            @McpServiceTool(service = "countryPackCompleteTask", topic = "country-packs", name = "complete_task",
                    description = "Set the state of a setup task: DONE (with the registration number), NOT_NEEDED or NEEDS_YOU.",
                    readOnly = false, order = 30)
        })
public final class CountryPackMcp {

    private CountryPackMcp() {
    }

    @McpTool(topic = "country-packs", name = "packs", readOnly = true, order = 10,
            description = "List the country packs that this server has: version, locale, currency, jurisdictions, legal texts.")
    public static Object packs(McpCallContext ctx) throws McpToolException {
        if (!CountryPackServiceImpl.canView(ctx.getSecurity(), ctx.getUserLogin())) {
            throw McpToolException.denied("Permission COUNTRYPACK_VIEW is required.");
        }
        Map<String, Object> out = new LinkedHashMap<>();
        out.put("packs", engine(ctx).list());
        return out;
    }

    @McpTool(topic = "country-packs", name = "status", readOnly = true, order = 25,
            description = "The packs of a store with the state of each task (needs you, done, done by Scipio, not needed).")
    public static Object status(McpCallContext ctx,
            @McpParam(name = "productStoreId", description = "Product store id; default: the store of the MCP session", required = false) String productStoreId)
            throws McpToolException {
        return serviceStatus(ctx, storeId(ctx, productStoreId));
    }

    @McpResource(uri = "scipio://setup/country-packs", name = "Country pack status", mimeType = "application/json",
            description = "Tasks of the country packs of the store of the session: the pack part of the setup checklist.")
    public static String statusResource(McpCallContext ctx) throws McpToolException {
        return JsonRpc.writePretty(serviceStatus(ctx, storeId(ctx, null)));
    }

    /** The status goes through the service countryPackStatus, so the permission check of the service applies to the caller. */
    private static Object serviceStatus(McpCallContext ctx, String storeId) throws McpToolException {
        Map<String, Object> result = ctx.runService("countryPackStatus", java.util.Collections.singletonMap("productStoreId", storeId));
        return result.get("status");
    }

    private static PackEngine engine(McpCallContext ctx) {
        return CountryPackServiceImpl.engine(ctx.getDelegator(), null);
    }

    private static String storeId(McpCallContext ctx, String productStoreId) throws McpToolException {
        String storeId = UtilValidate.isNotEmpty(productStoreId) ? productStoreId : ctx.getProductStoreId();
        if (UtilValidate.isEmpty(storeId)) {
            throw new McpToolException("productStoreId is required");
        }
        return storeId;
    }
}

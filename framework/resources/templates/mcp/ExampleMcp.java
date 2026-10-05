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
package @component-package@;

import java.util.Map;

import com.ilscipio.scipio.mcp.catalog.ResultConverter;
import com.ilscipio.scipio.mcp.def.McpAccess;
import com.ilscipio.scipio.mcp.def.McpParam;
import com.ilscipio.scipio.mcp.def.McpResource;
import com.ilscipio.scipio.mcp.def.McpServer;
import com.ilscipio.scipio.mcp.def.McpServiceTool;
import com.ilscipio.scipio.mcp.def.McpTool;
import com.ilscipio.scipio.mcp.registry.McpCallContext;

/**
 * Template MCP profile for one Scipio component. Copy this file into your component's own
 * {@code src/.../mcp/} package, rename the class, replace the placeholder package, and edit the
 * annotations to match your component. See docs/EXTENDING-AGENTIC.md for the full guide.
 */
@McpServer(
        // The server name. Clients see it as one tool group.
        name = "example",
        // A short human title. Clients may show this in a server list.
        title = "Scipio Example",
        // The component directory name. The server binds to every webapp of this component.
        component = "example",
        // Optional: bind to specific webapp context roots instead of the whole component.
        webapps = {},
        // Sent to clients as server instructions. Explain what the server is for.
        description = "Example MCP profile. Replace this text with a real description.",
        // Service names ranked first in scipio_search_services results for this server.
        featuredServices = {"createExampleThing"},
        // Glob patterns. serviceAllow widens visibility beyond the owning component; empty = component services only.
        serviceAllow = {},
        // Glob patterns denied even when the user has permission.
        serviceDeny = {"*Sql*"},
        // Entities readable through scipio_find_entity, gated by this webapp's _VIEW permission.
        entities = {"ExampleEntity"},
        // true only for a public-facing server (for example shop). PUBLIC tools then need no token.
        allowAnonymous = false,
        // An extra permission id required on top of the webapp base permission. Usually empty.
        requiredPermission = "",
        // One existing service exposed as a ready-made tool, with a schema derived from the service.
        serviceTools = {
                @McpServiceTool(
                        service = "updateExampleThing",
                        name = "example_update_thing",
                        description = "Update one example thing.",
                        featured = true,
                        readOnly = false,
                        destructive = "true",
                        requiresConfirmation = true,
                        exclude = {},
                        fixed = {})
        },
        // Extra McpToolProvider classes for tool sets a static annotation cannot express.
        providers = {})
public final class ExampleMcp {

    private ExampleMcp() {
    }

    /**
     * One hand-written tool. readOnly = true means the caller needs only the webapp _VIEW
     * permission, and the tool still works with a readOnly=Y token.
     */
    @McpTool(name = "example_find", description = "Find example things by id or name.",
            featured = true, readOnly = true, access = McpAccess.AUTH, permission = "",
            requiresConfirmation = false, tags = {})
    public static Object findExampleThings(McpCallContext ctx,
            @McpParam(name = "exampleId", description = "Example id", required = false) String exampleId,
            @McpParam(name = "name", description = "Name to search for", required = false) String name,
            @McpParam(name = "limit", description = "Max rows to return", required = false) Integer limit) {
        // Prefer an existing service over hand-written query code when one exists.
        Map<String, Object> params = ctx.serviceContext(Map.of(
                "exampleId", exampleId != null ? exampleId : "",
                "name", name != null ? name : "",
                "limit", ctx.limit(limit)));
        Map<String, Object> result = ctx.runService("findExampleThings", params);
        return ResultConverter.toJsonMap(result);
    }

    /**
     * One resource: a readable, addressable record, fetched by URI instead of by a tool call.
     * The {exampleId} placeholder in the uri makes this a template resource.
     */
    @McpResource(uri = "scipio://example/{exampleId}", name = "example",
            description = "One example thing, by id.", mimeType = "application/json")
    public static String exampleResource(McpCallContext ctx, Map<String, String> uriParams) {
        String exampleId = uriParams.get("exampleId");
        Map<String, Object> result = ctx.runService("getExampleThing",
                ctx.serviceContext(Map.of("exampleId", exampleId)));
        return ResultConverter.toJson(result);
    }
}

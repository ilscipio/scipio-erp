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

import java.util.Collections;
import java.util.List;

/**
 * SCIPIO: 4.0.0: Extension point that contributes tools, resources and prompts to a server.
 * Implementations need a public no-arg constructor and must be thread-safe; one instance serves all servers.
 * Register a provider through {@code @McpServer(providers = ...)}; the core and skill providers are always on.
 */
public interface McpToolProvider {

    List<McpToolDef> getTools(McpServerDef server);

    default List<McpResourceDef> getResources(McpServerDef server) {
        return Collections.emptyList();
    }

    default List<McpPromptDef> getPrompts(McpServerDef server) {
        return Collections.emptyList();
    }
}

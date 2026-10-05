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
import java.util.Map;

/**
 * SCIPIO: 4.0.0: MCP prompt definition.
 */
public final class McpPromptDef {

    public static final class Arg {
        public final String name;
        public final String description;
        public final boolean required;

        public Arg(String name, String description, boolean required) {
            this.name = name;
            this.description = description != null ? description : "";
            this.required = required;
        }
    }

    @FunctionalInterface
    public interface Renderer {
        /** Returns the prompt text (one user message). */
        String render(McpCallContext ctx, Map<String, String> arguments) throws Exception;
    }

    private final String name;
    private final String description;
    private final List<Arg> arguments;
    private final Renderer renderer;

    public McpPromptDef(String name, String description, List<Arg> arguments, Renderer renderer) {
        this.name = name;
        this.description = description != null ? description : "";
        this.arguments = arguments != null ? Collections.unmodifiableList(arguments) : Collections.emptyList();
        this.renderer = renderer;
    }

    public String getName() { return name; }
    public String getDescription() { return description; }
    public List<Arg> getArguments() { return arguments; }
    public Renderer getRenderer() { return renderer; }
}

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
package com.ilscipio.scipio.mcp.def;

import java.lang.annotation.Documented;
import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import com.ilscipio.scipio.mcp.registry.McpToolProvider;

/**
 * SCIPIO: 4.0.0: Adds tools, service tools, featured services, entities and providers to an existing
 * {@link McpServer} from any other component (an addon, a hot-deploy component, a customer project).
 *
 * <p>The annotated class is read exactly like an {@code @McpServer} class: public static methods with
 * {@link McpTool}, {@link McpResource} and {@link McpPrompt} become part of the named server. The extension
 * is merged after every {@code @McpServer} has been read; a tool name that already exists in the server is
 * skipped with a warning (the server's own definition wins). When {@link #server()} names no known server,
 * the extension is skipped with a warning.</p>
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface McpServerExtension {

    /** Name of the {@link McpServer} to extend, for example {@code order}. */
    String server();

    /** Additional services ranked first in {@code scipio_search_services}; unwrapped ones become featured tools. */
    String[] featuredServices() default {};

    /** Additional service name patterns visible and callable through the gateway in this server. */
    String[] serviceAllow() default {};

    /** Additional service name patterns denied in this server. */
    String[] serviceDeny() default {};

    /** Additional entities readable through {@code scipio_find_entity} with the webapp base permission. */
    String[] entities() default {};

    /** Existing services exposed as first-class tools with a derived JSON schema. */
    McpServiceTool[] serviceTools() default {};

    /** Composite tools added or extended by this class; members name them in {@code topic}. */
    McpTopic[] topics() default {};

    /** Additional tool providers instantiated for the server. */
    Class<? extends McpToolProvider>[] providers() default {};
}

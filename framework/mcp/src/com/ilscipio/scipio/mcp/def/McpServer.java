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
 * SCIPIO: 4.0.0: Declares an MCP server profile for an application.
 *
 * <p>The profile binds to every webapp of {@link #component()} unless {@link #webapps()} narrows it.
 * Static methods in the annotated class carry {@link McpTool}, {@link McpResource} and {@link McpPrompt}.
 * Every server automatically receives the shared core tools (service catalog, entity tools, skills).</p>
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface McpServer {

    /** Unique server name, for example {@code order}. */
    String name();

    String title() default "";

    /** Sent to clients as {@code instructions}; describe what the server is for and how to use it. */
    String description() default "";

    /** Component (directory) name whose webapps get this server, for example {@code order}. */
    String component() default "";

    /** Webapp names that get this server; overrides the component binding when set. */
    String[] webapps() default {};

    /** Services highlighted first in {@code scipio_search_services}. */
    String[] featuredServices() default {};

    /** Service name patterns (with {@code *}) visible and callable through the gateway in this server. Empty = component services. */
    String[] serviceAllow() default {};

    /** Service name patterns denied in this server even when the user has permission. */
    String[] serviceDeny() default {};

    /** Entity names readable through {@code scipio_find_entity} with the webapp base permission. */
    String[] entities() default {};

    /** Allow requests without a token; only tools with {@code access = PUBLIC} are visible to them. */
    boolean allowAnonymous() default false;

    /** Extra permission required on top of the webapp base permission, for example {@code MCP_ADMIN}. */
    String requiredPermission() default "";

    /** Marks the discovery hub (no component filter on the core tools). Only one server should set this. */
    boolean hub() default false;

    /** Existing services exposed as first-class tools with a derived JSON schema. */
    McpServiceTool[] serviceTools() default {};

    /** Composite tools; each groups the tools and service tools that name it in {@code topic}. */
    McpTopic[] topics() default {};

    /** Additional tool providers instantiated for this server. */
    Class<? extends McpToolProvider>[] providers() default {};
}

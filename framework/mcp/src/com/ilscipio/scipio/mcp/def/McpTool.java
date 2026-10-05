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

/**
 * SCIPIO: 4.0.0: Declares a hand-written MCP tool on a public static method of a {@link McpServer} class.
 *
 * <p>Method shape: {@code public static Object name(McpCallContext ctx, @McpParam(...) T param, ...)} or
 * {@code public static Object name(McpCallContext ctx, Map<String, Object> args)}. The return value may be a
 * {@code McpResult}, a Map, a List, a GenericValue or a String.</p>
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface McpTool {

    /** Tool name, snake_case, unique within the server, for example {@code order_find}. */
    /** Tool name, or the action name when {@link #topic()} is set. */
    String name();

    /** Name of the composite tool ({@link McpTopic}) this tool joins as one action; empty for a standalone tool. */
    String topic() default "";

    String title() default "";

    String description();

    boolean featured() default false;

    /** Sort key inside the server's tool list (lower first; featured tools sort before others at equal order). */
    int order() default 100;

    /** Read-only tools need only the webapp {@code _VIEW} permission and are allowed for read-only tokens. */
    boolean readOnly() default false;

    /** "true", "false" or "" (auto: the opposite of readOnly). */
    String destructive() default "";

    /** "true", "false" or "" (auto: same as readOnly). */
    String idempotent() default "";

    /** Extra permission the token user must hold, for example {@code ORDERMGR_UPDATE}. */
    String permission() default "";

    McpAccess access() default McpAccess.AUTH;

    /** Ask the client to confirm with a human before the call. Emitted as {@code _meta.requiresConfirmation}. */
    boolean requiresConfirmation() default false;

    String[] tags() default {};
}

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
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * SCIPIO: 4.0.0: Exposes an existing service as an MCP tool. Used only inside {@link McpServer#serviceTools()}.
 * The input schema is derived from the service IN parameters and the output schema from its OUT parameters.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface McpServiceTool {

    /** Service name. */
    String service();

    /** Name of the composite tool ({@link McpTopic}) this service joins as one action; empty for a standalone tool. */
    String topic() default "";

    /** Tool name; default is the service name converted to snake_case. */
    String name() default "";

    /** Tool description; default is the service description. */
    String description() default "";

    boolean featured() default false;

    /** Sort key inside the server's tool list (lower first). */
    int order() default 100;

    boolean readOnly() default false;

    /** "true", "false" or "" (auto: the opposite of readOnly). */
    String destructive() default "";

    /** "true", "false" or "" (auto: same as readOnly). */
    String idempotent() default "";

    /** Extra permission the token user must hold. */
    String permission() default "";

    boolean requiresConfirmation() default false;

    /** Service parameters hidden from the tool schema. */
    String[] exclude() default {};

    /** Fixed parameter values as {@code name=value}; hidden from the schema and always applied. */
    String[] fixed() default {};

    String[] tags() default {};
}

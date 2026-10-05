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
 * SCIPIO: 4.0.0: One composite tool that groups several actions. Every {@link McpTool} or {@link McpServiceTool}
 * whose {@code topic} names this topic becomes one value of the tool's {@code action} argument; the tool's schema
 * is the union of the action schemas. Declared in {@link McpServer#topics()} or {@link McpServerExtension#topics()}.
 */
@Documented
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface McpTopic {

    /** Tool name, e.g. {@code invoice}. */
    String name();

    String title() default "";

    /** One sentence that names the object and what the actions do. The action lines are appended automatically. */
    String description() default "";

    /** Sort key inside the server's tool list; lower first. */
    int order() default 100;

    boolean featured() default false;
}

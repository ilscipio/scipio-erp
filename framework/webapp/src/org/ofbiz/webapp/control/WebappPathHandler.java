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
package org.ofbiz.webapp.control;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.entity.Delegator;

/**
 * SCIPIO: 4.0.0: Generic handler for a fixed first path segment in every webapp (for example {@code /mcp}).
 *
 * <p>{@link ContextFilter} invokes the handler before any session is created and before the
 * {@code allowedPaths} check, so a component can add an endpoint to all webapps from one central definition.
 * Implementations are discovered through {@link WebappPathHandlerDef} and must be stateless and thread-safe.</p>
 */
public interface WebappPathHandler {

    /**
     * The first path segment this handler serves, without slashes (for example {@code mcp}).
     */
    String getPathSegment();

    /**
     * Handles the request. Returns true when the response is complete and the filter chain must stop.
     * Returns false to let normal processing continue.
     */
    boolean handle(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException;

    /**
     * SCIPIO: 4.0.0: Pooled runtime: returns the store that the credentials of the request belong to (for example
     * the MCP token route), or null. {@link TenantResolver} calls this before {@link #handle}; on a store host the
     * returned store must be the store of the host. Reads the master tables through baseDelegator only.
     */
    default String getTenantId(HttpServletRequest request, Delegator baseDelegator) {
        return null;
    }
}

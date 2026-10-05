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

import javax.servlet.DispatcherType;
import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * SCIPIO: 4.0.0: Pooled runtime: resolves the store of the request before any other filter (see {@link TenantResolver}).
 *
 * <p>Map it first in a webapp whose filters read the delegator before ContextFilter runs (the shop: URL rewrite,
 * CMS and SEO filters). In other webapps ContextFilter resolves the store itself. A no-op when multitenant=N.</p>
 */
public class TenantFilter implements Filter {

    private FilterConfig config;

    @Override
    public void init(FilterConfig config) throws ServletException {
        this.config = config;
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        if (!TenantResolver.resolve((HttpServletRequest) request, (HttpServletResponse) response, config.getServletContext())) {
            return;
        }
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        boolean slotHeld = httpRequest.getAttribute(TenantResolver.SLOT_ATTR) != null;
        if (!TenantResolver.acquireSlot(httpRequest, (HttpServletResponse) response)) {
            return;
        }
        try {
            chain.doFilter(request, response);
        } finally {
            if (!slotHeld) {
                TenantResolver.releaseSlot(httpRequest);
            }
            if (request.getDispatcherType() == DispatcherType.REQUEST) {
                TenantResolver.clear();
            }
        }
    }

    @Override
    public void destroy() {
        config = null;
    }
}

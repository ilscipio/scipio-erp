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

import java.io.File;
import java.io.IOException;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.tenant.TenantFiles;

/**
 * SCIPIO: 4.0.0: Pooled runtime: static files of each store in a shared webapp (G7). A no-op when multitenant=N.
 *
 * <p>Two layouts (init-param {@code layout}):</p>
 * <ul>
 * <li>{@code id} (default; images: image.url.prefix=/images/${tenantId}): the files of a store are under
 * {@code <prefix>/<tenantId>/}. A store host reads only its own folder; a first segment that names another store gets
 * 404. Base hosts read every folder.</li>
 * <li>{@code host} (sitemaps): a store host asks for {@code <prefix>/<file>}; the filter serves
 * {@code <prefix>/tenants/<tenantId of the host>/<file>}. A direct request for {@code <prefix>/tenants/...} on a store
 * host gets 404.</li>
 * </ul>
 * <p>An unknown host gets 404. With object storage ({@link TenantFiles}) and init-param {@code area}, a file that is
 * missing on this JVM's disk is read from the storage key {@code tenants/<tenantId>/<area>/<path>} first, so that a
 * file written by another JVM (upload, image job, sitemap job) is served here too.</p>
 * <p>Init-param {@code prefix}: the path below the context where the layout starts ("" for the images webapp,
 * "/sitemaps" in the shop). Requests outside the prefix pass unchanged.</p>
 */
public class TenantPathFilter implements Filter {

    private static final String TENANTS = "tenants";

    private String baseDelegatorName = "default";
    private String prefix = "";
    private boolean hostLayout;
    private String area;
    private ServletContext servletContext;

    @Override
    public void init(FilterConfig config) throws ServletException {
        servletContext = config.getServletContext();
        String name = servletContext.getInitParameter("entityDelegatorName");
        if (name != null && !name.isEmpty()) {
            baseDelegatorName = name;
        }
        String p = config.getInitParameter("prefix");
        if (UtilValidate.isNotEmpty(p) && !"/".equals(p)) {
            prefix = p.endsWith("/") ? p.substring(0, p.length() - 1) : p;
        }
        hostLayout = "host".equals(config.getInitParameter("layout"));
        area = config.getInitParameter("area");
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        if (!TenantResolver.isMultitenant()) {
            chain.doFilter(request, response);
            return;
        }
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;
        // decoded and normalized by the container
        String path = httpRequest.getServletPath() + (httpRequest.getPathInfo() != null ? httpRequest.getPathInfo() : "");
        if (!prefix.isEmpty() && !path.startsWith(prefix + "/")) {
            chain.doFilter(request, response);
            return;
        }
        String rest = path.substring(prefix.length()); // starts with "/"
        boolean baseHost = TenantResolver.isBaseHost(request.getServerName());
        Delegator baseDelegator = DelegatorFactory.getDelegator(baseDelegatorName);
        String hostStore = baseHost ? null : TenantResolver.getStoreOfHost(request.getServerName(), baseDelegator);
        if (!baseHost && hostStore == null) {
            httpResponse.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }
        if (hostLayout) {
            if (baseHost) {
                chain.doFilter(request, response); // base hosts read the tenants/ tree as it is
                return;
            }
            if (rest.startsWith("/" + TENANTS + "/") || rest.length() < 2) {
                httpResponse.sendError(HttpServletResponse.SC_NOT_FOUND);
                return;
            }
            String target = prefix + "/" + TENANTS + "/" + hostStore + rest;
            File fetched = ensureLocal(hostStore, rest.substring(1), target);
            if (fetched != null) {
                serve(fetched, httpResponse);
                return;
            }
            request.getRequestDispatcher(target).forward(request, response);
            return;
        }
        String first = RequestLinkUtil.getFirstPathElem(rest);
        if (first != null && !baseHost && !first.equals(hostStore) && TenantResolver.isStoreId(first, baseDelegator)) {
            httpResponse.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }
        if (first != null && rest.length() > first.length() + 2
                && (first.equals(hostStore) || (baseHost && TenantResolver.isStoreId(first, baseDelegator)))) {
            File fetched = ensureLocal(first, rest.substring(first.length() + 2), path);
            if (fetched != null) {
                serve(fetched, httpResponse);
                return;
            }
        }
        chain.doFilter(request, response);
    }

    /**
     * Reads the store file from the object storage when this JVM does not have it yet. Returns the file when it was
     * fetched now, null otherwise (it was there already, or the storage does not have it).
     */
    private File ensureLocal(String tenantId, String relPath, String webappPath) {
        if (UtilValidate.isEmpty(area) || TenantFiles.getStorage() == null) {
            return null;
        }
        String real = servletContext.getRealPath(webappPath);
        if (real == null) {
            return null;
        }
        File file = new File(real);
        if (!file.exists() && TenantFiles.fetch(tenantId, area, relPath, file)) {
            return file;
        }
        return null;
    }

    /**
     * Sends a file that was just fetched: the container's static resource cache still holds the "missing" state of this
     * path for a few seconds, so the default servlet would answer 404.
     */
    private void serve(File file, HttpServletResponse response) throws IOException {
        String type = servletContext.getMimeType(file.getName());
        response.setContentType(type != null ? type : "application/octet-stream");
        response.setContentLengthLong(file.length());
        java.nio.file.Files.copy(file.toPath(), response.getOutputStream());
    }

    @Override
    public void destroy() {
    }
}

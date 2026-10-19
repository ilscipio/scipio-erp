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
package com.ilscipio.scipio.acp.web;

import java.io.IOException;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.webapp.control.RequestLinkUtil;
import org.ofbiz.webapp.control.WebappPathHandler;
import org.ofbiz.webapp.control.WebappPathHandlerDef;

import com.ilscipio.scipio.acp.AcpConfig;
import com.ilscipio.scipio.acp.AcpJson;

/**
 * SCIPIO: 4.0.0: The ACP discovery document {@code GET /.well-known/acp.json} (no auth). A store webapp answers for
 * itself ({@code /shop/.well-known/acp.json}); a webapp without a store (the root webapp) answers for the webapp
 * named by {@code discovery.webapp} (default shop) of acp.properties.
 */
@WebappPathHandlerDef
public final class AcpDiscoveryHandler implements WebappPathHandler {

    @Override
    public String getPathSegment() {
        return ".well-known";
    }

    @Override
    public boolean handle(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        if (delegator == null || !AcpConfig.isEnabled(delegator)) return false;
        String path = RequestLinkUtil.getServletAndPathInfo(request);
        if (!"/.well-known/acp.json".equals(path) && !".well-known/acp.json".equals(path)) return false;
        if (!"GET".equals(request.getMethod()) && !"HEAD".equals(request.getMethod())) {
            response.setHeader("Allow", "GET");
            response.sendError(405);
            return true;
        }
        AcpPathHandler.Store store = AcpPathHandler.Store.of(request, delegator);
        String base;
        String productStoreId;
        if (store != null) {
            base = AcpPathHandler.baseUrl(request, store.productStoreId);
            productStoreId = store.productStoreId;
        } else {
            String webapp = AcpConfig.get(delegator, "discovery.webapp", "shop").trim();
            productStoreId = AcpConfig.get(delegator, "discovery.productStoreId", null);
            if (productStoreId == null) productStoreId = firstStore(delegator);
            String root = AcpPathHandler.baseUrl(request);
            if (root.endsWith(request.getContextPath()) && !request.getContextPath().isEmpty()) {
                root = root.substring(0, root.length() - request.getContextPath().length());
            }
            base = root + "/" + webapp;
        }
        response.setHeader("Cache-Control", "public, max-age=3600");
        response.setHeader("Access-Control-Allow-Origin", "*");
        AcpPathHandler.sendJson(response, 200, AcpJson.write(AcpPathHandler.discovery(delegator, base + "/acp", productStoreId)));
        return true;
    }

    private static String firstStore(Delegator delegator) {
        try {
            GenericValue ws = EntityQuery.use(delegator).from("WebSite").where("webSiteId", "ScipioWebStore").cache().queryOne();
            if (ws != null && UtilValidate.isNotEmpty(ws.getString("productStoreId"))) return ws.getString("productStoreId");
        } catch (GenericEntityException e) {
            return null;
        }
        return null;
    }
}

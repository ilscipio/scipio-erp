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
package com.ilscipio.scipio.mcp.web;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Locale;
import java.util.TimeZone;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.security.Security;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.website.WebSiteWorker;

import com.ilscipio.scipio.mcp.protocol.McpSession;
import com.ilscipio.scipio.mcp.registry.McpServerDef;
import com.ilscipio.scipio.mcp.security.McpPrincipal;

/**
 * SCIPIO: 4.0.0: Per-request state for one MCP HTTP call: webapp, framework objects, principal, session, server.
 */
public final class McpRequest {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final HttpServletRequest httpRequest;
    private final ServletContext servletContext;
    private final WebappInfo webappInfo;
    private final String webappName;
    private final String componentName;
    private final List<String> basePermissions;
    private final Delegator delegator;
    private final LocalDispatcher dispatcher;
    private final Security security;
    private final String requestId;
    private final String remoteAddr;

    private McpPrincipal principal;
    private McpSession session;
    private McpServerDef server;
    private String protocolVersion;

    private volatile boolean commerceResolved;
    private String webSiteId;
    private String productStoreId;
    private String currencyUomId;
    private Locale storeLocale;

    public McpRequest(HttpServletRequest httpRequest, ServletContext servletContext, WebappInfo webappInfo,
                      Delegator delegator, LocalDispatcher dispatcher, Security security, String requestId) {
        this.httpRequest = httpRequest;
        this.servletContext = servletContext;
        this.webappInfo = webappInfo;
        String ctx = webappInfo != null ? webappInfo.getContextRoot() : httpRequest.getContextPath();
        if (ctx == null) ctx = "";
        this.webappName = ctx.startsWith("/") ? ctx.substring(1) : ctx;
        this.componentName = webappInfo != null && webappInfo.getComponentConfig() != null ? webappInfo.getComponentConfig().getComponentName() : "";
        String[] perms = webappInfo != null ? webappInfo.getBasePermission() : null;
        java.util.List<String> permList = new java.util.ArrayList<>();
        if (perms != null) {
            for (String p : perms) {
                // "NONE" marks a public webapp (shop) and is not a permission id
                if (p != null && !p.trim().isEmpty() && !"NONE".equalsIgnoreCase(p.trim())) permList.add(p.trim());
            }
        }
        this.basePermissions = Collections.unmodifiableList(permList);
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.security = security;
        this.requestId = requestId;
        this.remoteAddr = httpRequest.getRemoteAddr();
    }

    /**
     * A view of this request bound to another server (used by the hub to call an app's tools): same principal,
     * session and framework objects, but the target server and its webapp base permissions.
     */
    public McpRequest forServer(McpServerDef targetServer, String targetWebappName, List<String> targetBasePermissions) {
        McpRequest r = new McpRequest(this, targetWebappName, targetBasePermissions);
        r.principal = this.principal;
        r.session = this.session;
        r.server = targetServer;
        r.protocolVersion = this.protocolVersion;
        return r;
    }

    private McpRequest(McpRequest base, String webappName, List<String> basePermissions) {
        this.httpRequest = base.httpRequest;
        this.servletContext = base.servletContext;
        this.webappInfo = base.webappInfo;
        this.webappName = webappName != null ? webappName : base.webappName;
        this.componentName = base.componentName;
        this.basePermissions = basePermissions != null ? Collections.unmodifiableList(new java.util.ArrayList<>(basePermissions)) : base.basePermissions;
        this.delegator = base.delegator;
        this.dispatcher = base.dispatcher;
        this.security = base.security;
        this.requestId = base.requestId;
        this.remoteAddr = base.remoteAddr;
    }

    public HttpServletRequest getHttpRequest() { return httpRequest; }
    public ServletContext getServletContext() { return servletContext; }
    public WebappInfo getWebappInfo() { return webappInfo; }
    /** Context root without the leading slash, e.g. {@code ordermgr}, {@code admin}, {@code shop}. */
    public String getWebappName() { return webappName; }
    public String getComponentName() { return componentName; }
    /** Base permissions of the webapp (e.g. ORDERMGR); empty when the webapp declares none. */
    public List<String> getBasePermissions() { return basePermissions; }
    public Delegator getDelegator() { return delegator; }
    public LocalDispatcher getDispatcher() { return dispatcher; }
    public Security getSecurity() { return security; }
    public String getRequestId() { return requestId; }
    public String getRemoteAddr() { return remoteAddr; }

    public McpPrincipal getPrincipal() { return principal; }
    public void setPrincipal(McpPrincipal principal) { this.principal = principal; }
    public boolean isAnonymous() { return principal == null; }
    public GenericValue getUserLogin() { return principal != null ? principal.getUserLogin() : null; }
    public String getUserLoginId() { return principal != null ? principal.getUserLoginId() : null; }
    public String getTokenId() { return principal != null ? principal.getTokenId() : null; }

    public McpSession getSession() { return session; }
    public void setSession(McpSession session) { this.session = session; }
    public McpServerDef getServer() { return server; }
    public void setServer(McpServerDef server) { this.server = server; }
    public String getProtocolVersion() { return protocolVersion; }
    public void setProtocolVersion(String protocolVersion) { this.protocolVersion = protocolVersion; }

    public Locale getLocale() {
        GenericValue ul = getUserLogin();
        if (ul != null && UtilValidate.isNotEmpty(ul.getString("lastLocale"))) {
            return Locale.forLanguageTag(ul.getString("lastLocale").replace('_', '-'));
        }
        resolveCommerce();
        return storeLocale != null ? storeLocale : Locale.getDefault();
    }

    public TimeZone getTimeZone() {
        GenericValue ul = getUserLogin();
        if (ul != null && UtilValidate.isNotEmpty(ul.getString("lastTimeZone"))) {
            return TimeZone.getTimeZone(ul.getString("lastTimeZone"));
        }
        return TimeZone.getDefault();
    }

    public String getWebSiteId() { resolveCommerce(); return webSiteId; }
    public String getProductStoreId() { resolveCommerce(); return productStoreId; }
    public String getCurrencyUomId() { resolveCommerce(); return currencyUomId; }

    private void resolveCommerce() {
        if (commerceResolved) return;
        synchronized (this) {
            if (commerceResolved) return;
            try {
                String wsId = WebSiteWorker.getWebSiteId(servletContext);
                if (UtilValidate.isNotEmpty(wsId)) {
                    webSiteId = wsId;
                    GenericValue webSite = EntityQuery.use(delegator).from("WebSite").where("webSiteId", wsId).cache().queryOne();
                    if (webSite != null) {
                        productStoreId = webSite.getString("productStoreId");
                        if (productStoreId != null) {
                            GenericValue store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", productStoreId).cache().queryOne();
                            if (store != null) {
                                currencyUomId = store.getString("defaultCurrencyUomId");
                                String loc = store.getString("defaultLocaleString");
                                if (UtilValidate.isNotEmpty(loc)) storeLocale = Locale.forLanguageTag(loc.replace('_', '-'));
                            }
                        }
                    }
                }
            } catch (GenericEntityException e) {
                Debug.logWarning(e, "MCP: could not resolve store context for webapp " + webappName, module);
            }
            commerceResolved = true;
        }
    }
}

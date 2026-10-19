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
package com.ilscipio.scipio.acp.checkout;

import java.util.Locale;

import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;

/**
 * SCIPIO: 4.0.0: The store and the agent of one ACP call (REST or the MCP binding).
 */
public final class AcpContext {

    private final Delegator delegator;
    private final LocalDispatcher dispatcher;
    private final String productStoreId;
    private final String webSiteId;
    private final String webappName;
    private final String baseUrl;
    private final String tokenId;
    private final String agentUserLoginId;
    private final Locale locale;
    private GenericValue store;
    private GenericValue systemLogin;

    public AcpContext(Delegator delegator, LocalDispatcher dispatcher, String productStoreId, String webSiteId, String webappName,
            String baseUrl, String tokenId, String agentUserLoginId, Locale locale) {
        this.delegator = delegator;
        this.dispatcher = dispatcher;
        this.productStoreId = productStoreId;
        this.webSiteId = webSiteId;
        this.webappName = webappName;
        this.baseUrl = baseUrl;
        this.tokenId = tokenId;
        this.agentUserLoginId = agentUserLoginId;
        this.locale = locale != null ? locale : Locale.US;
    }

    public Delegator getDelegator() { return delegator; }
    public LocalDispatcher getDispatcher() { return dispatcher; }
    public String getProductStoreId() { return productStoreId; }
    public String getWebSiteId() { return webSiteId; }
    public String getWebappName() { return webappName; }
    /** The secure address of the store webapp, for example https://shop.example.com/shop (no trailing slash). */
    public String getBaseUrl() { return baseUrl; }
    public String getTokenId() { return tokenId; }
    public String getAgentUserLoginId() { return agentUserLoginId; }
    public Locale getLocale() { return locale; }

    public GenericValue getStore() throws GenericEntityException {
        if (store == null) {
            store = EntityQuery.use(delegator).from("ProductStore").where("productStoreId", productStoreId).cache().queryOne();
        }
        return store;
    }

    public String getDefaultCurrency() throws GenericEntityException {
        GenericValue s = getStore();
        String c = s != null ? s.getString("defaultCurrencyUomId") : null;
        return UtilValidate.isNotEmpty(c) ? c : "USD";
    }

    /** The login that runs the order services (the agent token login is only recorded on the order). */
    public GenericValue getSystemLogin() throws GenericEntityException {
        if (systemLogin == null) {
            systemLogin = EntityQuery.use(delegator).from("UserLogin").where("userLoginId", "system").cache().queryOne();
        }
        return systemLogin;
    }

    public String url(String pathOrUrl) {
        if (UtilValidate.isEmpty(pathOrUrl)) return null;
        if (pathOrUrl.startsWith("http://") || pathOrUrl.startsWith("https://")) return pathOrUrl;
        return baseUrl + (pathOrUrl.startsWith("/") ? pathOrUrl : "/" + pathOrUrl);
    }
}

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
package com.ilscipio.scipio.product.seo;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletContext;

import org.ofbiz.base.util.Debug;

/**
 * SCIPIO: Used to register the SEO-enabled websites in a central registry.
 * <p>
 * TODO: currently under-exploited.
 */
public class WebsiteSeoConfig {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // WARN: special sync behavior
    private static Map<String, WebsiteSeoConfig> websiteSeoConfigs;

    static {
        Debug.logInfo("Scipio: SEO: Initializing central SEO website registry"
                + "\nNOTE: websites in this registry are not automatically SEO URL-producing; see SeoConfig.xml", module);
        websiteSeoConfigs = Collections.emptyMap();
    }

    private final String webSiteId;
    private final boolean seoEnabled;

    private WebsiteSeoConfig(String webSiteId, boolean seoEnabled) {
        this.webSiteId = webSiteId;
        this.seoEnabled = seoEnabled;
    }

    public static WebsiteSeoConfig makeConfig(ServletContext context, boolean seoEnabled) {
        return new WebsiteSeoConfig(getWebSiteId(context), seoEnabled);
    }

    public static WebsiteSeoConfig makeConfig(String webSiteId, boolean seoEnabled) {
        return new WebsiteSeoConfig(webSiteId, seoEnabled);
    }

    /**
     * Registers website into the global SEO website registry.
     */
    public static synchronized void registerWebsiteForSeo(WebsiteSeoConfig config) {
        Debug.logInfo("Scipio: SEO: Registering website for SEO: " + config.getWebSiteId(), module);
        Map<String, WebsiteSeoConfig> newConfigs = new HashMap<>(websiteSeoConfigs);
        newConfigs.put(config.getWebSiteId(), config);
        websiteSeoConfigs = Collections.unmodifiableMap(newConfigs);
    }

    public static WebsiteSeoConfig getRegisteredConfig(String webSiteId) {
        return websiteSeoConfigs.get(webSiteId);
    }

    public static boolean isSeoEnabled(String webSiteId) {
        WebsiteSeoConfig config = getRegisteredConfig(webSiteId);
        return (config != null) && config.isSeoEnabled();
    }

    public String getWebSiteId() {
        return webSiteId;
    }

    public boolean isSeoEnabled() {
        return seoEnabled;
    }

    public static String getWebSiteId(ServletContext context) {
        return context.getInitParameter("webSiteId");
    }
}
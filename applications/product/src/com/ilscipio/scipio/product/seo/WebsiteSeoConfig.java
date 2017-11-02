package com.ilscipio.scipio.product.seo;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletContext;

public class WebsiteSeoConfig {
    
    // WARN: special sync behavior
    private static Map<String, WebsiteSeoConfig> websiteSeoConfigs = Collections.emptyMap();

    private final String webSiteId;
    private final boolean seoEnabled;

    public static synchronized void registerWebsiteForSeo(WebsiteSeoConfig config) {
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
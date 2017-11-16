package com.ilscipio.scipio.product.seo.sitemap;

import java.io.Serializable;
import java.net.URL;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.LocalDispatcher;

@SuppressWarnings("serial")
public class SitemapConfig implements Serializable {
    public static final String module = SitemapConfig.class.getName();
    
    private static class StaticConfigHolder {
        private static final Map<String, SitemapConfig> staticConfigs = Collections.unmodifiableMap(readStaticConfigs());
    }
    
    protected final String webConfPath;
    protected final String baseUrl;
    protected final String sitemapDir;
    protected final String sitemapSubDir;
    protected final String sitemapExtension;
    protected final String sitemapIndexFile;
    protected final String productFilePrefix;
    protected final String categoryFilePrefix;
    protected final Integer sizemapSize;
    protected final Integer indexSize;
    protected final boolean doProduct;
    protected final boolean doCategory;
    
    public SitemapConfig(Map<String, Object> map, Integer sizemapSize, Integer indexSize, Boolean doProduct, Boolean doCategory) {
        this.webConfPath = (String) map.get("urlConfPath");
        this.baseUrl = (String) map.get("baseUrl");
        this.sitemapDir = (String) map.get("sitemapDir");
        this.sitemapSubDir = (String) map.get("sitemapSubDir");
        this.sitemapExtension = (String) map.get("sitemapExtension");
        this.sitemapIndexFile = (String) map.get("sitemapIndexFile");
        this.productFilePrefix = (String) map.get("productFilePrefix");
        this.categoryFilePrefix = (String) map.get("categoryFilePrefix");
        this.sizemapSize = intOrStr(map.get("sizemapSize"), SitemapWorker.DEFAULT_SITEMAP_SIZE);
        this.indexSize = intOrStr(map.get("indexSize"), SitemapWorker.DEFAULT_SITEMAP_SIZE);
        this.doProduct = UtilMisc.booleanValueVersatile(map.get("doProduct"), true);
        this.doCategory = UtilMisc.booleanValueVersatile(map.get("doCategory"), true);
    }
    
    public static SitemapConfig getSitemapConfigForWebsite(Delegator delegator, LocalDispatcher dispatcher, String webSiteId) {
        return StaticConfigHolder.staticConfigs.get(webSiteId);
    }
    
    protected static Map<String, SitemapConfig> readStaticConfigs() {
        Map<String, SitemapConfig> configs = new HashMap<>();
        try {
            ClassLoader loader = Thread.currentThread().getContextClassLoader();
            Enumeration<URL> resources = loader.getResources("sitemap.properties");
            while (resources.hasMoreElements()) {
                URL propertyURL = resources.nextElement();
                Debug.logInfo("Seo: Sitemap: loading properties: " + propertyURL, module);
                Properties props = UtilProperties.getProperties(propertyURL);
                if (UtilValidate.isEmpty(props)) {
                    Debug.logError("Seo: Sitemap: Unable to locate properties file " + propertyURL, module);
                } else {
                    loadConfigs(props, configs);
                }
            }
        } catch (Exception e) {
            Debug.logError(e, "Seo: Sitemap: Could not load list of sitemap.properties", module);
        }
        return configs;
    }
    
    protected static void loadConfigs(Properties props, Map<String, SitemapConfig> configs) {
        Map<String, Map<String, Object>> webSiteConfigs = new HashMap<>();
        UtilProperties.extractPropertiesWithPrefixAndId(webSiteConfigs, props, "sitemap.");
    }
    
    private static Integer intOrStr(Object obj, Integer defaultValue) {
        if (obj instanceof Integer) return obj != null ? (Integer) obj : defaultValue;
        else if (obj instanceof String) {
            String str = (String) obj;
            if (UtilValidate.isEmpty(str)) return defaultValue;
            try {
                return Integer.parseInt(str);
            } catch(Exception e) {
                Debug.logError("Invalid integer value from sitemap config: " + obj + "; using default: " + defaultValue, module);

            }
        } else {
            Debug.logError("Invalid integer value from sitemap config: " + obj + "; using default: " + defaultValue, module);
        }
        return defaultValue;
    }
}
package com.ilscipio.scipio.product.seo.sitemap;

import java.io.Serializable;
import java.net.URL;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.TimeZone;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.LocalDispatcher;

import com.redfin.sitemapgenerator.W3CDateFormat;
import com.redfin.sitemapgenerator.W3CDateFormat.Pattern;

@SuppressWarnings("serial")
public class SitemapConfig implements Serializable {
    public static final String module = SitemapConfig.class.getName();

    public static final String SITEMAPCONFIGS_RESOURCE = "sitemaps"; // .properties
    public static final String SITEMAPCOMMON_RESOURCE = "sitemapcommon"; // .properties
    
    public static final int DEFAULT_SITEMAP_SIZE = UtilProperties.getPropertyAsInteger(SITEMAPCOMMON_RESOURCE, "sitemap.default.sitemapsize", 50000);
    public static final int DEFAULT_INDEX_SIZE = UtilProperties.getPropertyAsInteger(SITEMAPCOMMON_RESOURCE, "sitemap.default.indexsize", 50000);
    
    private static final String logPrefix = SitemapWorker.logPrefix;
    
    private static class StaticConfigHolder {
        // TODO?: in future could have DB config
        private static final Map<String, SitemapConfig> staticConfigs = Collections.unmodifiableMap(readStaticConfigsFromProperties());
    }
    
    protected final String urlConfPath;
    protected final String baseUrl;
    protected final String contextPath;
    protected final String sitemapDir;
    protected final String sitemapSubdir;
    protected final String sitemapExtension;
    protected final String sitemapIndexFile;
    protected final String productFilePrefix;
    protected final String categoryFilePrefix;
    protected final Integer sizemapSize;
    protected final Integer indexSize;
    protected final boolean doProduct;
    protected final boolean doCategory;
    // TODO?: REVIEW: I don't see a reason to implement this for sitemaps yet...
    // see SitemapWorker#buildSitemapProduct
    protected final boolean doChildProduct = false;
    
    protected final W3CDateFormat dateFormat;
    protected final String compress;
    
    public SitemapConfig(Map<String, Object> map) {
        this.urlConfPath = asNormString(map.get("urlConfPath"));
        this.baseUrl = asNormString(map.get("baseUrl"));
        this.contextPath = asNormString(map.get("contextPath"));
        this.sitemapDir = asNormString(map.get("sitemapDir"));
        this.sitemapSubdir = asNormString(map.get("sitemapSubDir"));
        this.sitemapExtension = asNormString(map.get("sitemapExtension"));
        this.sitemapIndexFile = asNormString(map.get("sitemapIndexFile"));
        this.productFilePrefix = asNormString(map.get("productFilePrefix"));
        this.categoryFilePrefix = asNormString(map.get("categoryFilePrefix"));
        Integer sizemapSize = asInteger(map.get("sizemapSize"), DEFAULT_SITEMAP_SIZE);
        if (sizemapSize <= 0) sizemapSize = null; // explicit -1 means don't limit
        this.sizemapSize = sizemapSize;
        Integer indexSize = asInteger(map.get("indexSize"), DEFAULT_SITEMAP_SIZE);
        if (indexSize <= 0) indexSize = null; // explicit -1 means don't limit
        this.indexSize = indexSize;
        this.doProduct = asBoolean(map.get("doProduct"), true);
        this.doCategory = asBoolean(map.get("doCategory"), true);
        
        W3CDateFormat dateFormat = new W3CDateFormat(Pattern.DAY); // FIXME: unhardcode
        dateFormat.setTimeZone(TimeZone.getTimeZone(asNormString(map.get("timeZone"), "CET")));
        this.dateFormat = dateFormat;
        
        this.compress = asNormString(map.get("compress"), "gzip");
    }
    
    public static SitemapConfig getSitemapConfigForWebsite(Delegator delegator, LocalDispatcher dispatcher, String webSiteId) {
        return StaticConfigHolder.staticConfigs.get(webSiteId);
    }
    
    public static Map<String, SitemapConfig> getAllSitemapConfigs(Delegator delegator, LocalDispatcher dispatcher) {
        return StaticConfigHolder.staticConfigs;
    }
    
    protected static Map<String, SitemapConfig> readStaticConfigsFromProperties() {
        Map<String, SitemapConfig> configs = new HashMap<>();
        try {
            ClassLoader loader = Thread.currentThread().getContextClassLoader();
            Enumeration<URL> resources = loader.getResources(SITEMAPCONFIGS_RESOURCE + ".properties");
            while (resources.hasMoreElements()) {
                URL propertyURL = resources.nextElement();
                Debug.logInfo(logPrefix+"loading properties: " + propertyURL, module);
                Properties props = UtilProperties.getProperties(propertyURL);
                if (UtilValidate.isEmpty(props)) {
                    Debug.logError(logPrefix+"Unable to locate properties file " + propertyURL, module);
                } else {
                    Map<String, Map<String, Object>> webSiteConfigs = new HashMap<>();
                    UtilProperties.extractPropertiesWithPrefixAndId(webSiteConfigs, props, "sitemap.");
                    for(Map.Entry<String, Map<String, Object>> entry : webSiteConfigs.entrySet()) {
                        Debug.logInfo(logPrefix+"Read config for website '" + entry.getKey() + "': " + entry.getValue().toString(), module);
                        configs.put(entry.getKey(), new SitemapConfig(entry.getValue()));
                    }
                }
            }
            for(Map.Entry<String, SitemapConfig> entry : configs.entrySet()) {
                Debug.logInfo("Seo: Sitemap: Found sitemap config for website: " + entry.getKey(), module);
            }
        } catch (Exception e) {
            Debug.logError(e, "Seo: Sitemap: Could not load list of sitemap.properties", module);
        }
        return configs;
    }
    
    private static String asNormString(Object obj, String defaultValue) {
        if (obj == null) return defaultValue;
        String str = obj.toString().trim();
        return str.isEmpty() ? defaultValue : str;
    }
    
    private static String asNormString(Object obj) {
        return asNormString(obj, null);
    }
    
    private static Boolean asBoolean(Object obj, Boolean defaultValue) {
        return UtilMisc.booleanValueVersatile(obj, defaultValue);
    }
    
    private static Integer asInteger(Object obj, Integer defaultValue) {
        if (obj == null) return defaultValue;
        else if (obj instanceof Integer) return (Integer) obj;
        else if (obj instanceof Long) return ((Long) obj).intValue();
        else if (obj instanceof String) {
            String str = (String) obj;
            if (str.isEmpty()) return defaultValue;
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

    public String getUrlConfPath() {
        return urlConfPath;
    }

    public String getBaseUrl() {
        return baseUrl;
    }
    
    public String getContextPath() {
        return contextPath;
    }

    public String getSitemapDir() {
        return sitemapDir;
    }

    public String getSitemapSubdir() {
        return sitemapSubdir;
    }

    public String getSitemapExtension() {
        return sitemapExtension;
    }

    public String getSitemapIndexFile() {
        return sitemapIndexFile;
    }

    public String getProductFilePrefix() {
        return productFilePrefix;
    }

    public String getCategoryFilePrefix() {
        return categoryFilePrefix;
    }

    public Integer getSizemapSize() {
        return sizemapSize;
    }

    public Integer getIndexSize() {
        return indexSize;
    }

    public boolean isDoProduct() {
        return doProduct;
    }

    public boolean isDoCategory() {
        return doCategory;
    }

    public W3CDateFormat getDateFormat() {
        return dateFormat;
    }

    public boolean isDoChildProduct() {
        return doChildProduct;
    }

    public String getCompress() {
        return compress;
    }
}
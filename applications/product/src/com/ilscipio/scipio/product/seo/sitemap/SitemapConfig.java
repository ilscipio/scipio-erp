package com.ilscipio.scipio.product.seo.sitemap;

import java.io.IOException;
import java.io.Serializable;
import java.net.URL;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.TimeZone;

import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.OfbizUrlBuilder;
import org.ofbiz.webapp.WebAppUtil;
import org.ofbiz.webapp.control.WebAppConfigurationException;
import org.xml.sax.SAXException;

import com.ilscipio.scipio.ce.util.PathUtil;
import com.redfin.sitemapgenerator.W3CDateFormat;

@SuppressWarnings("serial")
public class SitemapConfig implements Serializable {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String SITEMAPCONFIGS_RESOURCE = "sitemaps"; // .properties
    public static final String SITEMAPCOMMON_RESOURCE = "sitemapcommon"; // .properties

    public static final int DEFAULT_SITEMAP_SIZE = UtilProperties.getPropertyAsInteger(SITEMAPCOMMON_RESOURCE, "sitemap.default.sitemapsize", 50000);
    public static final int DEFAULT_INDEX_SIZE = UtilProperties.getPropertyAsInteger(SITEMAPCOMMON_RESOURCE, "sitemap.default.indexsize", 50000);

    private static final String logPrefix = SitemapGenerator.logPrefix;

    private static class StaticConfigHolder {
        // TODO?: in future could have DB config
        private static final Map<String, SitemapConfig> staticConfigs = Collections.unmodifiableMap(readStaticConfigsFromProperties());
    }

    private static final Map<String, W3CDateFormat.Pattern> dateFormatTypeMap;
    static {
        Map<String, W3CDateFormat.Pattern> map = new HashMap<>();
        for(W3CDateFormat.Pattern pattern : W3CDateFormat.Pattern.values()) {
            map.put(pattern.toString(), pattern);
        }
        dateFormatTypeMap = Collections.unmodifiableMap(map);
    }

    private final String webSiteId;

    private final String urlConfPath;
    private final String baseUrl;
    private final boolean baseUrlSecure;
    private final String sitemapWebappPathPrefix;
    private final String sitemapContextPath;
    private final String sitemapDirPath;
    private final String webappPathPrefix;
    private final String contextPath;
    private final String sitemapDir;
    private final String sitemapExtension;
    private final String sitemapIndexFile;
    private final String productFilePrefix;
    private final String categoryFilePrefix;
    private final String contentFilePrefix;
    private final Integer sizemapSize;
    private final Integer indexSize;
    private final boolean doProduct;
    private final boolean doCategory;
    private final boolean doCmsPage;
    // TODO?: REVIEW: I don't see a reason to implement this for sitemaps yet...
    // see SitemapWorker#buildSitemapProduct
    private final boolean doChildProduct = false;

    private final boolean useProductLastModDate;
    // TODO: REVIEW: did not see guarantee that this class is thread-safe
    private final W3CDateFormat dateFormat;

    private final String compress;

    private final List<Locale> locales;

    private final Set<String> prodCatalogIds;
    private final Set<String> prodCatalogCategoryTypeIds;

    private final boolean includeVariant;

    public SitemapConfig(Map<String, Object> map, String webSiteId) {
        this.webSiteId = webSiteId;
        this.urlConfPath = asNormString(map.get("urlConfPath"));
        String baseUrl = asNormString(map.get("baseUrl"));
        if ("none".equalsIgnoreCase(baseUrl)) baseUrl = "";
        this.baseUrl = baseUrl;
        this.baseUrlSecure = asBoolean(map.get("baseUrlSecure"), true);
        this.sitemapWebappPathPrefix = asNormString(map.get("sitemapWebappPathPrefix"));
        this.sitemapContextPath = asNormString(map.get("sitemapContextPath"));
        this.sitemapDirPath = asNormString(map.get("sitemapDirPath"));
        this.webappPathPrefix = asNormString(map.get("webappPathPrefix"));
        this.contextPath = asNormString(map.get("contextPath"));
        this.sitemapDir = asNormString(map.get("sitemapDir"));
        this.sitemapExtension = asNormString(map.get("sitemapExtension"));
        this.sitemapIndexFile = asNormString(map.get("sitemapIndexFile"));
        this.productFilePrefix = asNormString(map.get("productFilePrefix"));
        this.categoryFilePrefix = asNormString(map.get("categoryFilePrefix"));
        this.contentFilePrefix = asNormString(map.get("contentFilePrefix"));
        Integer sizemapSize = asInteger(map.get("sizemapSize"), DEFAULT_SITEMAP_SIZE);
        if (sizemapSize <= 0) sizemapSize = null; // explicit -1 means don't limit
        this.sizemapSize = sizemapSize;
        Integer indexSize = asInteger(map.get("indexSize"), DEFAULT_SITEMAP_SIZE);
        if (indexSize <= 0) indexSize = null; // explicit -1 means don't limit
        this.indexSize = indexSize;
        this.doProduct = asBoolean(map.get("doProduct"), true);
        this.doCategory = asBoolean(map.get("doCategory"), true);
        this.doCmsPage = asBoolean(map.get("doCmsPage"), true);

        this.useProductLastModDate = asBoolean(map.get("useProductLastModDate"), false);
        String dateFormatStr = asNormString(map.get("dateFormat"));
        W3CDateFormat.Pattern pattern = null;
        if (UtilValidate.isNotEmpty(dateFormatStr)) {
            try {
                pattern = W3CDateFormat.Pattern.valueOf(dateFormatStr);
            } catch(Exception e) {
                Debug.logError(logPrefix+"website '" + webSiteId + "' sitemaps.properties configuration error: invalid dateFormat value (" + dateFormatStr + "): "
                        + e.getMessage() + " (supported values: " + dateFormatTypeMap.keySet().toString() + ")", module);
            }
        }
        W3CDateFormat dateFormat = new W3CDateFormat(pattern != null ? pattern : W3CDateFormat.Pattern.AUTO);
        String timeZoneStr = asNormString(map.get("timeZone"));
        if (timeZoneStr != null) {
            try {
                dateFormat.setTimeZone(TimeZone.getTimeZone(timeZoneStr));
            } catch(Exception e) {
                Debug.logError(logPrefix+"website '" + webSiteId + "' sitemaps.properties configuration error: invalid timeZone value (" + timeZoneStr + "): " + e.getMessage(), module);
            }
        }
        this.dateFormat = dateFormat;

        this.compress = asNormString(map.get("compress"), "gzip");

        this.locales = Collections.unmodifiableList(parseLocales(asNormString(map.get("locales"))));

        this.prodCatalogIds = splitTokensToUnmodifiableSetOrNull(asNormString(map.get("prodCatalogIds")));
        this.prodCatalogCategoryTypeIds = splitTokensToUnmodifiableSetOrNull(asNormString(map.get("prodCatalogCategoryTypeIds")));
        this.includeVariant = asBoolean(map.get("includeVariant"), false);
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
                        try {
                            configs.put(entry.getKey(), new SitemapConfig(entry.getValue(), entry.getKey()));
                        } catch(Exception e) {
                            Debug.logError(e, logPrefix+"Unable to read sitemap config for website '" + entry.getKey() + "': " + e.getMessage(), module);
                        }
                    }
                }
            }
            for(Map.Entry<String, SitemapConfig> entry : configs.entrySet()) {
                Debug.logInfo(logPrefix+"Found sitemap config for website: " + entry.getKey(), module);
            }
        } catch (Exception e) {
            Debug.logError(e, logPrefix+"Could not load list of " + SITEMAPCONFIGS_RESOURCE + ".properties", module);
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

    // SIMPLE GETTERS

    public String getWebSiteId() {
        return webSiteId;
    }

    public String getUrlConfPath() {
        return urlConfPath;
    }

    /**
     * NOTE: If null, means was not specified and should use default for website.
     * If empty, means don't append any.
     */
    public String getBaseUrl() {
        return baseUrl;
    }

    public boolean isBaseUrlSecure() {
        return baseUrlSecure;
    }

    public String getSitemapWebappPathPrefix() {
        return sitemapWebappPathPrefix;
    }

    public String getSitemapContextPath() {
        return sitemapContextPath;
    }

    public String getSitemapDirPath() {
        return sitemapDirPath;
    }

    public String getWebappPathPrefix() {
        return webappPathPrefix;
    }

    public String getContextPath() {
        return contextPath;
    }

    public String getSitemapDir() {
        return sitemapDir;
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

    public String getContentFilePrefix() {
        return contentFilePrefix;
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

    public boolean isDoContent() {
        return isDoCmsPage(); // only one for now
    }

    public boolean isDoCmsPage() {
        return doCmsPage;
    }

    public boolean isUseProductLastModDate() {
        return useProductLastModDate;
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

    public boolean isGzip() {
        return "gzip".equals(getCompress());
    }

    public List<Locale> getLocales() {
        return locales;
    }

    public List<Locale> getLocalesOrDefault(GenericValue webSite, GenericValue productStore) {
        return locales.isEmpty() ? UtilMisc.toList(getDefaultLocale(webSite, productStore)) : locales;
    }

    public Locale getDefaultLocale(GenericValue webSite, GenericValue productStore) {
        if (locales.size() > 0) return locales.get(0);
        else if (productStore != null) {
            // TODO?: future? there is no defaultLocaleString on WebSite yet, but it makes so much
            // sense I thought I had already done it!
//            Locale locale = UtilMisc.parseLocale(webSite.getString("defaultLocaleString"));
//            if (locale != null) return locale;
            Locale locale = UtilMisc.parseLocale(productStore.getString("defaultLocaleString"));
            return locale != null ? locale : Locale.getDefault();
        } else {
            return Locale.getDefault();
        }
    }

    /**
     * Allowed prodCatalogIds or null if no filter.
     */
    public Set<String> getProdCatalogIds() {
        return prodCatalogIds;
    }

    /**
     * Allowed prodCatalogCategoryTypeIds or null if no filter.
     */
    public Set<String> getProdCatalogCategoryTypeIds() {
        return prodCatalogCategoryTypeIds;
    }

    /**
     * NOTE: setting true does not automatically include variants that are not directly linked to a category (via ProductCategoryMember).
     * (TODO: REVIEW?)
     */
    public boolean isIncludeVariant() {
        return includeVariant;
    }

    // ADVANCED GETTERS

    public String getSitemapDirUrlLocation(String webappDir) {
        if (sitemapDir != null && sitemapDir.contains("://")) {
            return sitemapDir;
        } else {
            String result = concatPaths(webappDir, sitemapDir);
            if (result.contains("://")) return result;
            else return Paths.get(result).toUri().toString();
        }
    }

    /**
     * Abstraction method, for Sitemap use only.
     */
    public String getDefaultBaseUrl(OfbizUrlBuilder urlBuilder, boolean secure) throws GenericEntityException, WebAppConfigurationException, IOException, SAXException {
        StringBuilder sb = new StringBuilder();
        urlBuilder.buildHostPart(sb, secure);
        return sb.toString();
    }

    /**
     * Abstraction method, for Sitemap use only.
     */
    public String getDefaultWebappPathPrefix(OfbizUrlBuilder urlBuilder) throws GenericEntityException, WebAppConfigurationException, IOException, SAXException {
        StringBuilder sb = new StringBuilder();
        urlBuilder.buildPathPartWithWebappPathPrefix(sb, "");
        PathUtil.removeTrailDelim(sb);
        return sb.toString();
    }

    /**
     * Abstraction method, for Sitemap use only.
     */
    public String getDefaultContextPath(Delegator delegator) throws GenericEntityException, IOException, SAXException {
        WebappInfo webAppInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
        return webAppInfo.getContextRoot();
    }

    /**
     * Concats paths while handling bad input (to an extent).
     * TODO: central util and remove this (WARN: special null/empty check).
     */
    static String concatPaths(String... parts) {
        StringBuilder sb = new StringBuilder();
        for(String part : parts) {
            if (part == null || part.isEmpty()) continue;
            else if (sb.length() == 0) sb.append(part);
            else {
                if (sb.charAt(sb.length() - 1) == '/') {
                    if (part.startsWith("/")) {
                        sb.append(part.substring(1));
                    } else {
                        sb.append(part);
                    }
                } else {
                    if (part.startsWith("/")) {
                        sb.append(part);
                    } else {
                        sb.append('/');
                        sb.append(part);
                    }
                }
            }
        }
        return sb.toString();
    }

    /**
     * TODO: util instead
     */
    static List<Locale> parseLocales(String localesStr) {
        ArrayList<Locale> locales = new ArrayList<>();
        if (UtilValidate.isEmpty(localesStr)) {
            locales.trimToSize();
            return locales;
        }
        Set<String> dupSet = new HashSet<>();
        for(String locStr : localesStr.split("\\s*,\\s*")) {
            Locale locale = UtilMisc.parseLocale(locStr.trim());
            if (locale != null && !dupSet.contains(locale.toString())) {
                locales.add(locale);
                dupSet.add(locale.toString());
            }
        }
        locales.trimToSize();
        return locales;
    }

    static Set<String> splitTokensToSet(String str) {
        Set<String> set = new LinkedHashSet<>();
        if (str != null) {
            for(String token : str.split("\\s*,\\s*")) {
                set.add(token.trim());
            }
        }
        return set;
    }

    static Set<String> splitTokensToUnmodifiableSetOrNull(String str) {
        Set<String> set = splitTokensToSet(str);
        return set.isEmpty() ? null : Collections.unmodifiableSet(set);
    }

}
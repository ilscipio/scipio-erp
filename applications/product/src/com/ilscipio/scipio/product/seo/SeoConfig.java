/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package com.ilscipio.scipio.product.seo;

import java.io.FileInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilURL;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.entity.Delegator;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.ilscipio.scipio.product.seo.UrlProcessors.CharFilter;
import com.ilscipio.scipio.product.seo.UrlProcessors.CharFilterUrlProcessor;
import com.ilscipio.scipio.product.seo.UrlProcessors.StaticMethodUrlProcessor;
import com.ilscipio.scipio.product.seo.UrlProcessors.UrlProcessor;

/**
 * SeoConfig - SEO Configuration file model and utilities.
 * Restructured from the original SeoConfigUtil class so it can be passed to methods and
 * to remove the completely needless sync code.
 */
public class SeoConfig {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String SEO_CONFIG_FILENAME = "SeoConfig.xml";
    public static final String ALLOWED_CONTEXT_PATHS_SEPERATOR = ":";

    /**
     * DEV NOTE: compile with this true to force the SeoConfigs to reload constantly.
     */
    static final boolean DEBUG_FORCERELOAD = false;

    static {
        // TODO?: unhardcode via properties?
        SeoCatalogUrlWorker.registerUrlBuilder();
    }

    /**
     * SCIPIO: default/common configs holder - eliminates the need for synchronized/volatile/etc. - faster.
     */
    private static class CommonSeoConfigs {
        private static final SeoConfig INSTANCE = readDefaultCommonConfig();
        private static final SeoConfig EMPTY = new SeoConfig();
    }

    private final String webSiteId; // NOTE: null for common/default instance
    private final boolean isInitialed;
    private final boolean seoUrlEnabled;
    private final boolean categoryNameEnabled;
    private final String seoUrlSuffix;
    private final Pattern regexpIfMatch;
    private final boolean useUrlRegexp;
    private final Map<String, String> seoReplacements;
    private final Map<String, Pattern> seoPatterns;
    private final Map<String, String> forwardReplacements;
    private final Map<String, Integer> forwardResponseCodes;
    private final Map<String, UrlProcessors> urlProcessors;
    private final UrlProcessors altUrlGenProcessors;
    private final Map<String, List<CharFilter>> charFiltersMap;
    private final List<Pattern> userExceptionPatterns;
    private final Set<String> allowedContextPaths;
    private final Map<String, String> specialProductIds;

    // SCIPIO: new additions
    private final Integer categoryNameMaxLength;
    private final Integer productNameMaxLength;
    private final Locale namesLocaleOverride;
    private final boolean categoryNameSeparatePathElem;
    private final boolean categoryNameAppendId;
    private final boolean categoryNameAppendIdLast; // SCIPIO: new 2017
    private final boolean productNameAppendId; // SCIPIO: new 2017
    private final boolean handleImplicitRequests;
    private final boolean implicitRequestNameMatchesOnly;
    private final boolean generateImplicitCategoryUrl;
    private final boolean generateImplicitProductUrl;
    private final boolean generateImplicitProductUrlNoCat;
    private final boolean allowTargetOutsideCatalog;
    private final boolean allowInvalidCategoryPathElements;
    private final boolean generateProductAltUrlSuffix;
    private final boolean generateCategoryAltUrlSuffix;
    private final boolean handleAltUrlSuffix;
    private final String productAltUrlSuffix;
    private final String categoryAltUrlSuffix;
    private final ClosestTrailResolver.ResolverType newUrlTrailResolver;

    private final int defaultResponseCode;
    private final int defaultRedirectResponseCode;

    /**
     * SCIPIO: Returns the default common config.
     */
    public static SeoConfig getCommonConfig() {
        if (DEBUG_FORCERELOAD) return readDefaultCommonConfig();
        else return CommonSeoConfigs.INSTANCE;
    }

    /**
     * SCIPIO: Returns default empty constructor config, in other words static/hardcoded defaults;
     * no file is read.
     */
    public static SeoConfig getDefaultEmptyConfig() {
        return CommonSeoConfigs.EMPTY;
    }

    /**
     * SCIPIO: Returns config for website.
     * TODO: currently this always returns default config.
     */
    public static SeoConfig getConfig(Delegator delegator, String webSiteId) {
        // TODO?: possible website-specific stuff in future
        return getCommonConfig();
    }

    /**
     * SCIPIO: Force create config - for debugging only!
     */
    public static SeoConfig createConfig(Delegator delegator, String webSiteId) {
        // TODO?: possible website-specific stuff in future
        return readDefaultCommonConfig();
    }

    protected SeoConfig() {
        this(null, null, null, null, true);
    }

    protected SeoConfig(String webSiteId, boolean isInitialed) {
        this(webSiteId, null, null, null, isInitialed);
    }

    public SeoConfig(String webSiteId, Element rootElement, Document configDoc) {
        this(webSiteId, rootElement, configDoc, "document", true);
    }

    public SeoConfig(String webSiteId, Element rootElement, Document configDoc, String srcInfo) {
        this(webSiteId, rootElement, configDoc, srcInfo, true);
    }

    /**
     * Complete XML core constructor.
     * <p>
     * if rootElement==null or isInitialed==false, skips construction and builds default instance instead;
     * to avoid a bunch of copy paste in the other constructor.
     */
    protected SeoConfig(String webSiteId, Element rootElem, Document configDoc, String srcInfo, boolean isInitialed) {
        this.webSiteId = webSiteId; // may be null for common/default

        // DEFAULT VALUES
        boolean seoUrlEnabled = true;
        boolean categoryNameEnabled = false;
        String seoUrlSuffix = null;
        Pattern regexpIfMatch = null;
        boolean useUrlRegexp = false;
        Map<String, String> seoReplacements = new HashMap<>();
        Map<String, Pattern> seoPatterns = new HashMap<>();
        Map<String, String> forwardReplacements = new HashMap<>();
        Map<String, Integer> forwardResponseCodes = new HashMap<>();
        Map<String, UrlProcessors> urlProcessors = new HashMap<>();
        UrlProcessors altUrlGenProcessors = UrlProcessors.getDummyProcessors();
        Map<String, List<CharFilter>> charFiltersMap = new HashMap<>();
        List<Pattern> userExceptionPatterns = new ArrayList<>();
        Set<String> allowedContextPaths = null;
        Map<String, String> specialProductIds = new HashMap<>();

        // SCIPIO: new additions
        Integer categoryNameMaxLength = null;
        Integer productNameMaxLength = null;
        Locale namesLocaleOverride = null;

        boolean categoryNameSeparatePathElem = false;
        boolean categoryNameAppendId = false;
        boolean categoryNameAppendIdLast = false;
        boolean productNameAppendId = false; // SCIPIO: new 2017
        boolean handleImplicitRequests = false;
        boolean implicitRequestNameMatchesOnly = true; // default true => safer
        boolean generateImplicitCategoryUrl = false;
        boolean generateImplicitProductUrl = false;
        boolean generateImplicitProductUrlNoCat = false;

        boolean allowTargetOutsideCatalog = true;
        boolean allowInvalidCategoryPathElements = true;
        boolean generateProductAltUrlSuffix = false;
        boolean generateCategoryAltUrlSuffix = false;
        boolean handleAltUrlSuffix = true;
        String productAltUrlSuffix = "-p";
        String categoryAltUrlSuffix = "-c";
        ClosestTrailResolver.ResolverType newUrlTrailResolver = ClosestTrailResolver.ResolverType.NONCONSEC_ANYPART;

        int defaultResponseCode = HttpServletResponse.SC_MOVED_PERMANENTLY;
        int defaultRedirectResponseCode = HttpServletResponse.SC_MOVED_PERMANENTLY;

        if (rootElem != null && isInitialed) {

            String regexIfMatch = UtilXml.childElementValue(rootElem, "regexpifmatch", "^.*/.*$");
            Debug.logInfo("Parsing regexpifmatch: " + regexIfMatch, module);
            try {
                regexpIfMatch = Pattern.compile(regexIfMatch);
            } catch (Exception e) {
                Debug.logWarning(e, "Error while parsing: " + regexIfMatch, module);
            }

            // parse category-url element
            try {
                Element catUrlElem = UtilXml.firstChildElement(rootElem, "seo-url");
                Debug.logInfo("Parsing seo-url [" + (catUrlElem != null) + "]:", module);
                if (catUrlElem != null) {
                    seoUrlEnabled = booleanSetting(catUrlElem, "state", true);
                    if (seoUrlEnabled) {
                        String allowedContextValue = UtilXml.childElementValue(catUrlElem, "allowed-context-paths", null);
                        if ("*".equals(allowedContextValue) || "/*".equals(allowedContextValue)) {
                            allowedContextPaths = null; // no restrict
                        } else {
                            allowedContextPaths = new HashSet<>();
                            if (UtilValidate.isNotEmpty(allowedContextValue)) {
                                List<String> allowedContextPathList = StringUtil.split(allowedContextValue, ALLOWED_CONTEXT_PATHS_SEPERATOR);
                                for (String path : allowedContextPathList) {
                                    if (UtilValidate.isNotEmpty(path)) {
                                        path = path.trim();
                                        if (!allowedContextPaths.contains(path)) {
                                            allowedContextPaths.add(path);
                                            Debug.logInfo("  allowed-context-paths: " + path, module);
                                        }
                                    }
                                }
                            }
                        }

                        categoryNameEnabled = booleanSetting(catUrlElem, "category-name", categoryNameEnabled);

                        seoUrlSuffix = UtilXml.childElementValue(catUrlElem, "category-url-suffix", null);
                        if (UtilValidate.isNotEmpty(seoUrlSuffix)) {
                            seoUrlSuffix = seoUrlSuffix.trim();
                            if (seoUrlSuffix.contains("/")) {
                                seoUrlSuffix = null;
                            }
                        }
                        Debug.logInfo("  category-url-suffix: " + seoUrlSuffix, module);

                        categoryNameMaxLength = integerSetting(catUrlElem, "category-name-max-length", null, false);
                        productNameMaxLength = integerSetting(catUrlElem, "product-name-max-length", null, false);
                        namesLocaleOverride = localeSetting(catUrlElem, "names-locale-override", null);

                        categoryNameSeparatePathElem = booleanSetting(catUrlElem, "category-name-separate-path-elem", categoryNameSeparatePathElem);
                        categoryNameAppendId = booleanSetting(catUrlElem, "category-name-append-id", categoryNameAppendId);
                        categoryNameAppendIdLast = booleanSetting(catUrlElem, "category-name-append-id-last", categoryNameAppendIdLast);
                        productNameAppendId = booleanSetting(catUrlElem, "product-name-append-id", productNameAppendId); // SCIPIO: 2017: new
                        handleImplicitRequests = booleanSetting(catUrlElem, "handle-implicit-requests", handleImplicitRequests);
                        implicitRequestNameMatchesOnly = booleanSetting(catUrlElem, "implicit-request-name-matches-only", implicitRequestNameMatchesOnly);
                        generateImplicitCategoryUrl = booleanSetting(catUrlElem, "generate-implicit-category-url", generateImplicitCategoryUrl);
                        generateImplicitProductUrl = booleanSetting(catUrlElem, "generate-implicit-product-url", generateImplicitProductUrl);
                        generateImplicitProductUrlNoCat = booleanSetting(catUrlElem, "generate-implicit-product-url-nocat", generateImplicitProductUrlNoCat);

                        allowTargetOutsideCatalog = booleanSetting(catUrlElem, "allow-target-outside-catalog", allowTargetOutsideCatalog);
                        allowInvalidCategoryPathElements = booleanSetting(catUrlElem, "allow-invalid-category-path-elems", allowInvalidCategoryPathElements);
                        generateProductAltUrlSuffix = booleanSetting(catUrlElem, "generate-product-alt-url-suffix", generateProductAltUrlSuffix);
                        generateCategoryAltUrlSuffix = booleanSetting(catUrlElem, "generate-category-alt-url-suffix", generateCategoryAltUrlSuffix);
                        handleAltUrlSuffix = booleanSetting(catUrlElem, "handle-alt-url-suffix", handleAltUrlSuffix);
                        productAltUrlSuffix = stringSetting(catUrlElem, "product-alt-url-suffix", productAltUrlSuffix, falseBooleanSettingValues());
                        categoryAltUrlSuffix = stringSetting(catUrlElem, "category-alt-url-suffix", categoryAltUrlSuffix, falseBooleanSettingValues());

                        String newUrlTrailResolverName = stringSetting(catUrlElem, "new-url-trail-resolver", newUrlTrailResolver.getName());
                        newUrlTrailResolver = ClosestTrailResolver.ResolverType.fromNameOrDefault(newUrlTrailResolverName, newUrlTrailResolver);
                    }
                }
            } catch (NullPointerException e) {
                // no "category-url" element
                Debug.logWarning("No category-url element found in " + srcInfo, module);
            }

            // parse url-config(s) elements
            try {
                NodeList urlConfigsElems = rootElem.getElementsByTagName("url-configs");
                Debug.logInfo("Parsing url-configs", module);
                for (int k = 0; k < urlConfigsElems.getLength(); k++) {
                    Element urlConfigsElem = (Element) urlConfigsElems.item(k);

                    if (k == 0) {
                        defaultResponseCode = integerSetting(urlConfigsElem, "default-response-code", defaultResponseCode, false);
                        defaultRedirectResponseCode = integerSetting(urlConfigsElem, "default-redirect-response-code", defaultRedirectResponseCode, false);
                    }

                    NodeList configs = urlConfigsElem.getElementsByTagName("url-config");
                    Debug.logInfo("Parsing url-config", module);
                    for (int j = 0; j < configs.getLength(); j++) {
                        Element config = (Element) configs.item(j);
                        String urlpattern = UtilXml.childElementValue(config, "url-pattern", null);
                        if (UtilValidate.isEmpty(urlpattern)) {
                            continue;
                        }
                        Debug.logInfo("  url-pattern: " + urlpattern, module);
                        Pattern pattern;
                        try {
                            pattern = Pattern.compile(urlpattern);
                            seoPatterns.put(urlpattern, pattern);
                        } catch (Exception e) {
                            Debug.logWarning("Error while creating parttern for seo url-pattern: " + urlpattern, module);
                            continue;
                        }

                        // construct seo patterns
                        Element seo = UtilXml.firstChildElement(config, "seo");
                        if (UtilValidate.isNotEmpty(seo)) {
                            String replacement = UtilXml.childElementValue(seo, "replacement", null);
                            if (UtilValidate.isNotEmpty(replacement)) {
                                seoReplacements.put(urlpattern, replacement);
                                Debug.logInfo("    seo replacement: " + replacement, module);
                            }
                        }

                        // construct forward patterns
                        Element forward = UtilXml.firstChildElement(config, "forward");
                        if (UtilValidate.isNotEmpty(forward)) {
                            String replacement = UtilXml.childElementValue(forward, "replacement", null);
                            String responseCode = UtilXml.childElementValue(forward,
                                    "responsecode", String.valueOf(defaultResponseCode));
                            if (UtilValidate.isNotEmpty(replacement)) {
                                forwardReplacements.put(urlpattern, replacement);
                                Debug.logInfo("    forward replacement: " + replacement, module);
                                if (UtilValidate.isNotEmpty(responseCode)) {
                                    Integer responseCodeInt = defaultResponseCode;
                                    try {
                                        responseCodeInt = Integer.valueOf(responseCode);
                                    } catch (NumberFormatException nfe) {
                                        Debug.logWarning(nfe, "Error while parsing response code number: " + responseCode, module);
                                    }
                                    forwardResponseCodes.put(urlpattern, responseCodeInt);
                                    Debug.logInfo("    forward responsecode: " + responseCodeInt, module);
                                }
                            }
                        }
                    }
                }
            } catch (NullPointerException e) {
                Debug.logWarning("No url-config(s) element found in " + srcInfo, module);
            }

            // parse char-filters elements
            try {
                NodeList charFiltersNodes = rootElem.getElementsByTagName("char-filters");
                for (int j = 0; j < charFiltersNodes.getLength(); j++) {
                    Element parentNode = (Element) charFiltersNodes.item(j);
                    String charFiltersName = parentNode.getAttribute("name");
                    Debug.logInfo("Parsing " + "char-filters" + " set '" + charFiltersName + "': ", module);

                    List<CharFilter> charFilters = new ArrayList<>();

                    NodeList nameFilterNodes = parentNode.getElementsByTagName("char-filter");
                    for (int i = 0; i < nameFilterNodes.getLength(); i++) {
                        Element element = (Element) nameFilterNodes.item(i);
                        String charaterPattern = UtilXml.childElementValue(element, "character-pattern", null);
                        String replacement = UtilXml.childElementValue(element, "replacement", null);
                        if (UtilValidate.isNotEmpty(charaterPattern) && UtilValidate.isNotEmpty(replacement)) {
                            try {
                                charFilters.add(CharFilter.compile(charaterPattern, replacement));
                                Debug.logInfo("  character-pattern: " + charaterPattern, module);
                                Debug.logInfo("  replacement: " + replacement, module);
                            } catch (Exception e) {
                                // skip this filter (character-pattern replacement) if any error happened
                                Debug.logError(e, "Error parsing character-pattern: " + charaterPattern + ": " + e.getMessage(), module);
                            }
                        } else {
                            Debug.logError("Missing character-pattern or replacement in char-filter element", module);
                        }
                    }
                    charFiltersMap.put(charFiltersName, charFilters);
                }
            } catch (NullPointerException e) {
                Debug.logInfo("No char-filter element found in " + srcInfo, module);
            }

            try {
                NodeList urlProcessorsNodes = rootElem.getElementsByTagName("url-processors");
                for (int j = 0; j < urlProcessorsNodes.getLength(); j++) {
                    Element parentNode = (Element) urlProcessorsNodes.item(j);
                    String urlProcessorsType = parentNode.getAttribute("type");
                    Debug.logInfo("Parsing url-processors set '" + urlProcessorsType + "': ", module);

                    List<UrlProcessor> processorList = new ArrayList<>();

                    NodeList processorNodes = parentNode.getElementsByTagName("processor");
                    for (int i = 0; i < processorNodes.getLength(); i++) {
                        Element element = (Element) processorNodes.item(i);

                        String type = element.getAttribute("type");
                        String value = UtilXml.elementValue(element);

                        if (UtilValidate.isNotEmpty(type) && UtilValidate.isNotEmpty(value)) {
                            Debug.logInfo("  processor type '" + type + "': " + value, module);
                            try {
                                UrlProcessor processor;
                                if ("static-method".equals(type)) {
                                    processor = StaticMethodUrlProcessor.getProcessor(value);
                                } else if ("char-filters".equals(type)) {
                                    List<CharFilter> charFilters = charFiltersMap.get(value);
                                    if (charFilters == null) {
                                        throw new IllegalArgumentException("unknown char-filters set name: " + value);
                                    }
                                    processor = CharFilterUrlProcessor.getProcessor(charFilters);
                                } else {
                                    throw new UnsupportedOperationException("unknown processor type: " + type);
                                }
                                if (processor != null) {
                                    processorList.add(processor);
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error parsing processor: " + value + ": " + e.getMessage(), module);
                            }
                        } else {
                            Debug.logError("Missing type attribute or value in processor element", module);
                        }
                    }
                    urlProcessors.put(urlProcessorsType, UrlProcessors.createProcessors(processorList));
                }
            } catch (NullPointerException e) {
                Debug.logWarning("No url-processors element found in " + srcInfo, module);
            }

            // required
            altUrlGenProcessors = urlProcessors.get("alt-url-gen");
            if (altUrlGenProcessors == null) {
                altUrlGenProcessors = UrlProcessors.getDummyProcessors();
                urlProcessors.put("alt-url-gen", altUrlGenProcessors);
            }

            if (seoReplacements.keySet().isEmpty()) {
                useUrlRegexp = false;
            } else {
                useUrlRegexp = true;
            }
        }

        this.seoUrlEnabled = seoUrlEnabled;
        this.categoryNameEnabled = categoryNameEnabled;
        this.seoUrlSuffix = seoUrlSuffix;
        this.regexpIfMatch = regexpIfMatch;
        this.useUrlRegexp = useUrlRegexp;
        this.seoReplacements = seoReplacements;
        this.seoPatterns = seoPatterns;
        this.forwardReplacements = forwardReplacements;
        this.forwardResponseCodes = forwardResponseCodes;
        this.charFiltersMap = charFiltersMap;
        this.urlProcessors = urlProcessors;
        this.altUrlGenProcessors = altUrlGenProcessors;
        this.userExceptionPatterns = userExceptionPatterns;
        this.allowedContextPaths = allowedContextPaths;
        this.specialProductIds = specialProductIds;

        this.categoryNameMaxLength = categoryNameMaxLength;
        this.productNameMaxLength = productNameMaxLength;
        this.namesLocaleOverride = namesLocaleOverride;
        this.categoryNameSeparatePathElem = categoryNameSeparatePathElem;
        this.categoryNameAppendId = categoryNameAppendId;
        this.categoryNameAppendIdLast = categoryNameAppendIdLast;
        this.productNameAppendId = productNameAppendId;
        this.handleImplicitRequests = handleImplicitRequests;
        this.implicitRequestNameMatchesOnly = implicitRequestNameMatchesOnly;
        this.generateImplicitCategoryUrl = generateImplicitCategoryUrl;
        this.generateImplicitProductUrl = generateImplicitProductUrl;
        this.generateImplicitProductUrlNoCat = generateImplicitProductUrlNoCat;

        this.allowTargetOutsideCatalog = allowTargetOutsideCatalog;
        this.allowInvalidCategoryPathElements = allowInvalidCategoryPathElements;
        this.generateProductAltUrlSuffix = generateProductAltUrlSuffix;
        this.generateCategoryAltUrlSuffix = generateCategoryAltUrlSuffix;
        this.handleAltUrlSuffix = handleAltUrlSuffix;
        this.productAltUrlSuffix = productAltUrlSuffix;
        this.categoryAltUrlSuffix = categoryAltUrlSuffix;
        this.newUrlTrailResolver = newUrlTrailResolver;

        this.defaultResponseCode = defaultResponseCode;
        this.defaultRedirectResponseCode = defaultRedirectResponseCode;

        this.isInitialed = isInitialed;
    }

    private static SeoConfig readDefaultCommonConfig() {
        FileInputStream configFileIS = null;
        SeoConfig config = null;
        try {
            URL seoConfigFilename = UtilURL.fromResource(SEO_CONFIG_FILENAME);
            Document configDoc = UtilXml.readXmlDocument(seoConfigFilename, false);
            Element rootElement = configDoc.getDocumentElement();

            config = new SeoConfig(null, rootElement, configDoc, seoConfigFilename.toString());
        } catch (SAXException e) {
            Debug.logError(e, module);
        } catch (ParserConfigurationException e) {
            Debug.logError(e, module);
        } catch (IOException e) {
            Debug.logError(e, module);
        } finally {
            if (configFileIS != null) {
                try {
                    configFileIS.close();
                } catch (IOException e) {
                    Debug.logError(e, module);
                }
            }
        }
        return config != null ? config : new SeoConfig(null, false);
    }

    /**
     * webSiteId for this config, or null if this is the common/default/central config.
     */
    public String getWebSiteId() {
        return webSiteId;
    }

    /**
     * Check whether the configuration file has been read successfully.
     *
     * @return a boolean value to indicate whether the configuration file has been read.
     */
    public boolean isInitialed() {
        return isInitialed;
    }

    /**
     * Check whether url regexp should be used.
     *
     * @return a boolean value to indicate whether url regexp should be used.
     */
    public boolean checkUseUrlRegexp() {
        return useUrlRegexp;
    }

    /**
     * Get the general regexp pattern.
     *
     * @return the general regexp pattern.
     */
    public Pattern getGeneralRegexpPattern() {
        return regexpIfMatch;
    }

    /**
     * Check whether category url is enabled.
     */
    public boolean isSeoUrlEnabledStatic() {
        return seoUrlEnabled;
    }

    /**
     * SCIPIO: 2017: check if category URL enabled for context path and webSiteId.
     */
    public boolean isSeoUrlEnabled(String contextPath, String webSiteId) {
        return seoUrlEnabled &&
            (WebsiteSeoConfig.isSeoEnabled(webSiteId)) &&
            (allowedContextPaths == null || allowedContextPaths.contains(normContextPath(contextPath)));
    }

    /**
     * SCIPIO: 2017: check if category URL enabled for webSiteId.
     */
    public boolean isSeoUrlEnabledForWebsite(String webSiteId) {
        return seoUrlEnabled && WebsiteSeoConfig.isSeoEnabled(webSiteId);
    }

    /**
     * Check whether the context path is enabled.
     */
    public boolean isSeoUrlEnabledForContextPath(String contextPath) {
        return seoUrlEnabled && (allowedContextPaths == null || allowedContextPaths.contains(normContextPath(contextPath)));
    }

    private String normContextPath(String contextPath) {
        if (contextPath == null) return null;
        if (UtilValidate.isEmpty(contextPath)) {
            contextPath = "/";
        }
        return contextPath.trim();
    }

    /**
     * Check whether category name/trail is enabled for products.
     *
     * @return a boolean value to indicate whether category name is enabled.
     */
    public boolean isCategoryNameEnabled() {
        return categoryNameEnabled;
    }

    /**
     * Get category url suffix.
     *
     * @return String category url suffix.
     */
    public String getSeoUrlSuffix() {
        return seoUrlSuffix;
    }

    /**
     * Get user exception url pattern configures.
     * @deprecated 2017-11-18: TODO: REVIEW: any further use for this? (un-deprecate if one found)
     * @return user exception url pattern configures (java.util.List<Pattern>)
     */
    @Deprecated
    public List<Pattern> getUserExceptionPatterns() {
        return userExceptionPatterns;
    }

    /**
     * Returns the url processors by processors type.
     */
    public UrlProcessors getUrlProcessors(String type) {
        return urlProcessors.get(type);
    }

    /**
     * Returns the processors for "alt-url-gen" type, in other
     * words the processors for the generation of SEO alternative
     * URLS headed into the DB.
     */
    public UrlProcessors getAltUrlGenProcessors() {
        return altUrlGenProcessors;
    }

    /**
     * Get the named set of char filters, as list.
     *
     * @return char filters
     */
    public List<CharFilter> getCharFilters(String charFiltersSetName) {
        return charFiltersMap.get(charFiltersSetName);
    }

    /**
     * Get seo url pattern configures.
     * @deprecated 2017-11-18: TODO: REVIEW: any further use for this? (un-deprecate if one found)
     * @return seo url pattern configures (java.util.Map<String, Pattern>)
     */
    @Deprecated
    public Map<String, Pattern> getSeoPatterns() {
        return seoPatterns;
    }

    /**
     * Get seo replacement configures.
     * @deprecated 2017-11-18: TODO: REVIEW: any further use for this? (un-deprecate if one found)
     * @return seo replacement configures (java.util.Map<String, String>)
     */
    @Deprecated
    public Map<String, String> getSeoReplacements() {
        return seoReplacements;
    }

    /**
     * Get forward replacement configures.
     * @deprecated 2017-11-18: TODO: REVIEW: any further use for this? (un-deprecate if one found)
     * @return forward replacement configures (java.util.Map<String, String>)
     */
    @Deprecated
    public Map<String, String> getForwardReplacements() {
        return forwardReplacements;
    }

    /**
     * Get forward response codes.
     * @deprecated 2017-11-18: TODO: REVIEW: any further use for this? (un-deprecate if one found)
     * @return forward response code configures (java.util.Map<String, Integer>)
     */
    @Deprecated
    public Map<String, Integer> getForwardResponseCodes() {
        return forwardResponseCodes;
    }

    /**
     * Check whether a product id is in the special list. If we cannot get a product from a lower cased
     * or upper cased product id, then it's special.
     * @deprecated 2017: always returns false
     *
     * @return boolean to indicate whether the product id is special.
     */
    @Deprecated
    public boolean isSpecialProductId(String productId) {
        return specialProductIds.containsKey(productId);
    }

    /**
     * Add a special product id to the special list.
     * @deprecated SCIPIO: 2017: now throws UnsupportedOperationException - the config should not be modifiable publicly
     * anymore at this time.
     *
     * @param productId a product id get from database.
     * @return true to indicate it has been added to special product id; false to indicate it's not special.
     * @throws Exception to indicate there's already same lower cased product id in the list but value is a different product id.
     */
    @Deprecated
    public boolean addSpecialProductId(String productId) throws Exception {
        throw new UnsupportedOperationException();
//        if (productId.toLowerCase().equals(productId) || productId.toUpperCase().equals(productId)) {
//            return false;
//        }
//        if (isSpecialProductId(productId.toLowerCase())) {
//            if (specialProductIds.containsValue(productId)) {
//                return true;
//            } else {
//                throw new Exception("This product Id cannot be lower cased for SEO URL purpose: " + productId);
//            }
//        }
//        specialProductIds.put(productId.toLowerCase(), productId);
//        return true;
    }

    /**
     * Get a product id is in the special list.
     * @deprecated 2017: always returns false
     * @return String of the original product id
     */
    @Deprecated
    public String getSpecialProductId(String productId) {
        return specialProductIds.get(productId);
    }

    public Integer getCategoryNameMaxLength() {
        return categoryNameMaxLength;
    }

    public String limitCategoryNameLength(String categoryName) {
        if (categoryNameMaxLength != null && categoryName != null &&
                categoryName.length() > categoryNameMaxLength) {
            return categoryName.substring(0, categoryNameMaxLength);
        }
        return categoryName;
    }

    public Integer getProductNameMaxLength() {
        return productNameMaxLength;
    }

    public String limitProductNameLength(String productName) {
        if (productNameMaxLength != null && productName != null &&
                productName.length() > productNameMaxLength) {
            return productName.substring(0, productNameMaxLength);
        }
        return productName;
    }

    /**
     * @deprecated should not be needed anymore (2017-11-18); the generated
     * ALTERNATIVE_URL locales simply copy over the locales from the PRODUCT_NAME
     * and CATEGORY_NAME localized simple text content.
     * <p>
     * NOTE: sitemaps.properties has its own locale override.
     */
    @Deprecated
    public Locale getNamesLocaleOverride() {
        return namesLocaleOverride;
    }

    public boolean isCategoryNameSeparatePathElem() {
        return categoryNameSeparatePathElem;
    }

    public boolean isCategoryNameAppendId() {
        return categoryNameAppendId;
    }

    public boolean isCategoryNameAppendIdLast() {
        return categoryNameAppendIdLast;
    }

    public boolean isProductNameAppendId() {
        return productNameAppendId;
    }

    public boolean isHandleImplicitRequests() {
        return handleImplicitRequests;
    }

    public boolean isImplicitRequestNameMatchesOnly() {
        return implicitRequestNameMatchesOnly;
    }

    public boolean isGenerateImplicitCategoryUrl() {
        return generateImplicitCategoryUrl;
    }

    public boolean isGenerateImplicitProductUrl() {
        return generateImplicitProductUrl;
    }

    public boolean isGenerateImplicitProductUrlNoCat() {
        return generateImplicitProductUrlNoCat;
    }

    /**
     * If true, the target product/category will set the productId/productCategoryId
     * in request even if it's outside the catalog.
     * Can leave to true as long as screens perform the checks properly.
     */
    public boolean isAllowTargetOutsideCatalog() {
        return allowTargetOutsideCatalog;
    }

    /**
     * If false, a valid target product/category but with an invalid category path
     * will not register as a valid link; if true it will be mapped using a default
     * trail.
     * Almost always better if left true.
     */
    public boolean isAllowInvalidCategoryPathElements() {
        return allowInvalidCategoryPathElements;
    }

    public boolean isGenerateProductAltUrlSuffix() {
        return generateProductAltUrlSuffix && productAltUrlSuffix != null;
    }

    public boolean isGenerateCategoryAltUrlSuffix() {
        return generateCategoryAltUrlSuffix && categoryAltUrlSuffix != null;
    }

    /**
     * If true, the filter will accept suffixed alt-url generated links;
     * implemented for backward compatibility, but can be used for other reasons.
     */
    public boolean isHandleAltUrlSuffix() {
        return handleAltUrlSuffix;
    }

    public boolean isHandleProductAltUrlSuffix() {
        return handleAltUrlSuffix && productAltUrlSuffix != null;
    }

    public boolean isHandleCategoryAltUrlSuffix() {
        return handleAltUrlSuffix && categoryAltUrlSuffix != null;
    }

    /**
     * The alt url product suffix, legacy "-p".
     */
    public String getProductAltUrlSuffix() {
        return productAltUrlSuffix;
    }

    /**
     * The alt url category suffix, legacy "-c".
     */
    public String getCategoryAltUrlSuffix() {
        return categoryAltUrlSuffix;
    }

    /**
     * When generating links, the algorithm to use to pick best category rollup trail/path from the hint/breadcrumb trail.
     */
    public ClosestTrailResolver.ResolverType getNewUrlTrailResolver() {
        return newUrlTrailResolver;
    }

    /**
     * Default preferred response code for url-configs and things that resemble url-configs.
     * NOTE: avoid this from external - ambiguous - probably want {@link #getDefaultRedirectResponseCode()} instead.
     */
    public int getDefaultResponseCode() {
        return defaultResponseCode;
    }

    /**
     * Default redirect response, usually 301 for SEO URLs.
     */
    public int getDefaultRedirectResponseCode() {
        return defaultRedirectResponseCode;
    }

    // HELPERS

    static String stringSetting(Element parentElement, String settingName, String defaultValue, Set<String> specialNullValues) {
        String value = UtilXml.childElementValue(parentElement, settingName, defaultValue);
        if (value == null || value.isEmpty()) value = defaultValue;
        Debug.logInfo("  " + settingName + ": " + value, module);
        if (specialNullValues.contains(value)) return null;
        return value;
    }

    static String stringSetting(Element parentElement, String settingName, String defaultValue) {
        return stringSetting(parentElement, settingName, defaultValue, Collections.<String>emptySet());
    }

    private static final Set<String> falseBooleanSettingValues = UtilMisc.unmodifiableHashSet(
            "false", "N", "disable", "disabled"
    );

    static Set<String> falseBooleanSettingValues() {
        return falseBooleanSettingValues;
    }

    /**
     * SCIPIO: Boolean check for enable/disable features, supports old and new standard values.
     */
    static Boolean booleanSetting(String value, Boolean defaultValue) {
        if ("enable".equals(value) || "enabled".equals(value)) return true;
        else if ("disable".equals(value) || "disabled".equals(value)) return false;
        else {
            Boolean result = UtilMisc.booleanValueVersatile(value);
            if (result != null) return result;
            return defaultValue;
        }
    }

    static Boolean booleanSetting(String value) {
        return booleanSetting(value, (Boolean) null);
    }

    static Boolean booleanSetting(Element parentElement, String settingName, Boolean defaultValue) {
        String strValue = UtilXml.childElementValue(parentElement, settingName, (defaultValue != null) ? defaultValue.toString() : null);
        Boolean value = booleanSetting(strValue);
        if (value == null && strValue != null && strValue.length() > 0) {
            Debug.logError("Invalid value for '" + settingName + "' setting: " + strValue + " (should be 'true'/'false' or 'enable'/'disable')", module);
        }
        if (value == null) value = defaultValue;
        Debug.logInfo("  " + settingName + ": " + value, module);
        return value;
    }


    static Integer integerSetting(Element parentElement, String settingName, Integer defaultValue, boolean allowNegative) {
        Integer value = null;
        String strValue = UtilXml.childElementValue(parentElement, settingName, defaultValue != null ? defaultValue.toString() : null);
        if (UtilValidate.isNotEmpty(strValue)) {
            try {
                value = Integer.parseInt(strValue);
                if (!allowNegative && value < 0) {
                    value = (defaultValue != null) ? defaultValue : null;
                }
            } catch(Exception e) {
                Debug.logError(e, "Couldn't parse " + settingName + ": " + e.getMessage(), module);
            }
        }
        Debug.logInfo("  " + settingName + ": " + value, module);
        return value;
    }

    static Locale localeSetting(Element parentElement, String settingName, String defaultValue) {
        Locale value = null;
        String strValue = UtilXml.childElementValue(parentElement, settingName, defaultValue);
        if (UtilValidate.isNotEmpty(strValue)) {
            value = UtilMisc.parseLocale(strValue);
        }
        if (value == null && defaultValue != null) value = UtilMisc.parseLocale(defaultValue);
        Debug.logInfo("  " + settingName + ": " + value, module);
        return value;
    }
}

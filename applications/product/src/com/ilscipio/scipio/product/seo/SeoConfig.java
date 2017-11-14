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
import org.ofbiz.base.util.UtilURL;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.entity.Delegator;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

/**
 * SeoConfig - SEO Configuration file model and utilities.
 * Restructured from the original SeoConfigUtil class so it can be passed to methods and
 * to remove the completely needless sync code.
 */
public class SeoConfig {
    private static final String module = SeoConfig.class.getName();
    
    static {
        // TODO?: unhardcode via properties?
        SeoCatalogUrlWorker.registerUrlBuilder();
    }
    
    /**
     * SCIPIO: default config holder - eliminates the need for synchronized/volatile/etc. - faster.
     */
    private static class SeoConfigDefault {
        private static final SeoConfig INSTANCE = readDefaultConfig();
    }
    
    private final boolean isInitialed;
    private final boolean categoryUrlEnabled;
    private final boolean categoryNameEnabled;
    private final String categoryUrlSuffix;
    private final Pattern regexpIfMatch;
    private final boolean useUrlRegexp;
    private final boolean jSessionIdAnonEnabled;
    private final boolean jSessionIdUserEnabled;
    private final Map<String, String> seoReplacements;
    private final Map<String, Pattern> seoPatterns;
    private final Map<String, String> forwardReplacements;
    private final Map<String, Integer> forwardResponseCodes;
    private final Map<String, String> charFilters;
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
    private final boolean implicitRequestNameMatchesOnly; // default true => safer
    private final boolean generateImplicitCategoryUrl; 
    private final boolean generateImplicitProductUrl;
    private final boolean allowTargetOutsideCatalog = true; // TODO: unhardcode
    private final boolean allowInvalidCategoryPathElements = true; // TODO: unhardcode (but this is much better true)
    private final boolean allowAltUrlSuffix = true; // TODO: unhardcode
    private final String productAltUrlSuffix = "-p"; // LEGACY fallback - TODO: unhardcode
    private final String categoryAltUrlSuffix = "-c"; // LEGACY fallback - TODO: unhardcode
    private final ClosestTrailResolver.ResolverType newUrlTrailResolver = ClosestTrailResolver.ResolverType.NONCONSEC_ANYPART; // TODO: unhardcode
    
    public static final String DEFAULT_REGEXP = "^.*/.*$";
    public static final String ELEMENT_REGEXPIFMATCH = "regexpifmatch";
    public static final String ELEMENT_URL_CONFIG = "url-config";
    public static final String ELEMENT_DESCRIPTION = "description";
    public static final String ELEMENT_FORWARD = "forward";
    public static final String ELEMENT_SEO = "seo";
    public static final String ELEMENT_URLPATTERN = "url-pattern";
    public static final String ELEMENT_REPLACEMENT = "replacement";
    public static final String ELEMENT_RESPONSECODE = "responsecode";
    public static final String ELEMENT_JSESSIONID = "jsessionid";
    public static final String ELEMENT_ANONYMOUS = "anonymous";
    public static final String ELEMENT_VALUE = "value";
    public static final String ELEMENT_USER = "user";
    public static final String ELEMENT_EXCEPTIONS = "exceptions";
    public static final String ELEMENT_CHAR_FILTERS = "char-filters";
    public static final String ELEMENT_CHAR_FILTER = "char-filter";
    public static final String ELEMENT_CHARACTER_PATTERN = "character-pattern";
    public static final String ELEMENT_CATEGORY_URL = "category-url";
    public static final String ELEMENT_ALLOWED_CONTEXT_PATHS = "allowed-context-paths";
    public static final String ELEMENT_CATEGORY_NAME = "category-name";
    public static final String ELEMENT_CATEGORY_URL_SUFFIX = "category-url-suffix";
    public static final String SEO_CONFIG_FILENAME = "SeoConfig.xml";
    public static final int DEFAULT_RESPONSECODE = HttpServletResponse.SC_MOVED_PERMANENTLY;
    public static final String DEFAULT_ANONYMOUS_VALUE = "disable";
    public static final String DEFAULT_USER_VALUE = "disable";
    public static final String DEFAULT_CATEGORY_URL_VALUE = "enable";
    public static final String DEFAULT_CATEGORY_NAME_VALUE = "disable";
    public static final String ALLOWED_CONTEXT_PATHS_SEPERATOR = ":";
    
    public static final String ELEMENT_CATEGORY_NAME_MAX_LENGTH = "category-name-max-length";
    public static final String ELEMENT_PRODUCT_NAME_MAX_LENGTH = "product-name-max-length";
    public static final String ELEMENT_NAMES_LOCALE_OVERRIDE = "names-locale-override";
    public static final String ELEMENT_CATEGORY_NAME_SEPARATE_PATH_ELEM = "category-name-separate-path-elem";
    public static final String ELEMENT_CATEGORY_NAME_APPEND_ID = "category-name-append-id";
    public static final String ELEMENT_CATEGORY_NAME_APPEND_ID_LAST = "category-name-append-id-last";
    public static final String ELEMENT_PRODUCT_NAME_APPEND_ID = "product-name-append-id";
    public static final String ELEMENT_HANDLE_IMPLICIT_REQUESTS = "handle-implicit-requests";
    public static final String ELEMENT_IMPLICIT_REQUEST_NAME_MATCHES_ONLY = "implicit-request-name-matches-only";
    public static final String ELEMENT_GENERATE_IMPLICIT_CATEGORY_URL = "generate-implicit-category-url";
    public static final String ELEMENT_GENERATE_IMPLICIT_PRODUCT_URL = "generate-implicit-product-url";
    
    public static final String DEFAULT_CATEGORY_NAME_SEPARATE_PATH_ELEM_VALUE = "disable";
    public static final String DEFAULT_CATEGORY_NAME_APPEND_ID_VALUE = "disable";
    public static final String DEFAULT_CATEGORY_NAME_APPEND_ID_LAST_VALUE = "disable";
    public static final String DEFAULT_PRODUCT_NAME_APPEND_ID_VALUE = "disable";
    public static final String DEFAULT_HANDLE_IMPLICIT_REQUESTS_VALUE = "disable";
    public static final String DEFAULT_IMPLICIT_REQUEST_NAME_MATCHES_ONLY_VALUE = "enable";
    public static final String DEFAULT_GENERATE_IMPLICIT_CATEGORY_URL_VALUE = "disable";
    public static final String DEFAULT_GENERATE_IMPLICIT_PRODUCT_URL_VALUE = "disable";
    
    /**
     * SCIPIO: Returns the default or common config.
     */
    public static SeoConfig getDefaultConfig() {
        return SeoConfigDefault.INSTANCE;
    }
    
    /**
     * SCIPIO: Returns config for website.
     * TODO: currently this always returns default config.
     */
    public static SeoConfig getConfig(Delegator delegator, String webSiteId) {
        // TODO?: possible website-specific stuff in future
        return getDefaultConfig();
    }
    
    protected SeoConfig(boolean isInitialed) {        
        boolean categoryUrlEnabled = true;
        boolean categoryNameEnabled = false;
        String categoryUrlSuffix = null;
        Pattern regexpIfMatch = null;
        boolean useUrlRegexp = false;
        boolean jSessionIdAnonEnabled = false;
        boolean jSessionIdUserEnabled = false;
        Map<String, String> seoReplacements = new HashMap<>();
        Map<String, Pattern> seoPatterns = new HashMap<>();
        Map<String, String> forwardReplacements = new HashMap<>();
        Map<String, Integer> forwardResponseCodes = new HashMap<>();
        Map<String, String> charFilters = new HashMap<>();
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
        
        this.categoryUrlEnabled = categoryUrlEnabled;
        this.categoryNameEnabled = categoryNameEnabled;
        this.categoryUrlSuffix = categoryUrlSuffix;
        this.regexpIfMatch = regexpIfMatch;
        this.useUrlRegexp = useUrlRegexp;
        this.jSessionIdAnonEnabled = jSessionIdAnonEnabled;
        this.jSessionIdUserEnabled = jSessionIdUserEnabled;
        this.seoReplacements = seoReplacements;
        this.seoPatterns = seoPatterns;
        this.forwardReplacements = forwardReplacements;
        this.forwardResponseCodes = forwardResponseCodes;
        this.charFilters = charFilters;
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
        
        this.isInitialed = isInitialed;
    }
    
    public SeoConfig(Element rootElement, Document configDoc) {
        this(rootElement, configDoc, "document");
    }
    
    public SeoConfig(Element rootElement, Document configDoc, String srcInfo) {
        boolean categoryUrlEnabled = true;
        boolean categoryNameEnabled = false;
        String categoryUrlSuffix = null;
        Pattern regexpIfMatch = null;
        boolean useUrlRegexp = false;
        boolean jSessionIdAnonEnabled = false;
        boolean jSessionIdUserEnabled = false;
        Map<String, String> seoReplacements = new HashMap<>();
        Map<String, Pattern> seoPatterns = new HashMap<>();
        Map<String, String> forwardReplacements = new HashMap<>();
        Map<String, Integer> forwardResponseCodes = new HashMap<>();
        Map<String, String> charFilters = new HashMap<>();
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
        
        String regexIfMatch = UtilXml.childElementValue(rootElement, ELEMENT_REGEXPIFMATCH, DEFAULT_REGEXP);
        Debug.logInfo("Parsing " + regexIfMatch, module);
        try {
            regexpIfMatch = Pattern.compile(regexIfMatch);
        } catch (Exception e1) {
            Debug.logWarning(e1, "Error while parsing " + regexIfMatch, module);
        }

        // parse category-url element
        try {
            Element categoryUrlElement = UtilXml.firstChildElement(rootElement, ELEMENT_CATEGORY_URL);
            Debug.logInfo("Parsing " + ELEMENT_CATEGORY_URL + " [" + (categoryUrlElement != null) + "]:", module);
            if (categoryUrlElement != null) {
                String enableCategoryUrlValue = UtilXml.childElementValue(categoryUrlElement, ELEMENT_VALUE, DEFAULT_CATEGORY_URL_VALUE);
                if (DEFAULT_CATEGORY_URL_VALUE.equalsIgnoreCase(enableCategoryUrlValue)) {
                    categoryUrlEnabled = true;
                } else {
                    categoryUrlEnabled = false;
                }
                
                if (categoryUrlEnabled) {
                    String allowedContextValue = UtilXml.childElementValue(categoryUrlElement, ELEMENT_ALLOWED_CONTEXT_PATHS, null);
                    allowedContextPaths = new HashSet<>();
                    if (UtilValidate.isNotEmpty(allowedContextValue)) {
                        List<String> allowedContextPathList = StringUtil.split(allowedContextValue, ALLOWED_CONTEXT_PATHS_SEPERATOR);
                        for (String path : allowedContextPathList) {
                            if (UtilValidate.isNotEmpty(path)) {
                                path = path.trim();
                                if (!allowedContextPaths.contains(path)) {
                                    allowedContextPaths.add(path);
                                    Debug.logInfo("  " + ELEMENT_ALLOWED_CONTEXT_PATHS + ": " + path, module);
                                }
                            }
                        }
                    }
                    
                    String categoryNameValue = UtilXml.childElementValue(categoryUrlElement, ELEMENT_CATEGORY_NAME, DEFAULT_CATEGORY_NAME_VALUE);
                    if (DEFAULT_CATEGORY_NAME_VALUE.equalsIgnoreCase(categoryNameValue)) {
                        categoryNameEnabled = false;
                    } else {
                        categoryNameEnabled = true;
                    }
                    Debug.logInfo("  " + ELEMENT_CATEGORY_NAME + ": " + categoryNameEnabled, module);

                    categoryUrlSuffix = UtilXml.childElementValue(categoryUrlElement, ELEMENT_CATEGORY_URL_SUFFIX, null);
                    if (UtilValidate.isNotEmpty(categoryUrlSuffix)) {
                        categoryUrlSuffix = categoryUrlSuffix.trim();
                        if (categoryUrlSuffix.contains("/")) {
                            categoryUrlSuffix = null;
                        }
                    }
                    Debug.logInfo("  " + ELEMENT_CATEGORY_URL_SUFFIX + ": " + categoryUrlSuffix, module);
                    
                    
                    {
                        String maxLen = UtilXml.childElementValue(categoryUrlElement, ELEMENT_CATEGORY_NAME_MAX_LENGTH, null);
                        if (UtilValidate.isNotEmpty(maxLen)) {
                            try {
                                categoryNameMaxLength = Integer.parseInt(maxLen);
                                if (categoryNameMaxLength < 0) {
                                    categoryNameMaxLength = null;
                                }
                            }
                            catch(Exception e) {
                                Debug.logError(e, "Couldn't parse " + ELEMENT_CATEGORY_NAME_MAX_LENGTH + ": " + e.getMessage(), module);
                            }
                        }
                        Debug.logInfo("  " + ELEMENT_CATEGORY_NAME_MAX_LENGTH + ": " + categoryNameMaxLength, module);
                    }
                    
                    {
                        String maxLen = UtilXml.childElementValue(categoryUrlElement, ELEMENT_PRODUCT_NAME_MAX_LENGTH, null);
                        if (UtilValidate.isNotEmpty(maxLen)) {
                            try {
                                productNameMaxLength = Integer.parseInt(maxLen);
                                if (productNameMaxLength < 0) {
                                    productNameMaxLength = null;
                                }
                            }
                            catch(Exception e) {
                                Debug.logError(e, "Couldn't parse " + ELEMENT_PRODUCT_NAME_MAX_LENGTH + ": " + e.getMessage(), module);
                            }
                        }
                        Debug.logInfo("  " + ELEMENT_PRODUCT_NAME_MAX_LENGTH + ": " + productNameMaxLength, module);
                    }
                    
                    {
                        String namesLocaleOverrideStr = UtilXml.childElementValue(categoryUrlElement, ELEMENT_NAMES_LOCALE_OVERRIDE, null);
                        if (UtilValidate.isNotEmpty(namesLocaleOverrideStr)) {
                            try {
                                namesLocaleOverride = new Locale(namesLocaleOverrideStr);
                            }
                            catch(Exception e) {
                                Debug.logError(e, "Couldn't parse " + ELEMENT_NAMES_LOCALE_OVERRIDE + ": " + e.getMessage(), module);
                            }
                        }
                        Debug.logInfo("  " + ELEMENT_NAMES_LOCALE_OVERRIDE + ": " + namesLocaleOverride, module);
                    }
                    
                    {
                        String categoryNameSepValue = UtilXml.childElementValue(categoryUrlElement, 
                                ELEMENT_CATEGORY_NAME_SEPARATE_PATH_ELEM, DEFAULT_CATEGORY_NAME_SEPARATE_PATH_ELEM_VALUE);
                        if (DEFAULT_CATEGORY_NAME_SEPARATE_PATH_ELEM_VALUE.equalsIgnoreCase(categoryNameSepValue)) {
                            categoryNameSeparatePathElem = false;
                        } else {
                            categoryNameSeparatePathElem = true;
                        }
                        Debug.logInfo("  " + ELEMENT_CATEGORY_NAME_SEPARATE_PATH_ELEM + ": " + categoryNameSeparatePathElem, module);
                    }
                    
                    {
                        String categoryNameAppendIdValue = UtilXml.childElementValue(categoryUrlElement, 
                                ELEMENT_CATEGORY_NAME_APPEND_ID, DEFAULT_CATEGORY_NAME_APPEND_ID_VALUE);
                        if (DEFAULT_CATEGORY_NAME_APPEND_ID_VALUE.equalsIgnoreCase(categoryNameAppendIdValue)) {
                            categoryNameAppendId = false;
                        } else {
                            categoryNameAppendId = true;
                        }
                        Debug.logInfo("  " + ELEMENT_CATEGORY_NAME_APPEND_ID + ": " + categoryNameAppendId, module);
                    }
                    
                    {
                        String categoryNameAppendIdLastValue = UtilXml.childElementValue(categoryUrlElement, 
                                ELEMENT_CATEGORY_NAME_APPEND_ID_LAST, DEFAULT_CATEGORY_NAME_APPEND_ID_LAST_VALUE);
                        if (DEFAULT_CATEGORY_NAME_APPEND_ID_VALUE.equalsIgnoreCase(categoryNameAppendIdLastValue)) {
                            categoryNameAppendIdLast = false;
                        } else {
                            categoryNameAppendIdLast = true;
                        }
                        Debug.logInfo("  " + ELEMENT_CATEGORY_NAME_APPEND_ID + ": " + categoryNameAppendIdLast, module);
                    }
                    
                    // SCIPIO: 2017: new
                    {
                        String productNameAppendIdValue = UtilXml.childElementValue(categoryUrlElement, 
                                ELEMENT_PRODUCT_NAME_APPEND_ID, DEFAULT_PRODUCT_NAME_APPEND_ID_VALUE);
                        if (DEFAULT_PRODUCT_NAME_APPEND_ID_VALUE.equalsIgnoreCase(productNameAppendIdValue)) {
                            productNameAppendId = false;
                        } else {
                            productNameAppendId = true;
                        }
                        Debug.logInfo("  " + ELEMENT_PRODUCT_NAME_APPEND_ID + ": " + productNameAppendId, module);
                    }
                    
                    {
                        String handleImplicitRequestsValue = UtilXml.childElementValue(categoryUrlElement, 
                                ELEMENT_HANDLE_IMPLICIT_REQUESTS, DEFAULT_HANDLE_IMPLICIT_REQUESTS_VALUE);
                        if (DEFAULT_HANDLE_IMPLICIT_REQUESTS_VALUE.equalsIgnoreCase(handleImplicitRequestsValue)) {
                            handleImplicitRequests = false;
                        } else {
                            handleImplicitRequests = true;
                        }
                        Debug.logInfo("  " + ELEMENT_HANDLE_IMPLICIT_REQUESTS + ": " + handleImplicitRequests, module);
                    }
                    
                    
                    {
                        String implicitRequestNameMatchesOnlyValue = UtilXml.childElementValue(categoryUrlElement, 
                                ELEMENT_IMPLICIT_REQUEST_NAME_MATCHES_ONLY, DEFAULT_IMPLICIT_REQUEST_NAME_MATCHES_ONLY_VALUE);
                        if (DEFAULT_IMPLICIT_REQUEST_NAME_MATCHES_ONLY_VALUE.equalsIgnoreCase(implicitRequestNameMatchesOnlyValue)) {
                            implicitRequestNameMatchesOnly = true;
                        } else {
                            implicitRequestNameMatchesOnly = false;
                        }
                        Debug.logInfo("  " + ELEMENT_IMPLICIT_REQUEST_NAME_MATCHES_ONLY + ": " + implicitRequestNameMatchesOnly, module);
                    }
                    
                    {
                        String generateImplicitCategoryUrlValue = UtilXml.childElementValue(categoryUrlElement, 
                                ELEMENT_GENERATE_IMPLICIT_CATEGORY_URL, DEFAULT_GENERATE_IMPLICIT_CATEGORY_URL_VALUE);
                        if (DEFAULT_GENERATE_IMPLICIT_CATEGORY_URL_VALUE.equalsIgnoreCase(generateImplicitCategoryUrlValue)) {
                            generateImplicitCategoryUrl = false;
                        } else {
                            generateImplicitCategoryUrl = true;
                        }
                        Debug.logInfo("  " + ELEMENT_GENERATE_IMPLICIT_CATEGORY_URL + ": " + generateImplicitCategoryUrl, module);
                    }
                    
                    {
                        String generateImplicitProductUrlValue = UtilXml.childElementValue(categoryUrlElement, 
                                ELEMENT_GENERATE_IMPLICIT_PRODUCT_URL, DEFAULT_GENERATE_IMPLICIT_PRODUCT_URL_VALUE);
                        if (DEFAULT_GENERATE_IMPLICIT_PRODUCT_URL_VALUE.equalsIgnoreCase(generateImplicitProductUrlValue)) {
                            generateImplicitProductUrl = false;
                        } else {
                            generateImplicitProductUrl = true;
                        }
                        Debug.logInfo("  " + ELEMENT_GENERATE_IMPLICIT_PRODUCT_URL + ": " + generateImplicitProductUrl, module);
                    }
                }
            }
        } catch (NullPointerException e) {
            // no "category-url" element
            Debug.logWarning("No category-url element found in " + srcInfo, module);
        }

        // parse jsessionid element
        try {
            Element jSessionId = UtilXml.firstChildElement(rootElement, ELEMENT_JSESSIONID);
            Debug.logInfo("Parsing " + ELEMENT_JSESSIONID + " [" + (jSessionId != null) + "]:", module);
            if (jSessionId != null) {
                Element anonymous = UtilXml.firstChildElement(jSessionId, ELEMENT_ANONYMOUS);
                if (anonymous != null) {
                    String anonymousValue = UtilXml.childElementValue(anonymous, ELEMENT_VALUE, DEFAULT_ANONYMOUS_VALUE);
                    if (DEFAULT_ANONYMOUS_VALUE.equalsIgnoreCase(anonymousValue)) {
                        jSessionIdAnonEnabled = false;
                    } else {
                        jSessionIdAnonEnabled = true;
                    }
                } else {
                    jSessionIdAnonEnabled = Boolean.valueOf(DEFAULT_ANONYMOUS_VALUE).booleanValue();
                }
                Debug.logInfo("  " + ELEMENT_ANONYMOUS + ": " + jSessionIdAnonEnabled, module);
                
                Element user = UtilXml.firstChildElement(jSessionId, ELEMENT_USER);
                if (user != null) {
                    String userValue = UtilXml.childElementValue(user, ELEMENT_VALUE, DEFAULT_USER_VALUE);
                    if (DEFAULT_USER_VALUE.equalsIgnoreCase(userValue)) {
                        jSessionIdUserEnabled = false;
                    } else {
                        jSessionIdUserEnabled = true;
                    }

                    Element exceptions = UtilXml.firstChildElement(user, ELEMENT_EXCEPTIONS);
                    if (exceptions != null) {
                        Debug.logInfo("  " + ELEMENT_EXCEPTIONS + ": ", module);
                        List<? extends Element> exceptionUrlPatterns = UtilXml.childElementList(exceptions, ELEMENT_URLPATTERN);
                        for (int i = 0; i < exceptionUrlPatterns.size(); i++) {
                            Element element = exceptionUrlPatterns.get(i);
                            String urlpattern = element.getTextContent();
                            if (UtilValidate.isNotEmpty(urlpattern)) {
                                try {
                                    Pattern pattern = Pattern.compile(urlpattern);
                                    userExceptionPatterns.add(pattern);
                                    Debug.logInfo("    " + ELEMENT_URLPATTERN + ": " + urlpattern, module);
                                } catch (Exception e) {
                                    Debug.logWarning("Can NOT parse " + urlpattern + " in element " + ELEMENT_URLPATTERN + " of " + ELEMENT_EXCEPTIONS + ". Error: " + e.getMessage(), module);
                                }
                            }
                        }
                    }
                } else {
                    jSessionIdUserEnabled = Boolean.valueOf(DEFAULT_USER_VALUE).booleanValue();
                }
                Debug.logInfo("  " + ELEMENT_USER + ": " + jSessionIdUserEnabled, module);
            }
        } catch (NullPointerException e) {
            Debug.logWarning("No jsessionid element found in " + srcInfo, module);
        }
        
        // parse url-config elements
        try {
            NodeList configs = rootElement.getElementsByTagName(ELEMENT_URL_CONFIG);
            Debug.logInfo("Parsing " + ELEMENT_URL_CONFIG, module);
            for (int j = 0; j < configs.getLength(); j++) {
                Element config = (Element) configs.item(j);
                String urlpattern = UtilXml.childElementValue(config, ELEMENT_URLPATTERN, null);
                if (UtilValidate.isEmpty(urlpattern)) {
                    continue;
                }
                Debug.logInfo("  " + ELEMENT_URLPATTERN + ": " + urlpattern, module);
                Pattern pattern;
                try {
                    pattern = Pattern.compile(urlpattern);
                    seoPatterns.put(urlpattern, pattern);
                } catch (Exception e) {
                    Debug.logWarning("Error while creating parttern for seo url-pattern: " + urlpattern, module);
                    continue;
                }
                
                // construct seo patterns
                Element seo = UtilXml.firstChildElement(config, ELEMENT_SEO);
                if (UtilValidate.isNotEmpty(seo)) {
                    String replacement = UtilXml.childElementValue(seo, ELEMENT_REPLACEMENT, null);
                    if (UtilValidate.isNotEmpty(replacement)) {
                        seoReplacements.put(urlpattern, replacement);
                        Debug.logInfo("    " + ELEMENT_SEO + " " + ELEMENT_REPLACEMENT + ": " + replacement, module);
                    }
                }

                // construct forward patterns
                Element forward = UtilXml.firstChildElement(config, ELEMENT_FORWARD);
                if (UtilValidate.isNotEmpty(forward)) {
                    String replacement = UtilXml.childElementValue(forward, ELEMENT_REPLACEMENT, null);
                    String responseCode = UtilXml.childElementValue(forward,
                            ELEMENT_RESPONSECODE, String.valueOf(DEFAULT_RESPONSECODE));
                    if (UtilValidate.isNotEmpty(replacement)) {
                        forwardReplacements.put(urlpattern, replacement);
                        Debug.logInfo("    " + ELEMENT_FORWARD + " " + ELEMENT_REPLACEMENT + ": " + replacement, module);
                        if (UtilValidate.isNotEmpty(responseCode)) {
                            Integer responseCodeInt = DEFAULT_RESPONSECODE;
                            try {
                                responseCodeInt = Integer.valueOf(responseCode);
                            } catch (NumberFormatException nfe) {
                                Debug.logWarning(nfe, "Error while parsing response code number: " + responseCode, module);
                            }
                            forwardResponseCodes.put(urlpattern, responseCodeInt);
                            Debug.logInfo("    " + ELEMENT_FORWARD + " " + ELEMENT_RESPONSECODE + ": " + responseCodeInt, module);
                        }
                    }
                }
            }
        } catch (NullPointerException e) {
            // no "url-config" element
            Debug.logWarning("No " + ELEMENT_URL_CONFIG + " element found in " + srcInfo, module);
        }

        // parse char-filters elements
        try {
            NodeList nameFilterNodes = rootElement
                    .getElementsByTagName(ELEMENT_CHAR_FILTER);
            Debug.logInfo("Parsing " + ELEMENT_CHAR_FILTER + ": ", module);
            for (int i = 0; i < nameFilterNodes.getLength(); i++) {
                Element element = (Element) nameFilterNodes.item(i);
                String charaterPattern = UtilXml.childElementValue(element, ELEMENT_CHARACTER_PATTERN, null);
                String replacement = UtilXml.childElementValue(element, ELEMENT_REPLACEMENT, null);
                if (UtilValidate.isNotEmpty(charaterPattern)
                        && UtilValidate.isNotEmpty(replacement)) {
                    try {
                        Pattern.compile(charaterPattern);
                        charFilters.put(charaterPattern, replacement);
                        Debug.logInfo("  " + ELEMENT_CHARACTER_PATTERN + ": " + charaterPattern, module);
                        Debug.logInfo("  " + ELEMENT_REPLACEMENT + ": " + replacement, module);
                    } catch (Exception e) {
                        // skip this filter (character-pattern replacement) if any error happened
                        Debug.logWarning(e, "Error while parsing " + ELEMENT_CHARACTER_PATTERN + ": " + charaterPattern, module);
                    }
                }
            }
        } catch (NullPointerException e) {
            // no "char-filters" element
            Debug.logWarning("No " + ELEMENT_CHAR_FILTER + " element found in " + srcInfo, module);
        }
        
        if (seoReplacements.keySet().isEmpty()) {
            useUrlRegexp = false;
        } else {
            useUrlRegexp = true;
        }
        
        this.categoryUrlEnabled = categoryUrlEnabled;
        this.categoryNameEnabled = categoryNameEnabled;
        this.categoryUrlSuffix = categoryUrlSuffix;
        this.regexpIfMatch = regexpIfMatch;
        this.useUrlRegexp = useUrlRegexp;
        this.jSessionIdAnonEnabled = jSessionIdAnonEnabled;
        this.jSessionIdUserEnabled = jSessionIdUserEnabled;
        this.seoReplacements = seoReplacements;
        this.seoPatterns = seoPatterns;
        this.forwardReplacements = forwardReplacements;
        this.forwardResponseCodes = forwardResponseCodes;
        this.charFilters = charFilters;
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
        
        this.isInitialed = true;
    }

    private static SeoConfig readDefaultConfig() {
        FileInputStream configFileIS = null;
        SeoConfig config = null;
        try {
            URL seoConfigFilename = UtilURL.fromResource(SEO_CONFIG_FILENAME);
            Document configDoc = UtilXml.readXmlDocument(seoConfigFilename, false);
            Element rootElement = configDoc.getDocumentElement();

            config = new SeoConfig(rootElement, configDoc, seoConfigFilename.toString());
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
        return config != null ? config : new SeoConfig(false);
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
     * 
     * @return a boolean value to indicate whether category url is enabled.
     */
    public boolean isCategoryUrlEnabledStatic() {
        return categoryUrlEnabled;
    }

    /**
     * SCIPIO: 2017: check if category URL enabled for context path and webSiteId.
     * 
     * @return a boolean value to indicate whether the web site is enabled.
     */
    public boolean isCategoryUrlEnabled(String contextPath, String webSiteId) {
        return categoryUrlEnabled && 
            (WebsiteSeoConfig.isSeoEnabled(webSiteId)) && allowedContextPaths.contains(normContextPath(contextPath));
    }
    
    /**
     * SCIPIO: 2017: check if category URL enabled for webSiteId.
     * 
     * @return a boolean value to indicate whether the web site is enabled.
     */
    public boolean isCategoryUrlEnabledForWebsite(String webSiteId) {
        return categoryUrlEnabled && WebsiteSeoConfig.isSeoEnabled(webSiteId);
    }
    
    /**
     * Check whether the context path is enabled.
     * @deprecated SCIPIO: 2017: use {@link #isCategoryUrlEnabledForContextPath}.
     * 
     * @return a boolean value to indicate whether the context path is enabled.
     */
    @Deprecated
    public boolean isCategoryUrlEnabled(String contextPath) {
        return isCategoryUrlEnabledForContextPath(contextPath);
    }
    
    /**
     * Check whether the context path is enabled.
     * 
     * @return a boolean value to indicate whether the context path is enabled.
     */
    public boolean isCategoryUrlEnabledForContextPath(String contextPath) {
        return categoryUrlEnabled && allowedContextPaths.contains(normContextPath(contextPath));
    }
    
    private String normContextPath(String contextPath) {
        if (contextPath == null) return null;
        if (UtilValidate.isEmpty(contextPath)) {
            contextPath = "/";
        }
        return contextPath.trim();
    }

    /**
     * Check whether category name is enabled.
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
    public String getCategoryUrlSuffix() {
        return categoryUrlSuffix;
    }

    /**
     * Check whether jsessionid is enabled for anonymous.
     * 
     * @return a boolean value to indicate whether jsessionid is enabled for anonymous.
     */
    public boolean isJSessionIdAnonEnabled() {
        return jSessionIdAnonEnabled;
    }

    /**
     * Check whether jsessionid is enabled for user.
     * 
     * @return a boolean value to indicate whether jsessionid is enabled for user.
     */
    public boolean isJSessionIdUserEnabled() {
        return jSessionIdUserEnabled;
    }

    /**
     * Get user exception url pattern configures.
     * 
     * @return user exception url pattern configures (java.util.List<Pattern>)
     */
    public List<Pattern> getUserExceptionPatterns() {
        return userExceptionPatterns;
    }

    /**
     * Get char filters.
     * 
     * @return char filters (java.util.Map<String, String>)
     */
    public Map<String, String> getCharFilters() {
        return charFilters;
    }

    /**
     * Get seo url pattern configures.
     * 
     * @return seo url pattern configures (java.util.Map<String, Pattern>)
     */
    public Map<String, Pattern> getSeoPatterns() {
        return seoPatterns;
    }

    /**
     * Get seo replacement configures.
     * 
     * @return seo replacement configures (java.util.Map<String, String>)
     */
    public Map<String, String> getSeoReplacements() {
        return seoReplacements;
    }

    /**
     * Get forward replacement configures.
     * 
     * @return forward replacement configures (java.util.Map<String, String>)
     */
    public Map<String, String> getForwardReplacements() {
        return forwardReplacements;
    }

    /**
     * Get forward response codes.
     * 
     * @return forward response code configures (java.util.Map<String, Integer>)
     */
    public Map<String, Integer> getForwardResponseCodes() {
        return forwardResponseCodes;
    }

    /**
     * Check whether a product id is in the special list. If we cannot get a product from a lower cased
     * or upper cased product id, then it's special.
     * 
     * @return boolean to indicate whether the product id is special.
     */
    @Deprecated
    public boolean isSpecialProductId(String productId) {
        return specialProductIds.containsKey(productId);
    }

    /**
     * Add a special product id to the special list.
     * SCIPIO: 2017: this now throws an exception - the config should not be modifiable publicly
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
     * 
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
        if (categoryNameMaxLength != null && categoryName != null) {
            if (categoryName.length() > categoryNameMaxLength) {
                return categoryName.substring(0, categoryNameMaxLength);
            }
        }
        return categoryName;
    }
    
    public Integer getProductNameMaxLength() {
        return productNameMaxLength;
    }
    
    public String limitProductNameLength(String productName) {
        if (productNameMaxLength != null && productName != null) {
            if (productName.length() > productNameMaxLength) {
                return productName.substring(0, productNameMaxLength);
            }
        }
        return productName;
    }
    
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
    
    /**
     * SCIPIO: new 2017
     */
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
     * WARN: LEAVE TRUE FOR NOW: more user-friendly and faster.
     */
    public boolean isAllowInvalidCategoryPathElements() {
        return allowInvalidCategoryPathElements;
    }
    
    /**
     * If true, the filter will accept old alt-url generated links, for backward compat.
     */
    public boolean isAllowAltUrlSuffix() {
        return allowAltUrlSuffix;
    }
    
    public boolean isAllowProductAltUrlSuffix() {
        return allowAltUrlSuffix && productAltUrlSuffix != null;
    }

    public boolean isAllowCategoryAltUrlSuffix() {
        return allowAltUrlSuffix && categoryAltUrlSuffix != null;
    }

    /**
     * The legacy alt url product suffix.
     */
    public String getProductAltUrlSuffix() {
        return productAltUrlSuffix;
    }

    /**
     * The legacy alt url category suffix.
     */
    public String getCategoryAltUrlSuffix() {
        return categoryAltUrlSuffix;
    }

    public ClosestTrailResolver.ResolverType getNewUrlTrailResolver() {
        return newUrlTrailResolver;
    }
}

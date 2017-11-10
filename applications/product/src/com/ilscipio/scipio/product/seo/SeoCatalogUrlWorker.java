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

import java.io.IOException;
import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.common.UrlServletHelper;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.product.category.CatalogUrlFilter.CatalogAltUrlBuilder;
import org.ofbiz.product.category.CatalogUrlServlet.CatalogUrlBuilder;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.product.category.CategoryWorker;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.website.WebSiteWorker;

import com.ilscipio.scipio.product.category.CatalogAltUrlSanitizer;
import com.ilscipio.scipio.product.category.CatalogAltUrlSanitizer.ObjectType;
import com.ilscipio.scipio.util.SeoStringUtil;

/**
 * SCIPIO: SEO url building functions and callbacks.
 * <p>
 * Some parts adapted from the original <code>org.ofbiz.product.category.ftl.CatalogUrlSeoTransform</code>.
 * <p>
 * WARN: Do not call makeXxxUrl methods from client code.
 * Client code that need java methods should use (which these plug into):
 * {@link org.ofbiz.product.category.CatalogUrlFilter#makeCatalogAltLink}
 * {@link org.ofbiz.product.category.CatalogUrlServlet#makeCatalogLink}
 */
@SuppressWarnings("serial")
public class SeoCatalogUrlWorker implements Serializable {

    public static final String module = SeoCatalogUrlWorker.class.getName();

    public static final String DEFAULT_CONFIG_RESOURCE = "SeoConfigUiLabels";

    private static final SeoCatalogUrlWorker DEFAULT_INSTANCE = new SeoCatalogUrlWorker();

    // TODO: in production, these cache can be tweaked with non-soft refs, limits and expire time
    private static final UtilCache<String, Map<String, AltUrlPartInfo>> productAltUrlPartInfoCache = UtilCache.createUtilCache("seo.filter.product.alturl.part", true);
    private static final UtilCache<String, Map<String, AltUrlPartInfo>> categoryAltUrlPartInfoCache = UtilCache.createUtilCache("seo.filter.category.alturl.part", true);

    /**
     * FIXME: unhardcode; could be per-store.
     */
    private static final List<String> DEFAULT_BROWSABLE_ROOTCATTYPES = UtilMisc.unmodifiableArrayList(
            "PCCT_BROWSE_ROOT", "PCCT_PROMOTIONS", "PCCT_BEST_SELL"
            );
    
    /*
     * *****************************************************
     * Fields
     * *****************************************************
     */

    public enum UrlType {
        PRODUCT,
        CATEGORY;
        // TODO?: FUTURE: CONTENT
    }
    
    // TODO: intended for later
    //protected final String webSiteId;
    //protected final String contextPath;
    
    protected final String configResourceName;
    protected final String urlSuffix;
    
    // lazy load, these do not require sync
    protected LocalizedName productPathName = null;
    protected LocalizedName categoryPathName = null;
    
    // kludge for no multiple inheritance
    protected final SeoCatalogUrlBuilder catalogUrlBuilder;
    protected final SeoCatalogAltUrlBuilder catalogAltUrlBuilder;
    protected final CatalogAltUrlSanitizer catalogAltUrlSanitizer;

    /*
     * *****************************************************
     * Constructors and factories
     * *****************************************************
     */

    protected SeoCatalogUrlWorker() {
        this.configResourceName = DEFAULT_CONFIG_RESOURCE;
        this.urlSuffix = SeoConfigUtil.getCategoryUrlSuffix() != null ? SeoConfigUtil.getCategoryUrlSuffix() : "";
        this.catalogUrlBuilder = new SeoCatalogUrlBuilder();
        this.catalogAltUrlBuilder = new SeoCatalogAltUrlBuilder();
        this.catalogAltUrlSanitizer = new SeoCatalogAltUrlSanitizer();
    }

    /**
     * TODO: to be removed later.
     */
    static void registerUrlBuilder() {
        // TODO?: unhardcode via properties?
        CatalogUrlBuilder.registerUrlBuilder("seo", BuilderFactory.getInstance());
        CatalogAltUrlBuilder.registerUrlBuilder("seo", BuilderFactory.getInstance());
    }

    /**
     * Returns an instance that is UNABLE to perform website-specific operations.
     */
    public static SeoCatalogUrlWorker getDefaultInstance(Delegator delegator) {
        return DEFAULT_INSTANCE;
    }

    /**
     * Returns an instance with possible website-specific configuration.
     * <p>
     * TODO: currently only returns default instance, inevitably this will change.
     */
    public static SeoCatalogUrlWorker getInstance(Delegator delegator, String webSiteId) {
        // TODO: this should return different builder depending on store and config
        return getDefaultInstance(delegator);
    }

    /**
     * Returns an instance with possible website-specific configuration IF it is
     * enabled for this website/context, otherwise null.
     * <p>
     * TODO: currently only returns default instance, inevitably this will change.
     */
    public static SeoCatalogUrlWorker getInstanceIfEnabled(HttpServletRequest request,
                Delegator delegator, String contextPath, String webSiteId) {
        if (!SeoConfigUtil.isCategoryUrlEnabled(contextPath, webSiteId)) return null;
        // TODO: should return different builder depending on store and config
        return getDefaultInstance(delegator);
    }

    /**
     * Boilerplate factory that returns builder instances for the CatalogUrlFilter/CatalogUrlServlet builder registry.
     * Methods return null if SEO not enabled for the webSiteId/contextPath.
     */
    public static class BuilderFactory implements CatalogAltUrlBuilder.Factory, CatalogUrlBuilder.Factory {
        private static final BuilderFactory INSTANCE = new BuilderFactory();

        public static BuilderFactory getInstance() { return INSTANCE; }
        @Override
        public CatalogUrlBuilder getCatalogUrlBuilder(boolean withRequest, HttpServletRequest request,
                Delegator delegator, String contextPath, String webSiteId) {
            if (!SeoConfigUtil.isCategoryUrlEnabled(contextPath, webSiteId)) return null;
            return SeoCatalogUrlWorker.getInstance(delegator, webSiteId).getCatalogUrlBuilder();
        }
        @Override
        public CatalogAltUrlBuilder getCatalogAltUrlBuilder(boolean withRequest, HttpServletRequest request,
                Delegator delegator, String contextPath, String webSiteId) {
            if (!SeoConfigUtil.isCategoryUrlEnabled(contextPath, webSiteId)) return null;
            return SeoCatalogUrlWorker.getInstance(delegator, webSiteId).getCatalogAltUrlBuilder();
        }
    }

    /*
     * *****************************************************
     * Getters and config
     * *****************************************************
     */

    /**
     * Maps locale to name and vice-versa - optimization to avoid ResourceBundle.
     * FIXME?: this might not 100% emulate the properties map yet... but close.
     */
    protected static class LocalizedName {
        private final String defaultValue;
        private final Map<String, String> localeValueMap;
        private final Map<String, Locale> valueLocaleMap;
        
        public LocalizedName(Map<String, String> localeValueMap, Locale defaultLocale) {
            this.localeValueMap = new HashMap<>(localeValueMap);
            Map<String, Locale> valueLocaleMap = new HashMap<>();
            for(Map.Entry<String, String> entry : localeValueMap.entrySet()) {
                // SPECIAL: duplicates *could* be possible - keep the shortest locale name (most generic)
                // for the string-to-locale mapping
                Locale prevLocale = valueLocaleMap.get(entry.getValue());
                Locale nextLocale = UtilMisc.parseLocale(entry.getKey());
                if (prevLocale != null) {
                    if (nextLocale.toString().length() < prevLocale.toString().length()) {
                        valueLocaleMap.put(entry.getValue(), nextLocale);
                    }
                } else {
                    valueLocaleMap.put(entry.getValue(), nextLocale);
                }
            }
            this.valueLocaleMap = valueLocaleMap;
            this.defaultValue = getNameForLocale(defaultLocale);
        }
        
        public static LocalizedName getNormalizedFromProperties(String resource, String name) {
            // FIXME: NOT GUARANTEED PERFECT MAPPING!!
            Map<String, String> localeValueMap = new HashMap<>();
            for(Locale locale: UtilMisc.availableLocales()) {
                ResourceBundle bundle = null;
                try {
                    bundle = UtilProperties.getResourceBundle(resource, locale);
                } catch(Exception e) {
                    continue; // when locale is not there, throws IllegalArgumentException
                }
                if (bundle == null || !bundle.containsKey(name)) continue;
                String value = bundle.getString(name);
                if (value == null) continue;
                value = value.trim();
                if (value.isEmpty()) continue;
                localeValueMap.put(locale.toString(), value);
            }
            return new LocalizedName(localeValueMap, UtilProperties.getFallbackLocale());
        }
        
        public Locale getLocaleForName(String name) {
            return valueLocaleMap.get(name);
        }
        
        public String getNameForLocale(Locale locale) {
            String value = localeValueMap.get(locale.toString());
            if (value == null) value = localeValueMap.get(normalizeLocaleStr(locale));
            return value;
        }
        
        public String getNameForLocaleOrDefault(Locale locale) {
            if (locale == null) return defaultValue;
            String value = getNameForLocale(locale);
            if (value == null) value = defaultValue;
            return value;
        }
        
        public static String normalizeLocaleStr(Locale locale) {
            return locale.getLanguage();
        }
    }
    
    
    @Deprecated
    protected Locale getDefaultLocale() {
        return Locale.getDefault();
    }

    public String getConfigResourceName() {
        return configResourceName;
    }

    protected LocalizedName getProductPathName() {
        LocalizedName productPathName = this.productPathName;
        if (productPathName == null) {
            productPathName = LocalizedName.getNormalizedFromProperties(getConfigResourceName(), "SeoConfigPathNameProduct");
            this.productPathName = productPathName;
        }
        return productPathName;
    }
    
    public String getProductServletPathName(Locale locale) {
        return getProductPathName().getNameForLocaleOrDefault(locale);
        //return UtilProperties.getMessage(getConfigResourceName(), "SeoConfigPathNameProduct", locale);
    }

    public String getProductServletPath(Locale locale) {
        return "/" + getProductServletPathName(locale);
    }
    
    public Locale getProductServletPathNameLocale(String pathName) {
        return getProductPathName().getLocaleForName(pathName);
    }

    protected LocalizedName getCategoryPathName() {
        LocalizedName categoryPathName = this.categoryPathName;
        if (categoryPathName == null) {
            categoryPathName = LocalizedName.getNormalizedFromProperties(getConfigResourceName(), "SeoConfigPathNameCategory");
            this.categoryPathName = categoryPathName;
        }
        return categoryPathName;
    }
    
    public String getCategoryServletPathName(Locale locale) {
        return getCategoryPathName().getNameForLocaleOrDefault(locale);
        //return UtilProperties.getMessage(getConfigResourceName(), "SeoConfigPathNameCategory", locale);
    }

    @Deprecated
    public String getCategoryServletPathName() {
        return getCategoryServletPathName(getDefaultLocale());
    }

    public String getCategoryServletPath(Locale locale) {
        return "/" + getCategoryServletPathName(locale);
    }

    @Deprecated
    public String getCategoryServletPath() {
        return getCategoryServletPath(getDefaultLocale());
    }

    public Locale getCategoryServletPathNameLocale(String pathName) {
        return getCategoryPathName().getLocaleForName(pathName);
    }
    
    public String getUrlSuffix() {
        return urlSuffix;
    }

//    public String extractProductServletPrefix(String path) {
//        throw new UnsupportedOperationException(); // TODO: if needed
//    }
//
//    public String extractCategoryServletPrefix(String path) {
//        throw new UnsupportedOperationException(); // TODO: if needed
//    }
    
    /*
     * *****************************************************
     * High-level helper methods (for INTERNAL use)
     * *****************************************************
     */

    /**
     * Re-generates a link from SeoCatalogUrlInfo info.
     */
    public String makeCatalogLink(Delegator delegator, SeoCatalogUrlInfo urlInfo, Locale locale) {

        // TODO: 2017

        return "";
    }

    /*
     * *****************************************************
     * Low-level URL building callbacks from makeCatalog[Alt]Link
     * *****************************************************
     */

    public CatalogUrlBuilder getCatalogUrlBuilder() {
        return catalogUrlBuilder;
    }

    public class SeoCatalogUrlBuilder extends CatalogUrlBuilder implements Serializable {
        @Override
        public String makeCatalogUrl(HttpServletRequest request, Locale locale, String productId, String currentCategoryId,
                String previousCategoryId) throws IOException {
            if (UtilValidate.isNotEmpty(productId)) {
                return makeProductUrl(request, locale, previousCategoryId, currentCategoryId, productId);
            } else {
                return makeCategoryUrl(request, locale, previousCategoryId, currentCategoryId, productId, null, null, null, null);
            }
            //return CatalogUrlBuilder.getDefaultBuilder().makeCatalogUrl(request, locale, productId, currentCategoryId, previousCategoryId);
        }

        @Override
        public String makeCatalogUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, String webSiteId, String contextPath, String currentCatalogId, List<String> crumb, String productId,
                String currentCategoryId, String previousCategoryId) throws IOException {
            if (UtilValidate.isNotEmpty(productId)) {
                return makeProductUrl(delegator, dispatcher, locale, crumb, webSiteId, contextPath, currentCatalogId, previousCategoryId, currentCategoryId, productId);
            } else {
                return makeCategoryUrl(delegator, dispatcher, locale, crumb, webSiteId, contextPath, currentCatalogId, previousCategoryId, currentCategoryId, productId, null, null, null, null);
            }
            //return CatalogUrlBuilder.getDefaultBuilder().makeCatalogUrl(delegator, dispatcher, locale, contextPath, crumb, productId, currentCategoryId, previousCategoryId);
        }
    }

    public CatalogAltUrlBuilder getCatalogAltUrlBuilder() {
        return catalogAltUrlBuilder;
    }

    public class SeoCatalogAltUrlBuilder extends CatalogAltUrlBuilder implements Serializable {
        @Override
        public String makeProductAltUrl(HttpServletRequest request, Locale locale, String previousCategoryId, String productCategoryId,
                String productId) throws IOException {
            return makeProductUrl(request, locale, previousCategoryId, productCategoryId, productId);
            //return CatalogAltUrlBuilder.getDefaultBuilder().makeProductAltUrl(request, locale, previousCategoryId, productCategoryId, productId);
        }

        @Override
        public String makeProductAltUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail,
                String webSiteId, String contextPath, String currentCatalogId, String previousCategoryId, String productCategoryId, String productId) throws IOException {
            return makeProductUrl(delegator, dispatcher, locale, trail, webSiteId, contextPath, currentCatalogId, previousCategoryId, productCategoryId, productId);
            //return CatalogAltUrlBuilder.getDefaultBuilder().makeProductAltUrl(delegator, dispatcher, locale, trail, contextPath, previousCategoryId, productCategoryId, productId);
        }

        @Override
        public String makeCategoryAltUrl(HttpServletRequest request, Locale locale, String previousCategoryId,
                String productCategoryId, String productId, String viewSize, String viewIndex, String viewSort,
                String searchString) throws IOException {
            return makeCategoryUrl(request, locale, previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString);
            //return CatalogAltUrlBuilder.getDefaultBuilder().makeCategoryAltUrl(request, locale, previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString);
        }

        @Override
        public String makeCategoryAltUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail,
                String webSiteId, String contextPath, String currentCatalogId, String previousCategoryId, String productCategoryId, String productId,
                String viewSize, String viewIndex, String viewSort, String searchString) throws IOException {
            return makeCategoryUrl(delegator, dispatcher, locale, trail, webSiteId, contextPath, currentCatalogId, previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString);
            //return CatalogAltUrlBuilder.getDefaultBuilder().makeCategoryAltUrl(delegator, dispatcher, locale, trail, contextPath, previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString);
        }
    }

    /*
     * *****************************************************
     * URL sanitizing
     * *****************************************************
     * These control how much happens before & after storage.
     */
    
    public CatalogAltUrlSanitizer getCatalogAltUrlSanitizer() {
        return catalogAltUrlSanitizer;
    }
    
    public class SeoCatalogAltUrlSanitizer extends CatalogAltUrlSanitizer {
        
        @Override
        public String convertNameToDbAltUrl(String name, Locale locale, ObjectType entityType) {
            name = SeoStringUtil.constructSeoName(name);

            // TODO: REVIEW
            name = SeoUrlUtil.replaceSpecialCharsUrl(name, SeoConfigUtil.getCharFilters());

            // TODO: REVIEW
            name = UrlServletHelper.invalidCharacter(name); // (stock ofbiz)
            
            if (entityType == ObjectType.PRODUCT) {
                name = SeoConfigUtil.limitProductNameLength(name);
            } else if (entityType == ObjectType.CATEGORY) {
                name = SeoConfigUtil.limitCategoryNameLength(name);
            }

            return name;
        }
        

        @Override
        public String convertIdToDbAltUrl(String id, Locale locale, ObjectType entityType) {
            // TODO: REVIEW: leaving this same as live for now... doubtful...
            return convertIdToLiveAltUrl(id, locale, entityType);
        }

        @Override
        public String sanitizeAltUrlFromDb(String altUrl, Locale locale, ObjectType entityType) {
            // WARN: due to content wrapper the locale might not be the one from the altUrl!!
            // may also be null
            
            if (altUrl == null) return "";
            
            // 2017: LEAVE THIS METHOD EMPTY - allows better DB queries if no post-processing.
            
            // TODO: REVIEW: REMOVED all post-db processing for now
            // - omitting could permit better DB queries
            // the reason this existed in the first place was because users can input garbage
            // through the UI, could could prevent in other ways...
            //altUrl = UrlServletHelper.invalidCharacter(altUrl); // (stock ofbiz)
            
            return altUrl;
        }

        @Override
        public String convertIdToLiveAltUrl(String id, Locale locale, ObjectType entityType) {

            // TODO: REVIEW: this is what the old Seo code did, but it will just not work in the filters...
            // People should not generate DB IDs with spaces
            //return id.trim().replaceAll(" ", SeoStringUtil.URL_HYPHEN);
            
            return id;
        }

    }
    
    /*
     * *****************************************************
     * URL building core
     * *****************************************************
     * Derived from CatalogUrlFilter methods of same names.
     * NOTE: The alt and non-alt SEO methods are unified to produce same output.
     */

    /**
     * Convert list of categoryIds to formatted alt url names.
     */
    private List<String> getCategoryUrlTrailNames(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail) {
        if (trail == null || trail.isEmpty()) return new ArrayList<>();
        List<String> catNames = new ArrayList<>(trail.size());
        for(String productCategoryId : trail) {
            if ("TOP".equals(productCategoryId)) continue; // TODO: REVIEW
            String catName = null;
            if (productCategoryId != null) {
                try {
                    GenericValue productCategory = EntityQuery.use(delegator).from("ProductCategory")
                                .where("productCategoryId", productCategoryId).cache(true).queryOne();
                    if (productCategory != null) {
                        String altUrl = CategoryContentWrapper.getProductCategoryContentAsText(productCategory, "ALTERNATIVE_URL", locale, dispatcher, "raw");
                        if (altUrl != null) {
                            // FIXME: effective locale might not be same as "locale" variable!
                            altUrl = getCatalogAltUrlSanitizer().sanitizeAltUrlFromDb(altUrl, locale, ObjectType.CATEGORY);
                            if (!altUrl.isEmpty()) {
                                catName = altUrl;
                                
                                // TODO: REVIEW
                                if (SeoConfigUtil.isCategoryNameAppendId()) {
                                    catName += SeoStringUtil.URL_HYPHEN + getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productCategoryId, locale, ObjectType.CATEGORY);
                                }
                            }
                        }
                    } else {
                        ; // NOTE: this is possible due to cache and delays and such
                    }
                } catch(Exception e) {
                    Debug.logError(e, "Seo: Cannot get category '" + productCategoryId + "' alt url", module);
                }
            }
            if (catName == null) {
                // fallback
                catName = getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productCategoryId, locale, ObjectType.CATEGORY);
            }
            
            catNames.add(catName);
        }
        return catNames;
    }

    /**
     * This creates a full trail to the given category using hints from the incoming trail to select best.
     * FIXME: 2017: currently this is ignoring the hintTrail.
     */
    private List<String> makeFullCategoryUrlTrail(Delegator delegator, List<String> hintTrail, GenericValue productCategory, String webSiteId, String currentCatalogId) {

        // TODO: missing hints from hintTrail! 
        // browsing is limited until this is implemented!
        
        if (currentCatalogId == null) {
            currentCatalogId = getWebsiteTopCatalog(delegator, webSiteId);
            if (currentCatalogId == null) {
                Debug.logError("Seo: makeFullCategoryUrlTrail: cannot determine top catalog for webSiteId '" + webSiteId + "'; can't select best trail", module);
                return new ArrayList<>();
            }
        }
        
        // category URLs must allow multiple top categories
//        String topCategoryId = CatalogWorker.getCatalogTopCategoryId(delegator, currentCatalogId);
//        if (topCategoryId == null) {
//            Debug.logWarning("Seo: No top category found for catalog '" + currentCatalogId + "'; can't select best trail", module);
//            return new ArrayList<>();
//        }

        Set<String> topCategoryIds = getProdCatalogCategoryIds(delegator, currentCatalogId, null);
        
        List<List<String>> trails = getCategoryRollupTrails(delegator, productCategory.getString("productCategoryId"), topCategoryIds);
        return findBestTopCatTrailForNewUrl(delegator, hintTrail, trails);
    }

    /**
     * This creates a full trail to the given product using hints from the incoming trail to select best.
     * FIXME: 2017: currently this is ignoring the hintTrail.
     */
    private List<String> makeFullProductUrlTrail(Delegator delegator, List<String> hintTrail, GenericValue product, String webSiteId, String currentCatalogId) {
        List<String> newTrail = new ArrayList<>();
        
        // TODO: missing hints from hintTrail! 
        // browsing is limited until this is implemented!
        
        if (product != null) {
            
            if (currentCatalogId == null) {
                currentCatalogId = getWebsiteTopCatalog(delegator, webSiteId);
                if (currentCatalogId == null) {
                    Debug.logError("Seo: makeFullProductUrlTrail: cannot determine top catalog for webSiteId '" + webSiteId + "'; can't select best trail", module);
                    return new ArrayList<>();
                }
            }
            
            String topCategoryId = CatalogWorker.getCatalogTopCategoryId(delegator, currentCatalogId);
            if (topCategoryId == null) {
                Debug.logWarning("Seo: No top category found for catalog '" + currentCatalogId + "'; can't select best trail", module);
                return new ArrayList<>();
            }
            Set<String> topCategoryIds = new HashSet<>();
            topCategoryIds.add(topCategoryId);
            
            String primaryCatId = product.getString("primaryProductCategoryId");
            try {
                if (primaryCatId != null) {
                    List<List<String>> trails = getCategoryRollupTrails(delegator, primaryCatId, topCategoryIds);
                    if (trails.size() == 1) {
                        // if only one, it also assuredly leads to top category
                        return trails.get(0);
                    } else {
                        return findBestTopCatTrailForNewUrl(delegator, hintTrail, trails);
                    }
                } else {
                    List<GenericValue> prodCatMembers = EntityQuery.use(delegator).from("ProductCategoryMember")
                        .where("productId", product.getString("productId")).orderBy("-fromDate").filterByDate().queryList();
                    if (prodCatMembers.size() == 0) {
                        return new ArrayList<>();
                    } else {
                        List<List<String>> trails = null;
                        for(GenericValue prodCatMember : prodCatMembers) {
                            String productCategoryId = prodCatMember.getString("productCategoryId");
                            List<List<String>> memberTrails = getCategoryRollupTrails(delegator, productCategoryId, topCategoryIds);
                            if (trails == null) trails = memberTrails;
                            else trails.addAll(memberTrails);
                        }
                        if (trails.size() == 1) {
                            // if only one, it also assuredly leads to top category
                            return trails.get(0);
                        } else {
                            return findBestTopCatTrailForNewUrl(delegator, hintTrail, trails);
                        }
                    }
                }
            } catch(Exception e) {
                Debug.logError(e, "Seo: Error generating trail for product '" + product.getString("productId") + "': " + e.getMessage(), module);
                return new ArrayList<>();
            }
        }

        Collections.reverse(newTrail);
        return newTrail;
    }
    
    // TODO
    private List<String> findBestTopCatTrailForNewUrl(Delegator delegator, List<String> hintTrail, List<List<String>> trails) {
        
        // TODO: missing hints from hintTrail! 
        // browsing is limited until this is implemented!

        if (UtilValidate.isNotEmpty(trails)) {
            return trails.get(0);
        }
        
        return new ArrayList<>();
    }

    private String makeCategoryUrl(HttpServletRequest request, Locale locale, String previousCategoryId, String productCategoryId, String productId, String viewSize, String viewIndex, String viewSort, String searchString) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        List<String> trail = CategoryWorker.getTrail(request);
        return makeCategoryUrl(delegator, dispatcher, locale, trail, 
                WebSiteWorker.getWebSiteId(request), request.getContextPath(), CatalogWorker.getCurrentCatalogId(request),
                previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString);
    }

    /**
     * Make category url according to the configurations.
     */
    private String makeCategoryUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail, String webSiteId, String contextPath, String currentCatalogId, String previousCategoryId, String productCategoryId, String productId, String viewSize, String viewIndex, String viewSort, String searchString) {
        GenericValue productCategory;
        try {
            productCategory = EntityQuery.use(delegator).from("ProductCategory").where("productCategoryId", productCategoryId).cache().queryOne();
            if (productCategory == null) {
                Debug.logWarning("Seo: Category not found: Cannot create category's URL for: " + productCategoryId, module);
                return null;
            }
        } catch (GenericEntityException e) {
            Debug.logWarning(e, "Seo: Cannot create category's URL for: " + productCategoryId, module);
            return null;
        }

        StringBuilder urlBuilder = new StringBuilder();
        if (contextPath != null) {
            urlBuilder.append(contextPath);
        }

        boolean explicitCategoryRequest = false;
        if (!(SeoConfigUtil.isHandleImplicitRequests() && SeoConfigUtil.isGenerateImplicitCategoryUrl())) {
            explicitCategoryRequest = true;
        }
        if (explicitCategoryRequest) {
            if (urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
                urlBuilder.append("/");
            }
            urlBuilder.append(getCategoryServletPathName(locale));
        }
        
        // SCIPIO: refine and append trail
        trail = CategoryWorker.adjustTrail(trail, productCategoryId, previousCategoryId);
        trail = makeFullCategoryUrlTrail(delegator, trail, productCategory, webSiteId, currentCatalogId);
        List<String> trailNames = getCategoryUrlTrailNames(delegator, dispatcher, locale, trail);
        // NOTE: this loop includes the productCategoryId itself
        for(String trailName : trailNames) {
            if (urlBuilder.length() == 0 || urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
                urlBuilder.append("/");
            }
            urlBuilder.append(trailName);
        }
        
        // 2017: TRY to omit this
        //if (!explicitCategoryRequest) {
        //    urlBuilder.append("-c");
        //}
        
        if (!urlBuilder.toString().endsWith("/") && UtilValidate.isNotEmpty(getUrlSuffix())) {
            urlBuilder.append(getUrlSuffix());
        }
        
        // append view index
        if (UtilValidate.isNotEmpty(viewIndex)) {
            if (!urlBuilder.toString().endsWith("?") && !urlBuilder.toString().endsWith("&")) {
                urlBuilder.append("?");
            }
            urlBuilder.append("viewIndex=" + viewIndex + "&");
        }
        // append view size
        if (UtilValidate.isNotEmpty(viewSize)) {
            if (!urlBuilder.toString().endsWith("?") && !urlBuilder.toString().endsWith("&")) {
                urlBuilder.append("?");
            }
            urlBuilder.append("viewSize=" + viewSize + "&");
        }
        // append view sort
        if (UtilValidate.isNotEmpty(viewSort)) {
            if (!urlBuilder.toString().endsWith("?") && !urlBuilder.toString().endsWith("&")) {
                urlBuilder.append("?");
            }
            urlBuilder.append("viewSort=" + viewSort + "&");
        }
        // append search string
        if (UtilValidate.isNotEmpty(searchString)) {
            if (!urlBuilder.toString().endsWith("?") && !urlBuilder.toString().endsWith("&")) {
                urlBuilder.append("?");
            }
            urlBuilder.append("searchString=" + searchString + "&");
        }
        if (urlBuilder.toString().endsWith("&")) {
            return urlBuilder.toString().substring(0, urlBuilder.toString().length()-1);
        }

        return urlBuilder.toString();
    }

    private String makeProductUrl(HttpServletRequest request, Locale locale, String previousCategoryId, String productCategoryId, String productId) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        List<String> trail = CategoryWorker.getTrail(request);
        
        return makeProductUrl(delegator, dispatcher, locale, trail, 
                WebSiteWorker.getWebSiteId(request), request.getContextPath(), CatalogWorker.getCurrentCatalogId(request),
                previousCategoryId, productCategoryId, productId);
    }

    /**
     * Make product url according to the configurations.
     * <p>
     * SCIPIO: Modified for bugfixes and lookup via cache products map (TODO: REVIEW)
     */
    private String makeProductUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail, String webSiteId, String contextPath, String currentCatalogId, String previousCategoryId, String productCategoryId, String productId) {
        GenericValue product;
        try {
            product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache().queryOne();
        } catch (GenericEntityException e) {
            Debug.logWarning(e, "Seo: Cannot create product's URL for: " + productId, module);
            return null;
        }

        StringBuilder urlBuilder = new StringBuilder();
        if (contextPath != null) {
            urlBuilder.append(contextPath);
        }

        List<String> trailNames;
        if (!SeoConfigUtil.isCategoryNameEnabled()) {
            // no need for trail
            trail = Collections.emptyList();
            trailNames = Collections.emptyList();
        } else {
            if (UtilValidate.isNotEmpty(productCategoryId)) {
                trail = CategoryWorker.adjustTrail(trail, productCategoryId, previousCategoryId);
            }
            trail = makeFullProductUrlTrail(delegator, trail, product, webSiteId, currentCatalogId);
            trailNames = getCategoryUrlTrailNames(delegator, dispatcher, locale, trail);
        }
        
        boolean explicitProductRequest = false;
        if (!(SeoConfigUtil.isHandleImplicitRequests() && SeoConfigUtil.isGenerateImplicitProductUrl())) {
            explicitProductRequest = true;
        } else if (trailNames.size() == 0) {
            // 2017: TODO: REVIEW:
            // SCIPIO: We only support(ed) omitting product request if a category name is also present
            explicitProductRequest = true;
        }
        if (explicitProductRequest) {
            if (urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
                urlBuilder.append("/");
            }
            urlBuilder.append(getProductServletPathName(locale));
        }
        
        // append category names
        for(String trailName : trailNames) {
            if (urlBuilder.length() == 0 || urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
                urlBuilder.append("/");
            }
            urlBuilder.append(trailName);
        }

        // 2017-11-08: NOT SUPPORTED: could only theoretically work if chose different character than hyphen
        //if (!trailNames.isEmpty() && !SeoConfigUtil.isCategoryNameSeparatePathElem()) {
        //    urlBuilder.append(SeoStringUtil.URL_HYPHEN);
        //} else {
        if (urlBuilder.length() == 0 || urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
            urlBuilder.append("/");
        }
        //}
        
        // append product name
        // FIXME: effective locale might not be same as "locale" variable!
        String alternativeUrl = getCatalogAltUrlSanitizer().sanitizeAltUrlFromDb(ProductContentWrapper.getProductContentAsText(product, "ALTERNATIVE_URL", locale, dispatcher, "raw"), 
                locale, ObjectType.PRODUCT);
        if (UtilValidate.isNotEmpty(alternativeUrl)) {
            urlBuilder.append(alternativeUrl);
            
            if (SeoConfigUtil.isProductNameAppendId() && UtilValidate.isNotEmpty(productId)) {
                urlBuilder.append(SeoStringUtil.URL_HYPHEN);
                urlBuilder.append(getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productId, locale, ObjectType.PRODUCT));
            }
        } else {
            // FALLBACK ONLY
            urlBuilder.append(getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productId, locale, ObjectType.PRODUCT));
        }
        
        // 2017: TRY to omit this
        //if (!explicitProductRequest) {
        //    urlBuilder.append("-p");
        //}
        
        if (!urlBuilder.toString().endsWith("/") && UtilValidate.isNotEmpty(getUrlSuffix())) {
            urlBuilder.append(getUrlSuffix());
        }
        
        return urlBuilder.toString();
    }

    /*
     * *****************************************************
     * URL parsing
     * *****************************************************
     */

    /**
     * Returned whenever we find a URL that appears to be an SEO URL, even if
     * the request is not for a valid product or category.
     */
    public static class SeoCatalogUrlInfo implements Serializable {
        private final String origPath;
        private final String productId;
        private final String categoryId;
        private final boolean explicitProductRequest;
        private final boolean explicitCategoryRequest;
        private final List<String> pathCategoryIds;
        private final Locale locale; // the locale matched when URL is parsed (best-effort only)
        
        public SeoCatalogUrlInfo(String origPath, String productId, String categoryId, 
                boolean explicitProductRequest, boolean explicitCategoryRequest, 
                List<String> pathCategoryIds, Locale locale) {
            this.origPath = origPath;
            this.productId = productId;
            this.categoryId = categoryId;
            this.explicitProductRequest = explicitProductRequest;
            this.explicitCategoryRequest = explicitCategoryRequest;
            this.pathCategoryIds = pathCategoryIds;
            this.locale = locale;
        }

        public String getProductId() { return productId; }
        public String getCategoryId() { return categoryId; }
        public String getOrigPath() { return origPath; }

        public boolean isExplicitProductRequest() { return explicitProductRequest; }
        public boolean isExplicitCategoryRequest() { return explicitCategoryRequest; }

        public boolean hasTarget() { return productId != null || categoryId != null; }
        public boolean isProductRequest() { return explicitProductRequest || getProductId() != null; }
        public boolean isCategoryRequest() { return explicitCategoryRequest || (getCategoryId() != null && getProductId() == null); }

        public List<String> getPathCategoryIds() { return pathCategoryIds; }
        public Locale getLocale() { return locale; }
        
        /**
         * Predicts if the request is likely to cause an error at forward.
         * NOTE: Do not use this to prevent forward.
         */
        public boolean isValidRequest() {
            return (explicitProductRequest && getProductId() != null) ||
                    (explicitCategoryRequest && (getCategoryId() != null && getProductId() == null)) ||
                    getProductId() != null || getCategoryId() != null;
        }

    }

    /**
     * Checks if the path (starting after context path) appears to be an SEO
     * URL and returns its info if so.
     *
     * @param path path starting from context path, in other words servlet path + path info
     */
    public SeoCatalogUrlInfo matchInboundSeoCatalogUrl(Delegator delegator, String path, String contextPath, String webSiteId, String currentCatalogId) {

        String origPath = path;
        String pathInfo = path;
 
        // path must start with a slash, and remove it
        if (!pathInfo.startsWith("/")) return null;
        pathInfo = pathInfo.substring(1);
        
        // path may require suffix
        String urlSuffix = getUrlSuffix();
        if (UtilValidate.isNotEmpty(urlSuffix)) {
            if (pathInfo.endsWith(urlSuffix)) {
                pathInfo = pathInfo.substring(0, pathInfo.length() - pathInfo.length());
            } else {
                return null;
            }
        }
        
        // if path ends with slash (for whatever reason it was added), remove it
        if (pathInfo.endsWith("/")) pathInfo = pathInfo.substring(0, pathInfo.length() - 1);
        
        boolean explicitCategoryRequest = false;
        boolean explicitProductRequest = false;

        // split path into alt-url parts
        List<String> pathElements = StringUtil.split(pathInfo, "/");
        if (UtilValidate.isEmpty(pathElements)) {
            return null;
        }

        String productId = null;
        String categoryId = null;
        List<String> pathCategoryIds = null;
        Locale matchedLocale = null; // the locale the URL appears to be
        
        // basic match for explicit requests and check if implicit allowed
        matchedLocale = getProductServletPathNameLocale(pathElements.get(0));
        if (matchedLocale != null) {
            explicitProductRequest = true;
            pathElements.remove(0);
        } else {
            matchedLocale = getCategoryServletPathNameLocale(pathElements.get(0));
            if (matchedLocale != null) {
                explicitCategoryRequest = true;
                pathElements.remove(0);
            } else {
                if (!SeoConfigUtil.isHandleImplicitRequests()) {
                    return null;
                }
            }
        }

        String lastPathElem = null;
        if (pathElements.size() > 0) lastPathElem = pathElements.get(pathElements.size() - 1);

        if (UtilValidate.isNotEmpty(lastPathElem)) {
            pathElements.remove(pathElements.size() - 1);
            
            if (currentCatalogId == null) {
                currentCatalogId = getWebsiteTopCatalog(delegator, webSiteId);
                if (currentCatalogId == null) {
                    Debug.logError("Seo: matchInboundSeoCatalogUrl: cannot determine top catalog for webSiteId '" + webSiteId + "'; can't select best trail", module);
                }
            }
            String topCategoryId = null;
            if (currentCatalogId != null) topCategoryId = CatalogWorker.getCatalogTopCategoryId(delegator, currentCatalogId);
            Set<String> topCategoryIds = new HashSet<>();
            if (topCategoryId == null) {
                Debug.logWarning("Seo: matchInboundSeoCatalogUrl: cannot determine top category for webSiteId '" + webSiteId + "'; can't select best trail", module);
            } else {
                topCategoryIds.add(topCategoryId);
            }
            
            try {
                if (explicitProductRequest) { // EXPLICIT PRODUCT
                    Map<String, AltUrlPartInfo> productMatches = extractCandidateAltUrlProductIdCached(delegator, lastPathElem, true);
                    if (productMatches.size() > 0) {
                        AltUrlPartInfo match = AltUrlPartInfo.getSingleOrExact(productMatches);
                        if (match != null) {

                            List<List<String>> possibleTrails = getProductRollupTrails(delegator, match.getId(), topCategoryIds);
                            if (possibleTrails.size() > 0) {
                                if (possibleTrails.size() == 1) {
                                    // only one trail possible, so can ignore path
                                    pathCategoryIds = possibleTrails.get(0);
                                } else {
                                    if (pathElements.isEmpty()) {
                                        // no trail hint, so just select the first...
                                        pathCategoryIds = possibleTrails.get(0);
                                    } else {
                                        // find the trail closest to the passed path elems
                                        List<Map<String, AltUrlPartInfo>> resolvedPathElems = extractCandidateAltUrlCategoryIdsCached(delegator, pathElements, true);
                                        pathCategoryIds = findPathElemBestMatchingTrail(delegator, possibleTrails, resolvedPathElems);
                                    }
                                }
                            }
                            // TODO: REVIEW: if find no trail to store, should we still allow productId to go through??
                            productId = match.getId();
                        } else {
                            // CONFLICT: multiple product - only trails can disambiguate now
                            
                            
                            // TODO
                        }
                    }
                } else if (explicitCategoryRequest) { // EXPLICIT CATEGORY
                    
                    Set<String> topCategoryIdsForCategory = getProdCatalogCategoryIds(delegator, currentCatalogId, null);
                    
                    Map<String, AltUrlPartInfo> categoryMatches = extractCandidateAltUrlCategoryIdCached(delegator, lastPathElem, true);
                    if (categoryMatches.size() > 0) {
                        AltUrlPartInfo match = AltUrlPartInfo.getSingleOrExact(categoryMatches);
                        if (match != null) {
                            List<List<String>> possibleTrails = getCategoryRollupTrails(delegator, match.getId(), topCategoryIdsForCategory);
                            if (possibleTrails.size() > 0) {
                                if (possibleTrails.size() == 1) {
                                    // only one trail possible, so can ignore path
                                    pathCategoryIds = possibleTrails.get(0);
                                } else {
                                    if (pathElements.isEmpty()) {
                                        // no trail hint, so just select the first...
                                        pathCategoryIds = possibleTrails.get(0);
                                    } else {
                                        // find the trail closest to the passed path elems
                                        List<Map<String, AltUrlPartInfo>> resolvedPathElems = extractCandidateAltUrlCategoryIdsCached(delegator, pathElements, true);
                                        resolvedPathElems.add(categoryMatches);
                                        pathCategoryIds = findPathElemBestMatchingTrail(delegator, possibleTrails, resolvedPathElems);
                                    }
                                }
                            }
                            // TODO: REVIEW: if find no trail to store, should we still allow categoryId to go through??
                            categoryId = match.getId();
                            if (pathCategoryIds != null && pathCategoryIds.size() > 0) {
                                if (categoryId.equals(pathCategoryIds.get(pathCategoryIds.size() - 1))) {
                                    pathCategoryIds.remove(pathCategoryIds.size() - 1);
                                }
                            }
                        } else {
                            // CONFLICT: multiple category - only trails can disambiguate now
                            
                            // TODO
                            
                        }
                    }
                } else { // IMPLICIT
                    boolean allowIdOnly = !SeoConfigUtil.isImplicitRequestNameMatchesOnly();
                    Map<String, AltUrlPartInfo> productMatches = extractCandidateAltUrlProductIdCached(delegator, lastPathElem, allowIdOnly);
                    if (productMatches.size() > 0) {
                        AltUrlPartInfo match = AltUrlPartInfo.getSingleOrExact(productMatches);
                        if (match != null) {
                            
                        } else {
                            // TODO
                        }
                    } else {
                        Map<String, AltUrlPartInfo> categoryMatches = extractCandidateAltUrlCategoryIdCached(delegator, lastPathElem, allowIdOnly);
                        if (categoryMatches.size() > 0) {
                            AltUrlPartInfo match = AltUrlPartInfo.getSingleOrExact(categoryMatches);
                            if (match != null) {
                                
                            } else {
                                // TODO
                            }
                        }
                    }
                    
                    // TODO
                    
                    return null;
                }
            } catch(Exception e) {
                Debug.logError(e, "Seo: matchInboundSeoCatalogUrl: Error parsing catalog URL " + origPath + ": " + e.getMessage(), module);
                return null;
            }
        }

        if (explicitProductRequest || explicitCategoryRequest || productId != null || categoryId != null) {
            if (productId != null && categoryId == null && UtilValidate.isNotEmpty(pathCategoryIds)) {
                categoryId = pathCategoryIds.get(pathCategoryIds.size() - 1);
            }
            return new SeoCatalogUrlInfo(origPath, productId, categoryId, 
                    explicitProductRequest, explicitCategoryRequest, 
                    pathCategoryIds != null ? pathCategoryIds : new ArrayList<String>(), 
                    matchedLocale);
        } else {
            return null;
        }
    }

    /**
     * Uses the passed path elements (resolved) to try to select the best of the possible trails.
     * Returns null only if nothing matches at all.
     * BEST-EFFORT.
     * <p>
     * TODO: REVIEW: the possibility of each path elem matching multiple category IDs makes this extremely
     * complicated; so we ignore the specific implications and just match as much as possible.
     * <p>
     * For a trail to be selected, it must "end with" the pathElems; given this, the best trail is one that 
     * has smallest length.
     */
    protected List<String> findPathElemBestMatchingTrail(Delegator delegator, List<List<String>> possibleTrails, List<Map<String, AltUrlPartInfo>> pathElems) throws GenericEntityException {
        if (pathElems.isEmpty()) return null;
        List<String> bestTrail = null;
        for(List<String> trail : possibleTrails) {
            if (pathElems.size() > trail.size()) continue; // sure to fail
            
            ListIterator<Map<String, AltUrlPartInfo>> pit = pathElems.listIterator(pathElems.size());
            ListIterator<String> tit = trail.listIterator(trail.size());
            boolean matched = true;
            while(matched && pit.hasPrevious()) {
                Map<String, AltUrlPartInfo> urlInfos = pit.previous();
                String categoryId = tit.previous();
                
                // TODO: REVIEW: simplistic check, ignores exact vs name-only matches, but should work
                if (!urlInfos.containsKey(categoryId)) {
                    matched = false;
                }
            }
            if (matched) {
                if (trail.size() == pathElems.size()) { // ideal case
                    bestTrail = trail;
                    break;
                } else if (bestTrail == null || trail.size() < bestTrail.size()) { // smaller = better
                    bestTrail = trail;
                }
            }
        }
        return bestTrail;
    }


    /*
     * *****************************************************
     * Alternative URL individual path elem part parsing
     * *****************************************************
     */

    /**
     * A single alt url segment info (product or catalog).
     * Incoming full alt URL with categories becomes a list of these.
     */
    public static class AltUrlPartInfo implements Serializable {
        private final boolean exact;
        private final boolean idOnlyMatch;
        private final String id;
        private final String name;
        private final String localeString;
        protected AltUrlPartInfo(boolean exact, boolean idOnlyMatch, String id, String name, String localeString) {
            this.exact = exact;
            this.idOnlyMatch = idOnlyMatch;
            this.id = id;
            this.name = name;
            this.localeString = localeString;
        }
        public boolean isExact() { return exact; }
        public boolean isIdOnlyMatch() { return idOnlyMatch; }
        public String getId() { return id; }
        public String getName() { return name; }
        public String getLocaleString() { return localeString; }
        
        public static AltUrlPartInfo getSingleOrExact(Map<String, AltUrlPartInfo> results) {
            if (results.size() == 1) {
                return results.values().iterator().next();
            } else {
                for(AltUrlPartInfo info : results.values()) {
                    if (info.isExact()) return info;
                }
                return null;
            }
        }
    }
    
    public Map<String, AltUrlPartInfo> extractCandidateAltUrlProductIdCached(Delegator delegator, String altUrl, boolean allowIdOnly) throws GenericEntityException {
        // FIXME?: questionable use of cache; allowIdOnly as part of key may bloat it
        String key = altUrl + "::" + (allowIdOnly ? "Y" : "N");
        Map<String, AltUrlPartInfo> results = productAltUrlPartInfoCache.get(key);
        if (results == null) {
            boolean exactOnly = false; // TODO?: could be inferred based on SeoConfig?
            boolean singleExactOnly = true; // NOTE: there is a 0.001% chance of multiple exact matches; ignoring for now

            results = extractCandidateAltUrlProductId(delegator, altUrl, exactOnly, singleExactOnly, allowIdOnly);
            
            // NOTE: currently, only storing in cache if has match... 
            // this is tradeoff of memory vs misses (risky to allow empty due to incoming from public)
            if (!results.isEmpty()) {
                results = Collections.unmodifiableMap(results);
                productAltUrlPartInfoCache.put(key, results);
            }
        }
        return results;
    }
    
    /**
     * SCIPIO: Tries to match an alt URL path element to a product.
     * Heavily modified logic from CatalogUrlFilter.
     * <p>
     * TODO: this may need a server-side map cache (with localization support)
     * OR a reverse LIKE (but this may lead to uncacheable results).
     * <p>
     * Added 2017-11-08.
     */
    public Map<String, AltUrlPartInfo> extractCandidateAltUrlProductId(Delegator delegator, String altUrl, boolean exactOnly, boolean singleExactOnly, boolean allowIdOnly) throws GenericEntityException {
        Map<String, AltUrlPartInfo> results = new HashMap<>();
        AltUrlPartInfo exactResult = null;
        
        // SCIPIO: this is a new filter that narrows down results from DB, which otherwise may be huge.
        EntityCondition matchTextIdCond = makeAltUrlTextIdMatchCombinations(altUrl, "productId", "textData", exactOnly);
        if (matchTextIdCond == null) return results;
        
        EntityCondition contentTypeIdCond = EntityCondition.makeCondition("productContentTypeId", "ALTERNATIVE_URL");
        Timestamp moment = UtilDateTime.nowTimestamp();
        List<EntityCondition> condList;
        
        // Search for localized alt urls
        condList = new ArrayList<>();
        condList.add(contentTypeIdCond);
        condList.add(EntityCondition.makeCondition("contentAssocTypeId", "ALTERNATE_LOCALE"));
        if (matchTextIdCond != null) condList.add(matchTextIdCond);
        List<GenericValue> productContentInfos = EntityQuery.use(delegator).from("ProductContentAssocAndElecTextShort")
                .where(condList).select("productId", "textData", "localeString")
                .filterByDate(moment).filterByDate(moment, "caFromDate", "caThruDate")
                .orderBy("-fromDate", "-caFromDate").cache(true).queryList();
        exactResult = findExtractId(altUrl, productContentInfos, "productId", ObjectType.PRODUCT, exactOnly, singleExactOnly, results);
        if (exactResult != null && exactResult.isExact()) {
            return results;
        }

        // Search for non-localized alt urls
        condList = new ArrayList<>();
        condList.add(contentTypeIdCond);
        if (matchTextIdCond != null) condList.add(matchTextIdCond);
        productContentInfos = EntityQuery.use(delegator).from("ProductContentAndElecTextShort")
                .where(condList).select("productId", "textData", "localeString")
                .filterByDate(moment)
                .orderBy("-fromDate").cache(true).filterByDate().queryList();
        exactResult = findExtractId(altUrl, productContentInfos, "productId", ObjectType.PRODUCT, exactOnly, singleExactOnly, results);
        if (exactResult != null && exactResult.isExact()) {
            return results;
        }
        
        if (allowIdOnly) {
            GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", altUrl).cache(true).queryOne();
            if (product != null) {
                String productId = product.getString("productId");
                // this case has higher prio over non-exact match, but lower prio than alt url exact match
                AltUrlPartInfo prevMatch = results.get(productId);
                if (prevMatch == null || !prevMatch.isExact()) {
                    results.put(productId, new AltUrlPartInfo(true, true, productId, altUrl, null));
                }
            }
        }
        
        return results;
    }
    
    public Map<String, AltUrlPartInfo> extractCandidateAltUrlCategoryIdCached(Delegator delegator, String altUrl, boolean allowIdOnly) throws GenericEntityException {
        // FIXME?: questionable use of cache; allowIdOnly as part of key may bloat it
        String key = altUrl + "::" + (allowIdOnly ? "Y" : "N");
        
        Map<String, AltUrlPartInfo> results = categoryAltUrlPartInfoCache.get(key);
        if (results == null) {
            boolean exactOnly = false; // TODO?: could be inferred based on SeoConfig?
            boolean singleExactOnly = true; // NOTE: there is a 0.001% chance of multiple exact matches; ignoring for now
            
            results = extractCandidateAltUrlCategoryId(delegator, altUrl, exactOnly, singleExactOnly, allowIdOnly);
            
            // NOTE: currently, only storing in cache if has match... 
            // this is tradeoff of memory vs misses (risky to allow empty due to incoming from public)
            if (!results.isEmpty()) {
                results = Collections.unmodifiableMap(results);
                categoryAltUrlPartInfoCache.put(key, results);
            }
        }
        return results;
    }
    
    public List<Map<String, AltUrlPartInfo>> extractCandidateAltUrlCategoryIdsCached(Delegator delegator, Collection<String> altUrls, boolean allowIdOnly) throws GenericEntityException {
        List<Map<String, AltUrlPartInfo>> result = new ArrayList<>(altUrls.size());
        for(String altUrl : altUrls) {
            result.add(extractCandidateAltUrlCategoryIdCached(delegator, altUrl, allowIdOnly));
        }
        return result;
    }
    
    /**
     * SCIPIO: Tries to match an alt URL path element to a category.
     * Heavily modified logic from CatalogUrlFilter.
     * <p>
     * TODO: this may need a server-side map cache (with localization support)
     * OR a reverse LIKE (but this may lead to uncacheable results).
     * <p>
     * Added 2017-11-07.
     */
    public Map<String, AltUrlPartInfo> extractCandidateAltUrlCategoryId(Delegator delegator, String altUrl, boolean exactOnly, boolean singleExactOnly, boolean allowIdOnly) throws GenericEntityException {
        Map<String, AltUrlPartInfo> results = new HashMap<>();
        AltUrlPartInfo exactResult = null;
        
        // SCIPIO: this is a new filter that narrows down results from DB, which otherwise may be huge.
        EntityCondition matchTextIdCond = makeAltUrlTextIdMatchCombinations(altUrl, "productCategoryId", "textData", exactOnly);
        if (matchTextIdCond == null) return results;
        
        EntityCondition contentTypeIdCond = EntityCondition.makeCondition("prodCatContentTypeId", "ALTERNATIVE_URL");
        Timestamp moment = UtilDateTime.nowTimestamp();
        List<EntityCondition> condList;
        
        // Search for localized alt urls
        condList = new ArrayList<>();
        condList.add(contentTypeIdCond);
        condList.add(EntityCondition.makeCondition("contentAssocTypeId", "ALTERNATE_LOCALE"));
        if (matchTextIdCond != null) condList.add(matchTextIdCond);
        List<GenericValue> productCategoryContentInfos = EntityQuery.use(delegator).from("ProductCategoryContentAssocAndElecTextShort")
                .where(condList).select("productCategoryId", "textData", "localeString")
                .filterByDate(moment).filterByDate(moment, "caFromDate", "caThruDate")
                .orderBy("-fromDate", "-caFromDate").cache(true).queryList();
        exactResult = findExtractId(altUrl, productCategoryContentInfos, "productCategoryId", ObjectType.CATEGORY, exactOnly, singleExactOnly, results);
        if (exactResult != null && exactResult.isExact()) {
            return results;
        }
        
        // Search for non-localized alt urls
        condList = new ArrayList<>();
        condList.add(contentTypeIdCond);
        if (matchTextIdCond != null) condList.add(matchTextIdCond);
        productCategoryContentInfos = EntityQuery.use(delegator).from("ProductCategoryContentAndElecTextShort")
                .where(condList).select("productCategoryId", "textData", "localeString")
                .filterByDate(moment)
                .orderBy("-fromDate").cache(true).queryList();
        exactResult = findExtractId(altUrl, productCategoryContentInfos, "productCategoryId", ObjectType.CATEGORY, exactOnly, singleExactOnly, results);
        if (exactResult != null && exactResult.isExact()) {
            return results;
        }
        
        if (allowIdOnly) {
            GenericValue productCategory = EntityQuery.use(delegator).from("ProductCategory").where("productCategoryId", altUrl).cache(true).queryOne();
            if (productCategory != null) {
                String productCategoryId = productCategory.getString("productCategoryId");
                // this case has higher prio over non-exact match, but lower prio than alt url exact match
                AltUrlPartInfo prevMatch = results.get(productCategoryId);
                if (prevMatch == null || !prevMatch.isExact()) {
                    results.put(productCategoryId, new AltUrlPartInfo(true, true, productCategoryId, altUrl, null));
                }
            }
        }
        
        return results;
    }

    /**
     * This splits altUrl by hyphen "-" and creates OR condition for all the possible combinations
     * of name and ID.
     */
    public static EntityCondition makeAltUrlTextIdMatchCombinations(String altUrl, String idField, String textField, boolean exactOnly) {
        List<EntityCondition> condList = new ArrayList<>();
        int lastIndex = altUrl.lastIndexOf('-');
        while(lastIndex > 0) {
            if (lastIndex >= (altUrl.length() - 1)) continue; // bad format, missing id
            String name = altUrl.substring(0, lastIndex);
            String id = altUrl.substring(lastIndex + 1);
            condList.add(EntityCondition.makeCondition(
                        EntityCondition.makeCondition(textField, EntityOperator.LIKE, name),
                        EntityOperator.AND,
                        EntityCondition.makeCondition(idField, id)));
            lastIndex = altUrl.lastIndexOf('-', lastIndex - 1);
        }
        if (!exactOnly) {
            // try one without any ID
            condList.add(EntityCondition.makeCondition(textField, EntityOperator.LIKE, altUrl));
        }
        return EntityCondition.makeCondition(condList, EntityOperator.OR);
    }
    
    
    /**
     * Finds matches and adds to results map. If an exact match is found, returns immediately
     * with the result. If no exact, returns null.
     * Based on CatalogUrlFilter iteration.
     */
    private AltUrlPartInfo findExtractId(String altUrl, List<GenericValue> values, String idField, 
            ObjectType entityType, boolean exactOnly, boolean singleExactOnly, Map<String, AltUrlPartInfo> results) {
        for (GenericValue value : values) {
            String textData = value.getString("textData");
            
            // TODO: REVIEW: if we didn't have to sanitize the DB records, it could
            // allow some types of LIKE DB queries, so try to do without sanitize if possible...
            // SCIPIO: NOTE: assuming DB data good as-is for now - this loop could become very slow...
            //getCatalogAltUrlSanitizer().sanitizeAltUrlFromDb(textData, null, entityType);
            //textData = UrlServletHelper.invalidCharacter(textData); 
            
            if (altUrl.startsWith(textData)) {
                String altUrlIdStr = altUrl.substring(textData.length());
                String valueId = value.getString(idField);
                if (altUrlIdStr.isEmpty()) {
                    if (!exactOnly) {
                        // id omitted - add to results, but don't stop looking
                        if (!results.containsKey(valueId)) { // don't replace in case exact match (don't need check)
                            results.put(valueId, new AltUrlPartInfo(false, false, valueId, textData, value.getString("localeString")));
                        }
                    }
                } else {
                    if (altUrlIdStr.startsWith("-")) { // should always be a hyphen here
                        altUrlIdStr = altUrlIdStr.substring(1);
                        if (altUrlIdStr.equalsIgnoreCase(valueId)) {
                            results.clear();
                            AltUrlPartInfo urlInfo = new AltUrlPartInfo(true, false, valueId, textData, value.getString("localeString"));
                            results.put(valueId, urlInfo);
                            if (singleExactOnly) return urlInfo;
                        }
                    }
                }
            }
        }
        return null;
    }
    
    /*
     * *****************************************************
     * Helpers
     * *****************************************************
     */

    protected static boolean pathStartsWithDir(String path, String dir) {
        // needs delimiter logic
        if (UtilValidate.isEmpty(path)) {
            return UtilValidate.isEmpty(dir);
        }
        if (path.length() > dir.length()) {
            if (dir.endsWith("/")) return path.startsWith(dir);
            else return path.startsWith(dir + "/");
        } else {
            return path.equals(dir);
        }
    }
    
    // TODO?: perhaps can cache in future...
    protected List<List<String>> getProductRollupTrails(Delegator delegator, String productId, Set<String> topCategoryIds) {
        return ProductWorker.getProductRollupTrails(delegator, productId, topCategoryIds, true);
    }
    
    // TODO?: perhaps can cache in future...
    protected List<List<String>> getCategoryRollupTrails(Delegator delegator, String productCategoryId, Set<String> topCategoryIds) {
        return CategoryWorker.getCategoryRollupTrails(delegator, productCategoryId, topCategoryIds, true);
    }

    protected String getWebsiteTopCatalog(Delegator delegator, String webSiteId) {
        if (UtilValidate.isEmpty(webSiteId)) {
            return null;
        }
        try {
            GenericValue webSite = EntityQuery.use(delegator).from("WebSite").where("webSiteId", webSiteId).cache(true).queryOne();
            if (webSite == null) {
                Debug.logError("getWebsiteTopCatalog: invalid webSiteId '" + webSiteId + "'", module);
                return null;
            }
            String productStoreId = webSite.getString("productStoreId");
            if (productStoreId == null) return null;
            
            List<GenericValue> storeCatalogs = CatalogWorker.getStoreCatalogs(delegator, productStoreId);
            if (UtilValidate.isNotEmpty(storeCatalogs)) {
                return storeCatalogs.get(0).getString("prodCatalogId");
            }
        } catch(Exception e) {
            Debug.logError("getWebsiteTopCatalog: error while determining catalog for webSiteId '" + webSiteId + "': " + e.getMessage(), module);
            return null;
        }
        return null;
    }
    
    protected Set<String> getProdCatalogCategoryIds(Delegator delegator, String prodCatalogId, Collection<String> includeTypes) {
        List<GenericValue> values = CatalogWorker.getProdCatalogCategories(delegator, prodCatalogId, null);
        Set<String> idList = new LinkedHashSet<>();
        for(GenericValue value : values) {
            if (includeTypes == null || includeTypes.contains(value.getString("productCategoryId"))) {
                idList.add(value.getString("productCategoryId"));
            }
        }
        return idList;
    }
}

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
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.ListIterator;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
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
import com.ilscipio.scipio.product.category.CatalogUrlType;
import com.ilscipio.scipio.product.seo.SeoCatalogUrlWorker.SeoCatalogUrlInfo;
import com.ilscipio.scipio.util.SeoStringUtil;

/**
 * SCIPIO: SEO url building functions and callbacks.
 * <p>
 * Some parts adapted from the original <code>org.ofbiz.product.category.ftl.CatalogUrlSeoTransform</code>;
 * others re-done based on {@link org.ofbiz.product.category.CatalogUrlFilter}.
 * <p>
 * <strong>WARN:</strong> Do not call makeXxxUrl methods from this class from client code!
 * Client code that need java methods should use (which these plug into):
 * <ul>
 * <li>{@link org.ofbiz.product.category.CatalogUrlFilter#makeCatalogAltLink}</li>
 * <li>{@link org.ofbiz.product.category.CatalogUrlServlet#makeCatalogLink}</li>
 * </ul>
 * FIXME: makeXxxUrlPath methods do not respect useCache flag
 */
@SuppressWarnings("serial")
public class SeoCatalogUrlWorker implements Serializable {

    public static final String module = SeoCatalogUrlWorker.class.getName();

    public static final String DEFAULT_CONFIG_RESOURCE = "SeoConfigUiLabels";

    private static final SeoCatalogUrlWorker DEFAULT_INSTANCE = new SeoCatalogUrlWorker();

    // TODO: in production, these cache can be tweaked with non-soft refs, limits and expire time
    private static final UtilCache<String, AltUrlPartResults> productAltUrlPartInfoCache = UtilCache.createUtilCache("seo.filter.product.alturl.part", true);
    private static final UtilCache<String, AltUrlPartResults> categoryAltUrlPartInfoCache = UtilCache.createUtilCache("seo.filter.category.alturl.part", true);

    // trying to avoid this if possible... if needed, has to be configurable
//    /**
//     * FIXME: unhardcode; could be per-store.
//     */
//    private static final List<String> DEFAULT_BROWSABLE_ROOTCATTYPES = UtilMisc.unmodifiableArrayList(
//            "PCCT_BROWSE_ROOT", "PCCT_PROMOTIONS", "PCCT_BEST_SELL"
//            );
    
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
    
    protected SeoConfig config;
    
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

    protected SeoCatalogUrlWorker(SeoConfig config) {
        this.config = config;
        this.configResourceName = DEFAULT_CONFIG_RESOURCE;
        this.urlSuffix = config.getSeoUrlSuffix() != null ? config.getSeoUrlSuffix() : "";
        this.catalogUrlBuilder = new SeoCatalogUrlBuilder();
        this.catalogAltUrlBuilder = new SeoCatalogAltUrlBuilder();
        this.catalogAltUrlSanitizer = new SeoCatalogAltUrlSanitizer();
    }
    protected SeoCatalogUrlWorker() {
        this(SeoConfig.getCommonConfig());
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
        if (SeoConfig.DEBUG_FORCERELOAD) return createInstanceDeep(delegator, null);
        else return DEFAULT_INSTANCE;
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
        if (!SeoConfig.getCommonConfig().isSeoUrlEnabled(contextPath, webSiteId)) return null;
        // TODO: should return different builder depending on store and config
        return getDefaultInstance(delegator);
    }

    /**
     * Force create new instance - for debugging only!
     * <p>
     * TODO: currently only returns default instance, inevitably this will change.
     */
    public static SeoCatalogUrlWorker createInstance(Delegator delegator, String webSiteId) {
        return new SeoCatalogUrlWorker();
    }
    
    /**
     * Force create new instance deep - for debugging only!
     * <p>
     * TODO: currently only returns default instance, inevitably this will change.
     */
    public static SeoCatalogUrlWorker createInstanceDeep(Delegator delegator, String webSiteId) {
        return new SeoCatalogUrlWorker(SeoConfig.createConfig(delegator, webSiteId));
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
            if (!SeoConfig.getCommonConfig().isSeoUrlEnabled(contextPath, webSiteId)) return null;
            return SeoCatalogUrlWorker.getInstance(delegator, webSiteId).getCatalogUrlBuilder();
        }
        @Override
        public CatalogAltUrlBuilder getCatalogAltUrlBuilder(boolean withRequest, HttpServletRequest request,
                Delegator delegator, String contextPath, String webSiteId) {
            if (!SeoConfig.getCommonConfig().isSeoUrlEnabled(contextPath, webSiteId)) return null;
            return SeoCatalogUrlWorker.getInstance(delegator, webSiteId).getCatalogAltUrlBuilder();
        }
    }

    /*
     * *****************************************************
     * Getters and config
     * *****************************************************
     */

    public SeoConfig getConfig() {
        return config;
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
        public String convertNameToDbAltUrl(String name, Locale locale, CatalogUrlType entityType) {
            
            String url = getConfig().getAltUrlGenProcessors().processUrl(name);
                    
            if (entityType == CatalogUrlType.PRODUCT) {
                url = getConfig().limitProductNameLength(url);
            } else if (entityType == CatalogUrlType.CATEGORY) {
                url = getConfig().limitCategoryNameLength(url);
            }

            return url;
        }

        @Override
        public String convertIdToDbAltUrl(String id, Locale locale, CatalogUrlType entityType) {
            // TODO: REVIEW: leaving this same as live for now... doubtful...
            return convertIdToLiveAltUrl(id, locale, entityType);
        }

        @Override
        public String sanitizeAltUrlFromDb(String altUrl, Locale locale, CatalogUrlType entityType) {
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
        public String convertIdToLiveAltUrl(String id, Locale locale, CatalogUrlType entityType) {

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
    public List<String> getCategoryUrlTrailNames(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail, boolean useCache) {
        if (trail == null || trail.isEmpty()) return newPathList();
        List<String> catNames = newPathList(trail.size());
        ListIterator<String> trailIt = trail.listIterator();
        while(trailIt.hasNext()) {
            String productCategoryId = trailIt.next();
            if ("TOP".equals(productCategoryId)) continue;
            String catName = getCategoryUrlTrailName(delegator, dispatcher, locale, productCategoryId, !trailIt.hasNext(), useCache);
            if (catName != null) catNames.add(catName);
        }
        return catNames;
    }
    
    /**
     * Reads ALTERNATIVE_URL for category and locale from DB and builds config-specified alt url path part.
     * Fallback on ID.
     * FIXME: ContentWrapper does not respect useCache flag!
     */
    public String getCategoryUrlTrailName(Delegator delegator, LocalDispatcher dispatcher, Locale locale, GenericValue productCategory, boolean last, boolean useCache) {
        String catName = null;
        String productCategoryId = productCategory.getString("productCategoryId");
        try {
            String altUrl = CategoryContentWrapper.getProductCategoryContentAsText(productCategory, "ALTERNATIVE_URL", locale, dispatcher, useCache, "raw");
            if (UtilValidate.isNotEmpty(altUrl)) {
                // FIXME: effective locale might not be same as "locale" variable!
                altUrl = getCatalogAltUrlSanitizer().sanitizeAltUrlFromDb(altUrl, locale, CatalogUrlType.CATEGORY);
                if (!altUrl.isEmpty()) {
                    catName = altUrl;
                    
                    if (!last) {
                        if (getConfig().isCategoryNameAppendId()) {
                            catName += SeoStringUtil.URL_HYPHEN + getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productCategoryId, locale, CatalogUrlType.CATEGORY);
                        }
                    } else {
                        if (getConfig().isCategoryNameAppendIdLast()) {
                            catName += SeoStringUtil.URL_HYPHEN + getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productCategoryId, locale, CatalogUrlType.CATEGORY);
                        }
                    }
                }
            }
        } catch(Exception e) {
            Debug.logError(e, "Seo: Cannot get category '" + productCategoryId + "' alt url", module);
        }

        if (catName == null) {
            // fallback
            catName = getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productCategoryId, locale, CatalogUrlType.CATEGORY);
        }
        return catName;
    }

    /**
     * Reads ALTERNATIVE_URL for category and locale from DB and builds config-specified alt url path part.
     * Fallback on ID.
     */
    public String getCategoryUrlTrailName(Delegator delegator, LocalDispatcher dispatcher, Locale locale, String productCategoryId, boolean last, boolean useCache) {
        String catName = null;
        if (productCategoryId != null) {
            try {
                GenericValue productCategory = EntityQuery.use(delegator).from("ProductCategory")
                            .where("productCategoryId", productCategoryId).cache(useCache).queryOne();
                if (productCategory != null) {
                    catName = getCategoryUrlTrailName(delegator, dispatcher, locale, productCategory, last, useCache);
                } else {
                    ; // NOTE: this is possible due to cache and delays and such
                }
            } catch(Exception e) {
                Debug.logError(e, "Seo: Cannot get category '" + productCategoryId + "' alt url", module);
            }
        }
        return catName;
    }

    /**
     * Creates a full trail to the given category using hints from the incoming trail to select best.
     */
    protected List<String> makeFullCategoryUrlTrail(Delegator delegator, List<String> hintTrail, GenericValue productCategory, String webSiteId, String currentCatalogId) {
        if (productCategory == null) return newPathList();
        
        Set<String> topCategoryIds = getCatalogTopCategoriesForCategoryUrl(delegator, currentCatalogId, webSiteId);
        if (topCategoryIds.isEmpty()) {
            Debug.logWarning("Seo: makeFullCategoryUrlTrail: No top categories found for catalog '" + currentCatalogId + "'; can't select best trail", module);
            return newPathList();
        }
        
        List<List<String>> trails = getCategoryRollupTrails(delegator, productCategory.getString("productCategoryId"), topCategoryIds);
        return findBestTopCatTrailForNewUrl(delegator, trails, hintTrail, topCategoryIds);
    }

    /**
     * Creates a full trail to the given product using hints from the incoming trail to select best.
     */
    protected List<String> makeFullProductUrlTrail(Delegator delegator, List<String> hintTrail, GenericValue product, String webSiteId, String currentCatalogId) {
        if (product == null) return newPathList();

        Set<String> topCategoryIds = getCatalogTopCategoriesForProductUrl(delegator, currentCatalogId, webSiteId);
        if (topCategoryIds.isEmpty()) {
            Debug.logWarning("Seo: makeFullProductUrlTrail: No top category found for catalog '" + currentCatalogId + "'; can't select best trail", module);
            return newPathList();
        }
        
        try {
            String primaryCatId = product.getString("primaryProductCategoryId");
            if (primaryCatId != null) { // prioritize primary product category
                List<List<String>> trails = getCategoryRollupTrails(delegator, primaryCatId, topCategoryIds);
                return findBestTopCatTrailForNewUrl(delegator, trails, hintTrail, topCategoryIds);
            } else { // no primary, use rollups
                List<GenericValue> prodCatMembers = EntityQuery.use(delegator).from("ProductCategoryMember")
                    .where("productId", product.getString("productId")).orderBy("-fromDate").filterByDate().queryList();
                if (prodCatMembers.size() == 0) {
                    return newPathList();
                } else {
                    List<List<String>> trails = null;
                    for(GenericValue prodCatMember : prodCatMembers) {
                        String productCategoryId = prodCatMember.getString("productCategoryId");
                        List<List<String>> memberTrails = getCategoryRollupTrails(delegator, productCategoryId, topCategoryIds);
                        if (trails == null) trails = memberTrails;
                        else trails.addAll(memberTrails);
                    }
                    return findBestTopCatTrailForNewUrl(delegator, trails, hintTrail, topCategoryIds);
                }
            }
        } catch(Exception e) {
            Debug.logError(e, "Seo: Error generating trail for product '" + product.getString("productId") + "': " + e.getMessage(), module);
            return newPathList();
        }
    }
    
    protected List<String> findBestTopCatTrailForNewUrl(Delegator delegator, List<List<String>> trails, List<String> hintTrail, Collection<String> topCategoryIds) {
        if (trails.size() == 0) {
            return newPathList();
        } else if (trails.size() == 1) {
            // if only one trail, we'll assume it leads to top category
            return trails.get(0);
        } else {
            ClosestTrailResolver.ResolverType resolverType = getConfig().getNewUrlTrailResolver();
            return ensurePathList(resolverType.getResolver().findClosestTrail(trails, hintTrail, topCategoryIds));
        }
    }
    
    public String makeCategoryUrl(HttpServletRequest request, Locale locale, String previousCategoryId, String productCategoryId, String productId, String viewSize, String viewIndex, String viewSort, String searchString) {
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
    public String makeCategoryUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail, String webSiteId, String contextPath, String currentCatalogId, String previousCategoryId, String productCategoryId, String productId, String viewSize, String viewIndex, String viewSort, String searchString) {
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

        // SCIPIO: refine and append trail
        // NO LONGER NEED ADJUST - in fact it will prevent the valid trail selection after this from working
        //trail = CategoryWorker.adjustTrail(trail, productCategoryId, previousCategoryId);
        trail = makeFullCategoryUrlTrail(delegator, trail, productCategory, webSiteId, currentCatalogId);
        List<String> trailNames = getCategoryUrlTrailNames(delegator, dispatcher, locale, trail, true);
        
        // NOTE: pass null productCategory because already resolved in trailNames
        StringBuilder urlBuilder = makeCategoryUrlPath(delegator, dispatcher, locale, null, trailNames, contextPath, true);
        
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

    /**
     * Builds the core category URL path.
     * NOTE: productCategory may be null, in which case assume already resolved as part of trailNames. 
     * Assumes trailNames already valid.
     */
    public StringBuilder makeCategoryUrlPath(Delegator delegator, LocalDispatcher dispatcher, Locale locale, 
            GenericValue productCategory, List<String> trailNames, String contextPath, boolean useCache) {
        StringBuilder urlBuilder = new StringBuilder();
        if (contextPath != null) {
            urlBuilder.append(contextPath);
        }

        SeoConfig config = getConfig();
        
        // DEV NOTE: I removed getConfig().isHandleImplicitRequests() check from here because was inconsistent and not really needed or wanted
        boolean explicitCategoryRequest = !config.isGenerateImplicitCategoryUrl();
        
        if (explicitCategoryRequest && !config.isGenerateCategoryAltUrlSuffix()) {
            appendSlashAndValue(urlBuilder, getCategoryServletPathName(locale));
        }
        
        // NOTE: this loop includes the productCategoryId itself
        appendSlashAndValue(urlBuilder, trailNames);
        
        if (productCategory != null) {
            String catTrailName = getCategoryUrlTrailName(delegator, dispatcher, locale, productCategory, true, useCache);
            if (catTrailName != null) {
                appendSlashAndValue(urlBuilder, catTrailName);
            }
        }
        
        // legacy category alt url suffix ("-c") 
        if (explicitCategoryRequest && config.isGenerateCategoryAltUrlSuffix()) {
            urlBuilder.append(config.getCategoryAltUrlSuffix());
        }
        
        // general URL suffix/extension (".html")
        checkAddUrlSuffix(urlBuilder);
        
        return urlBuilder;
    }
    
    public String makeProductUrl(HttpServletRequest request, Locale locale, String previousCategoryId, String productCategoryId, String productId) {
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
    public String makeProductUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail, String webSiteId, String contextPath, String currentCatalogId, String previousCategoryId, String productCategoryId, String productId) {
        GenericValue product;
        try {
            product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache().queryOne();
        } catch (GenericEntityException e) {
            Debug.logWarning(e, "Seo: Cannot create product's URL for: " + productId, module);
            return null;
        }

        List<String> trailNames;
        if (!getConfig().isCategoryNameEnabled()) {
            // no need for trail
            trail = Collections.emptyList();
            trailNames = Collections.emptyList();
        } else {
            // NO LONGER NEED ADJUST - in fact it will prevent the valid trail selection after this from working
            //if (UtilValidate.isNotEmpty(productCategoryId)) {
            //    trail = CategoryWorker.adjustTrail(trail, productCategoryId, previousCategoryId);
            //}
            trail = makeFullProductUrlTrail(delegator, trail, product, webSiteId, currentCatalogId);
            trailNames = getCategoryUrlTrailNames(delegator, dispatcher, locale, trail, true);
        }
        
        StringBuilder urlBuilder = makeProductUrlPath(delegator, dispatcher, locale, product, trailNames, contextPath, true);
        
        return urlBuilder.toString();
    }

    /**
     * Builds the core product URL path.
     * NOTE: product may be null, in which case assume already resolved as part of trailNames. 
     * Assumes trailNames already valid.
     * FIXME: ContentWrapper does not respect useCache flag!
     */
    public StringBuilder makeProductUrlPath(Delegator delegator, LocalDispatcher dispatcher, Locale locale, GenericValue product, List<String> trailNames, String contextPath, boolean useCache) {
        StringBuilder urlBuilder = new StringBuilder();
        if (contextPath != null) {
            urlBuilder.append(contextPath);
        }
        
        SeoConfig config = getConfig();
        
        int catCount = trailNames.size();
        if (product == null && catCount > 0) catCount--; // product already in trail 
        
        boolean explicitProductRequest;
        // DEV NOTE: I removed config.isHandleImplicitRequests() check from here because was inconsistent and not really needed or wanted
        if (catCount > 0) {
            explicitProductRequest = !config.isGenerateImplicitProductUrl();
        } else {
            // NOTE: in old code, seo only support(ed) implicit request if there was at least one category
            explicitProductRequest = !config.isGenerateImplicitProductUrlNoCat();
        }

        if (explicitProductRequest && !config.isGenerateProductAltUrlSuffix()) {
            appendSlashAndValue(urlBuilder, getProductServletPathName(locale));
        }
        
        // append category names
        if (config.isCategoryNameEnabled()) {
            appendSlashAndValue(urlBuilder, trailNames);
        }
        
        if (product != null) {
            // 2017-11-08: NOT SUPPORTED: could only theoretically work if chose different character than hyphen
            //if (!trailNames.isEmpty() && !SeoConfigUtil.isCategoryNameSeparatePathElem()) {
            //    urlBuilder.append(SeoStringUtil.URL_HYPHEN);
            //} else {
            ensureTrailingSlash(urlBuilder);
            //}
            
            // append product name
            String productId = product.getString("productId");
            String alternativeUrl = ProductContentWrapper.getProductContentAsText(product, "ALTERNATIVE_URL", locale, dispatcher, useCache, "raw");
            // FIXME: effective locale might not be same as "locale" variable!
            alternativeUrl = getCatalogAltUrlSanitizer().sanitizeAltUrlFromDb(alternativeUrl, locale, CatalogUrlType.PRODUCT);
            if (UtilValidate.isNotEmpty(alternativeUrl)) {
                urlBuilder.append(alternativeUrl);
                
                if (config.isProductNameAppendId() && UtilValidate.isNotEmpty(productId)) {
                    urlBuilder.append(SeoStringUtil.URL_HYPHEN);
                    urlBuilder.append(getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productId, locale, CatalogUrlType.PRODUCT));
                }
            } else {
                // FALLBACK ONLY
                urlBuilder.append(getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productId, locale, CatalogUrlType.PRODUCT));
            }
        }
        
        // legacy product alt url suffix ("-p")
        if (explicitProductRequest && config.isGenerateProductAltUrlSuffix()) {
            urlBuilder.append(config.getProductAltUrlSuffix());
        }
        
        // general URL suffix/extension (".html")
        checkAddUrlSuffix(urlBuilder);
        
        return urlBuilder;
    }
    
    protected void checkAddUrlSuffix(StringBuilder sb) {
        String urlSuffix = getUrlSuffix();
        if (UtilValidate.isNotEmpty(urlSuffix) && (sb.length() > 0) && (sb.charAt(sb.length() - 1) != '/')) {
            sb.append(getUrlSuffix());
        }
    }

    
    /*
     * *****************************************************
     * URL matching & parsing
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
        private final Locale locale;
        
        protected SeoCatalogUrlInfo(String origPath, String productId, String categoryId, 
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
        
        public static SeoCatalogUrlInfo createIfValidRequest(String origPath, String productId, String categoryId, 
                boolean explicitProductRequest, boolean explicitCategoryRequest, 
                List<String> pathCategoryIds, Locale matchedLocale) {

            if (explicitProductRequest || explicitCategoryRequest || productId != null || categoryId != null) {
                if (productId != null) {
                    if (categoryId == null && UtilValidate.isNotEmpty(pathCategoryIds)) {
                        categoryId = pathCategoryIds.get(pathCategoryIds.size() - 1);
                    }
                } else if (categoryId != null) {
                    removeLastIfEquals(pathCategoryIds, categoryId);
                }
                return new SeoCatalogUrlInfo(origPath, productId, categoryId, 
                        explicitProductRequest, explicitCategoryRequest, 
                        ensurePathList(pathCategoryIds), 
                        matchedLocale);
            } else {
                return null;
            }
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
        /**
         * The locale that the explicit product/category path prefix matched OR the 
         * the locale that the product/category name matched for implicit mapping.
         * WARN: may be imprecise! multiple labels may map to the same language!
         */
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
        
        // clean up the path
        String pathInfo = preprocessInboundSeoCatalogUrlPath(path);
        if (pathInfo == null) return null;
        
        // split path into alt-url parts
        List<String> pathElements = StringUtil.split(pathInfo, "/");
        if (UtilValidate.isEmpty(pathElements)) return null;

        boolean explicitCategoryRequest = false;
        boolean explicitProductRequest = false;
        String productId = null;
        String categoryId = null;
        List<String> pathCategoryIds = null;
        // the locale the URL appears to be: the path prefix if explicit, or the cat/prod name language if implicit
        Locale matchedLocale = null;
        String lastPathElem = pathElements.get(pathElements.size() - 1);
        
        SeoConfig config = getConfig();
        
        // determine the general form of the URL: explicit or implicit (+ locale at same time)
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
                // LEGACY suffix support for backward compat with published links
                if (config.isHandleProductAltUrlSuffix() && lastPathElem.endsWith(config.getProductAltUrlSuffix())) {
                    explicitProductRequest = true;
                    lastPathElem = lastPathElem.substring(0, lastPathElem.length() - config.getProductAltUrlSuffix().length());
                    pathElements.set(pathElements.size() - 1, lastPathElem);
                } else if (config.isHandleCategoryAltUrlSuffix() && lastPathElem.endsWith(config.getCategoryAltUrlSuffix())) {
                    explicitCategoryRequest = true;
                    lastPathElem = lastPathElem.substring(0, lastPathElem.length() - config.getProductAltUrlSuffix().length());
                    pathElements.set(pathElements.size() - 1, lastPathElem);
                } else {
                    if (!config.isHandleImplicitRequests()) {
                        return null;
                    }
                }
            }
        }

        if (pathElements.size() > 0) lastPathElem = pathElements.get(pathElements.size() - 1);
        else lastPathElem = null;
        
        if (UtilValidate.isNotEmpty(lastPathElem)) {
            pathElements.remove(pathElements.size() - 1);
            
            try {
                if (explicitProductRequest) { 
                    // EXPLICIT PRODUCT
                    boolean exactOnly = false;
                    boolean allowIdOnly = true;
                    AltUrlPartResults productMatches = extractCandidateAltUrlProductIdCached(delegator, lastPathElem, exactOnly, allowIdOnly);
                    if (productMatches.size() > 0) {
                        AltUrlMatchInfo matchInfo = findBestProductMatch(delegator, productMatches, pathElements, currentCatalogId, webSiteId);
                        if (matchInfo != null) {
                            productId = matchInfo.getUrlInfo().getId();
                            pathCategoryIds = matchInfo.getPathCategoryIds();
                        }
                    }
                } else if (explicitCategoryRequest) { 
                    // EXPLICIT CATEGORY
                    boolean exactOnly = false;
                    boolean allowIdOnly = true;
                    AltUrlPartResults categoryMatches = extractCandidateAltUrlCategoryIdCached(delegator, lastPathElem, exactOnly, allowIdOnly);
                    if (categoryMatches.size() > 0) {
                        AltUrlMatchInfo matchInfo = findBestCategoryMatch(delegator, categoryMatches, pathElements, currentCatalogId, webSiteId);
                        if (matchInfo != null) {
                            categoryId = matchInfo.getUrlInfo().getId();
                            pathCategoryIds = matchInfo.getPathCategoryIds();
                        }
                    }
                } else { 
                    // IMPLICIT REQUEST
                    // WARN: best-effort, ambiguous - it is up to SeoConfig.xml to decide how risky this will be
                    boolean exactOnly = false; // TODO: REVIEW: is this safe if false?
                    boolean allowIdOnly = !config.isImplicitRequestNameMatchesOnly();
                    AltUrlPartResults productMatches = extractCandidateAltUrlProductIdCached(delegator, lastPathElem, exactOnly, allowIdOnly);
                    if (productMatches.size() > 0) {
                        AltUrlMatchInfo matchInfo = findBestProductMatch(delegator, productMatches, pathElements, currentCatalogId, webSiteId);
                        if (matchInfo != null) {
                            productId = matchInfo.getUrlInfo().getId();
                            pathCategoryIds = matchInfo.getPathCategoryIds();
                        }
                    } else {
                        AltUrlPartResults categoryMatches = extractCandidateAltUrlCategoryIdCached(delegator, lastPathElem, exactOnly, allowIdOnly);
                        if (categoryMatches.size() > 0) {
                            AltUrlMatchInfo matchInfo = findBestCategoryMatch(delegator, categoryMatches, pathElements, currentCatalogId, webSiteId);
                            if (matchInfo != null) {
                                categoryId = matchInfo.getUrlInfo().getId();
                                pathCategoryIds = matchInfo.getPathCategoryIds();
                            }
                        }
                    }
                }
            } catch(Exception e) {
                Debug.logError(e, "Seo: matchInboundSeoCatalogUrl: Error parsing catalog URL " + origPath + ": " + e.getMessage(), module);
                return null;
            }
        }

        return SeoCatalogUrlInfo.createIfValidRequest(origPath, productId, categoryId, explicitProductRequest, explicitCategoryRequest, pathCategoryIds, matchedLocale);
    }
    
    public static class AltUrlMatchInfo implements Serializable {
        private final AltUrlPartInfo urlInfo;
        private final List<String> pathCategoryIds;
        protected AltUrlMatchInfo(AltUrlPartInfo urlInfo, List<String> pathCategoryIds) {
            this.urlInfo = urlInfo;
            this.pathCategoryIds = pathCategoryIds;
        }
        public AltUrlPartInfo getUrlInfo() { return urlInfo;}
        public List<String> getPathCategoryIds() { return pathCategoryIds;}
    }
    
    protected AltUrlMatchInfo findBestProductMatch(Delegator delegator, AltUrlPartResults productMatches, List<String> pathElements, String currentCatalogId, String webSiteId) throws GenericEntityException {
        List<String> pathCategoryIds = null;
        
        Set<String> topCategoryIds = getCatalogTopCategoriesForProductUrl(delegator, currentCatalogId, webSiteId);
        
        AltUrlPartInfo singleMatch = productMatches.getSingle();
        if (singleMatch != null) {
            // SINGLE PRODUCT RESULT
            List<List<String>> possibleTrails = getProductRollupTrails(delegator, singleMatch.getId(), topCategoryIds);
            if (possibleTrails.size() > 0) {
                pathCategoryIds = findSingleMatchBestTrail(delegator, possibleTrails, pathElements, null, topCategoryIds);
                if (pathCategoryIds != null || config.isAllowInvalidCategoryPathElements()) {
                    return new AltUrlMatchInfo(singleMatch, pathCategoryIds);
                }
            } else {
                if (config.isAllowTargetOutsideCatalog()) {
                    return new AltUrlMatchInfo(singleMatch, pathCategoryIds);
                }
            }
        } else {
            // MULTIPLE PRODUCT RESULT (NAME/ID CONFLICT)
            AltUrlMatchInfo bestMatch = findMultiMatchBestMatch(delegator, productMatches, false, pathElements, topCategoryIds);
            if (bestMatch != null) {
                if (bestMatch.getPathCategoryIds() != null || config.isAllowInvalidCategoryPathElements()) {
                    return bestMatch;
                }
            } else {
                if (config.isAllowTargetOutsideCatalog()) {
                    return new AltUrlMatchInfo(productMatches.getFirst(), null);
                }
            }
        }
        return null;
    }
    
    protected AltUrlMatchInfo findBestCategoryMatch(Delegator delegator, AltUrlPartResults categoryMatches, List<String> pathElements, String currentCatalogId, String webSiteId) throws GenericEntityException {
        List<String> pathCategoryIds = null;
        
        Set<String> topCategoryIds = getCatalogTopCategoriesForCategoryUrl(delegator, currentCatalogId, webSiteId);
        
        AltUrlPartInfo singleMatch = categoryMatches.getSingle();
        if (singleMatch != null) {
            // SINGLE CATEGORY RESULT
            List<List<String>> possibleTrails = getCategoryRollupTrails(delegator, singleMatch.getId(), topCategoryIds);
            if (possibleTrails.size() > 0) {
                pathCategoryIds = findSingleMatchBestTrail(delegator, possibleTrails, pathElements, categoryMatches, topCategoryIds);
                if (pathCategoryIds != null || config.isAllowInvalidCategoryPathElements()) {
                    return new AltUrlMatchInfo(singleMatch, pathCategoryIds);
                }
            } else {
                if (config.isAllowTargetOutsideCatalog()) {
                    return new AltUrlMatchInfo(singleMatch, pathCategoryIds);
                }
            }
        } else {
            // MULTIPLE CATEGORY RESULT (NAME/ID CONFLICT)
            // find the most exact category that belongs to our store, and give priority
            // to one that matches path elements. if there are no path elements, give priority to full name+id match.
            AltUrlMatchInfo bestMatch = findMultiMatchBestMatch(delegator, categoryMatches, true, pathElements, topCategoryIds);
            if (bestMatch != null) {
                if (bestMatch.getPathCategoryIds() != null || config.isAllowInvalidCategoryPathElements()) {
                    return bestMatch;
                }
            } else {
                if (config.isAllowTargetOutsideCatalog()) {
                    return new AltUrlMatchInfo(categoryMatches.getFirst(), null);
                }
            }
        }
        return null;
    }
    
    protected List<String> findSingleMatchBestTrail(Delegator delegator, List<List<String>> possibleTrails, List<String> pathElements, AltUrlPartResults extraPathElement, Set<String> topCategoryIds) throws GenericEntityException {
        List<String> pathCategoryIds = null;
        
        if (possibleTrails.size() == 1 && getConfig().isAllowInvalidCategoryPathElements()) {
            // optimization: only one trail possible, path elements not important, so can ignore path elements
            pathCategoryIds = possibleTrails.get(0);
        } else {
            if (pathElements.isEmpty()) {
                // no trail hint, so just select the first...
                //pathCategoryIds = possibleTrails.get(0);
                pathCategoryIds = getFirstTopTrail(possibleTrails, topCategoryIds);
            } else {
                // find the trail closest to the passed path elems
                List<AltUrlPartResults> resolvedPathElems = extractCandidateAltUrlCategoryIdsCached(delegator, pathElements, false, true);
                if (extraPathElement != null) { // needed for categories
                    resolvedPathElems.add(extraPathElement);
                }
                pathCategoryIds = findBestTrailForUrlPathElems(delegator, possibleTrails, resolvedPathElems);
                
                if (pathCategoryIds == null && getConfig().isAllowInvalidCategoryPathElements()) {
                    pathCategoryIds = getFirstTopTrail(possibleTrails, topCategoryIds);
                }
            }
        }

        return pathCategoryIds;
    }
    
    protected AltUrlMatchInfo findMultiMatchBestMatch(Delegator delegator, AltUrlPartResults matches, boolean isCategory, List<String> pathElements, Set<String> topCategoryIds) throws GenericEntityException {
        List<AltUrlPartResults> resolvedPathElems = extractCandidateAltUrlCategoryIdsCached(delegator, pathElements, false, true);
        List<String> pathCategoryIds = null;

        AltUrlPartInfo bestMatch = null;
        List<List<String>> bestMatchTrails = null;
        List<String> bestPathCategoryIds = null; // NOTE: this contains the target category itself at end
        for(AltUrlPartInfo nextMatch : matches.values()) {
            List<List<String>> nextMatchTrails;
            if (isCategory) {
                nextMatchTrails = getCategoryRollupTrails(delegator, nextMatch.getId(), topCategoryIds);
            } else {
                nextMatchTrails = getProductRollupTrails(delegator, nextMatch.getId(), topCategoryIds);
            }
            if (nextMatchTrails.size() > 0) {
                if (pathElements.size() > 0) {
                    if (isCategory) {
                        // SPECIAL: for category, we have to re-add ourselves at the end for checking purposes
                        resolvedPathElems.add(nextMatch.getAsResult());
                    }
                    try {
                        List<String> nextPathCategoryIds = findBestTrailForUrlPathElems(delegator, nextMatchTrails, resolvedPathElems);
                        if (nextPathCategoryIds != null) {
                            // here, nextPathCategoryIds returned is always equal size or longer than pathElements.
                            // the best pathCategoryIds is actually the shortest one, because it's closest to pathElements.
                            if (bestMatch == null || (nextPathCategoryIds != null && 
                                ((bestPathCategoryIds == null) || 
                                (nextPathCategoryIds.size() < bestPathCategoryIds.size()) ||
                                (nextPathCategoryIds.size() == bestPathCategoryIds.size() && 
                                        isFirstTrailIsBetterMatchThanSecondTopCatPrecision(nextMatch, nextPathCategoryIds, bestMatch, bestPathCategoryIds, topCategoryIds))))) {
                                bestMatch = nextMatch;
                                bestMatchTrails = nextMatchTrails;
                                bestPathCategoryIds = nextPathCategoryIds;
                                // not sure about this anymore... better omit
                                //// special case: we won't find better than this (with out current algos)
                                //if (nextMatch.isFullMatch() && (nextPathCategoryIds != null && nextPathCategoryIds.size() == (pathElements.size()+1))) {
                                //    break;
                                //}
                            }
                        }
                    } finally {
                        if (isCategory) {
                            resolvedPathElems.remove(resolvedPathElems.size() - 1);
                        }
                    }
                } else {
                    List<String> nextPathCategoryIds = getFirstTopTrail(nextMatchTrails, topCategoryIds);
                    if (bestMatch == null || isFirstTrailIsBetterMatchThanSecondTopCatPrecision(nextMatch, nextPathCategoryIds, bestMatch, bestPathCategoryIds, topCategoryIds)) {
                        bestMatch = nextMatch;
                        bestMatchTrails = nextMatchTrails;
                        bestPathCategoryIds = nextPathCategoryIds;
                        // not sure about this anymore... better omit
                        //// special case: we won't find better than this
                        //if (nextMatch.isFullMatch()) break; 
                    }
                }
            }
        }
        if (bestMatch != null) {
            if (pathElements.size() > 0) {
                pathCategoryIds = bestPathCategoryIds;
                // NOTE: I don't think this ever gets called, leaving here just in case
                if (pathCategoryIds == null && getConfig().isAllowInvalidCategoryPathElements()) {
                    pathCategoryIds = getFirstTopTrail(bestMatchTrails, topCategoryIds);
                }
            } else {
                // now do this during iteration, otherwise we can't do the priority properly
                //pathCategoryIds = getFirstTopTrail(bestMatchTrails, topCategoryIds);
            }
            return new AltUrlMatchInfo(bestMatch, pathCategoryIds);
        }
        return null;
    }
    
    /**
     * Does top cat check and precision check, but assumes size was already checked.
     */
    private boolean isFirstTrailIsBetterMatchThanSecondTopCatPrecision(AltUrlPartInfo firstMatch, List<String> firstTrail, AltUrlPartInfo secondMatch, List<String> secondTrail, Set<String> topCategoryIds) {
        if (UtilValidate.isNotEmpty(firstTrail)) {
            if (UtilValidate.isNotEmpty(secondTrail)) {
                // 1) prefer the trail that is lower ProdCatalogCategory sequenceNum
                String firstTopCatId = firstTrail.get(0);
                String secondTopCatId = secondTrail.get(0);
                if (!firstTopCatId.equals(secondTopCatId)) {
                    for(String topCatId : topCategoryIds) {
                        // first to hit returns
                        if (firstTopCatId.equals(topCatId)) {
                            return true;
                        } else if (secondTopCatId.equals(topCatId)) {
                            return false;
                        }
                    }
                }
            } else {
                return true;
            }
        } else {
            if (UtilValidate.isNotEmpty(secondTrail)) {
                return false;
            } else {
                ;
            }
        }
        // fallback on precision check
        return firstMatch.isMorePreciseThan(secondMatch);
    }
    
    
    
    /**
     * Checks if matches suffix and has starting slash; removes starting and ending slashes.
     * Returns null if bad.
     * Result splits cleanly on "/".
     */
    public String preprocessInboundSeoCatalogUrlPath(String pathInfo) {
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
        
        return pathInfo;
    }
    
    /**
     * Uses the passed path elements (resolved) to try to select the best of the possible trails.
     * Returns null only if nothing matches at all.
     * BEST-EFFORT.
     * <p>
     * TODO: REVIEW: the possibility of each path elem matching multiple category IDs makes this extremely
     * complicated; so we ignore the specific implications and just match as much as possible.
     * <p>
     * For a trail to be selected, it must "end with" the pathElems; after that, the best trail is one that 
     * has smallest length.
     */
    protected List<String> findBestTrailForUrlPathElems(Delegator delegator, List<List<String>> possibleTrails, List<AltUrlPartResults> pathElems) throws GenericEntityException {
        if (pathElems.isEmpty()) return null;
        List<String> bestTrail = null;
        for(List<String> trail : possibleTrails) {
            if (pathElems.size() > trail.size()) continue; // sure to fail
            
            ListIterator<AltUrlPartResults> pit = pathElems.listIterator(pathElems.size());
            ListIterator<String> tit = trail.listIterator(trail.size());
            boolean matched = true;
            while(matched && pit.hasPrevious()) {
                AltUrlPartResults urlInfos = pit.previous();
                String categoryId = tit.previous();
                
                // simplistic check: ignores exact vs name-only matches, but may be good enough
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
     * The results of parsing an alt URL part/path element - one or more products/categories.
     * <p>
     * The Map interface maps ID to the part info.
     * WARN: The Map interface only allows one result per ID; it's technically possible
     * to have more than one result per ID.
     */
    public static class AltUrlPartResults implements Map<String, AltUrlPartInfo>, Serializable {
        private final Map<String, AltUrlPartInfo> idMap;
        private final AltUrlPartInfo single; // optimization: majority of cases
        
        /**
         * WARN: must be a HashMap and not reused (opt).
         */
        AltUrlPartResults(Map<String, AltUrlPartInfo> idMap) {
            this.idMap = idMap;
            this.single = (idMap.size() == 1) ? idMap.values().iterator().next() : null;
        }
        
        /**
         * WARN: must be a HashMap and not reused (opt).
         */
        AltUrlPartResults(AltUrlPartInfo single) {
            Map<String, AltUrlPartInfo> idMap = new HashMap<>();
            idMap.put(single.getId(), single);
            this.idMap = idMap;
            this.single = single;
        }

        /**
         * Returns single result or null if there are zero or multiple.
         */
        public AltUrlPartInfo getSingle() {
            return single;
        }
        
        /**
         * WARN: may not return the original first DB result.
         */
        public AltUrlPartInfo getFirst() {
            return (idMap.size() >= 1) ? idMap.values().iterator().next() : null;
        }
        
        /**
         * NOTE: this is now used post-cache in order to lessen the cache.
         * The fastest case is when (exactOnly==false && allowIdOnly==true).
         */
        public AltUrlPartResults filterResults(boolean exactOnly, boolean allowIdOnly) {
            if (exactOnly) {
                Map<String, AltUrlPartInfo> newIdMap = new HashMap<>();
                if (allowIdOnly) {
                    for(Map.Entry<String, AltUrlPartInfo> entry : idMap.entrySet()) {
                        if (entry.getValue().isExact()) {
                            newIdMap.put(entry.getKey(), entry.getValue());
                        }
                    }
                } else {
                    for(Map.Entry<String, AltUrlPartInfo> entry : idMap.entrySet()) {
                        if (entry.getValue().isFullMatch()) {
                            newIdMap.put(entry.getKey(), entry.getValue());
                        }
                    }
                }
                return newIdMap.isEmpty() ? null : new AltUrlPartResults(newIdMap);
            } else {
                if (allowIdOnly) {
                    return this;
                } else {
                    Map<String, AltUrlPartInfo> newIdMap = new HashMap<>();
                    for(Map.Entry<String, AltUrlPartInfo> entry : idMap.entrySet()) {
                        if (!entry.getValue().isIdOnlyMatch()) {
                            newIdMap.put(entry.getKey(), entry.getValue());
                        }
                    }
                    return newIdMap.isEmpty() ? null : new AltUrlPartResults(newIdMap);
                }
            }
        }
        
        /**
         * @deprecated wrote this but safer not to do this
         */
        @Deprecated
        public AltUrlPartInfo getSingleOrExact() {
            if (single != null) {
                return single;
            } else {
                // NOTE: this is clumsy, should have made a class instead of Map<String, AltUrlPartInfo>
                AltUrlPartInfo lastExactInfo = null;
                for(AltUrlPartInfo info : idMap.values()) {
                    if (info.isExact()) {
                        if (lastExactInfo != null) return null; // more than one exact - no good
                        lastExactInfo = info;
                    }
                }
                return lastExactInfo;
            }
        }

        @Override public int size() { return idMap.size(); }
        @Override public boolean isEmpty() { return idMap.isEmpty(); }
        @Override public boolean containsKey(Object key) { return idMap.containsKey(key); }
        @Override public boolean containsValue(Object value) { return idMap.containsValue(value); }
        @Override public AltUrlPartInfo get(Object key) { return idMap.get(key); }
        @Override public AltUrlPartInfo put(String key, AltUrlPartInfo value) { throw new UnsupportedOperationException(); }
        @Override public AltUrlPartInfo remove(Object key) { throw new UnsupportedOperationException(); }
        @Override public void putAll(Map<? extends String, ? extends AltUrlPartInfo> m) { throw new UnsupportedOperationException(); }
        @Override public void clear() { throw new UnsupportedOperationException(); }
        @Override public Set<String> keySet() { return Collections.unmodifiableMap(idMap).keySet(); }
        @Override public Collection<AltUrlPartInfo> values() { return Collections.unmodifiableMap(idMap).values(); }
        @Override public Set<java.util.Map.Entry<String, AltUrlPartInfo>> entrySet() { return Collections.unmodifiableMap(idMap).entrySet(); }
    }
    
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
        
        public AltUrlPartInfo(boolean exact, boolean idOnlyMatch, String id, String name, String localeString) {
            this.exact = exact;
            this.idOnlyMatch = idOnlyMatch;
            this.id = id;
            this.name = name;
            this.localeString = localeString;
        }
        
        /**
         * The ID from DB (NOT from the orig URL).
         */
        public String getId() { return id; }
        /**
         * The name from DB (NOT from the orig URL).
         */
        public String getName() { return name; }
        /**
         * The locale from DB or null if none.
         */
        public String getLocaleString() { return localeString; }
        
        /**
         * true if we had an ID match; false if name-only match.
         */
        public boolean isExact() { return exact; }
        /**
         * true if we had an ID match without name match.
         */
        public boolean isIdOnlyMatch() { return idOnlyMatch; }
        /**
         * true if full name+id match.
         */
        public boolean isFullMatch() { return exact && idOnlyMatch; }

        /**
         * NOTE: for our purposes, we consider name+id more precise than id only.
         */
        public boolean isMorePreciseThan(AltUrlPartInfo other) {
            if (other == null) return true;
            else if (!this.exact) return false; // we are name-only (lowest precision)
            else return (!other.exact) || (!this.idOnlyMatch && other.idOnlyMatch);
        }
        
        public AltUrlPartResults getAsResult() { return new AltUrlPartResults(this); }
    }
    
    public AltUrlPartResults extractCandidateAltUrlProductIdCached(Delegator delegator, String altUrl, boolean exactOnly, boolean allowIdOnly) throws GenericEntityException {
        String key = altUrl;
        
        AltUrlPartResults results = productAltUrlPartInfoCache.get(key);
        if (results == null) {
            boolean singleExactOnly = false; // NOTE: there is a 0.001% chance of multiple exact matches, slightly safer if false
            results = extractCandidateAltUrlProductId(delegator, altUrl, false, singleExactOnly, true);
            
            // NOTE: currently, only storing in cache if has match... 
            // this is tradeoff of memory vs misses (risky to allow empty due to incoming from public)
            if (!results.isEmpty()) {
                productAltUrlPartInfoCache.put(key, results);
            }
        }
        return results != null ? results.filterResults(exactOnly, allowIdOnly) : null;
    }
    
    /**
     * SCIPIO: Tries to match an alt URL path element to a product.
     * Heavily modified logic from CatalogUrlFilter.
     * <p>
     * Added 2017-11-08.
     */
    public AltUrlPartResults extractCandidateAltUrlProductId(Delegator delegator, String altUrl, boolean exactOnly, boolean singleExactOnly, boolean allowIdOnly) throws GenericEntityException {
        Map<String, AltUrlPartInfo> results = new HashMap<>();
        AltUrlPartInfo exactResult = null;
        
        // SCIPIO: this is a new filter that narrows down results from DB, which otherwise may be huge.
        EntityCondition matchTextIdCond = makeAltUrlTextIdMatchCombinations(altUrl, "productId", "textData", exactOnly);
        if (matchTextIdCond == null) new AltUrlPartResults(results);
        
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
        exactResult = findExtractAltUrlValueId(altUrl, productContentInfos, "productId", CatalogUrlType.PRODUCT, exactOnly, singleExactOnly, results);
        if (exactResult != null && exactResult.isExact()) {
            new AltUrlPartResults(results);
        }

        // Search for non-localized alt urls
        condList = new ArrayList<>();
        condList.add(contentTypeIdCond);
        if (matchTextIdCond != null) condList.add(matchTextIdCond);
        productContentInfos = EntityQuery.use(delegator).from("ProductContentAndElecTextShort")
                .where(condList).select("productId", "textData", "localeString")
                .filterByDate(moment)
                .orderBy("-fromDate").cache(true).filterByDate().queryList();
        exactResult = findExtractAltUrlValueId(altUrl, productContentInfos, "productId", CatalogUrlType.PRODUCT, exactOnly, singleExactOnly, results);
        if (exactResult != null && exactResult.isExact()) {
            new AltUrlPartResults(results);
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
        
        return new AltUrlPartResults(results);
    }
    
    public AltUrlPartResults extractCandidateAltUrlCategoryIdCached(Delegator delegator, String altUrl, boolean exactOnly, boolean allowIdOnly) throws GenericEntityException {
        String key = altUrl;
        
        AltUrlPartResults results = categoryAltUrlPartInfoCache.get(key);
        if (results == null) {
            boolean singleExactOnly = false; // NOTE: there is a 0.001% chance of multiple exact matches, slightly safer if false
            results = extractCandidateAltUrlCategoryId(delegator, altUrl, false, singleExactOnly, true);
            
            // NOTE: currently, only storing in cache if has match... 
            // this is tradeoff of memory vs misses (risky to allow empty due to incoming from public)
            if (!results.isEmpty()) {
                categoryAltUrlPartInfoCache.put(key, results);
            }
        }
        return results != null ? results.filterResults(exactOnly, allowIdOnly) : null;
    }
    
    public List<AltUrlPartResults> extractCandidateAltUrlCategoryIdsCached(Delegator delegator, Collection<String> altUrls, boolean exactOnly, boolean allowIdOnly) throws GenericEntityException {
        List<AltUrlPartResults> result = new ArrayList<>();
        for(String altUrl : altUrls) {
            result.add(extractCandidateAltUrlCategoryIdCached(delegator, altUrl, exactOnly, allowIdOnly));
        }
        return result;
    }
    
    /**
     * SCIPIO: Tries to match an alt URL path element to a category.
     * Heavily modified logic from CatalogUrlFilter.
     * <p>
     * Added 2017-11-07.
     */
    public AltUrlPartResults extractCandidateAltUrlCategoryId(Delegator delegator, String altUrl, boolean exactOnly, boolean singleExactOnly, boolean allowIdOnly) throws GenericEntityException {
        Map<String, AltUrlPartInfo> results = new HashMap<>();
        AltUrlPartInfo exactResult = null;
        
        // SCIPIO: this is a new filter that narrows down results from DB, which otherwise may be huge.
        EntityCondition matchTextIdCond = makeAltUrlTextIdMatchCombinations(altUrl, "productCategoryId", "textData", exactOnly);
        if (matchTextIdCond == null) return new AltUrlPartResults(results);
        
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
        exactResult = findExtractAltUrlValueId(altUrl, productCategoryContentInfos, "productCategoryId", CatalogUrlType.CATEGORY, exactOnly, singleExactOnly, results);
        if (exactResult != null && exactResult.isExact()) {
            return new AltUrlPartResults(results);
        }
        
        // Search for non-localized alt urls
        condList = new ArrayList<>();
        condList.add(contentTypeIdCond);
        if (matchTextIdCond != null) condList.add(matchTextIdCond);
        productCategoryContentInfos = EntityQuery.use(delegator).from("ProductCategoryContentAndElecTextShort")
                .where(condList).select("productCategoryId", "textData", "localeString")
                .filterByDate(moment)
                .orderBy("-fromDate").cache(true).queryList();
        exactResult = findExtractAltUrlValueId(altUrl, productCategoryContentInfos, "productCategoryId", CatalogUrlType.CATEGORY, exactOnly, singleExactOnly, results);
        if (exactResult != null && exactResult.isExact()) {
            return new AltUrlPartResults(results);
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
        
        return new AltUrlPartResults(results);
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
     * <p>
     * NOTE: 2017: the singleExactOnly flag was initially going to be important for performance reasons,
     * but it can now be left to false (which is 0.01% safer) as long as the caller uses
     * {@link #makeAltUrlTextIdMatchCombinations} in the values query.
     */
    private static AltUrlPartInfo findExtractAltUrlValueId(String altUrl, List<GenericValue> values, String idField, 
            CatalogUrlType entityType, boolean exactOnly, boolean singleExactOnly, Map<String, AltUrlPartInfo> results) {
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
                            AltUrlPartInfo urlInfo = new AltUrlPartInfo(true, false, valueId, textData, value.getString("localeString"));
                            if (singleExactOnly) {
                                results.clear();
                                results.put(valueId, urlInfo);
                                return urlInfo;
                            } else {
                                results.put(valueId, urlInfo);
                            }
                        }
                    }
                }
            }
        }
        return null;
    }
    
    /*
     * *****************************************************
     * Outbound matching
     * *****************************************************
     */
    
    private static final Pattern absUrlPat = Pattern.compile("(((.*?):)?//([^/]*))?(.*)");
    
    /**
     * Checks an outbound URL for /control/product, /control/category or other
     * such request and tries to extract the IDs.
     * FIXME: does not preserve trail, uses default always. Also didn't fully abstract this yet.
     * FIXME: cannot handle jsessionid
     */
    public String matchReplaceOutboundSeoTranslatableUrl(HttpServletRequest request, Delegator delegator, String url, String productReqPath, String categoryReqPath, String contextRoot) {
        if (url == null) return null;
        // pre-filters for speed
        if (url.contains(productReqPath) || url.contains(categoryReqPath)) {
            // Extract the relative URL from absolute
            Matcher mrel = absUrlPat.matcher(url);
            if (mrel.matches()) {
                
                String absPrefix = mrel.group(1);
                if (absPrefix == null) {
                    absPrefix = "";
                }
                String relUrl = mrel.group(5);
                
                // Check if within same webapp
                // (Note: in Ofbiz the encoded URLs contain the webapp context root in relative URLs)
                if (relUrl.startsWith((contextRoot.length() > 1) ? contextRoot + "/" : contextRoot)) {
                    String pathInfo = relUrl.substring(contextRoot.length());
                    if (pathInfo.startsWith(productReqPath)) {
                        if (pathInfo.length() > productReqPath.length()) {
                            Map<String, String> params = extractParamsFromRest(pathInfo, productReqPath.length());
                            if (params == null) return null;
                            String productId = params.remove("product_id");
                            if (productId == null) {
                                productId = params.remove("productId");
                            }
                            if (productId != null) {
                                String colonString = params.remove("colonString");
                                if (colonString == null) colonString = "";
                                
                                Locale locale = UtilHttp.getLocale(request); // FIXME?: sub-ideal
                                String newUrl = makeProductUrl(request, locale, null, null, productId);
                                if (newUrl == null) return null;

                                String remainingParams = makeRemainingParamsStr(params, pathInfo.contains("&amp;"), !newUrl.contains("?"));
 
                                return absPrefix + newUrl + colonString + remainingParams;
                            }
                        }
                    } else if (pathInfo.startsWith(categoryReqPath)) {
                        if (pathInfo.length() > categoryReqPath.length()) {
                            Map<String, String> params = extractParamsFromRest(pathInfo, categoryReqPath.length());
                            if (params == null) return null;
                            String categoryId = params.remove("category_id");
                            if (categoryId == null) {
                                categoryId = params.remove("productCategoryId");
                            }
                            if (categoryId != null) {
                                String colonString = params.remove("colonString");
                                if (colonString == null) colonString = "";
                                
                                Locale locale = UtilHttp.getLocale(request); // FIXME?: sub-ideal
                                // FIXME miss view size
                                String newUrl = makeCategoryUrl(request, locale, null, categoryId, null, null, null, null, null);
                                if (newUrl == null) return null;
                                
                                String remainingParams = makeRemainingParamsStr(params, pathInfo.contains("&amp;"), !newUrl.contains("?"));
                                
                                return absPrefix + newUrl + colonString + remainingParams;
                            }
                        }
                    }
                }
            }
        }
        return null;
    }

    private static final Pattern paramPat = Pattern.compile("&amp;|&");
    
    private static Map<String, String> extractParamsFromRest(String pathInfo, int index) {
        char nextChar = pathInfo.charAt(index);
        String queryString;
        String colonString;
        if (nextChar == '?') {
            // FIXME: can't handle colon parameters
            colonString = null;
            queryString = pathInfo.substring(index + 1);
//            int colonIndex = pathInfo.indexOf(';', index + 1);
//            if (colonIndex >= 0) {
//                colonString = pathInfo.substring(colonIndex);
//                queryString = pathInfo.substring(index + 1, colonIndex);
//            } else {
//                queryString = pathInfo.substring(index + 1);
//                colonString = null;
//            }
//        } else if (nextChar == ';') {
//            int colonIndex = nextChar;
//            index = pathInfo.indexOf('?', index+1);
//            if (index < 0) return new HashMap<>();
//            colonString = pathInfo.substring(colonIndex, index);
//            queryString = pathInfo.substring(index + 1);
        } else {
            return null;
        }
        Map<String, String> params = extractParamsFromQueryString(queryString);
        if (colonString != null) params.put("colonString", colonString);
        return params;
    }
    
    private static Map<String, String> extractParamsFromQueryString(String queryString) {
        String[] parts = paramPat.split(queryString);
        Map<String, String> params = new LinkedHashMap<>();
        for(String part : parts) {
            if (part.length() == 0) continue;
            int equalsIndex = part.indexOf('=');
            if (equalsIndex <= 0 || equalsIndex >= (part.length() - 1)) continue;
            params.put(part.substring(0, equalsIndex), part.substring(equalsIndex + 1));
        }
        return params;
    }

    private static String makeRemainingParamsStr(Map<String, String> params, boolean useEncoded, boolean firstParams) {
        StringBuilder sb = new StringBuilder();
        for(Map.Entry<String, String> entry : params.entrySet()) {
            if (sb.length() == 0 && firstParams) {
                sb.append("?");
            } else {
                sb.append(useEncoded ? "&amp;" : "&");
            }
            sb.append(entry.getKey());
            sb.append("=");
            sb.append(entry.getValue());
        }
        return sb.toString();
    }
    
    /*
     * *****************************************************
     * Abstracted general queries for product/category URLs
     * *****************************************************
     * NOTE: some of these may be cacheable if needed in future.
     */
    
    /**
     * Returns the top categories that are valid for use when generating category URLs,
     * as ORDERED set.
     * <p>
     * 2017: NOTE: for legacy behavior, this returns all ProdCatalogCategory IDs for category URLs (only).
     */
    protected Set<String> getCatalogTopCategoriesForCategoryUrl(Delegator delegator, String currentCatalogId, String webSiteId) {
        if (currentCatalogId == null) {
            currentCatalogId = getCurrentCatalogId(delegator, currentCatalogId, webSiteId);
            if (currentCatalogId == null) return new LinkedHashSet<>();
        }
        return getProdCatalogCategoryIds(delegator, currentCatalogId, null);
    }
    
    /**
     * Returns the top categories that are valid for use when generating product URLs,
     * as ORDERED set.
     * <p>
     * 2017: NOTE: for legacy behavior, this currently returns only the single first top catalog category.
     * <p>
     * TODO: REVIEW: this could support more, but it may affect store browsing.
     */
    protected Set<String> getCatalogTopCategoriesForProductUrl(Delegator delegator, String currentCatalogId, String webSiteId) {
        if (currentCatalogId == null) {
            currentCatalogId = getCurrentCatalogId(delegator, currentCatalogId, webSiteId);
            if (currentCatalogId == null) return new LinkedHashSet<>();
        }
        return getProdCatalogTopCategoryId(delegator, currentCatalogId);
    }

    /**
     * Return all paths from the given topCategoryIds to the product.
     * <p>
     * TODO?: perhaps can cache with UtilCache in future, or read from a cached category tree.
     */
    protected List<List<String>> getProductRollupTrails(Delegator delegator, String productId, Set<String> topCategoryIds) {
        return ProductWorker.getProductRollupTrails(delegator, productId, topCategoryIds, true);
    }
    
    /**
     * Return all paths from the given topCategoryIds to the category.
     * <p>
     * TODO?: perhaps can cache with UtilCache in future, or read from a cached category tree.
     */
    protected List<List<String>> getCategoryRollupTrails(Delegator delegator, String productCategoryId, Set<String> topCategoryIds) {
        return CategoryWorker.getCategoryRollupTrails(delegator, productCategoryId, topCategoryIds, true);
    }
    
    /*
     * *****************************************************
     * Generic/static helpers
     * *****************************************************
     */

    /**
     * Returns all ProdCatalogCategory IDs or the types requested (null for all).
     */
    protected static Set<String> getProdCatalogCategoryIds(Delegator delegator, String prodCatalogId, Collection<String> prodCatalogCategoryTypeIds) {
        if (prodCatalogId == null || prodCatalogId.isEmpty()) return new LinkedHashSet<>();
        List<GenericValue> values = CatalogWorker.getProdCatalogCategories(delegator, prodCatalogId, null);
        Set<String> idList = new LinkedHashSet<>();
        for(GenericValue value : values) {
            if (prodCatalogCategoryTypeIds == null || prodCatalogCategoryTypeIds.contains(value.getString("prodCatalogCategoryTypeId"))) {
                idList.add(value.getString("productCategoryId"));
            }
        }
        return idList;
    }
    
    /**
     * Returns the single top category.
     */
    protected static Set<String> getProdCatalogTopCategoryId(Delegator delegator, String prodCatalogId) {
        String topCategoryId = CatalogWorker.getCatalogTopCategoryId(delegator, prodCatalogId);
        Set<String> topCategoryIds = new LinkedHashSet<>();
        if (topCategoryId == null) {
            Debug.logWarning("Seo: matchInboundSeoCatalogUrl: cannot determine top category for prodCatalogId '" + prodCatalogId + "'; can't select best trail", module);
        } else {
            topCategoryIds.add(topCategoryId);
        }
        return topCategoryIds;
    }
    
    protected static String getCurrentCatalogId(Delegator delegator, String currentCatalogId, String webSiteId) {
        if (currentCatalogId == null) {
            currentCatalogId = getWebsiteTopCatalog(delegator, webSiteId);
            if (currentCatalogId == null) {
                Debug.logWarning("Seo: Cannot determine current or top catalog for webSiteId: " + webSiteId, module);
            }
        }
        return currentCatalogId;
    }
    
    protected static String getWebsiteTopCatalog(Delegator delegator, String webSiteId) {
        if (UtilValidate.isEmpty(webSiteId)) {
            return null;
        }
        try {
            GenericValue webSite = EntityQuery.use(delegator).from("WebSite").where("webSiteId", webSiteId).cache(true).queryOne();
            if (webSite == null) {
                Debug.logError("getWebsiteTopCatalog: Invalid webSiteId: " + webSiteId, module);
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
    
    protected static List<String> newPathList() { 
        return new ArrayList<>();
    }
    
    protected static List<String> newPathList(int initialCapacity) { 
        return new ArrayList<>(initialCapacity);
    }
    
    protected static List<String> ensurePathList(List<String> pathList) {
        return pathList != null ? pathList : newPathList();
    }
    
    /**
     * Returns the first trail having the topCategory which is the earliest possible in the topCategoryIds list,
     * or null if none of them.
     * Prefers lower ProdCatalogCategory sequenceNum.
     */
    protected static List<String> getFirstTopTrail(List<List<String>> possibleTrails, Collection<String> topCategoryIds) {
        for(String topCategoryId : topCategoryIds) { // usually just one iteration
            for(List<String> trail : possibleTrails) {
                if (trail != null && !trail.isEmpty() && topCategoryId.equals(trail.get(0))) return trail;
            }
        }
        return null;
    }
    
    // NOTE: if need this, there's should already be a util somewhere...
//    private static boolean pathStartsWithDir(String path, String dir) {
//        // needs delimiter logic
//        if (UtilValidate.isEmpty(path)) {
//            return UtilValidate.isEmpty(dir);
//        }
//        if (path.length() > dir.length()) {
//            if (dir.endsWith("/")) return path.startsWith(dir);
//            else return path.startsWith(dir + "/");
//        } else {
//            return path.equals(dir);
//        }
//    } 
    
    private static <T> void removeLastIfEquals(List<T> list, T value) {
        if (list != null && list.size() > 0 && value != null && value.equals(list.get(list.size() - 1))) {
            list.remove(list.size() - 1);
        }
    }
    
    /**
     * Last index of, with starting index (inclusive - .get(startIndex) is compared - like String interface).
     */
    static <T> int lastIndexOf(List<T> list, T object, int startIndex) {
        ListIterator<T> it = list.listIterator(startIndex + 1);
        while(it.hasPrevious()) {
            if (object.equals(it.previous())) {
                return it.nextIndex();
            }
        }
        return -1;
    }
    
    static void ensureTrailingSlash(StringBuilder sb) {
        if (sb.length() == 0 || sb.charAt(sb.length() - 1) != '/') {
            sb.append("/");
        }
    }
    
    static void appendSlashAndValue(StringBuilder sb, String value) {
        if (sb.length() == 0 || sb.charAt(sb.length() - 1) != '/') {
            sb.append("/");
        }
        sb.append(value);
    }
    
    static void appendSlashAndValue(StringBuilder sb, Collection<String> values) {
        for(String value : values) {
            if (sb.length() == 0 || sb.charAt(sb.length() - 1) != '/') {
                sb.append("/");
            }
            sb.append(value);
        }
    }
    
    /**
     * Maps locale to name and vice-versa - optimization to avoid ResourceBundle, which
     * has API limitations and needless lookups.
     * <p>
     * FIXME?: This has issues and might not 100% emulate the properties map behavior yet.
     */
    static class LocalizedName implements Serializable {
        private final String defaultValue;
        private final Map<String, String> localeValueMap;
        private final Map<String, Locale> valueLocaleMap;
        
        public LocalizedName(Map<String, String> localeValueMap, Locale defaultLocale) {
            this.localeValueMap = new HashMap<>(localeValueMap);
            this.valueLocaleMap = makeValueLocaleMap(localeValueMap);
            this.defaultValue = getNameForLocale(defaultLocale);
        }
        
        /**
         * NOTE: the resulting mapping may not be 1-for-1 - depends on the values.
         */
        static Map<String, Locale> makeValueLocaleMap(Map<String, String> localeValueMap) {
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
            return valueLocaleMap;
        }
        
        /**
         * Tries to create an instance from a property message.
         * <p>
         * FIXME?: there must be a better way than current loop with exception on missing locale...
         */
        public static LocalizedName getNormalizedFromProperties(String resource, String name, Locale defaultLocale) {
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
            return new LocalizedName(localeValueMap, defaultLocale);
        }
        
        /**
         * Tries to create an instance from a property message, with general.properties fallback
         * locale as the default locale.
         */
        public static LocalizedName getNormalizedFromProperties(String resource, String name) {
            return getNormalizedFromProperties(resource, name, UtilProperties.getFallbackLocale());
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
}

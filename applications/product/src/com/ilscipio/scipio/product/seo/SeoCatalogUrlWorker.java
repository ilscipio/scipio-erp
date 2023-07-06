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

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
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
import java.util.stream.Collectors;

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
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.product.category.CatalogUrlFilter.CatalogAltUrlBuilder;
import org.ofbiz.product.category.CatalogUrlServlet.CatalogUrlBuilder;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.product.category.CategoryWorker;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.product.product.ProductWorker;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.FullWebappInfo;

import com.ilscipio.scipio.ce.util.SeoStringUtil;
import com.ilscipio.scipio.product.category.CatalogAltUrlSanitizer;
import com.ilscipio.scipio.product.category.CatalogUrlType;

/**
 * SEO url building functions and callbacks.
 *
 * <p>Some parts adapted from the original <code>org.ofbiz.product.category.ftl.CatalogUrlSeoTransform</code>;
 * others re-done based on {@link org.ofbiz.product.category.CatalogUrlFilter}.</p>
 *
 * <p><strong>WARN:</strong> Do not call makeXxxUrl methods from this class from client code!
 * Client code that need java methods should use (which these plug into):</p>
 * <ul>
 * <li>{@link org.ofbiz.product.category.CatalogUrlFilter#makeCatalogAltLink}</li>
 * <li>{@link org.ofbiz.product.category.CatalogUrlServlet#makeCatalogLink}</li>
 * </ul>
 *
 * <p>FIXME: makeXxxUrlPath methods do not respect useCache flag</p>
 *
 * <p>SCIPIO: 3.0.0: Enhanced inbound URL matching logic, lookups and heuristic.</p>
 */
@SuppressWarnings("serial")
public class SeoCatalogUrlWorker implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String DEFAULT_CONFIG_RESOURCE = "SeoConfigUiLabels";

    public static final boolean DEBUG = false;

    // TODO: in production, these cache can be tweaked with non-soft refs, limits and expire time
    private static final UtilCache<String, PathSegmentEntities> productPathSegmentEntitiesCache = UtilCache.createUtilCache("seo.filter.product.pathsegment.entities", true);
    //private static final long productPathSegmentEntitiesCacheExpireTimeMs = productPathSegmentEntitiesCache.getExpireTime();

    private static final UtilCache<String, PathSegmentEntities> categoryPathSegmentEntitiesCache = UtilCache.createUtilCache("seo.filter.category.pathsegment.entities", true);
    //private static final long categoryPathSegmentEntitiesCacheExpireTimeMs = categoryPathSegmentEntitiesCache.getExpireTime();

    private static final UtilCache<String, String> productUrlCache = UtilCache.createUtilCache("seo.filter.product.url", true);
    private static final UtilCache<String, TrailCacheEntry> productTrailCache = UtilCache.createUtilCache("seo.filter.product.trails", true);
    private static final UtilCache<String, String> categoryUrlCache = UtilCache.createUtilCache("seo.filter.category.url", true);
    private static final UtilCache<String, TrailCacheEntry> categoryTrailCache = UtilCache.createUtilCache("seo.filter.category.trails", true);

    private static final EntityCondition productContentTypeIdAltUrlCond = EntityCondition.makeCondition("productContentTypeId", "ALTERNATIVE_URL");
    private static final EntityCondition contentAssocTypeIdAltLocaleCond = EntityCondition.makeCondition("contentAssocTypeId", "ALTERNATE_LOCALE");
    private static final Set<String> productContentAndElecTextShortMinimalSelectFields = UtilMisc.toSet("productId", "textData", "localeString", "contentId", "dataResourceId", "fromDate", "thruDate");
    private static final Set<String> productContentAssocAndElecTextShortMinimalSelectFields = UtilMisc.toSet("productId", "textData", "localeString", "contentId", "dataResourceId", "fromDate", "thruDate", "caFromDate", "caThruDate");

    private static final EntityCondition prodCatContentTypeIdAltUrlCond = EntityCondition.makeCondition("prodCatContentTypeId", "ALTERNATIVE_URL");
    private static final Set<String> productCategoryContentAndElecTextShortMinimalSelectFields = UtilMisc.toSet("productCategoryId", "textData", "localeString", "contentId", "dataResourceId", "fromDate", "thruDate");
    private static final Set<String> productCategoryContentAssocAndElecTextShortMinimalSelectFields = UtilMisc.toSet("productCategoryId", "textData", "localeString", "contentId", "dataResourceId", "fromDate", "thruDate", "caFromDate", "caThruDate");

    protected static class TrailCacheEntry implements Serializable {
        protected final Set<String> topCategoryIds;
        protected final List<List<String>> trails;
        protected TrailCacheEntry(Set<String> topCategoryIds, List<List<String>> trails) {
            this.topCategoryIds = topCategoryIds;
            this.trails = (trails != null) ? trails : Collections.emptyList();
        }
        public Set<String> getTopCategoryIds() { return topCategoryIds; }
        public List<List<String>> getTrails() { return trails; }
    }

    static {
        CatalogUrlBuilder.registerUrlBuilder("seo", BuilderFactory.getInstance());
        CatalogAltUrlBuilder.registerUrlBuilder("seo", BuilderFactory.getInstance());
    }

    public static void initStatic() {
        // formality
    }

    private static final class Instances {
        private static final SeoCatalogUrlWorker DEFAULT = new SeoCatalogUrlWorker();
    }

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
        this.catalogUrlBuilder = createCatalogUrlBuilder();
        this.catalogAltUrlBuilder = createCatalogAltUrlBuilder();
        this.catalogAltUrlSanitizer = createCatalogAltUrlSanitizer();
    }

    protected SeoCatalogUrlWorker() {
        this(SeoConfig.getCommonConfig());
    }

    public static class Factory<T extends SeoCatalogUrlWorker> implements Serializable {
        protected static final Factory<SeoCatalogUrlWorker> DEFAULT = new Factory<>();
        public static Factory<SeoCatalogUrlWorker> getDefault() { return DEFAULT; }
        public SeoCatalogUrlWorker getUrlWorker(SeoConfig config) {
            return new SeoCatalogUrlWorker(config);
        }
    }

    protected SeoCatalogUrlBuilder createCatalogUrlBuilder() {
        return new SeoCatalogUrlBuilder();
    }

    protected SeoCatalogAltUrlBuilder createCatalogAltUrlBuilder() {
        return new SeoCatalogAltUrlBuilder();
    }

    protected CatalogAltUrlSanitizer createCatalogAltUrlSanitizer() {
        return new SeoCatalogAltUrlSanitizer();
    }

    /**
     * Returns an instance with possible website-specific configuration.
     */
    public static SeoCatalogUrlWorker getInstance(Delegator delegator, String webSiteId) {
        return getInstance(SeoConfig.getConfig(delegator, webSiteId), delegator, webSiteId);
    }

    /**
     * Returns an instance with possible website-specific configuration.
     */
    public static SeoCatalogUrlWorker getInstance(SeoConfig config, Delegator delegator, String webSiteId) {
        return config.getUrlWorker();
    }

    /**
     * Returns an instance that is UNABLE to perform website-specific operations.
     */
    public static SeoCatalogUrlWorker getDefaultInstance(Delegator delegator) {
        if (SeoConfig.DEBUG_FORCERELOAD) return createInstanceDeep(delegator, null);
        else return Instances.DEFAULT;
    }

    /**
     * Force create new instance - for debugging only!
     */
    public static SeoCatalogUrlWorker createInstance(Delegator delegator, String webSiteId) {
        SeoConfig config = SeoConfig.getConfig(delegator, webSiteId);
        return config.getUrlWorkerFactory().getUrlWorker(config);
    }

    /**
     * Force create new instance deep - for debugging only!
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
        public CatalogUrlBuilder getCatalogUrlBuilder(Delegator delegator, FullWebappInfo targetWebappInfo) {
            SeoConfig config = SeoConfig.getConfig(delegator, targetWebappInfo);
            if (!config.isSeoUrlEnabled(targetWebappInfo.getContextPath(), targetWebappInfo.getWebSiteId())) return null;
            return SeoCatalogUrlWorker.getInstance(config, delegator, targetWebappInfo.getWebSiteId()).getCatalogUrlBuilder();
        }
        @Override
        public CatalogAltUrlBuilder getCatalogAltUrlBuilder(Delegator delegator, FullWebappInfo targetWebappInfo) {
            SeoConfig config = SeoConfig.getConfig(delegator, targetWebappInfo);
            if (!config.isSeoUrlEnabled(targetWebappInfo.getContextPath(), targetWebappInfo.getWebSiteId())) return null;
            return SeoCatalogUrlWorker.getInstance(config, delegator, targetWebappInfo.getWebSiteId()).getCatalogAltUrlBuilder();
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

    /**
     * NOTE: This can return null.
     */
    public String getWebSiteId() {
        return getConfig().getWebSiteId();
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
        return config.getSeoUrlSuffix();
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
     * Re-generates a link from PathMatch info (TODO).
     */
    public String makeCatalogLink(Delegator delegator, PathMatch urlInfo, Locale locale) {
        throw new UnsupportedOperationException("makeCatalogLink for PathMatch not yet implemented");
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
                String previousCategoryId) {
            if (UtilValidate.isNotEmpty(productId)) {
                return makeProductUrl(request, locale, previousCategoryId, currentCategoryId, productId);
            } else {
                return makeCategoryUrl(request, locale, previousCategoryId, currentCategoryId, productId, null, null, null, null);
            }
            //return CatalogUrlBuilder.getDefaultBuilder().makeCatalogUrl(request, locale, productId, currentCategoryId, previousCategoryId);
        }

        @Override
        public String makeCatalogUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, FullWebappInfo targetWebappInfo, String currentCatalogId, List<String> crumb, String productId,
                String currentCategoryId, String previousCategoryId) {
            if (UtilValidate.isNotEmpty(productId)) {
                return makeProductUrl(delegator, dispatcher, locale, crumb, targetWebappInfo, currentCatalogId, previousCategoryId, currentCategoryId, productId);
            } else {
                return makeCategoryUrl(delegator, dispatcher, locale, crumb, targetWebappInfo, currentCatalogId, previousCategoryId, currentCategoryId, productId, null, null, null, null);
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
                String productId) {
            return makeProductUrl(request, locale, previousCategoryId, productCategoryId, productId);
            //return CatalogAltUrlBuilder.getDefaultBuilder().makeProductAltUrl(request, locale, previousCategoryId, productCategoryId, productId);
        }

        @Override
        public String makeProductAltUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail,
                FullWebappInfo targetWebappInfo, String currentCatalogId, String previousCategoryId, String productCategoryId, String productId) {
            return makeProductUrl(delegator, dispatcher, locale, trail, targetWebappInfo, currentCatalogId, previousCategoryId, productCategoryId, productId);
            //return CatalogAltUrlBuilder.getDefaultBuilder().makeProductAltUrl(delegator, dispatcher, locale, trail, contextPath, previousCategoryId, productCategoryId, productId);
        }

        @Override
        public String makeCategoryAltUrl(HttpServletRequest request, Locale locale, String previousCategoryId,
                String productCategoryId, String productId, String viewSize, String viewIndex, String viewSort,
                String searchString) {
            return makeCategoryUrl(request, locale, previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString);
            //return CatalogAltUrlBuilder.getDefaultBuilder().makeCategoryAltUrl(request, locale, previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString);
        }

        @Override
        public String makeCategoryAltUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail,
                FullWebappInfo targetWebappInfo, String currentCatalogId, String previousCategoryId, String productCategoryId, String productId,
                String viewSize, String viewIndex, String viewSort, String searchString) {
            return makeCategoryUrl(delegator, dispatcher, locale, trail, targetWebappInfo, currentCatalogId, previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString);
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
        public String convertNameToDbAltUrl(String name, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo) {
            name = processNameToAltUrl(name, locale, entityType, ctxInfo);
            name = normalizeAltUrl(name, locale, entityType, ctxInfo);
            return name;
        }

        @Override
        public String convertIdToDbAltUrl(String id, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo) {
            // TODO: REVIEW: leaving this same as live for now... doubtful...
            return convertIdToLiveAltUrl(id, locale, entityType, ctxInfo);
        }

        @Override
        public String sanitizeAltUrlFromDb(String altUrl, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo) {
            // WARN: due to content wrapper the locale might not be the one from the pathSegment!!
            // may also be null
            if (altUrl == null) return "";

            // 2017: LEAVE THIS METHOD EMPTY - allows better DB queries if no post-processing.

            // TODO: REVIEW: REMOVED all post-db processing for now
            // - omitting could permit better DB queries
            // the reason this existed in the first place was because users can input garbage
            // through the UI, could could prevent in other ways...
            //pathSegment = UrlServletHelper.invalidCharacter(pathSegment); // (stock ofbiz)
            return altUrl;
        }

        @Override
        public String convertIdToLiveAltUrl(String id, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo) {
            // TODO: REVIEW: this is what the old Seo code did, but it will just not work in the filters...
            // People should not generate DB IDs with spaces
            //return id.trim().replaceAll(" ", SeoStringUtil.URL_HYPHEN);
            return id;
        }

        protected String processNameToAltUrl(String name, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo) {
            return getConfig().getAltUrlGenProcessors().processUrl(name);
        }

        protected String normalizeAltUrl(String name, Locale locale, CatalogUrlType entityType, SanitizeContext ctxInfo) {
            if (entityType == CatalogUrlType.PRODUCT) {
                name = truncateAltUrl(name, getConfig().getProductNameMaxLength());
            } else if (entityType == CatalogUrlType.CATEGORY) {
                name = truncateAltUrl(name, getConfig().getCategoryNameMaxLength());
            }
            return trimAltUrl(name);
        }

        protected String truncateAltUrl(String name, Integer maxLength) {
            if (name == null || maxLength == null || maxLength < 0 || name.length() <= maxLength) {
                return name;
            }
            return name.substring(0, maxLength);
        }

        protected String trimAltUrl(String name) {
            return SeoStringUtil.trimSeoName(name);
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
     * Convert list of categoryIds to formatted alt url names, EXCLUDING the passed targetCategory (skips last elem if matches).
     * WARNING: this method may change without notice.
     * TODO?: refactor?: tries to cover too many cases
     */
    protected List<String> getCategoryTrailPathSegments(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<GenericValue> trailEntities, GenericValue targetCategory,
                                                        SeoConfig.TrailFormat trailFormat, SeoConfig.TrailFormat targetCategoryFormat, CatalogUrlType urlType, CatalogAltUrlSanitizer.SanitizeContext targetSanitizeCtx, boolean useCache) {
        if (trailEntities == null || trailEntities.isEmpty()) return newPathList();
        List<String> catNames = newPathList(trailEntities.size());
        ListIterator<GenericValue> trailIt = trailEntities.listIterator();
        CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx = getCatalogAltUrlSanitizer().makeSanitizeContext(targetSanitizeCtx);
        // TODO: REVIEW: Just copy it and override for now; previously there was no copy-constructor
        //CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx = getCatalogAltUrlSanitizer().makeSanitizeContext(delegator, dispatcher, locale, useCache);
        //if (targetSanitizeCtx != null) {
        //    sanitizeCtx.setTargetProduct(targetSanitizeCtx.getTargetProduct());
        //    sanitizeCtx.setTargetCategory(targetSanitizeCtx.getTargetCategory());
        //    sanitizeCtx.setTotalNames(targetSanitizeCtx.getTotalNames());
        //}
        sanitizeCtx.setNameIndex(0);
        sanitizeCtx.setLast(false);
        String targetCategoryId = (targetCategory != null) ? targetCategory.getString("productCategoryId") : null;
        while(trailIt.hasNext()) {
            GenericValue productCategory = trailIt.next();
            if (productCategory == null) {
                continue;
            }
            String productCategoryId = productCategory.getString("productCategoryId");
            if ("TOP".equals(productCategoryId)) continue;
            if (targetCategoryId != null && !trailIt.hasNext() && targetCategoryId.equals(productCategoryId)) {
                break; // skip last if it's the target
            }
            String trailSegment = getCategoryPathSegment(delegator, dispatcher, locale, productCategory, trailFormat, sanitizeCtx, useCache);
            if (trailSegment != null) catNames.add(trailSegment);
            sanitizeCtx.increaseNameIndex();
        }
        return catNames;
    }

    /**
     * Reads ALTERNATIVE_URL for category and locale from DB and builds config-specified alt url path part, or according to the passed format.
     * Fallback on ID.
     */
    public String getCategoryPathSegment(Delegator delegator, LocalDispatcher dispatcher, Locale locale, GenericValue productCategory, SeoConfig.TrailFormat format, CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx, boolean useCache) {
        String name = getCategoryPathName(delegator, dispatcher, locale, productCategory, format, sanitizeCtx, useCache);
        if (UtilValidate.isNotEmpty(name)) {
            if (!sanitizeCtx.isLast()) {
                if (getConfig().isCategoryNameAppendId()) {
                    name += SeoStringUtil.URL_HYPHEN + getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productCategory.getString("productCategoryId"), locale, CatalogUrlType.CATEGORY, sanitizeCtx);
                }
            } else {
                if (getConfig().isCategoryNameAppendIdLast()) {
                    name += SeoStringUtil.URL_HYPHEN + getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productCategory.getString("productCategoryId"), locale, CatalogUrlType.CATEGORY, sanitizeCtx);
                }
            }
            return name;
        } else { // fallback
            return getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productCategory.getString("productCategoryId"), locale, CatalogUrlType.CATEGORY, sanitizeCtx);
        }
    }

    protected String getCategoryPathName(Delegator delegator, LocalDispatcher dispatcher, Locale locale, GenericValue productCategory, SeoConfig.TrailFormat format, CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx, boolean useCache) {
        if (format == SeoConfig.TrailFormat.ID) {
            return getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(productCategory.getString("productCategoryId"), locale, CatalogUrlType.CATEGORY, sanitizeCtx);
        }
        return getCategoryAltUrl(delegator, dispatcher, locale, productCategory, sanitizeCtx, useCache);
    }

    protected String getCategoryAltUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, GenericValue productCategory, CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx, boolean useCache) {
        String altUrl = CategoryContentWrapper.getProductCategoryContentAsText(productCategory, "ALTERNATIVE_URL", locale, dispatcher, useCache, "raw");
        if (UtilValidate.isNotEmpty(altUrl)) {
            // FIXME: effective locale might not be same as "locale" variable!
            altUrl = getCatalogAltUrlSanitizer().sanitizeAltUrlFromDb(altUrl, locale, CatalogUrlType.CATEGORY, sanitizeCtx);
        }
        return altUrl;
    }

    protected List<GenericValue> getCategoriesFromIdList(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> categoryIdList, boolean useCache) {
        List<GenericValue> categoryList = new ArrayList<>(categoryIdList.size());
        for(String productCategoryId : categoryIdList) {
            try {
                GenericValue productCategory = EntityQuery.use(delegator).from("ProductCategory")
                        .where("productCategoryId", productCategoryId).cache(useCache).queryOne();
                categoryList.add(productCategory);
            } catch(Exception e) {
                Debug.logError(e, "Seo: Cannot get category '" + productCategoryId + "' alt url", module);
            }
        }
        return categoryList;
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
        FullWebappInfo targetWebappInfo = FullWebappInfo.fromRequest(request);
        String currentCatalogId = CatalogWorker.getCurrentCatalogId(request);
        return makeCategoryUrl(delegator, dispatcher, locale, trail, targetWebappInfo, currentCatalogId,
                    previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString);
    }

    /**
     * Make category url according to the configurations.
     */
    public String makeCategoryUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> currentTrail, FullWebappInfo targetWebappInfo,
                                  String currentCatalogId, String previousCategoryId, String productCategoryId, String productId, String viewSize, String viewIndex, String viewSort, String searchString) {
        final boolean useCache = true; // NOTE: this is for entity cache lookups, not util caches
        List<String> trail;
        if (getConfig().getCategoryUrlTrailFormat().isOn()) {
            trail = mapCategoryUrlTrail(delegator, currentTrail, productCategoryId, targetWebappInfo.getWebSiteId(), currentCatalogId);
        } else {
            trail = Collections.emptyList();
        }
        String key = getCategoryCacheKey(delegator, targetWebappInfo, "Default", locale, previousCategoryId, productCategoryId, productId,
                viewSize, viewIndex, viewSort, searchString, currentCatalogId, trail);
        String url = categoryUrlCache.get(key);
        if (url == null) {
            url = makeCategoryUrlImpl(delegator, dispatcher, locale, trail, targetWebappInfo, currentCatalogId,
                    previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString, useCache);
            categoryUrlCache.put(key, url);
            if (DEBUG) {
                Debug.logInfo("Seo: makeCategoryUrl: Created category url [" + url + "] for key [" + key + "]", module);
            }
        } else {
            if (DEBUG) {
                Debug.logInfo("Seo: makeCategoryUrl: Got cached category url [" + url + "] for key [" + key + "]", module);
            }
        }
        return url;
    }

    /**
     * NOTE: caching the trail effectively renders the entries cached per-category-page, but there is no way around with without adding more complexity.
     */
    protected static String getCategoryCacheKey(Delegator delegator, FullWebappInfo targetWebappInfo, String type, Locale locale, String previousCategoryId, String productCategoryId, String productId,
                                                String viewSize, String viewIndex, String viewSort, String searchString, String currentCatalogId, List<String> trail) {
        String localeStr;
        if (locale == null) {
            localeStr = UtilProperties.getPropertyValue("scipiosetup", "store.defaultLocaleString");
            if (UtilValidate.isEmpty(localeStr)) {
                localeStr = Locale.getDefault().toString();
            }
        } else {
            localeStr = locale.toString();
        }
        return delegator.getDelegatorName()+"::"+type+"::"+targetWebappInfo.getContextPath()+"::"+targetWebappInfo.getWebSiteId()+"::"+localeStr+"::"+previousCategoryId+"::"+productCategoryId+"::"
                +productId+"::"+viewSize+"::"+viewSort+"::"+searchString+"::"+currentCatalogId+"::"+String.join("/", trail);
    }

    /**
     * Creates a full trail to the given category using hints from the incoming trail to select best.
     * Caching.
     */
    protected List<String> mapCategoryUrlTrail(Delegator delegator, List<String> hintTrail, String productCategoryId, String webSiteId, String currentCatalogId) {
        List<String> trail = null;
        String trailKey = delegator.getDelegatorName() + "::" + webSiteId + "::" + productCategoryId + "::" + currentCatalogId;
        TrailCacheEntry trailEntry = categoryTrailCache.get(trailKey);
        if (trailEntry == null) {
            Set<String> topCategoryIds = getCatalogTopCategoriesForCategoryUrl(delegator, currentCatalogId, webSiteId);
            List<List<String>> trails = null;
            if (topCategoryIds.isEmpty()) {
                Debug.logWarning("Seo: mapCategoryUrlTrail: No top categories found for catalog '" + currentCatalogId + "'; can't select best trail", module);
                topCategoryIds = null;
            } else {
                trails = getCategoryRollupTrails(delegator, productCategoryId, topCategoryIds);
            }
            trailEntry = new TrailCacheEntry(topCategoryIds, trails);
            categoryTrailCache.put(trailKey, trailEntry);
        }
        if (trailEntry.getTopCategoryIds() != null) {
            trail = findBestTopCatTrailForNewUrl(delegator, trailEntry.getTrails(), hintTrail, trailEntry.getTopCategoryIds()); // fast
        }
        return (trail != null) ? trail : Collections.emptyList();
    }

    protected String makeCategoryUrlImpl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail, FullWebappInfo targetWebappInfo,
                                  String currentCatalogId, String previousCategoryId, String productCategoryId, String productId, String viewSize, String viewIndex, String viewSort, String searchString, boolean useCache) {
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

        // NO LONGER NEED ADJUST - in fact it will prevent the valid trail selection after this from working
        //trail = CategoryWorker.adjustTrail(trail, productCategoryId, previousCategoryId);
        StringBuilder urlBuilder = makeCategoryUrlCore(delegator, dispatcher, locale, productCategory, currentCatalogId, previousCategoryId,
                getCategoriesFromIdList(delegator, dispatcher, locale, trail, useCache), targetWebappInfo, useCache);

        appendCategoryUrlParam(urlBuilder, "viewIndex", viewIndex);
        appendCategoryUrlParam(urlBuilder, "viewSize", viewSize);
        appendCategoryUrlParam(urlBuilder, "viewSort", viewSort);
        appendCategoryUrlParam(urlBuilder, "searchString", searchString);
        if (urlBuilder.toString().endsWith("&")) {
            return urlBuilder.toString().substring(0, urlBuilder.toString().length()-1);
        }
        return urlBuilder.toString();
    }

    private void appendCategoryUrlParam(StringBuilder urlBuilder, String paramName, String paramValue) {
        if (UtilValidate.isNotEmpty(paramValue)) {
            if (!urlBuilder.toString().endsWith("?") && !urlBuilder.toString().endsWith("&")) {
                urlBuilder.append("?");
            }
            urlBuilder.append(paramName);
            urlBuilder.append("=");
            urlBuilder.append(paramValue);
            urlBuilder.append("&");
        }
    }

    /**
     * Builds full category URL from trail of category IDs and the given category, according to configuration.
     */
    public StringBuilder makeCategoryUrlCore(Delegator delegator, LocalDispatcher dispatcher, Locale locale, GenericValue productCategory,
                                             String currentCatalogId, String previousCategoryId, List<GenericValue> trailEntities, FullWebappInfo targetWebappInfo, boolean useCache) {
        String productCategoryId = productCategory.getString("productCategoryId");
        if (trailEntities == null) {
            trailEntities = Collections.emptyList();
        } else if (!getConfig().getCategoryUrlTrailFormat().isOn() && trailEntities.size() > 1) {
            GenericValue lastElem = trailEntities.get(trailEntities.size() - 1);
            trailEntities = new ArrayList<>(1);
            trailEntities.add(lastElem);
        }
        CatalogAltUrlSanitizer.SanitizeContext targetSanitizeCtx = getCatalogAltUrlSanitizer().makeSanitizeContext(delegator, dispatcher, locale, useCache)
                .setTargetCategory(productCategory).setLast(true).setNameIndex(trailEntities.size() - 1).setTotalNames(trailEntities.size());
        List<String> trailNames = getCategoryTrailPathSegments(delegator, dispatcher, locale, trailEntities,
                productCategory, getConfig().getCategoryUrlTrailFormat(), SeoConfig.TrailFormat.NAME, CatalogUrlType.CATEGORY, targetSanitizeCtx, useCache);
        trailNames = getCatalogAltUrlSanitizer().adjustCategoryLiveAltUrlTrail(trailNames, locale, targetSanitizeCtx);
        // NOTE: pass null productCategory because already resolved in trailNames
        return makeCategoryUrlPath(delegator, dispatcher, locale, productCategory, trailNames, targetWebappInfo.getContextPath(), targetSanitizeCtx, useCache);
    }

    /**
     * Builds the core category URL path.
     * NOTE: productCategory may be null, in which case assume already resolved as part of trailNames.
     * Assumes trailNames already valid.
     */
    public StringBuilder makeCategoryUrlPath(Delegator delegator, LocalDispatcher dispatcher, Locale locale,
                                             GenericValue productCategory, List<String> trailNames, String contextPath, CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx, boolean useCache) {
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

        appendSlashAndValue(urlBuilder, trailNames);

        if (productCategory != null) {
            if (sanitizeCtx == null) {
                sanitizeCtx = getCatalogAltUrlSanitizer().makeSanitizeContext(delegator, dispatcher, locale, useCache).setTargetCategory(productCategory)
                        .setLast(true).setNameIndex(trailNames.size()).setTotalNames(trailNames.size() + 1);
            }
            String catTrailName = getCategoryPathSegment(delegator, dispatcher, locale, productCategory, null, sanitizeCtx, useCache);
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

        return getCatalogAltUrlSanitizer().adjustCategoryLiveAltUrlPath(urlBuilder, locale, sanitizeCtx);
    }

    public StringBuilder makeCategoryUrlPath(Delegator delegator, LocalDispatcher dispatcher, Locale locale,
                                             GenericValue productCategory, List<String> trailNames, String contextPath, boolean useCache) {
        return makeCategoryUrlPath(delegator, dispatcher, locale, productCategory, trailNames, contextPath, null, useCache);
    }

    public String makeProductUrl(HttpServletRequest request, Locale locale, String previousCategoryId, String productCategoryId, String productId) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        List<String> trail = CategoryWorker.getTrail(request);
        FullWebappInfo targetWebappInfo = FullWebappInfo.fromRequest(request);
        String currentCatalogId = CatalogWorker.getCurrentCatalogId(request);
        return makeProductUrl(delegator, dispatcher, locale, trail, targetWebappInfo, currentCatalogId, previousCategoryId, productCategoryId, productId);
    }

    /**
     * Make product url according to the configurations.
     * <p>
     * SCIPIO: Modified for bugfixes and lookup via cache products map.
     */
    public String makeProductUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> currentTrail, FullWebappInfo targetWebappInfo,
                                 String currentCatalogId, String previousCategoryId, String productCategoryId, String productId) {
        final boolean useCache = true; // NOTE: this is for entity cache lookups, not util caches
        List<String> trail;
        if (!getConfig().isCategoryNameEnabled() && !getConfig().getProductUrlTrailFormat().isOn()) {
            trail = Collections.emptyList(); // no need for trail
        } else {
            // NO LONGER NEED ADJUST (stock ofbiz logic) - in fact it will prevent the valid trail selection after this from working
            //if (UtilValidate.isNotEmpty(productCategoryId)) {
            //    currentTrail = CategoryWorker.adjustTrail(currentTrail, productCategoryId, previousCategoryId);
            //}
            trail = mapProductUrlTrail(delegator, currentTrail, productId, targetWebappInfo.getWebSiteId(), currentCatalogId);
        }
        String key = getProductUrlCacheKey(delegator, targetWebappInfo, "Default", locale, previousCategoryId, productCategoryId,
                productId, currentCatalogId, trail);
        String url = productUrlCache.get(key);
        if (url == null) {
            url = makeProductUrlImpl(delegator, dispatcher, locale, trail, targetWebappInfo, currentCatalogId, previousCategoryId, productCategoryId, productId, useCache);
            productUrlCache.put(key, url);
            if (DEBUG) {
                Debug.logInfo("makeProductUrl: Created product url [" + url + "] for key [" + key + "]", module);
            }
        } else {
            if (DEBUG) {
                Debug.logInfo("makeProductUrl: Got cached product url [" + url + "] for key [" + key + "]", module);
            }
        }
        return url;
    }

    protected static String getProductUrlCacheKey(Delegator delegator, FullWebappInfo targetWebappInfo, String type, Locale locale, String previousCategoryId, String productCategoryId, String productId, String currentCatalogId, List<String> trail) {
        String localeStr;
        if (locale == null) {
            localeStr = UtilProperties.getPropertyValue("scipiosetup", "store.defaultLocaleString");
            if (UtilValidate.isEmpty(localeStr)) {
                localeStr = Locale.getDefault().toString();
            }
        } else {
            localeStr = locale.toString();
        }
        return delegator.getDelegatorName()+"::"+type+"::"+targetWebappInfo.getContextPath()+"::"+targetWebappInfo.getWebSiteId()+"::"+localeStr+"::"+previousCategoryId+"::"+productCategoryId+"::"
                +productId+"::"+currentCatalogId+"::"+String.join("/", trail);
    }

    /**
     * Creates a full trail to the given product using hints from the incoming trail to select best.
     */
    protected List<String> mapProductUrlTrail(Delegator delegator, List<String> hintTrail, String productId, String webSiteId, String currentCatalogId) {
        List<String> trail = null;
        String trailKey = delegator.getDelegatorName() + "::" + webSiteId + "::" + productId + "::" + currentCatalogId;
        TrailCacheEntry trailEntry = productTrailCache.get(trailKey);
        if (trailEntry == null) {
            GenericValue product = null;
            try {
                product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache().queryOne();
            } catch (GenericEntityException e) {
                Debug.logWarning(e, "Seo: Cannot create product's URL for: " + productId, module);
            }
            Set<String> topCategoryIds = null;
            List<List<String>> trails = null;
            if (product != null) {
                topCategoryIds = getCatalogTopCategoriesForProductUrl(delegator, currentCatalogId, webSiteId);
                if (topCategoryIds.isEmpty()) {
                    Debug.logWarning("Seo: mapProductUrlTrail: No top category found for catalog '" + currentCatalogId + "'; can't select best trail", module);
                    topCategoryIds = null;
                } else {
                    try {
                        String primaryCatId = product.getString("primaryProductCategoryId");
                        if (primaryCatId != null) { // prioritize primary product category
                            trails = getCategoryRollupTrails(delegator, primaryCatId, topCategoryIds);
                        } else { // no primary, use rollups
                            List<GenericValue> prodCatMembers = EntityQuery.use(delegator).from("ProductCategoryMember")
                                    .where("productId", productId).orderBy("-fromDate").filterByDate().cache().queryList();
                            if (prodCatMembers.size() > 0) {
                                //trails = null;
                                for (GenericValue prodCatMember : prodCatMembers) {
                                    String productCategoryId = prodCatMember.getString("productCategoryId");
                                    List<List<String>> memberTrails = getCategoryRollupTrails(delegator, productCategoryId, topCategoryIds);
                                    if (trails == null) trails = memberTrails;
                                    else trails.addAll(memberTrails);
                                }
                            }
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Seo: Error generating trail for product '" + productId + "': " + e.getMessage(), module);
                    }
                }
            }
            trailEntry = new TrailCacheEntry(topCategoryIds, trails);
            productTrailCache.put(trailKey, trailEntry);
        }
        if (trailEntry.getTopCategoryIds() != null) {
            trail = findBestTopCatTrailForNewUrl(delegator, trailEntry.getTrails(), hintTrail, trailEntry.getTopCategoryIds()); // fast
        }
        return (trail != null) ? trail : Collections.emptyList();
    }

    /**
     * Make product url according to the configurations.
     * <p>
     * SCIPIO: Modified for bugfixes and lookup via cache products map.
     */
    protected String makeProductUrlImpl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail, FullWebappInfo targetWebappInfo,
                                 String currentCatalogId, String previousCategoryId, String productCategoryId, String productId, boolean useCache) {
        GenericValue product;
        try {
            product = EntityQuery.use(delegator).from("Product").where("productId", productId).cache().queryOne();
        } catch (GenericEntityException e) {
            Debug.logWarning(e, "Seo: Cannot create product's URL for: " + productId, module);
            return null;
        }
        return makeProductUrlCore(delegator, dispatcher, locale, product, currentCatalogId, previousCategoryId,
                getCategoriesFromIdList(delegator, dispatcher, locale, trail, useCache), targetWebappInfo, useCache).toString();
    }

    /**
     * Builds full product URL from trail of category IDs and the given product, according to configuration.
     */
    public StringBuilder makeProductUrlCore(Delegator delegator, LocalDispatcher dispatcher, Locale locale, GenericValue product,
                                            String currentCatalogId, String previousCategoryId, List<GenericValue> trailEntities, FullWebappInfo targetWebappInfo, boolean useCache) {
        CatalogAltUrlSanitizer.SanitizeContext targetSanitizeCtx = getCatalogAltUrlSanitizer().makeSanitizeContext(delegator, dispatcher, locale, useCache)
                .setLast(true).setNameIndex(trailEntities.size()).setTotalNames(trailEntities.size() + 1);
        List<String> trailNames = Collections.emptyList();
        if (UtilValidate.isNotEmpty(trailEntities) && getConfig().isCategoryNameEnabled() && getConfig().getProductUrlTrailFormat().isOn()) {
            trailNames = getCategoryTrailPathSegments(delegator, dispatcher, locale, trailEntities,
                    null, getConfig().getProductUrlTrailFormat(), null, CatalogUrlType.PRODUCT, targetSanitizeCtx, useCache);
        }
        trailNames = getCatalogAltUrlSanitizer().adjustProductLiveAltUrlTrail(trailNames, locale, targetSanitizeCtx);
        return makeProductUrlPath(delegator, dispatcher, locale, product, trailNames, targetWebappInfo.getContextPath(), targetSanitizeCtx, useCache);
    }

    /**
     * Builds the core product URL path.
     * NOTE: product may be null, in which case assume already resolved as part of trailNames.
     * Assumes trailNames already valid.
     */
    public StringBuilder makeProductUrlPath(Delegator delegator, LocalDispatcher dispatcher, Locale locale, GenericValue product, List<String> trailNames, String contextPath, CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx, boolean useCache) {
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
        if (config.isCategoryNameEnabled() && config.getProductUrlTrailFormat().isOn()) {
            appendSlashAndValue(urlBuilder, trailNames);
        }

        if (product != null) {
            // 2017-11-08: NOT SUPPORTED: could only theoretically work if chose different character than hyphen
            //if (!trailNames.isEmpty() && !SeoConfigUtil.isCategoryNameSeparatePathElem()) {
            //    urlBuilder.append(SeoStringUtil.URL_HYPHEN);
            //} else {
            ensureTrailingSlash(urlBuilder);
            //}

            if (sanitizeCtx == null) {
                sanitizeCtx = getCatalogAltUrlSanitizer().makeSanitizeContext(delegator, dispatcher, locale, useCache).setTargetProduct(product);
            }
            urlBuilder.append(getProductPathSegment(delegator, dispatcher, locale, product, sanitizeCtx, useCache));
        }

        // legacy product alt url suffix ("-p")
        if (explicitProductRequest && config.isGenerateProductAltUrlSuffix()) {
            urlBuilder.append(config.getProductAltUrlSuffix());
        }

        // general URL suffix/extension (".html")
        checkAddUrlSuffix(urlBuilder);

        return getCatalogAltUrlSanitizer().adjustProductLiveAltUrlPath(urlBuilder, locale, sanitizeCtx);
    }

    public StringBuilder makeProductUrlPath(Delegator delegator, LocalDispatcher dispatcher, Locale locale, GenericValue product, List<String> trailNames, String contextPath, boolean useCache) {
        return makeProductUrlPath(delegator, dispatcher, locale, product, trailNames, contextPath, null, useCache);
    }

    protected String getProductPathSegment(Delegator delegator, LocalDispatcher dispatcher, Locale locale, GenericValue product, CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx, boolean useCache) {
        String name = getProductPathName(delegator, dispatcher, locale, product, sanitizeCtx, useCache);
        if (UtilValidate.isNotEmpty(name)) {
            if (config.isProductNameAppendId()) {
                return getConfig().getProductUrlTargetPattern().expandString(UtilMisc.toMap("name", name, "id",
                        getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(product.getString("productId"), locale, CatalogUrlType.PRODUCT, sanitizeCtx)));
            } else {
                return name;
            }
        } else { // fallback
            return getCatalogAltUrlSanitizer().convertIdToLiveAltUrl(product.getString("productId"), locale, CatalogUrlType.PRODUCT, sanitizeCtx);
        }
    }

    protected String getProductPathName(Delegator delegator, LocalDispatcher dispatcher, Locale locale, GenericValue product, CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx, boolean useCache) {
        return getProductAltUrl(delegator, dispatcher, locale, product, sanitizeCtx, useCache);
    }

    protected String getProductAltUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, GenericValue product, CatalogAltUrlSanitizer.SanitizeContext sanitizeCtx, boolean useCache) {
        String altUrl = ProductContentWrapper.getProductContentAsText(product, "ALTERNATIVE_URL", locale, dispatcher, useCache, "raw");
        if (UtilValidate.isNotEmpty(altUrl)) {
            // FIXME: effective locale might not be same as "locale" variable!
            return getCatalogAltUrlSanitizer().sanitizeAltUrlFromDb(altUrl, locale, CatalogUrlType.PRODUCT, sanitizeCtx);
        }
        return null;
    }

    protected void checkAddUrlSuffix(StringBuilder sb) {
        String urlSuffix = getUrlSuffix();
        if (UtilValidate.isNotEmpty(urlSuffix) && (sb.length() > 0) && (sb.charAt(sb.length() - 1) != '/')) {
            sb.append(urlSuffix);
        }
    }

    /*
     * *****************************************************
     * URL matching & parsing
     * *****************************************************
     */

    public PathMatch makePathMatchIfValidRequest(Delegator delegator, PathEntity pathEntity, String path, List<String> pathSegments,
                                                 String contextPath, String webSiteId, String currentCatalogId,
                                                 boolean explicitProductRequest, boolean explicitCategoryRequest, Locale explicitLocale, Timestamp moment) {
        if (explicitProductRequest || explicitCategoryRequest || pathEntity != null) {
            return makePathMatch(delegator, pathEntity, path, pathSegments, contextPath, webSiteId, currentCatalogId, explicitProductRequest, explicitCategoryRequest, explicitLocale, moment);
        }
        return null;
    }

    public PathMatch makePathMatch(Delegator delegator, PathEntity pathEntity, String path, List<String> pathSegments,
                                   String contextPath, String webSiteId, String currentCatalogId,
                                   boolean explicitProductRequest, boolean explicitCategoryRequest, Locale explicitLocale, Timestamp moment) {
        return new PathMatch(pathEntity, path, pathSegments, contextPath, webSiteId, currentCatalogId, explicitProductRequest, explicitCategoryRequest, explicitLocale, moment);
    }

    /**
     * Returned whenever we find a URL that appears to be an SEO URL, even if the request is not for a valid product or category.
     *
     * <p>SCIPIO: 3.0.0: Also becomes a facade around {@link PathEntity}.</p>
     */
    public static class PathMatch implements Serializable {

        protected PathEntity pathEntity;
        protected String path;
        protected List<String> pathSegments; // path segments (after mount-point, if any)
        protected Locale explicitLocale;
        protected boolean explicitProductRequest;
        protected boolean explicitCategoryRequest;
        protected Timestamp moment;

        public PathMatch() {
        }

        public PathMatch(PathEntity pathEntity, String path, List<String> pathSegments, String contextPath, String webSiteId,
                         String currentCatalogId, boolean explicitProductRequest, boolean explicitCategoryRequest, Locale explicitLocale, Timestamp moment) {
            this.pathEntity = pathEntity;
            this.path = path;
            this.pathSegments = ensurePathList(pathSegments);
            this.explicitProductRequest = explicitProductRequest;
            this.explicitCategoryRequest = explicitCategoryRequest;
            this.explicitLocale = explicitLocale;
            this.moment = moment;
        }

        public PathMatch(PathMatch other) {
            this.pathEntity = other.pathEntity;
            this.path = other.path;
            this.pathSegments = other.pathSegments;
            this.explicitProductRequest = other.explicitProductRequest;
            this.explicitCategoryRequest = other.explicitCategoryRequest;
            this.explicitLocale = other.explicitLocale;
            this.moment = other.moment;
        }

        /**
         * Returns the main match entity and trail (aliased using other calls below for convenience).
         */
        public PathEntity getPathEntity() {
            return pathEntity;
        }

        /** Returns the original path that was matched. **/
        public String getPath() { return path; }

        /** Returns the original path elements, excluding any explicit /product or /category mount-point. **/
        public List<String> getPathSegments() { return pathSegments; }

        /** Returns the target match, or null if no productId or categoryId could be matched. **/
        public PathSegmentEntity getTargetEntity() {
            PathEntity pathEntity = getPathEntity();
            return (pathEntity != null) ? pathEntity.getTargetEntity() : null;
        }

        public CatalogUrlType getTargetEntityType() {
            PathEntity pathEntity = getPathEntity();
            return (pathEntity != null) ? pathEntity.getTargetEntityType() : null;
        }

        public String getTargetProductId() { return hasTargetProduct() ? getTargetEntity().getId() : null; }
        public String getTargetCategoryId() { return hasTargetCategory() ? getTargetEntity().getId() : null; }

        /**
         * Excludes target category (and product).
         */
        public List<String> getTrailCategoryIds() {
            PathEntity pathEntity = getPathEntity();
            return (pathEntity != null) ? pathEntity.getTrailCategoryIds() : null;
        }

        /**
         * Includes target category (but not product).
         */
        public List<String> getFullTrailCategoryIds() {
            PathEntity pathEntity = getPathEntity();
            return (pathEntity != null) ? pathEntity.getFullTrailCategoryIds() : null;
        }

        public String getParentCategoryId() {
            List<String> trailCategoryIds = getTrailCategoryIds();
            return UtilValidate.isNotEmpty(trailCategoryIds) ? trailCategoryIds.get(trailCategoryIds.size() - 1) : null;
        }

        public boolean hasTargetProduct() {
            return (getTargetEntityType() == CatalogUrlType.PRODUCT);
        }

        public boolean hasTargetCategory() {
            return (getTargetEntityType() == CatalogUrlType.CATEGORY);
        }

        public boolean isExplicitProductRequest() {
            return explicitProductRequest;
        }

        public boolean isExplicitCategoryRequest() {
            return explicitCategoryRequest;
        }

        public boolean isProductRequest() {
            return isExplicitProductRequest() || hasTargetProduct();
        }

        public boolean isCategoryRequest() {
            return isExplicitCategoryRequest() || hasTargetCategory();
        }

        /**
         * The locale that the explicit product/category path prefix matched (most reliable) - may be null for implicit requests.
         *
         * <p>If using implicit requests, locale will be implied by the target entity match.</p>
         */
        public Locale getExplicitLocale() {
            return explicitLocale;
        }

        public String getExplicitLocaleString() {
            Locale explicitLocale = getExplicitLocale();
            return (explicitLocale != null) ? explicitLocale.toString() : null;
        }

        public Timestamp getMoment() {
            return moment;
        }

        public PathMatch setPathEntity(PathEntity pathEntity) {
            this.pathEntity = pathEntity;
            return this;
        }

        public PathMatch setPath(String path) {
            this.path = path;
            return this;
        }

        public PathMatch setPathSegments(List<String> pathSegments) {
            this.pathSegments = pathSegments;
            return this;
        }

        public PathMatch setExplicitProductRequest(boolean explicitProductRequest) {
            this.explicitProductRequest = explicitProductRequest;
            return this;
        }

        public PathMatch setExplicitCategoryRequest(boolean explicitCategoryRequest) {
            this.explicitCategoryRequest = explicitCategoryRequest;
            return this;
        }

        public PathMatch setExplicitLocale(Locale explicitLocale) {
            this.explicitLocale = explicitLocale;
            return this;
        }

        public PathMatch setMoment(Timestamp moment) {
            this.moment = moment;
            return this;
        }
    }

    /**
     * Checks if the path (starting after context path) appears to be an SEO
     * URL and returns its info if so.
     *
     * @param path path starting from context path, in other words servlet path + path info
     */
    public PathMatch matchInboundSeoCatalogUrl(Delegator delegator, String path, String contextPath, String webSiteId, String currentCatalogId) {
        // clean up the path
        String pathInfo = preprocessInboundSeoCatalogUrlPath(path);
        if (pathInfo == null) {
            return null;
        }

        // check/strip the URL suffix
        // 2020-01-24: This used to be done in preprocessInboundSeoCatalogUrlPath, but the trailing slash
        // has to be stripped BEFORE the suffix stripped (it makes no sense for suffix to be separated by a slash)
        // and we need to handle when the suffix is configured but missing
        String urlSuffix = getUrlSuffix();
        boolean urlSuffixFail = false;
        if (UtilValidate.isNotEmpty(urlSuffix)) {
            if (pathInfo.endsWith(urlSuffix)) {
                pathInfo = pathInfo.substring(0, pathInfo.length() - urlSuffix.length());
            } else if (config.isSeoUrlSuffixMatchRequired()) {
                urlSuffixFail = true;
            }
        }

        // split path into alt-url parts
        List<String> pathSegments = StringUtil.split(pathInfo, "/");
        if (UtilValidate.isEmpty(pathSegments)) {
            return null;
        }

        boolean explicitCategoryRequest = false;
        boolean explicitProductRequest = false;
        // the locale the URL appears to be: the path prefix if explicit, or the cat/prod name language if implicit
        Locale explicitLocale = null;
        String lastPathSegment = pathSegments.get(pathSegments.size() - 1);

        SeoConfig config = getConfig();

        // determine the general form of the URL: explicit or implicit (+ locale at same time)
        explicitLocale = getProductServletPathNameLocale(pathSegments.get(0));
        if (explicitLocale != null) {
            explicitProductRequest = true;
            pathSegments.remove(0);
        } else {
            explicitLocale = getCategoryServletPathNameLocale(pathSegments.get(0));
            if (explicitLocale != null) {
                explicitCategoryRequest = true;
                pathSegments.remove(0);
            } else {
                // LEGACY suffix support for backward compat with published links
                if (config.isHandleProductAltUrlSuffix() && lastPathSegment.endsWith(config.getProductAltUrlSuffix())) {
                    explicitProductRequest = true;
                    lastPathSegment = lastPathSegment.substring(0, lastPathSegment.length() - config.getProductAltUrlSuffix().length());
                    pathSegments.set(pathSegments.size() - 1, lastPathSegment);
                } else if (config.isHandleCategoryAltUrlSuffix() && lastPathSegment.endsWith(config.getCategoryAltUrlSuffix())) {
                    explicitCategoryRequest = true;
                    lastPathSegment = lastPathSegment.substring(0, lastPathSegment.length() - config.getProductAltUrlSuffix().length());
                    pathSegments.set(pathSegments.size() - 1, lastPathSegment);
                } else {
                    if (!config.isHandleImplicitRequests()) {
                        return null;
                    }
                }
            }
        }

        if (pathSegments.size() > 0) {
            lastPathSegment = pathSegments.get(pathSegments.size() - 1);
        } else {
            lastPathSegment = null;
        }

        PathEntity pathEntity = null;
        List<String> allPathSegments = new ArrayList<>(pathSegments);
        Timestamp moment = UtilDateTime.nowTimestamp();
        if (UtilValidate.isNotEmpty(lastPathSegment) && !urlSuffixFail) {
            String firstPathSegment = (pathSegments.size() > 0) ? pathSegments.get(0) : null;
            try {
                if (explicitProductRequest) {
                    // EXPLICIT PRODUCT
                    // NOTE: Either the first path segment, last, or both may refer to a product, depending on configuration; simply try to handle all cases
                    //  since we have to isolate the category trail and support all configs
                    // TODO: Support config options to constrain this behavior for optimization purposes
                    PathSegmentEntities firstSegmentProducts = null;
                    PathSegmentEntities lastSegmentProducts = null;
                    if (getConfig().isProductSimpleIdLookup() && UtilValidate.isNotEmpty(firstPathSegment)) {
                        firstSegmentProducts = UtilValidate.nullIfEmpty(matchPathSegmentProductCached(delegator, firstPathSegment, PathSegmentMatchOptions.ID_ONLY, moment));
                    }
                    if ((firstSegmentProducts == null || firstSegmentProducts.isEmpty() || pathSegments.size() >= 2) && UtilValidate.isNotEmpty(lastPathSegment)) {
                        // Always check this even if the first one matched
                        lastSegmentProducts = UtilValidate.nullIfEmpty(matchPathSegmentProductCached(delegator, lastPathSegment, PathSegmentMatchOptions.ANY, moment));
                    }
                    PathSegmentEntities targetProducts = null;
                    if (firstSegmentProducts != null) {
                        targetProducts = firstSegmentProducts;
                        pathSegments.remove(0);
                        if (lastSegmentProducts != null) {
                            pathSegments.remove(pathSegments.size() - 1);
                        }
                    } else if (lastSegmentProducts != null) {
                        targetProducts = lastSegmentProducts;
                        pathSegments.remove(pathSegments.size() - 1);
                    }
                    if (targetProducts != null) {
                        pathEntity = matchBestProductAndTrail(delegator, targetProducts, lastSegmentProducts, pathSegments, currentCatalogId, webSiteId, explicitLocale, moment);
                    }
                } else if (explicitCategoryRequest) {
                    // EXPLICIT CATEGORY
                    PathSegmentEntities firstPathCategories = null;
                    PathSegmentEntities lastPathCategories = null;
                    if (getConfig().isCategorySimpleIdLookup() && UtilValidate.isNotEmpty(firstPathSegment)) {
                        firstPathCategories = UtilValidate.nullIfEmpty(matchPathSegmentCategoryCached(delegator, firstPathSegment, PathSegmentMatchOptions.ID_ONLY, moment));
                    }
                    if ((firstPathCategories == null || firstPathCategories.isEmpty() || pathSegments.size() >= 2) && UtilValidate.isNotEmpty(lastPathSegment)) {
                        lastPathCategories = UtilValidate.nullIfEmpty(matchPathSegmentCategoryCached(delegator, lastPathSegment, PathSegmentMatchOptions.ANY, moment));
                    }
                    PathSegmentEntities targetCategories = null;
                    if (firstPathCategories != null) {
                        targetCategories = firstPathCategories;
                        pathSegments.remove(0);
                        if (lastPathCategories != null) {
                            pathSegments.remove(pathSegments.size() - 1);
                        }
                    } else if (lastPathCategories != null) {
                        targetCategories = lastPathCategories;
                        pathSegments.remove(pathSegments.size() - 1);
                    }
                    if (targetCategories != null) {
                        pathEntity = matchBestCategoryAndTrail(delegator, targetCategories, lastPathCategories, pathSegments, currentCatalogId, webSiteId, explicitLocale, moment);
                    }
                } else {
                    // IMPLICIT REQUEST
                    // WARN: best-effort, ambiguous - it is up to SeoConfig.xml to decide how risky this will be
                    PathSegmentMatchOptions matchOptions = config.isImplicitRequestNameMatchesOnly() ? PathSegmentMatchOptions.REQUIRE_NAME : PathSegmentMatchOptions.ANY;
                    PathSegmentEntities targetProducts = matchPathSegmentProductCached(delegator, lastPathSegment, matchOptions, moment);
                    if (targetProducts.size() > 0) {
                        pathSegments.remove(pathSegments.size() - 1);
                        pathEntity = matchBestProductAndTrail(delegator, targetProducts, targetProducts, pathSegments, currentCatalogId, webSiteId, null, moment);
                    } else {
                        PathSegmentEntities targetCategories = matchPathSegmentCategoryCached(delegator, lastPathSegment, matchOptions, moment);
                        if (targetCategories.size() > 0) {
                            pathSegments.remove(pathSegments.size() - 1);
                            pathEntity = matchBestCategoryAndTrail(delegator, targetCategories, targetCategories, pathSegments, currentCatalogId, webSiteId, null, moment);
                        }
                    }
                }
            } catch (Exception e) {
                Debug.logError(e, "Seo: matchInboundSeoCatalogUrl: Error parsing catalog URL " + path + ": " + e.getMessage(), module);
                return null;
            }
        }

        return makePathMatchIfValidRequest(delegator, pathEntity, path, allPathSegments, contextPath, webSiteId, currentCatalogId, explicitProductRequest, explicitCategoryRequest, explicitLocale, moment);
    }

    /**
     * Refers to the {@link PathEntity#getTrailCategoryIds()} selection, based on attempted matching, referring to inputs and selection among category rollup trails.
     */
    public enum TrailMatchType {
        EXACT(5),
        PARTIAL(4),
        NONE(3),
        OUTSIDE_CATALOG(2),
        INVALID(1);

        private final int priority;

        TrailMatchType(int priority) {
            this.priority = priority;
        }

        private int getPriority() {
            return priority;
        }

        public int comparePriorityTo(TrailMatchType other) {
            return (other != null) ? Integer.compare(this.getPriority(), other.getPriority()) : 1;
        }

        public boolean isValidRequest() {
            return (this == EXACT || this == PARTIAL || this == NONE);
        }

        public boolean isValidNonEmptyRequestTrail() {
            return (this == EXACT || this == PARTIAL);
        }
    }

    /**
     * Local class used to return path segment entity and trailCategoryIds together from the methods.
     */
    public static class PathEntity implements Serializable {

        protected final TrailMatchType trailMatchType;
        protected final PathSegmentEntity targetEntity;
        protected final PathSegmentEntities lastSegmentEntities;
        protected transient PathSegmentEntities lastSegmentTargetEntities;
        protected final List<PathSegmentEntity> trailEntities;
        protected final List<String> trailCategoryIds;
        protected transient List<String> fullTrailCategoryIds;
        protected final List<PathSegmentEntities> requestedTrailEntities;
        protected transient List<PathSegmentEntities> fullRequestedTrailEntities;

        protected PathEntity(TrailMatchType trailMatchType, PathSegmentEntity targetEntity, PathSegmentEntities lastSegmentEntities, List<PathSegmentEntity> trailEntities,
                             List<String> pathCategoryIds, List<PathSegmentEntities> requestedTrailEntities) {
            this.trailMatchType = trailMatchType;
            this.targetEntity = targetEntity;
            this.lastSegmentEntities = lastSegmentEntities;
            this.trailEntities = trailEntities;
            this.trailCategoryIds = pathCategoryIds;
            this.requestedTrailEntities = requestedTrailEntities;
        }

        public TrailMatchType getTrailMatchType() {
            return trailMatchType;
        }

        public PathSegmentEntity getTargetEntity() {
            return targetEntity;
        }

        public String getId() {
            PathSegmentEntity targetEntity = getTargetEntity();
            return (targetEntity != null) ? targetEntity.getId() : null;
        }

        /**
         * Last segment (target) entities; normally contains {@link #getTargetEntity()} by ID but not guaranteed.
         */
        public PathSegmentEntities getLastSegmentEntities() {
            return lastSegmentEntities;
        }

        /**
         * Sometimes, the {@link #getLastSegmentTargetEntities()} entity was not the last path segment; this gets the first matching in the last path segment.
         */
        public PathSegmentEntities getLastSegmentTargetEntities() {
            PathSegmentEntities lastSegmentTargetEntities = this.lastSegmentTargetEntities;
            if (lastSegmentTargetEntities == null) {
                PathSegmentEntity targetEntity = getTargetEntity();
                PathSegmentEntities lastSegmentEntities = getLastSegmentEntities();
                if (targetEntity != null && lastSegmentEntities != null) {
                    lastSegmentTargetEntities = lastSegmentEntities.getAllForId(targetEntity.getId());
                    this.lastSegmentTargetEntities = lastSegmentTargetEntities;
                }
            }
            return lastSegmentTargetEntities;
        }

        public CatalogUrlType getTargetEntityType() {
            return getTargetEntity().getEntityType();
        }

        /**
         * Returns true if either no trail was present in the request (and this is allowed by config) or if all trail elements
         * matched or full or partial category rollup trail for the target category or product.
         */
        public boolean isValidRequest() {
            return getTrailMatchType().isValidRequest();
        }

        /**
         * Returns the category trail entity matches, excluding the target product and target category if this is a category path;
         * same size as {@link #getTrailCategoryIds()} unless no path was specific (and this is allowed) and no default trail was chosen,
         * in which case is empty list.
         *
         * <p>May return null if invalid trail entities; if empty list, is a validated no-trail request, but it's better to check
         * {@link }.</p>
         *
         * <p>NOTE: This only returns a non-empty list if the entities were successfully matched; if you want to check the original
         * request entities whether or not they matched successfully to the trail returned by {@link #getTrailCategoryIds()},
         * see {@link #getRequestedTrailEntities()} instead.</p>
         */
        public List<PathSegmentEntity> getTrailEntities() {
            return trailEntities;
        }

        /**
         * Returns the category trail, excluding the target category if this is a category path.
         *
         * <p>May return null if invalid trail or outside catalog (where permitted by config).</p>
         */
        public List<String> getTrailCategoryIds() {
            return trailCategoryIds;
        }

        /**
         * Returns the category trail, including the target category if this is a category path (excluding if product).
         */
        public List<String> getFullTrailCategoryIds() {
            List<String> fullTrailCategoryIds = this.fullTrailCategoryIds;
            if (fullTrailCategoryIds == null) {
                List<String> trailCategoryIds = getTrailCategoryIds();
                PathSegmentEntity entity = getTargetEntity();
                if (entity != null) {
                    if (entity.getEntityType() == CatalogUrlType.PRODUCT) {
                        fullTrailCategoryIds = trailCategoryIds;
                    } else {
                        if (UtilValidate.isNotEmpty(trailCategoryIds)) {
                            fullTrailCategoryIds = new ArrayList<>(trailCategoryIds.size() + 1);
                            fullTrailCategoryIds.addAll(trailCategoryIds);
                            fullTrailCategoryIds.add(entity.getId());
                        } else {
                            fullTrailCategoryIds = List.of(entity.getId());
                        }
                    }
                    this.fullTrailCategoryIds = fullTrailCategoryIds;
                }
            }
            return fullTrailCategoryIds;
        }

        /**
         * Returns the originally requested trail entities, excluding the entry for the target entity; these may be wrong length (different than {@link #getTrailCategoryIds()}) or not match the chosen trail entities/categories.
         */
        public List<PathSegmentEntities> getRequestedTrailEntities() {
            return requestedTrailEntities;
        }

        /**
         * Returns the originally requested trail entities, including the entries for the target entity or extra entities; these may be wrong length (different than {@link #getFullTrailCategoryIds()}) or not match the chosen trail entities/categories.
         */
        public List<PathSegmentEntities> getFullRequestedTrailEntities() {
            List<PathSegmentEntities> fullRequestedTrailEntities = this.fullRequestedTrailEntities;
            if (fullRequestedTrailEntities == null) {
                if (getTargetEntityType() == CatalogUrlType.PRODUCT) {
                    fullRequestedTrailEntities = requestedTrailEntities;
                } else {
                    PathSegmentEntities lastEntities = (this.lastSegmentEntities != null && this.lastSegmentEntities.size() > 0) ?
                            this.lastSegmentEntities : targetEntity.asEntitiesResult();
                    if (UtilValidate.isNotEmpty(requestedTrailEntities)) {
                        fullRequestedTrailEntities = new ArrayList<>(requestedTrailEntities.size() + 1);
                        fullRequestedTrailEntities.addAll(requestedTrailEntities);
                        fullRequestedTrailEntities.add(lastEntities);
                    } else {
                        fullRequestedTrailEntities = List.of(lastEntities);
                    }
                }
                this.fullRequestedTrailEntities = fullRequestedTrailEntities;
            }
            return fullRequestedTrailEntities;
        }

    }

    protected PathEntity matchBestProductAndTrail(Delegator delegator, PathSegmentEntities products, PathSegmentEntities lastSegmentProducts, List<String> pathSegments, String currentCatalogId, String webSiteId, Locale expectedLocale, Timestamp moment) throws GenericEntityException {
        return matchBestEntityAndTrail(delegator, products, lastSegmentProducts, pathSegments, currentCatalogId, webSiteId, expectedLocale, moment);
    }

    protected PathEntity matchBestCategoryAndTrail(Delegator delegator, PathSegmentEntities categories, PathSegmentEntities lastSegmentCategories, List<String> pathSegments, String currentCatalogId, String webSiteId, Locale expectedLocale, Timestamp moment) throws GenericEntityException {
        return matchBestEntityAndTrail(delegator, categories, lastSegmentCategories, pathSegments, currentCatalogId, webSiteId, expectedLocale, moment);
    }

    /**
     * Uses heuristic category trail matching to determine best entity and trail for a set of target entities against category rollup trails.
     *
     * <p>NOTE: This assumes the targetEntities are all product or all categories.</p>
     *
     * <p>TODO: This should use expectedLocale when non-null to bias targetEntities and trail selection.</p>
     */
    protected PathEntity matchBestEntityAndTrail(Delegator delegator, PathSegmentEntities targetEntities, PathSegmentEntities lastSegmentEntities,
                                                 List<String> pathSegments, String currentCatalogId, String webSiteId, Locale expectedLocale, Timestamp moment) throws GenericEntityException {
        PathSegmentMatchOptions matchOptions = PathSegmentMatchOptions.ANY; // TODO: Configurable
        List<PathSegmentEntities> requestedTrailEntities = matchPathSegmentCategoriesCached(delegator, pathSegments, matchOptions, moment);

        Set<String> topCategoryIds;
        if (targetEntities.getEntityType() == CatalogUrlType.CATEGORY) {
            topCategoryIds = getCatalogTopCategoriesForCategoryUrl(delegator, currentCatalogId, webSiteId);
        } else {
            topCategoryIds = getCatalogTopCategoriesForProductUrl(delegator, currentCatalogId, webSiteId);
        }

        PathSegmentEntity bestEntity = null;
        List<PathSegmentEntity> bestTrailEntities = null;
        List<List<String>> bestTrails = null;
        List<String> bestTrail = null; // NOTE: this contains the target category itself at end
        TrailMatchType bestTrailMatchType = null;

        for (PathSegmentEntity entity : targetEntities) {
            boolean isCategoryEntity = (entity.getEntityType() == CatalogUrlType.CATEGORY);
            List<PathSegmentEntity> trailEntities;
            List<List<String>> trails;
            List<String> trail;
            TrailMatchType trailMatchType;

            if (isCategoryEntity) {
                trails = getCategoryRollupTrails(delegator, entity.getId(), topCategoryIds);
            } else {
                trails = getProductRollupTrails(delegator, entity.getId(), topCategoryIds);
            }

            if (trails.size() > 0) {
                if (pathSegments.size() > 0) {
                    if (isCategoryEntity) {
                        // SPECIAL: for category, we have to re-add ourselves at the end for trail matching purposes (just modify list in-place for now)
                        requestedTrailEntities.add(targetEntities);
                    }
                    try {
                        trail = findBestTrailForPathSegments(delegator, trails, requestedTrailEntities, expectedLocale);
                    } finally {
                        if (isCategoryEntity) {
                            requestedTrailEntities.remove(requestedTrailEntities.size() - 1);
                        }
                    }

                    if (trail != null) {
                        if (isCategoryEntity) {
                            trail.remove(trail.size() - 1);
                        }
                        trailMatchType = (trail.size() == pathSegments.size()) ? TrailMatchType.EXACT : TrailMatchType.PARTIAL;

                        // NOTE: Here, trail returned by findBestTrailForPathSegments is always equal size or longer than pathSegments,
                        //  so the best trail is actually the shortest one, because it's closest to pathSegments (we're not comparing pathSegments)
                        //  - this is only true when comparing EXACT/PARTIAL matches
                        // TODO: REVIEW: Here, exact trail size is prioritized over top category ProdCatalogCategory sequenceNum, which is probably wanted but debatable
                        if (bestEntity == null || bestTrail == null || bestTrailMatchType.comparePriorityTo(trailMatchType) < 0 ||
                                (bestTrailMatchType.comparePriorityTo(trailMatchType) == 0 &&
                                    (trail.size() < bestTrail.size() ||
                                        (trail.size() == bestTrail.size() && isFirstTrailBetterMatchThanSecondTopCatPrecision(entity, trail, bestEntity, bestTrail, topCategoryIds))))) {
                            trailEntities = resolvePathSegmentEntityTrail(trail, requestedTrailEntities);
                            bestEntity = entity;
                            bestTrailEntities = trailEntities;
                            bestTrails = trails;
                            bestTrail = trail;
                            bestTrailMatchType = trailMatchType;
                        }
                    } else { // Match failure - invalid category trail passed - use default
                        if (getConfig().isAllowInvalidCategoryPathElements()) {
                            trailMatchType = TrailMatchType.INVALID;
                            trail = getFirstTopTrail(trails, topCategoryIds);
                            trailEntities = null; // null when invalid
                            // NOTE: Here the precision check is comparing two INVALID matches (meaning two first top trails)
                            //  so we're just picking a consistent default fallback for INVALID
                            if (bestEntity == null || bestTrailMatchType.comparePriorityTo(trailMatchType) < 0 ||
                                    (bestTrailMatchType.comparePriorityTo(trailMatchType) == 0 &&
                                            isFirstTrailBetterMatchThanSecondTopCatPrecision(entity, trail, bestEntity, bestTrail, topCategoryIds))) {
                                bestEntity = entity;
                                bestTrailEntities = trailEntities;
                                bestTrails = trails;
                                bestTrail = trail;
                                bestTrailMatchType = trailMatchType;
                            }
                        }
                    }
                } else { // (trails.size() > 0) && (pathSegments.size() == 0)
                    // No path specified (single product or category ID/name) - fallback to default trail
                    trail = getFirstTopTrail(trails, topCategoryIds);
                    trailEntities = List.of(); // explicit empty (none requested) - not invalid
                    if (trail != null) {
                        trailMatchType = TrailMatchType.NONE;
                    } else { // Shouldn't happen because (trails.size() > 0), but just in case
                        if (getConfig().isAllowTargetOutsideCatalog()) {
                            trailMatchType = TrailMatchType.OUTSIDE_CATALOG;
                        } else {
                            trailMatchType = null; // Don't consider result
                        }
                    }

                    if (trailMatchType != null && (bestEntity == null || bestTrailMatchType.comparePriorityTo(trailMatchType) < 0 ||
                            (bestTrailMatchType.comparePriorityTo(trailMatchType) == 0 &&
                                    isFirstTrailBetterMatchThanSecondTopCatPrecision(entity, trail, bestEntity, bestTrail, topCategoryIds)))) {
                        bestEntity = entity;
                        bestTrailEntities = trailEntities;
                        bestTrails = trails;
                        bestTrail = trail;
                        bestTrailMatchType = trailMatchType;
                    }
                }
            } else { // // (trails.size() == 0)
                // Unexpected: this product/category is not connected to the catalog through rollup/topCategoryIds
                if (getConfig().isAllowTargetOutsideCatalog()) {
                    trail = null;
                    if (pathSegments.isEmpty()) {
                        trailMatchType = TrailMatchType.OUTSIDE_CATALOG;
                        trailEntities = List.of(); // explicit empty (none requested) - not invalid
                    } else {
                        if (getConfig().isAllowInvalidCategoryPathElements()) {
                            trailMatchType = TrailMatchType.INVALID;
                        } else {
                            trailMatchType = null; // Don't consider result
                        }
                        trailEntities = null; // null when invalid
                    }

                    if ((trailMatchType != null) && (bestEntity == null || bestTrailMatchType.comparePriorityTo(trailMatchType) < 0 ||
                            (bestTrailMatchType.comparePriorityTo(trailMatchType) == 0 &&
                                    isFirstTrailBetterMatchThanSecondTopCatPrecision(entity, trail, bestEntity, bestTrail, topCategoryIds)))) {
                        bestEntity = entity;
                        bestTrailEntities = trailEntities;
                        bestTrails = trails;
                        bestTrail = trail;
                        bestTrailMatchType = trailMatchType;
                    }
                }
            }
        }

        return (bestEntity != null) ? new PathEntity(bestTrailMatchType, bestEntity, lastSegmentEntities, bestTrailEntities, bestTrail, requestedTrailEntities) : null;
    }

    /**
     * Does top cat check and precision check, but assumes size was already checked.
     */
    private boolean isFirstTrailBetterMatchThanSecondTopCatPrecision(PathSegmentEntity firstMatch, List<String> firstTrail, PathSegmentEntity secondMatch, List<String> secondTrail, Set<String> topCategoryIds) {
        if (UtilValidate.isNotEmpty(firstTrail)) {
            if (UtilValidate.isNotEmpty(secondTrail)) {
                // 1) prefer the trail that is lower ProdCatalogCategory sequenceNum
                String firstTopCatId = firstTrail.get(0);
                String secondTopCatId = secondTrail.get(0);
                if (!firstTopCatId.equals(secondTopCatId)) {
                    for (String topCatId : topCategoryIds) {
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
            }
        }
        // fallback on precision check
        return firstMatch.isMorePreciseThan(secondMatch);
    }

    /**
     * Checks if matches suffix and has starting slash; removes starting and ending slashes.
     *
     * <p>Returns null if bad. Result splits cleanly on "/".</p>
     */
    public String preprocessInboundSeoCatalogUrlPath(String pathInfo) {
        // path must start with a slash, and remove it
        if (!pathInfo.startsWith("/")) return null;
        pathInfo = pathInfo.substring(1);
        // if path ends with slash (for whatever reason it was added), remove it
        if (pathInfo.endsWith("/")) pathInfo = pathInfo.substring(0, pathInfo.length() - 1);
        return pathInfo;
    }

    /**
     * Uses the passed path elements (resolved) to try to select the best of the possible trails.
     *
     * <p>Returns null only if nothing matches at all. Best-effort.</p>
     *
     * <p>TODO: REVIEW: the possibility of each path elem matching multiple category IDs makes this extremely
     *     complicated; so we ignore the specific implications and just match as much as possible.</p>
     *
     * <p>For a trail to be selected, it must "end with" the pathElems; after that, the best trail is one that
     * has smallest length.</p>
     *
     * <p>TODO: This should use expectedLocale when non-null to bias targetEntities and trail selection.</p>
     */
    protected List<String> findBestTrailForPathSegments(Delegator delegator, List<List<String>> possibleTrails, List<PathSegmentEntities> pathSegments, Locale expectedLocale) throws GenericEntityException {
        if (pathSegments.isEmpty()) {
            return null;
        }
        List<String> bestTrail = null;
        for(List<String> trail : possibleTrails) {
            if (pathSegments.size() > trail.size()) {
                continue; // sure to fail
            }

            ListIterator<PathSegmentEntities> pit = pathSegments.listIterator(pathSegments.size());
            ListIterator<String> tit = trail.listIterator(trail.size());
            boolean matched = true;
            while (matched && pit.hasPrevious()) {
                PathSegmentEntities trailSegmentEntities = pit.previous();
                if (trailSegmentEntities == null) {
                    matched = false;
                } else {
                    String categoryId = tit.previous();
                    // TODO: Configurable: simplistic check: ignores exact vs name-only matches, but may be good enough
                    PathSegmentEntity trailSegmentEntity = trailSegmentEntities.getForId(categoryId);
                    if (trailSegmentEntity == null) {
                        matched = false;
                    }
                }
            }
            if (matched) {
                if (trail.size() == pathSegments.size()) { // ideal case
                    bestTrail = trail;
                    break;
                } else if (bestTrail == null || trail.size() < bestTrail.size()) { // smaller = better
                    bestTrail = trail;
                }
            }
        }
        return bestTrail;
    }

    /**
     * Based on {@link #findBestTrailForPathSegments}, should be called afterward.
     */
    protected List<PathSegmentEntity> resolvePathSegmentEntityTrail(List<String> trail, List<PathSegmentEntities> requestedTrailEntities) throws GenericEntityException {
        if (requestedTrailEntities == null || requestedTrailEntities.isEmpty()) {
            return List.of();
        } else if (trail == null || requestedTrailEntities.size() > trail.size()) {
            throw new IllegalArgumentException("Invalid category trail for path segments");
        }
        List<PathSegmentEntity> entityTrail = new ArrayList<>(requestedTrailEntities.size());
        ListIterator<PathSegmentEntities> pit = requestedTrailEntities.listIterator();
        ListIterator<String> tit = trail.listIterator(trail.size() - requestedTrailEntities.size());
        while (pit.hasNext()) {
            PathSegmentEntities trailSegmentEntities = pit.next();
            String id = tit.next();
            PathSegmentEntity trailSegmentEntity = trailSegmentEntities.getForId(id);
            if (trailSegmentEntity == null) {
                throw new IllegalArgumentException("Invalid category trail for path segments (ID not found in trail path segment entities: " + id + ")");
            }
            entityTrail.add(trailSegmentEntity);
        }
        return entityTrail;
    }

    /*
     * *****************************************************
     * Alternative URL individual path elem part parsing
     * *****************************************************
     */

    public static class PathSegmentMatchOptions implements Serializable {
        public static final PathSegmentMatchOptions ANY = new PathSegmentMatchOptions(false, true, true);
        public static final PathSegmentMatchOptions REQUIRE_NAME = new PathSegmentMatchOptions(false, false, true);
        public static final PathSegmentMatchOptions ID_ONLY = new PathSegmentMatchOptions(true, true, false);

        private final boolean requireId;

        private final boolean allowIdOnly;
        private final boolean allowName;
        private final String cacheKey;

        protected PathSegmentMatchOptions(boolean requireId, boolean allowIdOnly, boolean allowName) {
            this.requireId = requireId;
            this.allowIdOnly = allowIdOnly;
            this.allowName = allowName;
            this.cacheKey = (requireId ? "Y" : "N") + ":" + (allowIdOnly ? "Y" : "N") + ":" + (allowName ? "Y" : "N");
        }

        public boolean isRequireId() {
            return requireId;
        }

        public boolean isAllowIdOnly() {
            return allowIdOnly;
        }

        public boolean isAllowName() {
            return allowName;
        }

        public String getCacheKey() {
            return cacheKey;
        }
    }

    /**
     * All the entities a given path part may map to.
     *
     * <p>NOTE: This is most often size 1 but may map to multiple products/categories and is optimized for caching.</p>
     */
    public static class PathSegmentEntities implements List<PathSegmentEntity>, Serializable {
        private final CatalogUrlType entityType;
        private final String pathSegment;
        private final List<PathSegmentEntity> entities;
        private final Timestamp moment;

        public PathSegmentEntities(CatalogUrlType entityType, String pathSegment, Collection<PathSegmentEntity> entities, Timestamp moment) {
            this.entityType = entityType;
            this.pathSegment = pathSegment;
            this.entities = List.copyOf(entities); // Optimize for caching (never modified after this)
            this.moment = moment;
        }

        public PathSegmentEntities(PathSegmentEntity single, Timestamp moment) {
            this.entityType = single.getEntityType();
            this.pathSegment = single.getPathSegment();
            this.entities = List.of(single);
            this.moment = moment;
        }

        public CatalogUrlType getEntityType() {
            return entityType;
        }

        /**
         * The path we matched against (*should* be the same for all the entities matched for a path segment).
         */
        public String getPathSegment() {
            return pathSegment;
        }

        public List<PathSegmentEntity> getEntities() {
            return entities;
        }

        public Timestamp getMoment() {
            return moment;
        }

        public PathSegmentEntity getForId(String id) {
            for (PathSegmentEntity entity : getEntities()) {
                if (id.equals(entity.getId())) {
                    return entity;
                }
            }
            return null;
        }

        public PathSegmentEntities getAllForId(String id) {
            List<PathSegmentEntity> targetEntities = getEntities().stream().filter(e -> id.equals(e.getId())).collect(Collectors.toList());
            return new PathSegmentEntities(this.entityType, this.pathSegment, targetEntities, this.moment);
        }

        public List<String> getIdList() {
            return getEntities().stream().map(PathSegmentEntity::getId).collect(Collectors.toList());
        }

        public boolean isExpired(Timestamp moment, long expireTimeMs) {
            return moment.before(UtilDateTime.addMillisecondsToTimestamp(this.moment, (int) expireTimeMs));
        }

        public boolean sameEntities(PathSegmentEntities other) {
            if (entities.size() != other.entities.size()) {
                return false;
            }
            for (PathSegmentEntity entity : other.entities) {
                if (this.getForId(entity.getId()) == null) {
                    return false;
                }
            }
            return true;
        }

        /**
         * @deprecated This should not be needed anymore because the options are included in the caches (faster at cost of more memory)
         */
        @Deprecated
        public PathSegmentEntities filterResults(PathSegmentMatchOptions matchOptions, Timestamp moment) {
            List<PathSegmentEntity> newEntities = null;
            for (PathSegmentEntity entity : getEntities()) {
                PathSegmentEntity newEntity = entity.filterResults(matchOptions);
                if (newEntity != null) {
                    if (newEntities == null) {
                        newEntities = new ArrayList<>(entities.size());
                    }
                    newEntities.add(newEntity);
                }
            }
            return (newEntities != null) ? new PathSegmentEntities(getEntityType(), getPathSegment(), newEntities, moment) : null;
        }

        @Override
        public int size() {
            return entities.size();
        }

        @Override
        public boolean isEmpty() {
            return entities.isEmpty();
        }

        @Override
        public boolean contains(Object o) {
            return entities.contains(o);
        }

        @Override
        public Iterator<PathSegmentEntity> iterator() {
            return entities.iterator();
        }

        @Override
        public Object[] toArray() {
            return entities.toArray();
        }

        @Override
        public <T> T[] toArray(T[] a) {
            return entities.toArray(a);
        }

        @Override
        public boolean add(PathSegmentEntity pathSegmentEntity) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean remove(Object o) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean containsAll(Collection<?> c) {
            return entities.containsAll(c);
        }

        @Override
        public boolean addAll(Collection<? extends PathSegmentEntity> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean addAll(int index, Collection<? extends PathSegmentEntity> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean removeAll(Collection<?> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public boolean retainAll(Collection<?> c) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void clear() {
            throw new UnsupportedOperationException();
        }

        @Override
        public PathSegmentEntity get(int index) {
            return entities.get(index);
        }

        @Override
        public PathSegmentEntity set(int index, PathSegmentEntity element) {
            throw new UnsupportedOperationException();
        }

        @Override
        public void add(int index, PathSegmentEntity element) {
            throw new UnsupportedOperationException();
        }

        @Override
        public PathSegmentEntity remove(int index) {
            throw new UnsupportedOperationException();
        }

        @Override
        public int indexOf(Object o) {
            return entities.indexOf(o);
        }

        @Override
        public int lastIndexOf(Object o) {
            return entities.lastIndexOf(o);
        }

        @Override
        public ListIterator<PathSegmentEntity> listIterator() {
            return entities.listIterator();
        }

        @Override
        public ListIterator<PathSegmentEntity> listIterator(int index) {
            return entities.listIterator(index);
        }

        @Override
        public List<PathSegmentEntity> subList(int fromIndex, int toIndex) {
            return entities.subList(fromIndex, toIndex);
        }
    }

    protected <C extends Collection<PathSegmentEntity>> C optimizePathSegmentEntitiesReadOnly(C entities) {
        for (PathSegmentEntity entity : entities) {
            entity.optimizeReadOnly();
        }
        return entities;
    }

    public PathSegmentEntity makePathSegmentEntity(Delegator delegator, CatalogUrlType entityType, String id, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment, PathSegmentMatchType matchType, String name, String localeString, GenericValue record) {
        return new PathSegmentEntity(delegator, entityType, id, pathSegment, matchOptions, moment, matchType, name, localeString, record);
    }

    protected PathSegmentEntity addPathSegmentEntity(Map<String, PathSegmentEntity> results, Delegator delegator, CatalogUrlType entityType, String id, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment, PathSegmentMatchType matchType, String name, String localeString, GenericValue record) {
        PathSegmentEntity match = results.get(id);
        if (match != null) {
            match.addRecordMatch(matchType, name, localeString, record);
        } else {
            match = makePathSegmentEntity(delegator, entityType, id, pathSegment, matchOptions, moment, matchType, name, localeString, record);
            results.put(id, match);
        }
        return match;
    }

    /**
     * A path segment (alt url, id or alt url + id) mapping to an entity along with all its record matches (may be several for multiple locales, etc).
     */
    public static class PathSegmentEntity implements Serializable {
        protected final CatalogUrlType entityType;
        protected final String id;
        protected final String pathSegment;
        protected final PathSegmentMatchOptions matchOptions;
        protected final Timestamp moment; // NOTE: This is only for internal query use - this gets cached so client code may not want to reuse

        protected final String delegatorName;
        protected transient Delegator delegator;

        /**
         * Individual record/name matches, most precise first.
         *
         * <p>NOTE: thread safety requires safe publishing (don't modify after cache store).</p>
         */
        protected List<RecordMatch> recordMatches; // Not needed: volatile

        /**
         * Caches all ALTERNATIVE_URL shortened record values for this ID (not just matched textData or locale).
         */
        protected List<GenericValue> mainContentAltUrlValues;

        /**
         * Caches all ALTERNATIVE_URL ALTERNATE_LOCALE shortened record values for this ID (not just matched textData or locale).
         */
        protected List<GenericValue> altLocaleContentAltUrlValues;

        protected Map<String, GenericValue> localeContentAltUrlValueMap;

        protected PathSegmentEntity(Delegator delegator, CatalogUrlType entityType, String id, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment, PathSegmentMatchType matchType, String name, String localeString, GenericValue record)  {
            this.entityType = entityType;
            this.id = UtilValidate.nullIfEmpty(id);
            this.pathSegment = pathSegment;
            this.matchOptions = matchOptions;
            this.moment = moment;
            this.delegatorName = delegator.getDelegatorName();
            this.delegator = delegator;
            List<RecordMatch> recordMatches = new ArrayList<>(1);
            recordMatches.add(makeRecordMatch(matchType, name, localeString, record));
            this.recordMatches = recordMatches;
        }

        protected PathSegmentEntity(PathSegmentEntity other, PathSegmentMatchOptions matchOptions, boolean preserveRecordMatches) {
            this.entityType = other.entityType;
            this.id = other.id;
            this.pathSegment = other.pathSegment;
            this.matchOptions = (matchOptions != null) ? matchOptions : other.matchOptions;
            this.moment = other.moment;
            this.delegatorName = other.delegatorName;
            this.delegator = other.delegator;
            if (preserveRecordMatches) {
                List<RecordMatch> recordMatches = new ArrayList<>(other.recordMatches.size());
                for (RecordMatch recordMatch : other.getRecordMatches()) {
                    recordMatches.add(makeRecordMatch(recordMatch));
                }
                this.recordMatches = recordMatches;
            } else {
                this.recordMatches = new ArrayList<>(other.recordMatches.size());
            }
        }

        protected PathSegmentEntity makePathSegmentEntity(PathSegmentEntity other, PathSegmentMatchOptions matchOptions, boolean preserveMatches) {
            return new PathSegmentEntity(other, matchOptions, preserveMatches);
        }

        public RecordMatch makeRecordMatch(PathSegmentMatchType matchType, String name, String localeString, GenericValue record) {
            return new RecordMatch(matchType, name, localeString, record);
        }

        public RecordMatch makeRecordMatch(RecordMatch other) {
            return new RecordMatch(other);
        }

        public PathSegmentEntity addRecordMatch(RecordMatch recordMatch) {
            recordMatches.add(recordMatch);
            sortRecordMatches();
            return this;
        }

        public PathSegmentEntity addRecordMatch(PathSegmentMatchType matchType, String name, String localeString, GenericValue record) {
            return addRecordMatch(makeRecordMatch(matchType, name, localeString, record));
        }

        protected PathSegmentEntity addRecordMatchCopyUnsorted(RecordMatch recordMatch) {
            addRecordMatch(makeRecordMatch(recordMatch));
            return this;
        }

        public PathSegmentEntity sortRecordMatches() {
            recordMatches.sort((m1, m2) -> m2.comparePrecisionTo(m1));
            return this;
        }

        protected void optimizeReadOnly() {
            if (recordMatches instanceof ArrayList) {
                recordMatches = List.copyOf(recordMatches);
            }
        }

        protected Delegator getDelegator() {
            Delegator delegator = this.delegator;
            if (delegator == null) {
                delegator = Delegator.fromName(delegatorName);
                this.delegator = delegator;
            }
            return delegator;
        }

        public boolean isValid() {
            return (getId() != null);
        }

        /**
         * The type of target we found for this link: product or catalog.
         */
        public CatalogUrlType getEntityType() {
            return entityType;
        }

        /**
         * The path we matched against (*should* be the same for all entities matched for a path segment).
         */
        public String getPathSegment() {
            return pathSegment;
        }

        /**
         * The extraction options we used to produce this match.
         */
        public PathSegmentMatchOptions getMatchOptions() {
            return matchOptions;
        }

        /**
         * Gets the individual successful matches to this entity/record for this path part.
         *
         * <p>Often this will be size 1, but may be multiple for multiple matching locales with same alt URL, productIds also used as names, etc.</p>
         */
        public List<RecordMatch> getRecordMatches() {
            return recordMatches;
        }

        public boolean hasRecordMatches() {
            return (getRecordMatches().size() > 0);
        }

        /**
         * The ID from DB (NOT from the orig URL).
         */
        public String getId() {
            return id;
        }

        public PathSegmentMatchType getBestMatchType() {
            return getRecordMatches().get(0).getMatchType();
        }

        /**
         * NOTE: for our purposes, we consider name+id more precise than id only.
         */
        public boolean isMorePreciseThan(PathSegmentEntity other) {
            return this.getBestMatchType().isMorePreciseThan(other.getBestMatchType());
        }

        public int comparePrecisionTo(PathSegmentEntity other) {
            return this.getBestMatchType().comparePrecisionTo(other.getBestMatchType());
        }

        public PathSegmentEntities asEntitiesResult() {
            return new PathSegmentEntities(this, moment);
        }

        /**
         * Returns all ALTERNATIVE_URL shortened record values for this ID (not just matched textData or locale).
         */
        public List<GenericValue> getMainContentAltUrlValues() {
            return getMainContentAltUrlValues(null);
        }

        /**
         * Returns all ALTERNATIVE_URL shortened record values for this ID (not just matched textData or locale).
         */
        public List<GenericValue> getMainContentAltUrlValues(Boolean minimalSelect) {
            List<GenericValue> mainContentAltUrlValues = this.mainContentAltUrlValues;
            if (mainContentAltUrlValues == null ||
                    (Boolean.FALSE.equals(minimalSelect) && mainContentAltUrlValues.size() > 0 && !mainContentAltUrlValues.get(0).containsKey("ownerContentId"))) {
                try {
                    if (getEntityType() == CatalogUrlType.PRODUCT) {
                        mainContentAltUrlValues = findProductContentAltUrlValues(getDelegator(),
                                EntityCondition.makeCondition("productId", getId()), moment, minimalSelect != null ? minimalSelect : true);
                    } else {
                        mainContentAltUrlValues = findProductCategoryContentAltUrlValues(getDelegator(),
                                EntityCondition.makeCondition("productCategoryId", getId()), moment, minimalSelect != null ? minimalSelect : true);
                    }
                } catch (GenericEntityException e) {
                    Debug.logError(e, module);
                    this.mainContentAltUrlValues = List.of();
                    return null;
                }
                this.mainContentAltUrlValues = mainContentAltUrlValues;
            }
            return mainContentAltUrlValues;
        }

        /**
         * Returns all ALTERNATIVE_URL ALTERNATE_LOCALE shortened record values for this ID (not just matched textData or locale).
         */
        public List<GenericValue> getAltLocaleContentAltUrlValues() {
            return getAltLocaleContentAltUrlValues(null);
        }

        /**
         * Returns all ALTERNATIVE_URL ALTERNATE_LOCALE shortened record values for this ID (not just matched textData or locale).
         */
        public List<GenericValue> getAltLocaleContentAltUrlValues(Boolean minimalSelect) {
            List<GenericValue> altLocaleContentAltUrlValues = this.altLocaleContentAltUrlValues;
            if (altLocaleContentAltUrlValues == null ||
                    (Boolean.FALSE.equals(minimalSelect) && altLocaleContentAltUrlValues.size() > 0 && !altLocaleContentAltUrlValues.get(0).containsKey("ownerContentId"))) {
                try {
                    if (getEntityType() == CatalogUrlType.PRODUCT) {
                        altLocaleContentAltUrlValues = findProductContentAltUrlLocalizedValues(getDelegator(),
                                EntityCondition.makeCondition("productId", getId()), moment, minimalSelect != null ? minimalSelect : true);
                    } else {
                        altLocaleContentAltUrlValues = findProductCategoryContentAltUrlLocalizedValues(getDelegator(),
                                EntityCondition.makeCondition("productCategoryId", getId()), moment, minimalSelect != null ? minimalSelect : true);
                    }
                } catch (GenericEntityException e) {
                    Debug.logError(e, module);
                    this.altLocaleContentAltUrlValues = List.of();
                    return null;
                }
                this.altLocaleContentAltUrlValues = altLocaleContentAltUrlValues;
            }
            return altLocaleContentAltUrlValues;
        }

        /**
         * Returns an insertion-ordered map of locale strings to content values; empty string is supported and typically the default and first entry.
         */
        public Map<String, GenericValue> getLocaleContentAltUrlValueMap() {
            return getLocaleContentAltUrlValueMap(null);
        }

        /**
         * Returns an insertion-ordered map of locale strings to content values; empty string is supported and typically the default and first entry.
         */
        public Map<String, GenericValue> getLocaleContentAltUrlValueMap(Boolean minimalSelect) {
            Map<String, GenericValue> localeContentAltUrlValueMap = this.localeContentAltUrlValueMap;
            if (localeContentAltUrlValueMap == null ||
                    (Boolean.FALSE.equals(minimalSelect) && localeContentAltUrlValueMap.size() > 0 &&
                            !localeContentAltUrlValueMap.values().iterator().next().containsKey("ownerContentId"))) {
                localeContentAltUrlValueMap = new LinkedHashMap<>();
                for (GenericValue value : getMainContentAltUrlValues(minimalSelect)) {
                    String localeString = value.getString("localeString");
                    if (!localeContentAltUrlValueMap.containsKey(localeString)) {
                        localeContentAltUrlValueMap.put(localeString != null ? localeString : "", value);
                    }
                }
                for (GenericValue value : getAltLocaleContentAltUrlValues(minimalSelect)) {
                    String localeString = value.getString("localeString");
                    if (localeString != null) {
                        if (!localeContentAltUrlValueMap.containsKey(localeString)) {
                            localeContentAltUrlValueMap.put(localeString, value);
                        }
                    } else {
                        Debug.logWarning("Unexpected empty localeString on ALTERNATE_LOCALE content; ignoring: " + value, module);
                    }
                }
                localeContentAltUrlValueMap = Map.copyOf(localeContentAltUrlValueMap);
                this.localeContentAltUrlValueMap = localeContentAltUrlValueMap;
            }
            return localeContentAltUrlValueMap;
        }

        /**
         * @deprecated This should not be needed anymore because the options are included in the caches (faster at cost of more memory)
         */
        @Deprecated
        public PathSegmentEntity filterResults(PathSegmentMatchOptions matchOptions) {
            if (matchOptions.isRequireId()) {
                PathSegmentEntity newResults = null;
                if (matchOptions.isAllowIdOnly()) {
                    for (RecordMatch recordMatch : getRecordMatches()) {
                        if (recordMatch.getMatchType().hasId()) {
                            if (newResults == null) {
                                newResults = makePathSegmentEntity(this, matchOptions, false);
                            }
                            newResults.addRecordMatchCopyUnsorted(recordMatch);
                        }
                    }
                } else {
                    for (RecordMatch recordMatch : getRecordMatches()) {
                        if (recordMatch.getMatchType().isFull()) {
                            if (newResults == null) {
                                newResults = makePathSegmentEntity(this, matchOptions, false);
                            }
                            newResults.addRecordMatchCopyUnsorted(recordMatch);
                        }
                    }
                }
                return newResults;
            } else {
                if (matchOptions.isAllowIdOnly()) {
                    return this;
                } else {
                    PathSegmentEntity newResults = null;
                    for (RecordMatch recordMatch : getRecordMatches()) {
                        if (recordMatch.getMatchType().isIdOnly()) {
                            if (newResults == null) {
                                newResults = makePathSegmentEntity(this, matchOptions, false);
                            }
                            newResults.addRecordMatchCopyUnsorted(recordMatch);
                        }
                    }
                    return newResults;
                }
            }
        }

        public static class RecordMatch implements Serializable {
            protected final PathSegmentMatchType matchType;
            protected final String name;
            protected final String localeString;
            protected transient Locale locale;
            protected final GenericValue record;

            protected RecordMatch(PathSegmentMatchType matchType, String name, String localeString, GenericValue record) {
                this.matchType = matchType;
                this.name = name;
                this.localeString = localeString;
                this.record = record;
            }

            protected RecordMatch(RecordMatch other) {
                this.matchType = other.matchType;
                this.name = other.name;
                this.localeString = other.localeString;
                this.locale = other.locale;
                this.record = other.record;
            }

            public PathSegmentMatchType getMatchType() {
                return matchType;
            }

            /**
             * The name matched from DB (NOT from the orig URL), or null if it was an ID-only match.
             */
            public String getName() { return name; }

            /**
             * The locale string from DB or null if none.
             */
            public String getLocaleString() { return localeString; }

            /**
             * The locale from DB or null if none.
             */
            public Locale getLocale() {
                Locale locale = this.locale;
                if (locale == null && localeString != null) {
                    locale = UtilMisc.parseLocale(localeString);
                    this.locale = locale;
                }
                return locale;
            }

            public GenericValue getRecord() {
                return record;
            }

            public int comparePrecisionTo(RecordMatch other) {
                return this.getMatchType().comparePrecisionTo(other.getMatchType());
            }
        }

    }

    public enum PathSegmentMatchType {
        ID(2),
        NAME(1),
        NAME_ID(3);

        private final int precision;

        PathSegmentMatchType(int precision) {
            this.precision = precision;
        }

        public boolean isFull() {
            return (this == NAME_ID);
        }

        public boolean isIdOnly() {
            return (this == ID);
        }

        public boolean hasId() {
            return (this == ID || this == NAME_ID);
        }

        public boolean isNameOnly() {
            return (this == NAME);
        }

        public boolean hasName() {
            return (this == NAME || this == NAME_ID);
        }

        public boolean isMorePreciseThan(PathSegmentMatchType other) {
            return (comparePrecisionTo(other) > 0);
        }

        public int comparePrecisionTo(PathSegmentMatchType other) {
            if (other == null) {
                return 1;
            } else {
                return Integer.compare(this.precision, other.precision);
            }
        }
    }

    /**
     * SCIPIO: Tries to match an alt URL path element to a product and caches the results IF they match.
     * Heavily modified logic from CatalogUrlFilter.
     */
    public PathSegmentEntities matchPathSegmentProductCached(Delegator delegator, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment) throws GenericEntityException {
        String key = delegator.getDelegatorName() + "::" + pathSegment + "::" + matchOptions.getCacheKey();
        PathSegmentEntities results = productPathSegmentEntitiesCache.get(key);
        if (results == null) {
            results = matchPathSegmentProduct(delegator, pathSegment, matchOptions, moment);
            // NOTE: currently, only storing in cache if has match...
            // this is tradeoff of memory vs misses (risky to allow empty due to incoming from public)
            if (!results.isEmpty()) {
                productPathSegmentEntitiesCache.put(key, results);
            }
        }
        // We now do this within the cache completely, which uses more memory for multi-store setup but is faster
        //return results.filterResults(matchOptions);
        return results;
    }

    /**
     * SCIPIO: Tries to match an alt URL path element to a product.
     * Heavily modified logic from CatalogUrlFilter.
     * <p>
     * Added 2017-11-08.
     */
    public PathSegmentEntities matchPathSegmentProduct(Delegator delegator, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment) throws GenericEntityException {
        Map<String, PathSegmentEntity> results = new LinkedHashMap<>();
        matchPathSegmentProductImpl(delegator, pathSegment, matchOptions, moment, results);
        return new PathSegmentEntities(CatalogUrlType.PRODUCT, pathSegment, optimizePathSegmentEntitiesReadOnly(results.values()), moment);
    }

    /**
     * Extracts product ID from alt URL path element using any means applicable (core implementation/override), into the passed results map.
     * Returns non-null (only) if an exact match was found, which is also added to the passed results map.
     */
    protected void matchPathSegmentProductImpl(Delegator delegator, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment, Map<String, PathSegmentEntity> results) throws GenericEntityException {
        if (matchOptions.isAllowName()) {
            matchPathSegmentProductByAltUrl(delegator, pathSegment, matchOptions, moment, results);
        }
        if (matchOptions.isAllowIdOnly()) {
            matchPathSegmentProductById(delegator, pathSegment, matchOptions, moment, results);
        }
    }

    /**
     * Extracts product ID from alt URL path element, treating it as an Alternative URL (legacy definition), into the passed results map.
     * Returns non-null only if an exact match was found, which is also added to the passed results map.
     */
    protected void matchPathSegmentProductByAltUrl(Delegator delegator, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment, Map<String, PathSegmentEntity> results) throws GenericEntityException {
        // SCIPIO: this is a new filter that narrows down results from DB, which otherwise may be huge.
        EntityCondition matchTextIdCond = makeAltUrlTextIdMatchCombinations(pathSegment, "productId", "textData", matchOptions.isRequireId());

        // Search for non-localized alt urls
        List<GenericValue> productContentInfos = findProductContentAltUrlValues(delegator, matchTextIdCond, moment, true);
        matchPathSegmentAltUrl(delegator, pathSegment, productContentInfos, "productId", CatalogUrlType.PRODUCT, matchOptions, moment, results);

        // Search for localized alt urls
        List<GenericValue> productContentAssocInfos = findProductContentAltUrlLocalizedValues(delegator, matchTextIdCond, moment, true);
        matchPathSegmentAltUrl(delegator, pathSegment, productContentAssocInfos, "productId", CatalogUrlType.PRODUCT, matchOptions, moment, results);
    }

    public static List<GenericValue> findProductContentAltUrlValues(Delegator delegator, EntityCondition filterCondition, Timestamp moment, boolean minimalSelect) throws GenericEntityException {
        List<GenericValue> values = delegator.query().from("ProductContentAndElecTextShort")
                .where(productContentTypeIdAltUrlCond, filterCondition)
                .select(minimalSelect ? productContentAndElecTextShortMinimalSelectFields : null)
                .filterByDate(moment) // NOTE: When cache==true, this is applied by EntityQuery in-memory after the DB query (important here)
                .orderBy("-fromDate").cache(true).queryList();
        return filterProductContentAltUrlValuesByDate(delegator, values, moment);
    }

    public static List<GenericValue> filterProductContentAltUrlValuesByDate(Delegator delegator, List<GenericValue> values, Timestamp moment) throws GenericEntityException {
        if (moment == null) {
            return values;
        }
        return EntityUtil.filterByDate(values, moment, "fromDate", "thruDate", true);
    }

    public static List<GenericValue> findProductContentAltUrlLocalizedValues(Delegator delegator, EntityCondition filterCondition, Timestamp moment, boolean minimalSelect) throws GenericEntityException {
        List<GenericValue> values = EntityQuery.use(delegator).from("ProductContentAssocAndElecTextShort")
                .where(productContentTypeIdAltUrlCond, contentAssocTypeIdAltLocaleCond, filterCondition)
                .select(minimalSelect ? productContentAssocAndElecTextShortMinimalSelectFields : null)
                .orderBy("-fromDate", "-caFromDate").cache(true).queryList();
        return filterProductContentAltUrlLocalizedValuesByDate(delegator, values, moment);
    }

    public static List<GenericValue> filterProductContentAltUrlLocalizedValuesByDate(Delegator delegator, List<GenericValue> values, Timestamp moment) throws GenericEntityException {
        if (moment == null) {
            return values;
        }
        return EntityUtil.filterByCondition(values,
                EntityCondition.makeCondition(
                        EntityUtil.getFilterByDateExpr(moment, "fromDate", "thruDate"),
                        EntityUtil.getFilterByDateExpr(moment, "caFromDate", "caThruDate")
                ));
    }

    /**
     * Extracts product ID from alt URL path element, treating it as product ID only, into the passed results map.
     * Returns non-null only if an exact match was found, which is also added to the passed results map.
     * NOTE: This will skip returning a match if the results map already contains an exact match, but will replace a previous non-exact match.
     */
    protected void matchPathSegmentProductById(Delegator delegator, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment, Map<String, PathSegmentEntity> results) throws GenericEntityException {
        // TODO: REVIEW: Do not use entity cache here: we already have UtilCaches and too many cache misses is not good
        boolean useCache = false;
        GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", pathSegment).cache(useCache).queryOne();
        if (product != null) {
            String productId = product.getString("productId");
            // this case has higher prio over non-exact match, but lower prio than alt url exact match
            PathSegmentEntity prevMatch = results.get(productId);
            if (prevMatch == null || !prevMatch.getBestMatchType().hasId()) {
                addPathSegmentEntity(results, delegator, CatalogUrlType.PRODUCT, productId, pathSegment, matchOptions, moment, PathSegmentMatchType.ID, null, null, product);
            }
        }
    }

    /**
     * SCIPIO: Tries to match an alt URL path element to a category and caches the results IF they match.
     * Heavily modified logic from CatalogUrlFilter.
     */
    public PathSegmentEntities matchPathSegmentCategoryCached(Delegator delegator, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment) throws GenericEntityException {
        String key = delegator.getDelegatorName() + "::" + pathSegment + "::" + matchOptions.getCacheKey();
        PathSegmentEntities results = categoryPathSegmentEntitiesCache.get(key);
        if (results == null) {
            results = matchPathSegmentCategory(delegator, pathSegment, matchOptions, moment);
            // NOTE: currently, only storing in cache if has match...
            // this is tradeoff of memory vs misses (risky to allow empty due to incoming from public)
            if (!results.isEmpty()) {
                categoryPathSegmentEntitiesCache.put(key, results);
            }
        }
        // We now do this within the cache completely, which uses more memory for multi-store setup but is faster
        //return results.filterResults(matchOptions);
        return results;
    }

    /**
     * SCIPIO: Tries to match an alt URL path element to a category and caches the results IF they match.
     * Heavily modified logic from CatalogUrlFilter.
     */
    public List<PathSegmentEntities> matchPathSegmentCategoriesCached(Delegator delegator, Collection<String> pathSegments, PathSegmentMatchOptions matchOptions, Timestamp moment) throws GenericEntityException {
        List<PathSegmentEntities> result = new ArrayList<>();
        for(String pathSegment : pathSegments) {
            result.add(matchPathSegmentCategoryCached(delegator, pathSegment, matchOptions, moment));
        }
        return result;
    }

    /**
     * SCIPIO: Tries to match an alt URL path element to a category.
     * Heavily modified logic from CatalogUrlFilter.
     * <p>
     * Added 2017-11-07.
     */
    public PathSegmentEntities matchPathSegmentCategory(Delegator delegator, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment) throws GenericEntityException {
        Map<String, PathSegmentEntity> results = new LinkedHashMap<>();
        matchPathSegmentCategoryImpl(delegator, pathSegment, matchOptions, moment, results);
        return new PathSegmentEntities(CatalogUrlType.CATEGORY, pathSegment, optimizePathSegmentEntitiesReadOnly(results.values()), moment);
    }

    /**
     * Extracts category ID from alt URL path element using any means applicable (core implementation/override), into the passed results map.
     * Returns non-null only if an exact match was found, which is also added to the passed results map.
     */
    protected void matchPathSegmentCategoryImpl(Delegator delegator, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment, Map<String, PathSegmentEntity> results) throws GenericEntityException {
        if (matchOptions.isAllowName()) {
            matchPathSegmentCategoryByAltUrl(delegator, pathSegment, matchOptions, moment, results);
        }
        if (matchOptions.isAllowIdOnly()) {
            matchPathSegmentCategoryById(delegator, pathSegment, matchOptions, moment, results);
        }
    }

    /**
     * Extracts category ID from alt URL path element, treating it as an Alternative URL (legacy definition), into the passed results map.
     * Returns non-null only if an exact match was found, which is also added to the passed results map.
     */
    protected void matchPathSegmentCategoryByAltUrl(Delegator delegator, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment, Map<String, PathSegmentEntity> results) throws GenericEntityException {
        // SCIPIO: this is a new filter that narrows down results from DB, which otherwise may be huge.
        EntityCondition matchTextIdCond = makeAltUrlTextIdMatchCombinations(pathSegment, "productCategoryId", "textData", matchOptions.isRequireId());

        // Search for non-localized alt urls
        List<GenericValue> productCategoryContentInfos = findProductCategoryContentAltUrlValues(delegator, matchTextIdCond, moment, true);
        matchPathSegmentAltUrl(delegator, pathSegment, productCategoryContentInfos, "productCategoryId", CatalogUrlType.CATEGORY, matchOptions, moment, results);

        // Search for localized alt urls
        List<GenericValue> productCategoryContentAssocInfos = findProductCategoryContentAltUrlLocalizedValues(delegator, matchTextIdCond, moment, true);
        matchPathSegmentAltUrl(delegator, pathSegment, productCategoryContentAssocInfos, "productCategoryId", CatalogUrlType.CATEGORY, matchOptions, moment, results);
    }

    public static List<GenericValue> findProductCategoryContentAltUrlValues(Delegator delegator, EntityCondition filterCondition, Timestamp moment, boolean minimalSelect) throws GenericEntityException {
        List<GenericValue> values = delegator.query().from("ProductCategoryContentAndElecTextShort")
                .where(prodCatContentTypeIdAltUrlCond, filterCondition)
                .select(minimalSelect ? productCategoryContentAndElecTextShortMinimalSelectFields : null)
                .orderBy("-fromDate").cache(true).queryList();
        return filterProductCategoryContentAltUrlValuesByDate(delegator, values, moment);
    }

    public static List<GenericValue> filterProductCategoryContentAltUrlValuesByDate(Delegator delegator, List<GenericValue> values, Timestamp moment) throws GenericEntityException {
        if (moment == null) {
            return values;
        }
        return EntityUtil.filterByDate(values, moment, "fromDate", "thruDate", true);
    }

    public static List<GenericValue> findProductCategoryContentAltUrlLocalizedValues(Delegator delegator, EntityCondition filterCondition, Timestamp moment, boolean minimalSelect) throws GenericEntityException {
        List<GenericValue> values = delegator.query().from("ProductCategoryContentAssocAndElecTextShort")
                .where(prodCatContentTypeIdAltUrlCond, contentAssocTypeIdAltLocaleCond, filterCondition)
                .select(minimalSelect ? productCategoryContentAssocAndElecTextShortMinimalSelectFields : null)
                .orderBy("-fromDate", "-caFromDate").cache(true).queryList();
        return filterProductCategoryContentAltUrlLocalizedValuesByDate(delegator, values, moment);
    }

    public static List<GenericValue> filterProductCategoryContentAltUrlLocalizedValuesByDate(Delegator delegator, List<GenericValue> values, Timestamp moment) throws GenericEntityException {
        if (moment == null) {
            return values;
        }
        return EntityUtil.filterByCondition(values,
                EntityCondition.makeCondition(
                        EntityUtil.getFilterByDateExpr(moment, "fromDate", "thruDate"),
                        EntityUtil.getFilterByDateExpr(moment, "caFromDate", "caThruDate")
                ));
    }

    /**
     * Extracts category ID from alt URL path element, treating it as category ID only, into the passed results map.
     * Returns non-null only if an exact match was found, which is also added to the passed results map.
     * NOTE: This will skip returning a match if the results map already contains an exact match, but will replace a previous non-exact match.
     */
    protected void matchPathSegmentCategoryById(Delegator delegator, String pathSegment, PathSegmentMatchOptions matchOptions, Timestamp moment, Map<String, PathSegmentEntity> results) throws GenericEntityException {
        // TODO: REVIEW: Do not use entity cache here: we already have UtilCaches and too many cache misses is not good
        boolean useCache = false;
        GenericValue productCategory = delegator.query().from("ProductCategory").where("productCategoryId", pathSegment).cache(useCache).queryOne();
        if (productCategory != null) {
            String productCategoryId = productCategory.getString("productCategoryId");
            // this case has higher prio over non-exact match, but lower prio than alt url exact match
            PathSegmentEntity prevMatch = results.get(productCategoryId);
            if (prevMatch == null || !prevMatch.getBestMatchType().hasId()) {
                addPathSegmentEntity(results, delegator, CatalogUrlType.CATEGORY, productCategoryId, pathSegment, matchOptions, moment, PathSegmentMatchType.ID, null, null, productCategory);
            }
        }
    }

    /**
     * This splits pathSegment by hyphen "-" and creates OR condition for all the possible combinations
     * of name and ID.
     */
    public static EntityCondition makeAltUrlTextIdMatchCombinations(String pathSegment, String idField, String textField, boolean exactOnly) {
        List<EntityCondition> condList = new ArrayList<>();
        int lastIndex = pathSegment.lastIndexOf('-');
        while (lastIndex > 0) {
            String name = pathSegment.substring(0, lastIndex);
            String id = pathSegment.substring(lastIndex + 1);
            if (!id.isEmpty()) { // 2019-08-09: if ID is empty, we have a URL that ends with a dash ('-'); covered for backward-compat below
                condList.add(EntityCondition.makeCondition(
                        EntityCondition.makeCondition(textField, EntityOperator.LIKE, name),
                        EntityOperator.AND,
                        EntityCondition.makeCondition(idField, id)));
            }
            lastIndex = pathSegment.lastIndexOf('-', lastIndex - 1);
        }
        if (!exactOnly) {
            // try one without any ID
            condList.add(EntityCondition.makeCondition(textField, EntityOperator.LIKE, pathSegment));
        }
        return EntityCondition.makeCondition(condList, EntityOperator.OR);
    }

    /**
     * Finds ALTERNATIVE_URL matches and adds to results map. If an exact match is found, returns immediately
     * with the result. If no exact, returns null.
     * Based on CatalogUrlFilter iteration.
     * <p>
     * NOTE: 2017: the singleExactOnly flag was initially going to be important for performance reasons,
     * but it can now be left to false (which is 0.01% safer) as long as the caller uses
     * {@link #makeAltUrlTextIdMatchCombinations} in the values query.
     */
    private void matchPathSegmentAltUrl(Delegator delegator, String pathSegment, List<GenericValue> values, String idField,
                                        CatalogUrlType entityType, PathSegmentMatchOptions matchOptions, Timestamp moment, Map<String, PathSegmentEntity> results) {
        for (GenericValue value : values) {
            String textData = value.getString("textData");
            // TODO: REVIEW: Do nothing here because if we don't have to sanitize the DB records, it could
            //  allow some types of LIKE DB queries and allow for optimizations, so try to do without sanitize
            //getCatalogAltUrlSanitizer().sanitizeAltUrlFromDb(textData, null, entityType);
            //textData = UrlServletHelper.invalidCharacter(textData);

            if (pathSegment.startsWith(textData)) {
                String pathSegmentIdStr = pathSegment.substring(textData.length());
                String valueId = value.getString(idField);
                if (pathSegmentIdStr.isEmpty()) {
                    if (!matchOptions.isRequireId()) {
                        addPathSegmentEntity(results, delegator, entityType, valueId, pathSegment, matchOptions, moment, PathSegmentMatchType.NAME, textData, value.getString("localeString"), value);
                    }
                } else {
                    if (pathSegmentIdStr.startsWith("-")) { // should always be a hyphen here
                        pathSegmentIdStr = pathSegmentIdStr.substring(1);
                        if (pathSegmentIdStr.equalsIgnoreCase(valueId)) { // TODO: Case-ignore behavior here should be a configurable option
                            addPathSegmentEntity(results, delegator, entityType, valueId, pathSegment, matchOptions, moment, PathSegmentMatchType.NAME_ID, textData, value.getString("localeString"), value);
                        }
                    }
                }
            }
        }
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
//        String colonString;
        if (nextChar == '?') {
            // FIXME: can't handle colon parameters
//            colonString = null;
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
//        if (colonString != null) params.put("colonString", colonString);
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
    protected List<List<String>> getProductRollupTrails(Delegator delegator, String productId, Set<String> topCategoryIds, boolean useCache) {
        return ProductWorker.getProductRollupTrails(delegator, productId, topCategoryIds, useCache);
    }

    /**
     * Return all paths from the given topCategoryIds to the product.
     * <p>
     * TODO?: perhaps can cache with UtilCache in future, or read from a cached category tree.
     */
    protected List<List<String>> getProductRollupTrails(Delegator delegator, String productId, Set<String> topCategoryIds) {
        return getProductRollupTrails(delegator, productId, topCategoryIds, true);
    }

    /**
     * Return all paths from the given topCategoryIds to the category.
     * <p>
     * TODO?: perhaps can cache with UtilCache in future, or read from a cached category tree.
     */
    protected List<List<String>> getCategoryRollupTrails(Delegator delegator, String productCategoryId, Set<String> topCategoryIds, boolean useCache) {
        return CategoryWorker.getCategoryRollupTrails(delegator, productCategoryId, topCategoryIds, useCache);
    }

    /**
     * Return all paths from the given topCategoryIds to the category.
     * <p>
     * TODO?: perhaps can cache with UtilCache in future, or read from a cached category tree.
     */
    protected List<List<String>> getCategoryRollupTrails(Delegator delegator, String productCategoryId, Set<String> topCategoryIds) {
        return getCategoryRollupTrails(delegator, productCategoryId, topCategoryIds, true);
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

    protected static List<String> newPathList(Collection<String> fromList) {
        return new ArrayList<>(fromList);
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

    private static <T> void removeLastIfSameRef(List<T> list, T value) {
        if (list != null && list.size() > 0 && value != null && value == list.get(list.size() - 1)) {
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
            Map<String, Locale> valueLocaleMap = makeValueLocaleMap(localeValueMap);
            String defaultValue = (defaultLocale != null) ? getNameForLocale(localeValueMap, defaultLocale) : null;
            if (UtilValidate.isNotEmpty(defaultValue)) {
                // 2019-08-09: re-insert the default value for default locale because otherwise it may register to another language
                valueLocaleMap.put(defaultValue, defaultLocale);
            }
            this.valueLocaleMap = valueLocaleMap;
            this.defaultValue = defaultValue;
        }

        /**
         * NOTE: the resulting mapping may not be 1-for-1 - depends on the values.
         */
        private static Map<String, Locale> makeValueLocaleMap(Map<String, String> localeValueMap) {
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
            return getNameForLocale(localeValueMap, locale);
        }

        private static String getNameForLocale(Map<String, String> localeValueMap, Locale locale) {
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

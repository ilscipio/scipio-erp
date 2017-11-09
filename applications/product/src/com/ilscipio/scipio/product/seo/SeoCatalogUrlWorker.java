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
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.ResourceBundle;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.UrlServletHelper;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityExpr;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.category.CatalogUrlFilter.CatalogAltUrlBuilder;
import org.ofbiz.product.category.CatalogUrlServlet;
import org.ofbiz.product.category.CatalogUrlServlet.CatalogUrlBuilder;
import org.ofbiz.product.category.CategoryContentWrapper;
import org.ofbiz.product.category.CategoryWorker;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.service.LocalDispatcher;

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
     */
    protected static class LocalizedName {
        private final Map<Locale, String> localeValueMap;
        private final Map<String, Locale> valueLocaleMap;
        
        public LocalizedName(Map<Locale, String> localeValueMap) {
            this.localeValueMap = new HashMap<>(localeValueMap);
            Map<String, Locale> valueLocaleMap = new HashMap<>();
            for(Map.Entry<Locale, String> entry : localeValueMap.entrySet()) {
                // SPECIAL: duplicates *could* be possible - keep the shortest locale name (most generic)
                // for the string-to-locale mapping
                Locale prevLocale = valueLocaleMap.get(entry.getValue());
                if (prevLocale != null) {
                    if (entry.getKey().toString().length() < prevLocale.toString().length()) {
                        valueLocaleMap.put(entry.getValue(), entry.getKey());
                    }
                } else {
                    valueLocaleMap.put(entry.getValue(), entry.getKey());
                }
            }
            this.valueLocaleMap = valueLocaleMap;
        }
        
        public static LocalizedName getNormalizedFromProperties(String resource, String name) {
            // FIXME: NOT GUARANTEED PERFECT MAPPING!!
            Map<Locale, String> localeValueMap = new HashMap<>();
            for(Locale locale: UtilMisc.availableLocales()) {
                locale = normalizeLocale(locale); // TODO: REVIEW: is this good or bad??
                
                ResourceBundle bundle = UtilProperties.getResourceBundle(resource, locale);
                if (bundle == null || !bundle.containsKey(name)) continue;
                String value = bundle.getString(name);
                if (value == null) continue;
                value = value.trim();
                if (value.isEmpty()) continue;
                localeValueMap.put(locale, value);
            }
            return new LocalizedName(localeValueMap);
        }
        
        public Locale getLocaleForName(String name) {
            return valueLocaleMap.get(name);
        }
        
        public String getNameForLocale(Locale locale) {
            return localeValueMap.get(locale);
        }
        
        public String getNameForNormalizedLocale(Locale locale) {
            return localeValueMap.get(normalizeLocale(locale));
        }
        
        public static Locale normalizeLocale(Locale locale) {
            return new Locale(locale.getCountry());
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
        return getProductPathName().getNameForNormalizedLocale(locale);
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
        return getCategoryPathName().getNameForNormalizedLocale(locale);
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
        public String makeCatalogUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, String contextPath, List<String> crumb, String productId,
                String currentCategoryId, String previousCategoryId) throws IOException {
            if (UtilValidate.isNotEmpty(productId)) {
                return makeProductUrl(delegator, dispatcher, locale, crumb, contextPath, previousCategoryId, currentCategoryId, productId);
            } else {
                return makeCategoryUrl(delegator, dispatcher, locale, crumb, contextPath, previousCategoryId, currentCategoryId, productId, null, null, null, null);
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
                String contextPath, String previousCategoryId, String productCategoryId, String productId) throws IOException {
            return makeProductUrl(delegator, dispatcher, locale, trail, contextPath, previousCategoryId, productCategoryId, productId);
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
                String contextPath, String previousCategoryId, String productCategoryId, String productId,
                String viewSize, String viewIndex, String viewSort, String searchString) throws IOException {
            return makeCategoryUrl(delegator, dispatcher, locale, trail, contextPath, previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString);
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
            
            if (altUrl == null) return "";
            
            // TODO: REVIEW: for now leaving this here for stock compat because
            // users can manually edit the DB via interface and insert bad characters
            altUrl = UrlServletHelper.invalidCharacter(altUrl); // (stock ofbiz)
            
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
     * Either complete the trail or trash it to make it acceptable for SEO purposes.
     */
    private List<String> postProcessCategoryUrlTrail(Delegator delegator, List<String> trail) {
        
        // TODO: complex
        
        return trail;
    }
    
    /**
     * Either complete the trail or trash it to make it acceptable for SEO purposes.
     */
    private List<String> postProcessProductUrlTrail(Delegator delegator, List<String> trail, String productId) {
        
        // TODO: complex
        
        return trail;
    }


    private String makeCategoryUrl(HttpServletRequest request, Locale locale, String previousCategoryId, String productCategoryId, String productId, String viewSize, String viewIndex, String viewSort, String searchString) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        List<String> trail = CategoryWorker.getTrail(request);
        return makeCategoryUrl(delegator, dispatcher, locale, trail, request.getContextPath(), previousCategoryId, productCategoryId, productId, viewSize, viewIndex, viewSort, searchString);
    }

    /**
     * Make category url according to the configurations.
     */
    private String makeCategoryUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail, String contextPath, String previousCategoryId, String productCategoryId, String productId, String viewSize, String viewIndex, String viewSort, String searchString) {
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
        trail = postProcessCategoryUrlTrail(delegator, trail);
        List<String> trailNames = getCategoryUrlTrailNames(delegator, dispatcher, locale, trail);
        // NOTE: this loop includes the productCategoryId itself
        for(String trailName : trailNames) {
            if (urlBuilder.length() == 0 || urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
                urlBuilder.append("/");
            }
            urlBuilder.append(trailName);
        }
        
        // FIXME: currently can't omit this for implicit yet
        if (!explicitCategoryRequest) {
            urlBuilder.append("-c");
        }
        
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
        
        return makeProductUrl(delegator, dispatcher, locale, trail, request.getContextPath(), previousCategoryId, productCategoryId, productId);
    }

    /**
     * Make product url according to the configurations.
     * <p>
     * SCIPIO: Modified for bugfixes and lookup via cache products map (TODO: REVIEW)
     */
    private String makeProductUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, List<String> trail, String contextPath, String previousCategoryId, String productCategoryId, String productId) {
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
            trail = postProcessProductUrlTrail(delegator, trail, productId);
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
        
        // FIXME: currently can't omit this for implicit yet
        if (!explicitProductRequest) {
            urlBuilder.append("-p");
        }
        
        if (!urlBuilder.toString().endsWith("/") && UtilValidate.isNotEmpty(getUrlSuffix())) {
            urlBuilder.append(getUrlSuffix());
        }
        
        return urlBuilder.toString();
    }

    /**
     * Make product url according to the configurations.
     * @deprecated SCIPIO: 2017: replaced by above; for reference only
     *
     * @return String a catalog url
     */
    @Deprecated
    private String makeProductUrlOld(String contextPath, Locale locale, List<String> trail, String productId, String productName, String currentCategoryId, String previousCategoryId) {
        StringBuilder urlBuilder = new StringBuilder();
        urlBuilder.append(contextPath);
        if (urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
            urlBuilder.append("/");
        }
        if (!SeoConfigUtil.isCategoryUrlEnabledForContextPath(contextPath)) {
            // SCIPIO: 2017: THIS IS PROBABLY UNWANTED FOR SEO NOW? TODO: REVIEW
            //urlBuilder.append(CatalogUrlServlet.CATALOG_URL_MOUNT_POINT);
            urlBuilder.append(getProductServletPathName(locale));
        } else {
            // Probably should always require for now
            //if (!(SeoConfigUtil.isHandleImplicitRequests() && SeoConfigUtil.isGenerateImplicitProductUrl())) {
            urlBuilder.append(getProductServletPathName(locale) + "/");
            //}
        }

        if (UtilValidate.isNotEmpty(currentCategoryId)) {
            trail = CategoryWorker.adjustTrail(trail, currentCategoryId, previousCategoryId);
            if (!SeoConfigUtil.isCategoryUrlEnabledForContextPath(contextPath)) {
                for (String trailCategoryId: trail) {
                    if ("TOP".equals(trailCategoryId)) continue;
                    urlBuilder.append("/");
                    urlBuilder.append(trailCategoryId);
                }
            } else {
                if (trail.size() > 1) {
                    String lastCategoryId = trail.get(trail.size() - 1);
                    if (!"TOP".equals(lastCategoryId)) {
                        if (SeoConfigUtil.isCategoryNameEnabled()) {
                            //String categoryName = getCategoryIdNameMap().get(lastCategoryId);
                            String categoryName = null; //
                            if (UtilValidate.isNotEmpty(categoryName)) {
                                urlBuilder.append(SeoConfigUtil.limitCategoryNameLength(categoryName));
                                if (SeoConfigUtil.isCategoryNameSeparatePathElem()) {

                                    if (SeoConfigUtil.isCategoryNameAppendId()) {
                                        // SCIPIO: Also append category ID for now...
                                        urlBuilder.append(SeoStringUtil.URL_HYPHEN);
                                        urlBuilder.append(lastCategoryId);
                                    }

                                    urlBuilder.append("/");
                                }
                                else {
                                    urlBuilder.append(SeoStringUtil.URL_HYPHEN);
                                }
                            }
                        }
                    }
                }
            }
        }

        if (UtilValidate.isNotEmpty(productId)) {
            if (!SeoConfigUtil.isCategoryUrlEnabledForContextPath(contextPath)) {
                urlBuilder.append("/p_");
            } else {
                productName = SeoUrlUtil.replaceSpecialCharsUrl(productName, SeoConfigUtil.getCharFilters());
                if (UtilValidate.isNotEmpty(productName)) {
                    urlBuilder.append(productName + SeoStringUtil.URL_HYPHEN);
                }
            }
            urlBuilder.append(productId);
        }

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
    @SuppressWarnings("serial")
    public static class SeoCatalogUrlInfo extends CatalogUrlServlet.CatalogUrlInfo {
        private final String origPath;
        private final boolean explicitProductRequest;
        private final boolean explicitCategoryRequest;
        private final List<String> trailCatalogIds;
        private final Locale locale; // the locale matched when URL is parsed (best-effort only)
        
        public SeoCatalogUrlInfo(String pathInfo, String productId, String categoryId, List<String> pathElements,
                String origPath, boolean explicitProductRequest, boolean explicitCategoryRequest, List<String> trailCatalogIds,
                Locale locale) {
            super(pathInfo, productId, categoryId, pathElements);
            this.origPath = origPath;
            this.explicitProductRequest = explicitProductRequest;
            this.explicitCategoryRequest = explicitCategoryRequest;
            this.trailCatalogIds = trailCatalogIds;
            this.locale = locale;
        }

        public String getOrigPath() { return origPath; }
        public boolean isExplicitProductRequest() { return explicitProductRequest; }
        public boolean isExplicitCategoryRequest() { return explicitCategoryRequest; }

        public boolean isProductRequest() { return explicitProductRequest || getProductId() != null; }
        public boolean isCategoryRequest() { return explicitCategoryRequest || (getCategoryId() != null && getProductId() == null); }

        public List<String> getTrailCatalogIds() { return trailCatalogIds; }
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
    public SeoCatalogUrlInfo matchInboundSeoCatalogUrl(Delegator delegator, String path, String contextPath) {

        String pathInfo = path;
 
        String urlSuffix = getUrlSuffix();
        if (UtilValidate.isNotEmpty(urlSuffix)) {
            if (pathInfo.endsWith(urlSuffix)) {
                pathInfo = pathInfo.substring(0, pathInfo.length() - pathInfo.length());
            } else {
                return null;
            }
        }
        
        boolean explicitCategoryRequest = false;
        boolean explicitProductRequest = false;

        List<String> pathElements = StringUtil.split(pathInfo, "/");
        if (UtilValidate.isEmpty(pathElements)) {
            return null;
        }
        

        String productId = null;
        String categoryIdForProduct = null;

        String categoryId = null;

        List<String> trailCatalogIds = new ArrayList<>(); // SCIPIO: 2017: new
        Locale matchedLocale = null; // the locale the URL appears to be
        
        matchedLocale = getProductServletPathNameLocale(pathElements.get(0));
        if (matchedLocale != null) {
            pathElements.remove(0);
            
            explicitProductRequest = true;

            // TODO

            //return new SeoCatalogUrlInfo(path, true, false, productId, null, pathElements);

        } else {
            matchedLocale = getCategoryServletPathNameLocale(pathElements.get(0));
            if (matchedLocale != null) {
                pathElements.remove(0);
                explicitCategoryRequest = true;

                // TODO

            } else {

                if (!SeoConfigUtil.isHandleImplicitRequests()) {
                    return null;
                }

                // TODO

            }
        }

        // TODO: FIX ALL OLD CODE BELOW


        String lastPathElement = pathElements.get(pathElements.size() - 1);

        if (UtilValidate.isNotEmpty(lastPathElement)) {


            String prevPathElement = null;
            if (pathElements.size() >= 2) {
                prevPathElement = pathElements.get(pathElements.size() - 2);
                if (UtilValidate.isEmpty(prevPathElement)) {
                    prevPathElement = null;
                }
            }

            boolean implicitRequestNameMatchesOnly = SeoConfigUtil.isImplicitRequestNameMatchesOnly();


            // Check if last path elem is explicitly a category
            String lastPathCategoryId = null;
            boolean lastPathCategoryIdValid = false;
            if (!explicitProductRequest) {
                lastPathCategoryId = getNameBasedCategoryIdMatchFromPathElement(lastPathElement);
                if (lastPathCategoryId == null) {
                    if (explicitCategoryRequest || !implicitRequestNameMatchesOnly) {
                        lastPathCategoryId = getFallbackCategoryIdFromPathElement(lastPathElement);
                    }
                }
                lastPathCategoryIdValid = isValidCategory(delegator, lastPathCategoryId);
            }

            String prevPathCategoryId = null;
            boolean prevPathCategoryIdValid = false;
            if (UtilValidate.isNotEmpty(prevPathElement)) {
                prevPathCategoryId = getNameBasedCategoryIdMatchFromPathElement(prevPathElement);
                if (prevPathCategoryId == null) {
                    if (explicitCategoryRequest || explicitProductRequest || !implicitRequestNameMatchesOnly) {
                        prevPathCategoryId = getFallbackCategoryIdFromPathElement(prevPathElement);
                    }
                }
                prevPathCategoryIdValid = isValidCategory(delegator, prevPathCategoryId);
            }

            // Get category for product (either prev path elem or inlined in last path elem)
            // SCIPIO: This block is ONLY valid if have a product request (implicit or explicit); original patch botched this
            boolean categoryIdForProductValid = false;
            if (!explicitCategoryRequest && SeoConfigUtil.isCategoryNameEnabled()) { // SCIPIO: This is already handled above: || pathInfo.startsWith("/" + CatalogUrlServlet.CATEGORY_REQUEST + "/")
                if (UtilValidate.isNotEmpty(prevPathCategoryId)) {
                    // SCIPIO: new case
                    categoryIdForProduct = prevPathCategoryId;
                    categoryIdForProductValid = prevPathCategoryIdValid;
                }

            }
            if (UtilValidate.isEmpty(categoryIdForProduct)) {
                categoryIdForProduct = null;
            }

            // Get product itself (note: has priority over category, if somehow there was a clash)
            // SCIPIO: SOLR product may miss from DB. For this reason can't validate product links from IDs and names alone.
            // Also see makeProductUrl limitations; can't always generate implicit links
            // In addition, explicitly want to disallow root matches on product IDs (unsafe product check) because we only match on
            // IDs which are too generic and may cause conflicts with various requests

            boolean safeProductRequest = explicitProductRequest || (!lastPathCategoryIdValid && categoryIdForProductValid);
            boolean allowIdOnlyProductCheck = !implicitRequestNameMatchesOnly; // this isn't exact meaning, but close enough

            if (!explicitCategoryRequest && (safeProductRequest || allowIdOnlyProductCheck) && UtilValidate.isNotEmpty(lastPathElement)) {
                List<String> urlElements = StringUtil.split(lastPathElement, SeoStringUtil.URL_HYPHEN);
                if (UtilValidate.isEmpty(urlElements)) {
                    try {
                        if (delegator.findOne("Product", UtilMisc.toMap("productId", lastPathElement), true) != null) {
                            productId = lastPathElement;
                        }
                    } catch (GenericEntityException e) {
                        Debug.logError(e, "Error looking up product info for ProductUrl with path info [" + pathInfo + "]: " + e.toString(), module);
                    }
                } else {
                    int i = urlElements.size() - 1;
                    String tempProductId = urlElements.get(i);
                    while (i >= 0) {
                        try {
                            List<EntityExpr> exprs = new ArrayList<>();
                            exprs.add(EntityCondition.makeCondition("productId", EntityOperator.EQUALS, lastPathElement));
//                            if (SeoConfigUtil.isSpecialProductId(tempProductId)) {
//                                exprs.add(EntityCondition.makeCondition("productId", EntityOperator.EQUALS, SeoConfigUtil.getSpecialProductId(tempProductId)));
//                            } else {
                                exprs.add(EntityCondition.makeCondition("productId", EntityOperator.EQUALS, tempProductId));
//                            }
                            List<GenericValue> products = delegator.findList("Product", EntityCondition.makeCondition(exprs, EntityOperator.OR), UtilMisc.toSet("productId", "productName"), null, null, true);

                            if (products != null && products.size() > 0) {
                                if (products.size() == 1) {
                                    productId = products.get(0).getString("productId");
                                    break;
                                } else {
                                    productId = tempProductId;
                                    break;
                                }
                            } else if (i > 0) {
                                tempProductId = urlElements.get(i - 1) + SeoStringUtil.URL_HYPHEN + tempProductId;
                            }
                        } catch (GenericEntityException e) {
                            Debug.logError(e, "Error looking up product info for ProductUrl with path info [" + pathInfo + "]: " + e.toString(), module);
                        }
                        i--;
                    }
                }
                if (UtilValidate.isEmpty(productId)) {
                    productId = null;
                }

                // SCIPIO: SOLR products may not be in database. If had explicit product path, assume product wanted.
                // Can also try to deduce: if last path element or closest category was a category but current one isn't a category, must be a product (see safeProductRequest)
                if (productId == null) {
                    boolean assumeProduct = safeProductRequest;

                    if (assumeProduct) {
                        if (UtilValidate.isEmpty(urlElements)) {
                            if (UtilValidate.isNotEmpty(lastPathElement)) {
                                productId = lastPathElement;
                            }
                        }
                        else {
                            String tempProductId = urlElements.get(urlElements.size()-1);
                            if (UtilValidate.isNotEmpty(tempProductId)) {
                                productId = tempProductId;
                            }
                        }
                    }

                }

            }

            // Handle category case
            if (explicitCategoryRequest) {
                // For explicit request, use last category ID regardless (so screen can show error if really missing)
                categoryId = lastPathCategoryId;
            }
            else {
                if (!explicitProductRequest && productId == null) {
                    // Got no product... no explicit requests...
                    // Use the categoryId, but only if managed to validate it; otherwise could be anything
                    if (lastPathCategoryIdValid) {
                        categoryId = lastPathCategoryId;
                    }
                }
            }
            if (UtilValidate.isEmpty(categoryId)) {
                categoryId = null;
            }

        }


        if (explicitProductRequest || explicitCategoryRequest || productId != null || categoryId != null) {
            return new SeoCatalogUrlInfo(pathInfo, productId, categoryId, pathElements, path,
                    explicitProductRequest, explicitCategoryRequest, trailCatalogIds, matchedLocale);
        }

        return null;
    }

    /*
     * *****************************************************
     * Helpers
     * *****************************************************
     */

//    /**
//     * Get a string lower cased and hyphen connected.
//     *
//     * @param name a String to be transformed
//     * @return String nice name
//     */
//    private static String getNiceName(String name) {
//        String niceName = null;
//        if (UtilValidate.isNotEmpty(name)) {
//            name = name.trim().replaceAll(" ", SeoStringUtil.URL_HYPHEN);
//            if (UtilValidate.isNotEmpty(name) && asciiPattern.matcher(name).matches()) {
//                niceName = name;
//            }
//        }
//        return niceName;
//    }

    @Deprecated
    private static String getNameBasedCategoryIdMatchFromPathElement(String pathElement) {
        if (UtilValidate.isEmpty(pathElement)) {
            return null;
        }

        String categoryId = null;

        Map<String, String> categoryNameIdMap = Collections.emptyMap(); // getCategoryNameIdMap();

        // SCIPIO: nameIdMap keys are actually categoryName-categoryId!
        // FIXME: This is unable to handle case where -categoryId is omitted!
        for (String categoryName : categoryNameIdMap.keySet()) {
            // we might get lucky and whole thing just matches (original patch code)...
            if (pathElement.startsWith(categoryName)) {
                categoryId = categoryNameIdMap.get(categoryName);
                break;
            }
        }

        if (UtilValidate.isNotEmpty(categoryId)) {
            return categoryId;
        }
        else {
            return null;
        }
    }

    private static String getFallbackCategoryIdFromPathElement(String pathElement) {
        if (UtilValidate.isEmpty(pathElement)) {
            return null;
        }

        String categoryId = null;

        List<String> hyphenElems = StringUtil.split(pathElement, SeoStringUtil.URL_HYPHEN);
        if (UtilValidate.isNotEmpty(hyphenElems)) {
            categoryId = hyphenElems.get(hyphenElems.size() - 1);
        }
        else {
            // Default patch code...
            categoryId = pathElement.trim();
        }

        if (UtilValidate.isNotEmpty(categoryId)) {
            return categoryId;
        }
        else {
            return null;
        }
    }

    /**
     * SCIPIO: checks if valid category.
     * Rewritten 2017-11.
     */
    private static boolean isValidCategory(Delegator delegator, String categoryId) {
        if (UtilValidate.isNotEmpty(categoryId)) {
            try {
                return (delegator.findOne("ProductCategory", UtilMisc.toMap("productCategoryId", categoryId), true) != null);
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                // Don't return, not written to handle this
            }
        }
        return false;
    }


    /**
     * SCIPIO: Checks if path starts with dir.
     * Rewritten 2017-11.
     */
    static boolean pathStartsWithDir(String path, String dir) {
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
}

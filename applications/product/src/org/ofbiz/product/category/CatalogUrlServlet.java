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
package org.ofbiz.product.category;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestLinkUtil;
import org.ofbiz.webapp.website.WebSiteWorker;

/**
 * CatalogUrlServlet - Catalog servlet for the web application.
 */
@SuppressWarnings("serial")
public class CatalogUrlServlet extends HttpServlet {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String CATALOG_URL_MOUNT_POINT = "products";
    /**
     * @deprecated SCIPIO: 2017: this was unhardcoded; use {@link org.ofbiz.webapp.control.RequestHandler#getControlServletPath(HttpServletRequest)}.
     */
    @Deprecated
    public static final String CONTROL_MOUNT_POINT = "control";
    public static final String PRODUCT_REQUEST = "product";
    public static final String CATEGORY_REQUEST = "category";
    
    public CatalogUrlServlet() {
        super();
    }

    /**
     * @see javax.servlet.http.HttpServlet#init(javax.servlet.ServletConfig)
     */
    @Override
    public void init(ServletConfig config) throws ServletException {
        super.init(config);
    }

    /**
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    @Override
    public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        doGet(request, response);
    }

    /**
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest, javax.servlet.http.HttpServletResponse)
     */
    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        Delegator delegator = (Delegator) getServletContext().getAttribute("delegator");

        // SCIPIO: NOTE: 2017-11-03: entire method refactored for reuse
        
        String pathInfo = request.getPathInfo();
        CatalogUrlInfo urlInfo = parseCatalogUrlPathElements(request, delegator, pathInfo);

        if (urlInfo == null) {
            RequestDispatcher rd = request.getRequestDispatcher(getControlServletPath(request) + "/main");
            rd.forward(request, response);
        } else {
            updateRequestForCatalogUrl(request, delegator, urlInfo);
            RequestDispatcher rd = request.getRequestDispatcher(getControlServletPath(request) + "/" + (urlInfo.getProductId() != null ? PRODUCT_REQUEST : CATEGORY_REQUEST));
            rd.forward(request, response);
        }
    }

    /**
     * SCIPIO: Returns control servlet path or empty string (same as getServletPath).
     */
    protected String getControlServletPath(ServletRequest request) {
        return RequestHandler.getControlServletPath(request);
        //return "/" + CONTROL_MOUNT_POINT;
    }
    
    public static class CatalogUrlInfo {
        private final String pathInfo;
        private final String productId;
        private final String categoryId;
        private final List<String> pathElements;

        public CatalogUrlInfo(String pathInfo, String productId, String categoryId, List<String> pathElements) {
            this.pathInfo = pathInfo;
            this.productId = productId;
            this.categoryId = categoryId;
            this.pathElements = pathElements;
        }
        
        public String getPathInfo() {
            return pathInfo;
        }
        public String getProductId() {
            return productId;
        }
        public String getCategoryId() {
            return categoryId;
        }
        public boolean hasTarget() { 
            return productId != null || categoryId != null;
        }
        public List<String> getPathElements() {
            return pathElements;
        }
    }
    
    /**
     * SCIPIO: factored out method to parse path elements.
     * Added 2017-11-03.
     */
    public static CatalogUrlInfo parseCatalogUrlPathElements(HttpServletRequest request, Delegator delegator, String pathInfo) {
        List<String> pathElements = pathInfo != null ? StringUtil.split(pathInfo, "/") : null;

        if (UtilValidate.isEmpty(pathElements)) { // SCIPIO: prevent NPE
            return null;
        }
        
        String productId = null;
        String categoryId = null;
        try {
            String lastPathElement = pathElements.get(pathElements.size() - 1);
            if (lastPathElement.startsWith("p_")) {
                productId = lastPathElement.substring(2);
                // SCIPIO: remove for products only
                pathElements.remove(pathElements.size() - 1);
            } else {
                GenericValue productCategory = EntityQuery.use(delegator).from("ProductCategory").where("productCategoryId", lastPathElement).cache(true).queryOne();
                if (UtilValidate.isNotEmpty(productCategory)) {
                    categoryId = lastPathElement;
                } else {
                    productId = lastPathElement;
                    // SCIPIO: remove for products only
                    pathElements.remove(pathElements.size() - 1);
                }
            }
            // SCIPIO: Don't remove this here; remove only for products
            //pathElements.remove(pathElements.size() - 1);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error in looking up ProductUrl or CategoryUrl with path info [" + pathInfo + "]: " + e.toString(), module);
        }
        
        return new CatalogUrlInfo(pathInfo, productId, categoryId, pathElements);
    }
    
    /**
     * SCIPIO: Factored out request updating code.
     * Added 2017-11-03.
     */
    public static void updateRequestForCatalogUrl(HttpServletRequest request, Delegator delegator, 
            String pathInfo, String productId, String categoryId, List<String> pathElements) {

        // SCIPIO: 2016-03-22: NEW EXTRA BEHAVIOR FOR TOP-LESS BROWSING: 
        // We have a problem here that CatalogUrlFilter does not have:
        // CatalogUrlFilter should (now) always set a top category, but for CatalogUrlServlet,
        // it's often possible for us to receive links that don't indicate full path AND for which we
        // don't have a trail in session.
        // So in these cases, we will emulate CatalogUrlFilter and replace everything to a path
        // under the main top category OR as best determine by CatalogUrlFilter#makeTrailElements.
        // NOTE: CatalogUrlFilter's solution is imperfect and restricts browsing, so it's not
        // a great model, but we should at least follow it.
        // WARN: This does not guarantee we have a "valid" category path; it's still possible
        // for other weirdness between the top category and the last part, but this should
        // help the worst cases.
        //
        // CASE 2: We will now also force a default trail if we get a product without path.
        // otherwise. This will fix some other cases.
        //
        // CASE 3: We will now also force a default trail if we get a category that's not top
        // and has no other path elements than itself.
        //
        // The combination of above cases will now make it so a link must be a full path if it wants
        // to override the default topCategory-based lookup. All other cases will use the default lookup like CatalogUrlFilter does.
        if ((UtilValidate.isNotEmpty(productId) && pathElements.size() == 0) ||
            (UtilValidate.isNotEmpty(categoryId) && pathElements.size() <= 1 && !CategoryWorker.isCategoryTop(request, categoryId)) ||
            ((UtilValidate.isNotEmpty(productId) || UtilValidate.isNotEmpty(categoryId)) && !CatalogUrlFilter.hasTopCategory(request, categoryId, pathElements))
           ) {
            // We don't have a top category anywhere. So we'll emulate CatalogUrlFilter.
            List<String> trailElements = CatalogUrlFilter.makeTrailElements(request, delegator, categoryId, productId);
            if (trailElements != null) {
                // Replace the pathElements with our trail
                pathElements = trailElements;
            }
        }
        
        // SCIPIO: Update the categoryId to match the last path element
        if (pathElements.size() >= 1) {
            categoryId = pathElements.get(pathElements.size() - 1);
        }
        
        // SCIPIO: Delegate the logic previously here to factored method
        CatalogUrlFilter.updateRequestAndTrail(request, categoryId, productId, pathElements, null);

    }
    
    /**
     * SCIPIO: Factored out request updating code.
     * Added 2017-11-03.
     */
    public static void updateRequestForCatalogUrl(HttpServletRequest request, Delegator delegator, CatalogUrlInfo urlInfo) {
        updateRequestForCatalogUrl(request, delegator, urlInfo.getPathInfo(), urlInfo.getProductId(), urlInfo.getCategoryId(), urlInfo.getPathElements());
    }

    /**
     * @see javax.servlet.http.HttpServlet#destroy()
     */
    @Override
    public void destroy() {
        super.destroy();
    }

    public static String makeCatalogUrl(HttpServletRequest request, String productId, String currentCategoryId, String previousCategoryId) {
        StringBuilder urlBuilder = new StringBuilder();
        urlBuilder.append(request.getServletContext().getContextPath()); // SCIPIO: NOTE: no longer need getSession() for getServletContext(), since servlet API 3.0
        if (urlBuilder.length() == 0 || urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
            urlBuilder.append("/");
        }
        urlBuilder.append(CATALOG_URL_MOUNT_POINT);

        if (UtilValidate.isNotEmpty(currentCategoryId)) {
            List<String> trail = CategoryWorker.getTrail(request);
            trail = CategoryWorker.adjustTrail(trail, currentCategoryId, previousCategoryId);
            for (String trailCategoryId: trail) {
                if ("TOP".equals(trailCategoryId)) continue;
                urlBuilder.append("/");
                urlBuilder.append(trailCategoryId);
            }
        }

        if (UtilValidate.isNotEmpty(productId)) {
            urlBuilder.append("/p_");
            urlBuilder.append(productId);
        }

        return urlBuilder.toString();
    }

    public static String makeCatalogUrl(String contextPath, List<String> crumb, String productId, String currentCategoryId, String previousCategoryId) {
        StringBuilder urlBuilder = new StringBuilder();
        urlBuilder.append(contextPath);
        if (urlBuilder.length() == 0 || urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
            urlBuilder.append("/");
        }
        urlBuilder.append(CATALOG_URL_MOUNT_POINT);

        if (UtilValidate.isNotEmpty(currentCategoryId)) {
            crumb = CategoryWorker.adjustTrail(crumb, currentCategoryId, previousCategoryId);
            for (String trailCategoryId: crumb) {
                if ("TOP".equals(trailCategoryId)) continue;
                urlBuilder.append("/");
                urlBuilder.append(trailCategoryId);
            }
        }

        if (UtilValidate.isNotEmpty(productId)) {
            urlBuilder.append("/p_");
            urlBuilder.append(productId);
        }

        return urlBuilder.toString();
    }
    
    /**
     * SCIPIO: NEW, FULLY-FEATURED java-frontend catalog link building method, that passes everything through
     * request encoding and supports everything that <code>@ofbizCatalogUrl</code> FTL macro supports.
     * <p>
     * This version supports a webSiteId and contextPath that, if specified, will turn the link-building into an
     * inter-webapp mode that avoids use of session information.
     * NOTE: it will do this even if the passed webSiteId is the same as the one of current request
     * (there is intentionally no check for this, so the parameter has a double function).
     * If contextPath is omitted, it is determined automatically from webSiteId.
     * It is preferable to use webSiteId where possible.
     * <p>
     * 2017-11: This method will now automatically implement the alternative SEO link building.
     * NOTE: 2017-11-06: this overload requires explicit locale - it does NOT use locale from request.
     */
    public static String makeCatalogLink(HttpServletRequest request, HttpServletResponse response, Locale locale, String productId, String currentCategoryId,
            String previousCategoryId, Object params, FullWebappInfo targetWebappInfo, Boolean fullPath, Boolean secure, Boolean encode) {
        CatalogUrlBuilder builder = CatalogUrlBuilder.getBuilder(true, request, null, targetWebappInfo);
        return builder.makeCatalogLink(request, response, locale, productId, currentCategoryId, previousCategoryId, params, targetWebappInfo, fullPath, secure, encode);
    }

    /**
     * SCIPIO: NEW, FULLY-FEATURED java-frontend catalog link building method, that passes everything through
     * request encoding and supports everything that <code>@ofbizCatalogUrl</code> FTL macro supports.
     * <p>
     * This version assumes the current webapp is the target webapp and may use session information.
     */
    public static String makeCatalogLink(HttpServletRequest request, HttpServletResponse response, Locale locale,
            String productId, String currentCategoryId, String previousCategoryId, Object params, Boolean fullPath, Boolean secure, Boolean encode) {
        return makeCatalogLink(request, response, locale, productId, currentCategoryId, previousCategoryId, params, null, fullPath, secure, encode);
    }

    /**
     * SCIPIO: NEW, FULLY-FEATURED java-frontend catalog link building method, that passes everything through
     * request encoding and supports everything that <code>@ofbizCatalogUrl</code> FTL macro supports.
     * <p>
     * This builds the link in a completely static, inter-webapp way, using no request information.
     * <p>
     * NOTE: if contextPath is omitted (null), it will be determined automatically.
     */
    public static String makeCatalogLink(Delegator delegator, LocalDispatcher dispatcher, Locale locale, String productId, String currentCategoryId,  
            String previousCategoryId, Object params, FullWebappInfo targetWebappInfo, Boolean fullPath, Boolean secure, 
            FullWebappInfo currentWebappInfo, FullWebappInfo.Cache webappInfoCache) {
        return makeCatalogLink(delegator, dispatcher, locale, productId, currentCategoryId, previousCategoryId, params, targetWebappInfo, 
                fullPath, secure, null, currentWebappInfo, webappInfoCache, null, null);
    }
    
    /**
     * SCIPIO: NEW, FULLY-FEATURED java-frontend catalog link building method, that passes everything through
     * request encoding and supports everything that <code>@ofbizCatalogUrl</code> FTL macro supports.
     * <p>
     * This builds the link in a completely static, inter-webapp way, using no request information, but may also optionally encode
     * the resulting link.
     * <p>
     * NOTE: if contextPath is omitted (null), it will be determined automatically.
     * <p>
     * 2017-11: This method will now automatically implement the alternative SEO link building.
     */
    public static String makeCatalogLink(Delegator delegator, LocalDispatcher dispatcher, Locale locale, String productId, String currentCategoryId,  
            String previousCategoryId, Object params, FullWebappInfo targetWebappInfo, Boolean fullPath, Boolean secure,
            Boolean encode, FullWebappInfo currentWebappInfo, FullWebappInfo.Cache webappInfoCache, HttpServletRequest request, HttpServletResponse response) {
        CatalogUrlBuilder builder = CatalogUrlBuilder.getBuilder(false, request, delegator, targetWebappInfo);
        return builder.makeCatalogLink(delegator, dispatcher, locale, productId, currentCategoryId, previousCategoryId, params, targetWebappInfo, 
                null, null, fullPath, secure, encode, currentWebappInfo, webappInfoCache, request, response);
    }
    
    /**
     * SCIPIO: 2017: Wraps all the category URL method calls so they can be switched out without
     * ruining the code.
     */
    public static abstract class CatalogUrlBuilder {
        
        /**
         * SCIPIO: 2017: allows plugging in custom low-level URL builders.
         * FIXME: poor initialization logic
         */
        private static List<CatalogUrlBuilder.Factory> urlBuilderFactories = Collections.emptyList();
        
        public static CatalogUrlBuilder getDefaultBuilder() {
            return OfbizCatalogUrlBuilder.getInstance();
        }
        
        public static CatalogUrlBuilder getBuilder(boolean withRequest, HttpServletRequest request, Delegator delegator, FullWebappInfo targetWebappInfo) {
            if (withRequest) {
                if (delegator == null) delegator = (Delegator) request.getAttribute("delegator");
                if (targetWebappInfo == null) targetWebappInfo = FullWebappInfo.fromRequest(request);
            }
            for(CatalogUrlBuilder.Factory factory : urlBuilderFactories) {
                CatalogUrlBuilder builder = factory.getCatalogUrlBuilder(withRequest, request, delegator, targetWebappInfo);
                if (builder != null) return builder;
            }
            return getDefaultBuilder();
        }
        
        /**
         * @deprecated FIXME: this may need be redesigned with property config due to possible synchronization/ordering issues.
         */
        @Deprecated
        public static synchronized void registerUrlBuilder(String name, CatalogUrlBuilder.Factory builderFactory) {
            if (urlBuilderFactories.contains(builderFactory)) return;
            List<CatalogUrlBuilder.Factory> newList = new ArrayList<>(urlBuilderFactories);
            newList.add(builderFactory);
            urlBuilderFactories = Collections.unmodifiableList(newList);
        }
        
        
        public interface Factory {
            /**
             * Returns builder or null if not applicable to request.
             */
            CatalogUrlBuilder getCatalogUrlBuilder(boolean withRequest, HttpServletRequest request, Delegator delegator, FullWebappInfo targetWebappInfo);
        }
        
        // low-level building methods (named after legacy ofbiz methods)
        public abstract String makeCatalogUrl(HttpServletRequest request, Locale locale, String productId, String currentCategoryId, String previousCategoryId);
        public abstract String makeCatalogUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, FullWebappInfo targetWebappInfo, String currentCatalogId, List<String> crumb, String productId, String currentCategoryId, String previousCategoryId);
        
        /**
         * Common/default high-level makeCatalogLink implementation (new Scipio method).
         */
        public String makeCatalogLink(HttpServletRequest request, HttpServletResponse response, Locale locale, String productId, String currentCategoryId,
                String previousCategoryId, Object params, FullWebappInfo targetWebappInfo, Boolean fullPath, Boolean secure, Boolean encode) {
            // SCIPIO: 2017-11-06: NOT doing this here - caller or other overloads should do.
            //if (locale == null) {
            //    locale = UtilHttp.getLocale(request);
            //}
            
            if (targetWebappInfo != null) {
                // SPECIAL CASE: if there is a specific webSiteId, we must NOT use the current request stuff,
                // and build as if we had no request
                
                Delegator delegator = (Delegator) request.getAttribute("delegator");
                LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
                String currentCatalogId = CatalogWorker.getCurrentCatalogId(request);
                List<String> crumb = CategoryWorker.getTrail(request);
                
                FullWebappInfo.Cache webappInfoCache = FullWebappInfo.Cache.fromRequest(request);
                FullWebappInfo currentWebappInfo = FullWebappInfo.fromRequest(request, webappInfoCache);
                return this.makeCatalogLink(delegator, dispatcher, locale, productId, currentCategoryId, previousCategoryId, params, 
                        targetWebappInfo, currentCatalogId, crumb, fullPath, secure, encode, currentWebappInfo, webappInfoCache, request, response);
            } else {
                String url = this.makeCatalogUrl(request, locale, productId, currentCategoryId, previousCategoryId);
                if (url == null) return null;
                
                url = appendLinkParams(url, params);
                
                return RequestLinkUtil.buildLinkHostPartAndEncode(request, response, locale, url, fullPath, secure, encode, true);
            }
        }
        
        /**
         * Common/default high-level makeCatalogLink implementation (new Scipio method).
         */
        public String makeCatalogLink(Delegator delegator, LocalDispatcher dispatcher, Locale locale, String productId, String currentCategoryId,  
                String previousCategoryId, Object params, FullWebappInfo targetWebappInfo, String currentCatalogId, List<String> crumb, Boolean fullPath, Boolean secure,
                Boolean encode, FullWebappInfo currentWebappInfo, FullWebappInfo.Cache webappInfoCache, HttpServletRequest request, HttpServletResponse response) {
            if (targetWebappInfo == null) {
                throw new IllegalArgumentException("targetWebappInfo is missing - webSiteId or contextPath (prefix) must be specified");
            }
            
            String url = this.makeCatalogUrl(delegator, dispatcher, locale, targetWebappInfo, currentCatalogId, crumb, productId, currentCategoryId, previousCategoryId);
            if (url == null) return null;
            
            url = appendLinkParams(url, params);

            return RequestLinkUtil.buildLinkHostPartAndEncode(delegator, locale, targetWebappInfo, url, fullPath, secure, encode, true, currentWebappInfo, request, response);
        }
        
        /**
         * Implements the stock ofbiz catalog (non-alt) URLs.
         */
        public static class OfbizCatalogUrlBuilder extends CatalogUrlBuilder {
            private static final OfbizCatalogUrlBuilder INSTANCE = new OfbizCatalogUrlBuilder();
            
            public static final OfbizCatalogUrlBuilder getInstance() { return INSTANCE; }

            @Override
            public String makeCatalogUrl(HttpServletRequest request, Locale locale, String productId, String currentCategoryId,
                    String previousCategoryId) {
                return CatalogUrlServlet.makeCatalogUrl(request, productId, currentCategoryId, previousCategoryId);
            }
            @Override
            public String makeCatalogUrl(Delegator delegator, LocalDispatcher dispatcher, Locale locale, FullWebappInfo targetWebappInfo, String currentCatalogId, List<String> crumb, String productId,
                    String currentCategoryId, String previousCategoryId) {
                return CatalogUrlServlet.makeCatalogUrl(targetWebappInfo.getContextPath(), crumb, productId, currentCategoryId, previousCategoryId);
            }
        }
        
        // helpers
        
        /**
         * Appends params for catalog URLs.
         * <p>
         * WARN: this currently assumes the url contains no params, could change in future
         */
        protected static String appendLinkParams(String url, Object paramsObj) {
            if (paramsObj == null) {
                return url;
            }
            String params = paramsObj.toString();
            if (params.isEmpty()) {
                return url;
            }
            if (params.startsWith("?")) {
                url += params;
            } else {
                url += "?" + params;
            }
            return url;
        }
    }

}

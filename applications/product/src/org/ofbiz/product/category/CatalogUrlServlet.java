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
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.OfbizUrlBuilder;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestLinkUtil;
import org.ofbiz.webapp.control.WebAppConfigurationException;
import org.ofbiz.webapp.website.WebSiteWorker;

/**
 * CatalogUrlServlet - Catalog servlet for the web application.
 */
@SuppressWarnings("serial")
public class CatalogUrlServlet extends HttpServlet {

    public static final String module = CatalogUrlServlet.class.getName();

    public static final String CATALOG_URL_MOUNT_POINT = "products";
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
            RequestDispatcher rd = request.getRequestDispatcher("/" + CONTROL_MOUNT_POINT + "/main");
            rd.forward(request, response);
        } else {
            updateRequestForCatalogUrl(request, delegator, urlInfo);
            RequestDispatcher rd = request.getRequestDispatcher("/" + CONTROL_MOUNT_POINT + "/" + (urlInfo.getProductId() != null ? PRODUCT_REQUEST : CATEGORY_REQUEST));
            rd.forward(request, response);
        }
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
        urlBuilder.append(request.getSession().getServletContext().getContextPath());
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
     */
    public static String makeCatalogLink(HttpServletRequest request, HttpServletResponse response, String productId, String currentCategoryId,
            String previousCategoryId, Object params, String webSiteId, String contextPath, Boolean fullPath, Boolean secure, Boolean encode) throws WebAppConfigurationException, IOException {
        if (UtilValidate.isEmpty(webSiteId)) {
            webSiteId = null;
        }
        if (UtilValidate.isEmpty(contextPath)) {
            contextPath = null;
        }
        
        if (webSiteId != null || contextPath != null) {
            // SPECIAL CASE: if there is a specific webSiteId, we must NOT use the current session stuff,
            // and build as if we had no request
            
            Delegator delegator = (Delegator) request.getAttribute("delegator");
            LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
            Locale locale = UtilHttp.getLocale(request);
            
            return makeCatalogLink(delegator, dispatcher, locale, productId, currentCategoryId, previousCategoryId, params, webSiteId, contextPath, fullPath, secure, encode, request, response);
        } else {
            CatalogUrlBuilder builder = getCatalogUrlBuilder(true, request, null, contextPath, webSiteId);

            String url = builder.makeCatalogUrl(request, 
                    productId, currentCategoryId, previousCategoryId);
            
            url = appendLinkParams(url, params);
            
            return RequestLinkUtil.buildLinkHostPartAndEncode(request, response, url, fullPath, secure, encode);
        }
    }
    
    /**
     * SCIPIO: NEW, FULLY-FEATURED java-frontend catalog link building method, that passes everything through
     * request encoding and supports everything that <code>@ofbizCatalogUrl</code> FTL macro supports.
     * <p>
     * This version assumes the current webapp is the target webapp and may use session information.
     */
    public static String makeCatalogLink(HttpServletRequest request, HttpServletResponse response, 
            String productId, String currentCategoryId, String previousCategoryId, Object params, Boolean fullPath, Boolean secure, Boolean encode) throws WebAppConfigurationException, IOException {
        return makeCatalogLink(request, response, productId, currentCategoryId, previousCategoryId, params, null, null, fullPath, secure, encode);
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
            String previousCategoryId, Object params, String webSiteId, String contextPath, Boolean fullPath, Boolean secure) throws WebAppConfigurationException, IOException {
        return makeCatalogLink(delegator, dispatcher, locale, productId, currentCategoryId, previousCategoryId, params, webSiteId, contextPath, fullPath, secure, null, null, null);
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
            String previousCategoryId, Object params, String webSiteId, String contextPath, Boolean fullPath, Boolean secure,
            Boolean encode, HttpServletRequest request, HttpServletResponse response) throws WebAppConfigurationException, IOException {
        if (UtilValidate.isEmpty(webSiteId) && UtilValidate.isEmpty(contextPath)) {
            throw new IOException("webSiteId and contextPath (prefix) are missing - at least one must be specified");
        }
        
        if (UtilValidate.isEmpty(contextPath)) {
            contextPath = RequestLinkUtil.getWebSiteContextPath(delegator, webSiteId);
        }
        
        String url;
        
        CatalogUrlBuilder builder = getCatalogUrlBuilder(false, request, delegator, contextPath, webSiteId);
        
        url = builder.makeCatalogUrl(delegator, contextPath, null, productId, currentCategoryId, previousCategoryId);
        
        url = appendLinkParams(url, params);
        
        return RequestLinkUtil.buildLinkHostPartAndEncode(delegator, webSiteId, url, fullPath, secure, encode, request, response);
    }
    
    /**
     * Appends params for catalog URLs.
     * <p>
     * WARN: this currently assumes the url contains no params, could change in future
     */
    protected static String appendLinkParams(String url, Object paramsObj) throws IOException {
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
    
    /**
     * SCIPIO: 2017: Wraps all the category URL method calls so they can be switched out without
     * ruining the code.
     */
    public interface CatalogUrlBuilder {
        boolean isEnabled(boolean withRequest, HttpServletRequest request, Delegator delegator, String contextPath, String webSiteId);
        String makeCatalogUrl(HttpServletRequest request, String productId, String currentCategoryId, String previousCategoryId);
        String makeCatalogUrl(Delegator delegator, String contextPath, List<String> crumb, String productId, String currentCategoryId, String previousCategoryId);
    }
    
    public static class DefaultCatalogUrlBuilder implements CatalogUrlBuilder {
        private static final DefaultCatalogUrlBuilder INSTANCE = new DefaultCatalogUrlBuilder();
        
        public static final DefaultCatalogUrlBuilder getInstance() { return INSTANCE; }
        
        @Override
        public boolean isEnabled(boolean withRequest, HttpServletRequest request, Delegator delegator, String contextPath,
                String webSiteId) {
            return true;
        }
        @Override
        public String makeCatalogUrl(HttpServletRequest request, String productId, String currentCategoryId,
                String previousCategoryId) {
            return CatalogUrlServlet.makeCatalogUrl(request, productId, currentCategoryId, previousCategoryId);
        }
        @Override
        public String makeCatalogUrl(Delegator delegator, String contextPath, List<String> crumb, String productId,
                String currentCategoryId, String previousCategoryId) {
            return CatalogUrlServlet.makeCatalogUrl(contextPath, crumb, productId, currentCategoryId, previousCategoryId);
        }
    }
    
    protected static CatalogUrlBuilder getCatalogUrlBuilder(boolean withRequest, HttpServletRequest request, Delegator delegator, String contextPath, String webSiteId) {
        if (withRequest) {
            if (delegator == null) delegator = (Delegator) request.getAttribute("delegator");
            if (contextPath == null) contextPath = request.getContextPath();
            if (webSiteId == null) webSiteId = WebSiteWorker.getWebSiteId(request);
        }
        for(CatalogUrlBuilder builder : UrlBuilders.urlBuilders) {
            if (builder.isEnabled(withRequest, request, delegator, contextPath, webSiteId)) return builder;
        }
        return DefaultCatalogUrlBuilder.getInstance();
    }
    
    /**
     * SCIPIO: 2017: allows plugging in custom low-level URL builders.
     * FIXME: poor initialization logic
     */
    private static class UrlBuilders {
        private static List<CatalogUrlBuilder> urlBuilders = Collections.emptyList();
    }
    
    public static synchronized void registerUrlBuilder(String name, CatalogUrlBuilder builder) {
        if (UrlBuilders.urlBuilders.contains(builder)) return;
        List<CatalogUrlBuilder> newList = new ArrayList<>(UrlBuilders.urlBuilders);
        newList.add(builder);
        UrlBuilders.urlBuilders = Collections.unmodifiableList(newList);
    }
}

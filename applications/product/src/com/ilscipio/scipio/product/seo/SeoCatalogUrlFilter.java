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
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.UrlServletHelper;
import org.ofbiz.entity.Delegator;
import org.ofbiz.product.catalog.CatalogWorker;
import org.ofbiz.product.category.CatalogUrlFilter;
import org.ofbiz.product.category.CategoryWorker;
import org.ofbiz.webapp.WebAppUtil;
import org.ofbiz.webapp.control.ContextFilter;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestLinkUtil;
import org.ofbiz.webapp.website.WebSiteWorker;

import com.ilscipio.scipio.product.seo.SeoCatalogUrlWorker.SeoCatalogUrlInfo;

/**
 * SCIPIO: 2017: Seo catalog URL filter.
 * <p>
 * Some parts adapted from the original <code>org.ofbiz.product.category.ftl.CatalogUrlSeoTransform</code>.
 * <p>
 * TODO: FUTURE: This filter should not conceptually extend CatalogUrlFilter; should try to
 * separate them.
 */
public class SeoCatalogUrlFilter extends CatalogUrlFilter { // extends ContextFilter implements Filter

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static final String SEOURLINFO_ATTR = "_SCPSEO_URLINFO_";
    public static final String FORWARDED_ATTR = "_SCPSEO_FWDED_";
    public static final String REQWRAPPED_ATTR = "_SCPSEO_REQWRAP_";

    static {
        // TODO?: unhardcode via properties?
        SeoCatalogUrlWorker.registerUrlBuilder();
    }

    protected String productRequestPath = "/" + PRODUCT_REQUEST;
    protected String categoryRequestPath = "/" + CATEGORY_REQUEST;
    protected boolean seoUrlEnabled = true;
    protected boolean debug = false;

    // NOTE: this must not be static anymore (2017-11-18)
    protected SeoCatalogUrlWorker urlWorker = null;
    
    protected boolean rewriteOutboundUrls = false;

    @Override
    public void init(FilterConfig config) throws ServletException {
        super.init(config);

        debug = Boolean.TRUE.equals(UtilMisc.booleanValueVersatile(config.getInitParameter("debug")));

        seoUrlEnabled = !Boolean.FALSE.equals(UtilMisc.booleanValueVersatile(config.getInitParameter("seoUrlEnabled")));
        if (seoUrlEnabled) {
            WebsiteSeoConfig.registerWebsiteForSeo(WebsiteSeoConfig.makeConfig(config.getServletContext(), true));

            urlWorker = SeoCatalogUrlWorker.getInstance(null, config.getServletContext().getInitParameter("webSiteId"));
        }
        
        rewriteOutboundUrls = Boolean.TRUE.equals(UtilMisc.booleanValueVersatile(config.getInitParameter("rewriteOutboundUrls")));
    }

    @Override
    public void doFilter(ServletRequest req, ServletResponse resp, FilterChain chain) throws IOException, ServletException {
        HttpServletRequest request = (HttpServletRequest) req;
        HttpServletResponse response = (HttpServletResponse) resp;
        
        // TODO: REVIEW:
        //UrlServletHelper.setRequestAttributes(request, delegator, request.getServletContext());

        if (seoUrlEnabled) {
            
            Delegator delegator = WebAppUtil.getDelegatorFilterSafe(request);

            if (SeoConfig.DEBUG_FORCERELOAD) { // force reload the worker and config
                urlWorker = SeoCatalogUrlWorker.createInstanceDeep(delegator, config.getServletContext().getInitParameter("webSiteId"));
            }

            // TODO: REVIEW: it's possible some of the "always-run" calls below (such as prepareRequestAlways)
            // should actually run again even after forward...
            if (!Boolean.TRUE.equals(request.getAttribute(FORWARDED_ATTR))) {

                CatalogUrlFilter.prepareRequestAlways(request, response, delegator);
                
                String path = getMatchablePath(request); 
                
                if (UtilValidate.isNotEmpty(path)) {
                    if (urlWorker.getConfig().isSeoUrlEnabledForContextPath(request.getContextPath())) {
                        getCatalogTopCategory(request); // TODO: REVIEW
                        boolean forwarded = matchSeoCatalogUrlAndForward(request, response, delegator, path);
                        if (forwarded) return;
                    } else {
                        // TODO/FIXME: see CatalogUrlFitler for explanation for why these are here
                        getCatalogTopCategory(request);
                        UrlServletHelper.setViewQueryParameters(request, new StringBuilder());
                    }
        
                    //Check path alias (from CatalogUrlFilter)
                    // TODO/FIXME: REVIEW: control flow makes no sense for this (no boolean result to check)
                    //UrlServletHelper.checkPathAlias(request, response, delegator, pathInfo);
                }
            
                // must do at every forward - see below
//                if (!Boolean.TRUE.equals(request.getAttribute(REQWRAPPED_ATTR))) {
//                    request.setAttribute(REQWRAPPED_ATTR, Boolean.TRUE);
//                    chain.doFilter(request, getResponseWrapper(request, response, delegator));
//                    return;
//                }
            }

            // NOTE: 2017-11-21: we MUST rewrap the response every time we are visited!
            // otherwise it causes problems with the filter order.
            // hopefully this doesn't cause any issues.
            request.setAttribute(REQWRAPPED_ATTR, Boolean.TRUE);
            chain.doFilter(request, getResponseWrapper(request, response, delegator));
        } else {
            // currently delegates to CatalogUrlFilter.
            // TODO: compose them differently in the future
            super.doFilter(request, response, chain);
        }
    }
    
    public boolean matchSeoCatalogUrlAndForward(HttpServletRequest request, HttpServletResponse response, Delegator delegator, String matchablePath) throws ServletException, IOException {
        SeoCatalogUrlInfo urlInfo = matchInboundSeoCatalogUrl(request, delegator, matchablePath);
        if (urlInfo != null) {
            boolean res = updateRequestForSeoCatalogUrl(request, delegator, urlInfo);
            if (!res) return false;
            return forwardSeoUrl(request, response, delegator, urlInfo);
        }
        return false;
    }

    public static String getMatchablePath(HttpServletRequest request) {
        return RequestLinkUtil.getServletAndPathInfo(request);
    }

    public SeoCatalogUrlInfo matchInboundSeoCatalogUrl(HttpServletRequest request, Delegator delegator, String matchablePath) {
        return urlWorker.matchInboundSeoCatalogUrl(delegator, matchablePath, request.getContextPath(), 
                WebSiteWorker.getWebSiteId(request), CatalogWorker.getCurrentCatalogId(request));
    }

    /**
     * Sets the product/category IDs in request and updates trail.
     */
    public static boolean updateRequestForSeoCatalogUrl(HttpServletRequest request, Delegator delegator, SeoCatalogUrlInfo urlInfo) {

        if (urlInfo.isProductRequest()) {
            if (urlInfo.getProductId() != null) {
                request.setAttribute("product_id", urlInfo.getProductId());
                request.setAttribute("productId", urlInfo.getProductId());
            }
            request.setAttribute("productCategoryId", urlInfo.getCategoryId()); // EVEN IF NULL!
        } else { // if (CatalogUrlServlet.CATEGORY_REQUEST.equals(targetRequest)) {
            request.setAttribute("productCategoryId", urlInfo.getCategoryId()); // EVEN IF NULL!
        }
        
        String rootCategoryId = null;
        if (urlInfo.getPathCategoryIds() != null && urlInfo.getPathCategoryIds().size() >= 1) {
            rootCategoryId = urlInfo.getPathCategoryIds().get(0);
        }
        request.setAttribute("rootCategoryId", rootCategoryId); // EVEN IF NULL!

        // FIXME: Doing something completely different until further review...
//        String topCategoryId = CatalogUrlFilter.getCatalogTopCategory(request);
//        List<GenericValue> trailCategories = CategoryWorker.getRelatedCategoriesRet(request, "trailCategories", topCategoryId, false, false, true);
//        List<String> trailCategoryIds = EntityUtil.getFieldListFromEntityList(trailCategories, "productCategoryId", true);
//        updateRequestTrail(request, delegator, urlInfo.getProductId(), urlInfo.getCategoryId(), trailCategoryIds, topCategoryId);
        
        // FOR NOW, just replace the whole trail with what we got for time being
        List<String> newTrail = new ArrayList<>();
        newTrail.add("TOP");
        newTrail.addAll(urlInfo.getPathCategoryIds());
        if (urlInfo.getCategoryId() != null && !urlInfo.getCategoryId().equals(newTrail.get(newTrail.size() - 1))) {
            newTrail.add(urlInfo.getCategoryId());
        }
        CategoryWorker.setTrail(request, newTrail);
        
        request.setAttribute("categoryTrailUpdated", Boolean.TRUE); // SCIPIO: This is new
        
        return true;
    }

    /**
     * TRAIL UPDATES, emulates CatalogUrlFilter.doFilter,
     * TODO: 2017: MISSING TRAIL UPDATES HERE, REQUIRED BY SCREENS AND SHOP
     */
    static void updateRequestTrail(HttpServletRequest request, Delegator delegator, String productId, String productCategoryId, List<String> trailCategoryIds, String topCategoryId) {
        // look for productCategoryId from productId
        if (UtilValidate.isNotEmpty(productId)) {
            // SCIPIO: factored out
            String catId = CatalogUrlFilter.getProductMatchingCategoryId(delegator, productId, trailCategoryIds);
            if (catId != null) {
                productCategoryId = catId;
            }
        }

        // SCIPIO: 2016-03-22: FIXME?: The loop below was found to cause invalid category paths in SOLR addToSolr
        // (was very similar code) and had to be fixed there. I think there is a chance there may be bugs here as well,
        // but I'm not certain.

        // generate trail elements from productCategoryId
        if (UtilValidate.isNotEmpty(productCategoryId)) {
            // SCIPIO: 2017-11-07: factored out.
            CatalogUrlFilter.getTrailElementsAndUpdateRequestAndTrail(request, delegator, productId, productCategoryId, trailCategoryIds, topCategoryId);
        }
    }

    public boolean forwardSeoUrl(HttpServletRequest request, HttpServletResponse response, Delegator delegator, SeoCatalogUrlInfo urlInfo) throws ServletException, IOException {
        StringBuilder fwdUrl = new StringBuilder();
        fwdUrl.append(getControlServletPath(request));
        String targetRequest = urlInfo.isProductRequest() ? productRequestPath : categoryRequestPath;
        fwdUrl.append(targetRequest);

        // TODO: REVIEW: this is from CatalogUrlFilter.
        // adding parameters may cause unexpected behavior, but it is 
        // what CatalogUrlFilter is still doing, so until it's reviewed there, it stays here after all...
        UrlServletHelper.setViewQueryParameters(request, fwdUrl);
        // TODO: REVIEW: this is from CatalogUrlFilter.
        // This is done later by ContextFilter so I don't know why this is here...
        ContextFilter.setAttributesFromRequestBody(request);
        
        if (debug || Debug.verboseOn()) { // TODO?: verbose or debug flag?
            if (urlInfo.isProductRequest()) {
                Debug.logInfo("SEO: [Forwarding request]: " + urlInfo.getOrigPath()
                    + " (" + fwdUrl + "); args: [productId: " + urlInfo.getProductId()
                    + "; productCategoryId: " + urlInfo.getCategoryId() + "]", module);
            } else {
                Debug.logInfo("SEO: [Forwarding request]: " + urlInfo.getOrigPath()
                    + " (" + fwdUrl + "); args: [productCategoryId: " + urlInfo.getCategoryId() + "]", module);
            }
        }

        // TODO: REVIEW: this attr may cause problems with all the filters + generic name...
        request.setAttribute("ORIGINAL_REQUEST_URI", request.getRequestURI());

        RequestDispatcher rd = request.getRequestDispatcher(fwdUrl.toString());
        request.setAttribute(SEOURLINFO_ATTR, urlInfo);
        request.setAttribute(FORWARDED_ATTR, Boolean.TRUE);
        request.setAttribute(REQWRAPPED_ATTR, Boolean.TRUE);
        rd.forward(request, getResponseWrapper(request, response, delegator));
        return true;
    }

    @SuppressWarnings("unused")
    private String rebuildCatalogLink(HttpServletRequest request, Delegator delegator, SeoCatalogUrlInfo urlInfo) {
        Locale locale = UtilHttp.getLocale(request);
        return urlWorker.makeCatalogLink(delegator, urlInfo, locale);
    }

    protected ServletResponse getResponseWrapper(HttpServletRequest req, HttpServletResponse res, Delegator delegator) {
        if (rewriteOutboundUrls) {
            return new SeoCatalogUrlResponseWrapper(req, res, delegator);
        } else {
            return res;
        }
    }

    protected class SeoCatalogUrlResponseWrapper extends HttpServletResponseWrapper {
        private final HttpServletRequest request;
        private final Delegator delegator;
        private final String productReqPath;
        private final String categoryReqPath;

        public SeoCatalogUrlResponseWrapper(HttpServletRequest request, HttpServletResponse response, Delegator delegator) {
            super(response);
            this.request = request;
            this.delegator = delegator;
            String controlServletPath = RequestHandler.getControlServletPath(request);
            if (controlServletPath == null) controlServletPath = "";
            this.productReqPath = controlServletPath + "/" + PRODUCT_REQUEST;
            this.categoryReqPath = controlServletPath + "/" + CATEGORY_REQUEST;
        }

        @Override
        public String encodeURL(String url) {
            String rewrittenUrl = urlWorker.matchReplaceOutboundSeoTranslatableUrl(request, delegator, url, 
                    productReqPath, categoryReqPath, request.getContextPath());
            if (rewrittenUrl != null) {
                return super.encodeURL(rewrittenUrl);
            } else {
                return super.encodeURL(url);
            }
        }

        @Override
        public String encodeRedirectURL(String url) {
            String rewrittenUrl = urlWorker.matchReplaceOutboundSeoTranslatableUrl(request, delegator, url, 
                    productReqPath, categoryReqPath, request.getContextPath());
            if (rewrittenUrl != null) {
                return super.encodeRedirectURL(rewrittenUrl);
            } else {
                return super.encodeRedirectURL(url);
            }
        }

        @Override
        public void sendRedirect(String location) throws IOException {
            String rewrittenLocation = urlWorker.matchReplaceOutboundSeoTranslatableUrl(request, delegator, location, 
                    productReqPath, categoryReqPath, request.getContextPath());
            if (rewrittenLocation != null) {
                super.sendRedirect(rewrittenLocation);
            } else {
                super.sendRedirect(location);
            }
        }
    }

    /**
     * Forward a uri according to forward pattern regular expressions. Note: this is developed for Filter usage.
     * @Deprecated SCIPIO: 2017: redundant with urlrewrite.xml (NOTE: used to be in SeoContextFilter)
     *
     * @param uri String to reverse transform
     * @return String
     */
    @Deprecated
    protected static boolean applySeoConfigRegexpRedirects(HttpServletResponse response, String uri, SeoConfig config) {
        boolean foundMatch = false;
        Integer responseCodeInt = null;

        if (config.checkUseUrlRegexp() && config.getSeoPatterns() != null && config.getForwardReplacements() != null) {
            for(String key : config.getSeoPatterns().keySet()) {
                Pattern pattern = config.getSeoPatterns().get(key);
                String replacement = config.getForwardReplacements().get(key);
                Matcher matcher = pattern.matcher(uri);
                if (matcher.matches()) {
                    for (int i = matcher.groupCount(); i > 0; i--) {
                        replacement = replacement.replaceAll("\\$" + i, matcher.group(i));
                    }
                    uri = replacement;
                    responseCodeInt = config.getForwardResponseCodes().get(key);
                    foundMatch = true;
                    // be careful, we don't break after finding a match
                }
            }
        }

        if (foundMatch) {
            if (responseCodeInt == null) {
                response.setStatus(config.getDefaultRedirectResponseCode());
            } else {
                response.setStatus(responseCodeInt.intValue());
            }
            // SCIPIO: encodeURL?
            response.setHeader("Location", response.encodeRedirectURL(uri));
        } else {
            if (Debug.verboseOn()) Debug.logInfo("Can NOT redirect this url: " + uri, module);
        }

        return foundMatch;
    }
}

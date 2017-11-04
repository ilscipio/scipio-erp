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
import java.util.Iterator;
import java.util.Locale;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.UrlServletHelper;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.product.category.CatalogUrlServlet;
import org.ofbiz.webapp.control.ContextFilter;

import com.ilscipio.scipio.product.seo.SeoCatalogUrlBuilder.SeoCatalogUrlInfo;

/**
 * SCIPIO: 2017: Seo catalog URL filter.
 * <p>
 * Some parts adapted from the original <code>org.ofbiz.product.category.ftl.CatalogUrlSeoTransform</code>.
 * <p>
 * FIXME: this should not extend ContextFilter, is an ofbiz design issue
 * compat workaround, for now ensures delegator is available.
 * WARN: base class will be changed to Filter interface in future!
 */
public class SeoCatalogUrlFilter extends ContextFilter {

    public static final String module = SeoCatalogUrlFilter.class.getName();
    
    public static final String CONTROL_MOUNT_POINT = CatalogUrlServlet.CONTROL_MOUNT_POINT;
    public static final String PRODUCT_REQUEST = CatalogUrlServlet.PRODUCT_REQUEST;
    public static final String CATEGORY_REQUEST = CatalogUrlServlet.CATEGORY_REQUEST;
    public static final String CATALOG_URL_MOUNT_POINT = CatalogUrlServlet.CATALOG_URL_MOUNT_POINT;
    
    public static final String SEOURLINFO_ATTR = "_SCPSEO_URLINFO_";
    public static final String FORWARDED_ATTR = "_SCPSEO_FWDED_";
    public static final String REQWRAPPED_ATTR = "_SCPSEO_REQWRAP_";
    
    static {
        // TODO?: unhardcode via properties?
        SeoCatalogUrlBuilder.registerUrlBuilder();
    }
    
    protected String defaultLocaleString = null;
    protected String redirectUrl = null;
    protected String controlPrefix = null;
    protected String productRequestPath = "/" + PRODUCT_REQUEST;
    protected String categoryRequestPath = "/" + CATEGORY_REQUEST;
    protected boolean seoEnabled = true;
    
    protected static SeoCatalogUrlBuilder urlBuilder = null;

    @Override
    public void init(FilterConfig config) throws ServletException {
        super.init(config);
        
        SeoConfigUtil.init();
        
        seoEnabled = !Boolean.FALSE.equals(UtilMisc.booleanValueVersatile(config.getInitParameter("seoEnabled")));
        if (seoEnabled) {
            String initDefaultLocalesString = config.getInitParameter("defaultLocaleString");
            String initRedirectUrl = config.getInitParameter("redirectUrl");
            defaultLocaleString = UtilValidate.isNotEmpty(initDefaultLocalesString) ? initDefaultLocalesString : "";
            redirectUrl = UtilValidate.isNotEmpty(initRedirectUrl) ? initRedirectUrl : "";
            
            if (UtilValidate.isNotEmpty(CONTROL_MOUNT_POINT)) {
                controlPrefix = "/" + controlPrefix;
            } else {
                controlPrefix = "";
            }

            WebsiteSeoConfig.registerWebsiteForSeo(WebsiteSeoConfig.makeConfig(config.getServletContext(), true));
            
            urlBuilder = SeoCatalogUrlBuilder.getInstance(null, config.getServletContext().getInitParameter("webSiteId"));
        }
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        doFilter((HttpServletRequest) request, (HttpServletResponse) response, chain);
    }

    protected void doFilter(HttpServletRequest request, HttpServletResponse response, FilterChain chain) throws IOException, ServletException {
        
        // TODO: REVIEW:
        //UrlServletHelper.setRequestAttributes(request, delegator, request.getServletContext());
        
        if (seoEnabled) {
            Delegator delegator = getDelegatorForControl(request, request.getServletContext());
            
            if (isRequestApplicable(request, response)) {
                boolean forwarded = matchSeoUrlAndForward(request, response, delegator);
                if (forwarded) return;
            }
            
            // NOTE: For this filter, we must wrap the request/response even if we don't forward!
            // TODO/FIXME: REVIEW: even if disabled for this webapp, may need to wrap responses
            // to process inter-webapp links... so this might need to go outside the (enabled) block...
            // or need 2 disable flags for debugging...
            if (!Boolean.TRUE.equals(request.getAttribute(REQWRAPPED_ATTR))) {
                request.setAttribute(REQWRAPPED_ATTR, Boolean.TRUE);
                chain.doFilter(getRequestWrapper(request, delegator), getResponseWrapper(request, response, delegator));
                return;
            }
        }
        chain.doFilter(request, response);
    }
    
    /**
     * Returns true if not already forwarded (needed for FORWARD dispatcher) and if request itself
     * is not disabled.
     * WARN: does NOT check the servlet seoEnabled/webSiteId flag.
     */
    public static boolean isRequestApplicable(HttpServletRequest request, HttpServletResponse response) {
        return !Boolean.TRUE.equals(request.getAttribute(FORWARDED_ATTR)) && 
                SeoConfigUtil.isCategoryUrlEnabledForContextPath(request.getContextPath());
    }

    public boolean matchSeoUrlAndForward(HttpServletRequest request, HttpServletResponse response, Delegator delegator) throws ServletException, IOException {        
        SeoCatalogUrlInfo urlInfo = matchInboundSeoUrl(request, delegator);
        if (urlInfo != null) {
            boolean res = updateRequestForCatalogSeoUrl(request, delegator, urlInfo);
            if (!res) return false;
            return forwardSeoUrl(request, response, delegator, urlInfo);
        }
        return false;
    }
    
    public static String getMatchablePath(HttpServletRequest request) {
        return request.getServletPath() + request.getPathInfo();
    }
    
    public SeoCatalogUrlInfo matchInboundSeoUrl(HttpServletRequest request, Delegator delegator) {
        return urlBuilder.matchInboundSeoUrl(delegator, getMatchablePath(request), request.getContextPath());
    }
    
    /**
     * Sets the product/category IDs in request and updates trail.
     */
    public static boolean updateRequestForCatalogSeoUrl(HttpServletRequest request, Delegator delegator, SeoCatalogUrlInfo urlInfo) {

        if (urlInfo.isProductRequest()) {
            if (urlInfo.getProductId() != null) {
                request.setAttribute("product_id", urlInfo.getProductId());
                request.setAttribute("productId", urlInfo.getProductId());
                
                if (urlInfo.getCategoryId() != null) {
                    request.setAttribute("productCategoryId", urlInfo.getCategoryId());
                }
            }
        } else { // if (CatalogUrlServlet.CATEGORY_REQUEST.equals(targetRequest)) {
            if (urlInfo.getCategoryId() != null) {
                request.setAttribute("productCategoryId", urlInfo.getCategoryId());
            }
            if (Debug.infoOn()) {
                Debug.logInfo("SEO: [Forwarding request]: " + urlInfo.getPathInfo() 
                    + " (" + urlBuilder + "); args: [productCategoryId: " + urlInfo.getCategoryId() + "]", module);
            }
        }
        
        // TODO: 2017: MISSING TRAIL UPDATES HERE, REQUIRED BY SCREENS AND SHOP
        
        return true;
    }
    
    
    public boolean forwardSeoUrl(HttpServletRequest request, HttpServletResponse response, Delegator delegator, SeoCatalogUrlInfo urlInfo) throws ServletException, IOException {
        StringBuilder fwdUrl = new StringBuilder();
        fwdUrl.append(controlPrefix);
        String targetRequest = urlInfo.isProductRequest() ? productRequestPath : categoryRequestPath;
        fwdUrl.append(targetRequest);
        UrlServletHelper.setViewQueryParameters(request, fwdUrl); // TODO: REVIEW: 2017: looks bad
        
        if (Debug.infoOn()) { // TODO?: verbose or debug flag?
            if (urlInfo.isProductRequest()) {
                if (Debug.infoOn()) {
                    Debug.logInfo("SEO: [Forwarding request]: " + urlInfo.getPathInfo() 
                        + " (" + fwdUrl + "); args: [productId: " + urlInfo.getProductId() 
                        + "; productCategoryId: " + urlInfo.getCategoryId() + "]", module);
                }
            } else {
                if (Debug.infoOn()) {
                    Debug.logInfo("SEO: [Forwarding request]: " + urlInfo.getPathInfo() 
                        + " (" + fwdUrl + "); args: [productCategoryId: " + urlInfo.getCategoryId() + "]", module);
                }
            }
        }
        
        // TODO: REVIEW: this attr may cause problems with all the filters + generic name...
        request.setAttribute("ORIGINAL_REQUEST_URI", request.getRequestURI());
        
        RequestDispatcher rd = request.getRequestDispatcher(fwdUrl.toString());
        request.setAttribute(SEOURLINFO_ATTR, urlInfo);
        request.setAttribute(FORWARDED_ATTR, Boolean.TRUE);
        request.setAttribute(REQWRAPPED_ATTR, Boolean.TRUE);
        rd.forward(getRequestWrapper(request, delegator), getResponseWrapper(request, response, delegator));
        return true;
    }

    /**
     * Checks an outbound URL for /control/product, /control/category or other
     * such request and tries to extract the IDs.
     */
    public SeoCatalogUrlInfo matchOutboundSeoTranslatableUrl(Delegator delegator, String url) {
        
        // TODO
        
        return null;
    }

    private static String rebuildCatalogLink(HttpServletRequest request, Delegator delegator, SeoCatalogUrlInfo urlInfo) {
        Locale locale = UtilHttp.getLocale(request);
        return urlBuilder.makeCatalogLink(delegator, urlInfo, locale);
    }
    
    protected ServletRequest getRequestWrapper(HttpServletRequest req, Delegator delegator) {
        return req;
    }

    protected ServletResponse getResponseWrapper(HttpServletRequest req, HttpServletResponse res, Delegator delegator) {
        return new SeoCatalogUrlResponseWrapper(req, res, delegator);
    }
    
    protected class SeoCatalogUrlResponseWrapper extends HttpServletResponseWrapper {
        private final HttpServletRequest request;
        private final Delegator delegator;
        
        public SeoCatalogUrlResponseWrapper(HttpServletRequest request, HttpServletResponse response, Delegator delegator) {
            super(response);
            this.request = request;
            this.delegator = delegator;
        }
        
        @Override
        public String encodeURL(String url) {
            url = super.encodeURL(url);
            SeoCatalogUrlInfo urlInfo = matchOutboundSeoTranslatableUrl(delegator, url);
            if (urlInfo != null) {
                url = rebuildCatalogLink(request, delegator, urlInfo);
            }
            return url;
        }

        @Override
        public String encodeRedirectURL(String url) {
            url = super.encodeRedirectURL(url);
            SeoCatalogUrlInfo urlInfo = matchOutboundSeoTranslatableUrl(delegator, url);
            if (urlInfo != null) {
                url = rebuildCatalogLink(request, delegator, urlInfo);
            }
            return url;
        }

//        @Override
//        public void sendRedirect(String location) throws IOException {
//            super.sendRedirect(location);
//        }
    }

    /**
     * Special delegator lookup for filters which may run early in a chain; in this case,
     * request.getAttribute("delegator") may return nothing because 
     * ControlFilter/ControlServlet/LoginWorker not yet run.
     * <p>
     * FIXME?: This may be one request late for tenant delegator switches.
     * <p>
     * DEV NOTE: this is copied from CMS. TODO: move to a common util
     * 
     * @param request
     * @param servletContext
     */
    protected static Delegator getDelegatorForControl(HttpServletRequest request, ServletContext servletContext) {
        Delegator delegator = null;
        
        // Check request attribs
        delegator = (Delegator) request.getAttribute("delegator");
        if (delegator != null) {
            return delegator;
        }
        
        // Check session attribs (mainly for tenant delegator) - but don't create session if none yet!
        HttpSession session = request.getSession(false);
        if (session != null) {
            String delegatorName = (String) session.getAttribute("delegatorName");
            if (UtilValidate.isNotEmpty(delegatorName)) {
                delegator = DelegatorFactory.getDelegator(delegatorName);
                if (delegator != null) {
                    return delegator;
                } else {
                    Debug.logWarning("SCIPIO: SEO: ERROR: could not get session delegator for control/filter; " +
                            "delegator factory returned null for session delegatorName \"" + 
                            delegatorName + "\"; defaulting to servlet or default delegator", module);
                }
            }
        }
        
        // Check servlet context
        delegator = (Delegator) servletContext.getAttribute("delegator");
        if (delegator != null) {
            return delegator;
        }
        
        // Last resort: default delegator
        delegator = DelegatorFactory.getDelegator("default");
        
        if (delegator == null) {
            Debug.logError("SCIPIO: SEO: ERROR: could not get any delegator for control/filter!", module);
        }
        return delegator;
    }
    
    /**
     * Forward a uri according to forward pattern regular expressions. Note: this is developed for Filter usage.
     * 
     * @param uri String to reverse transform
     * @return String
     */
    protected static boolean forwardUri(HttpServletResponse response, String uri) {
        boolean foundMatch = false;
        Integer responseCodeInt = null;

        if (SeoConfigUtil.checkUseUrlRegexp() && SeoConfigUtil.getSeoPatterns() != null && SeoConfigUtil.getForwardReplacements() != null) {
            Iterator<String> keys = SeoConfigUtil.getSeoPatterns().keySet().iterator();
            while (keys.hasNext()) {
                String key = keys.next();
                Pattern pattern = SeoConfigUtil.getSeoPatterns().get(key);
                String replacement = SeoConfigUtil.getForwardReplacements().get(key);
                Matcher matcher = pattern.matcher(uri);
                if (matcher.matches()) {
                    for (int i = matcher.groupCount(); i > 0; i--) {
                        replacement = replacement.replaceAll("\\$" + i, matcher.group(i));
                    }
                    uri = replacement;
                    responseCodeInt = SeoConfigUtil.getForwardResponseCodes().get(key);
                    foundMatch = true;
                    // be careful, we don't break after finding a match
                }
            }
        }

        if (foundMatch) {
            if (responseCodeInt == null) {
                response.setStatus(SeoConfigUtil.DEFAULT_RESPONSECODE);
            } else {
                response.setStatus(responseCodeInt.intValue());
            }
            // Buchhandel: encodeURL?
            response.setHeader("Location", response.encodeRedirectURL(uri));
        } else {
            Debug.logInfo("Can NOT forward this url: " + uri, module);
        }
  
        return foundMatch;
    }
}

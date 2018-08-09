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

package org.ofbiz.content.content;

import java.io.IOException;
import java.util.Locale;

import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.UrlServletHelper;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.webapp.control.ContextFilter;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestLinkUtil;
import org.ofbiz.webapp.control.WebAppConfigurationException;

public class ContentUrlFilter extends ContextFilter {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    /**
     * @deprecated SCIPIO: 2017: this was unhardcoded; use {@link org.ofbiz.webapp.control.RequestHandler#getControlServletPath(HttpServletRequest)}.
     */
    @Deprecated
    public static final String CONTROL_MOUNT_POINT = "control";
    protected static String defaultLocaleString = null;
    protected static String redirectUrl = null;
    public static final String defaultViewRequest = "contentViewInfo"; // SCIPIO: final
    private static final String defaultUrlSuffix = "-content"; // SCIPIO: 2018-07-31: new

    // SCIPIO: 2018-07-31: local vars
    private String viewRequest;
    private String urlSuffix;
    
    @Override
    public void init(FilterConfig config) throws ServletException {
        super.init(config);
        
        // SCIPIO: 2018-07-31: put the config viewRequest in a servlet context attrib
        String viewRequest = config.getInitParameter("viewRequest");
        if (UtilValidate.isEmpty(viewRequest)) {
            viewRequest = defaultViewRequest;
        }
        config.getServletContext().setAttribute("scpCufViewRequest", viewRequest);
        this.viewRequest = viewRequest;
        
        // SCIPIO: 2018-07-31: NEW configurable URL suffix
        String urlSuffix = config.getInitParameter("urlSuffix");
        if (UtilValidate.isEmpty(urlSuffix)) {
            urlSuffix = defaultUrlSuffix;
        }
        config.getServletContext().setAttribute("scpCufUrlSuffix", urlSuffix);
        this.urlSuffix = urlSuffix;
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain)  throws IOException, ServletException {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;
        Delegator delegator = (Delegator) httpRequest.getServletContext().getAttribute("delegator"); // SCIPIO: NOTE: no longer need getSession() for getServletContext(), since servlet API 3.0
        
        //Get ServletContext
        ServletContext servletContext = config.getServletContext();

        ContextFilter.setCharacterEncoding(request);

        //Set request attribute and session
        UrlServletHelper.setRequestAttributes(request, delegator, servletContext);
        String urlContentId = null;
        // SCIPIO: 2018-07-30: stock bugfix: prevents having URL parameters
        //String pathInfo = UtilHttp.getFullRequestUrl(httpRequest);
        String pathInfo = httpRequest.getServletPath(); // SCIPIO: NOTE: getServletPath() is what CatalogUrlFilter did, and known to work
        if (UtilValidate.isNotEmpty(pathInfo)) {
            String alternativeUrl = pathInfo.substring(pathInfo.lastIndexOf("/"));
            if (alternativeUrl.endsWith(urlSuffix)) { // SCIPIO: unhardcode view request
                // SCIPIO: strip alt URL suffix
                alternativeUrl = alternativeUrl.substring(0, alternativeUrl.length() - urlSuffix.length());
                try {
                    GenericValue contentDataResourceView = EntityQuery.use(delegator).from("ContentDataResourceView")
                            .where("drObjectInfo", alternativeUrl)
                            .orderBy("createdDate DESC").queryFirst();
                    if (contentDataResourceView != null) {
                        GenericValue content = EntityQuery.use(delegator).from("ContentAssoc")
                                .where("contentAssocTypeId", "ALTERNATIVE_URL", 
                                        "contentIdTo", contentDataResourceView.get("contentId"))
                                .filterByDate().queryFirst();
                        if (content != null) {
                            urlContentId = content.getString("contentId");
                        }
                    }
                } catch (Exception e) {
                    Debug.logWarning(e.getMessage(), module);
                }
            }
            if (UtilValidate.isNotEmpty(urlContentId)) {
                StringBuilder urlBuilder = new StringBuilder();
                urlBuilder.append(RequestHandler.getControlServletPath(request)); // SCIPIO
                urlBuilder.append("/" + viewRequest + "?contentId=" + urlContentId); // SCIPIO: local var for viewRequest
                // SCIPIO: 2018-07-31: fix lost extra parameters 
                // NOTE: this will include the viewIndex, viewSize, etc. parameters that are processed again below, 
                // but this is unlikely to cause an issue...
                String queryString = httpRequest.getQueryString();
                if (queryString != null) {
                    urlBuilder.append("&");
                    urlBuilder.append(queryString);
                }
                
                ContextFilter.setAttributesFromRequestBody(request);
                //Set view query parameters
                UrlServletHelper.setViewQueryParameters(request, urlBuilder);
                //Debug.logInfo("[Filtered request]: " + pathInfo + " (" + urlBuilder + ")", module); // SCIPIO: 2018-07-31: makes no sense
                RequestDispatcher dispatch = request.getRequestDispatcher(urlBuilder.toString());
                dispatch.forward(request, response);
                return;
            }
            
            //Check path alias
            UrlServletHelper.checkPathAlias(request, httpResponse, delegator, pathInfo);
        }
        // we're done checking; continue on
        chain.doFilter(request, response);
    }
    
    /**
     * Builds an alt content URL.
     * <p>
     * SCIPIO: added a urlDecode boolean and changed the default behavior to NOT url-decode (FALSE);
     * it should be done before storing in the database - if/when needed.
     */
    public static String makeContentAltUrl(HttpServletRequest request, HttpServletResponse response, String contentId, String viewContent, Boolean urlDecode) {
        if (UtilValidate.isEmpty(contentId)) {
            return null;
        }
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        String url = null;
        try {
            GenericValue contentAssocDataResource = EntityQuery.use(delegator)
                    .select("contentIdStart", "drObjectInfo", "dataResourceId", "caFromDate", "caThruDate", "caCreatedDate")
                    .from("ContentAssocDataResourceViewTo")
                    .where("caContentAssocTypeId", "ALTERNATIVE_URL",
                            "caThruDate", null,
                            "contentIdStart", contentId)
                    .orderBy("-caFromDate")
                    .queryFirst();
            if (contentAssocDataResource != null) {
                url = contentAssocDataResource.getString("drObjectInfo");
                
                // SCIPIO: by default, don't url-decode here anymore.
                if (Boolean.TRUE.equals(urlDecode)) {
                    url = UtilCodec.getUrlDecoder().decode(url);
                }

                if (url != null && url.length() > 0) { // SCIPIO: stock bugfix: important null check, otherwise mount-point leads to issue below!
                    // SCIPIO: 2018-07-31: stock bugfix: detect missing slash
                    if (url.charAt(0) != '/') {
                        url = "/" + url;
                    }
                    // SCIPIO: 2018-07-31: stock bugfix: missing URL suffix append
                    String urlSuffix = (String) request.getServletContext().getAttribute("scpCufUrlSuffix");
                    url += (urlSuffix != null) ? urlSuffix : defaultUrlSuffix;

                    String mountPoint = request.getContextPath();
                    // SCIPIO: redundant
                    //if (!(mountPoint.equals("/")) && !(mountPoint.equals(""))) {
                    //    url = mountPoint + url;
                    //}
                    url = mountPoint + url;
                }
            }
        } catch (Exception e) {
            Debug.logWarning("[Exception] : " + e.getMessage(), module);
        }
         
        if (UtilValidate.isEmpty(url)) {
            // SCIPIO: 2018-07-31: useless, already contains a fallback
            //if (UtilValidate.isEmpty(viewContent)) {
            //    viewContent = defaultViewRequest;
            //}
            url = makeContentUrl(request, response, contentId, viewContent);
        }
        return url;
    }
    
    public static String makeContentAltUrl(HttpServletRequest request, HttpServletResponse response, String contentId, String viewContent) {
        return makeContentAltUrl(request, response, contentId, viewContent, null);
    }
    
    public static String makeContentUrl(HttpServletRequest request, HttpServletResponse response, String contentId, String viewContent) {
        if (UtilValidate.isEmpty(contentId)) {
            return null;
        }
        StringBuilder urlBuilder = new StringBuilder();

        // SCIPIO: 2018-07-31: don't go through session or servlet context for this
        //urlBuilder.append(request.getServletContext().getContextPath()); // SCIPIO: NOTE: no longer need getSession() for getServletContext(), since servlet API 3.0
        urlBuilder.append(request.getContextPath());

        // SCIPIO: 2018-07-31: this doubles the slash
        //if (urlBuilder.length() == 0 || urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
        //    urlBuilder.append("/");
        //}
        urlBuilder.append(RequestHandler.getControlServletPath(request)); // SCIPIO

        if (UtilValidate.isNotEmpty(viewContent)) {
            urlBuilder.append("/" + viewContent);
        } else {
            // SCIPIO: 2018-07-31: use the one from config
            //urlBuilder.append("/" + defaultViewRequest);
            String viewRequest = (String) request.getServletContext().getAttribute("scpCufViewRequest");
            urlBuilder.append("/" + (viewRequest != null ? viewRequest : defaultViewRequest));
        }
        urlBuilder.append("?contentId=" + contentId);
        return urlBuilder.toString();
    }
    
    /**
     * SCIPIO: High-level alt content link-building method, including request encoding.
     * Added 2018-07-30.
     */
    public static String makeContentAltLink(HttpServletRequest request, HttpServletResponse response, Locale locale, String contentId, String viewContent, Boolean urlDecode,
            Object params, Boolean fullPath, Boolean secure, Boolean encode) throws WebAppConfigurationException, IOException {
        String url = makeContentAltUrl(request, response, contentId, viewContent, urlDecode);
        if (url == null || url.isEmpty()) return null;
        url = appendLinkParams(url, params);
        return RequestLinkUtil.buildLinkHostPartAndEncode(request, response, locale, null, url, fullPath, secure, encode, true);
    }
    
    /**
     * SCIPIO: Appends params for catalog URLs.
     * TODO: refactor
     * Added 2018-07-31.
     */
    private static String appendLinkParams(String url, Object paramsObj) throws IOException {
        if (paramsObj == null) {
            return url;
        }
        String params = paramsObj.toString();
        if (params.isEmpty()) {
            return url;
        }
        if (url.contains("?")) {
            if (params.startsWith("?")) {
                url += "&" + params.substring(1);
            } else {
                url += "&" + params;
            }
        } else {
            if (params.startsWith("?")) {
                url += params;
            } else {
                url += "?" + params;
            }
        }
        return url;
    }
}

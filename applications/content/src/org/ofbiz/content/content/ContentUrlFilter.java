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

import javax.servlet.FilterChain;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.UrlServletHelper;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.webapp.control.ContextFilter;
import org.ofbiz.webapp.control.RequestHandler;

public class ContentUrlFilter extends ContextFilter {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    /**
     * @deprecated SCIPIO: 2017: this was unhardcoded; use {@link org.ofbiz.webapp.control.RequestHandler#getControlServletPath(HttpServletRequest)}.
     */
    @Deprecated
    public static final String CONTROL_MOUNT_POINT = "control";
    protected static String defaultLocaleString = null;
    protected static String redirectUrl = null;
    public static String defaultViewRequest = "contentViewInfo";
    
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
        String pathInfo = UtilHttp.getFullRequestUrl(httpRequest);
        if (UtilValidate.isNotEmpty(pathInfo)) {
            String alternativeUrl = pathInfo.substring(pathInfo.lastIndexOf("/"));
            if (alternativeUrl.endsWith("-content")) {
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
                urlBuilder.append("/" + config.getInitParameter("viewRequest") + "?contentId=" + urlContentId);

                ContextFilter.setAttributesFromRequestBody(request);
                //Set view query parameters
                UrlServletHelper.setViewQueryParameters(request, urlBuilder);
                Debug.logInfo("[Filtered request]: " + pathInfo + " (" + urlBuilder + ")", module);
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
                
                String mountPoint = request.getContextPath();
                if (!(mountPoint.equals("/")) && !(mountPoint.equals(""))) {
                    url = mountPoint + url;
                }
            }
        } catch (Exception e) {
            Debug.logWarning("[Exception] : " + e.getMessage(), module);
        }
         
        if (UtilValidate.isEmpty(url)) {
            if (UtilValidate.isEmpty(viewContent)) {
                viewContent = defaultViewRequest;
            }
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
        urlBuilder.append(request.getServletContext().getContextPath()); // SCIPIO: NOTE: no longer need getSession() for getServletContext(), since servlet API 3.0
        if (urlBuilder.length() == 0 || urlBuilder.charAt(urlBuilder.length() - 1) != '/') {
            urlBuilder.append("/");
        }
        urlBuilder.append(RequestHandler.getControlServletPath(request)); // SCIPIO
        
        if (UtilValidate.isNotEmpty(viewContent)) {
            urlBuilder.append("/" + viewContent);
        } else {
            urlBuilder.append("/" + defaultViewRequest);
        }
        urlBuilder.append("?contentId=" + contentId);
        return urlBuilder.toString();
    }
}

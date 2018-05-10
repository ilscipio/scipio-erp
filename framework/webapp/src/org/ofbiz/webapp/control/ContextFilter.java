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
package org.ofbiz.webapp.control;

import static org.ofbiz.base.util.UtilGenerics.checkMap;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.tomcat.util.descriptor.web.FilterDef;
import org.apache.tomcat.util.descriptor.web.WebXml;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilObject;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.security.Security;
import org.ofbiz.security.SecurityConfigurationException;
import org.ofbiz.security.SecurityFactory;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceContainer;
import org.ofbiz.webapp.event.RequestBodyMapHandlerFactory;
import org.ofbiz.webapp.website.WebSiteWorker;

/**
 * ContextFilter - Restricts access to raw files and configures servlet objects.
 */
public class ContextFilter implements Filter {

    public static final String module = ContextFilter.class.getName();
    private static final String contextFilterClassName = ContextFilter.class.getSimpleName();
    public static final String FORWARDED_FROM_SERVLET = "_FORWARDED_FROM_SERVLET_";

    protected FilterConfig config = null;
    protected boolean debug = false;
    protected Set<String> allowedPaths = null; // SCIPIO: new: prevent parsing at every request
    protected boolean forwardRootControllerUris = false; // SCIPIO: new

    // default charset used to decode requests body data if no encoding is specified in the request
    private String defaultCharacterEncoding;
    private boolean isMultitenant;

    /**
     * @see javax.servlet.Filter#init(javax.servlet.FilterConfig)
     */
    public void init(FilterConfig config) throws ServletException {
        this.config = config;

        // puts all init-parameters in ServletContext attributes for easier parametrization without code changes
        this.putAllInitParametersInAttributes();

        // set debug
        this.debug = "true".equalsIgnoreCase(config.getInitParameter("debug"));
        if (!debug) {
            debug = Debug.verboseOn();
        }

        defaultCharacterEncoding = config.getServletContext().getInitParameter("charset");
        if (UtilValidate.isEmpty(defaultCharacterEncoding)) {
            defaultCharacterEncoding = "UTF-8";
        }
        // check the serverId
        getServerId();
        // initialize the delegator
        getDelegator(config.getServletContext());
        // initialize security
        getSecurity();
        // initialize the services dispatcher
        getDispatcher(config.getServletContext());

        // check if multi tenant is enabled
        isMultitenant = EntityUtil.isMultiTenantEnabled();

        // this will speed up the initial sessionId generation
        new java.security.SecureRandom().nextLong();
        
        // SCIPIO: 2017-11-14: now pre-parsing allowedPath during init
        String allowedPath = config.getInitParameter("allowedPaths");
        List<String> allowList = null;
        if ((allowList = StringUtil.split(allowedPath, ":")) != null) {
            allowList.add("/");  // No path is allowed.
            allowList.add("");   // No path is allowed.
        }
        this.allowedPaths = (allowList != null) ? new HashSet<>(allowList) : null;
        
        // SCIPIO: new
        this.forwardRootControllerUris = getForwardRootControllerUrisSetting(ServletUtil.getInitParamsMapAdapter(config), false);
    }

    /**
     * @see javax.servlet.Filter#doFilter(javax.servlet.ServletRequest, javax.servlet.ServletResponse, javax.servlet.FilterChain)
     */
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {
        HttpServletRequest httpRequest = (HttpServletRequest) request;
        HttpServletResponse httpResponse = (HttpServletResponse) response;

        // Debug.logInfo("Running ContextFilter.doFilter", module);

        // SCIPIO: 2017: new special forwarding mode for root controller URI requests
        // NOTE: this requires that ContextFilter responds to FORWARD dispatcher, otherwise multitenant will not be init
        // FIXME: 2017-11: This setting currently can't auto-detect if a request URI is already in use by a servlet mapping
        String controlServletPath = RequestHandler.getControlServletPath(httpRequest);
        if (forwardRootControllerUris && controlServletPath != null && controlServletPath.length() > 1) {
            // previous filter may request custom forwards using _SCP_FWDROOTURIS_
            @SuppressWarnings("unchecked")
            Set<String> customRootRedirects = (Set<String>) request.getAttribute("_SCP_FWDROOTURIS_");
            Map<String, ?> reqUris = getControllerRequestUriMap(httpRequest);
            String servletAndPathInfo = RequestLinkUtil.getServletAndPathInfo(httpRequest);
            String firstPathElem = RequestLinkUtil.getFirstPathElem(servletAndPathInfo);
            if (reqUris.containsKey(firstPathElem) || (customRootRedirects != null && customRootRedirects.contains(firstPathElem))) {
                RequestDispatcher rd = request.getRequestDispatcher(controlServletPath + servletAndPathInfo);
                rd.forward(request, response);
                return;
            }
        }
        
        // ----- Servlet Object Setup -----

        // set the ServletContext in the request for future use
        httpRequest.setAttribute("servletContext", config.getServletContext());

        // set the webSiteId in the session
        if (UtilValidate.isEmpty(httpRequest.getSession().getAttribute("webSiteId"))){
            httpRequest.getSession().setAttribute("webSiteId", WebSiteWorker.getWebSiteId(httpRequest));
        }

        // set the filesystem path of context root.
        httpRequest.setAttribute("_CONTEXT_ROOT_", config.getServletContext().getRealPath("/"));

        // set the server root url
        httpRequest.setAttribute("_SERVER_ROOT_URL_", UtilHttp.getServerRootUrl(httpRequest));

        // request attributes from redirect call
        String reqAttrMapHex = (String) httpRequest.getSession().getAttribute("_REQ_ATTR_MAP_");
        if (UtilValidate.isNotEmpty(reqAttrMapHex)) {
            byte[] reqAttrMapBytes = StringUtil.fromHexString(reqAttrMapHex);
            Map<String, Object> reqAttrMap = checkMap(UtilObject.getObject(reqAttrMapBytes), String.class, Object.class);
            if (reqAttrMap != null) {
                for (Map.Entry<String, Object> entry: reqAttrMap.entrySet()) {
                    httpRequest.setAttribute(entry.getKey(), entry.getValue());
                }
            }
            httpRequest.getSession().removeAttribute("_REQ_ATTR_MAP_");
        }
        
        // ----- Context Security -----
        // check if we are disabled
        String disableSecurity = config.getInitParameter("disableContextSecurity");
        if (disableSecurity != null && "Y".equalsIgnoreCase(disableSecurity)) {
            chain.doFilter(httpRequest, httpResponse);
            return;
        }

        // check if we are told to redirect everthing
        String redirectAllTo = config.getInitParameter("forceRedirectAll");
        if (UtilValidate.isNotEmpty(redirectAllTo)) {
            // little trick here so we don't loop on ourself
            if (httpRequest.getSession().getAttribute("_FORCE_REDIRECT_") == null) {
                httpRequest.getSession().setAttribute("_FORCE_REDIRECT_", "true");
                Debug.logWarning("Redirecting user to: " + redirectAllTo, module);

                if (!redirectAllTo.toLowerCase().startsWith("http")) {
                    redirectAllTo = httpRequest.getContextPath() + redirectAllTo;
                }
                encodeAndSendRedirectURL(httpResponse, redirectAllTo); // SCIPIO
                return;
            } else {
                httpRequest.getSession().removeAttribute("_FORCE_REDIRECT_");
                chain.doFilter(httpRequest, httpResponse);
                return;
            }
        }
        
        // test to see if we have come through the control servlet already, if not do the processing
        String requestPath = null;
        String contextUri = null;
        if (httpRequest.getAttribute(ContextFilter.FORWARDED_FROM_SERVLET) == null) {
            // Debug.logInfo("In ContextFilter.doFilter, FORWARDED_FROM_SERVLET is NOT set", module);
            //String allowedPath = config.getInitParameter("allowedPaths"); // SCIPIO: see init
            String redirectPath = config.getInitParameter("redirectPath");
            String errorCode = config.getInitParameter("errorCode");

            // SCIPIO: 2017-11: now done in initialization - no reason to do this every request
//            List<String> allowList = null;
//            if ((allowList = StringUtil.split(allowedPath, ":")) != null) {
//                allowList.add("/");  // No path is allowed.
//                allowList.add("");   // No path is allowed.
//            }
            // SCIPIO: 2017-11: allowList should always have been a Set, not a List
            Set<String> allowList = this.allowedPaths;
            
            // SCIPIO: 2017-11: SPECIAL: auto-detect when ControlServlet is mapped to root (controlServletPath empty) and allow its requests
            Map<String, ?> allowRootList = Collections.emptyMap();
            if (UtilValidate.isEmpty(httpRequest.getServletPath()) && controlServletPath != null && controlServletPath.isEmpty()) {
                allowRootList = getControllerRequestUriMap(httpRequest);
            }

            if (debug) Debug.logInfo("[Domain]: " + httpRequest.getServerName() + " [Request]: " + httpRequest.getRequestURI(), module);

            requestPath = httpRequest.getServletPath();
            if (requestPath == null) requestPath = "";
            if (requestPath.lastIndexOf("/") > 0) {
                if (requestPath.indexOf("/") == 0) {
                    requestPath = "/" + requestPath.substring(1, requestPath.indexOf("/", 1));
                } else {
                    requestPath = requestPath.substring(1, requestPath.indexOf("/"));
                }
            }

            String requestInfo = httpRequest.getServletPath();
            if (requestInfo == null) requestInfo = "";
            if (requestInfo.lastIndexOf("/") >= 0) {
                requestInfo = requestInfo.substring(0, requestInfo.lastIndexOf("/")) + "/*";
            }

            StringBuilder contextUriBuffer = new StringBuilder();
            if (httpRequest.getContextPath() != null) {
                contextUriBuffer.append(httpRequest.getContextPath());
            }
            if (httpRequest.getServletPath() != null) {
                contextUriBuffer.append(httpRequest.getServletPath());
            }
            if (httpRequest.getPathInfo() != null) {
                contextUriBuffer.append(httpRequest.getPathInfo());
            }
            contextUri = contextUriBuffer.toString();

            // Verbose Debugging
            if (Debug.verboseOn()) {
                if (allowList != null) {
                    for (String allow: allowList) {
                        Debug.logVerbose("[Allow]: " + allow, module);
                    }
                }
                Debug.logVerbose("[Request path]: " + requestPath, module);
                Debug.logVerbose("[Request info]: " + requestInfo, module);
                Debug.logVerbose("[Servlet path]: " + httpRequest.getServletPath(), module);
            }

            // check to make sure the requested url is allowed
            if (allowList != null &&
                (!allowList.contains(requestPath) && !allowList.contains(requestInfo) && !allowList.contains(httpRequest.getServletPath())) &&
                (!allowRootList.containsKey(RequestLinkUtil.getFirstPathInfoElem(httpRequest))) // SCIPIO: new 2017-11-14: allow root control requests
                ) {
                String filterMessage = "[Filtered request]: " + contextUri;
                
                if (redirectPath == null) {
                    int error = 404;
                    if (UtilValidate.isNotEmpty(errorCode)) {
                        try {
                            error = Integer.parseInt(errorCode);
                        } catch (NumberFormatException nfe) {
                            Debug.logWarning(nfe, "Error code specified would not parse to Integer : " + errorCode, module);
                        }
                    }
                    filterMessage = filterMessage + " (" + error + ")";
                    // SCIPIO: 2018-04: here the message will be public-facing, so encode the URI for urlrewriting
                    //String reasonUri = contextUri;
                    String reasonUri = UtilValidate.isNotEmpty(contextUri) ? httpResponse.encodeURL(contextUri) : contextUri;
                    httpResponse.sendError(error, reasonUri);
                    request.setAttribute("filterRequestUriError", reasonUri);
                } else {
                    filterMessage = filterMessage + " (" + redirectPath + ")";
                    if (!redirectPath.toLowerCase().startsWith("http")) {
                        redirectPath = httpRequest.getContextPath() + redirectPath;
                    }
                    encodeAndSendRedirectURL(httpResponse, redirectPath); // SCIPIO
                }
                Debug.logWarning(filterMessage, module);
                return;
            }
        }

        if (request.getCharacterEncoding() == null) {
            request.setCharacterEncoding(defaultCharacterEncoding);
        }

        setAttributesFromRequestBody(request);

        request.setAttribute("delegator", config.getServletContext().getAttribute("delegator"));
        request.setAttribute("dispatcher", config.getServletContext().getAttribute("dispatcher"));
        request.setAttribute("security", config.getServletContext().getAttribute("security"));

        if (isMultitenant) {
            // get tenant delegator by domain name
            String serverName = httpRequest.getServerName();
            try {
                // if tenant was specified, replace delegator with the new per-tenant delegator and set tenantId to session attribute
                Delegator delegator = getDelegator(config.getServletContext());

                //Use base delegator for fetching data from entity of entityGroup org.ofbiz.tenant 
                Delegator baseDelegator = DelegatorFactory.getDelegator(delegator.getDelegatorBaseName());
                GenericValue tenantDomainName = EntityQuery.use(baseDelegator).from("TenantDomainName").where("domainName", serverName).queryOne();
                String tenantId = null;
                if(UtilValidate.isNotEmpty(tenantDomainName)) {
                    tenantId = tenantDomainName.getString("tenantId");
                }
                
                if(UtilValidate.isEmpty(tenantId)) {
                    tenantId = (String) httpRequest.getAttribute("userTenantId");
                }
                if(UtilValidate.isEmpty(tenantId)) {
                    tenantId = httpRequest.getParameter("userTenantId");
                }
                if (UtilValidate.isNotEmpty(tenantId)) {
                    // if the request path is a root mount then redirect to the initial path
                    if ("".equals(httpRequest.getContextPath()) && "".equals(httpRequest.getServletPath())) {
                        GenericValue tenant = EntityQuery.use(baseDelegator).from("Tenant").where("tenantId", tenantId).queryOne();
                        String initialPath = tenant.getString("initialPath");
                        if (UtilValidate.isNotEmpty(initialPath) && !"/".equals(initialPath)) {
                            encodeAndSendRedirectURL(httpResponse, initialPath); // SCIPIO
                            return;
                        }
                    }

                    // make that tenant active, setup a new delegator and a new dispatcher
                    String tenantDelegatorName = delegator.getDelegatorBaseName() + "#" + tenantId;
                    httpRequest.getSession().setAttribute("delegatorName", tenantDelegatorName);

                    // after this line the delegator is replaced with the new per-tenant delegator
                    delegator = DelegatorFactory.getDelegator(tenantDelegatorName);
                    config.getServletContext().setAttribute("delegator", delegator);

                    // clear web context objects
                    config.getServletContext().setAttribute("security", null);
                    config.getServletContext().setAttribute("dispatcher", null);

                    // initialize security
                    Security security = getSecurity();
                    // initialize the services dispatcher
                    LocalDispatcher dispatcher = getDispatcher(config.getServletContext());

                    // set web context objects
                    request.setAttribute("delegator", delegator);
                    request.setAttribute("dispatcher", dispatcher);
                    request.setAttribute("security", security);
                    
                    request.setAttribute("userTenantId", tenantId);
                }

                // NOTE DEJ20101130: do NOT always put the delegator name in the user's session because the user may 
                // have logged in and specified a tenant, and even if no Tenant record with a matching domainName field 
                // is found this will change the user's delegator back to the base one instead of the one for the 
                // tenant specified on login 
                // httpRequest.getSession().setAttribute("delegatorName", delegator.getDelegatorName());
            } catch (GenericEntityException e) {
                Debug.logWarning(e, "Unable to get Tenant", module);
            }
        }

        // we're done checking; continue on
        chain.doFilter(request, httpResponse);
    }

    /**
     * @see javax.servlet.Filter#destroy()
     */
    public void destroy() {
        getDispatcher(config.getServletContext()).deregister();
        config = null;
    }

    protected static LocalDispatcher getDispatcher(ServletContext servletContext) {
        LocalDispatcher dispatcher = (LocalDispatcher) servletContext.getAttribute("dispatcher");
        if (dispatcher == null) {
            Delegator delegator = getDelegator(servletContext);
            dispatcher = makeWebappDispatcher(servletContext, delegator);
            servletContext.setAttribute("dispatcher", dispatcher);
        }
        return dispatcher;
    }

    public static void setCharacterEncoding(ServletRequest request) throws UnsupportedEncodingException {
        String charset = request.getServletContext().getInitParameter("charset");
        if (UtilValidate.isEmpty(charset)) charset = request.getCharacterEncoding();
        if (UtilValidate.isEmpty(charset)) charset = "UTF-8";
        if (Debug.verboseOn()) Debug.logVerbose("The character encoding of the request is: [" + request.getCharacterEncoding() + "]. The character encoding we will use for the request is: [" + charset + "]", module);

        if (!"none".equals(charset)) {
            request.setCharacterEncoding(charset);
        }
    }

    public static void setAttributesFromRequestBody(ServletRequest request) {
        // read the body (for JSON requests) and set the parameters as attributes:
        Map<String, Object> requestBodyMap = null;
        try {
            requestBodyMap = RequestBodyMapHandlerFactory.extractMapFromRequestBody(request);
        } catch (IOException ioe) {
            Debug.logWarning(ioe, module);
        }
        if (requestBodyMap != null) {
            Set<String> parameterNames = requestBodyMap.keySet();
            for (String parameterName: parameterNames) {
                request.setAttribute(parameterName, requestBodyMap.get(parameterName));
            }
        }
    }

    /** This method only sets up a dispatcher for the current webapp and passed in delegator, it does not save it to the ServletContext or anywhere else, just returns it */
    public static LocalDispatcher makeWebappDispatcher(ServletContext servletContext, Delegator delegator) {
        if (delegator == null) {
            Debug.logError("[ContextFilter.init] ERROR: delegator not defined.", module);
            return null;
        }
        // get the unique name of this dispatcher
        String dispatcherName = servletContext.getInitParameter("localDispatcherName");

        if (dispatcherName == null) {
            Debug.logError("No localDispatcherName specified in the web.xml file", module);
            dispatcherName = delegator.getDelegatorName();
        }

        LocalDispatcher dispatcher = ServiceContainer.getLocalDispatcher(dispatcherName, delegator);
        if (dispatcher == null) {
            Debug.logError("[ContextFilter.init] ERROR: dispatcher could not be initialized.", module);
        }

        return dispatcher;
    }

    protected static Delegator getDelegator(ServletContext servletContext) {
        Delegator delegator = (Delegator) servletContext.getAttribute("delegator");
        if (delegator == null) {
            String delegatorName = servletContext.getInitParameter("entityDelegatorName");

            if (delegatorName == null || delegatorName.length() <= 0) {
                delegatorName = "default";
            }
            if (Debug.verboseOn()) Debug.logVerbose("Setup Entity Engine Delegator with name " + delegatorName, module);
            delegator = DelegatorFactory.getDelegator(delegatorName);
            servletContext.setAttribute("delegator", delegator);
            if (delegator == null) {
                Debug.logError("[ContextFilter.init] ERROR: delegator factory returned null for delegatorName \"" + delegatorName + "\"", module);
            }
        }
        return delegator;
    }

    protected Security getSecurity() {
        Security security = (Security) config.getServletContext().getAttribute("security");
        if (security == null) {
            Delegator delegator = (Delegator) config.getServletContext().getAttribute("delegator");

            if (delegator != null) {
                try {
                    security = SecurityFactory.getInstance(delegator);
                } catch (SecurityConfigurationException e) {
                    Debug.logError(e, "Unable to obtain an instance of the security object.", module);
                }
            }
            config.getServletContext().setAttribute("security", security);
            if (security == null) {
                Debug.logError("An invalid (null) Security object has been set in the servlet context.", module);
            }
        }
        return security;
    }

    protected void putAllInitParametersInAttributes() {
        Enumeration<String> initParamEnum = UtilGenerics.cast(config.getServletContext().getInitParameterNames());
        while (initParamEnum.hasMoreElements()) {
            String initParamName = initParamEnum.nextElement();
            String initParamValue = config.getServletContext().getInitParameter(initParamName);
            if (Debug.verboseOn()) Debug.logVerbose("Adding web.xml context-param to application attribute with name [" + initParamName + "] and value [" + initParamValue + "]", module);
            config.getServletContext().setAttribute(initParamName, initParamValue);
        }
        String GeronimoMultiOfbizInstances = (String) config.getServletContext().getAttribute("GeronimoMultiOfbizInstances");
        if (UtilValidate.isNotEmpty(GeronimoMultiOfbizInstances)) {
            String ofbizHome = System.getProperty("ofbiz.home");
            if (GeronimoMultiOfbizInstances.equalsIgnoreCase("true") && UtilValidate.isEmpty(ofbizHome)) {
                ofbizHome = System.getProperty("ofbiz.home"); // This is only used in case of Geronimo or WASCE using OFBiz multi-instances. It allows to retrieve ofbiz.home value set in JVM env
                System.out.println("Set OFBIZ_HOME to - " + ofbizHome);
                System.setProperty("ofbiz.home", ofbizHome);
            }
        }
    }

    protected String getServerId() {
        String serverId = (String) config.getServletContext().getAttribute("_serverId");
        if (serverId == null) {
            serverId = config.getServletContext().getInitParameter("ofbizServerName");
            config.getServletContext().setAttribute("_serverId", serverId);
        }
        return serverId;
    }
    
    /**
     * SCIPIO: local redirect URL encode method for ContextFilter redirects (only).
     * TODO: REVIEW: method used
     * Added 2017-11-03.
     */
    protected String encodeRedirectURL(HttpServletResponse response, String url) {
        // SCIPIO: TODO: REVIEW: It's possible this should be encodeURL + strip jsessionid instead (even if redirect)...
        return response.encodeRedirectURL(url);
    }
    
    /**
     * SCIPIO: local redirect URL encode + send redirect for ContextFilter redirects (only).
     * Added 2017-11-03.
     * @throws IOException 
     */
    protected void encodeAndSendRedirectURL(HttpServletResponse response, String url) throws IOException {
        response.sendRedirect(encodeRedirectURL(response, url));
    }
    
    protected Map<String, ConfigXMLReader.RequestMap> getControllerRequestUriMap(HttpServletRequest request) throws ServletException {
        try {
            RequestHandler rh = RequestHandler.getRequestHandler(request);
            return rh.getControllerConfig().getRequestMapMap();
        } catch (Exception e) {
            Debug.logError(e, "Error reading request names from controller.xml: " + e.getMessage(), module);
            throw new ServletException(e);
        }
    }
    
    /**
     * SCIPIO: Reads the forwardRootControllerUris init-param as Boolean.
     */
    public static Boolean getForwardRootControllerUrisSetting(Map<String, ?> initParams, Boolean defaultValue) {
        return UtilMisc.booleanValueVersatile(initParams.get("forwardRootControllerUris"), defaultValue);
    }
    
    /**
     * SCIPIO: Reads the forwardRootControllerUris value from ContextFilter init-params from webXml,
     * trying to find the most appropriate setting.
     */
    public static Boolean readForwardRootControllerUrisSetting(WebXml webXml, String logPrefix) {
        // HEURISTIC: we return the value of the highest-rated ContextFilter that
        // has forwardRootControllerUris present in its init-params (even if empty)
        
        int bestRating = 0;
        String aliasStr = null;
        String filterName = null;
        for(FilterDef filter : webXml.getFilters().values()) {
            int currentRating = rateContextFilterCandidate(filter);
            if (currentRating > 0) {
                Map<String, String> initParams = filter.getParameterMap();
                if (initParams != null && initParams.containsKey("forwardRootControllerUris")) {
                    if (currentRating > bestRating) {
                        bestRating = currentRating;
                        aliasStr = initParams.get("forwardRootControllerUris");
                        filterName = filter.getFilterName();
                    }
                }
            }
        }
        if (bestRating > 0) {
            Boolean alias = UtilMisc.booleanValueVersatile(aliasStr);
            if (alias != null) {
                if (logPrefix != null) Debug.logInfo(logPrefix+"Found web.xml ContextFilter (filter name '" + filterName + "') init-param forwardRootControllerUris boolean value: " + alias, module);
                return alias;
            } else {
                if (UtilValidate.isNotEmpty(aliasStr)) {
                    if (logPrefix != null) Debug.logError(logPrefix+"web.xml ContextFilter (filter name '" + filterName + "') init-param forwardRootControllerUris has invalid boolean value: " + aliasStr, module);
                } else {
                    if (logPrefix != null) Debug.logInfo(logPrefix+"Found web.xml ContextFilter (filter name '" + filterName + "') init-param forwardRootControllerUris, was empty; returning as unset", module);
                    return null;
                }
            }
        } else {
            if (logPrefix != null) Debug.logInfo(logPrefix+"web.xml ContextFilter init-param forwardRootControllerUris setting not found", module);
        }
        return null;
    }
 
    /**
     * SCIPIO: Verifies if the setting returned by {@link #readForwardRootControllerUrisSetting} is
     * sane for the given controlServletMapping (also works for controlServletPath).
     */
    public static Boolean verifyForwardRootControllerUrisSetting(Boolean hasContextFilterFlag, String controlServletMapping, String logPrefix) {
        if (hasContextFilterFlag != null) {
            if (hasContextFilterFlag) {
                if (controlServletMapping != null && controlServletMapping.startsWith("/") && controlServletMapping.length() > 1) {
                    // aliasing is enabled and controlPath is not the root, so pass
                    if (logPrefix != null) Debug.logInfo(logPrefix+"web.xml appears to have ContextFilter init-param forwardRootControllerUris enabled"
                            + " and a non-root control servlet mapping (" + controlServletMapping 
                            + "); now treating website as having enabled root aliasing/forwarding/rewriting of controller URIs", module);
                    return true;
                } else {
                    if (logPrefix != null) Debug.logWarning(logPrefix+"web.xml invalid configuration: ContextFilter init-param forwardRootControllerUris is enabled"
                            + ", but control servlet mapping (" + controlServletMapping 
                            + ") appears it may be mapped to root - invalid configuration", module);
                }
            }
        }
        return null;
    }
    
    /**
     * Heuristic for finding the most probable real ContextFilter.
     * NOTE: This is a problem because OFbiz frustratingly made a bunch of other classes
     * extend ContextFilter, and even scipio is forced to compound the problem as a result
     * of lack of good base classes.
     * @return a value between 0-5, 5 being best candidate
     */
    public static int rateContextFilterCandidate(FilterDef filter) {
        String filterClass = filter.getFilterClass();
        if (filterClass == null || filterClass.isEmpty()) return 0;
        
        // NOTE: this exact-class check is what stock code does for some other classes, so we have to follow suit
        if (ContextFilter.class.getName().equals(filterClass)) return 5;
        
        try {
            Class<?> filterCls = Thread.currentThread().getContextClassLoader().loadClass(filterClass);
            if (!ContextFilter.class.isAssignableFrom(filterCls)) return 0;
        } catch(Exception e) {
            Debug.logWarning("Could not load or test filter class (" + filterClass + "); may be invalid or a classloader issue: " 
                    + e.getMessage(), module);
            return 0;
        }
        
        String filterName = filter.getFilterName();
        
        if (contextFilterClassName.equals(filterName)) return 4;
        
        if (filterName != null && filterName.contains(contextFilterClassName)) {
            if (filterClass.contains(contextFilterClassName)) return 3;
        } else {
            if (filterClass.contains(contextFilterClassName)) return 2;
        }
        // 1: at least is subclass, but lowest because stock Ofbiz overextended ContextFilter everywhere
        return 1;
    }
}

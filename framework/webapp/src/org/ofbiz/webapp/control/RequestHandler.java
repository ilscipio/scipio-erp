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
import java.io.Serializable;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.cert.X509Certificate;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.start.Start;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.SSLUtil;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilCodec;
import org.ofbiz.base.util.UtilFormatOut;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilObject;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.webapp.OfbizUrlBuilder;
import org.ofbiz.webapp.WebAppUtil;
import org.ofbiz.webapp.control.ConfigXMLReader.ControllerConfig;
import org.ofbiz.webapp.event.EventFactory;
import org.ofbiz.webapp.event.EventHandler;
import org.ofbiz.webapp.event.EventHandlerException;
import org.ofbiz.webapp.renderer.RenderTargetUtil;
import org.ofbiz.webapp.stats.ServerHitBin;
import org.ofbiz.webapp.view.ViewFactory;
import org.ofbiz.webapp.view.ViewHandler;
import org.ofbiz.webapp.view.ViewHandlerException;
import org.ofbiz.webapp.view.ViewHandlerExt;
import org.ofbiz.webapp.website.WebSiteProperties;
import org.ofbiz.webapp.website.WebSiteWorker;
import org.xml.sax.SAXException;

/**
 * RequestHandler - Request Processor Object
 */
public class RequestHandler {

    public static final String module = RequestHandler.class.getName();
    private final String defaultStatusCodeString = UtilProperties.getPropertyValue("requestHandler.properties", "status-code", "301");
    private final ViewFactory viewFactory;
    private final EventFactory eventFactory;
    private final URL controllerConfigURL;
    private final boolean forceHttpSession;
    private final boolean trackServerHit;
    private final boolean trackVisit;
    private final boolean cookies;
    private final String charset;
    
    /**
     * SCIPIO: Allows or prevents override view URIs, based on web.xml config. Default: true (stock behavior).
     */
    private final boolean allowOverrideViewUri;

    public static RequestHandler getRequestHandler(ServletContext servletContext) {
        RequestHandler rh = (RequestHandler) servletContext.getAttribute("_REQUEST_HANDLER_");
        if (rh == null) {
            rh = new RequestHandler(servletContext);
            servletContext.setAttribute("_REQUEST_HANDLER_", rh);
        }
        return rh;
    }
    
    /**
     * SCIPIO: Gets request handler for the ServletContext associated to current request.
     * Added 2017-05-08.
     */
    public static RequestHandler getRequestHandler(HttpServletRequest request) {
        return getRequestHandler(request.getServletContext()); // NOTE: requires servlet API 3.0+
    }

    private RequestHandler(ServletContext context) {
        // init the ControllerConfig, but don't save it anywhere, just load it into the cache
        this.controllerConfigURL = ConfigXMLReader.getControllerConfigURL(context);
        try {
            ConfigXMLReader.getControllerConfig(this.controllerConfigURL);
        } catch (WebAppConfigurationException e) {
            // FIXME: controller.xml errors should throw an exception.
            Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
        }
        this.viewFactory = new ViewFactory(context, this.controllerConfigURL);
        this.eventFactory = new EventFactory(context, this.controllerConfigURL);

        this.forceHttpSession = "true".equalsIgnoreCase(context.getInitParameter("forceHttpSession"));
        this.trackServerHit = !"false".equalsIgnoreCase(context.getInitParameter("track-serverhit"));
        this.trackVisit = !"false".equalsIgnoreCase(context.getInitParameter("track-visit"));
        this.cookies = !"false".equalsIgnoreCase(context.getInitParameter("cookies"));
        this.charset = context.getInitParameter("charset");
        
        // SCIPIO: New (currently true by default)
        this.allowOverrideViewUri = !"false".equalsIgnoreCase(context.getInitParameter("allowOverrideViewUri"));
    }

    public ConfigXMLReader.ControllerConfig getControllerConfig() {
        try {
            return ConfigXMLReader.getControllerConfig(this.controllerConfigURL);
        } catch (WebAppConfigurationException e) {
            // FIXME: controller.xml errors should throw an exception.
            Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
        }
        return null;
    }

    public void doRequest(HttpServletRequest request, HttpServletResponse response, String requestUri) throws RequestHandlerException, RequestHandlerExceptionAllowExternalRequests {
        HttpSession session = request.getSession();
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        GenericValue userLogin = (GenericValue) session.getAttribute("userLogin");
        doRequest(request, response, requestUri, userLogin, delegator);
    }

    public void doRequest(HttpServletRequest request, HttpServletResponse response, String chain,
            GenericValue userLogin, Delegator delegator) throws RequestHandlerException, RequestHandlerExceptionAllowExternalRequests {

        final boolean throwRequestHandlerExceptionOnMissingLocalRequest = EntityUtilProperties.propertyValueEqualsIgnoreCase(
                "requestHandler.properties", "throwRequestHandlerExceptionOnMissingLocalRequest", "Y", delegator);
        long startTime = System.currentTimeMillis();
        HttpSession session = request.getSession();

        // get the controllerConfig once for this method so we don't have to get it over and over inside the method
        ConfigXMLReader.ControllerConfig controllerConfig = this.getControllerConfig();
        Map<String, ConfigXMLReader.RequestMap> requestMapMap = null;
        String statusCodeString = null;
        try {
            requestMapMap = controllerConfig.getRequestMapMap();
            statusCodeString = controllerConfig.getStatusCode();
        } catch (WebAppConfigurationException e) {
            Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
            throw new RequestHandlerException(e);
        }
        if (UtilValidate.isEmpty(statusCodeString)) {
            statusCodeString = defaultStatusCodeString;
        }

        // workaround if we are in the root webapp
        String cname = UtilHttp.getApplicationName(request);

        // Grab data from request object to process
        String defaultRequestUri = RequestHandler.getRequestUri(request.getPathInfo());
        if (request.getAttribute("targetRequestUri") == null) {
            if (request.getSession().getAttribute("_PREVIOUS_REQUEST_") != null) {
                request.setAttribute("targetRequestUri", request.getSession().getAttribute("_PREVIOUS_REQUEST_"));
            } else {
                request.setAttribute("targetRequestUri", "/" + defaultRequestUri);
            }
        }

        // SCIPIO: may now prevent this
        //String overrideViewUri = RequestHandler.getOverrideViewUri(request.getPathInfo());
        String overrideViewUri = null;
        if (allowOverrideViewUri) {
            overrideViewUri = RequestHandler.getOverrideViewUri(request.getPathInfo());
        }
        
        String requestMissingErrorMessage = "Unknown request [" + defaultRequestUri + "]; this request does not exist or cannot be called directly.";
        ConfigXMLReader.RequestMap requestMap = null;
        if (defaultRequestUri != null) {
            requestMap = requestMapMap.get(defaultRequestUri);
        }
        // check for default request
        if (requestMap == null) {
            String defaultRequest;
            try {
                defaultRequest = controllerConfig.getDefaultRequest();
            } catch (WebAppConfigurationException e) {
                Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                throw new RequestHandlerException(e);
            }
            if (defaultRequest != null) { // required! to avoid a null pointer exception and generate a requesthandler exception if default request not found.
                requestMap = requestMapMap.get(defaultRequest);
            }
        }

        // check for override view
        if (overrideViewUri != null) {
            ConfigXMLReader.ViewMap viewMap;
            try {
                viewMap = getControllerConfig().getViewMapMap().get(overrideViewUri);
                if (viewMap == null) {
                    String defaultRequest = controllerConfig.getDefaultRequest();
                    if (defaultRequest != null) { // required! to avoid a null pointer exception and generate a requesthandler exception if default request not found.
                        requestMap = requestMapMap.get(defaultRequest);
                    }
                }
            } catch (WebAppConfigurationException e) {
                Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                throw new RequestHandlerException(e);
            }
        }

        // if no matching request is found in the controller, depending on throwRequestHandlerExceptionOnMissingLocalRequest
        //  we throw a RequestHandlerException or RequestHandlerExceptionAllowExternalRequests
        if (requestMap == null) {
            if (throwRequestHandlerExceptionOnMissingLocalRequest) throw new RequestHandlerException(requestMissingErrorMessage);
            else throw new RequestHandlerExceptionAllowExternalRequests();
         }

        String eventReturn = null;
        if (requestMap.metrics != null && requestMap.metrics.getThreshold() != 0.0 && requestMap.metrics.getTotalEvents() > 3 && requestMap.metrics.getThreshold() < requestMap.metrics.getServiceRate()) {
            eventReturn = "threshold-exceeded";
        }
        ConfigXMLReader.RequestMap originalRequestMap = requestMap; // Save this so we can update the correct performance metrics.


        boolean interruptRequest = false;

        // Check for chained request.
        if (chain != null) {
            String chainRequestUri = RequestHandler.getRequestUri(chain);
            requestMap = requestMapMap.get(chainRequestUri);
            if (requestMap == null) {
                throw new RequestHandlerException("Unknown chained request [" + chainRequestUri + "]; this request does not exist");
            }
            if (request.getAttribute("_POST_CHAIN_VIEW_") != null) {
                overrideViewUri = (String) request.getAttribute("_POST_CHAIN_VIEW_");
            } else {
                // SCIPIO: may now prevent this
                if (allowOverrideViewUri) {
                    overrideViewUri = RequestHandler.getOverrideViewUri(chain);
                }
            }
            if (overrideViewUri != null) {
                // put this in a request attribute early in case an event needs to access it
                // not using _POST_CHAIN_VIEW_ because it shouldn't be set unless the event execution is successful
                request.setAttribute("_CURRENT_CHAIN_VIEW_", overrideViewUri);
            }
            if (Debug.infoOn()) Debug.logInfo("[RequestHandler]: Chain in place: requestUri=" + chainRequestUri + " overrideViewUri=" + overrideViewUri + " sessionId=" + UtilHttp.getSessionId(request), module);
        } else {
            // Check if X509 is required and we are not secure; throw exception
            if (!request.isSecure() && requestMap.securityCert) {
                throw new RequestHandlerException(requestMissingErrorMessage);
            }

            // Check to make sure we are allowed to access this request directly. (Also checks if this request is defined.)
            // If the request cannot be called, or is not defined, check and see if there is a default-request we can process
            if (!requestMap.securityDirectRequest) {
                String defaultRequest;
                try {
                    defaultRequest = controllerConfig.getDefaultRequest();
                } catch (WebAppConfigurationException e) {
                    Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                    throw new RequestHandlerException(e);
                }
                if (defaultRequest == null || !requestMapMap.get(defaultRequest).securityDirectRequest) {
                    // use the same message as if it was missing for security reasons, ie so can't tell if it is missing or direct request is not allowed
                    throw new RequestHandlerException(requestMissingErrorMessage);
                } else {
                    requestMap = requestMapMap.get(defaultRequest);
                }
            }
            // Check if we SHOULD be secure and are not.
            String forwardedProto = request.getHeader("X-Forwarded-Proto");
            boolean isForwardedSecure = UtilValidate.isNotEmpty(forwardedProto) && "HTTPS".equals(forwardedProto.toUpperCase());
            if ((!request.isSecure() && !isForwardedSecure) && requestMap.securityHttps) {
                // If the request method was POST then return an error to avoid problems with XSRF where the request may have come from another machine/program and had the same session ID but was not encrypted as it should have been (we used to let it pass to not lose data since it was too late to protect that data anyway)
                if (request.getMethod().equalsIgnoreCase("POST")) {
                    // we can't redirect with the body parameters, and for better security from XSRF, just return an error message
                    Locale locale = UtilHttp.getLocale(request);
                    String errMsg = UtilProperties.getMessage("WebappUiLabels", "requestHandler.InsecureFormPostToSecureRequest", locale);
                    Debug.logError("Got a insecure (non-https) form POST to a secure (http) request [" + requestMap.uri + "], returning error", module);

                    // see if HTTPS is enabled, if not then log a warning instead of throwing an exception
                    Boolean enableHttps = null;
                    String webSiteId = WebSiteWorker.getWebSiteId(request);
                    if (webSiteId != null) {
                        try {
                            GenericValue webSite = EntityQuery.use(delegator).from("WebSite").where("webSiteId", webSiteId).cache().queryOne();
                            if (webSite != null) enableHttps = webSite.getBoolean("enableHttps");
                        } catch (GenericEntityException e) {
                            Debug.logWarning(e, "Problems with WebSite entity; using global defaults", module);
                        }
                    }
                    if (enableHttps == null) {
                        enableHttps = EntityUtilProperties.propertyValueEqualsIgnoreCase("url.properties", "port.https.enabled", "Y", delegator);
                    }

                    if (Boolean.FALSE.equals(enableHttps)) {
                        Debug.logWarning("HTTPS is disabled for this site, so we can't tell if this was encrypted or not which means if a form was POSTed and it was not over HTTPS we don't know, but it would be vulnerable to an XSRF and other attacks: " + errMsg, module);
                    } else {
                        throw new RequestHandlerException(errMsg);
                    }
                } else {
                    StringBuilder urlBuf = new StringBuilder();
                    urlBuf.append(request.getPathInfo());
                    if (request.getQueryString() != null) {
                        urlBuf.append("?").append(request.getQueryString());
                    }
                    // SCIPIO: Always make full URL for redirect so uses host from entities
                    //String newUrl = RequestHandler.makeUrl(request, response, urlBuf.toString());
                    String newUrl = RequestHandler.makeUrlFull(request, response, urlBuf.toString());
                    if (newUrl.toUpperCase().startsWith("HTTPS")) {
                        // if we are supposed to be secure, redirect secure.
                        callRedirect(newUrl, response, request, statusCodeString);
                        return;
                    }
                }
            // if this is a new session and forceHttpSession is true and the request is secure but does not
            // need to be then we need the session cookie to be created via an http response (rather than https)
            // so we'll redirect to an unsecure request
            } else if (forceHttpSession && request.isSecure() && session.isNew() && !requestMap.securityHttps) {
                StringBuilder urlBuf = new StringBuilder();
                urlBuf.append(request.getPathInfo());
                if (request.getQueryString() != null) {
                    urlBuf.append("?").append(request.getQueryString());
                }
                // SCIPIO: Call proper method for this
                //String newUrl = RequestHandler.makeUrl(request, response, urlBuf.toString(), true, false, false);
                String newUrl = RequestHandler.makeUrlFull(request, response, urlBuf.toString());
                if (newUrl.toUpperCase().startsWith("HTTP")) {
                    callRedirect(newUrl, response, request, statusCodeString);
                    return;
                }
            }

            // Check for HTTPS client (x.509) security
            if (request.isSecure() && requestMap.securityCert) {
                X509Certificate[] clientCerts = (X509Certificate[]) request.getAttribute("javax.servlet.request.X509Certificate"); // 2.2 spec
                if (clientCerts == null) {
                    clientCerts = (X509Certificate[]) request.getAttribute("javax.net.ssl.peer_certificates"); // 2.1 spec
                }
                if (clientCerts == null) {
                    Debug.logWarning("Received no client certificates from browser", module);
                }

                // check if the client has a valid certificate (in our db store)
                boolean foundTrustedCert = false;

                if (clientCerts == null) {
                    throw new RequestHandlerException(requestMissingErrorMessage);
                } else {
                    if (Debug.infoOn()) {
                        for (int i = 0; i < clientCerts.length; i++) {
                            Debug.logInfo(clientCerts[i].getSubjectX500Principal().getName(), module);
                        }
                    }

                    // check if this is a trusted cert
                    if (SSLUtil.isClientTrusted(clientCerts, null)) {
                        foundTrustedCert = true;
                    }
                }

                if (!foundTrustedCert) {
                    Debug.logWarning(requestMissingErrorMessage, module);
                    throw new RequestHandlerException(requestMissingErrorMessage);
                }
            }

            // If its the first visit run the first visit events.
            if (this.trackVisit(request) && session.getAttribute("_FIRST_VISIT_EVENTS_") == null) {
                if (Debug.infoOn())
                    Debug.logInfo("This is the first request in this visit." + " sessionId=" + UtilHttp.getSessionId(request), module);
                session.setAttribute("_FIRST_VISIT_EVENTS_", "complete");
                try {
                    for (ConfigXMLReader.Event event: controllerConfig.getFirstVisitEventList().values()) {
                        try {
                            String returnString = this.runEvent(request, response, event, null, "firstvisit");
                            if (returnString == null || "none".equalsIgnoreCase(returnString)) {
                                interruptRequest = true;
                            } else if (!returnString.equalsIgnoreCase("success")) {
                                throw new EventHandlerException("First-Visit event did not return 'success'.");
                            }
                        } catch (EventHandlerException e) {
                            Debug.logError(e, module);
                        }
                    }
                } catch (WebAppConfigurationException e) {
                    Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                    throw new RequestHandlerException(e);
                }
            }

            // Invoke the pre-processor (but NOT in a chain)
            try {
                for (ConfigXMLReader.Event event: controllerConfig.getPreprocessorEventList().values()) {
                    try {
                        String returnString = this.runEvent(request, response, event, null, "preprocessor");
                        if (returnString == null || "none".equalsIgnoreCase(returnString)) {
                            interruptRequest = true;
                        } else if (!returnString.equalsIgnoreCase("success")) {
                            if (!returnString.contains(":_protect_:")) {
                                throw new EventHandlerException("Pre-Processor event [" + event.invoke + "] did not return 'success'.");
                            } else { // protect the view normally rendered and redirect to error response view
                                returnString = returnString.replace(":_protect_:", "");
                                if (returnString.length() > 0) {
                                    request.setAttribute("_ERROR_MESSAGE_", returnString);
                                }
                                eventReturn = null;
                                // check to see if there is a "protect" response, if so it's ok else show the default_error_response_view
                                if (!requestMap.requestResponseMap.containsKey("protect")) {
                                    String protectView = controllerConfig.getProtectView();
                                    if (protectView != null) {
                                        overrideViewUri = protectView;
                                    } else {
                                        overrideViewUri = EntityUtilProperties.getPropertyValue("security.properties", "default.error.response.view", delegator);
                                        overrideViewUri = overrideViewUri.replace("view:", "");
                                        if ("none:".equals(overrideViewUri)) {
                                            interruptRequest = true;
                                        }
                                    }
                                }
                            }
                        }
                    } catch (EventHandlerException e) {
                        Debug.logError(e, module);
                    }
                }
            } catch (WebAppConfigurationException e) {
                Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                throw new RequestHandlerException(e);
            }
        }

        // Pre-Processor/First-Visit event(s) can interrupt the flow by returning null.
        // Warning: this could cause problems if more then one event attempts to return a response.
        if (interruptRequest) {
            if (Debug.infoOn()) Debug.logInfo("[Pre-Processor Interrupted Request, not running: [" + requestMap.uri + "], sessionId=" + UtilHttp.getSessionId(request), module);
            return;
        }

        if (Debug.verboseOn()) Debug.logVerbose("[Processing Request]: " + requestMap.uri + " sessionId=" + UtilHttp.getSessionId(request), module);
        request.setAttribute("thisRequestUri", requestMap.uri); // store the actual request URI

        // SCIPIO
        ConfigXMLReader.ViewAsJsonConfig viewAsJsonConfig;
        try {
            viewAsJsonConfig = controllerConfig.getViewAsJsonConfigOrDefault();
        } catch (WebAppConfigurationException e) {
            Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
            throw new RequestHandlerException(e);
        }
        boolean viewAsJson = ViewAsJsonUtil.isViewAsJson(request, viewAsJsonConfig);

        // Perform security check.
        if (requestMap.securityAuth) {
            // Invoke the security handler
            // catch exceptions and throw RequestHandlerException if failed.
            if (Debug.verboseOn()) Debug.logVerbose("[RequestHandler]: AuthRequired. Running security check. sessionId=" + UtilHttp.getSessionId(request), module);
            ConfigXMLReader.Event checkLoginEvent = requestMapMap.get("checkLogin").event;
            String checkLoginReturnString = null;

            try {
                checkLoginReturnString = this.runEvent(request, response, checkLoginEvent, null, "security-auth");
            } catch (EventHandlerException e) {
                throw new RequestHandlerException(e.getMessage(), e);
            }
            if (!"success".equalsIgnoreCase(checkLoginReturnString)) {
                // previous URL already saved by event, so just do as the return says...
                eventReturn = checkLoginReturnString;
                // if the request is an ajax request we don't want to return the default login check
                if (!"XMLHttpRequest".equals(request.getHeader("X-Requested-With"))) {
                    requestMap = requestMapMap.get("checkLogin");
                } else {
                    // SCIPIO: 2017-05-15: for viewAsJson we have to check if we should
                    // use the regular or not
                    if (viewAsJson) {
                        if (ViewAsJsonUtil.isViewAsJsonRegularLogin(request, viewAsJsonConfig)) {
                            requestMap = requestMapMap.get("checkLogin");
                        } else {
                            // SCIPIO: If not using the regular login, we have to discard the render target expression, if any
                            requestMap = requestMapMap.get("ajaxCheckLogin");
                        }
                    } else {
                        requestMap = requestMapMap.get("ajaxCheckLogin");
                    }
                }
                
                // SCIPIO: if we require login, we may need to support an alternate render expr to handle login case
                Object scpLoginRenderTargetExpr = RenderTargetUtil.getRawRenderTargetExpr(request, RenderTargetUtil.LOGINRENDERTARGETEXPR_REQPARAM);
                if (scpLoginRenderTargetExpr != null) {
                    RenderTargetUtil.setRawRenderTargetExpr(request, scpLoginRenderTargetExpr);
                }
            }
            // SCIPIO: we have to mark a flag to say if was logged in for viewAsJson
            ViewAsJsonUtil.setRenderOutParam(request, ViewAsJsonUtil.LOGGEDIN_OUTPARAM, "success".equalsIgnoreCase(checkLoginReturnString));
        }

        // after security check but before running the event, see if a post-login redirect has completed and we have data from the pre-login request form to use now
        // we know this is the case if the _PREVIOUS_PARAM_MAP_ attribute is there, but the _PREVIOUS_REQUEST_ attribute has already been removed
        if (request.getSession().getAttribute("_PREVIOUS_PARAM_MAP_FORM_") != null && request.getSession().getAttribute("_PREVIOUS_REQUEST_") == null) {
            Map<String, Object> previousParamMap = UtilGenerics.checkMap(request.getSession().getAttribute("_PREVIOUS_PARAM_MAP_FORM_"), String.class, Object.class);
            for (Map.Entry<String, Object> previousParamEntry: previousParamMap.entrySet()) {
                request.setAttribute(previousParamEntry.getKey(), previousParamEntry.getValue());
            }

            // to avoid this data being included again, now remove the _PREVIOUS_PARAM_MAP_ attribute
            request.getSession().removeAttribute("_PREVIOUS_PARAM_MAP_FORM_");
        }

        // now we can start looking for the next request response to use
        ConfigXMLReader.RequestResponse nextRequestResponse = null;

        // Invoke the defined event (unless login failed)
        if (eventReturn == null && requestMap.event != null) {
            if (requestMap.event.type != null && requestMap.event.path != null && requestMap.event.invoke != null) {
                try {
                    long eventStartTime = System.currentTimeMillis();

                    // run the request event
                    eventReturn = this.runEvent(request, response, requestMap.event, requestMap, "request");
                                        
                    if (requestMap.event.metrics != null) {
                        requestMap.event.metrics.recordServiceRate(1, System.currentTimeMillis() - startTime);
                    }                    

                    // save the server hit for the request event
                    if (this.trackStats(request)) {
                        ServerHitBin.countEvent(cname + "." + requestMap.event.invoke, request, eventStartTime,
                                System.currentTimeMillis() - eventStartTime, userLogin);
                    }

                    // set the default event return
                    if (eventReturn == null) {
                        nextRequestResponse = ConfigXMLReader.emptyNoneRequestResponse;
                    }
                } catch (EventHandlerException e) {
                    // check to see if there is an "error" response, if so go there and make an request error message
                    if (requestMap.requestResponseMap.containsKey("error")) {
                        eventReturn = "error";
                        Locale locale = UtilHttp.getLocale(request);
                        String errMsg = UtilProperties.getMessage("WebappUiLabels", "requestHandler.error_call_event", locale);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg + ": " + e.toString());
                    } else {
                        throw new RequestHandlerException("Error calling event and no error response was specified", e);
                    }
                }
            }
        }

        // Process the eventReturn
        // at this point eventReturnString is finalized, so get the RequestResponse
        ConfigXMLReader.RequestResponse eventReturnBasedRequestResponse;
        if (eventReturn == null) {
            eventReturnBasedRequestResponse = null;
        } else {
            eventReturnBasedRequestResponse = requestMap.requestResponseMap.get(eventReturn);
            if (eventReturnBasedRequestResponse == null && eventReturn.equals("none")) {
                eventReturnBasedRequestResponse = ConfigXMLReader.emptyNoneRequestResponse;
            }
        }
        if (eventReturnBasedRequestResponse != null) {
            //String eventReturnBasedResponse = requestResponse.value;
            if (Debug.verboseOn()) Debug.logVerbose("[Response Qualified]: " + eventReturnBasedRequestResponse.name + ", " + eventReturnBasedRequestResponse.type + ":" + eventReturnBasedRequestResponse.value + " sessionId=" + UtilHttp.getSessionId(request), module);

            // If error, then display more error messages:
            if ("error".equals(eventReturnBasedRequestResponse.name)) {
                if (Debug.errorOn()) {
                    String errorMessageHeader = "Request " + requestMap.uri + " caused an error with the following message: ";
                    if (request.getAttribute("_ERROR_MESSAGE_") != null) {
                        Debug.logError(errorMessageHeader + request.getAttribute("_ERROR_MESSAGE_"), module);
                    }
                    if (request.getAttribute("_ERROR_MESSAGE_LIST_") != null) {
                        Debug.logError(errorMessageHeader + request.getAttribute("_ERROR_MESSAGE_LIST_"), module);
                    }
                }
            }
        } else if (eventReturn != null) {
            // only log this warning if there is an eventReturn (ie skip if no event, etc)
            Debug.logWarning("Could not find response in request [" + requestMap.uri + "] for event return [" + eventReturn + "]", module);
        }

        // Set the next view (don't use event return if success, default to nextView (which is set to eventReturn later if null); also even if success if it is a type "none" response ignore the nextView, ie use the eventReturn)
        if (eventReturnBasedRequestResponse != null && (!"success".equals(eventReturnBasedRequestResponse.name) || "none".equals(eventReturnBasedRequestResponse.type))) nextRequestResponse = eventReturnBasedRequestResponse;

        // get the previous request info
        String previousRequest = (String) request.getSession().getAttribute("_PREVIOUS_REQUEST_");
        String loginPass = (String) request.getAttribute("_LOGIN_PASSED_");

        // restore previous redirected request's attribute, so redirected page can display previous request's error msg etc.
        String preReqAttStr = (String) request.getSession().getAttribute("_REQ_ATTR_MAP_");
        Map<String, Object> previousRequestAttrMap = null;
        if (preReqAttStr != null) {
            previousRequestAttrMap = new HashMap<String, Object>();
            request.getSession().removeAttribute("_REQ_ATTR_MAP_");
            byte[] reqAttrMapBytes = StringUtil.fromHexString(preReqAttStr);
            Map<String, Object> preRequestMap = checkMap(UtilObject.getObject(reqAttrMapBytes), String.class, Object.class);
            if (UtilValidate.isNotEmpty(preRequestMap)) {
                for (Map.Entry<String, Object> entry: preRequestMap.entrySet()) {
                    String key = entry.getKey();
                    if ("_ERROR_MESSAGE_LIST_".equals(key) || "_ERROR_MESSAGE_MAP_".equals(key) || "_ERROR_MESSAGE_".equals(key) ||
                            "_EVENT_MESSAGE_LIST_".equals(key) || "_EVENT_MESSAGE_".equals(key)) {
                        request.setAttribute(key, entry.getValue());
                        previousRequestAttrMap.put(key, entry.getValue());
                   }
                }
            }
        }

        if (Debug.verboseOn()) Debug.logVerbose("[RequestHandler]: previousRequest - " + previousRequest + " (" + loginPass + ")" + " sessionId=" + UtilHttp.getSessionId(request), module);

        // if previous request exists, and a login just succeeded, do that now.
        if (previousRequest != null && loginPass != null && loginPass.equalsIgnoreCase("TRUE")) {
            request.getSession().removeAttribute("_PREVIOUS_REQUEST_");
            // special case to avoid login/logout looping: if request was "logout" before the login, change to null for default success view; do the same for "login" to avoid going back to the same page
            if ("logout".equals(previousRequest) || "/logout".equals(previousRequest) || "login".equals(previousRequest) || "/login".equals(previousRequest) || "checkLogin".equals(previousRequest) || "/checkLogin".equals(previousRequest) || "/checkLogin/login".equals(previousRequest)) {
                Debug.logWarning("Found special _PREVIOUS_REQUEST_ of [" + previousRequest + "], setting to null to avoid problems, not running request again", module);
            } else {
                if (Debug.infoOn()) Debug.logInfo("[Doing Previous Request]: " + previousRequest + " sessionId=" + UtilHttp.getSessionId(request), module);

                // note that the previous form parameters are not setup (only the URL ones here), they will be found in the session later and handled when the old request redirect comes back
                Map<String, Object> previousParamMap = UtilGenerics.checkMap(request.getSession().getAttribute("_PREVIOUS_PARAM_MAP_URL_"), String.class, Object.class);
                String queryString = UtilHttp.urlEncodeArgs(previousParamMap, false);
                String redirectTarget = previousRequest;
                if (UtilValidate.isNotEmpty(queryString)) {
                    redirectTarget += "?" + queryString;
                }
                
                // SCIPIO: Always make full link early
                //callRedirect(makeLink(request, response, redirectTarget), response, request, statusCodeString);
                callRedirect(makeLinkFull(request, response, redirectTarget), response, request, statusCodeString);

                // the old/uglier way: doRequest(request, response, previousRequest, userLogin, delegator);

                // this is needed as the request handled will be taking care of the view, etc
                return;
            }
        }

        ConfigXMLReader.RequestResponse successResponse = requestMap.requestResponseMap.get("success");
        if ((eventReturn == null || "success".equals(eventReturn)) && successResponse != null && "request".equals(successResponse.type)) {
            // chains will override any url defined views; but we will save the view for the very end
            if (UtilValidate.isNotEmpty(overrideViewUri)) {
                request.setAttribute("_POST_CHAIN_VIEW_", overrideViewUri);
            }
            nextRequestResponse = successResponse;
        }

        // Make sure we have some sort of response to go to
        if (nextRequestResponse == null) nextRequestResponse = successResponse;

        if (nextRequestResponse == null) {
            throw new RequestHandlerException("Illegal response; handler could not process request [" + requestMap.uri + "] and event return [" + eventReturn + "].");
        }

        // SCIPIO: Parse value
        String nextRequestResponseValue = parseResponseValue(request, response, nextRequestResponse.value, requestMap);
        
        if (Debug.verboseOn()) Debug.logVerbose("[Event Response Selected]  type=" + nextRequestResponse.type + ", value=" + nextRequestResponse.value + ", parsed-value=" + nextRequestResponseValue + ", sessionId=" + UtilHttp.getSessionId(request), module);

        // ========== Handle the responses - chains/views ==========

        // if the request has the save-last-view attribute set, save it now before the view can be rendered or other chain done so that the _LAST* session attributes will represent the previous request
        if (nextRequestResponse.saveLastView) {
            // Debug.logInfo("======save last view: " + session.getAttribute("_LAST_VIEW_NAME_"));
            String lastViewName = (String) session.getAttribute("_LAST_VIEW_NAME_");
            // Do not save the view if the last view is the same as the current view and saveCurrentView is false
            if (!(!nextRequestResponse.saveCurrentView && "view".equals(nextRequestResponse.type) && nextRequestResponseValue.equals(lastViewName))) {
                // SCIPIO: don't save for viewAsJson unless enabled
                if (!viewAsJson || ViewAsJsonUtil.isViewAsJsonUpdateSession(request, viewAsJsonConfig)) {
                    session.setAttribute("_SAVED_VIEW_NAME_", session.getAttribute("_LAST_VIEW_NAME_"));
                    session.setAttribute("_SAVED_VIEW_PARAMS_", session.getAttribute("_LAST_VIEW_PARAMS_"));
                }
            }
        }
        String saveName = null;
        if (nextRequestResponse.saveCurrentView) { saveName = "SAVED"; }
        if (nextRequestResponse.saveHomeView) { saveName = "HOME"; }

        if ("request".equals(nextRequestResponse.type)) {
            // chained request
            Debug.logInfo("[RequestHandler.doRequest]: Response is a chained request." + " sessionId=" + UtilHttp.getSessionId(request), module);
            doRequest(request, response, nextRequestResponseValue, userLogin, delegator);
        } else {
            // ======== handle views ========

            // first invoke the post-processor events.
            try {
                for (ConfigXMLReader.Event event: controllerConfig.getPostprocessorEventList().values()) {
                    try {
                        String returnString = this.runEvent(request, response, event, requestMap, "postprocessor");
                        if (returnString != null && !returnString.equalsIgnoreCase("success")) {
                            throw new EventHandlerException("Post-Processor event did not return 'success'.");
                        }
                    } catch (EventHandlerException e) {
                        Debug.logError(e, module);
                    }
                }
            } catch (WebAppConfigurationException e) {
                Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                throw new RequestHandlerException(e);
            }

            String responseStatusCode  = nextRequestResponse.statusCode;
            if(UtilValidate.isNotEmpty(responseStatusCode))
                statusCodeString = responseStatusCode;            
            
            if ("url".equals(nextRequestResponse.type)) {
                if (Debug.verboseOn()) Debug.logVerbose("[RequestHandler.doRequest]: Response is a URL redirect." + " sessionId=" + UtilHttp.getSessionId(request), module);
                // SCIPIO: Sanity check
                if (nextRequestResponseValue == null || nextRequestResponseValue.isEmpty()) {
                    Debug.logError("Scipio: Redirect URL is empty (request map URI: " + requestMap.uri + ")", module);
                    throw new RequestHandlerException("Scipio: Redirect URL is empty (request map URI: " + requestMap.uri + ")");
                }
                // SCIPIO: NOTE: Contrary to others, currently leaving this unchanged; full URLs may be completely external, and not sure want to pass them through encodeURL...
                callRedirect(nextRequestResponseValue, response, request, statusCodeString);
            } else if ("cross-redirect".equals(nextRequestResponse.type)) {
                // check for a cross-application redirect
                if (Debug.verboseOn()) Debug.logVerbose("[RequestHandler.doRequest]: Response is a Cross-Application redirect." + " sessionId=" + UtilHttp.getSessionId(request), module);
                // SCIPIO: Sanity check
                if (nextRequestResponseValue == null || nextRequestResponseValue.isEmpty()) {
                    Debug.logError("Scipio: Cross-redirect URL is empty (request map URI: " + requestMap.uri + ")", module);
                    throw new RequestHandlerException("Scipio: Cross-redirect URL is empty (request map URI: " + requestMap.uri + ")");
                }
                String url = nextRequestResponseValue.startsWith("/") ? nextRequestResponseValue : "/" + nextRequestResponseValue;
                // SCIPIO: Modified to pass through encodeURL and more intelligent link-building method
                // NOTE: no support for webSiteId, so absPath assumed true
                //callRedirect(url + this.makeQueryString(request, nextRequestResponse), response, request, statusCodeString);
                // SCIPIO: We MUST pass fullPath=true so that the host part will be looked up in Ofbiz entities as opposed to decided by Tomcat during redirect operation
                String targetUrl = makeLinkAutoFull(request, response, url + this.makeQueryString(request, nextRequestResponse), true, true, null, null);
                // SCIPIO: Sanity check
                if (targetUrl == null || targetUrl.isEmpty()) {
                    Debug.logError("Scipio: Could not build link for or resolve cross-redirect URI ('" + nextRequestResponseValue + "') (request map URI: " + requestMap.uri + ")", module);
                    throw new RequestHandlerException("Scipio: Could not build link for or resolve cross-redirect URI ('" + nextRequestResponseValue + "') (request map URI: " + requestMap.uri + ")");
                }
                callRedirect(targetUrl, response, request, statusCodeString);
            } else if ("request-redirect".equals(nextRequestResponse.type)) {
                if (Debug.verboseOn()) Debug.logVerbose("[RequestHandler.doRequest]: Response is a Request redirect." + " sessionId=" + UtilHttp.getSessionId(request), module);
                // SCIPIO: Sanity check
                if (nextRequestResponseValue == null || nextRequestResponseValue.isEmpty()) {
                    Debug.logError("Scipio: Request-redirect URI is empty (request map URI: " + requestMap.uri + ")", module);
                    throw new RequestHandlerException("Scipio: Request-redirect URI is empty (request map URI: " + requestMap.uri + ")");
                }
                // SCIPIO: We MUST pass fullPath=true so that the host part will be looked up in Ofbiz entities as opposed to decided by Tomcat during redirect operation
                //callRedirect(makeLinkWithQueryString(request, response, "/" + nextRequestResponseValue, nextRequestResponse), response, request, statusCodeString);
                String targetUrl = makeLinkFullWithQueryString(request, response, "/" + nextRequestResponseValue, nextRequestResponse);
                // SCIPIO: Sanity check
                if (targetUrl == null || targetUrl.isEmpty()) {
                    Debug.logError("Scipio: Could not build link for or resolve request-redirect URI ('" + nextRequestResponseValue + "') (request map URI: " + requestMap.uri + ")", module);
                    throw new RequestHandlerException("Scipio: Could not build link for or resolve request-redirect URI ('" + nextRequestResponseValue + "') (request map URI: " + requestMap.uri + ")");
                }
                callRedirect(targetUrl, response, request, statusCodeString);
            } else if ("request-redirect-noparam".equals(nextRequestResponse.type)) {
                if (Debug.verboseOn()) Debug.logVerbose("[RequestHandler.doRequest]: Response is a Request redirect with no parameters." + " sessionId=" + UtilHttp.getSessionId(request), module);
                // SCIPIO: Sanity check
                if (nextRequestResponseValue == null || nextRequestResponseValue.isEmpty()) {
                    Debug.logError("Scipio: Request-redirect-noparam URI is empty (request map URI: " + requestMap.uri + ")", module);
                    throw new RequestHandlerException("Scipio: Request-redirect-noparam URI is empty (request map URI: " + requestMap.uri + ")");
                }
                // SCIPIO: We MUST pass fullPath=true so that the host part will be looked up in Ofbiz entities as opposed to decided by Tomcat during redirect operation
                //callRedirect(makeLink(request, response, nextRequestResponseValue), response, request, statusCodeString);
                String targetUrl = makeLinkFull(request, response, nextRequestResponseValue);
                // SCIPIO: Sanity check
                if (targetUrl == null || targetUrl.isEmpty()) {
                    Debug.logError("Scipio: Could not build link for or resolve request-redirect-noparam URI ('" + nextRequestResponseValue + "') (request map URI: " + requestMap.uri + ")", module);
                    throw new RequestHandlerException("Scipio: Could not build link for or resolve request-redirect-noparam URI ('" + nextRequestResponseValue + "') (request map URI: " + requestMap.uri + ")");
                }
                callRedirect(targetUrl, response, request, statusCodeString);
            } else if ("view".equals(nextRequestResponse.type)) {
                if (Debug.verboseOn()) Debug.logVerbose("[RequestHandler.doRequest]: Response is a view." + " sessionId=" + UtilHttp.getSessionId(request), module);
                // check for an override view, only used if "success" = eventReturn
                String viewName = (UtilValidate.isNotEmpty(overrideViewUri) && (eventReturn == null || "success".equals(eventReturn))) ? overrideViewUri : nextRequestResponseValue;
                // SCIPIO: Sanity check
                if (viewName == null || viewName.isEmpty()) {
                    Debug.logError("Scipio: view name is empty (request map URI: " + requestMap.uri + ")", module);
                    throw new RequestHandlerException("Scipio: view name is empty (request map URI: " + requestMap.uri + ")");
                }
                renderView(viewName, requestMap.securityExternalView, request, response, saveName, controllerConfig, viewAsJsonConfig, viewAsJson);
            } else if ("view-last".equals(nextRequestResponse.type)) {
                if (Debug.verboseOn()) Debug.logVerbose("[RequestHandler.doRequest]: Response is a view." + " sessionId=" + UtilHttp.getSessionId(request), module);

                // check for an override view, only used if "success" = eventReturn
                String viewName = (UtilValidate.isNotEmpty(overrideViewUri) && (eventReturn == null || "success".equals(eventReturn))) ? overrideViewUri : nextRequestResponseValue;

                // as a further override, look for the _SAVED and then _HOME and then _LAST session attributes
                Map<String, Object> urlParams = null;
                if (session.getAttribute("_SAVED_VIEW_NAME_") != null) {
                    viewName = (String) session.getAttribute("_SAVED_VIEW_NAME_");
                    urlParams = UtilGenerics.<String, Object>checkMap(session.getAttribute("_SAVED_VIEW_PARAMS_"));
                } else if (session.getAttribute("_HOME_VIEW_NAME_") != null) {
                    viewName = (String) session.getAttribute("_HOME_VIEW_NAME_");
                    urlParams = UtilGenerics.<String, Object>checkMap(session.getAttribute("_HOME_VIEW_PARAMS_"));
                } else if (session.getAttribute("_LAST_VIEW_NAME_") != null) {
                    viewName = (String) session.getAttribute("_LAST_VIEW_NAME_");
                    urlParams = UtilGenerics.<String, Object>checkMap(session.getAttribute("_LAST_VIEW_PARAMS_"));
                } else if (UtilValidate.isNotEmpty(nextRequestResponseValue)) {
                    viewName = nextRequestResponseValue;
                }
                if (urlParams != null) {
                    for (Map.Entry<String, Object> urlParamEntry: urlParams.entrySet()) {
                        String key = urlParamEntry.getKey();
                        // Don't overwrite messages coming from the current event
                        if (!("_EVENT_MESSAGE_".equals(key) || "_ERROR_MESSAGE_".equals(key)
                                || "_EVENT_MESSAGE_LIST_".equals(key) || "_ERROR_MESSAGE_LIST_".equals(key))) {
                            request.setAttribute(key, urlParamEntry.getValue());
                        }
                    }
                }
                // SCIPIO: Sanity check
                if (viewName == null || viewName.isEmpty()) {
                    Debug.logError("Scipio: view-last view name is empty (request map URI: " + requestMap.uri + ")", module);
                    throw new RequestHandlerException("Scipio: view-last view name is empty (request map URI: " + requestMap.uri + ")");
                }
                renderView(viewName, requestMap.securityExternalView, request, response, null, controllerConfig, viewAsJsonConfig, viewAsJson);
            } else if ("view-last-noparam".equals(nextRequestResponse.type)) {
                 if (Debug.verboseOn()) Debug.logVerbose("[RequestHandler.doRequest]: Response is a view." + " sessionId=" + UtilHttp.getSessionId(request), module);

                 // check for an override view, only used if "success" = eventReturn
                 String viewName = (UtilValidate.isNotEmpty(overrideViewUri) && (eventReturn == null || "success".equals(eventReturn))) ? overrideViewUri : nextRequestResponseValue;

                 // as a further override, look for the _SAVED and then _HOME and then _LAST session attributes
                 if (session.getAttribute("_SAVED_VIEW_NAME_") != null) {
                     viewName = (String) session.getAttribute("_SAVED_VIEW_NAME_");
                 } else if (session.getAttribute("_HOME_VIEW_NAME_") != null) {
                     viewName = (String) session.getAttribute("_HOME_VIEW_NAME_");
                 } else if (session.getAttribute("_LAST_VIEW_NAME_") != null) {
                     viewName = (String) session.getAttribute("_LAST_VIEW_NAME_");
                 } else if (UtilValidate.isNotEmpty(nextRequestResponseValue)) {
                     viewName = nextRequestResponseValue;
                 }
                 // SCIPIO: Sanity check
                 if (viewName == null || viewName.isEmpty()) {
                     Debug.logError("Scipio: view-last-noparam view name is empty (request map URI: " + requestMap.uri + ")", module);
                     throw new RequestHandlerException("Scipio: view-last-noparam view name is empty (request map URI: " + requestMap.uri + ")");
                 }
                 renderView(viewName, requestMap.securityExternalView, request, response, null, controllerConfig, viewAsJsonConfig, viewAsJson);
            } else if ("view-home".equals(nextRequestResponse.type)) {
                if (Debug.verboseOn()) Debug.logVerbose("[RequestHandler.doRequest]: Response is a view." + " sessionId=" + UtilHttp.getSessionId(request), module);

                // check for an override view, only used if "success" = eventReturn
                String viewName = (UtilValidate.isNotEmpty(overrideViewUri) && (eventReturn == null || "success".equals(eventReturn))) ? overrideViewUri : nextRequestResponseValue;

                // as a further override, look for the _HOME session attributes
                Map<String, Object> urlParams = null;
                if (session.getAttribute("_HOME_VIEW_NAME_") != null) {
                    viewName = (String) session.getAttribute("_HOME_VIEW_NAME_");
                    urlParams = UtilGenerics.<String, Object>checkMap(session.getAttribute("_HOME_VIEW_PARAMS_"));
                }
                if (urlParams != null) {
                    for (Map.Entry<String, Object> urlParamEntry: urlParams.entrySet()) {
                        request.setAttribute(urlParamEntry.getKey(), urlParamEntry.getValue());
                    }
                }
                // SCIPIO: Sanity check
                if (viewName == null || viewName.isEmpty()) {
                    Debug.logError("Scipio: view-home view name is empty (request map URI: " + requestMap.uri + ")", module);
                    throw new RequestHandlerException("Scipio: view-last view name is empty (request map URI: " + requestMap.uri + ")");
                }
                renderView(viewName, requestMap.securityExternalView, request, response, null, controllerConfig, viewAsJsonConfig, viewAsJson);
            } else if ("none".equals(nextRequestResponse.type)) {
                // no view to render (meaning the return was processed by the event)
                if (Debug.verboseOn()) Debug.logVerbose("[RequestHandler.doRequest]: Response is handled by the event." + " sessionId=" + UtilHttp.getSessionId(request), module);
            }
        }
        if (originalRequestMap.metrics != null) {
            originalRequestMap.metrics.recordServiceRate(1, System.currentTimeMillis() - startTime);
        }
    }

    /**
     * SCIPIO: New feature that allows controller responses to dig values out of request attributes using the 
     * EL-like syntax: ${scope.name}. The value must be a string otherwise an error is logged.
     * returned.
     * <p>
     * Currently the supported scopes are:
     * requestAttributes
     * requestParameters
     * requestAttrParam - checks attributes; if null, uses params
     * sessionAttributes
     * applicationAttributes
     * <p>
     * Returns empty string instead of null if missing, for compatibility with existing Ofbiz code.
     */
    String parseResponseValue(HttpServletRequest request, HttpServletResponse response, String value, ConfigXMLReader.RequestMap requestMap) {
        if (value == null) {
            return "";
        }
        if (value.startsWith("${") && value.endsWith("}")) {
            Object attrValue = null;
            int dotIndex = value.indexOf('.');
            if (dotIndex >= 0) {
                String scope = value.substring(2, dotIndex).trim();
                String name = value.substring(dotIndex+1, value.length() - 1).trim();
                if (scope.length() > 0 && name.length() > 0) {
                    if ("requestAttributes".equals(scope)) {
                        attrValue = request.getAttribute(name);
                    } else if ("requestParameters".equals(scope)) {
                        attrValue = request.getParameter(name);
                    } else if ("requestAttrParam".equals(scope)) {
                        attrValue = request.getAttribute(name);
                        if (attrValue == null) {
                            attrValue = request.getParameter(name);
                        }
                    } else if ("sessionAttributes".equals(scope)) {
                        HttpSession session = request.getSession(false);
                        if (session != null) {
                            attrValue = session.getAttribute(name);
                        }
                    } else if ("applicationAttributes".equals(scope)) {
                        attrValue = request.getServletContext().getAttribute(name);
                    }
                }
            }
            if (attrValue != null) {
                if (attrValue instanceof String) {
                    String attrStr = (String) attrValue;
                    return attrStr;
                } else {
                    if (requestMap != null) {
                        Debug.logError("Scipio: Error in request handler: The interpreted request response value '" +
                                attrValue.toString() + "' from request URI '" + requestMap.uri + "' did not evaluate to a string; treating as empty", module);
                    } else {
                        Debug.logError("Scipio: Error in request handler: The interpreted request response value '" +
                                attrValue.toString() + "' did not evaluate to a string; treating as empty", module);
                    }
                }
            }
            return "";
        }
        return value;
    }
    
    
    /** Find the event handler and invoke an event. */
    public String runEvent(HttpServletRequest request, HttpServletResponse response,
            ConfigXMLReader.Event event, ConfigXMLReader.RequestMap requestMap, String trigger) throws EventHandlerException {
        EventHandler eventHandler = eventFactory.getEventHandler(event.type);
        String eventReturn = eventHandler.invoke(event, requestMap, request, response);
        if (Debug.verboseOn() || (Debug.infoOn() && "request".equals(trigger))) Debug.logInfo("Ran Event [" + event.type + ":" + event.path + "#" + event.invoke + "] from [" + trigger + "], result is [" + eventReturn + "]", module);
        return eventReturn;
    }

    /** Returns the default error page for this request. */
    public String getDefaultErrorPage(HttpServletRequest request) {
        String errorpage = null;
        try {
            errorpage = getControllerConfig().getErrorpage();
        } catch (WebAppConfigurationException e) {
            Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
        }
        if (UtilValidate.isNotEmpty(errorpage)) return errorpage;
        return "/error/error.jsp";
    }

    /** Returns the default status-code for this request. */
    public String getStatusCode(HttpServletRequest request) {
        String statusCode = null;
        try {
            statusCode = getControllerConfig().getStatusCode();
        } catch (WebAppConfigurationException e) {
            Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
        }
        if (UtilValidate.isNotEmpty(statusCode)) return statusCode;
        return null;
    }

    /** Returns the ViewFactory Object. */
    public ViewFactory getViewFactory() {
        return viewFactory;
    }

    /** Returns the EventFactory Object. */
    public EventFactory getEventFactory() {
        return eventFactory;
    }

    public static String getRequestUri(String path) {
        List<String> pathInfo = StringUtil.split(path, "/");
        if (UtilValidate.isEmpty(pathInfo)) {
            Debug.logWarning("Got nothing when splitting URI: " + path, module);
            return null;
        }
        if (pathInfo.get(0).indexOf('?') > -1) {
            return pathInfo.get(0).substring(0, pathInfo.get(0).indexOf('?'));
        } else {
            return pathInfo.get(0);
        }
    }

    public static String getOverrideViewUri(String path) {
        List<String> pathItemList = StringUtil.split(path, "/");
        if (pathItemList == null) {
            return null;
        }
        pathItemList = pathItemList.subList(1, pathItemList.size());

        String nextPage = null;
        for (String pathItem: pathItemList) {
            if (pathItem.indexOf('~') != 0) {
                if (pathItem.indexOf('?') > -1) {
                    pathItem = pathItem.substring(0, pathItem.indexOf('?'));
                }
                nextPage = (nextPage == null ? pathItem : nextPage + "/" + pathItem);
            }
        }
        return nextPage;
    }

    /**
     * Performs HTTP redirect to the given URL.
     * <p>
     * SCIPIO: NOTE: All the code currently calling this may append jsessionIds (through processing
     * of changing encode to true to correct filter hook behavior).
     * Currently I don't see how this is bad.
     * If need to remove jsessionId from redirects, could uncomment the lines below.
     */
    private void callRedirect(String url, HttpServletResponse resp, HttpServletRequest req, String statusCodeString) throws RequestHandlerException {
        // SCIPIO: Uncomment this to force remove jsessionId from controller redirects...
        //RequestUtil.removeJsessionId(url);
        if (Debug.infoOn()) Debug.logInfo("Sending redirect to: [" + url + "], sessionId=" + UtilHttp.getSessionId(req), module);
        // SCIPIO: sanity check
        if (url == null || url.isEmpty()) {
            Debug.logError("Scipio: Redirect URL is empty", module);
            throw new RequestHandlerException("Scipio: Redirect URL is empty");
        }
        // set the attributes in the session so we can access it.
        Enumeration<String> attributeNameEnum = UtilGenerics.cast(req.getAttributeNames());
        Map<String, Object> reqAttrMap = new HashMap<String, Object>();
        Integer statusCode;
        try {
            statusCode = Integer.valueOf(statusCodeString);
        } catch (NumberFormatException e) {
            statusCode = 303;
        } 
        while (attributeNameEnum.hasMoreElements()) {
            String name = attributeNameEnum.nextElement();
            Object obj = req.getAttribute(name);
            if (obj instanceof Serializable) {
                reqAttrMap.put(name, obj);
            }
        }
        if (reqAttrMap.size() > 0) {
            reqAttrMap.remove("_REQUEST_HANDLER_");  // RequestHandler is not serializable and must be removed first.  See http://issues.apache.org/jira/browse/OFBIZ-750
            byte[] reqAttrMapBytes = UtilObject.getBytes(reqAttrMap);
            if (reqAttrMapBytes != null) {
                req.getSession().setAttribute("_REQ_ATTR_MAP_", StringUtil.toHexString(reqAttrMapBytes));
            }
        }

        // send the redirect
        try {            
            resp.setStatus(statusCode);
            resp.setHeader("Location", url);
            resp.setHeader("Connection", "close");
        } catch (IllegalStateException ise) {
            throw new RequestHandlerException(ise.getMessage(), ise);
        }
    }
    private void renderView(String view, boolean allowExtView, HttpServletRequest req, HttpServletResponse resp, String saveName, ControllerConfig controllerConfig, ConfigXMLReader.ViewAsJsonConfig viewAsJsonConfig, boolean viewAsJson) throws RequestHandlerException, RequestHandlerExceptionAllowExternalRequests {
        // SCIPIO: sanity check
        if (view == null || view.isEmpty()) {
            Debug.logError("Scipio: View name is empty", module);
            throw new RequestHandlerException("Scipio: View name is empty");
        }
        
        GenericValue userLogin = (GenericValue) req.getSession().getAttribute("userLogin");
        // workaraound if we are in the root webapp
        String cname = UtilHttp.getApplicationName(req);
        String oldView = view;

        if (UtilValidate.isNotEmpty(view) && view.charAt(0) == '/') {
            view = view.substring(1);
        }

        // if the view name starts with the control servlet name and a /, then it was an
        // attempt to override the default view with a call back into the control servlet,
        // so just get the target view name and use that
        
        String servletName = req.getServletPath();
        if (servletName.startsWith("/")) {
            servletName = servletName.substring(1);
        }

        if (Debug.infoOn()) Debug.logInfo("Rendering View [" + view + "], sessionId=" + UtilHttp.getSessionId(req), module);
        if (view.startsWith(servletName + "/")) {
            view = view.substring(servletName.length() + 1);
            if (Debug.infoOn()) Debug.logInfo("a manual control servlet request was received, removing control servlet path resulting in: view=" + view, module);
        }

        if (Debug.verboseOn()) Debug.logVerbose("[Getting View Map]: " + view + " sessionId=" + UtilHttp.getSessionId(req), module);

        // before mapping the view, set a request attribute so we know where we are
        req.setAttribute("_CURRENT_VIEW_", view);

        if (!viewAsJson || ViewAsJsonUtil.isViewAsJsonUpdateSession(req, viewAsJsonConfig)) {
            // save the view in the session for the last view, plus the parameters Map (can use all parameters as they will never go into a URL, will only stay in the session and extra data will be ignored as we won't go to the original request just the view); note that this is saved after the request/view processing has finished so when those run they will get the value from the previous request
            Map<String, Object> paramMap = UtilHttp.getParameterMap(req, ViewAsJsonUtil.VIEWASJSON_RENDERTARGET_REQPARAM_ALL, false); // SCIPIO: SPECIAL EXCLUDES: these will mess up rendering if they aren't excluded
            // add in the attributes as well so everything needed for the rendering context will be in place if/when we get back to this view
            // SCIPIO: 2017-10-04: NEW VIEW-SAVE ATTRIBUTE EXCLUDES - these can be set by event to prevent cached and volatile results from going into session
            Set<String> viewSaveAttrExcl = UtilGenerics.checkSet(req.getAttribute("_SCP_VIEW_SAVE_ATTR_EXCL_"));
            if (viewSaveAttrExcl != null) {
                viewSaveAttrExcl.add("_SCP_VIEW_SAVE_ATTR_EXCL_");
            }
            //paramMap.putAll(UtilHttp.getAttributeMap(req));
            paramMap.putAll(UtilHttp.getAttributeMap(req, viewSaveAttrExcl));
            UtilMisc.makeMapSerializable(paramMap);
            if (paramMap.containsKey("_LAST_VIEW_NAME_")) { // Used by lookups to keep the real view (request)
                req.getSession().setAttribute("_LAST_VIEW_NAME_", paramMap.get("_LAST_VIEW_NAME_"));
            } else {
                req.getSession().setAttribute("_LAST_VIEW_NAME_", view);
            }
            req.getSession().setAttribute("_LAST_VIEW_PARAMS_", paramMap);
    
            if ("SAVED".equals(saveName)) {
                //Debug.logInfo("======save current view: " + view);
                req.getSession().setAttribute("_SAVED_VIEW_NAME_", view);
                req.getSession().setAttribute("_SAVED_VIEW_PARAMS_", paramMap);
            }
    
            if ("HOME".equals(saveName)) {
                //Debug.logInfo("======save home view: " + view);
                req.getSession().setAttribute("_HOME_VIEW_NAME_", view);
                req.getSession().setAttribute("_HOME_VIEW_PARAMS_", paramMap);
                // clear other saved views
                req.getSession().removeAttribute("_SAVED_VIEW_NAME_");
                req.getSession().removeAttribute("_SAVED_VIEW_PARAMS_");
            }
        }

        ConfigXMLReader.ViewMap viewMap = null;
        try {
            viewMap = (view == null ? null : getControllerConfig().getViewMapMap().get(view));
        } catch (WebAppConfigurationException e) {
            Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
            throw new RequestHandlerException(e);
        }
        if (viewMap == null) {
            throw new RequestHandlerException("No definition found for view with name [" + view + "]");
        }

        String nextPage;

        if (viewMap.page == null) {
            if (!allowExtView || viewAsJson) { // SCIPIO: NOTE: 2017-05-12: don't allow weird nextPage stuff for json for now - implications unclear
                throw new RequestHandlerException("No view to render.");
            } else {
                nextPage = "/" + oldView;
            }
        } else {
            nextPage = viewMap.page;
        }

        if (Debug.verboseOn()) Debug.logVerbose("[Mapped To]: " + nextPage + " sessionId=" + UtilHttp.getSessionId(req), module);

        long viewStartTime = System.currentTimeMillis();

        // setup character encoding and content type
        String charset;
        if (viewAsJson) {
            // SCIPIO: NOTE: we hardcode UTF-8 because JSON requests will be like this
            charset = "UTF-8";
        } else {
            charset = UtilFormatOut.checkEmpty(this.charset, req.getCharacterEncoding(), "UTF-8");
        }
        
        if (!viewAsJson) {
            String viewCharset = viewMap.encoding;
            //NOTE: if the viewCharset is "none" then no charset will be used
            if (UtilValidate.isNotEmpty(viewCharset)) {
                charset = viewCharset;
            }
        }

        if (!"none".equals(charset)) {
            try {
                req.setCharacterEncoding(charset);
            } catch (UnsupportedEncodingException e) {
                throw new RequestHandlerException("Could not set character encoding to " + charset, e);
            } catch (IllegalStateException e) {
                Debug.logInfo(e, "Could not set character encoding to " + charset + ", something has probably already committed the stream", module);
            }
        }

        // setup content type
        String contentType = "text/html";
        String viewContentType = viewMap.contentType;
        if (UtilValidate.isNotEmpty(viewContentType)) {
            contentType = viewContentType;
        }

        if (!viewAsJson) {
            if (charset.length() > 0 && !"none".equals(charset)) {
                resp.setContentType(contentType + "; charset=" + charset);
            } else {
                resp.setContentType(contentType);
            }
        }

        if (Debug.verboseOn()) Debug.logVerbose("The ContentType for the " + view + " view is: " + contentType, module);

        boolean viewNoCache = viewMap.noCache;
        if (viewNoCache) {
           UtilHttp.setResponseBrowserProxyNoCache(resp);
           if (Debug.verboseOn()) Debug.logVerbose("Sending no-cache headers for view [" + nextPage + "]", module);
        }

        try {
            if (Debug.verboseOn()) Debug.logVerbose("Rendering view [" + nextPage + "] of type [" + viewMap.type + "]", module);
            ViewHandler vh = viewFactory.getViewHandler(viewMap.type);
            if (viewAsJson) {
                invokeViewHandlerAsJson(vh, viewAsJsonConfig, view, nextPage, viewMap.info, contentType, charset, req, resp);
            } else {
                vh.render(view, nextPage, viewMap.info, contentType, charset, req, resp);
            }
        } catch (ViewHandlerException e) {
            Throwable throwable = e.getNested() != null ? e.getNested() : e;

            throw new RequestHandlerException(e.getNonNestedMessage(), throwable);
        }

        if (viewAsJson) {
            // SCIPIO: NOTE: we go to handler URI so potentially a webapp can tweak json output behavior.
            ViewAsJsonUtil.addDefaultRenderOutAttrNames(req);
            String jsonRequestUri;
            try {
                jsonRequestUri = ViewAsJsonUtil.getViewAsJsonRequestUri(req, viewAsJsonConfig);
            } catch (WebAppConfigurationException e) {
                Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                throw new RequestHandlerException(e);
            }
            doRequest(req, resp, jsonRequestUri, userLogin, (Delegator) req.getAttribute("delegator"));
        }
        
        // before getting the view generation time flush the response output to get more consistent results
        try {
            resp.flushBuffer();
        } catch (java.io.IOException e) {
            /* If any request gets aborted before completing, i.e if a user requests a page and cancels that request before the page is rendered and returned
               or if request is an ajax request and user calls abort() method for on ajax request then its showing broken pipe exception on console,
               skip throwing of RequestHandlerException. JIRA Ticket - OFBIZ-254
            */
            if (Debug.verboseOn()) Debug.logVerbose("Skip Request Handler Exception that is caused due to aborted requests. " + e.getMessage(), module);
        }

        String vname = (String) req.getAttribute("_CURRENT_VIEW_");

        if (this.trackStats(req) && vname != null) {
            ServerHitBin.countView(cname + "." + vname, req, viewStartTime,
                System.currentTimeMillis() - viewStartTime, userLogin);
        }
    }
    
    /**
     * SCIPIO: factored out viewAsJson view handler render wrapper code.
     */
    public static void invokeViewHandlerAsJson(ViewHandler vh, ConfigXMLReader.ViewAsJsonConfig viewAsJsonConfig, String view, String nextPage, String info, String contentType, String charset, HttpServletRequest req, HttpServletResponse resp) throws ViewHandlerException {
        if (vh instanceof ViewHandlerExt) {
            ViewHandlerExt vhe = (ViewHandlerExt) vh;
            // SPECIAL: we must save _ERROR_MESSAGE_ and the like because the screen handler destroys them!
            Map<String, Object> msgAttrMap = ViewAsJsonUtil.getMessageAttributes(req);
            Writer sw = ViewAsJsonUtil.prepareWriterAndMode(req, viewAsJsonConfig);
            try {
                vhe.render(view, nextPage, info, contentType, charset, req, resp, sw);
            } finally {
                ViewAsJsonUtil.setRenderOutParamFromWriter(req, sw);
                ViewAsJsonUtil.setMessageAttributes(req, msgAttrMap);
            }
        } else {
            throw new ViewHandlerException("View handler does not support extended interface (ViewHandlerExt)");
        }
    }
    
    /**
     * Returns a URL String that contains only the scheme and host parts. This method
     * should not be used because it ignores settings in the WebSite entity.
     * 
     * @param request
     * @param secure
     * @deprecated Use OfbizUrlBuilder
     */
    @Deprecated
    public static String getDefaultServerRootUrl(HttpServletRequest request, boolean secure) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        String httpsPort = EntityUtilProperties.getPropertyValue("url.properties", "port.https", "443", delegator);
        String httpsServer = EntityUtilProperties.getPropertyValue("url.properties", "force.https.host", delegator);
        String httpPort = EntityUtilProperties.getPropertyValue("url.properties", "port.http", "80", delegator);
        String httpServer = EntityUtilProperties.getPropertyValue("url.properties", "force.http.host", delegator);
        boolean useHttps = EntityUtilProperties.propertyValueEqualsIgnoreCase("url.properties", "port.https.enabled", "Y", delegator);

        if (Start.getInstance().getConfig().portOffset != 0) {
            // SCIPIO: ensure has value
            if (!httpPort.isEmpty()) {
                Integer httpPortValue = Integer.valueOf(httpPort);
                httpPortValue += Start.getInstance().getConfig().portOffset;
                httpPort = httpPortValue.toString();
            }
            if (!httpsPort.isEmpty()) {
                Integer httpsPortValue = Integer.valueOf(httpsPort);
                httpsPortValue += Start.getInstance().getConfig().portOffset;
                httpsPort = httpsPortValue.toString();
            }
        }
        
        StringBuilder newURL = new StringBuilder();

        if (secure && useHttps) {
            String server = httpsServer;
            if (UtilValidate.isEmpty(server)) {
                server = request.getServerName();
            }

            newURL.append("https://");
            newURL.append(server);
            if (!httpsPort.equals("443")) {
                newURL.append(":").append(httpsPort);
            }

        } else {
            String server = httpServer;
            if (UtilValidate.isEmpty(server)) {
                server = request.getServerName();
            }

            newURL.append("http://");
            newURL.append(server);
            if (!httpPort.equals("80")) {
                newURL.append(":").append(httpPort);
            }
        }
        return newURL.toString();
    }


    /**
     * Creates a query string based on the redirect parameters for a request response, if specified, or for all request parameters if no redirect parameters are specified.
     * <p>
     * SCIPIO: 2017-04-24: ENHANCED (see site-conf.xsd).
     *
     * @param request the Http request
     * @param requestResponse the RequestResponse Object
     * @return return the query string
     */
    public String makeQueryString(HttpServletRequest request, ConfigXMLReader.RequestResponse requestResponse) {
        if (requestResponse == null || 
                ("auto".equals(requestResponse.includeMode) && requestResponse.redirectParameterMap.size() == 0 && requestResponse.redirectParameterValueMap.size() == 0) ||
                !"url-params".equals(requestResponse.includeMode) || "all-params".equals(requestResponse.includeMode)) {
            Map<String, Object> urlParams;
            if ("all-params".equals(requestResponse.includeMode)) {
                urlParams = UtilHttp.getParameterMap(request, requestResponse.excludeParameterSet, false);
            } else {
                urlParams = UtilHttp.getUrlOnlyParameterMap(request);
                
                // SCIPIO: remove excluded
                if (requestResponse.excludeParameterSet != null) {
                    for(String name : requestResponse.excludeParameterSet) {
                        urlParams.remove(name);
                    }
                }
            }

            // SCIPIO: we now support adding extra params
            for (Map.Entry<String, String> entry: requestResponse.redirectParameterMap.entrySet()) {
                String name = entry.getKey();
                String from = entry.getValue();

                Object value = request.getAttribute(from);
                if (value == null) {
                    value = request.getParameter(from);
                }

                urlParams.put(name, value);
            }

            for (Map.Entry<String, String> entry: requestResponse.redirectParameterValueMap.entrySet()) {
                String name = entry.getKey();
                String value = entry.getValue();

                urlParams.put(name, value);
            }
            
            String queryString = UtilHttp.urlEncodeArgs(urlParams, false);
            if(UtilValidate.isEmpty(queryString)) {
                return queryString;
            }
            return "?" + queryString;
        } else {
            StringBuilder queryString = new StringBuilder();
            queryString.append("?");
            for (Map.Entry<String, String> entry: requestResponse.redirectParameterMap.entrySet()) {
                String name = entry.getKey();
                String from = entry.getValue();

                Object value = request.getAttribute(from);
                if (value == null) {
                    value = request.getParameter(from);
                }

                addNameValuePairToQueryString(queryString, name, (String) value);
            }

            for (Map.Entry<String, String> entry: requestResponse.redirectParameterValueMap.entrySet()) {
                String name = entry.getKey();
                String value = entry.getValue();

                addNameValuePairToQueryString(queryString, name, value);
            }

            return queryString.toString();
        }
    }

    private void addNameValuePairToQueryString(StringBuilder queryString, String name, String value) {
        if (UtilValidate.isNotEmpty(value)) {
            if (queryString.length() > 1) {
                queryString.append("&");
            }
            String encodedName = UtilCodec.getEncoder("url").encode(name);
            if (encodedName != null) {
                queryString.append(encodedName);
                queryString.append("=");
                queryString.append(UtilCodec.getEncoder("url").encode(value));
            }
        }
    }

    /**
     * Builds links with added query string.
     * <p>
     * SCIPIO: Modified overload to allow boolean flags.
     */
    public String makeLinkWithQueryString(HttpServletRequest request, HttpServletResponse response, String url, Boolean fullPath, Boolean secure, Boolean encode, 
            ConfigXMLReader.RequestResponse requestResponse) {
        String initialLink = this.makeLink(request, response, url, fullPath, secure, encode);
        String queryString = this.makeQueryString(request, requestResponse);
        return initialLink + queryString;
    }
    
    /**
     * Builds links with added query string.
     * <p>
     * SCIPIO: Original signature method, now delegates.
     */
    public String makeLinkWithQueryString(HttpServletRequest request, HttpServletResponse response, String url, ConfigXMLReader.RequestResponse requestResponse) {
        return makeLinkWithQueryString(request, response, url, null, null, null, requestResponse);
    }
    
    /**
     * SCIPIO: Builds a full-path link (HTTPS as necessary) with added query string.
     */
    public String makeLinkFullWithQueryString(HttpServletRequest request, HttpServletResponse response, String url, ConfigXMLReader.RequestResponse requestResponse) {
        return makeLinkWithQueryString(request, response, url, true, null, null, requestResponse);
    }    

    public String makeLink(HttpServletRequest request, HttpServletResponse response, String url) {
        return makeLink(request, response, url, null, null, null);
    }
    
    /**
     * SCIPIO: Builds a full-path link (HTTPS as necessary).
     */
    public String makeLinkFull(HttpServletRequest request, HttpServletResponse response, String url) {
        return makeLink(request, response, url, true, null, null);
    }    

    /**
     * Builds an Ofbiz navigation link.
     * <p>
     * SCIPIO: This function is heavily modified to support non-controller intra-webapp links
     * as well as inter-webapp links. It should be able to generate all possible types of webapp
     * navigation links. However, it will only build links for webapps recognized by the server,
     * because in most cases we require information from the webapp.
     * <p>
     * <strong>fullPath behavior change</strong>: In Scipio, when fullPath is specified for a controller request, if the 
     * request is defined as secure, a secure URL will be created. This method will now never allow an 
     * insecure URL to built for a controller request marked secure. In stock Ofbiz, this behavior was 
     * different: fullPath could generate insecure URLs to secure requests. In addition, fullPath will 
     * by default no longer downgrade HTTPS connections. To allow downgrades, you must explicitly specify 
     * request it by passing secure false, and this may still produce a secure link if the target
     * is marked secure. Currently, this applies to all links including inter-webapp links.
     * <p>
     * <strong>secure behavior change</strong>: In Scipio, if current browsing is secure, we NEVER downgrade to HTTPS unless 
     * explicitly requested by passing secure false, and secure false may still produce a secure link if
     * needed. Currently (2016-04-06), for security reasons, this 
     * downgrading request request only applies to the case where the target link is marked as non-secure (or is missing/unknown, as of 2016-07-14), such
     * that in general, setting secure false does not mean the link will be insecure in all cases.
     * In addition, in Scipio, secure flag no longer forces a fullPath link. Specify fullPath true in addition to 
     * secure to force a fullPath link. Links may still generate full-path secure links when needed even 
     * if not requested, however.
     * <p>
     * <strong>encode behavior</strong>: The <code>encode</code> flag controls whether the link should
     * be passed through <code>HttpServletResponse.encodeURL</code> method. For our purposes, this is <strong>NOT</strong>
     * equivalent to appending <code>jsessionid</code>; it has other functionality such as calling servlet filter hooks.
     * Almost all navigation links to Ofbiz webapps whether inter-webapp or intra-webapp should have encode <code>true</code>.
     * If jsessionid must be prevented for a link, currently this can be done by calling
     * {@link RequestLinkUtil#removeJsessionId}.
     * TODO: Could use an extra Boolean arg to force jsessionid on/off (null for default behavior).
     * <p>
     * <strong>URL format</strong>: The passed <code>url</code> should either be a controller URI (if <code>controller</code> true)
     * or a path relative to webapp context root. It should NEVER include the webapp context root (mount-point).
     * <p>
     * If both <code>interWebapp</code> and <code>controller</code> are false, it means we're building an intra-webapp URL
     * for an arbitrary servlet.
     * <p>
     * The caller sets <code>interWebapp</code> to the value he wants the link to be interpreted as; this method
     * will not try to detect if the link falls within current request webapp or not; it may be valid
     * to want to generate an intra-webapp link using inter-webapp building logic.
     * <p>
     * <em>DEV NOTE</em>: The ability to specify arbitrary absolute path as url has been explicitly prevented and removed
     * from this method, to simplify. The only case we can't generate
     * links is if for some reason a webapp is not recognized by the current server (no <code>WebappInfo</code>
     * available).
     * <p>
     * <em>DEV NOTE</em>: <code>interWebapp</code> must remain a separate boolean because it may be possible
     * to pass <code>webappInfo</code> even when intra-webapp or for other optimizations.
     * <p>
     * TODO: fullPath, secure, encode should be Boolean not boolean to allow null and finer grained control, current interface too limited for some cases.
     *
     * @param request the request (required)
     * @param response the response (required)
     * @param url the path or URI (required), relative (relative to controller servlet if controller true, or relative to webapp context root if controller false)
     * @param interWebapp if true, treat the link as inter-webapp (default: null/false) (Scipio: new parameter)
     * @param webappInfo the webapp info of the link's target webapp (optional, conditionally required) (Scipio: new parameter)
     * @param controller if true, assume is a controller link and refer to controller for building link (default: null/true) (Scipio: new parameter)
     * @param fullPath if true, always produce full URL (HTTP or HTTPS) (default: null/false) (Scipio: changed to Boolean instead of boolean, and changed behavior)
     * @param secure if true, resulting links is guaranteed to be secure (default: null/false) (Scipio: changed to Boolean instead of boolean, and changed behavior)
     * @param encode if true, pass through response.encodeURL (default: null/true) (Scipio: changed to Boolean instead of boolean)
     * @return the resulting URL
     */
    public String makeLink(HttpServletRequest request, HttpServletResponse response, String url, Boolean interWebapp, WebappInfo webappInfo, Boolean controller, 
            Boolean fullPath, Boolean secure, Boolean encode) {
        // SCIPIO: We now accept nulls for all booleans to prevent rehardcoding defaults and allow more options
        if (interWebapp == null) {
            interWebapp = Boolean.FALSE;
        }
        if (controller == null) {
            controller = Boolean.TRUE;
        }
        // SCIPIO: Code must be aware of whether these were explicitly requested or not
        // SCIPIO: NOTE: change to Boolean not fully exploited yet
        //if (fullPath == null) {
        //    fullPath = Boolean.FALSE;
        //}
        //if (secure == null) {
        //    // SCIPIO: NOTE: this does not mean the link is "insecure"!
        //    secure = Boolean.FALSE;
        //}
        if (encode == null) {
            encode = Boolean.TRUE;
        }
        
        Delegator delegator = (Delegator) request.getAttribute("delegator"); // SCIPIO: need delegator
        OfbizUrlBuilder builder = null; // SCIPIO: reuse this outside
        WebSiteProperties webSiteProps; // SCIPIO: NOTE: we *possibly* could want to accept this var as method parameter (as optimization/special), but to be safe, don't for now
        WebSiteProperties requestWebSiteProps; // SCIPIO: will always need request web site props, for comparisons

        // SCIPIO: enforce this check for time being
        if (interWebapp && webappInfo == null) {
            throw new IllegalArgumentException("Scipio: Cannot build inter-webapp URL without webapp info");
        }
        
        // SCIPIO: Sanity check: null/missing URL
        if (url == null || url.isEmpty()) {
            Debug.logError("Scipio: makeLink received null URL; returning null", module);
            return null;
        }
        
        // SCIPIO: always get current request webSiteProps
        try {
            requestWebSiteProps = WebSiteProperties.from(request);
        } catch (Exception e) { // SCIPIO: just catch everything: GenericEntityException
            // If the entity engine is throwing exceptions, then there is no point in continuing.
            Debug.logError(e, "Exception thrown while getting web site properties: ", module);
            return null;
        }
        
        // SCIPIO: Multiple possible ways to get webSiteProps
        if (interWebapp) {
            try {
                if (webappInfo != null) {
                    String webSiteId = WebAppUtil.getWebSiteId(webappInfo);
                    if (webSiteId != null && !webSiteId.isEmpty()) {
                        webSiteProps = WebSiteProperties.from(request, webSiteId);
                    }
                    else {
                        webSiteProps = WebSiteProperties.from(request);
                    }
                }
                else {
                    webSiteProps = WebSiteProperties.from(request);
                }
            } catch (Exception e) { // SCIPIO: just catch everything: GenericEntityException
                // If the entity engine is throwing exceptions, then there is no point in continuing.
                Debug.logError(e, "Exception thrown while getting web site properties: ", module);
                return null;
            }
        } else {
            // SCIPIO: stock case (get from request, or defaults)
            webSiteProps = requestWebSiteProps;
        }
        
        // SCIPIO: Special case: If we have inter-webapp, we need to check if the web site properties
        // for this link different from the current request's. If so, we have to force full-path
        // link. Here we compare the effective values to ensure correctness. 
        // TODO? It is possible we could want to always force fullPath for all inter-webapp links.
        // Maybe make a url.properties option and allow force fullPath and force secure.
        if (interWebapp) {
            if (!webSiteProps.equalsWithHardDefaults(requestWebSiteProps)) {
                fullPath = true;
            }
        }
        
        String requestUri = null;
        ConfigXMLReader.RequestMap requestMap = null;
        
        // SCIPIO: only lookup if we want to use controller
        if (controller) {
            requestUri = RequestHandler.getRequestUri(url);
            
            if (requestUri != null) {
                try {
                    // SCIPIO: Lookup correct controller for webapp
                    if (interWebapp) {
                        try {
                            requestMap = ConfigXMLReader.getControllerConfig(webappInfo).getRequestMapMap().get(requestUri);
                        } catch (MalformedURLException e) {
                            Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                            return null;
                        }
                    }
                    else {
                        // SCIPIO: stock case
                        requestMap = getControllerConfig().getRequestMapMap().get(requestUri);
                    }
                } catch (WebAppConfigurationException e) {
                    // If we can't read the controller.xml file, then there is no point in continuing.
                    Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                    return null;
                }
            }
            
            // SCIPIO: 2016-05-06: If controller requested and request could not be found, show an error and return null.
            // There is virtually no case where this is not a coding error we want to catch, and if we don't show an error,
            // then we can't use this as a security check. Likely also to make some template errors clearer.
            if (requestMap == null) {
                Debug.logError("Scipio: Cannot build link: could not locate the expected request '" + requestUri + "' in controller config", module);
                return null; 
            }
        }
        
        boolean didFullSecure = false;
        boolean didFullStandard = false;
        // SCIPIO: We need to enter even if no controller (and other cases)
        //if (requestMap != null && (webSiteProps.getEnableHttps() || fullPath || secure)) {
        // We don't need this condition anymore, because it doesn't make sense to require enableHttps to produce full path URLs
        //if (webSiteProps.getEnableHttps() || Boolean.TRUE.equals(fullPath) || Boolean.TRUE.equals(secure) || secure == null) {    
        {
            if (Debug.verboseOn()) Debug.logVerbose("In makeLink requestUri=" + requestUri, module);
            // SCIPIO: These conditions have been change (see method)
            //if (secure || (webSiteProps.getEnableHttps() && requestMap.securityHttps && !request.isSecure())) {
            //    didFullSecure = true;
            //} else if (fullPath || (webSiteProps.getEnableHttps() && !requestMap.securityHttps && request.isSecure())) {
            //    didFullStandard = true;
            //}
            Boolean secureFullPathFlag = checkFullSecureOrStandard(request, requestWebSiteProps, requestMap, interWebapp, fullPath, secure);
            if (secureFullPathFlag == Boolean.TRUE) {
                didFullSecure = true;
            } else if (secureFullPathFlag == Boolean.FALSE) {
                didFullStandard = true;
            } else {
                ;
            }
        }
        StringBuilder newURL = new StringBuilder(250);
        if (didFullSecure || didFullStandard) {
            // Build the scheme and host part
            try {
                if (builder == null) {
                    if (interWebapp) {
                        // SCIPIO: builder should be made using webappInfo if one was passed to us 
                        builder = OfbizUrlBuilder.from(webappInfo, webSiteProps, delegator);
                    } else {
                        // SCIPIO: stock case
                        builder = OfbizUrlBuilder.from(request);
                    }
                }
                builder.buildHostPart(newURL, url, didFullSecure, controller); // SCIPIO: controller flag
            } catch (GenericEntityException e) {
                // If the entity engine is throwing exceptions, then there is no point in continuing.
                Debug.logError(e, "Exception thrown while getting web site properties: ", module);
                return null;
            } catch (WebAppConfigurationException e) {
                // If we can't read the controller.xml file, then there is no point in continuing.
                Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                return null;
            } catch (IOException e) {
                // If we can't write to StringBuilder, then there is no point in continuing.
                Debug.logError(e, "Exception thrown while writing to StringBuilder: ", module);
                return null;
            } catch (SAXException e) {
                // SCIPIO: new case
                Debug.logError(e, "Scipio: Exception thrown while getting web site properties: ", module);
                return null;
            }
        }
        
        // SCIPIO: build the path part (context root, servlet/controller path)
        if (interWebapp) {
            if (builder == null) {
                try {
                    builder = OfbizUrlBuilder.from(webappInfo, webSiteProps, delegator);
                } catch (Exception e) {
                    // SCIPIO: new case
                    Debug.logError(e, "Scipio: Exception thrown while getting web site properties: ", module);
                    return null;
                }
            }

            try {
                if (controller) {
                    builder.buildPathPart(newURL, url);
                }
                else {
                    builder.buildPathPartWithContextRoot(newURL, url);
                }
            } catch (WebAppConfigurationException e) {
                // SCIPIO: new case
                Debug.logError(e, "Scipio: Exception thrown while building url path part: ", module);
                return null;
            } catch (IOException e) {
                // SCIPIO: new case
                Debug.logError(e, "Scipio: Exception thrown while building url path part: ", module);
                return null;
            }
        } else {
            if (controller) { 
                // SCIPIO: This is the original stock case: intra-webapp, controller link
                // create the path to the control servlet
                String controlPath = (String) request.getAttribute("_CONTROL_PATH_");
                newURL.append(controlPath);
            } else {
                // SCIPIO: Here we point to any servlet or file in the webapp, so only append context path
                String contextPath = request.getContextPath();
                newURL.append(contextPath.endsWith("/") ? contextPath.substring(0, contextPath.length() - 1) : contextPath);
            }
            
            // now add the actual passed url, but if it doesn't start with a / add one first
            if (!url.startsWith("/")) {
                newURL.append("/");
            }
            newURL.append(url);
        }

        String encodedUrl;
        if (encode) {
            // SCIPIO: Delegated code
            encodedUrl = doLinkURLEncode(request, response, newURL, interWebapp, didFullStandard, didFullSecure);
        } else {
            encodedUrl = newURL.toString();
        }
        //if (encodedUrl.indexOf("null") > 0) {
            //Debug.logError("in makeLink, controlPath:" + controlPath + " url:" + url, "");
            //throw new RuntimeException("in makeLink, controlPath:" + controlPath + " url:" + url);
        //}

        //Debug.logInfo("Making URL, encode=" + encode + " for URL: " + newURL + "\n encodedUrl: " + encodedUrl, module);

        return encodedUrl;
    }

    /**
     * SCIPIO: Factored-out makeLink code.
     * <p>
     * Returns null if no full required. Returns true if secure fullpath required. Returns false if standard fullpath required.
     */
    protected static Boolean checkFullSecureOrStandard(HttpServletRequest request, WebSiteProperties webSiteProps, ConfigXMLReader.RequestMap requestMap, 
            Boolean interWebapp, Boolean fullPath, Boolean secure) {
        // SCIPIO: These conditions have been change: if fullPath and target URI is secure, make secure URL instead of insecure.
        // We will NEVER build insecure URLs to requests marked secure.
        // This way, there is less control, but fullPath becomes easier and safer to use.
        // 2016-04-06: WE DO NOT DOWNGRADE CONNECTIONS WITH didFullStandard UNLESS EXPLICITLY REQUESTED
        // 2016-04-06: secure flag no longer forces a fullPath link, but we can only omit the full path in cases where we are already secure
        //if (secure || (webSiteProps.getEnableHttps() && requestMap.securityHttps && !request.isSecure())) {
        //    didFullSecure = true;
        //} else if (fullPath || (webSiteProps.getEnableHttps() && !requestMap.securityHttps && request.isSecure())) {
        //    didFullStandard = true;
        //}
        if (interWebapp == null) {
            interWebapp = Boolean.FALSE;
        }
        // 2016-07-14: NOTE: if for some reason webSiteProps was null, we assume enableHttps is true, for security reasons.
        // 2016-07-14: NOTE: if there is no request object (static rendering context), for now
        // we behave as if we had an insecure request, for better security.
        if ((Boolean.TRUE.equals(secure) && (Boolean.TRUE.equals(fullPath) || request == null || !request.isSecure())) // if secure requested, only case where don't need full path is if already secure
            || ((webSiteProps == null || webSiteProps.getEnableHttps()) && requestMap != null && requestMap.securityHttps && (request == null || !request.isSecure() || Boolean.TRUE.equals(fullPath))) // upgrade to secure target if we aren't secure or fullPath was requested (never make non-secure fullPath to secure target)
            || ((webSiteProps == null || webSiteProps.getEnableHttps()) && secure == null && Boolean.TRUE.equals(fullPath) && request != null && request.isSecure())) { // do not downgrade fullPath requests anymore, unless explicitly allowed (by passing secure false, case below)
            return Boolean.TRUE;
        } else if (Boolean.TRUE.equals(fullPath) // accept all other explicit fullPath requests
                || (requestMap != null && (Boolean.FALSE.equals(secure) && !requestMap.securityHttps && request != null && request.isSecure())) // allow downgrade from HTTPS to HTTP, but only if secure false explicitly passed and the target requestMap is not HTTPS. Also, removed this check: webSiteProps.getEnableHttps()  
                || (requestMap == null && (Boolean.FALSE.equals(secure) && request != null && request.isSecure()))) { // 2016-07-14: if there is no target requestMap or unknown, and secure=false was requested, we'll allow building a fullpath insecure link (this is acceptable only because of our widespread change making null the new default everywhere).
            return Boolean.FALSE;
        } else {
            return null;
        }
    }
    
    /**
     * SCIPIO: Factored-out makeLink code, that we must expose so other link-building code may reuse.
     * <p>
     * WARN: newURL is modified in-place, and then discarded, so only use the result. 
     */
    protected String doLinkURLEncode(HttpServletRequest request, HttpServletResponse response, StringBuilder newURL, boolean interWebapp,
            boolean didFullStandard, boolean didFullSecure) {
        String encodedUrl;
        // SCIPIO: do something different for inter-webapp links
        if (interWebapp) {
            if (response != null) {
                // SCIPIO: We want to run inter-webapp links through URL encoding for outbound-rules and things,
                // but we should never add a jsessionId.
                encodedUrl = RequestLinkUtil.encodeURLNoJsessionId(newURL.toString(), response);
            } else {
                encodedUrl = newURL.toString();    
            }
        } else {
            // SCIPIO: stock case
            boolean forceManualJsessionid = !cookies;
            boolean isSpider = false;

            // if the current request comes from a spider, we will not add the jsessionid to the link
            if (UtilHttp.checkURLforSpiders(request)) {
                isSpider = true;
            }

            // if this isn't a secure page, but we made a secure URL, make sure we manually add the jsessionid since the response.encodeURL won't do that
            if (!request.isSecure() && didFullSecure) {
                forceManualJsessionid = true;
            }

            // if this is a secure page, but we made a standard URL, make sure we manually add the jsessionid since the response.encodeURL won't do that
            if (request.isSecure() && didFullStandard) {
                forceManualJsessionid = true;
            }

            if (response != null && !forceManualJsessionid && !isSpider) {
                encodedUrl = response.encodeURL(newURL.toString());
            } else {
                if (!isSpider) {
                    String sessionId = ";jsessionid=" + request.getSession().getId();
                    // this should be inserted just after the "?" for the parameters, if there is one, or at the end of the string
                    int questionIndex = newURL.indexOf("?");
                    if (questionIndex == -1) {
                        newURL.append(sessionId);
                    } else {
                        newURL.insert(questionIndex, sessionId);
                    }
                }
                if (response != null) {
                    encodedUrl = response.encodeURL(newURL.toString());
                } else {
                    encodedUrl = newURL.toString();
                }
            }
        }
        return encodedUrl;
    }
    
    public String makeLink(HttpServletRequest request, HttpServletResponse response, String url, Boolean fullPath, Boolean secure, Boolean encode) {
        return makeLink(request, response, url, null, null, null, fullPath, secure, encode);
        
        /* SCIPIO: Below is the original implementation of this method (with this signature), for reference only; unmaintained and may become out of date
        WebSiteProperties webSiteProps = null;
        try {
            webSiteProps = WebSiteProperties.from(request);
        } catch (GenericEntityException e) {
            // If the entity engine is throwing exceptions, then there is no point in continuing.
            Debug.logError(e, "Exception thrown while getting web site properties: ", module);
            return null;
        }
        String requestUri = RequestHandler.getRequestUri(url);
        ConfigXMLReader.RequestMap requestMap = null;
        if (requestUri != null) {
            try {
                requestMap = getControllerConfig().getRequestMapMap().get(requestUri);
            } catch (WebAppConfigurationException e) {
                // If we can't read the controller.xml file, then there is no point in continuing.
                Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                return null;
            }
        }
        boolean didFullSecure = false;
        boolean didFullStandard = false;
        if (requestMap != null && (webSiteProps.getEnableHttps() || fullPath || secure)) {
            if (Debug.verboseOn()) Debug.logVerbose("In makeLink requestUri=" + requestUri, module);
            if (secure || (webSiteProps.getEnableHttps() && requestMap.securityHttps && !request.isSecure())) {
                didFullSecure = true;
            } else if (fullPath || (webSiteProps.getEnableHttps() && !requestMap.securityHttps && request.isSecure())) {
                didFullStandard = true;
            }
        }
        StringBuilder newURL = new StringBuilder(250);
        if (didFullSecure || didFullStandard) {
            // Build the scheme and host part
            try {
                OfbizUrlBuilder builder = OfbizUrlBuilder.from(request);
                builder.buildHostPart(newURL, url, didFullSecure);
            } catch (GenericEntityException e) {
                // If the entity engine is throwing exceptions, then there is no point in continuing.
                Debug.logError(e, "Exception thrown while getting web site properties: ", module);
                return null;
            } catch (WebAppConfigurationException e) {
                // If we can't read the controller.xml file, then there is no point in continuing.
                Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                return null;
            } catch (IOException e) {
                // If we can't write to StringBuilder, then there is no point in continuing.
                Debug.logError(e, "Exception thrown while writing to StringBuilder: ", module);
                return null;
            }
        }
        // create the path to the control servlet
        String controlPath = (String) request.getAttribute("_CONTROL_PATH_");
        newURL.append(controlPath);

        // now add the actual passed url, but if it doesn't start with a / add one first
        if (!url.startsWith("/")) {
            newURL.append("/");
        }
        newURL.append(url);

        String encodedUrl;
        if (encode) {
            boolean forceManualJsessionid = !cookies;
            boolean isSpider = false;

            // if the current request comes from a spider, we will not add the jsessionid to the link
            if (UtilHttp.checkURLforSpiders(request)) {
                isSpider = true;
            }

            // if this isn't a secure page, but we made a secure URL, make sure we manually add the jsessionid since the response.encodeURL won't do that
            if (!request.isSecure() && didFullSecure) {
                forceManualJsessionid = true;
            }

            // if this is a secure page, but we made a standard URL, make sure we manually add the jsessionid since the response.encodeURL won't do that
            if (request.isSecure() && didFullStandard) {
                forceManualJsessionid = true;
            }

            if (response != null && !forceManualJsessionid && !isSpider) {
                encodedUrl = response.encodeURL(newURL.toString());
            } else {
                if (!isSpider) {
                    String sessionId = ";jsessionid=" + request.getSession().getId();
                    // this should be inserted just after the "?" for the parameters, if there is one, or at the end of the string
                    int questionIndex = newURL.indexOf("?");
                    if (questionIndex == -1) {
                        newURL.append(sessionId);
                    } else {
                        newURL.insert(questionIndex, sessionId);
                    }
                }
                if (response != null) {
                    encodedUrl = response.encodeURL(newURL.toString());
                } else {
                    encodedUrl = newURL.toString();
                }
            }
        } else {
            encodedUrl = newURL.toString();
        }
        //if (encodedUrl.indexOf("null") > 0) {
            //Debug.logError("in makeLink, controlPath:" + controlPath + " url:" + url, "");
            //throw new RuntimeException("in makeLink, controlPath:" + controlPath + " url:" + url);
        //}

        //Debug.logInfo("Making URL, encode=" + encode + " for URL: " + newURL + "\n encodedUrl: " + encodedUrl, module);

        return encodedUrl;
        */
    }
    
    /**
     * SCIPIO: Builds an Ofbiz navigation link, where possible inferring <em>some</em> of its properties by analyzing the passed URI (<code>url</code>)
     * and <code>webSiteId</code>.
     * <p>
     * The <code>url</code> may be a relative URI such as controller URI, a servlet path relative to context root,
     * or a full absolute path including context root. In all cases, the method will parse the target
     * and it must point to a valid webapp on the server, as matched by mount-point.
     * <p>
     * For inter-webapp, if <code>webSiteId</code> is specified, it will determine the target webapp.
     * Some webapps don't have their own webSiteId, in which case the webapp will be inferred from
     * the absolute target link. If not enough information is available to pinpoint a web app (<code>WebappInfo<code>), 
     * the call will fail and return null.
     * <p>
     * Each of the options can be passed null to let the method figure out. If specified it 
     * will be taken into consideration. <strong>Exceptions</strong>: Currently, <code>interWebapp</code>
     * should be specified, otherwise <code>false</code> is always assumed regardless of <code>url</code> format
     * (there are currently no known cases where we need to infer this and creates ambiguities).
     * <p>
     * <strong>WARN</strong>: Currently, <code>absPath</code> is assumed to be <code>false</code>
     * for all intra-webapp links (interWebapp false), false for inter-webapp links with webSiteId specified,
     * and true for inter-webapp links without webSiteId specified. Is it NOT reactive to format of passed url.
     * <p>
     * <strong>WARN</strong>: Due to technical limitations (notably Java servlet spec), this method may
     * be forced to make inexact assumptions, which is one reason why it is implemented as a distinct method.
     * 
     * @param request
     * @param response
     * @param url
     * @param absPath
     * @param interWebapp
     * @param webSiteId
     * @param controller
     * @param fullPath
     * @param secure
     * @param encode
     * @return
     * 
     * @see #makeLink(HttpServletRequest, HttpServletResponse, String, boolean, WebappInfo, boolean, boolean, boolean, boolean)
     */
    public String makeLinkAuto(HttpServletRequest request, HttpServletResponse response, String url, Boolean absPath, Boolean interWebapp, String webSiteId, Boolean controller, 
            Boolean fullPath, Boolean secure, Boolean encode) {
        
        boolean absControlPathChecked = false;
        boolean absContextPathChecked = false;
        
        // Check inter-webapp
        if (interWebapp == null) {
            // FIXME? For now, can assume false unless requested otherwise.
            interWebapp = Boolean.FALSE;
            
            /* This is incomplete or invalid... don't always have webSiteId...
             * would have to compare WebappInfo from request to ours.. but the tests become circular...
            boolean sameWebSite = true;
            if (webSiteId != null && !webSiteId.isEmpty()) {
                String currWebSiteId = WebSiteWorker.getWebSiteId(request);
                if (currWebSiteId != null && !currWebSiteId.isEmpty()) {
                    sameWebSite = webSiteId.equals(currWebSiteId);
                }
            }
            */
        }
        
        // Check if absolute path
        if (absPath == null) {
            // FIXME?: Current default behavior is predictable but is non-reactive to the url format.
            // It is safer this way but templates must be aware...
            // I don't think safe to use the starting "/" as indicator...
            if (interWebapp) {
                if (webSiteId != null && !webSiteId.isEmpty()) {
                    absPath = false;
                } else {
                    absPath = true;
                }
            } else {
                // For non-inter-webapp links, just assume is relative.
                absPath = false;
            }
        }
        
        // Get target webapp info
        WebappInfo webappInfo = null;
        if (interWebapp) {
            if (webSiteId != null && !webSiteId.isEmpty()) {
                try {
                    webappInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
                } catch (Exception e) {
                    Debug.logError(e, module);
                    return null;
                }
            } else {
                // We should have an absolute path here. If not, we won't have enough info
                // to build the link.
                try {
                    webappInfo = WebAppUtil.getWebappInfoFromPath(url);
                } catch (Exception e) {
                    Debug.logError(e, "Scipio: Could not get webapp info from absolute path '" + url + "'", module);
                    return null;
                }
                absContextPathChecked = true; // since we built the webapp info from the URL, this is guaranteed fine
            }
        } else {
            try {
                webappInfo = WebAppUtil.getWebappInfoFromRequest(request);
            } catch (Exception e) {
                Debug.logError(e, "Scipio: Could not get webapp info from request (url: " + url + ")", module);
                return null;
            }
        }
        String contextPath = webappInfo.getContextRoot();
        if (!contextPath.endsWith("/")) {
            contextPath += "/";
        }
        
        // Check if controller should be used
        String controlPath = null;
        if (controller == null) {
            // in some cases we need to infer this...
            if (absPath) {
                controlPath = WebAppUtil.getControlServletPathSafeSlash(webappInfo);
                controller = (controlPath != null && url.startsWith(controlPath));
                absControlPathChecked = true;
            }
            else {
                // If intra-webapp and this boolean wasn't set, by default we assume TRUE (stock)
                // This means if want non-controller intra-webapp, it must be requested with controller=false
                controller = Boolean.TRUE;
            }
        }
        if (controller && controlPath == null) {
            // In some cases need to get controlPath AND this provides a sanity check
            controlPath = WebAppUtil.getControlServletPathSafeSlash(webappInfo);
            if (controlPath == null) {
                Debug.logError("Scipio: In makeLinkAuto, trying to make a controller "
                        + "link for a webapp that has no valid controller (" + webappInfo.getName() + ")", module);
                return null;
            }
        }
        
        // Sanity check only, for absolute paths (NOTE: slows things down... but do it for now);
        // make sure it starts with the same webapp we're targeting
        if (absPath) {
            if (controller) {
                if (!absControlPathChecked) {
                    if (!url.startsWith(controlPath)) {
                        Debug.logError("Scipio: In makeLinkAuto, trying to make a controller "
                                + "link using absolute path url, but prefix does not match (url: " + url + ", control path: " + controlPath + ")", module);
                        return null;
                    }
                    absControlPathChecked = true;
                }
            }
            else {
                if (!absContextPathChecked) {
                    if (!url.startsWith(contextPath)) {
                        Debug.logError("Scipio: In makeLinkAuto, trying to make a webapp "
                                + "link using absolute path url, but context root does not match (url: " + url + ", context path: " + contextPath + ")", module);
                        return null;
                    }
                    absContextPathChecked = true;
                }
            }
        }
        
        // Extract the relative part of the URL; this simplifies the makeLink function (and moves sanity checks here)
        String relUrl;
        if (absPath) {
            if (controller) {
                relUrl = url.substring(controlPath.length());
            }
            else {
                relUrl = url.substring(contextPath.length());
            }
        } else {
            relUrl = url;
        }

        return makeLink(request, response, relUrl, interWebapp, webappInfo, controller, fullPath, secure, encode);
    }

    /**
     * SCIPIO: Builds an Ofbiz navigation link, where possible inferring <em>some</em> of its properties by analyzing the passed URI (<code>url</code>)
     * and <code>webSiteId</code>.
     * 
     * @see #makeLinkAuto(HttpServletRequest, HttpServletResponse, String, Boolean, Boolean, String, Boolean, Boolean, Boolean, Boolean)
     */
    public String makeLinkAuto(HttpServletRequest request, HttpServletResponse response, String url, Boolean absPath, Boolean interWebapp, String webSiteId, Boolean controller) {
        return makeLinkAuto(request, response, url, absPath, interWebapp, webSiteId, controller, null, null, null);
    }
    
    /**
     * SCIPIO: Builds an Ofbiz navigation full link (with HTTPS as necessary), where possible inferring <em>some</em> of its properties by analyzing the passed URI (<code>url</code>)
     * and <code>webSiteId</code>.
     * 
     * @see #makeLinkAuto(HttpServletRequest, HttpServletResponse, String, Boolean, Boolean, String, Boolean, Boolean, Boolean, Boolean)
     */
    public String makeLinkAutoFull(HttpServletRequest request, HttpServletResponse response, String url, Boolean absPath, Boolean interWebapp, String webSiteId, Boolean controller) {
        return makeLinkAuto(request, response, url, absPath, interWebapp, webSiteId, controller, true, null, null);
    }    

    /**
     * Builds an Ofbiz URL.
     * <p>
     * SCIPIO: This is modified to pass encode <code>true</code> (<code>null</code>) instead of <code>false</code>.
     * This is <string>necessary</strong> to achieve filter hooks.
     * <strong>WARN</strong>: This may lead to extra jsessionid added in some cases.
     * See {@link #makeLink(HttpServletRequest, HttpServletResponse, String, boolean, WebappInfo, boolean, boolean, boolean, boolean)} for details.
     */
    public static String makeUrl(HttpServletRequest request, HttpServletResponse response, String url) {
        // SCIPIO: Pass encode = true
        //return makeUrl(request, response, url, false, false, false);
        return makeUrl(request, response, url, null, null, null);
    }

    public static String makeUrl(HttpServletRequest request, HttpServletResponse response, String url, Boolean fullPath, Boolean secure, Boolean encode) {
        ServletContext ctx = (ServletContext) request.getAttribute("servletContext");
        RequestHandler rh = (RequestHandler) ctx.getAttribute("_REQUEST_HANDLER_");
        return rh.makeLink(request, response, url, fullPath, secure, encode);
    }
    
    /**
     * SCIPIO: Builds a full-path link (HTTPS as necessary).
     */
    public static String makeUrlFull(HttpServletRequest request, HttpServletResponse response, String url) {
        return makeUrl(request, response, url, true, null, null);
    }    
    

    public void runAfterLoginEvents(HttpServletRequest request, HttpServletResponse response) {
        try {
            for (ConfigXMLReader.Event event: getControllerConfig().getAfterLoginEventList().values()) {
                try {
                    String returnString = this.runEvent(request, response, event, null, "after-login");
                    if (returnString != null && !returnString.equalsIgnoreCase("success")) {
                        throw new EventHandlerException("Pre-Processor event did not return 'success'.");
                    }
                } catch (EventHandlerException e) {
                    Debug.logError(e, module);
                }
            }
        } catch (WebAppConfigurationException e) {
            Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
        }
    }

    public void runBeforeLogoutEvents(HttpServletRequest request, HttpServletResponse response) {
        try {
            for (ConfigXMLReader.Event event: getControllerConfig().getBeforeLogoutEventList().values()) {
                try {
                    String returnString = this.runEvent(request, response, event, null, "before-logout");
                    if (returnString != null && !returnString.equalsIgnoreCase("success")) {
                        throw new EventHandlerException("Pre-Processor event did not return 'success'.");
                    }
                } catch (EventHandlerException e) {
                    Debug.logError(e, module);
                }
            }
        } catch (WebAppConfigurationException e) {
            Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
        }
    }

    public boolean trackStats(HttpServletRequest request) {
        if (trackServerHit) {
            String uriString = RequestHandler.getRequestUri(request.getPathInfo());
            if (uriString == null) {
                uriString="";
            }
            ConfigXMLReader.RequestMap requestMap = null;
            try {
                requestMap = getControllerConfig().getRequestMapMap().get(uriString);
            } catch (WebAppConfigurationException e) {
                Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
            }
            if (requestMap == null) return false;
            return requestMap.trackServerHit;
        } else {
            return false;
        }
    }

    public boolean trackVisit(HttpServletRequest request) {
        if (trackVisit) {
            String uriString = RequestHandler.getRequestUri(request.getPathInfo());
            if (uriString == null) {
                uriString="";
            }
            ConfigXMLReader.RequestMap requestMap = null;
            try {
                requestMap = getControllerConfig().getRequestMapMap().get(uriString);
            } catch (WebAppConfigurationException e) {
                Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
            }
            if (requestMap == null) return false;
            return requestMap.trackVisit;
        } else {
            return false;
        }
    }
    
    /**
     * SCIPIO: Utility method that can be used for security checks to check if controller of current webapp
     * has the specified URI and allows direct/public access.
     */
    public static boolean controllerHasRequestUriDirect(HttpServletRequest request, String uri) {
        if (request == null) {
            return false;
        }
        RequestHandler rh = RequestHandler.getRequestHandler(request.getServletContext());
        return rh.controllerHasRequestUriDirect(uri);
    }
    
    /**
     * SCIPIO: Utility method that can be used for security checks to check if controller of current webapp
     * has the specified URI and allows direct/public access.
     */
    public boolean controllerHasRequestUriDirect(String uri) {
        try {
            ConfigXMLReader.RequestMap requestMap = getControllerConfig().getRequestMapMap().get(uri);
            
            if (requestMap != null && requestMap.securityDirectRequest) {
                return true;
            }
            
        } catch (Exception e) {
            ;
        }
        return false;
    }
    
    /**
     * SCIPIO: Necessary accessor method for external code.
     */
    public boolean isUseCookies() {
        return cookies;
    }
    
    /**
     * SCIPIO: Returns the static charset (only).
     */
    public String getCharset() {
        return charset;
    }
}

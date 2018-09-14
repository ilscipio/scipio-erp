package com.ilscipio.scipio.ce.webapp.filter.urlrewrite.local;

import java.net.MalformedURLException;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;
import org.ofbiz.security.SecurityConfigurationException;
import org.ofbiz.security.SecurityFactory;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.OfbizUrlBuilder;
import org.ofbiz.webapp.control.ContextFilter;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.renderer.RenderEnvType;
import org.ofbiz.webapp.website.WebSiteWorker;

import com.ilscipio.scipio.ce.webapp.filter.UrlFilterHelper;

/**
 * LocalServletContainer - Local servlet API implementation for urlrewrite emulation.
 * <p>
 * DEV NOTE: DO NOT REMOVE THE <code>@SuppressWarnings("deprecation")</code>
 * - due to special library setup with ivy, the IDE does not see the exact same
 * interfaces that are loaded at runtime for Tomcat; the suppress
 * is needed for javac. EDIT: Added "all", as a rare exception for this case.
 */
@SuppressWarnings({ "all", "deprecation" })
public class LocalServletContainer {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    //private static final Set<String> srcReqAttrToSkip = UtilMisc.unmodifiableHashSet("locale", "delegator",
    //        "dispatcher", "security", "timeZone", "servletContext", "_CONTEXT_ROOT_", "_SERVER_ROOT_URL_",
    //        "_CONTROL_PATH_", "_REQUEST_HANDLER_");

    protected FullWebappInfo webappInfo;

    protected final LocalHttpServletRequest request;
    protected final LocalHttpServletResponse response;
    protected final LocalHttpSession session;
    protected final LocalServletContext servletContext;

    protected LocalServletContainer(FullWebappInfo webappInfo, Locale locale, String requestURL, String servletPath,
            Map<String, Object> reqAttribs, Map<String, String[]> headers, Map<String, String[]> reqParams) throws MalformedURLException {
        this.webappInfo = webappInfo;

        Map<String, String> scInitParams = new HashMap<>(webappInfo.getWebXml().getContextParams());
        LocalServletContext servletContext = new LocalServletContext(this, null, scInitParams);
        this.servletContext = servletContext;

        LocalHttpSession session = new LocalHttpSession(this, servletContext, null, null);
        this.session = session;

        LocalHttpServletRequest request = new LocalHttpServletRequest(this, session, servletContext,
                requestURL, servletPath, reqAttribs, reqParams, headers, locale, "utf-8", "text/html; charset=UTF-8");
        this.request = request;

        LocalHttpServletResponse response = new LocalHttpServletResponse(this, "utf-8", "text/html; charset=UTF-8", locale);
        this.response = response;
    }

    public static LocalServletContainer fromRequest(FullWebappInfo webappInfo,
            HttpServletRequest request, HttpServletResponse response) {

        // re-emulate context here, backward - easier than making a ton of overloads
        Map<String, Object> context = new HashMap<>();

        context.put("locale", UtilHttp.getLocaleExistingSession(request));
        context.put("delegator", request.getAttribute("delegator"));
        context.put("dispatcher", request.getAttribute("dispatcher"));
        context.put("security", request.getAttribute("security"));
        context.put("timeZone", request.getAttribute("timeZone"));
        context.put("request", request);
        context.put("response", response);
        HttpSession session = request.getSession(false);
        if (session != null) {
            context.put("userLogin", session.getAttribute("userLogin"));
        }

        // TODO: REVIEW: must transfer headers for X-Forwarded-xxx at least...
        // but the others might not make sense for the target webapp!!
        Map<String, String[]> headers = new HashMap<>();
        Enumeration<String> headerNames = request.getHeaderNames();
        while(headerNames.hasMoreElements()) {
            String headerName = headerNames.nextElement();
            Enumeration<String> headerValuesEnu = request.getHeaders(headerName);
            if (headerValuesEnu.hasMoreElements()) {
                List<String> headerValues = Collections.list(headerValuesEnu);
                headers.put(headerName, headerValues.toArray(new String[headerValues.size()]));
            }
        }

        // TODO: REVIEW: parameters? attributes? do they make sense to transfer over?

        return fromContext(webappInfo, context, RenderEnvType.WEBAPP, null, null, headers);
    }

    public static LocalServletContainer fromContext(FullWebappInfo webappInfo,
            Map<String, Object> context, RenderEnvType renderEnvType) {
        return fromContext(webappInfo, context, renderEnvType, null, null, null);
    }

    public static LocalServletContainer fromContext(FullWebappInfo webappInfo,
            Map<String, Object> context, RenderEnvType renderEnvType, Map<String, Object> reqAttribs,
            Map<String, String[]> reqParams, Map<String, String[]> headers) {

        Locale locale = (Locale) context.get("locale");
        if (locale == null) {
            Debug.logWarning("No locale found in context for LocalServletContainer; using default", module);
            Locale.getDefault();
        }

        StringBuilder requestUrl = new StringBuilder();
        String servletPath = "";
        try {
            OfbizUrlBuilder urlBuilder = webappInfo.getOfbizUrlBuilder();
            urlBuilder.buildHostPart(requestUrl, "main", true, true);
            urlBuilder.buildPathPartNoPathPrefix(requestUrl, "main");
            servletPath = urlBuilder.getContextAndServletPath().substring(urlBuilder.getContextPath().length());
        } catch (Exception e) {
            throw new IllegalStateException("Could not build a dummy URL for webapp " + webappInfo);
        }

        LocalServletContainer container;
        try {
            container = new LocalServletContainer(webappInfo, locale, requestUrl.toString(), servletPath, reqAttribs, headers, reqParams);
        } catch (MalformedURLException e) {
            throw new IllegalStateException("Could not build a valid dummy URL for webapp " + webappInfo + ": " + requestUrl);
        }
        container.setupFromOfbizContext(context, renderEnvType);

        // set this for UrlFilterHelper
        container.getRequest().setAttribute(UrlFilterHelper.URLREWRITE_CONF_WEBAPP, webappInfo);

        return container;
    }

    /**
     * APPROXIMATION of ofbiz setup of request, session and servlet context.
     */
    protected void setupFromOfbizContext(Map<String, Object> context, RenderEnvType renderEnvType) {
        setupServletContextCatalina();
        setupServletContextContextFilter();
        setupRequestContextFilter();
        setupRequestControlServlet();

        // override these in request and session if present...
        Delegator delegator = (Delegator) context.get("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) context.get("dispatcher");
        Security security = (Security) context.get("security");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (delegator != null) {
            request.setAttribute("delegator", delegator);
            session.setAttribute("delegatorName", delegator.getDelegatorName());
        }
        if (dispatcher != null) {
            request.setAttribute("dispatcher", delegator);
        }
        if (security != null) {
            request.setAttribute("security", security);
        }
        session.setAttribute("userLogin", userLogin);
    }

    protected void setupServletContextCatalina() {
        String serverId = (String) getServletContext().getAttribute("_serverId");
        if (serverId == null) {
            getServletContext().setAttribute("_serverId", getWebappInfo().getServerId());
        }

        // TODO?: could be more needed...
    }

    protected void setupServletContextContextFilter() {
        // FIXME massive copy paste for now, due to old ContextFilter code

        LocalServletContainer config = this; // hack

        Enumeration<String> initParamEnum = UtilGenerics.cast(config.getServletContext().getInitParameterNames());
        while (initParamEnum.hasMoreElements()) {
            String initParamName = initParamEnum.nextElement();
            String initParamValue = config.getServletContext().getInitParameter(initParamName);
            if (Debug.verboseOn()) Debug.logVerbose("Adding web.xml context-param to application attribute with name [" + initParamName + "] and value [" + initParamValue + "]", module);
            config.getServletContext().setAttribute(initParamName, initParamValue);
        }

        String serverId = (String) config.getServletContext().getAttribute("_serverId");
        if (serverId == null) {
            serverId = config.getServletContext().getInitParameter("ofbizServerName");
            config.getServletContext().setAttribute("_serverId", serverId);
        }

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

        Security security = (Security) config.getServletContext().getAttribute("security");
        if (security == null) {
            delegator = (Delegator) config.getServletContext().getAttribute("delegator");

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

        LocalDispatcher dispatcher = (LocalDispatcher) servletContext.getAttribute("dispatcher");
        if (dispatcher == null) {
            dispatcher = ContextFilter.makeWebappDispatcher(servletContext, delegator);
            servletContext.setAttribute("dispatcher", dispatcher);
        }
    }

    protected void setupRequestContextFilter() {
        LocalServletContainer config = this; // hack
        LocalHttpServletRequest httpRequest = request;


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

        request.setAttribute("delegator", config.getServletContext().getAttribute("delegator"));
        request.setAttribute("dispatcher", config.getServletContext().getAttribute("dispatcher"));
        request.setAttribute("security", config.getServletContext().getAttribute("security"));
    }

    protected void setupRequestControlServlet() {
        RequestHandler requestHandler = this.getRequestHandler();
        HttpSession session = request.getSession();

        // setup DEFAULT character encoding and content type, this will be overridden in the RequestHandler for view rendering
        String charset = request.getCharacterEncoding();

        // setup content type
        String contentType = "text/html";
        if (UtilValidate.isNotEmpty(charset) && !"none".equals(charset)) {
            response.setContentType(contentType + "; charset=" + charset);
            response.setCharacterEncoding(charset);
        } else {
            response.setContentType(contentType);
        }

        String rname = "";
        if (request.getPathInfo() != null) {
            rname = request.getPathInfo().substring(1);
        }
        if (rname.indexOf('/') > 0) {
            rname = rname.substring(0, rname.indexOf('/'));
        }

        // Setup the CONTROL_PATH for JSP dispatching.
        String contextPath = request.getContextPath();
        if (contextPath == null || "/".equals(contextPath)) {
            contextPath = "";
        }
        request.setAttribute("_CONTROL_PATH_", contextPath + request.getServletPath());
        if (Debug.verboseOn())
            Debug.logVerbose("Control Path: " + request.getAttribute("_CONTROL_PATH_"), module);

        // for convenience, and necessity with event handlers, make security and delegator available in the request:
        // try to get it from the session first so that we can have a delegator/dispatcher/security for a certain user if desired
        Delegator delegator = null;
        String delegatorName = (String) session.getAttribute("delegatorName");
        if (UtilValidate.isNotEmpty(delegatorName)) {
            delegator = DelegatorFactory.getDelegator(delegatorName);
        }
        if (delegator == null) {
            delegator = (Delegator) getServletContext().getAttribute("delegator");
        }
        if (delegator == null) {
            Debug.logError("[ControlServlet] ERROR: delegator not found in ServletContext", module);
        } else {
            request.setAttribute("delegator", delegator);
            // always put this in the session too so that session events can use the delegator
            session.setAttribute("delegatorName", delegator.getDelegatorName());
            /* Uncomment this to enable the EntityClassLoader
            ClassLoader loader = EntityClassLoader.getInstance(delegator.getDelegatorName(), Thread.currentThread().getContextClassLoader());
            Thread.currentThread().setContextClassLoader(loader);
            */
        }

        LocalDispatcher dispatcher = (LocalDispatcher) session.getAttribute("dispatcher");
        if (dispatcher == null) {
            dispatcher = (LocalDispatcher) getServletContext().getAttribute("dispatcher");
        }
        if (dispatcher == null) {
            Debug.logError("[ControlServlet] ERROR: dispatcher not found in ServletContext", module);
        }
        request.setAttribute("dispatcher", dispatcher);

        Security security = (Security) session.getAttribute("security");
        if (security == null) {
            security = (Security) getServletContext().getAttribute("security");
        }
        if (security == null) {
            Debug.logError("[ControlServlet] ERROR: security not found in ServletContext", module);
        }
        request.setAttribute("security", security);

        request.setAttribute("_REQUEST_HANDLER_", requestHandler);


        // setup some things that should always be there
        UtilHttp.setInitialRequestInfo(request);

        // some containers call filters on EVERY request, even forwarded ones, so let it know that it came from the control servlet
        request.setAttribute(ContextFilter.FORWARDED_FROM_SERVLET, Boolean.TRUE);

    }

    public FullWebappInfo getWebappInfo() {
        return webappInfo;
    }

    public void setWebappInfo(FullWebappInfo webappInfo) {
        this.webappInfo = webappInfo;
    }

    public LocalHttpServletRequest getRequest() {
        return request;
    }

    public LocalHttpServletResponse getResponse() {
        return response;
    }

    public LocalHttpSession getSession() {
        return session;
    }

    public LocalServletContext getServletContext() {
        return servletContext;
    }

    protected RequestHandler getRequestHandler() {
        return RequestHandler.getRequestHandler(getServletContext());
    }
}

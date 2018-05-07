package com.ilscipio.scipio.cms.control;

import java.io.IOException;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletRequestWrapper;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpServletResponseWrapper;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.webapp.control.RequestHandler;

import com.ilscipio.scipio.ce.util.PathUtil;
import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.control.CmsProcessMapping.ProcessMappingWorker.FindByRequestResult;
import com.ilscipio.scipio.cms.control.cmscall.CmsCallType;
import com.ilscipio.scipio.cms.data.CmsDataObject;
import com.ilscipio.scipio.cms.webapp.CmsWebappUtil;

/**
 * Cms process mapping filter.
 * <p>
 * NOTE: 2016: client webapps should include this in web.xml.
 * <p>
 * This filter implements the request URI forwarding functionality of Cms's process mappings.
 * It matches the current request against process mapping source paths and if matching
 * forwards to the found mapping's forward path.
 * <p>
 * NOTE: 2016: As indicated below, currently (2016-12-02), this expects to run before {@link org.ofbiz.webapp.control.ContextFilter},
 * which must also respond to FORWARD dispatcher (this is safe because ContextFilter has a double-included guard for forwards).
 * <p>
 * TODO?: For flexibility reasons, may be desirable for this to be able to support running
 * post-ContextFilter as well, but this would require some kind of cooperation with ContextFilter,
 * or alternatively, have CmsProcessFilter implement the full ContextFilter itself.
 * As-is, running post-ContextFilter will prevent many process mappings paths from working.
 * If could include the full ContextFilter implementation, it would allow omitted the FORWARD dispatcher,
 * which might simplify filters in some web.xmls.
 * <p>
 * Include:
 * <pre>
 * {@code
 *   <filter>
 *     <filter-name>CmsProcessMappingFilter</filter-name>
 *     <display-name>CmsProcessMappingFilter</display-name>
 *     <filter-class>com.ilscipio.scipio.cms.control.CmsProcessMappingFilter</filter-class> 
 *     <init-param><param-name>defaultForwardServletPathOverride</param-name><param-value></param-value></init-param>   
 *     <init-param><param-name>defaultForwardExtraPathInfo</param-name><param-value>true</param-value></init-param>     
 *   </filter>    
 *   <filter>
 *     <filter-name>ContextFilter</filter-name>
 *     <display-name>ContextFilter</display-name>
 *     <filter-class>org.ofbiz.webapp.control.ContextFilter</filter-class>
 *     <!-- [...] -->
 *   </filter>
 *   <!-- [...] -->
 *   <filter-mapping>
 *     <filter-name>CmsProcessMappingFilter</filter-name>
 *     <url-pattern>/*</url-pattern>
 *     <dispatcher>REQUEST</dispatcher>  
 *     <dispatcher>FORWARD</dispatcher>   
 *   </filter-mapping>
 *   <filter-mapping>
 *     <filter-name>ContextFilter</filter-name>
 *     <url-pattern>/*</url-pattern>
 *     <dispatcher>REQUEST</dispatcher>  
 *     <dispatcher>FORWARD</dispatcher>   
 *   </filter-mapping>
 * }
 * </pre>
 * <p>
 * WARN/FIXME?: The delegator lookup will be one request out of sync for tenant delegator switches
 * because/when the process filter is run before ofbiz's ContextFilter.
 * It is not clear in which circumstance this could be a problem.
 * 2016: would have to cooperate with ContextFilter to solve, or include its full implementation (which must be disable-able).
 */
public class CmsProcessFilter implements Filter {

    public static final String module = CmsProcessFilter.class.getName();

    //private FilterConfig config = null;
    private ServletContext servletContext;
    private CmsWebSiteConfig webSiteConfig = CmsWebSiteConfig.getDefault();
    
    /* Dev note: To disable the process rewriting, set the two following booleans to false: */
    private boolean forwardSourcePathParam = true;
    private boolean urlRewriteSourcePath = true;
    private boolean denyExternalRewriteSourcePathRequests = true;
    
    // finally removed
//    // 2016: legacy lookup enabled for now, copied from old CmsControlServlet
//    private final boolean legacyPageLookup = false; // TODO: disable/remove
//    // NOTE: 2016: we now have multiple possible targets (cmsPageXxx); use plain no auth for now for legacy cases
//    protected static String cmsDefaultForwardPath = "/control/cmsPagePlainNoAuth";
    
    @Override
    public void init(FilterConfig config) throws ServletException {
        //this.config = config;
        this.servletContext = config.getServletContext();

        // hasControllerHint false because process filter being present doesn't really guarantee a controller is there
        CmsWebSiteInfo webSiteInfo = CmsWebSiteInfo.registerCmsWebSite(this.servletContext, false);
        this.webSiteConfig = CmsWebSiteInfo.getWebSiteConfigOrDefaults(webSiteInfo, servletContext);
    }

    @Override
    public void doFilter(ServletRequest req, ServletResponse res, FilterChain chain) throws IOException, ServletException {
        HttpServletRequest request = (HttpServletRequest) req;
        HttpServletResponse response = (HttpServletResponse) res; 
        
        boolean cmsRequestForwarded = Boolean.TRUE.equals(req.getAttribute("cmsRequestForwarded"));
        boolean cmsRequestChained = Boolean.TRUE.equals(req.getAttribute("cmsRequestChained"));
        boolean cmsRequestVisited = (cmsRequestForwarded || cmsRequestChained);
        if (cmsRequestVisited) {
            // SPECIAL: we cannot set these below, because they have to match the TARGET servlet, not the incoming one.
            // as a workaround, we set at the beginning here so that when the forward happens, we will intercept the forward
            // and get the updated target values.
            // we cannot do this from the view handler because the controller wouldn't get them to save in session on redirects/forwards.
            // NOTE: we don't test for cmsRequestServletPath/cmsRequestPath presence because we need the values
            // from the LAST forward, not the first.
            // NOTE: this depends on the process filter being chained to FORWARD dispatcher
            if (request.getAttribute("cmsProcessMapping") != null) {
                request.setAttribute("cmsRequestServletPath", CmsControlUtil.normalizeServletPathNoNull(request.getServletPath()));
                request.setAttribute("cmsRequestPath", CmsControlUtil.normalizeServletRootRequestPathNoNull(request.getPathInfo()));
            }
            
            if (cmsRequestForwarded) {
                // ONLY SUPPORT ONE PROCESS MAP MATCHING PER REQUEST (LIMITATION)
                // Currently, should prevent self-forwards from invoking a second time because
                // if matched twice, attributes set can currently only reflect one mapping at a time.
                // Or maybe just that the behavior of two invocations is not well defined, so prevent for now.
                chain.doFilter(req, res);
                return;
            }
        }
 
        CmsProcessMapping mapping = null;
        
        // NOTE: 2016: currently no need to strip the preview path from this path; done in the findByRequest lookup
        String mappingPath = CmsProcessMapping.getMappingPath(request);
        String origSourcePath = CmsProcessMapping.getMappingPath(request, false, false);
        String extraPathInfo = null;
        boolean mappingActive = false;
        String processSourcePath = null;
        String relRequestUrl = request.getRequestURI() + (request.getQueryString() != null ? ("?" + request.getQueryString()) : "");
        
        if (!cmsRequestVisited) {
            request.setAttribute("cmsOrigRequestUri", request.getRequestURI());
            request.setAttribute("cmsOrigRequestContextPath", request.getContextPath());
            request.setAttribute("cmsOrigRequestSourcePath", origSourcePath);
            request.setAttribute("cmsOrigRequestQueryString", request.getQueryString());
            
            if (webSiteConfig.isSetResponseBrowserNoCache()) {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Setting browser no-proxy no-cache response" + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                CmsControlUtil.checkSetNoCacheResponse(request, response);
            }
        }
        
        if (CmsUtil.verboseOn()) {
            Debug.logInfo("Cms: Process Mapping Request: " + mappingPath + CmsControlUtil.getReqLogIdDelimStr(request), module);
        }

        // SECURITY PRECAUTION
        // If we enabled the outbound URL-rewriting scheme, we should not permit incoming
        // requests from an external source with parameters that might influence internal URL rewriting.
        // That parameter name is reserved - see CmsProcessRequestWrapper.
        if (denyExternalRewriteSourcePathRequests && (forwardSourcePathParam || urlRewriteSourcePath)) {
            // Only check if coming from external, in case filter gets called multiple times
            if (!cmsRequestVisited) {
                if (request.getParameter("cmsProcessSourcePath") != null) {
                    Debug.logError("Cms: Security: Illegal request: contains parameter cmsProcessSourcePath " +
                            "reserved for internal use: " + relRequestUrl + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    try {
                        response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                    } catch (IOException e1) {
                        Debug.logError(e1, "Cms: Error sending server error response" + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    }
                    return;
                }
            }
        }
        
        // WARN/FIXME?: The delegator lookup will be one request out of sync for tenant delegator switches
        // when the process filter run before ofbiz's ContextFilter.
        Delegator delegator = CmsControlUtil.getDelegatorForControl(request, servletContext);
        String webSiteId = CmsControlUtil.getWebSiteIdForControl(request, servletContext);
        // 2016: check render mode
        CmsCallType renderMode = CmsControlUtil.getRenderMode(request, webSiteConfig);
        
        // 2017-11: _SCP_FWDROOTURIS_ instructs ContextFilter (Scipio feature) to forward these root request URIs,
        // if configured to do so using forwardRootControllerUris.
        // This allows CMS mappings to follow the controller URI config.
        if (request.getAttribute("cmsControlUris") == null) {
            Set<String> cmsControlUris = CmsProcessMapping.getWorker().getRequestUrisUnderControl(delegator, webSiteId, RequestHandler.getControlServletPath(request), 
                    webSiteConfig.getDefaultSourceServletPath(), webSiteConfig.getDefaultSourceFromContextRoot(), renderMode.cachingAllowed());
            request.setAttribute("cmsControlUris", cmsControlUris);
            if (request.getAttribute("_SCP_FWDROOTURIS_") == null) {
                request.setAttribute("_SCP_FWDROOTURIS_", cmsControlUris);
            } else {
                @SuppressWarnings("unchecked")
                Set<String> uris = new HashSet<String>((Set<String>) request.getAttribute("_SCP_FWDROOTURIS_"));
                uris.addAll(cmsControlUris);
                request.setAttribute("_SCP_FWDROOTURIS_", uris);
            }
        }

        String requestPath;
        try {
            requestPath = CmsProcessMapping.getRequestPath(request);

            FindByRequestResult findRes = CmsProcessMapping.getWorker().findByRequestPath(delegator, requestPath, webSiteId, 
                    webSiteConfig.getDefaultSourceServletPath(), webSiteConfig.getDefaultSourceFromContextRoot(),
                    renderMode.isPreview(), renderMode.cachingAllowed(),
                    request);
            mapping = findRes.mapping;
            if (mapping != null) {
                mappingActive = mapping.isActiveLogical();
                extraPathInfo = findRes.extraPathInfo;
                processSourcePath = findRes.requestPath;
            }
        } catch (Exception e) {
            // an exception is thrown, return a 500 error
            Debug.logError(e, "Cms: Error retrieving page from database. URI: " + mappingPath + CmsControlUtil.getReqLogIdDelimStr(request), module);
            
            try {
                response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            } catch (IOException e1) {
                Debug.logError(e1, "Cms: Error sending server error response" + CmsControlUtil.getReqLogIdDelimStr(request), module);
            }
            return;
        }
        
        boolean extraPathReqOk = true;
        if (mapping != null) {
            Boolean requireExtraPathInfo = mapping.getRequireExtraPathInfo();
            if (requireExtraPathInfo == null) {
                requireExtraPathInfo = CmsProcessMapping.defaultRequireExtraPathInfo;
            }
            if (requireExtraPathInfo && UtilValidate.isEmpty(extraPathInfo)) {
                extraPathReqOk = false;
            }
        }
        
        if (mapping == null || !(mappingActive || renderMode.isPreview()) || !extraPathReqOk) {
            if (CmsUtil.verboseOn()) {
               if (mapping == null) {
                   Debug.logInfo("Cms: Process Mapping Request: No mapping found for " + mappingPath + 
                           "; continuing" + CmsControlUtil.getReqLogIdDelimStr(request), module);
               } else if (!(mappingActive || renderMode.isPreview())) {
                   Debug.logInfo("Cms: Process Mapping Request: Found mapping for " + mappingPath + 
                           "(" + mapping.getLogIdRepr() + "), but inactive" + "; continuing" + CmsControlUtil.getReqLogIdDelimStr(request), module);
               } else if (!extraPathReqOk) {
                   Debug.logInfo("Cms: Process Mapping Request: Found mapping for " + mappingPath + 
                           "(" + mapping.getLogIdRepr() + "), but mapping requires extra path info and " +
                           "none present in source path; continuing" + CmsControlUtil.getReqLogIdDelimStr(request), module);
               }
            }
            
            // We don't have a control servlet setting this anymore...
            //boolean wasForwarded = Boolean.TRUE.equals((Boolean) request.getAttribute("_HTTP_FORWARDED_"));
            
            if (!response.isCommitted()) { // || wasForwarded
                // Note: wrap response here to URL rewriting may happen post-login or basically all the time
                req.setAttribute("cmsRequestChained", Boolean.TRUE);
                chain.doFilter(req, getChainedResponse(req, res, cmsRequestVisited));
            }
        } else {
            String effectiveTargetServletPath;
            if (webSiteConfig.isAlwaysUseDefaultForwardServletPath()) {
                effectiveTargetServletPath = webSiteConfig.getDefaultForwardServletPath();
            } else {
                effectiveTargetServletPath = CmsDataObject.nonEmptyOrDefault(mapping.getTargetServletPath(), webSiteConfig.getDefaultTargetServletPath());
            }
            
            String forwardPathRequestUri = mapping.getForwardPathRequestUri(request, 
                    webSiteConfig.getDefaultForwardServletPath(), effectiveTargetServletPath, webSiteConfig.getDefaultForwardFromContextRoot());
            
            if (UtilValidate.isNotEmpty(forwardPathRequestUri)) {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Process Mapping Request: Mapping for " + mappingPath + " found: " +
                            mapping.getLogIdRepr() +
                            "; forwarding to " + forwardPathRequestUri + 
                            "; extra path info: " + extraPathInfo + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                
                // FIXME: 2017: we should probably rename these attributes to use a unique prefix,
                // like "sysCmsProcessMapping" or "cmsSys_xxx", because in the event there are screens that handle CMS parts while
                // hooked into CMS, this will conflict and crash.
                // ALTERNATIVE: bundle them all into a single object with getters and setters, would be much cleaner
                
                request.setAttribute("cmsProcessMapping", mapping);
                request.setAttribute("cmsProcessMappingId", mapping.getId());
                request.setAttribute("cmsPageRenderMode", renderMode); // 2016: new
                // CAN'T SET THIS HERE - instead we set at the BEGINNING of the servlet AFTER forward - see above
                //request.setAttribute("cmsRequestServletPath", CmsControlUtil.normalizeServletPathNoNull(request.getServletPath()));
                //request.setAttribute("cmsRequestPath", CmsControlUtil.normalizeServletRootRequestPathNoNull(request.getPathInfo()));
                
                boolean forwardExtraPathInfo = mapping.isForwardExtraPathInfoLogical(webSiteConfig.getDefaultForwardExtraPathInfo());
                
                String fullForwardPath;
                if (forwardExtraPathInfo && UtilValidate.isNotEmpty(extraPathInfo)) {
                    fullForwardPath = PathUtil.concatPaths(forwardPathRequestUri, extraPathInfo);
                } else {
                    fullForwardPath = forwardPathRequestUri;
                }
                
                request.setAttribute("cmsProcessFullForwardPath", fullForwardPath);
                request.setAttribute("cmsProcessExtraPathInfo", extraPathInfo);
                // 2016: make sure we include the preview mount point
                request.setAttribute("cmsProcessSourcePath", processSourcePath);
                request.setAttribute("cmsProcessSourcePathMatch", processSourcePath); // without any extra prefix paths (there used to be a preview path prefix)
                
                String fullForwardUrl;
                if (forwardSourcePathParam) {
                    fullForwardUrl = fullForwardPath + "?cmsProcessSourcePath=" + CmsWebappUtil.urlEncode(processSourcePath);
                    // NOTE: DOES NOT WORK - if do this, the container will return 2 values for each parameter 
                    // in the query string when request.getParameterValues() is called - instead, use CmsProcessRequestWrapper
                    //if (request.getQueryString() != null) {
                    //    fullForwardUrl += "&" + request.getQueryString();
                    //}
                } else {
                    fullForwardUrl = fullForwardPath;
                }
                
                RequestDispatcher rd = request.getRequestDispatcher(fullForwardUrl);
                req.setAttribute("cmsRequestForwarded", Boolean.TRUE);
                rd.forward(getForwardedRequest(req, cmsRequestForwarded), getChainedResponse(req, res, cmsRequestVisited));
            } else {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Process Mapping Request: Can't forward mapping " +
                            mapping.getLogIdRepr() + "; no forward path specified" + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                
                if (!response.isCommitted()) { // || wasForwarded
                    req.setAttribute("cmsRequestChained", Boolean.TRUE);
                    chain.doFilter(req, getChainedResponse(req, res, cmsRequestVisited));
                }
            }
        }        
    }

    @Override
    public void destroy() {
    }
    
    protected ServletRequest getForwardedRequest(ServletRequest req, boolean previousRequestWrapped) {
        // NOTE: 2017-11: here previousRequestWrapped should always be false, and in any case we
        // would be require to re-wrap anyway.
        if (forwardSourcePathParam) {
            return new CmsProcessRequestWrapper((HttpServletRequest) req);
        }
        return req;
    }
    
    protected ServletResponse getChainedResponse(ServletRequest req, ServletResponse res, boolean previousResponseWrapped) {
        if (previousResponseWrapped) {
            // 2017-11: DO NOT WRAP RESPONSE TWICE
            return res;
        } else if (urlRewriteSourcePath) {
            return new CmsProcessResponseWrapper((HttpServletRequest) req, (HttpServletResponse) res);
        } else {
            return res;
        }
    }
    
    /**
     * Overrides getQueryString because in the forwarded servlet whenever
     * getRequestDispatcher contains a query string it overrides the incoming query string,
     * and everything relies on getQueryString including Ofbiz functions.
     * <p>
     * NOTE: The original query string cannot be passed to the RequestDispatcher forward (see above), because
     * it results in parameter duplication (request.getParameterValues() returns two copies); 
     * we can only achieve the right behavior by overriding request.getQueryString().
     * <p>
     * WARN: This does not override getParameterXxx(); the parameters returned by getParameterXxx() here may 
     * not be from our getQueryString() call below (this is also why we must pass ?cmsProcessSourcePath= 
     * to the RequestDispatcher forward - which can do because there's only one forward allowed and 
     * that param is denied from external by security check).
     */
    public static class CmsProcessRequestWrapper extends HttpServletRequestWrapper {

        private final String origQueryString;
        private final String pspQueryString;
        
        public CmsProcessRequestWrapper(HttpServletRequest request) {
            super(request);
            // TODO: REVIEW: the fact we call getQueryString in the constructor may limit
            // supported behavior for other chained filters
            this.origQueryString = request.getQueryString();
            this.pspQueryString = "cmsProcessSourcePath=" + CmsWebappUtil.urlEncode((String) request.getAttribute("cmsProcessSourcePath"));
        }

        @Override
        public String getQueryString() {
            //String origQueryString = ((HttpServletRequest) getRequest()).getQueryString();
            if (origQueryString != null && origQueryString.length() > 0) {
                return pspQueryString + "&" + origQueryString;
            } else {
                return pspQueryString;
            }
        }

    }
    
    public static class CmsProcessResponseWrapper extends HttpServletResponseWrapper {

        private final HttpServletRequest request;
        
        public CmsProcessResponseWrapper(HttpServletRequest request, HttpServletResponse response) {
            super(response);
            this.request = request;
        }

        /* 
         * OLD COMMENT (pre-2016) - FOR OLD OFBIZ:
         * FOR THE TIME BEING we don't need to rewrite for encodeXxxURL methods; in fact the RequestHandler
         * doesn't even send redirect requests through these. So only intercept the callRedirect method.
         * This minimizes the odds of the cmsProcessSourcePath parameter causing issues.
         * Later if more cases are identified, may need to override these as well.
         * 
         * NEW (2016):
         * In Scipio, controller redirects do go through encodeURL. There is a further issue
         * that redirects do not go through the sendRedirect method anymore. So for now, we're forced
         * to use the encodeURL and encodeRedirectURL hooks as well.
         */
        
        @Override
        public String encodeURL(String url) {
            // Note: Here we must apply our transformation BEFORE the next URL encode, because the
            // response we wrap is the one from the parent filter
            String rewrittenUrl = rewriteProcessLinkFromParam(request, url);
            if (rewrittenUrl != null) {
                return super.encodeURL(rewrittenUrl);
            } else {
                return super.encodeURL(url);
            }
        }

        @Override
        public String encodeRedirectURL(String url) {
            String rewrittenUrl = rewriteProcessLinkFromParam(request, url);
            if (rewrittenUrl != null) {
                return super.encodeRedirectURL(rewrittenUrl);
            } else {
                return super.encodeRedirectURL(url);
            }
        }

        @Override
        public void sendRedirect(String location) throws IOException {
            String rewrittenLocation = rewriteProcessLinkFromParam(request, location);
            if (rewrittenLocation != null) {
                super.sendRedirect(rewrittenLocation);
            } else {
                super.sendRedirect(location);
            }
        }
        
    }
    
    private static final Pattern absUrlPat = Pattern.compile("(((.*?):)?//([^/]*))?(.*)");
    private static final Pattern sourcePathUrlPat = Pattern.compile("([^?&;=#]*)(.*)([?&]cmsProcessSourcePath=([^?&;=#]*))(.*)");
    
    /**
     * Rewrites the given url using the cmsProcessSourcePath parameter found in the link itself.
     * <p>
     * TODO: May also want a version that takes cmsProcessSourcePath from request attributes?
     */
    public static String rewriteProcessLinkFromParam(HttpServletRequest request, String url) {
        String rewrittenUrl = null;
        if (url != null) {
            
            // Extract the relative URL from absolute
            Matcher mrel = absUrlPat.matcher(url);
            if (mrel.matches()) {
                
                String absPrefix = mrel.group(1);
                if (absPrefix == null) {
                    absPrefix = "";
                }
                String relUrl = mrel.group(5);
                
                // Check if within same webapp
                // (Note: in Ofbiz the encoded URLs contain the webapp context root in relative URLs)
                String contextRoot = request.getContextPath();
                if (relUrl.startsWith(contextRoot)) {
                    // (if same length, will be nothing interesting for us)
                    if (relUrl.length() > contextRoot.length()) {
                        
                        char contextDelim = relUrl.charAt(contextRoot.length());
                        if (contextDelim == '/' || contextDelim == '?' || contextDelim == ';' || contextDelim == '#') {
                            
                            // Check if processSourcePath exists and try to fetch out along with the other parts
                            Matcher msp = sourcePathUrlPat.matcher(relUrl.substring(contextRoot.length()));
                            if (msp.matches()) {
                                //String requestPath = msp.group(1);
                                String preQueryString = msp.group(2);
                                String processSourcePathAndParam = msp.group(3);
                                String encodedProcessSourcePath = msp.group(4);
                                String postQueryString = msp.group(5);
                                
                                String processSourcePath = CmsWebappUtil.urlDecode(encodedProcessSourcePath);
                                
                                // Rebuild the query string without the process path parameter
                                String queryString = preQueryString;
                                // We're removing a parameter here; make sure doesn't break the query
                                if (processSourcePathAndParam.charAt(0) == '?') {
                                    int delimPos = postQueryString.indexOf("&");
                                    if (delimPos >= 0) {
                                        queryString += postQueryString.substring(0, delimPos) + "?" + postQueryString.substring(delimPos + 1);
                                    } else {
                                        queryString += postQueryString;
                                    }
                                } else {
                                    queryString += postQueryString;
                                }
                                
                                rewrittenUrl = absPrefix + contextRoot + processSourcePath + queryString;
                            }
                        }
                    }
                }
            }
            
        }
        return rewrittenUrl;
    }
}

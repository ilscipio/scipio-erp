package com.ilscipio.scipio.cms.control;

import java.io.IOException;
import java.io.Writer;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.webapp.website.WebSiteWorker;

import com.ilscipio.scipio.ce.util.PathUtil;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.control.cmscall.CmsCallType;
import com.ilscipio.scipio.cms.webapp.CmsWebappUtil;

/**
 * Cms control-related util methods; unlike WebappUtil this is Cms-specific control code
 * and factoring points.
 */
public abstract class CmsControlUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    public static final String CMS_NOCACHERESPONSESET_REQATTRNAME = "_CMS_NOCACHERESPONSE_SET_";
    
    public static final String CMS_NOCACHECMSRENDER_REQATTRNAME = "cmsSetResponseBrowserNoCacheCmsPage";
    
    private CmsControlUtil() {
    }
    
    /**
     * Gets a system-wide unique ID for the current request. WARNING: We cheat and use current thread ID for now!
     * Note the name: "current".
     */
    public static long getCurrentRequestUniqueId(HttpServletRequest request) {
        return CmsWebappUtil.getCurrentRequestUniqueId(request);
    }

    /**
     * @deprecated WARN: I'm unsure what this is doing here.
     */
    @Deprecated
    public static String normalizePath(String path) {
        if (UtilValidate.isNotEmpty(path)) { 
            // Cms: Do we want this regexp here?
            if (!path.endsWith("/") && !path.matches("[^\\s]+\\.([0-9a-zA-Z]{0,4}?)$")) 
                return path.concat("/");
        }
        return path;
    }
    
    public static void checkSetNoCacheResponse(HttpServletRequest request, HttpServletResponse response) {
        // Always set, even if multiple times; don't think there's an issue with it and overrides anything else
        //if (!Boolean.TRUE.equals((Boolean) request.getAttribute(CMS_NOCACHERESPONSESET_REQATTRNAME))) {
        UtilHttp.setResponseBrowserProxyNoCache(response);
        request.setAttribute(CMS_NOCACHERESPONSESET_REQATTRNAME, Boolean.TRUE);
        //}
    }
    
    public static boolean getPreviewModeParam(HttpServletRequest request, CmsWebSiteConfig webSiteConfig) {
        String previewMode = (String) request.getAttribute(webSiteConfig.getPreviewModeParamName());
        if (previewMode == null) {
            previewMode = request.getParameter(webSiteConfig.getPreviewModeParamName());
            if ("Y".equals(previewMode) || (previewMode != null && previewMode.length() >= 5)) {
                previewMode = "Y";
            } else {
                previewMode = "N";
            }
            request.setAttribute(webSiteConfig.getPreviewModeParamName(), previewMode);
        }
        return "Y".equals(previewMode);
    }
    
    public static CmsCallType getRenderModeParam(HttpServletRequest request, CmsWebSiteConfig webSiteConfig) {
        CmsCallType renderMode;
        if (webSiteConfig.isAllowPreviewMode()) {
            renderMode = CmsControlUtil.getPreviewModeParam(request, webSiteConfig) ?
                    CmsCallType.OFBIZ_PREVIEW : CmsCallType.OFBIZ_RENDER;
        } else {
            renderMode = CmsCallType.OFBIZ_RENDER;
        }
        return renderMode;
    }
    
    public static String getAccessTokenParam(HttpServletRequest request, CmsWebSiteConfig webSiteConfig) {
        String accessToken = (String) request.getAttribute(webSiteConfig.getAccessTokenParamName());
        if (accessToken == null) {
            accessToken = request.getParameter(webSiteConfig.getAccessTokenParamName());
            if (accessToken == null) {
                // access token may also be inlined into cmsPreviewMode param
                String inlineAccessToken = request.getParameter(webSiteConfig.getPreviewModeParamName());
                if (inlineAccessToken != null && inlineAccessToken.length() >= 5) {
                    accessToken = inlineAccessToken;
                } else {
                    accessToken = "";
                }
            }
            request.setAttribute(webSiteConfig.getAccessTokenParamName(), accessToken);
        }
        return accessToken.isEmpty() ? null : accessToken;
    }
    
    public static boolean verifyValidAccessToken(HttpServletRequest request, CmsWebSiteConfig webSiteConfig, CmsCallType renderMode) {
        if (renderMode == CmsCallType.OFBIZ_PREVIEW || webSiteConfig.isRequireLiveAccessToken()) {
            String accessToken = CmsControlUtil.getAccessTokenParam(request, webSiteConfig);
            // TODO: REVIEW: the request URI here might not necessarily match one of the page's URIs
            // but won't matter until isValidAccessToken actively checks it
            if (!CmsAccessHandler.isValidAccessToken(request, request.getRequestURI(), accessToken)) {
                return false;
            }
        }
        return true;
    }


    public static String normalizeServletPath(String servletPath) { // Servlet path only
        if (servletPath == null) return null;
        return PathUtil.ensureStartAndNoTrailDelim(servletPath);
    }
    
    public static String normalizeServletPathNoNull(String servletPath) { // Servlet path only
        if (servletPath == null) return "/";
        return PathUtil.ensureStartAndNoTrailDelim(servletPath);
    }
    
    public static String normalizeServerRootRequestPath(String requestPath) { // Path from server root to before query string
        if (requestPath == null) return null;
        return PathUtil.ensureStartAndNoTrailDelim(requestPath);
    }
    
    public static String normalizeServerRootRequestPathNoNull(String requestPath) { // Path from server root to before query string
        if (requestPath == null) return "/";
        return PathUtil.ensureStartAndNoTrailDelim(requestPath);
    }
    
    public static String normalizeContextRootRequestPath(String requestPath) { // Path from servlet context (webapp) root to before query string
        if (requestPath == null) return null;
        return PathUtil.ensureStartAndNoTrailDelim(requestPath);
    }
    
    public static String normalizeContextRootRequestPathNoNull(String requestPath) { // Path from servlet context (webapp) root to before query string
        if (requestPath == null) return "/";
        return PathUtil.ensureStartAndNoTrailDelim(requestPath);
    }
    
    public static String normalizeServletRootRequestPath(String requestPath) { // Path from servlet (controller) root to before query string
        if (requestPath == null) return null;
        return PathUtil.ensureStartAndNoTrailDelim(requestPath);
    }
    
    public static String normalizeServletRootRequestPathNoNull(String requestPath) { // Path from servlet (controller) root to before query string
        if (requestPath == null) return "/";
        return PathUtil.ensureStartAndNoTrailDelim(requestPath);
    }

    /**
     * @deprecated not really appropriate for local cms.
     */
    @Deprecated
    public static String normalizeCmsReqPath(String cmsReqPath) {
        if (cmsReqPath == null) return null;
        return PathUtil.ensureStartDelim(cmsReqPath);
    }
    
    public static String getReqLogIdStr(HttpServletRequest request) {
        HttpSession session = (request != null) ? request.getSession(false) : null;
        return (request != null ? "sessionId: " + (session == null ? "unknown" : session.getId()) + "; " : "") + "threadId: " + Thread.currentThread().getId();
    }
    
    public static String getReqLogIdDelimStr(HttpServletRequest request) {
        HttpSession session = (request != null) ? request.getSession(false) : null;
        return (request != null ? "; sessionId: " + (session == null ? "unknown" : session.getId()): "") + "; threadId: " + Thread.currentThread().getId();
    }
    
    // only use if request is not available
    public static String getReqLogIdStr(HttpSession session) {
        return "sessionId: " + (session == null ? "unknown" : session.getId()) + "; threadId: " + Thread.currentThread().getId();
    }
    
    // only use if request is not available
    public static String getReqLogIdDelimStr(HttpSession session) {
        return "; sessionId: " + (session == null ? "unknown" : session.getId()) + "; threadId: " + Thread.currentThread().getId();
    }

    /**
     * Special webSiteId lookup for filters which may run early in a chain; in this case, 
     * WebSiteWorker.getWebSiteId(request) may not return a value.
     * May also be useful elsewhere (view handler, just in case).
     */
    public static String getWebSiteIdForControl(HttpServletRequest request, ServletContext servletContext) {
        // Try WebSiteWorker first
        String webSiteId = WebSiteWorker.getWebSiteId(request);
        if (UtilValidate.isEmpty(webSiteId) && servletContext != null) {
            webSiteId = (String) servletContext.getAttribute("webSiteId");
        }
        return webSiteId;
    }

    /**
     * Special delegator lookup for filters which may run early in a chain; in this case,
     * request.getAttribute("delegator") may return nothing because 
     * ControlFilter/ControlServlet/LoginWorker not yet run.
     * <p>
     * FIXME?: This may be one request late for tenant delegator switches.
     * 
     * @param request
     * @param servletContext
     */
    public static Delegator getDelegatorForControl(HttpServletRequest request, ServletContext servletContext) {
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
                    Debug.logWarning("Cms: ERROR: could not get session delegator for control/filter; " +
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
            Debug.logError("Cms: ERROR: could not get any delegator for control/filter!", module);
        }
        return delegator;
    }
    
    /**
     * Gets the response writer, in the same fashion as done by 
     * {@link org.ofbiz.widget.renderer.macro.MacroScreenViewHandler}.
     */
    public static Writer getResponseWriter(HttpServletRequest request, HttpServletResponse response) throws IOException {
        // 2016: don't do this; do exact same as MacroScreenViewHandler
        //      Writer writer;
        //      if (servletCtx != null) {
        //          writer = response.getWriter();
        //      } else {
        //          ServletOutputStream ros = response.getOutputStream();
        //          writer = new OutputStreamWriter(ros, "UTF-8");
        //      }
        return response.getWriter();
    }
    
    public static String getPagePreviewVersionId(HttpServletRequest request) {
        String cmsPageVersionId = (String) request.getAttribute("cmsPageVersionId");
        if (cmsPageVersionId == null) {
            cmsPageVersionId = request.getParameter("cmsPageVersionId");
        }
        if (UtilValidate.isEmpty(cmsPageVersionId)) {
            cmsPageVersionId = null;
        }
        return cmsPageVersionId;
    }

    public static Delegator getDelegator(HttpSession session) throws IllegalStateException {
        Delegator delegator = (Delegator) session.getAttribute("delegator");
        if (delegator == null) {
            ServletContext sc = session.getServletContext();
            delegator = (Delegator) sc.getAttribute("delegator");
            if (delegator == null) {
                String delegatorName = (String) session.getAttribute("delegatorName");
                if (delegatorName == null) {
                    delegatorName = (String) sc.getAttribute("delegatorName");
                    if (delegatorName == null) {
                        Debug.logWarning("Cms: delegator not found in session or servlet context"
                                + " - using default delegator", module);
                        delegatorName = "default";
                    }
                }
                delegator = DelegatorFactory.getDelegator(delegatorName);
                if (delegator == null) {
                    throw new IllegalStateException("Could not get delegator from session, servlet context"
                            + " or from delegator name '" + delegatorName + "'");
                }
            }
        }
        return delegator;
    }
}

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
import com.ilscipio.scipio.cms.control.cmscall.CmsCallType;
import com.ilscipio.scipio.cms.webapp.CmsWebappUtil;

/**
 * Cms control-related util methods; unlike WebappUtil this is Cms-specific control code
 * and factoring points.
 */
public abstract class CmsControlUtil {

    public static final String module = CmsControlUtil.class.getName();
    
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
    
    public static boolean checkPreviewMode(HttpServletRequest request, String paramName) {
        String previewMode = (String) request.getAttribute(paramName);
        if (previewMode == null) {
            previewMode = request.getParameter(paramName);
            if (previewMode == null || "N".equals(previewMode) || previewMode.isEmpty()) {
                previewMode = "N";
            } else {
                // NOTE: 2018-05-06: We no longer accept "Y" coming from parameters;
                // must be a valid preview token
                // FIXME?: missing page and/or pagePath
                boolean validToken = CmsPreviewTokenHandler.isValidPreviewToken(request, null, null, previewMode);
                if (validToken) {
                    previewMode = "Y";
                } else {
                    Debug.logError("Cms: Preview Mode: Invalid preview token for session " 
                            + request.getSession().getId(), module);
                    
                    // TODO: THROW INVALID ACCESS ERROR
                    //throw new CmsPermissionException("Preview Mode: Invalid preview token for session " 
                    //  + request.getSession().getId());
                    
                    previewMode = "N";
                }
            }
            request.setAttribute(paramName, previewMode);
        }
        return "Y".equals(previewMode);
    }
    
    public static CmsCallType checkRenderMode(HttpServletRequest request, String paramName, boolean allowPreviewMode) {
        CmsCallType renderMode;
        if (allowPreviewMode) {
            renderMode = CmsControlUtil.checkPreviewMode(request, paramName) ?
                    CmsCallType.OFBIZ_PREVIEW : CmsCallType.OFBIZ_RENDER;
        } else {
            renderMode = CmsCallType.OFBIZ_RENDER;
        }
        return renderMode;
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
                            delegatorName + "\"; defaulting to servlet or default delegator", CmsWebappUtil.module);
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
            Debug.logError("Cms: ERROR: could not get any delegator for control/filter!", CmsWebappUtil.module);
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

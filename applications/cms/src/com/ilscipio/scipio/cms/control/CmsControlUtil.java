package com.ilscipio.scipio.cms.control;

import java.io.IOException;
import java.io.Writer;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.tomcat.util.descriptor.web.FilterDef;
import org.apache.tomcat.util.descriptor.web.WebXml;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.webapp.WebAppUtil;
import org.ofbiz.webapp.website.WebSiteWorker;

import com.ilscipio.scipio.cms.control.cmscall.CmsCallType;
import com.ilscipio.scipio.cms.util.PathUtil;
import com.ilscipio.scipio.cms.webapp.CmsWebappUtil;

/**
 * Cms control-related util methods; unlike WebappUtil this is Cms-specific control code
 * and factoring points.
 * <p>
 * 2016: FIXME: some generic methods belong in org.ofbiz.webapp package, and WebXml (tomcat class)
 * shouldn't be a dependency for this class.
 */
public abstract class CmsControlUtil {

    public static final String module = CmsControlUtil.class.getName();
    
    public static final String CMS_NOCACHERESPONSESET_REQATTRNAME = "_CMS_NOCACHERESPONSE_SET_";
    
    public static final String CMS_NOCACHECMSRENDER_REQATTRNAME = "cmsSetResponseBrowserNoCacheCmsPage";
    
    /**
     * 2016: WARN: this is now absolute last resort only; always try to lookup the real one before this!
     * @see #getControlServletPath
     */
    public static final String defaultServletPathDefault = "/control";
    
    public static final boolean setResponseBrowserNoCacheCmsPageDefault = false;
    public static final boolean setResponseBrowserNoCacheScreenDefault = false;
    public static final boolean setResponseBrowserNoCacheDefault = false;
    
    public static final boolean useDefaultCmsPageDefault = false;
    
    public static final String defaultCmsPageId = null;
    
    public static final boolean alwaysUseDefaultForwardServletPathDefault = false;
    public static final boolean defaultForwardExtraPathInfoDefault = true;
    public static final boolean defaultSourceFromContextRootDefault = true;
    public static final boolean defaultForwardFromContextRootDefault = true;

    public static final String previewModeDefaultParamName = "cmsPreviewMode";
    
    private CmsControlUtil() {
    }
    
    /**
     * Gets a system-wide unique ID for the current request. WARNING: We cheat and use current thread ID for now!
     * Note the name: "current".
     */
    public static long getCurrentRequestUniqueId(HttpServletRequest request) {
        return CmsWebappUtil.getCurrentRequestUniqueId(request);
    }

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
    
    public static String getDefaultCmsPageId(ServletContext servletContext) {
        String pageId = defaultCmsPageId;
        if (servletContext != null) {
            final String paramName = "cmsDefaultCmsPageId";
            String initPageId = servletContext.getInitParameter(paramName);
            if (UtilValidate.isNotEmpty(initPageId)) {
                pageId = initPageId;
            }
        }
        return pageId;
    }
    
    public static String getDefaultCmsPageId() {
        return defaultCmsPageId;
    }
    
    public static String getPreviewModeDefaultParamName(ServletContext servletContext) {
        return previewModeDefaultParamName; // TODO?: web.xml context-param
    }
    
    public static boolean checkPreviewMode(HttpServletRequest request, String paramName) {
        String previewMode = (String) request.getAttribute(paramName);
        if (previewMode == null) {
            previewMode = request.getParameter(paramName);
            if (!"Y".equals(previewMode)) {
                previewMode = "N";
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
    
    private static String getDefaultSpecificServletPath(ServletContext servletContext, String paramName, String defVal) {
        String servletPath = defVal;
        if (servletContext != null) {
            String paramPath = null;
            if (paramName != null) {
                paramPath = servletContext.getInitParameter(paramName);
                if (UtilValidate.isNotEmpty(paramPath)) {
                    servletPath = CmsControlUtil.normalizeServletPath(paramPath);
                }
            }
            paramPath = servletContext.getInitParameter("cmsDefaultServletPath");
            if (UtilValidate.isNotEmpty(paramPath)) {
                servletPath = CmsControlUtil.normalizeServletPath(paramPath);
            }
        }
        return servletPath;
    }
    
    private static String getDefaultSpecificServletPath(Map<String, String> contextParams, String paramName, String defVal) {
        String servletPath = defVal;
        if (contextParams != null) {
            String paramPath = null;
            if (paramName != null) {
                paramPath = contextParams.get(paramName);
                if (UtilValidate.isNotEmpty(paramPath)) {
                    servletPath = CmsControlUtil.normalizeServletPath(paramPath);
                }
            }
            paramPath = contextParams.get("cmsDefaultServletPath");
            if (UtilValidate.isNotEmpty(paramPath)) {
                servletPath = CmsControlUtil.normalizeServletPath(paramPath);
            }
        }
        return servletPath;
    }
    
    /**
     * 2016: gets the control servlet mapping for given servlet context.
     */
    public static String getControlServletPath(ServletContext servletContext) {
        return getControlServletPath(servletContext.getInitParameter("webSiteId"));
    }
    
    /**
     * 2016: gets the control servlet mapping for given webSiteId.
     */
    public static String getControlServletPath(String webSiteId) {
        if (UtilValidate.isEmpty(webSiteId)) {
            Debug.logWarning("Cms: could not get control servlet path from webSiteId; missing webSiteId", module);
            return null;
        }
        try {
            WebappInfo webappInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
            return WebAppUtil.getControlServletOnlyPath(webappInfo);
        } catch (Exception e) {
            Debug.logError(e, "Cms: could not get control servlet path from webSiteId: " + e.getMessage() + " (webSiteId: " + webSiteId + ")", module);
        }
        return null;
    }
    
    /**
     * 2016: gets the control servlet mapping for given webappInfo.
     */
    private static String getControlServletPath(WebappInfo webappInfo) {
        try {
            return WebAppUtil.getControlServletOnlyPath(webappInfo);
        } catch (Exception e) {
            Debug.logError(e, "Cms: could not get control servlet path: " + e.getMessage() + " (context root: " + webappInfo.getContextRoot() + ")", module);
        }
        return null;
    }
    
    public static String getDefaultServletPath(ServletContext servletContext) {
        // 2016: do NOT use hardcoded defaultServletPathDefault here; instead look it up
        String defaultServletPath = getControlServletPath(servletContext);
        if (UtilValidate.isEmpty(defaultServletPath)) {
            defaultServletPath = defaultServletPathDefault;
            Debug.logWarning("Cms: default servlet path: encountered website with no valid control servlet mapping; using hardcoded default (" + defaultServletPath + ")", module);
        }
        return getDefaultSpecificServletPath(servletContext, null, defaultServletPath);
    }
    
    public static String getDefaultServletPath(String webSiteId) {
        WebappInfo webappInfo;
        Map<String, String> contextParams;
        try {
            webappInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
            contextParams = WebAppUtil.getWebappContextParams(webSiteId);
        } catch (Exception e) {
            Debug.logError(e, "Cms: could not get webapp description for webSiteId '" + webSiteId + "'", module);
            return null;
        }
        return getDefaultServletPath(webSiteId, webappInfo, contextParams);
    }
    
    private static String getDefaultServletPath(String webSiteId, WebappInfo webappInfo, Map<String, String> contextParams) {
        // 2016: do NOT use hardcoded defaultServletPathDefault here; instead look it up
        String defaultServletPath = getControlServletPath(webappInfo);
        if (UtilValidate.isEmpty(defaultServletPath)) {
            defaultServletPath = defaultServletPathDefault;
            Debug.logWarning("Cms: default servlet path: encountered website with no valid control servlet mapping; using hardcoded default (" + defaultServletPath + ")", module);
        }
        return getDefaultSpecificServletPath(contextParams, null, defaultServletPath);
    }
    
    public static String getDefaultSpecificServletPath(ServletContext servletContext, String paramName) {
        return getDefaultSpecificServletPath(servletContext, paramName, getDefaultServletPath(servletContext));
    }
    
    public static String getDefaultSpecificServletPath(String webSiteId, String paramName) {
        WebappInfo webappInfo;
        Map<String, String> contextParams;
        try {
            webappInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
            contextParams = WebAppUtil.getWebappContextParams(webSiteId);
        } catch (Exception e) {
            Debug.logError(e, "Cms: could not get webapp description for webSiteId '" + webSiteId + "'", module);
            return null;
        }
        return getDefaultSpecificServletPath(contextParams, paramName, getDefaultServletPath(webSiteId, webappInfo, contextParams));
    }
    

    public static String getPrimaryPathFromContextRootDefault(String webSiteId) {
        return CmsProcessMapping.getPrimaryPathFromContextRootDefault(webSiteId);
    }
    
    public static String toCmsInitParamName(String paramName) {
        if (paramName.startsWith("cms")) {
            return paramName;
        }
        else {
            return "cms" + paramName.substring(0, 1).toUpperCase() + paramName.substring(1);
        }
    }
    
    public static Boolean getCmsBoolInitParam(ServletContext servletContext, String paramName, Boolean defValue) {
        if (servletContext == null) return defValue;
        String val = servletContext.getInitParameter(toCmsInitParamName(paramName));
        if (UtilValidate.isNotEmpty(val)) {
            if ("true".equalsIgnoreCase(val)) {
                return true;
            }
            else if ("false".equalsIgnoreCase(val)) {
                return false;
            }
        }
        return defValue;
    }
    
    public static Boolean getCmsBoolInitParam(Map<String, ?> contextParams, String paramName, Boolean defValue) {
        if (contextParams == null) return defValue;
        Object obj = contextParams.get(toCmsInitParamName(paramName));
        String val = obj != null ? obj.toString() : null;
        if (UtilValidate.isNotEmpty(val)) {
            if ("true".equalsIgnoreCase(val)) {
                return true;
            }
            else if ("false".equalsIgnoreCase(val)) {
                return false;
            }
        }
        return defValue;
    }
    
    
    public static String normalizeServletPath(String servletPath) { // Servlet path only
        if (servletPath == null) {
            return null;
        }
        return PathUtil.ensureStartAndNoTrailDelim(servletPath);
    }
    
    public static String normalizeServletPathNoNull(String servletPath) { // Servlet path only
        if (servletPath == null) {
            return "/";
        }
        return PathUtil.ensureStartAndNoTrailDelim(servletPath);
    }
    
    public static String normalizeServerRootRequestPath(String requestPath) { // Path from server root to before query string
        if (requestPath == null) {
            return null;
        }
        return PathUtil.ensureStartAndNoTrailDelim(requestPath);
    }
    
    public static String normalizeServerRootRequestPathNoNull(String requestPath) { // Path from server root to before query string
        if (requestPath == null) {
            return "/";
        }
        return PathUtil.ensureStartAndNoTrailDelim(requestPath);
    }
    
    public static String normalizeContextRootRequestPath(String requestPath) { // Path from servlet context (webapp) root to before query string
        if (requestPath == null) {
            return null;
        }
        return PathUtil.ensureStartAndNoTrailDelim(requestPath);
    }
    
    public static String normalizeContextRootRequestPathNoNull(String requestPath) { // Path from servlet context (webapp) root to before query string
        if (requestPath == null) {
            return "/";
        }
        return PathUtil.ensureStartAndNoTrailDelim(requestPath);
    }
    
    public static String normalizeServletRootRequestPath(String requestPath) { // Path from servlet (controller) root to before query string
        if (requestPath == null) {
            return null;
        }
        return PathUtil.ensureStartAndNoTrailDelim(requestPath);
    }
    
    public static String normalizeServletRootRequestPathNoNull(String requestPath) { // Path from servlet (controller) root to before query string
        if (requestPath == null) {
            return "/";
        }
        return PathUtil.ensureStartAndNoTrailDelim(requestPath);
    }

    /**
     * @deprecated not really appropriate for local cms.
     */
    @Deprecated
    public static String normalizeCmsReqPath(String cmsReqPath) {
        if (cmsReqPath == null) {
            return null;
        }
        return PathUtil.ensureStartDelim(cmsReqPath);
    }
    
    public static String getReqLogIdStr(HttpServletRequest request) {
        HttpSession session = request != null ? request.getSession(false) : null;
        return (request != null ? "sessionId: " + (session == null ? "unknown" : session.getId()) + "; " : "") + "threadId: " + Thread.currentThread().getId();
    }
    
    public static String getReqLogIdDelimStr(HttpServletRequest request) {
        HttpSession session = request != null ? request.getSession(false) : null;
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
    
    /**
     * If web.xml cmsControlRootAlias is set to true or false, take it at face
     * value and return it.
     * Otherwise, try to infer if can return true from ContextFilter cmsControlRootAlias.
     * However, if can't guarantee aliasing is not happening, return null instead of false. 
     */
    static Boolean readWebSiteControlRootAliasLogical(ExtWebSiteInfo webSiteInfo) {
        Boolean alias = null;
        try {
            Map<String, String> contextParams = webSiteInfo.getContextParams();
            if (contextParams != null) {
                // if this boolean set, we take it at face value, true or false
                alias = UtilMisc.booleanValueVersatile(contextParams.get("cmsControlRootAlias"));
            }
            if (alias != null) {
                Debug.logInfo("Cms: Website '" + webSiteInfo.getWebSiteId() 
                        + "': Resolved web.xml valid context-param cmsControlRootAlias boolean value: " + alias, module);
            } else {
                if (contextParams.containsKey("cmsControlRootAlias")) {
                    if (UtilValidate.isNotEmpty((String) contextParams.get("cmsControlRootAlias"))) {
                        Debug.logError("Cms: Website '" + webSiteInfo.getWebSiteId()  
                                + "': web.xml context-param cmsControlRootAlias has invalid boolean value: " + contextParams.get("cmsControlRootAlias"), module);
                    } else {
                        Debug.logInfo("Cms: Website '" + webSiteInfo.getWebSiteId()  
                                + "': Found web.xml context-param cmsControlRootAlias, but was empty (valid)", module);
                    }
                }
                alias = webSiteInfo.readVerifyForwardRootControllerUrisSetting(true);
            }
        } catch (Exception e) {
            Debug.logError(e, "Cms: could not read web.xml for webSiteId '" + webSiteInfo.getWebSiteId() + "' to determine cmsControlRootAlias", module);
        }
        return alias;
    }


    
}

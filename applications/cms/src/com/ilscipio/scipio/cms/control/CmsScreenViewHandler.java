package com.ilscipio.scipio.cms.control;

import java.io.IOException;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilRender;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.transaction.GenericTransactionException;
import org.ofbiz.entity.transaction.TransactionUtil;
import org.ofbiz.webapp.control.RequestUtil;
import org.ofbiz.webapp.view.ViewHandlerException;
import org.ofbiz.webapp.view.ViewHandlerExt;
import org.ofbiz.widget.renderer.macro.MacroScreenViewHandler;
import org.xml.sax.SAXException;

import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageInfo;
import com.ilscipio.scipio.cms.control.cmscall.CmsCallType;
import com.ilscipio.scipio.cms.control.cmscall.render.RenderInvoker;
import com.ilscipio.scipio.cms.data.CmsDataException;
import com.ilscipio.scipio.cms.template.CmsRenderUtil;

import freemarker.template.TemplateException;

/**
 * Cms screen view handler - invokes CMS rendering.
 * <p>
 * NOTE: 2016: client webapps should include this in controller.xml using
 * through <code>component://cms/webcommon/WEB-INF/cms-client-controller.xml</code>.
 * <p>
 * All CMS render invocations are done through this class, both process and view mappings.
 * It finds the page to render by matching the current view to a (process) view mapping, which
 * contains the address of a CMS page to invoke.
 * <p>
 * FIXME: 2016: This should be changed to not extend MacroScreenViewHandler and instead only
 * implement the basic interface and delegate to an instance of MacroScreenViewHandler instead, which
 * can then be configurable.
 * 
 * @see com.ilscipio.scipio.cms.control.cmscall.CmsCallParams
 */
public class CmsScreenViewHandler extends MacroScreenViewHandler implements ViewHandlerExt {
    
    public static final String module = CmsScreenViewHandler.class.getName();

    public static final Set<Integer> passOnHttpStatusesFromCms = Collections.unmodifiableSet(new HashSet<Integer>(Arrays.asList(
            new Integer[] { HttpServletResponse.SC_NOT_FOUND })));
    
    // (Keep our own var; don't depend on super implementation)
    protected ServletContext servletContext = null;
    protected RenderInvoker renderInvoker = null;
    
    protected boolean allowPreviewMode = false;
    private String previewModeParamName;
    
    protected boolean setResponseBrowserNoCacheCmsPage = CmsControlUtil.setResponseBrowserNoCacheCmsPageDefault;
    protected boolean setResponseBrowserNoCacheScreen = CmsControlUtil.setResponseBrowserNoCacheScreenDefault;
    protected boolean setResponseBrowserNoCache = CmsControlUtil.setResponseBrowserNoCacheDefault;
    
    protected String defaultTargetServletPath;
    protected boolean useDefaultCmsPage = CmsControlUtil.useDefaultCmsPageDefault;
    protected String defaultCmsPageId = CmsControlUtil.getDefaultCmsPageId();
    protected CmsPageInfo defaultCmsPage = new CmsPageInfo(defaultCmsPageId);
    
    @Override
    public void init(ServletContext context) throws ViewHandlerException {
        super.init(context);
        this.servletContext = context;
        
        this.allowPreviewMode = CmsControlUtil.getCmsBoolInitParam(servletContext, "cmsAllowPreviewMode", false); // 2016: new
        this.previewModeParamName = CmsControlUtil.getPreviewModeDefaultParamName(servletContext);

        this.setResponseBrowserNoCacheCmsPage = CmsControlUtil.getCmsBoolInitParam(this.servletContext, 
                "setResponseBrowserNoCacheCmsPage", this.setResponseBrowserNoCacheCmsPage);
        this.setResponseBrowserNoCacheScreen = CmsControlUtil.getCmsBoolInitParam(this.servletContext, 
                "setResponseBrowserNoCacheScreen", this.setResponseBrowserNoCacheScreen);
        this.setResponseBrowserNoCache = CmsControlUtil.getCmsBoolInitParam(this.servletContext, 
                "setResponseBrowserNoCache", this.setResponseBrowserNoCache);

        this.defaultTargetServletPath = CmsControlUtil.getDefaultSpecificServletPath(context, "cmsDefaultTargetServletPath");
        
        this.useDefaultCmsPage = CmsControlUtil.getCmsBoolInitParam(this.servletContext, 
                "useDefaultCmsPage", this.useDefaultCmsPage);
        this.defaultCmsPageId = CmsControlUtil.getDefaultCmsPageId(context);
        this.defaultCmsPage = new CmsPageInfo(this.defaultCmsPageId);
        
        if (this.useDefaultCmsPage && UtilValidate.isEmpty(this.defaultCmsPageId)) {
            this.defaultCmsPageId = null;
            Debug.logWarning("Cms: default CMS page fallback was enabled in web config, "
                    + "but no default CMS page specified; will treat as disabled", module); 
        }
        
        this.renderInvoker = RenderInvoker.getRenderInvoker(context);
        
        // hasControllerHint true because if there's a view handler there has to be a controller...
        CmsWebSiteInfo.registerCmsWebSite(context, true);
    }

    @Override
    public void render(String name, String page, String info, String contentType, String encoding,
            HttpServletRequest request, HttpServletResponse response, Writer writer) throws ViewHandlerException {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        
        String path = request.getPathInfo(); // DEV NOTE: do not rely on this
        
        String webSiteId = CmsControlUtil.getWebSiteIdForControl(request, servletContext);
        CmsPage cmsPage = null;
        CmsCallType renderMode;
        CmsView cmsView = CmsView.findByName(delegator, name, webSiteId, true);

        // 2017-04-03: due to redirection issues, we have to use saved path first, so use the same
        // info as the original request. re-lookup is only for pure view handler and for fallback.
        String requestServletPath = (String) request.getAttribute("cmsRequestServletPath");
        String requestPath = (String) request.getAttribute("cmsRequestPath");
        if (requestServletPath == null || requestPath == null) { // should be set together, and null makes NPE elsewhere
            requestServletPath = CmsControlUtil.normalizeServletPathNoNull(request.getServletPath());
            requestPath = CmsControlUtil.normalizeServletRootRequestPathNoNull(request.getPathInfo());
        }
        
        // This will only run if filter didn't already do it
        if (setResponseBrowserNoCache) {
            if (CmsUtil.verboseOn()) {
                Debug.logInfo("Cms: Setting browser no-proxy no-cache response" + CmsControlUtil.getReqLogIdDelimStr(request), module);
            }
            CmsControlUtil.checkSetNoCacheResponse(request, response);
        }
        
        // 2016: check preview flag
        renderMode = (CmsCallType) request.getAttribute("cmsPageRenderMode");
        
        // 2016: check preview mode parameter
        // NOTE: this MAY have been done in process filter, but it possible to run without it, so do it again here
        if (renderMode == null) {
            renderMode = CmsControlUtil.checkRenderMode(request, previewModeParamName, allowPreviewMode);
        }
        
        // 2016: MUST NOT CACHE PREVIEWS!
        boolean useDataObjectCache = renderMode != CmsCallType.OFBIZ_PREVIEW;
        
        // 2016: NEW MODE: use cmsPage if already set (by old CmsControlServlet or other)
        // NOTE: reliance on this is likely TEMPORARY as the CmsControlServlet itself will probably disappear or change significantly,
        // but this code can remain here anyway.
        cmsPage = (CmsPage) request.getAttribute("cmsPage"); 
        if (cmsPage == null) {
            // in case we run into serialization issues, allow re-lookup by ID
            String cmsPageId = (String) request.getAttribute("cmsPageId");
            if (cmsPageId != null && !cmsPageId.isEmpty()) {
                cmsPage = CmsPage.getWorker().findById(delegator, cmsPageId, useDataObjectCache);
                if (cmsPage == null) {
                    Debug.logWarning("Cms: Could not find page by ID: " + cmsPageId + "; ignoring", module);
                }
            }
        }

        // Check for process mapping
        if (cmsPage == null) {
            CmsProcessMapping procMapping = (CmsProcessMapping) request.getAttribute("cmsProcessMapping");
            if (procMapping == null) {
                // in case we run into serialization issues, allow re-lookup by ID
                String cmsProcessMappingId = (String) request.getAttribute("cmsProcessMappingId");
                if (cmsProcessMappingId != null && !cmsProcessMappingId.isEmpty()) {
                    procMapping = CmsProcessMapping.getWorker().findById(delegator, cmsProcessMappingId, useDataObjectCache);
                    if (procMapping == null) {
                        Debug.logWarning("Cms: Could not find process mapping by ID: " + procMapping + "; ignoring", module);
                    }
                }
            }
            
            if (procMapping != null) {
                // CMS: 2016: not needed without wildcard renders
                //String procExtraPathInfo = (String) request.getAttribute("cmsProcessExtraPathInfo");
                // CMS: 2016: wildcard renders not applicable to local renders
                //boolean pageFromPathWildcard = false;
                
                try {
                    if (CmsUtil.verboseOn()) {
                        Debug.logInfo("Cms: Looking for process mapping view matching process mapping (" +
                                procMapping.getLogIdRepr() + "), view (" + name + "), request servlet path (" +
                                requestServletPath + ") and request path (" + requestPath + ")" + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    }
                    
                    CmsProcessViewMapping procViewMapping = procMapping.getProcessViewMapping(requestServletPath, 
                            requestPath, name, defaultTargetServletPath, CmsProcessMapping.defaultMatchAnyTargetPath);
                    if (procViewMapping != null) {
                        if (procViewMapping.isActiveLogical()) {
                            if (CmsUtil.verboseOn()) {
                                Debug.logInfo("Cms: Found active process view mapping: " + procViewMapping.getLogIdRepr() + CmsControlUtil.getReqLogIdDelimStr(request), module);
                            }
        
                            cmsPage = procViewMapping.getPage();
                            if (CmsUtil.verboseOn()) {
                                if (cmsPage != null) {
                                    Debug.logInfo("Cms: Found page " + cmsPage.getLogIdRepr() + " for process view mapping" + CmsControlUtil.getReqLogIdDelimStr(request), module);
                                } else {
                                    Debug.logInfo("Cms: No page associated to process view mapping (or parent process mapping)" + CmsControlUtil.getReqLogIdDelimStr(request), module);
                                }
                            }
                            
                            // (technically should check this even if page was null; same result)
                            // CMS: 2016: not applicable to local renders
    //                        Boolean mappingPageFromPathWildcard = procViewMapping.getPageFromPathWildcard();
    //                        if (mappingPageFromPathWildcard == null) {
    //                            mappingPageFromPathWildcard = procMapping.getPageFromPathWildcard();
    //                            if (mappingPageFromPathWildcard == null) {
    //                                mappingPageFromPathWildcard = CmsProcessMapping.defaultPageFromPathWildcard;
    //                            }
    //                        }
    //                        pageFromPathWildcard = mappingPageFromPathWildcard;
                        } else {
                            if (CmsUtil.verboseOn()) {
                                Debug.logInfo("Cms: Found process view mapping, but is inactive: " + procViewMapping.getLogIdRepr() + CmsControlUtil.getReqLogIdDelimStr(request), module);
                            }
                        }
                    } else {
                        if (CmsUtil.verboseOn()) {
                            Debug.logInfo("Cms: No process view mapping found for request/view" + CmsControlUtil.getReqLogIdDelimStr(request), module);
                        }
                    }
                    
                } catch (Exception e) {
                    // an exception is thrown, return a 500 error
                    Debug.logError(e, "Cms: Error retrieving process mapping or page from database. URI: " + path + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    handleException(request, response, e, renderMode);
                    return; // Nothing can be sent after this
                }
            }
        }

        // Check for simple view mapping
        if (cmsPage == null && UtilValidate.isNotEmpty(name)) {
            try {
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Looking for simple view mapping matching view (" + name + "), request servlet path (" +
                            requestServletPath + ") and web site (" + webSiteId + ")" + CmsControlUtil.getReqLogIdDelimStr(request), module);
                }
                
                CmsViewMapping viewMapping = CmsViewMapping.findByView(delegator, webSiteId, requestServletPath, 
                        name, defaultTargetServletPath, useDataObjectCache, request);
                if (viewMapping != null) {
                    if (viewMapping.isActiveLogical()) {
                        cmsPage = viewMapping.getPage();
                        if (CmsUtil.verboseOn()) {
                            Debug.logInfo("Cms: Active view mapping found: " + viewMapping.getLogIdRepr() + "; "
                                    + (cmsPage != null ? "maps to page " + cmsPage.getLogIdRepr() : "has no valid page assigned; ignoring" 
                                    + CmsControlUtil.getReqLogIdDelimStr(request)), module);
                        }
                    } else {
                        if (CmsUtil.verboseOn()) {
                            Debug.logInfo("Cms: View mapping found, but is inactive: " + 
                                    viewMapping.getLogIdRepr() + "; ignoring" + CmsControlUtil.getReqLogIdDelimStr(request), module);
                        }
                    }
                } else {
                    if (CmsUtil.verboseOn()) {
                        Debug.logInfo("Cms: No view mapping found for: " + CmsViewMapping.makeLogIdRepr(name, webSiteId)
                            + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    }
                }
            } catch (Exception e) {
                // an exception is thrown, return a 500 error
                Debug.logError(e, "Cms: Error retrieving view mapping or page from database. URI: " 
                        + path + CmsControlUtil.getReqLogIdDelimStr(request), module);
                handleException(request, response, e, renderMode);
                return; // Nothing can be sent after this
            }
        } 
       
        boolean renderDefault;
        
        if (cmsPage != null) {
            //CmsPageInfo cmsPageInfo = new CmsPageInfo(cmsPage);
            
            // 2016: not applicable for local cms
//            if (pageFromPathWildcard) {
//                // Special case; here cmsPageInfo points to a page representing a base directory of pages
//                
//                String resolvedPagePath;
//                CmsPageInfo resolvedPageInfo;
//                if (UtilValidate.isNotEmpty(procExtraPathInfo)) {
//                    resolvedPagePath = PathUtil.concatPaths(cmsPageInfo.getCmsPageReqPath(), procExtraPathInfo);
//                    resolvedPageInfo = new CmsPageInfo("_NA_", resolvedPagePath);
//                }
//                else {
//                    // If there's no extra path info, just invoke the base page (this can be blocked at process forwarding level)
//                    resolvedPagePath = cmsPageInfo.getCmsPageReqPath();
//                    resolvedPageInfo = cmsPageInfo;
//                }
//                
//                Debug.logInfo("Cms: CMS wildcard page mapping found; processing view through CMS: request: " + path + "; view name: " + name + 
//                        "; CMS page mapping: " + resolvedPageInfo.getLogIdRepr() + ControlUtil.getReqLogIdDelimStr(request), module);
//                renderDefault = false;
//                boolean continueOk = renderCmsPage(request, response, path, resolvedPageInfo, cmsView, webSiteId);
//                if (!continueOk) {
//                    return;
//                }
//            }
//            else {
            Debug.logInfo("Cms: " + (renderMode == CmsCallType.OFBIZ_PREVIEW ? "PREVIEW: " : "") + "CMS page mapping found; processing view through CMS: request: " + path + "; view name: " + name + 
                    "; CMS page mapping: " + cmsPage.getLogIdRepr() + CmsControlUtil.getReqLogIdDelimStr(request), module);
            renderDefault = false;
            boolean continueOk = renderCmsPage(request, response, path, cmsPage, cmsView, webSiteId, renderMode, writer);
            if (!continueOk) {
                return;
            }
//            }
        } else {
            renderDefault = true;
        }
        
        if (renderDefault) {
            if (this.useDefaultCmsPage && UtilValidate.isNotEmpty(this.defaultCmsPageId)) {
                try {
                    cmsPage = CmsPage.getWorker().findById(delegator, this.defaultCmsPageId, true, request);
                    if (cmsPage == null) {
                        throw new CmsDataException("Unable to find CMS page with pageId: " + this.defaultCmsPageId);
                    }
                } catch(Exception e) {
                    Debug.logError(e, "Cms: Error retrieving default page from database. defaultCmsPageId: " + this.defaultCmsPageId
                            + CmsControlUtil.getReqLogIdDelimStr(request), module);
                    handleException(request, response, e, renderMode);
                    return; // Nothing can be sent after this
                }
                
                Debug.logInfo("Cms: " + (renderMode == CmsCallType.OFBIZ_PREVIEW ? "PREVIEW: " : "") + "No existing or active CMS page mapping found for view '" + name + "'; rendering default CMS page (" + defaultCmsPage.getLogIdReprTargetPage() + ")"
                        + CmsControlUtil.getReqLogIdDelimStr(request), module);
                boolean continueOk = renderCmsPage(request, response, path, cmsPage, cmsView, webSiteId, renderMode, writer);
                if (!continueOk) {
                    return;
                }
            }
            else {
                Debug.logInfo("Cms: No existing or active CMS page mapping found for view '" + name + "'; continuing with Ofbiz screen (" + page + ")"
                        + CmsControlUtil.getReqLogIdDelimStr(request), module);
                renderScreen(name, page, info, contentType, encoding, request, response, writer); 
            }
        }
    }
    
    private void renderScreen(String name, String page, String info, String contentType, String encoding, HttpServletRequest request, HttpServletResponse response, Writer writer) throws ViewHandlerException {
        if (CmsUtil.verboseOn()) {
            Debug.logInfo("Cms: Starting legacy screen widget render process" + CmsControlUtil.getReqLogIdDelimStr(request), module);
        }
        
        if (setResponseBrowserNoCacheScreen) {
            if (CmsUtil.verboseOn()) {
                Debug.logInfo("Cms: Setting browser no-proxy no-cache response" + CmsControlUtil.getReqLogIdDelimStr(request), module);
            }
            CmsControlUtil.checkSetNoCacheResponse(request, response);
        }
        
        super.render(name, page, info, contentType, encoding, request, response, writer); 
    }
    
    /**
     * Renders page.
     * <p>
     * DEV NOTE: do not use the "path" parameter.
     * 
     * @return false if must prevent any further web response
     */
    private boolean renderCmsPage(HttpServletRequest request, HttpServletResponse response, String path, CmsPage cmsPage, CmsView cmsView, String webSiteId, CmsCallType renderMode, Writer writer) throws ViewHandlerException {
        
        // We must make sure that if for whatever reason a transaction is in place, maybe
        // some runaway transaction, we kill it now, because the CMS rendering happens in a 
        // different thread and this could end in a total lockup if read/writing the same entity 
        // value if keep a transaction here.
        // Note: I wasn't sure if should do commit/rollback or suspend/resume here/after, 
        // but I'm not sure implications of suspend w.r.t. multithread here so doing commit/rollback for now
        endTransactionAlways(request, response);
        
        if (CmsUtil.verboseOn()) {
            Debug.logInfo("Cms: Starting CMS page render process" + CmsControlUtil.getReqLogIdDelimStr(request), module);
        }
        
        if (setResponseBrowserNoCacheCmsPage) {
            if (CmsUtil.verboseOn()) {
                Debug.logInfo("Cms: Setting browser no-proxy no-cache response" + CmsControlUtil.getReqLogIdDelimStr(request), module);
            }
            CmsControlUtil.checkSetNoCacheResponse(request, response);
        }
        
        try {
            
            // Main render invocation
            renderInvoker.invokeCmsRendering(request, response, servletContext, cmsPage, cmsView, webSiteId, renderMode, writer);
            
        // NOTE: 2016: this is never thrown from local cms renders, but leaving here for future use
//        } catch (CmsCallHttpException e) {
//            
//            int httpStatus = e.getHttpStatus();
//            int returnCode = passOnHttpStatusesFromCms.contains(httpStatus) ? httpStatus : HttpServletResponse.SC_INTERNAL_SERVER_ERROR;
//
//            Exception exToPrint = e;
//            if (returnCode == HttpServletResponse.SC_NOT_FOUND) {
//                // too verbose
//                exToPrint = null;
//            }
//            
//            Debug.logError(exToPrint, "Cms: Error rendering page " + cmsPage.getLogIdRepr() + 
//                    " for view " + cmsView.getLogIdRepr() + " via CMS invocation: " + e.getMessage() + CmsControlUtil.getReqLogIdDelimStr(request), module);
//            try {
//                
//                response.sendError(returnCode);
//            } catch (IOException e1) {
//                Debug.logError(e1, "Cms: Error sending server error response" + CmsControlUtil.getReqLogIdDelimStr(request), module);
//            }
//            return false; // Nothing can be sent after this
        } catch (Exception e) {
            Debug.logError(e, "Cms: Error rendering page " + cmsPage.getLogIdRepr() + 
                    " for view " + cmsView.getLogIdRepr() + " via CMS invocation: " + e.getMessage() + CmsControlUtil.getReqLogIdDelimStr(request), module);
            handleException(request, response, e, renderMode);
            return false; // Nothing can be sent after this
        }
        return true;
    }
    
    @Override
    public void render(String name, String page, String info, String contentType, String encoding,
            HttpServletRequest request, HttpServletResponse response) throws ViewHandlerException {
        Writer writer;
        try {
            writer = CmsControlUtil.getResponseWriter(request, response);
        } catch (IOException e) {
            Debug.logError(e, "Cms: Error getting response writer: " + e.getMessage() + CmsControlUtil.getReqLogIdDelimStr(request), module);
            handleException(request, response, e, null);
            return;
        }
        this.render(name, page, info, contentType, encoding, request, response, writer);
    }

    static void handleException(HttpServletRequest request, HttpServletResponse response, Exception ex, CmsCallType renderMode) throws ViewHandlerException {
        // 2017-03-24: only report detailed error if preview/debug mode
        if (renderMode == CmsCallType.OFBIZ_PREVIEW) {
            rethrowViewHandlerExceptionDetailed(request, ex);
        } else {
            if (CmsRenderUtil.getLiveExceptionMode(request.getServletContext()) == UtilRender.RenderExceptionMode.DEBUG) {
                rethrowViewHandlerExceptionDetailed(request, ex);
            } else {
                // OLD CODE: this was secure, but prevented controller from handling the error...
                // instead, rethrow an error with a generic message
//                try {
//                    response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
//                } catch (IOException e1) {
//                    Debug.logError(e1, "Cms: Error sending server error response" + CmsControlUtil.getReqLogIdDelimStr(request), module);
//                }
                rethrowViewHandlerExceptionGeneric(request, ex);
            }
        }
    }
    
    static void rethrowViewHandlerExceptionDetailed(HttpServletRequest request, Exception ex) throws ViewHandlerException {
        // EMULATION of MacroScreenViewHandler behavior - these throws would in stock be handled by ControlServlet and printed
        try {
            throw ex;
        } catch (TemplateException e) {
            Debug.logError(e, "Error initializing screen renderer", module);
            throw new ViewHandlerException(e.getMessage());
        } catch (IOException e) {
            throw new ViewHandlerException("Error in the response writer/output stream: " + e.toString(), e);
        } catch (SAXException e) {
            throw new ViewHandlerException("XML Error rendering page: " + e.toString(), e);
        } catch (ParserConfigurationException e) {
            throw new ViewHandlerException("XML Error rendering page: " + e.toString(), e);
        } catch (GeneralException e) {
            throw new ViewHandlerException("Lower level error rendering page: " + e.toString(), e);
        } catch (Exception e) {
            throw new ViewHandlerException("General error rendering page: " + e.toString(), e);
        } 
    }
    
    // generic exception to avoid divulging information in live render, for security; does NOT propagate the original error (already logged)
    static void rethrowViewHandlerExceptionGeneric(HttpServletRequest request, Exception ex) throws ViewHandlerException {
        final String msg = RequestUtil.getGenericErrorMessage(request);
        throw new ViewHandlerException(msg, new Exception(msg));
    }
        
    /**
     * This kills any transaction running. 
     * <p>
     * It currently does it by rolling back rather than
     * committing because there should never be a runaway transaction! If there is one it means
     * there was a code or system error, has to be fixed and the data could be bad, so don't save it.
     * Also, this is what Ofbiz's ControlServlet does with runaway transactions.
     */
    private static void endTransactionAlways(HttpServletRequest request, HttpServletResponse response) {
        
        if (CmsUtil.verboseOn()) {
            Debug.logInfo("Cms: Stopping any open Ofbiz transactions" + CmsControlUtil.getReqLogIdDelimStr(request), module);
        }
        
        boolean transInPlace = false;
        try {
            transInPlace = TransactionUtil.isTransactionInPlace();
        } catch (GenericTransactionException e) {
            Debug.logError(e, "Cms: Unable to verify if transaction is in place at " +
                    "time of CMS invocation; invocation may be dangerous!" + CmsControlUtil.getReqLogIdDelimStr(request), module);
        }
        if (transInPlace) {
            
            // Note: I think commit() or rollback() do the majority of the work here.
            Debug.logWarning("Cms: A transaction was in place at time of CMS invocation (runaway?); " +
                    "performing rollback (to prevent lockups)" + CmsControlUtil.getReqLogIdDelimStr(request), module);
            try {
                TransactionUtil.rollback();
            } catch (GenericTransactionException e) {
                Debug.logError(e, "Cms: Could not rollback transaction at time of CMS invocation" + CmsControlUtil.getReqLogIdDelimStr(request), module);
            }
 
            // I think there's no point to this... commit() and rollback() take care of most of the work,
            // and if there's a scenario where somehow the transaction wasn't ended, it will 99% likely be
            // shown in the exception catch above.
            // Also note: 
//            // Sanity check
//            transInPlace = false;
//            try {
//                transInPlace = TransactionUtil.isTransactionInPlace();
//            } catch (GenericTransactionException e) {
//                Debug.logError(e, "Cms: Unable to verify that no transaction is still in place at time of " +
//                        "CMS invocation following rollback; invocation may be dangerous!" + CmsControlUtil.getReqLogIdDelimStr(request), module);
//            }
//            if (transInPlace) {
//                Debug.logWarning("Cms: A transaction was still in place at time of CMS invocation following " +
//                        "commit and rollback attempts; invocation may be dangerous!" + CmsControlUtil.getReqLogIdDelimStr(request), module);
//            }
        }
    }
    
}

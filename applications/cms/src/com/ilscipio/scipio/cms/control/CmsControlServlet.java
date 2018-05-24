package com.ilscipio.scipio.cms.control;

import java.io.IOException;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.HttpSession;

import org.apache.commons.lang.StringUtils;
import org.apache.http.HttpStatus;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.webapp.website.WebSiteWorker;

import com.ilscipio.scipio.cms.CmsUtil;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.control.cmscall.CmsCallType;

/**
 * Control servlet for the cms and website webapps (OLD - pre-2013).
 * @deprecated: 2016: fully superseded by {@link CmsProcessFilter}, and possibly unsafe (security) - 
 * client webapps must use {@link CmsProcessFilter} in their web.xml.
 * <p>
 * <strong>WARNING</strong>: WHEN READING THIS AS REFERENCE, be aware that this worked only due to a special
 * handling in ofbiz's ContextFilter of paths that causes filtering (allowedPaths) to be bypassed
 * when a servlet is mapped to <code>/*</code>; so this servlet worked by bypassing
 * the ContextFilter through unwritten rules (bypassing allowedPaths), and required no changes to
 * ContextFilter's <code>allowedPaths</code>. The CmsProcessFitler is not subject to this
 * and uses explicit/cleaner behaviors to bypass ContextFilter (typically running before it).
 * In other words, this was dicey, as it is not clear it is safe to rely on that mechanism 
 * to bypass ContextFilter, because it probably causes <em>all</em> paths to bypass ContextFilter.
 * <p>
 * Include:
 * <pre>
 * {@code
 * <servlet>
 *     <servlet-name>CmsControlServlet</servlet-name>
 *     <display-name>CmsControlServlet</display-name>
 *     <description>URI to Page Mapping Servlet</description>
 *     <servlet-class>com.ilscipio.scipio.cms.control.CmsControlServlet</servlet-class>
 *     <load-on-startup>1</load-on-startup>
 * </servlet>
 * <servlet-mapping>
 *     <servlet-name>CmsControlServlet</servlet-name>
 *     <url-pattern>/*</url-pattern>
 * </servlet-mapping>
 * }
 * </pre>
 */
@SuppressWarnings("serial")
@Deprecated
public class CmsControlServlet extends HttpServlet {
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * The pageMountPoint sets the path which responds to page ids to return
     * individual pages. Example: /cms/{$previewMountPoint}/${pageId}
     */
    // 2016: we now have multiple possible targets, use plain no auth for now (FIXME)
    //protected static String cmsMountPoint = "/control/page";
    protected static String cmsMountPoint = "/control/cmsPagePlainNoAuth";
    
    protected static String cmsDefaultHome = "/control/main";
    protected static String liveMountPoint = "/page";
    protected static String previewMountPoint = "/preview";
    protected static String errorPage = "/error/error.jsp";

    /**
     * @see javax.servlet.Servlet#init(javax.servlet.ServletConfig)
     */
    @Override
    public void init(ServletConfig config) throws ServletException {
        super.init(config);
    }
    
    /**
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        String path = request.getPathInfo();
        String webSiteId = WebSiteWorker.getWebSiteId(request);
        CmsPage page = null;
        CmsCallType renderMode = null;
        
        Delegator delegator = getDelegator(request);

        if (CmsUtil.verboseOn())
            Debug.logInfo("[Processing Request]: " + path + " sessionId="
                    + UtilHttp.getSessionId(request), module);

        // There has to be a path following the servlet mount point to identify
        // the page to render
        if (UtilValidate.isNotEmpty(path) && !path.equals(liveMountPoint)) {
            
                //String lowerPath = path.toLowerCase(); // 2016: no
                if (!path.endsWith(".js") && !path.endsWith(".png") && !path.endsWith(".css")
                        && !path.endsWith(".jpeg") && !path.endsWith(".gif") && !path.endsWith(".jpg")) {
                    try {
                        // Decide if this is a page preview or the live version of the
                        // page
                        if (StringUtils.startsWith(path, previewMountPoint)) {
                            page = CmsPage.getWorker().findByPath(delegator, StringUtils.removeStart(path, 
                                    previewMountPoint), webSiteId, true); // NOTE: pass cache true here, always
                            renderMode = CmsCallType.OFBIZ_PREVIEW;
                        } else {
                            // Nope, no preview. Just find the page for live rendering.
                            page = CmsPage.getWorker().findByPath(delegator, path, webSiteId, true); // NOTE: pass cache true here, always
                        }
                    } catch (Exception e) {
                        // an exception is thrown, return a 500 error
                        Debug.logError(e, "Error retrieving page from database. URI: " + path, module);
                        response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                        return;
                    }
                }

        } else {
            page = (CmsPage) request.getAttribute("cmsPage");
        }
        
        if (page != null && (renderMode == CmsCallType.OFBIZ_PREVIEW || page.isActive())) {
            try {
                // 2016: leave this to screen view handler
                //CmsPageContext context = new CmsPageContext(request, response, request.getServletContext(), webSiteId, preview);
                request.setAttribute("cmsPage", page);
                //request.setAttribute("cmsPageContext", context);
                request.setAttribute("cmsPageRenderMode", renderMode); // 2016: new
                Debug.logInfo("Dispatching request to "+cmsMountPoint, module);
                RequestDispatcher rd = request.getRequestDispatcher(cmsMountPoint);
                rd.forward(request, response);
            } catch (Exception e) {
                Debug.logError(e, "Error rendering page: " + path, module);
                response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
            }
        } else {
            //Locale locale = UtilHttp.getLocale(request);
            //request.setAttribute("_ERROR_MSG_", ErrorCodes.ERR_PRODUCT_NOT_AVAILABLE.getErrorMessage(locale));
            if (UtilValidate.isEmpty(path)) {
                RequestDispatcher rd = request.getRequestDispatcher(cmsDefaultHome);
                response.setStatus(HttpStatus.SC_NOT_FOUND);
                rd.forward(request, response);
            } else {
                //FIXME: This will break when used in combination with the actual screen locations "control/somethingsomething"
                RequestDispatcher rd = request.getRequestDispatcher("/control/pagenotfound");
                response.setStatus(HttpStatus.SC_NOT_FOUND);
                rd.forward(request, response);
            }
        }
    }

    /**
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    @Override
    public void doPost(HttpServletRequest request, HttpServletResponse response)
            throws ServletException, IOException {
        doGet(request, response);
    }

    /**
     * @see javax.servlet.Servlet#destroy()
     */
    @Override
    public void destroy() {
        super.destroy();
    }

    /**
     * FIXME: the fact this seems needed (?) is sign of missing webapp initialization somewhere.
     */
    private static Delegator getDelegator(HttpServletRequest request) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        if (delegator == null) {
            // EMULATED FROM ControlServlet
            HttpSession session = request.getSession(false);
            if (session != null) {
                String delegatorName = (String) session.getAttribute("delegatorName");
                if (UtilValidate.isNotEmpty(delegatorName)) {
                    delegator = DelegatorFactory.getDelegator(delegatorName);
                }
                if (delegator == null) {
                    delegator = (Delegator) session.getServletContext().getAttribute("delegator");
                }
            } 
            if (delegator == null) {
                Debug.logWarning("CMS: delegator not found through session or servlet context; "
                        + "falling back to default system decorator", module);
                delegator = DelegatorFactory.getDelegator("default");
            }
        } 
        return delegator;
    }
}

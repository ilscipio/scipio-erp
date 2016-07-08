package com.ilscipio.scipio.cms.control;

import java.io.IOException;
import java.util.Locale;
import java.util.Map;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.apache.http.HttpStatus;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.website.WebSiteWorker;
import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContext;

@SuppressWarnings("serial")
public class CmsControlServlet extends HttpServlet {
    /**
     * The pageMountPoint sets the path which responds to page ids to return
     * individual pages. Example: /cms/{$previewMountPoint}/${pageId}
     */
    protected static String cmsMountPoint = "/control/page";
    protected static String liveMountPoint = "/page";
    protected static String previewMountPoint = "/preview";
    protected static String errorPage = "/error/error.jsp";
    public static final String module = CmsControlServlet.class.getName();

    
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
        boolean preview = false;

        if (Debug.verboseOn())
            Debug.logVerbose("[Processing Request]: " + path + " sessionId="
                    + UtilHttp.getSessionId(request), module);

        // There has to be a path following the servlet mount point to identify
        // the page to render
        if (UtilValidate.isNotEmpty(path) && !path.equals(liveMountPoint)){
            
                String lowerPath = path.toLowerCase();
                if (!lowerPath.endsWith(".js") && !lowerPath.endsWith(".png") && !lowerPath.endsWith(".css")
                        && !lowerPath.endsWith(".jpeg") && !lowerPath.endsWith(".gif") && !lowerPath.endsWith(".jpg")) {
                    try {
                        // Decide if this is a page preview or the live version of the
                        // page
                        if (StringUtils.startsWith(lowerPath, previewMountPoint)) {
                            page = CmsPage
                                    .findByPath(StringUtils.removeStart(lowerPath, previewMountPoint), webSiteId);
                            preview = true;
                        } else {
                            // Nope, no preview. Just find the page for live rendering.
                            page = CmsPage.findByPath(lowerPath, webSiteId);
                        }
                    } catch (Exception e) {
                        // an exception is thrown, return a 500 error
                        Debug.logError(e, "Error retrieving page from database. URI: " + lowerPath, module);
                        response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                        return;
                    }
                }

        }else{
            page = (CmsPage) request.getAttribute("cmsPage");
        }
        
        if (page != null && (preview || page.isActive())) {
            try {
                CmsPageContext context = new CmsPageContext(request, response, getServletConfig(),
                        webSiteId, preview);
                request.setAttribute("cmsPage", page);
                request.setAttribute("cmsPageContext", context);
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
            
            //FIXME: This will break when used in combination with the actual screen locations "control/somethingsomething"
            RequestDispatcher rd = request.getRequestDispatcher("/control/pagenotfound");
            response.setStatus(HttpStatus.SC_NOT_FOUND);
            rd.forward(request, response);

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

}

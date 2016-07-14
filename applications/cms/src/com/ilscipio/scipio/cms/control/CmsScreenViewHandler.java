package com.ilscipio.scipio.cms.control;

import java.io.IOException;

import javax.servlet.RequestDispatcher;
import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.view.ViewHandlerException;
import org.ofbiz.webapp.website.WebSiteWorker;
import org.ofbiz.widget.renderer.macro.MacroScreenViewHandler;

import com.ilscipio.scipio.cms.content.CmsPage;
import com.ilscipio.scipio.cms.content.CmsPageContext;

public class CmsScreenViewHandler extends MacroScreenViewHandler {

    protected static String cmsMountPoint = "/control/page";
    protected static String liveMountPoint = "/page";
    protected static String errorPage = "/error/error.jsp";
    protected static String previewMountPoint = "/preview";
   
    public CmsScreenViewHandler() {
        // TODO Auto-generated constructor stub
    }
    
    @Override
    public void init(ServletContext context) throws ViewHandlerException {
        // TODO Auto-generated method stub
        super.init(context);
    }

    @Override
    public void render(String name, String page, String info, String contentType, String encoding,
            HttpServletRequest request, HttpServletResponse response) throws ViewHandlerException {

        // TODO Stuff
        String path = request.getPathInfo();
       
        String webSiteId = WebSiteWorker.getWebSiteId(request);
        CmsPage cmsPage = null;
        boolean preview = false;

        if (Debug.verboseOn()) Debug.logVerbose("[Processing Request]: " + path + " sessionId=" + UtilHttp.getSessionId(request), module);
        String lowerCaseName = name.toLowerCase();
        Debug.logInfo("[Processing Request through new ScreenHandler]: " + path +" Name is "+lowerCaseName+ " sessionId="
                        + UtilHttp.getSessionId(request), module);
        // There has to be a path following the servlet mount point to identify the page to render
        if (UtilValidate.isNotEmpty(lowerCaseName)) {
            try {
                // Decide if this is a page preview or the live version of the  page
                if (StringUtils.startsWith(lowerCaseName, previewMountPoint)) {
                    cmsPage = CmsPage.findByPath(
                            StringUtils.removeStart(lowerCaseName, previewMountPoint),
                            webSiteId);
                    preview = true;
                } else {
                    // Nope, no preview. Just find the page for live rendering.
                    cmsPage = CmsPage.findByPath(lowerCaseName, webSiteId);
                }
                // Get page content as formatted string for output
                
            } catch (Exception e) {
                // an exception is thrown, return a 500 error
                Debug.logError(e, "Error retrieving page from database. URI: " + path, module);
                try {
                    response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                } catch (IOException e1) {
                    // TODO Auto-generated catch block
                    e1.printStackTrace();
                }
            }
        }
        
       

        if (cmsPage != null && (preview || cmsPage.isActive())) {
            try {
                CmsPageContext context = new CmsPageContext(request, response, null, webSiteId, preview);
                request.setAttribute("cmsPage", cmsPage);
                request.setAttribute("cmsPageContext", context);
                Debug.logInfo("Page found, using cms screen", module);
                RequestDispatcher rd = request.getRequestDispatcher(cmsMountPoint);
                rd.forward(request, response);
            } catch (Exception e) {
                Debug.logError(e, "Error rendering page: " + path, module);
                try {
                    response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
                } catch (IOException e1) {
                    // TODO Auto-generated catch block
                    e1.printStackTrace();
                }
            }
        } else {
            // Render normally
            Debug.logInfo("No page found, continue with regular screen", module);
            super.render(name, page, info, contentType, encoding, request, response);
        }

    }

}

package org.ofbiz.webapp.renderer;

import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilFormatOut;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.control.ConfigXMLReader;
import org.ofbiz.webapp.control.RequestHandler;
import org.ofbiz.webapp.control.RequestHandlerException;
import org.ofbiz.webapp.control.WebAppConfigurationException;
import org.ofbiz.webapp.view.ViewFactory;
import org.ofbiz.webapp.view.ViewHandler;
import org.ofbiz.webapp.view.ViewHandlerException;
import org.ofbiz.webapp.view.ViewHandlerExt;

/**
 * SCIPIO: Special events that can be invoked that provide renderer and view handling emulation.
 */
public abstract class RenderEvents {

    public static final String module = RenderEvents.class.getName();
    
    public static final String RENDER_VIEW_ATTR_PREFIX = "renderView_";
    
    public static final List<String> allowedOutParamNames = Collections.unmodifiableList(Arrays.asList(new String[] {
        "_ERROR_MESSAGE_", "_ERROR_MESSAGE_LIST_", "_EVENT_MESSAGE_", "_EVENT_MESSAGE_LIST_"
    }));
    
    protected RenderEvents() {
    }

    /**
     * Renders a requested view.
     * NOTE: only supports view handlers that implement ViewHandlerExt.
     * WARN: FIXME?: this is a PARTIAL emulation of RequestHandler, this means large duplication
     * and does not guarantee exact same behavior...
     * FIXME?: missing ":_protect_:" handling? are we bypassing it?
     * TODO: REVIEW the response configuration, may ruin json?
     */
    public static String renderViewToOutParams(HttpServletRequest request, HttpServletResponse response) {
        /* this is DANGEROUS to expose - instead, accept single view name, which is both safer and easier
        String name = getStrParam(request, "name", RENDER_VIEW_ATTR_PREFIX);
        String type = getStrParam(request, "type", RENDER_VIEW_ATTR_PREFIX);
        String page = getStrParam(request, "page", RENDER_VIEW_ATTR_PREFIX);
        String info = getStrParam(request, "info", RENDER_VIEW_ATTR_PREFIX);
        String contentType = getStrParam(request, "contentType", RENDER_VIEW_ATTR_PREFIX);
        String encoding = getStrParam(request, "encoding", RENDER_VIEW_ATTR_PREFIX);
        */
        String view = getStrParam(request, "view", RENDER_VIEW_ATTR_PREFIX);
        
        HttpServletRequest req = request;
        HttpServletResponse resp = response;
        try {
            if (UtilValidate.isEmpty(view)) {
                throw new RequestHandlerException("View name is empty");
            }
            
            RequestHandler rh = RequestHandler.getRequestHandler(request.getServletContext());
            ViewFactory viewFactory = rh.getViewFactory();
            StringWriter sw = new StringWriter();
            
            // SCIPIO: FIXME?: DUPLICATED FROM RequestHandler.renderView - should de-duplicate
            
            ConfigXMLReader.ViewMap viewMap = null;
            try {
                viewMap = (view == null ? null : rh.getControllerConfig().getViewMapMap().get(view));
            } catch (WebAppConfigurationException e) {
                Debug.logError(e, "Exception thrown while parsing controller.xml file: ", module);
                throw new RequestHandlerException(e);
            }
            if (viewMap == null) {
                throw new RequestHandlerException("No definition found for view with name [" + view + "]");
            }

            String nextPage;

            if (viewMap.page == null) {
                throw new RequestHandlerException("No view to render.");
            } else {
                nextPage = viewMap.page;
            }
            
            // before mapping the view, set a request attribute so we know where we are
            req.setAttribute("_CURRENT_VIEW_", view);
            
            if (Debug.verboseOn()) Debug.logVerbose("[Mapped To]: " + nextPage + " sessionId=" + UtilHttp.getSessionId(req), module);

            //long viewStartTime = System.currentTimeMillis();

            // setup character encoding and content type
            String charset = UtilFormatOut.checkEmpty(rh.getCharset(), req.getCharacterEncoding(), "UTF-8");

            String viewCharset = viewMap.encoding;
            //NOTE: if the viewCharset is "none" then no charset will be used
            if (UtilValidate.isNotEmpty(viewCharset)) {
                charset = viewCharset;
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

            // TODO: REVIEW
//            if (charset.length() > 0 && !"none".equals(charset)) {
//                resp.setContentType(contentType + "; charset=" + charset);
//            } else {
//                resp.setContentType(contentType);
//            }

            if (Debug.verboseOn()) Debug.logVerbose("The ContentType for the " + view + " view is: " + contentType, module);

            // TODO: REVIEW
//            boolean viewNoCache = viewMap.noCache;
//            if (viewNoCache) {
//               UtilHttp.setResponseBrowserProxyNoCache(resp);
//               if (Debug.verboseOn()) Debug.logVerbose("Sending no-cache headers for view [" + nextPage + "]", module);
//            }

            try {
                if (Debug.verboseOn()) Debug.logVerbose("Rendering view [" + nextPage + "] of type [" + viewMap.type + "]", module);
                ViewHandler vh = viewFactory.getViewHandler(viewMap.type);
                
                // SCIPIO: custom
                if (vh instanceof ViewHandlerExt) {
                    ViewHandlerExt vhe = (ViewHandlerExt) vh;
                    vhe.render(view, nextPage, viewMap.info, contentType, charset, req, resp, sw);
                } else {
                    throw new ViewHandlerException("View handler does not support extended interface (ViewHandlerExt)");
                }
            } catch (ViewHandlerException e) {
                Throwable throwable = e.getNested() != null ? e.getNested() : e;

                throw new RequestHandlerException(e.getNonNestedMessage(), throwable);
            }
            
            getRenderOutParams(request).put("renderOut", sw.toString());
            // NOTE: extra params may be in scipioOutParams req attr map or named in scipioOutAttrNames req attr list
        } catch(RequestHandlerException e) {
            Debug.logError(e, "Request handler error while rendering view: " + view, module);
            request.setAttribute("_ERROR_MESSAGE_", "Error rendering view with name [" + view + "]");
            return "error"; 
        } catch(Exception e) {
            Debug.logError(e, "Error while rendering view: " + view, module);
            request.setAttribute("_ERROR_MESSAGE_", "Error rendering view with name [" + view + "]");
            return "error";
        } finally {
            getRenderOutAttrNames(request).addAll(getAllowedOutParamNames(request));
        }
        
        return "success";
    }

    public static List<String> getAllowedOutParamNames(HttpServletRequest request) {
        return allowedOutParamNames;
    }
    
    private static String getStrParam(HttpServletRequest request, String name, String attrPrefix) {
        String param = (String) request.getAttribute(attrPrefix + name);
        if (param != null) {
            return param;
        } else {
            return request.getParameter(name);
        }
    }
    
    // FIXME: duplicated from CommonEvents due to build problems
    private static Map<String, Object> getRenderOutParams(HttpServletRequest request) { // SCIPIO
        Map<String, Object> outParams = UtilGenerics.checkMap(request.getAttribute("scipioOutParams"));
        if (outParams == null) {
            outParams = new HashMap<>();
        }
        request.setAttribute("scipioOutParams", outParams);
        return outParams;
    }
    // FIXME: duplicated from CommonEvents due to build problems
    private static List<String> getRenderOutAttrNames(HttpServletRequest request) { // SCIPIO
        List<String> outAttrNames = UtilGenerics.checkList(request.getAttribute("scipioOutAttrNames"));
        if (outAttrNames == null) {
            outAttrNames = new ArrayList<>();
        }
        request.setAttribute("scipioOutAttrNames", outAttrNames);
        return outAttrNames;
    }

}

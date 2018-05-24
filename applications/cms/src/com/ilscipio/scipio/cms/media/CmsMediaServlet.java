
package com.ilscipio.scipio.cms.media;

import java.io.IOException;
import java.io.InputStream;
import java.util.Locale;
import java.util.Map;

import javax.servlet.ServletConfig;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.content.data.DataResourceWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.DelegatorFactory;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.cms.CmsUtil;

/**
 * Servlet used to serve media files, which basically consists in getting the
 * corresponding raw data from database and stream it in the response.
 * FIXME?: 2017-08-08: currently there is little to no use of useCache...
 */
@SuppressWarnings("serial")
public class CmsMediaServlet extends HttpServlet {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final String FN_SOURCE = UtilProperties.getPropertyValue("cms", "media.serve.filename.source", "name");
    private static final String fnSrcFieldName = "origfn".equals(FN_SOURCE) ? "objectInfo" : "dataResourceName";
    private static final String fnSrcFieldNameFallback = "origfn".equals(FN_SOURCE) ? "dataResourceName" : "objectInfo";
    private static final boolean variantsEnabled = UtilProperties.getPropertyAsBoolean("cms", "media.variants.enabled", true);
    
    public CmsMediaServlet() {
        super();
    }

    /**
     * @see javax.servlet.http.HttpServlet#init(javax.servlet.ServletConfig)
     */
    @Override
    public void init(ServletConfig config) throws ServletException {
        super.init(config);
    }

    /**
     * @see javax.servlet.http.HttpServlet#doPost(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     */
    @Override
    public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        doGet(request, response);
    }

    /**
     * @see javax.servlet.http.HttpServlet#doGet(javax.servlet.http.HttpServletRequest,
     *      javax.servlet.http.HttpServletResponse)
     *      
     * reference: {@link org.ofbiz.content.data.DataEvents#serveImage}
     * 
     * TODO: still missing an "auto" best-size selection based on width and height
     * TODO: this isn't looking at the global debug flag yet for the error msgs
     * WARN: autoVariant logic has severe limitations - see {@link CmsMediaWorker#selectBestImageVariant}
     */
    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // SPECIAL: getDelegator/getDispatcher methods required so tenant db doesn't break (or breaks less)
        Delegator delegator = getDelegator(request);
        LocalDispatcher dispatcher = getDispatcher(request);
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        
        String contentId = request.getParameter("contentId");
        String dataResourceId = request.getParameter("dataResourceId");
        
        String variant = null; // this specifies an exact variant to use, by name
        ImageVariantConfig.FitMode autoVariantMode = null; // this tries to find a best-fit variant
        String widthStr = null;
        String heightStr = null;
        if (variantsEnabled) {
            variant = request.getParameter("variant");
            autoVariantMode = ImageVariantConfig.FitMode.fromStrNameParamSafe(request.getParameter("autoVariant"));
            widthStr = request.getParameter("width");
            heightStr = request.getParameter("height");
            if (autoVariantMode == null && (UtilValidate.isNotEmpty(widthStr) || UtilValidate.isNotEmpty(heightStr))) {
                autoVariantMode = ImageVariantConfig.FitMode.DEFAULT;
            }
        }
        
        GenericValue dataResource;
        try {
            String isPublic;
            
            // AUTO VARIANT MODE - more difficult
            if (autoVariantMode != null) {
                /**
                 * Tries to find the best image variant name to use for the width & height.
                 * WARN/FIXME: this has serious limitations in current form - we are forced to use
                 * ImageProperties.xml to get the dimensions rather than the actual resized dims of the
                 * images - there is no point fixing this here currently because can't solve this
                 * problem for the file-based storage elsewhere yet.
                 */
                ImageVariantConfig imgVariantCfg = CmsMediaWorker.getDefaultCmsImageVariantConfig();
                if (imgVariantCfg != null) {
                    if (CmsUtil.verboseOn()) {
                        Debug.logInfo("Cms: Auto-selecting image variant [contentId: " + contentId + ", mode: " + autoVariantMode.getStrName() + "]", module);
                    }
                    ImageVariantConfig.VariantInfo variantInfo = imgVariantCfg.getCanvasBestFitVariant(autoVariantMode, 
                            UtilValidate.isNotEmpty(widthStr) ? Integer.parseInt(widthStr) : null, 
                            UtilValidate.isNotEmpty(heightStr) ? Integer.parseInt(heightStr) : null);
                    if (variantInfo != null) {
                        variant = variantInfo.getName();
                        if (CmsUtil.verboseOn()) {
                            Debug.logInfo("Cms: Auto-selected image variant [contentId: " + contentId + ", mode: " + autoVariantMode.getStrName() + ", variant: " + variant + "]", module);
                        }
                    } else {
                        if (CmsUtil.verboseOn()) {
                            Debug.logInfo("Cms: No best image variant available [contentId: " + contentId + ", mode: " + autoVariantMode.getStrName() + "]; defaulting to original", module);
                        }
                    }
                } else {
                    if (CmsUtil.verboseOn()) { // don'
                        Debug.logWarning("Cms: Cannot auto-select image variant - no image variant config (ImageProperties.xml) available for CMS", module);
                    }
                }
            }
            
            if ((UtilValidate.isEmpty(variant) || "original".equals(variant))) {
                // STANDARD CASE
                if (UtilValidate.isNotEmpty(dataResourceId)) {
                    dataResource = EntityUtil.getFirst(EntityQuery.use(delegator).from("DataResourceContentRequiredView").where("dataResourceId", dataResourceId).queryList());
                    if (dataResource == null) {
                        response.sendError(HttpServletResponse.SC_NOT_FOUND,
                                "Media not found with dataResourceId [" + dataResourceId + "]");
                        return;
                    }
                } else if (UtilValidate.isNotEmpty(contentId)) {
                    dataResource = EntityUtil.getFirst(EntityQuery.use(delegator).from("DataResourceContentRequiredView").where("coContentId", contentId).queryList());
                    if (dataResource == null) {
                        response.sendError(HttpServletResponse.SC_NOT_FOUND,
                                "Media not found with contentId [" + contentId + "]");
                        return;
                    }
                } else {
                    response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Missing or invalid dataResourceId or contentId parameter - cannot determine media");
                    return;
                }
                
                if (CmsUtil.verboseOn()) {
                    Debug.logInfo("Cms: Serving media (original) [contentId: " + contentId + "]", module);
                }
                
                isPublic = dataResource.getString("isPublic");
            } else {
                GenericValue origDataResource = null;
                if (UtilValidate.isNotEmpty(dataResourceId)) {
                    origDataResource = EntityUtil.getFirst(EntityQuery.use(delegator).from("DataResourceContentRequiredView").where("dataResourceId", dataResourceId).queryList());
                    if (origDataResource == null) {
                        response.sendError(HttpServletResponse.SC_NOT_FOUND,
                                "Media not found with dataResourceId [" + dataResourceId + "]");
                        return;
                    }
                    contentId = origDataResource.getString("coContentId");
                } else if (UtilValidate.isNotEmpty(contentId)) {
                    // if contentId is passed, we can currently skip the original's lookup
                    // NOTE: this could change, but trying to avoid...
                    ;
                } else {
                    response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Missing or invalid dataResourceId or contentId parameter - cannot determine media");
                    return;
                }
                
                // this implies we're getting IMAGE_OBJECT type
                GenericValue contentAssoc = EntityUtil.getFirst(EntityQuery.use(delegator).from("ContentAssoc").where("contentId", contentId, 
                        "contentAssocTypeId", "IMGSZ_" + variant.toUpperCase()).queryList());
                
                if (contentAssoc != null) {
                    dataResource = EntityUtil.getFirst(EntityQuery.use(delegator).from("DataResourceContentRequiredView").where("coContentId", contentAssoc.getString("contentIdTo")).queryList());
                    if (dataResource == null) {
                        response.sendError(HttpServletResponse.SC_NOT_FOUND,
                                "Media not found with contentId [" + contentId + "]");
                        return;
                    }
                    
                    if (CmsUtil.verboseOn()) {
                        Debug.logInfo("Cms: Serving image variant [contentId: " + contentId + ", variant: " + variant + ", variant contentId: " + dataResource.getString("coContentId") + "]", module);
                    }
                } else {
                    if (Debug.verboseOn()) {
                        Debug.logInfo("Cms: No variant image found [contentId: " + contentId + ", variant: " + variant + "]; serving original", module);
                    }
                    if (origDataResource != null) {
                        dataResource = origDataResource;
                    } else {
                        dataResource = EntityUtil.getFirst(EntityQuery.use(delegator).from("DataResourceContentRequiredView").where("coContentId", contentId).queryList());
                        if (dataResource == null) {
                            response.sendError(HttpServletResponse.SC_NOT_FOUND,
                                    "Media not found with contentId [" + contentId + "]");
                            return;
                        }
                    }
                }
                
                // WARN: we are getting the isPublic from the RESIZED image here, NOT the original; this may allow
                // faster lookup (and more exact), but it relies on the media services properly updating the resized 
                // images!!
                isPublic = dataResource.getString("isPublic");
            }
            
            // SECURITY: absolutely must deny anything not marked as CMS media, otherwise this could be used to read sensitive internal documents!
            if (dataResource.getString("coContentTypeId") == null || !dataResource.getString("coContentTypeId").startsWith("SCP_MEDIA")) {
                response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Media not found");
                return;
            }
            
            // SECURITY: 2017-08-02: isPublic check; borrowed from DataEvents.serveObjectData
            String permissionService = EntityUtilProperties.getPropertyValue("content.properties", "stream.permission.service", "genericContentPermission", delegator);
            // see if data resource is public or not
            if (UtilValidate.isEmpty(isPublic)) {
                isPublic = "N";
            }

            // not public check security
            if (!"Y".equalsIgnoreCase(isPublic)) {
                // do security check
                Map<String, ? extends Object> permSvcCtx = UtilMisc.toMap("userLogin", userLogin, "locale", locale, "mainAction", "VIEW", "contentId", contentId);
                Map<String, Object> permSvcResp;
                try {
                    permSvcResp = dispatcher.runSync(permissionService, permSvcCtx);
                } catch (GenericServiceException e) {
                    Debug.logError(e, module);
                    //request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    //return "error";
                    response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Internal error"); // WARN: DO NOT send details, for security reasons
                    return;
                }
                if (ServiceUtil.isError(permSvcResp)) {
                    String errorMsg = ServiceUtil.getErrorMessage(permSvcResp);
                    Debug.logError(errorMsg, module);
                    //request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    //return "error";
                    response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Internal error"); // WARN: DO NOT send details, for security reasons
                    return;
                }

                // no service errors; now check the actual response
                Boolean hasPermission = (Boolean) permSvcResp.get("hasPermission");
                if (!hasPermission.booleanValue()) {
                    String errorMsg = (String) permSvcResp.get("failMessage");
                    Debug.logError(errorMsg, module);
                    //request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    //return "error";
                    response.sendError(HttpServletResponse.SC_FORBIDDEN, "Unauthorized"); // WARN: DO NOT send details, for security reasons
                    return;
                }
            }
            
            String fileName = (UtilValidate.isNotEmpty(dataResource.getString(fnSrcFieldName))) ? dataResource.getString(fnSrcFieldName)
                    : dataResource.getString(fnSrcFieldNameFallback);
            
            // see org.ofbiz.content.data.DataEvents#serveImage for reference code
            ServletContext application = request.getServletContext(); // SCIPIO: NOTE: no longer need getSession() for getServletContext(), since servlet API 3.0
            Map<String, Object> streamResult = DataResourceWorker.getDataResourceStream(dataResource, "", application.getInitParameter("webSiteId"), locale, application.getRealPath("/"), false);
            byte[] mediaData = (byte[]) streamResult.get("streamBytes");
            InputStream mediaStream = (InputStream) streamResult.get("stream");
            long mediaLength = (long) streamResult.get("length");
            
            response.setContentType(dataResource.getString("mimeTypeId"));
            response.setHeader("Content-Disposition", "inline; filename= " + fileName);
            response.setContentLengthLong(mediaLength);
            if (mediaData != null) {
                response.getOutputStream().write(mediaData, 0, (int) mediaLength);
            } else if (mediaStream != null) {
                UtilHttp.streamContent(response.getOutputStream(), mediaStream, (int) mediaLength);
            } else {
                Debug.logError("Cms: Bad stream/bytes source [effective contentId: " + dataResource.getString("coContentId") + "]", module);
                response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Internal error"); // WARN: DO NOT send details, for security reasons
                return;
            }
        } catch (Exception e) {
            Debug.logError(e, module);
            response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Internal error"); // WARN: DO NOT send details, for security reasons
            return;
        }
    }

    /**
     * @see javax.servlet.http.HttpServlet#destroy()
     */
    @Override
    public void destroy() {
        super.destroy();
    }

    /**
     * SPECIAL: needed to not break tenant dbs.
     */
    private Delegator getDelegator(HttpServletRequest request) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        if (delegator != null) return delegator;
        delegator = (Delegator) request.getSession().getAttribute("delegator");
        if (delegator != null) return delegator;
        delegator = (Delegator) request.getServletContext().getAttribute("delegator");
        if (delegator != null) return delegator;
        return DelegatorFactory.getDelegator("default");
    }
    
    /**
     * SPECIAL: needed to not break tenant dbs.
     */
    private LocalDispatcher getDispatcher(HttpServletRequest request) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        if (dispatcher != null) return dispatcher;
        dispatcher = (LocalDispatcher) request.getSession().getAttribute("dispatcher");
        if (dispatcher != null) return dispatcher;
        dispatcher = (LocalDispatcher) request.getServletContext().getAttribute("dispatcher");
        if (dispatcher != null) return dispatcher;
        Delegator delegator = getDelegator(request);
        return ServiceDispatcher.getLocalDispatcher(delegator.getDelegatorName(), delegator);
    }
}

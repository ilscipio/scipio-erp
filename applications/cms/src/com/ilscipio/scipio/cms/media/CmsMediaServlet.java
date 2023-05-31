
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
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.content.data.DataResourceWorker;
import com.ilscipio.scipio.content.image.ContentImageWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.WebAppUtil;

import com.ilscipio.scipio.cms.CmsUtil;

/**
 * Servlet used to serve media files, which basically consists in getting the
 * corresponding raw data from database and stream it in the response.
 * FIXME?: 2017-08-08: currently there is little to no use of useCache...
 */
@SuppressWarnings("serial")
public class CmsMediaServlet extends HttpServlet {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected static final String USE_CACHE_PARAM_DEFAULT = "cache";

    private static final String FN_SOURCE = UtilProperties.getPropertyValue("cms", "media.serve.filename.source", "name");
    private static final String fnSrcFieldName = "origfn".equals(FN_SOURCE) ? "objectInfo" : "dataResourceName";
    private static final String fnSrcFieldNameFallback = "origfn".equals(FN_SOURCE) ? "dataResourceName" : "objectInfo";
    private static final boolean variantsEnabled = UtilProperties.getPropertyAsBoolean("cms", "media.variants.enabled", true);

    private boolean useCacheDefault = true;
    private String useCacheParam = USE_CACHE_PARAM_DEFAULT;

    @Override
    public void init(ServletConfig config) throws ServletException {
        super.init(config);
        this.useCacheDefault = UtilMisc.booleanValue(config.getInitParameter("useCache"), true);
        String useCacheParam = config.getInitParameter("useCacheParam");
        if (useCacheParam != null && !"true".equals(useCacheParam)) {
            if (useCacheParam.isEmpty() || "false".equals(useCacheParam)) {
                this.useCacheParam = null;
            } else {
                this.useCacheParam = useCacheParam;
            }
        }
        if (Debug.infoOn()) {
            Debug.logInfo("Cms: Media servlet settings for servlet '" + config.getServletName() + "' of webapp '"
                    + config.getServletContext().getContextPath() + "': [" 
                    + "useCache=" + this.useCacheDefault + ", useCacheParam="
                    + (this.useCacheParam != null ? this.useCacheParam : "(disabled)")+ "]", module);
        }
    }

    @Override
    public void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        doGet(request, response);
    }

    /**
     * reference: {@link org.ofbiz.content.data.DataEvents#serveImage}
     * <p>
     * TODO: still missing an "auto" best-size selection based on width and height
     * TODO: this isn't looking at the global debug flag yet for the error msgs
     * WARN: autoVariant logic has severe limitations - see {@link CmsMediaWorker}.
     * TODO: REVIEW: Should this method be enclosed in a transaction? Mitigated for now using useCache.
     */
    @Override
    public void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        // SPECIAL: getDelegator/getDispatcher methods required so tenant db doesn't break (or breaks less)
        Delegator delegator = Delegator.from(request);
        LocalDispatcher dispatcher = LocalDispatcher.from(request);
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = WebAppUtil.getUserLogin(request);

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

        final boolean useCache = isUseCache(request);

        GenericValue content;
        GenericValue dataResource;
        try {
            String isPublic;

            // AUTO VARIANT MODE - more difficult
            if (autoVariantMode != null) {
                if (UtilValidate.isNotEmpty(contentId)) {


                    /**
                     * Tries to find the best image variant name to use for the width & height.
                     * WARN/FIXME: this has serious limitations in current form - we are forced to use
                     * ImageProperties.xml to get the dimensions rather than the actual resized dims of the
                     * images - there is no point fixing this here currently because can't solve this
                     * problem for the file-based storage elsewhere yet.
                     */
                    content = delegator.from("Content").where("contentId", contentId).cache().queryOne();
                    if (content == null) {
                        response.sendError(HttpServletResponse.SC_NOT_FOUND, "Media not found with contentId [" + contentId + "]");
                        return;
                    }
                } else if (request.getPathInfo() != null && request.getPathInfo().length() > 1) {
                    String contentPath = request.getPathInfo().substring(1); // no slash
                    content = delegator.from("Content").where("contentPath", contentPath).cache().queryFirst();
                    if (content == null) {
                        response.sendError(HttpServletResponse.SC_NOT_FOUND,
                                "Media not found with contentPath [" + contentPath + "]");
                        return;
                    }
                    contentId = content.getString("contentId");
                } else {
                    response.sendError(HttpServletResponse.SC_NOT_FOUND, "Media not found");
                    return;
                }
                ImageProfile imageProfile = ContentImageWorker.getContentImageProfileOrDefault(delegator, content, true, true);
                if (imageProfile == null) {
                    response.sendError(HttpServletResponse.SC_NOT_FOUND,"Invalid media with contentId [" + contentId + "]");
                    return;
                }
                ImageVariantConfig imgVariantCfg = imageProfile.getVariantConfig(); // Now cached by ImageProfile
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
                    if (CmsUtil.verboseOn()) {
                        Debug.logWarning("Cms: Cannot auto-select image variant - no image variant config (ImageProperties.xml) available for CMS", module);
                    }
                }
            }

            if ((UtilValidate.isEmpty(variant) || "original".equals(variant))) {
                // STANDARD CASE
                if (UtilValidate.isNotEmpty(dataResourceId)) {
                    dataResource = delegator.from("DataResourceContentRequiredView").where("dataResourceId", dataResourceId).cache(useCache).queryFirst();
                    if (dataResource == null) {
                        response.sendError(HttpServletResponse.SC_NOT_FOUND,
                                "Media not found with dataResourceId [" + dataResourceId + "]");
                        return;
                    }
                    contentId = dataResource.getString("coContentId");
                } else if (UtilValidate.isNotEmpty(contentId)) {
                    dataResource = delegator.from("DataResourceContentRequiredView").where("coContentId", contentId).cache(useCache).queryFirst();
                    if (dataResource == null) {
                        response.sendError(HttpServletResponse.SC_NOT_FOUND,
                                "Media not found with contentId [" + contentId + "]");
                        return;
                    }
                } else if (request.getPathInfo() != null && request.getPathInfo().length() > 1) {
                    String contentPath = request.getPathInfo().substring(1); // no slash
                    dataResource = delegator.from("DataResourceContentRequiredView").where("coContentPath", contentPath).cache(useCache).queryFirst();
                    if (dataResource == null) {
                        response.sendError(HttpServletResponse.SC_NOT_FOUND,
                                "Media not found with contentPath [" + contentPath + "]");
                        return;
                    }
                    contentId = dataResource.getString("coContentId");
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
                    origDataResource = EntityUtil.getFirst(EntityQuery.use(delegator).from("DataResourceContentRequiredView").where("dataResourceId", dataResourceId).cache(useCache).queryList());
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
                } else if (request.getPathInfo() != null && request.getPathInfo().length() > 1) {
                    String contentPath = request.getPathInfo().substring(1); // no slash
                    dataResource = delegator.from("DataResourceContentRequiredView").where("coContentPath", contentPath).cache(useCache).queryFirst();
                    if (dataResource == null) {
                        response.sendError(HttpServletResponse.SC_NOT_FOUND,
                                "Media not found with contentPath [" + contentPath + "]");
                        return;
                    }
                    contentId = dataResource.getString("coContentId");
                } else {
                    response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Missing or invalid dataResourceId or contentId parameter - cannot determine media");
                    return;
                }

                // this implies we're getting IMAGE_OBJECT type
                GenericValue contentAssoc = EntityUtil.getFirst(EntityQuery.use(delegator).from("ContentAssoc").where("contentId", contentId,
                        "contentAssocTypeId", "IMGSZ_" + variant.toUpperCase()).cache(useCache).queryList());

                if (contentAssoc != null) {
                    dataResource = EntityUtil.getFirst(EntityQuery.use(delegator).from("DataResourceContentRequiredView")
                            .where("coContentId", contentAssoc.getString("contentIdTo")).cache(useCache).queryList());
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
                        dataResource = EntityUtil.getFirst(EntityQuery.use(delegator).from("DataResourceContentRequiredView").where("coContentId", contentId).cache(useCache).queryList());
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
            String permissionService = EntityUtilProperties.getPropertyValue("content", "stream.permission.service", "genericContentPermission", delegator);
            // see if data resource is public or not
            if (UtilValidate.isEmpty(isPublic)) {
                isPublic = "N";
            }

            // not public check security
            if (!"Y".equalsIgnoreCase(isPublic)) {
                // 2021-01-13: if no userlogin, automatically deny otherwise below fails
                if (userLogin == null) {
                    response.sendError(HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "Internal error"); // WARN: DO NOT send details, for security reasons
                    return;
                }

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
                if (hasPermission == null || !hasPermission.booleanValue()) {
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
            Map<String, Object> streamResult = DataResourceWorker.getDataResourceStream(dataResource, "", application.getInitParameter("webSiteId"), locale, application.getRealPath("/"), useCache);
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

    protected boolean isUseCache(HttpServletRequest request) {
        if (useCacheParam != null) {
            String value = request.getParameter(useCacheParam);
            if (value != null) {
                if ("Y".equals(value)) {
                    return true;
                } else if ("N".equals(value)) {
                    return false;
                } else {
                    ; // ignore.
                }
            }
        }
        return useCacheDefault;
    }
}

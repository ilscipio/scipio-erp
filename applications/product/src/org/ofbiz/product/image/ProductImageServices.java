package org.ofbiz.product.image;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.ProcessSignals;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.content.image.ContentImageServices;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.ServiceValidationException;

import java.io.File;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * SCIPIO: New product image services, alternatives to {@link org.ofbiz.product.imagemanagement.ImageManagementServices}
 * and other utils.
 * TODO?: try to reconcile everything in the future, too difficult for now.
 * Added 2017-07-05.
 */
public abstract class ProductImageServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String resource = "ProductErrorUiLabels";
    private static final String resourceProduct = resource;
    private static final Locale LOG_LANG = Debug.getLogLocale();
    private static final ProcessSignals productImageAutoRescaleAllSignals = ProcessSignals.make("productImageAutoRescaleAll", true);

    protected ProductImageServices() {
    }

    /**
     * SCIPIO: Updated scaling implementation based on ScaleImage.scaleImageInAllSize, but allowing greater parameterization.
     * Fully implements the productImageFileScaleInAllSize service, see its interface for parameters.
     * Added 2017-07-05.
     * <p>
     * TODO?: reconcile with ScaleImage.scaleImageInAllSize eventually... for now, leaving separate to avoid
     * breaking product screens.
     */
    public static Map<String, Object> productImageFileScaleInAllSize(ServiceContext ctx) {
        Map<String, Object> context = ctx.context();
        Delegator delegator = ctx.delegator();
        String viewType = ctx.attr("viewType");
        String viewNumber = ctx.getString("viewNumber");
        Locale locale = ctx.attr("locale", Locale::getDefault);

        Map<String, Object> contentCtx;
        try {
            contentCtx = ctx.makeValidInContext("contentImageFileScaleInAllSizeCore", context);
        } catch (GenericServiceException e) {
            return ServiceUtil.returnError(e.getMessage());
        }

        if (isStrArgEmpty(contentCtx, "imageServerPath")) {
            String imageServerPath = EntityUtilProperties.getPropertyValue("catalog", "image.server.path", delegator);
            contentCtx.put("imageServerPath", imageServerPath);
        }
        if (isStrArgEmpty(contentCtx, "imageUrlPrefix")) {
            String imageUrlPrefix = EntityUtilProperties.getPropertyValue("catalog", "image.url.prefix", delegator);
            contentCtx.put("imageUrlPrefix", imageUrlPrefix);
        }

        Map<String, Object> imagePathArgs = new HashMap<>();

        String type = null;
        String id = (String) context.get("productId");
        if (viewType.toLowerCase().contains("main")) {
            if (isStrArgEmpty(contentCtx, "imageFnFmt")) {
                contentCtx.put("imageFnFmt", EntityUtilProperties.getPropertyValue("catalog", "image.filename.format", delegator));
            }
            imagePathArgs.putAll(UtilMisc.toMap("location", "products", "id", id, "type", "original"));
        } else if (viewType.toLowerCase().contains("additional") && viewNumber != null && !"0".equals(viewNumber)) {
            if (isStrArgEmpty(contentCtx, "imageFnFmt")) {
                contentCtx.put("imageFnFmt", EntityUtilProperties.getPropertyValue("catalog", "image.filename.additionalviewsize.format", delegator));
            }
            String filenameFormat = (String) contentCtx.get("imageFnFmt");
            if (filenameFormat.endsWith("${id}")) { // TODO: REVIEW: I don't get this
                id = id + "_View_" + viewNumber;
            } else {
                viewType = "additional" + viewNumber;
            }
            imagePathArgs.putAll(UtilMisc.toMap("location", "products", "id", id, "viewtype", viewType, "sizetype", "original"));
        } else {
            return ServiceUtil.returnError(UtilProperties.getMessage(resource, "ProductImageViewType", UtilMisc.toMap("viewType", viewType), locale));
        }

        Map<String, ?> imagePathArgsRcvd = UtilGenerics.checkMap(contentCtx.get("imagePathArgs"));
        if (imagePathArgsRcvd != null) {
            imagePathArgs.putAll(imagePathArgsRcvd); // explicit args crush ours
        }
        contentCtx.put("imagePathArgs", imagePathArgs);
        if (isStrArgEmpty(contentCtx, "imagePropXmlPath")) {
            try {
                contentCtx.put("imagePropXmlPath", ProductImageWorker.getProductImagePropertiesPath());
            } catch (Exception e) {
                Debug.logError("Product image configuration error: " + e.getMessage(), module);
                return ServiceUtil.returnError("Product image configuration error: " + e.getMessage());
            }
        }

        // TODO/FIXME: currently provides no deletion of the old images...

        Map<String, Object> result = ContentImageServices.contentImageFileScaleInAllSizeCore(ctx.dctx(), contentCtx);
        result.put("productSizeTypeList", ScaleImage.sizeTypeList);
        return result;
    }

    public static Map<String, Object> productImageAutoRescale(ServiceContext ctx) throws ServiceValidationException { // SCIPIO
        // NOTE: CMS images are identified by contentTypeId="SCP_MEDIA"
        GenericValue product = ctx.attr("product");
        String productId;
        if (product != null) {
            productId = product.getString("productId");
        } else {
            productId = ctx.attr("productId");
            product = ctx.delegator().from("Product").where("productId", productId).queryOneSafe();
            if (product == null) {
                return ServiceUtil.returnError("Could not find product [" + productId + "]");
            }
        }

        boolean allImages = Boolean.TRUE.equals(ctx.attr("allImages"));

        List<String> productContentTypeIdList = ctx.attr("productContentTypeIdList", Collections.emptyList());
        List<String> contentIdList = ctx.attr("contentIdList", Collections.emptyList());
        String productContentTypeIdParam = ctx.attr("productContentTypeId");
        if (productContentTypeIdParam != null) {
            productContentTypeIdList = new ArrayList<>(productContentTypeIdList);
            productContentTypeIdList.add(productContentTypeIdParam);
        }
        String contentIdParam = ctx.attr("contentId");
        if (contentIdParam != null) {
            contentIdList = new ArrayList<>(contentIdList);
            contentIdList.add(contentIdParam);
        }

        if (allImages) {
            try {
                productContentTypeIdList = new ArrayList<>(ProductImageWorker.getOriginalImageProductContentTypes(ctx.delegator()).keySet());
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.toString());
            }
        }

        int successCount = 0;
        int skipCount = 0;
        int errorCount = 0;
        int failCount = 0;
        for(String productContentTypeId : productContentTypeIdList) {
            Map<String, Object> res = productImageRescaleImage(ctx, product, productContentTypeId, null);
            if ("no-image-url".equals(res.get("reason")) && "ORIGINAL_IMAGE_URL".equals(productContentTypeId)) {
                // SPECIAL: stock Scipio and other products don't always have ORIGINAL_IMAGE_URL but sometimes only DETAIL_IMAGE_URL
                res = productImageRescaleImage(ctx, product, "DETAIL_IMAGE_URL", null);
            }
            if (ServiceUtil.isError(res)) {
                errorCount++;
            } else if (ServiceUtil.isFailure(res)) {
                failCount++;
            } else {
                if ("no-image-url".equals(res.get("reason"))) {
                    Debug.logInfo("productImageRescaleImage: " + ServiceUtil.getSuccessMessage(res), module);
                    skipCount++;
                } else if ("inapplicable".equals(res.get("reason"))) {
                    if (Debug.verboseOn()) {
                        Debug.logInfo("productImageRescaleImage: " + ServiceUtil.getSuccessMessage(res), module);
                    }
                    skipCount++;
                } else {
                    successCount++;
                }
            }
        }
        for(String contentId : contentIdList) {
            Map<String, Object> res = productImageRescaleImage(ctx, product, null, contentId);
            if (ServiceUtil.isError(res)) {
                errorCount++;
            } else if (ServiceUtil.isFailure(res)) {
                failCount++;
            } else {
                successCount++;
            }
        }
        if (errorCount <= 0) {
            return UtilMisc.put(ServiceUtil.returnSuccess(), "errorCount", errorCount, "failCount", failCount, "successCount", successCount, "skipCount", skipCount);
        } else {
            return UtilMisc.put(ServiceUtil.returnError(errorCount + " errors auto-rescaling images for product [" + productId + "]"),
                    "errorCount", errorCount, "failCount", failCount, "successCount", successCount, "skipCount", skipCount);
        }
    }

    private static Map<String, Object> productImageRescaleImage(ServiceContext ctx, GenericValue product, String productContentTypeId, String contentId) throws ServiceValidationException {
        String productId = product.getString("productId");
        GenericValue content = null;
        if (UtilValidate.isNotEmpty(contentId)) {
            try {
                content = ctx.delegator().from("Content").where("contentId", contentId).queryOne();
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (content == null) {
                Debug.logError("productImageRescaleImage: Content not found for contentId [" + contentId + "]", module);
                return ServiceUtil.returnError("Content not found for contentId [" + contentId + "]");
            }
        }

        if (UtilValidate.isEmpty(contentId)) {
            contentId = null;
        }
        if (UtilValidate.isEmpty(productContentTypeId)) {
            productContentTypeId = null;
        }
        if (contentId == null && productContentTypeId == null) {
            String errMsg = "Missing contentId or productContentTypeId for product [" + productId + "]";
            Debug.logError("productImageRescaleImage: " + errMsg, module);
            return ServiceUtil.returnError(errMsg);
        }
        GenericValue productContent = null;
        String productFieldName = null;
        if (contentId != null) {
            try {
                if (productContentTypeId == null) {
                    productContent = ctx.delegator().from("ProductContent").where("productId", productId,
                            "contentId", contentId).orderBy("-fromDate").filterByDate().queryFirst();
                    if (productContent == null) {
                        return ServiceUtil.returnError("Could not find ProductContent with product [" + productId + "] contentId [" + contentId + "]");
                    }
                    productContentTypeId = productContent.getString("productContentTypeId");
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.toString());
            }
        } else {
            try {
                productContent = ctx.delegator().from("ProductContent").where("productId", productId,
                        "productContentTypeId", productContentTypeId).orderBy("-fromDate").filterByDate().queryFirst();
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.toString());
            }
            if (productContent != null) {
                contentId = productContent.getString("contentId");
                try {
                    content = productContent.getRelatedOne("Content");
                } catch (GenericEntityException e) {
                    Debug.logError(e, module);
                    return ServiceUtil.returnError(e.toString());
                }
            } else {
                productFieldName = ModelUtil.dbNameToVarName(productContentTypeId);
                ModelEntity productModel = ctx.delegator().getModelEntity("Product");
                if (!productModel.isField(productFieldName)) {
                    String msg = "Inapplicable productContentTypeId [" + productContentTypeId
                            + "] for product [" + productId + "] for resize operation (parent products not consulted)";
                    return UtilMisc.put(ServiceUtil.returnSuccess(msg), "reason", "inapplicable");
                }
            }
        }

        // DON'T consult parent product because it complicates update logic
        //// NOTE: this consults the parent product which we don't want, but in known cases should return right value
        //String origImageUrl = ProductContentWrapper.getProductContentAsText(product, productContentTypeId,
        //        ctx.attr("locale"), ctx.dispatcher(), false, "raw");
        String origImageUrl = null;
        GenericValue origElecText = null;
        if (productContent != null) {
            try {
                GenericValue dataResource = (content != null) ? content.getRelatedOne("DataResource") : null;
                if (dataResource != null && "SHORT_TEXT".equals(dataResource.get("dataResourceTypeId"))) {
                    origImageUrl = dataResource.getString("objectInfo");
                } else if (dataResource != null && "ELECTRONIC_TEXT".equals(dataResource.get("dataResourceTypeId"))) {
                    origElecText = dataResource.getRelatedOne("ElectronicText");
                    if (origElecText == null) {
                        String msg = "Could not find ElectronicText for [" + productId + "] productContentTypeId [" + productContentTypeId + "], unexpected ProductContent format";
                        Debug.logWarning("productImageRescaleImage: " + msg, module);
                        return ServiceUtil.returnFailure(msg);
                    }
                    origImageUrl = origElecText.getString("textData");
                } else {
                    String msg = "Unexpected DataResource dataResourceTypeId for [" + productId + "] productContentTypeId [" + productContentTypeId + "], unexpected ProductContent format";
                    Debug.logWarning("productImageRescaleImage: " + msg, module);
                    return ServiceUtil.returnFailure(msg);
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.toString());
            }
        } else if (productFieldName != null) {
            origImageUrl = product.getString(productFieldName);
        }
        if (UtilValidate.isEmpty(origImageUrl)) {
            String msg = "No direct image URL for product [" + productId + "] productContentTypeId [" + productContentTypeId + "], not resizing";
            //Debug.logInfo("productImageRescaleImage: " + msg, module);
            return UtilMisc.put(ServiceUtil.returnSuccess(msg), "reason", "no-image-url");
        }

        /*
        Boolean deleteOld = ctx.getAttr("deleteOld");
        if (deleteOld == null) {
            deleteOld = UtilProperties.getPropertyAsBoolean("content", "image.auto.rescale.deleteOld", false);
        }
        boolean force = Boolean.TRUE.equals(ctx.getAttr("force"));
        boolean createInitial = Boolean.TRUE.equals(ctx.getAttr("createInitial"));
         */

        String mediaProfileName;
        if (content != null) {
            mediaProfileName = content.getString("mediaProfile");
        } else {
            mediaProfileName = product.getString("imageProfile");
        }
        if (mediaProfileName == null) {
            mediaProfileName = "IMAGE_PRODUCT"; // TODO?: better fallback default? don't use Product.imageProfile because it denotes the main product image (isMainImage)
        }

        ImageProfile imageProfile = ImageProfile.getImageProfile(ctx.delegator(), mediaProfileName);
        if (imageProfile == null) {
            Debug.logError("productImageRescaleImage: Could not find media profile [" + imageProfile + "]" + " for product [" + productId + "]", module);
            return ServiceUtil.returnError("Could not find media profile [" + imageProfile + "]" + " for product [" + productId + "]");
        }

        ProductImageWorker.ImageViewType imageViewType;
        try {
            imageViewType = ProductImageWorker.ImageViewType.from(productContentTypeId);
        } catch(Exception e) {
            Debug.logError(e,"productImageRescaleImage: Could not determine image view type from product [" + productId + "]: " + e.toString(), module);
            return ServiceUtil.returnError("Could not determine image view type from product [" + productId + "]: " + e.toString());
        }
        String viewType = imageViewType.getViewType();
        String viewNumber = imageViewType.getViewNumber();

        Collection<String> sizeTypeList = ctx.attr("sizeTypeList");
        boolean recreateExisting = ctx.attr("recreateExisting", false);
        if (!recreateExisting) {
            Map<String, String> sizeTypeMap = ProductImageServices.getProductImageMissingVariantSizeTypes(ctx.dctx(), ctx.locale(),
                    product, productContentTypeId, null, origImageUrl, sizeTypeList, false);
            if (UtilValidate.isEmpty(sizeTypeMap)) {
                String msg = "No missing sizeTypes for product [" + productId + "]" + (sizeTypeList != null ? " sizeTypeList [" + sizeTypeList + "]" : "");
                Debug.logInfo("productImageRescaleImage: " + msg, module);
                return ServiceUtil.returnSuccess(msg);
            }
            sizeTypeList = new ArrayList<>(sizeTypeMap.keySet());
        }

        Map<String, Object> resizeCtx = UtilMisc.toMap("productId", productId, "imageOrigUrl", origImageUrl, "viewType", viewType, "viewNumber", viewNumber,
                "locale", ctx.get("locale"), "userLogin", ctx.get("userLogin"), "timeZone", ctx.get("timeZone"), "imageProfile", imageProfile,
                "sizeTypeList", sizeTypeList);
        try {
            Map<String, Object> resizeResult = productImageFileScaleInAllSize(ctx.from(resizeCtx));
            if (!ServiceUtil.isSuccess(resizeResult)) {
                String errMsg = "Could not resize image for product [" + productId + "] contentId [" + contentId
                        + "] productContentTypeId [" + productContentTypeId + "]";
                Debug.logError("productImageRescaleImage: " + errMsg, module);
                return ServiceUtil.returnError(errMsg);
            }

            Map<String, String> imageUrlMap = UtilGenerics.cast(resizeResult.get("imageUrlMap"));
            if (UtilValidate.isNotEmpty(imageUrlMap)) {
                int imageNum = ProductImageWorker.getImageProductContentTypeNum(productContentTypeId);
                Timestamp fromDate = UtilDateTime.nowTimestamp();
                for(Map.Entry<String, String> entry : imageUrlMap.entrySet()) {
                    String sizeType = entry.getKey();
                    String url = entry.getValue();
                    GenericValue pct = ProductImageWorker.getImageSizeTypeProductContentType(ctx.delegator(), imageNum, sizeType);
                    if (pct == null) {
                        if (Debug.verboseOn()) {
                            Debug.logVerbose("productImageRescaleImage: ProductContentType not found for image sizeType [" + sizeType + "]"
                                    + " for product [" + productId + "]", module);
                        }
                        continue;
                    }
                    Map<String, Object> res = updateProductContentImageUrl(ctx, product, pct.getString("productContentTypeId"), url, productContentTypeId, fromDate,
                            ctx.attr("createSizeTypeContent"));
                    if (ServiceUtil.isError(res)) {
                        return res;
                    }
                }
            }
        } catch (Exception e) {
            Debug.logError(e, "productImageRescaleImage: Error resizing images for product [" + productId + "]: " + e.toString(), module);
            return ServiceUtil.returnError(e.toString());
        }
        return ServiceUtil.returnSuccessReadOnly();
    }

    /** Attempts to preserve previous data setup, best-effort. */
    public static Map<String, Object> updateProductContentImageUrl(ServiceContext ctx, GenericValue product, String productContentTypeId, String imageUrl,
                                                                   String origProductContentTypeId, Timestamp fromDate, Boolean createSizeTypeContent) {
        GenericValue productContent;
        try {
            productContent = ctx.delegator().from("ProductContent").where("productId", product.get("productId"),
                    "productContentTypeId", productContentTypeId).orderBy("-fromDate").filterByDate().queryFirst();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }
        try {
            if (productContent != null) {
                GenericValue content = productContent.getRelatedOne("Content");
                if (content == null) {
                    String errMsg = "Could not find Content record for contentId [" + productContent.get("contentId") + "]";
                    Debug.logError("updateProductContentImageUrl: " + errMsg, module);
                    return ServiceUtil.returnError(errMsg);
                }
                GenericValue dataResource = content.getRelatedOne("DataResource");
                if (dataResource == null) {
                    String errMsg = "Could not find image DataResource record for product [" + product.get("productId") + "] productContentTypeId [" +
                            productContentTypeId + "]";
                    Debug.logError("updateProductContentImageUrl: " + errMsg, module);
                    return ServiceUtil.returnError(errMsg);
                }
                if ("SHORT_TEXT".equals(dataResource.get("dataResourceTypeId"))) {
                    String prevImageUrl = dataResource.getString("objectInfo");
                    if (!imageUrl.equals(prevImageUrl)) {
                        dataResource.set("objectInfo", imageUrl);
                        dataResource.store();
                        Debug.logInfo("updateProductContentImageUrl: Updated DataResource imageUrl from [" + prevImageUrl
                                + "] to [" + imageUrl + "] for product [" + product.get("productId") + "] productContentTypeId [" + productContentTypeId + "]", module);
                    }
                } else if ("ELECTRONIC_TEXT".equals(dataResource.get("dataResourceTypeId"))) {
                    GenericValue elecText = ctx.delegator().from("ElectronicText").where("dataResourceId", content.get("dataResourceId")).queryOne();
                    if (elecText == null) {
                        String msg = "Could not find image ElectronicText for product [" + product.get("productId") + "] productContentTypeId [" +
                                productContentTypeId + "], unexpected ProductContent format";
                        Debug.logWarning("updateProductContentImageUrl: " + msg, module);
                        return ServiceUtil.returnFailure(msg);
                    }
                    String prevImageUrl = elecText.getString("textData");
                    if (!imageUrl.equals(prevImageUrl)) {
                        elecText.set("textData", imageUrl);
                        elecText.store();
                        Debug.logInfo("updateProductContentImageUrl: Updated ElectronicText imageUrl from [" + prevImageUrl
                                + "] to [" + imageUrl + "] for product [" + product.get("productId") + "] productContentTypeId [" + productContentTypeId + "]", module);
                    }
                } else {
                    String errMsg = "Invalid image DataResource record for product [" + product.get("productId") + "] productContentTypeId [" + productContentTypeId + "]";
                    Debug.logError("updateProductContentImageUrl: " + errMsg, module);
                    return ServiceUtil.returnError(errMsg);
                }
            } else {
                // Check field
                String productFieldName = ModelUtil.dbNameToVarName(productContentTypeId);
                ModelEntity productModel = ctx.delegator().getModelEntity("Product");
                if (productModel.isField(productFieldName)) {
                    String prevImageUrl = product.getString(productFieldName);
                    if (!imageUrl.equals(prevImageUrl)) {
                        product.set(productFieldName, imageUrl);
                        product.store();
                        Debug.logInfo("updateProductContentImageUrl: Updated Product field [" + productFieldName + "] imageUrl from [" + prevImageUrl
                                + "] to [" + imageUrl + "] for product [" + product.get("productId") + "] productContentTypeId [" + productContentTypeId + "]", module);
                    }
                } else if (Boolean.TRUE.equals(createSizeTypeContent)) {
                    // Try to find a ProductContent record to refer to
                    GenericValue origProductContent = ctx.delegator().from("ProductContentAndDataResource").where(
                                "productId", product.get("productId"), "productContentTypeId", origProductContentTypeId).orderBy("-fromDate").queryFirst();
                    if (origProductContent == null) {
                        if (!"ORIGINAL_IMAGE_URL".equals(origProductContentTypeId)) {
                            origProductContent = ctx.delegator().from("ProductContentAndDataResource").where(
                                    "productId", product.get("productId"), "productContentTypeId", "ORIGINAL_IMAGE_URL").orderBy("-fromDate").queryFirst();
                        }
                    }
                    // DEV NOTE: I don't see a good reason not to create these, makes the service usable in more cases as long as the defaults are sane
                    //if (origProductContent != null) {
                    Debug.logInfo("updateProductContentImageUrl: No existing image record for product [" + product.get("productId")
                            + "] productContentTypeId [" + productContentTypeId + "]; creating new DataResource/Content/ProductContent" +
                            " using reference productContentTypeId [" + (origProductContent != null ? origProductContent.get("productContentTypeId") : "(none)") + "]", module);
                    String dataResourceName = (origProductContent != null) ? origProductContent.getString("drDataResourceName") : null;
                    if (dataResourceName == null) {
                        int lastSlash = imageUrl.lastIndexOf('/');
                        dataResourceName = (lastSlash >= 0) ? imageUrl.substring(lastSlash) : imageUrl;
                    }
                    String statusId = (origProductContent != null) ? origProductContent.getString("drStatusId") : "CTNT_IN_PROGRESS";
                    GenericValue dataResource = ctx.delegator().makeValue("DataResource",
                            "dataResourceTypeId", "SHORT_TEXT", "dataTemplateTypeId", "NONE", "statusId", statusId,
                            "mimeTypeId", "text/html", "dataResourceName", dataResourceName, "objectInfo", imageUrl);
                    dataResource = dataResource.createSetNextSeqId();
                    statusId = (origProductContent != null) ? origProductContent.getString("statusId") : "CTNT_IN_PROGRESS";
                    GenericValue content = ctx.delegator().makeValue("Content", "dataResourceId", dataResource.get("dataResourceId"),
                            "statusId", statusId, "contentTypeId", "DOCUMENT");
                    content = content.createSetNextSeqId();
                    productContent = ctx.delegator().makeValue("ProductContent", "productId", product.get("productId"),
                            "contentId", content.get("contentId"), "productContentTypeId", productContentTypeId, "fromDate", fromDate).create();
                    //} else {
                    //    Debug.logWarning("updateProductContentImageUrl: No existing image record for product [" + product.get("productId")
                    //            + "] productContentTypeId [" + productContentTypeId + "]; not creating new DataResource/Content/ProductContent" +
                    //            " because", module);
                    //}
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }
        return ServiceUtil.returnSuccessReadOnly();
    }

    private static boolean isStrArgEmpty(Map<String, ?> context, String argName) {
        return UtilValidate.isEmpty((String) context.get(argName));
    }

    /** Returns map of missing variants, mapping size name to missing expected image URL. */
    public static Map<String, String> getProductImageMissingVariantSizeTypes(DispatchContext dctx, Locale locale, GenericValue product, String productContentTypeId,
                                                                             GenericValue productContent, String imageUrl, Collection<String> sizeTypeList, boolean useCache) {
        // FIXME: duplication (too many quirks)
        String logPrefix = "getProductImageMissingVariantSizeTypes: ";
        Delegator delegator = dctx.getDelegator();
        if (locale == null) {
            locale = Locale.getDefault();
        }

        /* productImageAutoRescale */

        String productId = product.getString("productId");
        GenericValue content = null;
        if (productContent == null) {
            try {
                productContent = dctx.getDelegator().from("ProductContent").where("productId", productId,
                        "productContentTypeId", productContentTypeId).orderBy("-fromDate").filterByDate().cache(useCache).queryFirst();
            } catch (GenericEntityException e) {
                Debug.logError(e, logPrefix+"Error looking up ProductContent for product [" + productId + "] productContentTypeId [" + productContentTypeId + "]", module);
                return null;
            }
        }
        if (productContent != null) {
            try {
                content = productContent.getRelatedOne("Content", useCache);
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return null;
            }
        } else {
            String productFieldName = ModelUtil.dbNameToVarName(productContentTypeId);
            ModelEntity productModel = dctx.getDelegator().getModelEntity("Product");
            if (!productModel.isField(productFieldName)) {
                Debug.logError(logPrefix+"Invalid productContentTypeId [" + productContentTypeId
                        + "] for product [" + productId + "] for resize operation (parent products not consulted)", module);
                return null;
            }
        }

        if (imageUrl == null) {
            // NOTE: this consults the parent product which we don't want, but in known cases should return right value
            imageUrl = ProductContentWrapper.getProductContentAsText(product, productContentTypeId,
                    locale, dctx.getDispatcher(), useCache, "raw");
            if (UtilValidate.isEmpty(imageUrl)) {
                Debug.logError(logPrefix+"Could not get existing image URL for product [" + productId + "] productContentTypeId [" + productContentTypeId + "], resize impossible", module);
                return null;
            }
        }

        String mediaProfileName;
        if (content != null) {
            mediaProfileName = content.getString("mediaProfile");
        } else {
            mediaProfileName = product.getString("imageProfile");
        }
        if (mediaProfileName == null) {
            mediaProfileName = "IMAGE_PRODUCT"; // TODO?: better fallback default? don't use Product.imageProfile because it denotes the main product image (isMainImage)
        }

        ImageProfile imageProfile = ImageProfile.getImageProfile(dctx.getDelegator(), mediaProfileName);
        if (imageProfile == null) {
            Debug.logError(logPrefix+"Could not find media profile [" + imageProfile + "]", module);
            return null;
        }

        ProductImageWorker.ImageViewType imageViewType;
        try {
            imageViewType = ProductImageWorker.ImageViewType.from(productContentTypeId);
        } catch(Exception e) {
            Debug.logError(logPrefix+"Could not determine image view type from product [" + productId + "]: " + e.toString(), module);
            return null;
        }
        String viewType = imageViewType.getViewType();
        String viewNumber = imageViewType.getViewNumber();

        /* productImageFileScaleInAllSize */

        String imageServerPath = EntityUtilProperties.getPropertyValue("catalog", "image.server.path", delegator);
        String imageUrlPrefix = EntityUtilProperties.getPropertyValue("catalog", "image.url.prefix", delegator);
        Map<String, Object> imagePathArgs = new HashMap<>();
        String imageOrigFn = imageUrl;
        if (imageOrigFn.lastIndexOf("/") != -1) {
            imageOrigFn = imageOrigFn.substring(imageOrigFn.lastIndexOf("/") + 1);
        }
        String imageFnFmt;
        String id = productId;
        if (viewType.toLowerCase().contains("main")) {
            imageFnFmt = EntityUtilProperties.getPropertyValue("catalog", "image.filename.format", delegator);
            imagePathArgs.putAll(UtilMisc.toMap("location", "products", "id", id, "type", "original"));
        } else if (viewType.toLowerCase().contains("additional") && viewNumber != null && !"0".equals(viewNumber)) {
            imageFnFmt = EntityUtilProperties.getPropertyValue("catalog", "image.filename.additionalviewsize.format", delegator);
            if (imageFnFmt.endsWith("${id}")) { // TODO: REVIEW: I don't get this
                id = id + "_View_" + viewNumber;
            } else {
                viewType = "additional" + viewNumber;
            }
            imagePathArgs.putAll(UtilMisc.toMap("location", "products", "id", id, "viewtype", viewType, "sizetype", "original"));
        } else {
            Debug.logError(UtilProperties.getMessage(resource, "ProductImageViewType", UtilMisc.toMap("viewType", viewType), LOG_LANG), module);
            return null;
        }

        /* contentImageFileScaleInAllSizeCore (heavily modified) */
        try {
            imageServerPath = FlexibleLocation.resolveFileUrlAsPathIfUrl(imageServerPath, imageServerPath);

            /* ImageProperties.xml */
            ImageVariantConfig imgPropCfg = imageProfile.getVariantConfig();
            if (sizeTypeList == null) {
                sizeTypeList = imgPropCfg.getVariantNames();
            }

            // get Name and Extension
            if (imageOrigFn.lastIndexOf(".") <= 0 || imageOrigFn.lastIndexOf(".") >= (imageOrigFn.length() - 1)) { // SCIPIO: added this to prevent problems
                throw new IllegalArgumentException("Original image filename [" + imageOrigFn + "] has missing or improper file extension (image type)");
            }
            String imgExtension = imageOrigFn.substring(imageOrigFn.lastIndexOf(".") + 1);

            // paths
            Map<String, Object> imageContext = new HashMap<>(); // new HashMap<>(context);
            imageContext.put("tenantId", delegator.getDelegatorTenantId());
            imageContext.putAll(imagePathArgs);
            imageServerPath = FlexibleStringExpander.expandString(UtilValidate.isNotEmpty(imageServerPath) ? imageServerPath : EntityUtilProperties.getPropertyValue("content", "image.server.path", delegator), imageContext);
            imageUrlPrefix = FlexibleStringExpander.expandString(UtilValidate.isNotEmpty(imageUrlPrefix) ? imageUrlPrefix : EntityUtilProperties.getPropertyValue("content", "image.url.prefix", delegator), imageContext);
            imageServerPath = imageServerPath.endsWith("/") ? imageServerPath.substring(0, imageServerPath.length()-1) : imageServerPath;
            imageUrlPrefix = imageUrlPrefix.endsWith("/") ? imageUrlPrefix.substring(0, imageUrlPrefix.length()-1) : imageUrlPrefix;
            FlexibleStringExpander imageFnFmtExpander = FlexibleStringExpander.getInstance(imageFnFmt);

            // Check for missing variants
            Map<String, String> imgUrlMap = new HashMap<>();
            for (String sizeType : sizeTypeList) {
                if (!imgPropCfg.hasVariant(sizeType)) {
                    Debug.logError(logPrefix+"sizeType " + sizeType + " is not part of ImageProperties.xml; ignoring", module);
                    continue;
                }

                ImageVariantConfig.VariantInfo variantInfo = imgPropCfg.getVariant(sizeType);
                String newFileLocation = ContentImageServices.expandImageFnFmt(imageFnFmtExpander, sizeType, imagePathArgs);
                //String targetDirectory = imageServerPath + "/" + ContentImageServices.getExpandedFnFmtDirPrefix(newFileLocation);

                // write new image
                String targetFileType = imgExtension;
                // SCIPIO: 2020-09: Support for specific storage format
                if (variantInfo != null && variantInfo.getFormat() != null) {
                    targetFileType = variantInfo.resolveFormatExt(delegator);
                }
                String newFileLocExt = newFileLocation + "." + targetFileType;
                String newFileFullLoc = imageServerPath + "/" + newFileLocExt;
                try {
                    if (!new File(newFileFullLoc).exists()) {
                        imgUrlMap.put(sizeType, imageUrlPrefix + "/" + newFileLocExt);
                    }
                } catch(Exception e) {
                    Debug.logError(e, logPrefix+"Error checking original file presence [" + newFileFullLoc + "]: " + e.getMessage(), module);
                }
            }
            return imgUrlMap;
        } catch(Exception e) {
            // FIXME?: more generic err msg
            Debug.logError(e, logPrefix + "Could not determine missing image variants for product [" + productId
                    + "] productContentTypeId [" + productContentTypeId + "] image [" + imageOrigFn + "]: " + e.getMessage(), module);
            return null;
        }
    }

    public static Map<String, Object> productImageAutoRescaleProducts(ServiceContext ctx, ProcessSignals processSignals, boolean logFinal) throws ServiceValidationException {
        String logPrefix = ctx.getModelService().name + ": ";
        int productCount = 0;
        int successCount = 0;
        int errorCount = 0;
        int failCount = 0;
        int skipCount = 0;
        Iterator<?> productsIt = (ctx.attr("products") != null) ? UtilMisc.asIterator(ctx.attr("products")) : null;
        int lastProductCount = ctx.attr("lastProductCount", 10);
        List<String> lastProductIdList = new LinkedList<>();
        try {
            Integer maxProducts = ctx.attr("maxProducts");
            Integer maxErrorCount = ctx.attr("maxProducts");
            boolean sepTrans = ctx.attr("sepTrans", true);
            Integer logBatch = ctx.attr("logBatch");
            if (logBatch != null && logBatch <= 0) {
                logBatch = null;
            }
            if (productsIt == null) {
                if (Boolean.TRUE.equals(ctx.attr("allProducts"))) {
                    try {
                        productsIt = ctx.delegator().from("Product").where((EntityCondition) ctx.attr("allCond"))
                                .maxRows(maxProducts).orderBy(ctx.<List<String>>attr("allOrderBy")).queryIterator();
                    } catch (GenericEntityException e) {
                        Debug.logError(e, module);
                        return ServiceUtil.returnError(e.toString());
                    }
                } else {
                    throw new ServiceValidationException("Missing products list/iterator or allProducts flag", ctx.getModelService());
                }
            }

            String allResumeId = ctx.attr("allResumeId");
            int resumeSkipped = 0;

            Object productObj;
            while ((productObj = UtilMisc.next(productsIt)) != null) {
                GenericValue product = (productObj instanceof GenericValue) ? (GenericValue) productObj : null;
                String productId = (product != null) ? product.getString("productId") : (String) productObj;
                if (allResumeId != null) {
                    if (allResumeId.equals(productId)) {
                        Debug.logInfo(logPrefix + "Resuming from product: " + productId + "(skipped: " + resumeSkipped + ")", module);
                        allResumeId = null;
                    } else {
                        resumeSkipped++;
                        continue;
                    }
                }
                if (logBatch != null) {
                    if ((productCount % logBatch) == 0) {
                        Debug.logInfo(logPrefix + "Processing product " + ((productCount + 1)) + " [" + productId + "] (last: " + lastProductIdList + ")", module);
                    }
                    while(lastProductIdList.size() >= lastProductCount) {
                        lastProductIdList.remove(0);
                    }
                    lastProductIdList.add(productId);
                }
                if (processSignals != null && processSignals.isSet("stop")) {
                    Debug.logWarning(logPrefix + " aborted (products: " + productCount + ", last: " + lastProductIdList + ")", module);
                    return ServiceUtil.returnFailure(processSignals.getProcess() + " aborted (last: " + lastProductIdList + ")");
                }

                productCount++;
                try {
                    Map<String, Object> servCtx = ctx.makeValidInContext("productImageAutoRescale", ctx);
                    if (product != null) {
                        servCtx.put("product", product);
                    } else if (UtilValidate.isNotEmpty(productId)) {
                        servCtx.put("productId", productObj);
                    } else {
                        throw new ServiceValidationException("Invalid product, should be GenericValue or String: " + productObj.getClass(), ctx.getModelService());
                    }
                    Map<String, Object> servResult = ctx.dispatcher().runSync("productImageAutoRescale", servCtx, sepTrans);
                    Integer servErrorCount = (Integer) servResult.get("errorCount");
                    if (servErrorCount == null) {
                        servErrorCount = ServiceUtil.isError(servResult) ? 1 : 0;
                    }
                    Integer servFailCount = (Integer) servResult.get("failCount");
                    if (servFailCount == null) {
                        servFailCount = ServiceUtil.isFailure(servResult) ? 1 : 0;
                    }
                    Integer servSuccessCount = (Integer) servResult.get("successCount");
                    if (servSuccessCount == null) {
                        servSuccessCount = ServiceUtil.isSuccess(servResult) ? 1 : 0;
                    }
                    Integer servSkipCount = (Integer) servResult.get("skipCount");
                    errorCount += servErrorCount;
                    failCount += servFailCount;
                    successCount += servSuccessCount;
                    if (servSkipCount != null) {
                        skipCount += servSkipCount;
                    }
                } catch (GenericServiceException e) {
                    Debug.logError(e, logPrefix + e.toString(), module);
                    errorCount++;
                }
                if (maxErrorCount != null && errorCount >= maxErrorCount) {
                    Debug.logError(logPrefix + "max errors reached (" + maxErrorCount + ")", module);
                    break;
                }
                if (maxProducts != null && productCount >= maxProducts) {
                    Debug.logInfo(logPrefix + "max products reached (" + maxProducts + ")", module);
                    break;
                }
            }
        } finally {
            if (productsIt instanceof AutoCloseable) {
                try {
                    ((AutoCloseable) productsIt).close();
                } catch(Exception e) {
                    Debug.logError(e, module);
                }
            }
        }
        if (logFinal && Debug.infoOn()) {
            if (errorCount > 0) {
                Debug.logWarning(logPrefix + errorCount + " errors processing product images (" + productCount + " products)" + " (last " + lastProductIdList.size() + " products: " + lastProductIdList + ")", module);
            } else {
                Debug.logInfo(logPrefix + "Processed products images (" + productCount + " products)" + " (last " + lastProductIdList.size() + " products: " + lastProductIdList + ")", module);
            }
        }
        return UtilMisc.put((errorCount > 0) ? ServiceUtil.returnFailure(errorCount + " errors processing product images (" + productCount + " products)" +
                        " (last " + lastProductIdList.size() + " products: " + lastProductIdList + ")") :
                ServiceUtil.returnSuccess("Processed products images (" + productCount + " products)" +
                        " (last " + lastProductIdList.size() + " products: " + lastProductIdList + ")"),
                "errorCount", errorCount, "failCount", failCount, "successCount", successCount, "skipCount", skipCount);
    }

    public static Map<String, Object> productImageAutoRescaleProducts(ServiceContext ctx) throws ServiceValidationException {
        return productImageAutoRescaleProducts(ctx, null, false);
    }

    public static Map<String, Object> productImageAutoRescaleAll(ServiceContext ctx) throws ServiceValidationException {
        try {
            productImageAutoRescaleAllSignals.clear();
            return productImageAutoRescaleProducts(ctx, productImageAutoRescaleAllSignals, true);
        } finally {
            productImageAutoRescaleAllSignals.clear();
        }
    }

    public static Map<String, Object> abortProductImageAutoRescaleAll(ServiceContext ctx) {
        productImageAutoRescaleAllSignals.put("stop");
        return ServiceUtil.returnSuccess();
    }
}

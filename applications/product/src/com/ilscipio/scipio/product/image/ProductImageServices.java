package com.ilscipio.scipio.product.image;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.ProcessSignals;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import com.ilscipio.scipio.content.image.ContentImageServices;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelUtil;
import org.ofbiz.product.image.ScaleImage;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.ServiceValidationException;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeMap;

/**
 * SCIPIO: New product image services, alternatives to {@link org.ofbiz.product.imagemanagement.ImageManagementServices}
 * and other utils.
 * TODO?: try to reconcile everything in the future, too difficult for now.
 * Added 2017-07-05.
 */
public class ProductImageServices {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
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
    public static Map<String, Object> productImageFileScaleInAllSize(ServiceContext ctx) throws ServiceValidationException {
        Delegator delegator = ctx.delegator();
        String productId = (String) ctx.get("productId");

        ProductImageViewType imageViewType = ctx.attr("imageViewType");
        if (imageViewType != null) {
            if (!imageViewType.isOriginal()) {
                String errorMsg = "productImageFileScaleInAllSize: product [" + productId + "]: imageViewType is not 'original' viewSize";
                Debug.logError("productImageFileScaleInAllSize: " + errorMsg, module);
                return ServiceUtil.returnError(errorMsg);
            }
        } else {
            try {
                String viewType = ctx.getString("viewType");
                String viewNumber = ctx.getString("viewNumber", "main".equals(viewType) ? "0" : null);
                imageViewType = ProductImageViewType.from(delegator, viewType, viewNumber, "original", true, true);
            } catch (Exception e) {
                Debug.logError(e, "productImageFileScaleInAllSize: product [" + productId + "]: error determining imageViewType for viewType [" +
                        ctx.getString("viewType") + "] viewNumber [" + ctx.getString("viewNumber") + "]: " + e.toString(), module);
                return ServiceUtil.returnError(e.toString());
            }
        }
        String productContentTypeId = imageViewType.getContentTypeId();

        Map<String, Object> contentCtx;
        try {
            contentCtx = ctx.makeValidContext("contentImageFileScaleInAllSizeCore", "IN", ctx.context());
        } catch (GenericServiceException e) {
            return ServiceUtil.returnError(e.getMessage());
        }

        ProductImageLocationInfo locInfo;
        try {
            String imagePath = ctx.attr("imageOrigFn");
            if (UtilValidate.isEmpty(imagePath)) {
                imagePath = ctx.attr("imageOrigPath");
                if (UtilValidate.isEmpty(imagePath)) {
                    imagePath = ctx.attr("imageOrigUrl");
                    if (UtilValidate.isEmpty(imagePath)) {
                        throw new IllegalArgumentException("Required parameter missing: imageOrigFn/imageOrigPath/imageOrigUrl");
                    }
                }
            }
            locInfo = ProductImageLocationInfo.from(ctx.dctx(), productId, imageViewType, (ImageProfile) null, imagePath, null,
                    false, false, null);

             if (isStrArgEmpty(contentCtx, "imageServerPath")) {
                contentCtx.put("imageServerPath", locInfo.getImageServerPathExpr());
            }
            if (isStrArgEmpty(contentCtx, "imageUrlPrefix")) {
                contentCtx.put("imageUrlPrefix", locInfo.getImageUrlPrefixExpr());
            }
            if (isStrArgEmpty(contentCtx, "imageFnFmt")) {
                contentCtx.put("imageFnFmt", locInfo.getImageFnFmtExpr());
            }

            Map<String, Object> imagePathArgs = locInfo.getImagePathArgs();
            Map<String, ?> imagePathArgsRcvd = UtilGenerics.checkMap(contentCtx.get("imagePathArgs"));
            if (imagePathArgsRcvd != null) {
                imagePathArgs = new HashMap<>(imagePathArgs);
                imagePathArgs.putAll(imagePathArgsRcvd); // explicit args crush ours
            }
            contentCtx.put("imagePathArgs", imagePathArgs);
            contentCtx.put("defaultImageProfile", ProductImageWorker.getDefaultProductImageProfile(delegator, productContentTypeId, false, false));

            // TODO/FIXME: currently provides no deletion of the old images...

        } catch(Exception e) {
            Debug.logError(e, "productImageFileScaleInAllSize: product [" + productId + "]: error preparing context: " + e.toString(), module);
            return ServiceUtil.returnError(e.toString());
        }

        Map<String, Object> result = ContentImageServices.contentImageFileScaleInAllSizeCore(ctx.from(contentCtx));
        result.put("productSizeTypeList", ScaleImage.sizeTypeList);
        return result;
    }

    public static Map<String, Object> productImageAutoRescale(ServiceContext ctx) throws ServiceValidationException { // SCIPIO
        boolean nonFatal = ctx.attr("nonFatal", false);

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
                productContentTypeIdList = new ArrayList<>(ProductImageViewType.getOriginalViewSizeProductContentTypes(ctx.delegator(), true).keySet());
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.toString());
            }
        }

        boolean logDetail = ctx.attr("logDetail", false);
        if (logDetail || Debug.verboseOn()) {
            Debug.logInfo("productImageRescaleImage: product [" + productId + "]: processing"
                    + (UtilValidate.isNotEmpty(productContentTypeIdList) ? " productContentTypeIdList " + productContentTypeIdList : "")
                    + (UtilValidate.isNotEmpty(contentIdList) ? " contentIdList " + contentIdList : ""), module);
        }

        Map<String, String> imageOrigUrlMap = ctx.attr("imageOrigUrlMap", Collections::emptyMap);
        String imageOrigUrlParam = ctx.attr("imageOrigUrl");
        if (UtilValidate.isNotEmpty(imageOrigUrlParam) && UtilValidate.isNotEmpty(productContentTypeIdParam)) {
            imageOrigUrlMap = new HashMap<>(imageOrigUrlMap);
            imageOrigUrlMap.put(productContentTypeIdParam, imageOrigUrlParam);
        }
        boolean copyOrig = ctx.attr("copyOrig");

        Map<String, Object> stats = UtilMisc.put(new LinkedHashMap<>(), "successCount", 0, "failCount", 0, "errorCount", 0, "skipCount", 0,
                "variantSuccessCount", 0, "variantFailCount", 0);
        for(String productContentTypeId : productContentTypeIdList) {
            String origImageUrl = imageOrigUrlMap.get(productContentTypeId);
            Map<String, Object> res = productImageRescaleImage(ctx, product, productContentTypeId, null, nonFatal, origImageUrl, copyOrig);
            if ("no-image-url".equals(res.get("reason")) && "ORIGINAL_IMAGE_URL".equals(productContentTypeId)) {
                // SPECIAL: stock Scipio and other products don't always have ORIGINAL_IMAGE_URL but sometimes only DETAIL_IMAGE_URL
                Map<String, Object> res2 = productImageRescaleImage(ctx, product, "DETAIL_IMAGE_URL", null, nonFatal, origImageUrl, copyOrig);
                if (!"no-image-url".equals(res2.get("reason"))) {
                    res = res2;
                } else {
                    Map<String, Object> res3 = productImageRescaleImage(ctx, product, "LARGE_IMAGE_URL", null, nonFatal, origImageUrl, copyOrig);
                    if (!"no-image-url".equals(res3.get("reason"))) {
                        res = res3;
                    }
                }
            }
            if (!productImageAutoRescaleRegisterResult(ctx, product, res, stats, nonFatal)) {
                Map<String, Object> result = ServiceUtil.returnError(((int) stats.get("errorCount")) + " errors auto-rescaling images for product [" + productId + "]" + " (stats: " + stats + ")");
                result.putAll(stats);
                return result;
            }
        }
        for(String contentId : contentIdList) {
            Map<String, Object> res = productImageRescaleImage(ctx, product, null, contentId, nonFatal, null, copyOrig);
            if (!productImageAutoRescaleRegisterResult(ctx, product, res, stats, nonFatal)) {
                Map<String, Object> result = ServiceUtil.returnError(((int) stats.get("errorCount")) + " errors auto-rescaling images for product [" + productId + "]" + " (stats: " + stats + ")");
                result.putAll(stats);
                return result;
            }
        }

        if (Boolean.TRUE.equals(ctx.attr("clearCaches"))) {
            try {
                Map<String, Object> clearCachesCtx = ctx.makeValidContext("productImageVariantsClearCaches", "IN", ctx.context());
                clearCachesCtx.put("productId", productId);
                clearCachesCtx.put("distribute", true);
                Map<String, Object> clearCachesResult = ctx.dispatcher().runSync("productImageVariantsClearCaches", clearCachesCtx);
                if (!ServiceUtil.isSuccess(clearCachesResult)) {
                    Debug.logWarning("productImageRescaleImage: product [" + productId + "]: error clearing caches: " + ServiceUtil.getErrorMessage(clearCachesResult), module);
                }
            } catch (GenericServiceException e) {
                Debug.logWarning(e, "productImageRescaleImage: product [" + productId + "]: error clearing caches: " + e.toString(), module);
            }
        }

        Map<String, Object> result;
        int successCount = ((int) stats.get("successCount"));
        int errorCount = ((int) stats.get("errorCount"));
        int failCount = ((int) stats.get("failCount"));

        String msg = "Rescaled " + successCount + " images for product [" + productId + "]" + " (stats: " + stats + ")";
        if (errorCount > 0 || failCount > 0) {
            if (nonFatal) {
                result = ServiceUtil.returnFailure(msg);
            } else {
                result = ServiceUtil.returnError(msg);
            }
        } else {
            result = ServiceUtil.returnSuccess(msg);
        }
        result.putAll(stats);
        return result;
    }

    private static boolean productImageAutoRescaleRegisterResult(ServiceContext ctx, GenericValue product, Map<String, Object> res, Map<String, Object> stats, boolean nonFatal) {
        Integer variantSuccessCount = (Integer) res.get("variantSuccessCount");
        if (variantSuccessCount != null) {
            stats.put("variantSuccessCount", ((int) stats.get("variantSuccessCount")) + variantSuccessCount);
        }
        Integer variantFailCount = (Integer) res.get("variantFailCount");
        if (variantFailCount != null) {
            stats.put("variantFailCount", ((int) stats.get("variantFailCount")) + variantFailCount);
        }
        if (ServiceUtil.isError(res)) {
            stats.put("errorCount", ((int) stats.get("errorCount")) + 1);
            if (!nonFatal) {
                return false;
            }
        } else if (ServiceUtil.isFailure(res)) {
            stats.put("failCount", ((int) stats.get("failCount")) + 1);
            if (!nonFatal) {
                return false;
            }
        } else {
            if ("no-image-url".equals(res.get("reason"))) {
                stats.put("skipCount", ((int) stats.get("skipCount")) + 1);
            } else if ("inapplicable".equals(res.get("reason"))) {
                stats.put("skipCount", ((int) stats.get("skipCount")) + 1);
            } else if ("all-exist".equals(res.get("reason"))) {
                stats.put("skipCount", ((int) stats.get("skipCount")) + 1);
            } else {
                stats.put("successCount", ((int) stats.get("successCount")) + 1);
            }
        }
        return true;
    }

    private static Map<String, Object> productImageRescaleImage(ServiceContext ctx, GenericValue product, String productContentTypeId, String contentId, boolean nonFatal,
                                                                String explOrigImageUrl, Boolean copyOrig) throws ServiceValidationException {
        int variantSuccessCount = 0;
        int variantFailCount = 0;
        boolean logDetail = ctx.attr("logDetail", false);
        String productId = product.getString("productId");

        ProductImageViewType imageViewType;
        ProductImageViewType origImageViewType;
        try {
            imageViewType = ProductImageViewType.from(ctx.delegator(), productContentTypeId, true, true);
            // NOTE: because productContentTypeId may be special exception values DETAIL_IMAGE_URL/LARGE_IMAGE_URL (instead of ORIGINAL_IMAGE_URL),
            // some code needs the following instead
            origImageViewType = imageViewType.getOriginal(true);
        } catch(Exception e) {
            String errorMsg = "product [" + productId + "] productContentTypeId [" + productContentTypeId + "]: could not determine image view type: " + e.toString();
            Debug.logError(e,"productImageRescaleImage: " + errorMsg, module);
            return ServiceUtil.returnError(errorMsg);
        }

        GenericValue content = null;
        if (UtilValidate.isNotEmpty(contentId)) {
            try {
                content = ctx.delegator().from("Content").where("contentId", contentId).queryOne();
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (content == null) {
                Debug.logError("productImageRescaleImage: product [" + productId + "] productContentTypeId [" + productContentTypeId +
                        "]: Content not found for contentId [" + contentId + "]", module);
                return ServiceUtil.returnError("product [" + productId + "] productContentTypeId [" + productContentTypeId +
                        "]: Content not found for contentId [" + contentId + "]");
            }
        }

        if (UtilValidate.isEmpty(contentId)) {
            contentId = null;
        }
        if (UtilValidate.isEmpty(productContentTypeId)) {
            productContentTypeId = null;
        }
        if (contentId == null && productContentTypeId == null) {
            String errMsg = "product [" + productId + "] productContentTypeId [" + productContentTypeId + "]: missing contentId or productContentTypeId";
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
                        return ServiceUtil.returnError("product [" + productId +
                                "] productContentTypeId [" + productContentTypeId + "]: could not find ProductContent for contentId [" + contentId + "]");
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
                    String msg = "product [" + productId + "] productContentTypeId [" + productContentTypeId +
                            "]: inapplicable productContentTypeId for resize operation (parent products not consulted)";
                    if (Debug.verboseOn()) { // typically not relevant: (logDetail || Debug.verboseOn())
                        Debug.logInfo("productImageRescaleImage: " + msg, module);
                    }
                    return UtilMisc.put(ServiceUtil.returnSuccess(msg), "reason", "inapplicable");
                }
            }
        }

        // DON'T consult parent product because it complicates update logic
        //// NOTE: this consults the parent product which we don't want, but in known cases should return right value
        //String origImageUrl = ProductContentWrapper.getProductContentAsText(product, productContentTypeId,
        //        ctx.attr("locale"), ctx.dispatcher(), false, "raw");
        String origImageUrl = explOrigImageUrl;
        if (origImageUrl == null) {
            if (productContent != null) {
                try {
                    GenericValue dataResource = (content != null) ? content.getRelatedOne("DataResource") : null;
                    origImageUrl = ProductImageWorker.getDataResourceImageUrl(dataResource, false);
                } catch (GenericEntityException e) {
                    Debug.logError(e, module);
                    return ServiceUtil.returnError(e.toString());
                }
            } else if (productFieldName != null) {
                origImageUrl = product.getString(productFieldName);
            }
            if (UtilValidate.isEmpty(origImageUrl)) {
                String msg = "product [" + productId + "] productContentTypeId [" + productContentTypeId + "] origImageUrl [" + origImageUrl + "]: no image URL, not resizing";
                if (logDetail || Debug.verboseOn()) {
                    Debug.logInfo("productImageRescaleImage: " + msg, module);
                }
                return UtilMisc.put(ServiceUtil.returnSuccess(msg), "reason", "no-image-url");
            }
        }

        /*
        Boolean deleteOld = ctx.getAttr("deleteOld");
        if (deleteOld == null) {
            deleteOld = UtilProperties.getPropertyAsBoolean("content", "image.auto.rescale.deleteOld", false);
        }
        boolean force = Boolean.TRUE.equals(ctx.getAttr("force"));
        boolean createInitial = Boolean.TRUE.equals(ctx.getAttr("createInitial"));
         */

        ImageProfile imageProfile = ProductImageWorker.getProductImageProfileOrDefault(ctx.delegator(),
                origImageViewType.getContentTypeId(), product, content, false, false);
        if (imageProfile == null) {
            String errorMsg = "product [" + productId + "] productContentTypeId [" + productContentTypeId +
                    "] origImageUrl [" + origImageUrl + "]: could not find media profile";
            Debug.logError("productImageRescaleImage: " + errorMsg, module);
            return ServiceUtil.returnError(errorMsg);
        }

        Collection<String> sizeTypeList = ctx.attr("sizeTypeList");
        boolean recreateExisting = ctx.attr("recreateExisting", false);
        if (!recreateExisting) {
            try {
                ProductImageLocationInfo pili = ProductImageLocationInfo.from(ctx.dctx(), ctx.locale(),
                        product, imageViewType, origImageUrl, sizeTypeList, false, false, false, null);
                sizeTypeList = (pili != null) ? pili.getMissingVariantNames() : null;
                if (UtilValidate.isEmpty(sizeTypeList)) {
                    String msg = "product [" + productId + "] productContentTypeId [" + productContentTypeId + "] origImageUrl [" + origImageUrl +
                            "]: no missing sizeTypes" + (sizeTypeList != null ? " for sizeTypeList [" + sizeTypeList + "]" : "") + "; not resizing";
                    if (logDetail || Debug.verboseOn()) {
                        Debug.logInfo("productImageRescaleImage: " + msg, module);
                    }
                    return UtilMisc.put(ServiceUtil.returnSuccess(msg), "reason", "all-exist");
                }
            } catch(Exception e) {
                String errorMsg = "product [" + productId + "] productContentTypeId [" + productContentTypeId + "] origImageUrl ["
                        + origImageUrl + "]: could not determine image variants: " + e.toString();
                Debug.logError(e,"productImageRescaleImage: " + errorMsg, module);
                return ServiceUtil.returnError(errorMsg);
            }
        }

        Debug.logInfo("productImageRescaleImage: product [" + productId + "] productContentTypeId [" + productContentTypeId
            + "] origImageUrl [" + origImageUrl + "]: begin scaling image variants", module);

        Map<String, Object> resizeCtx = UtilMisc.toMap("productId", productId, "imageOrigUrl", origImageUrl, "imageViewType", origImageViewType,
                "locale", ctx.get("locale"), "userLogin", ctx.get("userLogin"), "timeZone", ctx.get("timeZone"), "imageProfile", imageProfile,
                "sizeTypeList", sizeTypeList, "copyOrig", copyOrig);
        try {
            Map<String, Object> resizeResult;
            if (nonFatal) {
                resizeResult = ctx.dispatcher().runSync("productImageFileScaleInAllSize", resizeCtx, true);
            } else {
                resizeResult = ctx.dispatcher().runSync("productImageFileScaleInAllSize", resizeCtx);
            }
            if (resizeResult.get("successCount") != null) {
                variantSuccessCount += (Integer) resizeResult.get("successCount");
            }
            if (resizeResult.get("failCount") != null) {
                variantFailCount += (Integer) resizeResult.get("failCount");
            }
            if (!nonFatal) {
                if (!ServiceUtil.isSuccess(resizeResult)) {
                    throw new GeneralException("Error creating resized images: " + ServiceUtil.getErrorMessage(resizeResult));
                }
            }

            if (ServiceUtil.isError(resizeResult)) {
                String errMsg =  "product [" + productId + "] productContentTypeId [" + productContentTypeId + "] origImageUrl [" + origImageUrl +
                        "]: could not resize image: " + ServiceUtil.getErrorMessage(resizeResult);
                Debug.logError("productImageRescaleImage: " + errMsg, module);
                return UtilMisc.put(ServiceUtil.returnError(errMsg), "variantSuccessCount", variantSuccessCount, "variantFailCount", variantFailCount);
            } else if (ServiceUtil.isFailure(resizeResult)) {
                String errMsg =  "product [" + productId + "] productContentTypeId [" + productContentTypeId + "] origImageUrl [" + origImageUrl +
                        "]: could not resize image, non-fatal: " + ServiceUtil.getErrorMessage(resizeResult);
                Debug.logError("productImageRescaleImage: " + errMsg, module);
            }

            Map<String, Map<String, Object>> imageInfoMap = UtilGenerics.cast(resizeResult.get("imageInfoMap"));
            if (UtilValidate.isNotEmpty(imageInfoMap)) {
                Timestamp fromDate = UtilDateTime.nowTimestamp();
                for(Map.Entry<String, Map<String, Object>> entry : imageInfoMap.entrySet()) {
                    String sizeType = entry.getKey();
                    Map<String, Object> sizeTypeInfo = entry.getValue();
                    String imageUrl = (String) sizeTypeInfo.get("url");
                    ProductImageViewType scaledImageViewType = ProductImageViewType.from(ctx.delegator(),
                            imageViewType.getViewType(), imageViewType.getViewNumber(), sizeType, true, true); //ctx.attr("createSizeTypeContent")
                    if (imageUrl != null && (imageUrl.startsWith(".") || imageUrl.contains("/."))) { // SPECIAL: detect bug (missing filename)
                        throw new IllegalStateException("internal or data error: invalid url [" + imageUrl + "] for sizeType [" + sizeType + "], not updating");
                    }
                    Map<String, Object> res = updateProductContentImageUrl(ctx, product, scaledImageViewType.getContentTypeId(), imageUrl,
                            origImageUrl, productContentTypeId, fromDate, ctx.attr("createSizeTypeContent"), sizeTypeInfo);
                    if (ServiceUtil.isError(res)) {
                        return UtilMisc.put(new HashMap<>(res), "variantSuccessCount", variantSuccessCount, "variantFailCount", variantFailCount);
                    }
                }
            }
        } catch (Exception e) {
            Debug.logError(e, "productImageRescaleImage: product [" + productId + "] productContentTypeId [" + productContentTypeId +
                    "] origImageUrl [" + origImageUrl + "]: error resizing images: " + e.toString(), module);
            return UtilMisc.put(ServiceUtil.returnError(e.toString()), "variantSuccessCount", variantSuccessCount, "variantFailCount", variantFailCount);
        }
        String msg = variantSuccessCount + " variant success, " + variantFailCount + " variant failures";
        return UtilMisc.put(variantFailCount > 0 ? ServiceUtil.returnFailure(msg) : ServiceUtil.returnSuccess(msg), "reason", "success",
                "variantSuccessCount", variantSuccessCount, "variantFailCount", variantFailCount);
    }

    /** Attempts to preserve previous data setup, best-effort. */
    public static Map<String, Object> updateProductContentImageUrl(ServiceContext ctx, GenericValue product, String productContentTypeId, String imageUrl, String origImageUrl,
                                                                   String origProductContentTypeId, Timestamp fromDate, Boolean createSizeTypeContent,
                                                                   Map<String, Object> sizeTypeInfo) {
        GenericValue productContent;
        try {
            productContent = ctx.delegator().from("ProductContent").where("productId", product.get("productId"),
                    "productContentTypeId", productContentTypeId).orderBy("-fromDate").filterByDate().queryFirst();
        } catch (GenericEntityException e) {
            String errMsg = "product [" + product.get("productId") + "] productContentTypeId [" +
                    productContentTypeId + "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: error: " + e.toString();
            Debug.logError("updateProductContentImageUrl: " + errMsg, module);
            return ServiceUtil.returnError(errMsg);
        }
        try {
            if (productContent != null) {
                GenericValue content = productContent.getRelatedOne("Content");
                if (content == null) {
                    String errMsg = "product [" + product.get("productId") + "] productContentTypeId [" +
                            productContentTypeId + "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: could not find Content record for contentId [" + productContent.get("contentId") + "]";
                    Debug.logError("updateProductContentImageUrl: " + errMsg, module);
                    return ServiceUtil.returnError(errMsg);
                }
                GenericValue dataResource = content.getRelatedOne("DataResource");
                if (dataResource == null) {
                    String errMsg = "product [" + product.get("productId") + "] productContentTypeId [" +
                            productContentTypeId + "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: could not find image DataResource record";
                    Debug.logError("updateProductContentImageUrl: " + errMsg, module);
                    return ServiceUtil.returnError(errMsg);
                }

                boolean drModified = false;
                Integer widthInt = (Integer) sizeTypeInfo.get("width");
                Long width = (widthInt != null) ? widthInt.longValue() : null;
                Long prevWidth = dataResource.getLong("scpWidth");
                if (!Objects.equals(width, prevWidth)) {
                    dataResource.set("scpWidth", width);
                    drModified = true;
                }

                Integer heightInt = (Integer)  sizeTypeInfo.get("height");
                Long height = (heightInt != null) ? heightInt.longValue() : null;
                Long prevHeight = dataResource.getLong("scpHeight");
                if (!Objects.equals(height, prevHeight)) {
                    dataResource.set("scpHeight", height);
                    drModified = true;
                }

                if (!"ORIGINAL_IMAGE_URL".equals(productContentTypeId)) {
                    ImageVariantConfig.VariantInfo variantInfo = (ImageVariantConfig.VariantInfo) sizeTypeInfo.get("variantInfo");
                    Map<String, Object> prevJsonMap = dataResource.getJsonMap("srcPresetJson");
                    if (variantInfo != null) {
                        Map<String, Object> newJsonMap = variantInfo.configToMap();
                        if (!new TreeMap<>(newJsonMap).equals(new TreeMap<>(prevJsonMap))) {
                            dataResource.setJson("srcPresetJson", newJsonMap);
                            drModified = true;
                        }
                    } else if (!prevJsonMap.isEmpty()) {
                        dataResource.setJson("srcPresetJson", null);
                        drModified = true;
                    }
                }

                String mimeTypeId = (String) sizeTypeInfo.get("mimeTypeId");
                String prevMimeTypeId = dataResource.getString("srcMimeTypeId");
                if (!Objects.equals(mimeTypeId, prevMimeTypeId)) {
                    dataResource.put("srcMimeTypeId", mimeTypeId);
                    drModified = true;
                }

                if (imageUrl != null) {
                    if ("SHORT_TEXT".equals(dataResource.get("dataResourceTypeId"))) {
                        String prevImageUrl = dataResource.getString("objectInfo");
                        if (!imageUrl.equals(prevImageUrl)) {
                            Debug.logInfo("updateProductContentImageUrl: product [" + product.get("productId") + "] productContentTypeId [" + productContentTypeId +
                                    "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: updating DataResource imageUrl from [" + prevImageUrl + "] to [" + imageUrl + "]", module);
                            dataResource.set("objectInfo", imageUrl);
                            dataResource.store();
                            drModified = false;
                        }
                    } else if ("ELECTRONIC_TEXT".equals(dataResource.get("dataResourceTypeId"))) {
                        if (drModified) {
                            dataResource.store();
                            drModified = false;
                        }
                        GenericValue elecText = ctx.delegator().from("ElectronicText").where("dataResourceId", content.get("dataResourceId")).queryOne();
                        if (elecText == null) {
                            String msg = "product [" + product.get("productId") + "] productContentTypeId [" + productContentTypeId +
                                    "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: could not find image ElectronicText: unexpected ProductContent format";
                            Debug.logWarning("updateProductContentImageUrl: " + msg, module);
                            return ServiceUtil.returnFailure(msg);
                        }
                        String prevImageUrl = elecText.getString("textData");
                        if (!imageUrl.equals(prevImageUrl)) {
                            Debug.logInfo("updateProductContentImageUrl: product [" + product.get("productId") + "] productContentTypeId [" +
                                    productContentTypeId + "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl +
                                    "]: updating ElectronicText imageUrl from [" + prevImageUrl + "] to [" + imageUrl + "]", module);
                            elecText.set("textData", imageUrl);
                            elecText.store();
                        }
                    } else {
                        String errMsg = "product [" + product.get("productId") + "] productContentTypeId [" + productContentTypeId +
                                "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: invalid image DataResource record";
                        Debug.logError("updateProductContentImageUrl: " + errMsg, module);
                        return ServiceUtil.returnError(errMsg);
                    }
                }
                if (drModified) {
                    dataResource.store();
                }
            } else {
                // Check field
                String productFieldName = ModelUtil.dbNameToVarName(productContentTypeId);
                ModelEntity productModel = ctx.delegator().getModelEntity("Product");
                if (productModel.isField(productFieldName)) {
                    String prevImageUrl = product.getString(productFieldName);
                    if (imageUrl != null && !imageUrl.equals(prevImageUrl)) {
                        Debug.logInfo("updateProductContentImageUrl: product [" + product.get("productId") + "] productContentTypeId [" + productContentTypeId +
                                "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: updating Product field [" + productFieldName +
                                "] imageUrl from [" + prevImageUrl + "] to [" + imageUrl + "]", module);
                        product.set(productFieldName, imageUrl);
                        product.store();
                    }

                    if (productFieldName.endsWith("Url")) {
                        String fieldPrefix = productFieldName.substring(0, productFieldName.length() - "Url".length());
                        GenericValue details = ctx.delegator().findOne("ProductMediaDetails",
                                UtilMisc.toMap("productId", product.get("productId")), false);
                        boolean detailsCreated = (details == null);
                        if (detailsCreated) {
                            details = ctx.delegator().makeValue("ProductMediaDetails", "productId", product.get("productId"));
                        }
                        boolean detailsModified = detailsCreated;

                        Integer widthInt = (Integer) sizeTypeInfo.get("width");
                        Long width = (widthInt != null) ? widthInt.longValue() : null;
                        Long prevWidth = details.getLong(fieldPrefix + "Width");
                        if (!Objects.equals(width, prevWidth)) {
                            details.put(fieldPrefix + "Width", width);
                            detailsModified = true;
                        }

                        Integer heightInt = (Integer)  sizeTypeInfo.get("height");
                        Long height = (heightInt != null) ? heightInt.longValue() : null;
                        Long prevHeight = details.getLong(fieldPrefix + "Height");
                        if (!Objects.equals(height, prevHeight)) {
                            details.put(fieldPrefix + "Height", height);
                            detailsModified = true;
                        }

                        if (!"ORIGINAL_IMAGE_URL".equals(productContentTypeId)) {
                            ImageVariantConfig.VariantInfo variantInfo = (ImageVariantConfig.VariantInfo) sizeTypeInfo.get("variantInfo");
                            Map<String, Object> prevJsonMap = details.getJsonMap(fieldPrefix + "PresetJson");
                            if (variantInfo != null) {
                                Map<String, Object> newJsonMap = variantInfo.configToMap();
                                if (!new TreeMap<>(newJsonMap).equals(new TreeMap<>(prevJsonMap))) {
                                    details.setJson(fieldPrefix + "PresetJson", newJsonMap);
                                    detailsModified = true;
                                }
                            } else if (!prevJsonMap.isEmpty()) {
                                details.setJson(fieldPrefix + "PresetJson", null);
                                detailsModified = true;
                            }
                        }

                        String mimeTypeId = (String) sizeTypeInfo.get("mimeTypeId");
                        String prevMimeTypeId = details.getString(fieldPrefix + "MimeTypeId");
                        if (!Objects.equals(mimeTypeId, prevMimeTypeId)) {
                            details.set(fieldPrefix + "MimeTypeId", mimeTypeId);
                            detailsModified = true;
                        }

                        if (detailsCreated) {
                            details.create();
                        } else if (detailsModified) {
                            details.store();
                        }
                    } else {
                        Debug.logWarning("updateProductContentImageUrl: product [" + product.get("productId") + "] productContentTypeId [" + productContentTypeId +
                                "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: unrecognized Product field name [" + productFieldName + "]; cannot store InlineProductImageDetails for image field; width/height/preset will not be saved", module);
                    }
                } else if (Boolean.TRUE.equals(createSizeTypeContent) && !"ORIGINAL_IMAGE_URL".equals(productContentTypeId)) {
                    // Try to find a ProductContent record to refer to
                    GenericValue origProductContent = ctx.delegator().from("ProductContentAndDataResource").where(
                                "productId", product.get("productId"), "productContentTypeId", origProductContentTypeId).orderBy("-fromDate").filterByDate().queryFirst();
                    if (origProductContent == null) {
                        if (!"ORIGINAL_IMAGE_URL".equals(origProductContentTypeId)) {
                            origProductContent = ctx.delegator().from("ProductContentAndDataResource").where(
                                    "productId", product.get("productId"), "productContentTypeId", "ORIGINAL_IMAGE_URL").orderBy("-fromDate").filterByDate().queryFirst();
                        }
                    }
                    Debug.logInfo("updateProductContentImageUrl: product [" + product.get("productId")
                            + "] productContentTypeId [" + productContentTypeId + "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl +
                            "]: no existing record: creating new DataResource/Content/ProductContent" +
                            " using reference productContentTypeId [" + (origProductContent != null ? origProductContent.get("productContentTypeId") : "(none)") + "]", module);
                    String dataResourceName = (origProductContent != null) ? origProductContent.getString("drDataResourceName") : null;
                    if (dataResourceName == null) {
                        int lastSlash = imageUrl.lastIndexOf('/');
                        dataResourceName = (lastSlash >= 0) ? imageUrl.substring(lastSlash + 1) : imageUrl;
                    }
                    String statusId = (origProductContent != null) ? origProductContent.getString("drStatusId") : "CTNT_IN_PROGRESS";

                    GenericValue dataResource = ctx.delegator().makeValue("DataResource",
                            "dataResourceTypeId", "SHORT_TEXT", "dataTemplateTypeId", "NONE", "statusId", statusId,
                            "mimeTypeId", "text/html", "dataResourceName", dataResourceName, "objectInfo", imageUrl);

                    Integer width = (Integer) sizeTypeInfo.get("width");
                    dataResource.set("scpWidth", (width != null) ? width.longValue() : null);

                    Integer height = (Integer) sizeTypeInfo.get("height");
                    dataResource.set("scpHeight", (height != null) ? height.longValue() : null);

                    ImageVariantConfig.VariantInfo variantInfo = (ImageVariantConfig.VariantInfo) sizeTypeInfo.get("variantInfo");
                    dataResource.setJson("srcPresetJson", (variantInfo != null) ? variantInfo.configToMap() : null);

                    String mimeTypeId = (String) sizeTypeInfo.get("mimeTypeId");
                    dataResource.set("srcMimeTypeId", mimeTypeId);

                    dataResource = dataResource.createSetNextSeqId();

                    statusId = (origProductContent != null) ? origProductContent.getString("statusId") : "CTNT_IN_PROGRESS";
                    GenericValue content = ctx.delegator().makeValue("Content", "dataResourceId", dataResource.get("dataResourceId"),
                            "statusId", statusId, "contentTypeId", "DOCUMENT");
                    content = content.createSetNextSeqId();
                    productContent = ctx.delegator().makeValue("ProductContent", "productId", product.get("productId"),
                            "contentId", content.get("contentId"), "productContentTypeId", productContentTypeId, "fromDate", fromDate).create();
                }
            }
        } catch (GenericEntityException e) {
            String errMsg = "product [" + product.get("productId") + "] productContentTypeId [" +
                    productContentTypeId + "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: error: " + e.toString();
            Debug.logError("updateProductContentImageUrl: " + errMsg, module);
            return ServiceUtil.returnError(errMsg);
        }
        return ServiceUtil.returnSuccessReadOnly();
    }

    private static boolean isStrArgEmpty(Map<String, ?> context, String argName) {
        return UtilValidate.isEmpty((String) context.get(argName));
    }

    public static Map<String, Object> productImageAutoRescaleProducts(ServiceContext ctx, ProcessSignals processSignals, boolean logFinal) throws ServiceValidationException {
        String logPrefix = ctx.getModelService().name + ": ";
        int productCount = 0;
        int successCount = 0;
        int errorCount = 0;
        int failCount = 0;
        int skipCount = 0;
        int variantSuccessCount = 0;
        int variantFailCount = 0;
        Iterator<?> productsIt = (ctx.attr("products") != null) ? UtilMisc.asIterator(ctx.attr("products")) : null;
        if (productsIt == null) {
            productsIt = (ctx.attr("productIdList") != null) ? UtilMisc.asIterator(ctx.attr("productIdList")) : null;
        }
        int lastProductCount = ctx.attr("lastProductCount", 10);
        List<String> lastProductIdList = new LinkedList<>();
        List<String> failProductIdList = new ArrayList<>();
        try {
            Integer maxProducts = ctx.attr("maxProducts");
            Integer maxErrorCount = ctx.attr("maxProducts");
            boolean sepProductTrans = ctx.attr("sepProductTrans", true);
            Integer logBatch = ctx.attr("logBatch");
            if (logBatch != null && logBatch <= 0) {
                logBatch = null;
            }
            if (productsIt == null) {
                if (Boolean.TRUE.equals(ctx.attr("allProducts"))) {
                    try {
                        // REMOVED: orderBy even with cursorScrollInsensitive caused child transaction updates of Product to block.
                        //productsIt = ctx.delegator().from("Product").where((EntityCondition) ctx.attr("allCond"))
                        //        orderBy(ctx.<List<String>>attr("allOrderBy")).cursorScrollInsensitive().queryIterator();
                        // NOTE: REMOVED maxRows() because of allResumeId support
                        productsIt = ctx.delegator().from("Product").where((EntityCondition) ctx.attr("allCond"))
                                .orderBy(ctx.<List<String>>attr("allOrderBy")).getFieldList("productId").iterator();
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
                    String abortMsg = processSignals.getProcess() + " aborted (products: " + productCount + ", last: " + lastProductIdList + ")";
                    Debug.logWarning(logPrefix + abortMsg, module);
                    return ServiceUtil.returnFailure(abortMsg);
                }

                productCount++;
                try {
                    Map<String, Object> servCtx = ctx.makeValidContext("productImageAutoRescale", "IN", ctx);
                    if (product != null) {
                        // avoid just in case, separate transaction
                        //servCtx.put("product", product);
                        servCtx.put("productId", product.get("productId"));
                    } else if (UtilValidate.isNotEmpty(productId)) {
                        servCtx.put("productId", productObj);
                    } else {
                        throw new ServiceValidationException("Invalid product, should be GenericValue or String: " + productObj.getClass(), ctx.getModelService());
                    }
                    servCtx.put("nonFatal", true); // TODO: unhardcode (NOTE: causes extra separate transactions per-image)
                    Map<String, Object> servResult = ctx.dispatcher().runSync("productImageAutoRescale", servCtx, sepProductTrans);
                    if (!ServiceUtil.isSuccess(servResult)) {
                        failProductIdList.add(productId);
                    }
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
                    Integer servVariantSuccessCount = (Integer) servResult.get("variantSuccessCount");
                    if (servVariantSuccessCount != null) {
                        variantSuccessCount += servVariantSuccessCount;
                    }
                    Integer servVariantFailCount = (Integer) servResult.get("variantFailCount");
                    if (servVariantFailCount != null) {
                        variantFailCount += servVariantFailCount;
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
            if (allResumeId != null) {
                Debug.logWarning(logPrefix + "Did not reach allResumeId product [" + allResumeId + "]; either no products or wrong productId", module);
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
        Map<String, Object> stats = UtilMisc.put(new LinkedHashMap<>(), "successCount", successCount, "failCount", failCount, "errorCount", errorCount,
                "skipCount", skipCount, "variantSuccessCount", variantSuccessCount, "variantFailCount", variantFailCount);
        String failProductsStr = " (failed products: " + failProductIdList + ")";
        if (logFinal && Debug.infoOn()) {
            if (errorCount > 0) {
                Debug.logError(logPrefix + errorCount + " errors processing product images (" + productCount + " products)"
                        + " (last " + lastProductIdList.size() + " products: " + lastProductIdList + ")" + " (stats: " + stats + ")" + failProductsStr, module);
            } else {
                Debug.logInfo(logPrefix + "Processed products images (" + productCount + " products)" + " (last "
                        + lastProductIdList.size() + " products: " + lastProductIdList + ")" + " (stats: " + stats + ")" + failProductsStr, module);
            }
        }
        Map<String, Object> result = (errorCount > 0) ?
                ServiceUtil.returnFailure(errorCount + " errors processing product images (" + productCount + " products)" +
                        " (last " + lastProductIdList.size() + " products: " + lastProductIdList + ")" + failProductsStr) :
                ServiceUtil.returnSuccess("Processed products images (" + productCount + " products)" +
                        " (last " + lastProductIdList.size() + " products: " + lastProductIdList + ")" + failProductsStr);
        result.putAll(stats);
        result.put("failProductIdList", failProductIdList);
        return result;
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

    public static Map<String, Object> productImageMigrateImageUrlProductContentTypeData(ServiceContext ctx) throws ServiceValidationException {
        try {
            boolean forceAll = Boolean.TRUE.equals(ctx.attr("forceAll"));
            boolean force = Boolean.TRUE.equals(ctx.attr("force")) || forceAll;
            boolean preview = Boolean.TRUE.equals(ctx.attr("preview"));
            if (!force) {
                GenericValue origPct = ctx.delegator().from("ProductContentType").where("productContentTypeId", "ORIGINAL_IMAGE_URL").queryOne();
                if (origPct == null) {
                    Debug.logError("productImageMigrateImageUrlProductContentTypeData: Missing ORIGINAL_IMAGE_URL", module);
                    return ServiceUtil.returnError("Missing ORIGINAL_IMAGE_URL");
                }
                if (isProductContentTypeImageUrlRecordComplete(origPct)) {
                    Debug.logInfo("productImageMigrateImageUrlProductContentTypeData: ORIGINAL_IMAGE_URL populated, not running", module);
                    return ServiceUtil.returnSuccess("ORIGINAL_IMAGE_URL populated, not running");
                }
            }

            int updated = 0;

            GenericValue baseImagePct = ctx.delegator().findOne("ProductContentType", UtilMisc.toMap("productContentTypeId", "IMAGE_URL_BASE"), false);
            if (baseImagePct == null) {
                baseImagePct = ctx.delegator().makeValue("ProductContentType", "productContentTypeId", "IMAGE_URL_BASE",
                        "hasTable", "N", "description", "Image - Base").create();
                updated++;
                Debug.logInfo("productImageMigrateImageUrlProductContentTypeData: created: " + baseImagePct, module);
            }
            GenericValue fullImagePct = ctx.delegator().findOne("ProductContentType", UtilMisc.toMap("productContentTypeId", "IMAGE_URL_FULL"), false);
            if (fullImagePct == null) {
                fullImagePct = ctx.delegator().makeValue("ProductContentType", "productContentTypeId", "IMAGE_URL_FULL",
                        "hasTable", "N", "description", "Image - Full", "parentTypeId", "IMAGE_URL_BASE").create();
                updated++;
                Debug.logInfo("productImageMigrateImageUrlProductContentTypeData: created: " + fullImagePct, module);
            }

            List<EntityCondition> orCondList = new ArrayList<>();
            orCondList.add(EntityCondition.makeCondition("productContentTypeId", EntityOperator.LIKE, "%_IMAGE_URL"));
            orCondList.add(EntityCondition.makeCondition("productContentTypeId", EntityOperator.LIKE, "ADDITIONAL_IMAGE_%"));
            orCondList.add(EntityCondition.makeCondition("productContentTypeId", EntityOperator.LIKE, "XTRA_IMG_%"));

            List<GenericValue> pcts = ctx.delegator().from("ProductContentType")
                    .where(EntityCondition.makeCondition(orCondList, EntityOperator.OR)).queryList();
            for(GenericValue pct : pcts) {
                if (forceAll || !isProductContentTypeImageUrlRecordComplete(pct)) {
                    String pctId = pct.getString("productContentTypeId");
                    String viewType = ProductImageViewType.extractProductContentTypeIdViewType(ctx.delegator(), pctId);
                    String viewNumber = ProductImageViewType.extractProductContentTypeIdViewNumber(ctx.delegator(), pctId);
                    String viewSize = ProductImageViewType.extractProductContentTypeIdViewSize(ctx.delegator(), pctId);
                    if ("L".equals(viewNumber)) {
                        continue;
                    }

                    String parentTypeId;
                    if ("original".equals(viewSize)) {
                        parentTypeId = "IMAGE_URL_FULL";
                    } else {
                        if ("main".equals(viewType)) {
                            parentTypeId = "ORIGINAL_IMAGE_URL";
                        } else {
                            parentTypeId = "ADDITIONAL_IMAGE_" + viewNumber;
                        }
                    }

                    pct.set("parentTypeId", parentTypeId);
                    pct.set("viewType", viewType);
                    pct.set("viewNumber", viewNumber);
                    pct.set("viewSize", viewSize);
                    if ("ORIGINAL_IMAGE_URL".equals(pctId)) {
                        pct.set("viewVariantId", "${VIEWSIZE}_IMAGE_URL");
                        pct.set("viewVariantDesc", "Image - ${viewSize}");
                    } else if (pctId.startsWith("ADDITIONAL_IMAGE_")) {
                        pct.set("viewVariantId", "XTRA_IMG_${VIEWNUMBER}_${VIEWSIZE}");
                        pct.set("viewVariantDesc", "Image - Additional View ${viewNumber} ${viewSize}");
                    }

                    pct.store();
                    updated++;
                    Debug.logInfo("productImageMigrateImageUrlProductContentTypeData: updated: " + pct, module);
                } else if (preview) {
                    Debug.logInfo("productImageMigrateImageUrlProductContentTypeData: skipped: " + pct, module);
                }
            }

            if (preview) {
                return ServiceUtil.returnError("preview - aborted with error - updated: " + updated);
            }
            return ServiceUtil.returnSuccess("updated: " + updated);
        } catch(Exception e) {
            Debug.logError(e, "productImageMigrateImageUrlProductContentTypeData: " + e.toString(), module);
            return ServiceUtil.returnError(e.toString());
        }
    }

    private static boolean isProductContentTypeImageUrlRecordComplete(GenericValue pct) {
        return pct.get("parentTypeId") != null && pct.get("viewType") != null && pct.get("viewNumber") != null && pct.get("viewSize") != null;
    }

    public static Map<String, Object> productImageOpRequest(ServiceContext ctx) throws ServiceValidationException {
        GenericValue opReq = ctx.attr("opReq");
        String serviceId = opReq.getString("serviceId");
        String mode = opReq.getString("mode");
        Map<String, Object> serviceArgs = opReq.getJsonMap("serviceArgsJson");

        Map<String, Object> servCtx = new HashMap<>(serviceArgs);
        Map<String, Object> servResult = null;
        try {
            if ("async".equals(mode) || "async-memory".equals(mode)) {
                ctx.dispatcher().runAsync(serviceId, servCtx, false);
            } else if ("async-persist".equals(mode)) {
                ctx.dispatcher().runAsync(serviceId, servCtx, true);
            } else {
                servResult = ctx.dispatcher().runSync(serviceId, servCtx);
                if (ServiceUtil.isError(servResult)) {
                    return ServiceUtil.returnResultSysFields(servResult);
                }
            }
        } catch(GenericServiceException e) {
            Debug.logError("productImageOpRequest: error running operation: " + e.toString(), module);
            return ServiceUtil.returnError("error running operation: " + e.toString());
        }

        if (Boolean.TRUE.equals(ctx.attr("deleteReq"))) {
            try {
                opReq.remove();
            } catch (GenericEntityException e) {
                Debug.logError("productImageOpRequest: error deleting: " + e.toString(), module);
                return ServiceUtil.returnFailure("error deleting: " + e.toString());
            }
        }
        return (servResult != null) ? ServiceUtil.returnResultSysFields(servResult) : ServiceUtil.returnSuccess();
    }
}

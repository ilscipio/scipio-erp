package com.ilscipio.scipio.category.image;

import com.ilscipio.scipio.content.image.ContentImageServices;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
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

/**
 * SCIPIO: New category image services, alternatives to {@link org.ofbiz.product.imagemanagement.ImageManagementServices}
 * and other utils.
 * TODO?: try to reconcile everything in the future, too difficult for now.
 */
public class CategoryImageServices {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    /**
     * SCIPIO: Updated scaling implementation based on ScaleImage.scaleImageInAllSize, but allowing greater parameterization.
     * Fully implements the categoryImageFileScaleInAllSize service, see its interface for parameters.
     *
     */
    public static Map<String, Object> categoryImageFileScaleInAllSize(ServiceContext ctx) throws ServiceValidationException {
        Delegator delegator = ctx.delegator();
        String productCategoryId = (String) ctx.get("productCategoryId");

        CategoryImageViewType imageViewType = ctx.attr("imageViewType");
        if (imageViewType != null) {
            if (!imageViewType.isOriginal()) {
                String errorMsg = "categoryImageFileScaleInAllSize: category [" + productCategoryId + "]: imageViewType is not 'original' viewSize";
                Debug.logError("categoryImageFileScaleInAllSize: " + errorMsg, module);
                return ServiceUtil.returnError(errorMsg);
            }
        } else {
            try {
                String viewType = ctx.getString("viewType");
                String viewNumber = ctx.getString("viewNumber", "main".equals(viewType) ? "0" : null);
                imageViewType = CategoryImageViewType.from(delegator, viewType, viewNumber, "original", true, true);
            } catch (Exception e) {
                Debug.logError(e, "categoryImageFileScaleInAllSize: category [" + productCategoryId + "]: error determining imageViewType for viewType [" +
                        ctx.getString("viewType") + "] viewNumber [" + ctx.getString("viewNumber") + "]: " + e.toString(), module);
                return ServiceUtil.returnError(e.toString());
            }
        }
        String categoryContentTypeId = imageViewType.getContentTypeId();

        Map<String, Object> contentCtx;
        try {
            contentCtx = ctx.makeValidInContext("contentImageFileScaleInAllSizeCore", ctx.context());
        } catch (GenericServiceException e) {
            return ServiceUtil.returnError(e.getMessage());
        }

        CategoryImageLocationInfo locInfo;
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
            locInfo = CategoryImageLocationInfo.from(ctx.dctx(), productCategoryId, imageViewType, (ImageProfile) null, imagePath, null,
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
            contentCtx.put("defaultImageProfile", CategoryImageWorker.getDefaultCategoryImageProfile(delegator, categoryContentTypeId, false, false));

            // TODO/FIXME: currently provides no deletion of the old images...

        } catch (Exception e) {
            Debug.logError(e, "categoryImageFileScaleInAllSize: category [" + productCategoryId + "]: error preparing context: " + e.toString(), module);
            return ServiceUtil.returnError(e.toString());
        }

        Map<String, Object> result = ContentImageServices.contentImageFileScaleInAllSizeCore(ctx.from(contentCtx));
        result.put("categorySizeTypeList", ScaleImage.sizeTypeList);
        return result;
    }

    public static Map<String, Object> categoryImageAutoRescale(ServiceContext ctx) throws ServiceValidationException { // SCIPIO
        boolean nonFatal = ctx.attr("nonFatal", false);

        // NOTE: CMS images are identified by contentTypeId="SCP_MEDIA"
        GenericValue productCategory = ctx.attr("productCategory");
        String productCategoryId;
        if (productCategory != null) {
            productCategoryId = productCategory.getString("productCategoryId");
        } else {
            productCategoryId = ctx.attr("productCategoryId");
            productCategory = ctx.delegator().from("ProductCategory").where("productCategoryId", productCategoryId).queryOneSafe();
            if (productCategory == null) {
                return ServiceUtil.returnError("Could not find category [" + productCategoryId + "]");
            }
        }

        boolean allImages = Boolean.TRUE.equals(ctx.attr("allImages"));

        List<String> categoryContentTypeIdList = ctx.attr("categoryContentTypeIdList", Collections.emptyList());
        List<String> contentIdList = ctx.attr("contentIdList", Collections.emptyList());
        String categoryContentTypeIdParam = ctx.attr("prodCatContentTypeId");
        if (categoryContentTypeIdParam != null) {
            categoryContentTypeIdList = new ArrayList<>(categoryContentTypeIdList);
            categoryContentTypeIdList.add(categoryContentTypeIdParam);
        }
        String contentIdParam = ctx.attr("contentId");
        if (contentIdParam != null) {
            contentIdList = new ArrayList<>(contentIdList);
            contentIdList.add(contentIdParam);
        }

        if (allImages) {
            try {
                categoryContentTypeIdList = new ArrayList<>(CategoryImageViewType.getOriginalViewSizeProductCategoryContentTypes(ctx.delegator(), true).keySet());
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.toString());
            }
        }

        boolean logDetail = ctx.attr("logDetail", false);
        if (logDetail || Debug.verboseOn()) {
            Debug.logInfo("categoryImageRescaleImage: category [" + productCategoryId + "]: processing"
                    + (UtilValidate.isNotEmpty(categoryContentTypeIdList) ? " productContentTypeIdList " + categoryContentTypeIdList : "")
                    + (UtilValidate.isNotEmpty(contentIdList) ? " contentIdList " + contentIdList : ""), module);
        }

        Map<String, String> imageOrigUrlMap = ctx.attr("imageOrigUrlMap", Collections::emptyMap);
        String imageOrigUrlParam = ctx.attr("imageOrigUrl");
        if (UtilValidate.isNotEmpty(imageOrigUrlParam) && UtilValidate.isNotEmpty(categoryContentTypeIdParam)) {
            imageOrigUrlMap = new HashMap<>(imageOrigUrlMap);
            imageOrigUrlMap.put(categoryContentTypeIdParam, imageOrigUrlParam);
        }
        boolean copyOrig = ctx.attr("copyOrig");

        Map<String, Object> stats = UtilMisc.put(new LinkedHashMap<>(), "successCount", 0, "failCount", 0, "errorCount", 0, "skipCount", 0,
                "variantSuccessCount", 0, "variantFailCount", 0);
        for (String prodCatContentTypeId : categoryContentTypeIdList) {
            String origImageUrl = imageOrigUrlMap.get(prodCatContentTypeId);
            Map<String, Object> res = categoryImageRescaleImage(ctx, productCategory, prodCatContentTypeId, null, nonFatal, origImageUrl, copyOrig);
            if ("no-image-url".equals(res.get("reason")) && "CATEGORY_IMAGE_URL".equals(prodCatContentTypeId)) {
                Map<String, Object> res2 = categoryImageRescaleImage(ctx, productCategory, "DETAIL_IMAGE_URL", null, nonFatal, origImageUrl, copyOrig);
                if (!"no-image-url".equals(res2.get("reason"))) {
                    res = res2;
                } else {
                    Map<String, Object> res3 = categoryImageRescaleImage(ctx, productCategory, "LARGE_IMAGE_URL", null, nonFatal, origImageUrl, copyOrig);
                    if (!"no-image-url".equals(res3.get("reason"))) {
                        res = res3;
                    }
                }
            }
            if (!categoryImageAutoRescaleRegisterResult(ctx, productCategory, res, stats, nonFatal)) {
                Map<String, Object> result = ServiceUtil.returnError(((int) stats.get("errorCount")) + " errors auto-rescaling images for category [" + productCategoryId + "]" + " (stats: " + stats + ")");
                result.putAll(stats);
                return result;
            }
        }
        for (String contentId : contentIdList) {
            Map<String, Object> res = categoryImageRescaleImage(ctx, productCategory, null, contentId, nonFatal, null, copyOrig);
            if (!categoryImageAutoRescaleRegisterResult(ctx, productCategory, res, stats, nonFatal)) {
                Map<String, Object> result = ServiceUtil.returnError(((int) stats.get("errorCount")) + " errors auto-rescaling images for category [" + productCategoryId + "]" + " (stats: " + stats + ")");
                result.putAll(stats);
                return result;
            }
        }

        if (Boolean.TRUE.equals(ctx.attr("clearCaches"))) {
            try {
                Map<String, Object> clearCachesCtx = ctx.makeValidInContext("categoryImageVariantsClearCaches", ctx.context());
                clearCachesCtx.put("productCategoryId", productCategoryId);
                clearCachesCtx.put("distribute", true);
                Map<String, Object> clearCachesResult = ctx.dispatcher().runSync("categoryImageVariantsClearCaches", clearCachesCtx);
                if (!ServiceUtil.isSuccess(clearCachesResult)) {
                    Debug.logWarning("categoryImageRescaleImage: category [" + productCategoryId + "]: error clearing caches: " + ServiceUtil.getErrorMessage(clearCachesResult), module);
                }
            } catch (GenericServiceException e) {
                Debug.logWarning(e, "categoryImageRescaleImage: category [" + productCategoryId + "]: error clearing caches: " + e.toString(), module);
            }
        }

        Map<String, Object> result;
        int successCount = ((int) stats.get("successCount"));
        int errorCount = ((int) stats.get("errorCount"));
        int failCount = ((int) stats.get("failCount"));

        String msg = "Rescaled " + successCount + " images for category [" + productCategoryId + "]" + " (stats: " + stats + ")";
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

    private static boolean categoryImageAutoRescaleRegisterResult(ServiceContext ctx, GenericValue productCategory, Map<String, Object> res, Map<String, Object> stats, boolean nonFatal) {
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

    private static Map<String, Object> categoryImageRescaleImage(ServiceContext ctx, GenericValue productCategory, String prodCatContentTypeId, String contentId, boolean nonFatal,
                                                                 String explOrigImageUrl, Boolean copyOrig) throws ServiceValidationException {
        int variantSuccessCount = 0;
        int variantFailCount = 0;
        boolean logDetail = ctx.attr("logDetail", false);
        String productCategoryId = productCategory.getString("productCategoryId");

        CategoryImageViewType imageViewType;
        CategoryImageViewType origImageViewType;
        try {
            imageViewType = CategoryImageViewType.from(ctx.delegator(), prodCatContentTypeId, true, true);
            // NOTE: because prodCatContentTypeId may be special exception values DETAIL_IMAGE_URL/LARGE_IMAGE_URL (instead of CATEGORY_IMAGE_URL),
            // some code needs the following instead
            origImageViewType = imageViewType.getOriginal(true);
        } catch (Exception e) {
            String errorMsg = "category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId + "]: could not determine image view type: " + e.toString();
            Debug.logError(e, "categoryImageRescaleImage: " + errorMsg, module);
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
                Debug.logError("categoryImageRescaleImage: category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId +
                        "]: Content not found for contentId [" + contentId + "]", module);
                return ServiceUtil.returnError("categoryImageRescaleImage [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId +
                        "]: Content not found for contentId [" + contentId + "]");
            }
        }

        if (UtilValidate.isEmpty(contentId)) {
            contentId = null;
        }
        if (UtilValidate.isEmpty(prodCatContentTypeId)) {
            prodCatContentTypeId = null;
        }
        if (contentId == null && prodCatContentTypeId == null) {
            String errMsg = "category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId + "]: missing contentId or prodCatContentTypeId";
            Debug.logError("categoryImageRescaleImage: " + errMsg, module);
            return ServiceUtil.returnError(errMsg);
        }
        GenericValue productCategoryContent = null;
        String productFieldName = null;
        if (contentId != null) {
            try {
                if (prodCatContentTypeId == null) {
                    productCategoryContent = ctx.delegator().from("ProductCategoryContent").where("productCategoryId", productCategoryId,
                            "contentId", contentId).orderBy("-fromDate").filterByDate().queryFirst();
                    if (productCategoryContent == null) {
                        return ServiceUtil.returnError("category [" + productCategoryId +
                                "] prodCatContentTypeId [" + prodCatContentTypeId + "]: could not find ProductCategoryContent for contentId [" + contentId + "]");
                    }
                    prodCatContentTypeId = productCategoryContent.getString("prodCatContentTypeId");
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.toString());
            }
        } else {
            try {
                productCategoryContent = ctx.delegator().from("ProductCategoryContent").where("productCategoryId", productCategoryId,
                        "prodCatContentTypeId", prodCatContentTypeId).orderBy("-fromDate").filterByDate().queryFirst();
            } catch (GenericEntityException e) {
                Debug.logError(e, module);
                return ServiceUtil.returnError(e.toString());
            }
            if (productCategoryContent != null) {
                contentId = productCategoryContent.getString("contentId");
                try {
                    content = productCategoryContent.getRelatedOne("Content");
                } catch (GenericEntityException e) {
                    Debug.logError(e, module);
                    return ServiceUtil.returnError(e.toString());
                }
            } else {
                productFieldName = ModelUtil.dbNameToVarName(prodCatContentTypeId);
                ModelEntity productModel = ctx.delegator().getModelEntity("ProductCategory");
                if (!productModel.isField(productFieldName)) {
                    String msg = "category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId +
                            "]: inapplicable prodCatContentTypeId for resize operation";
                    if (Debug.verboseOn()) { // typically not relevant: (logDetail || Debug.verboseOn())
                        Debug.logInfo("categoryImageRescaleImage: " + msg, module);
                    }
                    return UtilMisc.put(ServiceUtil.returnSuccess(msg), "reason", "inapplicable");
                }
            }
        }

        String origImageUrl = explOrigImageUrl;
        if (origImageUrl == null) {
            if (productCategoryContent != null) {
                try {
                    GenericValue dataResource = (content != null) ? content.getRelatedOne("DataResource") : null;
                    origImageUrl = com.ilscipio.scipio.category.image.CategoryImageWorker.getDataResourceImageUrl(dataResource, false);
                } catch (GenericEntityException e) {
                    Debug.logError(e, module);
                    return ServiceUtil.returnError(e.toString());
                }
            } else if (productFieldName != null) {
                origImageUrl = productCategory.getString(productFieldName);
            }
            if (UtilValidate.isEmpty(origImageUrl)) {
                String msg = "category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId + "] origImageUrl [" + origImageUrl + "]: no image URL, not resizing";
                if (logDetail || Debug.verboseOn()) {
                    Debug.logInfo("categoryImageRescaleImage: " + msg, module);
                }
                return UtilMisc.put(ServiceUtil.returnSuccess(msg), "reason", "no-image-url");
            }
        }

        ImageProfile imageProfile = com.ilscipio.scipio.category.image.CategoryImageWorker.getCategoryImageProfileOrDefault(ctx.delegator(),
                origImageViewType.getContentTypeId(), productCategory, content, false, false);
        if (imageProfile == null) {
            String errorMsg = "category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId +
                    "] origImageUrl [" + origImageUrl + "]: could not find media profile";
            Debug.logError("categoryImageRescaleImage: " + errorMsg, module);
            return ServiceUtil.returnError(errorMsg);
        }

        Collection<String> sizeTypeList = ctx.attr("sizeTypeList");
        boolean recreateExisting = ctx.attr("recreateExisting", false);
        if (!recreateExisting) {
            try {
                CategoryImageLocationInfo cili = CategoryImageLocationInfo.from(ctx.dctx(), ctx.locale(),
                        productCategory, imageViewType, origImageUrl, sizeTypeList, false, false, false, null);
                sizeTypeList = (cili != null) ? cili.getMissingVariantNames() : null;
                if (UtilValidate.isEmpty(sizeTypeList)) {
                    String msg = "category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId + "] origImageUrl [" + origImageUrl +
                            "]: no missing sizeTypes" + (sizeTypeList != null ? " for sizeTypeList [" + sizeTypeList + "]" : "") + "; not resizing";
                    if (logDetail || Debug.verboseOn()) {
                        Debug.logInfo("categoryImageRescaleImage: " + msg, module);
                    }
                    return UtilMisc.put(ServiceUtil.returnSuccess(msg), "reason", "all-exist");
                }
            } catch (Exception e) {
                String errorMsg = "category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId + "] origImageUrl ["
                        + origImageUrl + "]: could not determine image variants: " + e.toString();
                Debug.logError(e, "productImageRescaleImage: " + errorMsg, module);
                return ServiceUtil.returnError(errorMsg);
            }
        }

        Debug.logInfo("categoryImageRescaleImage: category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId
                + "] origImageUrl [" + origImageUrl + "]: begin scaling image variants", module);

        Map<String, Object> resizeCtx = UtilMisc.toMap("productCategoryId", productCategoryId, "imageOrigUrl", origImageUrl, "imageViewType", origImageViewType,
                "locale", ctx.get("locale"), "userLogin", ctx.get("userLogin"), "timeZone", ctx.get("timeZone"), "imageProfile", imageProfile,
                "sizeTypeList", sizeTypeList, "copyOrig", copyOrig);
        try {
            Map<String, Object> resizeResult;
            if (nonFatal) {
                resizeResult = ctx.dispatcher().runSync("categoryImageFileScaleInAllSize", resizeCtx, true);
            } else {
                resizeResult = ctx.dispatcher().runSync("categoryImageFileScaleInAllSize", resizeCtx);
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
                String errMsg = "category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId + "] origImageUrl [" + origImageUrl +
                        "]: could not resize image: " + ServiceUtil.getErrorMessage(resizeResult);
                Debug.logError("categoryImageRescaleImage: " + errMsg, module);
                return UtilMisc.put(ServiceUtil.returnError(errMsg));
            } else if (ServiceUtil.isFailure(resizeResult)) {
                String errMsg = "category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId + "] origImageUrl [" + origImageUrl +
                        "]: could not resize image, non-fatal: " + ServiceUtil.getErrorMessage(resizeResult);
                Debug.logError("categoryImageRescaleImage: " + errMsg, module);
            }

            Map<String, Map<String, Object>> imageInfoMap = UtilGenerics.cast(resizeResult.get("imageInfoMap"));
            if (UtilValidate.isNotEmpty(imageInfoMap)) {
                Timestamp fromDate = UtilDateTime.nowTimestamp();
                for (Map.Entry<String, Map<String, Object>> entry : imageInfoMap.entrySet()) {
                    String sizeType = entry.getKey();
                    Map<String, Object> sizeTypeInfo = entry.getValue();
                    String imageUrl = (String) sizeTypeInfo.get("url");
                    CategoryImageViewType scaledImageViewType = CategoryImageViewType.from(ctx.delegator(),
                            imageViewType.getViewType(), imageViewType.getViewNumber(), sizeType, true, true); //ctx.attr("createSizeTypeContent")
                    if (imageUrl != null && (imageUrl.startsWith(".") || imageUrl.contains("/."))) { // SPECIAL: detect bug (missing filename)
                        throw new IllegalStateException("internal or data error: invalid url [" + imageUrl + "] for sizeType [" + sizeType + "], not updating");
                    }
                    Map<String, Object> res = updateCategoryContentImageUrl(ctx, productCategory, scaledImageViewType.getContentTypeId(), imageUrl,
                            origImageUrl, prodCatContentTypeId, fromDate, ctx.attr("createSizeTypeContent"), sizeTypeInfo);
                    if (ServiceUtil.isError(res)) {
                        return UtilMisc.put(new HashMap<>(res));
                    }
                }
            }
        } catch (Exception e) {
            Debug.logError(e, "categoryImageRescaleImage: category [" + productCategoryId + "] prodCatContentTypeId [" + prodCatContentTypeId +
                    "] origImageUrl [" + origImageUrl + "]: error resizing images: " + e.toString(), module);
            return UtilMisc.put(ServiceUtil.returnError(e.toString()));
        }
        String msg = variantSuccessCount + " variant success, " + variantFailCount + " variant failures";
        return UtilMisc.put(variantFailCount > 0 ? ServiceUtil.returnFailure(msg) : ServiceUtil.returnSuccess(msg), "reason", "success",
                "variantSuccessCount", variantSuccessCount, "variantFailCount", variantFailCount);
    }

    /**
     * Attempts to preserve previous data setup, best-effort.
     */
    public static Map<String, Object> updateCategoryContentImageUrl(ServiceContext ctx, GenericValue productCategory, String prodCatContentTypeId, String imageUrl, String origImageUrl,
                                                                    String origCategoryContentTypeId, Timestamp fromDate, Boolean createSizeTypeContent,
                                                                    Map<String, Object> sizeTypeInfo) {
        GenericValue productCategoryContent;
        try {
            productCategoryContent = ctx.delegator().from("ProductCategoryContent").where("productCategoryId", productCategory.get("productCategoryId"),
                    "prodCatContentTypeId", prodCatContentTypeId).orderBy("-fromDate").filterByDate().queryFirst();
        } catch (GenericEntityException e) {
            String errMsg = "category [" + productCategory.get("productCategoryId") + "] prodCatContentTypeId [" +
                    prodCatContentTypeId + "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: error: " + e.toString();
            Debug.logError("updateCategoryContentImageUrl: " + errMsg, module);
            return ServiceUtil.returnError(errMsg);
        }
        try {
            if (productCategoryContent != null) {
                GenericValue content = productCategoryContent.getRelatedOne("Content");
                if (content == null) {
                    String errMsg = "category [" + productCategory.get("productCategoryId") + "] prodCatContentTypeId [" +
                            prodCatContentTypeId + "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: could not find Content record for contentId [" + productCategoryContent.get("contentId") + "]";
                    Debug.logError("updateCategoryContentImageUrl: " + errMsg, module);
                    return ServiceUtil.returnError(errMsg);
                }
                GenericValue dataResource = content.getRelatedOne("DataResource");
                if (dataResource == null) {
                    String errMsg = "category [" + productCategory.get("productCategoryId") + "] prodCatContentTypeId [" +
                            prodCatContentTypeId + "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: could not find image DataResource record";
                    Debug.logError("updateCategoryContentImageUrl: " + errMsg, module);
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

                Integer heightInt = (Integer) sizeTypeInfo.get("height");
                Long height = (heightInt != null) ? heightInt.longValue() : null;
                Long prevHeight = dataResource.getLong("scpHeight");
                if (!Objects.equals(height, prevHeight)) {
                    dataResource.set("scpHeight", height);
                    drModified = true;
                }

                if (!"CATEGORY_IMAGE_URL".equals(prodCatContentTypeId)) {
                    Map<String, Object> prevJsonMap = dataResource.getJsonMap("srcPresetJson");
                    if (!prevJsonMap.isEmpty()) {
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
                            Debug.logInfo("updateCategoryContentImageUrl: category [" + productCategory.get("productCategoryId") + "] prodCatContentTypeId [" + prodCatContentTypeId +
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
                            String msg = "category [" + productCategory.get("productCategoryId") + "] prodCatContentTypeId [" + prodCatContentTypeId +
                                    "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: could not find image ElectronicText: unexpected ProductContent format";
                            Debug.logWarning("updateCategoryContentImageUrl: " + msg, module);
                            return ServiceUtil.returnFailure(msg);
                        }
                        String prevImageUrl = elecText.getString("textData");
                        if (!imageUrl.equals(prevImageUrl)) {
                            Debug.logInfo("updateCategoryContentImageUrl: category [" + productCategory.get("productCategoryId") + "] prodCatContentTypeId [" +
                                    prodCatContentTypeId + "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl +
                                    "]: updating ElectronicText imageUrl from [" + prevImageUrl + "] to [" + imageUrl + "]", module);
                            elecText.set("textData", imageUrl);
                            elecText.store();
                        }
                    } else {
                        String errMsg = "category [" + productCategory.get("productCategoryId") + "] prodCatContentTypeId [" + prodCatContentTypeId +
                                "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: invalid image DataResource record";
                        Debug.logError("updateCategoryContentImageUrl: " + errMsg, module);
                        return ServiceUtil.returnError(errMsg);
                    }
                }
                if (drModified) {
                    dataResource.store();
                }
            } else {
                // Check field
                String productCategoryFieldName = ModelUtil.dbNameToVarName(prodCatContentTypeId);
                ModelEntity productCategoryModel = ctx.delegator().getModelEntity("ProductCategory");
                if (productCategoryModel.isField(productCategoryFieldName)) {
                    String prevImageUrl = productCategory.getString(productCategoryFieldName);
                    if (imageUrl != null && !imageUrl.equals(prevImageUrl)) {
                        Debug.logInfo("updateCategoryContentImageUrl: category [" + productCategory.get("productCategoryId") + "] prodCatContentTypeId [" + prodCatContentTypeId +
                                "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: updating ProductCategory field [" + productCategoryFieldName +
                                "] imageUrl from [" + prevImageUrl + "] to [" + imageUrl + "]", module);
                        productCategory.set(productCategoryFieldName, imageUrl);
                        productCategory.store();
                    }

                    if (productCategoryFieldName.endsWith("Url")) {
                        String fieldPrefix = productCategoryFieldName.substring(0, productCategoryFieldName.length() - "Url".length());
                        GenericValue details = ctx.delegator().findOne("CategoryMediaDetails",
                                UtilMisc.toMap("productCategoryId", productCategory.get("productCategoryId")), false);
                        boolean detailsCreated = (details == null);
                        if (detailsCreated) {
                            details = ctx.delegator().makeValue("CategoryMediaDetails", "productCategoryId", productCategory.get("productCategoryId"));
                        }
                        boolean detailsModified = detailsCreated;

                        Integer widthInt = (Integer) sizeTypeInfo.get("width");
                        Long width = (widthInt != null) ? widthInt.longValue() : null;
                        Long prevWidth = details.getLong(fieldPrefix + "Width");
                        if (!Objects.equals(width, prevWidth)) {
                            details.put(fieldPrefix + "Width", width);
                            detailsModified = true;
                        }

                        Integer heightInt = (Integer) sizeTypeInfo.get("height");
                        Long height = (heightInt != null) ? heightInt.longValue() : null;
                        Long prevHeight = details.getLong(fieldPrefix + "Height");
                        if (!Objects.equals(height, prevHeight)) {
                            details.put(fieldPrefix + "Height", height);
                            detailsModified = true;
                        }

                        if (!"CATEGORY_IMAGE_URL".equals(prodCatContentTypeId)) {
                            Map<String, Object> prevJsonMap = details.getJsonMap(fieldPrefix + "PresetJson");
                            if (!prevJsonMap.isEmpty()) {
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
                        Debug.logWarning("updateCategoryContentImageUrl: category [" + productCategory.get("productCategoryId") + "] prodCatContentTypeId [" + prodCatContentTypeId +
                                "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: unrecognized Product field name [" + productCategoryFieldName + "]; cannot store InlineProductImageDetails for image field; width/height/preset will not be saved", module);
                    }
                } else if (Boolean.TRUE.equals(createSizeTypeContent) && !"CATEGORY_IMAGE_URL".equals(prodCatContentTypeId)) {
                    // Try to find a ProductContent record to refer to
                    GenericValue origProductCategoryContent = ctx.delegator().from("ProductCategoryContentAndInfo").where(
                            "productCategoryId", productCategory.get("productCategoryId"), "prodCatContentTypeId", origCategoryContentTypeId).orderBy("-fromDate").filterByDate().queryFirst();
                    if (origProductCategoryContent == null) {
                        if (!"CATEGORY_IMAGE_URL".equals(origCategoryContentTypeId)) {
                            origProductCategoryContent = ctx.delegator().from("ProductCategoryContentAndInfo").where(
                                    "productCategoryId", productCategory.get("productCategoryId"), "prodCatContentTypeId", "CATEGORY_IMAGE_URL").orderBy("-fromDate").filterByDate().queryFirst();
                        }
                    }
                    Debug.logInfo("updateCategoryContentImageUrl: category [" + productCategory.get("productCategoryId")
                            + "] prodCatContentTypeId [" + prodCatContentTypeId + "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl +
                            "]: no existing record: creating new DataResource/Content/ProductCategoryContent" +
                            " using reference prodCatContentTypeId [" + (origProductCategoryContent != null ? origProductCategoryContent.get("prodCatContentTypeId") : "(none)") + "]", module);
                    String dataResourceName = (origProductCategoryContent != null) ? origProductCategoryContent.getString("drDataResourceName") : null;
                    if (dataResourceName == null) {
                        int lastSlash = imageUrl.lastIndexOf('/');
                        dataResourceName = (lastSlash >= 0) ? imageUrl.substring(lastSlash + 1) : imageUrl;
                    }
                    String statusId = (origProductCategoryContent != null) ? origProductCategoryContent.getString("drStatusId") : "CTNT_IN_PROGRESS";

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

                    statusId = (origProductCategoryContent != null) ? origProductCategoryContent.getString("statusId") : "CTNT_IN_PROGRESS";
                    GenericValue content = ctx.delegator().makeValue("Content", "dataResourceId", dataResource.get("dataResourceId"),
                            "statusId", statusId, "contentTypeId", "DOCUMENT");
                    content = content.createSetNextSeqId();
                    productCategoryContent = ctx.delegator().makeValue("ProductCategoryContent", "productCategoryId", productCategory.get("productCategoryId"),
                            "contentId", content.get("contentId"), "prodCatContentTypeId", prodCatContentTypeId, "fromDate", fromDate).create();
                }
            }
        } catch (GenericEntityException e) {
            String errMsg = "category [" + productCategory.get("productCategoryId") + "] prodCatContentTypeId [" +
                    prodCatContentTypeId + "] origImageUrl [" + origImageUrl + "] imageUrl [" + imageUrl + "]: error: " + e.toString();
             Debug.logError("updateCategoryContentImageUrl: " + errMsg, module);
            return ServiceUtil.returnError(errMsg);
        }
        return ServiceUtil.returnSuccessReadOnly();
    }

    private static boolean isStrArgEmpty(Map<String, ?> context, String argName) {
        return UtilValidate.isEmpty((String) context.get(argName));
    }

    public static Map<String, Object> categoryImageAutoRescaleCategories(ServiceContext ctx, boolean logFinal) throws ServiceValidationException {
        String logPrefix = ctx.getModelService().name + ": ";
        int productCategoryCount = 0;
        int successCount = 0;
        int errorCount = 0;
        int failCount = 0;
        int skipCount = 0;

        Iterator<?> productCategoriesIt = (ctx.attr("productCategories") != null) ? UtilMisc.asIterator(ctx.attr("productCategories")) : null;
        if (productCategoriesIt == null) {
            productCategoriesIt = (ctx.attr("productCategoryIdList") != null) ? UtilMisc.asIterator(ctx.attr("productCategoryIdList")) : null;
        }
        // FIXME: This doesn't exist, not even in the equivalent (original) Product service
        int lastProductCategoryCount = ctx.attr("lastProductCategoryCount", 10);
        List<String> lastProductCategoryIdList = new LinkedList<>();
        List<String> failProductCategoryIdList = new ArrayList<>();
        try {
            Integer maxProductCategories = ctx.attr("maxProductCategories");
            Integer maxErrorCount = ctx.attr("maxErrorCount");
            boolean sepProductCategoryTrans = ctx.attr("sepProductCategoryTrans", true);
            Integer logBatch = ctx.attr("logBatch");
            if (logBatch != null && logBatch <= 0) {
                logBatch = null;
            }
            if (productCategoriesIt == null) {
                if (Boolean.TRUE.equals(ctx.attr("allProductCategories"))) {
                    try {
                        productCategoriesIt = ctx.delegator().from("ProductCategory").where((EntityCondition) ctx.attr("allCond"))
                                .orderBy(ctx.<List<String>>attr("allOrderBy")).getFieldList("productCategoryId").iterator();
                    } catch (GenericEntityException e) {
                        Debug.logError(e, module);
                        return ServiceUtil.returnError(e.toString());
                    }
                } else {
                    throw new ServiceValidationException("Missing productCategories list/iterator or allProductCategories flag", ctx.getModelService());
                }
            }

            String allResumeId = ctx.attr("allResumeId");
            int resumeSkipped = 0;

            Object productCategoryObj;
            while ((productCategoryObj = UtilMisc.next(productCategoriesIt)) != null) {
                GenericValue productCategory = (productCategoryObj instanceof GenericValue) ? (GenericValue) productCategoryObj : null;
                String productCategoryId = (productCategory != null) ? productCategory.getString("productCategoryId") : (String) productCategoryObj;
                if (allResumeId != null) {
                    if (allResumeId.equals(productCategoryId)) {
                        Debug.logInfo(logPrefix + "Resuming from category: " + productCategoryId + "(skipped: " + resumeSkipped + ")", module);
                        allResumeId = null;
                    } else {
                        resumeSkipped++;
                        continue;
                    }
                }
                if (logBatch != null) {
                    if ((productCategoryCount % logBatch) == 0) {
                        Debug.logInfo(logPrefix + "Processing category " + ((productCategoryCount + 1)) + " [" + productCategoryId + "] (last: " + lastProductCategoryIdList + ")", module);
                    }
                    while (lastProductCategoryIdList.size() >= lastProductCategoryCount) {
                        lastProductCategoryIdList.remove(0);
                    }
                    lastProductCategoryIdList.add(productCategoryId);
                }

                productCategoryCount++;
                try {
                    Map<String, Object> servCtx = ctx.makeValidInContext("categoryImageAutoRescale", ctx);
                    if (productCategory != null) {
                        servCtx.put("productCategoryId", productCategory.get("productCategoryId"));
                    } else if (UtilValidate.isNotEmpty(productCategoryId)) {
                        servCtx.put("productCategoryId", productCategoryObj);
                    } else {
                        throw new ServiceValidationException("Invalid category, should be GenericValue or String: " + productCategoryObj.getClass(), ctx.getModelService());
                    }
                    servCtx.put("nonFatal", true); // TODO: unhardcode (NOTE: causes extra separate transactions per-image)
                    Map<String, Object> servResult = ctx.dispatcher().runSync("categoryImageAutoRescale", servCtx, sepProductCategoryTrans);
                    if (!ServiceUtil.isSuccess(servResult)) {
                        failProductCategoryIdList.add(productCategoryId);
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
                } catch (GenericServiceException e) {
                    Debug.logError(e, logPrefix + e.toString(), module);
                    errorCount++;
                }
                if (maxErrorCount != null && errorCount >= maxErrorCount) {
                    Debug.logError(logPrefix + "max errors reached (" + maxErrorCount + ")", module);
                    break;
                }
                if (maxProductCategories != null && productCategoryCount >= maxProductCategories) {
                    Debug.logInfo(logPrefix + "max categories reached (" + maxProductCategories + ")", module);
                    break;
                }
            }
            if (allResumeId != null) {
                Debug.logWarning(logPrefix + "Did not reach allResumeId category [" + allResumeId + "]; either no categories or wrong productCategoryId", module);
            }
        } finally {
            if (productCategoriesIt instanceof AutoCloseable) {
                try {
                    ((AutoCloseable) productCategoriesIt).close();
                } catch (Exception e) {
                    Debug.logError(e, module);
                }
            }
        }
        Map<String, Object> stats = UtilMisc.put(new LinkedHashMap<>(), "successCount", successCount, "failCount", failCount, "errorCount", errorCount,
                "skipCount", skipCount);
        String failProductsStr = " (failed categories: " + failProductCategoryIdList + ")";
        if (logFinal && Debug.infoOn()) {
            if (errorCount > 0) {
                Debug.logError(logPrefix + errorCount + " errors processing category images (" + productCategoryCount + " categories)"
                        + " (last " + lastProductCategoryIdList.size() + " categories: " + lastProductCategoryIdList + ")" + " (stats: " + stats + ")" + failProductsStr, module);
            } else {
                Debug.logInfo(logPrefix + "Processed categories images (" + productCategoryCount + " categories)" + " (last "
                        + lastProductCategoryIdList.size() + " categories: " + lastProductCategoryIdList + ")" + " (stats: " + stats + ")" + failProductsStr, module);
            }
        }
        Map<String, Object> result = (errorCount > 0) ?
                ServiceUtil.returnFailure(errorCount + " errors processing category images (" + productCategoryCount + " categories)" +
                        " (last " + lastProductCategoryIdList.size() + " categories: " + lastProductCategoryIdList + ")" + failProductsStr) :
                ServiceUtil.returnSuccess("Processed categories images (" + productCategoryCount + " categories)" +
                        " (last " + lastProductCategoryIdList.size() + " categories: " + lastProductCategoryIdList + ")" + failProductsStr);
        result.putAll(stats);
        result.put("failProductIdList", failProductCategoryIdList);
        return result;
    }

    public static Map<String, Object> categoryImageAutoRescaleCategories(ServiceContext ctx) throws ServiceValidationException {
        return categoryImageAutoRescaleCategories(ctx, false);
    }

    public static Map<String, Object> categoryImageAutoRescaleAll(ServiceContext ctx) throws ServiceValidationException {
        return categoryImageAutoRescaleCategories(ctx, true);
    }

    public static Map<String, Object> abortCategoryImageAutoRescaleAll(ServiceContext ctx) {
        return ServiceUtil.returnSuccess();
    }

    public static Map<String, Object> categoryImageMigrateImageUrlCategoryContentTypeData(ServiceContext ctx) throws ServiceValidationException {
        try {
            boolean forceAll = Boolean.TRUE.equals(ctx.attr("forceAll"));
            boolean force = Boolean.TRUE.equals(ctx.attr("force")) || forceAll;
            boolean preview = Boolean.TRUE.equals(ctx.attr("preview"));
            if (!force) {
                GenericValue origPct = ctx.delegator().from("ProductCategoryContentType").where("prodCatContentTypeId", "CATEGORY_IMAGE_URL").queryOne();
                if (origPct == null) {
                    Debug.logError("categoryImageMigrateImageUrlCategoryContentTypeData: Missing CATEGORY_IMAGE_URL", module);
                    return ServiceUtil.returnError("Missing CATEGORY_IMAGE_URL");
                }
                if (isCategoryContentTypeImageUrlRecordComplete(origPct)) {
                    Debug.logInfo("categoryImageMigrateImageUrlCategoryContentTypeData: CATEGORY_IMAGE_URL populated, not running", module);
                    return ServiceUtil.returnSuccess("CATEGORY_IMAGE_URL populated, not running");
                }
            }

            int updated = 0;

            GenericValue baseImagePct = ctx.delegator().findOne("ProductCategoryContentType", UtilMisc.toMap("prodCatContentTypeId", "IMAGE_URL_BASE"), false);
            if (baseImagePct == null) {
                baseImagePct = ctx.delegator().makeValue("ProductCategoryContentType", "prodCatContentTypeId", "IMAGE_URL_BASE",
                        "hasTable", "N", "description", "Image - Base").create();
                updated++;
                Debug.logInfo("categoryImageMigrateImageUrlCategoryContentTypeData: created: " + baseImagePct, module);
            }
            GenericValue fullImagePct = ctx.delegator().findOne("ProductCategoryContentType", UtilMisc.toMap("prodCatContentTypeId", "IMAGE_URL_FULL"), false);
            if (fullImagePct == null) {
                fullImagePct = ctx.delegator().makeValue("ProductCategoryContentType", "prodCatContentTypeId", "IMAGE_URL_FULL",
                        "hasTable", "N", "description", "Image - Full", "parentTypeId", "IMAGE_URL_BASE").create();
                updated++;
                Debug.logInfo("categoryImageMigrateImageUrlCategoryContentTypeData: created: " + fullImagePct, module);
            }

            List<EntityCondition> orCondList = new ArrayList<>();
            orCondList.add(EntityCondition.makeCondition("prodCatContentTypeId", EntityOperator.LIKE, "%_IMAGE_URL"));
//            orCondList.add(EntityCondition.makeCondition("prodCatContentTypeId", EntityOperator.LIKE, "ADDITIONAL_IMAGE_%"));
//            orCondList.add(EntityCondition.makeCondition("prodCatContentTypeId", EntityOperator.LIKE, "XTRA_IMG_%"));

            List<GenericValue> pccts = ctx.delegator().from("ProductCategoryContentType")
                    .where(EntityCondition.makeCondition(orCondList, EntityOperator.OR)).queryList();
            for (GenericValue pcct : pccts) {
                if (forceAll || !isCategoryContentTypeImageUrlRecordComplete(pcct)) {
                    String pcctId = pcct.getString("prodCatContentTypeId");
                    String viewType = CategoryImageViewType.extractProductCategoryContentTypeIdViewType(ctx.delegator(), pcctId);
                    String viewNumber = CategoryImageViewType.extractProductCategoryContentTypeIdViewNumber(ctx.delegator(), pcctId);
                    String viewSize = CategoryImageViewType.extractProductContentTypeIdViewSize(ctx.delegator(), pcctId);
                    if ("L".equals(viewNumber)) {
                        continue;
                    }

                    String parentTypeId;
                    if ("original".equals(viewSize)) {
                        parentTypeId = "IMAGE_URL_FULL";
                    } else {
                        if ("main".equals(viewType)) {
                            parentTypeId = "CATEGORY_IMAGE_URL";
                        } else {
                            parentTypeId = "ADDITIONAL_IMAGE_" + viewNumber;
                        }
                    }

                    pcct.set("parentTypeId", parentTypeId);
                    pcct.set("viewType", viewType);
                    pcct.set("viewNumber", viewNumber);
                    pcct.set("viewSize", viewSize);
                    if ("CATEGORY_IMAGE_URL".equals(pcctId)) {
                        pcct.set("viewVariantId", "${VIEWSIZE}_IMAGE_URL");
                        pcct.set("viewVariantDesc", "Image - ${viewSize}");
                    } else if (pcctId.startsWith("ADDITIONAL_IMAGE_")) {
                        pcct.set("viewVariantId", "XTRA_IMG_${VIEWNUMBER}_${VIEWSIZE}");
                        pcct.set("viewVariantDesc", "Image - Additional View ${viewNumber} ${viewSize}");
                    }

                    pcct.store();
                    updated++;
                    Debug.logInfo("categoryImageMigrateImageUrlCategoryContentTypeData: updated: " + pcct, module);
                } else if (preview) {
                    Debug.logInfo("categoryImageMigrateImageUrlCategoryContentTypeData: skipped: " + pcct, module);
                }
            }

            if (preview) {
                return ServiceUtil.returnError("preview - aborted with error - updated: " + updated);
            }
            return ServiceUtil.returnSuccess("updated: " + updated);
        } catch (Exception e) {
            Debug.logError(e, "categoryImageMigrateImageUrlCategoryContentTypeData: " + e.toString(), module);
            return ServiceUtil.returnError(e.toString());
        }
    }

    private static boolean isCategoryContentTypeImageUrlRecordComplete(GenericValue pcct) {
        return pcct.get("parentTypeId") != null && pcct.get("viewType") != null && pcct.get("viewNumber") != null && pcct.get("viewSize") != null;
    }

    public static Map<String, Object> categoryImageOpRequest(ServiceContext ctx) throws ServiceValidationException {
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
        } catch (GenericServiceException e) {
            Debug.logError("categoryImageOpRequest: error running operation: " + e.toString(), module);
            return ServiceUtil.returnError("error running operation: " + e.toString());
        }

        if (Boolean.TRUE.equals(ctx.attr("deleteReq"))) {
            try {
                opReq.remove();
            } catch (GenericEntityException e) {
                Debug.logError("categoryImageOpRequest: error deleting: " + e.toString(), module);
                return ServiceUtil.returnFailure("error deleting: " + e.toString());
            }
        }
        return (servResult != null) ? ServiceUtil.returnResultSysFields(servResult) : ServiceUtil.returnSuccess();
    }
}

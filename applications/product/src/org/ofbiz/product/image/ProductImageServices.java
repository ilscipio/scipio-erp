package org.ofbiz.product.image;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.ImageVariantConfig;
import org.ofbiz.content.image.ContentImageServices;
import org.ofbiz.content.image.ContentImageWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
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
import java.util.Collection;
import java.util.HashMap;
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
        String productId = ctx.attr("productId");
        GenericValue product = ctx.delegator().from("Product").where("productId", productId).queryOneSafe();
        if (product == null) {
            return ServiceUtil.returnError("Could not find product [" + productId + "]");
        }

        String productContentTypeId = null;
        GenericValue content = ctx.attr("content");
        String contentId;
        if (content != null) {
            if (!content.isMutable()) {
                throw new ServiceValidationException("content value is cached or read-only", ctx.getModelService());
            }
            contentId = content.getString("contentId");
        } else {
            contentId = ctx.attr("contentId");
            if (UtilValidate.isNotEmpty(contentId)) {
                try {
                    content = ctx.delegator().from("Content").where("contentId", contentId).queryOne();
                } catch (GenericEntityException e) {
                    Debug.logError(e, module);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (content == null) {
                    Debug.logError("Content not found for contentId [" + contentId + "]", module);
                    return ServiceUtil.returnError("Content not found for contentId [" + contentId + "]");
                }
            } else {
                productContentTypeId = ctx.attr("productContentTypeId");
            }
        }
        if (UtilValidate.isEmpty(contentId)) {
            contentId = null;
        }
        if (UtilValidate.isEmpty(productContentTypeId)) {
            productContentTypeId = null;
        }
        if (contentId == null && productContentTypeId == null) {
            throw new ServiceValidationException("Missing contentId or productContentTypeId", ctx.getModelService());
        }
        GenericValue productContent = null;
        if (contentId != null) {
            try {
                if (productContentTypeId == null) {
                    productContent = ctx.delegator().from("ProductContent").where("productId", productId,
                            "contentId", contentId).orderBy("-fromDate").filterByDate().queryFirst();
                    if (productContent == null) {
                        return ServiceUtil.returnError("Could not find ProductContent with productId [" + productId + "] contentId [" + contentId + "]");
                    }
                }
                productContentTypeId = productContent.getString("productContentTypeId");
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
                String productFieldName = ModelUtil.dbNameToVarName(productContentTypeId);
                ModelEntity productModel = ctx.delegator().getModelEntity("Product");
                if (!productModel.isField(productFieldName)) {
                    return ServiceUtil.returnError("Invalid productContentTypeId [" + productContentTypeId
                            + "] for product [" + productId + "] for resize operation (parent products not consulted)");
                }
            }
        }

        // NOTE: this consults the parent product which we don't want, but in known cases should return right value
        String imageUrl = ProductContentWrapper.getProductContentAsText(product, productContentTypeId,
                ctx.attr("locale"), ctx.dispatcher(), false, "raw");
        if (UtilValidate.isEmpty(imageUrl)) {
            String errMsg = "Could not get existing image URL for product [" + productId + "] productContentTypeId [" + productContentTypeId + "], resize impossible";
            Debug.logError(errMsg, module);
            return ServiceUtil.returnError(errMsg);
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

        // TODO: not yet supported here, file-based only
        //GenericValue imageSizePreset;
        //try {
        //    imageSizePreset = ctx.getDelegator().from("ImageSizePreset").where("presetId", mediaProfile).cache().queryOne();
        //    if ()
        //} catch (GenericEntityException e) {
        //    Debug.logError(e, module);
        //    return ServiceUtil.returnError(e.toString());
        //}

        ImageProfile imageProfile = ImageProfile.getImageProfile(ctx.delegator(), mediaProfileName);
        if (imageProfile == null) {
            Debug.logError("Could not find media profile [" + imageProfile + "]", module);
            return ServiceUtil.returnError("Could not find media profile [" + imageProfile + "]");
        }

        ProductImageWorker.ImageViewType imageViewType;
        try {
            imageViewType = ProductImageWorker.ImageViewType.from(productContentTypeId);
        } catch(Exception e) {
            Debug.logError(e,"Could not determine image view type from product [" + productId + "]: " + e.toString(), module);
            return null;
        }
        String viewType = imageViewType.getViewType();
        String viewNumber = imageViewType.getViewNumber();

        Map<String, Object> resizeCtx = UtilMisc.toMap("productId", productId, "imageOrigUrl", imageUrl, "viewType", viewType, "viewNumber", viewNumber,
                "locale", ctx.get("locale"), "userLogin", ctx.get("userLogin"), "timeZone", ctx.get("timeZone"), "imageProfile", imageProfile,
                "sizeTypeList", ctx.get("sizeTypeList"));
        try {
            Map<String, Object> resizeResult = productImageFileScaleInAllSize(ctx.from(resizeCtx));
            if (!ServiceUtil.isSuccess(resizeResult)) {
                String errMsg = "Could not resize image for product [" + productId + "] contentId [" + contentId
                        + "] productContentTypeId [" + productContentTypeId + "]";
                Debug.logError(errMsg, module);
                return ServiceUtil.returnError(errMsg);
            }
        } catch (Exception e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }

        // TODO: If ProductContent existed for the original record, should create any missing ProductContent records for small/medium/large/detail
        // TODO: REVIEW: This TODO might not apply anymore, depends on future...

        return ServiceUtil.returnSuccess();
    }

    private static boolean isStrArgEmpty(Map<String, ?> context, String argName) {
        return UtilValidate.isEmpty((String) context.get(argName));
    }

    /** Returns map of missing variants, mapping size name to missing expected image URL. */
    public static Map<String, String> getProductImageMissingVariantSizeTypes(DispatchContext dctx, Locale locale, GenericValue product, String productContentTypeId,
                                                                             GenericValue productContent, String imageUrl, boolean useCache) {
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

        Collection<String> sizeTypeList;
        try {
            imageServerPath = FlexibleLocation.resolveFileUrlAsPathIfUrl(imageServerPath, imageServerPath);

            /* ImageProperties.xml */
            ImageVariantConfig imgPropCfg = imageProfile.getVariantConfig();
            sizeTypeList = imgPropCfg.getVariantNames();

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
}

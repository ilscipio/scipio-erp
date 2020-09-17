package org.ofbiz.product.image;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.ImageProfile;
import org.ofbiz.common.image.MediaProfile;
import org.ofbiz.content.image.ContentImageServices;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.model.ModelEntity;
import org.ofbiz.entity.model.ModelUtil;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.product.product.ProductContentWrapper;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.service.ServiceValidationException;

/**
 * SCIPIO: New product image services, alternatives to {@link org.ofbiz.product.imagemanagement.ImageManagementServices}
 * and other utils.
 * TODO?: try to reconcile everything in the future, too difficult for now.
 * Added 2017-07-05.
 */
public abstract class ProductImageServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String resource = "ProductErrorUiLabels";

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
    public static Map<String, Object> productImageFileScaleInAllSize(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        String viewType = (String) context.get("viewType");
        Integer viewNumber = (Integer) context.get("viewNumber");
        Locale locale = (Locale) context.get("locale");
        if (locale == null) locale = Locale.getDefault();

        Map<String, Object> contentCtx;
        try {
            contentCtx = dctx.makeValidContext("contentImageFileScaleInAllSizeCore", ModelService.IN_PARAM, context);
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
        } else if (viewType.toLowerCase().contains("additional") && viewNumber != null && viewNumber != 0) {
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
            return ServiceUtil.returnError(UtilProperties.getMessage(resource, "ProductImageViewType", UtilMisc.toMap("viewType", type), locale));
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

        Map<String, Object> result = ContentImageServices.contentImageFileScaleInAllSizeCore(dctx, contentCtx);
        result.put("productSizeTypeList", ScaleImage.sizeTypeList);
        return result;
    }

    public static Map<String, Object> productImageAutoRescale(DispatchContext dctx, Map<String, ?> context) throws ServiceValidationException { // SCIPIO
        // NOTE: CMS images are identified byt contentTypeId="SCP_MEDIA"
        ServiceContext ctx = ServiceContext.from(dctx, context);

        String productId = ctx.getAttr("productId");
        GenericValue product = ctx.getDelegator().from("Product").where("productId", productId).queryOneSafe();
        if (product == null) {
            return ServiceUtil.returnError("Could not find product [" + productId + "]");
        }

        String productContentTypeId = null;
        GenericValue content = ctx.getAttr("content");
        String contentId;
        if (content != null) {
            if (!content.isMutable()) {
                throw new ServiceValidationException("content value is cached or read-only", ctx.getModelService());
            }
            contentId = content.getString("contentId");
        } else {
            contentId = ctx.getAttr("contentId");
            if (UtilValidate.isNotEmpty(contentId)) {
                try {
                    content = dctx.getDelegator().from("Content").where("contentId", contentId).queryOne();
                } catch (GenericEntityException e) {
                    Debug.logError(e, module);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (content == null) {
                    Debug.logError("Content not found for contentId [" + contentId + "]", module);
                    return ServiceUtil.returnError("Content not found for contentId [" + contentId + "]");
                }
            } else {
                productContentTypeId = ctx.getAttr("productContentTypeId");
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
                    productContent = ctx.getDelegator().from("ProductContent").where("productId", productId,
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
                productContent = ctx.getDelegator().from("ProductContent").where("productId", productId,
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
                ModelEntity productModel = dctx.getDelegator().getModelEntity("Product");
                if (!productModel.isField(productFieldName)) {
                    return ServiceUtil.returnError("Invalid productContentTypeId [" + productContentTypeId
                            + "] for product [" + productId + "] for resize operation (parent products not consulted)");
                }
            }
        }

        // NOTE: this consults the parent product which we don't want, but in known cases should return right value
        String imageUrl = ProductContentWrapper.getProductContentAsText(product, productContentTypeId,
                ctx.getAttr("locale"), ctx.getDispatcher(), false, "raw");
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

        ImageProfile imageProfile = ImageProfile.getImageProfile(ctx.getDelegator(), mediaProfileName);
        if (imageProfile == null) {
            Debug.logError("Could not find media profile [" + imageProfile + "]", module);
            return ServiceUtil.returnError("Could not find media profile [" + imageProfile + "]");
        }

        String viewType;
        Integer viewNumber;
        if ("ORIGINAL_IMAGE_URL".equals(productContentTypeId)) {
            viewType = "main";
            viewNumber = null;
        } else if (productContentTypeId.startsWith("ADDITIONAL_IMAGE_")) {
            viewType = "additional";
            try {
                viewNumber = Integer.parseInt(productContentTypeId.substring("ADDITIONAL_IMAGE_".length()));
                if (viewNumber <= 0) {
                    throw new IllegalArgumentException("Invalid additional image number");
                }
            } catch(Exception e) {
                return ServiceUtil.returnError("Unsupported productContentTypeId [" + productContentTypeId
                        + "] product [" + productId + "]: should be ORIGINAL_IMAGE_URL or ADDITIONAL_IMAGE_x: " + e.toString());
            }
        } else {
            // TODO: REVIEW: additional field support may be required
            return ServiceUtil.returnError("Unsupported productContentTypeId [" + productContentTypeId
                    + "] product [" + productId + "]: should be ORIGINAL_IMAGE_URL or ADDITIONAL_IMAGE_x");
        }

        Map<String, Object> resizeCtx = UtilMisc.toMap("productId", productId, "imageOrigUrl", imageUrl, "viewType", viewType, "viewNumber", viewNumber,
                "locale", ctx.get("locale"), "userLogin", ctx.get("userLogin"), "timeZone", ctx.get("timeZone"), "imageProfile", imageProfile);
        try {
            Map<String, Object> resizeResult = productImageFileScaleInAllSize(dctx, resizeCtx);
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

        return ServiceUtil.returnSuccess();
    }

    private static boolean isStrArgEmpty(Map<String, ?> context, String argName) {
        return UtilValidate.isEmpty((String) context.get(argName));
    }
}

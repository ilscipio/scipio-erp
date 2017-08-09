package org.ofbiz.product.image;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.image.ContentImageServices;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

/**
 * SCIPIO: New product image services, alternatives to {@link org.ofbiz.product.imagemanagement.ImageManagementServices}
 * and other utils.
 * TODO?: try to reconcile everything in the future, too difficult for now.
 * Added 2017-07-05.
 */
public abstract class ProductImageServices {

    public static final String module = ProductImageServices.class.getName();
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

    private static boolean isStrArgEmpty(Map<String, ?> context, String argName) {
        return UtilValidate.isEmpty((String) context.get(argName));
    }
}

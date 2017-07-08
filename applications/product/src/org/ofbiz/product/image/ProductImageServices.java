package org.ofbiz.product.image;

import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.content.image.ContentImageServices;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
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
        
        if (locale == null) locale = Locale.getDefault(); // FIXME?: default for output (not log)
        
        Map<String, Object> contentCtx = new HashMap<>(context);
        
        String imageServerPath = EntityUtilProperties.getPropertyValue("catalog", "image.server.path", delegator);
        String imageUrlPrefix = EntityUtilProperties.getPropertyValue("catalog", "image.url.prefix", delegator);
        contentCtx.put("imageServerPath", imageServerPath);
        contentCtx.put("imageUrlPrefix", imageUrlPrefix);
        
        Map<String, Object> imagePathArgs = new HashMap<>();
        
        String type = null;
        String id = (String) context.get("productId");
        if (viewType.toLowerCase().contains("main")) {
            String filenameFormat = EntityUtilProperties.getPropertyValue("catalog", "image.filename.format", (Delegator) context.get("delegator"));
            imagePathArgs.putAll(UtilMisc.toMap("location", "products", "id", id, "type", "original"));
            contentCtx.put("imageFnFmt", filenameFormat);
        } else if (viewType.toLowerCase().contains("additional") && viewNumber != null && viewNumber != 0) {
            String filenameFormat = EntityUtilProperties.getPropertyValue("catalog", "image.filename.additionalviewsize.format", (Delegator) context.get("delegator"));
            if (filenameFormat.endsWith("${id}")) { // TODO: REVIEW: don't get this
                id = id + "_View_" + viewNumber;
            } else {
                viewType = "additional" + viewNumber;
            }    
            imagePathArgs.putAll(UtilMisc.toMap("location", "products", "id", id, "viewtype", viewType, "sizetype", "original"));
            contentCtx.put("imageFnFmt", filenameFormat);
        } else {
            return ServiceUtil.returnError(UtilProperties.getMessage(resource, "ProductImageViewType", UtilMisc.toMap("viewType", type), locale));
        }
        
        contentCtx.put("imagePathArgs", imagePathArgs);
        contentCtx.put("imagePropXmlPath", ProductImageWorker.getProductImagePropertiesPath());
        
        // TODO/FIXME: currently provides no deletion of the old images...
        
        Map<String, Object> result = ContentImageServices.contentImageFileScaleInAllSizeCore(dctx, contentCtx);
        result.put("productSizeTypeList", ScaleImage.sizeTypeList);
        return result;
    }
    
}

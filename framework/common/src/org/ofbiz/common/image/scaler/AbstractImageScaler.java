package org.ofbiz.common.image.scaler;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.common.image.AbstractImageOp;
import org.ofbiz.common.image.ImageTransform;
import org.ofbiz.common.image.ImageType;
import org.ofbiz.common.image.ImageType.ImagePixelType;
import org.ofbiz.common.image.ImageUtil;

/**
 * SCIPIO: Common image scaler code, does all the boilerplate stuff.
 * Added 2017-07-10.
 */
public abstract class AbstractImageScaler extends AbstractImageOp implements ImageScaler {
    public static final String module = AbstractImageScaler.class.getName();
    
    protected static void putDefaultImageTypeOptions(Map<String, Object> options) {
        // NOTE: comments below only apply to the filters that make use of these options
        
        // TODO: REVIEW: if this is uncommented, it will force the specified types
        // as result formats even if the filter could have preserved the original/input image format
        //options.put("targettype", ImageType.DEFAULT);
        
        // used when targettype not set
        options.put("defaulttype", ImageType.DEFAULT);
        
        // if these fallbacks are set, they are used as result formats for any
        // input types the filter can't replicated (custom and/or indexed)
        options.put("fallbacktype", ImageType.DEFAULT);
    }
    
    protected AbstractImageScaler(AbstractImageScalerFactory<? extends AbstractImageScaler> factory, String name, 
            Map<String, Object> confOptions, Map<String, Object> defOptions) {
        super(factory, name, confOptions, defOptions);
    }
    
    protected AbstractImageScaler(AbstractImageScalerFactory<? extends AbstractImageScaler> factory, String name, 
            Map<String, Object> confOptions) {
        super(factory, name, confOptions);
    }

    @Override
    public BufferedImage scaleImage(BufferedImage image, int targetWidth, int targetHeight) throws IOException {
        return scaleImage(image, targetWidth, targetHeight, null);
    }
    
    @Override
    public BufferedImage scaleImage(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException {
        if (!requiresScaling(image, targetWidth, targetHeight)) return image;
        if (ImageUtil.debugOn()) return scaleImageDebug(image, targetWidth, targetHeight, getEffectiveScalingOptions(image, targetWidth, targetHeight, options));
        else return scaleImageCore(image, targetWidth, targetHeight, getEffectiveScalingOptions(image, targetWidth, targetHeight, options));
    }
    
    protected boolean requiresScaling(BufferedImage image, int targetWidth, int targetHeight) {
        // SPECIAL CASE: by default, in all cases, if the image is same dimensions, we'll just return as-is
        if (image.getWidth() == targetWidth && image.getHeight() == targetHeight) return false;
        else return true;
    }
    
    protected Map<String, Object> getEffectiveScalingOptions(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) {
        return getEffectiveOptions(options);
    }
    
    protected BufferedImage scaleImageDebug(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException {
        long startTime = System.nanoTime();
        BufferedImage result = scaleImageCore(image, targetWidth, targetHeight, options);
        long endTime = System.nanoTime();
        Debug.logInfo("Scaled image in " + ((endTime - startTime) / 1000000) + "ms using " + this.toString(options)
            + "; input image type: " + image.getType()+ "; output image type: " + result.getType(), module);
        return result;
    }
        
    /**
     * Calls the core image scaling, with option arguments merged with the ones from this instance.
     * options = confDefOptions + passedOptions = defOptions + confOptions + passedOptions
     */
    protected abstract BufferedImage scaleImageCore(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException;
    
    @SuppressWarnings("unchecked")
    @Override
    public AbstractImageScalerFactory<? extends AbstractImageScaler> getFactory() {
        return (AbstractImageScalerFactory<? extends AbstractImageScaler>) factory;
    }

    protected static String getFilterMapLogRepr(String apiName, Map<String, ?> filterMap) {
        StringBuilder sb = new StringBuilder("Image scaler [" + apiName + "] supported filters:\n");
        for(Map.Entry<String, ?> entry : filterMap.entrySet()) {
            sb.append(entry.getKey());
            sb.append(" -> ");
            sb.append(entry.getValue()+"");
            sb.append("\n");
        }
        return sb.toString();
    }
    
    // NOTE: ugly 2 parameters required to keep hierarchy consistent
    public static abstract class AbstractImageScalerFactory<T extends AbstractImageScaler> extends AbstractImageOpFactory<AbstractImageScaler, ImageScaler> implements ImageScalerFactory {
        
        protected void putCommonImageTypeOptions(Map<String, Object> destOptions, Map<String, Object> srcOptions) {
            putOption(destOptions, "overridetype", getImageTypeOption(srcOptions, "overridetype"), srcOptions);
            putOption(destOptions, "targettype", getImageTypeOption(srcOptions, "targettype"), srcOptions);
            putOption(destOptions, "defaulttype", getImageTypeOption(srcOptions, "defaulttype"), srcOptions);
            putOption(destOptions, "fallbacktype", getImageTypeOption(srcOptions, "fallbacktype"), srcOptions);
        }
        
    }
    
    protected static ImageType getMergedTargetImageType(Map<String, Object> options) {
        ImageType type = getImageTypeOption(options, "overridetype");
        if (type != null) return type;
        type = getImageTypeOption(options, "targettype");
        if (type != null) return type;
        return getImageTypeOption(options, "defaulttype");
    }
    
    protected static Integer getMergedTargetImagePixelType(Map<String, Object> options, BufferedImage srcImage) {
        ImageType imageType = getMergedTargetImageType(options);
        return imageType != null ? imageType.getPixelTypeFor(srcImage) : null;
    }
    
    /**
     * Best-effort attempt to honor requests for specific format or orig-preserve of the returned image after an image operation.
     * <p>
     * NOTE: targetType should be already resolved (don't pass TYPE_PRESERVE here).
     * <p>
     * FIXME?: this checks for type using a simple (modifiedImage.getType() == targetType) check, which 
     * may miss details about the image...
     */
    protected BufferedImage checkConvertResultImageType(BufferedImage srcImage, BufferedImage modifiedImage, 
            Map<String, Object> options, Integer targetPixelType, Integer fallbackPixelType) {
        if (targetPixelType == null) return modifiedImage;
        Integer origTargetPixelType = targetPixelType;
        
        if (ImagePixelType.isTypePreserve(targetPixelType)) targetPixelType = srcImage.getType();
        
        // APPROXIMATION of an image type equality check
        if (modifiedImage.getType() == targetPixelType) return modifiedImage;
        
        // TODO: REVIEW: DO NOT convert to indexed if lossless mode
        if (ImagePixelType.isTypeIndexedOrCustom(targetPixelType) && origTargetPixelType == ImagePixelType.TYPE_PRESERVE_IF_LOSSLESS)
            return modifiedImage;
        
        if (ImageUtil.verboseOn()) 
            Debug.logInfo("Image op reconvert required: image op type: " + modifiedImage.getType() + "; target type: " + targetPixelType, module);
        
        BufferedImage resultImage;
        if (targetPixelType == srcImage.getType()) {
            resultImage = ImageTransform.createCompatibleBufferedImage(srcImage, modifiedImage.getWidth(), modifiedImage.getHeight());
        } else {
            resultImage = ImageTransform.createBufferedImage(modifiedImage.getWidth(), modifiedImage.getHeight(), targetPixelType, null);
        }
        ImageTransform.copyToBufferedImage(modifiedImage, resultImage);
        return resultImage;
    }

}

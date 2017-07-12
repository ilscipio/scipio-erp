package org.ofbiz.common.image.scaler;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.common.image.AbstractImageOp;
import org.ofbiz.common.image.ImageTransform;
import org.ofbiz.common.image.ImageUtil;

/**
 * SCIPIO: Common image scaler code, does all the boilerplate stuff.
 * Added 2017-07-10.
 */
public abstract class AbstractImageScaler extends AbstractImageOp implements ImageScaler {
    public static final String module = AbstractImageScaler.class.getName();
    
    protected static void putDefaultBufTypeOptions(Map<String, Object> options) {
        // NOTE: comments below only apply to the filters that make use of these options
        
        // TODO: REVIEW: if this is uncommented, it will force the specified types
        // as result formats even if the filter could have preserved the original/input image format
        //options.put("targettype", ImageTransform.DEFAULT_BUFIMAGE_TYPE);
        //options.put("targettypenoalpha", ImageTransform.DEFAULT_BUFIMAGE_TYPE_NOALPHA);
        
        // if these fallbacks are set, they are used as result formats for any
        // input types the filter can't replicated (custom and/or indexed)
        options.put("fallbacktype", ImageTransform.DEFAULT_BUFIMAGE_TYPE);
        options.put("fallbacktypenoalpha", ImageTransform.DEFAULT_BUFIMAGE_TYPE_NOALPHA);
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
        
        protected void putCommonBufTypeOptions(Map<String, Object> destOptions, Map<String, Object> srcOptions) {
            putOption(destOptions, "targettype", getBufImgTypeOption(srcOptions, "targettype"), srcOptions);
            putOption(destOptions, "targettypenoalpha", getBufImgTypeOption(srcOptions, "targettypenoalpha"), srcOptions);
            
            putOption(destOptions, "fallbacktype", getBufImgTypeOption(srcOptions, "fallbacktype"), srcOptions);
            putOption(destOptions, "fallbacktypenoalpha", getBufImgTypeOption(srcOptions, "fallbacktypenoalpha"), srcOptions);
        }
        
    }
    
    protected static Integer getBufImgTypeOption(Map<String, Object> options, String fieldName) {
        Object typeObj = options.get(fieldName);
        if (typeObj == null) return null;
        else if (typeObj instanceof Integer) return (Integer) typeObj;
        else {
            String name = (String) typeObj;
            if (name.isEmpty()) return null;
            try {
                return ImageUtil.getBufferedImageTypeInt(name);
            } catch(Exception e) {
                throw new IllegalArgumentException(fieldName + " '" + name + "' not supported by library or does not exist", e);
            }
        }
    }
    
    protected static Integer getTargetOrFallbackBufImgType(BufferedImage srcImg, Map<String, Object> options, boolean supportsIndexed) {
        Integer targetType = srcImg.getColorModel().hasAlpha() ? getBufImgTypeOption(options, "targettype") : getBufImgTypeOption(options, "targettypenoalpha");
        if (targetType != null) {
            return targetType;
        } else {
            if ((!supportsIndexed && (srcImg.getType() == BufferedImage.TYPE_BYTE_INDEXED || srcImg.getType() == BufferedImage.TYPE_BYTE_BINARY)) || 
                    srcImg.getType() == BufferedImage.TYPE_CUSTOM) {
                return srcImg.getColorModel().hasAlpha() ? getBufImgTypeOption(options, "fallbacktype") : getBufImgTypeOption(options, "fallbacktypenoalpha");
            } else {
                return null;
            }
        }
    }
    
}

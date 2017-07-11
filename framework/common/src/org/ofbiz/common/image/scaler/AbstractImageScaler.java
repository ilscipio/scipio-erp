package org.ofbiz.common.image.scaler;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.common.image.AbstractImageOp;
import org.ofbiz.common.image.ImageUtil;

/**
 * SCIPIO: Common image scaler code, does all the boilerplate stuff.
 * Added 2017-07-10.
 */
public abstract class AbstractImageScaler extends AbstractImageOp implements ImageScaler {
    public static final String module = AbstractImageScaler.class.getName();
    
    protected AbstractImageScaler(AbstractImageScalerFactory<? extends AbstractImageScaler> factory, String name, 
            Map<String, Object> confOptions, Map<String, Object> defOptions) {
        super(factory, name, confOptions, defOptions);
    }
    
    protected AbstractImageScaler(AbstractImageScalerFactory<? extends AbstractImageScaler> factory, String name, 
            Map<String, Object> confOptions) {
        super(factory, name, confOptions);
    }

    @Override
    public Image scaleImage(BufferedImage image, int targetWidth, int targetHeight) throws IOException {
        return scaleImage(image, targetWidth, targetHeight, null);
    }
    
    @Override
    public Image scaleImage(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException {
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
    
    protected Image scaleImageDebug(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException {
        long startTime = System.nanoTime();
        Image result = scaleImageCore(image, targetWidth, targetHeight, options);
        long endTime = System.nanoTime();
        Debug.logInfo("Scaled image in " + ((endTime - startTime) / 1000000) + "ms using " + this.toString(options)
            + "; is result BufferedImage? " + (result instanceof BufferedImage), module);
        return result;
    }
        
    /**
     * Calls the core image scaling, with option arguments merged with the ones from this instance.
     * options = confDefOptions + passedOptions = defOptions + confOptions + passedOptions
     */
    protected abstract Image scaleImageCore(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException;
    
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
    }
}

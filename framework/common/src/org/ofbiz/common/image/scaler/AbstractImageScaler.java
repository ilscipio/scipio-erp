package org.ofbiz.common.image.scaler;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Map;

import org.ofbiz.common.image.AbstractImageOp;

/**
 * SCIPIO: Common image scaler code, does all the boilerplate stuff.
 * Added 2017-07-10.
 */
public abstract class AbstractImageScaler extends AbstractImageOp implements ImageScaler {

    protected AbstractImageScaler(AbstractImageScalerFactory<? extends AbstractImageScaler> factory, String name, Map<String, Object> defaultScalingOptions) {
        super(factory, name, defaultScalingOptions);
    }

    @Override
    public Image scaleImage(BufferedImage image, int targetWidth, int targetHeight) throws IOException {
        return scaleImageCore(image, targetWidth, targetHeight, defaultOptions);
    }
    
    @Override
    public Image scaleImage(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException {
        return scaleImageCore(image, targetWidth, targetHeight, getEffectiveOptions(options));
    }
    
    /**
     * Calls the core image scaling, with option arguments merged with the ones from this instance.
     * @throws IOException TODO
     */
    protected abstract Image scaleImageCore(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> scalingOptions) throws IOException;
    
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
            sb.append(entry.getValue().toString());
            sb.append("\n");
        }
        return sb.toString();
    }
    
    // NOTE: ugly 2 parameters required to keep hierarchy consistent
    public static abstract class AbstractImageScalerFactory<T extends AbstractImageScaler> extends AbstractImageOpFactory<AbstractImageScaler, ImageScaler> implements ImageScalerFactory {
    }
}

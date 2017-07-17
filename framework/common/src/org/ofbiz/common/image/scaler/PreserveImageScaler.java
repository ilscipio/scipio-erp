package org.ofbiz.common.image.scaler;

import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.common.image.ImageTransform;

/**
 * SCIPIO: Null resizing, in other words doesn't resize (only crops/fills).
 * Use mainly for same-size.
 * Added 2017-07-10.
 */
public class PreserveImageScaler extends AbstractImageScaler {

    public static final String module = PreserveImageScaler.class.getName();
    public static final String API_NAME = "preserve";

    public static final Map<String, Object> DEFAULT_OPTIONS;
    static {
        Map<String, Object> options = new HashMap<>();
        DEFAULT_OPTIONS = Collections.unmodifiableMap(options);
    }
    
    protected PreserveImageScaler(Factory factory, String name, Map<String, Object> confOptions) {
        super(factory, name, confOptions, DEFAULT_OPTIONS);
    }

    public static class Factory extends AbstractImageScalerFactory<PreserveImageScaler> {
        @Override
        public PreserveImageScaler getImageOpInstStrict(String name, Map<String, Object> defaultScalingOptions) {
            return new PreserveImageScaler(this, name, defaultScalingOptions);
        }

        @Override
        public Map<String, Object> makeValidOptions(Map<String, Object> options) {
            Map<String, Object> validOptions = new HashMap<>();
            return validOptions;
        }

        @Override protected String getApiName() { return API_NAME; }
        @Override public Map<String, Object> getDefaultOptions() { return DEFAULT_OPTIONS; }
    }
    
    @Override
    public BufferedImage scaleImageCore(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException {
        if (image.getWidth() == targetWidth && image.getHeight() == targetHeight) {
            return image;
        } else {
            BufferedImage destImage = ImageTransform.createCompatibleBufferedImage(image, targetWidth, targetHeight);
            Graphics g = destImage.createGraphics();
            try {
                g.drawImage(image, 0, 0, null);
            } finally {
                g.dispose();
            }
            return destImage;
        }
    }
    
    @Override
    public boolean isNativeSupportedDestImagePixelType(int imagePixelType) {
        return true;
    }
}

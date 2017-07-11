package org.ofbiz.common.image.scaler;

import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.awt.image.IndexColorModel;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.common.image.ImageUtil;

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
    public Image scaleImageCore(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException {
        if (image.getWidth() == targetWidth && image.getHeight() == targetHeight) {
            return image;
        } else {
            BufferedImage destImage;
            
            // WARN: we need to preserve the color model if there is one! stock ofbiz did not do this!
            int imgType = image.getType();
            if (imgType == BufferedImage.TYPE_BYTE_BINARY) {
                if (ImageUtil.verboseOn()) Debug.logInfo("Image type: TYPE_BYTE_BINARY", module);
                destImage = new BufferedImage(targetWidth, targetHeight, image.getType(), (IndexColorModel) image.getColorModel());
            } else if (imgType == BufferedImage.TYPE_BYTE_INDEXED) {
                if (ImageUtil.verboseOn()) Debug.logInfo("Image type: TYPE_BYTE_INDEXED", module);
                destImage = new BufferedImage(targetWidth, targetHeight, image.getType(), (IndexColorModel) image.getColorModel());
            } else if (imgType == BufferedImage.TYPE_CUSTOM) {
                if (ImageUtil.verboseOn()) Debug.logInfo("Image type: TYPE_CUSTOM; using TYPE_INT_ARGB_PRE", module);
                // this is the default used in stock Ofbiz, it appears sane
                destImage = new BufferedImage(targetWidth, targetHeight, BufferedImage.TYPE_INT_ARGB_PRE);
            } else {
                if (ImageUtil.verboseOn()) Debug.logInfo("Image type: " + image.getType(), module);
                destImage = new BufferedImage(targetWidth, targetHeight, image.getType());
            }

            Graphics g = destImage.createGraphics();
            try {
                g.drawImage(image, 0, 0, null);
            } finally {
                g.dispose();
            }
            return destImage;
        }
    }
}

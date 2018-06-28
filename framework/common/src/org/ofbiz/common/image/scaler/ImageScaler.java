package org.ofbiz.common.image.scaler;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Map;

import org.ofbiz.common.image.ImageOp;

/**
 * SCIPIO: Simple image scaling interface, to allow to plugin different scaling algorithms
 * from different libraries.
 * Added 2017-07-10.
 */
public interface ImageScaler extends ImageOp {

    BufferedImage scaleImage(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException;

    BufferedImage scaleImage(BufferedImage image, int targetWidth, int targetHeight) throws IOException;
    
    public interface ImageScalerFactory extends ImageOpFactory<ImageScaler> {
    }
}

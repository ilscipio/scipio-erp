package org.ofbiz.common.image.scaler;

import org.ofbiz.common.image.ImageOp;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Map;

/**
 * Simple file-encoded image scaling interface for image scalers, to allow to plugin different scaling algorithms
 * from different libraries (SCIPIO).
 * TODO: not yet implemented, requires a separate integration from ImageScaler that uses exclusively BufferedImage
 *  OR integrate this into ImageScaler as new methods with abstract defaults.
 */
public interface ImageFileScaler extends ImageOp {

    /** Scales a full encoded image (file path, File or byte[] array) to byte array. */
    byte[] scaleImageFileToBytes(Object image, String targetFormat, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException;

    /** Scales a full encoded image (file path, File or byte[] array) to file storage (file path, File). */
    Object scaleImageFileToFile(Object image, Object targetFile, String targetFormat, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException;

    interface ImageFileScalerFactory extends ImageOpFactory<ImageFileScaler> {
    }
}

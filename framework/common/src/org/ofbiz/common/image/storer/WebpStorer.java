package org.ofbiz.common.image.storer;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;

import javax.imageio.ImageIO;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import java.awt.image.RenderedImage;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * ImageIO Webp storer.
 * <p>Configured in imageops.properties</p>.
 */
public class WebpStorer extends AbstractImageStorer {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String API_NAME = "format-webp";

    private static final int PARAM_WARN_MAX = 10;
    private int PARAM_WARN_COUNT = 0; // no sync necessary

    public WebpStorer(Factory factory, String name, Map<String, Object> confOptions) {
        super(factory, name, confOptions, factory.getDefaultOptions());
    }

    public static class Factory extends AbstractImageStorerFactory<ImageIOStorer> {
        @Override
        public ImageStorer getImageOpInstStrict(String name, Map<String, Object> defaultOptions) {
            return new WebpStorer(this, name, defaultOptions);
        }

        @Override
        protected String getApiName() {
            return API_NAME;
        }

        @Override
        public Map<String, Object> makeValidOptions(Map<String, Object> options) {
            Map<String, Object> validOptions = new HashMap<>();
            putCommonValidOptions(validOptions, options);
            putOption(validOptions, "compressionType", options.get("compressionType"), options);
            putOption(validOptions, "compressionQuality", UtilMisc.toFloat(options.get("compressionQuality"), null), options);
            return validOptions;
        }

        @Override
        public Map<String, Object> getDefaultOptions() {
            return Collections.emptyMap();
        }
    }

    @Override
    public boolean write(RenderedImage im, String formatName, Object output, String imageProfile, Map<String, Object> options, Delegator delegator) throws IOException {
        return writeCore(im, formatName, output, imageProfile, options, delegator);
    }

    protected boolean writeCore(RenderedImage im, String formatName, Object output, String imageProfile, Map<String, Object> options, Delegator delegator) throws IOException {
        options = getEffectiveOptions(options);

        // Obtain a WebP ImageWriter instance
        ImageWriter writer = ImageIO.getImageWritersByMIMEType("image/webp").next();
        if (writer == null) {
            throw new IOException("Could not find image/webp ImageWriter");
            //return false;
        }

        // Configure encoding parameters
        ImageWriteParam writeParam = writer.getDefaultWriteParam();

        String compressionType = (String) options.get("compressionType");
        if (compressionType != null && !compressionType.isEmpty()) {
            String[] compressionTypes = writeParam.getCompressionTypes();
            if (compressionTypes != null && compressionTypes.length > 0) {
                // case-correction for backward-compatibility
                for(String cType : compressionTypes) {
                    if (compressionType.equalsIgnoreCase(cType)) {
                        compressionType = cType;
                        break;
                    }
                }
            }
            writeParam.setCompressionType(compressionType);
        }

        Float compressionQuality = UtilMisc.toFloat(options.get("compressionQuality"), null);
        if (compressionQuality != null) {
            writeParam.setCompressionQuality(compressionQuality);
        }

        // TODO: more writeParam settings

        setOutputAndWriteIOImage(writer, writeParam, im, formatName, output, imageProfile, options, delegator);
        return true;
    }
}

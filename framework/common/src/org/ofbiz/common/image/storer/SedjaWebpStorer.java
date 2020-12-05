package org.ofbiz.common.image.storer;

import com.luciad.imageio.webp.WebPWriteParam;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;

import javax.imageio.IIOImage;
import javax.imageio.ImageIO;
import javax.imageio.ImageWriteParam;
import javax.imageio.ImageWriter;
import javax.imageio.stream.ImageOutputStream;
import java.awt.image.RenderedImage;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Sejda ImageIO Webp storer.
 * https://github.com/sejda-pdf/webp-imageio
 * Configured in imageops.properties.
 */
public class SedjaWebpStorer extends AbstractImageStorer {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String API_NAME = "sedja-webp";

    public SedjaWebpStorer(Factory factory, String name, Map<String, Object> confOptions) {
        super(factory, name, confOptions, factory.getDefaultOptions());
    }

    public static class Factory extends AbstractImageStorerFactory<ImageIOStorer> {
        @Override
        public ImageStorer getImageOpInstStrict(String name, Map<String, Object> defaultOptions) {
            return new SedjaWebpStorer(this, name, defaultOptions);
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
        WebPWriteParam writeParam = new WebPWriteParam(writer.getLocale());
        writeParam.setCompressionMode(ImageWriteParam.MODE_EXPLICIT);

        String compressionType = (String) options.get("compressionType");
        if ("lossy".equals(compressionType)) {
            writeParam.setCompressionType(writeParam.getCompressionTypes()[WebPWriteParam.LOSSY_COMPRESSION]);
        } else {
            writeParam.setCompressionType(writeParam.getCompressionTypes()[WebPWriteParam.LOSSLESS_COMPRESSION]);
        }

        Float compressionQuality = UtilMisc.toFloat(options.get("compressionQuality"), null);
        if (compressionQuality != null) {
            writeParam.setCompressionQuality(compressionQuality);
        }

        // TODO: more writeParam settings

        // Configure the output on the ImageWriter
        ImageOutputStream tempStream = null;
        try {
            if (output instanceof ImageOutputStream) {
                writer.setOutput(output);
            } else if (output instanceof OutputStream) {
                tempStream = ImageIO.createImageOutputStream(output);
                writer.setOutput(tempStream);
            } else if (output instanceof File) {
                ((File) output).delete(); // see ImageIO.write
                tempStream = ImageIO.createImageOutputStream(output);
                writer.setOutput(tempStream);
                //writer.setOutput(new FileImageOutputStream((File) output));
            } else {
                // It'll do it for us if needed
                //throw new IOException("Unsupported output: " + (output != null ? output.getClass().getName() : output));
                writer.setOutput(output);
            }
            // Encode
            writer.write(null, new IIOImage(im, null, null), writeParam);
        } finally {
            if (tempStream != null) {
                tempStream.close();
            }
        }
        return true;
    }
}

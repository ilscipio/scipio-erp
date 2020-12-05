package org.ofbiz.common.image.storer;

import com.tinify.Source;
import com.tinify.Tinify;
import org.apache.commons.io.FileUtils;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;

import javax.imageio.stream.ImageOutputStream;
import java.awt.image.RenderedImage;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * Tinify TinyPNG/TinyJPG image optimization storer.
 * Configured in imageops.properties (uncomment
 */
public class TinifyStorer extends AbstractImageStorer {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String API_NAME = "tinify";

    public TinifyStorer(Factory factory, String name, Map<String, Object> confOptions) {
        super(factory, name, confOptions, factory.getDefaultOptions());
    }

    public static class Factory extends AbstractImageStorerFactory<ImageIOStorer> {
        @Override
        public ImageStorer getImageOpInstStrict(String name, Map<String, Object> defaultOptions) {
            return new TinifyStorer(this, name, defaultOptions);
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
            putOption(validOptions, "wrappedStorer", options.get("wrappedStorer"), options);
            putOption(validOptions, "apiKey", options.get("apiKey"), options);
            putOption(validOptions, "proxy", options.get("proxy"), options);
            putOption(validOptions, "logLevel", options.get("logLevel"), options);
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

        String apiKey = (String) options.get("apiKey");
        if (UtilValidate.isEmpty(apiKey)) {
            throw new IllegalStateException("Missing tinify apiKey, please configure imageops.properties#image.storer." + getName() + ".options.apiKey");
        }
        // FIXME?: In the Tinify API implementation at time of writing (2020-11), its static mechanism is thread-unsafe so must be called like this each time
        Tinify.setKey(apiKey);
        String proxy = (String) options.get("proxy");
        if (UtilValidate.isNotEmpty(proxy)) {
            Tinify.setProxy(proxy);
        }

        ImageStorer storer = ImageStorers.getStorerOrDefault((String) options.get("wrappedStorer"));
        long startTs = System.currentTimeMillis();
        ByteArrayOutputStream baos = new ByteArrayOutputStream();
        // Write original
        storer.write(im, formatName, baos, imageProfile, options, delegator);

        long initialWriteTs = System.currentTimeMillis();
        // Optimize (remote call)
        byte[] imageBytes;
        try {
            Source src = Tinify.fromBuffer(baos.toByteArray());
            imageBytes = src.toBuffer();
        } catch(Exception e) {
            String targetName = (output instanceof File) ? output.toString() : "stream";
            Debug.logError("tinify: could not optimize image (" + targetName + "): " + e.toString(), module);
            //return ImageStorers.getDefaultStorer().write(im, formatName, output, options, delegator);
            return storer.write(im, formatName, output, imageProfile, options, delegator);
        }
        long optimizeTs = System.currentTimeMillis();

        if (output instanceof ImageOutputStream) {
            ImageOutputStream os = (ImageOutputStream) output;
            os.write(imageBytes);
        } else if (output instanceof OutputStream) {
            OutputStream os = (OutputStream) output;
            os.write(imageBytes);
        } else if (output instanceof File) {
            ((File) output).delete(); // see ImageIO.write
            FileUtils.writeByteArrayToFile((File) output, imageBytes);
        } else {
            throw new IOException("Unsupported image output for byte array: " + (output != null ? output.getClass().getName() : output));
        }

        long outputTs = System.currentTimeMillis();
        if (Debug.isLowerEqualPrio(options.get("logLevel"), Debug.INFO)) {
            String targetName = (output instanceof File) ? output.toString() : "stream";
            Debug.logInfo("tinify: converted image (" + targetName + ", " + formatName +
                    ", initial size: " + baos.size() + "b, optimized size: " + imageBytes.length + "b) in " +
                    (outputTs - startTs) + "ms (initial write: " + (initialWriteTs - startTs) + "ms, remote convert: " +
                    (optimizeTs - initialWriteTs) + "ms, " + "final write: " + (outputTs - optimizeTs) + "ms)", module);
        }
        return true;
    }
}

package org.ofbiz.common.image.storer;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.common.image.scaler.AbstractImageScaler;
import org.ofbiz.entity.Delegator;

import javax.imageio.ImageIO;
import javax.imageio.stream.ImageOutputStream;
import java.awt.image.RenderedImage;
import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public class ImageIOStorer extends AbstractImageStorer {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String API_NAME = "java-imageio";

    public ImageIOStorer(Factory factory, String name, Map<String, Object> confOptions) {
        super(factory, name, confOptions, factory.getDefaultOptions());
    }

    public static class Factory extends AbstractImageStorerFactory<ImageIOStorer> {
        @Override
        public ImageIOStorer getImageOpInstStrict(String name, Map<String, Object> defaultOptions) {
            return new ImageIOStorer(this, name, defaultOptions);
        }

        @Override
        public Map<String, Object> makeValidOptions(Map<String, Object> options) {
            Map<String, Object> validOptions = new HashMap<>();
            putCommonValidOptions(validOptions, options);
            return validOptions;
        }

        @Override
        public Map<String, Object> getDefaultOptions() {
            return Collections.emptyMap();
        }

        @Override
        protected String getApiName() {
            return API_NAME;
        }
    }

    @Override
    public boolean write(RenderedImage im, String formatName, Object output, Map<String, Object> options, Delegator delegator) throws IOException {
        return writeIO(im, formatName, output, options, delegator);
    }

    public static boolean writeIO(RenderedImage im, String formatName, Object output, Map<String, Object> options, Delegator delegator) throws IOException {
        if (output instanceof ImageOutputStream) {
            return ImageIO.write(im, formatName, (ImageOutputStream) output);
        } else if (output instanceof OutputStream) {
            return ImageIO.write(im, formatName, (OutputStream) output);
        } else if (output instanceof File) {
            return ImageIO.write(im, formatName, (File) output);
        } else {
            throw new IOException("Unsupported output: " + (output != null ? output.getClass().getName() : output));
        }
    }

    @Override
    public boolean isApplicable(RenderedImage im, String formatName, Object output, Map<String, Object> options, Delegator delegator) {
        return true; // supports any format, in theory
    }
}

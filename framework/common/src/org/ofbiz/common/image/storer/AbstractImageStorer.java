package org.ofbiz.common.image.storer;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.common.image.AbstractImageOp;
import org.ofbiz.common.image.ImageOp;
import org.ofbiz.common.image.scaler.AbstractImageScaler;
import org.ofbiz.common.image.scaler.ImageScaler;
import org.ofbiz.entity.Delegator;

import java.awt.image.RenderedImage;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

public abstract class AbstractImageStorer extends AbstractImageOp implements ImageStorer {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected final Set<String> formats;

    protected AbstractImageStorer(AbstractImageStorerFactory<? extends AbstractImageStorer> factory, String name, Map<String, Object> confOptions, Map<String, Object> defaultOptions) {
        super(factory, name, confOptions, defaultOptions);
        Set<String> formats = Collections.unmodifiableSet(StringUtil.splitNames(new LinkedHashSet<>(), (String) confOptions.get("formats")));
        if (formats.contains("*")) {
            formats = Collections.emptySet();
        }
        this.formats = formats;
    }

    // NOTE: ugly 2 parameters required to keep hierarchy consistent
    public static abstract class AbstractImageStorerFactory<T extends AbstractImageStorer> extends AbstractImageOp.AbstractImageOpFactory<AbstractImageStorer, ImageStorer> implements ImageStorer.ImageStorerFactory {
        public Map<String, Object> putCommonValidOptions(Map<String, Object> validOptions, Map<String, Object> options) {
            putOption(validOptions, "sequenceNum", UtilMisc.toInteger(options.get("sequenceNum"), null), options);
            putOption(validOptions, "formats", options.get("formats"), options);
            return validOptions;
        }
    }

    @Override
    public boolean isApplicable(RenderedImage im, String formatName, Object output, Map<String, Object> options, Delegator delegator) {
        return isApplicableCommon(im, formatName, output, options, delegator);
    }

    public boolean isApplicableCommon(RenderedImage im, String formatName, Object output, Map<String, Object> options, Delegator delegator) {
        return isFormatApplicable(formatName, delegator);
    }

    public Set<String> getFormats() {
        return formats;
    }

    protected boolean isFormatApplicable(String formatName, Delegator delegator) {
        return (getFormats().isEmpty()) ? true : getFormats().contains(formatName);
    }

    @Override
    public boolean isNativeSupportedDestImagePixelType(int imagePixelType) {
        //return false; // not applicable? class design issue
        throw new UnsupportedOperationException();
    }

}

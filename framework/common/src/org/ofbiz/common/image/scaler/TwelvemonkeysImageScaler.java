package org.ofbiz.common.image.scaler;

import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.common.image.ImageType.ImagePixelType;

import com.twelvemonkeys.image.ResampleOp;

/**
 * SCIPIO: Twelvemonkeys common-image image scaler implementation.
 * @deprecated 2017-07-14: This scaler implementation is currently not being maintained, but could be used again later.
 * <p>
 * Supported scalingOptions:
 * <ul>
 * <li>filter (String) - "smooth" (default) or substitute (see {@link #filterMap} below for supported)</li>
 * </ul>
 * </p>
 * Added 2017-07-12.
 */
@Deprecated
public class TwelvemonkeysImageScaler extends AbstractImageScaler {

    public static final String module = TwelvemonkeysImageScaler.class.getName();
    public static final String API_NAME = "twelvemonkeys";
    
    /**
     * Maps <code>scalingOptions.filter</code> to ResampleFilter instances.
     */
    private static final Map<String, Integer> filterMap;
    static {
        Map<String, Integer> map = new HashMap<>();
        
        // GENERALIZED
        //map.put("areaaveraging", Image.SCALE_AREA_AVERAGING); // TODO
        //map.put("default", Image.SCALE_DEFAULT); // TODO
        //map.put("fast", Image.SCALE_FAST); // TODO
        //map.put("replicate", Image.SCALE_REPLICATE); // TODO
        map.put("smooth", ResampleOp.FILTER_LANCZOS);
        
        // SPECIFIC ALGORITHMS
        map.put("lanczos3", ResampleOp.FILTER_LANCZOS);
        
        // API-SPECIFIC
        // (none)
        
        filterMap = Collections.unmodifiableMap(map);
        Debug.logInfo(AbstractImageScaler.getFilterMapLogRepr(API_NAME, map), module);
    }
    
    public static final Map<String, Object> DEFAULT_OPTIONS;
    static {
        Map<String, Object> options = new HashMap<>();
        putDefaultImageTypeOptions(options);
        options.put("filter", filterMap.get("smooth")); // String
        DEFAULT_OPTIONS = Collections.unmodifiableMap(options);
    }
    
    protected TwelvemonkeysImageScaler(AbstractImageScalerFactory<TwelvemonkeysImageScaler> factory, String name, Map<String, Object> confOptions) {
        super(factory, name, confOptions);
    }

    public static class Factory extends AbstractImageScalerFactory<TwelvemonkeysImageScaler> {

        @Override
        public TwelvemonkeysImageScaler getImageOpInstStrict(String name, Map<String, Object> defaultScalingOptions) {
            return new TwelvemonkeysImageScaler(this, name, defaultScalingOptions);
        }

        @Override
        public Map<String, Object> makeValidOptions(Map<String, Object> options) {
            Map<String, Object> validOptions = new HashMap<>(options);
            putCommonImageTypeOptions(validOptions, options);
            putOption(validOptions, "filter", getFilter(options), options);
            return validOptions;
        }

        @Override protected String getApiName() { return API_NAME; }
        @Override public Map<String, Object> getDefaultOptions() { return DEFAULT_OPTIONS; }
    }
    
    @Override
    protected BufferedImage scaleImageCore(BufferedImage image, int targetWidth, int targetHeight,
            Map<String, Object> options) throws IOException {
        Integer filter = getFilter(options);
        BufferedImageOp op;
        if (filter != null) {
            op = new ResampleOp(targetWidth, targetHeight, filter);
        } else {
            op = new ResampleOp(targetWidth, targetHeight);
        }

        // TODO: REVIEW: this is copy-pasted from morten scaler because very similar interfaces
    
        Integer targetType = getMergedTargetImagePixelType(options, image);
        Integer fallbackType = getFallbackImagePixelType(options, image);
        BufferedImage result;
        if (targetType != null) {
            int idealType = ImagePixelType.isTypePreserve(targetType) ? image.getType() : targetType;
            
            if (ImagePixelType.isTypeIndexedOrCustom(idealType)) {
                if (targetType == ImagePixelType.TYPE_PRESERVE_IF_LOSSLESS) {
                    Integer defaultType = getImagePixelTypeOption(options, "defaulttype", image);
                    if (defaultType != null && !ImagePixelType.isTypeSpecial(defaultType) && 
                        !ImagePixelType.isTypeIndexedOrCustom(defaultType)) {
                        BufferedImage destImage = new BufferedImage(targetWidth, targetHeight, defaultType);
                        result = op.filter(image, destImage);
                    } else {
                        if (fallbackType != null && !ImagePixelType.isTypeSpecial(fallbackType) && 
                                !ImagePixelType.isTypeIndexedOrCustom(fallbackType)) {
                            BufferedImage destImage = new BufferedImage(targetWidth, targetHeight, fallbackType);
                            result = op.filter(image, destImage);
                        } else {
                            result = op.filter(image, null);
                        }
                    }
                } else {
                    result = op.filter(image, null);
                    result = checkConvertResultImageType(image, result, options, targetType, fallbackType);
                }
            } else {
                BufferedImage destImage = new BufferedImage(targetWidth, targetHeight, idealType);
                result = op.filter(image, destImage);
            }
        } else {
            result = op.filter(image, null);
        }
        return result;
    }
    
    // NOTE: defaults are handled through the options merging with defaults
    protected static Integer getFilter(Map<String, Object> options) throws IllegalArgumentException {
        Object filterObj = options.get("filter");
        if (filterObj == null) return null;
        else if (filterObj instanceof Integer) return (Integer) filterObj;
        else {
            String filterName = (String) filterObj;
            if (filterName.isEmpty()) return null;
            if (!filterMap.containsKey(filterName)) throw new IllegalArgumentException("filter '" + filterName + "' not supported by " + API_NAME + " library");
            return filterMap.get(filterName);
        }
    }
}

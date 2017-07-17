package org.ofbiz.common.image.scaler;

import java.awt.image.BufferedImage;
import java.awt.image.BufferedImageOp;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.common.image.ImageTransform;
import org.ofbiz.common.image.ImageType;
import org.ofbiz.common.image.ImageType.ImagePixelType;
import org.ofbiz.common.image.ImageType.ImageTypeInfo;

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
    
        ImageType targetType = getMergedTargetImageType(options, ImageType.EMPTY);
        ImageTypeInfo targetTypeInfo = targetType.getImageTypeInfoFor(image);
        
        BufferedImage resultImage;
        if (!ImagePixelType.isTypeNoPreserveOrNull(targetTypeInfo.getPixelType())) {
            ImageTypeInfo resolvedTargetTypeInfo = ImageType.resolveTargetType(targetTypeInfo, image);
            
            if (isNativeSupportedDestImageType(resolvedTargetTypeInfo)) {
                // here lib will _probably_ support the type we want...
                BufferedImage destImage = ImageTransform.createBufferedImage(resolvedTargetTypeInfo, targetWidth, targetHeight);
                resultImage = op.filter(image, destImage);
            } else {
                if (isPostConvertResultImage(image, options, targetTypeInfo)) {
                    resultImage = op.filter(image, null); // lib default image type should preserve best for intermediate
                    resultImage = checkConvertResultImageType(image, resultImage, options, targetTypeInfo);
                } else {
                    int nextTargetType = getFirstSupportedDestPixelTypeFromAllDefaults(options, image);
                    resultImage = op.filter(image, new BufferedImage(targetWidth, targetHeight, nextTargetType));
                }
            }
        } else {
            int nextTargetType = getFirstSupportedDestPixelTypeFromAllDefaults(options, image);
            resultImage = op.filter(image, new BufferedImage(targetWidth, targetHeight, nextTargetType));
        }
        return resultImage;
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
    
    @Override
    public boolean isNativeSupportedDestImagePixelType(int imagePixelType) {
        return !ImagePixelType.isTypeIndexedOrCustom(imagePixelType);
    }
}

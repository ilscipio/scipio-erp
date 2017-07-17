package org.ofbiz.common.image.scaler;

import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.imgscalr.Scalr;
import org.ofbiz.base.util.Debug;
import org.ofbiz.common.image.ImageType;
import org.ofbiz.common.image.ImageType.ImageTypeInfo;

/**
 * SCIPIO: Imgscalr image scaler implementation.
 * @deprecated 2017-07-14: This scaler implementation is currently not being maintained, but could be used again later.
 * <p>
 * Supported scalingOptions:
 * <ul>
 * <li>filter (String) - "smooth" (default) or substitute (see {@link #filterMap} below for supported)</li>
 * </ul>
 * </p>
 * <p>
 * Added 2017-07-12.
 */
public class ImgscalrImageScaler extends AbstractImageScaler {

    public static final String module = ImgscalrImageScaler.class.getName();
    public static final String API_NAME = "imgscalr";
    
    /**
     * Maps <code>scalingOptions.filter</code> to ResampleFilter instances.
     */
    private static final Map<String, Scalr.Method> filterMap;
    static {
        Map<String, Scalr.Method> map = new HashMap<>();
        
        // GENERALIZED
        //map.put("areaaveraging", Image.SCALE_AREA_AVERAGING); // TODO
        //map.put("default", Image.SCALE_DEFAULT); // TODO
        map.put("fast", Scalr.Method.SPEED); // TODO
        //map.put("replicate", Image.SCALE_REPLICATE); // TODO
        map.put("smooth", Scalr.Method.ULTRA_QUALITY);
        
        // SPECIFIC ALGORITHMS
        // (none)
        
        // API-SPECIFIC
        map.put("ultra", Scalr.Method.ULTRA_QUALITY);
        map.put("quality", Scalr.Method.QUALITY);
        map.put("speed", Scalr.Method.SPEED);
        
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
    
    protected ImgscalrImageScaler(AbstractImageScalerFactory<ImgscalrImageScaler> factory, String name, Map<String, Object> confOptions) {
        super(factory, name, confOptions);
    }

    public static class Factory extends AbstractImageScalerFactory<ImgscalrImageScaler> {

        @Override
        public ImgscalrImageScaler getImageOpInstStrict(String name, Map<String, Object> defaultScalingOptions) {
            return new ImgscalrImageScaler(this, name, defaultScalingOptions);
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
        
        // FIXME?: imgscalr supports no target image types at all...
        BufferedImage result = Scalr.resize(image, getFilter(options), Scalr.Mode.FIT_EXACT, targetWidth, targetHeight);
        
        ImageType targetType = getMergedTargetImageType(options, ImageType.EMPTY);
        ImageTypeInfo targetTypeInfo = targetType.getImageTypeInfoFor(image);

        // FIXME?: for now don't bother post-converting anything at all unless we're forced...
        return isPostConvertResultImage(image, options, targetTypeInfo) ?
                checkConvertResultImageType(image, result, options, targetTypeInfo) : result;
    }
    
    // NOTE: defaults are handled through the options merging with defaults
    protected static Scalr.Method getFilter(Map<String, Object> options) throws IllegalArgumentException {
        Object filterObj = options.get("filter");
        if (filterObj == null) return null;
        else if (filterObj instanceof Scalr.Method) return (Scalr.Method) filterObj;
        else {
            String filterName = (String) filterObj;
            if (filterName.isEmpty()) return null;
            if (!filterMap.containsKey(filterName)) throw new IllegalArgumentException("filter '" + filterName + "' not supported by " + API_NAME + " library");
            return filterMap.get(filterName);
        }
    }
    
    @Override
    public boolean isNativeSupportedDestImagePixelType(int imagePixelType) {
        return false; // FIXME: this isn't true, it can output ONE format... should check...
    }
}

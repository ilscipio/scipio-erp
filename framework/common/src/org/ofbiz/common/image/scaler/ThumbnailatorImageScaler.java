package org.ofbiz.common.image.scaler;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;

import net.coobird.thumbnailator.Thumbnails;
import net.coobird.thumbnailator.resizers.configurations.Antialiasing;
import net.coobird.thumbnailator.resizers.configurations.Dithering;
import net.coobird.thumbnailator.resizers.configurations.ScalingMode;

/**
 * SCIPIO: Thumbnailator image scaler implementation.
 * <p>
 * Supported scalingOptions:
 * <ul>
 * <li>filter (String) - "smooth" (default) or substitute (see {@link #filterMap} below for supported)</li>
 * <li>dithering (Boolean) - true/false/empty/null
 * <li>antialiasing (Boolean) - true/false/empty/null
 * </ul>
 * </p>
 * TODO: add more scalingOptions
 * <p>
 * Added 2017-07-10.
 */
public class ThumbnailatorImageScaler extends AbstractImageScaler {

    public static final String module = ThumbnailatorImageScaler.class.getName();
    public static final String API_NAME = "thumbnailator";
    
    /**
     * Maps <code>scalingOptions.filter</code> to Thumbnailator ScalingMode instances.
     * FIXME?: this does not properly represent/support all the ResizerFactory/Resizer classes/combinations
     * supported by Thumbnailator; however because these are internal definitions (client code should NOT
     * try to pass them), we can fix this later if wanted...
     */
    private static final Map<String, ScalingMode> filterMap;
    static {
        Map<String, ScalingMode> map = new HashMap<>();
        
        // GENERALIZED
        //map.put("areaaveraging", Image.SCALE_AREA_AVERAGING); // TODO
        //map.put("default", Image.SCALE_DEFAULT); // TODO
        //map.put("fast", Image.SCALE_FAST); // TODO
        //map.put("replicate", Image.SCALE_REPLICATE); // TODO
        map.put("smooth", null); // Thumbnailator default (smooth-auto)
        
        // SPECIFIC ALGORITHMS
        map.put("bicubic", ScalingMode.BICUBIC);
        map.put("bilinear", ScalingMode.BILINEAR);
        map.put("progbilinear", ScalingMode.PROGRESSIVE_BILINEAR);
        
        // API-SPECIFIC
        map.put("smooth-auto", null); // Thumbnailator default (selects between progbilinear, bicubic and bilinear automatically)
                                      // Behavior described in net.coobird.thumbnailator.resizers.DefaultResizerFactory
        
        filterMap = Collections.unmodifiableMap(map);
        Debug.logInfo(AbstractImageScaler.getFilterMapLogRepr(API_NAME, map), module);
    }
    
    public static final Map<String, Object> DEFAULT_OPTIONS;
    static {
        Map<String, Object> options = new HashMap<>();
        options.put("filter", filterMap.get("smooth")); // String
        options.put("dithering", null); // Boolean; we set null to use Thumbnailator default
        options.put("antialiasing", null); // Boolean
        DEFAULT_OPTIONS = Collections.unmodifiableMap(options);
    }
    
    protected ThumbnailatorImageScaler(AbstractImageScalerFactory<? extends AbstractImageScaler> factory, String name,
            Map<String, Object> confOptions) {
        super(factory, name, confOptions);
    }

    public static class Factory extends AbstractImageScalerFactory<ThumbnailatorImageScaler> {
        @Override
        public ThumbnailatorImageScaler getImageOpInstStrict(String name, Map<String, Object> defaultScalingOptions) {
            return new ThumbnailatorImageScaler(this, name, defaultScalingOptions);
        }

        @Override
        public Map<String, Object> makeValidOptions(Map<String, Object> options) {
            Map<String, Object> validOptions = new HashMap<>();
            putOption(validOptions, "filter", getFilter(options), options);
            putOption(validOptions, "dithering", getDithering(options), options);
            putOption(validOptions, "antialiasing", getAntialiasing(options), options);
            return validOptions;
        }

        @Override protected String getApiName() { return API_NAME; }
        @Override public Map<String, Object> getDefaultOptions() { return DEFAULT_OPTIONS; }
    }
    
    @Override
    protected Image scaleImageCore(BufferedImage image, int targetWidth, int targetHeight,
            Map<String, Object> options) throws IOException {
        Thumbnails.Builder<BufferedImage> builder = Thumbnails.of(image).size(targetWidth, targetHeight);
        ScalingMode filter = getFilter(options);
        if (filter != null) {
            builder = builder.scalingMode(getFilter(options));
        }
        Boolean dithering = getDithering(options);
        if (dithering != null) {
            builder = builder.dithering(dithering ? Dithering.ENABLE : Dithering.DISABLE);
        }
        Boolean antialiasing = getAntialiasing(options);
        if (antialiasing != null) {
            builder = builder.antialiasing(antialiasing ? Antialiasing.ON : Antialiasing.OFF);
        }
        return builder.asBufferedImage();
    }
    
    // NOTE: defaults are handled through the options merging with defaults
    protected static ScalingMode getFilter(Map<String, Object> options) throws IllegalArgumentException {
        Object filterObj = options.get("filter");
        if (filterObj == null) return null;
        else if (filterObj instanceof ScalingMode) return (ScalingMode) filterObj;
        else {
            String filterName = (String) filterObj;
            if (filterName.isEmpty()) return null;
            if (!filterMap.containsKey(filterName)) throw new IllegalArgumentException("filter '" + filterName + "' not supported by " + API_NAME + " library");
            return filterMap.get(filterName);
        }
    }
    
    protected static Boolean getDithering(Map<String, Object> options) {
        return UtilMisc.booleanValue(options.get("dithering"));
    }
    
    protected static Boolean getAntialiasing(Map<String, Object> options) {
        return UtilMisc.booleanValue(options.get("antialiasing"));
    }
}

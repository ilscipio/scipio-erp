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
import net.coobird.thumbnailator.resizers.configurations.Dithering;
import net.coobird.thumbnailator.resizers.configurations.ScalingMode;

/**
 * SCIPIO: Thumbnailator image scaler implementation.
 * <p>
 * Supported scalingOptions:
 * <ul>
 * <li>filter (String) - "smooth" (default) or substitute (see {@link #filterMap} below for supported)</li>
 * <li>dithering (boolean) - true/false
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
     * Maps <code>scalingOptions.filter</code> to Thumbnailator filter instances.
     */
    private static final Map<String, ScalingMode> filterMap;
    static {
        Map<String, ScalingMode> map = new HashMap<>();
        
        // GENERALIZED
        //map.put("areaaveraging", Image.SCALE_AREA_AVERAGING); // TODO
        //map.put("default", Image.SCALE_DEFAULT); // TODO
        //map.put("fast", Image.SCALE_FAST); // TODO
        //map.put("replicate", Image.SCALE_REPLICATE); // TODO
        map.put("smooth", ScalingMode.PROGRESSIVE_BILINEAR);
        
        // SPECIFIC ALGORITHMS
        map.put("bicubic", ScalingMode.BICUBIC);
        map.put("bilinear", ScalingMode.BILINEAR);
        map.put("progbilinear", ScalingMode.PROGRESSIVE_BILINEAR);
        
        // API-SPECIFIC
        // (none)
        
        filterMap = Collections.unmodifiableMap(map);
        
        Debug.logInfo(AbstractImageScaler.getFilterMapLogRepr(API_NAME, map), module);
    }
    
    private static final ScalingMode defaultFilter = filterMap.get("smooth");
    private static final Boolean defaultDithering = null; // use Thumbnailator different for scaling mode
    
    protected ThumbnailatorImageScaler(AbstractImageScalerFactory<? extends AbstractImageScaler> factory, String name,
            Map<String, Object> defaultScalingOptions) {
        super(factory, name, defaultScalingOptions);
    }

    public static class Factory extends AbstractImageScalerFactory<ThumbnailatorImageScaler> {
        @Override
        public ThumbnailatorImageScaler getImageOpInstStrict(String name, Map<String, Object> defaultScalingOptions) {
            return new ThumbnailatorImageScaler(this, name, defaultScalingOptions);
        }

        @Override
        public Map<String, Object> makeValidOptions(Map<String, Object> options) {
            Map<String, Object> validOptions = new HashMap<>();
            ScalingMode filter = getFilter(options, null);
            if (filter != null) {
                validOptions.put("filter", filter);
            }
            Boolean dithering = getDithering(options, null);
            if (dithering != null) {
                validOptions.put("dithering", dithering);
            }
            return validOptions;
        }

        @Override
        protected String getApiName() {
            return API_NAME;
        }
    }
    
    @Override
    protected Image scaleImageCore(BufferedImage image, int targetWidth, int targetHeight,
            Map<String, Object> scalingOptions) throws IOException {
        Thumbnails.Builder<BufferedImage> builder = Thumbnails.of(image).size(targetWidth, targetHeight);
        builder = builder.scalingMode(getFilter(scalingOptions, defaultFilter));
        Boolean dithering = getDithering(scalingOptions, defaultDithering);
        if (dithering != null) {
            builder = builder.dithering(dithering ? Dithering.ENABLE : Dithering.DISABLE);
        }
        return builder.asBufferedImage();
    }
    
    protected static ScalingMode getFilter(Map<String, Object> options, ScalingMode defaultValue) throws IllegalArgumentException {
        Object filterObj = options.get("filter");
        if (filterObj == null) return defaultValue;
        else if (filterObj instanceof ScalingMode) return (ScalingMode) filterObj;
        else {
            String filterName = (String) filterObj;
            if (filterName.isEmpty()) return defaultValue;
            ScalingMode filter = filterMap.get(filterName);
            if (filter == null) throw new IllegalArgumentException("filter '" + filterName + "' not supported by Thumbnailator library");
            return filter;
        }
    }
    
    protected static Boolean getDithering(Map<String, Object> options, Boolean defaultValue) {
        Boolean dithering = UtilMisc.booleanValue(options.get("dithering"));
        return dithering != null ? dithering : defaultValue;
    }
}

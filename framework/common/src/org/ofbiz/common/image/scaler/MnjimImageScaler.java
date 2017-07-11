package org.ofbiz.common.image.scaler;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;

import com.mortennobel.imagescaling.ResampleFilter;
import com.mortennobel.imagescaling.ResampleFilters;
import com.mortennobel.imagescaling.ResampleOp;

/**
 * SCIPIO: Morten Nobel java-image-scaler image scaler implementation.
 * By default uses lanczos3 filter.
 * <p>
 * Supported scalingOptions:
 * <ul>
 * <li>filter (String) - "smooth" (default) or substitute (see {@link #filterMap} below for supported)</li>
 * </ul>
 * </p>
 * TODO: this is only using the ResampleOp for now; there is also MultiStepRescaleOp.
 * <p>
 * Added 2017-07-10.
 */
public class MnjimImageScaler extends AbstractImageScaler {

    public static final String module = MnjimImageScaler.class.getName();
    public static final String API_NAME = "mortennobel";
    
    /**
     * Maps <code>scalingOptions.filter</code> to ResampleFilter instances.
     */
    private static final Map<String, ResampleFilter> filterMap;
    static {
        Map<String, ResampleFilter> map = new HashMap<>();
        
        // GENERALIZED
        //map.put("areaaveraging", Image.SCALE_AREA_AVERAGING); // TODO
        //map.put("default", Image.SCALE_DEFAULT); // TODO
        //map.put("fast", Image.SCALE_FAST); // TODO
        //map.put("replicate", Image.SCALE_REPLICATE); // TODO
        map.put("smooth", ResampleFilters.getLanczos3Filter());
        
        // SPECIFIC ALGORITHMS
        map.put("lanczos3", ResampleFilters.getLanczos3Filter());
        map.put("bicubic", ResampleFilters.getBiCubicFilter());
        map.put("bicubichf", ResampleFilters.getBiCubicHighFreqResponse());
        map.put("mitchell", ResampleFilters.getMitchellFilter());
        map.put("hermite", ResampleFilters.getHermiteFilter());
        map.put("bspline", ResampleFilters.getBSplineFilter());
        map.put("triangle", ResampleFilters.getTriangleFilter());
        map.put("bell", ResampleFilters.getBellFilter());
        map.put("box", ResampleFilters.getBoxFilter());
        
        // API-SPECIFIC
        // (none)
        
        filterMap = Collections.unmodifiableMap(map);
        
        Debug.logInfo(AbstractImageScaler.getFilterMapLogRepr(API_NAME, map), module);
    }
    
    private static final ResampleFilter defaultFilter = filterMap.get("smooth");
    
    protected MnjimImageScaler(AbstractImageScalerFactory<MnjimImageScaler> factory, String name, Map<String, Object> defaultScalingOptions) {
        super(factory, name, defaultScalingOptions);
    }

    public static class Factory extends AbstractImageScalerFactory<MnjimImageScaler> {

        @Override
        public MnjimImageScaler getImageOpInstStrict(String name, Map<String, Object> defaultScalingOptions) {
            return new MnjimImageScaler(this, name, defaultScalingOptions);
        }

        @Override
        public Map<String, Object> makeValidOptions(Map<String, Object> options) {
            Map<String, Object> validOptions = new HashMap<>(options);
            ResampleFilter filter = getFilter(options, null);
            if (filter != null) {
                validOptions.put("filter", filter);
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
        ResampleFilter filter = getFilter(scalingOptions, defaultFilter);
        
        ResampleOp op = new ResampleOp(targetWidth, targetHeight);
        op.setFilter(filter);
        
        return op.filter(image, null);
    }
    
    protected static ResampleFilter getFilter(Map<String, Object> options, ResampleFilter defaultValue) throws IllegalArgumentException {
        Object filterObj = options.get("filter");
        if (filterObj == null) return defaultValue;
        else if (filterObj instanceof ResampleFilter) return (ResampleFilter) filterObj;
        else {
            String filterName = (String) filterObj;
            if (filterName.isEmpty()) return defaultValue;
            ResampleFilter filter = filterMap.get(filterName);
            if (filter == null) throw new IllegalArgumentException("filter '" + filterName + "' not supported by java-image-scaler library");
            return filter;
        }
    }
}

package org.ofbiz.common.image.scaler;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;

/**
 * SCIPIO: Java JDK image scaler implementation.
 * This is the Ofbiz stock/default behavior.
 * <p>
 * Supported scalingOptions:
 * <ul>
 * <li>filter (String) - "smooth" (default) or substitute (see {@link #filterMap} below for supported)</li>
 * </ul>
 * </p>
 * Added 2017-07-10.
 */
public class JdkImageScaler extends AbstractImageScaler {

    public static final String module = JdkImageScaler.class.getName();
    public static final String API_NAME = "jdk";
    
    /**
     * Maps <code>scalingOptions.filter</code> to {@link java.awt.Image#SCALE_SMOOTH} or substitutes.
     */
    private static final Map<String, Integer> filterMap;
    static {
        Map<String, Integer> map = new HashMap<>();
        
        // GENERALIZED
        map.put("areaaveraging", Image.SCALE_AREA_AVERAGING);
        map.put("default", Image.SCALE_DEFAULT);
        map.put("fast", Image.SCALE_FAST);
        map.put("replicate", Image.SCALE_REPLICATE);
        map.put("smooth", Image.SCALE_SMOOTH);
        
        // SPECIFIC ALGORITHMS
        // (none)
        
        // API-SPECIFIC
        // (none)
        
        filterMap = Collections.unmodifiableMap(map);
        
        Debug.logInfo(AbstractImageScaler.getFilterMapLogRepr(API_NAME, map), module);
    }
    
    private static final int defaultFilter = filterMap.get("smooth");
    
    protected JdkImageScaler(Factory factory, String name, Map<String, Object> defaultScalingOptions) {
        super(factory, name, defaultScalingOptions);
    }

    public static class Factory extends AbstractImageScalerFactory<JdkImageScaler> {
        @Override
        public JdkImageScaler getImageOpInstStrict(String name, Map<String, Object> defaultScalingOptions) {
            return new JdkImageScaler(this, name, defaultScalingOptions);
        }

        @Override
        public Map<String, Object> makeValidOptions(Map<String, Object> options) {
            Map<String, Object> validOptions = new HashMap<>();
            Integer filter = getFilter(options, null);
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
    public Image scaleImageCore(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException {
        return image.getScaledInstance(targetWidth, targetHeight, getFilter(options, defaultFilter));
    }
    
    protected static Integer getFilter(Map<String, Object> options, Integer defaultValue) {
        Object filterObj = options.get("filter");
        if (filterObj == null) return defaultValue;
        else if (filterObj instanceof Integer) return (Integer) filterObj;
        else {
            String filterName = (String) filterObj;
            if (filterName.isEmpty()) return defaultValue;
            Integer filter = filterMap.get(filterName);
            if (filter == null) throw new IllegalArgumentException("filter '" + filterName + "' not supported by JDK image scaler library");
            return filter;
        }
    }

}

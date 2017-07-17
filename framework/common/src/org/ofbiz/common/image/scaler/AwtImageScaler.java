package org.ofbiz.common.image.scaler;

import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.common.image.ImageTransform;
import org.ofbiz.common.image.ImageType;
import org.ofbiz.common.image.ImageType.ImagePixelType;
import org.ofbiz.common.image.ImageType.ImageTypeInfo;

/**
 * SCIPIO: Java AWT (JRE/JDK) image scaler implementation.
 * This is the Ofbiz stock/default behavior.
 * <p>
 * Supported scalingOptions:
 * <ul>
 * <li>filter (String) - "smooth" (default) or substitute (see {@link #filterMap} below for supported)</li>
 * </ul>
 * </p>
 * NOTE: In theory AWT itself is supposed to be configurable, but there are no good filter implementations
 * of {@link java.awt.image.ImageFilter} nor system-configurable libraries containing them.
 * <p>
 * FIXME?: unclear whether this correctly preserves ColorModel for indexed images.
 * <p>
 * Added 2017-07-10.
 */
public class AwtImageScaler extends AbstractImageScaler {

    public static final String module = AwtImageScaler.class.getName();
    public static final String API_NAME = "java-awt";
    
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
    
    public static final Map<String, Object> DEFAULT_OPTIONS;
    static {
        Map<String, Object> options = new HashMap<>();
        options.put("filter", filterMap.get("smooth")); // String
        DEFAULT_OPTIONS = Collections.unmodifiableMap(options);
    }
    
    protected AwtImageScaler(Factory factory, String name, Map<String, Object> confOptions) {
        super(factory, name, confOptions, DEFAULT_OPTIONS);
    }

    public static class Factory extends AbstractImageScalerFactory<AwtImageScaler> {
        @Override
        public AwtImageScaler getImageOpInstStrict(String name, Map<String, Object> defaultScalingOptions) {
            return new AwtImageScaler(this, name, defaultScalingOptions);
        }

        @Override
        public Map<String, Object> makeValidOptions(Map<String, Object> options) {
            Map<String, Object> validOptions = new HashMap<>();
            putOption(validOptions, "filter", getFilter(options), options);
            return validOptions;
        }

        @Override protected String getApiName() { return API_NAME; }
        @Override public Map<String, Object> getDefaultOptions() { return DEFAULT_OPTIONS; }
    }
    
    @Override
    public BufferedImage scaleImageCore(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException {
        Integer filter = getFilter(options);
        if (filter == null) filter = 0;
        
        // WARN: getScaledInstance is an ancient and hardcoded method. It's usually here for
        // backward-compat, because it's what Ofbiz was using.
        Image modifiedImage = image.getScaledInstance(targetWidth, targetHeight, filter);

        ImageType targetType = getMergedTargetImageType(options, ImageType.EMPTY);
        ImageTypeInfo targetTypeInfo = targetType.getImageTypeInfoFor(image);
        
        if (!ImagePixelType.isTypeNoPreserveOrNull(targetTypeInfo.getPixelType())) {
            //if (ImagePixelType.imageMatchesRequestedType(imageToTest, targetPixelType, targetColorMode, srcImage))
            ImageTypeInfo resolvedTargetTypeInfo = ImageType.resolveTargetType(targetTypeInfo, image);

            // NOTE: this check and the first if block could probably be omitted, but passing BufferedImage instance
            // is slightly better 
            if (ImageType.imageMatchesType(image, resolvedTargetTypeInfo)) {
                BufferedImage resultImage = ImageTransform.createCompatibleBufferedImage(image, modifiedImage.getWidth(null), modifiedImage.getHeight(null));
                ImageTransform.copyToBufferedImage(modifiedImage, resultImage);
                return resultImage;
            } else {
                BufferedImage resultImage = ImageTransform.createCompatibleBufferedImage(image, resolvedTargetTypeInfo, modifiedImage.getWidth(null), modifiedImage.getHeight(null));
                ImageTransform.copyToBufferedImage(modifiedImage, resultImage);
                return resultImage;
            }
        } else {
            // WARN: this is flawed, but it will practically never happen.
            return ImageTransform.toCompatibleBufferedImage(modifiedImage, ImageTypeInfo.from(image));
        }
    }
    
    // NOTE: defaults are handled through the options merging with defaults
    protected static Integer getFilter(Map<String, Object> options) {
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
    public boolean isNativeSupportedDestImagePixelType(int targetPixelType) {
        return true; // TODO: review
    }

}

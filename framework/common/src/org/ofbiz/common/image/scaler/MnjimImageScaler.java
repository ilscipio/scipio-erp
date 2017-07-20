package org.ofbiz.common.image.scaler;

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
    
    public static final Map<String, Object> DEFAULT_OPTIONS;
    static {
        Map<String, Object> options = new HashMap<>();
        putDefaultImageTypeOptions(options);
        options.put("filter", filterMap.get("smooth")); // String
        DEFAULT_OPTIONS = Collections.unmodifiableMap(options);
    }
    
    protected MnjimImageScaler(AbstractImageScalerFactory<MnjimImageScaler> factory, String name, Map<String, Object> confOptions) {
        super(factory, name, confOptions);
    }

    public static class Factory extends AbstractImageScalerFactory<MnjimImageScaler> {

        @Override
        public MnjimImageScaler getImageOpInstStrict(String name, Map<String, Object> defaultScalingOptions) {
            return new MnjimImageScaler(this, name, defaultScalingOptions);
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
        ResampleFilter filter = getFilter(options);
        
        // this appears to be pointless - morten already converts to a type that it likes - it's 
        // the result image type that matters to us...
//        // PRE-CONVERT: morten doesn't use the same defaults as us...
//        if (image.getType() == BufferedImage.TYPE_BYTE_BINARY ||
//                image.getType() == BufferedImage.TYPE_BYTE_INDEXED ||
//                        image.getType() == BufferedImage.TYPE_CUSTOM) {
//            Integer fallbacktype = image.getColorModel().hasAlpha() ? getFallbacktype(options) : getFallbacktypenoalpha(options);
//            image = ImageUtils.convert(image, fallbacktype);
//            // orig:
//            //image = ImageUtils.convert(image, image.getColorModel().hasAlpha() ?
//            //        BufferedImage.TYPE_4BYTE_ABGR : BufferedImage.TYPE_3BYTE_BGR);
//        }
        
        ResampleOp op = new ResampleOp(targetWidth, targetHeight);
        if (filter != null) {
            op.setFilter(filter);
        }
        
        // HOW THIS WORKS: we try our best to get the filter to write directly in the image pixel type requested.
        // But morten does not support indexed images as output, and in addition, reconverting after is not guaranteed lossless.
        // In that case there are two options:
        // 1) scale + reconvert in two steps to honor requested image pixel type: could result in losses
        // 2) don't honor the requested image pixel type: changes the format, but usually lossless
        // TODO: REVIEW: the new BufferedImage calls do not transfer over all possible info like ColorModel, but morten itself doesn't either, so...
        // maybe we should re-check TYPE_PRESERVE and call ImageTransform.createCompatibleBufferedImage...
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
    protected static ResampleFilter getFilter(Map<String, Object> options) throws IllegalArgumentException {
        Object filterObj = options.get("filter");
        if (filterObj == null) return null;
        else if (filterObj instanceof ResampleFilter) return (ResampleFilter) filterObj;
        else {
            String filterName = (String) filterObj;
            if (filterName.isEmpty()) return null;
            if (!filterMap.containsKey(filterName)) throw new IllegalArgumentException("filter '" + filterName + "' not supported by " + API_NAME + " library");
            return filterMap.get(filterName);
        }
    }
    
    @Override
    public boolean isNativeSupportedDestImagePixelType(int imagePixelType) {
        // TODO: REVIEW
        return !ImagePixelType.isTypeIndexedOrCustom(imagePixelType);
    }
}

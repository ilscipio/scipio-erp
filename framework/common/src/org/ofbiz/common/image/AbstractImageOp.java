package org.ofbiz.common.image;

import java.awt.image.BufferedImage;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.common.image.ImageType.ImagePixelType;
import org.ofbiz.common.image.ImageType.ImageTypeInfo;

/**
 * SCIPIO: Common image operation base class (scaling, etc.), does all the boilerplate stuff.
 * Added 2017-07-10.
 */
public abstract class AbstractImageOp implements ImageOp {
    public static final String module = AbstractImageOp.class.getName();
    
    protected static void putDefaultImageTypeOptions(Map<String, Object> options) {
        // NOTE: comments below only apply to the filters that make use of these options
        
        // TODO: REVIEW: if this is uncommented, it will force the specified types
        // as result formats even if the filter could have preserved the original/input image format
        //options.put("targettype", ImageType.DEFAULT);
        
        // used when targettype not set
        options.put("defaulttype", ImageType.DEFAULT);
        
        // if these fallbacks are set, they are used as result formats for any
        // input types the filter can't replicated (custom and/or indexed)
        options.put("fallbacktype", ImageType.DEFAULT);
    }
    
    protected final AbstractImageOpFactory<? extends AbstractImageOp, ? extends ImageOp> factory;
    protected final String name;
    protected final Map<String, Object> confOptions;
    protected final Map<String, Object> defOptions;
    /**
     * This is defaultOptions + confOptions.
     */
    protected final Map<String, Object> confDefOptions;
    
    protected AbstractImageOp(AbstractImageOpFactory<? extends AbstractImageOp, ? extends ImageOp> factory, String name, 
            Map<String, Object> confOptions, Map<String, Object> defOptions) {
        this.factory = factory;
        this.name = name;
        this.confOptions = confOptions != null ? Collections.unmodifiableMap(
                factory.makeOptionsMap(confOptions)) : Collections.<String, Object>emptyMap();
        this.defOptions = confOptions != null ? Collections.unmodifiableMap(
                factory.makeOptionsMap(defOptions)) : Collections.<String, Object>emptyMap();
        Map<String, Object> confDefOptions = factory.makeOptionsMap(this.defOptions);
        confDefOptions.putAll(this.confOptions);
        this.confDefOptions = confDefOptions;
    }
    
    protected AbstractImageOp(AbstractImageOpFactory<? extends AbstractImageOp, ? extends ImageOp> factory, String name, 
            Map<String, Object> confOptions) {
        this(factory, name, confOptions, factory.getDefaultOptions());
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Map<String, Object> getConfiguredOptions() {
        return confOptions;
    }

    @Override
    public Map<String, Object> getDefaultOptions() {
        return defOptions;
    }

    @Override
    public Map<String, Object> getConfiguredAndDefaultOptions() {
        return confDefOptions;
    }

    @Override
    public AbstractImageOpFactory<? extends AbstractImageOp, ? extends ImageOp> getFactory() {
        return factory;
    }
    
    @Override
    public Map<String, Object> makeValidOptions(Map<String, Object> options) {
        return factory.makeValidOptions(options);
    }
    
    protected Map<String, Object> getEffectiveOptions(Map<String, Object> options) {
        if (options == null || options.isEmpty()) return confDefOptions;
        else if (confDefOptions.isEmpty()) return options;
        else {
            Map<String, Object> mergedMap = getFactory().makeOptionsMap(confDefOptions);
            mergedMap.putAll(options);
            return mergedMap;
        }
    }
    
    protected String getApiName() {
        return getFactory().getApiName();
    }

    @Override
    public String toString() {
        return "[" + getName() + "/" + getApiName() + "/defaults:" + getConfiguredAndDefaultOptions().toString() + "]";
    }
    
    public String toString(Map<String, Object> options) {
        return "[" + getName() + "/" + getApiName() + "/options:" + options.toString() + "]";
    }
    
    // NOTE: this needs two params otherwise the multiple inherit hierarchy breaks
    public static abstract class AbstractImageOpFactory<T extends AbstractImageOp, V extends ImageOp> implements ImageOpFactory<V> {
        @Override
        public V getImageOpInst(String name, Map<String, Object> defaultScalingOptions) {
            return getImageOpInstStrict(name, makeValidOptions(defaultScalingOptions));
        }

        @Override
        public V getDerivedImageOpInst(String name, Map<String, Object> defaultScalingOptions, ImageOp other) {
            Map<String, Object> mergedOptions = makeOptionsMap(other.getConfiguredOptions());
            if (defaultScalingOptions != null) {
                mergedOptions.putAll(defaultScalingOptions);
            }
            return getImageOpInst(name, mergedOptions);
        }
        
        protected abstract String getApiName();
        
        /**
         * Helper for implementing {@link ImageOpFactory#makeValidOptions}.
         * If the value is null but srcOptions contained the key, then we put null, because this may
         * mean the caller wanted to pass explicit null (also converts empty string to null in some cases).
         */
        protected void putOption(Map<String, Object> destOptions, String name, Object value, Map<String, Object> srcOptions) {
            if (value != null || srcOptions.containsKey(name)) {
                destOptions.put(name, value);
            }
        }
        
        protected Map<String, Object> makeOptionsMap(Map<String, Object> srcMap) {
            return new HashMap<>(srcMap);
        }
        protected Map<String, Object> makeOptionsMap() {
            return new HashMap<>();
        }
        
        protected void putCommonImageTypeOptions(Map<String, Object> destOptions, Map<String, Object> srcOptions) {
            putOption(destOptions, "overridetype", getImageTypeOption(srcOptions, "overridetype"), srcOptions);
            putOption(destOptions, "targettype", getImageTypeOption(srcOptions, "targettype"), srcOptions);
            putOption(destOptions, "defaulttype", getImageTypeOption(srcOptions, "defaulttype"), srcOptions);
            putOption(destOptions, "fallbacktype", getImageTypeOption(srcOptions, "fallbacktype"), srcOptions);
        }
    }

    protected static Boolean getForceOp(Map<String, Object> options) {
        return UtilMisc.booleanValue(options.get("forceop"));
    }
    
    /**
     * Should return true if the the op implementation PROPERLY supports writing out directly
     * to given dest image type without extra conversions.
     */
    public abstract boolean isNativeSupportedDestImagePixelType(int imagePixelType);
    
    /**
     * Should return true if the the op implementation PROPERLY supports writing out directly
     * to given dest image type without extra conversions.
     * Subclasses should override if they need to override based on ColorModel or other (not just image pixel type).
     */
    public boolean isNativeSupportedDestImageType(ImageTypeInfo type) {
        return isNativeSupportedDestImagePixelType(type.getPixelType());
    }
    
    /**
     * Parses (if needed) and returns an ImageType option from the options map.
     * This always returns an instance except when the map does not contain the field name at all,
     * or when parsing error (exception).
     */
    public static ImageType getImageTypeOption(Map<String, Object> options, String fieldName, ImageType defaultValue) {
        try {
            if (!options.containsKey(fieldName)) return defaultValue;
            else return ImageType.getForObject(options.get(fieldName));
        } catch(Exception e) {
            throw new IllegalArgumentException("Invalid image type option " + fieldName + ": " + e.getMessage(), e);
        }
    }
    
    public static ImageType getImageTypeOption(Map<String, Object> options, String fieldName) {
        return getImageTypeOption(options, fieldName, null);
    }
    
    /**
     * Returns an ImagePixelType (BufferedImage TYPE_XXX) int for the given ImageType option appropriate
     * for the given image. May return null.
     */
    protected static Integer getImagePixelTypeOption(Map<String, Object> options, String fieldName, BufferedImage srcImage) {
        ImageType imageType = getImageTypeOption(options, fieldName);
        if (imageType != null) return imageType.getPixelTypeFor(srcImage);
        return null;
    }

    protected static ImageType getMergedTargetImageType(Map<String, Object> options, ImageType defaultImageType) {
        ImageType type = getImageTypeOption(options, "overridetype", null);
        if (type != null) return type;
        type = getImageTypeOption(options, "targettype", null);
        if (type != null) return type;
        type = getImageTypeOption(options, "defaulttype", null);
        if (type != null) return type;
        return defaultImageType;
    }
    
    protected static ImageType getFallbackImageType(Map<String, Object> options, BufferedImage srcImage, ImageType defaultImageType) {
        return getImageTypeOption(options, "fallbacktype", defaultImageType);
    }
    
    protected int getFirstSupportedDestPixelTypeFromAllDefaults(Map<String, Object> options, BufferedImage srcImage, Integer fallbackType) {
        return getFirstSupportedDestPixelType(options, srcImage, "defaulttype", fallbackType, ImageType.DEFAULT, ImageType.DEFAULT_DIRECT, ImageType.INT_ARGB_OR_RGB);
    }
    
    protected int getFirstSupportedDestPixelTypeFromAllDefaults(Map<String, Object> options, BufferedImage srcImage) {
        return getFirstSupportedDestPixelType(options, srcImage, "defaulttype", "fallbackType", ImageType.DEFAULT, ImageType.DEFAULT_DIRECT, ImageType.INT_ARGB_OR_RGB);
    }
    
    protected int getFirstSupportedDestPixelTypeFromSystemDefaults(Map<String, Object> options, BufferedImage srcImage) {
        return getFirstSupportedDestPixelType(options, srcImage, ImageType.DEFAULT, ImageType.DEFAULT_DIRECT, ImageType.INT_ARGB_OR_RGB);
    }
    
    protected Integer getFirstSupportedDestPixelTypeFromOptionDefaults(Map<String, Object> options, BufferedImage srcImage, Integer fallbackType) {
        return getFirstSupportedDestPixelType(options, srcImage, "defaulttype", fallbackType);
    }
    
    protected Integer getFirstSupportedDestPixelTypeFromOptionDefaults(Map<String, Object> options, BufferedImage srcImage) {
        return getFirstSupportedDestPixelType(options, srcImage, "defaulttype", "fallbackType");
    }
    
    protected Integer getFirstSupportedDestPixelType(Map<String, Object> options, BufferedImage srcImage, Object... candidateTypes) {
        for(Object candidateType : candidateTypes) {
            if (candidateType == null) continue;
            Integer intCandidateType = null;
            if (candidateType instanceof Integer) {
                intCandidateType = (Integer) candidateType;
            } else if (candidateType instanceof ImageTypeInfo) {
                intCandidateType = ((ImageTypeInfo)candidateType).getPixelType();
            } else if (candidateType instanceof ImageType) {
                intCandidateType = ((ImageType)candidateType).getPixelTypeFor(srcImage);
            } else if (candidateType instanceof String) {
                intCandidateType = getImagePixelTypeOption(options, (String) candidateType, srcImage);
            } else {
                throw new IllegalArgumentException("invalid candidate type");
            }
            if (intCandidateType != null && !ImagePixelType.isTypeSpecial(intCandidateType) && 
                    isNativeSupportedDestImagePixelType(intCandidateType)) {
                return intCandidateType;
            }
        }
        return null;
    }
    
    /**
     * Returns true if and only if checkConvertResultImageType is expected to perform a post-op conversion.
     * Used to optimize the intermediate image type.
     * Does NOT guarantee that a post-op conversion will actually happen.
     */
    protected boolean isPostConvertResultImage(BufferedImage srcImage, Map<String, Object> options, ImageTypeInfo targetTypeInfo) {
        Integer targetPixelType = targetTypeInfo != null ? targetTypeInfo.getPixelType() : null;
        if (targetPixelType == null || targetPixelType == ImagePixelType.TYPE_NOPRESERVE) return false; // sanity check, mostly
        
        ImageTypeInfo resolvedTargetPixelType = ImageType.resolveTargetType(targetTypeInfo, srcImage);
        
        // TODO?: other missing type conversion loss checks...
        
        // DO NOT convert to indexed if lossless mode (TODO: REVIEW)
        if (ImagePixelType.isTypeIndexedOrCustom(resolvedTargetPixelType.getPixelType()) && targetPixelType == ImagePixelType.TYPE_PRESERVE_IF_LOSSLESS)
            return false;
        return true;
    }
    
    /**
     * Best-effort attempt to honor requests for specific format or orig-preserve of the returned image after an image operation.
     * <p>
     * COLOR LOSS: In principle, by default this method may produce color loss but only within the limits acceptable for the
     * flag {@link org.ofbiz.common.image.ImageType.ImagePixelType#TYPE_PRESERVE_IF_LOWLOSS}.
     * If targetPixelType is TYPE_PRESERVE_IF_LOSSLESS this will return modifiedImage as-is.
     * WARN: This guarantee currently mainly applies only to target images of indexed type.
     * <p>
     * WARN: the color loss guarantee may not apply for TYPE_CUSTOM.
     * FIXME: the color loss guarantee currently does not properly evaluate for non-indexed types!
     * (should check for BPP and alpha channel downgrade...)
     * <p>
     * NOTE: targetType should be already resolved (don't pass TYPE_PRESERVE here).
     * <p>
     * FIXME?: this checks for type using a simple (modifiedImage.getType() == targetType) check, which 
     * may miss details about the image...
     */
    protected BufferedImage checkConvertResultImageType(BufferedImage srcImage, BufferedImage modifiedImage, 
            Map<String, Object> options, ImageTypeInfo targetTypeInfo) {
        // TODO: optimize this call (plus caller should already have done it), the same things are redone several times
        if (!isPostConvertResultImage(srcImage, options, targetTypeInfo)) {
            return modifiedImage;
        }
        
        // TODO: optimize this call
        if (ImageType.imageMatchesRequestedType(modifiedImage, targetTypeInfo, srcImage)) return modifiedImage;

        ImageTypeInfo resolvedTargetPixelType = ImageType.resolveTargetType(targetTypeInfo, srcImage);
        BufferedImage resultImage;
        if (ImageType.imageMatchesType(modifiedImage, resolvedTargetPixelType)) {
            // ALTERNATIVE implementation (not as good):
            //resultImage = ImageTransform.createBufferedImage(modifiedImage.getWidth(), modifiedImage.getHeight(), targetPixelType, srcImage.getColorModel());
            resultImage = ImageTransform.createCompatibleBufferedImage(srcImage, modifiedImage.getWidth(), modifiedImage.getHeight());
        } else {
            resultImage = ImageTransform.createBufferedImage(resolvedTargetPixelType, modifiedImage.getWidth(), modifiedImage.getHeight());
        }

        if (ImageUtil.verboseOn()) 
            Debug.logInfo("Applying required image pixel type post-op conversion ("
                    + "input type: " + ImageType.printImageTypeInfo(srcImage) 
                    + "; post-op type: " + ImageType.printImageTypeInfo(modifiedImage)
                    + "; target type: " + ImageType.printImageTypeInfo(resultImage)
                    + ")", module);

        ImageTransform.copyToBufferedImage(modifiedImage, resultImage);
        return resultImage;
    }
}

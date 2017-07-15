package org.ofbiz.common.image.scaler;

import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.io.IOException;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.common.image.AbstractImageOp;
import org.ofbiz.common.image.ImageTransform;
import org.ofbiz.common.image.ImageType;
import org.ofbiz.common.image.ImageType.ImagePixelType;
import org.ofbiz.common.image.ImageUtil;

/**
 * SCIPIO: Common image scaler code, does all the boilerplate stuff.
 * Added 2017-07-10.
 */
public abstract class AbstractImageScaler extends AbstractImageOp implements ImageScaler {
    public static final String module = AbstractImageScaler.class.getName();
    
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
    
    protected AbstractImageScaler(AbstractImageScalerFactory<? extends AbstractImageScaler> factory, String name, 
            Map<String, Object> confOptions, Map<String, Object> defOptions) {
        super(factory, name, confOptions, defOptions);
    }
    
    protected AbstractImageScaler(AbstractImageScalerFactory<? extends AbstractImageScaler> factory, String name, 
            Map<String, Object> confOptions) {
        super(factory, name, confOptions);
    }

    @Override
    public BufferedImage scaleImage(BufferedImage image, int targetWidth, int targetHeight) throws IOException {
        return scaleImage(image, targetWidth, targetHeight, null);
    }
    
    @Override
    public BufferedImage scaleImage(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException {
        options = getEffectiveScalingOptions(image, targetWidth, targetHeight, options);
        if (!requiresScaling(image, targetWidth, targetHeight, options)) return image;
        if (ImageUtil.debugOn()) return scaleImageDebug(image, targetWidth, targetHeight, options);
        else return scaleImageCore(image, targetWidth, targetHeight, options);
    }
    
    protected boolean requiresScaling(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) {
        if (Boolean.TRUE.equals(getForceOp(options))) {
            return true;
        }
        
        // SPECIAL CASE: by default, in all cases, if the image is same dimensions, we'll just return as-is
        if (image.getWidth() == targetWidth && image.getHeight() == targetHeight) {
            if (ImageUtil.verboseOn()) Debug.logInfo("Not scaling image; output dimensions match input (" + targetWidth + "x" + targetHeight + ")", module);
            return false;
        }
        else return true;
    }
    
    protected Map<String, Object> getEffectiveScalingOptions(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) {
        return getEffectiveOptions(options);
    }
    
    protected BufferedImage scaleImageDebug(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException {
        long startTime = System.nanoTime();
        BufferedImage resultImage = scaleImageCore(image, targetWidth, targetHeight, options);
        long endTime = System.nanoTime();
        Debug.logInfo("Scaled image in " + ((endTime - startTime) / 1000000) + "ms using " + this.toString(options)
            + "; input type: " + ImageType.printImageTypeInfo(image) 
            + "; output type: " + ImageType.printImageTypeInfo(resultImage), module);
        return resultImage;
    }
        
    /**
     * Calls the core image scaling, with option arguments merged with the ones from this instance.
     * options = confDefOptions + passedOptions = defOptions + confOptions + passedOptions
     */
    protected abstract BufferedImage scaleImageCore(BufferedImage image, int targetWidth, int targetHeight, Map<String, Object> options) throws IOException;
    
    @SuppressWarnings("unchecked")
    @Override
    public AbstractImageScalerFactory<? extends AbstractImageScaler> getFactory() {
        return (AbstractImageScalerFactory<? extends AbstractImageScaler>) factory;
    }

    protected static String getFilterMapLogRepr(String apiName, Map<String, ?> filterMap) {
        StringBuilder sb = new StringBuilder("Image scaler [" + apiName + "] supported filters:\n");
        for(Map.Entry<String, ?> entry : filterMap.entrySet()) {
            sb.append(entry.getKey());
            sb.append(" -> ");
            sb.append(entry.getValue()+"");
            sb.append("\n");
        }
        return sb.toString();
    }
    
    // NOTE: ugly 2 parameters required to keep hierarchy consistent
    public static abstract class AbstractImageScalerFactory<T extends AbstractImageScaler> extends AbstractImageOpFactory<AbstractImageScaler, ImageScaler> implements ImageScalerFactory {
        
        protected void putCommonImageTypeOptions(Map<String, Object> destOptions, Map<String, Object> srcOptions) {
            putOption(destOptions, "overridetype", getImageTypeOption(srcOptions, "overridetype"), srcOptions);
            putOption(destOptions, "targettype", getImageTypeOption(srcOptions, "targettype"), srcOptions);
            putOption(destOptions, "defaulttype", getImageTypeOption(srcOptions, "defaulttype"), srcOptions);
            putOption(destOptions, "fallbacktype", getImageTypeOption(srcOptions, "fallbacktype"), srcOptions);
        }
        
    }
    
    protected static ImageType getMergedTargetImageType(Map<String, Object> options) {
        ImageType type = getImageTypeOption(options, "overridetype");
        if (type != null) return type;
        type = getImageTypeOption(options, "targettype");
        if (type != null) return type;
        return getImageTypeOption(options, "defaulttype");
    }
    
    protected static Integer getMergedTargetImagePixelType(Map<String, Object> options, BufferedImage srcImage) {
        ImageType imageType = getMergedTargetImageType(options);
        return imageType != null ? imageType.getPixelTypeFor(srcImage) : null;
    }
    
    protected static Integer getFallbackImagePixelType(Map<String, Object> options, BufferedImage srcImage) {
        ImageType imageType = getImageTypeOption(options, "fallbacktype");
        return imageType != null ? imageType.getPixelTypeFor(srcImage) : null;
    }
    
    /**
     * Returns default or fallback pixel type or null if they are set to special value, or not native-supported.
     * Intended for scalers that can't write out to indexed/custom types (basically all of them).
     * DEV NOTE: WARN: be careful before changing this (you may have to copy-paste it).
     */
    protected Integer getDefaultOrFallbackRegularNativeSupportedDestPixelType(Map<String, Object> options, Integer fallbackType, BufferedImage srcImage) {
        Integer defaultType = getImagePixelTypeOption(options, "defaulttype", srcImage);
        if (defaultType != null && !ImagePixelType.isTypeSpecial(defaultType) && 
                isNativeSupportedDestImagePixelType(defaultType)) {
            return defaultType;
        } else {
            // check fallback
            if (fallbackType != null && !ImagePixelType.isTypeSpecial(fallbackType) && 
                    isNativeSupportedDestImagePixelType(fallbackType)) {
                return fallbackType;
            } else {
                return null;
            }
        }
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
    protected boolean isPostConvertResultImage(BufferedImage srcImage, Map<String, Object> options, Integer targetPixelType) {
        Integer effTargetPixelType = ImagePixelType.resolveTargetType(targetPixelType, srcImage);
        
        // DO NOT convert to indexed if lossless mode (TODO: REVIEW)
        if (ImagePixelType.isTypeIndexedOrCustom(effTargetPixelType) && targetPixelType == ImagePixelType.TYPE_PRESERVE_IF_LOSSLESS)
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
            Map<String, Object> options, Integer targetPixelType) {
        if (targetPixelType == null || targetPixelType == ImagePixelType.TYPE_NOPRESERVE) return modifiedImage;
        Integer effTargetPixelType = ImagePixelType.resolveTargetType(targetPixelType, srcImage);

        // APPROXIMATION of an image type equality check
        if (modifiedImage.getType() == effTargetPixelType) return modifiedImage;
        
        // DO NOT convert to indexed if lossless mode (TODO: REVIEW)
        if (ImagePixelType.isTypeIndexedOrCustom(effTargetPixelType) && targetPixelType == ImagePixelType.TYPE_PRESERVE_IF_LOSSLESS)
            return modifiedImage;
        
        // TODO?: other missing type conversion loss checks...
        
        BufferedImage resultImage;
        // FIXME?: we make the assumption that if the type is the same, it means all other parameters
        // about color space and data should be preserved from the original - but this is flawed assumption
        if (effTargetPixelType == srcImage.getType()) {
            // ALTERNATIVE implementation (not as good):
            //resultImage = ImageTransform.createBufferedImage(modifiedImage.getWidth(), modifiedImage.getHeight(), targetPixelType, srcImage.getColorModel());
            resultImage = ImageTransform.createCompatibleBufferedImage(srcImage, modifiedImage.getWidth(), modifiedImage.getHeight());
        } else {
            resultImage = ImageTransform.createBufferedImage(modifiedImage.getWidth(), modifiedImage.getHeight(), effTargetPixelType, null);
        }

        // WARN: this may not be respected by Graphics2D.drawImage - see copyToBufferedImage comments
        RenderingHints renderingHints = ImageTransform.getNoDitheringRenderingHintsIfIndexed(resultImage.getType());
        
        if (ImageUtil.verboseOn()) 
            Debug.logInfo("Applying required image pixel type post-op conversion ("
                    + "input type: " + ImageType.printImageTypeInfo(srcImage) 
                    + "; post-op type: " + ImageType.printImageTypeInfo(modifiedImage)
                    + "; target type: " + ImageType.printImageTypeInfo(resultImage)
                    + ")", module);

        ImageTransform.copyToBufferedImage(modifiedImage, resultImage, renderingHints);
        return resultImage;
    }

}

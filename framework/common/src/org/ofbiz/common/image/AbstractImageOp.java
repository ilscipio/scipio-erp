package org.ofbiz.common.image;

import java.awt.image.BufferedImage;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * SCIPIO: Common image operation base class (scaling, etc.), does all the boilerplate stuff.
 * Added 2017-07-10.
 */
public abstract class AbstractImageOp implements ImageOp {
    public static final String module = AbstractImageOp.class.getName();
    
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
    }

    /**
     * Parses (if needed) and returns an ImageType option from the options map.
     * This always returns an instance except when the map does not contain the field name at all,
     * or when parsing error (exception).
     */
    public static ImageType getImageTypeOption(Map<String, Object> options, String fieldName) {
        try {
            if (!options.containsKey(fieldName)) return null;
            else return ImageType.getForObject(options.get(fieldName));
        } catch(Exception e) {
            throw new IllegalArgumentException("Invalid image type option " + fieldName + ": " + e.getMessage(), e);
        }
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
    
}

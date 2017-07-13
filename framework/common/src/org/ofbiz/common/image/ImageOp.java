package org.ofbiz.common.image;

import java.util.Map;

/**
 * SCIPIO: Image operation base interface (scaling, etc.).
 * Added 2017-07-10.
 */
public interface ImageOp {

    String getName();
    
    Map<String, Object> getConfiguredOptions();
    Map<String, Object> getDefaultOptions();
    Map<String, Object> getConfiguredAndDefaultOptions();
    
    /**
     * Returns a new Map with options parsed in the format recognized by
     * this ImageOp. In other words, converts strings to numbers, discards unrecognized, etc.
     * <p>
     * This is a convenience method for {@link #getFactory()} + {@link ImageOpFactory#makeValidOptions(Map)}.
     */
    Map<String, Object> makeValidOptions(Map<String, Object> options);
    
    /**
     * Returns the factory that created this instance.
     * NOTE: best to avoid this in client code; usually requires casting.
     */
    ImageOpFactory<?> getFactory();
    
    // DEV NOTE: The T parameter is intended to be a sub-interface like ImageScaler, NOT an implementing or abstract class.
    public interface ImageOpFactory<T extends ImageOp> {
        /**
         * Returns new ImageOp instance.
         * The defaultScalingOptions may be in non-validated format (e.g. strings instead of ints, where applicable).
         */
        T getImageOpInst(String name, Map<String, Object> defaultScalingOptions);
        
        /**
         * Returns new ImageOp instance.
         * The defaultScalingOptions must be in validated format and types, in other words
         * must have been processed by {@link #makeValidOptions(Map)}.
         */
        T getImageOpInstStrict(String name, Map<String, Object> defaultScalingOptions);
        
        /**
         * Derives or extends the given op, usually simply replacing its default options.
         * The defaultScalingOptions may be in non-validated format.
         */
        T getDerivedImageOpInst(String name, Map<String, Object> defaultScalingOptions, ImageOp other);
        
        /**
         * Returns a new Map with options parsed in the format recognized by
         * this ImageOp. In other words, converts strings to numbers, discards unrecognized, etc.
         */
        Map<String, Object> makeValidOptions(Map<String, Object> options);
        
        Map<String, Object> getDefaultOptions();
    }
}

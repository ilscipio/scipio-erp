package org.ofbiz.common.image;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/**
 * SCIPIO: Common image operation base class (scaling, etc.), does all the boilerplate stuff.
 * Added 2017-07-10.
 */
public abstract class AbstractImageOp implements ImageOp {

    protected final AbstractImageOpFactory<? extends AbstractImageOp, ? extends ImageOp> factory;
    protected final String name;
    protected final Map<String, Object> defaultOptions;
    
    public AbstractImageOp(AbstractImageOpFactory<? extends AbstractImageOp, ? extends ImageOp> factory, String name, Map<String, Object> defaultOptions) {
        this.factory = factory;
        this.name = name;
        this.defaultOptions = defaultOptions != null ? Collections.unmodifiableMap(
                new HashMap<String, Object>(defaultOptions)) : Collections.<String, Object>emptyMap();
    }

    @Override
    public String getName() {
        return name;
    }

    @Override
    public Map<String, Object> getDefaultOptions() {
        return defaultOptions;
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
        if (options == null || options.isEmpty()) return defaultOptions;
        else if (defaultOptions.isEmpty()) return options;
        else {
            Map<String, Object> mergedMap = new HashMap<>(defaultOptions);
            mergedMap.putAll(options);
            return mergedMap;
        }
    }
    
    // NOTE: this needs two params otherwise the multiple inherit hierarchy breaks
    public static abstract class AbstractImageOpFactory<T extends AbstractImageOp, V extends ImageOp> implements ImageOpFactory<V> {
        @Override
        public V getImageOpInst(String name, Map<String, Object> defaultScalingOptions) {
            return getImageOpInstStrict(name, makeValidOptions(defaultScalingOptions));
        }

        @Override
        public V getDerivedImageOpInst(String name, Map<String, Object> defaultScalingOptions, ImageOp other) {
            Map<String, Object> mergedOptions = new HashMap<>(other.getDefaultOptions());
            if (defaultScalingOptions != null) {
                mergedOptions.putAll(defaultScalingOptions);
            }
            return getImageOpInst(name, mergedOptions);
        }
        
        protected abstract String getApiName();
    }

}

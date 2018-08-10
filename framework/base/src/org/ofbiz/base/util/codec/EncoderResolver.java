package org.ofbiz.base.util.codec;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.TreeSet;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilCodec.SimpleEncoder;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.codec.EncoderFactory.EncoderSource;

/**
 * SCIPIO: for use with utilcodec.properties.
 * NOT thread-safe.
 * Added 2018-06-11.
 */
public class EncoderResolver implements EncoderSource {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final Map<String, EncoderFactory> encoderFactories;
    private final Map<String, Map<String, String>> encoderConfigs;
    private final Map<String, SimpleEncoder> encoders = new HashMap<>();
    
    public EncoderResolver(Map<String, EncoderFactory> encoderFactories,
            Map<String, Map<String, String>> encoderConfigs) {
        this.encoderFactories = encoderFactories;
        this.encoderConfigs = encoderConfigs;
    }

    public static Map<String, SimpleEncoder> resolveFromProperties(String resource, String prefix) {
        Map<String, Map<String, String>> encoderConfigs = new HashMap<>();
        UtilProperties.extractPropertiesWithPrefixAndId(encoderConfigs, UtilProperties.getProperties(resource), prefix);

        Map<String, EncoderFactory> encoderFactories = new HashMap<>();
        for(Map.Entry<String, Map<String, String>> entry : encoderConfigs.entrySet()) {
            String name = entry.getKey();
            String factoryClass = entry.getValue().get("factoryClass");
            if (UtilValidate.isEmpty(factoryClass)) {
                Debug.logError("Error in '" + resource + "' properties configuration: "
                        + " entry '" + name + "' missing factoryClass", module);
                continue;
            }
            try {
                @SuppressWarnings("unchecked")
                Class<? extends EncoderFactory> factoryCls = (Class<? extends EncoderFactory>) Thread.currentThread().getContextClassLoader()
                    .loadClass(factoryClass);
                encoderFactories.put(name, factoryCls.newInstance());
            } catch (Exception e) {
                Debug.logError(e, "Error in '" + resource + "' properties configuration: "
                        + " invalid factoryClass for entry '" + name + "'", module);
                continue;
            }
        }

        EncoderResolver resolver = new EncoderResolver(encoderFactories, encoderConfigs);
        Map<String, SimpleEncoder> encoders = resolver.resolveEncoders();
        Debug.logInfo("Resolved " + encoders.size() + " policy sanitizers from '" + resource 
            + "' properties: " + new TreeSet<>(encoders.keySet()), module);
        return encoders;
    }


    public Map<String, EncoderFactory> getEncoderFactories() {
        return encoderFactories;
    }

    public Map<String, Map<String, String>> getEncoderConfigs() {
        return encoderConfigs;
    }

    @Override
    public Map<String, String> getEncoderConfig(String name) {
        return encoderConfigs.get(name);
    }

    public SimpleEncoder getEncoder(String name) {
        SimpleEncoder encoder = encoders.get(name);
        if (encoder != null || encoders.containsKey(name)) { // NOTE: null means already tried and failed
            return encoder; // 
        }
        encoder = createEncoder(name);
        encoders.put(name, encoder);
        return encoder;
    }
    
    protected SimpleEncoder createEncoder(String name) {
        SimpleEncoder encoder = null;
        EncoderFactory factory = encoderFactories.get(name);
        if (factory != null) {
            try {
                encoder = factory.createEncoder(name, encoderConfigs.get(name), this);
                encoders.put(name, encoder);
                if (encoder == null) throw new IllegalStateException("Null encoder was returned");
            } catch(Exception e) {
                Debug.logError(e, "Could not initialize sanitizer '" + name  + "' using factory "
                        + factory.getClass().getName() + ": " + e.getMessage(), module);
            }
        } else {
            Debug.logError("Tried to initialize sanitizer '" + name + "', but has no factory! Should not happen!", module);
        }
        return encoder;
    }

    public Map<String, SimpleEncoder> resolveEncoders() {
        Set<String> failedEncoders = new HashSet<>();
        for(String name : encoderFactories.keySet()) {
            SimpleEncoder encoder = getEncoder(name);
            if (encoder == null) failedEncoders.add(name);
        }
        for(String name : failedEncoders) {
            encoders.remove(name);
        }
        return encoders;
    }
}

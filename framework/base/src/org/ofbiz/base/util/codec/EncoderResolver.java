package org.ofbiz.base.util.codec;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilCodec.SimpleEncoder;
import org.ofbiz.base.util.codec.EncoderFactory.EncoderSource;

/**
 * SCIPIO: for use with owasp.properties.
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
        
        Map<String, Map<String, String>> encoderLangs = new HashMap<>();
        UtilProperties.extractPropertiesWithPrefixAndId(encoderLangs, UtilProperties.getProperties(resource), prefix);
        for(Map.Entry<String, Map<String, String>> entry : encoderLangs.entrySet()) {
            Map<String, Map<String, String>> subEncoderConfigs = new HashMap<>();
            UtilProperties.extractPropertiesWithPrefixAndId(subEncoderConfigs, entry.getValue(), "");
            for(Map.Entry<String, Map<String, String>> subEntry : subEncoderConfigs.entrySet()) {
                encoderConfigs.put(entry.getKey() + "-" + subEntry.getKey(), subEntry.getValue());
            }
        }

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
        Debug.logInfo("Resolved " + encoders.size()
            + " policy sanitizers from '" + resource + "' properties: " + encoders.keySet(), module);
        return encoders;
    }


    public Map<String, EncoderFactory> getEncoderFactories() {
        return encoderFactories;
    }

    public Map<String, Map<String, String>> getEncoderConfigs() {
        return encoderConfigs;
    }

    public SimpleEncoder getEncoder(String name) {
        if (encoders.containsKey(name)) return null;
        SimpleEncoder encoder = encoders.get(name);
        EncoderFactory factory = encoderFactories.get(name);
        if (factory != null) {
            try {
                encoder = factory.createEncoder(encoderConfigs.get(name), this);
                encoders.put(name, encoder);
                if (encoder == null) {
                    Debug.logError("Could not initialize sanitizer '"
                            + name + "' using factory " + factory.getClass().getName() 
                            + ": no encoder returned", module);
                }
            } catch(Exception e) {
                Debug.logError(e, "Could not initialize sanitizer '"
                    + name + "' using factory " + factory.getClass().getName(), module);
                encoders.put(name, null);
            }
        } else {
            Debug.logWarning("Tried to initialize sanitizer '" + name + "', but has no factory", module);
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

    @Override
    public Map<String, String> getEncoderConfig(String name) {
        return encoderConfigs.get(name);
    }
}

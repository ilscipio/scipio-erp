package org.ofbiz.common.image;

import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.common.image.ImageOp.ImageOpFactory;

/**
 * SCIPIO: Generic image util.
 * Added 2017-07-10.
 */
public abstract class ImageUtil {
    public static final String module = ImageUtil.class.getName();
    
    protected ImageUtil() {
    }

    public static Map<String, Object> readImagePropOptions(Properties props, String optionPropPrefix) {
        Map<String, Object> options = new HashMap<>();
        try {
            UtilProperties.putPropertiesWithPrefixSuffix(options, props, optionPropPrefix, null, true, false, false);
        } catch(Exception e) {
            Debug.logError(e, "Unable to load image op options from options properties with prefix " + optionPropPrefix + ": " + e.getMessage(), module);
        }
        return options;
    }
    
    public static <T extends ImageOp> Map<String, T> readImagePropsToImageOpMap(Properties props, String propPrefix, Class<T> imageOpCls) {
        Map<String, T> scalerMap = new HashMap<>();
        
        // get the real factories
        Set<String> factoryEntries = UtilProperties.getPropertyNamesWithPrefixSuffix(props, propPrefix, ".factoryClass", false, false, false);
        for(String factoryEntry : factoryEntries) {
            String name = factoryEntry;
            String factoryClass = props.getProperty(propPrefix+factoryEntry+".factoryClass");
            ImageOpFactory<T> factory;
            try {
                @SuppressWarnings("unchecked")
                Class<ImageOpFactory<T>> cls = (Class<ImageOpFactory<T>>) Thread.currentThread().getContextClassLoader().loadClass(factoryClass);
                factory = cls.newInstance();
            } catch (Exception e) {
                Debug.logError(e, "Unable to load image op factory from factory property " + propPrefix+factoryEntry+".factoryClass: " + e.getMessage(), module);
                continue;
            }
            Map<String, Object> defaultScalingOptions = readImagePropOptions(props, propPrefix+factoryEntry+"options.");
            T scaler;
            try {
                scaler = factory.getImageOpInst(name, defaultScalingOptions);
                if (!imageOpCls.isAssignableFrom(scaler.getClass())) {
                    throw new IllegalArgumentException("Invalid or broken image op factory: factory [" + scaler.getClass().getName() 
                            + "] did not produce image op instance of expected type [" + imageOpCls.getClass().getName() 
                            + "]; instead got instance of type [" + scaler.getClass().getName() + "]");
                }
            } catch (Exception e) {
                Debug.logError(e, "Unable to instantiate image op class from factory property " + propPrefix+factoryEntry+".factoryClass: " + e.getMessage(), module);
                continue;
            }
            scalerMap.put(name, scaler);
        }
        
        // resolve the aliases
        Set<String> aliasEntries = UtilProperties.getPropertyNamesWithPrefixSuffix(props, propPrefix, ".alias", false, false, false);
        for(String aliasEntry : aliasEntries) {
            String name = aliasEntry;
            String aliasName = props.getProperty(propPrefix+aliasEntry+".alias");
            T scaler = scalerMap.get(aliasName);
            if (scaler != null) {
                Map<String, Object> defaultScalingOptions = readImagePropOptions(props, propPrefix+aliasEntry+"options.");
                try {
                    // TODO: REVIEW: currently passing the aliasName instance of name, not sure which is best
                    @SuppressWarnings("unchecked")
                    T newScaler = (T) scaler.getFactory().getDerivedImageOpInst(aliasName, defaultScalingOptions, scaler);
                    scalerMap.put(name, newScaler);
                } catch(Exception e) {
                    Debug.logError("Could not instantiate image op instance " + aliasName + " for alias property " + propPrefix+aliasEntry+".alias: " + e.getMessage(), module);
                    continue;
                }
            } else {
                Debug.logError("Could not find image op instance " + aliasName + " for alias property " + propPrefix+aliasEntry+".alias", module);
                continue;
            }
        }
        
        return scalerMap;
    }
    
}

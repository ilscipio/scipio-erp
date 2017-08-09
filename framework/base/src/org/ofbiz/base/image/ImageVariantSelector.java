package org.ofbiz.base.image;

import java.io.IOException;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;

/**
 * SCIPIO: Image variant selector interface and factory - WORKAROUND.
 * This exists to get around a dependency problem between the stock ofbiz components
 * related to image classes, which currently mostly reside in the common package.
 * TODO: REVIEW: doesn't hurt to have some interfaces, but would have been simpler
 * if didn't need this...
 * Added 2017-08-08.
 */
public interface ImageVariantSelector {

    VariantInfo getCanvasBestFitVariant(String mode, Integer width, Integer height);
    
    public interface VariantInfo {
        String getName();
        int getWidth();
        int getHeight();
    }
    
    public interface Factory {
        
        /**
         * Gets a variant selector from a pre-parsed ImageProperties.xml file as map of string entries.
         */
        ImageVariantSelector fromImagePropertiesMap(String name, String sourceType, String location, Map<String, Map<String, String>> imgPropsMap);
        
        /**
         * @param location
         * @param locale
         * @return
         * @throws IOException
         * @throws IllegalArgumentException
         */
        ImageVariantSelector fromImagePropertiesXml(String imgPropsPath, Locale locale) throws IOException, IllegalArgumentException;
        
        ImageVariantSelector fromImagePropertiesXml(String imgPropsPath) throws IOException;
        
        /**
         * Best-effort attempt to get a variant selector for the specific resource's URL path, e.g.,
         * an image link such as /images/somedir/myimage.jpg. See implementations for caveats.
         */
        ImageVariantSelector fromResourceUrlPath(String path) throws IOException;
        
    }
    
    /**
     * For use in configuration files (to bypass dependencies).
     */
    public interface FactorySource {
        Factory getFactory();
    }
    
    public static class FactoryUtil {
        
        public static final String module = FactoryUtil.class.getName();
        
        public static Factory getFactoryFromSource(String factorySourceClass) throws InstantiationException, IllegalAccessException, ClassNotFoundException {
            return ((FactorySource) Thread.currentThread().getContextClassLoader().loadClass(factorySourceClass).newInstance()).getFactory();
        }
        
        public static Factory getFactoryFromPropertySafe(String resource, String name, String defaultVal) {
            String factorySourceClass = null;
            try {
                factorySourceClass = UtilProperties.getPropertyValue(resource, name);
                if (UtilValidate.isEmpty(factorySourceClass)) factorySourceClass = defaultVal;
                return getFactoryFromSource(factorySourceClass);
            } catch(Exception e) {
                Debug.logError(e, "Unable to get image variant selector factory from property [" + resource + "/" + name + "]: " + e.getMessage(), module);
                if (defaultVal != null && !defaultVal.equals(factorySourceClass)) {
                    try {
                        return getFactoryFromSource(defaultVal);
                    } catch(Exception e1) {
                        Debug.logError(e1, "Unable to get image variant selector factory default: " + defaultVal + ": " + e1.getMessage(), module);
                        return null;
                    }
                } else {
                    return null;
                }
            }
        }
    }
}

package org.ofbiz.common.image;

import java.io.IOException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.image.ImageOp.ImageOpFactory;

import com.ilscipio.scipio.ce.build.util.DependencyGraph;

/**
 * SCIPIO: Generic image util.
 * Added 2017-07-10.
 */
public abstract class ImageUtil {
    public static final String module = ImageUtil.class.getName();
    public static final String IMAGECOMMON_PROP_RESOURCE = "imagecommon.properties";
    public static final String IMAGECOMMON_PROP_PREFIX = "image.";
    public static final String IMAGEOP_PROP_RESOURCE = "imageops.properties";
    public static final String IMAGEOP_PROP_PREFIX = "image.";
    
    private static boolean DEBUG = UtilProperties.getPropertyAsBoolean(IMAGECOMMON_PROP_RESOURCE, 
            IMAGECOMMON_PROP_PREFIX+"debug", false) || Debug.verboseOn(); // FIXME?: should really read verboseOn dynamically, not cache the value here...
    
    protected ImageUtil() {
    }

    public static boolean debugOn() {
        return DEBUG;
    }
    
    public static boolean verboseOn() {
        return DEBUG;
    }
    
    public static Map<String, Object> readImagePropOptions(Properties props, String optionPropPrefix, Map<String, Object> options) {
        try {
            UtilProperties.putPropertiesWithPrefixSuffix(options, props, optionPropPrefix, null, true, false, false);
        } catch(Exception e) {
            Debug.logError(e, "Unable to load image op options from options properties with prefix " + optionPropPrefix + ": " + e.getMessage(), module);
        }
        return options;
    }
    
    public static Map<String, Object> readImagePropOptions(Map<String, Object> props, String optionPropPrefix, Map<String, Object> options) {
        for(Map.Entry<String, Object> entry : props.entrySet()) {
            if (entry.getKey().startsWith(optionPropPrefix)) {
                options.put(entry.getKey().substring(optionPropPrefix.length()), entry.getValue());
            }
        }
        return options;
    }
    
    public static <T extends ImageOp> Map<String, T> readImagePropsToImageOpMap(Properties props, String propPrefix, Class<T> imageOpCls) {
        return readImagePropsToImageOpMap(Arrays.asList(new Properties[]{props}), propPrefix, imageOpCls);
    }
    
    public static <T extends ImageOp> Map<String, T> readImagePropsToImageOpMap(Collection<Properties> propsList, String propPrefix, Class<T> imageOpCls) {
        Map<String, T> imageOpMap = new LinkedHashMap<>();
        
        // first, get the real factories across all files, and simply replace in order found
        for(Properties props : propsList) {
            Set<String> factoryEntries = UtilProperties.getPropertyNamesWithPrefixSuffix(props, propPrefix, ".factoryClass", false, false, false);
            for(String factoryEntry : factoryEntries) {
                String name = factoryEntry;
                String factoryClass = props.getProperty(propPrefix+factoryEntry+".factoryClass");
                if (factoryClass != null) factoryClass.trim();
                if (UtilValidate.isEmpty(factoryClass)) {
                    Debug.logWarning("Empty image op factoryClass property: " + propPrefix+factoryEntry+".factoryClass", module);
                    continue;
                }
                ImageOpFactory<T> factory;
                try {
                    @SuppressWarnings("unchecked")
                    Class<ImageOpFactory<T>> cls = (Class<ImageOpFactory<T>>) Thread.currentThread().getContextClassLoader().loadClass(factoryClass);
                    factory = cls.newInstance();
                } catch (Exception e) {
                    Debug.logError(e, "Unable to load image op factory from factory property " + propPrefix+factoryEntry+".factoryClass: " + e.getMessage(), module);
                    continue;
                }
                Map<String, Object> defaultOptions = readImagePropOptions(props, propPrefix+factoryEntry+".options.", new LinkedHashMap<String, Object>());
                T scaler;
                try {
                    scaler = factory.getImageOpInst(name, defaultOptions);
                    if (!imageOpCls.isAssignableFrom(scaler.getClass())) {
                        throw new IllegalArgumentException("Invalid or broken image op factory: factory [" + scaler.getClass().getName() 
                                + "] did not produce image op instance of expected type [" + imageOpCls.getClass().getName() 
                                + "]; instead got instance of type [" + scaler.getClass().getName() + "]");
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Unable to instantiate image op class from factory property " + propPrefix+factoryEntry+".factoryClass: " + e.getMessage(), module);
                    continue;
                }
                imageOpMap.put(name, scaler);
            }
        }
        
        
        // for aliases, read them all and put them through dependency resolution, to avoid headaches
        Map<String, List<String>> depMap = new LinkedHashMap<>();
        // must add the factories first
        for(String key : imageOpMap.keySet()) {
            depMap.put(key, Collections.<String>emptyList());
        }
        
        Map<String, Properties> aliasPropsMap = new HashMap<>(); // back-pointers to the orig Properties for each alias def
        
        for(Properties props : propsList) {
            // resolve the aliases
            Set<String> aliasEntries = UtilProperties.getPropertyNamesWithPrefixSuffix(props, propPrefix, ".alias", false, false, false);
            for(String aliasEntry : aliasEntries) {
                String name = aliasEntry;
                String aliasName = props.getProperty(propPrefix+aliasEntry+".alias");
                if (aliasName != null) aliasName = aliasName.trim();
                if (UtilValidate.isEmpty(aliasName)) {
                    Debug.logWarning("Empty image op alias property: " + propPrefix+aliasEntry+".alias", module);
                    continue;
                }
                depMap.put(name, Arrays.asList(new String[]{aliasName}));
                aliasPropsMap.put(name, props);
            }
        }
        
        List<String> allOrdered;
        try {
            DependencyGraph<String> aliasDepGraph = new DependencyGraph<>(depMap);
            allOrdered = aliasDepGraph.getResolvedDependenciesDfs();
        } catch(Exception e) {
            Debug.logError(e, "Unable to resolve image op properties alias entries: "
                    + "please verify configuration and make sure no dangling or circular aliases: " + e.getMessage(), module);
            return imageOpMap; // abort
        }
        
        for(String name : allOrdered) {
            Properties props = aliasPropsMap.get(name);
            if (props == null) continue; // this skips the factories
            String aliasEntry = name;
            String aliasName = depMap.get(name).get(0);
            T scaler = imageOpMap.get(aliasName);
            if (scaler != null) {
                Map<String, Object> defaultOptions = readImagePropOptions(props, propPrefix+aliasEntry+".options.", new LinkedHashMap<String, Object>());
                try {
                    // TODO: REVIEW: currently passing the aliasName instance of name, not sure which is best
                    @SuppressWarnings("unchecked")
                    T newScaler = (T) scaler.getFactory().getDerivedImageOpInst(name, defaultOptions, scaler);
                    imageOpMap.put(name, newScaler);
                } catch(Exception e) {
                    Debug.logError("Could not instantiate image op instance " + aliasName + " for alias property " + propPrefix+aliasEntry+".alias: " + e.getMessage(), module);
                    continue;
                }
            } else {
                Debug.logError("Could not find image op instance " + aliasName + " for alias property " + propPrefix+aliasEntry+".alias", module);
                continue;
            }
        }
        
        if (verboseOn()) {
            StringBuilder sb = new StringBuilder("Image op properties resolved config ImageOp map:\n");
            Debug.logInfo(printImageOpMap(sb, imageOpMap, "\n").toString(), module);
        }
        return imageOpMap;
    }
    
    public static StringBuilder printImageOpMap(StringBuilder sb, Map<String, ? extends ImageOp> imageOpMap, String sep) {
        for(Map.Entry<String, ? extends ImageOp> entry : imageOpMap.entrySet()) {
            sb.append(entry.getKey());
            sb.append(" -> ");
            sb.append(entry.getValue().toString());
            sb.append(sep);
        }
        return sb;
    }
    
    public static Collection<Properties> getAllPropertiesFiles(String resource) {
        if (!resource.endsWith(".properties")) resource = resource+".properties";
        ClassLoader loader = Thread.currentThread().getContextClassLoader();
        Enumeration<URL> resources;
        try {
            resources = loader.getResources(resource);
        } catch (IOException e) {
            Debug.logError(e, "Could not load any properties for "+resource + ": " + e.getMessage(), module);
            return Collections.emptyList();
        }
        List<Properties> propsList = new ArrayList<>();
        while (resources.hasMoreElements()) {
            URL propertyURL = resources.nextElement();
            Debug.logInfo("loading properties: " + propertyURL, module);
            Properties props = UtilProperties.getProperties(propertyURL);
            if (props == null) {
                Debug.logError("Unable to load properties file " + propertyURL, module);
            } else {
                propsList.add(props);
            }
        }
        return propsList;
    }
    
    public static Collection<Properties> getSinglePropertiesFile(String resource) {
        return Arrays.asList(new Properties[]{UtilProperties.getProperties(resource)});
    }
    
    public static Map<String, Object> makeOptions() {
        return new HashMap<String, Object>(); 
    }
    
    public static Map<String, Object> copyOptions(Map<String, Object> options) {
        return options != null ? new HashMap<String, Object>(options) : new HashMap<String, Object>(); 
    }
    
    /**
     * Adds an image option to the options map and returns.
     * If options were null, returns a new map (result never null).
     */
    public static Map<String, Object> addImageOpOption(Map<String, Object> options, String name, Object value) {
        if (options == null) options = new HashMap<>();
        options.put(name, value);
        return options;
    }
    
    /**
     * Adds an image option to the options map without modifying the original, and returns the new one,
     * but only if the map did not already contain the name as key.
     * NOTE: null value is significant.
     * If options were null, returns a new map (result never null).
     * TODO: REVIEW: need LinkedHashMap?
     */
    public static Map<String, Object> addImageOpOptionIfNotSet(Map<String, Object> options, String name, Object value) {
        if (options == null) {
            options = new HashMap<>();
            options.put(name, value);
        } else if (!options.containsKey(name)) {
            options.put(name, value);
        }
        return options;
    }
    
    /**
     * Similar to {@link #addImageOpOptionIfNotSet}, but only sets the option if neither the incoming options
     * nor the ImageOp's defaults already contain the option.
     * This allows to set defaults that have lower priority than both incoming and config options.
     */
    public static Map<String, Object> addImageOpOptionIfDefaultNotSet(Map<String, Object> options, String name, Object value, ImageOp imageOpInst) {
        if (options == null) {
            options = new HashMap<>();
            if (!imageOpInst.getConfiguredAndDefaultOptions().containsKey(name)) {
                options.put(name, value);
            }
        } else if (!options.containsKey(name)) {
            if (!imageOpInst.getConfiguredAndDefaultOptions().containsKey(name)) {
                options.put(name, value);
            }
        }
        return options;
    }
}

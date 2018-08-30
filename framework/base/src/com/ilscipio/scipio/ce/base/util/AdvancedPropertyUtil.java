package com.ilscipio.scipio.ce.base.util;

import java.io.InputStream;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.ofbiz.base.start.StartupLoader;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilProperties;

public abstract class AdvancedPropertyUtil {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    protected AdvancedPropertyUtil() {
    }

    private static final Pattern numberedNameSuffixPat = Pattern.compile("^(\\d+)(\\.(.+))?$");
    
    /**
     * Reads flexible class definitions from a single Properties object.
     * <p>
     * Supports two formats:
     * <ul>
     * <li>[prefix].[name].class=[class] / [prefix].[name].prio=[priority] (name-based format)</li>
     * <li>[prefix][priority]=[class] / [prefix][priority].name=[name] (priority-based start.properties format)</li>
     * </ul>
     * <p>
     * This returns as Map, which must be converted afterward to {@link ClassDef} or other.
     */
    public static void readClassDefMaps(Map<String, Map<String, String>> outLoaderMapDefs, Properties properties, String propPrefix) {
        for(String propName : properties.stringPropertyNames()) {
            if ((propName.length() > propPrefix.length()) && propName.startsWith(propPrefix)) {
                if (propName.charAt(propPrefix.length()) == '.') {
                    // name-based format
                    String handlerName = propName.substring(propPrefix.length() + 1);
                    if (handlerName.length() > 0) {
                        String subPropName;
                        int nameDotIndex = handlerName.indexOf('.');
                        if (nameDotIndex < 0) {
                            subPropName = "class";
                        } else {
                            subPropName = handlerName.substring(nameDotIndex + 1);
                            handlerName = handlerName.substring(0, nameDotIndex);
                            if (handlerName.isEmpty() || subPropName.isEmpty()) {
                                continue;
                            }
                        }
                        Map<String, String> defs = outLoaderMapDefs.get(handlerName);
                        if (defs == null) {
                            defs = new HashMap<>();
                            outLoaderMapDefs.put(handlerName, defs);
                        }
                        if (!defs.containsKey("name")) {
                            defs.put("name", handlerName);
                        }
                        defs.put(subPropName, properties.getProperty(propName));
                    }
                } else {
                    // priority-based start.properties format
                    Matcher m = numberedNameSuffixPat.matcher(propName.substring(propPrefix.length()));
                    if (m.matches()) {
                        String priority = m.group(1);

                        // get handler name; if no explicit name, use class name
                        String handlerName = properties.getProperty(propPrefix+priority+".name");
                        if (handlerName == null || handlerName.isEmpty()) {
                            handlerName = properties.getProperty(propPrefix+priority+".class");
                            if (handlerName == null || handlerName.isEmpty()) {
                                handlerName = properties.getProperty(propPrefix+priority);
                                if (handlerName == null || handlerName.isEmpty()) {
                                    continue;
                                }
                            }
                        }

                        Map<String, String> defs = outLoaderMapDefs.get(handlerName);
                        if (defs == null) {
                            defs = new HashMap<>();
                            outLoaderMapDefs.put(handlerName, defs);
                        }
                        if (m.group(2) == null || m.group(2).isEmpty()) {
                            defs.put("class", properties.getProperty(propName));
                        } else {
                            defs.put(m.group(3), properties.getProperty(propName));
                        }
                        defs.put("prio", priority);
                    }
                }
            }
        }
    }

    /**
     * Reads flexible class definitions from the given properties filename from all components.
     */
    public static void readClassDefMapsFromAllComponents(Map<String, Map<String, String>> allMapDefs, String resource, String propPrefix, ClassLoader classLoader) {
        for(Properties props : AdvancedPropertyUtil.getAllComponentResourcePropsSafe(resource, classLoader)) {
            AdvancedPropertyUtil.readClassDefMaps(allMapDefs, props, propPrefix);
        }
    }

    public static Properties getResourcePropsSafe(String resource, ClassLoader classLoader) {
        if (classLoader == null) {
            classLoader = getComponentClassLoader();
        }
        Properties props = new Properties();
        try {
            InputStream is = classLoader.getResourceAsStream(resource);
            if (is != null) {
                props.load(is);
            }
        } catch(Exception e) {
            Debug.logError(e, "Scipio: Could not load resource '" + resource + "'", module);
        }
        return props;
    }
    
    public static List<Properties> getAllComponentResourcePropsSafe(String resource, ClassLoader classLoader) {
        if (classLoader == null) {
            classLoader = getComponentClassLoader();
        }
        List<Properties> allProps = new ArrayList<>();
        Enumeration<URL> resources;
        try {
            resources = classLoader.getResources(resource);
        } catch (Exception e) {
            Debug.logError(e, "Scipio: Could not list resources: " + resource, module);
            return allProps;
        }
        while (resources.hasMoreElements()) {
            allProps.add(UtilProperties.getProperties(resources.nextElement()));
        }
        return allProps;
    }

    
    public static <T extends ClassDef<H>, H> List<T> createClassDefsFromMaps(Map<String, Map<String, String>> allMapDefs, 
            ClassLoader classLoader, ClassDefFactory<T, H> classDefFactory) {
        List<T> allDefs = new ArrayList<>(allMapDefs.size());
        for(Map<String, String> props : allMapDefs.values()) {
            T classDef = classDefFactory.newInstance(props, classLoader);
            if (classDef != null) {
                allDefs.add(classDef);
            }
        }
        Collections.sort(allDefs);
        return allDefs;
    }
    
    /**
     * High level helper method to read class defs from all components in one shot, with optional initial and override defs.
     * <p>
     * WARN: the defs can actually override each other; caller should expect his input maps may get altered.
     */
    public static <T extends ClassDef<H>, H> List<T> readClassDefsFromAllComponents(Map<String, Map<String, String>> initialMapDefs, 
            String resource, String propPrefix, Map<String, Map<String, String>> overrideMapDefs, ClassLoader classLoader, ClassDefFactory<T, H> classDefFactory) {
        Map<String, Map<String, String>> allMapDefs = (initialMapDefs != null) ? new HashMap<>(initialMapDefs) : new HashMap<>();
        if (resource != null) {
            readClassDefMapsFromAllComponents(allMapDefs, resource, propPrefix, classLoader);
        }
        if (overrideMapDefs != null) {
            allMapDefs.putAll(overrideMapDefs);
        }
        return createClassDefsFromMaps(allMapDefs, classLoader, classDefFactory);
    }
    
    /**
     * High level helper method to read class defs from all components in one shot.
     */
    public static <T extends ClassDef<H>, H> List<T> readClassDefsFromAllComponents(String resource, String propPrefix, ClassDefFactory<T, H> classDefFactory) {
        return readClassDefsFromAllComponents(null, resource, propPrefix, null, null, classDefFactory);
    }

    public interface ClassDefFactory<T extends ClassDef<H>, H> {
        T newInstance(Map<String, String> props, ClassLoader classLoader);
    }
    
    public static ClassLoader getComponentClassLoader() {
        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        if (classLoader == null) {
            Debug.logWarning("Unable to get context classloader; using system", module);
            classLoader = ClassLoader.getSystemClassLoader();
        }
        return classLoader;
    }
    
    /**
     * An ordered class definition and instance.
     */
    public static class ClassDef<H> implements Comparable<ClassDef<H>> {
        private final String name;
        private final Integer priority;
        private final H instance;
        private final Map<String, String> props;

        public ClassDef(String name, Integer priority, H instance, Map<String, String> props) {
            this.name = name;
            this.priority = priority;
            this.instance = instance;
            props = new HashMap<>(props);
            props.remove("name");
            props.remove("class");
            props.remove("prio");
            this.props = Collections.unmodifiableMap(props);
        }

        public ClassDef(Map<String, String> props, ClassLoader classLoader, Class<H> targetCls) throws IllegalArgumentException {
            this(getName(props), getPriority(props), loadInstance(props.get("class"), classLoader, targetCls), props);
        }

        public static String getName(Map<String, String> props) throws IllegalArgumentException {
            String name = props.get("name");
            if (name != null && !name.isEmpty()) return name;
            name = props.get("class");
            if (name != null && !name.isEmpty()) return name;
            return null;
        }

        public static Integer getPriority(Map<String, String> props) throws IllegalArgumentException {
            String prio = props.get("prio");
            if (prio == null || prio.isEmpty()) {
                return null;
            }
            try {
                return Integer.parseInt(prio);
            } catch(Exception e) {
                throw new IllegalArgumentException("invalid startup loader priority: " + prio);
            }
        }

        @SuppressWarnings("unchecked")
        public static <H> H loadInstance(String className, ClassLoader classLoader, Class<H> targetCls) throws IllegalArgumentException {
            if (className == null || className.isEmpty()) {
                throw new IllegalArgumentException("missing class");
            }
            Class<?> cls;
            try {
                cls = classLoader.loadClass(className);
            } catch (ClassNotFoundException e) {
                throw new IllegalArgumentException("class '" + className + "' not found");
            }
            if (!targetCls.isAssignableFrom(cls)) {
                throw new IllegalArgumentException("class '" + className + "' does not implement '" + StartupLoader.class.getName() + "'");
            }
            try {
                return (H) cls.newInstance();
            } catch(Exception e) {
                throw new IllegalArgumentException("error loading class '" + className + ": " + e.getMessage(), e);
            }
        }

        public Integer getPriority() {
            return priority;
        }

        public H getInstance() {
            return instance;
        }

        public Map<String, String> getProps() {
            return props;
        }

        @Override
        public String toString() {
            return "[" + priority + "] " + name + " (" + instance.getClass().getName() + ")";
        }

        @Override
        public int compareTo(ClassDef<H> o) {
            if (this.priority == null) {
                if (o.priority == null) {
                    return 0;
                } else {
                    return 1; // null means runs at end
                }
            } else {
                if (o.priority == null) {
                    return -1;
                } else {
                    return Integer.compare(this.priority, o.priority);
                }
            }
        }
    }
}

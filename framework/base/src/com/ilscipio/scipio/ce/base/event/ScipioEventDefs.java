package com.ilscipio.scipio.ce.base.event;

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

public class ScipioEventDefs<H> {

    private static final String module = ScipioEventDefs.class.getName();

    // DEV NOTE: DO NOT remove the .properties suffix from either of these; will break the scanning!
    public static final String EVENTS_PROPRES = "scipio-events.properties";
    public static final String EVENTS_PROPRES_STATIC = "org/ofbiz/base/start/scipio-events.properties";

    private static final Pattern numberedNameSuffixPat = Pattern.compile("^(\\d+)(\\.(.+))?$");

    protected final List<EventDef<H>> eventDefs;

    protected ScipioEventDefs(List<EventDef<H>> eventDefs) {
        this.eventDefs = eventDefs;
    }

    public static <H> ScipioEventDefs<H> readEventDefs(String propPrefix, Class<H> handlerIf) {
        List<EventDef<H>> allDefs = new ArrayList<>();
        Map<String, Map<String, String>> allMapDefs = new HashMap<>();

        ClassLoader classLoader = Thread.currentThread().getContextClassLoader();
        if (classLoader == null) {
            Debug.logWarning("Unable to get context classloader; using system", module);
            classLoader = ClassLoader.getSystemClassLoader();
        }

        // static properties (compiled with Start.java)
        ScipioEventDefs.readEventMapDefs(allMapDefs,
                ScipioEventDefs.getClassResourcePropsSafe(ScipioEventDefs.EVENTS_PROPRES_STATIC),
                propPrefix);

        // component properties
        for(Properties props : ScipioEventDefs.getAllComponentResourcePropsSafe(ScipioEventDefs.EVENTS_PROPRES, classLoader)) {
            ScipioEventDefs.readEventMapDefs(allMapDefs, props, propPrefix);
        }

        for(Map<String, String> defMap : allMapDefs.values()) {
            EventDef<H> eventDef;
            try {
                eventDef = new EventDef<>(defMap, classLoader, handlerIf);
            } catch(Exception e) {
                Debug.logError("Scipio: Error initializing " + ScipioEventDefs.EVENTS_PROPRES
                        + " " + propPrefix
                        + " handler '" + defMap + "': " + e.getMessage(), module);
                continue;
            }
            allDefs.add(eventDef);
        }
        Collections.sort(allDefs);
        
        StringBuilder sb = new StringBuilder("Scipio: Registered event handlers for " + EVENTS_PROPRES + " " + propPrefix + ":");

        if (allDefs.isEmpty()) {
            sb.append("(none)");
        } else {
            for(EventDef<H> eventDef : allDefs) {
                sb.append("\n");
                sb.append(eventDef);
            }
        }
        Debug.logInfo(sb.toString(), module);
        
        return new ScipioEventDefs<H>(allDefs);
    }

    public List<EventDef<H>> getLoaderDefs() {
        return eventDefs;
    }

    protected static Properties getClassResourcePropsSafe(String resource) {
        Properties props = new Properties();
        try {
            InputStream is = ScipioEventDefs.class.getClassLoader().getResourceAsStream(resource);
            if (is != null) {
                props.load(is);
            }
        } catch(Exception e) {
            Debug.logError(e, "Scipio: Could not load resource '" + resource + "'", module);
        }
        return props;
    }

    protected static List<Properties> getAllComponentResourcePropsSafe(String resource, ClassLoader classLoader) {
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

    /**
     * Extracts event handler def properties.
     * <p>
     * Supports two formats:
     * <ul>
     * <li>[prefix].[name].class=[class] / [prefix].[name].prio=[priority] (name-based format)</li>
     * <li>[prefix][priority]=[class] / [prefix][priority].name=[name] (priority-based start.properties format)</li>
     * </ul>
     */
    protected static void readEventMapDefs(Map<String, Map<String, String>> outLoaderMapDefs, Properties properties, String propPrefix) {
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

    public static class EventDef<H> implements Comparable<EventDef<H>> {
        private final String name;
        private final Integer priority;
        private final H handler;
        private final Map<String, String> props;

        public EventDef(String name, Integer priority, H handler, Map<String, String> props) {
            this.name = name;
            this.priority = priority;
            this.handler = handler;
            props = new HashMap<>(props);
            props.remove("name");
            props.remove("class");
            props.remove("prio");
            this.props = Collections.unmodifiableMap(props);
        }

        public EventDef(Map<String, String> props, ClassLoader classLoader, Class<H> handlerIf) throws IllegalArgumentException {
            this(getName(props), getPriority(props), loadEventHandler(props.get("class"), classLoader, handlerIf), props);
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
        public static <H> H loadEventHandler(String className, ClassLoader classLoader, Class<H> handlerIf) throws IllegalArgumentException {
            if (className == null || className.isEmpty()) {
                throw new IllegalArgumentException("missing class");
            }
            Class<?> cls;
            try {
                cls = classLoader.loadClass(className);
            } catch (ClassNotFoundException e) {
                throw new IllegalArgumentException("class '" + className + "' not found");
            }
            if (!handlerIf.isAssignableFrom(cls)) {
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

        public H getLoader() {
            return handler;
        }

        public Map<String, String> getProps() {
            return props;
        }

        @Override
        public String toString() {
            return "[" + priority + "] " + name + " (" + handler.getClass().getName() + ")";
        }

        @Override
        public int compareTo(EventDef<H> o) {
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

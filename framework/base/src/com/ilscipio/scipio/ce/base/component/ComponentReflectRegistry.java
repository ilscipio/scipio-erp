package com.ilscipio.scipio.ce.base.component;

import com.ilscipio.scipio.ce.lang.reflect.ReflectQuery;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.Debug;

import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Per-component reflection and annotation scanner - excludes webapp jars.
 * <p>Abstracts the jar file locations and bundles them using {@link ReflectQuery}.</p>
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
public class ComponentReflectRegistry {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final Map<String, ComponentReflectInfo> REGISTRY = new ConcurrentHashMap<>();

    public static ComponentReflectInfo registerComponentReflectInfo(ComponentConfig component, Collection<URL> jarUrls) {
        String cacheKey = toKey(component);
        ComponentReflectInfo cri = REGISTRY.get(cacheKey);
        if (cri == null) {
            cri = new ComponentReflectInfo(component, jarUrls, true);
            ComponentReflectInfo prevCri = REGISTRY.putIfAbsent(cacheKey, cri);
            if (prevCri != null) {
                cri = prevCri;
            }
        }
        return cri;
    }

    public static ComponentReflectInfo registerComponentReflectInfo(WebappReflectRegistry.WebappReflectInfo webappReflectInfo,
                                                                    Collection<URL> jarUrls) {
        Collection<URL> componentJarUrls = new ArrayList<>(jarUrls.size());
        for(URL url : jarUrls) {
            // FIXME: Heuristic, generally works since build/lib is never in a "webapp" subfolder
            if (!url.toString().contains("/webapp/")) {
                componentJarUrls.add(url);
            }
        }
        return registerComponentReflectInfo(webappReflectInfo.getWebappInfo().componentConfig, componentJarUrls);
    }

    public static Collection<ComponentReflectInfo> getComponentReflectInfos() {
        return REGISTRY.values();
    }

    public static ComponentReflectInfo getComponentReflectInfo(ComponentConfig component) {
        return REGISTRY.get(toKey(component));
    }

    public static ComponentReflectInfo getComponentReflectInfo(String componentName) {
        return REGISTRY.get(componentName);
    }

    protected static String toKey(ComponentConfig component) {
        return component.getGlobalName();
    }

    public static class ComponentReflectInfo {
        protected final ComponentConfig component;
        protected final ReflectQuery reflectQuery;

        public ComponentReflectInfo(ComponentConfig component, ReflectQuery reflectQuery) {
            this.component = component;
            this.reflectQuery = reflectQuery;
        }

        public ComponentReflectInfo(ComponentConfig component, Collection<URL> jarUrls, boolean useCache) {
            this.component = component;
            this.reflectQuery = ReflectQuery.fromJarUrls(jarUrls, useCache);
        }

        public ComponentConfig getComponent() {
            return component;
        }

        public ReflectQuery getReflectQuery() {
            return reflectQuery;
        }
    }

}

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
 *
 * <p>Abstracts the jar file locations and bundles them using {@link ReflectQuery}.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
public class ComponentReflectRegistry {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final Map<String, ComponentReflectInfo> NAME_REGISTRY = new ConcurrentHashMap<>();

    public static Collection<ComponentReflectInfo> getReflectInfos() {
        return NAME_REGISTRY.values();
    }

    public static ComponentReflectInfo getReflectInfo(ComponentConfig component) {
        return NAME_REGISTRY.get(component.getGlobalName());
    }

    public static ComponentReflectInfo getReflectInfoByName(String componentName) {
        return NAME_REGISTRY.get(componentName);
    }

    static ComponentReflectInfo registerReflectInfo(ComponentConfig component, Collection<URL> jarUrls) {
        String nameKey = component.getGlobalName();
        ComponentReflectInfo cri = NAME_REGISTRY.get(nameKey);
        if (cri == null) {
            cri = new ComponentReflectInfo(component, jarUrls, true);
            ComponentReflectInfo prevCri = NAME_REGISTRY.putIfAbsent(nameKey, cri);
            if (prevCri != null) {
                cri = prevCri;
            }
        }
        return cri;
    }

}

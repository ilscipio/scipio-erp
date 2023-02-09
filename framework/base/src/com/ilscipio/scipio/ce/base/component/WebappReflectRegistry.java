package com.ilscipio.scipio.ce.base.component;

import com.ilscipio.scipio.ce.lang.reflect.ReflectQuery;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.Debug;

import java.net.URL;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Per-webapp reflection and annotation scanner.
 *
 * <p>Abstracts the jar file locations and bundles them using {@link ReflectQuery}.</p>
 *
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
public class WebappReflectRegistry {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final Map<String, WebappReflectInfo> NAME_REGISTRY = new ConcurrentHashMap<>();
    private static final Map<String, WebappReflectInfo> MOUNT_REGISTRY = new ConcurrentHashMap<>();

    public static Collection<WebappReflectInfo> getWebappReflectInfos() {
        return NAME_REGISTRY.values();
    }

    public static WebappReflectInfo getWebappReflectInfo(ComponentConfig.WebappInfo webappInfo) {
        return NAME_REGISTRY.get(webappInfo.getComponentConfig().getGlobalName() + "::" + webappInfo.getName());
    }

    public static WebappReflectInfo getWebappReflectInfoByName(String componentName, String webappName) {
        return NAME_REGISTRY.get(componentName + "::" + webappName);
    }

    public static WebappReflectInfo getWebappReflectInfoByMount(String server, String contextRoot) {
        return MOUNT_REGISTRY.get(server + "::" + contextRoot);
    }

    public static WebappReflectInfo registerWebappReflectInfo(ComponentConfig.WebappInfo webappInfo,
                                                              Collection<URL> jarUrls) {
        String nameKey = webappInfo.getComponentConfig().getGlobalName() + "::" + webappInfo.getName();
        WebappReflectInfo wri = NAME_REGISTRY.get(nameKey);
        if (wri == null) {
            wri = new WebappReflectInfo(webappInfo, jarUrls, true);
            WebappReflectInfo prevWri = NAME_REGISTRY.putIfAbsent(nameKey, wri);
            if (prevWri != null) {
                wri = prevWri;
            } else {
                MOUNT_REGISTRY.put(webappInfo.getServer() + "::" + webappInfo.getContextRoot(), wri);
                // Also register the component part, excluding webapp jars
                // SCIPIO: 3.0.0: Now loaded separately by CatalinaContainer
                //ComponentReflectRegistry.registerComponentReflectInfo(wri, jarUrls);
            }
        }
        return wri;
    }

    public static class WebappReflectInfo {
        protected final ComponentConfig.WebappInfo webappInfo;
        protected final ReflectQuery reflectQuery;

        public WebappReflectInfo(ComponentConfig.WebappInfo webappInfo, ReflectQuery reflectQuery) {
            this.webappInfo = webappInfo;
            this.reflectQuery = reflectQuery;
        }

        public WebappReflectInfo(ComponentConfig.WebappInfo webappInfo, Collection<URL> jarUrls, boolean useCache) {
            this.webappInfo = webappInfo;
            this.reflectQuery = ReflectQuery.fromJarUrls(jarUrls, useCache);
        }

        public ComponentConfig.WebappInfo getWebappInfo() {
            return webappInfo;
        }

        public ReflectQuery getReflectQuery() {
            return reflectQuery;
        }
    }

    /**
     * For an absolute or relative-to-project-root path, URI or URL to a file, if the path points under a webapp,
     * returns a {@link ReflectQuery} for the whole webapp and component jars; if only under component, returns for
     * component jars only.
     */
    public static ReflectQuery getReflectQueryForResource(Object location) {
        ComponentConfig.WebappInfo webappInfo = ComponentConfig.getWebappInfoFromResource(location, false);
        if (webappInfo != null) {
            WebappReflectRegistry.WebappReflectInfo wri = WebappReflectRegistry.getWebappReflectInfo(webappInfo);
            if (wri != null) {
                return wri.getReflectQuery();
            }
        }
        ComponentConfig componentConfig = ComponentConfig.getComponentConfigFromResource(location);
        if (componentConfig != null) {
            ComponentReflectRegistry.ComponentReflectInfo cri = ComponentReflectRegistry.getComponentReflectInfo(componentConfig);
            if (cri != null) {
                return cri.getReflectQuery();
            }
        }
        return null;
    }
}

package com.ilscipio.scipio.ce.base.component;

import com.ilscipio.scipio.ce.lang.reflect.ReflectQuery;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.Debug;

import java.io.File;
import java.net.URL;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Per-webapp reflection and annotation scanner.
 * <p>Abstracts the jar file locations and bundles them using {@link ReflectQuery}.</p>
 * <p>SCIPIO: 3.0.0: Added for annotations support.</p>
 */
public class WebappReflectRegistry {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final Map<String, WebappReflectInfo> REGISTRY = new ConcurrentHashMap<>();

    public static WebappReflectInfo getWebappReflectInfo(ComponentConfig.WebappInfo webappInfo) {
        return REGISTRY.get(toKey(webappInfo));
    }

    public static Collection<WebappReflectInfo> getWebappReflectInfos() {
        return REGISTRY.values();
    }

    public static WebappReflectInfo registerWebappReflectInfo(ComponentConfig.WebappInfo webappInfo,
                                                              Collection<URL> jarUrls) {
        String cacheKey = toKey(webappInfo);
        WebappReflectInfo wri = REGISTRY.get(cacheKey);
        if (wri == null) {
            wri = new WebappReflectInfo(webappInfo, jarUrls, true);
            WebappReflectInfo prevWri = REGISTRY.putIfAbsent(cacheKey, wri);
            if (prevWri != null) {
                wri = prevWri;
            } else {
                // Also register the component part, excluding webapp jars
                ComponentReflectRegistry.registerComponentReflectInfo(wri, jarUrls);
            }
        }
        return wri;
    }

    public static WebappReflectInfo registerWebappReflectInfo(ComponentConfig.WebappInfo webappInfo) {
        String cacheKey = toKey(webappInfo);
        return REGISTRY.get(cacheKey);
    }

    protected static String toKey(ComponentConfig.WebappInfo webappInfo) {
        return webappInfo.getServer() + "::" + webappInfo.getContextRoot();
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

    public static class JarScanner implements FilterJarsScanner {
        private static final JarScanner DEFAULT = new JarScanner();

        @Override
        public void scanJars(ComponentConfig.WebappInfo webappInfo, List<File> jarFiles, Set<String> jarNames) {
            registerWebappReflectInfo(webappInfo, ReflectQuery.getJarUrlsForFiles(jarFiles));
        }

        public static class Factory implements FilterJarsScanner.Factory {
            @Override
            public FilterJarsScanner makeScanner() {
                return DEFAULT;
            }
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

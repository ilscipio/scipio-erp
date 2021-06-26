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

public class WebappReflectRegistry {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final Map<String, WebappReflectInfo> REGISTRY = new ConcurrentHashMap<>();

    public static WebappReflectInfo registerWebappReflectInfo(ComponentConfig.WebappInfo webappInfo,
                                                              Collection<URL> jarUrls, boolean useCache) {
        if (!useCache) {
            return new WebappReflectInfo(webappInfo, jarUrls, false);
        }
        String cacheKey = toKey(webappInfo);
        WebappReflectInfo wri = REGISTRY.get(cacheKey);
        if (wri == null) {
            wri = new WebappReflectInfo(webappInfo, jarUrls, true);
            WebappReflectInfo prevWri = REGISTRY.putIfAbsent(cacheKey, wri);
            if (prevWri != null) {
                wri = prevWri;
            }
        }
        return wri;
    }

    public static WebappReflectInfo registerWebappReflectInfo(ComponentConfig.WebappInfo webappInfo) {
        String cacheKey = toKey(webappInfo);
        return REGISTRY.get(cacheKey);
    }

    public static Collection<WebappReflectInfo> getWebappReflectInfos() {
        return REGISTRY.values();
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
            registerWebappReflectInfo(webappInfo, ReflectQuery.getJarUrlsForFiles(jarFiles), true);
        }

        public static class Factory implements FilterJarsScanner.Factory {
            @Override
            public FilterJarsScanner makeScanner() {
                return DEFAULT;
            }
        }
    }

}

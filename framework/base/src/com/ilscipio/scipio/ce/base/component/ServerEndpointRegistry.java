package com.ilscipio.scipio.ce.base.component;

import com.ilscipio.scipio.ce.lang.reflect.ReflectQuery;
import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.Debug;

import javax.websocket.server.ServerEndpoint;
import java.io.Serializable;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Registry of per-webapp ServerEndpoint-annotated classes.
 *
 * <p>SCIPIO: 3.0.0: Enhanced for annotations support.</p>
 */
public class ServerEndpointRegistry {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final ServerEndpointRegistry INSTANCE = new ServerEndpointRegistry();

    /** Maps webapp to endpoints */
    private final Map<String, WebappEndpoints> webappEndpointsMap = new ConcurrentHashMap<>();

    public static ServerEndpointRegistry getInstance() {
        return INSTANCE;
    }

    public WebappEndpoints getWebappEndpoints(ComponentConfig.WebappInfo webappInfo) {
        String cacheKey = toKey(webappInfo);
        WebappEndpoints we = webappEndpointsMap.get(cacheKey);
        if (we == null) {
            WebappReflectInfo wri = WebappReflectRegistry.getReflectInfo(webappInfo);
            Set<Class<?>> annotatedClasses = Collections.emptySet();
            if (wri != null) {
                ReflectQuery rq = wri.getReflectQuery();
                if (rq != null) {
                    annotatedClasses = rq.getAnnotatedClasses(ServerEndpoint.class);
                }
            }
            we = new WebappEndpoints(webappInfo, annotatedClasses);
            WebappEndpoints prevWe = webappEndpointsMap.putIfAbsent(cacheKey, we);
            if (prevWe != null) {
                we = prevWe;
            }
        }
        return we;
    }

    public class WebappEndpoints implements Serializable {
        protected final ComponentConfig.WebappInfo webappInfo;
        protected final Map<Class<?>, EndpointInfo> endpointMap;
        protected final Map<Class<?>, String> endpointPaths;

        protected WebappEndpoints(ComponentConfig.WebappInfo webappInfo, Set<Class<?>> annotatedClasses) {
            this.webappInfo = webappInfo;
            Map<Class<?>, EndpointInfo> endpointMap = new LinkedHashMap<>();
            Map<Class<?>, String> endpointPaths = new LinkedHashMap<>();
            for(Class<?> cls : annotatedClasses) {
                EndpointInfo endpointInfo = new EndpointInfo(cls);
                endpointMap.put(cls, endpointInfo);
                endpointPaths.put(cls, endpointInfo.getEndpointPath());
            }
            this.endpointMap = Collections.unmodifiableMap(endpointMap);
            this.endpointPaths = Collections.unmodifiableMap(endpointPaths);
        }

        public ComponentConfig.WebappInfo getWebappInfo() {
            return webappInfo;
        }

        public Map<Class<?>, EndpointInfo> getEndpointMap() {
            return endpointMap;
        }

        protected String toKey() {
            return ServerEndpointRegistry.this.toKey(getWebappInfo());
        }
    }

    public static class EndpointInfo implements Serializable {
        protected final Class<?> endpointClass;
        protected final ServerEndpoint serverEndpoint;
        protected final String endpointPath;

        public EndpointInfo(Class<?> endpointClass) {
            this.serverEndpoint = endpointClass.getAnnotation(ServerEndpoint.class);
            if (this.serverEndpoint == null) {
                throw new IllegalStateException("Couldn't get ServerEndpoint annotation from " + endpointClass);
            }
            this.endpointClass = endpointClass;
            this.endpointPath = this.serverEndpoint.value();
        }

        public Class<?> getEndpointClass() {
            return endpointClass;
        }

        public String getEndpointPath() {
            return endpointPath;
        }
    }

    protected String toKey(ComponentConfig.WebappInfo webappInfo) {
        return webappInfo.getServer() + "::" + webappInfo.getContextRoot();
    }
}

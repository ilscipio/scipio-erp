package org.ofbiz.webapp;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.apache.tomcat.util.descriptor.web.WebXml;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.webapp.control.ConfigXMLReader.ControllerConfig;
import org.ofbiz.webapp.renderer.RenderEnvType;
import org.ofbiz.webapp.website.WebSiteProperties;
import org.ofbiz.webapp.website.WebSiteWorker;
import org.xml.sax.SAXException;

import com.ilscipio.scipio.ce.util.Optional;

/**
 * SCIPIO: Dynamic "full" information about a webapp, including
 * webSiteId, WebappInfo, ExtWebappInfo, WebSiteProperties,
 * and ControllerConfig.
 * <p>
 * IMPORTANT: This class is intended for short scopes only,
 * mainly single requests. It should not be cached in static
 * variables because it relies on database lookups.
 * <p>
 * This serves as a request cache.
 * <p>
 * FullWebappInfo is basically thread-safe, as long as client does not need
 * objects returned by getters to be truly unique.
 * However, the Cache below is NOT thread-safe.
 * <p>
 * DEV NOTE: do not make this serializable, so as to avoid this
 * getting transferred into session attributes by RequestHandler.
 * <p>
 * Added 2018-08-02.
 */
public class FullWebappInfo {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final Delegator delegator;
    private ExtWebappInfo extWebappInfo;
    private WebSiteProperties webSiteProperties;

    private Optional<ControllerConfig> controllerConfig;
    private OfbizUrlBuilder ofbizUrlBuilder;

    protected FullWebappInfo(Delegator delegator, ExtWebappInfo extWebappInfo, WebSiteProperties webSiteProperties,
            ControllerConfig controllerConfig, OfbizUrlBuilder ofbizUrlBuilder) {
        this.delegator = delegator; // can be null if all others are non-null
        this.extWebappInfo = extWebappInfo;
        this.webSiteProperties = webSiteProperties;
        this.controllerConfig = Optional.ofNullable(controllerConfig);
        this.ofbizUrlBuilder = ofbizUrlBuilder;
    }

    protected FullWebappInfo(Delegator delegator, ExtWebappInfo extWebappInfo, WebSiteProperties webSiteProperties) {
        this.delegator = delegator;
        this.extWebappInfo = extWebappInfo;
        this.webSiteProperties = webSiteProperties;
    }

    protected FullWebappInfo(Delegator delegator, ExtWebappInfo extWebappInfo) {
        this(delegator, extWebappInfo, null);
    }

    /*
     * ******************************************************
     * High-level factory methods
     * ******************************************************
     */

    public static FullWebappInfo fromRequest(HttpServletRequest request) throws IllegalArgumentException {
        return fromRequest(request, Cache.fromRequest(request));
    }

    public static FullWebappInfo fromRequest(HttpServletRequest request, Cache cache) throws IllegalArgumentException {
        if (cache == null) return newFromRequest(request);
        FullWebappInfo fullWebappInfo = cache.getCurrentWebappInfo();
        if (fullWebappInfo == null) {
            fullWebappInfo = newFromRequest(request);
            cache.setCurrentWebappInfo(fullWebappInfo);
        }
        return fullWebappInfo;
    }

    /**
     * Gets from context. NOTE: this will NOT try to get from "request" in context.
     * <p>
     * NOTE: for static render contexts, this only works if there is a webSiteId.
     * For now only webapp render context can return an instance with no webSiteId.
     */
    public static FullWebappInfo fromContext(Map<String, Object> context, RenderEnvType renderEnvType) throws IllegalArgumentException {
        return fromContext(context, renderEnvType, Cache.fromContext(context, renderEnvType));
    }

    public static FullWebappInfo fromContext(Map<String, Object> context, RenderEnvType renderEnvType, Cache cache) throws IllegalArgumentException {
        if (renderEnvType.isStatic()) {
            FullWebappInfo fullWebappInfo;
            if (cache != null) {
                fullWebappInfo = cache.getCurrentWebappInfo();
                if (fullWebappInfo != null) return fullWebappInfo;
            }
            String webSiteId = WebSiteWorker.getWebSiteIdFromContext(context, renderEnvType);
            if (webSiteId != null) {
                fullWebappInfo = FullWebappInfo.fromWebSiteId((Delegator) context.get("delegator"), webSiteId, cache);
                if (cache != null) cache.setCurrentWebappInfoOnly(fullWebappInfo); // 
            }
        } else if (renderEnvType.isWebapp()) {
            return fromRequest((HttpServletRequest) context.get("request"), cache);
        }
        return null;
    }

    public static FullWebappInfo fromRequestOrContext(HttpServletRequest request, Map<String, Object> context, RenderEnvType renderEnvType) {
        return (request != null) ? fromRequest(request) : fromContext(context, renderEnvType);
    }

    public static FullWebappInfo fromWebSiteIdOrRequest(HttpServletRequest request, String webSiteId) throws IllegalArgumentException {
        return fromWebSiteIdOrRequest(request, webSiteId, Cache.fromRequest(request));
    }

    public static FullWebappInfo fromWebSiteIdOrRequest(HttpServletRequest request, String webSiteId, Cache cache) throws IllegalArgumentException {
        if (UtilValidate.isNotEmpty(webSiteId)) {
            return fromWebSiteId((Delegator) request.getAttribute("delegator"), webSiteId, cache);
        } else {
            return fromRequest(request, cache);
        }
    }

    public static FullWebappInfo fromWebSiteIdOrContextPath(Delegator delegator, String webSiteId, String contextPath, Cache cache) throws IllegalArgumentException {
        if (UtilValidate.isNotEmpty(webSiteId)) {
            return fromWebSiteId(delegator, webSiteId, cache);
        } else if (UtilValidate.isNotEmpty(contextPath)) {
            return fromContextPath(delegator, contextPath, cache);
        }
        return null;
    }
    
    /**
     * SCIPIO: Returns the <code>FullWebappInfo</code> instance that has the same mount-point prefix as
     * the given path.
     * <p>
     * <strong>WARN:</strong> Webapp mounted on root (/*) will usually cause a catch-all here.
     */
    public static FullWebappInfo fromPath(Delegator delegator, String path, Cache cache)  throws IllegalArgumentException {
        WebappInfo webappInfo;
        try {
            webappInfo = WebAppUtil.getWebappInfoFromPath(path);
        } catch (IOException e) {
            throw new IllegalArgumentException(e);
        } catch (SAXException e) {
            throw new IllegalArgumentException(e);
        }
        return fromWebappInfo(delegator, webappInfo, cache);
    }
    
    /*
     * ******************************************************
     * Individual element factory methods
     * ******************************************************
     */

    public static FullWebappInfo fromExtWebappInfo(Delegator delegator, ExtWebappInfo extWebappInfo, Cache cache) throws IllegalArgumentException {
        if (cache == null) return newFromExtWebappInfo(delegator, extWebappInfo);
        FullWebappInfo fullWebappInfo = cache.getByContextPath(extWebappInfo.getContextPath());
        if (fullWebappInfo == null) {
            fullWebappInfo = newFromExtWebappInfo(delegator, extWebappInfo);
            cache.addWebappInfo(fullWebappInfo);
        }
        return fullWebappInfo;
    }

    public static FullWebappInfo fromWebappInfo(Delegator delegator, WebappInfo webappInfo, Cache cache) throws IllegalArgumentException {
        if (cache == null) return newFromWebappInfo(delegator, webappInfo);
        FullWebappInfo fullWebappInfo = cache.getByContextPath(webappInfo.getContextRoot());
        if (fullWebappInfo == null) {
            fullWebappInfo = newFromWebappInfo(delegator, webappInfo);
            cache.addWebappInfo(fullWebappInfo);
        }
        return fullWebappInfo;
    }

    public static FullWebappInfo fromWebSiteId(Delegator delegator, String webSiteId, Cache cache) throws IllegalArgumentException {
        if (cache == null) return newFromWebSiteId(delegator, webSiteId);
        FullWebappInfo fullWebappInfo = cache.getByWebSiteId(webSiteId);
        if (fullWebappInfo == null) {
            fullWebappInfo = newFromWebSiteId(delegator, webSiteId);
            cache.addWebappInfo(fullWebappInfo);
        }
        return fullWebappInfo;
    }

    public static FullWebappInfo fromContextPath(Delegator delegator, String contextPath, Cache cache) throws IllegalArgumentException {
        if (cache == null) return newFromContextPath(delegator, contextPath);
        FullWebappInfo fullWebappInfo = cache.getByContextPath(contextPath);
        if (fullWebappInfo == null) {
            fullWebappInfo = newFromContextPath(delegator, contextPath);
            cache.addWebappInfo(fullWebappInfo);
        }
        return fullWebappInfo;
    }

    /*
     * ******************************************************
     * New-instance factory methods (avoid in client code)
     * ******************************************************
     */

    protected static FullWebappInfo newFromRequest(HttpServletRequest request) throws IllegalArgumentException {
        try {
            // SPECIAL: in this case we must initialize WebSiteProperties immediately because
            // we can't store the HttpServletRequest object in FullWebappInfo
            return new FullWebappInfo((Delegator) request.getAttribute("delegator"),
                    ExtWebappInfo.fromContextPath(request.getContextPath()),
                    WebSiteProperties.from(request));
        } catch (GenericEntityException e) {
            throw new IllegalArgumentException(e);
        }
    }

    public static FullWebappInfo newFromExtWebappInfo(Delegator delegator, ExtWebappInfo extWebappInfo) throws IllegalArgumentException {
        return new FullWebappInfo(delegator, extWebappInfo);
    }

    public static FullWebappInfo newFromWebappInfo(Delegator delegator, WebappInfo webappInfo) throws IllegalArgumentException {
        return new FullWebappInfo(delegator, ExtWebappInfo.fromContextPath(webappInfo.getContextRoot()));
    }

    public static FullWebappInfo newFromWebSiteId(Delegator delegator, String webSiteId) throws IllegalArgumentException {
        return new FullWebappInfo(delegator, ExtWebappInfo.fromWebSiteId(webSiteId));
    }

    public static FullWebappInfo newFromContextPath(Delegator delegator, String contextPath) throws IllegalArgumentException {
        return new FullWebappInfo(delegator, ExtWebappInfo.fromContextPath(contextPath));
    }


    /*
     * ******************************************************
     * Other type factory methods (mainly to exploit the cache).
     * ******************************************************
     */

    public static OfbizUrlBuilder getOfbizUrlBuilderFromWebSiteIdOrDefaults(Delegator delegator, String webSiteId, Cache cache) throws IllegalArgumentException {
        if (UtilValidate.isNotEmpty(webSiteId)) {
            if (cache != null && cache.getByWebSiteId(webSiteId) != null) {
                return cache.getByWebSiteId(webSiteId).getOfbizUrlBuilder();
            }
            try {
                return OfbizUrlBuilder.fromWebSiteId(webSiteId, delegator);
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
        } else {
            if (cache != null && cache.getDefaultOfbizUrlBuilder() != null) {
                return cache.getDefaultOfbizUrlBuilder();
            }
            OfbizUrlBuilder builder;
            try {
                builder = OfbizUrlBuilder.fromServerDefaults(delegator);
            } catch (Exception e) {
                throw new IllegalStateException(e);
            }
            if (cache != null) cache.setDefaultOfbizUrlBuilder(builder);
            return builder;
        }
    }

    /*
     * ******************************************************
     * Object getters
     * ******************************************************
     */

    public ExtWebappInfo getExtWebappInfo() {
        return extWebappInfo;
    }

    public WebSiteProperties getWebSiteProperties() {
        WebSiteProperties webSiteProperties = this.webSiteProperties;
        if (webSiteProperties == null) {
            String webSiteId = getWebSiteId();
            try {
                if (webSiteId != null) {
                    webSiteProperties = WebSiteProperties.from(delegator, webSiteId);
                } else {
                    webSiteProperties = WebSiteProperties.defaults(delegator);
                }
            } catch (Exception e) {
                if (webSiteId != null) {
                    Debug.logError(e, "Error getting WebSiteProperties for webSiteId '" + webSiteId + "'; trying defaults...", module);
                    try {
                        webSiteProperties = WebSiteProperties.defaults(delegator);
                    } catch (Exception e2) {
                        throw new IllegalStateException("Could not get default system WebSiteProperties", e); // very unlikely...
                    }
                } else {
                    throw new IllegalStateException("Could not get default system WebSiteProperties", e); // very unlikely...
                }
            }
            this.webSiteProperties = webSiteProperties;
        }
        return webSiteProperties;
    }

    /**
     * Returns the ControllerConfig for this webapp or null if it has none.
     */
    public ControllerConfig getControllerConfig() {
        Optional<ControllerConfig> controllerConfig = this.controllerConfig;
        if (controllerConfig == null) {
            controllerConfig = Optional.ofNullable(extWebappInfo.getControllerConfig());
            this.controllerConfig = controllerConfig;
        }
        return controllerConfig.orElse(null);
    }

    /**
     * Returns a URL builder or throws exception.
     */
    public OfbizUrlBuilder getOfbizUrlBuilder() throws IllegalArgumentException {
        OfbizUrlBuilder ofbizUrlBuilder = this.ofbizUrlBuilder;
        if (ofbizUrlBuilder == null) {
            try {
                ofbizUrlBuilder = OfbizUrlBuilder.from(this, delegator);
                this.ofbizUrlBuilder = ofbizUrlBuilder;
            } catch (Exception e) {
                throw new IllegalArgumentException(e); // caller isn't expecting null
            }
        }
        return ofbizUrlBuilder;
    }

    /*
     * ******************************************************
     * Helpers
     * ******************************************************
     */

    @Override
    public int hashCode() {
        return getContextPath().hashCode();
    }

    @Override
    public boolean equals(Object obj) {
        return (this == obj) ||
               ((obj instanceof FullWebappInfo) && this.getContextPath().equals(((FullWebappInfo) obj).getContextPath()));
    }

    public boolean equalsProtoHostPort(FullWebappInfo other) {
        return this.getWebSiteProperties().equalsProtoHostPort(other.getWebSiteProperties());
    }

    public boolean equalsProtoHostPortWithHardDefaults(FullWebappInfo other) {
        return this.getWebSiteProperties().equalsProtoHostPortWithHardDefaults(other.getWebSiteProperties());
    }

    @Override
    public String toString() {
        return "[contextPath=" + getContextPath() + ", webSiteId=" + getWebSiteId() + "]";
    }

    /*
     * ******************************************************
     * ExtWebappInfo-delegated methods
     * ******************************************************
     */

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getWebSiteId()
     */
    public String getWebSiteId() {
        return extWebappInfo.getWebSiteId();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getWebappInfo()
     */
    public WebappInfo getWebappInfo() {
        return extWebappInfo.getWebappInfo();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getWebXml()
     */
    public WebXml getWebXml() {
        return extWebappInfo.getWebXml();
    }

    public String getWebappName() {
        return extWebappInfo.getWebappName();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getContextPath()
     */
    public String getContextPath() {
        return extWebappInfo.getContextPath();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getControlServletPath()
     */
    public String getControlServletPath() {
        return extWebappInfo.getControlServletPath();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getControlServletMapping()
     */
    public String getControlServletMapping() {
        return extWebappInfo.getControlServletMapping();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getFullControlPath()
     */
    public String getFullControlPath() {
        return extWebappInfo.getFullControlPath();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getContextParams()
     */
    public Map<String, String> getContextParams() {
        return extWebappInfo.getContextParams();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getForwardRootControllerUris()
     */
    public Boolean getForwardRootControllerUris() {
        return extWebappInfo.getForwardRootControllerUris();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getForwardRootControllerUrisValidated()
     */
    public Boolean getForwardRootControllerUrisValidated() {
        return extWebappInfo.getForwardRootControllerUrisValidated();
    }


    /**
     * WARN: not thread-safe at current time (meant for request scope only).
     */
    public static class Cache {
        private FullWebappInfo currentWebappInfo;
        private WebSiteProperties defaultWebSiteProperties;
        private OfbizUrlBuilder defaultOfbizUrlBuilder;
        private Map<String, FullWebappInfo> webSiteIdCache = new HashMap<>();
        private Map<String, FullWebappInfo> contextPathCache = new HashMap<>();

        protected Cache(Delegator delegator) {
            //this.delegator = delegator;
        }

        public static Cache newCache(Delegator delegator) {
            return new Cache(delegator);
        }
        
        public static Cache fromRequest(HttpServletRequest request) {
            Cache cache = (Cache) request.getAttribute("scpFullWebappInfoCache");
            if (cache == null) {
                cache = new Cache((Delegator) request.getAttribute("delegator"));
                request.setAttribute("scpFullWebappInfoCache", cache);
            }
            return cache;
        }

        public static Cache fromContext(Map<String, Object> context, RenderEnvType renderEnvType) {
            if (renderEnvType.isStatic()) {
                @SuppressWarnings("unchecked")
                Map<String, Object> srcContext = (Map<String, Object>) context.get("globalContext");
                if (srcContext == null) srcContext = context; // fallback
    
                Cache cache = (Cache) srcContext.get("scpFullWebappInfoCache");
                if (cache == null) {
                    cache = new Cache((Delegator) context.get("delegator"));
                    srcContext.put("scpFullWebappInfoCache", cache);
                }
                return cache;
            } else if (renderEnvType.isWebapp()) {
                return fromRequest((HttpServletRequest) context.get("request"));
            }
            return null;
        }

        public static Cache fromRequestOrContext(HttpServletRequest request, Map<String, Object> context,
                RenderEnvType renderEnvType) {
            return (request != null) ? fromRequest(request) : fromContext(context, renderEnvType);
        }

        public FullWebappInfo getCurrentWebappInfo() {
            return currentWebappInfo;
        }
        
        public String getCurrentWebappWebSiteId() {
            return (currentWebappInfo != null) ? currentWebappInfo.getWebSiteId() : null;
        }

        public void setCurrentWebappInfo(FullWebappInfo currentWebappInfo) {
            this.currentWebappInfo = currentWebappInfo;
            addWebappInfo(currentWebappInfo);
        }
        
        public void setCurrentWebappInfoOnly(FullWebappInfo currentWebappInfo) {
            this.currentWebappInfo = currentWebappInfo;
        }

        public WebSiteProperties getDefaultWebSiteProperties() {
            return defaultWebSiteProperties;
        }

        public void setDefaultWebSiteProperties(WebSiteProperties defaultWebSiteProperties) {
            this.defaultWebSiteProperties = defaultWebSiteProperties;
        }

        public OfbizUrlBuilder getDefaultOfbizUrlBuilder() {
            return defaultOfbizUrlBuilder;
        }

        public void setDefaultOfbizUrlBuilder(OfbizUrlBuilder defaultOfbizUrlBuilder) {
            this.defaultOfbizUrlBuilder = defaultOfbizUrlBuilder;
        }

        public void addWebappInfo(FullWebappInfo webappInfo) {
            contextPathCache.put(webappInfo.getContextPath(), webappInfo);
            if (webappInfo.getWebSiteId() != null) {
                webSiteIdCache.put(webappInfo.getWebSiteId(), webappInfo);
            }
        }

        public FullWebappInfo getByWebSiteId(String webSiteId) {
            return webSiteIdCache.get(webSiteId);
        }

        public FullWebappInfo getByContextPath(String contextPath) {
            return contextPathCache.get(contextPath);
        }
    }

}

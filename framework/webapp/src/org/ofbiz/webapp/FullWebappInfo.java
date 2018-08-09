package org.ofbiz.webapp;

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
import org.ofbiz.webapp.control.WebAppConfigurationException;
import org.ofbiz.webapp.renderer.RenderEnvType;
import org.ofbiz.webapp.website.WebSiteProperties;
import org.ofbiz.webapp.website.WebSiteWorker;

import com.ilscipio.scipio.ce.util.Optional;

/**
 * SCIPIO: Contextual, "full" information about a webapp, including
 * webSiteId, WebappInfo, ExtWebappInfo, WebSiteProperties,
 * and ControllerConfig - with webapp request or static rendering context scope.
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
 * All factory methods may throw IllegalArgumentException.
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
            Optional<ControllerConfig> controllerConfig, OfbizUrlBuilder ofbizUrlBuilder) {
        this.delegator = delegator; // can be null if all others are non-null
        this.extWebappInfo = extWebappInfo;
        this.webSiteProperties = webSiteProperties;
        this.controllerConfig = controllerConfig;
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
     * Intra-webapp live request factory methods
     * ******************************************************
     */

    protected static FullWebappInfo newFromRequest(HttpServletRequest request) throws IllegalArgumentException {
        try {
            // SPECIAL: in this case we must initialize WebSiteProperties immediately because
            // we can't store the HttpServletRequest object in FullWebappInfo
            OfbizUrlBuilder ofbizUrlBuilder = OfbizUrlBuilder.from(request);
            return new FullWebappInfo((Delegator) request.getAttribute("delegator"),
                    ExtWebappInfo.fromContextPath(request.getContextPath()),
                    ofbizUrlBuilder.getWebSiteProperties(),
                    Optional.ofNullable(ofbizUrlBuilder.getControllerConfig()),
                    ofbizUrlBuilder);
        } catch (GenericEntityException e) {
            throw new IllegalArgumentException(e);
        } catch (WebAppConfigurationException e) {
            throw new IllegalArgumentException(e);
        }
    }
    
    /**
     * Gets webapp info for current webapp from request.
     * For intra-webapp links.
     */
    public static FullWebappInfo fromRequest(HttpServletRequest request) throws IllegalArgumentException {
        return fromRequest(request, Cache.fromRequest(request));
    }

    /**
     * Gets webapp info for current webapp from request.
     * For intra-webapp links.
     */
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
     * Reads webapp info from request, but if the request does not appear set up properly
     * yet (i.e. by ContextFilter), prevent caching the result, so as to not pollute the rest
     * of the request with an uncertain state.
     * For intra-webapp links.
     * <p>
     * DEV NOTE: WARN: This is dirty to maintain, beware.
     */
    public static FullWebappInfo fromRequestFilterSafe(HttpServletRequest request) throws IllegalArgumentException {
        if (request.getAttribute("delegator") != null) {
            // request appears set up already (e.g. by ContextFilter)
            return fromRequest(request);
        } else {
            // no delegator, request is not set up
            request.setAttribute("delegator", WebAppUtil.getDelegatorFilterSafe(request));
            boolean hadOfbizUrlBuilderAttr = (request.getAttribute("_OFBIZ_URL_BUILDER_") != null);
            boolean hadWebSitePropsAttr = (request.getAttribute("_WEBSITE_PROPS_") != null);
            boolean addedControlPathAttr = false;
            if (request.getAttribute("_CONTROL_PATH_") == null) {
                // From ControlServlet logic
                addedControlPathAttr = true;
                String contextPath = request.getContextPath();
                if (contextPath == null || "/".equals(contextPath)) {
                    contextPath = "";
                }
                request.setAttribute("_CONTROL_PATH_", contextPath + request.getServletPath());
            }
            try {
                return newFromRequest(request);
            } finally {
                // do not let filter state affect rest of request - clear cached objects
                if (!hadOfbizUrlBuilderAttr) request.removeAttribute("_OFBIZ_URL_BUILDER_");
                if (!hadWebSitePropsAttr) request.removeAttribute("_WEBSITE_PROPS_");
                if (addedControlPathAttr) request.removeAttribute("_CONTROL_PATH_");
                Cache.clearRequestCache(request);
                request.removeAttribute("delegator");
            }
        }
    }
    
    /*
     * ******************************************************
     * Inter-webapp live request factory methods
     * ******************************************************
     */

    protected static FullWebappInfo newFromWebapp(HttpServletRequest request, ExtWebappInfo extWebappInfo) throws IllegalArgumentException {
        try {
            // SPECIAL: in this case we must initialize WebSiteProperties immediately because
            // we can't store the HttpServletRequest object in FullWebappInfo
            OfbizUrlBuilder ofbizUrlBuilder = OfbizUrlBuilder.from(extWebappInfo, request);
            return new FullWebappInfo((Delegator) request.getAttribute("delegator"),
                    extWebappInfo,
                    ofbizUrlBuilder.getWebSiteProperties(),
                    Optional.ofNullable(ofbizUrlBuilder.getControllerConfig()),
                    ofbizUrlBuilder);
        } catch (GenericEntityException e) {
            throw new IllegalArgumentException(e);
        } catch (WebAppConfigurationException e) {
            throw new IllegalArgumentException(e);
        }
    }
    
    /**
     * Gets full webapp info, using webapp request for caching and context information.
     * For inter-webapp links.
     */
    public static FullWebappInfo fromWebapp(HttpServletRequest request, ExtWebappInfo extWebappInfo) throws IllegalArgumentException {
        return fromWebapp(request, extWebappInfo, Cache.fromRequest(request));
    }
    
    /**
     * Gets full webapp info, using webapp request for caching and context information.
     * For inter-webapp links.
     */
    public static FullWebappInfo fromWebapp(HttpServletRequest request, ExtWebappInfo extWebappInfo, Cache cache) throws IllegalArgumentException {
        if (cache == null) return newFromWebapp(request, extWebappInfo);
        FullWebappInfo fullWebappInfo = cache.getByContextPath(extWebappInfo.getContextPath());
        if (fullWebappInfo == null) {
            fullWebappInfo = newFromWebapp(request, extWebappInfo);
            cache.addWebappInfo(fullWebappInfo);
        }
        return fullWebappInfo;
    }

    /*
     * ******************************************************
     * Intra-webapp static context factory methods
     * ******************************************************
     */

    /**
     * Gets webapp info for current webapp from render context.
     * For intra-webapp links.
     * <p>
     * NOTE: This only works if there is a webSiteId (or baseWebSiteId) in context..
     * For now only webapp requests can return an instance without webSiteId.
     */
    public static FullWebappInfo fromContext(Map<String, Object> context, RenderEnvType renderEnvType) throws IllegalArgumentException {
        return fromContext(context, renderEnvType, Cache.fromContext(context, renderEnvType));
    }

    /**
     * Gets webapp info for current webapp from render context.
     * For intra-webapp links.
     * <p>
     * NOTE: This only works if there is a webSiteId (or baseWebSiteId) in context..
     * For now only webapp requests can return an instance without webSiteId.
     */
    public static FullWebappInfo fromContext(Map<String, Object> context) throws IllegalArgumentException {
        RenderEnvType renderEnvType = RenderEnvType.fromContext(context);
        return fromContext(context, renderEnvType, Cache.fromContext(context, renderEnvType));
    }

    /**
     * Gets webapp info for current webapp from render context.
     * For intra-webapp links.
     * <p>
     * NOTE: This only works if there is a webSiteId (or baseWebSiteId) in context..
     * For now only webapp requests can return an instance without webSiteId.
     */
    public static FullWebappInfo fromContext(Map<String, Object> context, RenderEnvType renderEnvType, Cache cache) throws IllegalArgumentException {
        if (renderEnvType.isStatic()) {
            FullWebappInfo fullWebappInfo;
            if (cache != null) {
                fullWebappInfo = cache.getCurrentWebappInfo();
                if (fullWebappInfo != null) return fullWebappInfo;
            }
            String webSiteId = WebSiteWorker.getWebSiteIdFromContext(context, renderEnvType);
            if (webSiteId != null) {
                fullWebappInfo = FullWebappInfo.fromWebapp((Delegator) context.get("delegator"), 
                        ExtWebappInfo.fromWebSiteId(webSiteId), cache);
                if (cache != null) cache.setCurrentWebappInfoOnly(fullWebappInfo); //
            }
        } else if (renderEnvType.isWebapp()) { // NOTE: it is important to check isWebapp here and not (request != null), because these could disassociate in future
            return fromRequest((HttpServletRequest) context.get("request"), cache);
        }
        return null;
    }
    
    /*
     * ******************************************************
     * Inter-webapp static context factory methods
     * ******************************************************
     */
    
    /**
     * Gets webapp info, using render context for caching and context information.
     * For inter-webapp links.
     */
    public static FullWebappInfo fromWebapp(Map<String, Object> context, ExtWebappInfo extWebappInfo) throws IllegalArgumentException {
        return fromWebapp(context, extWebappInfo, Cache.fromContext(context));
    }

    /**
     * Gets webapp info, using render context for caching and context information.
     * For inter-webapp links.
     */
    public static FullWebappInfo fromWebapp(Map<String, Object> context, ExtWebappInfo extWebappInfo, Cache cache) throws IllegalArgumentException {
        if (cache == null) return new FullWebappInfo((Delegator) context.get("delegator"), extWebappInfo);
        FullWebappInfo fullWebappInfo = cache.getByContextPath(extWebappInfo.getContextPath());
        if (fullWebappInfo == null) {
            fullWebappInfo = new FullWebappInfo((Delegator) context.get("delegator"), extWebappInfo);
            cache.addWebappInfo(fullWebappInfo);
        }
        return fullWebappInfo;
    }
    
    /**
     * Gets webapp info, without any context information (low-level no-context factory method).
     * For inter-webapp links.
     * <p>
     * WARN: Prefer ones with context or request instead wherever available.
     * This method has less information to work with compared to request and context overloads.
     */
    public static FullWebappInfo fromWebapp(Delegator delegator, ExtWebappInfo extWebappInfo, Cache cache) throws IllegalArgumentException {
        if (cache == null) return new FullWebappInfo(delegator, extWebappInfo);
        FullWebappInfo fullWebappInfo = cache.getByContextPath(extWebappInfo.getContextPath());
        if (fullWebappInfo == null) {
            fullWebappInfo = new FullWebappInfo(delegator, extWebappInfo);
            cache.addWebappInfo(fullWebappInfo);
        }
        return fullWebappInfo;
    }

    /*
     * ******************************************************
     * High-level/combination factory methods
     * ******************************************************
     */
    
    /**
     * Gets webapp info, using info/cache from request if available otherwise context (high-level factory method).
     */
    public static FullWebappInfo fromWebapp(HttpServletRequest request, Map<String, Object> context, ExtWebappInfo extWebappInfo) throws IllegalArgumentException {
        if (request != null) {
            return fromWebapp(request, extWebappInfo, Cache.fromRequest(request));
        } else {
            return fromWebapp(context, extWebappInfo, Cache.fromContext(context));
        }
    }
    
    /**
     * Gets webapp info for webSiteId or contextPath, otherwise null (high-level factory method).
     * Caches in request if available, otherwise context.
     */
    public static FullWebappInfo fromWebSiteIdOrContextPathOrNull(HttpServletRequest request, 
            Map<String, Object> context, String webSiteId, String contextPath) throws IllegalArgumentException {
        if (UtilValidate.isNotEmpty(webSiteId)) {
            return fromWebapp(request, context, ExtWebappInfo.fromWebSiteId(webSiteId));
        } else if (UtilValidate.isNotEmpty(contextPath)) {
            return fromWebapp(request, context, ExtWebappInfo.fromContextPath(contextPath));
        }
        return null;
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
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#hasUrlRewriteFilter()
     */
    public boolean hasUrlRewriteFilter() {
        return extWebappInfo.hasUrlRewriteFilter();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getUrlRewriteConfPath()
     */
    public String getUrlRewriteConfPath() {
        return extWebappInfo.getUrlRewriteConfPath();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getUrlRewriteFullConfPath()
     */
    public String getUrlRewriteFullConfPath() {
        return extWebappInfo.getUrlRewriteFullConfPath();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#getUrlRewriteRealConfPath()
     */
    public String getUrlRewriteRealConfPath() {
        return extWebappInfo.getUrlRewriteRealConfPath();
    }

    /**
     * @return
     * @see org.ofbiz.webapp.ExtWebappInfo#useUrlManualInterWebappFilter()
     */
    public boolean useUrlManualInterWebappFilter() {
        return extWebappInfo.useUrlManualInterWebappFilter();
    }

    /**
     * WARN: not thread-safe at current time (meant for request scope only).
     */
    public static class Cache {

        public static final String FIELD_NAME = "scpFWICache";

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
            Cache cache = (Cache) request.getAttribute(FIELD_NAME);
            if (cache == null) {
                cache = new Cache((Delegator) request.getAttribute("delegator"));
                request.setAttribute(FIELD_NAME, cache);
            }
            return cache;
        }

        public static void clearRequestCache(HttpServletRequest request) {
            request.removeAttribute(FIELD_NAME);
        }

        public static Cache fromContext(Map<String, Object> context, RenderEnvType renderEnvType) {
            if (renderEnvType.isStatic()) {
                @SuppressWarnings("unchecked")
                Map<String, Object> srcContext = (Map<String, Object>) context.get("globalContext");
                if (srcContext == null) srcContext = context; // fallback

                Cache cache = (Cache) srcContext.get(FIELD_NAME);
                if (cache == null) {
                    cache = new Cache((Delegator) context.get("delegator"));
                    srcContext.put(FIELD_NAME, cache);
                }
                return cache;
            } else if (renderEnvType.isWebapp()) { // NOTE: it is important to check isWebapp here and not (request != null), because these could disassociate in future
                return fromRequest((HttpServletRequest) context.get("request"));
            }
            return null;
        }

        public static Cache fromContext(Map<String, Object> context) {
            return fromContext(context, RenderEnvType.fromContext(context)); // TODO?: optimize
        }

        public static void clearContextCache(Map<String, Object> context, RenderEnvType renderEnvType) {
            if (renderEnvType.isStatic()) {
                @SuppressWarnings("unchecked")
                Map<String, Object> srcContext = (Map<String, Object>) context.get("globalContext");
                if (srcContext == null) srcContext = context; // fallback

                srcContext.remove(FIELD_NAME);
            } else if (renderEnvType.isWebapp()) {
                clearRequestCache((HttpServletRequest) context.get("request"));
            }
        }

        public static void clearContextCache(Map<String, Object> context) {
            clearContextCache(context, RenderEnvType.fromContext(context)); // TODO?: optimize 
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

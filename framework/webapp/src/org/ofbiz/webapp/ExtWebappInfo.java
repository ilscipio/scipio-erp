package org.ofbiz.webapp;

import java.io.IOException;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.tomcat.util.descriptor.web.FilterDef;
import org.apache.tomcat.util.descriptor.web.WebXml;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.control.ContextFilter;
import org.xml.sax.SAXException;

import com.ilscipio.scipio.ce.util.Optional;

/**
 * SCIPIO: Extended webapp info class, which contains both component WebappInfo
 * and WebXml info in a single class linked to the webSiteId in a global cache.
 * <p>
 * It will cache several settings to avoid constantly re-traversing the WebXml
 * and preload a bunch of calls from {@link WebAppUtil}.
 * <p>
 * NOTE: The first accesses have to be done at late enough times in the loading, because
 * the instances are cached, and it would be bad to cache partially-loaded results.
 * In other words this should not be accessed within filter or servlet init methods.
 * If you need an instance in such times, call the {@link #fromWebSiteIdNew} method.
 * TODO?: We could code the webapp loader to invoke {@link #clearCaches()} at end of loading
 * to avoid issues.
 */
@SuppressWarnings("serial")
public class ExtWebappInfo implements Serializable {
    
    public static final String module = ExtWebappInfo.class.getName();
    private static final String contextFilterClassName = ContextFilter.class.getSimpleName();
    private static final boolean LOGGING = Debug.infoOn();
    
    private static final Object webSiteIdCacheLock = new Object();
    private static Map<String, ExtWebappInfo> webSiteIdCache = Collections.emptyMap();
    
    // REQUIRED FIELDS (for this instance to exist)
    private final String webSiteId;
    private final WebappInfo webappInfo;
    private final WebXml webXml;
    
    // OPTIONAL FIELDS (instance may be created/cached even if lookup fails for these)
    private String controlServletPath;
    private String controlServletMapping; // /control (servlet)
    private String fullControlPath;
    
    private Optional<Boolean> forwardRootControllerUris;
    private Optional<Boolean> forwardRootControllerUrisValid;
    
    /**
     * Basic constructor.
     */
    public ExtWebappInfo(String webSiteId, WebappInfo webappInfo, WebXml webXml) {
        this.webSiteId = webSiteId;
        this.webappInfo = webappInfo;
        this.webXml = webXml;
    }
    
    /**
     * Full constructor.
     * WARN: subject to change frequently.
     */
    protected ExtWebappInfo(String webSiteId, WebappInfo webappInfo, WebXml webXml, String controlServletPath, String controlServletMapping,
            String fullControlPath) {
        this.webSiteId = webSiteId;
        this.webappInfo = webappInfo;
        this.webXml = webXml;
        if (controlServletMapping == null )
        this.controlServletPath = controlServletPath;
        this.controlServletMapping = controlServletMapping;
        this.fullControlPath = fullControlPath;
    }

    /**
     * Clears all the ExtWebSiteInfo global caches.
     */
    public static void clearCaches() {
        synchronized(webSiteIdCacheLock) { // this only prevents other thread from accidentally restoring the map we just deleted
            webSiteIdCache = Collections.emptyMap();
        }
    }
    
    /**
     * Gets from webSiteId, with caching.
     * Cache only allows websites with registered WebappInfo and WebXml.
     * NOTE: If accessing from early loading process, do not call this, instead call
     * {@link #fromWebSiteIdNew(String)}.
     */
    public static ExtWebappInfo fromWebSiteId(String webSiteId) throws IOException {
        ExtWebappInfo info = webSiteIdCache.get(webSiteId);
        if (info != null) return info;
        synchronized(webSiteIdCacheLock) {
            info = webSiteIdCache.get(webSiteId);
            if (info != null) return info;
            
            info = fromWebSiteIdNew(webSiteId); 
            
            // copy cache for synch semantics
            Map<String, ExtWebappInfo> newCache = new HashMap<>(webSiteIdCache);
            newCache.put(webSiteId, info);
            webSiteIdCache = Collections.unmodifiableMap(webSiteIdCache);
        }
        return info;
    }
    
    /**
     * Gets from webSiteId, no caching.
     */
    public static ExtWebappInfo fromWebSiteIdNew(String webSiteId) throws IOException {
        String controlServletPath = null;
        String controlServletMapping = null;
        String fullControlPath = null;
        
        WebappInfo webappInfo;
        WebXml webXml;
        try {
            webappInfo = WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
            webXml = WebAppUtil.getWebXml(webappInfo);
        } catch (SAXException e) {
            throw new IOException(e);
        }
        
        try {
            fullControlPath = WebAppUtil.getControlServletPath(webappInfo);
        } catch (IllegalArgumentException e) {
            // DO NOT fail - usually will be an error, but not necessarily is.
            Debug.logWarning("Cannot get control servlet path for website '" + webSiteId + "': " + e.getMessage(), module);
        } catch (SAXException e) {
            throw new IOException(e);
        }
        controlServletMapping = WebAppUtil.getControlServletOnlyPathFromFull(webappInfo, fullControlPath);
        controlServletPath = controlServletMapping;
        if ("/".equals(controlServletPath)) controlServletPath = "";
        
        return new ExtWebappInfo(webSiteId, webappInfo, webXml, controlServletPath, controlServletMapping, fullControlPath);
    }

    public String getWebSiteId() {
        return webSiteId;
    }

    public WebappInfo getWebappInfo() {
        return webappInfo;
    }
    
    public WebXml getWebXml() {
        return webXml;
    }
    
    public String getContextRoot() {
        return (webappInfo != null) ? webappInfo.getContextRoot() : null;
    }
    
    /**
     * The control servlet mapping or empty string "" if set as root.
     * This is same as {@link #getControlServletMapping()} except the empty string,
     * menat for easy prepending.
     */
    public String getControlServletPath() {
        return controlServletPath;
    }

    /**
     * The control servlet mapping or "/" if set as root.
     */
    public String getControlServletMapping() {
        return controlServletMapping;
    }
    
    /**
     * Essentially {@link #getContextRoot()} + {@link #getControlServletMapping()}.
     */
    public String getFullControlPath() {
        return fullControlPath;
    }
    
    public Map<String, String> getContextParams() {
        return webXml.getContextParams();
    }
    
    /**
     * Returns whether the forwardRootControllerUris ContextFilter settings is
     * on, off, or undetermined (null).
     */
    public Boolean getForwardRootControllerUris() {
        Optional<Boolean> forwardRootControllerUris = this.forwardRootControllerUris;
        if (forwardRootControllerUris == null) {
            forwardRootControllerUris = Optional.ofNullable(readForwardRootControllerUrisSetting(LOGGING));
            this.forwardRootControllerUris = forwardRootControllerUris;
        }
        return forwardRootControllerUris.orElse(null);
    }

    /**
     * Returns whether the forwardRootControllerUris ContextFilter settings is
     * on, off, or undetermined (null), and whether the setting makes sense
     * against the current control mount and potentially other settings.
     */
    public Boolean getForwardRootControllerUrisValidated() {
        Optional<Boolean> forwardRootControllerUrisValid = this.forwardRootControllerUrisValid;
        if (forwardRootControllerUrisValid == null) {
            forwardRootControllerUrisValid = Optional.ofNullable(verifyForwardRootControllerUrisSetting(getForwardRootControllerUris(), LOGGING));
            this.forwardRootControllerUrisValid = forwardRootControllerUrisValid;
        }
        return forwardRootControllerUrisValid.orElse(null);
    }

    /**
     * Reads the forwardRootControllerUris value from ContextFilter init-params and
     * extra checks. Does not cache.
     */
    Boolean readVerifyForwardRootControllerUrisSetting(boolean log) {
        // NOTE: in this case, we can never know for sure that alias should be false,
        // so we leave null if not found (more info to caller)
        Boolean hasContextFilterFlag = readForwardRootControllerUrisSetting(log);
        return verifyForwardRootControllerUrisSetting(hasContextFilterFlag, log);
    }
    
    Boolean verifyForwardRootControllerUrisSetting(Boolean hasContextFilterFlag, boolean log) {
        if (hasContextFilterFlag != null) {
            if (hasContextFilterFlag) {
                String controlPath = getControlServletMapping();
                if (controlPath != null && controlPath.startsWith("/") && controlPath.length() >= 2) {
                    // aliasing is enabled and controlPath is not the root, so pass
                    if (log) Debug.logInfo("Website '" + webSiteId 
                            + "': web.xml appears to have ContextFilter init-param forwardRootControllerUris enabled"
                            + " and a non-root control servlet mapping (" + controlPath 
                            + "); now treating website as having enabled root aliasing/forwarding/rewriting of controller URIs", module);
                    return true;
                } else {
                    Debug.logWarning("Website '" + webSiteId 
                            + "': web.xml invalid configuration: ContextFilter init-param forwardRootControllerUris is enabled"
                            + ", but control servlet mapping (" + controlPath 
                            + ") appears it may be mapped to root - invalid configuration", module);
                }
            }
        }
        return null;
    }
    
    /**
     * Reads the forwardRootControllerUris value from ContextFilter init-params.
     * Does not verify if the setting appears valid in context. Does not cache.
     */
    Boolean readForwardRootControllerUrisSetting(boolean log) {
        // HEURISTIC: we return the value of the highest-rated ContextFilter that
        // has forwardRootControllerUris present in its init-params (even if empty)
        
        int bestRating = 0;
        String aliasStr = null;
        String filterName = null;
        for(FilterDef filter : webXml.getFilters().values()) {
            int currentRating = rateContextFilterCandidate(filter);
            if (currentRating > 0) {
                Map<String, String> initParams = filter.getParameterMap();
                if (initParams != null && initParams.containsKey("forwardRootControllerUris")) {
                    if (currentRating > bestRating) {
                        bestRating = currentRating;
                        aliasStr = initParams.get("forwardRootControllerUris");
                        filterName = filter.getFilterName();
                    }
                }
            }
        }
        if (bestRating > 0) {
            Boolean alias = UtilMisc.booleanValueVersatile(aliasStr);
            if (alias != null) {
                if (log) Debug.logInfo("Website '" + webSiteId 
                        + "': Found web.xml ContextFilter (filter name '" + filterName + "') init-param forwardRootControllerUris boolean value: " + alias, module);
                return alias;
            } else {
                if (UtilValidate.isNotEmpty(aliasStr)) {
                    Debug.logError("Website '" + webSiteId 
                            + "': web.xml ContextFilter (filter name '" + filterName + "') init-param forwardRootControllerUris has invalid boolean value: " + aliasStr, module);
                } else {
                    if (log) Debug.logInfo("Website '" + webSiteId 
                            + "': Found web.xml ContextFilter (filter name '" + filterName + "') init-param forwardRootControllerUris, was empty; returning as unset", module);
                    return null;
                }
            }
        } else {
            if (log) Debug.logInfo("Website '" + webSiteId 
                        + "': web.xml ContextFilter init-param forwardRootControllerUris setting not found", module);
        }
        return null;
    }
 
    /**
     * Heuristic for finding the most probable real ContextFilter.
     * NOTE: This is a problem because OFbiz frustratingly made a bunch of other classes
     * extend ContextFilter, and even scipio is forced to compound the problem as a result
     * of lack of good base classes.
     */
    private int rateContextFilterCandidate(FilterDef filter) {
        String filterClass = filter.getFilterClass();
        if (filterClass == null || filterClass.isEmpty()) return 0;
        
        // NOTE: this exact-class check is what stock code does for some other classes, so we have to follow suit
        if (ContextFilter.class.getName().equals(filterClass)) return 5;
        
        try {
            Class<?> filterCls = Thread.currentThread().getContextClassLoader().loadClass(filterClass);
            if (!ContextFilter.class.isAssignableFrom(filterCls)) return 0;
        } catch(Exception e) {
            Debug.logWarning("Could not loadClass filter; may be invalid or classloader issue: " 
                    + filterClass + ": " + e.getMessage(), module);
            return 0;
        }
        
        String filterName = filter.getFilterName();
        
        if (contextFilterClassName.equals(filterName)) return 4;
        
        if (filterName != null && filterName.contains(contextFilterClassName)) {
            if (filterClass.contains(contextFilterClassName)) return 3;
        } else {
            if (filterClass.contains(contextFilterClassName)) return 2;
        }
        // 1: at least is subclass, but lowest because stock Ofbiz overextended ContextFilter everywhere
        return 1;
    }

}
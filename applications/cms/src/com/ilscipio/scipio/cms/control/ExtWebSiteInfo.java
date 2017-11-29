package com.ilscipio.scipio.cms.control;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.apache.tomcat.util.descriptor.web.FilterDef;
import org.apache.tomcat.util.descriptor.web.WebXml;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.WebAppUtil;
import org.ofbiz.webapp.control.ContextFilter;
import org.xml.sax.SAXException;

/**
 * Extended (mostly static) website info in single class with caching.
 * <p>
 * TODO: move for reuse, needed elsewhere
 */
@SuppressWarnings("serial")
public class ExtWebSiteInfo implements Serializable {
    
    public static final String module = ExtWebSiteInfo.class.getName();
    
    private static final Object webSiteIdCacheLock = new Object();
    private static Map<String, ExtWebSiteInfo> webSiteIdCache = Collections.emptyMap();
    
    // REQUIRED FIELDS (for this instance to exist)
    private final String webSiteId;
    private final WebappInfo webappInfo;
    private final WebXml webXml;
    
    // OPTIONAL FIELDS (instance may be created/cached even if lookup fails for these)
    private String controlMapping; // /control (servlet)
    private String fullControlPath;
    
    private Boolean forwardRootControllerUris;
    private Boolean forwardRootControllerUrisValid;
    
    /**
     * Full constructor.
     * WARN: subject to change frequently.
     */
    public ExtWebSiteInfo(String webSiteId, WebappInfo webappInfo, WebXml webXml, String controlMapping,
            String fullControlPath) {
        this.webSiteId = webSiteId;
        this.webappInfo = webappInfo;
        this.webXml = webXml;
        this.controlMapping = controlMapping;
        this.fullControlPath = fullControlPath;
    }
    
    /**
     * Gets from webSiteId, with caching.
     * Cache only allows websites with registered WebappInfo and WebXml.
     */
    public static ExtWebSiteInfo fromWebSiteId(String webSiteId) throws IOException {
        ExtWebSiteInfo info = webSiteIdCache.get(webSiteId);
        if (info != null) return info;
        synchronized(webSiteIdCacheLock) {
            info = webSiteIdCache.get(webSiteId);
            if (info != null) return info;
            
            info = fromWebSiteIdNew(webSiteId); 
            
            // copy cache for synch semantics
            Map<String, ExtWebSiteInfo> newCache = new HashMap<>(webSiteIdCache);
            newCache.put(webSiteId, info);
            webSiteIdCache = Collections.unmodifiableMap(webSiteIdCache);
        }
        return info;
    }
    
    /**
     * Gets from webSiteId, no caching.
     */
    public static ExtWebSiteInfo fromWebSiteIdNew(String webSiteId) throws IOException {
        String controlMapping = null;
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
        controlMapping = getControlServletOnlyPathFromFull(webappInfo, fullControlPath);

        return new ExtWebSiteInfo(webSiteId, webappInfo, webXml, controlMapping, fullControlPath);
    }
    
    // TODO: MOVE TO WebAppUtil - DUPLICATED
    /**
     * SCIPIO: Strips the result of a {@link #getControlServletPath} call to
     * get the control servlet mapping for given webappInfo, WITHOUT the
     * webapp context root. There is never a terminating slash, except if root,
     * where it will be "/".
     */
    private static String getControlServletOnlyPathFromFull(WebappInfo webAppInfo, String controlPath) {
        if (controlPath != null) {
            if (webAppInfo.contextRoot != null && !webAppInfo.contextRoot.isEmpty() && !"/".equals(webAppInfo.contextRoot)) {
                controlPath = controlPath.substring(webAppInfo.contextRoot.length());
            }
            if (controlPath.length() > 1 && controlPath.endsWith("/")) {
                controlPath = controlPath.substring(0, controlPath.length() - 1);
            }
            if (controlPath.length() == 0) {
                controlPath = "/";
            }
            return controlPath;
        } else {
            return null;
        }
    }
    
    
    public static void clearCaches() {
        synchronized(webSiteIdCacheLock) { // this only prevents other thread from accidentally restoring the map we just deleted
            webSiteIdCache = Collections.emptyMap();
        }
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
    
    public String getControlMapping() {
        return controlMapping;
    }
    
    public String getFullControlPath() {
        return fullControlPath;
    }
    
    public Map<String, String> getContextParams() {
        return webXml.getContextParams();
    }
    
    public Boolean getForwardRootControllerUris() {
        return forwardRootControllerUris;
    }

    public Boolean getForwardRootControllerUrisValid() {
        return forwardRootControllerUrisValid;
    }


    /**
     * Reads the forwardRootControllerUris value from ContextFilter init-params.
     * Does not verify. Does not cache.
     * TODO?: move elsewhere?
     */
    public Boolean readForwardRootControllerUrisSetting(boolean log) {
        for(FilterDef filter : webXml.getFilters().values()) {
            if (rateContextFilterCandidate(filter) > 0) {
                Map<String, String> initParams = filter.getParameterMap();
                Boolean alias = null;
                if (initParams != null) {
                    alias = UtilMisc.booleanValueVersatile(initParams.get("forwardRootControllerUris"));
                }
                if (alias != null) {
                    if (log) Debug.logInfo("Website '" + webSiteId 
                            + "': Found web.xml ContextFilter init-param forwardRootControllerUris boolean value: " + alias + "; using", module);
                    return alias;
                } else {
                    if (initParams.containsKey("forwardRootControllerUris")) {
                        if (UtilValidate.isNotEmpty((String) initParams.get("forwardRootControllerUris"))) {
                            Debug.logError("Website '" + webSiteId 
                                    + "': web.xml ContextFilter init-param forwardRootControllerUris has invalid boolean value: " + initParams.get("forwardRootControllerUris"), module);
                        } else {
                            if (log) Debug.logInfo("Website '" + webSiteId 
                                    + "': Found web.xml ContextFilter init-param forwardRootControllerUris, was empty - returning", module);
                            return null;
                        }
                    }
                }
            }
        }
        return null;
    }
 
    private static final String contextFilterName = ContextFilter.class.getSimpleName();
    
    /**
     * Heuristic.
     */
    private int rateContextFilterCandidate(FilterDef filter) {
        String filterClass = filter.getFilterClass();
        if (filterClass == null || filterClass.isEmpty()) return 0;
        
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
        
        if (contextFilterName.equals(filterName)) return 4;
        
        if (filterName != null && filterName.contains(contextFilterName)) {
            if (filterClass.contains(contextFilterName)) return 3;
        } else {
            if (filterClass.contains(contextFilterName)) return 2;
        }
        // 1: at least is subclass, but lowest because stock Ofbiz overextended ContextFilter everywhere
        return 1;
    }
    
    
    /**
     * Reads the forwardRootControllerUris value from ContextFilter init-params and
     * extra checks.
     * Does not cache.
     * TODO?: move elsewhere?
     */
    public Boolean readVerifyForwardRootControllerUrisSetting(boolean log) {
        // NOTE: in this case, we can never know for sure that alias should be false,
        // so we leave null if not found (more info to caller)
        Boolean hasContextFilterFlag = readForwardRootControllerUrisSetting(log);
        if (hasContextFilterFlag != null) {
            if (hasContextFilterFlag) {
                String controlPath = getControlMapping();
                if (controlPath != null && controlPath.startsWith("/") && controlPath.length() >= 2) {
                    // aliasing is enabled and controlPath is not the root, so pass
                    if (log) Debug.logInfo("Cms: Website '" + webSiteId 
                            + "': web.xml appears to have ContextFilter init-param forwardRootControllerUris enabled"
                            + " and a non-root control servlet mapping (" + controlPath 
                            + "); now treating website as having enabled root aliasing/forwarding/rewriting of controller URIs", module);
                    return true;
                } else {
                    Debug.logWarning("Cms: Website '" + webSiteId 
                            + "': web.xml invalid configuration: ContextFilter init-param forwardRootControllerUris is enabled"
                            + ", but control servlet mapping (" + controlPath 
                            + ") appears it may be mapped to root - invalid configuration", module);
                }
            }
        }
        return null;
    }

    
}
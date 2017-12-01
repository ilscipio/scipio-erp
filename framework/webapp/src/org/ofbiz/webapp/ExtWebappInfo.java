package org.ofbiz.webapp;

import java.io.IOException;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.tomcat.util.descriptor.web.WebXml;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.webapp.control.ContextFilter;
import org.xml.sax.SAXException;

import com.ilscipio.scipio.ce.util.Optional;

/**
 * SCIPIO: Extended webapp info class, which contains both component WebappInfo
 * and WebXml info in a single class linked to the webSiteId in a global cache.
 * Should only be used for webapps that have a defined webSiteId (in web.xml).
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
            fullControlPath = WebAppUtil.getControlServletPath(webappInfo, true);
            if (fullControlPath == null) {
                Debug.logWarning(getLogMsgPrefix(webSiteId)+"Cannot get ControlServlet mapping for website."
                        + " This is only acceptable if the webapp did not intend to have a controller.", module);
            }
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
            Boolean setting;
            try {
                setting = ContextFilter.readForwardRootControllerUrisSetting(getWebXml(), getLogMsgPrefix());
            } catch(Exception e) {
                Debug.logError(e, getLogMsgPrefix()+"Error while trying to determine forwardRootControllerUris ContextFilter setting: " 
                        + e.getMessage(), module);
                setting = null;
            }
            forwardRootControllerUris = Optional.ofNullable(setting);
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
            Boolean setting;
            try {
                setting = ContextFilter.verifyForwardRootControllerUrisSetting(getForwardRootControllerUris(), getControlServletMapping(), getLogMsgPrefix());
            } catch(Exception e) {
                Debug.logError(e, getLogMsgPrefix()+"Error while trying to determine forwardRootControllerUris ContextFilter setting: " 
                        + e.getMessage(), module);
                setting = null;
            }
            forwardRootControllerUrisValid = Optional.ofNullable(setting);
            this.forwardRootControllerUrisValid = forwardRootControllerUrisValid;
        }
        return forwardRootControllerUrisValid.orElse(null);
    }
    
    private static String getLogMsgPrefix(String webSiteId) {
        return "Website '" + webSiteId + "': ";
    }
    
    private String getLogMsgPrefix() {
        return "Website '" + webSiteId + "': ";
    }
}
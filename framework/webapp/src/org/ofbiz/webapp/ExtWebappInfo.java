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
    
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
    private static final Object webSiteIdCacheLock = new Object();
    private static Map<String, ExtWebappInfo> webSiteIdCache = Collections.emptyMap();
    
    // REQUIRED FIELDS (for this instance to exist)
    private final String webSiteId;
    private final WebappInfo webappInfo;
    private final WebXml webXml;
    
    // OPTIONAL FIELDS (instance may be created/cached even if lookup fails for these)
    private final String controlServletPath; // empty string for root
    private final String controlServletMapping; // single slash for root
    private final String fullControlPath; // with context root and trailing slash
    
    private Optional<Boolean> forwardRootControllerUris;
    private Optional<Boolean> forwardRootControllerUrisValid;
    
    /**
     * Main constructor.
     */
    protected ExtWebappInfo(String webSiteId, WebappInfo webappInfo, WebXml webXml) throws IOException {
        // sanity check
        if (webappInfo == null) throw new IllegalArgumentException("Missing ofbiz webapp info (WebappInfo) for website ID '" + webSiteId + "' - required to instantiate ExtWebappInfo");
        if (webXml == null) throw new IllegalArgumentException("Missing webapp container info (web.xml) for website ID '" + webSiteId + "' (mount-point '" + webappInfo.getContextRoot() + "')");
            
        this.webSiteId = webSiteId;
        this.webappInfo = webappInfo;
        this.webXml = webXml;
        try {
            this.fullControlPath = WebAppUtil.getControlServletPath(webappInfo, true);
            if (this.fullControlPath == null) {
                Debug.logWarning(getLogMsgPrefix(webSiteId)+"Cannot find ControlServlet mapping for website"
                        + " (this is an error if the website was meant to have a controller)", module);
            }
            
            this.controlServletMapping = WebAppUtil.getControlServletOnlyPathFromFull(webappInfo, fullControlPath);
            this.controlServletPath = ("/".equals(this.controlServletMapping)) ? "" : this.controlServletMapping;
        } catch (Exception e) {
            throw new IOException("Could not determine ControlServlet mapping for website ID '" + webSiteId + "': " + e.getMessage(), e);
        }
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
     * {@link #fromWebSiteIdNew(String)}, otherwise there is a risk of caching
     * incomplete instances.
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
     * NOTE: This is the factory method that should be used during loading.
     * @see #fromWebSiteId(String)
     */
    public static ExtWebappInfo fromWebSiteIdNew(String webSiteId) throws IOException {
        WebappInfo webappInfo = getWebappInfoAlways(webSiteId);
        WebXml webXml = getWebXmlAlways(webSiteId, webappInfo);
        return new ExtWebappInfo(webSiteId, webappInfo, webXml);
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
     * The control servlet mapping, or empty string "" if it is the root mapping.
     * No trailing slash.
     * <p>
     * This is same as {@link #getControlServletMapping()} except it uses empty string
     * instead of single slash for the root mapping, meant for easy prepending.
     */
    public String getControlServletPath() {
        return controlServletPath;
    }

    /**
     * The control servlet mapping, or "/" if it is the root mapping.
     * No trailing slash unless it's the root mapping.
     * <p>
     * This is same as {@link #getControlServletPath()} except it uses single slash
     * instead of empty string for the root mapping, meant for easy prepending.
     */
    public String getControlServletMapping() {
        return controlServletMapping;
    }
    
    /**
     * Returns the control path prefixed with the webapp context root and 
     * suffixed with trailing slash.
     * <p>
     * Essentially {@link #getContextRoot()} + {@link #getControlServletMapping()} + "/".
     * Includes a trailing slash.
     * <p>
     * NOTE: The trailing slash inconsistency is due to this coming from the stock ofbiz
     * {@link WebAppUtil#getControlServletPath(WebappInfo)}.
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
    
    
    /**
     * Helper wrapper to read WebappInfo reliably.
     */
    public static WebappInfo getWebappInfoAlways(String webSiteId) throws IOException {
        try {
            return WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
        } catch(IllegalArgumentException e) {
            throw new IOException(e); // exception message already good
        } catch (Exception e) {
            throw new IOException("Could not read or find ofbiz webapp info (WebappInfo) for website ID '" + webSiteId + "': " + e.getMessage(), e);
        }
    }
    
    /**
     * Helper wrapper to read WebXml reliably.
     */
    public static WebXml getWebXmlAlways(String webSiteId, WebappInfo webappInfo) throws IOException {
        try {
            return WebAppUtil.getWebXml(webappInfo);
        } catch(Exception e) {
            throw new IOException("Could not read or find webapp container info (web.xml) for website ID '" + webSiteId 
                        + "' (mount-point '" + webappInfo.getContextRoot() + "'): " + e.getMessage(), e);
        }
    }
}
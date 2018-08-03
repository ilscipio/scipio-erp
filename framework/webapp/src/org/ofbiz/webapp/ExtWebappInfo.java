package org.ofbiz.webapp;

import java.io.IOException;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import org.apache.tomcat.util.descriptor.web.WebXml;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.lang.ThreadSafe;
import org.ofbiz.base.util.Debug;
import org.ofbiz.webapp.control.ConfigXMLReader.ControllerConfig;
import org.ofbiz.webapp.control.ConfigXMLReader;
import org.ofbiz.webapp.control.ContextFilter;
import org.xml.sax.SAXException;

import com.ilscipio.scipio.ce.util.Optional;

/**
 * SCIPIO: Extended static webapp info class, which contains both component WebappInfo
 * and WebXml info in a single class linked to the webSiteId in a global cache.
 * Should only be used for webapps that have a defined webSiteId (in web.xml).
 * <p>
 * It will cache several settings to avoid constantly re-traversing the WebXml
 * and preload a bunch of calls from {@link WebAppUtil}.
 * <p>
 * This does not contain DB data such as WebSiteProperties, only statically-accessible
 * files and configuration.
 * <p>
 * NOTE: The first accesses have to be done at late enough times in the loading, because
 * the instances are cached, and it would be bad to cache partially-loaded results.
 * In other words this should not be accessed within filter or servlet init methods.
 * If you need an instance in such times, call the {@link #fromWebSiteIdNew} method.
 * TODO?: We could code the webapp loader to invoke {@link #clearCaches()} at end of loading
 * to avoid issues.
 */
@SuppressWarnings("serial")
@ThreadSafe
public class ExtWebappInfo implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final Object cacheLock = new Object(); // NOTE: both caches lock together
    private static Map<String, ExtWebappInfo> webSiteIdCache = Collections.emptyMap();
    private static Map<String, ExtWebappInfo> contextPathCache = Collections.emptyMap();

    private final String webSiteId;
    // REQUIRED FIELDS (for this instance to exist)
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
    protected ExtWebappInfo(String webSiteId, WebappInfo webappInfo, WebXml webXml) {
        // sanity check
        if (webappInfo == null) throw new IllegalArgumentException("Missing ofbiz webapp info (WebappInfo) for website ID '" + webSiteId + "' - required to instantiate ExtWebappInfo");
        if (webXml == null) throw new IllegalArgumentException("Missing webapp container info (web.xml) for website ID '" + webSiteId + "' (mount-point '" + webappInfo.getContextRoot() + "')");

        this.webSiteId = webSiteId;
        this.webappInfo = webappInfo;
        this.webXml = webXml;
        try {
            this.fullControlPath = WebAppUtil.getControlServletPath(webappInfo, true);
            if (this.fullControlPath == null) {
                Debug.logWarning(getLogMsgPrefix()+"Cannot find ControlServlet mapping for website"
                        + " (this is an error if the website was meant to have a controller)", module);
            }

            this.controlServletMapping = WebAppUtil.getControlServletOnlyPathFromFull(webappInfo, fullControlPath);
            this.controlServletPath = ("/".equals(this.controlServletMapping)) ? "" : this.controlServletMapping;
        } catch (Exception e) {
            throw new IllegalArgumentException("Could not determine ControlServlet mapping for website ID '" + webSiteId + "': " + e.getMessage(), e);
        }
    }

    /**
     * Clears all the ExtWebSiteInfo global caches.
     */
    public static void clearCaches() {
        synchronized(cacheLock) { // this only prevents other thread from accidentally restoring the map we just deleted
            webSiteIdCache = Collections.emptyMap();
            contextPathCache = Collections.emptyMap();
        }
    }

    /**
     * Gets from webSiteId, with caching.
     * Cache only allows websites with registered WebappInfo and WebXml.
     * NOTE: If accessing from early loading process, do not call this, instead call
     * {@link #fromWebSiteIdNew(String)}, otherwise there is a risk of caching
     * incomplete instances.
     */
    public static ExtWebappInfo fromWebSiteId(String webSiteId) throws IllegalArgumentException {
        ExtWebappInfo info = webSiteIdCache.get(webSiteId);
        if (info != null) return info;
        synchronized(cacheLock) {
            info = webSiteIdCache.get(webSiteId);
            if (info != null) return info;

            info = fromWebSiteIdNew(webSiteId);

            // copy cache for synch semantics
            Map<String, ExtWebappInfo> newCache = new HashMap<>(webSiteIdCache);
            newCache.put(webSiteId, info);
            webSiteIdCache = Collections.unmodifiableMap(newCache);

            // also put into context root cache to prevent duplicates
            newCache = new HashMap<>(contextPathCache);
            newCache.put(info.getContextPath(), info);
            contextPathCache = Collections.unmodifiableMap(newCache);
        }
        return info;
    }

    /**
     * Gets from webSiteId, no caching.
     * NOTE: This is the factory method that should be used during loading.
     * @see #fromWebSiteId(String)
     */
    public static ExtWebappInfo fromWebSiteIdNew(String webSiteId) throws IllegalArgumentException {
        WebappInfo webappInfo = getWebappInfoAlways(webSiteId);
        WebXml webXml = getWebXmlAlways(webSiteId, webappInfo);
        return new ExtWebappInfo(webSiteId, webappInfo, webXml);
    }

    /**
     * Gets from contextPath, with caching.
     * Cache only allows websites with registered WebappInfo and WebXml.
     * NOTE: If accessing from early loading process, do not call this, instead call
     * {@link #fromWebSiteIdNew(String)}, otherwise there is a risk of caching
     * incomplete instances.
     */
    public static ExtWebappInfo fromContextPath(String contextPath) throws IllegalArgumentException {
        ExtWebappInfo info = contextPathCache.get(contextPath);
        if (info != null) return info;
        synchronized(cacheLock) {
            info = contextPathCache.get(contextPath);
            if (info != null) return info;

            info = fromContextPathNew(contextPath);

            // copy cache for synch semantics
            Map<String, ExtWebappInfo> newCache = new HashMap<>(contextPathCache);
            newCache.put(contextPath, info);
            contextPathCache = Collections.unmodifiableMap(contextPathCache);

            if (info.getWebSiteId() != null) {
                newCache = new HashMap<>(webSiteIdCache);
                newCache.put(info.getWebSiteId(), info);
                webSiteIdCache = Collections.unmodifiableMap(newCache);
            }
        }
        return info;
    }
    
    /**
     * Gets from arbitary path, with caching.
     * Cache only allows websites with registered WebappInfo and WebXml.
     * NOTE: If accessing from early loading process, do not call this, instead call
     * {@link #fromWebSiteIdNew(String)}, otherwise there is a risk of caching
     * incomplete instances.
     */
    public static ExtWebappInfo fromPath(String path) throws IllegalArgumentException {
        WebappInfo webappInfo;
        try {
            webappInfo = WebAppUtil.getWebappInfoFromPath(path);
        } catch (IOException e) {
            throw new IllegalArgumentException(e);
        } catch (SAXException e) {
            throw new IllegalArgumentException(e);
        }
        return fromContextPath(webappInfo.getContextRoot());
    }

    /**
     * Gets from webSiteId, no caching.
     * NOTE: This is the factory method that should be used during loading.
     * @see #fromWebSiteId(String)
     */
    public static ExtWebappInfo fromContextPathNew(String contextPath) throws IllegalArgumentException {
        WebappInfo webappInfo;
        try {
            webappInfo = WebAppUtil.getWebappInfoFromContextPath(contextPath);
        } catch(IllegalArgumentException e) {
            throw new IllegalArgumentException(e);
        } catch (Exception e) {
            throw new IllegalArgumentException("Could not read or find ofbiz webapp info (WebappInfo) for context path '" + contextPath + "': " + e.getMessage(), e);
        }
        WebXml webXml = getWebXmlAlways(null, webappInfo);
        String webSiteId = null;
        try {
            webSiteId = WebAppUtil.getWebSiteId(webXml);
        } catch (Exception e) {
            throw new IllegalArgumentException("Could not get webSiteId for context path '" + contextPath + "': " + e.getMessage(), e);
        }
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
    
    public String getWebappName() {
        return webappInfo.getName();
    }
    
    public String getContextPath() {
        return webappInfo.getContextRoot();
    }

    /**
     * @deprecated use {@link #getContextPath()} instead
     */
    @Deprecated
    public String getContextRoot() {
        return getContextPath();
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
     * Essentially {@link #getContextPath()} + {@link #getControlServletMapping()} + "/".
     * Includes a trailing slash.
     * <p>
     * NOTE: The trailing slash inconsistency is due to this coming from the stock ofbiz
     * {@link WebAppUtil#getControlServletPath(WebappInfo)} (FIXME?)
     */
    public String getFullControlPath() {
        return fullControlPath;
    }

    /**
     * Gets controller config or null if none for this webapp.
     * <p>
     * NOTE: This is NOT cached, because the ExtWebappInfo caches are static
     * and they would cause a second-layer caching around the ControllerConfig cache.
     */
    public ControllerConfig getControllerConfig() {
        try {
            return ConfigXMLReader.getControllerConfig(getWebappInfo(), true);
        } catch (Exception e) {
            // Do not throw this because caller would probably not expect it;
            // he would expect it to be thrown during construction, but can't
            Debug.logError(e, module);
            return null;
        }
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

    private String getLogMsgPrefix() {
        if (webSiteId != null) {
            return "Website '" + webSiteId + "': ";
        } else {
            return "Webapp '" + getContextPath() + "': ";
        }
    }

    /**
     * Helper wrapper to read WebappInfo reliably.
     */
    public static WebappInfo getWebappInfoAlways(String webSiteId) throws IllegalArgumentException {
        try {
            return WebAppUtil.getWebappInfoFromWebsiteId(webSiteId);
        } catch(IllegalArgumentException e) {
            throw new IllegalArgumentException(e); // exception message already good
        } catch (Exception e) {
            throw new IllegalArgumentException("Could not read or find ofbiz webapp info (WebappInfo) for website ID '" + webSiteId + "': " + e.getMessage(), e);
        }
    }

    /**
     * Helper wrapper to read WebXml reliably.
     */
    public static WebXml getWebXmlAlways(String webSiteId, WebappInfo webappInfo) throws IllegalArgumentException {
        try {
            return WebAppUtil.getWebXml(webappInfo);
        } catch(Exception e) {
            throw new IllegalArgumentException("Could not read or find webapp container info (web.xml) for website ID '" + webSiteId
                        + "' (mount-point '" + webappInfo.getContextRoot() + "'): " + e.getMessage(), e);
        }
    }
}

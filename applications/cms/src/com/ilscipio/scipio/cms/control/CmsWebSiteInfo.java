package com.ilscipio.scipio.cms.control;

import java.io.Serializable;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

import javax.servlet.ServletContext;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.ExtWebappInfo;
import org.ofbiz.webapp.WebAppUtil;
import org.ofbiz.webapp.control.ConfigXMLReader;
import org.ofbiz.webapp.control.ConfigXMLReader.ControllerConfig;
import org.ofbiz.webapp.control.ConfigXMLReader.RequestMap;
import org.ofbiz.webapp.control.ConfigXMLReader.RequestResponse;
import org.ofbiz.webapp.control.ConfigXMLReader.ViewMap;
import org.ofbiz.webapp.control.WebAppConfigurationException;

import com.ilscipio.scipio.cms.webapp.InvalidWebappException;

/**
 * Cms WebSite information.
 * <p>
 * DEV NOTE: 2016: methods still useful to build request tree?
 */
@SuppressWarnings("serial")
public class CmsWebSiteInfo implements Serializable {

    /*
     * Note: For future code: Probably shouldn't keep references to any ServletContexts anywhere in this class
     * because might violate container security. RequestHandler contains such a reference, so don't keep a reference
     * to that either... only ControllerConfig is fine.
     */

    // Use Ofbiz logging carefully only...
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static volatile Map<String, CmsWebSiteInfo> cmsRegisteredWebSites = Collections.emptyMap();

    private final String webSiteId;
    private final boolean hasController;
    private final URL controllerConfigUrl;
    private final CmsWebSiteConfig webSiteConfig;
    private final boolean cmsRegistered = true; // Always true at the moment
    
    // delayed lookup fields
    private transient ExtWebappInfo extWebappInfo = null;
    private transient Boolean controlRootAlias = null;

    CmsWebSiteInfo(String webSiteId, boolean hasController, URL controllerConfigUrl, CmsWebSiteConfig webSiteConfig) {
        this.webSiteId = webSiteId;
        this.hasController = hasController;
        this.controllerConfigUrl = controllerConfigUrl;
        this.webSiteConfig = webSiteConfig;
    }

    /**
     * @param webSiteId
     * @param context
     * @param hasControllerHint  true if caller knows a controller has to exist
     * @return
     * @throws InvalidWebappException
     */
    static CmsWebSiteInfo makeForWebSiteContext(String webSiteId, ServletContext servletContext, boolean hasControllerHint) throws InvalidWebappException {
        URL controllerConfigURL = ConfigXMLReader.getControllerConfigURL(servletContext);
        
        // tthis code may be called even before ControlServlet is initialized.
        // we have to read the whole WebXml manual.
        // NOTE: DO NOT cache the extWebappInfo here; ditch it
        ExtWebappInfo extWebappInfo = null;
        try {
            extWebappInfo = ExtWebappInfo.fromWebSiteIdNew(webSiteId);
        } catch(Exception e) {
            Debug.logError(e, "Cms: Website '" + webSiteId 
                    + "': fatal error determining core webapp properties: " + e.getMessage(), module);
        }
        
        boolean hasController = false;
        if (hasControllerHint) {
            hasController = true;
        } else {
            if (controllerConfigURL != null && extWebappInfo != null && extWebappInfo.getFullControlPath() != null) {
                hasController = true;
            }
        }
        CmsWebSiteConfig webSiteConfig = CmsWebSiteConfig.fromServletContext(extWebappInfo, servletContext);
        return new CmsWebSiteInfo(webSiteId, hasController, controllerConfigURL, webSiteConfig);
    }
    
    static CmsWebSiteInfo makeForWebSiteContext(String webSiteId, ServletContext context) throws InvalidWebappException {
        return makeForWebSiteContext(webSiteId, context, false);
    }

    /**
     * Get all CMS-compatible websites info.
     * <p>
     * In principle, this would return info for all websites with sufficient config. 
     * But in practice, we currently delegate
     * to getAllCmsRegWebSitesInfo because we can only find info for Cms-registered web sites;
     * still fine for now (2017).
     */
    public static Map<String, CmsWebSiteInfo> getAllWebSitesInfo() {
        return cmsRegisteredWebSites;
    }

    /**
     * Gets CMS website info by webSiteId.
     * Returns null if not found or not CMS-valid.
     */
    public static CmsWebSiteInfo getWebSiteInfo(String webSiteId) {
        return cmsRegisteredWebSites.get(webSiteId);
    }
    
    /**
     * Convenience method to get website config from webSiteId.
     * Returns null if not found or not CMS-valid.
     */
    public static CmsWebSiteConfig getWebSiteConfig(String webSiteId) {
        CmsWebSiteInfo webSiteInfo = cmsRegisteredWebSites.get(webSiteId);
        return (webSiteInfo != null) ? webSiteInfo.getWebSiteConfig() : null;
    }
    
    /**
     * Convenience method to get website config from webSiteId.
     * Returns the default config ({@link CmsWebSiteConfig#getDefault}) if not found 
     * or not CMS-valid.
     * <p>
     * NOTE: This is only useful if you don't already have a CmsWebSiteInfo instead,
     * because the instance method {@link #getWebSiteConfig()} always returns an instance.
     */
    public static CmsWebSiteConfig getWebSiteConfigOrDefault(String webSiteId) {
        CmsWebSiteInfo webSiteInfo = cmsRegisteredWebSites.get(webSiteId);
        return (webSiteInfo != null) ? webSiteInfo.getWebSiteConfig() : CmsWebSiteConfig.getDefault();
    }
    
    /**
     * Gets the web site info for all websites registered to function with Cms (through filters, view handler, etc.).
     */
    public static Map<String, CmsWebSiteInfo> getAllCmsRegWebSitesInfo() {
        return cmsRegisteredWebSites;
    }
    
    public static Collection<CmsWebSiteInfo> getAllCmsRegWebSitesInfoList() {
        return cmsRegisteredWebSites.values();
    }

    public static CmsWebSiteInfo getCmsRegWebSiteInfo(String webSiteId) {
        return cmsRegisteredWebSites.get(webSiteId);
    }
    
    public static Set<String> getAllCmsRegWebSiteIds() {
        return cmsRegisteredWebSites.keySet();
    }

    /**
     * Registers the website using the webSiteId in the servlet context.
     * Returns the new or existing registration website info.
     * <p>
     * Only call this from Ofbiz webapps!
     * Currently, we need websites to call this to be recognized by Cms via filter/handlers at startup.
     */
    static CmsWebSiteInfo registerCmsWebSite(ServletContext servletContext, boolean hasControllerHint) {
        try {
            String webSiteId = getWebSiteId(servletContext);
            if (webSiteId == null || webSiteId.length() <= 0) {
                throw new InvalidWebappException("WebSite has no webSiteId");
            }

            // Only register once; accept multiple to support best-effort
            // attempts.
            CmsWebSiteInfo webSiteInfo = cmsRegisteredWebSites.get(webSiteId);
            boolean newReg = false;
            if (webSiteInfo == null) {
                synchronized(CmsWebSiteInfo.class) {
                    Map<String, CmsWebSiteInfo> prevRegs = cmsRegisteredWebSites;
                    webSiteInfo = prevRegs.get(webSiteId);
                    if (webSiteInfo == null) {
                        // we use map copy + unmodifiable map to avoid need for synchronization on read
                        Map<String, CmsWebSiteInfo> newRegs = new HashMap<>(prevRegs);

                        webSiteInfo = CmsWebSiteInfo.makeForWebSiteContext(webSiteId, servletContext, hasControllerHint);
                        if (webSiteInfo.hasController()) {
                            // Make a call to getControllerConfig once just to trigger controller load if never happened yet.
                            webSiteInfo.getControllerConfig();
                        }
                        newRegs.put(webSiteInfo.getWebSiteId(), webSiteInfo);
                        
                        cmsRegisteredWebSites = Collections.unmodifiableMap(newRegs);
                        newReg = true;
                    }
                }
                
                if (newReg) { // don't sync the logging
                    Debug.logInfo("Cms: Registered website '" + webSiteInfo.getWebSiteId() + "' with configuration: " 
                            + webSiteInfo.getWebSiteConfig().toStringDescSingleLine(false), module);
                    if (!webSiteInfo.hasController()) {
                        Debug.logWarning("Cms: Website '" + webSiteId 
                                + "' has no controller config detected - CMS may not work as expected", module);
                    }
                }
            }
            return webSiteInfo;
        } catch (InvalidWebappException e) {
            Debug.logError("Cms: Tried to register a web site invalid for Cms use (servlet context path: " 
                    + servletContext.getContextPath() + "): " + e.getMessage() + "; ignoring", module);
            return null;
        } catch (Exception e) {
            Debug.logError("Cms: Error registering website for CMS (servlet context path: " 
                    + servletContext.getContextPath() + "): " + e.getMessage() + "; skipping", module);
            return null;
        }
    }

    static String getWebSiteId(ServletContext servletContext) {
        return servletContext.getInitParameter("webSiteId");
    }
    
    public static CmsWebSiteConfig getWebSiteConfigOrDefaults(CmsWebSiteInfo webSiteInfo, String webSiteId) {
        if (webSiteInfo != null) return webSiteInfo.getWebSiteConfig();
        else return CmsWebSiteConfig.fromDefaults(webSiteId);
    }
    
    public static CmsWebSiteConfig getWebSiteConfigOrDefaults(CmsWebSiteInfo webSiteInfo, ServletContext servletContext) {
        if (webSiteInfo != null) return webSiteInfo.getWebSiteConfig();
        else return CmsWebSiteConfig.fromDefaults(getWebSiteId(servletContext));
    }


    public String getWebSiteId() {
        return webSiteId;
    }

    public boolean isCmsRegistered() {
        return cmsRegistered;
    }
    
    public boolean hasController() {
        return hasController;
    }
    
    /**
     * Get Ofbiz controller config.
     */
    public ControllerConfig getControllerConfig() {
        
        /*
         * Warning: Ofbiz API here; not safe from CMS env! Just wrap everything.
         * <p>
         * Also: check for null result just in case.
         * <p>
         * Note: Due to Ofbiz design this can technically trigger the config to get built.
         * <p>
         * Note: Not caching ControllerConfig in instance as RequestHandler has a comment about not doing that.
         * Already cached in ConfigXMLReader anyway.
         */
        
        if (controllerConfigUrl != null) {
            try {
                return ConfigXMLReader.getControllerConfig(controllerConfigUrl);
            } catch (WebAppConfigurationException e) {
                // TODO: REVIEW: Should this throw exception instead?
                Debug.logError(e, module);
                return null;
            }
        } else {
            return null;
        }
    }
    
    public Map<String, String> getContextParams() {
        ExtWebappInfo extInfo = this.extWebappInfo; // not using getExtInfo(); don't trigger init from this, may be too early...
        if (extInfo != null) { 
            return extInfo.getContextParams();
        } else {
            return WebAppUtil.getWebappContextParamsSafe(getWebSiteId());
        }
    }
    
    public String getControlServletMapping() {
        return getExtWebappInfo().getControlServletMapping();
    }
    
    /**
     * Gets the new extended webapp info.
     * NOTE: this is done on first access.
     * DEV NOTE: IMPORTANT: THIS CANNOT BE DONE IN CONSTRUCTOR.
     */
    public ExtWebappInfo getExtWebappInfo() {
        ExtWebappInfo extWebappInfo = this.extWebappInfo;
        if (extWebappInfo == null) {
            try {
                extWebappInfo = ExtWebappInfo.fromWebSiteId(webSiteId);
            } catch(Exception e) {
                Debug.logError(e, "Cms: Error: Cannot read website '" + webSiteId + "' configurations: " + e.getMessage(), module);
            }
            this.extWebappInfo = extWebappInfo;
        }
        return extWebappInfo;
    }
    
    public CmsWebSiteConfig getWebSiteConfig() {
        return webSiteConfig;
    }

    public boolean isControlRootAlias() {
        Boolean controlRootAlias = this.controlRootAlias;
        if (controlRootAlias == null) {
            ExtWebappInfo extWebappInfo = getExtWebappInfo();
            if (extWebappInfo != null) {
                controlRootAlias = readWebSiteControlRootAliasLogical();
            }
            if (controlRootAlias == null) controlRootAlias = false;
            this.controlRootAlias = controlRootAlias;
        }
        return controlRootAlias;
    }
    
    
    /**
     * If web.xml cmsControlRootAlias is set to true or false, take it at face
     * value and return it.
     * Otherwise, try to infer if can return true from ContextFilter cmsControlRootAlias.
     * However, if can't guarantee aliasing is not happening, return null instead of false. 
     */
    Boolean readWebSiteControlRootAliasLogical() {
        Boolean alias = null;
        ExtWebappInfo extWebappInfo = getExtWebappInfo();
        try {
            Map<String, String> contextParams = extWebappInfo.getContextParams();
            if (contextParams != null) {
                // if this boolean set, we take it at face value, true or false
                alias = getWebSiteConfig().isControlRootAlias();
            }
            if (alias != null) {
                Debug.logInfo("Cms: Website '" + extWebappInfo.getWebSiteId() 
                        + "': Found web.xml context-param cmsControlRootAlias valid value: " + alias, module);
            } else {
                if (contextParams.containsKey("cmsControlRootAlias")) {
                    if (UtilValidate.isNotEmpty((String) contextParams.get("cmsControlRootAlias"))) {
                        Debug.logError("Cms: Website '" + extWebappInfo.getWebSiteId()  
                                + "': web.xml context-param cmsControlRootAlias has invalid boolean value: " + contextParams.get("cmsControlRootAlias"), module);
                    } else {
                        Debug.logInfo("Cms: Website '" + extWebappInfo.getWebSiteId()  
                                + "': Found web.xml context-param cmsControlRootAlias, but was empty (valid)", module);
                    }
                }
                Debug.logInfo("Cms: Website '" + extWebappInfo.getWebSiteId()  
                    + "': Trying to infer cmsControlRootAlias setting from forwardRootControllerUris ContextFilter init-param", module);
                alias = extWebappInfo.getForwardRootControllerUrisValidated();
            }
        } catch (Exception e) {
            Debug.logError(e, "Cms: Could not read web.xml for webSiteId '" + extWebappInfo.getWebSiteId() + "' to determine cmsControlRootAlias", module);
        }
        return alias;
    }

    
    /**
     * Helper method. 
     * 
     * @see #getControllerConfig
     */
    public Map<String, RequestMap> getRequestMapMap() {
        ControllerConfig cc = getControllerConfig();
        if (cc != null) {
            try {
                return cc.getRequestMapMap();
            } catch (WebAppConfigurationException e) {
                // TODO: REVIEW: Should this throw exception instead?
                Debug.logError(e, module);
                return null;
            }
        } else {
            return null;
        }
    }
    
    /**
     * Helper method.
     * 
     * @see #getControllerConfig
     */
    public Map<String, ViewMap> getViewMapMap() {
        ControllerConfig cc = getControllerConfig();
        if (cc != null) {
            try {
                return cc.getViewMapMap();
            } catch (WebAppConfigurationException e) {
                // TODO: REVIEW: Should this throw exception instead?
                Debug.logError(e, module);
                return null;
            }
        } else {
            return null;
        }
    }
    
    /**
     * Builds a basic request tree with lists and maps only.
     */
    @Deprecated
    public Map<String, Object> buildRequestSimpleTree(Integer maxDepth) {
        
        /*
         * I made this method for testing purposes and to output in Freemarker.
         * Probably you should use the Ofbiz classes above.
         */

        Map<String, Object> simpleTree = new HashMap<>();
        Map<String, RequestMap> reqMapMap = getRequestMapMap();
        if (maxDepth == null || maxDepth != 0) {
            for (RequestMap req : reqMapMap.values()) {
                Set<String> branchRequestUris = new HashSet<String>();
                Map<String, Object> reqMapSimpleTree = buildRequestMapSimpleTree(req, reqMapMap, branchRequestUris, maxDepth, 1);
                reqMapSimpleTree.put("branchRequestUris", branchRequestUris);
                simpleTree.put(req.uri, reqMapSimpleTree);
            }
        }
        return simpleTree;
    }
    
    @Deprecated
    protected Map<String, Object> buildRequestMapSimpleTree(RequestMap req, Map<String, RequestMap> reqMapMap, Set<String> branchReqUris, Integer maxDepth, int currDepth) {
        Map<String, Object> simpleReq = new HashMap<>();
        
        simpleReq.put("uri", req.uri);
        
        if (branchReqUris.contains(req.uri)) {
            simpleReq.put("alreadyInBranch", true);
            simpleReq.put("expanded", false);
        } else {
            simpleReq.put("alreadyInBranch", false);
            branchReqUris.add(req.uri);
            
            if (maxDepth == null || maxDepth < 0 || (currDepth <= maxDepth)) {
                
                // Only record view names once (turned to list at the end)
                Set<String> viewNames = new LinkedHashSet<String>();
                
                Map<String, Object> subRequestsMap = new HashMap<>();
                for(RequestResponse resp : req.requestResponseMap.values()) {
                    if ("view".equals(resp.type)) {
                        viewNames.add(resp.value);
                    } else if ("request".equals(resp.type)) {
                        // Important: It's possible for the same request to be referenced in more than one response.
                        // Only record the first one. This also keeps the current alreadyInBranch logic from messing up.
                        if (!subRequestsMap.containsKey(resp.value)) {
                            RequestMap subReq = reqMapMap.get(resp.value);
                            if (subReq != null) {
        
                                subRequestsMap.put(subReq.uri, buildRequestMapSimpleTree(subReq, reqMapMap, branchReqUris, maxDepth, currDepth + 1));
                               
                            }
                        }
                    }
                }
                
                
                simpleReq.put("expanded", true);
                simpleReq.put("viewNames", new ArrayList<String>(viewNames));
                simpleReq.put("subRequestsMap", subRequestsMap);
            } else {
                simpleReq.put("expanded", false);
            }
        }
        
        return simpleReq;
    }
    
}
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
    public static final String module = CmsWebSiteInfo.class.getName();

    private static volatile Map<String, CmsWebSiteInfo> cmsRegisteredWebSites = Collections.emptyMap();

    private final String webSiteId;
    private final boolean hasController;
    private final URL controllerConfigUrl;
    
    private final boolean cmsRegistered = true; // Always true at the moment
    
    private transient ExtWebSiteInfo extWebSiteInfo = null;
    private transient Boolean controlRootAlias = null;
    
    private CmsWebSiteInfo(String webSiteId, boolean hasController, URL controllerConfigUrl) {
        this.webSiteId = webSiteId;
        this.hasController = hasController;
        this.controllerConfigUrl = controllerConfigUrl;
    }

    /**
     * @param webSiteId
     * @param context
     * @param hasControllerHint  true if caller knows a controller has to exist
     * @return
     * @throws InvalidWebappException
     */
    static CmsWebSiteInfo makeForWebSiteContext(String webSiteId, ServletContext context, boolean hasControllerHint) throws InvalidWebappException {
        URL controllerConfigURL = ConfigXMLReader.getControllerConfigURL(context);
        
        // TODO: has controller logic...
        boolean hasController;
        if (hasControllerHint) {
            hasController = true;
        }
        else {
            // FIXME: This doesn't work.
            // Right now there isn't really a way to detect whether controller or its servlets exist for a webapp; 
            // made worse because this code might be called even before ControlServlet is initialized.
            hasController = (controllerConfigURL != null);
        }
        
        return new CmsWebSiteInfo(webSiteId, hasController, controllerConfigURL);
    }
    
    static CmsWebSiteInfo makeForWebSiteContext(String webSiteId, ServletContext context) throws InvalidWebappException {
        return makeForWebSiteContext(webSiteId, context, false);
    }

    /*
     * In principle, this would return info for all websites. In practice, we currently delegate
     * to getAllCmsRegWebSitesInfo because we can only find info for Cms-registered web sites;
     * fine for our purposes.
     */
    public static Map<String, CmsWebSiteInfo> getAllWebSitesInfo() {
        return getAllCmsRegWebSitesInfo();
    }

    public static CmsWebSiteInfo getWebSiteInfo(String webSiteId) {
        return getCmsRegWebSiteInfo(webSiteId);
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
        return Collections.unmodifiableSet(cmsRegisteredWebSites.keySet());
    }

    /**
     * Only call this from Ofbiz webapps!
     * Currently, we need websites to call this to be recognized by Cms via filter/handlers at startup.
     */
    static void registerCmsWebSite(ServletContext context, boolean hasControllerHint) {
        try {
            String webSiteId = getWebSiteId(context);
            if (webSiteId == null || webSiteId.length() <= 0) {
                throw new InvalidWebappException("WebSite has no webSiteId");
            }

            // Only register once; accept multiple to support best-effort
            // attempts.
            if (!cmsRegisteredWebSites.containsKey(webSiteId)) {
                Map<String, CmsWebSiteInfo> newRegs = null;
                CmsWebSiteInfo regInfo = null;
                synchronized (CmsWebSiteInfo.class) {
                    if (!cmsRegisteredWebSites.containsKey(webSiteId)) {
                        // Replace whole map just to avoid sync issues (optimize
                        // for reads without sync blocks - map only changes at
                        // startup)
                        newRegs = new HashMap<String, CmsWebSiteInfo>(cmsRegisteredWebSites);

                        regInfo = CmsWebSiteInfo.makeForWebSiteContext(webSiteId, context, hasControllerHint);
                        
                        if (regInfo.hasController()) {
                            // Make a call to getControllerConfig once just to trigger controller load if never happened yet.
                            regInfo.getControllerConfig();
                        }
                        
                        newRegs.put(regInfo.getWebSiteId(), regInfo);
                    }
                }
                if (newRegs != null) {
                    if (regInfo.hasController()) {
                        Debug.logInfo("Cms: Registered web site with webSiteId '" + webSiteId + "'; has controller config", module);
                    } else {
                        Debug.logWarning("Cms: Registered web site with webSiteId '" + webSiteId + "', but has no detected controller config!", module);
                    }
                    cmsRegisteredWebSites = Collections.unmodifiableMap(newRegs);
                }
            }
        } catch (InvalidWebappException e) {
            // We can log the Ofbiz way here because this should only be called from Ofbiz context
            Debug.logError("Cms: Tried to register a web site invalid for Cms use (servlet context path: " 
                    + context.getContextPath() + "): " + e.getMessage() + "; ignoring", module);
        }
    }

    static String getWebSiteId(ServletContext context) {
        return context.getInitParameter("webSiteId");
    }

    
    
    
    public String getWebSiteId() {
        return webSiteId;
    }

    public boolean isCmsRegistered() {
        return cmsRegistered;
    }
    
    /*
     * FIXME: This currently doesn't indicate reality - @see makeForWebSiteContext.
     * However for Cms probably not a problem right now.
     */
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
        ExtWebSiteInfo extInfo = this.extWebSiteInfo; // not using getExtInfo(); don't trigger init from this, may be too early...
        if (extInfo != null) { 
            return extInfo.getContextParams();
        } else {
            return WebAppUtil.getWebappContextParamsSafe(getWebSiteId());
        }
    }
    

    public ExtWebSiteInfo getExtWebSiteInfo() {
        ExtWebSiteInfo extWebSiteInfo = this.extWebSiteInfo;
        if (extWebSiteInfo == null) {
            try {
                extWebSiteInfo = ExtWebSiteInfo.fromWebSiteId(webSiteId);
            } catch(Exception e) {
                Debug.logError(e, "Cms: Error: Cannot read website '" + webSiteId + "' configurations: " + e.getMessage(), module);
            }
            this.extWebSiteInfo = extWebSiteInfo;
        }
        return extWebSiteInfo;
    }


    public boolean isControlRootAlias() {
        Boolean controlRootAlias = this.controlRootAlias;
        if (controlRootAlias == null) {
            ExtWebSiteInfo extWebSiteInfo = getExtWebSiteInfo();
            if (extWebSiteInfo != null) {
                controlRootAlias = CmsControlUtil.readWebSiteControlRootAliasLogical(extWebSiteInfo);
            }
            if (controlRootAlias == null) controlRootAlias = false;
            this.controlRootAlias = controlRootAlias;
        }
        return controlRootAlias;
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
    public Map<String, Object> buildRequestSimpleTree(Integer maxDepth) {
        
        /*
         * I made this method for testing purposes and to output in Freemarker.
         * Probably you should use the Ofbiz classes above.
         */

        Map<String, Object> simpleTree = new HashMap<String, Object>();
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
    
    protected Map<String, Object> buildRequestMapSimpleTree(RequestMap req, Map<String, RequestMap> reqMapMap, Set<String> branchReqUris, Integer maxDepth, int currDepth) {
        Map<String, Object> simpleReq = new HashMap<String, Object>();
        
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
                
                Map<String, Object> subRequestsMap = new HashMap<String, Object>();
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
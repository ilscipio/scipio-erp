package com.ilscipio.scipio.cms.webapp;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.component.ComponentConfig.WebappInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.WebAppUtil;
import org.ofbiz.webapp.control.ConfigXMLReader;
import org.ofbiz.webapp.control.ConfigXMLReader.RequestMap;
import org.ofbiz.webapp.control.ConfigXMLReader.ViewMap;

/**
 * Component utilities
 * <p>
 * 2017-02-08: moved back to CMS package because too specific code inside. 
 * TODO?: re-refactor out generic code later.
 */
public class ComponentUtil {
    
    public static final String module = UtilHttp.class.getName();

    /**
     * Return a list of Maps containing all component/webapp informations
     */
    public static List<Map<String, Object>> getComponentInfos() {
        return getComponentInfos(null);
    }

    /**
     * Return a list of Maps containing all component/webapp informations
     *
     * @param webSiteIds only include trees for these webSiteIds
     */
    public static List<Map<String, Object>> getComponentInfos(Set<String> webSiteIds) {
        Collection<ComponentConfig> components = ComponentConfig.getAllComponents();
        List<Map<String, Object>> componentList = new ArrayList<>(components.size());

        for (ComponentConfig component : components) {
            List<WebappInfo> webApps = component.getWebappInfos();
            for (WebappInfo webApp : webApps) {
                Map<String, Object> newMap = getWebAppInfo(component, webApp);
                if (newMap != null && (webSiteIds == null || webSiteIds.contains(newMap.get("webSiteId"))))
                    componentList.add(newMap);
            }
        }
        return componentList;
    }

    /**
     * Returns a map containing component information
     */
    public static Map<String, Object> getWebAppInfo(ComponentConfig component, WebappInfo webApp) {
        Map<String, Object> componentMap = UtilMisc.newMap();
        String webSiteId = null;
        try {
            webSiteId = WebAppUtil.getWebSiteId(webApp);
        } catch (Exception e) {
            ;
        }
        componentMap.put("compName", component.getComponentName());
        componentMap.put("rootLocation", component.getRootLocation());
        componentMap.put("enabled", component.enabled() == true ? "Y" : "N");
        componentMap.put("webAppInfo", webApp);
        componentMap.put("webAppName", webApp.getName());
        componentMap.put("contextRoot", webApp.getContextRoot()); // equals
                                                                  // mount point
        componentMap.put("webSiteId", webSiteId != null ? webSiteId : "noWebSiteId");
        componentMap.put("location", webApp.getLocation());
        return componentMap;
    }

    /**
     * Returns all available requests by component as a Map
     */
    public static Map<String, Map<String, Object>> getWebappRequestMaps() {
        return getWebappRequestMaps(null, null);
    }

    /**
     * Returns all available requests by component as a Map
     *
     * @param webSiteIds only include trees for these webSiteIds
     * @param webSiteExtraConfigs each website can get extra config
     */
    public static Map<String, Map<String, Object>> getWebappRequestMaps(Set<String> webSiteIds,
            Map<String, Map<String, Object>> webSiteExtraConfigs) {
        Map<String, Map<String, Object>> requestMaps = UtilMisc.newMap();
        //Map<String, ConfigXMLReader.RequestMap> requestMapMap = null;
        List<Map<String, Object>> components = ComponentUtil.getComponentInfos(webSiteIds);

        if (webSiteExtraConfigs == null) {
            webSiteExtraConfigs = Collections.emptyMap();
        }

        for (Map<String, Object> component : components) {
            try {
                // String contextRoot = (String) component.get("contextRoot");
                ConfigXMLReader.ControllerConfig conf = ConfigXMLReader
                        .getControllerConfig((WebappInfo) component.get("webAppInfo"));
                Map<String, RequestMap> requestMap = conf.getRequestMapMap();

                Map<String, Object> webSiteExtraConfig = null;
                String webSiteId = (String) component.get("webSiteId");
                if (webSiteId != null && !webSiteId.isEmpty()) {
                    webSiteExtraConfig = webSiteExtraConfigs.get(webSiteId);
                }
                if (webSiteExtraConfig == null) {
                    webSiteExtraConfig = Collections.emptyMap();
                }

                String requestServletPath = (String) webSiteExtraConfig.get("editorRequestPathPrefix");
                if (requestServletPath == null || requestServletPath.isEmpty()) {
                    requestServletPath = "/";
                } else {
                    if (!requestServletPath.endsWith("/")) {
                        requestServletPath += "/";
                    }
                }

                for (String key : requestMap.keySet()) {
                    String requestUri = requestServletPath + key;

                    if (requestMaps.containsKey(webSiteId)) {
                        Map<String, List<Map<String, Object>>> currMap = UtilGenerics.checkMap(requestMaps.get(webSiteId));
                        if (currMap.containsKey("pages")) {
                            List<Map<String, Object>> pages = currMap.get("pages");
                            generateMapFromRequestUri(requestUri, "request", pages, webSiteId, webSiteId);
                        } else {
                            List<Map<String, Object>> pages = new ArrayList<>();
                            generateMapFromRequestUri(requestUri, "request", pages, webSiteId, webSiteId);
                            currMap.put("pages", pages);
                        }

                    } else {
                        Map<String, Object> currMap = component;
                        List<Map<String, Object>> pages = new ArrayList<>();
                        generateMapFromRequestUri(requestUri, "request", pages, webSiteId, webSiteId);
                        currMap.put("pages", pages);
                        requestMaps.put(webSiteId, currMap);
                    }
                }
            } catch (Exception e) {
                ;
            }
        }
        return requestMaps;
    }

    /**
     * Returns all available view-maps by component as a Map
     */
    public static Map<String, Map<String, Object>> getWebappViewMaps() {
        return getWebappViewMaps(null);
    }

    /**
     * Returns all available view-maps by component as a Map
     *
     * @param webSiteIds only include trees for these webSiteIds
     */
    public static Map<String, Map<String, Object>> getWebappViewMaps(Set<String> webSiteIds) {
        Map<String, Map<String, Object>> viewMaps = UtilMisc.newMap();
        List<Map<String, Object>> components = ComponentUtil.getComponentInfos(webSiteIds);
        for (Map<String, Object> component : components) {
            try {
                ConfigXMLReader.ControllerConfig conf = ConfigXMLReader
                        .getControllerConfig((WebappInfo) component.get("webAppInfo"));
                Map<String, ViewMap> viewMap = conf.getViewMapMap();
                for (String viewName : viewMap.keySet()) {
                    String webSiteId = (String) component.get("webSiteId");
                    if (viewMaps.containsKey(webSiteId)) {
                        Map<String, List<Map<String, Object>>> currMap = UtilGenerics.checkMap(viewMaps.get(webSiteId));
                        if (currMap.containsKey("views")) {
                            List<Map<String, Object>> views = currMap.get("views");
                            generateMapFromViewName(viewName, "view", views, webSiteId);
                        } else {
                            List<Map<String, Object>> views = new ArrayList<>();
                            generateMapFromViewName(viewName, "view", views, webSiteId);
                            currMap.put("views", views);
                        }
                    } else {
                        Map<String, Object> currMap = component;
                        List<Map<String, Object>> views = new ArrayList<>();
                        generateMapFromViewName(viewName, "view", views, webSiteId);
                        currMap.put("views", views);
                        viewMaps.put(webSiteId, currMap);
                    }
                }
            } catch (Exception e) {
                ;
            }
        }
        return viewMaps;
    }

    private static void generateMapFromRequestUri(String path, String type, List<Map<String, Object>> pages, String parent,
            String webSiteId) {
        generateMapFromRequestUri(path, type, pages, null, null, parent, webSiteId);
    }

    /**
     * Iterates over path recursively and adds a map to each of the subpaths
     * containing the most basic information. State and Icon are inherited from
     * parent by default. If the path has no subpaths, it will return a simple
     * map containing the base information
     * <p>
     * path should start with slash (/).
     */
    private static void generateMapFromRequestUri(String path, String type, List<Map<String, Object>> pages, Map<String, Object> state, String icon,
            String parent, String webSiteId) {
        if (path == null) { // sanity checks
            throw new IllegalArgumentException("generateMapFromRequestUri: received null path");
        } else if (UtilValidate.isEmpty(webSiteId)) {
            throw new IllegalArgumentException("generateMapFromRequestUri: missing webSiteId");
        }
        if (!path.startsWith("/")) {
            // we'll adjust it, but other code elsewhere is likely to fail, so warn.
            Debug.logWarning("Cms: generateMapFromRequestUri: passed path did not start with a slash (/): " 
                    + path + "; this may get mishandled elsewhere", module);
            path = "/" + path;
        }
        
        String[] items = path.split("/");
        StringBuilder subPath = new StringBuilder();
        // Iterate over all Subpaths
        if (parent == null)
            parent = "#";
        Map<String, Object> itemMap = null;
        for (String item : items) {
            if (item.length() > 0 && items.length > 1) {
                /*
                 * if(itemMap!=null){ itemMap.put("icon", "jstree-folder"); }
                 */
                String parentPath = subPath.toString();
                subPath.append("/" + item);
                String subPathStr = subPath.toString();
                
                // NOTE: 2017-02-14: id must contain webSiteId otherwise may not be unique globally.
                String idPath = subPath.toString().replaceAll("/", "_");
                String id = webSiteId + idPath;

                boolean isTargetPath = subPathStr.equals(path);

                // determine if node is parent and add appropriate folder
                itemMap = UtilMisc.newMap(UtilMisc.toMap("text", item, "a_attr", subPathStr, "id", id, "state", state,
                        "parent", parent, "data", UtilMisc.toMap("type", type, "parentPath", parentPath, "path",
                                subPathStr, "websiteid", webSiteId, "isTargetPath", isTargetPath)));
                parent = id;

                // check for map in array and add if none has been added before
                if (!listContainsMapofValue(pages, "id", id)) {
                    pages.add(itemMap);
                }
            }
        }
    }

    private static void generateMapFromViewName(String viewName, String type, List<Map<String, Object>> pages, String webSiteId) {
        generateMapFromViewName(viewName, type, pages, null, null, webSiteId);
    }

    private static void generateMapFromViewName(String viewName, String type, List<Map<String, Object>> pages, Map<String, ?> state, String icon,
            String webSiteId) {
        if (UtilValidate.isEmpty(webSiteId)) {
            throw new IllegalArgumentException("generateMapFromViewName: missing webSiteId");
        }
        
        String id = webSiteId + "_" + viewName;
        String formalId = webSiteId + "::" + viewName;
        Map<String, Object> itemMap = null;
        /*
         * if(itemMap!=null){ itemMap.put("icon", "jstree-folder"); }
         */
        // determine if node is parent and add appropriate folder
        itemMap = UtilMisc.toMap("text", viewName, "id", id, "state", state, "parent", webSiteId, "data",
                UtilMisc.toMap("type", type, "websiteid", webSiteId, "formalId", formalId)); // ,"a_attr",viewName

        // check for map in array and add if none has been added before
        if (!listContainsMapofValue(pages, "id", id)) {
            pages.add(itemMap);
        }
    }

    public static boolean listContainsMapofValue(Collection<Map<String, Object>> c, String key, String value) {
        for (Map<?, ?> o : c) {
            if (o != null && o.get(key).equals(value)) {
                return true;
            }
        }
        return false;
    }

    public static Map<String, Object> getMapOfValue(Collection<Map<String, Object>> c, String key, String value) {
        for (Map<String, Object> o : c) {
            if (o != null && o.get(key).equals(value)) {
                return o;
            }
        }
        return null;
    }

    /**
     * Returns all available requests by component as a RequestMapMap.
     */
    public static Map<String, Map<String, RequestMap>> getWebappRequestMapMaps() {
        Map<String, Map<String, RequestMap>> requestMaps = new HashMap<>();
        //Map<String, ConfigXMLReader.RequestMap> requestMapMap = null;
        List<Map<String, Object>> components = ComponentUtil.getComponentInfos();
        for (Map<String, Object> component : components) {
            try {
                ConfigXMLReader.ControllerConfig conf = ConfigXMLReader
                        .getControllerConfig((WebappInfo) component.get("webAppInfo"));
                Map<String, RequestMap> requestMap = conf.getRequestMapMap();
                requestMaps.put((String) component.get("webSiteId"), requestMap);
            } catch (Exception e) {
                ;
            }
        }
        return requestMaps;
    }

    /**
     * Returns all available requests by component as a List.
     * TODO: REVIEW: error (?) revealed due to missing generics: this returns list of strings but seems
     * to indicate it should have been a list of maps?
     * @deprecated Do not rely on this until reviewed
     */
    @Deprecated
    public static List<String> getWebappRequests() {
        List<String> requestMaps = new ArrayList<>();
        //Map<String, ConfigXMLReader.RequestMap> requestMapMap = null;
        List<Map<String, Object>> components = ComponentUtil.getComponentInfos();
        for (Map<String, Object> component : components) {
            try {
                String contextRoot = (String) component.get("contextRoot");
                ConfigXMLReader.ControllerConfig conf = ConfigXMLReader
                        .getControllerConfig((WebappInfo) component.get("webAppInfo"));
                Map<String, RequestMap> requestMap = conf.getRequestMapMap();
                for (String key : requestMap.keySet()) {
                    String requestUri = contextRoot + "/" + key;
                    requestMaps.add(requestUri);
                }
            } catch (Exception e) {
                ;
            }
        }
        return requestMaps;
    }
}

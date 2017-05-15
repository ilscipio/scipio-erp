package org.ofbiz.webapp.control;

import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.webapp.control.ConfigXMLReader.ViewAsJsonConfig;

/**
 * SCIPIO: helper for viewAsJson mode.
 * viewAsJson mode is enabled by passing request parameter (or attribute, as Boolean) 
 * <code>scpViewAsJson=true</code>.
 */
public abstract class ViewAsJsonUtil {

    /**
     * Name of the request parameter/attribute checked for true/false for viewAsJson on/off
     * (default: false).
     */
    public static final String VIEWASJSON_REQPARAM = "scpViewAsJson";
    /**
     * Name of the request parameter/attribute checked for whether viewAsJson mode should
     * still update browsing session or not (default: false).
     */
    public static final String VIEWASJSONUSESESSION_REQPARAM = "scpViewAsJsonUpSess";
    /**
     * Name of the request parameter/attribute checked for whether viewAsJson mode should
     * use the regular login or the ajax login (default: true).
     * Also see {@link #VIEWASJSONREGLOGIN_REQPARAM}.
     */
    public static final String VIEWASJSONREGLOGIN_REQPARAM = "scpViewAsJsonRegLogin";

    /**
     * Name of request attribute for a map containing explicit values to output via json.
     */
    public static final String RENDEROUTPARAMS_ATTR = "scpOutParams";
    /**
     * Name of request attribute for a set of names of request attributes to allow output via json.
     */
    public static final String RENDEROUTATTRNAMES_ATTR = "scpOutAttrNames";

    /**
     * The name of the attribute/parameter used to contain the output HTML for the view in
     * viewAsJson mode. Stored in scpOutParams.
     */
    public static final String RENDEROUT_OUTPARAM = "renderOut";
    
    /**
     * The name of the attribute/parameter with boolean that says if was logged in when
     * request happened. If missing, this should be assumed true. Stored in scpOutParams.
     */
    public static final String LOGGEDIN_OUTPARAM = "isLoggedIn";
    
    private static final Set<String> defaultRenderOutAttrNames = UtilMisc.unmodifiableHashSet(
        "_ERROR_MESSAGE_", "_ERROR_MESSAGE_LIST_", "_EVENT_MESSAGE_", "_EVENT_MESSAGE_LIST_"
    );
    
    private static final Set<String> msgAttrNames = UtilMisc.unmodifiableHashSet(
            "_ERROR_MESSAGE_", "_ERROR_MESSAGE_LIST_", "_EVENT_MESSAGE_", "_EVENT_MESSAGE_LIST_"
        );
    
    //private static final String defaultViewAsJsonRequestUri = "jsonExplicit";
    
    private ViewAsJsonUtil() {
    }

    public static Set<String> getDefaultRenderOutAttrNames(HttpServletRequest request) {
        return defaultRenderOutAttrNames;
    }

    public static Map<String, Object> getRenderOutParams(HttpServletRequest request) {
        Map<String, Object> outParams = UtilGenerics.checkMap(request.getAttribute(RENDEROUTPARAMS_ATTR));
        if (outParams == null) {
            outParams = new HashMap<>();
        }
        request.setAttribute(RENDEROUTPARAMS_ATTR, outParams);
        return outParams;
    }
    
    public static Map<String, Object> getRenderOutParamsOrNull(HttpServletRequest request) {
        return UtilGenerics.checkMap(request.getAttribute(RENDEROUTPARAMS_ATTR));
    }
    
    public static void setRenderOutParams(HttpServletRequest request, Map<String, Object> params) {
        getRenderOutParams(request).putAll(params);
    }
    
    public static void setRenderOutParam(HttpServletRequest request, String name, Object value) {
        getRenderOutParams(request).put(name, value);
    }
    
    public static Set<String> getRenderOutAttrNames(HttpServletRequest request) {
        Set<String> outAttrNames = UtilGenerics.checkSet(request.getAttribute(RENDEROUTATTRNAMES_ATTR));
        if (outAttrNames == null) {
            outAttrNames = new HashSet<>();
        }
        request.setAttribute(RENDEROUTATTRNAMES_ATTR, outAttrNames);
        return outAttrNames;
    }
    
    public static Set<String> getRenderOutAttrNamesOrNull(HttpServletRequest request) {
        return UtilGenerics.checkSet(request.getAttribute(RENDEROUTATTRNAMES_ATTR));
    }
    
    public static void addRenderOutAttrName(HttpServletRequest request, String name) {
        getRenderOutAttrNames(request).add(name);
    }
    
    public static void addRenderOutAttrNames(HttpServletRequest request, String... names) {
        getRenderOutAttrNames(request).addAll(Arrays.asList(names));
    }
    
    public static void addRenderOutAttrNames(HttpServletRequest request, Collection<String> names) {
        getRenderOutAttrNames(request).addAll(names);
    }

    public static void addDefaultRenderOutAttrNames(HttpServletRequest request) {
        getRenderOutAttrNames(request).addAll(getDefaultRenderOutAttrNames(request));
    }
    
    public static Map<String, Object> collectRenderOutAttributes(HttpServletRequest request) {
        Map<String, Object> outAttrMap = ViewAsJsonUtil.getRenderOutParamsOrNull(request);
        Map<String, Object> attrMap = (outAttrMap != null) ? UtilHttp.transformJSONAttributeMap(outAttrMap) : new HashMap<String, Object>();
        Set<String> attrNames = ViewAsJsonUtil.getRenderOutAttrNamesOrNull(request);
        if (attrNames != null && attrNames.size() > 0) {
            // TODO: optimize
            Map<String, Object> allAttrMap = UtilHttp.getJSONAttributeMap(request);
            for (String attr : attrNames) {
                if (allAttrMap.containsKey(attr)) {
                    attrMap.put(attr, allAttrMap.get(attr));
                }
            }
        }
        return attrMap;
    }
    
    /**
     * Checks if viewAsJson is enabled in the system && this webapp.
     * WARN: does NOT check if enabled in the request (use {@link #isViewAsJson(HttpServletRequest)}).
     */
    public static boolean isViewAsJsonEnabled(HttpServletRequest request, ViewAsJsonConfig config) {
        return config.isEnabled();
    }
    
    /**
     * Returns true if viewAsJson is enabled in the system and for the current request.
     */
    public static boolean isViewAsJson(HttpServletRequest request, ViewAsJsonConfig config) {
        if (!isViewAsJsonEnabled(request, config)) return false;
        else return isViewAsJsonRequest(request, config);
    }
    
    /**
     * Returns true if viewAsJson is requested for the current request.
     * WARN: does not check if enabled in system.
     */
    public static boolean isViewAsJsonRequest(HttpServletRequest request, ViewAsJsonConfig config) {
        Boolean viewAsJson = (Boolean) request.getAttribute(VIEWASJSON_REQPARAM);
        if (viewAsJson != null) return viewAsJson;
        else return "true".equals(request.getParameter(VIEWASJSON_REQPARAM));
    }
    
    /**
     * Checks if view updating session is enabled.
     * WARN: does NOT check if viewAsJson is on; caller should have already checked (use {@link #isViewAsJson(HttpServletRequest)}).
     */
    public static boolean isViewAsJsonUpdateSession(HttpServletRequest request, ViewAsJsonConfig config) {
        Boolean viewAsJson = (Boolean) request.getAttribute(VIEWASJSONUSESESSION_REQPARAM);
        if (viewAsJson != null) return viewAsJson;
        else return UtilMisc.booleanValue(request.getParameter(VIEWASJSONUSESESSION_REQPARAM), config.isUpdateSession());
    }
    
    /**
     * Checks if view updating session is enabled.
     * WARN: does NOT check if viewAsJson is on; caller should have already checked (use {@link #isViewAsJson(HttpServletRequest)}).
     */
    public static boolean isViewAsJsonRegularLogin(HttpServletRequest request, ViewAsJsonConfig config) {
        Boolean viewAsJson = (Boolean) request.getAttribute(VIEWASJSONREGLOGIN_REQPARAM);
        if (viewAsJson != null) return viewAsJson;
        else return UtilMisc.booleanValue(request.getParameter(VIEWASJSONREGLOGIN_REQPARAM), config.isRegularLogin());
    }
    
    public static String getViewAsJsonRequestUri(HttpServletRequest request, ViewAsJsonConfig config) throws WebAppConfigurationException {
        return config.getJsonRequestUriAlways(); // FIXME: unhardcode (needs site-conf.xsd entry)
    }
    
    public static Map<String, Object> getMessageAttributes(HttpServletRequest request) {
        Map<String, Object> map = new HashMap<>();
        for(String attr : msgAttrNames) {
            Object value = request.getAttribute(attr);
            if (value != null) {
                map.put(attr, value);
            }
        }
        return map;
    }
    
    public static void setMessageAttributes(HttpServletRequest request, Map<String, Object> map) {
        for(Map.Entry<String, Object> entry : map.entrySet()) {
            if (entry.getValue() != null) {
                request.setAttribute(entry.getKey(), entry.getValue());
            }
        }
    }
}

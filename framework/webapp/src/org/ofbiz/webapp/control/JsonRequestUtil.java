package org.ofbiz.webapp.control;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.webapp.control.ConfigXMLReader.ControllerConfig;

/**
 * SCIPIO: helper for viewAsJson mode
 */
public abstract class JsonRequestUtil {

    public static final String VIEWASJSON_REQPARAM = "scpViewAsJson";
    public static final String VIEWASJSONUSESESSION_REQPARAM = "scpViewAsJsonUpSess";
    
    // FIXME: unhardcode
    private static final String defaultViewAsJsonRequestUri = "jsonExplicit";
    
    private static final Set<String> allowedOutParamNames = UtilMisc.unmodifiableHashSet(
        "_ERROR_MESSAGE_", "_ERROR_MESSAGE_LIST_", "_EVENT_MESSAGE_", "_EVENT_MESSAGE_LIST_"
    );
    
    //private static final String defaultViewAsJsonRequestUri = "jsonExplicit";
    
    private JsonRequestUtil() {
    }

    public static Set<String> getDefaultAllowedParamNames(HttpServletRequest request) {
        return allowedOutParamNames;
    }

    public static Map<String, Object> getRenderOutParams(HttpServletRequest request) {
        Map<String, Object> outParams = UtilGenerics.checkMap(request.getAttribute("scipioOutParams"));
        if (outParams == null) {
            outParams = new HashMap<>();
        }
        request.setAttribute("scipioOutParams", outParams);
        return outParams;
    }
    
    public static Map<String, Object> getRenderOutParamsOrNull(HttpServletRequest request) {
        return UtilGenerics.checkMap(request.getAttribute("scipioOutParams"));
    }
    
    public static Set<String> getRenderOutAttrNames(HttpServletRequest request) {
        Set<String> outAttrNames = UtilGenerics.checkSet(request.getAttribute("scipioOutAttrNames"));
        if (outAttrNames == null) {
            outAttrNames = new HashSet<>();
        }
        request.setAttribute("scipioOutAttrNames", outAttrNames);
        return outAttrNames;
    }
    
    public static Set<String> getRenderOutAttrNamesOrNull(HttpServletRequest request) {
        return UtilGenerics.checkSet(request.getAttribute("scipioOutAttrNames"));
    }

    public static void addAllDefaultAllowedParamNames(HttpServletRequest request) {
        getRenderOutAttrNames(request).addAll(getDefaultAllowedParamNames(request));
    }
    
    public static Map<String, Object> collectRenderOutAttributes(HttpServletRequest request) {
        Map<String, Object> outAttrMap = JsonRequestUtil.getRenderOutParamsOrNull(request);
        Map<String, Object> attrMap = (outAttrMap != null) ? UtilHttp.transformJSONAttributeMap(outAttrMap) : new HashMap<String, Object>();
        Set<String> attrNames = JsonRequestUtil.getRenderOutAttrNamesOrNull(request);
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
    
    public static boolean isViewAsJson(HttpServletRequest request) {
        Boolean viewAsJson = (Boolean) request.getAttribute(VIEWASJSON_REQPARAM);
        if (viewAsJson != null) return viewAsJson;
        else return "true".equals(request.getParameter(VIEWASJSON_REQPARAM));
    }
    
    public static boolean isViewAsJsonUpdateSession(HttpServletRequest request) {
        Boolean viewAsJson = (Boolean) request.getAttribute(VIEWASJSONUSESESSION_REQPARAM);
        if (viewAsJson != null) return viewAsJson;
        else return "true".equals(request.getParameter(VIEWASJSONUSESESSION_REQPARAM));
    }
    
    public static String getDefaultViewAsJsonRequestUri() {
        return defaultViewAsJsonRequestUri; // FIXME: unhardcode
    }
    
    public static String getViewAsJsonRequestUri(HttpServletRequest request, ControllerConfig controllerConfig) {
        return defaultViewAsJsonRequestUri; // FIXME: unhardcode (needs site-conf.xsd entry)
    }
}

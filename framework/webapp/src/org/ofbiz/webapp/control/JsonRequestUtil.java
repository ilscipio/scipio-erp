package org.ofbiz.webapp.control;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.webapp.control.ConfigXMLReader.ControllerConfig;

/**
 * SCIPIO: helper for viewAsJson mode
 */
public abstract class JsonRequestUtil {

    public static final String VIEWASJSON_REQPARAM = "scpViewAsJson";
    
    private static final List<String> allowedOutParamNames = Collections.unmodifiableList(Arrays.asList(new String[] {
        "_ERROR_MESSAGE_", "_ERROR_MESSAGE_LIST_", "_EVENT_MESSAGE_", "_EVENT_MESSAGE_LIST_"
    }));
    
    //private static final String defaultViewAsJsonRequestUri = "jsonExplicit";
    
    private JsonRequestUtil() {
    }

    public static List<String> getDefaultAllowedParamNames(HttpServletRequest request) {
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
    
    public static List<String> getRenderOutAttrNames(HttpServletRequest request) {
        List<String> outAttrNames = UtilGenerics.checkList(request.getAttribute("scipioOutAttrNames"));
        if (outAttrNames == null) {
            outAttrNames = new ArrayList<>();
        }
        request.setAttribute("scipioOutAttrNames", outAttrNames);
        return outAttrNames;
    }
    
    public static List<String> getRenderOutAttrNamesOrNull(HttpServletRequest request) {
        return UtilGenerics.checkList(request.getAttribute("scipioOutAttrNames"));
    }

    public static Map<String, Object> collectRenderOutAttributes(HttpServletRequest request) {
        Map<String, Object> outAttrMap = JsonRequestUtil.getRenderOutParamsOrNull(request);
        Map<String, Object> attrMap = (outAttrMap != null) ? UtilHttp.transformJSONAttributeMap(outAttrMap) : new HashMap<String, Object>();
        List<String> attrNames = JsonRequestUtil.getRenderOutAttrNamesOrNull(request);
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
    
}

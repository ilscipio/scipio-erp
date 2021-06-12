package org.ofbiz.webapp.event;

import com.ilscipio.scipio.ce.util.servlet.FieldFilter;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.webapp.control.RequestHandler;

import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * Helpers for JSON response events (org.ofbiz.common.CommonEvents#jsonResponseFrom...) (SCIPIO).
 */
public abstract class JsonEventUtil {

    /**
     * Name of request attribute for a map containing explicit values to output via json.
     */
    public static final String OUT_PARAMS = "scpOutParams";

    /**
     * Name of request attribute for a set of names of request attributes to allow output via json.
     */
    public static final String OUT_ATTR = "scpOutAttrNames";

    private static final Set<String> DEFAULT_OUT_ATTRNAMES = UtilMisc.unmodifiableHashSet(
        "_ERROR_MESSAGE_", "_ERROR_MESSAGE_LIST_", "_EVENT_MESSAGE_", "_EVENT_MESSAGE_LIST_"
    );

    private JsonEventUtil() {}

    /**
     * Collects out request attributes for a JSON request without any specific filter or only basic
     * required for basic functionality/security; acceptable for backend use.
     * This is the original behavior of CommonEvents.jsonResponseFromRequestAttributes.
     */
    public static Map<String, Object> collectOutAttributes(HttpServletRequest request) {
        Map<String, Object> outParamsMap = getOutParamsOrNull(request);
        Set<String> attrNames = getOutAttrNamesOrUnmodifiableEmptySet(request);
        FieldFilter requestParamFilter = RequestHandler.getWebappRequestParamFilter(request);
        Map<String, Object> attrMap = UtilHttp.getJSONAttributeMap(request, (key, value) -> {
            if (attrNames.contains(key)) {
                return true;
            }
            if (requestParamFilter != null) {
                return requestParamFilter.getOutputFilter().allows(key);
            }
            return null;
        });
        if (UtilValidate.isNotEmpty(outParamsMap)) {
            UtilHttp.transformJSONAttributeMap(attrMap, outParamsMap, null);
        }
        return attrMap;
    }

    /**
     * Collects out request attributes for a JSON request using explicit whitelist.
     * Only the basic error message results are allowed by default; most secure form.
     * Currently used by view-as-json controller mode (see ViewAsJsonUtil).
     */
    public static Map<String, Object> collectOutAttributesExplicit(HttpServletRequest request) {
        Map<String, Object> outAttrMap = getOutParamsOrNull(request);
        Map<String, Object> attrMap = (outAttrMap != null) ? UtilHttp.transformJSONAttributeMap(outAttrMap) : new HashMap<>();
        Set<String> attrNames = getOutAttrNamesOrNull(request);
        if (UtilValidate.isNotEmpty(attrNames)) {
            UtilHttp.getJSONAttributeMap(attrMap, request, (key, value) -> {
                if (attrNames.contains(key)) {
                    return null;
                }
                return false;
            });
        }
        return attrMap;
    }


    /**
     * Collects out request attributes for a JSON request using restricted blacklist; acceptable for frontend use.
     * By default, anything not on the blacklist is allowed.
     * @deprecated SCIPIO: 2.1.0: use {@link #collectOutAttributes(HttpServletRequest)}; configure in request-parameter-filter in controller.
     */
    @Deprecated
    public static Map<String, Object> collectOutAttributesRestricted(HttpServletRequest request) {
        return collectOutAttributes(request);
    }

    public static Set<String> getDefaultOutAttrNames(HttpServletRequest request) {
        return DEFAULT_OUT_ATTRNAMES;
    }

    public static void initOutVars(HttpServletRequest request) {
        getOutParams(request);
        getOutAttrNames(request);
    }

    public static void copyOutVarsToCtx(HttpServletRequest request, Map<String, Object> context) {
        Map<String, Object> globalContext = UtilGenerics.checkMap(context.get("globalContext"));
        globalContext.put(OUT_PARAMS, getOutParams(request));
        globalContext.put(OUT_ATTR, getOutAttrNames(request));
    }

    public static Map<String, Object> getOutParams(HttpServletRequest request) {
        Map<String, Object> outParams = UtilGenerics.checkMap(request.getAttribute(OUT_PARAMS));
        if (outParams == null) {
            outParams = new HashMap<>();
        }
        request.setAttribute(OUT_PARAMS, outParams);
        return outParams;
    }

    public static Map<String, Object> getOutParamsOrNull(HttpServletRequest request) {
        return UtilGenerics.checkMap(request.getAttribute(OUT_PARAMS));
    }

    public static void setOutParams(HttpServletRequest request, Map<String, Object> params) {
        getOutParams(request).putAll(params);
    }

    public static void setOutParam(HttpServletRequest request, String name, Object value) {
        getOutParams(request).put(name, value);
    }

    public static Set<String> getOutAttrNames(HttpServletRequest request) {
        Set<String> outAttrNames = UtilGenerics.checkSet(request.getAttribute(OUT_ATTR));
        if (outAttrNames == null) {
            outAttrNames = new HashSet<>();
        }
        request.setAttribute(OUT_ATTR, outAttrNames);
        return outAttrNames;
    }

    public static Set<String> getOutAttrNamesOrNull(HttpServletRequest request) {
        return UtilGenerics.checkSet(request.getAttribute(OUT_ATTR));
    }

    private static Set<String> getOutAttrNamesOrUnmodifiableEmptySet(HttpServletRequest request) {
        Set<String> outAttrNames = UtilGenerics.checkSet(request.getAttribute(OUT_ATTR));
        return (outAttrNames != null) ? outAttrNames : Collections.emptySet();
    }

    public static void addOutAttrName(HttpServletRequest request, String name) {
        getOutAttrNames(request).add(name);
    }

    public static void addOutAttrNames(HttpServletRequest request, String... names) {
        getOutAttrNames(request).addAll(Arrays.asList(names));
    }

    public static void addOutAttrNames(HttpServletRequest request, Collection<String> names) {
        getOutAttrNames(request).addAll(names);
    }

    public static void addDefaultOutAttrNames(HttpServletRequest request) {
        getOutAttrNames(request).addAll(getDefaultOutAttrNames(request));
    }

}

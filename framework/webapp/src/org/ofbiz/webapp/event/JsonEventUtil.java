package org.ofbiz.webapp.event;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;

import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
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

    /** Attributes ignored by all collectOuAttributes calls. Originally found as CommonEvents.ignoreAttrs */
    private static final Set<String> DEFAULT_IGNORE_ATTRNAMES = UtilMisc.unmodifiableHashSet( // Attributes removed for security reason; _ERROR_MESSAGE_ is kept
            "javax.servlet.request.key_size",
            "_CONTEXT_ROOT_",
            "_FORWARDED_FROM_SERVLET_",
            "javax.servlet.request.ssl_session",
            "javax.servlet.request.ssl_session_id",
            "multiPartMap",
            "javax.servlet.request.cipher_suite",
            "targetRequestUri",
            "_SERVER_ROOT_URL_",
            "_CONTROL_PATH_",
            "thisRequestUri",
            "org.apache.tomcat.util.net.secure_protocol_version",
            OUT_PARAMS,
            OUT_ATTR,
            "cmsCtrlState"
    );

    /** Attributes ignored only by collectOutAttributesRestricted. NOTE: There are more prefixed keys below. */
    private static final Set<String> DEFAULT_IGNORE_ATTRNAMES_RESTRICTED_ONLY = UtilMisc.unmodifiableHashSet(
            "scpUrlReCmnSet",
            "scpCtrlMapping",
            "cmsPreviewMode",
            "scpCtrlServPath",
            "userPresent",
            "_SCPSEO_REQWRAP_",
            "cmsPreviewMode"
    );

    private static final List<String> DEFAULT_IGNORE_ATTRNAMES_RESTRICTED_PREFIXES = UtilMisc.unmodifiableArrayList(
            "javax.servlet.",
            "org.tuckey."
    );

    private static final Set<String> DEFAULT_IGNORE_ATTRNAMES_RESTRICTED = Collections.unmodifiableSet(UtilMisc.addAll(new HashSet<>(),
            DEFAULT_IGNORE_ATTRNAMES, DEFAULT_IGNORE_ATTRNAMES_RESTRICTED_ONLY));

    private JsonEventUtil() {}

    /**
     * Collects out request attributes for a JSON request without any specific filter or only basic
     * required for basic functionality/security; acceptable for backend use.
     * This is the original behavior of CommonEvents.jsonResponseFromRequestAttributes.
     */
    public static Map<String, Object> collectOutAttributes(HttpServletRequest request) {
        Map<String, Object> outParamsMap = getOutParamsOrNull(request);
        Set<String> attrNames = getOutAttrNamesOrUnmodifiableEmptySet(request);
        Map<String, Object> attrMap = UtilHttp.getJSONAttributeMap(request, new UtilHttp.AttributeFilter() {
            @Override
            public Boolean includeAttribute(String key, Object value) {
                if (attrNames.contains(key)) {
                    return true;
                }
                if (DEFAULT_IGNORE_ATTRNAMES.contains(key)) {
                    return false;
                }
                return null;
            }
        });
        if (UtilValidate.isNotEmpty(outParamsMap)) {
            UtilHttp.transformJSONAttributeMap(attrMap, outParamsMap, null);
        }
        return attrMap;
    }

    /**
     * Collects out request attributes for a JSON request using restricted blacklist; acceptable for frontend use.
     * By default, anything not on the blacklist is allowed.
     */
    public static Map<String, Object> collectOutAttributesRestricted(HttpServletRequest request) {
        Map<String, Object> outParamsMap = getOutParamsOrNull(request);
        Set<String> attrNames = getOutAttrNamesOrUnmodifiableEmptySet(request);
        Map<String, Object> attrMap = UtilHttp.getJSONAttributeMap(request, new UtilHttp.AttributeFilter() {
            @Override
            public Boolean includeAttribute(String key, Object value) {
                if (attrNames.contains(key)) {
                    return true;
                }
                if (DEFAULT_IGNORE_ATTRNAMES_RESTRICTED.contains(key)) {
                    return false;
                }
                if (key == null) { // probably shouldn't happen?
                    return false;
                }
                for(String prefix : DEFAULT_IGNORE_ATTRNAMES_RESTRICTED_PREFIXES) {
                    if (key.startsWith(prefix)) {
                        return false;
                    }
                }
                return null;
            }
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
            UtilHttp.getJSONAttributeMap(attrMap, request, new UtilHttp.AttributeFilter() {
                @Override
                public Boolean includeAttribute(String key, Object value) {
                    if (attrNames.contains(key)) {
                        return null;
                    }
                    return false;
                }
            });
        }
        return attrMap;
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

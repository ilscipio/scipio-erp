package org.ofbiz.webapp.control;

import java.io.StringWriter;
import java.io.Writer;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.webapp.control.ConfigXMLReader.ViewAsJsonConfig;
import org.ofbiz.webapp.renderer.RenderTargetUtil;

/**
 * SCIPIO: helper for viewAsJson mode.
 * viewAsJson mode is enabled by passing request parameter (or attribute, as Boolean) 
 * <code>scpViewAsJson=true</code>.
 */
public abstract class ViewAsJsonUtil {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    
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
     * Name of the request parameter/attribute checked for whether should use
     * default split mode ("default") or unreliable fast split mode ("fast").
     * 
     * TODO: NOT IMPLEMENTED: fast mode not yet implemented because it is more likely to fail
     * than the default one.
     * This will require a special new version of {@link org.ofbiz.webapp.renderer.RenderWriter.SwitchRenderWriter}
     * and in current ofbiz code such writer is NOT guaranteed to work because such writer can't cross
     * intermediate StringWriter boundaries.
     */
    @Deprecated
    public static final String VIEWASJSONSPLITMODE_REQPARAM = "scpViewAsJsonSplitMode";

    public static final Set<String> VIEWASJSON_REQPARAM_ALL = UtilMisc.unmodifiableHashSet(VIEWASJSON_REQPARAM, VIEWASJSONUSESESSION_REQPARAM, VIEWASJSONREGLOGIN_REQPARAM, VIEWASJSONSPLITMODE_REQPARAM);
    public static final Set<String> VIEWASJSON_RENDERTARGET_REQPARAM_ALL;
    static {
        Set<String> set = new HashSet<>();
        set.addAll(VIEWASJSON_REQPARAM_ALL);
        set.addAll(RenderTargetUtil.RENDERTARGET_REQPARAM_ALL);
        VIEWASJSON_RENDERTARGET_REQPARAM_ALL = Collections.unmodifiableSet(set);
    }
    
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

    public static void initRenderOutVars(HttpServletRequest request) {
        getRenderOutParams(request);
        getRenderOutAttrNames(request);
    }
    
    public static void copyRenderOutVarsToCtx(HttpServletRequest request, Map<String, Object> context) {
        Map<String, Object> globalContext = UtilGenerics.checkMap(context.get("globalContext"));
        globalContext.put(RENDEROUTPARAMS_ATTR, getRenderOutParams(request));
        globalContext.put(RENDEROUTATTRNAMES_ATTR, getRenderOutAttrNames(request));
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
    
    public static ViewAsJsonConfig getViewAsJsonConfigOrDefault(HttpServletRequest request) {
        try {
            RequestHandler rh = RequestHandler.getRequestHandler(request);
            return rh.getControllerConfig().getViewAsJsonConfigOrDefault();
        } catch (Exception e) {
            Debug.logError(e, "Could not get ViewAsJsonConfig", module);
            return new ConfigXMLReader.ViewAsJsonConfig();
        }
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
        Boolean updateSession = (Boolean) request.getAttribute(VIEWASJSONUSESESSION_REQPARAM);
        if (updateSession != null) return updateSession;
        else return UtilMisc.booleanValue(request.getParameter(VIEWASJSONUSESESSION_REQPARAM), config.isUpdateSession());
    }
    
    /**
     * Checks if view updating session is enabled.
     * WARN: does NOT check if viewAsJson is on; caller should have already checked (use {@link #isViewAsJson(HttpServletRequest)}).
     */
    public static boolean isViewAsJsonRegularLogin(HttpServletRequest request, ViewAsJsonConfig config) {
        Boolean regLogin = (Boolean) request.getAttribute(VIEWASJSONREGLOGIN_REQPARAM);
        if (regLogin != null) return regLogin;
        else return UtilMisc.booleanValue(request.getParameter(VIEWASJSONREGLOGIN_REQPARAM), config.isRegularLogin());
    }
    
    public static boolean isViewAsJsonSplitModeFast(HttpServletRequest request, ViewAsJsonConfig config) {
        String mode = (String) request.getAttribute(VIEWASJSONSPLITMODE_REQPARAM);
        if (mode != null) return "fast".equals(mode);
        else return "fast".equals(request.getParameter(VIEWASJSONSPLITMODE_REQPARAM));
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
    
    public static Writer prepareWriterAndMode(HttpServletRequest request, ViewAsJsonConfig config) {
        // PRE-INITIALIZE the out params map and names list, so that screens can more easily access and so we pre-share the instances
        initRenderOutVars(request);
        
        // TODO: REVIEW/MOVE: SPECIAL TARGETED RENDERING LOGIC NEEDED FOR MULTI-TARGET SUPPORT
        Object expr = RenderTargetUtil.getSetRawRenderTargetExpr(request);
        if (RenderTargetUtil.isRenderTargetExprOn(request, expr)) {
            if (RenderTargetUtil.isRenderTargetExprMulti(expr)) {
                RenderTargetUtil.generateSetMultiTargetDelimiterPrefix(request);
                // TODO: NOT IMPLEMENTED: fast mode not yet implemented because it is more likely to fail.
//                if (isViewAsJsonSplitModeFast(request, config)) {
//                    // TODO: this requires something else
//                    return new StringWriter();
//                } else {
//                    return new StringWriter();
//                }
                return new StringWriter();
            } else {
                return new StringWriter();
            }
        } else {
            return new StringWriter();
        }
    }
    
    public static Object makeRenderOutParamValueFromWriter(HttpServletRequest request, Writer writer) {
        Object expr = RenderTargetUtil.getRawRenderTargetExpr(request);
        if (RenderTargetUtil.isRenderTargetExprOn(request, expr) && RenderTargetUtil.isRenderTargetExprMulti(expr)) {
            String delimPrefix = (String) request.getAttribute(RenderTargetUtil.MULTITARGET_DELIM_PREFIX_ATTR);
            if (delimPrefix != null && !delimPrefix.isEmpty()) {
                return RenderTargetUtil.extractMultiTargetOutputs(writer, delimPrefix);
            } else {
                throw new IllegalStateException("viewAsJson: targeted rendering failed to set a " 
                        + RenderTargetUtil.MULTITARGET_DELIM_PREFIX_ATTR + " request attribute");
            }
        } else {
            if (writer instanceof StringWriter) {
                StringWriter sw = (StringWriter) writer;
                return sw.toString();
            } else {
                throw new IllegalArgumentException("Unsupported writer class for viewAsJson: " + writer.getClass().getName());
            }
        }
    }
    
    public static void setRenderOutParamFromWriter(HttpServletRequest request, Writer writer) {
        ViewAsJsonUtil.setRenderOutParam(request, ViewAsJsonUtil.RENDEROUT_OUTPARAM, makeRenderOutParamValueFromWriter(request, writer));
    }
}

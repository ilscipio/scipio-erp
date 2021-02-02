package org.ofbiz.widget.renderer;

import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import com.ilscipio.scipio.ce.webapp.ftl.context.ContextFtlUtil;
import freemarker.template.TemplateModelException;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.ScriptUtil;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.FullWebappInfo;
import org.ofbiz.webapp.renderer.RenderEnvType;
import org.ofbiz.webapp.website.WebSiteWorker;

/**
 * SCIPIO: Helper utils to get/set renderer visual theme resources.
 */
public class VisualThemeWorker {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final UtilCache<String, String> libLocationExprCache = UtilCache.createUtilCache("renderer.visualtheme.resources.liblocation");
    private static final Map<String, Object> emptyContext = Collections.unmodifiableMap(new HashMap<String, Object>());

    // SCIPIO
    private static final Map<String, List<String>> FTL_LIB_VAR_RESOURCE_NAMES = Collections.unmodifiableMap(UtilMisc.toMap(
            "web", UtilMisc.unmodifiableArrayList("VT_STL_VAR_WEB", "VT_STL_VAR_LOC"),
            "email", UtilMisc.unmodifiableArrayList("VT_STL_VAR_MAIL", "VT_STL_VAR_LOC"),
            "default", UtilMisc.unmodifiableArrayList("VT_STL_VAR_LOC")));
    private static final Map<String, List<String>> FTL_LIB_TMPL_RESOURCE_NAMES = Collections.unmodifiableMap(UtilMisc.toMap(
            "web", UtilMisc.unmodifiableArrayList("VT_STL_TMPLT_WEB", "VT_STL_TMPLT_LOC"),
            "email", UtilMisc.unmodifiableArrayList("VT_STL_TMPLT_MAIL", "VT_STL_TMPLT_LOC"),
            "default", UtilMisc.unmodifiableArrayList("VT_STL_TMPLT_LOC")));
    private static final UtilCache<String, Map<String, Object>> FTL_LIB_VAR_STATIC_CACHE = UtilCache.createUtilCache("render.api.lib.variables.static");


    /**
     * SCIPIO: Gets visual theme resources from context or if missing, calculates appropriate
     * based on values in context, best-effort, using as generalized logic as possible, saves in context and returns.
     * <p>
     * Resources are cached in <code>rendererVisualThemeResources</code> context variable.
     */
    public static Map<String, List<String>> getVisualThemeResources(Map<String, Object> context) {
        Map<String, List<String>> themeResources = null;

        if (Boolean.TRUE.equals((Boolean) context.get("rendererVisualThemeResourcesChecked"))) {
            themeResources = UtilGenerics.cast(context.get("rendererVisualThemeResources"));
        } else {
            try {
                themeResources = VisualThemeWorker.loadVisualThemeResources(context);
            } catch (GenericServiceException e) {
                Debug.logError(e, "Could not load visual theme resources", module);
            }

            context.put("rendererVisualThemeResources", themeResources);
            context.put("rendererVisualThemeResourcesChecked", Boolean.TRUE);
        }
        return themeResources;
    }

    /**
     * SCIPIO: Loads visual theme resources from context, best-effort, using as generalized logic as possible.
     * <p>
     * Code migrated from <code>org.ofbiz.widget.renderer.macro.MacroScreenViewHandler#loadRenderers}</code>.
     * Original code only checked for preference of userPreferences.
     */
    public static Map<String, List<String>> loadVisualThemeResources(Map<String, Object> context) throws GenericServiceException {
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        Map<String, List<String>> themeResources = null;
        if (request != null) {
            GenericValue webSite = WebSiteWorker.getWebSite(request);
            String script = null;
            if (webSite != null) {
                script = webSite.getString("visualThemeSelectorScript");
            }

            if (UtilValidate.isNotEmpty(script)) {
                themeResources = VisualThemeWorker.loadVisualThemeResourcesFromScript(context, script, webSite);
            } else {
                // original stock behavior
                themeResources = VisualThemeWorker.getVisualThemeResourcesFromUserPrefs(context);
            }
        }
        else {
            // this might be an email...
            String webSiteId = (String) context.get("baseWebSiteId"); // set by org.ofbiz.common.email.NotificationServices#setBaseUrl
            if (UtilValidate.isEmpty(webSiteId)) {
                // FIXME? fallback to webSiteId var, but it's not clear this is a good idea
                webSiteId = (String) context.get("webSiteId");
            }
            String script = null;
            GenericValue webSite = null;
            if (UtilValidate.isNotEmpty(webSiteId)) {
                webSite = WebSiteWorker.findWebSite((Delegator) context.get("delegator"), webSiteId);
                if (webSite != null) {
                    script = webSite.getString("visualThemeSelectorScript");
                }
            }

            if (UtilValidate.isNotEmpty(script)) {
                themeResources = VisualThemeWorker.loadVisualThemeResourcesFromScript(context, script, webSite);
            } else {
                // FIXME? not sure original stock behavior makes sense here because userLoginId ambiguous - may not be end user
                //themeResources = getVisualThemeResourcesFromUserPrefs(context);
                themeResources = VisualThemeWorker.getDefaultVisualThemeResources(context);
            }
        }
        return themeResources;
    }

    /**
     * SCIPIO: Gets resources from user preferences already in context.
     * <p>
     * Code migrated from <code>org.ofbiz.widget.renderer.macro.MacroScreenViewHandler#loadRenderers</code>.
     * @throws GenericServiceException
     */
    public static Map<String, List<String>> getVisualThemeResourcesFromUserPrefs(Map<String, Object> context) throws GenericServiceException {
        Map<String, Object> userPreferences = UtilGenerics.cast(context.get("userPreferences"));
        return VisualThemeWorker.getVisualThemeResourcesFromUserPrefs(context, userPreferences);
    }

    public static Map<String, List<String>> getVisualThemeResourcesFromUserPrefs(Map<String, Object> context,
            Map<String, Object> userPreferences) throws GenericServiceException {
        if (userPreferences != null) {
            String visualThemeId = (String) userPreferences.get("VISUAL_THEME");
            if (visualThemeId != null) {
                LocalDispatcher dispatcher = (LocalDispatcher) context.get("dispatcher");
                Map<String, Object> serviceCtx = dispatcher.getDispatchContext().makeValidContext("getVisualThemeResources",
                        ModelService.IN_PARAM, context);
                serviceCtx.put("visualThemeId", visualThemeId);
                Map<String, Object> serviceResult = dispatcher.runSync("getVisualThemeResources", serviceCtx);
                if (ServiceUtil.isSuccess(serviceResult)) {
                    return UtilGenerics.cast(serviceResult.get("themeResources"));
                }
            }
        }
        return null;
    }

    /**
     * SCIPIO: Gets default system visual theme resources, i.e. for _NA_ user login.
     */
    public static Map<String, List<String>> getDefaultVisualThemeResources(Map<String, Object> context) throws GenericServiceException {
        // emulate default call exactly, but with no userLogin
        LocalDispatcher dispatcher = (LocalDispatcher) context.get("dispatcher");
        Map<String, Object> userPreferences = null;
        try {
            Map<String, Object> result = dispatcher.runSync("getUserPreferenceGroup", UtilMisc.toMap("userLogin", null, "userPrefGroupTypeId", "GLOBAL_PREFERENCES"));
            userPreferences = UtilGenerics.cast(result.get("userPrefMap"));
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error while getting user preferences: ", module);
        }

        return getVisualThemeResourcesFromUserPrefs(context, userPreferences);
    }

    /**
     * SCIPIO: Invokes a script to choose a visualThemeId and load its resources.
     */
    public static Map<String, List<String>> loadVisualThemeResourcesFromScript(Map<String, Object> context,
            String script, GenericValue webSite) throws GenericServiceException {
        Map<String, Object> scriptCtx = new HashMap<String, Object>(context);
        scriptCtx.put("webSite", webSite);
        scriptCtx.remove("visualThemeId");
        ScriptUtil.executeScript(script, null, scriptCtx);

        String visualThemeId = (String) scriptCtx.get("visualThemeId");
        if (UtilValidate.isNotEmpty(visualThemeId)) {
            LocalDispatcher dispatcher = (LocalDispatcher) context.get("dispatcher");
            Map<String, Object> serviceCtx = dispatcher.getDispatchContext().makeValidContext("getVisualThemeResources",
                    ModelService.IN_PARAM, context);
            serviceCtx.put("visualThemeId", visualThemeId);
            Map<String, Object> serviceResult = dispatcher.runSync("getVisualThemeResources", serviceCtx);
            if (ServiceUtil.isSuccess(serviceResult)) {
                return UtilGenerics.cast(serviceResult.get("themeResources"));
            }
        }
        return null;
    }


    /**
     * SCIPIO: Gets a library location from a library location expression for the given rendering platform.
     * <p>
     * The values returned from this method are cached in a global cache, so they should
     * not be used for arbitrary inputs.
     *
     * @param locationExpr the location expression
     * @param platform the current rendering platform name, e.g. "html", "xml", etc.
     * @return the location or null if not present and no default
     * @see #getMacroLibraryLocation(String, String, Map)
     */
    public static String getMacroLibraryLocationStatic(String locationExpr, String platform) {
        if (locationExpr == null || locationExpr.isEmpty()) {
            return null;
        }
        final String cacheKey = locationExpr + "::" + (platform != null ? platform : "");
        String res = libLocationExprCache.get(cacheKey);
        if (res == null) {
            res = getMacroLibraryLocation(locationExpr, platform, emptyContext);
            // empty string marks the lookup
            libLocationExprCache.put(cacheKey, (res != null ? res : ""));
        }
        if (res != null && !res.isEmpty()) {
            return res;
        } else {
            return null;
        }
    }

    public static String getMacroLibraryLocationStaticFromResources(String platform, Map<String, List<String>> themeResources,
                                                                    Collection<String> resourceNames) {
        if (themeResources == null) {
            return null;
        }
        for(String resourceName : resourceNames) {
            List<String> resourceList = UtilGenerics.cast(themeResources.get(resourceName));
            if (resourceList != null && !resourceList.isEmpty()) {
                String macroLibraryPath = getMacroLibraryLocationStatic(resourceList.get(0), platform);
                if (macroLibraryPath != null && !macroLibraryPath.isEmpty()) {
                    return macroLibraryPath;
                }
            }
        }
        return null;
    }

    public static String getMacroLibraryLocationStaticFromResources(String platform, Map<String, List<String>> themeResources,
                                                                    String... resourceNames) {
        return getMacroLibraryLocationStaticFromResources(platform, themeResources, Arrays.asList(resourceNames));
    }

    /**
     * SCIPIO: Gets a library location from a library location expression for the given rendering platform.
     * <p>
     * The expression is either a simple location or a flexible EL defining a map.
     * The map maps platform names to locations.
     * If the location is simple, it is only returned if the platform is "html".
     * The map expression supports an additional "default" key that will be used in case platform
     * matches nothing else.
     *
     * @param locationExpr the location expression
     * @param platform the current rendering platform name, e.g. "html", "xml", etc.
     * @return the location or null if not present and no default
     */
    public static String getMacroLibraryLocation(String locationExpr, String platform, Map<String, Object> context) {
        if (locationExpr == null || locationExpr.isEmpty()) {
            return null;
        }
        Object expanded;
        if (locationExpr.startsWith(FlexibleStringExpander.openBracket)) {
            FlexibleStringExpander ex = FlexibleStringExpander.getInstance(locationExpr);
            expanded = ex.expand(context);
            if (expanded == null) {
                return null;
            }
        } else if (locationExpr.startsWith("{")) {
            expanded = StringUtil.toMap(locationExpr.replaceAll("\\s+", ""));
        } else {
            expanded = locationExpr;
        }

        if (expanded instanceof String) {
            // simple location, meant for html
            if ("html".equals(platform)) {
                String res = (String) expanded;
                return res.isEmpty() ? null : res;
            }
            else {
                return null;
            }
        } else if (expanded instanceof Map) {
            return getMacroLibraryLocation(UtilGenerics.<Map<String, String>>cast(expanded), platform);
        } else {
            return null;
        }
    }

    public static String getMacroLibraryLocation(Map<String, String> platformLocationMap, String platform) {
        if (platform == null) {
            platform = "";
        }
        String res = platformLocationMap.get(platform);
        if (res != null && !res.isEmpty()) {
            return res;
        } else {
            res = platformLocationMap.get("default");
            if (res != null && !res.isEmpty()) {
                return res;
            } else {
                return null;
            }
        }
    }

    public static String getDefaultScipioLibLocation(String libName, String renderPlatformType, String renderContextType, Delegator delegator) {
        // NOTE: delegator could be null
        if (renderPlatformType == null) {
            renderPlatformType = "default";
        }
        if (renderContextType == null) {
            renderContextType = "general";
        }
        String loc = UtilProperties.getPropertyValue("scipioWebapp", "scipio.templating.lib." + renderContextType + "." + renderPlatformType + "."  + libName  + ".location");
        if (UtilValidate.isNotEmpty(loc)) {
            return loc;
        }
        if (!"general".equals(renderContextType)) {
            loc = UtilProperties.getPropertyValue("scipioWebapp", "scipio.templating.lib.general." + renderPlatformType  + "." + libName + ".location");
            if (UtilValidate.isNotEmpty(loc)) {
                return loc;
            }
        }
        if (!"default".equals(renderPlatformType)) {
            loc = UtilProperties.getPropertyValue("scipioWebapp", "scipio.templating.lib." + renderContextType + ".default." + libName + ".location");
            if (UtilValidate.isNotEmpty(loc)) {
                return loc;
            }
        }
        loc = UtilProperties.getPropertyValue("scipioWebapp", "scipio.templating.lib.general.default." + libName + ".location");
        if (UtilValidate.isNotEmpty(loc)) {
            return loc;
        }
        return null;
    }

    public static List<String> getFtlLibVariableResourceNames(String renderContextType) {
        List<String> resourceNames = FTL_LIB_VAR_RESOURCE_NAMES.get(renderContextType);
        return (resourceNames != null) ? resourceNames : FTL_LIB_VAR_RESOURCE_NAMES.get("default");
    }

    public static List<String> getFtlLibTemplateResourceNames(String renderContextType) {
        List<String> resourceNames = FTL_LIB_TMPL_RESOURCE_NAMES.get(renderContextType);
        return (resourceNames != null) ? resourceNames : FTL_LIB_TMPL_RESOURCE_NAMES.get("default");
    }

    public static Map<String, Object> getFtlLibVariables(Map<String, Object> context, Map<String, Object> args) {
        if (context == null) {
            context = new HashMap<>(); // May be written to
        }
        if (args == null) {
            args = Collections.emptyMap();
        }
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        boolean useCache = !Boolean.FALSE.equals(args.get("useCache"));

        String renderPlatformType = RenderEnvWorker.getRenderPlatformType(context);
        String renderContextType = RenderEnvWorker.getRenderContextType(context);
        Map<String, List<String>> themeResources = UtilGenerics.cast(context.get("rendererVisualThemeResources"));

        Map<String, Object> scpTmplGlobalVars = null;
        String staticCacheKey = null;
        if (useCache) {
            try {
                scpTmplGlobalVars = UtilGenerics.cast(ContextFtlUtil.getRequestVar("scpLibVarsRaw", request, context));
                if (scpTmplGlobalVars != null) {
                    return scpTmplGlobalVars;
                }
            } catch (ClassCastException | TemplateModelException e) {
                Debug.logError("Could not read scpLibVarsRaw from context: " + e.toString(), module);
            }
            if (FTL_LIB_VAR_STATIC_CACHE.isEnabled()) {
                Delegator delegator = (Delegator) context.get("delegator");
                RenderEnvType renderEnvType = RenderEnvType.fromContext(context);
                FullWebappInfo webappInfo = FullWebappInfo.fromContext(context, renderEnvType);
                if (themeResources != null || webappInfo != null) {
                    // Cache is static and non-weak so key can only includes webapp-level
                    staticCacheKey = (themeResources != null ? UtilMisc.firstSafe(themeResources.get("VT_ID")) : null) + "::" +
                            (webappInfo != null ? webappInfo.getWebSiteId() : null) + "::" +
                            (webappInfo != null ? webappInfo.getServerId() : null) + "::" +
                            (webappInfo != null ? webappInfo.getContextPath() : null) + "::" +
                            delegator.getDelegatorName();
                    scpTmplGlobalVars = FTL_LIB_VAR_STATIC_CACHE.get(staticCacheKey);
                    if (scpTmplGlobalVars != null) {
                        return scpTmplGlobalVars;
                    }
                }
            }
        }

        String scpVarLibPath = null;
        if (themeResources != null) {
            scpVarLibPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, themeResources,
                    getFtlLibVariableResourceNames(renderContextType));
        }
        if (UtilValidate.isEmpty(scpVarLibPath)) {
            scpVarLibPath = getDefaultScipioLibLocation("variables", renderPlatformType, renderContextType, (Delegator) context.get("delegator"));
            if (UtilValidate.isEmpty(scpVarLibPath)) {
                Debug.logWarning("No library variables location defined in system or visual theme, cannot fetch Scipio " +
                        "variables for renderPlatformType [" + renderPlatformType + "] renderContextType [" + renderContextType + "]", module);
                return null;
            }
        }

        try {
            scpTmplGlobalVars = GroovyUtil.runScriptAtLocationNewEmptyContext(scpVarLibPath, "");
        } catch (GeneralException e) {
            Debug.logError("Could not run library variables template [" + scpVarLibPath + "] for renderPlatformType [" + renderPlatformType +
                "] renderContextType [" + renderContextType + "]: " + e.toString(), module);
            return null;
        }
        scpTmplGlobalVars = Collections.unmodifiableMap(scpTmplGlobalVars);

        if (useCache) {
            try {
                ContextFtlUtil.setRequestVar("scpLibVarsRaw", scpTmplGlobalVars, request, context);
            } catch (TemplateModelException e) {
                Debug.logError("Could not set request var scpLibVarsRaw: " + e.toString(), module);
            }
            if (staticCacheKey != null) {
                FTL_LIB_VAR_STATIC_CACHE.put(staticCacheKey, scpTmplGlobalVars);
            }
        }
        return scpTmplGlobalVars;
    }

    public static Map<String, Object> getFtlLibVariables(Map<String, Object> context) {
        return getFtlLibVariables(context, Collections.emptyMap());
    }

    public static String getFtlLibTemplatePath(Map<String, Object> context, Map<String, Object> args) {
        if (context == null) {
            context = new HashMap<>(); // May be written to
        }
        if (args == null) {
            args = Collections.emptyMap();
        }
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        boolean useCache = !Boolean.FALSE.equals(args.get("useCache"));

        String scpLibTmplPath = null;
        if (useCache) {
            try {
                scpLibTmplPath = UtilGenerics.cast(ContextFtlUtil.getRequestVar("scpLibTmplPath", request, context));
                if (scpLibTmplPath != null) {
                    return scpLibTmplPath;
                }
            } catch (ClassCastException | TemplateModelException e) {
                Debug.logError("Could not read scpLibTmplPath from context: " + e.toString(), module);
            }
        }

        String renderPlatformType = RenderEnvWorker.getRenderPlatformType(context);
        String renderContextType = RenderEnvWorker.getRenderContextType(context);
        Map<String, List<String>> themeResources = UtilGenerics.cast(context.get("rendererVisualThemeResources"));

        if (themeResources != null) {
            scpLibTmplPath = getMacroLibraryLocationStaticFromResources(renderPlatformType, themeResources,
                    getFtlLibTemplateResourceNames(renderContextType));
        }
        if (UtilValidate.isEmpty(scpLibTmplPath)) {
            scpLibTmplPath = getDefaultScipioLibLocation("template", renderPlatformType, renderContextType, (Delegator) context.get("delegator"));
            if (UtilValidate.isEmpty(scpLibTmplPath)) {
                Debug.logWarning("No library template location defined in system or visual theme, cannot fetch Scipio" +
                        " variables template for renderPlatformType [" + renderPlatformType + "] renderContextType [" + renderContextType + "]", module);
                return null;
            }
        }

        try {
            ContextFtlUtil.setRequestVar("scpLibTmplPath", scpLibTmplPath, request, context);
        } catch (TemplateModelException e) {
            Debug.logError("Could not set request var scpLibTmplPath: " + e.toString(), module);
        }
        return scpLibTmplPath;
    }

    public static String getFtlLibTemplatePath(Map<String, Object> context) {
        return getFtlLibTemplatePath(context, Collections.emptyMap());
    }
}

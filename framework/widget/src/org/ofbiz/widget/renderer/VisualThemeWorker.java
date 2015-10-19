package org.ofbiz.widget.renderer;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.ScriptUtil;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.webapp.website.WebSiteWorker;

/**
 * Cato: Helper utils to get/set renderer visual theme resources.
 */
public class VisualThemeWorker {

    /**
     * Cato: Gets visual theme resources from context or if missing, calculates appropriate
     * based on values in context, best-effort, using as generalized logic as possible, saves in context and returns.
     * <p>
     * Resources are cached in <code>rendererVisualThemeResources</code> context variable.
     */
    public static Map<String, List<String>> getVisualThemeResources(Map<String, Object> context) {
        Map<String, List<String>> themeResources = null;
        
        if (Boolean.TRUE.equals((Boolean) context.get("rendererVisualThemeResourcesChecked"))) {
            themeResources = UtilGenerics.cast(context.get("rendererVisualThemeResources"));
        }
        else {
            try {
                themeResources = VisualThemeWorker.loadVisualThemeResources(context);
            } catch (GenericServiceException e) {
                Debug.logError(e, "Could not load visual theme resources", ScreenRenderer.module);
            }
    
            context.put("rendererVisualThemeResources", themeResources);
            context.put("rendererVisualThemeResourcesChecked", Boolean.TRUE);
        }
        return themeResources;
    }

    /**
     * Cato: Loads visual theme resources from context, best-effort, using as generalized logic as possible.
     * <p>
     * Code migrated from {@link org.ofbiz.widget.renderer.macro.MacroScreenViewHandler#loadRenderers(HttpServletRequest, HttpServletResponse, Map<String, Object>, Writer)}.
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
            }
            else {
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
            }
            else {
                // FIXME? not sure original stock behavior makes sense here because userLoginId ambiguous - may not be end user
                //themeResources = getVisualThemeResourcesFromUserPrefs(context);
                themeResources = VisualThemeWorker.getDefaultVisualThemeResources(context);           
            }
        }
        return themeResources;
    }

    /**
     * Cato: Gets resources from user preferences already in context.
     * <p>
     * Code migrated from {@link org.ofbiz.widget.renderer.macro.MacroScreenViewHandler#loadRenderers(HttpServletRequest, 
     * HttpServletResponse, Map<String, Object>, Writer)}.
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
     * Cato: Gets default system visual theme resources, i.e. for _NA_ user login.
     */
    public static Map<String, List<String>> getDefaultVisualThemeResources(Map<String, Object> context) throws GenericServiceException {
        // emulate default call exactly, but with no userLogin
        LocalDispatcher dispatcher = (LocalDispatcher) context.get("dispatcher");
        Map<String, Object> userPreferences = null;
        try {
            Map<String, Object> result = dispatcher.runSync("getUserPreferenceGroup", UtilMisc.toMap("userLogin", null, "userPrefGroupTypeId", "GLOBAL_PREFERENCES"));
            userPreferences = UtilGenerics.cast(result.get("userPrefMap"));
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error while getting user preferences: ", ScreenRenderer.module);
        }
        
        return getVisualThemeResourcesFromUserPrefs(context, userPreferences);
    }

    /**
     * Cato: Invokes a script to choose a visualThemeId and load its resources.
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

}

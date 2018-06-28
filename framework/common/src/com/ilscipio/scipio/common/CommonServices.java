package com.ilscipio.scipio.common;

import java.io.IOException;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.common.email.NotificationServices;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.widget.model.ModelScreen;
import org.ofbiz.widget.renderer.ScreenRenderer;
import org.ofbiz.widget.renderer.ScreenStringRenderer;
import org.ofbiz.widget.renderer.macro.MacroScreenRenderer;
import org.xml.sax.SAXException;

import freemarker.template.TemplateException;

/**
 * SCIPIO: Common Services
 */
public class CommonServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final String resource = "CommonUiLabels";

    /**
     * Reload visual theme definitions of specified theme or all themes if not specified.
     * 
     * @param dctx The DispatchContext that this service is operating in
     * @param context Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> reloadVisualThemeResources(DispatchContext dctx, Map<String, ?> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        String singleVisualThemeId = (String) context.get("visualThemeId");

        try {
            // 2016-04-25: VS type defs in file
            // TODO: unhardcode locations into properties
            {
                final String filename = "component://common/data/CommonVisualThemeTypeData.xml"; // TODO: Unhardcode
                List<String> messages = new ArrayList<>();
                Map<String, Object> servCtx = dctx.makeValidContext("entityImport", ModelService.IN_PARAM, context);
                servCtx.put("filename", filename);
                servCtx.put("isUrl", "Y");
                servCtx.put("messages", messages);
                Map<String, Object> servResult = dispatcher.runSync("entityImport", servCtx);
                if (ServiceUtil.isError(servResult)) {
                    return ServiceUtil.returnError("Could not import common visual theme type data from " + filename + ": " + 
                            ServiceUtil.getErrorMessage(servResult)); 
                }
            }
            
            delegator.clearCacheLine("EnumerationType"); // FIXME: overly broad, but it's probably okay
            delegator.clearCacheLine("Enumeration");
            delegator.clearCacheLine("VisualThemeSet");
            
            List<String> visualThemeIds = new ArrayList<>();
            if (singleVisualThemeId != null) {
                GenericValue visualTheme = delegator.findOne("VisualTheme", false, UtilMisc.toMap("visualThemeId", singleVisualThemeId));
                if (visualTheme != null) {
                    visualThemeIds.add(singleVisualThemeId);
                }
                else {
                    return ServiceUtil.returnError("Could not find visual theme " + singleVisualThemeId);
                }
            }
            else {
                List<GenericValue> visualThemes = delegator.findAll("VisualTheme", false);
                if (visualThemes != null) {
                    for(GenericValue visualTheme : visualThemes) {
                        visualThemeIds.add(visualTheme.getString("visualThemeId"));
                    }
                }
            }
            
            if (!visualThemeIds.isEmpty()) {
                int numReloaded = 0;
                for(String visualThemeId : visualThemeIds) {
            
                    List<GenericValue> themeFileValues = delegator.findByAnd("VisualThemeResource", 
                            UtilMisc.toMap("visualThemeId", visualThemeId, "resourceTypeEnumId", "VT_THEME_DATA_RES"),
                            UtilMisc.toList("sequenceId"), false);
                    
                    List<String> themeFileLocations = new ArrayList<>();
                    if (themeFileValues != null) {
                        for(GenericValue themeFileValue : themeFileValues) {
                            String themeFileLocation = themeFileValue.getString("resourceValue");
                            if (UtilValidate.isNotEmpty(themeFileLocation)) {
                                themeFileLocations.add(themeFileLocation);
                            }
                        }
                    }
                    
                    if (UtilValidate.isNotEmpty(themeFileLocations)) {
                        delegator.removeByAnd("VisualThemeResource", UtilMisc.toMap("visualThemeId", visualThemeId));
                        // Don't do this line because technically violates foreign keys on other tables; not really needed anyway
                        //delegator.removeByAnd("VisualTheme", UtilMisc.toMap("visualThemeId", visualThemeId));
                        
                        for(String themeFileLocation : themeFileLocations) {
                            List<String> messages = new ArrayList<>();
                            Map<String, Object> servCtx = dctx.makeValidContext("entityImport", ModelService.IN_PARAM, context);
                            servCtx.put("filename", themeFileLocation);
                            servCtx.put("isUrl", "Y");
                            servCtx.put("messages", messages);
                            Map<String, Object> servResult = dispatcher.runSync("entityImport", servCtx);
                            if (ServiceUtil.isError(servResult)) {
                                return ServiceUtil.returnError("Could not load visual theme " + visualThemeId + 
                                        " resource data " + themeFileLocations + ": " + 
                                        ServiceUtil.getErrorMessage(servResult)); 
                            }
                        }
                        numReloaded++;
                    }
                    else {
                        Debug.logWarning("Visual theme " + visualThemeId + 
                                " has no data files to reload from (VT_THEME_DATA_RES resource); skipping; " +
                                "consider updating the theme to specify VT_THEME_DATA_RES resources and " +
                                "performing an initial re-seed", visualThemeId);
                    }
                }
                
                delegator.clearCacheLine("VisualThemeSet");
                delegator.clearCacheLine("VisualThemeResource");
                delegator.clearCacheLine("VisualTheme");
                
                final String msg = "Reloaded " + numReloaded + "/" + visualThemeIds.size() + " visual theme(s)";
                if (numReloaded >= visualThemeIds.size()) {
                    return ServiceUtil.returnSuccess(msg);
                }
                else {
                    return ServiceUtil.returnFailure(msg);
                }
            }
            else {
                return ServiceUtil.returnSuccess("No visual themes to reload");
            }
        }
        catch(Exception e) {
            final String errorMsg = "Exception trying to reload visual theme(s)";
            Debug.logError(e, errorMsg, module);
            return ServiceUtil.returnError(errorMsg + ": " + e.getMessage());
        }
    }
    
    /**
     * A service used to capture file changes on the system. Automatically implements file listeners for all components recursively.
     * 
     * @param dctx The DispatchContext that this service is operating in
     * @param context Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> startFileListener(DispatchContext dctx, Map<String, ?> context) {
        //Delegator delegator = dctx.getDelegator();
        //LocalDispatcher dispatcher = dctx.getDispatcher();
        try {
            Collection<ComponentConfig> allComponents = ComponentConfig.getAllComponents();
            for (ComponentConfig config : allComponents) {
                String name = "component-"+config.getComponentName();
                String location = config.getRootLocation();
                FileListener.startFileListener(name,location); 
            }
            return ServiceUtil.returnSuccess("File event registered.");
        }
        catch(Exception e) {
            final String errorMsg = "Exception triggering File Event";
            Debug.logError(e, errorMsg, module);
            return ServiceUtil.returnError(errorMsg + ": " + e.getMessage());
        }
    }
    
    
    /**
     * An empty service used to capture file changes on the system.
     * 
     * @param dctx The DispatchContext that this service is operating in
     * @param context Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> triggerFileEvent(DispatchContext dctx, Map<String, ?> context) {
        //Delegator delegator = dctx.getDelegator();
        //LocalDispatcher dispatcher = dctx.getDispatcher();
        try {
            return ServiceUtil.returnSuccess("File event registered.");
        }
        catch(Exception e) {
            final String errorMsg = "Exception triggering File Event";
            Debug.logError(e, errorMsg, module);
            return ServiceUtil.returnError(errorMsg + ": " + e.getMessage());
        }
    }
    
    
    /**
     * Clears the system cache for location
     * 
     * @param dctx The DispatchContext that this service is operating in
     * @param context Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> clearFileCaches(DispatchContext dctx, Map<String, ?> context) {
        try {
            if(UtilMisc.booleanValue(UtilProperties.getPropertyValue("cache", "cache.fileupdate.enable","true"), true)){
                String cacheName = (String) context.get("cacheName");
                String fileType = (String) context.get("fileType");
                String fileLocation = (String) context.get("fileLocation");
                UtilCache.clearCache(cacheName);            
                return ServiceUtil.returnSuccess("Cache cleared for "+cacheName);
            }else{
                return ServiceUtil.returnSuccess("Cache-Lock set. Cache not cleared");
            }
        }
        catch(Exception e) {
            final String errorMsg = "Exception triggering File Event";
            Debug.logError(e, errorMsg, module);
            return ServiceUtil.returnError(errorMsg + ": " + e.getMessage());
        }
    }
    
    /**
     * Render Partial Screen 
     * 
     * @param dctx The DispatchContext that this service is operating in
     * @param context Map containing the input parameters
     * @return Map with the result of the service, the output parameters
     */
    public static Map<String, Object> renderPartialScreen(DispatchContext dctx, Map<String, ?> rServiceContext) {
        Map<String, Object> responseMap = ServiceUtil.returnSuccess();
        try {
            Map<String, Object> serviceContext = UtilMisc.makeMapWritable(rServiceContext);
            LocalDispatcher dispatcher = dctx.getDispatcher();
            String webSiteId = (String) serviceContext.remove("webSiteId");
            String resource = (String) serviceContext.remove("resource");
            String screenName = (String) serviceContext.remove("screenName");
            
            Locale locale = (Locale) serviceContext.get("locale");
            Map<String, Object> bodyParameters = UtilGenerics.checkMap(serviceContext.remove("bodyParameters"));
            if (bodyParameters == null) {
                bodyParameters = MapStack.create();
            }
            if (!bodyParameters.containsKey("locale")) {
                bodyParameters.put("locale", locale);
            } else {
                locale = (Locale) bodyParameters.get("locale");
            }
            String partyId = (String) serviceContext.get("partyId");
            if (partyId == null) {
                partyId = (String) bodyParameters.get("partyId");
            }
            String orderId = (String) bodyParameters.get("orderId");
            String custRequestId = (String) bodyParameters.get("custRequestId");
            
            bodyParameters.put("communicationEventId", serviceContext.get("communicationEventId"));
            NotificationServices.setBaseUrl(dctx.getDelegator(), webSiteId, bodyParameters);
            String contentType = (String) serviceContext.remove("contentType");

            StringWriter bodyWriter = new StringWriter();

            MapStack<String> screenContext = MapStack.create();
            screenContext.put("locale", locale);

            // Scipio new
            Delegator delegator = dctx.getDelegator();
            try {
                MapStack<String> context = screenContext;
                context.put("locale", locale);

                // get the screen renderer; or create a new one
                ScreenRenderer screens = (ScreenRenderer) context.get("screens");
                if (screens == null) {
                 // TODO: replace "screen" to support dynamic rendering of different output
                    ScreenStringRenderer screenStringRenderer = new MacroScreenRenderer(EntityUtilProperties.getPropertyValue("widget", "screen.name", delegator), EntityUtilProperties.getPropertyValue("widget", "screen.screenrenderer", delegator));
                    screens = ScreenRenderer.makeWithEnvAwareFetching(bodyWriter, context, screenStringRenderer);
                    screens.getContext().put("screens", screens);
                }
                
                // render the screen
                ModelScreen modelScreen = null;
                ScreenStringRenderer renderer = screens.getScreenStringRenderer();
                screens.populateContextForService(dctx, bodyParameters);
                screenContext.putAll(bodyParameters);


                if (resource != null) {
                        String html = screens.renderScoped(resource, screenName, true, true);
                        responseMap.put("html", html);
                }
            } catch (GeneralException e) {
                Debug.logError(e, "Error rendering screen: " + e.toString(), module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resource, "CommonEmailSendRenderingScreenEmailError", UtilMisc.toMap("errorString", e.toString()), locale));
            } catch (IOException e) {
                Debug.logError(e, "Error rendering screen: " + e.toString(), module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resource, "CommonEmailSendRenderingScreenEmailError", UtilMisc.toMap("errorString", e.toString()), locale));
            } catch (SAXException e) {
                Debug.logError(e, "Error rendering screen: " + e.toString(), module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resource, "CommonEmailSendRenderingScreenEmailError", UtilMisc.toMap("errorString", e.toString()), locale));
            } catch (ParserConfigurationException e) {
                Debug.logError(e, "Error rendering screen: " + e.toString(), module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resource, "CommonEmailSendRenderingScreenEmailError", UtilMisc.toMap("errorString", e.toString()), locale));
            } catch (TemplateException e) {
                Debug.logError(e, "Error rendering screen: " + e.toString(), module);
                return ServiceUtil.returnError(UtilProperties.getMessage(resource, "CommonEmailSendRenderingScreenEmailError", UtilMisc.toMap("errorString", e.toString()), locale));
            }
        }
        catch(Exception e) {
            final String errorMsg = "Exception triggering File Event";
            Debug.logError(e, errorMsg, module);
            return ServiceUtil.returnError(errorMsg + ": " + e.getMessage());
        }
        
        return responseMap;
    }

}

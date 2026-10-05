/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
 */
package org.ofbiz.widget.model;

import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.xml.parsers.ParserConfigurationException;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.ce.base.component.ComponentReflectRegistry;
import com.ilscipio.scipio.widget.def.screen.ScreenAnnotationReader;
import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.widget.model.ScreenFallback.ScreenFallbackSettings;
import org.ofbiz.widget.renderer.ScreenStringRenderer;
import org.w3c.dom.Document;
import org.xml.sax.SAXException;


/**
 * Widget Library - Screen factory class
 * <p>
 * SCIPIO: now also as instance
 */
@SuppressWarnings("serial")
public class ScreenFactory extends WidgetFactory {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // SCIPIO: 2016-10-30: Instead of Map<String, ModelScreen>, we now have a dedicated ModelScreens model.
    // it maintains backward-compat by implementing Map, so all old code should still work.
    public static final UtilCache<String, ModelScreens> screenLocationCache = UtilCache.createUtilCache("widget.screen.locationResource", 0, 0, false);
    public static final UtilCache<String, ModelScreens> screenWebappCache = UtilCache.createUtilCache("widget.screen.webappResource", 0, 0, false);

    // SCIPIO: 4.0.0: Annotation-based screen cache - key is "class://fully.qualified.ClassName"
    private static final String CLASS_LOCATION_PREFIX = "class://";
    private static volatile Map<String, ModelScreen> annotationScreenCache = null;
    // SCIPIO: 4.0.0: screens grouped by class resource ("class://Outer" and "class://Outer$Inner") -> name -> screen,
    // so class:// lookups resolve the definition from the same class file instead of a same-named screen elsewhere
    private static volatile Map<String, Map<String, ModelScreen>> annotationScreensByClass = null;
    // SCIPIO: 4.0.0: Location alias registry - maps component:// URLs to screen maps
    private static volatile Map<String, Map<String, ModelScreen>> screenLocationAliases = null;
    private static volatile boolean annotationScreensLoaded = false;
    // SCIPIO: 4.0.0: set on the thread that runs loadAnnotationScreens, to break reentrant loads (see there)
    private static final ThreadLocal<Boolean> annotationLoadInProgress = new ThreadLocal<>();

    public static ScreenFactory getScreenFactory() { // SCIPIO: new
        return screenFactory;
    }

    /**
     * SCIPIO: 4.0.0: Checks if a resource location refers to an annotation-based screen class.
     */
    public static boolean isClassLocation(String resourceName) {
        return resourceName != null && resourceName.startsWith(CLASS_LOCATION_PREFIX);
    }

    /**
     * SCIPIO: 4.0.0: Gets the class name from a class:// location.
     */
    public static String getClassNameFromLocation(String resourceName) {
        if (!isClassLocation(resourceName)) {
            throw new IllegalArgumentException("Not a class location: " + resourceName);
        }
        return resourceName.substring(CLASS_LOCATION_PREFIX.length());
    }

    /**
     * SCIPIO: 4.0.0: Loads all annotation-based screens from all components.
     */
    private static void loadAnnotationScreens() {
        if (annotationScreensLoaded) {
            return;
        }
        synchronized (ScreenFactory.class) {
            if (annotationScreensLoaded) {
                return;
            }
            // SCIPIO: 4.0.0: an annotation screen class asks for its folder's CommonScreens.xml settings while it
            // is being loaded (ModelScreenGroup -> getScreensFromLocation -> here); the monitor is reentrant, so
            // without this guard the load would restart inside itself. The partial alias map serves that lookup.
            if (Boolean.TRUE.equals(annotationLoadInProgress.get())) {
                return;
            }
            annotationLoadInProgress.set(Boolean.TRUE);
            try {
                loadAnnotationScreensCore();
            } finally {
                annotationLoadInProgress.remove();
            }
        }
    }

    /**
     * SCIPIO: 4.0.0: True while this thread runs the annotation screen load (see {@link #loadAnnotationScreens}).
     */
    public static boolean isAnnotationLoadInProgress() {
        return Boolean.TRUE.equals(annotationLoadInProgress.get());
    }

    private static void loadAnnotationScreensCore() {
        {
            Map<String, ModelScreen> screens = new ConcurrentHashMap<>();
            Map<String, Map<String, ModelScreen>> locationAliases = new ConcurrentHashMap<>();
            annotationScreenCache = screens;
            Map<String, Map<String, ModelScreen>> byClass = new ConcurrentHashMap<>();
            annotationScreensByClass = byClass;
            screenLocationAliases = locationAliases;
            // SCIPIO: 4.0.0: Tracks whether the load was aborted early by an uncaught Throwable, so we
            // never leave annotationScreensLoaded=true silently pointing at a partial component scan.
            boolean partialLoad = false;
            try {
                for (ComponentReflectInfo cri : ComponentReflectRegistry.getReflectInfos()) {
                    try {
                        ScreenAnnotationReader reader = new ScreenAnnotationReader(cri);
                        // Pass locationAliases map to collect location aliases
                        Map<String, ModelScreen> componentScreens = reader.getModelScreens(locationAliases);
                        for (Map.Entry<String, ModelScreen> entry : componentScreens.entrySet()) {
                            String screenName = entry.getKey();
                            ModelScreen screen = entry.getValue();
                            if (screens.containsKey(screenName)) {
                                Debug.logWarning("Annotation screen [" + screenName +
                                        "] is defined more than once, most recent will over-write previous definition(s)", module);
                            }
                            screens.put(screenName, screen);
                            String src = screen.getSourceLocation();
                            if (src != null && src.startsWith(CLASS_LOCATION_PREFIX)) {
                                byClass.computeIfAbsent(src, k -> new ConcurrentHashMap<>()).put(screenName, screen);
                                int dollar = src.indexOf('$');
                                if (dollar > 0) {
                                    byClass.computeIfAbsent(src.substring(0, dollar), k -> new ConcurrentHashMap<>()).put(screenName, screen);
                                }
                            }
                        }
                    } catch (Throwable t) { // SCIPIO: 4.0.0: was catch (Exception) - an Error must not
                        // silently abort the whole component loop
                        Debug.logError(t, "Error loading annotation screens from component [" +
                                cri.getComponent().getGlobalName() + "]: " + t, module);
                    }
                }
                Debug.logInfo("Loaded [" + screens.size() + "] annotation-based screens with [" +
                        locationAliases.size() + "] location aliases", module);
                // Log all location aliases for debugging
                for (Map.Entry<String, Map<String, ModelScreen>> locEntry : locationAliases.entrySet()) {
                    Debug.logInfo("Screen location alias [" + locEntry.getKey() + "] has [" +
                            locEntry.getValue().size() + "] screens: " + locEntry.getValue().keySet(), module);
                }
            } catch (Throwable t) { // SCIPIO: 4.0.0: was catch (Exception) - must catch Throwable so a
                // fatal Error is logged instead of escaping silently and leaving annotationScreensLoaded=false
                // forever (which would force a full component rescan on every single request)
                partialLoad = true;
                Debug.logError(t, "FATAL: annotation screen loading aborted - " + t, module);
            } finally {
                // SCIPIO: 4.0.0: Set the loaded flag in finally so a partial load (due to a caught
                // Throwable above) is never retried on every subsequent request.
                annotationScreensLoaded = true;
                if (partialLoad) {
                    Debug.logError("FATAL: annotation screen loading only partially completed - " +
                            "[" + screens.size() + "] screens collected before abort; " +
                            "some components were NOT scanned; see FATAL stack trace above", module);
                }
            }
        }
    }

    /**
     * SCIPIO: 4.0.0: Gets an annotation-based screen by name.
     */
    public static ModelScreen getAnnotationScreen(String screenName) {
        loadAnnotationScreens();
        return annotationScreenCache != null ? annotationScreenCache.get(screenName) : null;
    }

    /**
     * SCIPIO: 4.0.0: Gets all annotation-based screens.
     */
    /**
     * SCIPIO: 4.0.0: Class-aware annotation screen lookup: same class (or its outer class) first, then any screen by name.
     */
    public static ModelScreen getAnnotationScreen(String classResource, String screenName) {
        loadAnnotationScreens();
        Map<String, Map<String, ModelScreen>> byClass = annotationScreensByClass;
        if (byClass != null && classResource != null) {
            Map<String, ModelScreen> m = byClass.get(classResource);
            ModelScreen screen = (m != null) ? m.get(screenName) : null;
            if (screen == null) {
                int dollar = classResource.indexOf('$');
                if (dollar > 0) {
                    m = byClass.get(classResource.substring(0, dollar));
                    screen = (m != null) ? m.get(screenName) : null;
                }
            }
            if (screen != null) {
                return screen;
            }
        }
        return getAnnotationScreen(screenName);
    }

    /**
     * SCIPIO: 4.0.0: All annotation screens by name, with the screens of the given class (and its outer class) taking
     * precedence over same-named screens from other classes.
     */
    public static Map<String, ModelScreen> getAnnotationScreens(String classResource) {
        Map<String, ModelScreen> all = new HashMap<>(getAnnotationScreens());
        Map<String, Map<String, ModelScreen>> byClass = annotationScreensByClass;
        if (byClass != null && classResource != null) {
            int dollar = classResource.indexOf('$');
            if (dollar > 0) {
                Map<String, ModelScreen> outer = byClass.get(classResource.substring(0, dollar));
                if (outer != null) all.putAll(outer);
            }
            Map<String, ModelScreen> own = byClass.get(classResource);
            if (own != null) all.putAll(own);
        }
        return all;
    }

    public static Map<String, ModelScreen> getAnnotationScreens() {
        loadAnnotationScreens();
        return annotationScreenCache != null ? annotationScreenCache : new HashMap<>();
    }

    /**
     * SCIPIO: 4.0.0: Gets a screen from a location alias.
     *
     * <p>This allows component:// style URLs to resolve to annotation-based screens
     * when the @Screen annotation specifies a location or locations attribute.</p>
     *
     * @param location The component:// style location (e.g., "component://setup/widget/SetupScreens.xml")
     * @param screenName The screen name within that location
     * @return The ModelScreen, or null if no alias is registered for this location/name
     */
    public static ModelScreen getScreenFromLocationAlias(String location, String screenName) {
        loadAnnotationScreens();
        if (screenLocationAliases == null) {
            return null;
        }
        Map<String, ModelScreen> screensAtLocation = screenLocationAliases.get(location);
        if (screensAtLocation == null) {
            return null;
        }
        return screensAtLocation.get(screenName);
    }

    /**
     * SCIPIO: 4.0.0: Checks if a location has any registered screen aliases.
     */
    public static boolean hasLocationAlias(String location) {
        loadAnnotationScreens();
        return screenLocationAliases != null && screenLocationAliases.containsKey(location);
    }

    /**
     * SCIPIO: 4.0.0: Gets all annotation-based screens from a specific location.
     * Uses the location aliases map populated during annotation loading.
     */
    public static Map<String, ModelScreen> getAnnotationScreensByLocation(String location) {
        loadAnnotationScreens();
        if (screenLocationAliases == null) {
            return new HashMap<>();
        }
        Map<String, ModelScreen> screensAtLocation = screenLocationAliases.get(location);
        if (screensAtLocation == null) {
            return new HashMap<>();
        }
        return new HashMap<>(screensAtLocation);
    }

    public static boolean isCombinedName(String combinedName) {
        int numSignIndex = combinedName.lastIndexOf("#");
        if (numSignIndex == -1) {
            return false;
        }
        if (numSignIndex + 1 >= combinedName.length()) {
            return false;
        }
        return true;
    }

    public static String getResourceNameFromCombined(String combinedName) {
        // split out the name on the last "#"
        int numSignIndex = combinedName.lastIndexOf("#");
        if (numSignIndex == -1) {
            throw new IllegalArgumentException("Error in screen location/name: no \"#\" found to separate the location from the name; correct example: component://product/widget/catalog/ProductScreens.xml#EditProduct");
        }
        if (numSignIndex + 1 >= combinedName.length()) {
            throw new IllegalArgumentException("Error in screen location/name: the \"#\" was at the end with no screen name after it; correct example: component://product/widget/catalog/ProductScreens.xml#EditProduct");
        }
        String resourceName = combinedName.substring(0, numSignIndex);
        return resourceName;
    }

    public static String getScreenNameFromCombined(String combinedName) {
        // split out the name on the last "#"
        int numSignIndex = combinedName.lastIndexOf("#");
        if (numSignIndex == -1) {
            throw new IllegalArgumentException("Error in screen location/name: no \"#\" found to separate the location from the name; correct example: component://product/widget/catalog/ProductScreens.xml#EditProduct");
        }
        if (numSignIndex + 1 >= combinedName.length()) {
            throw new IllegalArgumentException("Error in screen location/name: the \"#\" was at the end with no screen name after it; correct example: component://product/widget/catalog/ProductScreens.xml#EditProduct");
        }
        String screenName = combinedName.substring(numSignIndex + 1);
        return screenName;
    }

    public static ModelScreen getScreenFromLocation(String combinedName)
            throws IOException, SAXException, ParserConfigurationException {
        String resourceName = getResourceNameFromCombined(combinedName);
        String screenName = getScreenNameFromCombined(combinedName);
        return getScreenFromLocation(resourceName, screenName);
    }

    public static ModelScreen getScreenFromLocation(String resourceName, String screenName)
            throws IOException, SAXException, ParserConfigurationException {
        // SCIPIO: 4.0.0: Handle annotation-based screens with class:// prefix
        if (isClassLocation(resourceName)) {
            ModelScreen modelScreen = getAnnotationScreen(resourceName, screenName); // SCIPIO: 4.0.0: class-aware
            if (modelScreen == null) {
                throw new IllegalArgumentException("Could not find annotation-based screen with name [" + screenName + "] from class [" + resourceName + "]");
            }
            return modelScreen;
        }

        // SCIPIO: 4.0.0: Check location aliases first (allows annotation-based screens to replace XML)
        ModelScreen aliasScreen = getScreenFromLocationAlias(resourceName, screenName);
        if (aliasScreen != null) {
            return aliasScreen;
        }

        Map<String, ModelScreen> modelScreenMap = getScreensFromLocation(resourceName);
        ModelScreen modelScreen = modelScreenMap.get(screenName);
        if (modelScreen == null) {
            // SCIPIO: 4.0.0: Final fallback - try to find screen by name in all annotation screens
            // This handles cases where XML was deleted and the alias lookup fails due to loading order
            modelScreen = getAnnotationScreen(screenName);
            if (modelScreen != null) {
                Debug.logWarning("Screen [" + screenName + "] not found at [" + resourceName +
                    "] but found as annotation screen; consider updating the reference to use class:// location", module);
                return modelScreen;
            }
            throw new IllegalArgumentException("Could not find screen with name [" + screenName + "] in class resource [" + resourceName + "]");
        }
        return modelScreen;
    }

    /**
     * SCIPIO: Returns the specified screen, or null if the name does not exist in the given location.
     * <p>
     * NOTE: The resource must exist, however, otherwise IllegalArgumentException is thrown (TODO: REVIEW: is this really desirable?).
     */
    public static ModelScreen getScreenFromLocationOrNull(String combinedName)
            throws IOException, SAXException, ParserConfigurationException {
        String resourceName = getResourceNameFromCombined(combinedName);
        String screenName = getScreenNameFromCombined(combinedName);
        return getScreenFromLocationOrNull(resourceName, screenName);
    }

    /**
     * SCIPIO: Returns the specified screen, or null if the name does not exist in the given location.
     * <p>
     * NOTE: The resource must exist, however, otherwise IllegalArgumentException is thrown (TODO: REVIEW: is this really desirable?).
     */
    public static ModelScreen getScreenFromLocationOrNull(String resourceName, String screenName)
            throws IOException, SAXException, ParserConfigurationException {
        // SCIPIO: 4.0.0: Handle annotation-based screens with class:// prefix
        if (isClassLocation(resourceName)) {
            return getAnnotationScreen(resourceName, screenName); // SCIPIO: 4.0.0: class-aware
        }

        // SCIPIO: 4.0.0: Check location aliases first (allows annotation-based screens to replace XML)
        ModelScreen aliasScreen = getScreenFromLocationAlias(resourceName, screenName);
        if (aliasScreen != null) {
            return aliasScreen;
        }

        Map<String, ModelScreen> modelScreenMap = getScreensFromLocation(resourceName);
        return modelScreenMap.get(screenName);
    }

    // SCIPIO: new: ModelScreens return value
    public static ModelScreens getScreensFromLocation(String resourceName)
            throws IOException, SAXException, ParserConfigurationException {
        // SCIPIO: 4.0.0: Handle annotation-based screens with class:// prefix
        if (isClassLocation(resourceName)) {
            return new ModelScreens(getAnnotationScreens(resourceName), resourceName); // SCIPIO: 4.0.0: class-aware
        }
        ModelScreens modelScreenMap = screenLocationCache.get(resourceName);
        if (modelScreenMap == null) {
            // SCIPIO: refactored
            synchronized (ScreenFactory.class) {
                modelScreenMap = screenLocationCache.get(resourceName);
                if (modelScreenMap == null) {
                    long startTime = System.currentTimeMillis();
                    // SCIPIO: 4.0.0: Use unified WidgetLocationResolver for file existence checking and fallback
                    URL screenFileUrl = WidgetLocationResolver.resolveWidgetLocation(resourceName, "screen");

                    // SCIPIO: 4.0.0: If no XML file, try annotation-based screens by location alias
                    if (screenFileUrl == null) {
                        Map<String, ModelScreen> annotationScreensByLocation = getAnnotationScreensByLocation(resourceName);
                        if (!annotationScreensByLocation.isEmpty()) {
                            Debug.logInfo("Screen location [" + resourceName + "] not found as file, using [" +
                                    annotationScreensByLocation.size() + "] annotation-based screen(s)", module);
                            // SCIPIO: 4.0.0: build through the XML constructor with an empty root so the folder's
                            // CommonScreens.xml auto-include-settings (render-init, decorator fallback) apply as they
                            // did for the XML file this alias replaces (was: settings-less ModelScreens).
                            modelScreenMap = new ModelScreens(UtilXml.makeEmptyXmlDocument("screens").getDocumentElement(),
                                    resourceName, true, annotationScreensByLocation);
                            if (!isAnnotationLoadInProgress()) { // SCIPIO: 4.0.0: a partial alias map must not be cached
                                screenLocationCache.put(resourceName, modelScreenMap);
                            }
                            return modelScreenMap;
                        }
                        throw new IllegalArgumentException("Could not resolve screen file location [" + resourceName + "]");
                    }
                    Document screenFileDoc = UtilXml.readXmlDocument(screenFileUrl, true, true);
                    if (screenFileDoc == null) { // SCIPIO
                        throw new IllegalArgumentException("Could not read screen file at location [" + resourceName + "]");
                    }
                    // SCIPIO: New: Save original location as user data in Document
                    WidgetDocumentInfo.retrieveAlways(screenFileDoc).setResourceLocation(resourceName);
                    modelScreenMap = readScreenDocument(screenFileDoc, resourceName);
                    // SCIPIO: 4.0.0: settings-only stub XML (screens migrated to annotations): keep the XML screen-settings
                    // and merge the annotation screens registered at this location (was: settings silently dropped).
                    if (modelScreenMap.isEmpty()) {
                        if (isAnnotationLoadInProgress()) {
                            // SCIPIO: 4.0.0: an annotation class being loaded asked for this settings stub; the alias
                            // map is still partial, so serve the settings-only model now, uncached and unmerged.
                            return modelScreenMap;
                        }
                        Map<String, ModelScreen> annotationScreensByLocation = getAnnotationScreensByLocation(resourceName);
                        if (!annotationScreensByLocation.isEmpty()) {
                            Debug.logInfo("Screen file [" + resourceName + "] has no screen definitions, merging [" +
                                    annotationScreensByLocation.size() + "] annotation-based screen(s) with its settings", module);
                            modelScreenMap = new ModelScreens(screenFileDoc.getDocumentElement(), resourceName, true, annotationScreensByLocation);
                        }
                    }
                    screenLocationCache.put(resourceName, modelScreenMap);
                    double totalSeconds = (System.currentTimeMillis() - startTime)/1000.0;
                    Debug.logInfo("Got " + modelScreenMap.size() + " screens in " + totalSeconds + "s from: " + screenFileUrl.toExternalForm(), module);
                }
            }
        }
        // SCIPIO: 4.0.0: When the XML file has no screens (e.g., only screen-settings after annotation migration),
        // check for annotation-based screens registered at this location and create a new ModelScreens
        // that inherits settings from the XML but gets screen definitions from annotations.
        if (modelScreenMap.isEmpty()) {
            Map<String, ModelScreen> annotationScreensByLocation = getAnnotationScreensByLocation(resourceName);
            if (!annotationScreensByLocation.isEmpty()) {
                Debug.logInfo("Screen file [" + resourceName + "] has no screen definitions, using [" +
                        annotationScreensByLocation.size() + "] annotation-based screen(s)", module);
                modelScreenMap = new ModelScreens(annotationScreensByLocation, resourceName);
                screenLocationCache.put(resourceName, modelScreenMap);
                return modelScreenMap;
            }
            throw new IllegalArgumentException("Could not find screen file with name [" + resourceName + "]");
        }
        return modelScreenMap;
    }

    public static ModelScreen getScreenFromWebappContext(String resourceName, String screenName, HttpServletRequest request)
            throws IOException, SAXException, ParserConfigurationException {
        String webappName = UtilHttp.getApplicationName(request);
        String cacheKey = webappName + "::" + resourceName;
        ModelScreens modelScreenMap = screenWebappCache.get(cacheKey); // SCIPIO: new: ModelScreens
        if (modelScreenMap == null) {
            // SCIPIO: refactored
            synchronized (ScreenFactory.class) {
                modelScreenMap = screenWebappCache.get(cacheKey);
                if (modelScreenMap == null) {
                    ServletContext servletContext = request.getServletContext(); // SCIPIO: get context using servlet API 3.0
                    URL screenFileUrl = servletContext.getResource(resourceName);
                    if (screenFileUrl == null) {
                        throw new IllegalArgumentException("Could not resolve screen file location [" + resourceName + "]");
                    }
                    Document screenFileDoc = UtilXml.readXmlDocument(screenFileUrl, true, true);
                    if (screenFileDoc == null) {
                        throw new IllegalArgumentException("Could not read screen file at location [" + resourceName + "] in the webapp [" + webappName + "]");
                    }
                    // SCIPIO: New: Save original location as user data in Document
                    WidgetDocumentInfo.retrieveAlways(screenFileDoc).setResourceLocation(resourceName);
                    modelScreenMap = readScreenDocument(screenFileDoc, resourceName);
                    screenWebappCache.put(cacheKey, modelScreenMap);
                }
            }
        }
        ModelScreen modelScreen = modelScreenMap.get(screenName);
        if (modelScreen == null) {
            throw new IllegalArgumentException("Could not find screen with name [" + screenName + "] in webapp resource [" + resourceName + "] in the webapp [" + webappName + "]");
        }
        return modelScreen;
    }

    // SCIPIO: new: ModelScreens
    public static ModelScreens readScreenDocument(Document screenFileDoc, String sourceLocation) {
        if (screenFileDoc != null) {
            // SCIPIO: all the old code here delegated to ModelScreens
            return new ModelScreens(screenFileDoc.getDocumentElement(), sourceLocation);
        }
        return new ModelScreens();
    }

    /**
     * Renders referenced screen, with fallback support, optionally taking care of checking
     * the screen name for combined loc#name format.
     * <p>
     * SCIPIO: modified to support fallback and make name parsing optional.
     * NOTE: fallbackSettings methods isEnabled and getFallbackIfEmpty are checked BEFORE resolution (getResolved).
     * Default for fallbackIfEmpty is currently FALSE.
     */
    public static void renderReferencedScreen(String name, String location, ModelScreenWidget parentWidget, Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer,
            boolean parseRefs, ScreenFallbackSettings fallbackSettings) throws GeneralException, IOException {
        if (parseRefs) { // SCIPIO: this is optional; caller may also handle
            // check to see if the name is a composite name separated by a #, if so split it up and get it by the full loc#name
            if (ScreenFactory.isCombinedName(name)) {
                String combinedName = name;
                location = ScreenFactory.getResourceNameFromCombined(combinedName);
                name = ScreenFactory.getScreenNameFromCombined(combinedName);
            }
        }

        ModelScreen modelScreen = null;
        if (UtilValidate.isNotEmpty(location)) {
            try {
                // SCIPIO: fallback possible, so support null
                modelScreen = ScreenFactory.getScreenFromLocationOrNull(location, name);
                if (modelScreen == null) {
                    if (fallbackSettings != null && fallbackSettings.isEnabled()) {
                        fallbackSettings = fallbackSettings.getResolved(); // optimization
                        // SCIPIO: DEV NOTE: DUPLICATED below; keep in sync
                        String fallbackName = fallbackSettings.getName();
                        String fallbackLocation = fallbackSettings.getLocation();
                        if (parseRefs) { // SCIPIO: this is optional; caller may also handle
                            // SCIPIO: parsing for fallbacks
                            if (fallbackName != null && ScreenFactory.isCombinedName(fallbackName)) {
                                String combinedName = fallbackName;
                                fallbackLocation = ScreenFactory.getResourceNameFromCombined(combinedName);
                                fallbackName = ScreenFactory.getScreenNameFromCombined(combinedName);
                            }
                        }
                        if (UtilValidate.isEmpty(fallbackName)) {
                            fallbackName = name;
                        }

                        if (UtilValidate.isNotEmpty(fallbackLocation)) {
                            try {
                                modelScreen = ScreenFactory.getScreenFromLocation(fallbackLocation, fallbackName);
                            } catch (Exception e) { // SCIPIO: Changed (IOException | SAXException | ParserConfigurationException e) to Exception (for improved breadcrumb trail)
                                String errMsg = "Error rendering included (fallback) screen named [" + fallbackName + "] at location [" + fallbackLocation + "]";
                                //Debug.logError(e, errMsg, module); // SCIPIO: Redundant logging
                                throw new WidgetRenderException(errMsg + ": " + e, e, null, context); // SCIPIO: Changed RuntimeException to WidgetRenderException
                            }
                        } else {
                            modelScreen = parentWidget.getModelScreen().getModelScreenMap().get(fallbackName);
                            // SCIPIO: 4.0.0: Annotation-based same-file fallback
                            if (modelScreen == null && isClassLocation(parentWidget.getModelScreen().getSourceLocation())) {
                                modelScreen = getAnnotationScreen(parentWidget.getModelScreen().getSourceLocation(), fallbackName); // SCIPIO: 4.0.0: same class first
                            }
                            if (modelScreen == null) {
                                throw new IllegalArgumentException("Could not find (fallback) screen with name [" + fallbackName + "] in the same file as the screen with name [" + parentWidget.getModelScreen().getName() + "]");
                            }
                        }
                    } else {
                        throw new IllegalArgumentException("Could not find screen with name [" + name + "] in class resource [" + location + "]");
                    }
                }
            } catch (Exception e) { // SCIPIO: Changed (IOException | SAXException | ParserConfigurationException e) to Exception (for improved breadcrumb trail)
                String errMsg = "Error rendering included screen named [" + name + "] at location [" + location + "]";
                //Debug.logError(e, errMsg, module); // SCIPIO: Redundant logging
                throw new WidgetRenderException(errMsg + ": " + e, e, null, context); // SCIPIO: Changed RuntimeException to WidgetRenderException
            }
        } else {
            if (fallbackSettings != null && fallbackSettings.isEnabledForEmptyLocation()) { // SCIPIO: fallback if empty
                // SCIPIO: DEV NOTE: DUPLICATED above; keep in sync
                fallbackSettings = fallbackSettings.getResolved(); // optimization
                String fallbackName = fallbackSettings.getName();
                String fallbackLocation = fallbackSettings.getLocation();
                if (parseRefs) { // SCIPIO: this is optional; caller may also handle
                    // SCIPIO: parsing for fallbacks
                    if (fallbackName != null && ScreenFactory.isCombinedName(fallbackName)) {
                        String combinedName = fallbackName;
                        fallbackLocation = ScreenFactory.getResourceNameFromCombined(combinedName);
                        fallbackName = ScreenFactory.getScreenNameFromCombined(combinedName);
                    }
                }
                if (UtilValidate.isEmpty(fallbackName)) {
                    fallbackName = name;
                }

                if (UtilValidate.isNotEmpty(fallbackLocation)) {
                    try {
                        modelScreen = ScreenFactory.getScreenFromLocation(fallbackLocation, fallbackName);
                    } catch (Exception e) { // SCIPIO: Changed (IOException | SAXException | ParserConfigurationException e) to Exception (for improved breadcrumb trail)
                        String errMsg = "Error rendering included (fallback) screen named [" + fallbackName + "] at location [" + fallbackLocation + "]";
                        //Debug.logError(e, errMsg, module); // SCIPIO: Redundant logging
                        throw new WidgetRenderException(errMsg + ": " + e, e, null, context); // SCIPIO: Changed RuntimeException to WidgetRenderException
                    }
                } else {
                    modelScreen = parentWidget.getModelScreen().getModelScreenMap().get(fallbackName);
                    // SCIPIO: 4.0.0: Annotation-based same-file fallback
                    if (modelScreen == null && isClassLocation(parentWidget.getModelScreen().getSourceLocation())) {
                        modelScreen = getAnnotationScreen(parentWidget.getModelScreen().getSourceLocation(), fallbackName); // SCIPIO: 4.0.0: same class first
                    }
                    if (modelScreen == null) {
                        throw new IllegalArgumentException("Could not find (fallback) screen with name [" + fallbackName + "] in the same file as the screen with name [" + parentWidget.getModelScreen().getName() + "]");
                    }
                }
            } else {
                modelScreen = parentWidget.getModelScreen().getModelScreenMap().get(name);
                // SCIPIO: 4.0.0: For annotation-based screens, same-file lookup may fail because each annotation
                // produces its own isolated document. Fall back to annotation screen cache.
                // SCIPIO: 4.0.0: For annotation-based screens, same-file lookup may fail because each annotation
                // produces its own isolated document. Fall back to annotation screen cache.
                if (modelScreen == null && isClassLocation(parentWidget.getModelScreen().getSourceLocation())) {
                    modelScreen = getAnnotationScreen(parentWidget.getModelScreen().getSourceLocation(), name); // SCIPIO: 4.0.0: same class first
                }
                if (modelScreen == null) {
                    throw new IllegalArgumentException("Could not find screen with name [" + name + "] in the same file as the screen with name [" + parentWidget.getModelScreen().getName() + "]");
                }
            }
        }
        modelScreen.renderScreenString(writer, context, screenStringRenderer);
    }

    /**
     * Renders referenced screen, taking care of checking the screen name for combined loc#name format.
     * <p>
     * SCIPIO: delegating.
     */
    public static void renderReferencedScreen(String name, String location, ModelScreenWidget parentWidget, Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
        renderReferencedScreen(name, location, parentWidget, writer, context, screenStringRenderer, true, null);
    }

    @Override
    public ModelScreen getWidgetFromLocation(ModelLocation modelLoc) throws IOException, IllegalArgumentException { // SCIPIO
        try {
            return getScreenFromLocation(modelLoc.getResource(), modelLoc.getName());
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }

    @Override
    public ModelScreen getWidgetFromLocationOrNull(ModelLocation modelLoc) throws IOException { // SCIPIO
        try {
            return getScreenFromLocationOrNull(modelLoc.getResource(), modelLoc.getName());
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }
}

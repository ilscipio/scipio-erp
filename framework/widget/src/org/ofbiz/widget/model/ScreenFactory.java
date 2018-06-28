/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package org.ofbiz.widget.model;

import java.io.IOException;
import java.net.URL;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.xml.parsers.ParserConfigurationException;

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

    // SCIPIO: NOTE: 2016-10-30: instead of Map<String, ModelScreen>, we now have a dedicated ModelScreens model.
    // it maintains backward-compat by implementing Map, so all old code should still work.
    
    public static final UtilCache<String, ModelScreens> screenLocationCache = UtilCache.createUtilCache("widget.screen.locationResource", 0, 0, false);
    public static final UtilCache<String, ModelScreens> screenWebappCache = UtilCache.createUtilCache("widget.screen.webappResource", 0, 0, false);
    
    public static ScreenFactory getScreenFactory() { // SCIPIO: new
        return screenFactory;
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
            throw new IllegalArgumentException("Error in screen location/name: no \"#\" found to separate the location from the name; correct example: component://product/screen/product/ProductScreens.xml#EditProduct");
        }
        if (numSignIndex + 1 >= combinedName.length()) {
            throw new IllegalArgumentException("Error in screen location/name: the \"#\" was at the end with no screen name after it; correct example: component://product/screen/product/ProductScreens.xml#EditProduct");
        }
        String resourceName = combinedName.substring(0, numSignIndex);
        return resourceName;
    }

    public static String getScreenNameFromCombined(String combinedName) {
        // split out the name on the last "#"
        int numSignIndex = combinedName.lastIndexOf("#");
        if (numSignIndex == -1) {
            throw new IllegalArgumentException("Error in screen location/name: no \"#\" found to separate the location from the name; correct example: component://product/screen/product/ProductScreens.xml#EditProduct");
        }
        if (numSignIndex + 1 >= combinedName.length()) {
            throw new IllegalArgumentException("Error in screen location/name: the \"#\" was at the end with no screen name after it; correct example: component://product/screen/product/ProductScreens.xml#EditProduct");
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
        Map<String, ModelScreen> modelScreenMap = getScreensFromLocation(resourceName);
        ModelScreen modelScreen = modelScreenMap.get(screenName);
        if (modelScreen == null) {
            throw new IllegalArgumentException("Could not find screen with name [" + screenName + "] in class resource [" + resourceName + "]");
        }
        return modelScreen;
    }
    
    /**
     * SCIPIO: Returns the specified screen, or null if the name does not exist in the given location.
     * <p>
     * NOTE: the resource must exist, however.
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
     * NOTE: the resource must exist, however.
     */
    public static ModelScreen getScreenFromLocationOrNull(String resourceName, String screenName)
            throws IOException, SAXException, ParserConfigurationException {
        Map<String, ModelScreen> modelScreenMap = getScreensFromLocation(resourceName);
        return modelScreenMap.get(screenName);
    }

    // SCIPIO: new: ModelScreens
    public static ModelScreens getScreensFromLocation(String resourceName)
            throws IOException, SAXException, ParserConfigurationException {
        ModelScreens modelScreenMap = screenLocationCache.get(resourceName);
        if (modelScreenMap == null) {
            synchronized (ScreenFactory.class) {
                modelScreenMap = screenLocationCache.get(resourceName);
                if (modelScreenMap == null) {
                    long startTime = System.currentTimeMillis();
                    URL screenFileUrl = null;
                    screenFileUrl = FlexibleLocation.resolveLocation(resourceName);
                    if (screenFileUrl == null) {
                        throw new IllegalArgumentException("Could not resolve location to URL: " + resourceName);
                    }
                    Document screenFileDoc = UtilXml.readXmlDocument(screenFileUrl, true, true);
                    // SCIPIO: New: Save original location as user data in Document
                    if (screenFileDoc != null) {
                        WidgetDocumentInfo.retrieveAlways(screenFileDoc).setResourceLocation(resourceName);
                    }
                    modelScreenMap = readScreenDocument(screenFileDoc, resourceName);
                    screenLocationCache.put(resourceName, modelScreenMap);
                    double totalSeconds = (System.currentTimeMillis() - startTime)/1000.0;
                    Debug.logInfo("Got " + modelScreenMap.size() + " screens in " + totalSeconds + "s from: " + screenFileUrl.toExternalForm(), module);
                }
            }
        }

        if (modelScreenMap.isEmpty()) {
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
            synchronized (ScreenFactory.class) {
                modelScreenMap = screenWebappCache.get(cacheKey);
                if (modelScreenMap == null) {
                    ServletContext servletContext = (ServletContext) request.getAttribute("servletContext");

                    URL screenFileUrl = servletContext.getResource(resourceName);
                    Document screenFileDoc = UtilXml.readXmlDocument(screenFileUrl, true, true);
                    // SCIPIO: New: Save original location as user data in Document
                    if (screenFileDoc != null) {
                        WidgetDocumentInfo.retrieveAlways(screenFileDoc).setResourceLocation(resourceName);
                    }
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
        } else {
            return new ModelScreens();
        }
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
                            } catch (IOException e) {
                                String errMsg = "Error rendering included (fallback) screen named [" + fallbackName + "] at location [" + fallbackLocation + "]: " + e.toString();
                                Debug.logError(e, errMsg, module);
                                throw new RuntimeException(errMsg);
                            } catch (SAXException e) {
                                String errMsg = "Error rendering included (fallback) screen named [" + fallbackName + "] at location [" + fallbackLocation + "]: " + e.toString();
                                Debug.logError(e, errMsg, module);
                                throw new RuntimeException(errMsg);
                            } catch (ParserConfigurationException e) {
                                String errMsg = "Error rendering included (fallback) screen named [" + fallbackName + "] at location [" + fallbackLocation + "]: " + e.toString();
                                Debug.logError(e, errMsg, module);
                                throw new RuntimeException(errMsg);
                            }
                        } else {
                            modelScreen = parentWidget.getModelScreen().getModelScreenMap().get(fallbackName);
                            if (modelScreen == null) {
                                throw new IllegalArgumentException("Could not find (fallback) screen with name [" + fallbackName + "] in the same file as the screen with name [" + parentWidget.getModelScreen().getName() + "]");
                            }
                        }
                    } else {
                        throw new IllegalArgumentException("Could not find screen with name [" + name + "] in class resource [" + location + "]");
                    }
                }
            } catch (IOException e) {
                String errMsg = "Error rendering included screen named [" + name + "] at location [" + location + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            } catch (SAXException e) {
                String errMsg = "Error rendering included screen named [" + name + "] at location [" + location + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            } catch (ParserConfigurationException e) {
                String errMsg = "Error rendering included screen named [" + name + "] at location [" + location + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
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
                    } catch (IOException e) {
                        String errMsg = "Error rendering included (fallback) screen named [" + fallbackName + "] at location [" + fallbackLocation + "]: " + e.toString();
                        Debug.logError(e, errMsg, module);
                        throw new RuntimeException(errMsg);
                    } catch (SAXException e) {
                        String errMsg = "Error rendering included (fallback) screen named [" + fallbackName + "] at location [" + fallbackLocation + "]: " + e.toString();
                        Debug.logError(e, errMsg, module);
                        throw new RuntimeException(errMsg);
                    } catch (ParserConfigurationException e) {
                        String errMsg = "Error rendering included (fallback) screen named [" + fallbackName + "] at location [" + fallbackLocation + "]: " + e.toString();
                        Debug.logError(e, errMsg, module);
                        throw new RuntimeException(errMsg);
                    }
                } else {
                    modelScreen = parentWidget.getModelScreen().getModelScreenMap().get(fallbackName);
                    if (modelScreen == null) {
                        throw new IllegalArgumentException("Could not find (fallback) screen with name [" + fallbackName + "] in the same file as the screen with name [" + parentWidget.getModelScreen().getName() + "]");
                    }
                }
            } else {
                modelScreen = parentWidget.getModelScreen().getModelScreenMap().get(name);
                if (modelScreen == null) {
                    throw new IllegalArgumentException("Could not find screen with name [" + name + "] in the same file as the screen with name [" + parentWidget.getModelScreen().getName() + "]");
                }
            }
        }
        //Debug.logInfo("parent(" + parentWidget + ") rendering(" + modelScreen + ")", module);
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
    public ModelScreen getWidgetFromLocation(ModelLocation modelLoc) throws IOException, IllegalArgumentException {
        try {
            return getScreenFromLocation(modelLoc.getResource(), modelLoc.getName());
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }

    @Override
    public ModelScreen getWidgetFromLocationOrNull(ModelLocation modelLoc) throws IOException {
        try {
            return getScreenFromLocationOrNull(modelLoc.getResource(), modelLoc.getName());
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }
}

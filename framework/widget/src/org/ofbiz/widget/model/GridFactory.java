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
import java.util.List;
import java.util.Map;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.location.FlexibleLocation;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.cache.UtilCache;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.model.ModelReader;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.widget.model.FormFactory.FormInitInfo;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;


/**
 * Widget Library - Grid factory class
 * <p>
 * SCIPIO: now also as instance
 */
@SuppressWarnings("serial")
public class GridFactory extends WidgetFactory {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    // SCIPIO: 2018-12-05: These caches are modified to hold whole file instead of individual ModelForm
    private static final UtilCache<String, Map<String, ModelGrid>> gridLocationCache = UtilCache.createUtilCache("widget.grid.locationResource", 0, 0, false);
    private static final UtilCache<String, Map<String, ModelGrid>> gridWebappCache = UtilCache.createUtilCache("widget.grid.webappResource", 0, 0, false);

    public static GridFactory getGridFactory() { // SCIPIO: new
        return gridFactory;
    }

    public static Map<String, ModelGrid> getGridsFromLocation(String resourceName, ModelReader entityModelReader, DispatchContext dispatchContext)
            throws IOException, SAXException, ParserConfigurationException {
        URL gridFileUrl = FlexibleLocation.resolveLocation(resourceName);
        Document gridFileDoc = UtilXml.readXmlDocument(gridFileUrl, true, true);
        // SCIPIO: New: Save original location as user data in Document
        if (gridFileDoc != null) {
            WidgetDocumentInfo.retrieveAlways(gridFileDoc).setResourceLocation(resourceName);
        }
        return readGridDocument(gridFileDoc, entityModelReader, dispatchContext, resourceName);
    }

    /**
     * Gets widget from location or exception.
     * <p>
     * SCIPIO: now delegating.
     */
    public static ModelGrid getGridFromLocation(String resourceName, String gridName, ModelReader entityModelReader, DispatchContext dispatchContext)
            throws IOException, SAXException, ParserConfigurationException {
        // SCIPIO: 4.0.0: Handle annotation-based grids with class:// prefix
        if (FormFactory.isClassLocation(resourceName)) {
            ModelForm modelForm = FormFactory.getAnnotationFormFromClass(resourceName, gridName); // SCIPIO: 4.0.0: class-exact
            if (modelForm instanceof ModelGrid) {
                return (ModelGrid) modelForm;
            }
            throw new IllegalArgumentException("Could not find annotation-based grid with name [" + gridName + "] from class [" + resourceName + "]");
        }
        // SCIPIO: 4.0.0: Check location aliases first (allows annotation-based grids to replace XML)
        ModelForm aliasForm = FormFactory.getFormFromLocationAlias(resourceName, gridName);
        if (aliasForm instanceof ModelGrid) {
            return (ModelGrid) aliasForm;
        }

        ModelGrid modelGrid = getGridFromLocationOrNull(resourceName, gridName, entityModelReader, dispatchContext);
        if (modelGrid == null) {
            // SCIPIO: 4.0.0: Final fallback - try to find grid by name in all annotation forms
            ModelForm modelForm = FormFactory.getAnnotationForm(gridName);
            if (modelForm instanceof ModelGrid) {
                Debug.logWarning("Grid [" + gridName + "] not found at [" + resourceName +
                    "] but found as annotation grid; consider updating the reference to use class:// location", module);
                return (ModelGrid) modelForm;
            }
            throw new IllegalArgumentException("Could not find grid with name [" + gridName + "] in resource [" + resourceName + "]");
        }
        return modelGrid;
    }

    /**
     * SCIPIO: Gets widget from location or null if name not within the location.
     */
    public static ModelGrid getGridFromLocationOrNull(String resourceName, String gridName, ModelReader entityModelReader, DispatchContext dispatchContext)
            throws IOException, SAXException, ParserConfigurationException {
        // SCIPIO: 4.0.0: Handle annotation-based grids with class:// prefix
        if (FormFactory.isClassLocation(resourceName)) {
            // SCIPIO: 4.0.0: reuse only a grid already built from exactly this class (a same-named grid of another
            // class must not be substituted); while this document is being resolved, null = build from the element
            ModelForm modelForm = FormFactory.getCachedAnnotationForm(resourceName, gridName);
            if (modelForm instanceof ModelGrid) {
                return (ModelGrid) modelForm;
            }
            return null;
        }
        StringBuilder sb = new StringBuilder(dispatchContext.getDelegator().getDelegatorName());
        sb.append(":").append(resourceName); // .append("#").append(gridName);
        String cacheKey = sb.toString();
        Map<String, ModelGrid> modelGridMap = gridLocationCache.get(cacheKey);
        if (modelGridMap == null) {
            // SCIPIO: refactored
            FormInitInfo formInitInfo = FormInitInfo.get();
            if (formInitInfo != null) {
                ModelGrid modelGrid = formInitInfo.getGrid(resourceName+"#"+gridName);
                if (modelGrid != null) {
                    return modelGrid;
                }
                Document doc = formInitInfo.docCache.get(resourceName);
                if (doc != null) {
                    return createModelGrid(doc, entityModelReader, dispatchContext, resourceName, gridName);
                }
            }
            synchronized (GridFactory.class) {
                modelGridMap = gridLocationCache.get(cacheKey);
                if (modelGridMap == null) {
                    URL gridFileUrl = FlexibleLocation.resolveLocation(resourceName);

                    // SCIPIO: 4.0.0: Fallback logic to try with/without .xml extension
                    if (gridFileUrl == null) {
                        String fallbackResourceName = null;
                        if (resourceName.endsWith(".xml")) {
                            // Try without .xml extension (annotation-based)
                            fallbackResourceName = resourceName.substring(0, resourceName.length() - 4);
                        } else {
                            // Try with .xml extension (XML file)
                            fallbackResourceName = resourceName + ".xml";
                        }

                        // Try the fallback location
                        URL fallbackUrl = FlexibleLocation.resolveLocation(fallbackResourceName);
                        if (fallbackUrl != null) {
                            Debug.logInfo("Grid location [" + resourceName + "] not found, using fallback [" + fallbackResourceName + "]", module);
                            gridFileUrl = fallbackUrl;
                            resourceName = fallbackResourceName; // Update resourceName for caching
                        }
                    }

                    // SCIPIO: 4.0.0: Return null instead of throwing - this is "OrNull" method
                    if (gridFileUrl == null) {
                        return null;
                    }
                    Document gridFileDoc = UtilXml.readXmlDocument(gridFileUrl, true, true);
                    if (gridFileDoc == null) {
                        return null;
                    }
                    // SCIPIO: New: Save original location as user data in Document
                    WidgetDocumentInfo.retrieveAlways(gridFileDoc).setResourceLocation(resourceName);
                    formInitInfo = FormInitInfo.begin(formInitInfo);
                    formInitInfo.gridNestedLevel++;
                    try {
                        formInitInfo.docCache.put(resourceName, gridFileDoc);
                        modelGridMap = readGridDocument(gridFileDoc, entityModelReader, dispatchContext, resourceName);
                    } finally {
                        FormInitInfo.end(formInitInfo);
                        formInitInfo.gridNestedLevel--;
                    }
                    if (formInitInfo.gridNestedLevel <= 0) {
                        gridLocationCache.put(cacheKey, modelGridMap);
                    }
                }
            }
        }
        // SCIPIO: done by non-*OrNull method
        //if (modelGrid == null) {
        //    throw new IllegalArgumentException("Could not find grid with name [" + gridName + "] in class resource [" + resourceName + "]");
        //}
        return modelGridMap.get(gridName);
    }

    public static ModelGrid getGridFromWebappContext(String resourceName, String gridName, HttpServletRequest request)
            throws IOException, SAXException, ParserConfigurationException {
        String webappName = UtilHttp.getApplicationName(request);
        String cacheKey = webappName + "::" + resourceName; // + "::" + gridName;
        Map<String, ModelGrid> modelGridMap = gridWebappCache.get(cacheKey);
        if (modelGridMap == null) {
            // SCIPIO: refactored
            synchronized (GridFactory.class) {
                modelGridMap = gridWebappCache.get(cacheKey);
                if (modelGridMap == null) {
                    ServletContext servletContext = request.getServletContext(); // SCIPIO: get context using servlet API 3.0
                    Delegator delegator = (Delegator) request.getAttribute("delegator");
                    LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
                    URL gridFileUrl = servletContext.getResource(resourceName);
                    if (gridFileUrl == null) {
                        throw new IllegalArgumentException("Could not resolve grid file location [" + resourceName + "] in the webapp [" + webappName + "]");
                    }
                    Document gridFileDoc = UtilXml.readXmlDocument(gridFileUrl, true, true);
                    if (gridFileDoc == null) {
                        throw new IllegalArgumentException("Could not read grid file at resource [" + resourceName + "] in the webapp [" + webappName + "]");
                    }
                    // SCIPIO: New: Save original location as user data in Document
                    WidgetDocumentInfo.retrieveAlways(gridFileDoc).setResourceLocation(resourceName);
                    modelGridMap = readGridDocument(gridFileDoc, delegator.getModelReader(), dispatcher.getDispatchContext(), resourceName);
                    gridWebappCache.put(cacheKey, modelGridMap);
                }
            }
        }
        ModelGrid modelGrid = modelGridMap.get(gridName);
        if (modelGrid == null) {
            throw new IllegalArgumentException("Could not find grid with name [" + gridName + "] in webapp resource [" + resourceName + "] in the webapp [" + webappName + "]");
        }
        return modelGrid;
    }

    public static Map<String, ModelGrid> readGridDocument(Document gridFileDoc, ModelReader entityModelReader, DispatchContext dispatchContext, String gridLocation) {
        Map<String, ModelGrid> modelGridMap = new HashMap<>();
        if (gridFileDoc != null) {
            // read document and construct ModelGrid for each grid element
            Element rootElement = gridFileDoc.getDocumentElement();
            // SCIPIO: Fixed missing stock backward-compat for "form" here
            //List<? extends Element> gridElements = UtilXml.childElementList(rootElement, "grid");
            List<? extends Element> gridElements = UtilXml.childElementList(rootElement);
            for (Element gridElement : gridElements) {
                if (!("grid".equals(gridElement.getTagName()) || "form".equals(gridElement.getTagName()))) { // SCIPIO
                    continue;
                }
                String gridName = gridElement.getAttribute("name");
                /* SCIPIO: don't cache at this level
                String cacheKey = gridLocation + "#" + gridName;
                ModelGrid modelGrid = gridLocationCache.get(cacheKey);
                if (modelGrid == null) {
                    modelGrid = createModelGrid(gridElement, entityModelReader, dispatchContext, gridLocation, gridName);
                    modelGrid = gridLocationCache.putIfAbsentAndGet(cacheKey, modelGrid);
                }
                */
                ModelGrid modelGrid = createModelGrid(gridElement, entityModelReader, dispatchContext, gridLocation, gridName);
                modelGridMap.put(gridName, modelGrid);
            }
        }
        return modelGridMap;
    }

    public static ModelGrid createModelGrid(Document gridFileDoc, ModelReader entityModelReader, DispatchContext dispatchContext, String gridLocation, String gridName) {
        Element gridElement = UtilXml.firstChildElement(gridFileDoc.getDocumentElement(), "grid", "name", gridName);
        if (gridElement == null) {
            // Backwards compatibility - look for form definition
            gridElement = UtilXml.firstChildElement(gridFileDoc.getDocumentElement(), "form", "name", gridName);
        }
        return createModelGrid(gridElement, entityModelReader, dispatchContext, gridLocation, gridName);
    }

    public static ModelGrid createModelGrid(Element gridElement, ModelReader entityModelReader, DispatchContext dispatchContext, String gridLocation, String gridName) {
        // SCIPIO: Due to changes in initialization method, this may be empty...
        if (UtilValidate.isEmpty(gridLocation)) {
            gridLocation = WidgetDocumentInfo.retrieveAlways(gridElement).getResourceLocation();
        }
        // SCIPIO: refactored for ThreadLocal
        FormInitInfo formInitInfo = FormInitInfo.get();
        if (formInitInfo != null) {
            ModelGrid modelGrid = formInitInfo.getGrid(gridLocation+"#"+gridName);
            if (modelGrid != null) {
                return modelGrid;
            }
        }
        ModelGrid modelGrid = new ModelGrid(gridElement, gridLocation, entityModelReader, dispatchContext);
        if (formInitInfo != null) {
            formInitInfo.set(gridLocation+"#"+gridName, modelGrid);
        }
        return modelGrid;
    }

    @Override
    public ModelGrid getWidgetFromLocation(ModelLocation modelLoc) throws IOException, IllegalArgumentException { // SCIPIO
        try {
            DispatchContext dctx = getDefaultDispatchContext();
            return getGridFromLocation(modelLoc.getResource(), modelLoc.getName(),
                    dctx.getDelegator().getModelReader(), dctx);
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }

    @Override
    public ModelGrid getWidgetFromLocationOrNull(ModelLocation modelLoc) throws IOException { // SCIPIO
        try {
            DispatchContext dctx = getDefaultDispatchContext();
            return getGridFromLocationOrNull(modelLoc.getResource(), modelLoc.getName(),
                    dctx.getDelegator().getModelReader(), dctx);
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }
}

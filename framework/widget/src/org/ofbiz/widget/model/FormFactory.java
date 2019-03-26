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
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

/**
 * Widget Library - Form factory class
 * <p>
 * SCIPIO: now also as instance
 */
@SuppressWarnings("serial")
public class FormFactory extends WidgetFactory {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    // SCIPIO: 2018-12-05: These caches are modified to hold whole file instead of individual ModelForm
    private static final UtilCache<String, Map<String, ModelForm>> formLocationCache = UtilCache.createUtilCache("widget.form.locationResource", 0, 0, false);
    private static final UtilCache<String, Map<String, ModelForm>> formWebappCache = UtilCache.createUtilCache("widget.form.webappResource", 0, 0, false);

    public static FormFactory getFormFactory() { // SCIPIO: new
        return formFactory;
    }

    public static Map<String, ModelForm> getFormsFromLocation(String resourceName, ModelReader entityModelReader, DispatchContext dispatchContext)
            throws IOException, SAXException, ParserConfigurationException {
        URL formFileUrl = FlexibleLocation.resolveLocation(resourceName);
        Document formFileDoc = UtilXml.readXmlDocument(formFileUrl, true, true);
        // SCIPIO: New: Save original location as user data in Document
        if (formFileDoc != null) {
            WidgetDocumentInfo.retrieveAlways(formFileDoc).setResourceLocation(resourceName);
        }
        return readFormDocument(formFileDoc, entityModelReader, dispatchContext, resourceName);
    }

    /**
     * Gets widget from location or exception.
     * <p>
     * SCIPIO: now delegating.
     */
    public static ModelForm getFormFromLocation(String resourceName, String formName, ModelReader entityModelReader, DispatchContext dispatchContext)
            throws IOException, SAXException, ParserConfigurationException {
        ModelForm modelForm = getFormFromLocationOrNull(resourceName, formName, entityModelReader, dispatchContext);
        if (modelForm == null) {
            throw new IllegalArgumentException("Could not find form with name [" + formName + "] in resource [" + resourceName + "]");
        }
        return modelForm;
    }

    /**
     * SCIPIO: Accumulates the form instances during construction, for extends-resource resolution.
     */
    static class FormInitInfo { // SCIPIO
        private static final ThreadLocal<FormInitInfo> CURRENT = new ThreadLocal<>();

        // SCIPIO: GRID DEFINITION FORWARD/BACKWARD COMPATIBILITY: forms and grids accumulate together in one map
        Map<String, ModelForm> modelMap = new HashMap<>();
        Map<String, Document> docCache = new HashMap<>();
        int nestedLevel = 0;
        int formNestedLevel = 0;
        int gridNestedLevel = 0;

        static FormInitInfo get() {
            return CURRENT.get();
        }
        
        static FormInitInfo begin(FormInitInfo formInitInfo) {
            if (formInitInfo == null) {
                formInitInfo = new FormInitInfo();
                CURRENT.set(formInitInfo);
            } else {
                formInitInfo.nestedLevel++;
            }
            return formInitInfo;
        }
        
        static void end(FormInitInfo formInitInfo) {
            if (formInitInfo.nestedLevel <= 0) {
                FormInitInfo.CURRENT.remove();
            } else {
                formInitInfo.nestedLevel--;
            }
        }
        
        ModelForm getForm(String key) {
            return modelMap.get(key);
        }

        ModelGrid getGrid(String key) {
            ModelForm form = getForm(key);
            if (form instanceof ModelGrid) {
                return (ModelGrid) form;
            }
            return null;
        }

        void set(String key, ModelForm form) {
            if (form != null) {
                ModelForm prevForm = modelMap.get(key);
                if (prevForm != null && prevForm != form) {
                    if (prevForm instanceof ModelSingleForm) {
                        Debug.logWarning("Redefinition of form [" + key + "] during construction; discarding extra instance (" 
                            + form.getClass().getSimpleName() + ") and keeping previous (" + prevForm.getClass().getSimpleName(), module);
                        return;
                    }
                    Debug.logWarning("Redefinition of form [" + key + "] during construction; using new instance (" 
                            + form.getClass().getSimpleName() + ") and discarding previous (" + prevForm.getClass().getSimpleName(), module);
                }
                modelMap.put(key, form);
            }
        }
    }

    /**
     * SCIPIO: Gets widget from location or null if name not within the location.
     */
    public static ModelForm getFormFromLocationOrNull(String resourceName, String formName, ModelReader entityModelReader, DispatchContext dispatchContext)
            throws IOException, SAXException, ParserConfigurationException {
        StringBuilder sb = new StringBuilder(dispatchContext.getDelegator().getDelegatorName());
        sb.append(":").append(resourceName);
        String cacheKey = sb.toString();
        Map<String, ModelForm> modelFormMap = formLocationCache.get(cacheKey);
        if (modelFormMap == null) {
            // SCIPIO: refactored
            FormInitInfo formInitInfo = FormInitInfo.get();
            if (formInitInfo != null) {
                ModelForm modelForm = formInitInfo.getForm(resourceName+"#"+formName);
                if (modelForm != null) {
                    return modelForm;
                }
                Document doc = formInitInfo.docCache.get(resourceName);
                if (doc != null) {
                    return createModelForm(doc, entityModelReader, dispatchContext, resourceName, formName);
                }
            }
            synchronized (FormFactory.class) {
                modelFormMap = formLocationCache.get(cacheKey);
                if (modelFormMap == null) {
                    URL formFileUrl = FlexibleLocation.resolveLocation(resourceName);
                    if (formFileUrl == null) {
                        throw new IllegalArgumentException("Could not resolve form file location [" + resourceName + "]");
                    }
                    Document formFileDoc = UtilXml.readXmlDocument(formFileUrl, true, true);
                    if (formFileDoc == null) {
                        throw new IllegalArgumentException("Could not read form file at resource [" + resourceName + "]");
                    }
                    // SCIPIO: New: Save original location as user data in Document
                    WidgetDocumentInfo.retrieveAlways(formFileDoc).setResourceLocation(resourceName);
                    formInitInfo = FormInitInfo.begin(formInitInfo);
                    formInitInfo.formNestedLevel++;
                    try {
                        formInitInfo.docCache.put(resourceName, formFileDoc);
                        modelFormMap = readFormDocument(formFileDoc, entityModelReader, dispatchContext, resourceName);
                    } finally {
                        FormInitInfo.end(formInitInfo);
                        formInitInfo.formNestedLevel--;
                    }
                    if (formInitInfo.formNestedLevel <= 0) {
                        formLocationCache.put(cacheKey, modelFormMap);
                    }
                }
            }
        }
        return modelFormMap.get(formName);
    }

    public static ModelForm getFormFromWebappContext(String resourceName, String formName, HttpServletRequest request)
            throws IOException, SAXException, ParserConfigurationException {
        String webappName = UtilHttp.getApplicationName(request);
        String cacheKey = webappName + "::" + resourceName;
        Map<String, ModelForm> modelFormMap = formWebappCache.get(cacheKey);
        if (modelFormMap == null) {
            // SCIPIO: refactored
            synchronized (FormFactory.class) {
                modelFormMap = formWebappCache.get(cacheKey);
                if (modelFormMap == null) {
                    ServletContext servletContext = request.getServletContext(); // SCIPIO: get context using servlet API 3.0
                    Delegator delegator = (Delegator) request.getAttribute("delegator");
                    LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
                    URL formFileUrl = servletContext.getResource(resourceName);
                    if (formFileUrl == null) {
                        throw new IllegalArgumentException("Could not resolve form file location [" + resourceName + "] in the webapp [" + webappName + "]");
                    }
                    Document formFileDoc = UtilXml.readXmlDocument(formFileUrl, true, true);
                    if (formFileDoc == null) {
                        throw new IllegalArgumentException("Could not read form file at resource [" + resourceName + "] in the webapp [" + webappName + "]");
                    }
                    // SCIPIO: New: Save original location as user data in Document
                    WidgetDocumentInfo.retrieveAlways(formFileDoc).setResourceLocation(resourceName);
                    modelFormMap = readFormDocument(formFileDoc, delegator.getModelReader(), dispatcher.getDispatchContext(), resourceName);
                    formWebappCache.put(cacheKey, modelFormMap);
                }
            }
        }
        ModelForm modelForm = modelFormMap.get(formName); // SCIPIO
        if (modelForm == null) {
            throw new IllegalArgumentException("Could not find form with name [" + formName + "] in webapp resource [" + resourceName + "] in the webapp [" + webappName + "]");
        }
        return modelForm;
    }

    public static Map<String, ModelForm> readFormDocument(Document formFileDoc, ModelReader entityModelReader, DispatchContext dispatchContext, String formLocation) {
        Map<String, ModelForm> modelFormMap = new HashMap<>();
        if (formFileDoc != null) {
            // read document and construct ModelForm for each form element
            Element rootElement = formFileDoc.getDocumentElement();
            if (!"forms".equalsIgnoreCase(rootElement.getTagName())) {
                rootElement = UtilXml.firstChildElement(rootElement, "forms");
            }
            // SCIPIO: GRID DEFINITION FORWARD COMPATIBILITY
            //List<? extends Element> formElements = UtilXml.childElementList(rootElement, "form");
            List<? extends Element> formElements = UtilXml.childElementList(rootElement);
            for (Element formElement : formElements) {
                if (!("grid".equals(formElement.getTagName()) || "form".equals(formElement.getTagName()))) { // SCIPIO
                    continue;
                }
                String formName = formElement.getAttribute("name");
                /* SCIPIO: don't cache at this level
                String cacheKey = formLocation + "#" + formName;
                ModelForm modelForm = formLocationCache.get(cacheKey);
                if (modelForm == null) {
                    modelForm = createModelForm(formElement, entityModelReader, dispatchContext, formLocation, formName);
                    modelForm = formLocationCache.putIfAbsentAndGet(cacheKey, modelForm);
                }
                */
                ModelForm modelForm = createModelForm(formElement, entityModelReader, dispatchContext, formLocation, formName);
                modelFormMap.put(formName, modelForm);
            }
        }
        return modelFormMap;
    }

    public static ModelForm createModelForm(Document formFileDoc, ModelReader entityModelReader, DispatchContext dispatchContext, String formLocation, String formName) {
        Element rootElement = formFileDoc.getDocumentElement();
        if (!"forms".equalsIgnoreCase(rootElement.getTagName())) {
            rootElement = UtilXml.firstChildElement(rootElement, "forms");
        }
        Element formElement = UtilXml.firstChildElement(rootElement, "form", "name", formName);
        if (formElement == null) {
            // SCIPIO: GRID DEFINITION FORWARD COMPATIBILITY
            formElement = UtilXml.firstChildElement(rootElement, "grid", "name", formName);
            if (formElement == null) { // SCIPIO
                return null;
            }
        }
        return createModelForm(formElement, entityModelReader, dispatchContext, formLocation, formName);
    }

    public static ModelForm createModelForm(Element formElement, ModelReader entityModelReader, DispatchContext dispatchContext, String formLocation, String formName) {
        // SCIPIO: Due to changes in initialization method, this may be empty...
        if (UtilValidate.isEmpty(formLocation)) {
            formLocation = WidgetDocumentInfo.retrieveAlways(formElement).getResourceLocation();
        }
        // SCIPIO: refactored for ThreadLocal
        FormInitInfo formInitInfo = FormInitInfo.get();
        ModelForm modelForm;
        String formType = formElement.getAttribute("type");
        boolean isGrid = "grid".equals(formElement.getTagName()); // SCIPIO: never initialize as ModelSingleFrom if it's a <grid> element
        if (!isGrid && (formType.isEmpty() || "single".equals(formType) || "upload".equals(formType))) {
            if (formInitInfo != null) {
                modelForm = formInitInfo.getForm(formLocation+"#"+formName);
                if (modelForm instanceof ModelSingleForm) {
                    return modelForm;
                }
            }
            modelForm = new ModelSingleForm(formElement, formLocation, entityModelReader, dispatchContext);
        } else {
            if (formInitInfo != null) {
                modelForm = formInitInfo.getForm(formLocation+"#"+formName);
                if (modelForm instanceof ModelGrid) {
                    return modelForm;
                }
            }
            // SCIPIO: reuse ModelGrid instances 
            // NOTE: very likely it was returned by formInitInfo.getForm already, but it's possible
            // for the grid cache to have been initialized fully in a second call separately from form cache,
            // so we have to do a cache lookup first
            //modelForm = new ModelGrid(formElement, formLocation, entityModelReader, dispatchContext);
            try {
                modelForm = GridFactory.getGridFromLocationOrNull(formLocation, formName, entityModelReader, dispatchContext);
                if (modelForm == null) {
                    Debug.logWarning("Could not reuse ModelGrid [" + formLocation + "#" + formName + "]; creating new", module);
                    modelForm = new ModelGrid(formElement, formLocation, entityModelReader, dispatchContext);
                }
            } catch (IOException | SAXException | ParserConfigurationException e) {
                Debug.logError(e, "Could not load ModelGrid [" + formLocation + "#" + formName + "]", module);
                modelForm = null;
            }
        }
        if (formInitInfo != null && modelForm != null) {
            formInitInfo.set(formLocation+"#"+formName, modelForm);
        }
        return modelForm;
    }

    @Override
    public ModelForm getWidgetFromLocation(ModelLocation modelLoc) throws IOException, IllegalArgumentException { // SCIPIO
        try {
            DispatchContext dctx = getDefaultDispatchContext();
            return getFormFromLocation(modelLoc.getResource(), modelLoc.getName(),
                    dctx.getDelegator().getModelReader(), dctx);
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }

    @Override
    public ModelForm getWidgetFromLocationOrNull(ModelLocation modelLoc) throws IOException { // SCIPIO
        try {
            DispatchContext dctx = getDefaultDispatchContext();
            return getFormFromLocationOrNull(modelLoc.getResource(), modelLoc.getName(),
                    dctx.getDelegator().getModelReader(), dctx);
        } catch (SAXException e) {
            throw new IOException(e);
        } catch (ParserConfigurationException e) {
            throw new IOException(e);
        }
    }
}

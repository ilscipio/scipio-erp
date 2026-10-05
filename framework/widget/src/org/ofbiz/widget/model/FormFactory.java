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
import java.util.concurrent.ConcurrentHashMap;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpServletRequest;
import javax.xml.parsers.ParserConfigurationException;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.ce.base.component.ComponentReflectRegistry;
import com.ilscipio.scipio.widget.def.form.FormAnnotationReader;
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

    // SCIPIO: 4.0.0: Annotation-based form cache - key is "class://fully.qualified.ClassName"
    private static final String CLASS_LOCATION_PREFIX = "class://";
    private static volatile Map<String, ModelForm> annotationFormCache = null;
    // SCIPIO: 4.0.0: forms keyed by "sourceLocation#name" (unique); annotationFormCache stays name-keyed for name-only fallback
    private static volatile Map<String, ModelForm> annotationFormCacheByKey = null;
    // SCIPIO: 4.0.0: Location alias registry - maps component:// URLs to form maps
    private static volatile Map<String, Map<String, ModelForm>> formLocationAliases = null;
    private static volatile boolean annotationFormsLoaded = false;
    // SCIPIO: 4.0.0: ThreadLocal to detect recursive calls during loading (prevents StackOverflowError)
    private static final ThreadLocal<Boolean> annotationFormsLoading = ThreadLocal.withInitial(() -> Boolean.FALSE);
    // SCIPIO: 4.0.0: ThreadLocal to track forms currently being resolved (prevents circular dependency StackOverflow)
    private static final ThreadLocal<java.util.Set<String>> formsBeingResolved = ThreadLocal.withInitial(() -> new java.util.HashSet<>());
    // SCIPIO: 4.0.0: Pending form documents - collected during annotation loading, resolved lazily
    private static volatile Map<String, FormDocumentInfo> pendingFormDocuments = null;

    public static FormFactory getFormFactory() { // SCIPIO: new
        return formFactory;
    }

    /**
     * SCIPIO: 4.0.0: Checks if a resource location refers to an annotation-based form class.
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
     * SCIPIO: 4.0.0: Container for form document info during two-pass loading.
     *
     * <p>This class holds the form Document and metadata collected during the first pass
     * of annotation loading (when no DispatchContext is available). The actual ModelForm
     * is created lazily on first access.</p>
     */
    public static class FormDocumentInfo {
        public final Document document;
        public final String sourceLocation;
        public final String formName;
        public final com.ilscipio.scipio.widget.def.form.Form formDef;

        public FormDocumentInfo(Document document, String sourceLocation, String formName,
                                com.ilscipio.scipio.widget.def.form.Form formDef) {
            this.document = document;
            this.sourceLocation = sourceLocation;
            this.formName = formName;
            this.formDef = formDef;
        }
    }

    /**
     * SCIPIO: 4.0.0: Loads all annotation-based form Documents from all components.
     *
     * <p>Uses two-pass loading like MenuFactory to avoid DispatchContext dependency:
     * <ol>
     *   <li>First pass (here): Collect all form Documents from annotations (no DispatchContext needed)</li>
     *   <li>Second pass (in getAnnotationForm): Create ModelForms lazily when accessed</li>
     * </ol></p>
     *
     * <p>This solves the circular dependency issue where FormFactory.loadAnnotationForms()
     * was calling getDefaultDispatchContext() during ComponentContainer startup, before
     * the delegator was available.</p>
     */
    private static void loadAnnotationForms() {
        if (annotationFormsLoaded) {
            return;
        }
        // SCIPIO: 4.0.0: Check for recursive call within same thread (prevents StackOverflowError)
        // This can happen when form parent resolution triggers another loadAnnotationForms() call
        if (annotationFormsLoading.get()) {
            return; // Already loading in this thread, skip to prevent recursion
        }
        synchronized (FormFactory.class) {
            if (annotationFormsLoaded) {
                return;
            }
            // Set loading flag BEFORE starting the loop
            annotationFormsLoading.set(Boolean.TRUE);

            // SCIPIO: 4.0.0: Initialize caches FIRST to prevent issues during loading
            Map<String, ModelForm> forms = new ConcurrentHashMap<>();
            Map<String, Map<String, ModelForm>> locationAliases = new ConcurrentHashMap<>();
            Map<String, FormDocumentInfo> pendingDocs = new ConcurrentHashMap<>();
            annotationFormCache = forms;
            annotationFormCacheByKey = new java.util.concurrent.ConcurrentHashMap<>();
            formLocationAliases = locationAliases;
            pendingFormDocuments = pendingDocs;

            // SCIPIO: 4.0.0: Tracks whether the pass-1 load was aborted early by an
            // uncaught Throwable, so we never leave annotationFormsLoaded=true silently
            // pointing at a partial/incomplete component scan.
            boolean partialLoad = false;
            try {
                // SCIPIO: 4.0.0: Two-pass loading - NO DispatchContext needed in first pass
                // Pass 1: Collect all Documents without creating ModelForms
                for (ComponentReflectInfo cri : ComponentReflectRegistry.getReflectInfos()) {
                    try {
                        // Create reader WITHOUT DispatchContext - just for document collection
                        FormAnnotationReader reader = new FormAnnotationReader(cri, null, null);
                        Map<String, FormDocumentInfo> componentDocs = reader.getFormDocuments();

                        for (Map.Entry<String, FormDocumentInfo> entry : componentDocs.entrySet()) {
                            String formName = entry.getKey();
                            FormDocumentInfo docInfo = entry.getValue();
                            if (pendingDocs.containsKey(formName)) {
                                Debug.logWarning("Annotation form [" + formName +
                                        "] is defined more than once, most recent will over-write previous definition(s)", module);
                            }
                            pendingDocs.put(formName, docInfo);

                            // Register location aliases from the form annotation
                            if (docInfo.formDef != null) {
                                registerFormLocationAliasesFromDef(locationAliases, formName, docInfo);
                            }
                        }
                    } catch (Throwable t) { // SCIPIO: 4.0.0: was catch (Exception) - an Error (e.g. NoClassDefFoundError)
                        // from one component must not silently abort the whole component loop
                        Debug.logError(t, "Error collecting annotation form documents from component [" +
                                cri.getComponent().getGlobalName() + "]: " + t, module);
                    }
                }

                Debug.logInfo("Collected [" + pendingDocs.size() + "] annotation-based form documents with [" +
                        locationAliases.size() + "] location aliases (lazy ModelForm creation)", module);

            } catch (Throwable t) { // SCIPIO: 4.0.0: was catch (Exception) - must catch Throwable so a fatal
                // Error is logged instead of escaping silently and leaving annotationFormsLoaded=false forever
                // (which caused a full component rescan, ~10s, on every single request)
                partialLoad = true;
                Debug.logError(t, "FATAL: annotation form loading aborted - " + t, module);
            } finally {
                // Always clear loading flag
                annotationFormsLoading.set(Boolean.FALSE);
                // SCIPIO: 4.0.0: Set the loaded flag in finally so a partial load (due to a caught
                // Throwable above) is never retried on every subsequent request.
                annotationFormsLoaded = true;
                if (partialLoad) {
                    Debug.logError("FATAL: annotation form loading only partially completed - " +
                            "[" + pendingDocs.size() + "] forms collected before abort; " +
                            "some components were NOT scanned; see FATAL stack trace above", module);
                }
            }
        }
    }

    /**
     * SCIPIO: 4.0.0: Registers location aliases from form definition during document collection.
     * The actual ModelForm is created lazily, but we can register aliases based on annotation metadata.
     */
    private static void registerFormLocationAliasesFromDef(Map<String, Map<String, ModelForm>> locationAliases,
                                                            String formName, FormDocumentInfo docInfo) {
        com.ilscipio.scipio.widget.def.form.Form formDef = docInfo.formDef;
        if (formDef == null) {
            return;
        }
        // Note: We can't register the actual ModelForm yet (it's not created),
        // but we can note which locations will have this form.
        // The actual ModelForm registration happens in resolveFormFromDocument.
        // For now, we just track the location alias mapping for the pending doc.
        // (The real alias registration with ModelForm happens when the form is resolved)
    }

    /**
     * SCIPIO: 4.0.0: Gets an annotation-based form by name.
     *
     * <p>This method implements lazy ModelForm creation. During annotation loading (first pass),
     * only Documents are collected. When a form is first accessed, the ModelForm is created
     * from the cached Document using the current DispatchContext.</p>
     */
    public static ModelForm getAnnotationForm(String formName) {
        loadAnnotationForms();

        // Check if already resolved
        if (annotationFormCache != null) {
            ModelForm cached = annotationFormCache.get(formName);
            if (cached != null) {
                return cached;
            }
        }

        // Check pending documents and resolve lazily
        if (pendingFormDocuments != null) {
            FormDocumentInfo docInfo = pendingFormDocuments.get(formName);
            if (docInfo == null) {
                docInfo = findPendingFormByName(formName); // SCIPIO: 4.0.0: keys are "sourceLocation#name"
            }
            if (docInfo != null) {
                return resolveFormFromDocument(docInfo.formName, docInfo);
            }
        }

        return null;
    }

    /**
     * SCIPIO: 4.0.0: Resolves a ModelForm from a pending FormDocumentInfo.
     *
     * <p>This is called lazily when a form is first accessed. At this point,
     * DispatchContext should be available from the runtime context.</p>
     */
    /** SCIPIO: 4.0.0: Name-only fallback over the pending documents (last definition wins, like the old name-keyed map). */
    private static FormDocumentInfo findPendingFormByName(String formName) {
        FormDocumentInfo found = null;
        for (FormDocumentInfo d : pendingFormDocuments.values()) {
            if (formName.equals(d.formName)) {
                found = d;
            }
        }
        return found;
    }

    /** SCIPIO: 4.0.0: Resolves a form for a class:// resource: exact class first, then a nested class of it, then by name. */
    /**
     * SCIPIO: 4.0.0: Returns the already-built annotation form of exactly this class resource, or null (no
     * resolution, no name fallback) - used by GridFactory to reuse a grid without cross-class name collisions.
     */
    public static ModelForm getCachedAnnotationForm(String classResource, String formName) {
        Map<String, ModelForm> byKey = annotationFormCacheByKey;
        return (byKey != null && classResource != null) ? byKey.get(classResource + "#" + formName) : null;
    }

    public static ModelForm getAnnotationFormFromClass(String classResource, String formName) {
        loadAnnotationForms();
        if (pendingFormDocuments == null) {
            return getAnnotationForm(formName);
        }
        FormDocumentInfo nested = null;
        for (FormDocumentInfo d : pendingFormDocuments.values()) {
            if (!formName.equals(d.formName) || d.sourceLocation == null) continue;
            if (d.sourceLocation.equals(classResource)) {
                return resolveFormFromDocument(formName, d);
            }
            if (nested == null && d.sourceLocation.startsWith(classResource + "$")) {
                nested = d;
            }
        }
        if (nested != null) {
            return resolveFormFromDocument(formName, nested);
        }
        return getAnnotationForm(formName);
    }

    private static synchronized ModelForm resolveFormFromDocument(String formName, FormDocumentInfo docInfo) {
        // SCIPIO: 4.0.0: cache per document key, not per name (same-named forms exist in different files/locations)
        String docKey = docInfo.sourceLocation + "#" + formName;
        if (annotationFormCacheByKey != null) {
            ModelForm cached = annotationFormCacheByKey.get(docKey);
            if (cached != null) {
                return cached;
            }
        }

        // SCIPIO: 4.0.0: Check for circular resolution - if this form is already being resolved
        // in the current call stack, return null to break the cycle (prevents StackOverflowError)
        java.util.Set<String> resolving = formsBeingResolved.get();
        if (resolving.contains(docKey)) {
            Debug.logWarning("Circular form resolution detected for [" + formName + "], breaking cycle", module);
            return null;
        }

        resolving.add(docKey);
        try {
            // Get DispatchContext now (at runtime, it's available)
            DispatchContext dctx = getDefaultDispatchContext();
            ModelReader entityModelReader = dctx.getDelegator().getModelReader();

            // SCIPIO: 4.0.0: Set WidgetDocumentInfo on the document (required for form resolution)
            WidgetDocumentInfo.retrieveAlways(docInfo.document).setResourceLocation(docInfo.sourceLocation);

            // Create ModelForm from the cached document
            ModelForm modelForm = createModelForm(docInfo.document, entityModelReader, dctx,
                    docInfo.sourceLocation, formName);

            if (modelForm != null && annotationFormCache != null) {
                if (annotationFormCacheByKey != null) {
                    annotationFormCacheByKey.put(docKey, modelForm);
                }
                annotationFormCache.putIfAbsent(formName, modelForm);

                // Register location aliases now that we have the ModelForm
                if (docInfo.formDef != null) {
                    registerFormLocationAliases(formLocationAliases, modelForm, docInfo.formDef);
                }
            }

            return modelForm;
        } catch (Exception e) {
            Debug.logError(e, "Error resolving annotation form [" + formName + "] from document", module);
            return null;
        } finally {
            resolving.remove(docKey);
        }
    }

    /**
     * SCIPIO: 4.0.0: Registers location aliases for a resolved ModelForm.
     */
    private static void registerFormLocationAliases(Map<String, Map<String, ModelForm>> locationAliases,
                                                     ModelForm form, com.ilscipio.scipio.widget.def.form.Form formDef) {
        if (formDef == null || form == null || locationAliases == null) {
            return;
        }
        // Single location alias
        String location = formDef.location();
        if (UtilValidate.isNotEmpty(location)) {
            locationAliases
                    .computeIfAbsent(location, k -> new ConcurrentHashMap<>())
                    .put(form.getName(), form);
        }
        // Multiple location aliases
        for (String loc : formDef.locations()) {
            if (UtilValidate.isNotEmpty(loc)) {
                locationAliases
                        .computeIfAbsent(loc, k -> new ConcurrentHashMap<>())
                        .put(form.getName(), form);
            }
        }
    }

    /**
     * SCIPIO: 4.0.0: Gets all annotation-based forms.
     *
     * <p>Note: This will trigger resolution of all pending forms.</p>
     */
    public static Map<String, ModelForm> getAnnotationForms() {
        loadAnnotationForms();

        // Resolve all pending forms
        if (pendingFormDocuments != null && annotationFormCache != null) {
            for (FormDocumentInfo d : pendingFormDocuments.values()) {
                if (annotationFormCacheByKey == null || !annotationFormCacheByKey.containsKey(d.sourceLocation + "#" + d.formName)) {
                    resolveFormFromDocument(d.formName, d); // This triggers lazy resolution
                }
            }
        }

        return annotationFormCache != null ? annotationFormCache : new HashMap<>();
    }

    /**
     * SCIPIO: 4.0.0: Gets a form from a location alias.
     *
     * <p>This allows component:// style URLs to resolve to annotation-based forms
     * when the @Form annotation specifies a location or locations attribute.</p>
     *
     * @param location The component:// style location (e.g., "component://setup/widget/SetupForms.xml")
     * @param formName The form name within that location
     * @return The ModelForm, or null if no alias is registered for this location/name
     */
    public static ModelForm getFormFromLocationAlias(String location, String formName) {
        loadAnnotationForms();

        // First check already-resolved forms
        if (formLocationAliases != null) {
            Map<String, ModelForm> formsAtLocation = formLocationAliases.get(location);
            if (formsAtLocation != null) {
                ModelForm form = formsAtLocation.get(formName);
                if (form != null) {
                    return form;
                }
            }
        }

        // Check if there's a pending form with this location alias
        if (pendingFormDocuments != null) {
            for (Map.Entry<String, FormDocumentInfo> entry : pendingFormDocuments.entrySet()) {
                FormDocumentInfo docInfo = entry.getValue();
                if (docInfo.formDef != null && formName.equals(docInfo.formName)) {
                    // Check if this form has the requested location alias
                    String loc = docInfo.formDef.location();
                    if (location.equals(loc)) {
                        return resolveFormFromDocument(formName, docInfo); // SCIPIO: 4.0.0: resolve THIS location's definition
                    }
                    for (String l : docInfo.formDef.locations()) {
                        if (location.equals(l)) {
                            return resolveFormFromDocument(formName, docInfo); // SCIPIO: 4.0.0: resolve THIS location's definition
                        }
                    }
                }
            }
        }

        return null;
    }

    /**
     * SCIPIO: 4.0.0: Checks if a location has any registered form aliases.
     */
    public static boolean hasLocationAlias(String location) {
        loadAnnotationForms();

        // Check resolved aliases
        if (formLocationAliases != null && formLocationAliases.containsKey(location)) {
            return true;
        }

        // Check pending forms for potential aliases
        if (pendingFormDocuments != null) {
            for (FormDocumentInfo docInfo : pendingFormDocuments.values()) {
                if (docInfo.formDef != null) {
                    if (location.equals(docInfo.formDef.location())) {
                        return true;
                    }
                    for (String l : docInfo.formDef.locations()) {
                        if (location.equals(l)) {
                            return true;
                        }
                    }
                }
            }
        }

        return false;
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
        // SCIPIO: 4.0.0: Handle annotation-based forms with class:// prefix
        if (isClassLocation(resourceName)) {
            ModelForm modelForm = getAnnotationFormFromClass(resourceName, formName); // SCIPIO: 4.0.0: class-aware
            if (modelForm == null) {
                throw new IllegalArgumentException("Could not find annotation-based form with name [" + formName + "] from class [" + resourceName + "]");
            }
            return modelForm;
        }

        // SCIPIO: 4.0.0: Check location aliases first (allows annotation-based forms to replace XML)
        ModelForm aliasForm = getFormFromLocationAlias(resourceName, formName);
        if (aliasForm != null) {
            return aliasForm;
        }

        ModelForm modelForm = getFormFromLocationOrNull(resourceName, formName, entityModelReader, dispatchContext);
        if (modelForm == null) {
            // SCIPIO: 4.0.0: Final fallback - try to find form by name in all annotation forms
            // This handles cases where XML was deleted and the alias lookup fails due to loading order
            modelForm = getAnnotationForm(formName);
            if (modelForm != null) {
                Debug.logWarning("Form [" + formName + "] not found at [" + resourceName +
                    "] but found as annotation form; consider updating the reference to use class:// location", module);
                return modelForm;
            }
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
        // SCIPIO: 4.0.0: Handle annotation-based forms with class:// prefix
        if (isClassLocation(resourceName)) {
            return getAnnotationFormFromClass(resourceName, formName); // SCIPIO: 4.0.0: class-aware
        }
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
                    // SCIPIO: 4.0.0: Use unified WidgetLocationResolver for consistent fallback logic
                    URL formFileUrl = WidgetLocationResolver.resolveWidgetLocation(resourceName, "form");

                    // SCIPIO: 4.0.0: Return null instead of throwing - this is "OrNull" method
                    if (formFileUrl == null) {
                        return null;
                    }
                    Document formFileDoc = UtilXml.readXmlDocument(formFileUrl, true, true);
                    if (formFileDoc == null) {
                        return null;
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

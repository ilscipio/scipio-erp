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
package com.ilscipio.scipio.widget.def.form;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.ConditionExpr;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilTimer;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.model.ModelReader;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.widget.model.FormFactory;
import org.ofbiz.widget.model.ModelForm;
import org.w3c.dom.Document;
import org.w3c.dom.Element;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Form annotation reader - creates ModelForm objects from @Form annotations.
 *
 * <p>This reader scans classes annotated with @Form and builds corresponding
 * ModelForm objects that can be used by the widget framework.</p>
 *
 * <p>The reader generates synthetic XML elements from annotations, which are then
 * passed to the existing ModelForm/FormFactory constructors. This approach
 * ensures compatibility with the existing widget infrastructure.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for form annotations support.</p>
 */
@SuppressWarnings("serial")
public class FormAnnotationReader implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // SCIPIO: 4.0.0: Reused per-thread DocumentBuilder. Previously buildSingleFormDocument() called
    // DocumentBuilderFactory.newInstance().newDocumentBuilder() PER FORM (~2000 forms across all
    // components), and each instantiation re-scans the Xerces classpath resources - this was the
    // dominant cost of the ~10s per-component annotation form load. A DocumentBuilder is not
    // thread-safe but IS safe to reuse sequentially within a single thread (newDocument() carries
    // no state between calls), so a ThreadLocal avoids both the re-instantiation cost and any
    // cross-thread sharing hazard.
    private static final ThreadLocal<DocumentBuilder> THREAD_LOCAL_DOCUMENT_BUILDER = ThreadLocal.withInitial(() -> {
        try {
            return DocumentBuilderFactory.newInstance().newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            throw new IllegalStateException("Error creating shared DocumentBuilder for form annotation reading", e);
        }
    });

    protected final ComponentReflectInfo reflectInfo;
    protected final ModelReader entityModelReader;
    protected final DispatchContext dispatchContext;

    public FormAnnotationReader(ComponentReflectInfo reflectInfo, ModelReader entityModelReader, DispatchContext dispatchContext) {
        this.reflectInfo = reflectInfo;
        this.entityModelReader = entityModelReader;
        this.dispatchContext = dispatchContext;
    }

    /**
     * Reads all @Form annotated classes/interfaces and returns a map of form names to ModelForm objects.
     */
    public Map<String, ModelForm> getModelForms() {
        return getModelForms(null);
    }

    /**
     * SCIPIO: 4.0.0: Reads all @Form annotated classes/interfaces and returns a map of form names to
     * FormDocumentInfo objects (containing Documents, not ModelForms).
     *
     * <p>This method is used for two-pass loading to avoid the circular dependency on DispatchContext.
     * The Documents are collected first (no DispatchContext needed), then ModelForms are created
     * lazily on first access when DispatchContext is available.</p>
     *
     * @return Map of form names to FormDocumentInfo objects
     */
    public Map<String, FormFactory.FormDocumentInfo> getFormDocuments() {
        UtilTimer utilTimer = new UtilTimer();
        if (reflectInfo != null) {
            utilTimer.timerString("Before start of form document collection for component [" +
                    reflectInfo.getComponent().getGlobalName() + "]");
        }

        Map<String, FormFactory.FormDocumentInfo> formDocs = new LinkedHashMap<>();
        int formCount = 0;

        if (reflectInfo == null || reflectInfo.getReflectQuery() == null) {
            return formDocs;
        }

        for (Class<?> formClass : reflectInfo.getReflectQuery().getAnnotatedClasses(Form.class)) {
            try {
                String sourceLocation = "class://" + formClass.getName();

                // Check for @FormList (container for multiple @Form)
                FormList formList = formClass.getAnnotation(FormList.class);
                if (formList != null) {
                    for (Form formDef : formList.value()) {
                        String formName = formDef.name();
                        if (UtilValidate.isEmpty(formName)) {
                            Debug.logWarning("Form annotation in class " + formClass.getName() +
                                    " has no name, skipping", module);
                            continue;
                        }
                        Document doc = buildFormDocument(formDef, formClass, sourceLocation);
                        if (doc != null) {
                            if (formDocs.containsKey(formName)) {
                                Debug.logWarning("Form " + formName + " is defined more than once, " +
                                        "most recent will over-write previous definition(s)", module);
                            }
                            formDocs.put(sourceLocation + "#" + formName, new FormFactory.FormDocumentInfo(doc, sourceLocation, formName, formDef)); // SCIPIO: 4.0.0: unique key (same-named forms in different files must not overwrite each other)
                            formCount++;
                        }
                    }
                }

                // Check for single @Form annotation
                Form formDef = formClass.getAnnotation(Form.class);
                if (formDef != null) {
                    String formName = formDef.name();
                    if (UtilValidate.isEmpty(formName)) {
                        Debug.logWarning("Form annotation in class " + formClass.getName() +
                                " has no name, skipping", module);
                    } else {
                        Document doc = buildFormDocument(formDef, formClass, sourceLocation);
                        if (doc != null) {
                            if (formDocs.containsKey(formName)) {
                                Debug.logWarning("Form " + formName + " is defined more than once, " +
                                        "most recent will over-write previous definition(s)", module);
                            }
                            formDocs.put(sourceLocation + "#" + formName, new FormFactory.FormDocumentInfo(doc, sourceLocation, formName, formDef)); // SCIPIO: 4.0.0: unique key (same-named forms in different files must not overwrite each other)
                            formCount++;
                        }
                    }
                }
            } catch (Exception e) {
                Debug.logError(e, "Error collecting form documents from annotations in class " + formClass.getName(), module);
            }
        }

        if (reflectInfo != null) {
            utilTimer.timerString("Finished form document collection for component [" +
                    reflectInfo.getComponent().getGlobalName() + "] - Total Forms: " + formCount + " FINISHED");
            Debug.logInfo("Collected [" + formCount + "] form documents from annotations for component [" +
                    reflectInfo.getComponent().getGlobalName() + "]", module);
        }

        return formDocs;
    }

    /**
     * Reads all @Form annotated classes/interfaces and returns a map of form names to ModelForm objects.
     * Also populates the locationAliases map if provided.
     *
     * @param locationAliases Optional map to populate with location aliases (location -> name -> form)
     */
    public Map<String, ModelForm> getModelForms(Map<String, Map<String, ModelForm>> locationAliases) {
        UtilTimer utilTimer = new UtilTimer();
        utilTimer.timerString("Before start of form loop in form annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]");

        Map<String, ModelForm> modelForms = new LinkedHashMap<>();
        int formCount = 0;

        for (Class<?> formClass : reflectInfo.getReflectQuery().getAnnotatedClasses(Form.class)) {
            try {
                List<FormWithAnnotation> formsWithAnnotations = readFormsWithAnnotationsFromClass(formClass);
                for (FormWithAnnotation fwa : formsWithAnnotations) {
                    ModelForm form = fwa.form;
                    Form formDef = fwa.annotation;

                    if (modelForms.containsKey(form.getName())) {
                        Debug.logWarning("Form " + form.getName() + " is defined more than once, " +
                                "most recent will over-write previous definition(s)", module);
                    }
                    modelForms.put(form.getName(), form);
                    formCount++;

                    // Register location aliases if map is provided
                    if (locationAliases != null) {
                        registerLocationAliases(locationAliases, form, formDef);
                    }
                }
            } catch (Exception e) {
                Debug.logError(e, "Error creating forms from annotations in class " + formClass.getName(), module);
            }
        }

        utilTimer.timerString("Finished form annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "] - Total Forms: " + formCount + " FINISHED");
        Debug.logInfo("Loaded [" + formCount + "] Forms from annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]", module);

        return modelForms;
    }

    /**
     * Registers location aliases for a form based on the @Form annotation's location/locations attributes.
     */
    protected void registerLocationAliases(Map<String, Map<String, ModelForm>> locationAliases,
                                           ModelForm form, Form formDef) {
        // Single location alias
        if (UtilValidate.isNotEmpty(formDef.location())) {
            locationAliases
                    .computeIfAbsent(formDef.location(), k -> new LinkedHashMap<>())
                    .put(form.getName(), form);
        }
        // Multiple location aliases
        for (String location : formDef.locations()) {
            if (UtilValidate.isNotEmpty(location)) {
                locationAliases
                        .computeIfAbsent(location, k -> new LinkedHashMap<>())
                        .put(form.getName(), form);
            }
        }
    }

    /**
     * Helper class to hold a ModelForm with its annotation.
     */
    protected static class FormWithAnnotation {
        final ModelForm form;
        final Form annotation;

        FormWithAnnotation(ModelForm form, Form annotation) {
            this.form = form;
            this.annotation = annotation;
        }
    }

    /**
     * Reads @Form annotations from a class (may have multiple via @FormList).
     */
    protected List<ModelForm> readFormsFromClass(Class<?> formClass) throws ParserConfigurationException {
        List<FormWithAnnotation> formsWithAnnotations = readFormsWithAnnotationsFromClass(formClass);
        List<ModelForm> forms = new ArrayList<>();
        for (FormWithAnnotation fwa : formsWithAnnotations) {
            forms.add(fwa.form);
        }
        return forms;
    }

    /**
     * Reads @Form annotations from a class, returning both the ModelForm and the annotation.
     */
    protected List<FormWithAnnotation> readFormsWithAnnotationsFromClass(Class<?> formClass) throws ParserConfigurationException {
        List<FormWithAnnotation> forms = new ArrayList<>();
        String sourceLocation = "class://" + formClass.getName();

        // Check for @FormList (container for multiple @Form)
        FormList formList = formClass.getAnnotation(FormList.class);
        if (formList != null) {
            for (Form formDef : formList.value()) {
                ModelForm modelForm = createModelForm(formDef, formClass, sourceLocation);
                if (modelForm != null) {
                    forms.add(new FormWithAnnotation(modelForm, formDef));
                }
            }
        }

        // Check for single @Form annotation
        Form formDef = formClass.getAnnotation(Form.class);
        if (formDef != null) {
            ModelForm modelForm = createModelForm(formDef, formClass, sourceLocation);
            if (modelForm != null) {
                forms.add(new FormWithAnnotation(modelForm, formDef));
            }
        }

        return forms;
    }

    /**
     * Creates a ModelForm from a @Form annotation.
     */
    protected ModelForm createModelForm(Form formDef, Class<?> formClass, String sourceLocation)
            throws ParserConfigurationException {
        String formName = formDef.name();
        if (UtilValidate.isEmpty(formName)) {
            Debug.logWarning("Form annotation in class " + formClass.getName() +
                    " has no name, skipping", module);
            return null;
        }

        // Build synthetic XML document
        Document doc = buildFormDocument(formDef, formClass, sourceLocation);
        if (doc == null) {
            return null;
        }

        // Create ModelForm from the document using FormFactory
        return FormFactory.createModelForm(doc, entityModelReader, dispatchContext, sourceLocation, formName);
    }

    /**
     * Builds a synthetic XML document for a form, INCLUDING any same-document parent forms it
     * {@code extends} (transitively). Annotation-based forms are otherwise built one-per-document,
     * which broke {@link org.ofbiz.widget.model.ModelSingleForm}'s same-document {@code extends}
     * resolution (a {@code <form extends="X">} without {@code extends-resource} looks for {@code X}
     * among sibling forms in the same document). We inline the extends ancestry so it resolves.
     */
    protected Document buildFormDocument(Form formDef, Class<?> formClass, String sourceLocation)
            throws ParserConfigurationException {
        Document doc = buildSingleFormDocument(formDef, formClass, sourceLocation);
        if (doc == null) {
            return null;
        }
        Element formsElement = doc.getDocumentElement();
        java.util.Set<String> present = new java.util.HashSet<>();
        present.add(formDef.name());
        java.util.Deque<Form> queue = new java.util.ArrayDeque<>();
        queue.add(formDef);
        while (!queue.isEmpty()) {
            Form cur = queue.poll();
            // Only same-document extends (no extends-resource) need the parent inlined here.
            if (UtilValidate.isEmpty(cur.extendsForm()) || UtilValidate.isNotEmpty(cur.extendsResource())) {
                continue;
            }
            String parentName = cur.extendsForm();
            if (present.contains(parentName)) {
                continue;
            }
            present.add(parentName);
            Form parent = findFormInClass(formClass, parentName);
            if (parent == null) {
                // Parent defined elsewhere; ModelSingleForm logs if truly unresolvable.
                continue;
            }
            Document parentDoc = buildSingleFormDocument(parent, formClass, sourceLocation);
            if (parentDoc != null) {
                org.w3c.dom.NodeList parentForms = parentDoc.getDocumentElement().getElementsByTagName("form");
                if (parentForms.getLength() > 0) {
                    formsElement.appendChild(doc.importNode(parentForms.item(0), true));
                }
            }
            queue.add(parent);
        }
        return doc;
    }

    /**
     * Finds a sibling {@code @Form} annotation by name. Forms from one source XML are generated as
     * separate nested interfaces (each with its own {@code @Form}) inside a single container class,
     * so a same-document {@code extends} parent lives on a sibling nested interface. Search the
     * enclosing container class and ALL its nested {@code @Form} types (also handles {@code @FormList}).
     */
    protected Form findFormInClass(Class<?> formClass, String name) {
        Class<?> container = formClass.getEnclosingClass();
        if (container == null) {
            container = formClass;
        }
        Form f = matchFormByName(container, name);
        if (f != null) {
            return f;
        }
        for (Class<?> nested : container.getDeclaredClasses()) {
            f = matchFormByName(nested, name);
            if (f != null) {
                return f;
            }
        }
        return null;
    }

    private Form matchFormByName(Class<?> c, String name) {
        FormList formList = c.getAnnotation(FormList.class);
        if (formList != null) {
            for (Form f : formList.value()) {
                if (name.equals(f.name())) {
                    return f;
                }
            }
        }
        Form single = c.getAnnotation(Form.class);
        if (single != null && name.equals(single.name())) {
            return single;
        }
        return null;
    }

    /**
     * Builds a synthetic XML document representing a single form definition (no extends inlining).
     */
    protected Document buildSingleFormDocument(Form formDef, Class<?> formClass, String sourceLocation)
            throws ParserConfigurationException {
        // SCIPIO: 4.0.0: Reuse the per-thread DocumentBuilder instead of creating a new
        // DocumentBuilderFactory/DocumentBuilder for every single form (see field javadoc above).
        DocumentBuilder builder = THREAD_LOCAL_DOCUMENT_BUILDER.get();
        Document doc = builder.newDocument();

        // Root <forms> element
        Element formsElement = doc.createElement("forms");
        doc.appendChild(formsElement);

        // <form> element
        Element formElement = doc.createElement("form");
        formElement.setAttribute("name", formDef.name());

        // Type attribute
        String typeValue = formDef.type().getXmlValue();
        if (UtilValidate.isNotEmpty(typeValue)) {
            formElement.setAttribute("type", typeValue);
        }

        // Basic attributes
        setAttrIfNotEmpty(formElement, "target", formDef.target());
        setAttrIfNotEmpty(formElement, "target-window", formDef.targetWindow());
        if (formDef.targetType() != TargetType.INTRA_APP) {
            formElement.setAttribute("target-type", formDef.targetType().getXmlValue());
        }
        setAttrIfNotEmpty(formElement, "id", formDef.id());
        setAttrIfNotEmpty(formElement, "style", formDef.style());
        setAttrIfNotEmpty(formElement, "focus-field-name", formDef.focusFieldName());
        setAttrIfNotEmpty(formElement, "title", formDef.title());
        setAttrIfNotEmpty(formElement, "empty-form-data-message", formDef.emptyFormDataMessage());
        setAttrIfNotEmpty(formElement, "tooltip", formDef.tooltip());
        setAttrIfNotEmpty(formElement, "list-name", formDef.listName());
        setAttrIfNotEmpty(formElement, "list-entry-name", formDef.listEntryName());
        setAttrIfNotEmpty(formElement, "default-map-name", formDef.defaultMapName());
        setAttrIfNotEmpty(formElement, "default-entity-name", formDef.defaultEntityName());
        setAttrIfNotEmpty(formElement, "default-service-name", formDef.defaultServiceName());
        setAttrIfNotEmpty(formElement, "extends", formDef.extendsForm());
        setAttrIfNotEmpty(formElement, "extends-resource", formDef.extendsResource());

        // Pagination attributes
        setAttrIfNotEmpty(formElement, "paginate", formDef.paginate());
        setAttrIfNotEmpty(formElement, "paginate-target", formDef.paginateTarget());
        if (!"viewSize".equals(formDef.paginateSizeField())) {
            formElement.setAttribute("paginate-size-field", formDef.paginateSizeField());
        }
        if (!"viewIndex".equals(formDef.paginateIndexField())) {
            formElement.setAttribute("paginate-index-field", formDef.paginateIndexField());
        }
        setAttrIfNotEmpty(formElement, "paginate-first-label", formDef.paginateFirstLabel());
        setAttrIfNotEmpty(formElement, "paginate-previous-label", formDef.paginatePreviousLabel());
        setAttrIfNotEmpty(formElement, "paginate-next-label", formDef.paginateNextLabel());
        setAttrIfNotEmpty(formElement, "paginate-last-label", formDef.paginateLastLabel());
        setAttrIfNotEmpty(formElement, "paginate-view-size-label", formDef.paginateViewSizeLabel());
        setAttrIfNotEmpty(formElement, "paginate-style", formDef.paginateStyle());
        setAttrIfNotEmpty(formElement, "paginate-target-anchor", formDef.paginateTargetAnchor());
        setAttrIfNotEmpty(formElement, "override-list-size", formDef.overrideListSize());
        setAttrIfNotEmpty(formElement, "item-index-separator", formDef.itemIndexSeparator());
        if (formDef.viewSize() != 0) {
            formElement.setAttribute("view-size", String.valueOf(formDef.viewSize()));
        }
        setAttrIfNotEmpty(formElement, "row-count", formDef.rowCount());

        // Style attributes
        setAttrIfNotEmpty(formElement, "header-row-style", formDef.headerRowStyle());
        setAttrIfNotEmpty(formElement, "odd-row-style", formDef.oddRowStyle());
        setAttrIfNotEmpty(formElement, "even-row-style", formDef.evenRowStyle());
        setAttrIfNotEmpty(formElement, "default-table-style", formDef.defaultTableStyle());
        setAttrIfNotEmpty(formElement, "default-title-style", formDef.defaultTitleStyle());
        setAttrIfNotEmpty(formElement, "default-widget-style", formDef.defaultWidgetStyle());
        setAttrIfNotEmpty(formElement, "default-tooltip-style", formDef.defaultTooltipStyle());
        setAttrIfNotEmpty(formElement, "default-title-area-style", formDef.defaultTitleAreaStyle());
        setAttrIfNotEmpty(formElement, "default-widget-area-style", formDef.defaultWidgetAreaStyle());
        setAttrIfNotEmpty(formElement, "form-title-area-style", formDef.formTitleAreaStyle());
        setAttrIfNotEmpty(formElement, "form-widget-area-style", formDef.formWidgetAreaStyle());
        setAttrIfNotEmpty(formElement, "default-required-field-style", formDef.defaultRequiredFieldStyle());
        setAttrIfNotEmpty(formElement, "sort-field-parameter-name", formDef.sortFieldParameterName());
        setAttrIfNotEmpty(formElement, "default-sort-field-style", formDef.defaultSortFieldStyle());
        setAttrIfNotEmpty(formElement, "default-sort-field-asc-style", formDef.defaultSortFieldAscStyle());
        setAttrIfNotEmpty(formElement, "default-sort-field-desc-style", formDef.defaultSortFieldDescStyle());

        // Behavior attributes
        if (!formDef.clientAutocompleteFields()) {
            formElement.setAttribute("client-autocomplete-fields", "false");
        }
        if (formDef.separateColumns()) {
            formElement.setAttribute("separate-columns", "true");
        }
        if (!formDef.groupColumns()) {
            formElement.setAttribute("group-columns", "false");
        }
        if (formDef.hideHeader()) {
            formElement.setAttribute("hide-header", "true");
        }
        if (formDef.useRowSubmit()) {
            formElement.setAttribute("use-row-submit", "true");
        }
        setAttrIfNotEmpty(formElement, "skip-start", formDef.skipStart());
        setAttrIfNotEmpty(formElement, "skip-end", formDef.skipEnd());
        setAttrIfNotEmpty(formElement, "use-request-parameters", formDef.useRequestParameters());

        // SCIPIO-specific attributes
        setAttrIfNotEmpty(formElement, "method", formDef.method());
        setAttrIfNotEmpty(formElement, "attribs", formDef.attribs());
        if (formDef.defaultPositionSpan() != 0) {
            formElement.setAttribute("default-position-span", String.valueOf(formDef.defaultPositionSpan()));
        }
        setAttrIfNotEmpty(formElement, "hide-header-when", formDef.hideHeaderWhen());
        setAttrIfNotEmpty(formElement, "hide-table-when", formDef.hideTableWhen());
        setAttrIfNotEmpty(formElement, "use-alternate-text-when", formDef.useAlternateTextWhen());
        setAttrIfNotEmpty(formElement, "alternate-text", formDef.alternateText());
        setAttrIfNotEmpty(formElement, "alternate-text-style", formDef.alternateTextStyle());
        if (formDef.positions() != 0) {
            formElement.setAttribute("positions", String.valueOf(formDef.positions()));
        }
        if (!formDef.defaultCombineActionFields()) {
            formElement.setAttribute("default-combine-action-fields", "false");
        }

        formsElement.appendChild(formElement);

        // Build actions
        if (!formDef.actions().UNSET()) {
            Element actionsElement = buildActionsElement(doc, formDef.actions());
            if (actionsElement != null && actionsElement.hasChildNodes()) {
                formElement.appendChild(actionsElement);
            }
        }

        // Build row-actions for list/multi forms
        if (!formDef.rowActions().UNSET()) {
            Element rowActionsElement = buildRowActionsElement(doc, formDef.rowActions());
            if (rowActionsElement != null && rowActionsElement.hasChildNodes()) {
                formElement.appendChild(rowActionsElement);
            }
        }

        // Build alt-targets
        for (AltTarget altTarget : formDef.altTargets()) {
            addAltTargetElement(doc, formElement, altTarget);
        }

        // Build auto-fields-service elements
        for (AutoFieldsService afs : formDef.autoFieldsService()) {
            addAutoFieldsServiceElement(doc, formElement, afs);
        }

        // Build auto-fields-entity elements
        for (AutoFieldsEntity afe : formDef.autoFieldsEntity()) {
            addAutoFieldsEntityElement(doc, formElement, afe);
        }

        // Build field elements
        for (FormField field : formDef.fields()) {
            addFieldElement(doc, formElement, field);
        }

        // Build sort-order
        if (!formDef.sortOrder().UNSET()) {
            Element sortOrderElement = buildSortOrderElement(doc, formDef.sortOrder());
            if (sortOrderElement != null && sortOrderElement.hasChildNodes()) {
                formElement.appendChild(sortOrderElement);
            }
        }

        return doc;
    }

    /**
     * Builds the actions element from FormActions annotation.
     */
    protected Element buildActionsElement(Document doc, FormActions actions) {
        Element actionsElement = doc.createElement("actions");

        for (SetAction setAction : actions.set()) {
            addSetActionElement(doc, actionsElement, setAction);
        }
        for (ServiceAction serviceAction : actions.service()) {
            addServiceActionElement(doc, actionsElement, serviceAction);
        }
        for (EntityOneAction entityOne : actions.entityOne()) {
            addEntityOneActionElement(doc, actionsElement, entityOne);
        }
        for (EntityConditionAction entityCondition : actions.entityCondition()) {
            addEntityConditionActionElement(doc, actionsElement, entityCondition);
        }
        for (ScriptAction script : actions.script()) {
            addScriptActionElement(doc, actionsElement, script);
        }
        for (PropertyToFieldAction prop : actions.propertyToField()) {
            addPropertyToFieldActionElement(doc, actionsElement, prop);
        }

        return actionsElement;
    }

    /**
     * Builds the row-actions element from RowActions annotation.
     */
    protected Element buildRowActionsElement(Document doc, RowActions rowActions) {
        Element rowActionsElement = doc.createElement("row-actions");

        for (SetAction setAction : rowActions.set()) {
            addSetActionElement(doc, rowActionsElement, setAction);
        }
        for (ServiceAction serviceAction : rowActions.service()) {
            addServiceActionElement(doc, rowActionsElement, serviceAction);
        }
        for (EntityOneAction entityOne : rowActions.entityOne()) {
            addEntityOneActionElement(doc, rowActionsElement, entityOne);
        }
        for (EntityConditionAction entityCondition : rowActions.entityCondition()) {
            addEntityConditionActionElement(doc, rowActionsElement, entityCondition);
        }
        for (ScriptAction script : rowActions.script()) {
            addScriptActionElement(doc, rowActionsElement, script);
        }
        for (PropertyToFieldAction prop : rowActions.propertyToField()) {
            addPropertyToFieldActionElement(doc, rowActionsElement, prop);
        }

        return rowActionsElement;
    }

    /**
     * Builds the sort-order element from SortOrder annotation.
     */
    protected Element buildSortOrderElement(Document doc, SortOrder sortOrder) {
        Element sortOrderElement = doc.createElement("sort-order");

        // Add field groups
        for (FieldGroup fg : sortOrder.fieldGroups()) {
            Element fgElement = doc.createElement("field-group");
            setAttrIfNotEmpty(fgElement, "id", fg.id());
            setAttrIfNotEmpty(fgElement, "title", fg.title());
            setAttrIfNotEmpty(fgElement, "style", fg.style());
            if (fg.collapsible()) {
                fgElement.setAttribute("collapsible", "true");
            }
            if (fg.initiallyCollapsed()) {
                fgElement.setAttribute("initially-collapsed", "true");
            }
            sortOrderElement.appendChild(fgElement);
        }

        // Add sort fields
        for (SortField sf : sortOrder.sortFields()) {
            Element sfElement = doc.createElement("sort-field");
            sfElement.setAttribute("name", sf.name());
            if (sf.position() != 0) {
                sfElement.setAttribute("position", String.valueOf(sf.position()));
            }
            sortOrderElement.appendChild(sfElement);
        }

        // Add last fields
        for (LastField lf : sortOrder.lastFields()) {
            Element lfElement = doc.createElement("last-field");
            lfElement.setAttribute("name", lf.name());
            sortOrderElement.appendChild(lfElement);
        }

        // Add banners
        for (Banner banner : sortOrder.banners()) {
            addBannerElement(doc, sortOrderElement, banner);
        }

        return sortOrderElement;
    }

    /**
     * Adds a field element.
     */
    protected void addFieldElement(Document doc, Element formElement, FormField field) {
        Element fieldElement = doc.createElement("field");
        fieldElement.setAttribute("name", field.name());

        // Basic field attributes
        setAttrIfNotEmpty(fieldElement, "map-name", field.mapName());
        setAttrIfNotEmpty(fieldElement, "entry-name", field.entryName());
        setAttrIfNotEmpty(fieldElement, "field-name", field.fieldName()); // SCIPIO: 4.0.0: was never emitted (entity field for display-entity/type derivation)
        setAttrIfNotEmpty(fieldElement, "title", field.title());
        setAttrIfNotEmpty(fieldElement, "tooltip", field.tooltip());
        setAttrIfNotEmpty(fieldElement, "use-when", field.useWhen());
        setAttrIfNotEmpty(fieldElement, "ignore-when", field.ignoreWhen());
        setAttrIfNotEmpty(fieldElement, "red-when", field.redWhen());
        setAttrIfNotEmpty(fieldElement, "id-name", field.idName());
        setAttrIfNotEmpty(fieldElement, "title-style", field.titleStyle());
        setAttrIfNotEmpty(fieldElement, "widget-style", field.widgetStyle());
        setAttrIfNotEmpty(fieldElement, "tooltip-style", field.tooltipStyle());
        setAttrIfNotEmpty(fieldElement, "title-area-style", field.titleAreaStyle());
        setAttrIfNotEmpty(fieldElement, "widget-area-style", field.widgetAreaStyle());
        setAttrIfNotEmpty(fieldElement, "required-field-style", field.requiredFieldStyle());
        setAttrIfNotEmpty(fieldElement, "sort-field-style", field.sortFieldStyle());
        setAttrIfNotEmpty(fieldElement, "sort-field-asc-style", field.sortFieldAscStyle());
        setAttrIfNotEmpty(fieldElement, "sort-field-desc-style", field.sortFieldDescStyle());

        if (field.position() != 1) {
            fieldElement.setAttribute("position", String.valueOf(field.position()));
        }
        if (field.positionSpan() != 0) {
            fieldElement.setAttribute("position-span", String.valueOf(field.positionSpan()));
        }

        if (field.separateColumn()) {
            fieldElement.setAttribute("separate-column", "true");
        }
        if (!field.encodeOutput()) {
            fieldElement.setAttribute("encode-output", "false");
        }
        if (field.requiredField()) {
            fieldElement.setAttribute("required-field", "true");
        }
        if (field.sortField()) {
            fieldElement.setAttribute("sort-field", "true");
        }
        if (field.disabled()) {
            fieldElement.setAttribute("disabled", "true");
        }

        setAttrIfNotEmpty(fieldElement, "attribute-name", field.attributeName());
        setAttrIfNotEmpty(fieldElement, "parameter-name", field.parameterName());
        setAttrIfNotEmpty(fieldElement, "header-link", field.headerLink());
        setAttrIfNotEmpty(fieldElement, "header-link-style", field.headerLinkStyle());
        setAttrIfNotEmpty(fieldElement, "event", field.event());
        setAttrIfNotEmpty(fieldElement, "action", field.action());

        // Add field type element
        addFieldTypeElement(doc, fieldElement, field);

        formElement.appendChild(fieldElement);
    }

    /**
     * Adds the appropriate field type element based on which type is set.
     */
    protected void addFieldTypeElement(Document doc, Element fieldElement, FormField field) {
        // Check each field type and add the appropriate element
        if (!field.text().UNSET()) {
            addTextFieldElement(doc, fieldElement, field.text());
        } else if (!field.textarea().UNSET()) {
            addTextareaFieldElement(doc, fieldElement, field.textarea());
        } else if (!field.password().UNSET()) {
            addPasswordFieldElement(doc, fieldElement, field.password());
        } else if (!field.dropDown().UNSET()) {
            addDropDownFieldElement(doc, fieldElement, field.dropDown());
        } else if (!field.check().UNSET()) {
            addCheckFieldElement(doc, fieldElement, field.check());
        } else if (!field.radio().UNSET()) {
            addRadioFieldElement(doc, fieldElement, field.radio());
        } else if (!field.dateTime().UNSET()) {
            addDateTimeFieldElement(doc, fieldElement, field.dateTime());
        } else if (!field.dateFind().UNSET()) {
            addDateFindFieldElement(doc, fieldElement, field.dateFind());
        } else if (!field.display().UNSET()) {
            addDisplayFieldElement(doc, fieldElement, field.display());
        } else if (!field.displayEntity().UNSET()) {
            addDisplayEntityFieldElement(doc, fieldElement, field.displayEntity());
        } else if (!field.hidden().UNSET()) {
            addHiddenFieldElement(doc, fieldElement, field.hidden());
        } else if (!field.ignored().UNSET()) {
            addIgnoredFieldElement(doc, fieldElement);
        } else if (!field.hyperlink().UNSET()) {
            addHyperlinkFieldElement(doc, fieldElement, field.hyperlink());
        } else if (!field.submit().UNSET()) {
            addSubmitFieldElement(doc, fieldElement, field.submit());
        } else if (!field.reset().UNSET()) {
            addResetFieldElement(doc, fieldElement);
        } else if (!field.lookup().UNSET()) {
            addLookupFieldElement(doc, fieldElement, field.lookup());
        } else if (!field.file().UNSET()) {
            addFileFieldElement(doc, fieldElement, field.file());
        } else if (!field.image().UNSET()) {
            addImageFieldElement(doc, fieldElement, field.image());
        } else if (!field.textFind().UNSET()) {
            addTextFindFieldElement(doc, fieldElement, field.textFind());
        } else if (!field.rangeFind().UNSET()) {
            addRangeFindFieldElement(doc, fieldElement, field.rangeFind());
        } else if (!field.container().UNSET()) {
            addContainerFieldElement(doc, fieldElement, field.container());
        } else if (!field.includeScreen().UNSET()) {
            addIncludeScreenFieldElement(doc, fieldElement, field.includeScreen());
        } else if (!field.includeForm().UNSET()) {
            addIncludeFormFieldElement(doc, fieldElement, field.includeForm());
        } else if (!field.includeMenu().UNSET()) {
            addIncludeMenuFieldElement(doc, fieldElement, field.includeMenu());
        } else if (!field.includeGrid().UNSET()) {
            addIncludeGridFieldElement(doc, fieldElement, field.includeGrid());
        }
        // If no field type is set, it defaults to display in the XML
    }

    // Simplified field type element methods

    protected void addTextFieldElement(Document doc, Element fieldElement, TextField textField) {
        Element textElement = doc.createElement("text");
        setAttrIfNotEmpty(textElement, "default-value", textField.defaultValue());
        if (textField.size() > 0) {
            textElement.setAttribute("size", String.valueOf(textField.size()));
        }
        if (textField.maxlength() > 0) {
            textElement.setAttribute("maxlength", String.valueOf(textField.maxlength()));
        }
        setAttrIfNotEmpty(textElement, "placeholder", textField.placeholder());
        setAttrIfNotEmpty(textElement, "mask", textField.mask());
        if (textField.disabled()) {
            textElement.setAttribute("disabled", "true");
        }
        if (!textField.subHyperlink().UNSET()) {
            addSubHyperlinkElement(doc, textElement, textField.subHyperlink());
        }
        fieldElement.appendChild(textElement);
    }

    protected void addTextareaFieldElement(Document doc, Element fieldElement, TextareaField textarea) {
        Element textareaElement = doc.createElement("textarea");
        setAttrIfNotEmpty(textareaElement, "default-value", textarea.defaultValue());
        if (textarea.cols() > 0) {
            textareaElement.setAttribute("cols", String.valueOf(textarea.cols()));
        }
        if (textarea.rows() > 0) {
            textareaElement.setAttribute("rows", String.valueOf(textarea.rows()));
        }
        if (textarea.maxlength() > 0) {
            textareaElement.setAttribute("maxlength", String.valueOf(textarea.maxlength()));
        }
        fieldElement.appendChild(textareaElement);
    }

    protected void addPasswordFieldElement(Document doc, Element fieldElement, PasswordField password) {
        Element passwordElement = doc.createElement("password");
        if (password.size() > 0) {
            passwordElement.setAttribute("size", String.valueOf(password.size()));
        }
        if (password.maxlength() > 0) {
            passwordElement.setAttribute("maxlength", String.valueOf(password.maxlength()));
        }
        fieldElement.appendChild(passwordElement);
    }

    protected void addDropDownFieldElement(Document doc, Element fieldElement, DropDownField dropDown) {
        Element dropDownElement = doc.createElement("drop-down");
        setAttrIfNotEmpty(dropDownElement, "current", dropDown.current());
        setAttrIfNotEmpty(dropDownElement, "current-description", dropDown.currentDescription());
        if (dropDown.allowEmpty()) {
            dropDownElement.setAttribute("allow-empty", "true");
        }
        if (dropDown.allowMulti()) {
            dropDownElement.setAttribute("allow-multiple", "true");
        }
        if (dropDown.size() > 0) {
            dropDownElement.setAttribute("size", String.valueOf(dropDown.size()));
        }
        if (dropDown.otherFieldSize() > 0) {
            dropDownElement.setAttribute("other-field-size", String.valueOf(dropDown.otherFieldSize()));
        }
        setAttrIfNotEmpty(dropDownElement, "no-current-selected-key", dropDown.noCurrentSelectedKey());

        // Add options
        for (Option option : dropDown.options()) {
            addOptionElement(doc, dropDownElement, option);
        }

        // Add entity-options
        if (!dropDown.entityOptions().UNSET()) {
            addEntityOptionsElement(doc, dropDownElement, dropDown.entityOptions());
        }

        // Add list-options
        if (!dropDown.listOptions().UNSET()) {
            addListOptionsElement(doc, dropDownElement, dropDown.listOptions());
        }

        // Add sub-hyperlink
        if (!dropDown.subHyperlink().UNSET()) {
            addSubHyperlinkElement(doc, dropDownElement, dropDown.subHyperlink());
        }

        fieldElement.appendChild(dropDownElement);
    }

    protected void addCheckFieldElement(Document doc, Element fieldElement, CheckField check) {
        Element checkElement = doc.createElement("check");
        if (check.disabled()) {
            checkElement.setAttribute("disabled", "true");
        }

        // Add options
        for (Option option : check.options()) {
            addOptionElement(doc, checkElement, option);
        }

        // Add entity-options
        if (!check.entityOptions().UNSET()) {
            addEntityOptionsElement(doc, checkElement, check.entityOptions());
        }

        // Add list-options
        if (!check.listOptions().UNSET()) {
            addListOptionsElement(doc, checkElement, check.listOptions());
        }

        fieldElement.appendChild(checkElement);
    }

    protected void addRadioFieldElement(Document doc, Element fieldElement, RadioField radio) {
        Element radioElement = doc.createElement("radio");
        setAttrIfNotEmpty(radioElement, "no-current-selected-key", radio.noCurrentSelectedKey());

        // Add options
        for (Option option : radio.options()) {
            addOptionElement(doc, radioElement, option);
        }

        // Add entity-options
        if (!radio.entityOptions().UNSET()) {
            addEntityOptionsElement(doc, radioElement, radio.entityOptions());
        }

        // Add list-options
        if (!radio.listOptions().UNSET()) {
            addListOptionsElement(doc, radioElement, radio.listOptions());
        }

        fieldElement.appendChild(radioElement);
    }

    protected void addDateTimeFieldElement(Document doc, Element fieldElement, DateTimeField dateTime) {
        Element dateTimeElement = doc.createElement("date-time");
        setAttrIfNotEmpty(dateTimeElement, "type", dateTime.type());
        setAttrIfNotEmpty(dateTimeElement, "default-value", dateTime.defaultValue());
        setAttrIfNotEmpty(dateTimeElement, "input-method", dateTime.inputMethod());
        setAttrIfNotEmpty(dateTimeElement, "clock", dateTime.clock());
        setAttrIfNotEmpty(dateTimeElement, "step", dateTime.step());
        setAttrIfNotEmpty(dateTimeElement, "mask", dateTime.mask());
        fieldElement.appendChild(dateTimeElement);
    }

    protected void addDateFindFieldElement(Document doc, Element fieldElement, DateFindField dateFind) {
        Element dateFindElement = doc.createElement("date-find");
        setAttrIfNotEmpty(dateFindElement, "type", dateFind.type());
        setAttrIfNotEmpty(dateFindElement, "default-value", dateFind.defaultValue());
        fieldElement.appendChild(dateFindElement);
    }

    protected void addDisplayFieldElement(Document doc, Element fieldElement, DisplayField display) {
        Element displayElement = doc.createElement("display");
        setAttrIfNotEmpty(displayElement, "type", display.type());
        setAttrIfNotEmpty(displayElement, "default-value", display.defaultValue());
        if (display.alsoHidden()) {
            displayElement.setAttribute("also-hidden", "true");
        }
        setAttrIfNotEmpty(displayElement, "description", display.description());
        setAttrIfNotEmpty(displayElement, "currency", display.currency());
        fieldElement.appendChild(displayElement);
    }

    protected void addDisplayEntityFieldElement(Document doc, Element fieldElement, DisplayEntityField displayEntity) {
        Element displayEntityElement = doc.createElement("display-entity");
        setAttrIfNotEmpty(displayEntityElement, "entity-name", displayEntity.entityName());
        setAttrIfNotEmpty(displayEntityElement, "key-field-name", displayEntity.keyFieldName());
        setAttrIfNotEmpty(displayEntityElement, "description", displayEntity.description());
        if (displayEntity.alsoHidden()) {
            displayEntityElement.setAttribute("also-hidden", "true");
        }
        if (displayEntity.cache()) {
            displayEntityElement.setAttribute("cache", "true");
        }

        // Add sub-hyperlink
        if (!displayEntity.subHyperlink().UNSET()) {
            addSubHyperlinkElement(doc, displayEntityElement, displayEntity.subHyperlink());
        }

        fieldElement.appendChild(displayEntityElement);
    }

    protected void addHiddenFieldElement(Document doc, Element fieldElement, HiddenField hidden) {
        Element hiddenElement = doc.createElement("hidden");
        setAttrIfNotEmpty(hiddenElement, "value", hidden.value());
        fieldElement.appendChild(hiddenElement);
    }

    protected void addIgnoredFieldElement(Document doc, Element fieldElement) {
        Element ignoredElement = doc.createElement("ignored");
        fieldElement.appendChild(ignoredElement);
    }

    protected void addHyperlinkFieldElement(Document doc, Element fieldElement, HyperlinkField hyperlink) {
        Element hyperlinkElement = doc.createElement("hyperlink");
        setAttrIfNotEmpty(hyperlinkElement, "target", hyperlink.target());
        setAttrIfNotEmpty(hyperlinkElement, "target-window", hyperlink.targetWindow());
        setAttrIfNotEmpty(hyperlinkElement, "description", hyperlink.description());
        setAttrIfNotEmpty(hyperlinkElement, "style", hyperlink.style());
        if (hyperlink.alsoHidden()) {
            hyperlinkElement.setAttribute("also-hidden", "true");
        }

        // Add parameters
        for (ParameterDef param : hyperlink.parameters()) {
            addParameterElement(doc, hyperlinkElement, param);
        }

        fieldElement.appendChild(hyperlinkElement);
    }

    protected void addSubmitFieldElement(Document doc, Element fieldElement, SubmitField submit) {
        Element submitElement = doc.createElement("submit");
        setAttrIfNotEmpty(submitElement, "button-type", submit.buttonType());
        fieldElement.appendChild(submitElement);
    }

    protected void addResetFieldElement(Document doc, Element fieldElement) {
        Element resetElement = doc.createElement("reset");
        fieldElement.appendChild(resetElement);
    }

    protected void addLookupFieldElement(Document doc, Element fieldElement, LookupField lookup) {
        Element lookupElement = doc.createElement("lookup");
        setAttrIfNotEmpty(lookupElement, "target-form-name", lookup.targetFormName());
        setAttrIfNotEmpty(lookupElement, "description-field-name", lookup.descriptionFieldName());
        setAttrIfNotEmpty(lookupElement, "presentation", lookup.presentation());
        if (lookup.size() > 0) {
            lookupElement.setAttribute("size", String.valueOf(lookup.size()));
        }
        if (lookup.maxlength() > 0) {
            lookupElement.setAttribute("maxlength", String.valueOf(lookup.maxlength()));
        }
        setAttrIfNotEmpty(lookupElement, "default-value", lookup.defaultValue());

        // Add sub-hyperlink
        if (!lookup.subHyperlink().UNSET()) {
            addSubHyperlinkElement(doc, lookupElement, lookup.subHyperlink());
        }

        fieldElement.appendChild(lookupElement);
    }

    protected void addFileFieldElement(Document doc, Element fieldElement, FileField file) {
        Element fileElement = doc.createElement("file");
        if (file.size() > 0) {
            fileElement.setAttribute("size", String.valueOf(file.size()));
        }
        if (file.maxlength() > 0) {
            fileElement.setAttribute("maxlength", String.valueOf(file.maxlength()));
        }

        // Add sub-hyperlink
        if (!file.subHyperlink().UNSET()) {
            addSubHyperlinkElement(doc, fileElement, file.subHyperlink());
        }

        fieldElement.appendChild(fileElement);
    }

    protected void addImageFieldElement(Document doc, Element fieldElement, ImageField image) {
        Element imageElement = doc.createElement("image");
        setAttrIfNotEmpty(imageElement, "value", image.value());
        setAttrIfNotEmpty(imageElement, "default-value", image.defaultValue());
        setAttrIfNotEmpty(imageElement, "style", image.style());
        setAttrIfNotEmpty(imageElement, "description", image.description());
        setAttrIfNotEmpty(imageElement, "alternate", image.alternate());

        // Add sub-hyperlink
        if (!image.subHyperlink().UNSET()) {
            addSubHyperlinkElement(doc, imageElement, image.subHyperlink());
        }

        fieldElement.appendChild(imageElement);
    }

    protected void addTextFindFieldElement(Document doc, Element fieldElement, TextFindField textFind) {
        Element textFindElement = doc.createElement("text-find");
        setAttrIfNotEmpty(textFindElement, "default-value", textFind.defaultValue());
        setAttrIfNotEmpty(textFindElement, "default-option", textFind.defaultOption());
        if (textFind.size() > 0) {
            textFindElement.setAttribute("size", String.valueOf(textFind.size()));
        }
        if (textFind.maxlength() > 0) {
            textFindElement.setAttribute("maxlength", String.valueOf(textFind.maxlength()));
        }
        if (textFind.ignoreCase()) {
            textFindElement.setAttribute("ignore-case", "true");
        }
        fieldElement.appendChild(textFindElement);
    }

    protected void addRangeFindFieldElement(Document doc, Element fieldElement, RangeFindField rangeFind) {
        Element rangeFindElement = doc.createElement("range-find");
        setAttrIfNotEmpty(rangeFindElement, "default-value", rangeFind.defaultValue());
        if (rangeFind.size() > 0) {
            rangeFindElement.setAttribute("size", String.valueOf(rangeFind.size()));
        }
        if (rangeFind.maxlength() > 0) {
            rangeFindElement.setAttribute("maxlength", String.valueOf(rangeFind.maxlength()));
        }
        fieldElement.appendChild(rangeFindElement);
    }

    protected void addContainerFieldElement(Document doc, Element fieldElement, ContainerField container) {
        Element containerElement = doc.createElement("container");
        fieldElement.appendChild(containerElement);
    }

    protected void addIncludeScreenFieldElement(Document doc, Element fieldElement, IncludeScreenField includeScreen) {
        Element includeScreenElement = doc.createElement("include-screen");
        setAttrIfNotEmpty(includeScreenElement, "name", includeScreen.name());
        setAttrIfNotEmpty(includeScreenElement, "location", includeScreen.location());
        fieldElement.appendChild(includeScreenElement);
    }

    protected void addIncludeFormFieldElement(Document doc, Element fieldElement, IncludeFormField includeForm) {
        Element includeFormElement = doc.createElement("include-form");
        setAttrIfNotEmpty(includeFormElement, "name", includeForm.name());
        setAttrIfNotEmpty(includeFormElement, "location", includeForm.location());
        fieldElement.appendChild(includeFormElement);
    }

    protected void addIncludeMenuFieldElement(Document doc, Element fieldElement, IncludeMenuField includeMenu) {
        Element includeMenuElement = doc.createElement("include-menu");
        setAttrIfNotEmpty(includeMenuElement, "name", includeMenu.name());
        setAttrIfNotEmpty(includeMenuElement, "location", includeMenu.location());
        fieldElement.appendChild(includeMenuElement);
    }

    protected void addIncludeGridFieldElement(Document doc, Element fieldElement, IncludeGridField includeGrid) {
        Element includeGridElement = doc.createElement("include-grid");
        setAttrIfNotEmpty(includeGridElement, "name", includeGrid.name());
        setAttrIfNotEmpty(includeGridElement, "location", includeGrid.location());
        fieldElement.appendChild(includeGridElement);
    }

    // Helper methods for options, entity-options, list-options, etc.

    protected void addOptionElement(Document doc, Element parentElement, Option option) {
        Element optionElement = doc.createElement("option");
        optionElement.setAttribute("key", option.key());
        setAttrIfNotEmpty(optionElement, "description", option.description());
        parentElement.appendChild(optionElement);
    }

    protected void addEntityOptionsElement(Document doc, Element parentElement, EntityOptions entityOptions) {
        Element entityOptionsElement = doc.createElement("entity-options");
        setAttrIfNotEmpty(entityOptionsElement, "entity-name", entityOptions.entityName());
        setAttrIfNotEmpty(entityOptionsElement, "description", entityOptions.description());
        setAttrIfNotEmpty(entityOptionsElement, "key-field-name", entityOptions.keyFieldName());
        if (entityOptions.cache()) {
            entityOptionsElement.setAttribute("cache", "true");
        }

        // Add entity-constraint elements
        for (EntityConstraint constraint : entityOptions.constraints()) {
            addEntityConstraintElement(doc, entityOptionsElement, constraint);
        }

        // Add entity-order-by elements
        for (EntityOrderBy orderBy : entityOptions.orderBy()) {
            addEntityOrderByElement(doc, entityOptionsElement, orderBy);
        }

        parentElement.appendChild(entityOptionsElement);
    }

    protected void addEntityConstraintElement(Document doc, Element parentElement, EntityConstraint constraint) {
        Element constraintElement = doc.createElement("entity-constraint");
        setAttrIfNotEmpty(constraintElement, "name", constraint.name());
        setAttrIfNotEmpty(constraintElement, "value", constraint.value());
        setAttrIfNotEmpty(constraintElement, "operator", constraint.operator());
        setAttrIfNotEmpty(constraintElement, "env-name", constraint.envName());
        parentElement.appendChild(constraintElement);
    }

    protected void addEntityOrderByElement(Document doc, Element parentElement, EntityOrderBy orderBy) {
        Element orderByElement = doc.createElement("entity-order-by");
        orderByElement.setAttribute("field-name", orderBy.fieldName());
        parentElement.appendChild(orderByElement);
    }

    protected void addListOptionsElement(Document doc, Element parentElement, ListOptions listOptions) {
        Element listOptionsElement = doc.createElement("list-options");
        setAttrIfNotEmpty(listOptionsElement, "list-name", listOptions.listName());
        setAttrIfNotEmpty(listOptionsElement, "list-entry-name", listOptions.listEntryName());
        setAttrIfNotEmpty(listOptionsElement, "key-name", listOptions.keyName());
        setAttrIfNotEmpty(listOptionsElement, "description", listOptions.description());
        parentElement.appendChild(listOptionsElement);
    }

    protected void addSubHyperlinkElement(Document doc, Element parentElement, SubHyperlink subHyperlink) {
        Element subHyperlinkElement = doc.createElement("sub-hyperlink");
        setAttrIfNotEmpty(subHyperlinkElement, "target", subHyperlink.target());
        setAttrIfNotEmpty(subHyperlinkElement, "target-window", subHyperlink.targetWindow());
        setAttrIfNotEmpty(subHyperlinkElement, "description", subHyperlink.description());
        setAttrIfNotEmpty(subHyperlinkElement, "style", subHyperlink.style());

        // Add parameters
        for (ParameterDef param : subHyperlink.parameters()) {
            addParameterElement(doc, subHyperlinkElement, param);
        }

        parentElement.appendChild(subHyperlinkElement);
    }

    protected void addParameterElement(Document doc, Element parentElement, ParameterDef param) {
        Element paramElement = doc.createElement("parameter");
        paramElement.setAttribute("param-name", param.paramName());
        setAttrIfNotEmpty(paramElement, "from-field", param.fromField());
        setAttrIfNotEmpty(paramElement, "value", param.value());
        parentElement.appendChild(paramElement);
    }

    // Auto-fields methods

    protected void addAutoFieldsServiceElement(Document doc, Element formElement, AutoFieldsService afs) {
        Element afsElement = doc.createElement("auto-fields-service");
        afsElement.setAttribute("service-name", afs.serviceName());
        if (afs.defaultFieldType() != DefaultFieldType.EDIT) {
            afsElement.setAttribute("default-field-type", afs.defaultFieldType().getXmlValue());
        }
        setAttrIfNotEmpty(afsElement, "map-name", afs.mapName());
        formElement.appendChild(afsElement);
    }

    protected void addAutoFieldsEntityElement(Document doc, Element formElement, AutoFieldsEntity afe) {
        Element afeElement = doc.createElement("auto-fields-entity");
        afeElement.setAttribute("entity-name", afe.entityName());
        if (afe.defaultFieldType() != DefaultFieldType.EDIT) {
            afeElement.setAttribute("default-field-type", afe.defaultFieldType().getXmlValue());
        }
        setAttrIfNotEmpty(afeElement, "map-name", afe.mapName());
        formElement.appendChild(afeElement);
    }

    // Other helper methods

    protected void addAltTargetElement(Document doc, Element formElement, AltTarget altTarget) {
        Element altTargetElement = doc.createElement("alt-target");
        altTargetElement.setAttribute("target", altTarget.target());
        setAttrIfNotEmpty(altTargetElement, "use-when", altTarget.useWhen());
        formElement.appendChild(altTargetElement);
    }

    protected void addBannerElement(Document doc, Element parentElement, Banner banner) {
        Element bannerElement = doc.createElement("banner");
        setAttrIfNotEmpty(bannerElement, "text", banner.text());
        setAttrIfNotEmpty(bannerElement, "style", banner.style());
        setAttrIfNotEmpty(bannerElement, "text-style", banner.textStyle());
        setAttrIfNotEmpty(bannerElement, "left-text", banner.leftText());
        setAttrIfNotEmpty(bannerElement, "left-text-style", banner.leftTextStyle());
        setAttrIfNotEmpty(bannerElement, "right-text", banner.rightText());
        setAttrIfNotEmpty(bannerElement, "right-text-style", banner.rightTextStyle());
        parentElement.appendChild(bannerElement);
    }

    // Action element methods (reused from ScreenAnnotationReader pattern)

    protected void addSetActionElement(Document doc, Element actionsElement, SetAction setAction) {
        if (UtilValidate.isEmpty(setAction.field())) {
            return;
        }

        Element setElement = doc.createElement("set");
        setElement.setAttribute("field", setAction.field());

        setAttrIfNotEmpty(setElement, "value", setAction.value());
        setAttrIfNotEmpty(setElement, "from-field", setAction.fromField());
        setAttrIfNotEmpty(setElement, "default-value", setAction.defaultValue());
        setAttrIfNotEmpty(setElement, "type", setAction.type());
        if (setAction.global()) {
            setElement.setAttribute("global", "true");
        }
        if (!setAction.setIfEmpty()) {
            setElement.setAttribute("set-if-empty", "false");
        }
        if (!setAction.setIfNull()) {
            setElement.setAttribute("set-if-null", "false");
        }

        actionsElement.appendChild(setElement);
    }

    protected void addServiceActionElement(Document doc, Element actionsElement, ServiceAction serviceAction) {
        if (UtilValidate.isEmpty(serviceAction.serviceName())) {
            return;
        }

        Element serviceElement = doc.createElement("service");
        serviceElement.setAttribute("service-name", serviceAction.serviceName());

        setAttrIfNotEmpty(serviceElement, "result-map", serviceAction.resultMapName());
        setAttrIfNotEmpty(serviceElement, "result-map-list", serviceAction.resultMapList());
        serviceElement.setAttribute("auto-field-map", serviceAction.autoFieldMap() ? "true" : "false"); // SCIPIO: 4.0.0: always emit; synthetic DOM has no XSD default and empty means no auto-field-map at runtime
        setAttrIfNotEmpty(serviceElement, "result-map-field", serviceAction.resultMapField());

        for (FieldMap fieldMap : serviceAction.fieldMaps()) {
            Element fieldMapElement = doc.createElement("field-map");
            fieldMapElement.setAttribute("field-name", fieldMap.fieldName());
            setAttrIfNotEmpty(fieldMapElement, "from-field", fieldMap.fromField());
            setAttrIfNotEmpty(fieldMapElement, "value", fieldMap.value());
            serviceElement.appendChild(fieldMapElement);
        }

        actionsElement.appendChild(serviceElement);
    }

    protected void addEntityOneActionElement(Document doc, Element actionsElement, EntityOneAction entityOne) {
        if (UtilValidate.isEmpty(entityOne.entityName()) || UtilValidate.isEmpty(entityOne.valueField())) {
            return;
        }

        Element entityOneElement = doc.createElement("entity-one");
        entityOneElement.setAttribute("entity-name", entityOne.entityName());
        entityOneElement.setAttribute("value-field", entityOne.valueField());

        entityOneElement.setAttribute("auto-field-map", entityOne.autoFieldMap() ? "true" : "false"); // SCIPIO: 4.0.0: always emit; synthetic DOM has no XSD default and empty means no auto-field-map at runtime
        if (entityOne.useCache()) {
            entityOneElement.setAttribute("use-cache", "true");
        }

        for (FieldMap fieldMap : entityOne.fieldMaps()) {
            Element fieldMapElement = doc.createElement("field-map");
            fieldMapElement.setAttribute("field-name", fieldMap.fieldName());
            setAttrIfNotEmpty(fieldMapElement, "from-field", fieldMap.fromField());
            setAttrIfNotEmpty(fieldMapElement, "value", fieldMap.value());
            entityOneElement.appendChild(fieldMapElement);
        }

        actionsElement.appendChild(entityOneElement);
    }

    protected void addEntityConditionActionElement(Document doc, Element actionsElement, EntityConditionAction entityCondition) {
        if (UtilValidate.isEmpty(entityCondition.entityName()) || UtilValidate.isEmpty(entityCondition.list())) {
            return;
        }

        Element entityConditionElement = doc.createElement("entity-condition");
        entityConditionElement.setAttribute("entity-name", entityCondition.entityName());
        entityConditionElement.setAttribute("list", entityCondition.list());

        if (entityCondition.useCache()) {
            entityConditionElement.setAttribute("use-cache", "true");
        }
        if (entityCondition.filterByDate()) {
            entityConditionElement.setAttribute("filter-by-date", "true");
        }
        if (entityCondition.distinct()) {
            entityConditionElement.setAttribute("distinct", "true");
        }
        setAttrIfNotEmpty(entityConditionElement, "delegator-name", entityCondition.delegatorName());

        for (ConditionExpr condExpr : entityCondition.conditions()) {
            Element condExprElement = doc.createElement("condition-expr");
            condExprElement.setAttribute("field-name", condExpr.fieldName());
            condExprElement.setAttribute("operator", condExpr.operator());
            setAttrIfNotEmpty(condExprElement, "value", condExpr.value());
            setAttrIfNotEmpty(condExprElement, "from-field", condExpr.fromField());
            setAttrIfNotEmpty(condExprElement, "env-name", condExpr.envName());
            if (condExpr.ignoreCase()) {
                condExprElement.setAttribute("ignore-case", "true");
            }
            if (condExpr.ignoreIfEmpty()) {
                condExprElement.setAttribute("ignore-if-empty", "true");
            }
            if (condExpr.ignoreIfNull()) {
                condExprElement.setAttribute("ignore-if-null", "true");
            }
            entityConditionElement.appendChild(condExprElement);
        }

        for (String selectField : entityCondition.selectFields()) {
            Element selectFieldElement = doc.createElement("select-field");
            selectFieldElement.setAttribute("field-name", selectField);
            entityConditionElement.appendChild(selectFieldElement);
        }

        for (String orderBy : entityCondition.orderBy()) {
            Element orderByElement = doc.createElement("order-by");
            orderByElement.setAttribute("field-name", orderBy);
            entityConditionElement.appendChild(orderByElement);
        }

        actionsElement.appendChild(entityConditionElement);
    }

    protected void addScriptActionElement(Document doc, Element actionsElement, ScriptAction scriptAction) {
        if (UtilValidate.isEmpty(scriptAction.location()) && UtilValidate.isEmpty(scriptAction.script())) {
            return;
        }

        Element scriptElement = doc.createElement("script");

        if (UtilValidate.isNotEmpty(scriptAction.location())) {
            scriptElement.setAttribute("location", scriptAction.location());
        } else if (UtilValidate.isNotEmpty(scriptAction.script())) {
            scriptElement.setAttribute("lang", scriptAction.lang());
            scriptElement.setTextContent(scriptAction.script());
        }

        actionsElement.appendChild(scriptElement);
    }

    protected void addPropertyToFieldActionElement(Document doc, Element actionsElement, PropertyToFieldAction prop) {
        if (UtilValidate.isEmpty(prop.field()) || UtilValidate.isEmpty(prop.resource()) || UtilValidate.isEmpty(prop.property())) {
            return;
        }

        Element propElement = doc.createElement("property-to-field");
        propElement.setAttribute("field", prop.field());
        propElement.setAttribute("resource", prop.resource());
        propElement.setAttribute("property", prop.property());

        setAttrIfNotEmpty(propElement, "default", prop.defaultValue());
        if (prop.noLocale()) {
            propElement.setAttribute("no-locale", "true");
        }
        setAttrIfNotEmpty(propElement, "arg-list-name", prop.argListName());
        if (prop.global()) {
            propElement.setAttribute("global", "true");
        }

        actionsElement.appendChild(propElement);
    }

    /**
     * Helper method to set an attribute only if the value is not empty.
     */
    protected void setAttrIfNotEmpty(Element element, String attrName, String value) {
        if (UtilValidate.isNotEmpty(value)) {
            element.setAttribute(attrName, value);
        }
    }
}

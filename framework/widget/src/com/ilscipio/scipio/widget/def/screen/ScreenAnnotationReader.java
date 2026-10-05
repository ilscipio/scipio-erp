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
package com.ilscipio.scipio.widget.def.screen;

import com.ilscipio.scipio.ce.base.component.ComponentReflectInfo;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilTimer;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.widget.model.ModelScreen;
import org.ofbiz.widget.model.ModelScreens;
import org.ofbiz.widget.model.WidgetDocumentInfo;
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
 * Screen annotation reader - creates ModelScreen objects from @Screen annotations.
 *
 * <p>This reader scans classes annotated with @Screen and builds corresponding
 * ModelScreen objects that can be used by the widget framework.</p>
 *
 * <p>The reader generates synthetic XML elements from annotations, which are then
 * passed to the existing ModelScreen/ModelScreens constructors. This approach
 * ensures compatibility with the existing widget infrastructure.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@SuppressWarnings("serial")
public class ScreenAnnotationReader implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // SCIPIO: 4.0.0: Reused per-thread DocumentBuilder. Previously buildScreenDocument() called
    // DocumentBuilderFactory.newInstance().newDocumentBuilder() PER SCREEN, and each instantiation
    // re-scans the Xerces classpath resources. Mirrors the identical fix in FormAnnotationReader.
    private static final ThreadLocal<DocumentBuilder> THREAD_LOCAL_DOCUMENT_BUILDER = ThreadLocal.withInitial(() -> {
        try {
            return DocumentBuilderFactory.newInstance().newDocumentBuilder();
        } catch (ParserConfigurationException e) {
            throw new IllegalStateException("Error creating shared DocumentBuilder for screen annotation reading", e);
        }
    });

    protected final ComponentReflectInfo reflectInfo;

    public ScreenAnnotationReader(ComponentReflectInfo reflectInfo) {
        this.reflectInfo = reflectInfo;
    }

    /**
     * Reads all @Screen annotated classes/interfaces and returns a map of screen names to ModelScreen objects.
     */
    public Map<String, ModelScreen> getModelScreens() {
        return getModelScreens(null);
    }

    /**
     * Reads all @Screen annotated classes/interfaces and returns a map of screen names to ModelScreen objects.
     * Also populates the locationAliases map if provided.
     *
     * @param locationAliases Optional map to populate with location aliases (location -> name -> screen)
     */
    public Map<String, ModelScreen> getModelScreens(Map<String, Map<String, ModelScreen>> locationAliases) {
        UtilTimer utilTimer = new UtilTimer();
        utilTimer.timerString("Before start of screen loop in screen annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]");

        Map<String, ModelScreen> modelScreens = new LinkedHashMap<>();
        int screenCount = 0;

        for (Class<?> screenClass : reflectInfo.getReflectQuery().getAnnotatedClasses(Screen.class)) {
            try {
                List<ScreenWithAnnotation> screensWithAnnotations = readScreensWithAnnotationsFromClass(screenClass);
                for (ScreenWithAnnotation swa : screensWithAnnotations) {
                    ModelScreen screen = swa.screen;
                    Screen screenDef = swa.annotation;

                    if (modelScreens.containsKey(screen.getName())) {
                        Debug.logWarning("Screen " + screen.getName() + " is defined more than once, " +
                                "most recent will over-write previous definition(s)", module);
                    }
                    modelScreens.put(screen.getName(), screen);
                    screenCount++;

                    // Register location aliases if map is provided
                    if (locationAliases != null) {
                        registerLocationAliases(locationAliases, screen, screenDef);
                    }
                }
            } catch (Exception e) {
                Debug.logError(e, "Error creating screens from annotations in class " + screenClass.getName(), module);
            }
        }

        utilTimer.timerString("Finished screen annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "] - Total Screens: " + screenCount + " FINISHED");
        Debug.logInfo("Loaded [" + screenCount + "] Screens from annotations for component [" +
                reflectInfo.getComponent().getGlobalName() + "]", module);

        return modelScreens;
    }

    /**
     * Registers location aliases for a screen based on the @Screen annotation's location/locations attributes.
     */
    protected void registerLocationAliases(Map<String, Map<String, ModelScreen>> locationAliases,
                                           ModelScreen screen, Screen screenDef) {
        // Single location alias
        if (UtilValidate.isNotEmpty(screenDef.location())) {
            locationAliases
                    .computeIfAbsent(screenDef.location(), k -> new LinkedHashMap<>())
                    .put(screen.getName(), screen);
        }
        // Multiple location aliases
        for (String location : screenDef.locations()) {
            if (UtilValidate.isNotEmpty(location)) {
                locationAliases
                        .computeIfAbsent(location, k -> new LinkedHashMap<>())
                        .put(screen.getName(), screen);
            }
        }
    }

    /**
     * Helper class to hold a ModelScreen with its annotation.
     */
    protected static class ScreenWithAnnotation {
        final ModelScreen screen;
        final Screen annotation;

        ScreenWithAnnotation(ModelScreen screen, Screen annotation) {
            this.screen = screen;
            this.annotation = annotation;
        }
    }

    /**
     * Reads @Screen annotations from a class (may have multiple via @ScreenList).
     */
    protected List<ModelScreen> readScreensFromClass(Class<?> screenClass) throws ParserConfigurationException {
        List<ScreenWithAnnotation> screensWithAnnotations = readScreensWithAnnotationsFromClass(screenClass);
        List<ModelScreen> screens = new ArrayList<>();
        for (ScreenWithAnnotation swa : screensWithAnnotations) {
            screens.add(swa.screen);
        }
        return screens;
    }

    /**
     * Reads @Screen annotations from a class, returning both the ModelScreen and the annotation.
     */
    protected List<ScreenWithAnnotation> readScreensWithAnnotationsFromClass(Class<?> screenClass) throws ParserConfigurationException {
        List<ScreenWithAnnotation> screens = new ArrayList<>();
        String sourceLocation = "class://" + screenClass.getName();

        // Check for @ScreenList (container for multiple @Screen)
        ScreenList screenList = screenClass.getAnnotation(ScreenList.class);
        if (screenList != null) {
            for (Screen screenDef : screenList.value()) {
                ModelScreen modelScreen = createModelScreen(screenDef, screenClass, sourceLocation);
                if (modelScreen != null) {
                    screens.add(new ScreenWithAnnotation(modelScreen, screenDef));
                }
            }
        }

        // Check for single @Screen annotation
        Screen screenDef = screenClass.getAnnotation(Screen.class);
        if (screenDef != null) {
            ModelScreen modelScreen = createModelScreen(screenDef, screenClass, sourceLocation);
            if (modelScreen != null) {
                screens.add(new ScreenWithAnnotation(modelScreen, screenDef));
            }
        }

        return screens;
    }

    /**
     * Creates a ModelScreen from a @Screen annotation.
     */
    protected ModelScreen createModelScreen(Screen screenDef, Class<?> screenClass, String sourceLocation)
            throws ParserConfigurationException {
        String screenName = screenDef.name();
        if (UtilValidate.isEmpty(screenName)) {
            Debug.logWarning("Screen annotation in class " + screenClass.getName() +
                    " has no name, skipping", module);
            return null;
        }

        // Build synthetic XML document
        Document doc = buildScreenDocument(screenDef, screenClass, sourceLocation);
        if (doc == null) {
            return null;
        }

        // Create ModelScreens from the document, which creates ModelScreen objects
        // SCIPIO: 4.0.0: the declared XML location lets the folder's CommonScreens.xml settings (decorator
        // fallback, render-init) apply to the annotation screen as they did to the XML file it replaces
        ModelScreens modelScreens = new ModelScreens(doc.getDocumentElement(), sourceLocation, true, null,
                UtilValidate.isNotEmpty(screenDef.location()) ? screenDef.location() : null);
        return modelScreens.get(screenName);
    }

    /**
     * Builds a synthetic XML document representing the screen definition.
     */
    protected Document buildScreenDocument(Screen screenDef, Class<?> screenClass, String sourceLocation)
            throws ParserConfigurationException {
        // SCIPIO: 4.0.0: Reuse the per-thread DocumentBuilder instead of creating a new
        // DocumentBuilderFactory/DocumentBuilder for every single screen (see field javadoc above).
        DocumentBuilder builder = THREAD_LOCAL_DOCUMENT_BUILDER.get();
        Document doc = builder.newDocument();

        // SCIPIO: 4.0.0: Set the document's resource location from the @Screen annotation's declared location.
        // This allows include-screen-actions and include-screen (same-file) resolution to work correctly
        // for annotation-based screens, since the widget framework uses this to resolve relative references.
        if (UtilValidate.isNotEmpty(screenDef.location())) {
            WidgetDocumentInfo docInfo = WidgetDocumentInfo.retrieveAlways(doc);
            docInfo.setResourceLocation(screenDef.location());
        }

        // Root <screens> element
        Element screensElement = doc.createElement("screens");
        doc.appendChild(screensElement);

        // <screen> element
        Element screenElement = doc.createElement("screen");
        screenElement.setAttribute("name", screenDef.name());

        if (UtilValidate.isNotEmpty(screenDef.transactionTimeout())) {
            screenElement.setAttribute("transaction-timeout", screenDef.transactionTimeout());
        }
        if (UtilValidate.isNotEmpty(screenDef.transactionTimeoutParam())) {
            screenElement.setAttribute("transaction-timeout-param", screenDef.transactionTimeoutParam());
        }
        if (screenDef.useTransaction()) {
            screenElement.setAttribute("use-transaction", "true");
        }
        if (screenDef.useCache()) {
            screenElement.setAttribute("use-cache", "true");
        }

        screensElement.appendChild(screenElement);

        // Build section structure
        Element sectionElement = doc.createElement("section");
        screenElement.appendChild(sectionElement);
        // SCIPIO: 4.0.0: root section condition
        Element rootConditionElement = buildConditionElement(doc, screenDef.condition());
        if (rootConditionElement != null) {
            sectionElement.appendChild(rootConditionElement);
        }

        // Build actions
        Element actionsElement = buildActionsElement(doc, screenDef, screenClass);
        if (actionsElement != null && actionsElement.hasChildNodes()) {
            sectionElement.appendChild(actionsElement);
            // SCIPIO: Debug logging for action processing
            if (screenDef.name().equals("ScipioLogView")) {
                Debug.logInfo("ScipioLogView: actionsElement has " + actionsElement.getChildNodes().getLength() + " children", module);
            }
        } else if (screenDef.name().equals("ScipioLogView")) {
            Debug.logInfo("ScipioLogView: actionsElement is empty or null", module);
        }

        // Build widgets
        Element widgetsElement = buildWidgetsElement(doc, screenDef, screenClass);
        if (widgetsElement != null && widgetsElement.hasChildNodes()) {
            sectionElement.appendChild(widgetsElement);
        } else if (actionsElement == null || !actionsElement.hasChildNodes()) {
            // Must have at least actions or widgets - create empty widgets
            widgetsElement = doc.createElement("widgets");
            sectionElement.appendChild(widgetsElement);
        }

        // SCIPIO: 4.0.0: root section fail-widgets
        try {
            Element rootFailWidgetsElement = doc.createElement("fail-widgets");
            addWidgetsForContainerContent(doc, rootFailWidgetsElement, screenDef.failWidgets());
            if (rootFailWidgetsElement.hasChildNodes()) {
                sectionElement.appendChild(rootFailWidgetsElement);
            }
        } catch (ReflectiveOperationException e) {
            Debug.logError(e, "Error building root fail-widgets for screen [" + screenDef.name() + "]", module);
        }
        // SCIPIO: 4.0.0: root section catch-actions and finally-actions
        for (Object[] block : new Object[][] {{"catch-actions", screenDef.catchActions()}, {"finally-actions", screenDef.finallyActions()}}) {
            if (hasActions((Actions) block[1])) {
                Element blockElement = doc.createElement((String) block[0]);
                addActionsContent(doc, blockElement, (Actions) block[1]);
                if (blockElement.hasChildNodes()) {
                    sectionElement.appendChild(blockElement);
                }
            }
        }
        return doc;
    }

    /**
     * Builds the actions element from annotations.
     */
    protected Element buildActionsElement(Document doc, Screen screenDef, Class<?> screenClass) {
        Element actionsElement = doc.createElement("actions");

        // SCIPIO: 4.0.0: Processing order matches common XML convention:
        // 1. property-map (load labels first - uiLabelMap etc.)
        // 2. set (may reference labels loaded above)
        // 3. service, entity-one, entity-condition
        // 4. property-to-field, condition-to-field
        // 5. include-screen-actions (may use all context values set above)
        // 6. script (last - may use all above)

        // Process @PropertyMapAction annotations (load label maps FIRST)
        processPropertyMapActions(doc, actionsElement, screenClass);

        // Process @SetAction annotations
        processSetActions(doc, actionsElement, screenClass);

        // Process @ClearFieldAction annotations
        processClearFieldActions(doc, actionsElement, screenClass);

        // Process @ServiceAction annotations
        processServiceActions(doc, actionsElement, screenClass);

        // Process @EntityOneAction annotations
        processEntityOneActions(doc, actionsElement, screenClass);

        // Process @EntityConditionAction annotations
        processEntityConditionActions(doc, actionsElement, screenClass);

        // Process @EntityAndAction annotations
        processEntityAndActions(doc, actionsElement, screenClass);

        // Process @GetRelatedOneAction / @GetRelatedAction annotations (need prior entity lookups)
        processGetRelatedOneActions(doc, actionsElement, screenClass);
        processGetRelatedActions(doc, actionsElement, screenClass);

        // Process @PropertyToFieldAction annotations
        processPropertyToFieldActions(doc, actionsElement, screenClass);

        // Process @ConditionToFieldAction annotations
        processConditionToFieldActions(doc, actionsElement, screenClass);

        // Process @IncludeScreenActionsAction annotations
        processIncludeScreenActionsActions(doc, actionsElement, screenClass);

        // Process @IncludeFormActionsAction / row / menu / tree include-actions annotations
        processIncludeFormActionsActions(doc, actionsElement, screenClass);
        processIncludeFormRowActionsActions(doc, actionsElement, screenClass);
        processIncludeMenuActionsActions(doc, actionsElement, screenClass);
        processIncludeTreeActionsActions(doc, actionsElement, screenClass);

        // Process @ScriptAction annotations (LAST - scripts may use all context values set above)
        processScriptActions(doc, actionsElement, screenClass);

        // Process @CloseObjectAction / @ThrowExceptionAction annotations (terminal actions)
        processCloseObjectActions(doc, actionsElement, screenClass);
        processThrowExceptionActions(doc, actionsElement, screenClass);

        // Process repeatable type-level @Action / @IfAction annotations (unified format),
        // merged by their order() index so cross-type declaration order is preserved
        processOrderedUnifiedActions(doc, actionsElement, screenClass);

        // Process actions from @Screen.actions() - uses unified Action format
        for (Action action : screenDef.actions()) {
            addUnifiedActionElement(doc, actionsElement, action);
        }

        return actionsElement;
    }

    protected void processClearFieldActions(Document doc, Element actionsElement, Class<?> screenClass) {
        ClearFieldActionList list = screenClass.getAnnotation(ClearFieldActionList.class);
        if (list != null) {
            for (ClearFieldAction action : list.value()) {
                addClearFieldActionElement(doc, actionsElement, action);
            }
        }
        ClearFieldAction action = screenClass.getAnnotation(ClearFieldAction.class);
        if (action != null) {
            addClearFieldActionElement(doc, actionsElement, action);
        }
    }

    protected void processEntityAndActions(Document doc, Element actionsElement, Class<?> screenClass) {
        EntityAndActionList list = screenClass.getAnnotation(EntityAndActionList.class);
        if (list != null) {
            for (EntityAndAction action : list.value()) {
                addEntityAndActionElement(doc, actionsElement, action);
            }
        }
        EntityAndAction action = screenClass.getAnnotation(EntityAndAction.class);
        if (action != null) {
            addEntityAndActionElement(doc, actionsElement, action);
        }
    }

    protected void processGetRelatedOneActions(Document doc, Element actionsElement, Class<?> screenClass) {
        GetRelatedOneActionList list = screenClass.getAnnotation(GetRelatedOneActionList.class);
        if (list != null) {
            for (GetRelatedOneAction action : list.value()) {
                addGetRelatedOneActionElement(doc, actionsElement, action);
            }
        }
        GetRelatedOneAction action = screenClass.getAnnotation(GetRelatedOneAction.class);
        if (action != null) {
            addGetRelatedOneActionElement(doc, actionsElement, action);
        }
    }

    protected void processGetRelatedActions(Document doc, Element actionsElement, Class<?> screenClass) {
        GetRelatedActionList list = screenClass.getAnnotation(GetRelatedActionList.class);
        if (list != null) {
            for (GetRelatedAction action : list.value()) {
                addGetRelatedActionElement(doc, actionsElement, action);
            }
        }
        GetRelatedAction action = screenClass.getAnnotation(GetRelatedAction.class);
        if (action != null) {
            addGetRelatedActionElement(doc, actionsElement, action);
        }
    }

    protected void processIncludeFormActionsActions(Document doc, Element actionsElement, Class<?> screenClass) {
        IncludeFormActionsActionList list = screenClass.getAnnotation(IncludeFormActionsActionList.class);
        if (list != null) {
            for (IncludeFormActionsAction action : list.value()) {
                addIncludeFormActionsActionElement(doc, actionsElement, action);
            }
        }
        IncludeFormActionsAction action = screenClass.getAnnotation(IncludeFormActionsAction.class);
        if (action != null) {
            addIncludeFormActionsActionElement(doc, actionsElement, action);
        }
    }

    protected void processIncludeFormRowActionsActions(Document doc, Element actionsElement, Class<?> screenClass) {
        IncludeFormRowActionsActionList list = screenClass.getAnnotation(IncludeFormRowActionsActionList.class);
        if (list != null) {
            for (IncludeFormRowActionsAction action : list.value()) {
                addIncludeFormRowActionsActionElement(doc, actionsElement, action);
            }
        }
        IncludeFormRowActionsAction action = screenClass.getAnnotation(IncludeFormRowActionsAction.class);
        if (action != null) {
            addIncludeFormRowActionsActionElement(doc, actionsElement, action);
        }
    }

    protected void processIncludeMenuActionsActions(Document doc, Element actionsElement, Class<?> screenClass) {
        IncludeMenuActionsActionList list = screenClass.getAnnotation(IncludeMenuActionsActionList.class);
        if (list != null) {
            for (IncludeMenuActionsAction action : list.value()) {
                addIncludeMenuActionsActionElement(doc, actionsElement, action);
            }
        }
        IncludeMenuActionsAction action = screenClass.getAnnotation(IncludeMenuActionsAction.class);
        if (action != null) {
            addIncludeMenuActionsActionElement(doc, actionsElement, action);
        }
    }

    protected void processIncludeTreeActionsActions(Document doc, Element actionsElement, Class<?> screenClass) {
        IncludeTreeActionsActionList list = screenClass.getAnnotation(IncludeTreeActionsActionList.class);
        if (list != null) {
            for (IncludeTreeActionsAction action : list.value()) {
                addIncludeTreeActionsActionElement(doc, actionsElement, action);
            }
        }
        IncludeTreeActionsAction action = screenClass.getAnnotation(IncludeTreeActionsAction.class);
        if (action != null) {
            addIncludeTreeActionsActionElement(doc, actionsElement, action);
        }
    }

    protected void processCloseObjectActions(Document doc, Element actionsElement, Class<?> screenClass) {
        CloseObjectActionList list = screenClass.getAnnotation(CloseObjectActionList.class);
        if (list != null) {
            for (CloseObjectAction action : list.value()) {
                addCloseObjectActionElement(doc, actionsElement, action);
            }
        }
        CloseObjectAction action = screenClass.getAnnotation(CloseObjectAction.class);
        if (action != null) {
            addCloseObjectActionElement(doc, actionsElement, action);
        }
    }

    protected void processThrowExceptionActions(Document doc, Element actionsElement, Class<?> screenClass) {
        ThrowExceptionActionList list = screenClass.getAnnotation(ThrowExceptionActionList.class);
        if (list != null) {
            for (ThrowExceptionAction action : list.value()) {
                addThrowExceptionActionElement(doc, actionsElement, action);
            }
        }
        ThrowExceptionAction action = screenClass.getAnnotation(ThrowExceptionAction.class);
        if (action != null) {
            addThrowExceptionActionElement(doc, actionsElement, action);
        }
    }

    protected void processUnifiedActions(Document doc, Element actionsElement, Class<?> screenClass) {
        ActionList list = screenClass.getAnnotation(ActionList.class);
        if (list != null) {
            for (Action action : list.value()) {
                addUnifiedActionElement(doc, actionsElement, action);
            }
        }
        Action action = screenClass.getAnnotation(Action.class);
        if (action != null) {
            addUnifiedActionElement(doc, actionsElement, action);
        }
    }

    /**
     * Processes type-level @Action and @IfAction annotations merged by their order() index.
     * Java reflection loses declaration order across different annotation types, so the
     * converter stamps a sequential order attribute when a screen mixes them; unordered
     * entries (order = -1) keep their per-type order and run after the ordered ones.
     */
    protected void processOrderedUnifiedActions(Document doc, Element actionsElement, Class<?> screenClass) {
        List<Object[]> entries = new ArrayList<>(); // [order, annotation]
        ActionList list = screenClass.getAnnotation(ActionList.class);
        if (list != null) {
            for (Action action : list.value()) {
                entries.add(new Object[] { action.order(), action });
            }
        }
        Action single = screenClass.getAnnotation(Action.class);
        if (single != null) {
            entries.add(new Object[] { single.order(), single });
        }
        IfActionList ifList = screenClass.getAnnotation(IfActionList.class);
        if (ifList != null) {
            for (IfAction ifAction : ifList.value()) {
                entries.add(new Object[] { ifAction.order(), ifAction });
            }
        }
        IfAction ifSingle = screenClass.getAnnotation(IfAction.class);
        if (ifSingle != null) {
            entries.add(new Object[] { ifSingle.order(), ifSingle });
        }
        // Stable sort: explicit orders ascending, unordered (-1) after them in insertion order
        entries.sort(java.util.Comparator.comparingInt(e ->
                ((Integer) e[0]) < 0 ? Integer.MAX_VALUE : (Integer) e[0]));
        for (Object[] entry : entries) {
            Object ann = entry[1];
            if (ann instanceof Action) {
                addUnifiedActionElement(doc, actionsElement, (Action) ann);
            } else {
                addIfActionElement(doc, actionsElement, (IfAction) ann);
            }
        }
    }

    /**
     * Adds an &lt;if&gt; action element (AbstractModelAction.MasterIf contract:
     * condition attribute or &lt;condition&gt; child, &lt;then&gt;, &lt;else-if&gt;*, &lt;else&gt;).
     */
    /** SCIPIO: 4.0.0: Builds an &lt;if&gt; element from a nested {@link IfAction2} (leaf branches). */
    protected void addIfAction2Element(Document doc, Element actionsElement, IfAction2 ifAction) {
        Element ifElement = doc.createElement("if");
        if (UtilValidate.isNotEmpty(ifAction.conditionExpr())) {
            ifElement.setAttribute("condition", ifAction.conditionExpr());
        } else {
            Element conditionElement = buildConditionElement(doc, ifAction.condition());
            if (conditionElement != null) {
                ifElement.appendChild(conditionElement);
            } else {
                ifElement.setAttribute("condition", "true");
            }
        }
        Element thenElement = doc.createElement("then");
        for (Action action : ifAction.then().value()) {
            addUnifiedActionElement(doc, thenElement, action);
        }
        ifElement.appendChild(thenElement);
        for (ElseIfBlock2 elseIfBlock : ifAction.elseIf()) {
            Element elseIfElement = doc.createElement("else-if");
            if (UtilValidate.isNotEmpty(elseIfBlock.conditionExpr())) {
                elseIfElement.setAttribute("condition", elseIfBlock.conditionExpr());
            } else {
                Element conditionElement = buildConditionElement(doc, elseIfBlock.condition());
                if (conditionElement != null) {
                    elseIfElement.appendChild(conditionElement);
                } else {
                    elseIfElement.setAttribute("condition", "true");
                }
            }
            Element elseIfThen = doc.createElement("then");
            for (Action action : elseIfBlock.then().value()) {
                addUnifiedActionElement(doc, elseIfThen, action);
            }
            elseIfElement.appendChild(elseIfThen);
            ifElement.appendChild(elseIfElement);
        }
        if (ifAction.elseActions().value().length > 0) {
            Element elseElement = doc.createElement("else");
            for (Action action : ifAction.elseActions().value()) {
                addUnifiedActionElement(doc, elseElement, action);
            }
            ifElement.appendChild(elseElement);
        }
        actionsElement.appendChild(ifElement);
    }

    protected void addIfActionElement(Document doc, Element actionsElement, IfAction ifAction) {
        Element ifElement = doc.createElement("if");
        if (UtilValidate.isNotEmpty(ifAction.conditionExpr())) {
            ifElement.setAttribute("condition", ifAction.conditionExpr());
        } else {
            Element conditionElement = buildConditionElement(doc, ifAction.condition());
            if (conditionElement != null) {
                ifElement.appendChild(conditionElement);
            } else {
                ifElement.setAttribute("condition", "true"); // SCIPIO: 4.0.0: Always/empty condition = unconditional (MasterIf treats a missing condition as always-false)
            }
        }
        Element thenElement = doc.createElement("then");
        addActionsContent(doc, thenElement, ifAction.then());
        ifElement.appendChild(thenElement);
        for (ElseIfBlock elseIfBlock : ifAction.elseIf()) {
            Element elseIfElement = doc.createElement("else-if");
            if (UtilValidate.isNotEmpty(elseIfBlock.conditionExpr())) {
                elseIfElement.setAttribute("condition", elseIfBlock.conditionExpr());
            } else {
                Element conditionElement = buildConditionElement(doc, elseIfBlock.condition());
                if (conditionElement != null) {
                    elseIfElement.appendChild(conditionElement);
                } else {
                    elseIfElement.setAttribute("condition", "true");
                }
            }
            Element elseIfThen = doc.createElement("then");
            addActionsContent(doc, elseIfThen, elseIfBlock.then());
            elseIfElement.appendChild(elseIfThen);
            ifElement.appendChild(elseIfElement);
        }
        if (hasActions(ifAction.elseActions())) {
            Element elseElement = doc.createElement("else");
            addActionsContent(doc, elseElement, ifAction.elseActions());
            ifElement.appendChild(elseElement);
        }
        actionsElement.appendChild(ifElement);
    }

    protected void processSetActions(Document doc, Element actionsElement, Class<?> screenClass) {
        SetActionList setActionList = screenClass.getAnnotation(SetActionList.class);
        if (setActionList != null) {
            for (SetAction setAction : setActionList.value()) {
                addSetActionElement(doc, actionsElement, setAction);
            }
        }
        SetAction setAction = screenClass.getAnnotation(SetAction.class);
        if (setAction != null) {
            addSetActionElement(doc, actionsElement, setAction);
        }
    }

    protected void processServiceActions(Document doc, Element actionsElement, Class<?> screenClass) {
        ServiceActionList serviceActionList = screenClass.getAnnotation(ServiceActionList.class);
        if (serviceActionList != null) {
            for (ServiceAction serviceAction : serviceActionList.value()) {
                addServiceActionElement(doc, actionsElement, serviceAction);
            }
        }
        ServiceAction serviceAction = screenClass.getAnnotation(ServiceAction.class);
        if (serviceAction != null) {
            addServiceActionElement(doc, actionsElement, serviceAction);
        }
    }

    protected void processEntityOneActions(Document doc, Element actionsElement, Class<?> screenClass) {
        EntityOneActionList entityOneList = screenClass.getAnnotation(EntityOneActionList.class);
        if (entityOneList != null) {
            for (EntityOneAction entityOne : entityOneList.value()) {
                addEntityOneActionElement(doc, actionsElement, entityOne);
            }
        }
        EntityOneAction entityOne = screenClass.getAnnotation(EntityOneAction.class);
        if (entityOne != null) {
            addEntityOneActionElement(doc, actionsElement, entityOne);
        }
    }

    protected void processEntityConditionActions(Document doc, Element actionsElement, Class<?> screenClass) {
        EntityConditionActionList entityConditionList = screenClass.getAnnotation(EntityConditionActionList.class);
        if (entityConditionList != null) {
            for (EntityConditionAction entityCondition : entityConditionList.value()) {
                addEntityConditionActionElement(doc, actionsElement, entityCondition);
            }
        }
        EntityConditionAction entityCondition = screenClass.getAnnotation(EntityConditionAction.class);
        if (entityCondition != null) {
            addEntityConditionActionElement(doc, actionsElement, entityCondition);
        }
    }

    protected void processScriptActions(Document doc, Element actionsElement, Class<?> screenClass) {
        ScriptActionList scriptList = screenClass.getAnnotation(ScriptActionList.class);
        if (scriptList != null) {
            for (ScriptAction script : scriptList.value()) {
                addScriptActionElement(doc, actionsElement, script);
            }
        }
        ScriptAction script = screenClass.getAnnotation(ScriptAction.class);
        if (script != null) {
            addScriptActionElement(doc, actionsElement, script);
        }
    }

    protected void processPropertyToFieldActions(Document doc, Element actionsElement, Class<?> screenClass) {
        PropertyToFieldActionList propList = screenClass.getAnnotation(PropertyToFieldActionList.class);
        if (propList != null) {
            for (PropertyToFieldAction prop : propList.value()) {
                addPropertyToFieldActionElement(doc, actionsElement, prop);
            }
        }
        PropertyToFieldAction prop = screenClass.getAnnotation(PropertyToFieldAction.class);
        if (prop != null) {
            addPropertyToFieldActionElement(doc, actionsElement, prop);
        }
    }

    protected void processConditionToFieldActions(Document doc, Element actionsElement, Class<?> screenClass) {
        ConditionToFieldActionList conditionToFieldList = screenClass.getAnnotation(ConditionToFieldActionList.class);
        if (conditionToFieldList != null) {
            for (ConditionToFieldAction conditionToField : conditionToFieldList.value()) {
                addConditionToFieldActionElement(doc, actionsElement, conditionToField);
            }
        }
        ConditionToFieldAction conditionToField = screenClass.getAnnotation(ConditionToFieldAction.class);
        if (conditionToField != null) {
            addConditionToFieldActionElement(doc, actionsElement, conditionToField);
        }
    }

    protected void processPropertyMapActions(Document doc, Element actionsElement, Class<?> screenClass) {
        PropertyMapActionList propMapList = screenClass.getAnnotation(PropertyMapActionList.class);
        if (propMapList != null) {
            for (PropertyMapAction propMap : propMapList.value()) {
                addPropertyMapActionElement(doc, actionsElement, propMap);
            }
        }
        PropertyMapAction propMap = screenClass.getAnnotation(PropertyMapAction.class);
        if (propMap != null) {
            addPropertyMapActionElement(doc, actionsElement, propMap);
        }
    }

    protected void processIncludeScreenActionsActions(Document doc, Element actionsElement, Class<?> screenClass) {
        IncludeScreenActionsActionList includeList = screenClass.getAnnotation(IncludeScreenActionsActionList.class);
        if (includeList != null) {
            for (IncludeScreenActionsAction include : includeList.value()) {
                addIncludeScreenActionsActionElement(doc, actionsElement, include);
            }
        }
        IncludeScreenActionsAction include = screenClass.getAnnotation(IncludeScreenActionsAction.class);
        if (include != null) {
            addIncludeScreenActionsActionElement(doc, actionsElement, include);
        }
    }

    /**
     * Adds a &lt;set&gt; element to actions.
     */
    protected void addSetActionElement(Document doc, Element actionsElement, SetAction setAction) {
        if (UtilValidate.isEmpty(setAction.field())) {
            return;
        }

        Element setElement = doc.createElement("set");
        setElement.setAttribute("field", setAction.field());

        if (UtilValidate.isNotEmpty(setAction.value())) {
            setElement.setAttribute("value", setAction.value());
        }
        if (UtilValidate.isNotEmpty(setAction.fromField())) {
            setElement.setAttribute("from-field", setAction.fromField());
        }
        if (UtilValidate.isNotEmpty(setAction.defaultValue())) {
            setElement.setAttribute("default-value", setAction.defaultValue());
        }
        if (UtilValidate.isNotEmpty(setAction.type())) {
            setElement.setAttribute("type", setAction.type());
        }
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

    /**
     * Adds a &lt;service&gt; element to actions.
     */
    protected void addServiceActionElement(Document doc, Element actionsElement, ServiceAction serviceAction) {
        if (UtilValidate.isEmpty(serviceAction.serviceName())) {
            return;
        }

        Element serviceElement = doc.createElement("service");
        serviceElement.setAttribute("service-name", serviceAction.serviceName());

        if (UtilValidate.isNotEmpty(serviceAction.resultMapName())) {
            serviceElement.setAttribute("result-map", serviceAction.resultMapName());
        }
        // SCIPIO: Always emit auto-field-map explicitly. The synthetic DOM built here has no XSD, so
        // an omitted attribute reads as empty at runtime (AbstractModelAction.Service => NO auto field
        // mapping, dropping userLogin and other IN params), whereas parsed widget XML has the schema
        // default "true". Omitting it broke <service> actions (e.g. permission checks => "must be logged in").
        serviceElement.setAttribute("auto-field-map", serviceAction.autoFieldMap() ? "true" : "false");
        if (UtilValidate.isNotEmpty(serviceAction.resultMapField())) {
            serviceElement.setAttribute("result-map-field", serviceAction.resultMapField());
        }

        // Add field-map elements
        for (FieldMap fieldMap : serviceAction.fieldMaps()) {
            Element fieldMapElement = doc.createElement("field-map");
            fieldMapElement.setAttribute("field-name", fieldMap.fieldName());

            if (UtilValidate.isNotEmpty(fieldMap.fromField())) {
                fieldMapElement.setAttribute("from-field", fieldMap.fromField());
            }
            if (UtilValidate.isNotEmpty(fieldMap.value())) {
                fieldMapElement.setAttribute("value", fieldMap.value());
            }

            serviceElement.appendChild(fieldMapElement);
        }

        actionsElement.appendChild(serviceElement);
    }

    /**
     * Adds an &lt;entity-one&gt; element to actions.
     */
    protected void addEntityOneActionElement(Document doc, Element actionsElement, EntityOneAction entityOne) {
        if (UtilValidate.isEmpty(entityOne.entityName()) || UtilValidate.isEmpty(entityOne.valueField())) {
            return;
        }

        Element entityOneElement = doc.createElement("entity-one");
        entityOneElement.setAttribute("entity-name", entityOne.entityName());
        entityOneElement.setAttribute("value-field", entityOne.valueField());

        if (!entityOne.autoFieldMap()) {
            entityOneElement.setAttribute("auto-field-map", "false");
        }
        if (entityOne.useCache()) {
            entityOneElement.setAttribute("use-cache", "true");
        }

        // Add field-map elements
        for (FieldMap fieldMap : entityOne.fieldMaps()) {
            Element fieldMapElement = doc.createElement("field-map");
            fieldMapElement.setAttribute("field-name", fieldMap.fieldName());
            if (UtilValidate.isNotEmpty(fieldMap.fromField())) {
                fieldMapElement.setAttribute("from-field", fieldMap.fromField());
            }
            if (UtilValidate.isNotEmpty(fieldMap.value())) {
                fieldMapElement.setAttribute("value", fieldMap.value());
            }
            entityOneElement.appendChild(fieldMapElement);
        }

        actionsElement.appendChild(entityOneElement);
    }

    /**
     * Adds an &lt;entity-condition&gt; element to actions.
     */
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
        if (UtilValidate.isNotEmpty(entityCondition.delegatorName())) {
            entityConditionElement.setAttribute("delegator-name", entityCondition.delegatorName());
        }

        // Add condition-expr elements
        for (ConditionExpr condExpr : entityCondition.conditions()) {
            Element condExprElement = doc.createElement("condition-expr");
            condExprElement.setAttribute("field-name", condExpr.fieldName());
            condExprElement.setAttribute("operator", condExpr.operator());
            if (UtilValidate.isNotEmpty(condExpr.value())) {
                condExprElement.setAttribute("value", condExpr.value());
            }
            if (UtilValidate.isNotEmpty(condExpr.fromField())) {
                condExprElement.setAttribute("from-field", condExpr.fromField());
            }
            if (UtilValidate.isNotEmpty(condExpr.envName())) {
                condExprElement.setAttribute("env-name", condExpr.envName());
            }
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

        // Add select-field elements
        for (String selectField : entityCondition.selectFields()) {
            Element selectFieldElement = doc.createElement("select-field");
            selectFieldElement.setAttribute("field-name", selectField);
            entityConditionElement.appendChild(selectFieldElement);
        }

        // Add order-by elements
        for (String orderBy : entityCondition.orderBy()) {
            Element orderByElement = doc.createElement("order-by");
            orderByElement.setAttribute("field-name", orderBy);
            entityConditionElement.appendChild(orderByElement);
        }

        actionsElement.appendChild(entityConditionElement);
    }

    /**
     * Adds a &lt;script&gt; element to actions.
     */
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

    /**
     * Adds a &lt;property-to-field&gt; element to actions.
     */
    protected void addPropertyToFieldActionElement(Document doc, Element actionsElement, PropertyToFieldAction prop) {
        if (UtilValidate.isEmpty(prop.field()) || UtilValidate.isEmpty(prop.resource()) || UtilValidate.isEmpty(prop.property())) {
            return;
        }

        Element propElement = doc.createElement("property-to-field");
        propElement.setAttribute("field", prop.field());
        propElement.setAttribute("resource", prop.resource());
        propElement.setAttribute("property", prop.property());

        if (UtilValidate.isNotEmpty(prop.defaultValue())) {
            propElement.setAttribute("default", prop.defaultValue());
        }
        if (prop.noLocale()) {
            propElement.setAttribute("no-locale", "true");
        }
        if (UtilValidate.isNotEmpty(prop.argListName())) {
            propElement.setAttribute("arg-list-name", prop.argListName());
        }
        if (prop.global()) {
            propElement.setAttribute("global", "true");
        }

        actionsElement.appendChild(propElement);
    }

    /**
     * Builds the widgets element from annotations.
     */
    protected Element buildWidgetsElement(Document doc, Screen screenDef, Class<?> screenClass) {
        Element widgetsElement = doc.createElement("widgets");

        // Check for @DecoratorScreen on class
        DecoratorScreen decoratorScreen = screenClass.getAnnotation(DecoratorScreen.class);
        if (decoratorScreen != null && UtilValidate.isNotEmpty(decoratorScreen.name())) {
            addDecoratorScreenElement(doc, widgetsElement, decoratorScreen, screenClass);
            return widgetsElement;
        }

        // Check decorator from @Screen.decorator()
        if (UtilValidate.isNotEmpty(screenDef.decorator().name())) {
            addDecoratorScreenElement(doc, widgetsElement, screenDef.decorator(), screenClass);
            return widgetsElement;
        }

        // Process widgets from class annotations
        processWidgetAnnotations(doc, widgetsElement, screenClass);

        // Check for @IncludeScreen on class
        processIncludeScreens(doc, widgetsElement, screenClass);

        // Check includeScreen from @Screen.includeScreen()
        if (UtilValidate.isNotEmpty(screenDef.includeScreen().name())) {
            addIncludeScreenElement(doc, widgetsElement, screenDef.includeScreen());
        }

        return widgetsElement;
    }

    protected void processWidgetAnnotations(Document doc, Element widgetsElement, Class<?> screenClass) {
        // Process @Label annotations
        LabelList labelList = screenClass.getAnnotation(LabelList.class);
        if (labelList != null) {
            for (Label label : labelList.value()) {
                addLabelElement(doc, widgetsElement, label);
            }
        }
        Label label = screenClass.getAnnotation(Label.class);
        if (label != null && UtilValidate.isNotEmpty(label.text())) {
            addLabelElement(doc, widgetsElement, label);
        }

        // Process @Screenlet annotations
        ScreenletList screenletList = screenClass.getAnnotation(ScreenletList.class);
        if (screenletList != null) {
            for (Screenlet screenlet : screenletList.value()) {
                addScreenletElement(doc, widgetsElement, screenlet);
            }
        }
        Screenlet screenlet = screenClass.getAnnotation(Screenlet.class);
        if (screenlet != null) {
            addScreenletElement(doc, widgetsElement, screenlet);
        }

        // Process @Container annotations
        ContainerList containerList = screenClass.getAnnotation(ContainerList.class);
        if (containerList != null) {
            for (Container container : containerList.value()) {
                addContainerElement(doc, widgetsElement, container);
            }
        }
        Container container = screenClass.getAnnotation(Container.class);
        if (container != null && UtilValidate.isNotEmpty(container.style())) {
            addContainerElement(doc, widgetsElement, container);
        }

        // Process @HtmlTemplate annotations
        HtmlTemplateList htmlTemplateList = screenClass.getAnnotation(HtmlTemplateList.class);
        if (htmlTemplateList != null) {
            for (HtmlTemplate htmlTemplate : htmlTemplateList.value()) {
                addHtmlTemplateElement(doc, widgetsElement, htmlTemplate);
            }
        }
        HtmlTemplate htmlTemplate = screenClass.getAnnotation(HtmlTemplate.class);
        if (htmlTemplate != null && UtilValidate.isNotEmpty(htmlTemplate.location())) {
            addHtmlTemplateElement(doc, widgetsElement, htmlTemplate);
        }

        // Process @Section annotations
        SectionList sectionList = screenClass.getAnnotation(SectionList.class);
        if (sectionList != null) {
            for (Section section : sectionList.value()) {
                addSectionElement(doc, widgetsElement, section);
            }
        }
        Section section = screenClass.getAnnotation(Section.class);
        if (section != null) {
            addSectionElement(doc, widgetsElement, section);
        }
    }

    protected void processIncludeScreens(Document doc, Element widgetsElement, Class<?> screenClass) {
        IncludeScreenList includeScreenList = screenClass.getAnnotation(IncludeScreenList.class);
        if (includeScreenList != null) {
            for (IncludeScreen includeScreen : includeScreenList.value()) {
                addIncludeScreenElement(doc, widgetsElement, includeScreen);
            }
        }
        IncludeScreen includeScreen = screenClass.getAnnotation(IncludeScreen.class);
        if (includeScreen != null && UtilValidate.isNotEmpty(includeScreen.name())) {
            addIncludeScreenElement(doc, widgetsElement, includeScreen);
        }
    }

    /**
     * Adds a &lt;label&gt; element to widgets.
     */
    protected void addLabelElement(Document doc, Element parentElement, Label label) {
        if (UtilValidate.isEmpty(label.text())) {
            return;
        }

        Element labelElement = doc.createElement("label");
        labelElement.setAttribute("text", label.text());
        if (UtilValidate.isNotEmpty(label.style())) {
            labelElement.setAttribute("style", label.style());
        }
        if (UtilValidate.isNotEmpty(label.id())) {
            labelElement.setAttribute("id", label.id());
        }
        parentElement.appendChild(labelElement);
    }

    /**
     * Adds a &lt;screenlet&gt; element to widgets.
     */
    protected void addScreenletElement(Document doc, Element parentElement, Screenlet screenlet) {
        Element screenletElement = doc.createElement("screenlet");
        // SCIPIO: 4.0.0: screenlet-level actions (from XML screenlet/section/actions) - wrap content in a section
        Element contentParent = screenletElement;
        if (hasActions(screenlet.actions())) {
            Element sectionElement = doc.createElement("section");
            Element actionsElement = doc.createElement("actions");
            addActionsContent(doc, actionsElement, screenlet.actions());
            sectionElement.appendChild(actionsElement);
            Element widgetsElement = doc.createElement("widgets");
            sectionElement.appendChild(widgetsElement);
            screenletElement.appendChild(sectionElement);
            contentParent = widgetsElement;
        }

        if (UtilValidate.isNotEmpty(screenlet.title())) {
            screenletElement.setAttribute("title", screenlet.title());
        }
        if (UtilValidate.isNotEmpty(screenlet.name())) {
            screenletElement.setAttribute("name", screenlet.name());
        }
        if (screenlet.collapsible()) {
            screenletElement.setAttribute("collapsible", "true");
        }
        if (screenlet.initiallyCollapsed()) {
            screenletElement.setAttribute("initially-collapsed", "true");
        }
        if (!screenlet.saveCollapsed()) {
            screenletElement.setAttribute("save-collapsed", "false");
        }
        if (!screenlet.padded()) {
            screenletElement.setAttribute("padded", "false");
        }
        if (UtilValidate.isNotEmpty(screenlet.titleStyle())) {
            screenletElement.setAttribute("title-style", screenlet.titleStyle());
        }
        if (UtilValidate.isNotEmpty(screenlet.navigationMenuName())) {
            screenletElement.setAttribute("navigation-menu-name", screenlet.navigationMenuName());
        }
        if (UtilValidate.isNotEmpty(screenlet.navigationFormName())) {
            screenletElement.setAttribute("navigation-form-name", screenlet.navigationFormName());
        }
        if (UtilValidate.isNotEmpty(screenlet.tabMenuName())) {
            screenletElement.setAttribute("tab-menu-name", screenlet.tabMenuName());
        }
        if (UtilValidate.isNotEmpty(screenlet.contains())) {
            screenletElement.setAttribute("contains", screenlet.contains());
        }

        // SCIPIO: 4.0.0: Add nested widgets directly to screenlet (no <widgets> wrapper needed)
        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered()
        Element contentTarget = contentParent;
        List<OrderedChild> children = new ArrayList<>();
        // SCIPIO: 4.0.0: link, content, include-tree, iterate-section, ... (were dropped by the converter)
        for (Widget x : screenlet.widgets()) {
            children.add(new OrderedChild(x, () -> addUnifiedWidgetElement(doc, contentTarget, x)));
        }
        for (IncludeForm includeForm : screenlet.includeForms()) {
            children.add(new OrderedChild(includeForm, () -> addIncludeFormElement(doc, contentTarget, includeForm)));
        }
        for (IncludeScreen includeScreen : screenlet.includeScreens()) {
            children.add(new OrderedChild(includeScreen, () -> addIncludeScreenElement(doc, contentTarget, includeScreen)));
        }
        for (IncludeMenu includeMenu : screenlet.includeMenus()) {
            children.add(new OrderedChild(includeMenu, () -> addIncludeMenuElement(doc, contentTarget, includeMenu)));
        }
        for (HtmlTemplate htmlTemplate : screenlet.htmlTemplates()) {
            children.add(new OrderedChild(htmlTemplate, () -> addHtmlTemplateElement(doc, contentTarget, htmlTemplate)));
        }
        for (Label label : screenlet.labels()) {
            children.add(new OrderedChild(label, () -> addLabelElement(doc, contentTarget, label)));
        }
        // SCIPIO: 4.0.0: containers were never emitted, so FindScreenDecorator lost its search form
        for (Container container : screenlet.containers()) {
            children.add(new OrderedChild(container, () -> addContainerElement(doc, contentTarget, container)));
        }
        for (ScreenletNested x : screenlet.screenlets()) {
            children.add(new OrderedChild(x, () -> addScreenletNestedElement(doc, contentTarget, x)));
        }

        // SCIPIO: 4.0.0: conditional sections (kept as real <section> children of the screenlet)
        for (SectionNested sectionAnn : screenlet.sections()) {
            children.add(new OrderedChild(sectionAnn, () -> addSectionNestedElement(doc, screenletElement, sectionAnn)));
        }
        for (DecoratorSectionInclude decoratorSectionInclude : screenlet.decoratorSectionIncludes()) {
            children.add(new OrderedChild(decoratorSectionInclude, () -> addDecoratorSectionIncludeElement(doc, screenletElement, decoratorSectionInclude)));
        }
        emitOrderedUnchecked(children);
        parentElement.appendChild(screenletElement);
    }

    /**
     * Adds a &lt;container&gt; element to widgets.
     */
    private static void setAttrIfNotEmpty(Element element, String name, String value) {
        if (UtilValidate.isNotEmpty(value)) element.setAttribute(name, value);
    }

    /**
     * SCIPIO: 4.0.0: Builds a &lt;section&gt; from SectionNested / SectionNested2 / 3 / 4 (identical shape, cycle-breaking
     * annotation types), including condition, actions, widgets and fail-widgets (WidgetsForContainerN).
     */
    protected void addSectionNestedElement(Document doc, Element parentElement, java.lang.annotation.Annotation sectionAnn) {
        try {
            Class<?> t = sectionAnn.annotationType();
            Element sectionElement = doc.createElement("section");
            setAttrIfNotEmpty(sectionElement, "name", (String) t.getMethod("name").invoke(sectionAnn));
            setAttrIfNotEmpty(sectionElement, "contains", (String) t.getMethod("contains").invoke(sectionAnn));
            setAttrIfNotEmpty(sectionElement, "id", (String) t.getMethod("id").invoke(sectionAnn));
            setAttrIfNotEmpty(sectionElement, "style", (String) t.getMethod("style").invoke(sectionAnn));
            if (Boolean.TRUE.equals(t.getMethod("shareScope").invoke(sectionAnn))) {
                sectionElement.setAttribute("share-scope", "true");
            }
            Element conditionElement = buildConditionElement(doc, (Condition) t.getMethod("condition").invoke(sectionAnn));
            if (conditionElement != null) {
                sectionElement.appendChild(conditionElement);
            }
            Actions actions = (Actions) t.getMethod("actions").invoke(sectionAnn);
            if (hasActions(actions)) {
                Element actionsElement = doc.createElement("actions");
                addActionsContent(doc, actionsElement, actions);
                sectionElement.appendChild(actionsElement);
            }
            Element widgetsElement = doc.createElement("widgets");
            addWidgetsForContainerContent(doc, widgetsElement, t.getMethod("widgets").invoke(sectionAnn));
            if (widgetsElement.hasChildNodes()) {
                sectionElement.appendChild(widgetsElement);
            }
            Element failWidgetsElement = doc.createElement("fail-widgets");
            addWidgetsForContainerContent(doc, failWidgetsElement, t.getMethod("failWidgets").invoke(sectionAnn));
            if (failWidgetsElement.hasChildNodes()) {
                sectionElement.appendChild(failWidgetsElement);
            }
            parentElement.appendChild(sectionElement);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("Cannot read nested section annotation " + sectionAnn, e);
        }
    }

    /** SCIPIO: 4.0.0: Appends the widgets of a WidgetsForContainer / 2 / 3 / 4 annotation to parent. */
    protected void addWidgetsForContainerContent(Document doc, Element parentElement, Object widgetsAnn) throws ReflectiveOperationException {
        Class<?> t = ((java.lang.annotation.Annotation) widgetsAnn).annotationType();
        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered()
        List<OrderedChild> children = new ArrayList<>();
        for (Widget w : (Widget[]) t.getMethod("value").invoke(widgetsAnn)) {
            children.add(new OrderedChild(w, () -> addUnifiedWidgetElement(doc, parentElement, w)));
        }
        // SCIPIO: 4.0.0: a decorator-screen inside a nested section was silently dropped
        try {
            DecoratorScreenNested nestedDecorator = (DecoratorScreenNested) t.getMethod("decorator").invoke(widgetsAnn);
            if (UtilValidate.isNotEmpty(nestedDecorator.name())) {
                children.add(new OrderedChild(nestedDecorator, () -> addDecoratorScreenNestedElement(doc, parentElement, nestedDecorator)));
            }
        } catch (NoSuchMethodException e) {
            // WidgetsForContainer4 holds no decorator (deepest level)
        }
        for (IncludeScreen x : (IncludeScreen[]) t.getMethod("includeScreens").invoke(widgetsAnn)) {
            children.add(new OrderedChild(x, () -> addIncludeScreenElement(doc, parentElement, x)));
        }
        for (IncludeForm x : (IncludeForm[]) t.getMethod("includeForms").invoke(widgetsAnn)) {
            children.add(new OrderedChild(x, () -> addIncludeFormElement(doc, parentElement, x)));
        }
        for (IncludeMenu x : (IncludeMenu[]) t.getMethod("includeMenus").invoke(widgetsAnn)) {
            children.add(new OrderedChild(x, () -> addIncludeMenuElement(doc, parentElement, x)));
        }
        for (Label x : (Label[]) t.getMethod("labels").invoke(widgetsAnn)) {
            children.add(new OrderedChild(x, () -> addLabelElement(doc, parentElement, x)));
        }
        for (HtmlTemplate x : (HtmlTemplate[]) t.getMethod("htmlTemplates").invoke(widgetsAnn)) {
            children.add(new OrderedChild(x, () -> addHtmlTemplateElement(doc, parentElement, x)));
        }
        for (ScreenletNested x : (ScreenletNested[]) t.getMethod("screenlets").invoke(widgetsAnn)) {
            children.add(new OrderedChild(x, () -> addScreenletNestedElement(doc, parentElement, x)));
        }
        for (Object c : (Object[]) t.getMethod("containers").invoke(widgetsAnn)) {
            if (c instanceof Container2) {
                children.add(new OrderedChild(c, () -> addContainer2Element(doc, parentElement, (Container2) c)));
            } else if (c instanceof Container3) {
                children.add(new OrderedChild(c, () -> addContainer3Element(doc, parentElement, (Container3) c)));
            } else if (c instanceof Container4) {
                children.add(new OrderedChild(c, () -> addContainer4Element(doc, parentElement, (Container4) c)));
            }
        }
        for (Object nested : (Object[]) t.getMethod("sections").invoke(widgetsAnn)) {
            // SCIPIO: 4.0.0: the deepest level carries SectionLeaf; it used to carry nothing, so a
            // section there was flattened and lost its condition and its own actions.
            if (nested instanceof SectionLeaf) {
                children.add(new OrderedChild(nested, () -> addSectionLeafElement(doc, parentElement, (SectionLeaf) nested)));
            } else {
                children.add(new OrderedChild(nested, () -> addSectionNestedElement(doc, parentElement, (java.lang.annotation.Annotation) nested)));
            }
        }
        emitOrdered(children);
    }

    /**
     * SCIPIO: 4.0.0: Adds a &lt;decorator-screen&gt; element from a nested section's DecoratorScreenNested.
     */
    protected void addDecoratorScreenNestedElement(Document doc, Element parentElement, DecoratorScreenNested decoratorScreen)
            throws ReflectiveOperationException {
        Element decoratorElement = doc.createElement("decorator-screen");
        decoratorElement.setAttribute("name", decoratorScreen.name());
        if (UtilValidate.isNotEmpty(decoratorScreen.location())) {
            decoratorElement.setAttribute("location", decoratorScreen.location());
        }
        if (UtilValidate.isNotEmpty(decoratorScreen.fallbackName())) {
            decoratorElement.setAttribute("fallback-name", decoratorScreen.fallbackName());
        }
        if (UtilValidate.isNotEmpty(decoratorScreen.fallbackLocation())) {
            decoratorElement.setAttribute("fallback-location", decoratorScreen.fallbackLocation());
        }
        if (decoratorScreen.fallbackIfEmpty()) {
            decoratorElement.setAttribute("fallback-if-empty", "true");
        }
        if (decoratorScreen.autoDecoratorSectionInclude()) {
            decoratorElement.setAttribute("auto-decorator-section-include", "true");
        }
        for (DecoratorSectionNested section : decoratorScreen.sections()) {
            Element sectionElement = doc.createElement("decorator-section");
            sectionElement.setAttribute("name", section.name());
            if (UtilValidate.isNotEmpty(section.useWhen())) {
                sectionElement.setAttribute("use-when", section.useWhen());
            }
            if (section.fallbackAutoInclude()) {
                sectionElement.setAttribute("fallback-auto-include", "true");
            }
            if (section.overrideByAutoInclude()) {
                sectionElement.setAttribute("override-by-auto-include", "true");
            }
            if (UtilValidate.isNotEmpty(section.contains())) {
                sectionElement.setAttribute("contains", section.contains());
            }
            addWidgetsForContainerContent(doc, sectionElement, section.widgets());
            decoratorElement.appendChild(sectionElement);
        }
        parentElement.appendChild(decoratorElement);
    }

    /** SCIPIO: 4.0.0: if-true/if-false take a field name or a ${...} value expression. */
    protected void setFieldOrValue(Element element, String fieldOrValue) {
        element.setAttribute(fieldOrValue.contains("${") ? "value" : "field", fieldOrValue);
    }

    /** SCIPIO: 4.0.0: Emits a nested decorator unless it is the empty default of a single-valued member. */
    protected void addDecoratorScreenNestedIfPresent(Document doc, Element parentElement, DecoratorScreenNested decoratorScreen) {
        if (UtilValidate.isEmpty(decoratorScreen.name())) {
            return;
        }
        try {
            addDecoratorScreenNestedElement(doc, parentElement, decoratorScreen);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException("Cannot read nested decorator " + decoratorScreen.name(), e);
        }
    }

    /**
     * SCIPIO: 4.0.0: Adds a &lt;section&gt; element from a SectionLeaf, used where the
     * SectionNested chain has no level left.
     */
    protected void addSectionLeafElement(Document doc, Element parentElement, SectionLeaf section) {
        Element sectionElement = doc.createElement("section");
        if (UtilValidate.isNotEmpty(section.name())) {
            sectionElement.setAttribute("name", section.name());
        }
        if (section.shareScope()) {
            sectionElement.setAttribute("share-scope", "true");
        }
        if (UtilValidate.isNotEmpty(section.contains())) {
            sectionElement.setAttribute("contains", section.contains());
        }
        if (UtilValidate.isNotEmpty(section.id())) {
            sectionElement.setAttribute("id", section.id());
        }
        if (UtilValidate.isNotEmpty(section.style())) {
            sectionElement.setAttribute("style", section.style());
        }

        Element conditionElement = buildFunctionalConditionElement(doc, section.condition());
        if (conditionElement != null) {
            Element conditionWrapper = doc.createElement("condition");
            conditionWrapper.appendChild(conditionElement);
            sectionElement.appendChild(conditionWrapper);
        }

        if (hasActions(section.actions())) {
            Element actionsElement = doc.createElement("actions");
            addActionsContent(doc, actionsElement, section.actions());
            if (actionsElement.hasChildNodes()) {
                sectionElement.appendChild(actionsElement);
            }
        }

        Element widgetsElement = doc.createElement("widgets");
        addWidgetsLeafContent(doc, widgetsElement, section.widgets());
        if (widgetsElement.hasChildNodes()) {
            sectionElement.appendChild(widgetsElement);
        }

        Element failWidgetsElement = doc.createElement("fail-widgets");
        addWidgetsLeafContent(doc, failWidgetsElement, section.failWidgets());
        if (failWidgetsElement.hasChildNodes()) {
            sectionElement.appendChild(failWidgetsElement);
        }

        parentElement.appendChild(sectionElement);
    }

    /** SCIPIO: 4.0.0: Appends the content of a WidgetsLeaf to parent. */
    protected void addWidgetsLeafContent(Document doc, Element parentElement, WidgetsLeaf widgets) {
        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered()
        List<OrderedChild> children = new ArrayList<>();
        for (Widget widget : widgets.value()) {
            children.add(new OrderedChild(widget, () -> addUnifiedWidgetElement(doc, parentElement, widget)));
        }
        for (ContainerLeaf container : widgets.containers()) {
            children.add(new OrderedChild(container, () -> addContainerLeafElement(doc, parentElement, container)));
        }
        for (IncludeScreen x : widgets.includeScreens()) {
            children.add(new OrderedChild(x, () -> addIncludeScreenElement(doc, parentElement, x)));
        }
        for (IncludeForm x : widgets.includeForms()) {
            children.add(new OrderedChild(x, () -> addIncludeFormElement(doc, parentElement, x)));
        }
        for (IncludeMenu x : widgets.includeMenus()) {
            children.add(new OrderedChild(x, () -> addIncludeMenuElement(doc, parentElement, x)));
        }
        for (Label x : widgets.labels()) {
            children.add(new OrderedChild(x, () -> addLabelElement(doc, parentElement, x)));
        }
        for (HtmlTemplate x : widgets.htmlTemplates()) {
            children.add(new OrderedChild(x, () -> addHtmlTemplateElement(doc, parentElement, x)));
        }
        for (DecoratorSectionInclude x : widgets.decoratorSectionIncludes()) {
            children.add(new OrderedChild(x, () -> addDecoratorSectionIncludeElement(doc, parentElement, x)));
        }
        emitOrderedUnchecked(children);
    }

    /** SCIPIO: 4.0.0: Adds a &lt;container&gt; element from a ContainerLeaf. */
    protected void addContainerLeafElement(Document doc, Element parentElement, ContainerLeaf container) {
        Element containerElement = doc.createElement("container");
        if (UtilValidate.isNotEmpty(container.style())) {
            containerElement.setAttribute("style", container.style());
        }
        if (UtilValidate.isNotEmpty(container.id())) {
            containerElement.setAttribute("id", container.id());
        }
        if (UtilValidate.isNotEmpty(container.type())) {
            containerElement.setAttribute("type", container.type());
        }
        if (UtilValidate.isNotEmpty(container.contains())) {
            containerElement.setAttribute("contains", container.contains());
        }
        if (UtilValidate.isNotEmpty(container.autoUpdateTargetId())) {
            containerElement.setAttribute("auto-update-target-id", container.autoUpdateTargetId());
        }
        if (container.autoUpdateInterval() > 0) {
            containerElement.setAttribute("auto-update-interval", String.valueOf(container.autoUpdateInterval()));
        }
        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered()
        List<OrderedChild> children = new ArrayList<>();
        for (Widget widget : container.widgets()) {
            children.add(new OrderedChild(widget, () -> addUnifiedWidgetElement(doc, containerElement, widget)));
        }
        for (IncludeForm includeForm : container.includeForms()) {
            children.add(new OrderedChild(includeForm, () -> addIncludeFormElement(doc, containerElement, includeForm)));
        }
        for (IncludeScreen includeScreen : container.includeScreens()) {
            children.add(new OrderedChild(includeScreen, () -> addIncludeScreenElement(doc, containerElement, includeScreen)));
        }
        for (IncludeMenu includeMenu : container.includeMenus()) {
            children.add(new OrderedChild(includeMenu, () -> addIncludeMenuElement(doc, containerElement, includeMenu)));
        }
        for (Label label : container.labels()) {
            children.add(new OrderedChild(label, () -> addLabelElement(doc, containerElement, label)));
        }
        for (HtmlTemplate htmlTemplate : container.htmlTemplates()) {
            children.add(new OrderedChild(htmlTemplate, () -> addHtmlTemplateElement(doc, containerElement, htmlTemplate)));
        }
        for (DecoratorSectionInclude decoratorSectionInclude : container.decoratorSectionIncludes()) {
            children.add(new OrderedChild(decoratorSectionInclude, () -> addDecoratorSectionIncludeElement(doc, containerElement, decoratorSectionInclude)));
        }
        emitOrderedUnchecked(children);
        parentElement.appendChild(containerElement);
    }

    protected void addContainerElement(Document doc, Element parentElement, Container container) {
        // SCIPIO: 4.0.0: The XML-to-annotation converter had no way to attach a bare <section> to a
        // widgets block, so it wrapped each one in a container. That container emits a div the XML
        // never had; the first of them lands before <html> and wrecks the document. Such a wrapper
        // carries no attributes and no content other than its sections, so emit the sections in
        // place. Emitting in place also keeps the sections in order with their sibling containers.
        if (isBareSectionWrapper(container)) {
            for (SectionNested nestedSection : container.sections()) {
                addSectionNestedElement(doc, parentElement, nestedSection);
            }
            return;
        }

        Element containerElement = doc.createElement("container");

        if (UtilValidate.isNotEmpty(container.style())) {
            containerElement.setAttribute("style", container.style());
        }
        if (UtilValidate.isNotEmpty(container.id())) {
            containerElement.setAttribute("id", container.id());
        }
        if (UtilValidate.isNotEmpty(container.autoUpdateTargetId())) {
            containerElement.setAttribute("auto-update-target-id", container.autoUpdateTargetId());
        }
        if (container.autoUpdateInterval() > 0) {
            containerElement.setAttribute("auto-update-interval", String.valueOf(container.autoUpdateInterval()));
        }
        if (UtilValidate.isNotEmpty(container.type())) {
            containerElement.setAttribute("type", container.type());
        }
        if (UtilValidate.isNotEmpty(container.contains())) {
            containerElement.setAttribute("contains", container.contains());
        }

        // Add nested widgets
        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered()
        List<OrderedChild> children = new ArrayList<>();
        // SCIPIO: 4.0.0: generic widgets (link, image, content, sub-content, ...) were dropped by the converter
        for (Widget containerWidget : container.widgets()) {
            children.add(new OrderedChild(containerWidget, () -> addUnifiedWidgetElement(doc, containerElement, containerWidget)));
        }
        // SCIPIO: 4.0.0: a decorator-screen in a container was dropped (e.g. FindContacts)
        DecoratorScreenNested decorator = container.decorator();
        if (UtilValidate.isNotEmpty(decorator.name())) {
            children.add(new OrderedChild(decorator, () -> addDecoratorScreenNestedElement(doc, containerElement, decorator)));
        }
        for (IncludeForm includeForm : container.includeForms()) {
            children.add(new OrderedChild(includeForm, () -> addIncludeFormElement(doc, containerElement, includeForm)));
        }
        for (IncludeScreen includeScreen : container.includeScreens()) {
            children.add(new OrderedChild(includeScreen, () -> addIncludeScreenElement(doc, containerElement, includeScreen)));
        }
        for (Label label : container.labels()) {
            children.add(new OrderedChild(label, () -> addLabelElement(doc, containerElement, label)));
        }
        for (HtmlTemplate htmlTemplate : container.htmlTemplates()) {
            children.add(new OrderedChild(htmlTemplate, () -> addHtmlTemplateElement(doc, containerElement, htmlTemplate)));
        }
        for (IncludeMenu includeMenu : container.includeMenus()) {
            children.add(new OrderedChild(includeMenu, () -> addIncludeMenuElement(doc, containerElement, includeMenu)));
        }
        for (Container2 nestedContainer : container.containers()) {
            children.add(new OrderedChild(nestedContainer, () -> addContainer2Element(doc, containerElement, nestedContainer)));
        }
        // SCIPIO: 4.0.0: Process screenlets nested inside containers (uses ScreenletNested)
        for (ScreenletNested screenlet : container.screenlets()) {
            children.add(new OrderedChild(screenlet, () -> addScreenletNestedElement(doc, containerElement, screenlet)));
        }

        // SCIPIO: 4.0.0: nested sections with condition/actions (were unsupported: converter flattened them and lost the condition)
        for (SectionNested nestedSection : container.sections()) {
            children.add(new OrderedChild(nestedSection, () -> addSectionNestedElement(doc, containerElement, nestedSection)));
        }
        for (DecoratorSectionInclude decoratorSectionInclude : container.decoratorSectionIncludes()) {
            children.add(new OrderedChild(decoratorSectionInclude, () -> addDecoratorSectionIncludeElement(doc, containerElement, decoratorSectionInclude)));
        }
        emitOrderedUnchecked(children);
        parentElement.appendChild(containerElement);
    }

    /**
     * Adds a &lt;container&gt; element from Container2 annotation.
     */
    /** SCIPIO: 4.0.0: True when the container only exists to carry sections, so it must emit no div. */
    protected boolean isBareSectionWrapper(Container container) {
        return container.sections().length > 0
                && UtilValidate.isEmpty(container.style())
                && UtilValidate.isEmpty(container.id())
                && UtilValidate.isEmpty(container.type())
                && UtilValidate.isEmpty(container.contains())
                && UtilValidate.isEmpty(container.autoUpdateTargetId())
                && container.autoUpdateInterval() <= 0
                && container.widgets().length == 0
                && container.includeForms().length == 0
                && container.includeScreens().length == 0
                && container.labels().length == 0
                && container.htmlTemplates().length == 0
                && container.includeMenus().length == 0
                && container.containers().length == 0
                && container.screenlets().length == 0
                && container.decoratorSectionIncludes().length == 0
                && UtilValidate.isEmpty(container.decorator().name());
    }

    protected void addContainer2Element(Document doc, Element parentElement, Container2 container) {
        Element containerElement = doc.createElement("container");

        if (UtilValidate.isNotEmpty(container.style())) {
            containerElement.setAttribute("style", container.style());
        }
        if (UtilValidate.isNotEmpty(container.id())) {
            containerElement.setAttribute("id", container.id());
        }
        if (UtilValidate.isNotEmpty(container.autoUpdateTargetId())) {
            containerElement.setAttribute("auto-update-target-id", container.autoUpdateTargetId());
        }
        if (container.autoUpdateInterval() > 0) {
            containerElement.setAttribute("auto-update-interval", String.valueOf(container.autoUpdateInterval()));
        }
        if (UtilValidate.isNotEmpty(container.type())) {
            containerElement.setAttribute("type", container.type());
        }
        if (UtilValidate.isNotEmpty(container.contains())) {
            containerElement.setAttribute("contains", container.contains());
        }

        // Add nested widgets
        // SCIPIO: 4.0.0: generic widgets (link, image, content, sub-content, ...) were dropped by the converter
        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered()
        List<OrderedChild> children = new ArrayList<>();
        for (Widget containerWidget : container.widgets()) {
            children.add(new OrderedChild(containerWidget, () -> addUnifiedWidgetElement(doc, containerElement, containerWidget)));
        }
        for (IncludeForm includeForm : container.includeForms()) {
            children.add(new OrderedChild(includeForm, () -> addIncludeFormElement(doc, containerElement, includeForm)));
        }
        for (IncludeScreen includeScreen : container.includeScreens()) {
            children.add(new OrderedChild(includeScreen, () -> addIncludeScreenElement(doc, containerElement, includeScreen)));
        }
        for (Label label : container.labels()) {
            children.add(new OrderedChild(label, () -> addLabelElement(doc, containerElement, label)));
        }
        for (HtmlTemplate htmlTemplate : container.htmlTemplates()) {
            children.add(new OrderedChild(htmlTemplate, () -> addHtmlTemplateElement(doc, containerElement, htmlTemplate)));
        }
        for (IncludeMenu includeMenu : container.includeMenus()) {
            children.add(new OrderedChild(includeMenu, () -> addIncludeMenuElement(doc, containerElement, includeMenu)));
        }
        for (Container3 nestedContainer : container.containers()) {
            children.add(new OrderedChild(nestedContainer, () -> addContainer3Element(doc, containerElement, nestedContainer)));
        }
        // SCIPIO: 4.0.0: Process screenlets nested inside containers (uses ScreenletNested)
        for (ScreenletNested screenlet : container.screenlets()) {
            children.add(new OrderedChild(screenlet, () -> addScreenletNestedElement(doc, containerElement, screenlet)));
        }

        // SCIPIO: 4.0.0: nested sections with condition/actions (were unsupported: converter flattened them and lost the condition)
        for (SectionNested2 nestedSection : container.sections()) {
            children.add(new OrderedChild(nestedSection, () -> addSectionNestedElement(doc, containerElement, nestedSection)));
        }
        for (DecoratorSectionInclude decoratorSectionInclude : container.decoratorSectionIncludes()) {
            children.add(new OrderedChild(decoratorSectionInclude, () -> addDecoratorSectionIncludeElement(doc, containerElement, decoratorSectionInclude)));
        }
        emitOrderedUnchecked(children);
        parentElement.appendChild(containerElement);
    }

    /**
     * Adds a &lt;container&gt; element from Container3 annotation.
     */
    protected void addContainer3Element(Document doc, Element parentElement, Container3 container) {
        Element containerElement = doc.createElement("container");

        if (UtilValidate.isNotEmpty(container.style())) {
            containerElement.setAttribute("style", container.style());
        }
        if (UtilValidate.isNotEmpty(container.id())) {
            containerElement.setAttribute("id", container.id());
        }
        if (UtilValidate.isNotEmpty(container.autoUpdateTargetId())) {
            containerElement.setAttribute("auto-update-target-id", container.autoUpdateTargetId());
        }
        if (container.autoUpdateInterval() > 0) {
            containerElement.setAttribute("auto-update-interval", String.valueOf(container.autoUpdateInterval()));
        }
        if (UtilValidate.isNotEmpty(container.type())) {
            containerElement.setAttribute("type", container.type());
        }
        if (UtilValidate.isNotEmpty(container.contains())) {
            containerElement.setAttribute("contains", container.contains());
        }

        // Add nested widgets
        // SCIPIO: 4.0.0: generic widgets (link, image, content, sub-content, ...) were dropped by the converter
        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered()
        List<OrderedChild> children = new ArrayList<>();
        for (Widget containerWidget : container.widgets()) {
            children.add(new OrderedChild(containerWidget, () -> addUnifiedWidgetElement(doc, containerElement, containerWidget)));
        }
        for (IncludeForm includeForm : container.includeForms()) {
            children.add(new OrderedChild(includeForm, () -> addIncludeFormElement(doc, containerElement, includeForm)));
        }
        for (IncludeScreen includeScreen : container.includeScreens()) {
            children.add(new OrderedChild(includeScreen, () -> addIncludeScreenElement(doc, containerElement, includeScreen)));
        }
        for (Label label : container.labels()) {
            children.add(new OrderedChild(label, () -> addLabelElement(doc, containerElement, label)));
        }
        for (HtmlTemplate htmlTemplate : container.htmlTemplates()) {
            children.add(new OrderedChild(htmlTemplate, () -> addHtmlTemplateElement(doc, containerElement, htmlTemplate)));
        }
        for (IncludeMenu includeMenu : container.includeMenus()) {
            children.add(new OrderedChild(includeMenu, () -> addIncludeMenuElement(doc, containerElement, includeMenu)));
        }
        for (Container4 nestedContainer : container.containers()) {
            children.add(new OrderedChild(nestedContainer, () -> addContainer4Element(doc, containerElement, nestedContainer)));
        }
        // SCIPIO: 4.0.0: Process screenlets nested inside containers (uses ScreenletNested)
        for (ScreenletNested screenlet : container.screenlets()) {
            children.add(new OrderedChild(screenlet, () -> addScreenletNestedElement(doc, containerElement, screenlet)));
        }

        for (DecoratorSectionInclude decoratorSectionInclude : container.decoratorSectionIncludes()) {
            children.add(new OrderedChild(decoratorSectionInclude, () -> addDecoratorSectionIncludeElement(doc, containerElement, decoratorSectionInclude)));
        }
        emitOrderedUnchecked(children);
        parentElement.appendChild(containerElement);
    }

    /**
     * Adds a &lt;container&gt; element from Container4 annotation (leaf level).
     */
    protected void addContainer4Element(Document doc, Element parentElement, Container4 container) {
        Element containerElement = doc.createElement("container");

        if (UtilValidate.isNotEmpty(container.style())) {
            containerElement.setAttribute("style", container.style());
        }
        if (UtilValidate.isNotEmpty(container.id())) {
            containerElement.setAttribute("id", container.id());
        }
        if (UtilValidate.isNotEmpty(container.autoUpdateTargetId())) {
            containerElement.setAttribute("auto-update-target-id", container.autoUpdateTargetId());
        }
        if (container.autoUpdateInterval() > 0) {
            containerElement.setAttribute("auto-update-interval", String.valueOf(container.autoUpdateInterval()));
        }
        if (UtilValidate.isNotEmpty(container.type())) {
            containerElement.setAttribute("type", container.type());
        }
        if (UtilValidate.isNotEmpty(container.contains())) {
            containerElement.setAttribute("contains", container.contains());
        }

        // Add nested widgets (no further container nesting allowed at this level)
        // SCIPIO: 4.0.0: generic widgets (link, image, content, sub-content, ...) were dropped by the converter
        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered()
        List<OrderedChild> children = new ArrayList<>();
        for (Widget containerWidget : container.widgets()) {
            children.add(new OrderedChild(containerWidget, () -> addUnifiedWidgetElement(doc, containerElement, containerWidget)));
        }
        for (IncludeForm includeForm : container.includeForms()) {
            children.add(new OrderedChild(includeForm, () -> addIncludeFormElement(doc, containerElement, includeForm)));
        }
        for (IncludeScreen includeScreen : container.includeScreens()) {
            children.add(new OrderedChild(includeScreen, () -> addIncludeScreenElement(doc, containerElement, includeScreen)));
        }
        for (Label label : container.labels()) {
            children.add(new OrderedChild(label, () -> addLabelElement(doc, containerElement, label)));
        }
        for (HtmlTemplate htmlTemplate : container.htmlTemplates()) {
            children.add(new OrderedChild(htmlTemplate, () -> addHtmlTemplateElement(doc, containerElement, htmlTemplate)));
        }
        for (IncludeMenu includeMenu : container.includeMenus()) {
            children.add(new OrderedChild(includeMenu, () -> addIncludeMenuElement(doc, containerElement, includeMenu)));
        }
        // SCIPIO: 4.0.0: Process screenlets nested inside containers (uses ScreenletNested)
        for (ScreenletNested screenlet : container.screenlets()) {
            children.add(new OrderedChild(screenlet, () -> addScreenletNestedElement(doc, containerElement, screenlet)));
        }

        for (DecoratorSectionInclude decoratorSectionInclude : container.decoratorSectionIncludes()) {
            children.add(new OrderedChild(decoratorSectionInclude, () -> addDecoratorSectionIncludeElement(doc, containerElement, decoratorSectionInclude)));
        }
        emitOrderedUnchecked(children);
        parentElement.appendChild(containerElement);
    }

    /**
     * Adds a &lt;screenlet&gt; element from ScreenletNested annotation.
     * SCIPIO: 4.0.0: Added for nested screenlet support inside containers.
     */
    protected void addScreenletNestedElement(Document doc, Element parentElement, ScreenletNested screenlet) {
        Element screenletElement = doc.createElement("screenlet");

        if (UtilValidate.isNotEmpty(screenlet.title())) {
            screenletElement.setAttribute("title", screenlet.title());
        }
        if (UtilValidate.isNotEmpty(screenlet.name())) {
            screenletElement.setAttribute("name", screenlet.name());
        }
        if (UtilValidate.isNotEmpty(screenlet.id())) {
            screenletElement.setAttribute("id", screenlet.id());
        }
        if (screenlet.collapsible()) {
            screenletElement.setAttribute("collapsible", "true");
        }
        if (screenlet.initiallyCollapsed()) {
            screenletElement.setAttribute("initially-collapsed", "true");
        }
        if (!screenlet.saveCollapsed()) {
            screenletElement.setAttribute("save-collapsed", "false");
        }
        if (!screenlet.padded()) {
            screenletElement.setAttribute("padded", "false");
        }
        if (UtilValidate.isNotEmpty(screenlet.titleStyle())) {
            screenletElement.setAttribute("title-style", screenlet.titleStyle());
        }
        if (UtilValidate.isNotEmpty(screenlet.navigationMenuName())) {
            screenletElement.setAttribute("navigation-menu-name", screenlet.navigationMenuName());
        }
        if (UtilValidate.isNotEmpty(screenlet.navigationFormName())) {
            screenletElement.setAttribute("navigation-form-name", screenlet.navigationFormName());
        }
        if (UtilValidate.isNotEmpty(screenlet.tabMenuName())) {
            screenletElement.setAttribute("tab-menu-name", screenlet.tabMenuName());
        }

        // SCIPIO: 4.0.0: Screenlet children are added directly (no <widgets> wrapper - Screenlet reads children directly)
        // Add containers (uses ContainerInScreenlet to avoid cycles)
        // SCIPIO: 4.0.0: a section directly inside a nested screenlet was dropped without a warning
        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered()
        List<OrderedChild> children = new ArrayList<>();
        // SCIPIO: 4.0.0: link, content, include-tree, iterate-section, ... (were dropped by the converter)
        for (Widget x : screenlet.widgets()) {
            children.add(new OrderedChild(x, () -> addUnifiedWidgetElement(doc, screenletElement, x)));
        }
        for (SectionLeaf section : screenlet.sections()) {
            children.add(new OrderedChild(section, () -> addSectionLeafElement(doc, screenletElement, section)));
        }
        for (ContainerInScreenlet container : screenlet.containers()) {
            children.add(new OrderedChild(container, () -> addContainerInScreenletElement(doc, screenletElement, container)));
        }
        for (IncludeForm includeForm : screenlet.includeForms()) {
            children.add(new OrderedChild(includeForm, () -> addIncludeFormElement(doc, screenletElement, includeForm)));
        }
        for (IncludeScreen includeScreen : screenlet.includeScreens()) {
            children.add(new OrderedChild(includeScreen, () -> addIncludeScreenElement(doc, screenletElement, includeScreen)));
        }
        for (IncludeMenu includeMenu : screenlet.includeMenus()) {
            children.add(new OrderedChild(includeMenu, () -> addIncludeMenuElement(doc, screenletElement, includeMenu)));
        }
        for (Label label : screenlet.labels()) {
            children.add(new OrderedChild(label, () -> addLabelElement(doc, screenletElement, label)));
        }
        for (HtmlTemplate htmlTemplate : screenlet.htmlTemplates()) {
            children.add(new OrderedChild(htmlTemplate, () -> addHtmlTemplateElement(doc, screenletElement, htmlTemplate)));
        }

        for (DecoratorSectionInclude decoratorSectionInclude : screenlet.decoratorSectionIncludes()) {
            children.add(new OrderedChild(decoratorSectionInclude, () -> addDecoratorSectionIncludeElement(doc, screenletElement, decoratorSectionInclude)));
        }
        emitOrderedUnchecked(children);
        parentElement.appendChild(screenletElement);
    }

    /**
     * Adds a container element from ContainerInScreenlet annotation.
     * SCIPIO: 4.0.0: Added for container support inside screenlets.
     */
    protected void addContainerInScreenletElement(Document doc, Element parentElement, ContainerInScreenlet container) {
        Element containerElement = doc.createElement("container");
        for (DecoratorSectionInclude dsi : container.decoratorSectionIncludes()) { // SCIPIO: 4.0.0
            Element dsiElement = doc.createElement("decorator-section-include");
            dsiElement.setAttribute("name", dsi.name());
            containerElement.appendChild(dsiElement);
        }

        if (UtilValidate.isNotEmpty(container.id())) {
            containerElement.setAttribute("id", container.id());
        }
        if (UtilValidate.isNotEmpty(container.style())) {
            containerElement.setAttribute("style", container.style());
        }

        // Add nested containers (level 2)
        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered()
        List<OrderedChild> children = new ArrayList<>();
        for (ContainerInScreenlet2 nested : container.containers()) {
            children.add(new OrderedChild(nested, () -> addContainerInScreenlet2Element(doc, containerElement, nested)));
        }
        // Add direct widgets
        for (IncludeScreen includeScreen : container.includeScreens()) {
            children.add(new OrderedChild(includeScreen, () -> addIncludeScreenElement(doc, containerElement, includeScreen)));
        }
        // SCIPIO: 4.0.0: generic widgets (link, image, content, sub-content, ...) were dropped by the converter
        for (Widget containerWidget : container.widgets()) {
            children.add(new OrderedChild(containerWidget, () -> addUnifiedWidgetElement(doc, containerElement, containerWidget)));
        }
        for (IncludeForm includeForm : container.includeForms()) {
            children.add(new OrderedChild(includeForm, () -> addIncludeFormElement(doc, containerElement, includeForm)));
        }
        for (HtmlTemplate htmlTemplate : container.htmlTemplates()) {
            children.add(new OrderedChild(htmlTemplate, () -> addHtmlTemplateElement(doc, containerElement, htmlTemplate)));
        }
        // SCIPIO: 4.0.0: label/include-menu children were dropped by the converter
        for (Label label : container.labels()) {
            children.add(new OrderedChild(label, () -> addLabelElement(doc, containerElement, label)));
        }
        for (IncludeMenu includeMenu : container.includeMenus()) {
            children.add(new OrderedChild(includeMenu, () -> addIncludeMenuElement(doc, containerElement, includeMenu)));
        }
        // SCIPIO: 4.0.0: a section inside a screenlet container was dropped without a warning
        for (SectionLeaf section : container.sections()) {
            children.add(new OrderedChild(section, () -> addSectionLeafElement(doc, containerElement, section)));
        }
        emitOrderedUnchecked(children);

        parentElement.appendChild(containerElement);
    }

    /**
     * Adds a container element from ContainerInScreenlet2 annotation (leaf level).
     * SCIPIO: 4.0.0: Added for container support inside screenlets.
     */
    protected void addContainerInScreenlet2Element(Document doc, Element parentElement, ContainerInScreenlet2 container) {
        Element containerElement = doc.createElement("container");
        for (DecoratorSectionInclude dsi : container.decoratorSectionIncludes()) { // SCIPIO: 4.0.0
            Element dsiElement = doc.createElement("decorator-section-include");
            dsiElement.setAttribute("name", dsi.name());
            containerElement.appendChild(dsiElement);
        }

        if (UtilValidate.isNotEmpty(container.id())) {
            containerElement.setAttribute("id", container.id());
        }
        if (UtilValidate.isNotEmpty(container.style())) {
            containerElement.setAttribute("style", container.style());
        }

        // Add direct widgets (no further nesting - leaf level)
        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered()
        List<OrderedChild> children = new ArrayList<>();
        for (IncludeScreen includeScreen : container.includeScreens()) {
            children.add(new OrderedChild(includeScreen, () -> addIncludeScreenElement(doc, containerElement, includeScreen)));
        }
        // SCIPIO: 4.0.0: generic widgets (link, image, content, sub-content, ...) were dropped by the converter
        for (Widget containerWidget : container.widgets()) {
            children.add(new OrderedChild(containerWidget, () -> addUnifiedWidgetElement(doc, containerElement, containerWidget)));
        }
        for (IncludeForm includeForm : container.includeForms()) {
            children.add(new OrderedChild(includeForm, () -> addIncludeFormElement(doc, containerElement, includeForm)));
        }
        for (HtmlTemplate htmlTemplate : container.htmlTemplates()) {
            children.add(new OrderedChild(htmlTemplate, () -> addHtmlTemplateElement(doc, containerElement, htmlTemplate)));
        }
        // SCIPIO: 4.0.0: label/include-menu children were dropped by the converter
        for (Label label : container.labels()) {
            children.add(new OrderedChild(label, () -> addLabelElement(doc, containerElement, label)));
        }
        for (IncludeMenu includeMenu : container.includeMenus()) {
            children.add(new OrderedChild(includeMenu, () -> addIncludeMenuElement(doc, containerElement, includeMenu)));
        }
        // SCIPIO: 4.0.0: a section inside a screenlet container was dropped without a warning
        for (SectionLeaf section : container.sections()) {
            children.add(new OrderedChild(section, () -> addSectionLeafElement(doc, containerElement, section)));
        }
        emitOrderedUnchecked(children);

        parentElement.appendChild(containerElement);
    }

    /**
     * Adds a &lt;platform-specific&gt;&lt;html&gt;&lt;html-template&gt; element to widgets.
     */
    /**
     * SCIPIO: 4.0.0: Appends a platform branch (html, xsl-fo, text, xml, csv, xls, email) to the
     * previous platform-specific sibling when that one has no such branch yet, else to a new element.
     * The XML held the alternates of one slot in one platform-specific element and the renderer picks
     * the branch by its own name; one element per template would make the FO renderer fall back to html.
     */
    protected Element appendPlatformBranch(Document doc, Element parentElement, String platform) {
        String branchName = UtilValidate.isNotEmpty(platform) ? platform : "html";
        org.w3c.dom.Node lastChild = parentElement.getLastChild();
        while (lastChild != null && lastChild.getNodeType() != org.w3c.dom.Node.ELEMENT_NODE) {
            lastChild = lastChild.getPreviousSibling();
        }
        if (lastChild != null && "platform-specific".equals(lastChild.getNodeName())) {
            Element previous = (Element) lastChild;
            if (previous.getElementsByTagName(branchName).getLength() == 0) {
                Element branch = doc.createElement(branchName);
                previous.appendChild(branch);
                return branch;
            }
        }
        Element platformSpecific = doc.createElement("platform-specific");
        Element branch = doc.createElement(branchName);
        platformSpecific.appendChild(branch);
        parentElement.appendChild(platformSpecific);
        return branch;
    }

    protected void addHtmlTemplateElement(Document doc, Element parentElement, HtmlTemplate htmlTemplate) {
        // Either location or content must be provided
        if (UtilValidate.isEmpty(htmlTemplate.location()) && UtilValidate.isEmpty(htmlTemplate.content())) {
            return;
        }

        // SCIPIO: 4.0.0: platform() selects the branch (html, xsl-fo, text, xml); alternates of one slot
        // share one platform-specific element, as the XML had them
        Element html = appendPlatformBranch(doc, parentElement, htmlTemplate.platform());
        Element htmlTemplateElement = doc.createElement("html-template");

        if (UtilValidate.isNotEmpty(htmlTemplate.location())) {
            htmlTemplateElement.setAttribute("location", htmlTemplate.location());
        }
        if (!"ftl".equals(htmlTemplate.lang())) {
            htmlTemplateElement.setAttribute("lang", htmlTemplate.lang());
        }
        if (!htmlTemplate.trimLines()) {
            htmlTemplateElement.setAttribute("trim-lines", "false");
        }
        // Add inline content if provided
        if (UtilValidate.isNotEmpty(htmlTemplate.content())) {
            htmlTemplateElement.setTextContent(htmlTemplate.content());
        }

        html.appendChild(htmlTemplateElement);
    }

    /**
     * Adds a &lt;section&gt; element to widgets.
     */
    protected void addSectionElement(Document doc, Element parentElement, Section section) {
        Element sectionElement = doc.createElement("section");

        if (UtilValidate.isNotEmpty(section.name())) {
            sectionElement.setAttribute("name", section.name());
        }
        if (section.shareScope()) {
            sectionElement.setAttribute("share-scope", "true");
        }
        if (UtilValidate.isNotEmpty(section.contains())) {
            sectionElement.setAttribute("contains", section.contains());
        }
        if (UtilValidate.isNotEmpty(section.id())) {
            sectionElement.setAttribute("id", section.id());
        }
        if (UtilValidate.isNotEmpty(section.style())) {
            sectionElement.setAttribute("style", section.style());
        }

        // Add condition if present
        Condition condition = section.condition();
        if (hasCondition(condition)) {
            Element conditionElement = buildConditionElement(doc, condition);
            if (conditionElement != null) {
                sectionElement.appendChild(conditionElement);
            }
        }

        // Add actions if present
        Actions actions = section.actions();
        if (hasActions(actions)) {
            Element actionsElement = buildSectionActionsElement(doc, actions);
            if (actionsElement != null && actionsElement.hasChildNodes()) {
                sectionElement.appendChild(actionsElement);
            }
        }

        // Add widgets
        Widgets widgets = section.widgets();
        if (hasWidgets(widgets)) {
            Element widgetsElement = buildSectionWidgetsElement(doc, widgets);
            if (widgetsElement != null && widgetsElement.hasChildNodes()) {
                sectionElement.appendChild(widgetsElement);
            }
        }

        // Add fail-widgets from failWidgets()
        Widgets failWidgets = section.failWidgets();
        if (hasWidgets(failWidgets)) {
            Element failWidgetsElement = doc.createElement("fail-widgets");
            addWidgetsContent(doc, failWidgetsElement, failWidgets);
            if (failWidgetsElement.hasChildNodes()) {
                sectionElement.appendChild(failWidgetsElement);
            }
        }

        // Add fail-widgets from failWidgetsBlock() if not already added
        FailWidgets failWidgetsBlock = section.failWidgetsBlock();
        if (hasFailWidgets(failWidgetsBlock) && !hasWidgets(failWidgets)) {
            Element failWidgetsElement = doc.createElement("fail-widgets");
            addFailWidgetsContent(doc, failWidgetsElement, failWidgetsBlock);
            if (failWidgetsElement.hasChildNodes()) {
                sectionElement.appendChild(failWidgetsElement);
            }
        }

        // Add catch-actions if present
        Actions catchActions = section.catchActions();
        if (hasActions(catchActions)) {
            Element catchActionsElement = doc.createElement("catch-actions");
            addActionsContent(doc, catchActionsElement, catchActions);
            if (catchActionsElement.hasChildNodes()) {
                sectionElement.appendChild(catchActionsElement);
            }
        }

        // Add finally-actions if present
        Actions finallyActions = section.finallyActions();
        if (hasActions(finallyActions)) {
            Element finallyActionsElement = doc.createElement("finally-actions");
            addActionsContent(doc, finallyActionsElement, finallyActions);
            if (finallyActionsElement.hasChildNodes()) {
                sectionElement.appendChild(finallyActionsElement);
            }
        }

        parentElement.appendChild(sectionElement);
    }

    /**
     * Checks if FailWidgets has any content.
     */
    protected boolean hasFailWidgets(FailWidgets failWidgets) {
        return failWidgets.includeScreens().length > 0 ||
               failWidgets.includeForms().length > 0 ||
               failWidgets.includeMenus().length > 0 ||
               failWidgets.labels().length > 0 ||
               failWidgets.screenlets().length > 0 ||
               failWidgets.containers().length > 0 ||
               failWidgets.htmlTemplates().length > 0 ||
               failWidgets.decoratorSectionIncludes().length > 0 ||
               failWidgets.images().length > 0 ||
               failWidgets.horizontalSeparators().length > 0 ||
               failWidgets.contents().length > 0;
    }

    /**
     * Adds content from FailWidgets to a fail-widgets element.
     */
    protected void addFailWidgetsContent(Document doc, Element failWidgetsElement, FailWidgets failWidgets) {
        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered()
        List<OrderedChild> children = new ArrayList<>();
        for (DecoratorSectionInclude x : failWidgets.decoratorSectionIncludes()) {
            children.add(new OrderedChild(x, () -> addDecoratorSectionIncludeElement(doc, failWidgetsElement, x)));
        }
        for (IncludeScreen x : failWidgets.includeScreens()) {
            children.add(new OrderedChild(x, () -> addIncludeScreenElement(doc, failWidgetsElement, x)));
        }
        for (IncludeForm x : failWidgets.includeForms()) {
            children.add(new OrderedChild(x, () -> addIncludeFormElement(doc, failWidgetsElement, x)));
        }
        for (IncludeMenu x : failWidgets.includeMenus()) {
            children.add(new OrderedChild(x, () -> addIncludeMenuElement(doc, failWidgetsElement, x)));
        }
        for (Label x : failWidgets.labels()) {
            children.add(new OrderedChild(x, () -> addLabelElement(doc, failWidgetsElement, x)));
        }
        for (Screenlet x : failWidgets.screenlets()) {
            children.add(new OrderedChild(x, () -> addScreenletElement(doc, failWidgetsElement, x)));
        }
        for (Container x : failWidgets.containers()) {
            children.add(new OrderedChild(x, () -> addContainerElement(doc, failWidgetsElement, x)));
        }
        for (HtmlTemplate x : failWidgets.htmlTemplates()) {
            children.add(new OrderedChild(x, () -> addHtmlTemplateElement(doc, failWidgetsElement, x)));
        }
        for (Image x : failWidgets.images()) {
            children.add(new OrderedChild(x, () -> addImageElement(doc, failWidgetsElement, x)));
        }
        for (HorizontalSeparator x : failWidgets.horizontalSeparators()) {
            children.add(new OrderedChild(x, () -> addHorizontalSeparatorElement(doc, failWidgetsElement, x)));
        }
        for (Content x : failWidgets.contents()) {
            children.add(new OrderedChild(x, () -> addContentElement(doc, failWidgetsElement, x)));
        }
        emitOrderedUnchecked(children);
    }

    /**
     * Adds content from Actions to an actions element.
     *
     * <p>Supports two modes:</p>
     * <ul>
     *   <li><strong>Unified mode:</strong> When {@code actions.value()} contains Action elements,
     *       they are processed in array order, preserving execution sequence.</li>
     *   <li><strong>Legacy mode:</strong> When using type-specific arrays (set, service, etc.),
     *       actions are processed in a fixed type order (NOT recommended for order-sensitive cases).</li>
     * </ul>
     */
    protected void addActionsContent(Document doc, Element actionsElement, Actions actions) {
        Action[] unifiedActions = actions.value();
        IfAction2[] nestedIfs = actions.ifs();
        if (unifiedActions.length > 0 || nestedIfs.length > 0) {
            // SCIPIO: 4.0.0: merge unified actions and nested if blocks by declaration order when stamped
            java.util.List<Object[]> entries = new java.util.ArrayList<>();
            int seq = 0;
            for (Action action : unifiedActions) {
                entries.add(new Object[] { action.order() >= 0 ? action.order() : Integer.MIN_VALUE, seq++, action });
            }
            for (IfAction2 nestedIf : nestedIfs) {
                entries.add(new Object[] { nestedIf.order() >= 0 ? nestedIf.order() : Integer.MAX_VALUE, seq++, nestedIf });
            }
            entries.sort((a, b) -> {
                int c = Integer.compare((Integer) a[0], (Integer) b[0]);
                return (c != 0) ? c : Integer.compare((Integer) a[1], (Integer) b[1]);
            });
            for (Object[] entry : entries) {
                if (entry[2] instanceof Action) {
                    addUnifiedActionElement(doc, actionsElement, (Action) entry[2]);
                } else {
                    addIfAction2Element(doc, actionsElement, (IfAction2) entry[2]);
                }
            }
            return;
        }

        // Legacy mode: type-specific arrays (order NOT guaranteed)
        for (SetAction setAction : actions.set()) {
            addSetActionElement(doc, actionsElement, setAction);
        }
        for (ClearFieldAction clearField : actions.clearField()) {
            addClearFieldActionElement(doc, actionsElement, clearField);
        }
        for (ServiceAction serviceAction : actions.service()) {
            addServiceActionElement(doc, actionsElement, serviceAction);
        }
        for (EntityOneAction entityOne : actions.entityOne()) {
            addEntityOneActionElement(doc, actionsElement, entityOne);
        }
        for (EntityAndAction entityAnd : actions.entityAnd()) {
            addEntityAndActionElement(doc, actionsElement, entityAnd);
        }
        for (EntityConditionAction entityCondition : actions.entityCondition()) {
            addEntityConditionActionElement(doc, actionsElement, entityCondition);
        }
        for (GetRelatedOneAction getRelatedOne : actions.getRelatedOne()) {
            addGetRelatedOneActionElement(doc, actionsElement, getRelatedOne);
        }
        for (GetRelatedAction getRelated : actions.getRelated()) {
            addGetRelatedActionElement(doc, actionsElement, getRelated);
        }
        for (ScriptAction script : actions.script()) {
            addScriptActionElement(doc, actionsElement, script);
        }
        for (PropertyToFieldAction prop : actions.propertyToField()) {
            addPropertyToFieldActionElement(doc, actionsElement, prop);
        }
        for (PropertyMapAction propMap : actions.propertyMap()) {
            addPropertyMapActionElement(doc, actionsElement, propMap);
        }
        for (IncludeScreenActionsAction includeScreenActions : actions.includeScreenActions()) {
            addIncludeScreenActionsActionElement(doc, actionsElement, includeScreenActions);
        }
        for (IncludeFormActionsAction includeFormActions : actions.includeFormActions()) {
            addIncludeFormActionsActionElement(doc, actionsElement, includeFormActions);
        }
        for (IncludeFormRowActionsAction includeFormRowActions : actions.includeFormRowActions()) {
            addIncludeFormRowActionsActionElement(doc, actionsElement, includeFormRowActions);
        }
        for (IncludeMenuActionsAction includeMenuActions : actions.includeMenuActions()) {
            addIncludeMenuActionsActionElement(doc, actionsElement, includeMenuActions);
        }
        for (IncludeTreeActionsAction includeTreeActions : actions.includeTreeActions()) {
            addIncludeTreeActionsActionElement(doc, actionsElement, includeTreeActions);
        }
        for (ConditionToFieldAction conditionToField : actions.conditionToField()) {
            addConditionToFieldActionElement(doc, actionsElement, conditionToField);
        }
        for (CloseObjectAction closeObject : actions.closeObject()) {
            addCloseObjectActionElement(doc, actionsElement, closeObject);
        }
        for (ThrowExceptionAction throwException : actions.throwException()) {
            addThrowExceptionActionElement(doc, actionsElement, throwException);
        }
    }

    /**
     * Adds a unified Action element to the actions element.
     * Dispatches to the appropriate element builder based on action type.
     */
    protected void addUnifiedActionElement(Document doc, Element actionsElement, Action action) {
        switch (action.type()) {
            case SET:
                addSetActionElementFromUnified(doc, actionsElement, action);
                break;
            case CLEAR_FIELD:
                addClearFieldActionElementFromUnified(doc, actionsElement, action);
                break;
            case SERVICE:
                addServiceActionElementFromUnified(doc, actionsElement, action);
                break;
            case ENTITY_ONE:
                addEntityOneActionElementFromUnified(doc, actionsElement, action);
                break;
            case ENTITY_AND:
                addEntityAndActionElementFromUnified(doc, actionsElement, action);
                break;
            case ENTITY_CONDITION:
                addEntityConditionActionElementFromUnified(doc, actionsElement, action);
                break;
            case GET_RELATED_ONE:
                addGetRelatedOneActionElementFromUnified(doc, actionsElement, action);
                break;
            case GET_RELATED:
                addGetRelatedActionElementFromUnified(doc, actionsElement, action);
                break;
            case SCRIPT:
                addScriptActionElementFromUnified(doc, actionsElement, action);
                break;
            case PROPERTY_TO_FIELD:
                addPropertyToFieldActionElementFromUnified(doc, actionsElement, action);
                break;
            case PROPERTY_MAP:
                addPropertyMapActionElementFromUnified(doc, actionsElement, action);
                break;
            case INCLUDE_SCREEN_ACTIONS:
                addIncludeScreenActionsActionElementFromUnified(doc, actionsElement, action);
                break;
            case INCLUDE_FORM_ACTIONS:
                addIncludeFormActionsActionElementFromUnified(doc, actionsElement, action);
                break;
            case INCLUDE_FORM_ROW_ACTIONS:
                addIncludeFormRowActionsActionElementFromUnified(doc, actionsElement, action);
                break;
            case INCLUDE_MENU_ACTIONS:
                addIncludeMenuActionsActionElementFromUnified(doc, actionsElement, action);
                break;
            case INCLUDE_TREE_ACTIONS:
                addIncludeTreeActionsActionElementFromUnified(doc, actionsElement, action);
                break;
            case CONDITION_TO_FIELD:
                addConditionToFieldActionElementFromUnified(doc, actionsElement, action);
                break;
            case CLOSE_OBJECT:
                addCloseObjectActionElementFromUnified(doc, actionsElement, action);
                break;
            case THROW_EXCEPTION:
                addThrowExceptionActionElementFromUnified(doc, actionsElement, action);
                break;
            default:
                Debug.logWarning("Unknown action type: " + action.type(), module);
        }
    }

    // ========== Unified Action Element Builders ==========

    protected void addSetActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.field())) {
            return;
        }
        Element setElement = doc.createElement("set");
        setElement.setAttribute("field", action.field());
        if (UtilValidate.isNotEmpty(action.value())) {
            setElement.setAttribute("value", action.value());
        }
        if (UtilValidate.isNotEmpty(action.fromField())) {
            setElement.setAttribute("from-field", action.fromField());
        }
        if (UtilValidate.isNotEmpty(action.defaultValue())) {
            setElement.setAttribute("default-value", action.defaultValue());
        }
        if (UtilValidate.isNotEmpty(action.valueType())) {
            setElement.setAttribute("type", action.valueType());
        }
        if (action.global()) {
            setElement.setAttribute("global", "true");
        }
        if (!action.setIfEmpty()) {
            setElement.setAttribute("set-if-empty", "false");
        }
        if (!action.setIfNull()) {
            setElement.setAttribute("set-if-null", "false");
        }
        if (UtilValidate.isNotEmpty(action.fromScope())) {
            setElement.setAttribute("from-scope", action.fromScope());
        }
        actionsElement.appendChild(setElement);
    }

    protected void addClearFieldActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.field())) {
            return;
        }
        Element clearFieldElement = doc.createElement("clear-field");
        clearFieldElement.setAttribute("field", action.field());
        actionsElement.appendChild(clearFieldElement);
    }

    protected void addServiceActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.serviceName())) {
            return;
        }
        Element serviceElement = doc.createElement("service");
        serviceElement.setAttribute("service-name", action.serviceName());
        if (UtilValidate.isNotEmpty(action.resultMapName())) {
            serviceElement.setAttribute("result-map", action.resultMapName());
        }
        if (UtilValidate.isNotEmpty(action.resultMapList())) {
            serviceElement.setAttribute("result-map-list", action.resultMapList());
        }
        // SCIPIO: Always emit auto-field-map explicitly - the synthetic DOM has no XSD default-fill,
        // so an omitted attribute reads as auto-field-map OFF at runtime (drops userLogin etc.),
        // unlike parsed widget XML where the schema defaults it to "true".
        serviceElement.setAttribute("auto-field-map", action.autoFieldMap() ? "true" : "false");
        if (UtilValidate.isNotEmpty(action.resultMapField())) {
            serviceElement.setAttribute("result-map-field", action.resultMapField());
        }
        // Add field-maps
        for (FieldMap fieldMap : action.fieldMaps()) {
            addFieldMapElement(doc, serviceElement, fieldMap);
        }
        actionsElement.appendChild(serviceElement);
    }

    protected void addEntityOneActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.entityName())) {
            return;
        }
        Element entityOneElement = doc.createElement("entity-one");
        entityOneElement.setAttribute("entity-name", action.entityName());
        if (UtilValidate.isNotEmpty(action.valueField())) {
            entityOneElement.setAttribute("value-field", action.valueField());
        }
        if (!action.autoFieldMap()) {
            entityOneElement.setAttribute("auto-field-map", "false");
        }
        if (action.useCache()) {
            entityOneElement.setAttribute("use-cache", "true");
        }
        // Add field-maps
        for (FieldMap fieldMap : action.fieldMaps()) {
            addFieldMapElement(doc, entityOneElement, fieldMap);
        }
        actionsElement.appendChild(entityOneElement);
    }

    protected void addEntityAndActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.entityName()) || UtilValidate.isEmpty(action.list())) {
            return;
        }
        Element entityAndElement = doc.createElement("entity-and");
        entityAndElement.setAttribute("entity-name", action.entityName());
        entityAndElement.setAttribute("list", action.list());
        if (action.useCache()) {
            entityAndElement.setAttribute("use-cache", "true");
        }
        if (action.filterByDate()) {
            entityAndElement.setAttribute("filter-by-date", "true");
        }
        if (!"scroll".equals(action.resultSetType())) {
            entityAndElement.setAttribute("result-set-type", action.resultSetType());
        }
        if (action.limitStart() >= 0) {
            entityAndElement.setAttribute("limit-start", String.valueOf(action.limitStart()));
        }
        if (action.limitSize() >= 0) {
            entityAndElement.setAttribute("limit-size", String.valueOf(action.limitSize()));
        }
        if (action.useIterator()) {
            entityAndElement.setAttribute("use-iterator", "true");
        }
        // Add field-maps
        for (FieldMap fieldMap : action.fieldMaps()) {
            addFieldMapElement(doc, entityAndElement, fieldMap);
        }
        // Add select-fields
        for (String selectField : action.selectFields()) {
            Element selectFieldElement = doc.createElement("select-field");
            selectFieldElement.setAttribute("field-name", selectField);
            entityAndElement.appendChild(selectFieldElement);
        }
        // Add order-by
        for (String orderByField : action.orderBy()) {
            Element orderByElement = doc.createElement("order-by");
            orderByElement.setAttribute("field-name", orderByField);
            entityAndElement.appendChild(orderByElement);
        }
        actionsElement.appendChild(entityAndElement);
    }

    protected void addEntityConditionActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.entityName()) || UtilValidate.isEmpty(action.list())) {
            return;
        }
        Element entityConditionElement = doc.createElement("entity-condition");
        entityConditionElement.setAttribute("entity-name", action.entityName());
        entityConditionElement.setAttribute("list", action.list());
        if (action.useCache()) {
            entityConditionElement.setAttribute("use-cache", "true");
        }
        if (action.filterByDate()) {
            entityConditionElement.setAttribute("filter-by-date", "true");
        }
        if (action.distinct()) {
            entityConditionElement.setAttribute("distinct", "true");
        }
        if (UtilValidate.isNotEmpty(action.delegatorName())) {
            entityConditionElement.setAttribute("delegator-name", action.delegatorName());
        }
        // Add conditions
        if (action.conditions().length > 0) {
            Element conditionListElement = doc.createElement("condition-list");
            conditionListElement.setAttribute("combine", "and");
            for (ConditionExpr condExpr : action.conditions()) {
                addConditionExprElement(doc, conditionListElement, condExpr);
            }
            entityConditionElement.appendChild(conditionListElement);
        }
        // Add select-fields
        for (String selectField : action.selectFields()) {
            Element selectFieldElement = doc.createElement("select-field");
            selectFieldElement.setAttribute("field-name", selectField);
            entityConditionElement.appendChild(selectFieldElement);
        }
        // Add order-by
        for (String orderByField : action.orderBy()) {
            Element orderByElement = doc.createElement("order-by");
            orderByElement.setAttribute("field-name", orderByField);
            entityConditionElement.appendChild(orderByElement);
        }
        actionsElement.appendChild(entityConditionElement);
    }

    protected void addGetRelatedOneActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.valueField()) || UtilValidate.isEmpty(action.relationName()) ||
            UtilValidate.isEmpty(action.toValueField())) {
            return;
        }
        Element getRelatedOneElement = doc.createElement("get-related-one");
        getRelatedOneElement.setAttribute("value-field", action.valueField());
        getRelatedOneElement.setAttribute("relation-name", action.relationName());
        getRelatedOneElement.setAttribute("to-value-field", action.toValueField());
        if (action.useCache()) {
            getRelatedOneElement.setAttribute("use-cache", "true");
        }
        actionsElement.appendChild(getRelatedOneElement);
    }

    protected void addGetRelatedActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.valueField()) || UtilValidate.isEmpty(action.relationName()) ||
            UtilValidate.isEmpty(action.list())) {
            return;
        }
        Element getRelatedElement = doc.createElement("get-related");
        getRelatedElement.setAttribute("value-field", action.valueField());
        getRelatedElement.setAttribute("relation-name", action.relationName());
        getRelatedElement.setAttribute("list", action.list());
        if (UtilValidate.isNotEmpty(action.map())) {
            getRelatedElement.setAttribute("map", action.map());
        }
        if (UtilValidate.isNotEmpty(action.orderByList())) {
            getRelatedElement.setAttribute("order-by-list", action.orderByList());
        }
        if (action.useCache()) {
            getRelatedElement.setAttribute("use-cache", "true");
        }
        actionsElement.appendChild(getRelatedElement);
    }

    protected void addScriptActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.location()) && UtilValidate.isEmpty(action.script())) {
            return;
        }
        Element scriptElement = doc.createElement("script");
        if (UtilValidate.isNotEmpty(action.location())) {
            scriptElement.setAttribute("location", action.location());
        }
        if (UtilValidate.isNotEmpty(action.script())) {
            scriptElement.setTextContent(action.script());
        }
        if (UtilValidate.isNotEmpty(action.lang()) && !"groovy".equals(action.lang())) {
            scriptElement.setAttribute("lang", action.lang());
        }
        actionsElement.appendChild(scriptElement);
    }

    protected void addPropertyToFieldActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.field()) || UtilValidate.isEmpty(action.resource()) ||
            UtilValidate.isEmpty(action.property())) {
            return;
        }
        Element propElement = doc.createElement("property-to-field");
        propElement.setAttribute("field", action.field());
        propElement.setAttribute("resource", action.resource());
        propElement.setAttribute("property", action.property());
        if (UtilValidate.isNotEmpty(action.defaultValue())) {
            propElement.setAttribute("default", action.defaultValue());
        }
        if (action.noLocale()) {
            propElement.setAttribute("no-locale", "true");
        }
        if (UtilValidate.isNotEmpty(action.argListName())) {
            propElement.setAttribute("arg-list-name", action.argListName());
        }
        if (action.global()) {
            propElement.setAttribute("global", "true");
        }
        actionsElement.appendChild(propElement);
    }

    protected void addPropertyMapActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.resource()) || UtilValidate.isEmpty(action.mapName())) {
            return;
        }
        Element propMapElement = doc.createElement("property-map");
        propMapElement.setAttribute("resource", action.resource());
        propMapElement.setAttribute("map-name", action.mapName());
        if (action.global()) {
            propMapElement.setAttribute("global", "true");
        }
        if (action.optional()) {
            propMapElement.setAttribute("optional", "true");
        }
        actionsElement.appendChild(propMapElement);
    }

    protected void addIncludeScreenActionsActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-screen-actions");
        includeElement.setAttribute("name", action.name());
        if (UtilValidate.isNotEmpty(action.location())) {
            includeElement.setAttribute("location", action.location());
        }
        actionsElement.appendChild(includeElement);
    }

    protected void addIncludeFormActionsActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-form-actions");
        includeElement.setAttribute("name", action.name());
        if (UtilValidate.isNotEmpty(action.location())) {
            includeElement.setAttribute("location", action.location());
        }
        actionsElement.appendChild(includeElement);
    }

    protected void addIncludeFormRowActionsActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-form-row-actions");
        includeElement.setAttribute("name", action.name());
        if (UtilValidate.isNotEmpty(action.location())) {
            includeElement.setAttribute("location", action.location());
        }
        actionsElement.appendChild(includeElement);
    }

    protected void addIncludeMenuActionsActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-menu-actions");
        includeElement.setAttribute("name", action.name());
        if (UtilValidate.isNotEmpty(action.location())) {
            includeElement.setAttribute("location", action.location());
        }
        actionsElement.appendChild(includeElement);
    }

    protected void addIncludeTreeActionsActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-tree-actions");
        includeElement.setAttribute("name", action.name());
        if (UtilValidate.isNotEmpty(action.location())) {
            includeElement.setAttribute("location", action.location());
        }
        actionsElement.appendChild(includeElement);
    }

    protected void addConditionToFieldActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.field())) {
            return;
        }
        Element conditionToFieldElement = doc.createElement("condition-to-field");
        conditionToFieldElement.setAttribute("field", action.field());
        if (UtilValidate.isNotEmpty(action.valueType())) {
            conditionToFieldElement.setAttribute("type", action.valueType());
        }
        if (action.global()) {
            conditionToFieldElement.setAttribute("global", "true");
        }
        if (UtilValidate.isNotEmpty(action.toScope())) {
            conditionToFieldElement.setAttribute("to-scope", action.toScope());
        }
        if (UtilValidate.isNotEmpty(action.onlyIfField())) {
            conditionToFieldElement.setAttribute("only-if-field", action.onlyIfField());
        }
        // Add condition if present
        Condition condition = action.condition();
        if (hasCondition(condition)) {
            Element innerCondition = buildConditionContent(doc, condition);
            if (innerCondition != null) {
                if (condition.not()) {
                    Element notElement = doc.createElement("not");
                    notElement.appendChild(innerCondition);
                    conditionToFieldElement.appendChild(notElement);
                } else {
                    conditionToFieldElement.appendChild(innerCondition);
                }
            }
        }
        actionsElement.appendChild(conditionToFieldElement);
    }

    protected void addCloseObjectActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.field())) {
            return;
        }
        Element closeObjectElement = doc.createElement("close-object");
        closeObjectElement.setAttribute("field", action.field());
        actionsElement.appendChild(closeObjectElement);
    }

    protected void addThrowExceptionActionElementFromUnified(Document doc, Element actionsElement, Action action) {
        if (UtilValidate.isEmpty(action.field())) {
            return;
        }
        Element throwExceptionElement = doc.createElement("throw-exception");
        throwExceptionElement.setAttribute("field", action.field());
        actionsElement.appendChild(throwExceptionElement);
    }

    // ========== Helper Methods for Unified Actions ==========

    /**
     * Adds a field-map element to a parent element.
     */
    protected void addFieldMapElement(Document doc, Element parentElement, FieldMap fieldMap) {
        if (UtilValidate.isEmpty(fieldMap.fieldName())) {
            return;
        }
        Element fieldMapElement = doc.createElement("field-map");
        fieldMapElement.setAttribute("field-name", fieldMap.fieldName());
        if (UtilValidate.isNotEmpty(fieldMap.fromField())) {
            fieldMapElement.setAttribute("from-field", fieldMap.fromField());
        }
        if (UtilValidate.isNotEmpty(fieldMap.value())) {
            fieldMapElement.setAttribute("value", fieldMap.value());
        }
        parentElement.appendChild(fieldMapElement);
    }

    /**
     * Adds a condition-expr element to a condition-list element.
     */
    protected void addConditionExprElement(Document doc, Element parentElement, ConditionExpr condExpr) {
        if (UtilValidate.isEmpty(condExpr.fieldName())) {
            return;
        }
        Element condExprElement = doc.createElement("condition-expr");
        condExprElement.setAttribute("field-name", condExpr.fieldName());
        if (UtilValidate.isNotEmpty(condExpr.operator())) {
            condExprElement.setAttribute("operator", condExpr.operator());
        }
        if (UtilValidate.isNotEmpty(condExpr.value())) {
            condExprElement.setAttribute("value", condExpr.value());
        }
        if (UtilValidate.isNotEmpty(condExpr.fromField())) {
            condExprElement.setAttribute("from-field", condExpr.fromField());
        }
        if (UtilValidate.isNotEmpty(condExpr.envName())) {
            condExprElement.setAttribute("env-name", condExpr.envName());
        }
        if (condExpr.ignoreCase()) {
            condExprElement.setAttribute("ignore-case", "true");
        }
        if (condExpr.ignoreIfNull()) {
            condExprElement.setAttribute("ignore-if-null", "true");
        }
        if (condExpr.ignoreIfEmpty()) {
            condExprElement.setAttribute("ignore-if-empty", "true");
        }
        parentElement.appendChild(condExprElement);
    }

    /**
     * Adds a &lt;clear-field&gt; element to actions.
     */
    protected void addClearFieldActionElement(Document doc, Element actionsElement, ClearFieldAction clearField) {
        if (UtilValidate.isEmpty(clearField.field())) {
            return;
        }
        Element clearFieldElement = doc.createElement("clear-field");
        clearFieldElement.setAttribute("field", clearField.field());
        actionsElement.appendChild(clearFieldElement);
    }

    /**
     * Adds an &lt;entity-and&gt; element to actions.
     */
    protected void addEntityAndActionElement(Document doc, Element actionsElement, EntityAndAction entityAnd) {
        if (UtilValidate.isEmpty(entityAnd.entityName()) || UtilValidate.isEmpty(entityAnd.list())) {
            return;
        }
        Element entityAndElement = doc.createElement("entity-and");
        entityAndElement.setAttribute("entity-name", entityAnd.entityName());
        entityAndElement.setAttribute("list", entityAnd.list());

        if (entityAnd.useCache()) {
            entityAndElement.setAttribute("use-cache", "true");
        }
        if (entityAnd.filterByDate()) {
            entityAndElement.setAttribute("filter-by-date", "true");
        }
        if (!"scroll".equals(entityAnd.resultSetType())) {
            entityAndElement.setAttribute("result-set-type", entityAnd.resultSetType());
        }

        // Add field-map elements
        for (FieldMap fieldMap : entityAnd.fieldMaps()) {
            Element fieldMapElement = doc.createElement("field-map");
            fieldMapElement.setAttribute("field-name", fieldMap.fieldName());
            if (UtilValidate.isNotEmpty(fieldMap.fromField())) {
                fieldMapElement.setAttribute("from-field", fieldMap.fromField());
            }
            if (UtilValidate.isNotEmpty(fieldMap.value())) {
                fieldMapElement.setAttribute("value", fieldMap.value());
            }
            entityAndElement.appendChild(fieldMapElement);
        }

        // Add select-field elements
        for (String selectField : entityAnd.selectFields()) {
            Element selectFieldElement = doc.createElement("select-field");
            selectFieldElement.setAttribute("field-name", selectField);
            entityAndElement.appendChild(selectFieldElement);
        }

        // Add order-by elements
        for (String orderBy : entityAnd.orderBy()) {
            Element orderByElement = doc.createElement("order-by");
            orderByElement.setAttribute("field-name", orderBy);
            entityAndElement.appendChild(orderByElement);
        }

        // Add limit-range if specified
        if (entityAnd.limitStart() >= 0 || entityAnd.limitSize() >= 0) {
            Element limitRangeElement = doc.createElement("limit-range");
            if (entityAnd.limitStart() >= 0) {
                limitRangeElement.setAttribute("start", String.valueOf(entityAnd.limitStart()));
            }
            if (entityAnd.limitSize() >= 0) {
                limitRangeElement.setAttribute("size", String.valueOf(entityAnd.limitSize()));
            }
            entityAndElement.appendChild(limitRangeElement);
        }

        // Add use-iterator if specified
        if (entityAnd.useIterator()) {
            Element useIteratorElement = doc.createElement("use-iterator");
            entityAndElement.appendChild(useIteratorElement);
        }

        actionsElement.appendChild(entityAndElement);
    }

    /**
     * Adds a &lt;get-related-one&gt; element to actions.
     */
    protected void addGetRelatedOneActionElement(Document doc, Element actionsElement, GetRelatedOneAction getRelatedOne) {
        if (UtilValidate.isEmpty(getRelatedOne.valueField()) || UtilValidate.isEmpty(getRelatedOne.relationName())
                || UtilValidate.isEmpty(getRelatedOne.toValueField())) {
            return;
        }
        Element getRelatedOneElement = doc.createElement("get-related-one");
        getRelatedOneElement.setAttribute("value-field", getRelatedOne.valueField());
        getRelatedOneElement.setAttribute("relation-name", getRelatedOne.relationName());
        getRelatedOneElement.setAttribute("to-value-field", getRelatedOne.toValueField());
        if (getRelatedOne.useCache()) {
            getRelatedOneElement.setAttribute("use-cache", "true");
        }
        actionsElement.appendChild(getRelatedOneElement);
    }

    /**
     * Adds a &lt;get-related&gt; element to actions.
     */
    protected void addGetRelatedActionElement(Document doc, Element actionsElement, GetRelatedAction getRelated) {
        if (UtilValidate.isEmpty(getRelated.valueField()) || UtilValidate.isEmpty(getRelated.relationName())
                || UtilValidate.isEmpty(getRelated.list())) {
            return;
        }
        Element getRelatedElement = doc.createElement("get-related");
        getRelatedElement.setAttribute("value-field", getRelated.valueField());
        getRelatedElement.setAttribute("relation-name", getRelated.relationName());
        getRelatedElement.setAttribute("list", getRelated.list());
        if (UtilValidate.isNotEmpty(getRelated.map())) {
            getRelatedElement.setAttribute("map", getRelated.map());
        }
        if (UtilValidate.isNotEmpty(getRelated.orderByList())) {
            getRelatedElement.setAttribute("order-by-list", getRelated.orderByList());
        }
        if (getRelated.useCache()) {
            getRelatedElement.setAttribute("use-cache", "true");
        }
        actionsElement.appendChild(getRelatedElement);
    }

    /**
     * Adds a &lt;property-map&gt; element to actions.
     */
    protected void addPropertyMapActionElement(Document doc, Element actionsElement, PropertyMapAction propMap) {
        if (UtilValidate.isEmpty(propMap.resource()) || UtilValidate.isEmpty(propMap.mapName())) {
            return;
        }
        Element propMapElement = doc.createElement("property-map");
        propMapElement.setAttribute("resource", propMap.resource());
        propMapElement.setAttribute("map-name", propMap.mapName());
        if (propMap.global()) {
            propMapElement.setAttribute("global", "true");
        }
        if (propMap.optional()) {
            propMapElement.setAttribute("optional", "true");
        }
        actionsElement.appendChild(propMapElement);
    }

    /**
     * Adds an &lt;include-screen-actions&gt; element to actions.
     */
    protected void addIncludeScreenActionsActionElement(Document doc, Element actionsElement, IncludeScreenActionsAction includeScreenActions) {
        if (UtilValidate.isEmpty(includeScreenActions.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-screen-actions");
        includeElement.setAttribute("name", includeScreenActions.name());
        if (UtilValidate.isNotEmpty(includeScreenActions.location())) {
            includeElement.setAttribute("location", includeScreenActions.location());
        }
        actionsElement.appendChild(includeElement);
    }

    /**
     * Adds an &lt;include-form-actions&gt; element to actions.
     */
    protected void addIncludeFormActionsActionElement(Document doc, Element actionsElement, IncludeFormActionsAction includeFormActions) {
        if (UtilValidate.isEmpty(includeFormActions.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-form-actions");
        includeElement.setAttribute("name", includeFormActions.name());
        if (UtilValidate.isNotEmpty(includeFormActions.location())) {
            includeElement.setAttribute("location", includeFormActions.location());
        }
        actionsElement.appendChild(includeElement);
    }

    /**
     * Adds an &lt;include-form-row-actions&gt; element to actions.
     */
    protected void addIncludeFormRowActionsActionElement(Document doc, Element actionsElement, IncludeFormRowActionsAction includeFormRowActions) {
        if (UtilValidate.isEmpty(includeFormRowActions.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-form-row-actions");
        includeElement.setAttribute("name", includeFormRowActions.name());
        if (UtilValidate.isNotEmpty(includeFormRowActions.location())) {
            includeElement.setAttribute("location", includeFormRowActions.location());
        }
        actionsElement.appendChild(includeElement);
    }

    /**
     * Adds an &lt;include-menu-actions&gt; element to actions.
     */
    protected void addIncludeMenuActionsActionElement(Document doc, Element actionsElement, IncludeMenuActionsAction includeMenuActions) {
        if (UtilValidate.isEmpty(includeMenuActions.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-menu-actions");
        includeElement.setAttribute("name", includeMenuActions.name());
        if (UtilValidate.isNotEmpty(includeMenuActions.location())) {
            includeElement.setAttribute("location", includeMenuActions.location());
        }
        actionsElement.appendChild(includeElement);
    }

    /**
     * Adds an &lt;include-tree-actions&gt; element to actions.
     */
    protected void addIncludeTreeActionsActionElement(Document doc, Element actionsElement, IncludeTreeActionsAction includeTreeActions) {
        if (UtilValidate.isEmpty(includeTreeActions.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-tree-actions");
        includeElement.setAttribute("name", includeTreeActions.name());
        if (UtilValidate.isNotEmpty(includeTreeActions.location())) {
            includeElement.setAttribute("location", includeTreeActions.location());
        }
        actionsElement.appendChild(includeElement);
    }

    /**
     * Adds a &lt;condition-to-field&gt; element to actions.
     */
    protected void addConditionToFieldActionElement(Document doc, Element actionsElement, ConditionToFieldAction conditionToField) {
        if (UtilValidate.isEmpty(conditionToField.field())) {
            return;
        }
        Element conditionToFieldElement = doc.createElement("condition-to-field");
        conditionToFieldElement.setAttribute("field", conditionToField.field());
        if (UtilValidate.isNotEmpty(conditionToField.type())) {
            conditionToFieldElement.setAttribute("type", conditionToField.type());
        }
        if (conditionToField.global()) {
            conditionToFieldElement.setAttribute("global", "true");
        }
        if (UtilValidate.isNotEmpty(conditionToField.toScope())) {
            conditionToFieldElement.setAttribute("to-scope", conditionToField.toScope());
        }
        if (UtilValidate.isNotEmpty(conditionToField.onlyIfField())) {
            conditionToFieldElement.setAttribute("only-if-field", conditionToField.onlyIfField());
        }

        // Add condition if present - condition-to-field expects condition directly, not wrapped in <condition>
        Condition condition = conditionToField.condition();
        if (hasCondition(condition)) {
            Element innerCondition = buildConditionContent(doc, condition);
            if (innerCondition != null) {
                // Handle NOT wrapper if needed
                if (condition.not()) {
                    Element notElement = doc.createElement("not");
                    notElement.appendChild(innerCondition);
                    conditionToFieldElement.appendChild(notElement);
                } else {
                    conditionToFieldElement.appendChild(innerCondition);
                }
            }
        }

        actionsElement.appendChild(conditionToFieldElement);
    }

    /**
     * Adds a &lt;close-object&gt; element to actions.
     */
    protected void addCloseObjectActionElement(Document doc, Element actionsElement, CloseObjectAction closeObject) {
        if (UtilValidate.isEmpty(closeObject.field())) {
            return;
        }
        Element closeObjectElement = doc.createElement("close-object");
        closeObjectElement.setAttribute("field", closeObject.field());
        actionsElement.appendChild(closeObjectElement);
    }

    /**
     * Adds a &lt;throw-exception&gt; element to actions.
     */
    protected void addThrowExceptionActionElement(Document doc, Element actionsElement, ThrowExceptionAction throwException) {
        if (UtilValidate.isEmpty(throwException.field())) {
            return;
        }
        Element throwExceptionElement = doc.createElement("throw-exception");
        throwExceptionElement.setAttribute("field", throwException.field());
        actionsElement.appendChild(throwExceptionElement);
    }

    protected boolean hasCondition(Condition condition) {
        return condition.functionalConditions().length > 0 ||
               UtilValidate.isNotEmpty(condition.ifEmpty()) ||
               UtilValidate.isNotEmpty(condition.ifNotEmpty()) ||
               UtilValidate.isNotEmpty(condition.ifTrue()) ||
               UtilValidate.isNotEmpty(condition.ifFalse()) ||
               condition.ifCompare().length > 0 ||
               condition.ifCompareField().length > 0 ||
               condition.ifHasPermission().length > 0 ||
               condition.ifServicePermission().length > 0 ||
               condition.ifValidateMethod().length > 0 ||
               condition.ifRegexp().length > 0 ||
               condition.ifEmptySection().length > 0 ||
               condition.ifEntityPermission().length > 0 ||
               condition.ifWidget().length > 0 ||
               condition.ifComponent().length > 0 ||
               condition.ifEntity().length > 0 ||
               condition.ifService().length > 0 ||
               condition.and().length > 0 ||
               condition.or().length > 0 ||
               condition.xor().length > 0 ||
               condition.not();
    }

    protected boolean hasActions(Actions actions) {
        return actions.value().length > 0 || actions.ifs().length > 0 ||
               actions.set().length > 0 ||
               actions.clearField().length > 0 ||
               actions.service().length > 0 ||
               actions.entityOne().length > 0 ||
               actions.entityAnd().length > 0 ||
               actions.entityCondition().length > 0 ||
               actions.getRelatedOne().length > 0 ||
               actions.getRelated().length > 0 ||
               actions.script().length > 0 ||
               actions.propertyToField().length > 0 ||
               actions.propertyMap().length > 0 ||
               actions.includeScreenActions().length > 0 ||
               actions.includeFormActions().length > 0 ||
               actions.includeFormRowActions().length > 0 ||
               actions.includeMenuActions().length > 0 ||
               actions.includeTreeActions().length > 0 ||
               actions.conditionToField().length > 0 ||
               actions.closeObject().length > 0 ||
               actions.throwException().length > 0;
    }

    protected boolean hasWidgets(Widgets widgets) {
        boolean hasUnified = widgets.value().length > 0;
        boolean result = hasUnified ||  // SCIPIO: 4.0.0: Check unified widgets first
               UtilValidate.isNotEmpty(widgets.decorator().name()) ||
               widgets.includeScreens().length > 0 ||
               widgets.includeForms().length > 0 ||
               widgets.includeMenus().length > 0 ||
               widgets.labels().length > 0 ||
               widgets.screenlets().length > 0 ||
               widgets.containers().length > 0 ||
               widgets.htmlTemplates().length > 0 ||
               widgets.images().length > 0 ||
               widgets.horizontalSeparators().length > 0 ||
               widgets.contents().length > 0 ||
               widgets.decoratorSectionIncludes().length > 0 ||
               // SCIPIO: 4.0.0: these were missing, so a widgets block holding only nested sections (the
               // whole body of the shop CommonCheckoutDecorator) counted as empty and was never emitted
               widgets.sections().length > 0 ||
               widgets.includeGrids().length > 0 ||
               widgets.includeTrees().length > 0 ||
               widgets.links().length > 0 ||
               widgets.subContents().length > 0 ||
               widgets.columnContainers().length > 0 ||
               widgets.iterateSections().length > 0 ||
               widgets.includePortalPages().length > 0;
        if (hasUnified) {
            if (Debug.verboseOn()) Debug.logVerbose("hasWidgets: found " + widgets.value().length + " unified widgets, returning " + result, module);
        }
        return result;
    }

    protected Element buildConditionElement(Document doc, Condition condition) {
        Element conditionElement = doc.createElement("condition");

        // Build the inner condition content
        Element innerConditionContent = buildConditionContent(doc, condition);

        // If NOT condition wrapper is enabled, wrap the content in a <not> element
        if (condition.not() && innerConditionContent != null) {
            Element notElement = doc.createElement("not");
            notElement.appendChild(innerConditionContent);
            conditionElement.appendChild(notElement);
        } else if (innerConditionContent != null) {
            conditionElement.appendChild(innerConditionContent);
        }

        return conditionElement.hasChildNodes() ? conditionElement : null;
    }

    /**
     * Builds the actual condition content (without the outer condition wrapper).
     */
    protected Element buildConditionContent(Document doc, Condition condition) {
        // Check for functional conditions first (takes precedence)
        com.ilscipio.scipio.widget.def.condition.Condition[] funcConditions = condition.functionalConditions();
        if (funcConditions != null && funcConditions.length > 0) {
            if (funcConditions.length == 1) {
                return buildFunctionalConditionElement(doc, funcConditions[0]);
            } else {
                // Multiple conditions - wrap in <and>
                Element andElement = doc.createElement("and");
                for (com.ilscipio.scipio.widget.def.condition.Condition funcCond : funcConditions) {
                    Element condElem = buildFunctionalConditionElement(doc, funcCond);
                    if (condElem != null) {
                        andElement.appendChild(condElem);
                    }
                }
                return andElement;
            }
        }

        // Simple conditions - return the first matching one
        if (UtilValidate.isNotEmpty(condition.ifEmpty())) {
            Element ifEmptyElement = doc.createElement("if-empty");
            ifEmptyElement.setAttribute("field", condition.ifEmpty());
            return ifEmptyElement;
        }

        if (UtilValidate.isNotEmpty(condition.ifNotEmpty())) {
            Element notElement = doc.createElement("not");
            Element ifEmptyElement = doc.createElement("if-empty");
            ifEmptyElement.setAttribute("field", condition.ifNotEmpty());
            notElement.appendChild(ifEmptyElement);
            return notElement;
        }

        if (UtilValidate.isNotEmpty(condition.ifTrue())) {
            Element ifTrueElement = doc.createElement("if-true");
            ifTrueElement.setAttribute("field", condition.ifTrue());
            return ifTrueElement;
        }

        if (UtilValidate.isNotEmpty(condition.ifFalse())) {
            // SCIPIO: 4.0.0: if-false, not a negated if-true: an unset field is neither, so not(if-false) holds
            Element ifFalseElement = doc.createElement("if-false");
            ifFalseElement.setAttribute("field", condition.ifFalse());
            return ifFalseElement;
        }

        // Handle if-compare conditions
        for (IfCompare ifCompare : condition.ifCompare()) {
            Element ifCompareElement = doc.createElement("if-compare");
            ifCompareElement.setAttribute("field", ifCompare.field());
            ifCompareElement.setAttribute("operator", ifCompare.operator());
            ifCompareElement.setAttribute("value", ifCompare.value());
                            ifCompareElement.setAttribute("type", ifCompare.type()); // SCIPIO: 4.0.0: always emit (synthetic DOM has no XSD default; runtime requires it)
            if (UtilValidate.isNotEmpty(ifCompare.format())) {
                ifCompareElement.setAttribute("format", ifCompare.format());
            }
            return ifCompareElement;
        }

        // Handle if-compare-field conditions
        for (IfCompareField ifCompareField : condition.ifCompareField()) {
            Element ifCompareFieldElement = doc.createElement("if-compare-field");
            ifCompareFieldElement.setAttribute("field", ifCompareField.field());
            ifCompareFieldElement.setAttribute("operator", ifCompareField.operator());
            ifCompareFieldElement.setAttribute("to-field", ifCompareField.toField());
            if (!"String".equals(ifCompareField.type())) {
                ifCompareFieldElement.setAttribute("type", ifCompareField.type());
            }
            if (UtilValidate.isNotEmpty(ifCompareField.format())) {
                ifCompareFieldElement.setAttribute("format", ifCompareField.format());
            }
            return ifCompareFieldElement;
        }

        // Handle if-has-permission conditions
        for (IfHasPermission ifHasPerm : condition.ifHasPermission()) {
            Element ifHasPermElement = doc.createElement("if-has-permission");
            ifHasPermElement.setAttribute("permission", ifHasPerm.permission());
            if (UtilValidate.isNotEmpty(ifHasPerm.action())) {
                ifHasPermElement.setAttribute("action", ifHasPerm.action());
            }
            return ifHasPermElement;
        }

        // Handle if-service-permission conditions
        for (IfServicePermission ifServPerm : condition.ifServicePermission()) {
            Element ifServPermElement = doc.createElement("if-service-permission");
            ifServPermElement.setAttribute("service-name", ifServPerm.serviceName());
            if (UtilValidate.isNotEmpty(ifServPerm.mainAction())) {
                ifServPermElement.setAttribute("main-action", ifServPerm.mainAction());
            }
            return ifServPermElement;
        }

        // Handle if-validate-method conditions
        for (IfValidateMethod ifValidate : condition.ifValidateMethod()) {
            Element ifValidateElement = doc.createElement("if-validate-method");
            ifValidateElement.setAttribute("field", ifValidate.field());
            ifValidateElement.setAttribute("method", ifValidate.method());
            if (UtilValidate.isNotEmpty(ifValidate.className())) {
                ifValidateElement.setAttribute("class", ifValidate.className());
            }
            return ifValidateElement;
        }

        // Handle if-regexp conditions
        for (IfRegexp ifRegexp : condition.ifRegexp()) {
            Element ifRegexpElement = doc.createElement("if-regexp");
            ifRegexpElement.setAttribute("field", ifRegexp.field());
            ifRegexpElement.setAttribute("expr", ifRegexp.expr());
            return ifRegexpElement;
        }

        // Handle if-empty-section conditions (screen-specific)
        for (IfEmptySection ifEmptySection : condition.ifEmptySection()) {
            Element ifEmptySectionElement = doc.createElement("if-empty-section");
            ifEmptySectionElement.setAttribute("section-name", ifEmptySection.sectionName());
            return ifEmptySectionElement;
        }

        // Handle if-entity-permission conditions
        for (IfEntityPermission ifEntityPerm : condition.ifEntityPermission()) {
            Element ifEntityPermElement = doc.createElement("if-entity-permission");
            ifEntityPermElement.setAttribute("entity-name", ifEntityPerm.entityName());
            if (UtilValidate.isNotEmpty(ifEntityPerm.entityId())) {
                ifEntityPermElement.setAttribute("entity-id", ifEntityPerm.entityId());
            }
            if (UtilValidate.isNotEmpty(ifEntityPerm.targetOperation())) {
                ifEntityPermElement.setAttribute("target-operation", ifEntityPerm.targetOperation());
            }
            if (ifEntityPerm.displayFailCond()) {
                ifEntityPermElement.setAttribute("display-fail-cond", "true");
            }
            return ifEntityPermElement;
        }

        // Handle if-widget conditions (Scipio extension)
        for (IfWidget ifWidget : condition.ifWidget()) {
            Element ifWidgetElement = doc.createElement("if-widget");
            ifWidgetElement.setAttribute("name", ifWidget.name());
            if (UtilValidate.isNotEmpty(ifWidget.location())) {
                ifWidgetElement.setAttribute("location", ifWidget.location());
            }
            if (UtilValidate.isNotEmpty(ifWidget.type())) {
                ifWidgetElement.setAttribute("type", ifWidget.type());
            }
            if (!"exists".equals(ifWidget.operator())) {
                ifWidgetElement.setAttribute("operator", ifWidget.operator());
            }
            return ifWidgetElement;
        }

        // Handle if-component conditions (Scipio extension)
        for (IfComponent ifComponent : condition.ifComponent()) {
            Element ifComponentElement = doc.createElement("if-component");
            ifComponentElement.setAttribute("component-name", ifComponent.componentName());
            return ifComponentElement;
        }

        // Handle if-entity conditions (Scipio extension)
        for (IfEntity ifEntity : condition.ifEntity()) {
            Element ifEntityElement = doc.createElement("if-entity");
            ifEntityElement.setAttribute("entity-name", ifEntity.entityName());
            return ifEntityElement;
        }

        // Handle if-service conditions (Scipio extension)
        for (IfServiceDef ifService : condition.ifService()) {
            Element ifServiceElement = doc.createElement("if-service");
            ifServiceElement.setAttribute("service-name", ifService.serviceName());
            return ifServiceElement;
        }

        // Handle AND conditions
        if (condition.and().length > 0) {
            Element andElement = doc.createElement("and");
            for (AndCondition andCond : condition.and()) {
                addCompoundConditionContent(doc, andElement, andCond);
            }
            return andElement;
        }

        // Handle OR conditions
        if (condition.or().length > 0) {
            Element orElement = doc.createElement("or");
            for (OrCondition orCond : condition.or()) {
                addCompoundConditionContent(doc, orElement, orCond);
            }
            return orElement;
        }

        // Handle XOR conditions
        if (condition.xor().length > 0) {
            Element xorElement = doc.createElement("xor");
            for (XorCondition xorCond : condition.xor()) {
                addCompoundConditionContent(doc, xorElement, xorCond);
            }
            return xorElement;
        }

        return null;
    }

    /**
     * Builds an XML element from a functional condition annotation.
     */
    protected Element buildFunctionalConditionElement(Document doc, com.ilscipio.scipio.widget.def.condition.Condition funcCond) {
        String typeName = funcCond.type().getSimpleName();
        // SCIPIO: 4.0.0: composite conditions were unsupported here, so or/xor/not silently became always-true
        if (isCompositeConditionType(typeName) && funcCond.tree().length > 0) {
            return buildConditionTreeElement(doc, typeName, funcCond.tree());
        }
        switch (typeName) {
            case "Always":
                return null;
            case "And":
            case "Or":
            case "Xor": {
                Element compositeElem = doc.createElement(typeName.toLowerCase());
                for (com.ilscipio.scipio.widget.def.condition.NestedCondition nested : funcCond.nested()) {
                    Element child = buildNestedConditionElement(doc, nested);
                    if (child != null) {
                        compositeElem.appendChild(child);
                    }
                }
                return compositeElem.hasChildNodes() ? compositeElem : null;
            }
            case "Not": {
                com.ilscipio.scipio.widget.def.condition.NestedCondition[] nestedConds = funcCond.nested();
                if (nestedConds.length == 1) {
                    Element child = buildNestedConditionElement(doc, nestedConds[0]);
                    if (child == null) {
                        return null;
                    }
                    if ("not".equals(child.getNodeName())) {
                        return child; // double negation already applied by the nested not flag
                    }
                    Element notElem = doc.createElement("not");
                    notElem.appendChild(child);
                    return notElem;
                }
                return null;
            }
        }
        return buildSimpleConditionElement(doc, typeName, funcCond.params());
    }

    /**
     * SCIPIO: 4.0.0: Builds a composite condition element from the flat Condition.tree() form,
     * which carries any depth (the NestedCondition chain stopped at a fixed one).
     */
    protected Element buildConditionTreeElement(Document doc, String typeName,
            com.ilscipio.scipio.widget.def.condition.ConditionNode[] nodes) {
        Element root = doc.createElement(typeName.toLowerCase());
        appendConditionTreeChildren(doc, root, nodes, -1);
        if (!root.hasChildNodes()) {
            return null;
        }
        if ("not".equals(root.getNodeName()) && root.getChildNodes().getLength() != 1) {
            return null;
        }
        return root;
    }

    /** SCIPIO: 4.0.0: Appends the tree nodes whose parent is parentIndex, depth first. */
    protected void appendConditionTreeChildren(Document doc, Element parent,
            com.ilscipio.scipio.widget.def.condition.ConditionNode[] nodes, int parentIndex) {
        for (int i = 0; i < nodes.length; i++) {
            com.ilscipio.scipio.widget.def.condition.ConditionNode node = nodes[i];
            if (node.parent() != parentIndex) {
                continue;
            }
            String nodeType = node.type().getSimpleName();
            Element elem;
            if (isCompositeConditionType(nodeType)) {
                elem = doc.createElement(nodeType.toLowerCase());
                appendConditionTreeChildren(doc, elem, nodes, i);
                if (!elem.hasChildNodes()) {
                    continue;
                }
            } else {
                elem = buildSimpleConditionElement(doc, nodeType, node.params());
                if (elem == null) {
                    continue;
                }
            }
            if (node.not()) {
                Element notElem = doc.createElement("not");
                notElem.appendChild(elem);
                elem = notElem;
            }
            parent.appendChild(elem);
        }
    }

    /** SCIPIO: 4.0.0: True for the condition types that hold members rather than parameters. */
    protected boolean isCompositeConditionType(String typeName) {
        return "And".equals(typeName) || "Or".equals(typeName)
                || "Xor".equals(typeName) || "Not".equals(typeName);
    }

    /**
     * SCIPIO: 4.0.0: Builds the XML element of a nested (one level deep) condition, negated when requested.
     */
    protected Element buildNestedConditionElement(Document doc, com.ilscipio.scipio.widget.def.condition.NestedCondition nested) {
        Element elem = buildCompositeOrSimpleElement(doc, nested.type().getSimpleName(), nested.params(), nested.nested());
        if (elem == null || !nested.not()) {
            return elem;
        }
        Element notElem = doc.createElement("not");
        notElem.appendChild(elem);
        return notElem;
    }

    /**
     * SCIPIO: 4.0.0: Builds a condition element that may itself be a composite of second-level conditions.
     */
    protected Element buildCompositeOrSimpleElement(Document doc, String typeName, String[] params,
            com.ilscipio.scipio.widget.def.condition.NestedCondition2[] members) {
        switch (typeName) {
            case "Always":
                return null;
            case "And":
            case "Or":
            case "Xor": {
                Element compositeElem = doc.createElement(typeName.toLowerCase());
                for (com.ilscipio.scipio.widget.def.condition.NestedCondition2 member : members) {
                    Element child = buildNested2ConditionElement(doc, member);
                    if (child != null) {
                        compositeElem.appendChild(child);
                    }
                }
                return compositeElem.hasChildNodes() ? compositeElem : null;
            }
            case "Not": {
                if (members.length == 1) {
                    Element child = buildNested2ConditionElement(doc, members[0]);
                    if (child == null) {
                        return null;
                    }
                    if ("not".equals(child.getNodeName())) {
                        return child;
                    }
                    Element notElem = doc.createElement("not");
                    notElem.appendChild(child);
                    return notElem;
                }
                return null;
            }
        }
        return buildSimpleConditionElement(doc, typeName, params);
    }

    /**
     * SCIPIO: 4.0.0: Builds the XML element of a second-level nested condition, negated when requested.
     */
    protected Element buildNested2ConditionElement(Document doc,
            com.ilscipio.scipio.widget.def.condition.NestedCondition2 nested) {
        Element elem = buildSimpleConditionElement(doc, nested.type().getSimpleName(), nested.params());
        if (elem == null || !nested.not()) {
            return elem;
        }
        Element notElem = doc.createElement("not");
        notElem.appendChild(elem);
        return notElem;
    }

    /**
     * SCIPIO: 4.0.0: Builds the XML element of a non-composite condition type.
     */
    protected Element buildSimpleConditionElement(Document doc, String typeName, String[] params) {
        // Map functional condition types to XML elements
        switch (typeName) {
            case "Empty":
                if (params.length > 0) {
                    Element elem = doc.createElement("if-empty");
                    elem.setAttribute("field", params[0]);
                    return elem;
                }
                break;
            case "NotEmpty":
                if (params.length > 0) {
                    Element notElem = doc.createElement("not");
                    Element ifEmptyElem = doc.createElement("if-empty");
                    ifEmptyElem.setAttribute("field", params[0]);
                    notElem.appendChild(ifEmptyElem);
                    return notElem;
                }
                break;
            case "True":
                if (params.length > 0) {
                    Element elem = doc.createElement("if-true");
                    setFieldOrValue(elem, params[0]);
                    return elem;
                }
                break;
            case "False":
                if (params.length > 0) {
                    // SCIPIO: 4.0.0: if-false, not a negated if-true: an unset field is neither, so not(if-false) holds
                    Element elem = doc.createElement("if-false");
                    setFieldOrValue(elem, params[0]);
                    return elem;
                }
                break;
            case "HasPermission":
                Element permElem = doc.createElement("if-has-permission");
                if (params.length > 0) permElem.setAttribute("permission", params[0]);
                if (params.length > 1) permElem.setAttribute("action", params[1]);
                return permElem;
            case "ServicePermission":
                Element servPermElem = doc.createElement("if-service-permission");
                if (params.length > 0) servPermElem.setAttribute("service-name", params[0]);
                if (params.length > 1) servPermElem.setAttribute("main-action", params[1]);
                return servPermElem;
            case "EntityPermission":
                Element entityPermElem = doc.createElement("if-entity-permission");
                if (params.length > 0) entityPermElem.setAttribute("entity-name", params[0]);
                if (params.length > 1) entityPermElem.setAttribute("entity-id", params[1]);
                if (params.length > 2) entityPermElem.setAttribute("target-operation", params[2]);
                return entityPermElem;
            case "Compare":
                Element compElem = doc.createElement("if-compare");
                if (params.length > 0) compElem.setAttribute("field", params[0]);
                if (params.length > 1) compElem.setAttribute("operator", params[1]);
                if (params.length > 2) compElem.setAttribute("value", params[2]);
                // SCIPIO: Always set type - default to String if not provided
                if (params.length > 3 && UtilValidate.isNotEmpty(params[3])) {
                    compElem.setAttribute("type", params[3]);
                } else {
                    compElem.setAttribute("type", "String");
                }
                if (params.length > 4) compElem.setAttribute("format", params[4]);
                return compElem;
            case "CompareField":
                Element compFieldElem = doc.createElement("if-compare-field");
                if (params.length > 0) compFieldElem.setAttribute("field", params[0]);
                if (params.length > 1) compFieldElem.setAttribute("operator", params[1]);
                if (params.length > 2) compFieldElem.setAttribute("to-field", params[2]);
                // SCIPIO: Always set type - default to String if not provided
                if (params.length > 3 && UtilValidate.isNotEmpty(params[3])) {
                    compFieldElem.setAttribute("type", params[3]);
                } else {
                    compFieldElem.setAttribute("type", "String");
                }
                if (params.length > 4) compFieldElem.setAttribute("format", params[4]);
                return compFieldElem;
            case "Regexp":
                Element regexpElem = doc.createElement("if-regexp");
                if (params.length > 0) regexpElem.setAttribute("field", params[0]);
                if (params.length > 1) regexpElem.setAttribute("expr", params[1]);
                return regexpElem;
            case "ValidateMethod":
                Element validateElem = doc.createElement("if-validate-method");
                if (params.length > 0) validateElem.setAttribute("field", params[0]);
                if (params.length > 1) validateElem.setAttribute("method", params[1]);
                if (params.length > 2) validateElem.setAttribute("class", params[2]);
                return validateElem;
            case "EmptySection":
                if (params.length > 0) {
                    Element emptySectionElem = doc.createElement("if-empty-section");
                    emptySectionElem.setAttribute("section-name", params[0]);
                    return emptySectionElem;
                }
                break;
            case "WidgetDefined":
                Element widgetElem = doc.createElement("if-widget");
                if (params.length > 0) widgetElem.setAttribute("name", params[0]);
                if (params.length > 1) widgetElem.setAttribute("location", params[1]);
                if (params.length > 2) widgetElem.setAttribute("type", params[2]);
                // SCIPIO: 4.0.0: the runtime requires the operator and knows only "defined"; without it the
                // whole screen class failed to load (WebtoolsLayoutDemoOfbizWidgets)
                widgetElem.setAttribute("operator", "defined");
                return widgetElem;
            case "ComponentEnabled":
                Element compElemDef = doc.createElement("if-component");
                if (params.length > 0) compElemDef.setAttribute("component-name", params[0]);
                return compElemDef;
            case "EntityDefined":
                Element entityDefElem = doc.createElement("if-entity");
                if (params.length > 0) entityDefElem.setAttribute("entity-name", params[0]);
                return entityDefElem;
            case "ServiceDefined":
                Element serviceDefElem = doc.createElement("if-service");
                if (params.length > 0) serviceDefElem.setAttribute("service-name", params[0]);
                return serviceDefElem;
        }
        return null;
    }

    /**
     * Adds content from an AndCondition to a parent element.
     */
    protected void addCompoundConditionContent(Document doc, Element parentElement, AndCondition andCond) {
        // Add if-empty conditions
        for (String field : andCond.ifEmpty()) {
            Element ifEmptyElement = doc.createElement("if-empty");
            ifEmptyElement.setAttribute("field", field);
            parentElement.appendChild(ifEmptyElement);
        }
        // Add if-not-empty conditions
        for (String field : andCond.ifNotEmpty()) {
            Element notElement = doc.createElement("not");
            Element ifEmptyElement = doc.createElement("if-empty");
            ifEmptyElement.setAttribute("field", field);
            notElement.appendChild(ifEmptyElement);
            parentElement.appendChild(notElement);
        }
        // Add if-true conditions
        for (String field : andCond.ifTrue()) {
            Element ifTrueElement = doc.createElement("if-true");
            ifTrueElement.setAttribute("field", field);
            parentElement.appendChild(ifTrueElement);
        }
        // Add if-false conditions
        for (String field : andCond.ifFalse()) {
            // SCIPIO: 4.0.0: if-false, not a negated if-true: an unset field is neither, so not(if-false) holds
            Element ifFalseElement = doc.createElement("if-false");
            ifFalseElement.setAttribute("field", field);
            parentElement.appendChild(ifFalseElement);
        }
        // Add if-compare conditions
        for (IfCompare ifCompare : andCond.ifCompare()) {
            Element ifCompareElement = doc.createElement("if-compare");
            ifCompareElement.setAttribute("field", ifCompare.field());
            ifCompareElement.setAttribute("operator", ifCompare.operator());
            ifCompareElement.setAttribute("value", ifCompare.value());
                            ifCompareElement.setAttribute("type", ifCompare.type()); // SCIPIO: 4.0.0: always emit (synthetic DOM has no XSD default; runtime requires it)
            if (UtilValidate.isNotEmpty(ifCompare.format())) {
                ifCompareElement.setAttribute("format", ifCompare.format());
            }
            parentElement.appendChild(ifCompareElement);
        }
        // Add if-compare-field conditions
        for (IfCompareField ifCompareField : andCond.ifCompareField()) {
            Element ifCompareFieldElement = doc.createElement("if-compare-field");
            ifCompareFieldElement.setAttribute("field", ifCompareField.field());
            ifCompareFieldElement.setAttribute("operator", ifCompareField.operator());
            ifCompareFieldElement.setAttribute("to-field", ifCompareField.toField());
            if (!"String".equals(ifCompareField.type())) {
                ifCompareFieldElement.setAttribute("type", ifCompareField.type());
            }
            if (UtilValidate.isNotEmpty(ifCompareField.format())) {
                ifCompareFieldElement.setAttribute("format", ifCompareField.format());
            }
            parentElement.appendChild(ifCompareFieldElement);
        }
        // Add if-has-permission conditions
        for (IfHasPermission ifHasPerm : andCond.ifHasPermission()) {
            Element ifHasPermElement = doc.createElement("if-has-permission");
            ifHasPermElement.setAttribute("permission", ifHasPerm.permission());
            if (UtilValidate.isNotEmpty(ifHasPerm.action())) {
                ifHasPermElement.setAttribute("action", ifHasPerm.action());
            }
            parentElement.appendChild(ifHasPermElement);
        }
        // Add if-service-permission conditions
        for (IfServicePermission ifServPerm : andCond.ifServicePermission()) {
            Element ifServPermElement = doc.createElement("if-service-permission");
            ifServPermElement.setAttribute("service-name", ifServPerm.serviceName());
            if (UtilValidate.isNotEmpty(ifServPerm.mainAction())) {
                ifServPermElement.setAttribute("main-action", ifServPerm.mainAction());
            }
            parentElement.appendChild(ifServPermElement);
        }
        // Add if-validate-method conditions
        for (IfValidateMethod ifValidate : andCond.ifValidateMethod()) {
            Element ifValidateElement = doc.createElement("if-validate-method");
            ifValidateElement.setAttribute("field", ifValidate.field());
            ifValidateElement.setAttribute("method", ifValidate.method());
            if (UtilValidate.isNotEmpty(ifValidate.className())) {
                ifValidateElement.setAttribute("class", ifValidate.className());
            }
            parentElement.appendChild(ifValidateElement);
        }
        // Add if-regexp conditions
        for (IfRegexp ifRegexp : andCond.ifRegexp()) {
            Element ifRegexpElement = doc.createElement("if-regexp");
            ifRegexpElement.setAttribute("field", ifRegexp.field());
            ifRegexpElement.setAttribute("expr", ifRegexp.expr());
            parentElement.appendChild(ifRegexpElement);
        }
        // Add if-empty-section conditions (screen-specific)
        for (IfEmptySection ifEmptySection : andCond.ifEmptySection()) {
            Element ifEmptySectionElement = doc.createElement("if-empty-section");
            ifEmptySectionElement.setAttribute("section-name", ifEmptySection.sectionName());
            parentElement.appendChild(ifEmptySectionElement);
        }
        // Add if-entity-permission conditions
        for (IfEntityPermission ifEntityPerm : andCond.ifEntityPermission()) {
            Element ifEntityPermElement = doc.createElement("if-entity-permission");
            ifEntityPermElement.setAttribute("entity-name", ifEntityPerm.entityName());
            if (UtilValidate.isNotEmpty(ifEntityPerm.entityId())) {
                ifEntityPermElement.setAttribute("entity-id", ifEntityPerm.entityId());
            }
            if (UtilValidate.isNotEmpty(ifEntityPerm.targetOperation())) {
                ifEntityPermElement.setAttribute("target-operation", ifEntityPerm.targetOperation());
            }
            if (ifEntityPerm.displayFailCond()) {
                ifEntityPermElement.setAttribute("display-fail-cond", "true");
            }
            parentElement.appendChild(ifEntityPermElement);
        }
        // Add if-widget conditions (Scipio extension)
        for (IfWidget ifWidget : andCond.ifWidget()) {
            Element ifWidgetElement = doc.createElement("if-widget");
            ifWidgetElement.setAttribute("name", ifWidget.name());
            if (UtilValidate.isNotEmpty(ifWidget.location())) {
                ifWidgetElement.setAttribute("location", ifWidget.location());
            }
            if (UtilValidate.isNotEmpty(ifWidget.type())) {
                ifWidgetElement.setAttribute("type", ifWidget.type());
            }
            if (!"exists".equals(ifWidget.operator())) {
                ifWidgetElement.setAttribute("operator", ifWidget.operator());
            }
            parentElement.appendChild(ifWidgetElement);
        }
        // Add if-component conditions (Scipio extension)
        for (IfComponent ifComponent : andCond.ifComponent()) {
            Element ifComponentElement = doc.createElement("if-component");
            ifComponentElement.setAttribute("component-name", ifComponent.componentName());
            parentElement.appendChild(ifComponentElement);
        }
        // Add if-entity conditions (Scipio extension)
        for (IfEntity ifEntity : andCond.ifEntity()) {
            Element ifEntityElement = doc.createElement("if-entity");
            ifEntityElement.setAttribute("entity-name", ifEntity.entityName());
            parentElement.appendChild(ifEntityElement);
        }
        // Add if-service conditions (Scipio extension)
        for (IfServiceDef ifService : andCond.ifService()) {
            Element ifServiceElement = doc.createElement("if-service");
            ifServiceElement.setAttribute("service-name", ifService.serviceName());
            parentElement.appendChild(ifServiceElement);
        }
    }

    /**
     * Adds content from an OrCondition to a parent element.
     */
    protected void addCompoundConditionContent(Document doc, Element parentElement, OrCondition orCond) {
        for (String field : orCond.ifEmpty()) {
            Element ifEmptyElement = doc.createElement("if-empty");
            ifEmptyElement.setAttribute("field", field);
            parentElement.appendChild(ifEmptyElement);
        }
        for (String field : orCond.ifNotEmpty()) {
            Element notElement = doc.createElement("not");
            Element ifEmptyElement = doc.createElement("if-empty");
            ifEmptyElement.setAttribute("field", field);
            notElement.appendChild(ifEmptyElement);
            parentElement.appendChild(notElement);
        }
        for (String field : orCond.ifTrue()) {
            Element ifTrueElement = doc.createElement("if-true");
            ifTrueElement.setAttribute("field", field);
            parentElement.appendChild(ifTrueElement);
        }
        for (String field : orCond.ifFalse()) {
            // SCIPIO: 4.0.0: if-false, not a negated if-true: an unset field is neither, so not(if-false) holds
            Element ifFalseElement = doc.createElement("if-false");
            ifFalseElement.setAttribute("field", field);
            parentElement.appendChild(ifFalseElement);
        }
        for (IfCompare ifCompare : orCond.ifCompare()) {
            Element ifCompareElement = doc.createElement("if-compare");
            ifCompareElement.setAttribute("field", ifCompare.field());
            ifCompareElement.setAttribute("operator", ifCompare.operator());
            ifCompareElement.setAttribute("value", ifCompare.value());
                            ifCompareElement.setAttribute("type", ifCompare.type()); // SCIPIO: 4.0.0: always emit (synthetic DOM has no XSD default; runtime requires it)
            if (UtilValidate.isNotEmpty(ifCompare.format())) {
                ifCompareElement.setAttribute("format", ifCompare.format());
            }
            parentElement.appendChild(ifCompareElement);
        }
        for (IfCompareField ifCompareField : orCond.ifCompareField()) {
            Element ifCompareFieldElement = doc.createElement("if-compare-field");
            ifCompareFieldElement.setAttribute("field", ifCompareField.field());
            ifCompareFieldElement.setAttribute("operator", ifCompareField.operator());
            ifCompareFieldElement.setAttribute("to-field", ifCompareField.toField());
            if (!"String".equals(ifCompareField.type())) {
                ifCompareFieldElement.setAttribute("type", ifCompareField.type());
            }
            if (UtilValidate.isNotEmpty(ifCompareField.format())) {
                ifCompareFieldElement.setAttribute("format", ifCompareField.format());
            }
            parentElement.appendChild(ifCompareFieldElement);
        }
        for (IfHasPermission ifHasPerm : orCond.ifHasPermission()) {
            Element ifHasPermElement = doc.createElement("if-has-permission");
            ifHasPermElement.setAttribute("permission", ifHasPerm.permission());
            if (UtilValidate.isNotEmpty(ifHasPerm.action())) {
                ifHasPermElement.setAttribute("action", ifHasPerm.action());
            }
            parentElement.appendChild(ifHasPermElement);
        }
        for (IfServicePermission ifServPerm : orCond.ifServicePermission()) {
            Element ifServPermElement = doc.createElement("if-service-permission");
            ifServPermElement.setAttribute("service-name", ifServPerm.serviceName());
            if (UtilValidate.isNotEmpty(ifServPerm.mainAction())) {
                ifServPermElement.setAttribute("main-action", ifServPerm.mainAction());
            }
            parentElement.appendChild(ifServPermElement);
        }
        for (IfValidateMethod ifValidate : orCond.ifValidateMethod()) {
            Element ifValidateElement = doc.createElement("if-validate-method");
            ifValidateElement.setAttribute("field", ifValidate.field());
            ifValidateElement.setAttribute("method", ifValidate.method());
            if (UtilValidate.isNotEmpty(ifValidate.className())) {
                ifValidateElement.setAttribute("class", ifValidate.className());
            }
            parentElement.appendChild(ifValidateElement);
        }
        for (IfRegexp ifRegexp : orCond.ifRegexp()) {
            Element ifRegexpElement = doc.createElement("if-regexp");
            ifRegexpElement.setAttribute("field", ifRegexp.field());
            ifRegexpElement.setAttribute("expr", ifRegexp.expr());
            parentElement.appendChild(ifRegexpElement);
        }
        for (IfEmptySection ifEmptySection : orCond.ifEmptySection()) {
            Element ifEmptySectionElement = doc.createElement("if-empty-section");
            ifEmptySectionElement.setAttribute("section-name", ifEmptySection.sectionName());
            parentElement.appendChild(ifEmptySectionElement);
        }
        for (IfEntityPermission ifEntityPerm : orCond.ifEntityPermission()) {
            Element ifEntityPermElement = doc.createElement("if-entity-permission");
            ifEntityPermElement.setAttribute("entity-name", ifEntityPerm.entityName());
            if (UtilValidate.isNotEmpty(ifEntityPerm.entityId())) {
                ifEntityPermElement.setAttribute("entity-id", ifEntityPerm.entityId());
            }
            if (UtilValidate.isNotEmpty(ifEntityPerm.targetOperation())) {
                ifEntityPermElement.setAttribute("target-operation", ifEntityPerm.targetOperation());
            }
            if (ifEntityPerm.displayFailCond()) {
                ifEntityPermElement.setAttribute("display-fail-cond", "true");
            }
            parentElement.appendChild(ifEntityPermElement);
        }
        for (IfWidget ifWidget : orCond.ifWidget()) {
            Element ifWidgetElement = doc.createElement("if-widget");
            ifWidgetElement.setAttribute("name", ifWidget.name());
            if (UtilValidate.isNotEmpty(ifWidget.location())) {
                ifWidgetElement.setAttribute("location", ifWidget.location());
            }
            if (UtilValidate.isNotEmpty(ifWidget.type())) {
                ifWidgetElement.setAttribute("type", ifWidget.type());
            }
            if (!"exists".equals(ifWidget.operator())) {
                ifWidgetElement.setAttribute("operator", ifWidget.operator());
            }
            parentElement.appendChild(ifWidgetElement);
        }
        for (IfComponent ifComponent : orCond.ifComponent()) {
            Element ifComponentElement = doc.createElement("if-component");
            ifComponentElement.setAttribute("component-name", ifComponent.componentName());
            parentElement.appendChild(ifComponentElement);
        }
        for (IfEntity ifEntity : orCond.ifEntity()) {
            Element ifEntityElement = doc.createElement("if-entity");
            ifEntityElement.setAttribute("entity-name", ifEntity.entityName());
            parentElement.appendChild(ifEntityElement);
        }
        for (IfServiceDef ifService : orCond.ifService()) {
            Element ifServiceElement = doc.createElement("if-service");
            ifServiceElement.setAttribute("service-name", ifService.serviceName());
            parentElement.appendChild(ifServiceElement);
        }
    }

    /**
     * Adds content from an XorCondition to a parent element.
     */
    protected void addCompoundConditionContent(Document doc, Element parentElement, XorCondition xorCond) {
        for (String field : xorCond.ifEmpty()) {
            Element ifEmptyElement = doc.createElement("if-empty");
            ifEmptyElement.setAttribute("field", field);
            parentElement.appendChild(ifEmptyElement);
        }
        for (String field : xorCond.ifNotEmpty()) {
            Element notElement = doc.createElement("not");
            Element ifEmptyElement = doc.createElement("if-empty");
            ifEmptyElement.setAttribute("field", field);
            notElement.appendChild(ifEmptyElement);
            parentElement.appendChild(notElement);
        }
        for (String field : xorCond.ifTrue()) {
            Element ifTrueElement = doc.createElement("if-true");
            ifTrueElement.setAttribute("field", field);
            parentElement.appendChild(ifTrueElement);
        }
        for (String field : xorCond.ifFalse()) {
            Element notElement = doc.createElement("not");
            Element ifTrueElement = doc.createElement("if-true");
            ifTrueElement.setAttribute("field", field);
            notElement.appendChild(ifTrueElement);
            parentElement.appendChild(notElement);
        }
        for (IfCompare ifCompare : xorCond.ifCompare()) {
            Element ifCompareElement = doc.createElement("if-compare");
            ifCompareElement.setAttribute("field", ifCompare.field());
            ifCompareElement.setAttribute("operator", ifCompare.operator());
            ifCompareElement.setAttribute("value", ifCompare.value());
                            ifCompareElement.setAttribute("type", ifCompare.type()); // SCIPIO: 4.0.0: always emit (synthetic DOM has no XSD default; runtime requires it)
            if (UtilValidate.isNotEmpty(ifCompare.format())) {
                ifCompareElement.setAttribute("format", ifCompare.format());
            }
            parentElement.appendChild(ifCompareElement);
        }
        for (IfCompareField ifCompareField : xorCond.ifCompareField()) {
            Element ifCompareFieldElement = doc.createElement("if-compare-field");
            ifCompareFieldElement.setAttribute("field", ifCompareField.field());
            ifCompareFieldElement.setAttribute("operator", ifCompareField.operator());
            ifCompareFieldElement.setAttribute("to-field", ifCompareField.toField());
            if (!"String".equals(ifCompareField.type())) {
                ifCompareFieldElement.setAttribute("type", ifCompareField.type());
            }
            if (UtilValidate.isNotEmpty(ifCompareField.format())) {
                ifCompareFieldElement.setAttribute("format", ifCompareField.format());
            }
            parentElement.appendChild(ifCompareFieldElement);
        }
        for (IfHasPermission ifHasPerm : xorCond.ifHasPermission()) {
            Element ifHasPermElement = doc.createElement("if-has-permission");
            ifHasPermElement.setAttribute("permission", ifHasPerm.permission());
            if (UtilValidate.isNotEmpty(ifHasPerm.action())) {
                ifHasPermElement.setAttribute("action", ifHasPerm.action());
            }
            parentElement.appendChild(ifHasPermElement);
        }
        for (IfServicePermission ifServPerm : xorCond.ifServicePermission()) {
            Element ifServPermElement = doc.createElement("if-service-permission");
            ifServPermElement.setAttribute("service-name", ifServPerm.serviceName());
            if (UtilValidate.isNotEmpty(ifServPerm.mainAction())) {
                ifServPermElement.setAttribute("main-action", ifServPerm.mainAction());
            }
            parentElement.appendChild(ifServPermElement);
        }
        for (IfValidateMethod ifValidate : xorCond.ifValidateMethod()) {
            Element ifValidateElement = doc.createElement("if-validate-method");
            ifValidateElement.setAttribute("field", ifValidate.field());
            ifValidateElement.setAttribute("method", ifValidate.method());
            if (UtilValidate.isNotEmpty(ifValidate.className())) {
                ifValidateElement.setAttribute("class", ifValidate.className());
            }
            parentElement.appendChild(ifValidateElement);
        }
        for (IfRegexp ifRegexp : xorCond.ifRegexp()) {
            Element ifRegexpElement = doc.createElement("if-regexp");
            ifRegexpElement.setAttribute("field", ifRegexp.field());
            ifRegexpElement.setAttribute("expr", ifRegexp.expr());
            parentElement.appendChild(ifRegexpElement);
        }
        for (IfEmptySection ifEmptySection : xorCond.ifEmptySection()) {
            Element ifEmptySectionElement = doc.createElement("if-empty-section");
            ifEmptySectionElement.setAttribute("section-name", ifEmptySection.sectionName());
            parentElement.appendChild(ifEmptySectionElement);
        }
        for (IfEntityPermission ifEntityPerm : xorCond.ifEntityPermission()) {
            Element ifEntityPermElement = doc.createElement("if-entity-permission");
            ifEntityPermElement.setAttribute("entity-name", ifEntityPerm.entityName());
            if (UtilValidate.isNotEmpty(ifEntityPerm.entityId())) {
                ifEntityPermElement.setAttribute("entity-id", ifEntityPerm.entityId());
            }
            if (UtilValidate.isNotEmpty(ifEntityPerm.targetOperation())) {
                ifEntityPermElement.setAttribute("target-operation", ifEntityPerm.targetOperation());
            }
            if (ifEntityPerm.displayFailCond()) {
                ifEntityPermElement.setAttribute("display-fail-cond", "true");
            }
            parentElement.appendChild(ifEntityPermElement);
        }
        for (IfWidget ifWidget : xorCond.ifWidget()) {
            Element ifWidgetElement = doc.createElement("if-widget");
            ifWidgetElement.setAttribute("name", ifWidget.name());
            if (UtilValidate.isNotEmpty(ifWidget.location())) {
                ifWidgetElement.setAttribute("location", ifWidget.location());
            }
            if (UtilValidate.isNotEmpty(ifWidget.type())) {
                ifWidgetElement.setAttribute("type", ifWidget.type());
            }
            if (!"exists".equals(ifWidget.operator())) {
                ifWidgetElement.setAttribute("operator", ifWidget.operator());
            }
            parentElement.appendChild(ifWidgetElement);
        }
        for (IfComponent ifComponent : xorCond.ifComponent()) {
            Element ifComponentElement = doc.createElement("if-component");
            ifComponentElement.setAttribute("component-name", ifComponent.componentName());
            parentElement.appendChild(ifComponentElement);
        }
        for (IfEntity ifEntity : xorCond.ifEntity()) {
            Element ifEntityElement = doc.createElement("if-entity");
            ifEntityElement.setAttribute("entity-name", ifEntity.entityName());
            parentElement.appendChild(ifEntityElement);
        }
        for (IfServiceDef ifService : xorCond.ifService()) {
            Element ifServiceElement = doc.createElement("if-service");
            ifServiceElement.setAttribute("service-name", ifService.serviceName());
            parentElement.appendChild(ifServiceElement);
        }
    }

    protected Element buildSectionActionsElement(Document doc, Actions actions) {
        Element actionsElement = doc.createElement("actions");
        // Delegate to the canonical handler, which supports both the unified value() array
        // (order-preserving, emitted by the converter) and the legacy type-specific arrays.
        addActionsContent(doc, actionsElement, actions);
        return actionsElement;
    }

    protected Element buildSectionWidgetsElement(Document doc, Widgets widgets) {
        Element widgetsElement = doc.createElement("widgets");
        addWidgetsContent(doc, widgetsElement, widgets);
        return widgetsElement;
    }

    /** SCIPIO: 4.0.0: a child of a widgets block with its declared position and the code that emits it. */
    private static final class OrderedChild {
        private final int position;
        private final Emit emit;
        OrderedChild(Object annotation, Emit emit) {
            this.position = positionOf(annotation);
            this.emit = emit;
        }
    }

    @FunctionalInterface
    private interface Emit {
        void run() throws ReflectiveOperationException;
    }

    /** SCIPIO: 4.0.0: position() of an annotation, or -1 when the type declares none. */
    private static int positionOf(Object annotation) {
        try {
            return (Integer) ((java.lang.annotation.Annotation) annotation).annotationType()
                    .getMethod("position").invoke(annotation);
        } catch (ReflectiveOperationException | RuntimeException e) {
            return -1;
        }
    }

    /**
     * SCIPIO: 4.0.0: Emits the children of a widgets block in order. A child with position() >= 0 takes
     * that slot among all its siblings; the others fill the remaining slots in declaration order. Without
     * any position the order is unchanged. This restores an XML order between typed arrays that the
     * annotation model cannot express, such as the global decorator body between head and foot sections.
     */
    private static void emitOrdered(List<OrderedChild> children) throws ReflectiveOperationException {
        List<OrderedChild> positioned = new ArrayList<>();
        java.util.Deque<OrderedChild> unpositioned = new java.util.ArrayDeque<>();
        for (OrderedChild child : children) {
            if (child.position >= 0) {
                positioned.add(child);
            } else {
                unpositioned.add(child);
            }
        }
        if (positioned.isEmpty()) {
            for (OrderedChild child : children) {
                child.emit.run();
            }
            return;
        }
        positioned.sort(java.util.Comparator.comparingInt(c -> c.position));
        java.util.Deque<OrderedChild> pending = new java.util.ArrayDeque<>(positioned);
        for (int slot = 0; slot < children.size(); slot++) {
            if (!pending.isEmpty() && (pending.peek().position <= slot || unpositioned.isEmpty())) {
                pending.poll().emit.run();
            } else {
                unpositioned.poll().emit.run();
            }
        }
    }

    private static void emitOrderedUnchecked(List<OrderedChild> children) {
        try {
            emitOrdered(children);
        } catch (ReflectiveOperationException e) {
            throw new IllegalStateException(e);
        }
    }

    protected void addWidgetsContent(Document doc, Element widgetsElement, Widgets widgets) {
        // Add decorator if present
        if (UtilValidate.isNotEmpty(widgets.decorator().name())) {
            addDecoratorScreenElement(doc, widgetsElement, widgets.decorator(), null);
        }

        // SCIPIO: 4.0.0: every child keeps its declared position(); the rest fill the remaining slots in
        // the default order value, sections, screenlets, containers, htmlTemplates, then (legacy mode
        // only, when no unified widget is present) the other type-specific arrays. See emitOrdered().
        List<OrderedChild> children = new ArrayList<>();
        Widget[] unifiedWidgets = widgets.value();
        boolean hasUnifiedWidgets = unifiedWidgets.length > 0;
        if (hasUnifiedWidgets && Debug.verboseOn()) {
            Debug.logVerbose("addWidgetsContent: Processing " + unifiedWidgets.length + " unified widgets", module);
        }
        for (Widget widget : unifiedWidgets) {
            children.add(new OrderedChild(widget, () -> addUnifiedWidgetElement(doc, widgetsElement, widget)));
        }
        for (SectionNested nestedSection : widgets.sections()) {
            children.add(new OrderedChild(nestedSection, () -> addSectionNestedElement(doc, widgetsElement, nestedSection)));
        }
        for (Screenlet screenlet : widgets.screenlets()) {
            children.add(new OrderedChild(screenlet, () -> addScreenletElement(doc, widgetsElement, screenlet)));
        }
        for (Container container : widgets.containers()) {
            children.add(new OrderedChild(container, () -> addContainerElement(doc, widgetsElement, container)));
        }
        for (HtmlTemplate htmlTemplate : widgets.htmlTemplates()) {
            children.add(new OrderedChild(htmlTemplate, () -> addHtmlTemplateElement(doc, widgetsElement, htmlTemplate)));
        }
        if (!hasUnifiedWidgets) {
            for (DecoratorSectionInclude x : widgets.decoratorSectionIncludes()) {
                children.add(new OrderedChild(x, () -> addDecoratorSectionIncludeElement(doc, widgetsElement, x)));
            }
            for (IncludeScreen x : widgets.includeScreens()) {
                children.add(new OrderedChild(x, () -> addIncludeScreenElement(doc, widgetsElement, x)));
            }
            for (IncludeForm x : widgets.includeForms()) {
                children.add(new OrderedChild(x, () -> addIncludeFormElement(doc, widgetsElement, x)));
            }
            for (IncludeMenu x : widgets.includeMenus()) {
                children.add(new OrderedChild(x, () -> addIncludeMenuElement(doc, widgetsElement, x)));
            }
            for (Label x : widgets.labels()) {
                children.add(new OrderedChild(x, () -> addLabelElement(doc, widgetsElement, x)));
            }
            for (Image x : widgets.images()) {
                children.add(new OrderedChild(x, () -> addImageElement(doc, widgetsElement, x)));
            }
            for (HorizontalSeparator x : widgets.horizontalSeparators()) {
                children.add(new OrderedChild(x, () -> addHorizontalSeparatorElement(doc, widgetsElement, x)));
            }
            for (Content x : widgets.contents()) {
                children.add(new OrderedChild(x, () -> addContentElement(doc, widgetsElement, x)));
            }
        }
        emitOrderedUnchecked(children);
    }

    /**
     * Adds a &lt;decorator-screen&gt; element to widgets.
     */
    protected void addDecoratorScreenElement(Document doc, Element widgetsElement,
                                              DecoratorScreen decoratorScreen, Class<?> screenClass) {
        Element decoratorElement = doc.createElement("decorator-screen");
        decoratorElement.setAttribute("name", decoratorScreen.name());

        if (UtilValidate.isNotEmpty(decoratorScreen.location())) {
            decoratorElement.setAttribute("location", decoratorScreen.location());
        }
        if (UtilValidate.isNotEmpty(decoratorScreen.fallbackName())) {
            decoratorElement.setAttribute("fallback-name", decoratorScreen.fallbackName());
        }
        if (UtilValidate.isNotEmpty(decoratorScreen.fallbackLocation())) {
            decoratorElement.setAttribute("fallback-location", decoratorScreen.fallbackLocation());
        }
        if (decoratorScreen.fallbackIfEmpty()) {
            decoratorElement.setAttribute("fallback-if-empty", "true");
        }
        if (decoratorScreen.autoDecoratorSectionInclude()) {
            decoratorElement.setAttribute("auto-decorator-section-include", "true");
        }

        // Process decorator sections from the annotation
        for (DecoratorSection section : decoratorScreen.sections()) {
            addDecoratorSectionElement(doc, decoratorElement, section);
        }

        // Also check for @DecoratorSection annotations on the class
        if (screenClass != null) {
            DecoratorSectionList sectionList = screenClass.getAnnotation(DecoratorSectionList.class);
            if (sectionList != null) {
                for (DecoratorSection section : sectionList.value()) {
                    addDecoratorSectionElement(doc, decoratorElement, section);
                }
            }

            DecoratorSection section = screenClass.getAnnotation(DecoratorSection.class);
            if (section != null && UtilValidate.isNotEmpty(section.name())) {
                addDecoratorSectionElement(doc, decoratorElement, section);
            }
        }

        widgetsElement.appendChild(decoratorElement);
    }

    /**
     * Adds a &lt;decorator-section&gt; element to decorator-screen.
     *
     * <p>SCIPIO: 4.0.0: Updated to support unified Widget[] value array for order preservation.
     * If value() array is populated, widgets are processed in array order.
     * Otherwise, falls back to legacy type-specific arrays (order NOT guaranteed).</p>
     */
    protected void addDecoratorSectionElement(Document doc, Element decoratorElement, DecoratorSection section) {
        if (UtilValidate.isEmpty(section.name())) {
            return;
        }

        Element sectionElement = doc.createElement("decorator-section");
        sectionElement.setAttribute("name", section.name());

        // Add optional attributes
        if (UtilValidate.isNotEmpty(section.useWhen())) {
            sectionElement.setAttribute("use-when", section.useWhen());
        }
        if (section.fallbackAutoInclude()) {
            sectionElement.setAttribute("fallback-auto-include", "true");
        }
        if (section.overrideByAutoInclude()) {
            sectionElement.setAttribute("override-by-auto-include", "true");
        }
        if (UtilValidate.isNotEmpty(section.contains())) {
            sectionElement.setAttribute("contains", section.contains());
        }

        // SCIPIO: 4.0.0: children keep their declared position(); see emitOrdered(). Without positions the
        // order is: unified widgets, legacy arrays, screenlets, containers, inline sections, decorators,
        // which put a results template above its search screenlet (BomSimulation, EditCostCalcs).
        List<OrderedChild> children = new ArrayList<>();
        Widget[] unifiedWidgets = section.value();
        boolean hasUnifiedWidgets = unifiedWidgets.length > 0;
        for (Widget widget : unifiedWidgets) {
            children.add(new OrderedChild(widget, () -> addUnifiedWidgetElement(doc, sectionElement, widget)));
        }

        // Check for pure legacy mode (no unified widgets, using deprecated type-specific arrays)
        boolean hasPureLegacyWidgets = !hasUnifiedWidgets && (
                section.decoratorSectionIncludes().length > 0 ||
                section.includeForms().length > 0 ||
                section.includeMenus().length > 0 ||
                section.includeScreens().length > 0 ||
                section.htmlTemplates().length > 0 ||
                section.labels().length > 0);

        if (hasPureLegacyWidgets) {
            Debug.logWarning("DecoratorSection [" + section.name() + "] uses legacy type-specific " +
                    "widget arrays which do NOT preserve rendering order. Consider migrating to " +
                    "unified @Widget(type=...) array format using value = {...}.", module);

            for (DecoratorSectionInclude x : section.decoratorSectionIncludes()) {
                children.add(new OrderedChild(x, () -> addDecoratorSectionIncludeElement(doc, sectionElement, x)));
            }
            for (IncludeForm x : section.includeForms()) {
                children.add(new OrderedChild(x, () -> addIncludeFormElement(doc, sectionElement, x)));
            }
            for (IncludeMenu x : section.includeMenus()) {
                children.add(new OrderedChild(x, () -> addIncludeMenuElement(doc, sectionElement, x)));
            }
            for (IncludeScreen x : section.includeScreens()) {
                children.add(new OrderedChild(x, () -> addIncludeScreenElement(doc, sectionElement, x)));
            }
            for (HtmlTemplate x : section.htmlTemplates()) {
                children.add(new OrderedChild(x, () -> addHtmlTemplateElement(doc, sectionElement, x)));
            }
            for (Label x : section.labels()) {
                children.add(new OrderedChild(x, () -> addLabelElement(doc, sectionElement, x)));
            }
        }

        // Containers/screenlets with nested content always come from the legacy arrays
        for (Screenlet x : section.screenlets()) {
            children.add(new OrderedChild(x, () -> addScreenletElement(doc, sectionElement, x)));
        }
        for (Container x : section.containers()) {
            children.add(new OrderedChild(x, () -> addContainerElement(doc, sectionElement, x)));
        }
        // Inline sections (nested sections with conditions)
        for (InlineSection x : section.sections()) {
            children.add(new OrderedChild(x, () -> addInlineSectionElement(doc, sectionElement, x)));
        }
        // SCIPIO: 4.0.0: a decorator-screen directly in the section (FindScreenDecorator in a body) was
        // dropped, which left the body empty
        for (DecoratorScreenNested x : section.decorators()) {
            children.add(new OrderedChild(x, () -> addDecoratorScreenNestedElement(doc, sectionElement, x)));
        }
        emitOrderedUnchecked(children);

        decoratorElement.appendChild(sectionElement);
    }

    /**
     * Adds a &lt;section&gt; element from an InlineSection annotation.
     * This is used for nested sections inside decorator-sections.
     */
    protected void addInlineSectionElement(Document doc, Element parentElement, InlineSection inlineSection) {
        Element sectionElement = doc.createElement("section");

        // Add optional name attribute
        if (UtilValidate.isNotEmpty(inlineSection.name())) {
            sectionElement.setAttribute("name", inlineSection.name());
        }
        if (inlineSection.shareScope()) {
            sectionElement.setAttribute("share-scope", "true");
        }
        if (UtilValidate.isNotEmpty(inlineSection.id())) {
            sectionElement.setAttribute("id", inlineSection.id());
        }
        if (UtilValidate.isNotEmpty(inlineSection.style())) {
            sectionElement.setAttribute("style", inlineSection.style());
        }
        if (UtilValidate.isNotEmpty(inlineSection.contains())) {
            sectionElement.setAttribute("contains", inlineSection.contains());
        }

        // Add condition element if defined
        Element conditionElement = buildConditionElement(doc, inlineSection.condition());
        if (conditionElement != null) {
            sectionElement.appendChild(conditionElement);
        }

        // Add actions element if defined
        Element actionsElement = buildActionsElementFromAnnotation(doc, inlineSection.actions());
        if (actionsElement != null && actionsElement.hasChildNodes()) {
            sectionElement.appendChild(actionsElement);
        }

        // Add widgets element
        Element widgetsElement = buildInlineWidgetsElement(doc, inlineSection.widgets());
        if (widgetsElement != null && widgetsElement.hasChildNodes()) {
            sectionElement.appendChild(widgetsElement);
        }

        // Add fail-widgets element
        Element failWidgetsElement = buildInlineFailWidgetsElement(doc, inlineSection.failWidgets());
        if (failWidgetsElement != null && failWidgetsElement.hasChildNodes()) {
            sectionElement.appendChild(failWidgetsElement);
        }

        // NOTE: Nested InlineSection[] sections() removed because Java annotations
        // cannot have self-referential types. Complex multi-level conditionals
        // should use separate screens or nested include-screen widgets.

        parentElement.appendChild(sectionElement);
    }

    /**
     * Builds an actions element directly from an Actions annotation.
     * Used for inline sections where we have an Actions annotation directly.
     */
    protected Element buildActionsElementFromAnnotation(Document doc, Actions actions) {
        Element actionsElement = doc.createElement("actions");
        // Delegate to the canonical handler, which supports both the unified value() array
        // (order-preserving, emitted by the converter) and the legacy type-specific arrays.
        addActionsContent(doc, actionsElement, actions);
        return actionsElement;
    }

    /**
     * Builds a &lt;widgets&gt; element from InlineWidgets annotation.
     */
    protected Element buildInlineWidgetsElement(Document doc, InlineWidgets widgets) {
        Element widgetsElement = doc.createElement("widgets");
        addInlineWidgetsContent(doc, widgetsElement, widgets);
        return widgetsElement;
    }

    /**
     * Builds a &lt;fail-widgets&gt; element from InlineWidgets annotation.
     */
    protected Element buildInlineFailWidgetsElement(Document doc, InlineWidgets widgets) {
        Element failWidgetsElement = doc.createElement("fail-widgets");
        addInlineWidgetsContent(doc, failWidgetsElement, widgets);
        return failWidgetsElement;
    }

    /**
     * SCIPIO: 4.0.0: Appends the children of an InlineWidgets annotation; the widgets and fail-widgets
     * builders were copies of each other. Children keep their declared position(); see emitOrdered().
     */
    protected void addInlineWidgetsContent(Document doc, Element parentElement, InlineWidgets widgets) {
        List<OrderedChild> children = new ArrayList<>();
        // SCIPIO: 4.0.0: a decorator-screen in a conditional section was dropped (e.g. FindBillingAccount)
        DecoratorScreenNested decorator = widgets.decorator();
        if (UtilValidate.isNotEmpty(decorator.name())) {
            children.add(new OrderedChild(decorator, () -> addDecoratorScreenNestedElement(doc, parentElement, decorator)));
        }

        // Check for unified widget array (recommended, preserves order)
        Widget[] unifiedWidgets = widgets.value();
        for (Widget x : unifiedWidgets) {
            children.add(new OrderedChild(x, () -> addUnifiedWidgetElement(doc, parentElement, x)));
        }

        // SCIPIO: 4.0.0: Always process screenlets, containers, and htmlTemplates from legacy arrays
        // These can complement unified widgets (e.g., complex screenlets that can't be unified)
        for (Screenlet x : widgets.screenlets()) {
            children.add(new OrderedChild(x, () -> addScreenletElement(doc, parentElement, x)));
        }
        for (Container x : widgets.containers()) {
            children.add(new OrderedChild(x, () -> addContainerElement(doc, parentElement, x)));
        }
        for (HtmlTemplate x : widgets.htmlTemplates()) {
            children.add(new OrderedChild(x, () -> addHtmlTemplateElement(doc, parentElement, x)));
        }

        if (unifiedWidgets.length == 0) {
            // Legacy mode: type-specific arrays (deprecated, order NOT guaranteed)
            for (DecoratorSectionInclude x : widgets.decoratorSectionIncludes()) {
                children.add(new OrderedChild(x, () -> addDecoratorSectionIncludeElement(doc, parentElement, x)));
            }
            for (IncludeScreen x : widgets.includeScreens()) {
                children.add(new OrderedChild(x, () -> addIncludeScreenElement(doc, parentElement, x)));
            }
            for (IncludeForm x : widgets.includeForms()) {
                children.add(new OrderedChild(x, () -> addIncludeFormElement(doc, parentElement, x)));
            }
            for (IncludeMenu x : widgets.includeMenus()) {
                children.add(new OrderedChild(x, () -> addIncludeMenuElement(doc, parentElement, x)));
            }
            for (Label x : widgets.labels()) {
                children.add(new OrderedChild(x, () -> addLabelElement(doc, parentElement, x)));
            }
            for (Image x : widgets.images()) {
                children.add(new OrderedChild(x, () -> addImageElement(doc, parentElement, x)));
            }
            for (HorizontalSeparator x : widgets.horizontalSeparators()) {
                children.add(new OrderedChild(x, () -> addHorizontalSeparatorElement(doc, parentElement, x)));
            }
            for (Content x : widgets.contents()) {
                children.add(new OrderedChild(x, () -> addContentElement(doc, parentElement, x)));
            }
            for (IncludeGrid x : widgets.includeGrids()) {
                children.add(new OrderedChild(x, () -> addIncludeGridElement(doc, parentElement, x)));
            }
            for (IncludeTreeWidget x : widgets.includeTrees()) {
                children.add(new OrderedChild(x, () -> addIncludeTreeElement(doc, parentElement, x)));
            }
            for (ScreenLink x : widgets.links()) {
                children.add(new OrderedChild(x, () -> addLinkElement(doc, parentElement, x)));
            }
            for (SubContent x : widgets.subContents()) {
                children.add(new OrderedChild(x, () -> addSubContentElement(doc, parentElement, x)));
            }
            for (ColumnContainer x : widgets.columnContainers()) {
                children.add(new OrderedChild(x, () -> addColumnContainerElement(doc, parentElement, x)));
            }
            for (IterateSection x : widgets.iterateSections()) {
                children.add(new OrderedChild(x, () -> addIterateSectionElement(doc, parentElement, x)));
            }
        }

        // SCIPIO: 4.0.0: a conditional section in the widgets was dropped (e.g. CommonInvoicesDecorator)
        for (SectionNested x : widgets.sections()) {
            children.add(new OrderedChild(x, () -> addSectionNestedElement(doc, parentElement, x)));
        }
        emitOrderedUnchecked(children);
    }

    /**
     * Adds a &lt;decorator-section-include&gt; element.
     */
    protected void addDecoratorSectionIncludeElement(Document doc, Element parentElement, DecoratorSectionInclude decoratorSectionInclude) {
        if (UtilValidate.isEmpty(decoratorSectionInclude.name())) {
            return;
        }

        Element includeElement = doc.createElement("decorator-section-include");
        includeElement.setAttribute("name", decoratorSectionInclude.name());
        parentElement.appendChild(includeElement);
    }

    /**
     * Adds an &lt;include-screen&gt; element.
     */
    protected void addIncludeScreenElement(Document doc, Element parentElement, IncludeScreen includeScreen) {
        if (UtilValidate.isEmpty(includeScreen.name())) {
            return;
        }

        Element includeElement = doc.createElement("include-screen");
        includeElement.setAttribute("name", includeScreen.name());

        if (UtilValidate.isNotEmpty(includeScreen.location())) {
            includeElement.setAttribute("location", includeScreen.location());
        }
        if (includeScreen.shareScope()) {
            includeElement.setAttribute("share-scope", "true");
        }

        parentElement.appendChild(includeElement);
    }

    /**
     * Adds an &lt;include-form&gt; element.
     */
    protected void addIncludeFormElement(Document doc, Element parentElement, IncludeForm includeForm) {
        if (UtilValidate.isEmpty(includeForm.name())) {
            return;
        }

        Element includeElement = doc.createElement("include-form");
        includeElement.setAttribute("name", includeForm.name());
        includeElement.setAttribute("location", includeForm.location());
        if (includeForm.shareScope()) {
            includeElement.setAttribute("share-scope", "true");
        }

        parentElement.appendChild(includeElement);
    }

    /**
     * Adds an &lt;include-menu&gt; element.
     */
    protected void addIncludeMenuElement(Document doc, Element parentElement, IncludeMenu includeMenu) {
        if (UtilValidate.isEmpty(includeMenu.name())) {
            return;
        }

        Element includeElement = doc.createElement("include-menu");
        includeElement.setAttribute("name", includeMenu.name());
        includeElement.setAttribute("location", includeMenu.location());
        if (includeMenu.shareScope()) {
            includeElement.setAttribute("share-scope", "true");
        }
        if (includeMenu.maxDepth() >= 0) {
            includeElement.setAttribute("max-depth", String.valueOf(includeMenu.maxDepth()));
        }
        if (UtilValidate.isNotEmpty(includeMenu.subMenus())) {
            includeElement.setAttribute("sub-menus", includeMenu.subMenus());
        }
        if (UtilValidate.isNotEmpty(includeMenu.itemConditionMode())) {
            includeElement.setAttribute("item-condition-mode", includeMenu.itemConditionMode());
        }

        parentElement.appendChild(includeElement);
    }

    /**
     * Adds an &lt;image&gt; element to widgets.
     */
    protected void addImageElement(Document doc, Element parentElement, Image image) {
        if (UtilValidate.isEmpty(image.src())) {
            return;
        }

        Element imageElement = doc.createElement("image");
        imageElement.setAttribute("src", image.src());

        if (UtilValidate.isNotEmpty(image.id())) {
            imageElement.setAttribute("id", image.id());
        }
        if (UtilValidate.isNotEmpty(image.style())) {
            imageElement.setAttribute("style", image.style());
        }
        if (UtilValidate.isNotEmpty(image.width())) {
            imageElement.setAttribute("width", image.width());
        }
        if (UtilValidate.isNotEmpty(image.height())) {
            imageElement.setAttribute("height", image.height());
        }
        if (UtilValidate.isNotEmpty(image.border())) {
            imageElement.setAttribute("border", image.border());
        }
        if (UtilValidate.isNotEmpty(image.alt())) {
            imageElement.setAttribute("alt", image.alt());
        }
        if (!"content".equals(image.urlMode())) {
            imageElement.setAttribute("url-mode", image.urlMode());
        }

        parentElement.appendChild(imageElement);
    }

    /**
     * Adds a &lt;horizontal-separator&gt; element to widgets.
     */
    protected void addHorizontalSeparatorElement(Document doc, Element parentElement, HorizontalSeparator separator) {
        Element separatorElement = doc.createElement("horizontal-separator");

        if (UtilValidate.isNotEmpty(separator.id())) {
            separatorElement.setAttribute("id", separator.id());
        }
        if (UtilValidate.isNotEmpty(separator.name())) {
            separatorElement.setAttribute("name", separator.name());
        }
        if (UtilValidate.isNotEmpty(separator.style())) {
            separatorElement.setAttribute("style", separator.style());
        }

        parentElement.appendChild(separatorElement);
    }

    /**
     * Adds a &lt;content&gt; element to widgets.
     */
    protected void addContentElement(Document doc, Element parentElement, Content content) {
        if (UtilValidate.isEmpty(content.contentId()) && UtilValidate.isEmpty(content.dataResourceId())) {
            return;
        }

        Element contentElement = doc.createElement("content");

        if (UtilValidate.isNotEmpty(content.contentId())) {
            contentElement.setAttribute("content-id", content.contentId());
        }
        if (UtilValidate.isNotEmpty(content.dataResourceId())) {
            contentElement.setAttribute("dataresource-id", content.dataResourceId());
        }
        if (UtilValidate.isNotEmpty(content.editRequest())) {
            contentElement.setAttribute("edit-request", content.editRequest());
        }
        if (!"editWrapper".equals(content.editContainerStyle())) {
            contentElement.setAttribute("edit-container-style", content.editContainerStyle());
        }
        if (!"enableEdit".equals(content.enableEditName())) {
            contentElement.setAttribute("enable-edit-name", content.enableEditName());
        }
        if (content.xmlEscape()) {
            contentElement.setAttribute("xml-escape", "true");
        }
        if (UtilValidate.isNotEmpty(content.width())) {
            contentElement.setAttribute("width", content.width());
        }
        if (UtilValidate.isNotEmpty(content.height())) {
            contentElement.setAttribute("height", content.height());
        }
        if (UtilValidate.isNotEmpty(content.border())) {
            contentElement.setAttribute("border", content.border());
        }

        parentElement.appendChild(contentElement);
    }

    // ========== Unified Widget Support ==========

    /**
     * Adds a widget element from a unified Widget annotation.
     * This dispatcher method routes to the appropriate type-specific method based on widget type.
     */
    protected void addUnifiedWidgetElement(Document doc, Element parentElement, Widget widget) {
        switch (widget.type()) {
            case INCLUDE_SCREEN:
                addIncludeScreenFromUnified(doc, parentElement, widget);
                break;
            case INCLUDE_FORM:
                addIncludeFormFromUnified(doc, parentElement, widget);
                break;
            case INCLUDE_MENU:
                addIncludeMenuFromUnified(doc, parentElement, widget);
                break;
            case INCLUDE_GRID:
                addIncludeGridFromUnified(doc, parentElement, widget);
                break;
            case INCLUDE_TREE:
                addIncludeTreeFromUnified(doc, parentElement, widget);
                break;
            case LABEL:
                addLabelFromUnified(doc, parentElement, widget);
                break;
            case SCREENLET:
                addScreenletFromUnified(doc, parentElement, widget);
                break;
            case CONTAINER:
                addContainerFromUnified(doc, parentElement, widget);
                break;
            case HTML_TEMPLATE:
                addHtmlTemplateFromUnified(doc, parentElement, widget);
                break;
            case IMAGE:
                addImageFromUnified(doc, parentElement, widget);
                break;
            case HORIZONTAL_SEPARATOR:
                addHorizontalSeparatorFromUnified(doc, parentElement, widget);
                break;
            case CONTENT:
                addContentFromUnified(doc, parentElement, widget);
                break;
            case SUB_CONTENT:
                addSubContentFromUnified(doc, parentElement, widget);
                break;
            case DECORATOR_SECTION_INCLUDE:
                addDecoratorSectionIncludeFromUnified(doc, parentElement, widget);
                break;
            case LINK:
                addLinkFromUnified(doc, parentElement, widget);
                break;
            case COLUMN_CONTAINER:
                // Column container requires nested columns, which unified Widget doesn't support
                Debug.logWarning("Unified Widget type COLUMN_CONTAINER not fully supported - use @ColumnContainer annotation instead", module);
                break;
            case ITERATE_SECTION:
                // SCIPIO: 4.0.0: a Widget cannot hold a section, so the converter moves the section of an
                // iterate-section into a helper screen; this used to render nothing at all
                addIterateSectionFromUnified(doc, parentElement, widget);
                break;
            case INCLUDE_PORTAL_PAGE:
                addIncludePortalPageFromUnified(doc, parentElement, widget);
                break;
            default:
                Debug.logWarning("Unknown unified widget type: " + widget.type(), module);
        }
    }

    /**
     * Adds an include-screen element from unified Widget.
     */
    protected void addIncludeScreenFromUnified(Document doc, Element parentElement, Widget widget) {
        if (UtilValidate.isEmpty(widget.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-screen");
        includeElement.setAttribute("name", widget.name());
        if (UtilValidate.isNotEmpty(widget.location())) {
            includeElement.setAttribute("location", widget.location());
        }
        if (widget.shareScope()) {
            includeElement.setAttribute("share-scope", "true");
        }
        parentElement.appendChild(includeElement);
    }

    /**
     * Adds an include-form element from unified Widget.
     */
    protected void addIncludeFormFromUnified(Document doc, Element parentElement, Widget widget) {
        if (UtilValidate.isEmpty(widget.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-form");
        includeElement.setAttribute("name", widget.name());
        if (UtilValidate.isNotEmpty(widget.location())) {
            includeElement.setAttribute("location", widget.location());
        }
        if (widget.shareScope()) {
            includeElement.setAttribute("share-scope", "true");
        }
        parentElement.appendChild(includeElement);
    }

    /**
     * Adds an include-menu element from unified Widget.
     */
    protected void addIncludeMenuFromUnified(Document doc, Element parentElement, Widget widget) {
        if (UtilValidate.isEmpty(widget.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-menu");
        includeElement.setAttribute("name", widget.name());
        if (UtilValidate.isNotEmpty(widget.location())) {
            includeElement.setAttribute("location", widget.location());
        }
        parentElement.appendChild(includeElement);
    }

    /**
     * Adds an include-grid element from unified Widget.
     */
    protected void addIncludeGridFromUnified(Document doc, Element parentElement, Widget widget) {
        if (UtilValidate.isEmpty(widget.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-grid");
        includeElement.setAttribute("name", widget.name());
        if (UtilValidate.isNotEmpty(widget.location())) {
            includeElement.setAttribute("location", widget.location());
        }
        if (widget.shareScope()) {
            includeElement.setAttribute("share-scope", "true");
        }
        parentElement.appendChild(includeElement);
    }

    /**
     * Adds an include-tree element from unified Widget.
     */
    protected void addIncludeTreeFromUnified(Document doc, Element parentElement, Widget widget) {
        if (UtilValidate.isEmpty(widget.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-tree");
        includeElement.setAttribute("name", widget.name());
        if (UtilValidate.isNotEmpty(widget.location())) {
            includeElement.setAttribute("location", widget.location());
        }
        parentElement.appendChild(includeElement);
    }

    /**
     * Adds a label element from unified Widget.
     */
    protected void addLabelFromUnified(Document doc, Element parentElement, Widget widget) {
        Element labelElement = doc.createElement("label");
        if (UtilValidate.isNotEmpty(widget.text())) {
            labelElement.setAttribute("text", widget.text());
        }
        if (UtilValidate.isNotEmpty(widget.style())) {
            labelElement.setAttribute("style", widget.style());
        }
        if (UtilValidate.isNotEmpty(widget.id())) {
            labelElement.setAttribute("id", widget.id());
        }
        parentElement.appendChild(labelElement);
    }

    /**
     * Adds an html-template element from unified Widget.
     */
    protected void addHtmlTemplateFromUnified(Document doc, Element parentElement, Widget widget) {
        // Create platform-specific wrapper
        // SCIPIO: 4.0.0: platform() selects the branch; alternates of one slot share one element
        Element htmlElement = appendPlatformBranch(doc, parentElement, widget.platform());
        Element templateElement = doc.createElement("html-template");

        if (UtilValidate.isNotEmpty(widget.location())) {
            templateElement.setAttribute("location", widget.location());
        }
        // SCIPIO: 4.0.0: a template written inline
        if (UtilValidate.isNotEmpty(widget.content())) {
            templateElement.setTextContent(widget.content());
        }

        htmlElement.appendChild(templateElement);
    }

    /**
     * Adds an image element from unified Widget.
     */
    protected void addImageFromUnified(Document doc, Element parentElement, Widget widget) {
        if (UtilValidate.isEmpty(widget.src())) {
            return;
        }
        Element imageElement = doc.createElement("image");
        imageElement.setAttribute("src", widget.src());
        if (UtilValidate.isNotEmpty(widget.id())) {
            imageElement.setAttribute("id", widget.id());
        }
        if (UtilValidate.isNotEmpty(widget.style())) {
            imageElement.setAttribute("style", widget.style());
        }
        if (UtilValidate.isNotEmpty(widget.width())) {
            imageElement.setAttribute("width", widget.width());
        }
        if (UtilValidate.isNotEmpty(widget.height())) {
            imageElement.setAttribute("height", widget.height());
        }
        if (UtilValidate.isNotEmpty(widget.border())) {
            imageElement.setAttribute("border", widget.border());
        }
        if (UtilValidate.isNotEmpty(widget.alt())) {
            imageElement.setAttribute("alt", widget.alt());
        }
        if (UtilValidate.isNotEmpty(widget.urlMode())) {
            imageElement.setAttribute("url-mode", widget.urlMode());
        }
        parentElement.appendChild(imageElement);
    }

    /**
     * Adds a horizontal-separator element from unified Widget.
     */
    protected void addHorizontalSeparatorFromUnified(Document doc, Element parentElement, Widget widget) {
        Element separatorElement = doc.createElement("horizontal-separator");
        if (UtilValidate.isNotEmpty(widget.id())) {
            separatorElement.setAttribute("id", widget.id());
        }
        if (UtilValidate.isNotEmpty(widget.name())) {
            separatorElement.setAttribute("name", widget.name());
        }
        if (UtilValidate.isNotEmpty(widget.style())) {
            separatorElement.setAttribute("style", widget.style());
        }
        parentElement.appendChild(separatorElement);
    }

    /**
     * Adds a content element from unified Widget.
     */
    protected void addContentFromUnified(Document doc, Element parentElement, Widget widget) {
        if (UtilValidate.isEmpty(widget.contentId()) && UtilValidate.isEmpty(widget.dataResourceId())) {
            return;
        }
        Element contentElement = doc.createElement("content");
        if (UtilValidate.isNotEmpty(widget.contentId())) {
            contentElement.setAttribute("content-id", widget.contentId());
        }
        if (UtilValidate.isNotEmpty(widget.dataResourceId())) {
            contentElement.setAttribute("dataresource-id", widget.dataResourceId());
        }
        if (UtilValidate.isNotEmpty(widget.editRequest())) {
            contentElement.setAttribute("edit-request", widget.editRequest());
        }
        if (UtilValidate.isNotEmpty(widget.editContainerStyle())) {
            contentElement.setAttribute("edit-container-style", widget.editContainerStyle());
        }
        if (UtilValidate.isNotEmpty(widget.enableEditValue())) {
            contentElement.setAttribute("enable-edit-value", widget.enableEditValue());
        }
        // SCIPIO: 4.0.0: enable-edit-name was dropped by the converter
        if (UtilValidate.isNotEmpty(widget.enableEditName())) {
            contentElement.setAttribute("enable-edit-name", widget.enableEditName());
        }
        if (widget.xmlEscape()) {
            contentElement.setAttribute("xml-escape", "true");
        }
        parentElement.appendChild(contentElement);
    }

    /**
     * Adds a sub-content element from unified Widget.
     */
    protected void addSubContentFromUnified(Document doc, Element parentElement, Widget widget) {
        if (UtilValidate.isEmpty(widget.contentId())) {
            return;
        }
        Element subContentElement = doc.createElement("sub-content");
        subContentElement.setAttribute("content-id", widget.contentId());
        if (UtilValidate.isNotEmpty(widget.mapKey())) {
            subContentElement.setAttribute("map-key", widget.mapKey());
        }
        if (UtilValidate.isNotEmpty(widget.assocName())) {
            subContentElement.setAttribute("assoc-name", widget.assocName());
        }
        // SCIPIO: 4.0.0: edit-request/edit-container-style/enable-edit-name/enable-edit-value were dropped by the converter
        if (UtilValidate.isNotEmpty(widget.editRequest())) {
            subContentElement.setAttribute("edit-request", widget.editRequest());
        }
        if (UtilValidate.isNotEmpty(widget.editContainerStyle())) {
            subContentElement.setAttribute("edit-container-style", widget.editContainerStyle());
        }
        if (UtilValidate.isNotEmpty(widget.enableEditName())) {
            subContentElement.setAttribute("enable-edit-name", widget.enableEditName());
        }
        if (UtilValidate.isNotEmpty(widget.enableEditValue())) {
            subContentElement.setAttribute("enable-edit-value", widget.enableEditValue());
        }
        if (widget.xmlEscape()) {
            subContentElement.setAttribute("xml-escape", "true");
        }
        parentElement.appendChild(subContentElement);
    }

    /**
     * Adds a decorator-section-include element from unified Widget.
     */
    protected void addDecoratorSectionIncludeFromUnified(Document doc, Element parentElement, Widget widget) {
        if (UtilValidate.isEmpty(widget.name())) {
            return;
        }
        Element includeElement = doc.createElement("decorator-section-include");
        includeElement.setAttribute("name", widget.name());
        parentElement.appendChild(includeElement);
    }

    /**
     * Adds a link element from unified Widget.
     */
    protected void addLinkFromUnified(Document doc, Element parentElement, Widget widget) {
        Element linkElement = doc.createElement("link");
        if (UtilValidate.isNotEmpty(widget.text())) {
            linkElement.setAttribute("text", widget.text());
        }
        if (UtilValidate.isNotEmpty(widget.target())) {
            linkElement.setAttribute("target", widget.target());
        }
        if (UtilValidate.isNotEmpty(widget.targetWindow())) {
            linkElement.setAttribute("target-window", widget.targetWindow());
        }
        if (UtilValidate.isNotEmpty(widget.style())) {
            linkElement.setAttribute("style", widget.style());
        }
        if (UtilValidate.isNotEmpty(widget.id())) {
            linkElement.setAttribute("id", widget.id());
        }
        if (UtilValidate.isNotEmpty(widget.linkType())) {
            linkElement.setAttribute("link-type", widget.linkType());
        }
        if (UtilValidate.isNotEmpty(widget.title())) {
            linkElement.setAttribute("title", widget.title());
        }
        parentElement.appendChild(linkElement);
    }

    /**
     * Adds a simple container element from unified Widget.
     * Only supports containers WITHOUT nested content. Containers with nested
     * widgets must use the legacy @Container annotation due to Java annotation
     * cycle limitations.
     * SCIPIO: 4.0.0: Added for unified container support (simple containers only).
     */
    protected void addContainerFromUnified(Document doc, Element parentElement, Widget widget) {
        Element containerElement = doc.createElement("container");

        if (UtilValidate.isNotEmpty(widget.id())) {
            containerElement.setAttribute("id", widget.id());
        }
        if (UtilValidate.isNotEmpty(widget.style())) {
            containerElement.setAttribute("style", widget.style());
        }
        if (UtilValidate.isNotEmpty(widget.autoUpdateTargetId())) {
            containerElement.setAttribute("auto-update-target-id", widget.autoUpdateTargetId());
        }
        if (widget.autoUpdateInterval() > 0) {
            containerElement.setAttribute("auto-update-interval", String.valueOf(widget.autoUpdateInterval()));
        }
        if (UtilValidate.isNotEmpty(widget.contains())) {
            containerElement.setAttribute("contains", widget.contains());
        }

        // Note: Nested widgets are NOT supported in unified @Widget due to Java annotation cycles.
        // Use legacy @Container annotation for containers with nested content.

        parentElement.appendChild(containerElement);
    }

    /**
     * Adds a simple screenlet element from unified Widget.
     * Only supports screenlets WITHOUT nested content. Screenlets with nested
     * widgets must use the legacy @Screenlet annotation due to Java annotation
     * cycle limitations.
     * SCIPIO: 4.0.0: Added for unified screenlet support (simple screenlets only).
     */
    protected void addScreenletFromUnified(Document doc, Element parentElement, Widget widget) {
        Element screenletElement = doc.createElement("screenlet");

        if (UtilValidate.isNotEmpty(widget.name())) {
            screenletElement.setAttribute("name", widget.name());
        }
        if (UtilValidate.isNotEmpty(widget.title())) {
            screenletElement.setAttribute("title", widget.title());
        }
        if (UtilValidate.isNotEmpty(widget.id())) {
            screenletElement.setAttribute("id", widget.id());
        }

        // Note: Nested widgets are NOT supported in unified @Widget due to Java annotation cycles.
        // Use legacy @Screenlet annotation for screenlets with nested content.

        parentElement.appendChild(screenletElement);
    }

    // ========== Legacy Widget Helpers (for type-specific annotations) ==========

    /**
     * Adds an include-grid element.
     */
    protected void addIncludeGridElement(Document doc, Element parentElement, IncludeGrid includeGrid) {
        if (UtilValidate.isEmpty(includeGrid.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-grid");
        includeElement.setAttribute("name", includeGrid.name());
        if (UtilValidate.isNotEmpty(includeGrid.location())) {
            includeElement.setAttribute("location", includeGrid.location());
        }
        if (includeGrid.shareScope()) {
            includeElement.setAttribute("share-scope", "true");
        }
        parentElement.appendChild(includeElement);
    }

    /**
     * Adds an include-tree element.
     */
    protected void addIncludeTreeElement(Document doc, Element parentElement, IncludeTreeWidget includeTree) {
        if (UtilValidate.isEmpty(includeTree.name())) {
            return;
        }
        Element includeElement = doc.createElement("include-tree");
        includeElement.setAttribute("name", includeTree.name());
        if (UtilValidate.isNotEmpty(includeTree.location())) {
            includeElement.setAttribute("location", includeTree.location());
        }
        parentElement.appendChild(includeElement);
    }

    /**
     * Adds a link element.
     */
    protected void addLinkElement(Document doc, Element parentElement, ScreenLink link) {
        Element linkElement = doc.createElement("link");
        if (UtilValidate.isNotEmpty(link.text())) {
            linkElement.setAttribute("text", link.text());
        }
        if (UtilValidate.isNotEmpty(link.target())) {
            linkElement.setAttribute("target", link.target());
        }
        if (UtilValidate.isNotEmpty(link.targetWindow())) {
            linkElement.setAttribute("target-window", link.targetWindow());
        }
        if (UtilValidate.isNotEmpty(link.style())) {
            linkElement.setAttribute("style", link.style());
        }
        if (UtilValidate.isNotEmpty(link.id())) {
            linkElement.setAttribute("id", link.id());
        }
        if (UtilValidate.isNotEmpty(link.linkType())) {
            linkElement.setAttribute("link-type", link.linkType());
        }
        if (UtilValidate.isNotEmpty(link.name())) {
            linkElement.setAttribute("name", link.name());
        }
        if (UtilValidate.isNotEmpty(link.prefix())) {
            linkElement.setAttribute("prefix", link.prefix());
        }
        if (UtilValidate.isNotEmpty(link.title())) {
            linkElement.setAttribute("title", link.title());
        }
        if (UtilValidate.isNotEmpty(link.urlMode())) {
            linkElement.setAttribute("url-mode", link.urlMode());
        }
        if (UtilValidate.isNotEmpty(link.fullPath())) {
            linkElement.setAttribute("full-path", link.fullPath());
        }
        if (UtilValidate.isNotEmpty(link.secure())) {
            linkElement.setAttribute("secure", link.secure());
        }
        if (UtilValidate.isNotEmpty(link.encode())) {
            linkElement.setAttribute("encode", link.encode());
        }
        parentElement.appendChild(linkElement);
    }

    /**
     * Adds a sub-content element.
     */
    protected void addSubContentElement(Document doc, Element parentElement, SubContent subContent) {
        if (UtilValidate.isEmpty(subContent.contentId())) {
            return;
        }
        Element subContentElement = doc.createElement("sub-content");
        subContentElement.setAttribute("content-id", subContent.contentId());
        if (UtilValidate.isNotEmpty(subContent.mapKey())) {
            subContentElement.setAttribute("map-key", subContent.mapKey());
        }
        if (subContent.xmlEscape()) {
            subContentElement.setAttribute("xml-escape", "true");
        }
        parentElement.appendChild(subContentElement);
    }

    /**
     * Adds a column-container element.
     */
    protected void addColumnContainerElement(Document doc, Element parentElement, ColumnContainer columnContainer) {
        Element containerElement = doc.createElement("column-container");
        if (UtilValidate.isNotEmpty(columnContainer.id())) {
            containerElement.setAttribute("id", columnContainer.id());
        }
        if (UtilValidate.isNotEmpty(columnContainer.style())) {
            containerElement.setAttribute("style", columnContainer.style());
        }
        // Note: columns would need to be added here if we supported nested column definitions
        parentElement.appendChild(containerElement);
    }

    /**
     * Adds an iterate-section element.
     */
    /**
     * SCIPIO: 4.0.0: Builds an &lt;iterate-section&gt; whose section includes the helper screen named by the
     * widget with a shared scope, so the helper sees the entry of each iteration.
     */
    protected void addIterateSectionFromUnified(Document doc, Element parentElement, Widget widget) {
        if (UtilValidate.isEmpty(widget.list())) {
            return;
        }
        Element iterateElement = doc.createElement("iterate-section");
        iterateElement.setAttribute("list", widget.list());
        if (UtilValidate.isNotEmpty(widget.entry())) {
            iterateElement.setAttribute("entry", widget.entry());
        }
        if (UtilValidate.isNotEmpty(widget.key())) {
            iterateElement.setAttribute("key", widget.key());
        }
        if (widget.viewSize() > 0) {
            iterateElement.setAttribute("view-size", String.valueOf(widget.viewSize()));
        }
        if (!widget.paginate()) {
            iterateElement.setAttribute("paginate", "false");
        }
        if (UtilValidate.isNotEmpty(widget.paginateTarget())) {
            iterateElement.setAttribute("paginate-target", widget.paginateTarget());
        }
        Element sectionElement = doc.createElement("section");
        if (UtilValidate.isNotEmpty(widget.name())) {
            Element widgetsElement = doc.createElement("widgets");
            Element includeElement = doc.createElement("include-screen");
            includeElement.setAttribute("name", widget.name());
            if (UtilValidate.isNotEmpty(widget.location())) {
                includeElement.setAttribute("location", widget.location());
            }
            includeElement.setAttribute("share-scope", "true");
            widgetsElement.appendChild(includeElement);
            sectionElement.appendChild(widgetsElement);
        }
        iterateElement.appendChild(sectionElement);
        parentElement.appendChild(iterateElement);
    }

    /** SCIPIO: 4.0.0: Builds an &lt;include-portal-page&gt; element from a unified Widget. */
    protected void addIncludePortalPageFromUnified(Document doc, Element parentElement, Widget widget) {
        Element element = doc.createElement("include-portal-page");
        element.setAttribute("id", widget.id());
        if (UtilValidate.isNotEmpty(widget.confMode())) {
            element.setAttribute("conf-mode", widget.confMode());
        }
        if (UtilValidate.isNotEmpty(widget.usePrivate())) {
            element.setAttribute("use-private", widget.usePrivate());
        }
        parentElement.appendChild(element);
    }

    protected void addIterateSectionElement(Document doc, Element parentElement, IterateSection iterateSection) {
        if (UtilValidate.isEmpty(iterateSection.list())) {
            return;
        }
        Element iterateElement = doc.createElement("iterate-section");
        iterateElement.setAttribute("list", iterateSection.list());
        if (UtilValidate.isNotEmpty(iterateSection.entry())) {
            iterateElement.setAttribute("entry", iterateSection.entry());
        }
        if (UtilValidate.isNotEmpty(iterateSection.key())) {
            iterateElement.setAttribute("key", iterateSection.key());
        }
        if (UtilValidate.isNotEmpty(iterateSection.viewSize())) {
            iterateElement.setAttribute("view-size", iterateSection.viewSize());
        }
        if (UtilValidate.isNotEmpty(iterateSection.paginate()) && !"${paginate}".equals(iterateSection.paginate())) {
            iterateElement.setAttribute("paginate", iterateSection.paginate());
        }
        // Note: section widgets would need to be added here if we supported nested definitions
        parentElement.appendChild(iterateElement);
    }
}

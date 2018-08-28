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
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Set;

import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.collections.MapStack;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.webapp.renderer.RenderContextFetcher;
import org.ofbiz.widget.WidgetFactory;
import org.ofbiz.widget.WidgetWorker;
import org.ofbiz.widget.model.CommonWidgetModels.AutoEntityParameters;
import org.ofbiz.widget.model.CommonWidgetModels.AutoServiceParameters;
import org.ofbiz.widget.model.CommonWidgetModels.Image;
import org.ofbiz.widget.model.CommonWidgetModels.Link;
import org.ofbiz.widget.model.CommonWidgetModels.Parameter;
import org.ofbiz.widget.model.ScreenFallback.FlexibleScreenFallbackSettings;
import org.ofbiz.widget.model.ScreenFallback.SimpleFlexibleScreenFallbackSettings;
import org.ofbiz.widget.portal.PortalPageWorker;
import org.ofbiz.widget.renderer.FormRenderer;
import org.ofbiz.widget.renderer.FormStringRenderer;
import org.ofbiz.widget.renderer.MenuStringRenderer;
import org.ofbiz.widget.renderer.ScreenRenderer;
import org.ofbiz.widget.renderer.ScreenStringRenderer;
import org.ofbiz.widget.renderer.TreeStringRenderer;
import org.ofbiz.widget.renderer.WidgetRenderTargetExpr;
import org.ofbiz.widget.renderer.WidgetRenderTargetExpr.WidgetRenderTargetState;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;


/**
 * Widget Library - Screen model class
 */
@SuppressWarnings("serial")
public abstract class ModelScreenWidget extends ModelWidget implements ContainsExpr.FlexibleContainsExprAttrWidget {
    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private final ModelScreen modelScreen;
    /**
     * SCIPIO: contains-expression, supported on any item for which the "contains" attribute is
     * defined in widget-screen.xsd. Handled by {@link WidgetRenderTargetExpr.WidgetRenderTargetState}. Default is contains all.
     * TODO: REVIEW: to simplify implementation, I have simply added this to all elements for now, even
     * those that don't define it in widget-screen.xsd; maybe split up...
     * Added 2017-05-04.
     */
    private final ContainsExpr.ContainsExprHolder containsExpr;

    public ModelScreenWidget(ModelScreen modelScreen, Element widgetElement) {
        super(widgetElement);
        this.modelScreen = modelScreen;
        if (Debug.verboseOn()) Debug.logVerbose("Reading Screen sub-widget with name: " + widgetElement.getNodeName(), module);
        // SCIPIO: new
        this.containsExpr = ContainsExpr.ContainsExprHolder.getInstanceOrDefault(widgetElement.getAttribute("contains"), widgetElement);
    }

    /**
     * Renders the widget.
     * SCIPIO: NOTE: As of 2017-05-04, all subclasses now override {@link #renderWidgetStringCore} instead of this.
     * This separation allows us to insert logic at every element visit.
     * <p>
     * <strong>TARGETED RENDERING</strong><br/>
     * This method now performs a targeted rendering applicability check, recorded through {@link WidgetRenderTargetState}.
     * <p>
     * If we haven't found the target element to render yet, we will for the most part
     * enter into all elements here EXCEPT those that have been explicitly marked as not
     * containing the target element, using section contains string such as: 
     * {@code <section contains="!$My-Section">}.
     * Those that are entered perform their actions as usual, but output is prevented where possible
     * (FTL may cause issues with this).
     * <p>
     * If this widget is the target, we turn on the matched/rendering flag in the state, so that
     * all children and their markup will render. When done, we mark finished.
     * <p>
     * Everything after return from target widget render is discarded.
     */
    public final void renderWidgetString(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
        // SCIPIO: targeted rendering applicability check.
        WidgetRenderTargetState renderTargetState = WidgetRenderTargetExpr.getRenderTargetState(context);
        WidgetRenderTargetState.ExecutionInfo execInfo = renderTargetState.handleShouldExecute(this, writer, context, screenStringRenderer);
        if (!execInfo.shouldExecute()) {
            return;
        }
        try {
            renderWidgetStringCore(execInfo.getWriterForElementRender(), context, screenStringRenderer);
        } finally {
            execInfo.handleFinished(context); // SCIPIO: return logic
        }
    }

    /**
     * SCIPIO: Widget render core implementation.
     * As of 2017-05-04, all subclasses now override this instead of {@link #renderWidgetString} (they were all renamed).
     */
    protected abstract void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException;
    
    protected static List<ModelScreenWidget> readSubWidgets(ModelScreen modelScreen, List<? extends Element> subElementList) {
        if (subElementList.isEmpty()) {
            return Collections.emptyList();
        }
        List<ModelScreenWidget> subWidgets = new ArrayList<ModelScreenWidget>(subElementList.size());
        for (Element subElement: subElementList) {
            subWidgets.add(WidgetFactory.getModelScreenWidget(modelScreen, subElement));
        }
        return Collections.unmodifiableList(subWidgets);
    }

    protected static void renderSubWidgetsString(List<ModelScreenWidget> subWidgets, Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
        if (subWidgets == null) {
            return;
        }
        for (ModelScreenWidget subWidget: subWidgets) {
            if (Debug.verboseOn()) Debug.logVerbose("Rendering screen " + subWidget.getModelScreen().getName() + "; widget class is " + subWidget.getClass().getName(), module);

            // render the sub-widget itself
            subWidget.renderWidgetString(writer, context, screenStringRenderer);
        }
    }

    public ModelScreen getModelScreen() {
        return this.modelScreen;
    }
    
    /**
     * SCIPIO: Returns the complex contains-expression. Never null.
     */
    @Override
    public ContainsExpr getContainsExpr(Map<String, Object> context) {
        return containsExpr.getContainsExpr(context);
    }

    public static final class SectionsRenderer implements org.ofbiz.webapp.renderer.BasicSectionsRenderer, Map<String, ModelScreenWidget> { // SCIPIO: new BasicSectionsRenderer interface
        private final Map<String, ModelScreenWidget> sectionMap;
        private final ScreenStringRenderer screenStringRenderer;
        //private final Map<String, Object> context;
        //private final Appendable writer;
        // SCIPIO: 2017-03-09: now using RenderContextFetcher
        private final RenderContextFetcher contextFetcher;
        
        // SCIPIO: feature: ability to render previously-defined sections (from a caller) as if part of these sections.
        // Essentially we mix sections from different decorators therefore different contexts.
        // This is not well encapsulated; SectionsRenderer implements
        // Map and that part is public. but context is private so it should be
        // impossible for other code to apply the wrong context on the sections; must go through render(String).
        private final Map<String, ModelScreenWidget> localSectionMap;
        private final Map<String, ModelScreenWidget> prevSectionMap; // Need separate from prevSections so can filter further
        private final SectionsRenderer prevSections;
        private final boolean includePrevSections;
        private final ModelScreenWidget sourceDecoratorScreen; // SCIPIO: remembers which DecoratorScreen created this object

        public SectionsRenderer(Map<String, ModelScreenWidget> sectionMap, RenderContextFetcher contextFetcher,
                ScreenStringRenderer screenStringRenderer, ModelScreenWidget sourceDecoratorScreen) {
            Map<String, ModelScreenWidget> localMap = new HashMap<String, ModelScreenWidget>();
            localMap.putAll(sectionMap);
            this.sectionMap = Collections.unmodifiableMap(localMap);
            this.contextFetcher = contextFetcher;
            this.screenStringRenderer = screenStringRenderer;
            
            this.includePrevSections = false;
            this.localSectionMap = this.sectionMap;
            this.prevSectionMap = null;
            this.prevSections = null;
            this.sourceDecoratorScreen = sourceDecoratorScreen;
        }
        
        public SectionsRenderer(Map<String, ModelScreenWidget> sectionMap, RenderContextFetcher contextFetcher,
                ScreenStringRenderer screenStringRenderer, 
                SectionsRenderer prevSections, Map<String, ModelScreenWidget> prevSectionMap, boolean includePrevSections, ModelScreenWidget sourceDecoratorScreen) {
            Map<String, ModelScreenWidget> localMap = new HashMap<String, ModelScreenWidget>();
            if (includePrevSections && prevSections != null && prevSectionMap != null) {
                localMap.putAll(prevSectionMap);
                // overridden by local sections
            }
            localMap.putAll(sectionMap);
            this.sectionMap = Collections.unmodifiableMap(localMap);
            this.contextFetcher = contextFetcher;
            this.screenStringRenderer = screenStringRenderer;
            
            this.includePrevSections = includePrevSections;
            if (includePrevSections) {
                Map<String, ModelScreenWidget> localSectionMap = new HashMap<String, ModelScreenWidget>();
                localSectionMap.putAll(sectionMap);
                this.localSectionMap = Collections.unmodifiableMap(localSectionMap);
            }
            else {
                this.localSectionMap = this.sectionMap;
            }
            if (prevSectionMap != null) {
                Map<String, ModelScreenWidget> tempPrevSectionMap = new HashMap<String, ModelScreenWidget>();
                tempPrevSectionMap.putAll(prevSectionMap);
                this.prevSectionMap = Collections.unmodifiableMap(tempPrevSectionMap);
            }
            else {
                this.prevSectionMap = null;
            }
            this.prevSections = prevSections;
            this.sourceDecoratorScreen = sourceDecoratorScreen;
        }

        /** 
         * This is a lot like the ScreenRenderer class and returns an empty String so it can be used more easily with FreeMarker 
         * <p>
         * SCIPIO: supports asString bool, to render as string to result instead of default writer, logical default false
         * NOTE: this method has shareScope=true implied (no protection). See renderScoped for protection.
         */
        public String render(String sectionName, boolean asString) throws GeneralException, IOException {
            return render(sectionName, asString, true, null);
        }
        
        /** 
         * SCIPIO: supports asString bool, to render as string to result instead of default writer, logical default false
         * */
        protected String render(String sectionName, boolean asString, boolean shareScope, Map<String, ?> ctxVars) throws GeneralException, IOException {
            if (includePrevSections) {
                // SCIPIO: new handling for previous section support
                ModelScreenWidget section = localSectionMap.get(sectionName);
                // if no section by that name, write nothing
                if (section != null) {
                    Appendable effWriter = asString ? new java.io.StringWriter() : this.getWriter();
                    renderSection(effWriter, section, shareScope, ctxVars);
                    return asString ? effWriter.toString() : "";
                }
                else if (prevSections != null && prevSectionMap != null && prevSectionMap.get(sectionName) != null) {
                    // render previous sections with previous renderer so that it uses the right context
                    return prevSections.render(sectionName, asString, shareScope, ctxVars);
                }
                return "";
            }
            else {
                ModelScreenWidget section = sectionMap.get(sectionName);
                Appendable effWriter = asString ? new java.io.StringWriter() : this.getWriter();
                // if no section by that name, write nothing
                if (section != null) {
                    renderSection(effWriter, section, shareScope, ctxVars);
                }
                return asString ? effWriter.toString() : "";
            }
        }
        
        /**
         * SCIPIO: the inner call around renderWidgetString
         * 2017-04-26: this is refactored because the ctxVars put and the context push has to be done around
         * the innermost renderWidgetString call due to prevSections.
         */
        protected void renderSection(Appendable writer, ModelScreenWidget section, boolean shareScope, Map<String, ?> ctxVars) throws GeneralException, IOException {
            MapStack<String> context = this.getContext();
            if (shareScope) {
                if (ctxVars != null && !ctxVars.isEmpty()) {
                    context.putAll(ctxVars);
                }
                Object prevSections = context.get("scpCurrentSections");
                try {
                    // SCIPIO: 2017-05-16: we have to set this section renderer in the target context
                    // so that targeted rendering can pick it up
                    context.put("scpCurrentSections", this);
                    section.renderWidgetString(writer, context, this.screenStringRenderer);
                } finally {
                    if (prevSections != null) context.put("scpCurrentSections", prevSections);
                }
            } else {
                context.push();
                try {
                    if (ctxVars != null && !ctxVars.isEmpty()) {
                        context.putAll(ctxVars);
                    }
                    context.put("scpCurrentSections", this);
                    section.renderWidgetString(writer, context, this.screenStringRenderer);
                } finally {
                    context.pop();
                }
            }
        }
        
        /** 
         * This is a lot like the ScreenRenderer class and returns an empty String so it can be used more easily with FreeMarker 
         */
        public String render(String sectionName) throws GeneralException, IOException {
            return render(sectionName, false);
        }
        
        /** 
         * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE).
         * 
         * @param asString whether to render as string instead of saved writer; default: false
         * @param shareScope whether to share scope; default: false (for renderScoped only!)
         */
        public String renderScoped(String sectionName, Boolean asString, Boolean shareScope, Map<String, ?> ctxVars) throws GeneralException, IOException {
            return render(sectionName, asString != null ? asString : Boolean.FALSE, shareScope != null ? shareScope : Boolean.FALSE, ctxVars);
        }
        
        /** 
         * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE).
         */
        public String renderScoped(String sectionName, Boolean asString, Boolean shareScope) throws GeneralException, IOException {
            return renderScoped(sectionName, asString, shareScope, null);
        }
        
        /** 
         * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE),
         * generic object/ftl-friendly version.
         */
        public String renderScopedGen(String sectionName, Object asString, Object shareScope, Map<String, ?> ctxVars) throws GeneralException, IOException {
            return renderScoped(sectionName, UtilMisc.booleanValue(asString), UtilMisc.booleanValue(shareScope), ctxVars);
        }
        
        /** 
         * SCIPIO: version which scopes by default by pushing context stack (shareScope FALSE),
         * generic object/ftl-friendly version.
         */
        public String renderScopedGen(String sectionName, Object asString, Object shareScope) throws GeneralException, IOException {
            return renderScoped(sectionName, UtilMisc.booleanValue(asString), UtilMisc.booleanValue(shareScope), null);
        }

        public Appendable getWriter() { // SCIPIO
            return contextFetcher.getWriter();
        }
        
        public MapStack<String> getContext() { // SCIPIO
            return contextFetcher.getContext();
        }

        public ModelScreenWidget getSourceDecoratorScreen() { // SCIPIO
            return sourceDecoratorScreen;
        }

        @Override
        public int size() {
            return sectionMap.size();
        }

        @Override
        public boolean isEmpty() {
            return sectionMap.isEmpty();
        }

        @Override
        public boolean containsKey(Object key) {
            return sectionMap.containsKey(key);
        }

        @Override
        public boolean containsValue(Object value) {
            return sectionMap.containsValue(value);
        }

        @Override
        public ModelScreenWidget get(Object key) {
            return sectionMap.get(key);
        }

        @Override
        public ModelScreenWidget put(String key, ModelScreenWidget value) {
            return sectionMap.put(key, value);
        }

        @Override
        public ModelScreenWidget remove(Object key) {
            return sectionMap.remove(key);
        }

        @Override
        public void clear() {
            sectionMap.clear();
        }

        @Override
        public Set<String> keySet() {
            return sectionMap.keySet();
        }

        @Override
        public Collection<ModelScreenWidget> values() {
            return sectionMap.values();
        }

        @Override
        public Set<java.util.Map.Entry<String, ModelScreenWidget>> entrySet() {
            return sectionMap.entrySet();
        }

        @Override
        public boolean equals(Object o) {
            return sectionMap.equals(o);
        }

        @Override
        public int hashCode() {
            return sectionMap.hashCode();
        }

        @Override
        public void putAll(Map<? extends String, ? extends ModelScreenWidget> m) {
            sectionMap.putAll(m);
        }
    }

    public static final class Section extends ModelScreenWidget {
        public static final String TAG_NAME = "section";
        private final ModelCondition condition;
        private final List<ModelAction> actions;
        private final List<ModelScreenWidget> subWidgets;
        private final List<ModelScreenWidget> failWidgets;
        private final boolean isMainSection;
        private final FlexibleStringExpander shareScopeExdr;
        private final boolean actionsOnly; // SCIPIO: extra flag hint

        public Section(ModelScreen modelScreen, Element sectionElement) {
            this(modelScreen, sectionElement, false);
        }
        
        public Section(ModelScreen modelScreen, Element sectionElement, boolean isMainSection) {
            super(modelScreen, sectionElement);

            boolean hasActionsElement = false;
            boolean hasWidgetsElement = false;
            boolean hasFailWidgetsElement = false;
            
            // SCIPIO: SHORTHANDS: this code now support having an actions or widgets element in place of section.
            // this is remarkable easy!
            if ("actions".equals(sectionElement.getTagName())) {
                this.condition = null;
                this.actions = AbstractModelAction.readSubActions(modelScreen, sectionElement);
                this.subWidgets = Collections.emptyList();
                this.failWidgets = Collections.emptyList();
                hasActionsElement = true;
            } else if ("widgets".equals(sectionElement.getTagName())) {
                this.condition = null;
                this.actions = Collections.emptyList();
                List<? extends Element> subElementList = UtilXml.childElementList(sectionElement);
                this.subWidgets = ModelScreenWidget.readSubWidgets(getModelScreen(), subElementList);
                this.failWidgets = Collections.emptyList();
                hasWidgetsElement = true;
            } else { // SCIPIO: default/stock case: ("section".equals(sectionElement.getTagName()))
                // read condition under the "condition" element
                Element conditionElement = UtilXml.firstChildElement(sectionElement, "condition");
                if (conditionElement != null) {
                    conditionElement = UtilXml.firstChildElement(conditionElement);
                    this.condition = ModelScreenCondition.SCREEN_CONDITION_FACTORY.newInstance(modelScreen, conditionElement);
                } else {
                    this.condition = null;
                }
    
                // read all actions under the "actions" element
                Element actionsElement = UtilXml.firstChildElement(sectionElement, "actions");
                if (actionsElement != null) {
                    this.actions = AbstractModelAction.readSubActions(modelScreen, actionsElement);
                    hasActionsElement = true;
                } else {
                    this.actions = Collections.emptyList();
                }
    
                // read sub-widgets
                Element widgetsElement = UtilXml.firstChildElement(sectionElement, "widgets");
                if (widgetsElement != null) {
                    List<? extends Element> subElementList = UtilXml.childElementList(widgetsElement);
                    this.subWidgets = ModelScreenWidget.readSubWidgets(getModelScreen(), subElementList);
                    hasWidgetsElement = true;
                } else {
                    this.subWidgets = Collections.emptyList();
                }
    
                // read fail-widgets
                Element failWidgetsElement = UtilXml.firstChildElement(sectionElement, "fail-widgets");
                if (failWidgetsElement != null) {
                    List<? extends Element> failElementList = UtilXml.childElementList(failWidgetsElement);
                    this.failWidgets = ModelScreenWidget.readSubWidgets(getModelScreen(), failElementList);
                    hasFailWidgetsElement = true;
                } else {
                    this.failWidgets = Collections.emptyList();
                }
            }
            this.isMainSection = isMainSection;
            this.shareScopeExdr = FlexibleStringExpander.getInstance(sectionElement.getAttribute("share-scope"));
            
            // SCIPIO: warn about this case, which should basically be considered an error
            if (condition != null && hasActionsElement && !hasWidgetsElement && !hasFailWidgetsElement) {
                Debug.logWarning("screen [" + modelScreen.getSourceLocation() + "#" + modelScreen.getName() + "] " +
                        "contains a section condition, with actions, but without widgets or fail-widgets element; actions-only " +
                        "screens do not support the section condition element and it will be bypassed by directives such as " +
                        "include-screen-actions - use the actions master if directive instead (see widget-common.xsd)", module);
            }

            // SCIPIO: determine if actions-only
            this.actionsOnly = hasActionsElement && condition == null && !hasFailWidgetsElement && 
                    UtilValidate.isEmpty(subWidgets) && UtilValidate.isEmpty(failWidgets) &&
                    !(UtilValidate.isEmpty(this.actions) && hasWidgetsElement); // NOTE: the last is special case, ambiguous, but bias toward widgets
        }

        /**
         * SCIPIO: Returns true if this section only contains actions directives.
         */
        public boolean isActionsOnly() {
            return actionsOnly;
        }
        
        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
            // SCIPIO: share-scope
            boolean protectScope = !shareScope(context);
            if (protectScope) {
                if (!(context instanceof MapStack<?>)) {
                    context = MapStack.create(context);
                }
                UtilGenerics.<MapStack<String>>cast(context).push();
            }
            
            // check the condition, if there is one
            boolean condTrue = true;
            if (this.condition != null) {
                if (!this.condition.eval(context)) {
                    condTrue = false;
                }
            }

            // if condition does not exist or evals to true run actions and render widgets, otherwise render fail-widgets
            if (condTrue) {
                // run the actions only if true
                AbstractModelAction.runSubActions(this.actions, context);

                try {
                    // section by definition do not themselves do anything, so this method will generally do nothing, but we'll call it anyway
                    screenStringRenderer.renderSectionBegin(writer, context, this);

                    // render sub-widgets
                    renderSubWidgetsString(this.subWidgets, writer, context, screenStringRenderer);

                    screenStringRenderer.renderSectionEnd(writer, context, this);
                } catch (IOException e) {
                    String errMsg = "Error rendering widgets section [" + getName() + "] in screen named [" + getModelScreen().getName() + "]: " + e.toString();
                    Debug.logError(e, errMsg, module);
                    throw new RuntimeException(errMsg);
                }
            } else {
                try {
                    // section by definition do not themselves do anything, so this method will generally do nothing, but we'll call it anyway
                    screenStringRenderer.renderSectionBegin(writer, context, this);

                    // render sub-widgets
                    renderSubWidgetsString(this.failWidgets, writer, context, screenStringRenderer);

                    screenStringRenderer.renderSectionEnd(writer, context, this);
                } catch (IOException e) {
                    String errMsg = "Error rendering fail-widgets section [" + this.getName() + "] in screen named [" + getModelScreen().getName() + "]: " + e.toString();
                    Debug.logError(e, errMsg, module);
                    throw new RuntimeException(errMsg);
                }
            }
            
            // SCIPIO: share-scope
            if (protectScope) {
                UtilGenerics.<MapStack<String>>cast(context).pop();
            }
        }

        @Override
        public String getBoundaryCommentName() {
            if (isMainSection) {
                return getModelScreen().getSourceLocation() + "#" + getModelScreen().getName();
            } else {
                return getName();
            }
        }

        public List<ModelAction> getActions() {
            return actions;
        }

        public List<ModelScreenWidget> getSubWidgets() {
            return subWidgets;
        }

        public List<ModelScreenWidget> getFailWidgets() {
            return failWidgets;
        }

        public boolean isMainSection() {
            return isMainSection;
        }

        public ModelCondition getCondition() {
            return condition;
        }
        
        public boolean shareScope(Map<String, Object> context) {
            String shareScopeString = this.shareScopeExdr.expandString(context);
            // defaults to true, so anything but false is true
            return !"false".equals(shareScopeString);
        }

        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class ColumnContainer extends ModelScreenWidget {
        public static final String TAG_NAME = "column-container";
        private final FlexibleStringExpander idExdr;
        private final FlexibleStringExpander styleExdr;
        private final List<Column> columns;

        public ColumnContainer(ModelScreen modelScreen, Element containerElement) {
            super(modelScreen, containerElement);
            this.idExdr = FlexibleStringExpander.getInstance(containerElement.getAttribute("id"));
            this.styleExdr = FlexibleStringExpander.getInstance(containerElement.getAttribute("style"));
            List<? extends Element> subElementList = UtilXml.childElementList(containerElement, "column");
            List<Column> columns = new ArrayList<Column>(subElementList.size());
            for (Element element : subElementList) {
                columns.add(new Column(modelScreen, element));
            }
            this.columns = Collections.unmodifiableList(columns);
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
            try {
                screenStringRenderer.renderColumnContainer(writer, context, this);
            } catch (IOException e) {
                String errMsg = "Error rendering container in screen named [" + getModelScreen().getName() + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            }
        }

        public List<Column> getColumns() {
            return this.columns;
        }

        public String getId(Map<String, Object> context) {
            return this.idExdr.expandString(context);
        }

        public String getStyle(Map<String, Object> context) {
            return this.styleExdr.expandString(context);
        }

        public FlexibleStringExpander getIdExdr() {
            return idExdr;
        }

        public FlexibleStringExpander getStyleExdr() {
            return styleExdr;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class Column extends ModelWidget {
        public static final String TAG_NAME = "column"; // SCIPIO: added for consistency, but NOTE: not in xsd
        
        private final FlexibleStringExpander idExdr;
        private final FlexibleStringExpander styleExdr;
        private final List<ModelScreenWidget> subWidgets;
        private final ModelScreen modelScreen;

        public Column(ModelScreen modelScreen, Element columnElement) {
            super(columnElement);
            this.idExdr = FlexibleStringExpander.getInstance(columnElement.getAttribute("id"));
            this.styleExdr = FlexibleStringExpander.getInstance(columnElement.getAttribute("style"));
            List<? extends Element> subElementList = UtilXml.childElementList(columnElement);
            this.subWidgets = Collections.unmodifiableList(readSubWidgets(modelScreen, subElementList));
            this.modelScreen = modelScreen;
        }
        
        public List<ModelScreenWidget> getSubWidgets() {
            return this.subWidgets;
        }

        public String getId(Map<String, Object> context) {
            return this.idExdr.expandString(context);
        }

        public String getStyle(Map<String, Object> context) {
            return this.styleExdr.expandString(context);
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getIdExdr() {
            return idExdr;
        }

        public FlexibleStringExpander getStyleExdr() {
            return styleExdr;
        }
        
        @Override
        public String getWidgetType() { // SCIPIO: new
            return TAG_NAME;
        }
        
        @Override
        public String getContainerLocation() { // SCIPIO: new
            return modelScreen != null ? modelScreen.getFullLocationAndName() : null;
        }
    }

    public static final class Container extends ModelScreenWidget implements FlexibleIdAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "container";
        private final FlexibleStringExpander idExdr;
        private final FlexibleStringExpander styleExdr;
        private final FlexibleStringExpander autoUpdateTargetExdr;
        private final FlexibleStringExpander autoUpdateInterval;
        private final List<ModelScreenWidget> subWidgets;

        public Container(ModelScreen modelScreen, Element containerElement) {
            super(modelScreen, containerElement);
            this.idExdr = FlexibleStringExpander.getInstance(containerElement.getAttribute("id"));
            this.styleExdr = FlexibleStringExpander.getInstance(containerElement.getAttribute("style"));
            this.autoUpdateTargetExdr = FlexibleStringExpander.getInstance(containerElement.getAttribute("auto-update-target"));
            String autoUpdateInterval = containerElement.getAttribute("auto-update-interval");
            if (autoUpdateInterval.isEmpty()) {
                autoUpdateInterval = "2";
            }
            this.autoUpdateInterval = FlexibleStringExpander.getInstance(autoUpdateInterval);
            // read sub-widgets
            List<? extends Element> subElementList = UtilXml.childElementList(containerElement);
            this.subWidgets = ModelScreenWidget.readSubWidgets(getModelScreen(), subElementList);
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
            try {
                screenStringRenderer.renderContainerBegin(writer, context, this);

                // render sub-widgets
                renderSubWidgetsString(this.subWidgets, writer, context, screenStringRenderer);

                screenStringRenderer.renderContainerEnd(writer, context, this);
            } catch (IOException e) {
                String errMsg = "Error rendering container in screen named [" + getModelScreen().getName() + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            }
        }

        public String getId(Map<String, Object> context) {
            return this.idExdr.expandString(context);
        }

        public String getStyle(Map<String, Object> context) {
            return this.styleExdr.expandString(context);
        }

        public String getAutoUpdateTargetExdr(Map<String, Object> context) {
            return this.autoUpdateTargetExdr.expandString(context);
        }

        public String getAutoUpdateInterval(Map<String, Object> context) {
            return this.autoUpdateInterval.expandString(context);
        }

        public List<ModelScreenWidget> getSubWidgets() {
            return subWidgets;
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getIdExdr() {
            return idExdr;
        }

        public FlexibleStringExpander getStyleExdr() {
            return styleExdr;
        }

        public FlexibleStringExpander getAutoUpdateTargetExdr() {
            return autoUpdateTargetExdr;
        }

        public FlexibleStringExpander getAutoUpdateInterval() {
            return autoUpdateInterval;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class Screenlet extends ModelScreenWidget implements FlexibleIdAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "screenlet";
        private final FlexibleStringExpander idExdr;
        private final FlexibleStringExpander titleExdr;
        private final FlexibleStringExpander titleStyleExdr;
        private final Menu navigationMenu;
        private final Menu tabMenu;
        private final Form navigationForm;
        private final boolean collapsible;
        private final FlexibleStringExpander initiallyCollapsed;
        private final boolean saveCollapsed;
        private final boolean padded;
        private final List<ModelScreenWidget> subWidgets;

        public Screenlet(ModelScreen modelScreen, Element screenletElement) {
            super(modelScreen, screenletElement);
            this.idExdr = FlexibleStringExpander.getInstance(screenletElement.getAttribute("id"));
            boolean collapsible = "true".equals(screenletElement.getAttribute("collapsible"));
            this.initiallyCollapsed = FlexibleStringExpander.getInstance(screenletElement.getAttribute("initially-collapsed"));
            if ("true".equals(this.initiallyCollapsed.getOriginal())) {
                collapsible = true;
            }
            this.collapsible = collapsible;
            // By default, for a collapsible screenlet, the collapsed/expanded status must be saved
            this.saveCollapsed = !("false".equals(screenletElement.getAttribute("save-collapsed")));

            boolean padded = !"false".equals(screenletElement.getAttribute("padded"));
            if (this.collapsible && getName().isEmpty() && idExdr.isEmpty()) {
                throw new IllegalArgumentException("Collapsible screenlets must have a name or id [" + getModelScreen().getName() + "]");
            }
            this.titleExdr = FlexibleStringExpander.getInstance(screenletElement.getAttribute("title"));
            this.titleStyleExdr = FlexibleStringExpander.getInstance(screenletElement.getAttribute("title-style"));
            List<? extends Element> subElementList = UtilXml.childElementList(screenletElement);
            // Make a copy of the unmodifiable List so we can modify it.
            List<ModelScreenWidget> subWidgets = new ArrayList<ModelScreenWidget>(ModelScreenWidget.readSubWidgets(getModelScreen(), subElementList));
            Menu navigationMenu = null;
            String navMenuName = screenletElement.getAttribute("navigation-menu-name");
            if (!navMenuName.isEmpty()) {
                for (ModelWidget subWidget : subWidgets) {
                    if (navMenuName.equals(subWidget.getName()) && subWidget instanceof Menu) {
                        navigationMenu = (Menu) subWidget;
                        subWidgets.remove(subWidget);
                        break;
                    }
                }
            }
            this.navigationMenu = navigationMenu;
            Menu tabMenu = null;
            String tabMenuName = screenletElement.getAttribute("tab-menu-name");
            if (!tabMenuName.isEmpty()) {
                for (ModelWidget subWidget : subWidgets) {
                    if (tabMenuName.equals(subWidget.getName()) && subWidget instanceof Menu) {
                        tabMenu = (Menu) subWidget;
                        subWidgets.remove(subWidget);
                        break;
                    }
                }
            }
            this.tabMenu = tabMenu;
            Form navigationForm = null;
            String formName = screenletElement.getAttribute("navigation-form-name");
            if (!formName.isEmpty() && this.navigationMenu == null) {
                for (ModelWidget subWidget : subWidgets) {
                    if (formName.equals(subWidget.getName()) && subWidget instanceof Form) {
                        navigationForm = (Form) subWidget;
                        padded = false;
                        break;
                    }
                }
            }
            this.subWidgets = Collections.unmodifiableList(subWidgets);
            this.navigationForm = navigationForm;
            this.padded = padded;
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
            boolean collapsed = getInitiallyCollapsed(context);
            if (this.collapsible) {
                String preferenceKey = getPreferenceKey(context) + "_collapsed";
                Map<String, Object> requestParameters = UtilGenerics.checkMap(context.get("requestParameters"));
                if (requestParameters != null) {
                    String collapsedParam = (String) requestParameters.get(preferenceKey);
                    if (collapsedParam != null)
                        collapsed = "true".equals(collapsedParam);
                }
            }
            try {
                screenStringRenderer.renderScreenletBegin(writer, context, collapsed, this);
                for (ModelScreenWidget subWidget : this.subWidgets) {
                    screenStringRenderer.renderScreenletSubWidget(writer, context, subWidget, this);
                }
                screenStringRenderer.renderScreenletEnd(writer, context, this);
            } catch (IOException e) {
                String errMsg = "Error rendering screenlet in screen named [" + getModelScreen().getName() + "]: ";
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg + e);
            }
        }

        public boolean collapsible() {
            return this.collapsible;
        }

        //initially-collapsed status, which may be overriden by user preference
        public boolean getInitiallyCollapsed(Map<String, Object> context) {
            String screenletId = this.getId(context) + "_collapsed";
            Map<String, ? extends Object> userPreferences = UtilGenerics.checkMap(context.get("userPreferences"));
            if (userPreferences != null && userPreferences.containsKey(screenletId)) {
                return Boolean.valueOf((String)userPreferences.get(screenletId)).booleanValue() ;
            }

            return "true".equals(this.initiallyCollapsed.expand(context));
        }

        public boolean saveCollapsed() {
            return this.saveCollapsed;
        }
        public boolean padded() {
            return this.padded;
        }

        public String getPreferenceKey(Map<String, Object> context) {
            String name = this.idExdr.expandString(context);
            if (name.isEmpty()) {
                name = "screenlet" + "_" + Integer.toHexString(hashCode());
            }
            return name;
        }

        public String getId(Map<String, Object> context) {
            return this.idExdr.expandString(context);
        }

        public List<ModelScreenWidget> getSubWidgets() {
            return subWidgets;
        }

        public String getTitle(Map<String, Object> context) {
            String title = this.titleExdr.expandString(context);
            title = WidgetWorker.getEarlyEncoder(context).encode(title); // SCIPIO: simplified
            return title;
        }
        
        public String getTitleStyle(Map<String, Object> context) {
            return this.titleStyleExdr.expandString(context);
        }

        public Menu getNavigationMenu() {
            return this.navigationMenu;
        }

        public Form getNavigationForm() {
            return this.navigationForm;
        }

        public Menu getTabMenu() {
            return this.tabMenu;
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getIdExdr() {
            return idExdr;
        }

        public FlexibleStringExpander getTitleExdr() {
            return titleExdr;
        }

        public boolean getCollapsible() {
            return collapsible;
        }

        public FlexibleStringExpander getInitiallyCollapsed() {
            return initiallyCollapsed;
        }

        public boolean getSaveCollapsed() {
            return saveCollapsed;
        }

        public boolean getPadded() {
            return padded;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class HorizontalSeparator extends ModelScreenWidget implements FlexibleIdAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "horizontal-separator";
        private final FlexibleStringExpander idExdr;
        private final FlexibleStringExpander styleExdr;

        public HorizontalSeparator(ModelScreen modelScreen, Element separatorElement) {
            super(modelScreen, separatorElement);
            this.idExdr = FlexibleStringExpander.getInstance(separatorElement.getAttribute("id"));
            this.styleExdr = FlexibleStringExpander.getInstance(separatorElement.getAttribute("style"));
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
            screenStringRenderer.renderHorizontalSeparator(writer, context, this);
        }

        public String getId(Map<String, Object> context) {
            return this.idExdr.expandString(context);
        }

        public String getStyle(Map<String, Object> context) {
            return this.styleExdr.expandString(context);
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getIdExdr() {
            return idExdr;
        }

        public FlexibleStringExpander getStyleExdr() {
            return styleExdr;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    /**
     * SCIPIO: Builds actions list
     */
    static List<ModelAction> readActions(ModelWidget modelWidget, Element parentElement, String childName) {
        // read all actions under the "actions" element
        if (childName != null) {
            parentElement = UtilXml.firstChildElement(parentElement, childName);
        }
        if (parentElement != null) {
            return AbstractModelAction.readSubActions(modelWidget, parentElement);
        } else {
            return null; //Collections.emptyList();
        }
    }
    
    public static final class IncludeScreen extends ModelScreenWidget implements FlexibleNameAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "include-screen";
        private final FlexibleStringExpander nameExdr;
        private final FlexibleStringExpander locationExdr;
        private final FlexibleStringExpander shareScopeExdr;
        private final List<ModelAction> actions; // SCIPIO: 2017-05-01: new post-context-stack-push actions

        public IncludeScreen(ModelScreen modelScreen, Element includeScreenElement) {
            super(modelScreen, includeScreenElement);
            this.nameExdr = FlexibleStringExpander.getInstance(includeScreenElement.getAttribute("name"));
            this.locationExdr = FlexibleStringExpander.getInstance(includeScreenElement.getAttribute("location"));
            this.shareScopeExdr = FlexibleStringExpander.getInstance(includeScreenElement.getAttribute("share-scope"));
            this.actions = readActions(modelScreen, includeScreenElement, "actions");
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
            // if we are not sharing the scope, protect it using the MapStack
            boolean protectScope = !shareScope(context);
            if (protectScope) {
                if (!(context instanceof MapStack<?>)) {
                    context = MapStack.create(context);
                }
                int renderSeqNumber = (Integer) (context.get("renderSeqNumber") != null ? context.get("renderSeqNumber") : 0);
                renderSeqNumber++;
                context.put("renderSeqNumber", renderSeqNumber);
                
                UtilGenerics.<MapStack<String>>cast(context).push();
                

                // build the widgetpath
                List<String> widgetTrail = UtilGenerics.toList(context.get("_WIDGETTRAIL_"));
                if (widgetTrail == null) {
                    widgetTrail = new LinkedList<String>();
                }

                String thisName = nameExdr.expandString(context);
                widgetTrail.add(thisName);
                context.put("_WIDGETTRAIL_", widgetTrail);
            }
            
            AbstractModelAction.runSubActions(this.actions, context); // SCIPIO: 2017-05-01: new post-context-stack-push actions

            // don't need the renderer here, will just pass this on down to another screen call; screenStringRenderer.renderContainerBegin(writer, context, this);
            String name = this.getName(context);
            String location = this.getLocation(context);

            if (name.isEmpty()) {
                if (Debug.verboseOn()) Debug.logVerbose("In the include-screen tag the screen name was empty, ignoring include; in screen [" + getModelScreen().getName() + "]", module);
                return;
            }

            ScreenFactory.renderReferencedScreen(name, location, this, writer, context, screenStringRenderer);

            if (protectScope) {
                UtilGenerics.<MapStack<String>>cast(context).pop();
            }
        }

        public String getName(Map<String, Object> context) {
            return this.nameExdr.expandString(context);
        }

        public String getLocation(Map<String, Object> context) {
            return this.locationExdr.expandString(context);
        }

        public boolean shareScope(Map<String, Object> context) {
            String shareScopeString = this.shareScopeExdr.expandString(context);
            // defaults to false, so anything but true is false
            return "true".equals(shareScopeString);
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getNameExdr() {
            return nameExdr;
        }

        public FlexibleStringExpander getLocationExdr() {
            return locationExdr;
        }

        public FlexibleStringExpander getShareScopeExdr() {
            return shareScopeExdr;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class DecoratorScreen extends ModelScreenWidget implements FlexibleNameAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "decorator-screen";
        private final FlexibleStringExpander nameExdr;
        private final FlexibleStringExpander locationExdr;
        private final Map<String, ModelScreenWidget> sectionMap;
        
        // SCIPIO: fallback decorator
        private final FlexibleScreenFallbackSettings fallbackSettings;
        
        // SCIPIO: FIXME: Terrible ThreadLocal-based hack to propagate default values from screen-group
        // NOTE: the initial value for this is NULL and must be set back to NULL before the construction is over (use a stack-like idiom).
        private static ThreadLocal<FlexibleScreenFallbackSettings> overridingDefaultFallbackSettings = new ThreadLocal<>();

        private static final FlexibleScreenFallbackSettings defaultFallbackSettings = 
                new SimpleFlexibleScreenFallbackSettings("", "", Boolean.FALSE);
               
        
        // SCIPIO: if true, automatically include sections defined in higher screens
        private final boolean autoDecoratorSectionIncludes;

        public DecoratorScreen(ModelScreen modelScreen, Element decoratorScreenElement) {
            super(modelScreen, decoratorScreenElement);
            this.nameExdr = FlexibleStringExpander.getInstance(decoratorScreenElement.getAttribute("name"));
            this.locationExdr = FlexibleStringExpander.getInstance(decoratorScreenElement.getAttribute("location"));
            Map<String, ModelScreenWidget> sectionMap = new HashMap<String, ModelScreenWidget>();
            List<? extends Element> decoratorSectionElementList = UtilXml.childElementList(decoratorScreenElement, "decorator-section");
            for (Element decoratorSectionElement: decoratorSectionElementList) {
                DecoratorSection decoratorSection = new DecoratorSection(modelScreen, decoratorSectionElement);
                sectionMap.put(decoratorSection.getName(), decoratorSection);
            }
            this.sectionMap = Collections.unmodifiableMap(sectionMap);
            
            FlexibleScreenFallbackSettings defFallbackSettings = getOverridingDefaultFallbackSettings();
            if (defFallbackSettings == null) {
                defFallbackSettings = DecoratorScreen.defaultFallbackSettings;
            }
            this.fallbackSettings = new SimpleFlexibleScreenFallbackSettings(defFallbackSettings, 
                FlexibleStringExpander.getInstance(decoratorScreenElement.getAttribute("fallback-name")),
                FlexibleStringExpander.getInstance(decoratorScreenElement.getAttribute("fallback-location")),
                UtilMisc.booleanValue(decoratorScreenElement.getAttribute("fallback-if-empty"))
                );
            
            this.autoDecoratorSectionIncludes = !"false".equals(decoratorScreenElement.getAttribute("auto-decorator-section-include"));
        }

        @Override
        @SuppressWarnings("unchecked")
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {

            SectionsRenderer prevSections = (SectionsRenderer) context.get("sections");
            // SCIPIO: filter the sections to render by the new use-when condition and overrides
            Map<String, ModelScreenWidget> filteredSectionMap = new HashMap<String, ModelScreenWidget>();
            for(Map.Entry<String, ModelScreenWidget> entry : this.sectionMap.entrySet()) {
                ModelScreenWidget section = entry.getValue();
                if (section != null && section instanceof DecoratorSection) {
                    DecoratorSection decSection = (DecoratorSection) section;
                    if (decSection.shouldUse(context)) {
                        // don't include if allowed to be overridden and auto-include is incoming
                        ModelScreenWidget prevSection = (prevSections != null) ? prevSections.get(decSection.getName()) : null;
                        if (!(decSection.isOverrideByAutoInclude() && prevSection != null && prevSection instanceof DecoratorSection)) {
                            filteredSectionMap.put(entry.getKey(), section);
                        }
                    }
                }
            }
            filteredSectionMap = Collections.unmodifiableMap(filteredSectionMap);
            
            // SCIPIO: get previous sections renderer and include if auto-decorator-section-include enabled
            // Must not recognize any sections from prev for which this decorator-screen already had a decorator-section in xml
            Map<String, ModelScreenWidget> filteredPrevSectionMap = new HashMap<String, ModelScreenWidget>();
            if (prevSections != null) {
                for(Map.Entry<String, ModelScreenWidget> entry : prevSections.entrySet()) {
                    ModelScreenWidget section = entry.getValue();
                    if (section != null && section instanceof DecoratorSection) {
                        String name = entry.getKey();
                        ModelScreenWidget defSection = this.sectionMap.get(name);
                        // autoDecoratorSectionIncludes only adds auto-includes for sections not already defined, statically 
                        // use sectionMap here, not filteredSectionMap, so that use-when doesn't permit auto-includes when it evaluates to false...
                        if (this.autoDecoratorSectionIncludes && defSection == null) {
                            filteredPrevSectionMap.put(name, section);
                        }
                        else {
                            // ... unless use fallback or override is set
                            // fallback and override also work when autoDecoratorSectionIncludes disabled
                            // difference is override filters filteredSectionMap so will always fallback
                            if (defSection != null && defSection instanceof DecoratorSection) {
                                DecoratorSection defDecSection = ((DecoratorSection) defSection);
                                if (defDecSection.isFallbackAutoInclude() || defDecSection.isOverrideByAutoInclude()) {
                                    filteredPrevSectionMap.put(name, section);
                                }
                            }
                        }
                    }
                }
            }
            filteredPrevSectionMap = Collections.unmodifiableMap(filteredPrevSectionMap);
            
            // isolate the scope
            if (!(context instanceof MapStack)) {
                context = MapStack.create(context);
            }

            MapStack contextMs = (MapStack) context;

            // create a standAloneStack, basically a "save point" for this SectionsRenderer, and make a new "screens" object just for it so it is isolated and doesn't follow the stack down
            MapStack standAloneStack = contextMs.standAloneChildStack();
            // SCIPIO: 2017-03-09: workaround for context fetching problems
            RenderContextFetcher contextFetcher = ScreenRenderer.makeEnvAwareContextFetcher(writer, standAloneStack);
            standAloneStack.put("screens", new ScreenRenderer(contextFetcher, screenStringRenderer));
            
            SectionsRenderer sections;
            if (!filteredPrevSectionMap.isEmpty()) {
                sections = new SectionsRenderer(filteredSectionMap, contextFetcher, screenStringRenderer, prevSections, filteredPrevSectionMap, true, this);
            }
            else {
                sections = new SectionsRenderer(filteredSectionMap, contextFetcher, screenStringRenderer, this);
            }
            
            // put the sectionMap in the context, make sure it is in the sub-scope, ie after calling push on the MapStack
            contextMs.push();
            context.put("sections", sections);

            String name = this.getName(context);
            String location = this.getLocation(context);
            
            // SCIPIO: fallback added
            ScreenFactory.renderReferencedScreen(name, location, this, writer, context, screenStringRenderer,
                    true, this.fallbackSettings.getResolvedForScreenLogic(context));

            contextMs.pop();
        }

        public String getName(Map<String, Object> context) {
            return this.nameExdr.expandString(context);
        }

        public String getLocation(Map<String, Object> context) {
            return this.locationExdr.expandString(context);
        }

        public Map<String, ModelScreenWidget> getSectionMap() {
            return sectionMap;
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getNameExdr() {
            return nameExdr;
        }

        public FlexibleStringExpander getLocationExdr() {
            return locationExdr;
        }

        // SCIPIO: new
        public FlexibleScreenFallbackSettings getFallbackLocation() {
            return fallbackSettings;
        }

        // SCIPIO: FIXME: Terrible ThreadLocal-based methods, used during construction only

        static FlexibleScreenFallbackSettings getOverridingDefaultFallbackSettings() {
            return overridingDefaultFallbackSettings.get();
        }

        static void setOverridingDefaultFallbackSettings(FlexibleScreenFallbackSettings defaultFallbackSettings) {
            DecoratorScreen.overridingDefaultFallbackSettings.set(defaultFallbackSettings);
        }

        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class DecoratorSection extends ModelScreenWidget {
        public static final String TAG_NAME = "decorator-section";
        private final List<ModelScreenWidget> subWidgets;
        
        // SCIPIO: feature: conditional section definitions
        private final FlexibleStringExpander useWhen;
        private final boolean fallbackAutoInclude;
        private final boolean overrideByAutoInclude;

        public DecoratorSection(ModelScreen modelScreen, Element decoratorSectionElement) {
            super(modelScreen, decoratorSectionElement);
            // read sub-widgets
            List<? extends Element> subElementList = UtilXml.childElementList(decoratorSectionElement);
            this.subWidgets = ModelScreenWidget.readSubWidgets(getModelScreen(), subElementList);
            this.useWhen = FlexibleStringExpander.getInstance(decoratorSectionElement.getAttribute("use-when"));
            this.fallbackAutoInclude = "true".equals(decoratorSectionElement.getAttribute("fallback-auto-include"));
            this.overrideByAutoInclude = "true".equals(decoratorSectionElement.getAttribute("override-by-auto-include"));
        }
        
        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
            // render sub-widgets
            renderSubWidgetsString(this.subWidgets, writer, context, screenStringRenderer);
        }

        public List<ModelScreenWidget> getSubWidgets() {
            return subWidgets;
        }

        public FlexibleStringExpander getUseWhen() {
            return useWhen;
        }
        
        public boolean isFallbackAutoInclude() {
            return fallbackAutoInclude;  
        }
        
        public boolean isOverrideByAutoInclude() {
            return overrideByAutoInclude;
        }
        
        /**
         * SCIPIO: Returns true if this section should be used as-is,
         * based on the decorator-section use-when minilang/EL/bsh/groovy-style condition
         * and current screen context.
         * <p>
         * Based on {@link ModelFormField#shouldUse(Map)} but converted
         * to support more than just the bsh interpreter.
         * <p>
         * Scipio addition.
         */
        public boolean shouldUse(Map<String, Object> context) {
            if (this.getUseWhen() == null || UtilValidate.isEmpty(this.getUseWhen().toString())) {
                // no use-when
                return true;
            }
            
            boolean condTrue;
            Object retVal;
            
            try {
                retVal = this.getUseWhen().expand(context);
            }
            catch(Exception e) {
                String errMsg = "Error evaluating use-when condition [" + this.getUseWhen().toString() + "] on the "
                        + "decoration-section " + this.getName() + " of screen " 
                        + this.getModelScreen().getSourceLocation() + "#" + this.getModelScreen().getName()
                        + ": " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new IllegalArgumentException(errMsg, e);
            }
            
            if (retVal instanceof Boolean) {
                Boolean boolVal = (Boolean) retVal;
                condTrue = boolVal.booleanValue();
            } 
            else if ("true".equals(retVal)) {
                condTrue = true;
            }
            else if ("false".equals(retVal)) {
                condTrue = false;
            } else {
                throw new IllegalArgumentException("Return value from use-when condition eval was not a boolean: "
                        + "[" + this.getUseWhen().toString() + "] on the "
                        + "decoration-section " + this.getName() + " of screen " 
                        + this.getModelScreen().getSourceLocation() + "#" + this.getModelScreen().getName());
            }
            return condTrue;
        }
        
        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class DecoratorSectionInclude extends ModelScreenWidget {
        public static final String TAG_NAME = "decorator-section-include";

        public DecoratorSectionInclude(ModelScreen modelScreen, Element decoratorSectionElement) {
            super(modelScreen, decoratorSectionElement);
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
            Map<String, ? extends Object> preRenderedContent = UtilGenerics.checkMap(context.get("preRenderedContent"));
            if (!(context instanceof MapStack<?>)) {
                context = MapStack.create(context);
            }
            int renderSeqNumber = (Integer) (context.get("renderSeqNumber") != null ? context.get("renderSeqNumber") : 0);
            renderSeqNumber++;
            context.put("renderSeqNumber", renderSeqNumber);
            
            UtilGenerics.<MapStack<String>>cast(context).push();
            if (preRenderedContent != null && preRenderedContent.containsKey(getName())) {
                try {
                    writer.append((String) preRenderedContent.get(getName()));
                } catch (IOException e) {
                    String errMsg = "Error rendering pre-rendered content in screen named [" + getModelScreen().getName() + "]: " + e.toString();
                    Debug.logError(e, errMsg, module);
                    throw new RuntimeException(errMsg);
                }
            } else {
                SectionsRenderer sections = (SectionsRenderer) context.get("sections");
                // for now if sections is null, just log a warning; may be permissible to make the screen for flexible
                if (sections == null) {
                    Debug.logWarning("In decorator-section-include could not find sections object in the context, not rendering section with name [" + getName() + "]", module);
                } else {
                    sections.render(getName());
                }
            }
            UtilGenerics.<MapStack<String>>cast(context).pop();
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class Label extends ModelScreenWidget implements FlexibleIdAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "label";
        private final FlexibleStringExpander textExdr;
        private final FlexibleStringExpander idExdr;
        private final FlexibleStringExpander styleExdr;

        public Label(ModelScreen modelScreen, Element labelElement) {
            super(modelScreen, labelElement);

            // put the text attribute first, then the pcdata under the element, if both are there of course
            String textAttr = labelElement.getAttribute("text");
            String pcdata = UtilXml.elementValue(labelElement);
            if (pcdata == null) {
                pcdata = "";
            }
            this.textExdr = FlexibleStringExpander.getInstance(textAttr + pcdata);

            this.idExdr = FlexibleStringExpander.getInstance(labelElement.getAttribute("id"));
            this.styleExdr = FlexibleStringExpander.getInstance(labelElement.getAttribute("style"));
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) {
            try {
                screenStringRenderer.renderLabel(writer, context, this);
            } catch (IOException e) {
                String errMsg = "Error rendering label in screen named [" + getModelScreen().getName() + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            }
        }

        public String getText(Map<String, Object> context) {
            String text = this.textExdr.expandString(context);
            // FIXME: Encoding should be done by the renderer, not by the model.
            text = WidgetWorker.getEarlyEncoder(context).encode(text); // SCIPIO: simplified
            return text;
        }

        public String getId(Map<String, Object> context) {
            return this.idExdr.expandString(context);
        }

        public String getStyle(Map<String, Object> context) {
            return this.styleExdr.expandString(context);
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getTextExdr() {
            return textExdr;
        }

        public FlexibleStringExpander getIdExdr() {
            return idExdr;
        }

        public FlexibleStringExpander getStyleExdr() {
            return styleExdr;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class Form extends ModelScreenWidget implements FlexibleNameAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "include-form";
        private final FlexibleStringExpander nameExdr;
        private final FlexibleStringExpander locationExdr;
        private final FlexibleStringExpander shareScopeExdr;
        private final List<ModelAction> actions; // SCIPIO: 2017-05-01: new post-context-stack-push actions

        public Form(ModelScreen modelScreen, Element formElement) {
            super(modelScreen, formElement);
            this.nameExdr = FlexibleStringExpander.getInstance(formElement.getAttribute("name"));
            this.locationExdr = FlexibleStringExpander.getInstance(formElement.getAttribute("location"));
            this.shareScopeExdr = FlexibleStringExpander.getInstance(formElement.getAttribute("share-scope"));
            this.actions = readActions(modelScreen, formElement, "actions");
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) {
            // Output format might not support forms, so make form rendering optional.
            FormStringRenderer formStringRenderer = (FormStringRenderer) context.get("formStringRenderer");
            if (formStringRenderer == null) {
                if (Debug.verboseOn()) Debug.logVerbose("FormStringRenderer instance not found in rendering context, form not rendered.", module);
                return;
            }
            boolean protectScope = !shareScope(context);
            if (protectScope) {
                if (!(context instanceof MapStack<?>)) {
                    context = MapStack.create(context);
                }
                UtilGenerics.<MapStack<String>>cast(context).push();
            }
            
            AbstractModelAction.runSubActions(this.actions, context); // SCIPIO: 2017-05-01: new post-context-stack-push actions

            try {
                ModelForm modelForm = getModelForm(context);
                // SCIPIO: targeted rendering applicability check.
                WidgetRenderTargetState renderTargetState = WidgetRenderTargetExpr.getRenderTargetState(context);
                WidgetRenderTargetState.ExecutionInfo execInfo = renderTargetState.handleShouldExecute(modelForm, writer, context, screenStringRenderer);
                if (!execInfo.shouldExecute()) {
                    return;
                }
                try {
                    FormRenderer renderer = new FormRenderer(modelForm, formStringRenderer);
                    renderer.render(writer, context);
                } finally {
                    execInfo.handleFinished(context); // SCIPIO: return logic
                }
            } catch (Exception e) {
                String errMsg = "Error rendering included form named [" + getName() + "] at location [" + this.getLocation(context) + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg + e);
            }

            if (protectScope) {
                UtilGenerics.<MapStack<String>>cast(context).pop();
            }
        }

        public ModelForm getModelForm(Map<String, Object> context) throws IOException, SAXException, ParserConfigurationException {
            String name = this.getName(context);
            String location = this.getLocation(context);
            return FormFactory.getFormFromLocation(location, name, getModelScreen().getDelegator(context).getModelReader(),
                    getModelScreen().getDispatcher(context).getDispatchContext());
        }

        public String getName(Map<String, Object> context) {
            return this.nameExdr.expandString(context);
        }

        public String getLocation() {
            return locationExdr.getOriginal();
        }

        public String getLocation(Map<String, Object> context) {
            return this.locationExdr.expandString(context);
        }

        public boolean shareScope(Map<String, Object> context) {
            String shareScopeString = this.shareScopeExdr.expandString(context);
            // defaults to false, so anything but true is false
            return "true".equals(shareScopeString);
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getNameExdr() {
            return nameExdr;
        }

        public FlexibleStringExpander getLocationExdr() {
            return locationExdr;
        }

        public FlexibleStringExpander getShareScopeExdr() {
            return shareScopeExdr;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class Grid extends ModelScreenWidget implements FlexibleNameAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "include-grid";
        private final FlexibleStringExpander nameExdr;
        private final FlexibleStringExpander locationExdr;
        private final FlexibleStringExpander shareScopeExdr;
        private final List<ModelAction> actions; // SCIPIO: 2017-05-01: new post-context-stack-push actions

        public Grid(ModelScreen modelScreen, Element formElement) {
            super(modelScreen, formElement);
            this.nameExdr = FlexibleStringExpander.getInstance(formElement.getAttribute("name"));
            this.locationExdr = FlexibleStringExpander.getInstance(formElement.getAttribute("location"));
            this.shareScopeExdr = FlexibleStringExpander.getInstance(formElement.getAttribute("share-scope"));
            this.actions = readActions(modelScreen, formElement, "actions");
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) {
            // Output format might not support forms, so make form rendering optional.
            FormStringRenderer formStringRenderer = (FormStringRenderer) context.get("formStringRenderer");
            if (formStringRenderer == null) {
                if (Debug.verboseOn()) Debug.logVerbose("FormStringRenderer instance not found in rendering context, form not rendered.", module);
                return;
            }
            boolean protectScope = !shareScope(context);
            if (protectScope) {
                if (!(context instanceof MapStack<?>)) {
                    context = MapStack.create(context);
                }
                UtilGenerics.<MapStack<String>>cast(context).push();
            }
            
            AbstractModelAction.runSubActions(this.actions, context); // SCIPIO: 2017-05-01: new post-context-stack-push actions

            try {
                ModelForm modelForm = getModelForm(context);
                // SCIPIO: targeted rendering applicability check.
                WidgetRenderTargetState renderTargetState = WidgetRenderTargetExpr.getRenderTargetState(context);
                WidgetRenderTargetState.ExecutionInfo execInfo = renderTargetState.handleShouldExecute(modelForm, writer, context, screenStringRenderer);
                if (!execInfo.shouldExecute()) {
                    return;
                }
                try {
                    FormRenderer renderer = new FormRenderer(modelForm, formStringRenderer);
                    renderer.render(writer, context);
                } finally {
                    execInfo.handleFinished(context); // SCIPIO: return logic
                }
            } catch (Exception e) {
                String errMsg = "Error rendering included grid named [" + getName() + "] at location [" + this.getLocation(context) + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg + e);
            }
            if (protectScope) {
                UtilGenerics.<MapStack<String>>cast(context).pop();
            }
        }

        public ModelForm getModelForm(Map<String, Object> context) {
            ModelForm modelForm = null;
            String name = this.getName(context);
            String location = this.getLocation(context);
            try {
                modelForm = GridFactory.getGridFromLocation(location, name, getModelScreen().getDelegator(context).getModelReader(), getModelScreen().getDispatcher(context).getDispatchContext());
            } catch (Exception e) {
                String errMsg = "Error rendering included form named [" + name + "] at location [" + location + "]: ";
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg + e);
            }
            return modelForm;
        }

        public String getName(Map<String, Object> context) {
            return this.nameExdr.expandString(context);
        }

        public String getLocation() {
            return locationExdr.getOriginal();
        }

        public String getLocation(Map<String, Object> context) {
            return this.locationExdr.expandString(context);
        }

        public boolean shareScope(Map<String, Object> context) {
            String shareScopeString = this.shareScopeExdr.expandString(context);
            // defaults to false, so anything but true is false
            return "true".equals(shareScopeString);
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getNameExdr() {
            return nameExdr;
        }

        public FlexibleStringExpander getLocationExdr() {
            return locationExdr;
        }

        public FlexibleStringExpander getShareScopeExdr() {
            return shareScopeExdr;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class Tree extends ModelScreenWidget implements FlexibleNameAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "include-tree";
        private final FlexibleStringExpander nameExdr;
        private final FlexibleStringExpander locationExdr;
        private final FlexibleStringExpander shareScopeExdr;
        private final List<ModelAction> actions; // SCIPIO: 2017-05-01: new post-context-stack-push actions

        public Tree(ModelScreen modelScreen, Element treeElement) {
            super(modelScreen, treeElement);
            this.nameExdr = FlexibleStringExpander.getInstance(treeElement.getAttribute("name"));
            this.locationExdr = FlexibleStringExpander.getInstance(treeElement.getAttribute("location"));
            this.shareScopeExdr = FlexibleStringExpander.getInstance(treeElement.getAttribute("share-scope"));
            this.actions = readActions(modelScreen, treeElement, "actions");
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
            // Output format might not support trees, so make tree rendering optional.
            TreeStringRenderer treeStringRenderer = (TreeStringRenderer) context.get("treeStringRenderer");
            if (treeStringRenderer == null) {
                if (Debug.verboseOn()) Debug.logVerbose("TreeStringRenderer instance not found in rendering context, tree not rendered.", module);
                return;
            }
            boolean protectScope = !shareScope(context);
            if (protectScope) {
                if (!(context instanceof MapStack<?>)) {
                    context = MapStack.create(context);
                }
                UtilGenerics.<MapStack<String>>cast(context).push();
            }

            AbstractModelAction.runSubActions(this.actions, context); // SCIPIO: 2017-05-01: new post-context-stack-push actions

            String name = this.getName(context);
            String location = this.getLocation(context);
            ModelTree modelTree = null;
            try {
                modelTree = TreeFactory.getTreeFromLocation(this.getLocation(context), this.getName(context), getModelScreen().getDelegator(context), getModelScreen().getDispatcher(context));
            } catch (IOException e) {
                String errMsg = "Error rendering included tree named [" + name + "] at location [" + location + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            } catch (SAXException e) {
                String errMsg = "Error rendering included tree named [" + name + "] at location [" + location + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            } catch (ParserConfigurationException e) {
                String errMsg = "Error rendering included tree named [" + name + "] at location [" + location + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            }
            modelTree.renderTreeString(writer, context, treeStringRenderer);
            if (protectScope) {
                UtilGenerics.<MapStack<String>>cast(context).pop();
            }
        }

        public String getName(Map<String, Object> context) {
            return this.nameExdr.expandString(context);
        }

        public String getLocation(Map<String, Object> context) {
            return this.locationExdr.expandString(context);
        }

        public boolean shareScope(Map<String, Object> context) {
            String shareScopeString = this.shareScopeExdr.expandString(context);
            // defaults to false, so anything but true is false
            return "true".equals(shareScopeString);
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getLocationExdr() {
            return locationExdr;
        }

        public FlexibleStringExpander getShareScopeExdr() {
            return shareScopeExdr;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class PlatformSpecific extends ModelScreenWidget {
        public static final String TAG_NAME = "platform-specific";
        private final Map<String, ModelScreenWidget> subWidgets;

        public PlatformSpecific(ModelScreen modelScreen, Element platformSpecificElement) {
            super(modelScreen, platformSpecificElement);
            Map<String, ModelScreenWidget> subWidgets = new HashMap<String, ModelScreenWidget>();
            List<? extends Element> childElements = UtilXml.childElementList(platformSpecificElement);
            if (childElements != null) {
                for (Element childElement: childElements) {
                    if ("html".equals(childElement.getNodeName())) {
                        subWidgets.put("html", new HtmlWidget(modelScreen, childElement));
                    } else if ("xsl-fo".equals(childElement.getNodeName())) {
                        subWidgets.put("xsl-fo", new HtmlWidget(modelScreen, childElement));
                    } else if ("xml".equals(childElement.getNodeName())) {
                        subWidgets.put("xml", new HtmlWidget(modelScreen, childElement));
                    }  else if ("email".equals(childElement.getNodeName())) { 
                        // SCIPIO Email template renderer
                        subWidgets.put("email", new HtmlWidget(modelScreen, childElement));
                    } else {
                        throw new IllegalArgumentException("Tag not supported under the platform-specific tag with name: " + childElement.getNodeName());
                    }
                }
            }
            this.subWidgets = Collections.unmodifiableMap(subWidgets);
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
            ModelScreenWidget subWidget = null;
            subWidget = subWidgets.get(screenStringRenderer.getRendererName());
            if (subWidget == null) {
                // This is here for backward compatibility
                Debug.logWarning("In platform-dependent could not find template for " + screenStringRenderer.getRendererName() + ", using the one for html.", module);
                subWidget = subWidgets.get("html");
            }
            if (subWidget != null) {
                subWidget.renderWidgetString(writer, context, screenStringRenderer);
            }
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public Map<String, ModelScreenWidget> getSubWidgets() {
            return subWidgets;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class Content extends ModelScreenWidget {
        public static final String TAG_NAME = "content";

        private final FlexibleStringExpander contentId;
        private final FlexibleStringExpander editRequest;
        private final FlexibleStringExpander editContainerStyle;
        private final FlexibleStringExpander enableEditName;
        private final boolean xmlEscape;
        private final FlexibleStringExpander dataResourceId;
        private final String width;
        private final String height;
        private final String border;

        public Content(ModelScreen modelScreen, Element subContentElement) {
            super(modelScreen, subContentElement);
            this.contentId = FlexibleStringExpander.getInstance(subContentElement.getAttribute("content-id"));
            this.dataResourceId = FlexibleStringExpander.getInstance(subContentElement.getAttribute("dataresource-id"));
            this.editRequest = FlexibleStringExpander.getInstance(subContentElement.getAttribute("edit-request"));
            this.editContainerStyle = FlexibleStringExpander.getInstance(subContentElement.getAttribute("edit-container-style"));
            this.enableEditName = FlexibleStringExpander.getInstance(subContentElement.getAttribute("enable-edit-name"));
            this.xmlEscape = "true".equals(subContentElement.getAttribute("xml-escape"));
            String width = subContentElement.getAttribute("width");
            if (width.isEmpty())
                width = "60%";
            this.height = subContentElement.getAttribute("height");
            if (this.height.isEmpty())
                width = "400px";
            this.width = width;
            this.border = subContentElement.getAttribute("border");
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) {
            try {
                // pushing the contentId on the context as "contentId" is done
                // because many times there will be embedded "subcontent" elements
                // that use the syntax: <subcontent content-id="${contentId}"...
                // and this is a step to make sure that it is there.
                Delegator delegator = (Delegator) context.get("delegator");
                GenericValue content = null;
                String expandedDataResourceId = getDataResourceId(context);
                String expandedContentId = getContentId(context);
                if (!(context instanceof MapStack<?>)) {
                    context = MapStack.create(context);
                }

                // This is an important step to make sure that the current contentId is in the context
                // as templates that contain "subcontent" elements will expect to find the master
                // contentId in the context as "contentId".
                UtilGenerics.<MapStack<String>>cast(context).push();
                context.put("contentId", expandedContentId);

                if (expandedDataResourceId.isEmpty()) {
                    if (!expandedContentId.isEmpty()) {
                        content = EntityQuery.use(delegator).from("Content").where("contentId", expandedContentId).cache().queryOne();
                    } else {
                        String errMsg = "contentId is empty.";
                        Debug.logError(errMsg, module);
                        return;
                    }
                    if (content != null) {
                        if (content.get("dataResourceId") != null) expandedDataResourceId = content.getString("dataResourceId");
                    } else {
                        String errMsg = "Could not find content with contentId [" + expandedContentId + "] ";
                        Debug.logError(errMsg, module);
                        throw new RuntimeException(errMsg);
                    }
                }

                GenericValue dataResource = null;
                if (!expandedDataResourceId.isEmpty()) {
                    dataResource = EntityQuery.use(delegator).from("DataResource").where("dataResourceId", expandedDataResourceId).cache().queryOne();
                }

                String mimeTypeId = null;
                if (dataResource != null) {
                    mimeTypeId = dataResource.getString("mimeTypeId");
                }
                if (content != null) {
                    mimeTypeId = content.getString("mimeTypeId");
                }

                if (!(mimeTypeId != null
                        && ((mimeTypeId.indexOf("application") >= 0) || (mimeTypeId.indexOf("image")) >= 0))) {
                    screenStringRenderer.renderContentBegin(writer, context, this);
                    screenStringRenderer.renderContentBody(writer, context, this);
                    screenStringRenderer.renderContentEnd(writer, context, this);
                }
                UtilGenerics.<MapStack<String>>cast(context).pop();
            } catch (IOException e) {
                String errMsg = "Error rendering content with contentId [" + getContentId(context) + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            } catch (GenericEntityException e) {
                String errMsg = "Error obtaining content with contentId [" + getContentId(context) + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            }

        }

        public String getContentId(Map<String, Object> context) {
            return this.contentId.expandString(context);
        }

        public String getDataResourceId() {
            return this.dataResourceId.getOriginal();
        }

        public String getDataResourceId(Map<String, Object> context) {
            return this.dataResourceId.expandString(context);
        }

        public String getEditRequest(Map<String, Object> context) {
            return this.editRequest.expandString(context);
        }

        public String getEditContainerStyle(Map<String, Object> context) {
            return this.editContainerStyle.expandString(context);
        }

        public String getEnableEditName(Map<String, Object> context) {
            return this.enableEditName.expandString(context);
        }

        public boolean xmlEscape() {
            return this.xmlEscape;
        }

        public String getWidth() {
            return this.width;
        }

        public String getHeight() {
            return this.height;
        }

        public String getBorder() {
            return this.border;
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getContentId() {
            return contentId;
        }

        public FlexibleStringExpander getEditRequest() {
            return editRequest;
        }

        public FlexibleStringExpander getEditContainerStyle() {
            return editContainerStyle;
        }

        public FlexibleStringExpander getEnableEditName() {
            return enableEditName;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class SubContent extends ModelScreenWidget {
        public static final String TAG_NAME = "sub-content";
        private final FlexibleStringExpander contentId;
        private final FlexibleStringExpander mapKey;
        private final FlexibleStringExpander editRequest;
        private final FlexibleStringExpander editContainerStyle;
        private final FlexibleStringExpander enableEditName;
        private final boolean xmlEscape;

        public SubContent(ModelScreen modelScreen, Element subContentElement) {
            super(modelScreen, subContentElement);
            this.contentId = FlexibleStringExpander.getInstance(subContentElement.getAttribute("content-id"));
            String mapKey = subContentElement.getAttribute("map-key");
            if (mapKey.isEmpty()) {
                mapKey = subContentElement.getAttribute("assoc-name");
            }
            this.mapKey = FlexibleStringExpander.getInstance(mapKey);
            this.editRequest = FlexibleStringExpander.getInstance(subContentElement.getAttribute("edit-request"));
            this.editContainerStyle = FlexibleStringExpander.getInstance(subContentElement.getAttribute("edit-container-style"));
            this.enableEditName = FlexibleStringExpander.getInstance(subContentElement.getAttribute("enable-edit-name"));
            this.xmlEscape = "true".equals(subContentElement.getAttribute("xml-escape"));
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) {
            try {
                screenStringRenderer.renderSubContentBegin(writer, context, this);
                screenStringRenderer.renderSubContentBody(writer, context, this);
                screenStringRenderer.renderSubContentEnd(writer, context, this);
            } catch (IOException e) {
                String errMsg = "Error rendering subContent with contentId [" + getContentId(context) + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            }
        }

        public String getContentId(Map<String, Object> context) {
            return this.contentId.expandString(context);
        }

        public String getMapKey(Map<String, Object> context) {
            return this.mapKey.expandString(context);
        }

        public String getEditRequest(Map<String, Object> context) {
            return this.editRequest.expandString(context);
        }

        public String getEditContainerStyle(Map<String, Object> context) {
            return this.editContainerStyle.expandString(context);
        }

        public String getEnableEditName(Map<String, Object> context) {
            return this.enableEditName.expandString(context);
        }

        public boolean xmlEscape() {
            return this.xmlEscape;
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            // TODO Auto-generated method stub
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class Menu extends ModelScreenWidget implements FlexibleNameAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "include-menu";
        private final FlexibleStringExpander nameExdr;
        private final FlexibleStringExpander locationExdr;
        private final FlexibleStringExpander shareScopeExdr; // SCIPIO: added share-scope for menus (not in stock ofbiz)
        private final FlexibleStringExpander maxDepthExdr; // SCIPIO: new
        private final FlexibleStringExpander subMenuFilterExdr; // SCIPIO: new
        private final List<ModelAction> actions; // SCIPIO: 2017-05-01: new post-context-stack-push actions

        public Menu(ModelScreen modelScreen, Element menuElement) {
            super(modelScreen, menuElement);
            this.nameExdr = FlexibleStringExpander.getInstance(menuElement.getAttribute("name"));
            this.locationExdr = FlexibleStringExpander.getInstance(menuElement.getAttribute("location"));
            this.shareScopeExdr = FlexibleStringExpander.getInstance(menuElement.getAttribute("share-scope")); // SCIPIO: added
            this.maxDepthExdr = FlexibleStringExpander.getInstance(menuElement.getAttribute("max-depth")); // SCIPIO: added
            this.subMenuFilterExdr = FlexibleStringExpander.getInstance(menuElement.getAttribute("sub-menus")); // SCIPIO: added
            this.actions = readActions(modelScreen, menuElement, "actions");
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws IOException {
            // Output format might not support menus, so make menu rendering optional.
            MenuStringRenderer menuStringRenderer = (MenuStringRenderer) context.get("menuStringRenderer");
            if (menuStringRenderer == null) {
                if (Debug.verboseOn()) Debug.logVerbose("MenuStringRenderer instance not found in rendering context, menu not rendered.", module);
                return;
            }
            
            ModelMenu modelMenu = getModelMenu(context);
            
            // SCIPIO: targeted rendering applicability check.
            WidgetRenderTargetState renderTargetState = WidgetRenderTargetExpr.getRenderTargetState(context);
            WidgetRenderTargetState.ExecutionInfo execInfo = renderTargetState.handleShouldExecute(modelMenu, writer, context, screenStringRenderer);
            if (!execInfo.shouldExecute()) {
                return;
            }
            try {
            
                // SCIPIO: caller may have set these. Remove and transfer them to MenuRenderState
                Map<String, Object> menuRenderArgs = UtilGenerics.checkMap(context.remove("menuRenderArgs"));
                
                // SCIPIO: added scope protect
                boolean protectScope = !shareScope(context);
                if (protectScope) {
                    if (!(context instanceof MapStack<?>)) {
                        context = MapStack.create(context);
                    }
                    UtilGenerics.<MapStack<String>>cast(context).push();
                }
                
                AbstractModelAction.runSubActions(this.actions, context); // SCIPIO: 2017-05-01: new post-context-stack-push actions
    
                // SCIPIO: new render state to carry around max depth
                // NOTE: we'll manually save/restore the previous one in case share-scope is not enabled
                MenuRenderState prevRenderState = MenuRenderState.retrieve(context);
                if (prevRenderState != null) {
                    Debug.logWarning("include-menu: Rendering: A MenuRenderState was already in context at the time "
                        + "a new menu render was started", module);
                }
                try {
                    MenuRenderState renderState = MenuRenderState.createAndStore(context, modelMenu);
                    if (menuRenderArgs != null) {
                        renderState.putAll(menuRenderArgs); // keep same names
                    }
                    renderState.setMaxDepth(getMaxDepth(context));
                    renderState.setSubMenuFilter(getSubMenuFilter(context));
                    
                    modelMenu.renderMenuString(writer, context, menuStringRenderer);
                } finally {
                    // SCIPIO: restore the previous render state just in case
                    MenuRenderState.store(context, prevRenderState);
                }
                
                // SCIPIO: added scope protect
                if (protectScope) {
                    UtilGenerics.<MapStack<String>>cast(context).pop();
                }
            } finally {
                execInfo.handleFinished(context); // SCIPIO: return logic
            }
        }

        public ModelMenu getModelMenu(Map<String, Object> context) {
            String name = this.getName(context);
            String location = this.getLocation(context);
            ModelMenu modelMenu = null;
            try {
                modelMenu = MenuFactory.getMenuFromLocation(location, name);
            } catch (Exception e) {
                String errMsg = "Error rendering included menu named [" + name + "] at location [" + location + "]: ";
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg + e);
            }
            return modelMenu;
        }

        public String getName(Map<String, Object> context) {
            return this.nameExdr.expandString(context);
        }

        public String getLocation(Map<String, Object> context) {
            return this.locationExdr.expandString(context);
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getLocationExdr() {
            return locationExdr;
        }
        
        public FlexibleStringExpander getShareScopeExdr() { // SCIPIO: added
            return shareScopeExdr;
        }
        
        public boolean shareScope(Map<String, Object> context) { // SCIPIO: added
            String shareScopeString = this.shareScopeExdr.expandString(context);
            // defaults to false, so anything but true is false
            return "true".equals(shareScopeString);
        }

        public FlexibleStringExpander getMaxDepthExdr() {
            return maxDepthExdr;
        }
        
        public Integer getMaxDepth(Map<String, Object> context) {
            String maxDepthStr = this.maxDepthExdr.expandString(context);
            if (UtilValidate.isEmpty(maxDepthStr)) {
                return null;
            } else {
                try {
                    return Integer.parseInt(maxDepthStr);
                } catch (NumberFormatException e) {
                    Debug.logError(e, "Menu max-depth expression '" + this.maxDepthExdr.getOriginal() + "' evaluated to invalid number "
                            + "(from include-menu element referencing " + this.locationExdr.getOriginal() + "#" + this.nameExdr.getOriginal() + ")", module);
                    return null;
                }
            }
        }

        public FlexibleStringExpander getSubMenuFilterExdr() {
            return subMenuFilterExdr;
        }
        
        public String getSubMenuFilter(Map<String, Object> context) {
            return this.subMenuFilterExdr.expandString(context);
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class ScreenLink extends ModelScreenWidget implements FlexibleIdAttrWidget, FlexibleNameAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "link";
        private final Link link;
        private final ScreenImage image;

        public ScreenLink(ModelScreen modelScreen, Element linkElement) {
            super(modelScreen, linkElement);
            this.link = new Link(linkElement);
            Element imageElement = UtilXml.firstChildElement(linkElement, "image");
            if (imageElement != null) {
                this.image = new ScreenImage(modelScreen, imageElement);
            } else {
                this.image = null;
            }
        }

        public String getName() {
            return link.getName();
        }

        public String getText(Map<String, Object> context) {
            return link.getText(context);
        }

        public String getId(Map<String, Object> context) {
            return link.getId(context);
        }

        public String getStyle(Map<String, Object> context) {
            return link.getStyle(context);
        }

        public String getTarget(Map<String, Object> context) {
            return link.getTarget(context);
        }

        public String getName(Map<String, Object> context) {
            return link.getName(context);
        }

        public String getTargetWindow(Map<String, Object> context) {
            return link.getTargetWindow(context);
        }

        public String getUrlMode() {
            return link.getUrlMode();
        }

        public String getPrefix(Map<String, Object> context) {
            return link.getPrefix(context);
        }

        public Boolean getFullPath() { // SCIPIO: changed from boolean to Boolean
            return link.getFullPath();
        }

        public Boolean getSecure() { // SCIPIO: changed from boolean to Boolean
            return link.getSecure();
        }

        public Boolean getEncode() { // SCIPIO: changed from boolean to Boolean
            return link.getEncode();
        }

        public ScreenImage getImage() {
            return image;
        }

        public String getLinkType() {
            return link.getLinkType();
        }

        public String getWidth() {
            return link.getWidth();
        }

        public String getHeight() {
            return link.getHeight();
        }

        public Map<String, String> getParameterMap(Map<String, Object> context) {
            return link.getParameterMap(context);
        }

        public FlexibleStringExpander getTextExdr() {
            return link.getTextExdr();
        }

        public FlexibleStringExpander getIdExdr() {
            return link.getIdExdr();
        }

        public FlexibleStringExpander getStyleExdr() {
            return link.getStyleExdr();
        }

        public FlexibleStringExpander getTargetExdr() {
            return link.getTargetExdr();
        }

        public FlexibleStringExpander getTargetWindowExdr() {
            return link.getTargetWindowExdr();
        }

        public FlexibleStringExpander getPrefixExdr() {
            return link.getPrefixExdr();
        }

        public FlexibleStringExpander getNameExdr() {
            return link.getNameExdr();
        }

        public List<Parameter> getParameterList() {
            return link.getParameterList();
        }

        public AutoServiceParameters getAutoServiceParameters() {
            return link.getAutoServiceParameters();
        }

        public AutoEntityParameters getAutoEntityParameters() {
            return link.getAutoEntityParameters();
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) {
            try {
                screenStringRenderer.renderLink(writer, context, this);
            } catch (IOException e) {
                String errMsg = "Error rendering link with id [" + link.getId(context) + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            }
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public Link getLink() {
            return link;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class ScreenImage extends ModelScreenWidget implements FlexibleIdAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "image";
        private final Image image;

        public String getName() {
            return image.getName();
        }

        public String getSrc(Map<String, Object> context) {
            return image.getSrc(context);
        }

        public String getId(Map<String, Object> context) {
            return image.getId(context);
        }

        public String getStyle(Map<String, Object> context) {
            return image.getStyle(context);
        }

        public String getWidth(Map<String, Object> context) {
            return image.getWidth(context);
        }

        public String getHeight(Map<String, Object> context) {
            return image.getHeight(context);
        }

        public String getBorder(Map<String, Object> context) {
            return image.getBorder(context);
        }

        public String getAlt(Map<String, Object> context) {
            return image.getAlt(context);
        }

        public String getUrlMode() {
            return image.getUrlMode();
        }

        public FlexibleStringExpander getSrcExdr() {
            return image.getSrcExdr();
        }

        public FlexibleStringExpander getIdExdr() {
            return image.getIdExdr();
        }

        public FlexibleStringExpander getStyleExdr() {
            return image.getStyleExdr();
        }

        public FlexibleStringExpander getWidthExdr() {
            return image.getWidthExdr();
        }

        public FlexibleStringExpander getHeightExdr() {
            return image.getHeightExdr();
        }

        public FlexibleStringExpander getBorderExdr() {
            return image.getBorderExdr();
        }

        public FlexibleStringExpander getAlt() {
            return image.getAlt();
        }

        public ScreenImage(ModelScreen modelScreen, Element imageElement) {
            super(modelScreen, imageElement);
            this.image = new Image(imageElement);
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) {
            try {
                screenStringRenderer.renderImage(writer, context, this);
            } catch (IOException e) {
                String errMsg = "Error rendering image with id [" + image.getId(context) + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            }
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public Image getImage() {
            return image;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    public static final class PortalPage extends ModelScreenWidget implements FlexibleIdAttrWidget { // SCIPIO: interfaces
        public static final String TAG_NAME = "include-portal-page";
        private final FlexibleStringExpander idExdr;
        private final FlexibleStringExpander confModeExdr;
        private final Boolean usePrivate;

        public PortalPage(ModelScreen modelScreen, Element portalPageElement) {
            super(modelScreen, portalPageElement);
            this.idExdr = FlexibleStringExpander.getInstance(portalPageElement.getAttribute("id"));
            this.confModeExdr = FlexibleStringExpander.getInstance(portalPageElement.getAttribute("conf-mode"));
            this.usePrivate = !("false".equals(portalPageElement.getAttribute("use-private")));
        }

        private GenericValue getPortalPageValue(Map<String, Object> context) {
            Delegator delegator = (Delegator) context.get("delegator");
            String expandedPortalPageId = getId(context);
            GenericValue portalPage = null;
            if (!expandedPortalPageId.isEmpty()) {
                if (usePrivate) {
                    portalPage = PortalPageWorker.getPortalPage(expandedPortalPageId, context);
                } else {
                    try {
                        portalPage = EntityQuery.use(delegator)
                                                .from("PortalPage")
                                                .where("portalPageId", expandedPortalPageId)
                                                .cache().queryOne();
                    } catch (GenericEntityException e) {
                        throw new RuntimeException(e);
                    }
                }
            }
            if (portalPage == null) {
                String errMsg = "Could not find PortalPage with portalPageId [" + expandedPortalPageId + "] ";
                Debug.logError(errMsg, module);
                throw new RuntimeException(errMsg);
            }
            return portalPage;
        }

        @Override
        public void renderWidgetStringCore(Appendable writer, Map<String, Object> context, ScreenStringRenderer screenStringRenderer) throws GeneralException, IOException {
            try {
                Delegator delegator = (Delegator) context.get("delegator");
                List<GenericValue> portalPageColumns = null;
                List<GenericValue> portalPagePortlets = null;
                List<GenericValue> portletAttributes = null;
                GenericValue portalPage = getPortalPageValue(context);
                String actualPortalPageId = portalPage.getString("portalPageId");
                portalPageColumns = EntityQuery.use(delegator)
                                               .from("PortalPageColumn")
                                               .where("portalPageId", actualPortalPageId)
                                               .orderBy("columnSeqId")
                                               .cache(true)
                                               .queryList();
                
                int columnCount = portalPageColumns.size();
                context.put("portletColumnCount", columnCount);
                
                // Renders the portalPage header
                screenStringRenderer.renderPortalPageBegin(writer, context, this);
                
                // First column has no previous column
                String prevColumnSeqId = "";
                
                // Iterates through the PortalPage columns
                ListIterator <GenericValue>columnsIterator = portalPageColumns.listIterator();
                int columnIndex = 0;
                while(columnsIterator.hasNext()) {
                    GenericValue columnValue = columnsIterator.next();
                    String columnSeqId = columnValue.getString("columnSeqId");
                    
                    context.put("portletColumnIndex", columnIndex);
                    
                    // Renders the portalPageColumn header
                    screenStringRenderer.renderPortalPageColumnBegin(writer, context, this, columnValue);

                    // Get the Portlets located in the current column
                    portalPagePortlets = EntityQuery.use(delegator)
                                                    .from("PortalPagePortletView")
                                                    .where("portalPageId", portalPage.getString("portalPageId"), "columnSeqId", columnSeqId)
                                                    .orderBy("sequenceNum")
                                                    .queryList();
                    // First Portlet in a Column has no previous Portlet
                    String prevPortletId = "";
                    String prevPortletSeqId = "";

                    // If this is not the last column, get the next columnSeqId
                    String nextColumnSeqId = "";
                    if (columnsIterator.hasNext()) {
                        nextColumnSeqId = portalPageColumns.get(columnsIterator.nextIndex()).getString("columnSeqId");
                    }
                    
                    // Iterates through the Portlets in the Column
                    ListIterator <GenericValue>portletsIterator = portalPagePortlets.listIterator();
                    while(portletsIterator.hasNext()) {
                        GenericValue portletValue = portletsIterator.next();

                        // If not the last portlet in the column, get the next nextPortletId and nextPortletSeqId
                        String nextPortletId = "";
                        String nextPortletSeqId = "";
                        if (portletsIterator.hasNext()) {
                            nextPortletId = portalPagePortlets.get(portletsIterator.nextIndex()).getString("portalPortletId");
                            nextPortletSeqId = portalPagePortlets.get(portletsIterator.nextIndex()).getString("portletSeqId");
                        }

                        // Set info to allow portlet movement in the page
                        context.put("prevPortletId", prevPortletId);
                        context.put("prevPortletSeqId", prevPortletSeqId);
                        context.put("nextPortletId", nextPortletId);
                        context.put("nextPortletSeqId", nextPortletSeqId);
                        context.put("prevColumnSeqId", prevColumnSeqId);
                        context.put("nextColumnSeqId", nextColumnSeqId);
                        
                        // SCIPIO: make these available to portlets
                        context.put("columnWidthPercentage", columnValue.getString("columnWidthPercentage"));
                        context.put("columnWidthPixels", columnValue.getString("columnWidthPixels"));
                       
                        // Get portlet's attributes
                        portletAttributes = EntityQuery.use(delegator)
                                                       .from("PortletAttribute")
                                                       .where("portalPageId", portletValue.get("portalPageId"), "portalPortletId", portletValue.get("portalPortletId"), "portletSeqId", portletValue.get("portletSeqId"))
                                                       .queryList();
                        
                        ListIterator <GenericValue>attributesIterator = portletAttributes.listIterator();
                        while (attributesIterator.hasNext()) {
                            GenericValue attribute = attributesIterator.next();
                            context.put(attribute.getString("attrName"), attribute.getString("attrValue"));
                        }
                        
                        // Renders the portalPagePortlet
                        screenStringRenderer.renderPortalPagePortletBegin(writer, context, this, portletValue);
                        screenStringRenderer.renderPortalPagePortletBody(writer, context, this, portletValue);
                        screenStringRenderer.renderPortalPagePortletEnd(writer, context, this, portletValue);

                        // Remove the portlet's attributes so that these are not available for other portlets
                        while (attributesIterator.hasPrevious()) {
                            GenericValue attribute = attributesIterator.previous();
                            context.remove(attribute.getString("attrName"));
                        }
                        
                        // Uses the actual portlet as prevPortlet for next iteration
                        prevPortletId = (String) portletValue.get("portalPortletId");
                        prevPortletSeqId = (String) portletValue.get("portletSeqId");
                    }
                    // Renders the portalPageColumn footer
                    screenStringRenderer.renderPortalPageColumnEnd(writer, context, this, columnValue);

                    // Uses the actual columnSeqId as prevColumnSeqId for next iteration
                    prevColumnSeqId = columnSeqId;
                    columnIndex++;
                }
                // Renders the portalPage footer
                screenStringRenderer.renderPortalPageEnd(writer, context, this);
            } catch (IOException e) {
                String errMsg = "Error rendering PortalPage with portalPageId [" + getId(context) + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            } catch (GenericEntityException e) {
                String errMsg = "Error obtaining PortalPage with portalPageId [" + getId(context) + "]: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new RuntimeException(errMsg);
            }
        }

        public String getId(Map<String, Object> context) {
            return this.idExdr.expandString(context);
        }

        public String getOriginalPortalPageId(Map<String, Object> context) {
            GenericValue portalPage = getPortalPageValue(context);
            return portalPage.getString("originalPortalPageId");
        }
        
        public String getActualPortalPageId(Map<String, Object> context) {
            GenericValue portalPage = getPortalPageValue(context);
            return portalPage.getString("portalPageId");
        }

        public String getConfMode(Map<String, Object> context) {
            return this.confModeExdr.expandString(context);
        }

        public String getUsePrivate() {
            return Boolean.toString(this.usePrivate);
        }

        @Override
        public void accept(ModelWidgetVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getIdExdr() {
            return idExdr;
        }

        public FlexibleStringExpander getConfModeExdr() {
            return confModeExdr;
        }
        
        @Override
        public String getWidgetType() {
            return TAG_NAME;
        }
    }

    @Override
    public String getContainerLocation() { // SCIPIO: new
        return getModelScreen() != null ? getModelScreen().getFullLocationAndName() : null;
    }
}

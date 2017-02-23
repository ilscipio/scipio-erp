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
import java.io.Serializable;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;
import java.util.TimeZone;
import java.util.regex.PatternSyntaxException;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpSession;
import javax.xml.parsers.ParserConfigurationException;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.ScriptUtil;
import org.ofbiz.base.util.Scriptlet;
import org.ofbiz.base.util.StringUtil;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.UtilXml.ElementHelper;
import org.ofbiz.base.util.collections.FlexibleMapAccessor;
import org.ofbiz.base.util.collections.ResourceBundleMapWrapper;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.finder.ByAndFinder;
import org.ofbiz.entity.finder.ByConditionFinder;
import org.ofbiz.entity.finder.EntityFinderUtil;
import org.ofbiz.entity.finder.PrimaryKeyFinder;
import org.ofbiz.entity.util.EntityUtilProperties;
import org.ofbiz.minilang.MiniLangException;
import org.ofbiz.minilang.MiniLangRuntimeException;
import org.ofbiz.minilang.MiniLangUtil;
import org.ofbiz.minilang.SimpleMethod;
import org.ofbiz.minilang.method.MethodContext;
import org.ofbiz.minilang.method.MethodOperation;
import org.ofbiz.minilang.method.conditional.Conditional;
import org.ofbiz.minilang.method.conditional.ConditionalFactory;
import org.ofbiz.minilang.method.conditional.ElseIf;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.widget.WidgetWorker;
import org.w3c.dom.Element;
import org.xml.sax.SAXException;

/**
 * Abstract base class for the action models.
 */
@SuppressWarnings("serial")
public abstract class AbstractModelAction implements Serializable, ModelAction {

    /*
     * ----------------------------------------------------------------------- *
     *                     DEVELOPERS PLEASE READ
     * ----------------------------------------------------------------------- *
     * 
     * This model is intended to be a read-only data structure that represents
     * an XML element. Outside of object construction, the class should not
     * have any behaviors.
     * 
     * Instances of this class will be shared by multiple threads - therefore
     * it is immutable. DO NOT CHANGE THE OBJECT'S STATE AT RUN TIME!
     * 
     */

    public static final String module = AbstractModelAction.class.getName();

    /**
     * Returns a new <code>ModelAction</code> instance, built from <code>actionElement</code>.
     * 
     * @param modelWidget The <code>ModelWidget</code> that contains the &lt;actions&gt; element
     * @param actionElement
     * @return A new <code>ModelAction</code> instance
     */
    public static ModelAction newInstance(ModelWidget modelWidget, Element actionElement) {
        if ("set".equals(actionElement.getNodeName())) {
            return new SetField(modelWidget, actionElement);
        } else if ("property-map".equals(actionElement.getNodeName())) {
            return new PropertyMap(modelWidget, actionElement);
        } else if ("property-to-field".equals(actionElement.getNodeName())) {
            return new PropertyToField(modelWidget, actionElement);
        } else if ("script".equals(actionElement.getNodeName())) {
            return new Script(modelWidget, actionElement);
        } else if ("service".equals(actionElement.getNodeName())) {
            return new Service(modelWidget, actionElement);
        } else if ("entity-one".equals(actionElement.getNodeName())) {
            return new EntityOne(modelWidget, actionElement);
        } else if ("entity-and".equals(actionElement.getNodeName())) {
            return new EntityAnd(modelWidget, actionElement);
        } else if ("entity-condition".equals(actionElement.getNodeName())) {
            return new EntityCondition(modelWidget, actionElement);
        } else if ("get-related-one".equals(actionElement.getNodeName())) {
            return new GetRelatedOne(modelWidget, actionElement);
        } else if ("get-related".equals(actionElement.getNodeName())) {
            return new GetRelated(modelWidget, actionElement);
        } else if ("condition-to-field".equals(actionElement.getNodeName())) { // SCIPIO: new
            return new ConditionToField(modelWidget, actionElement);
        } else if ("if".equals(actionElement.getNodeName())) { // SCIPIO: new
            return new MasterIf(modelWidget, actionElement);
        } else if (IncludeActions.isIncludeActions(actionElement)) { // SCIPIO: new
            return IncludeActions.newInstance(modelWidget, actionElement);
        } else {
            throw new IllegalArgumentException("Action element not supported with name: " + actionElement.getNodeName());
        }
    }

    public static List<ModelAction> readSubActions(ModelWidget modelWidget, Element parentElement) {
        List<? extends Element> actionElementList = UtilXml.childElementList(parentElement);
        List<ModelAction> actions = new ArrayList<ModelAction>(actionElementList.size());
        for (Element actionElement : actionElementList) {
            actions.add(newInstance(modelWidget, actionElement));
        }
        return Collections.unmodifiableList(actions);
    }

    /**
     * Executes the actions contained in <code>actions</code>.
     * 
     * @param actions
     * @param context
     */
    public static void runSubActions(List<ModelAction> actions, Map<String, Object> context) {
        if (actions == null)
            return;
        for (ModelAction action : actions) {
            if (Debug.verboseOn())
                Debug.logVerbose("Running action " + action.getClass().getName(), module);
            try {
                action.runAction(context);
            } catch (GeneralException e) {
                throw new RuntimeException(e);
            }
        }
    }
    
    /**
     * SCIPIO: Executes the actions contained in <code>actions</code>, without wrapping exceptions
     * in RuntimeException.
     * 
     * @param actions
     * @param context
     * @throws GeneralException 
     */
    public static void runSubActionsEx(List<ModelAction> actions, Map<String, Object> context) throws GeneralException {
        if (actions == null)
            return;
        for (ModelAction action : actions) {
            if (Debug.verboseOn())
                Debug.logVerbose("Running action " + action.getClass().getName(), module);
            action.runAction(context);
        }
    }

    private final ModelWidget modelWidget;

    protected AbstractModelAction() {
        // FIXME: This should not be null.
        this.modelWidget = null;
    }

    protected AbstractModelAction(ModelWidget modelWidget, Element actionElement) {
        this.modelWidget = modelWidget;
        if (Debug.verboseOn())
            Debug.logVerbose("Reading widget action with name: " + actionElement.getNodeName(), module);
    }

    /**
     * Returns the <code>ModelWidget</code> that contains the &lt;actions&gt; element.
     * 
     * @return The <code>ModelWidget</code> that contains the &lt;actions&gt; element
     */
    public ModelWidget getModelWidget() {
        return modelWidget;
    }
    
    /**
     * SCIPIO: Returns suffix log message with location/id of directive (best-effort).
     */
    public String getLogDirectiveLocationString() {
        return modelWidget != null ? modelWidget.getLogWidgetLocationString() : " (untracked widget)";
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        ModelActionVisitor visitor = new XmlWidgetActionVisitor(sb);
        try {
            accept(visitor);
        } catch (Exception e) {
            Debug.logWarning(e, "Exception thrown in XmlWidgetActionVisitor: ", module);
        }
        return sb.toString();
    }

    /**
     * Models the &lt;entity-and&gt; element.
     * 
     * @see <code>widget-screen.xsd</code>
     */
    public static class EntityAnd extends AbstractModelAction {
        private final ByAndFinder finder;

        public EntityAnd(ModelWidget modelWidget, Element entityAndElement) {
            super(modelWidget, entityAndElement);
            finder = new ByAndFinder(entityAndElement);
        }

        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public ByAndFinder getFinder() {
            return this.finder;
        }

        @Override
        public void runAction(Map<String, Object> context) {
            try {
                finder.runFind(context, WidgetWorker.getDelegator(context));
            } catch (GeneralException e) {
                String errMsg = "Error doing entity query by condition: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new IllegalArgumentException(errMsg);
            }
        }
    }

    /**
     * Models the &lt;entity-condition&gt; element.
     * 
     * @see <code>widget-screen.xsd</code>
     */
    public static class EntityCondition extends AbstractModelAction {
        private final ByConditionFinder finder;

        public EntityCondition(ModelWidget modelWidget, Element entityConditionElement) {
            super(modelWidget, entityConditionElement);
            finder = new ByConditionFinder(entityConditionElement);
        }

        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public ByConditionFinder getFinder() {
            return this.finder;
        }

        @Override
        public void runAction(Map<String, Object> context) {
            try {
                finder.runFind(context, WidgetWorker.getDelegator(context));
            } catch (GeneralException e) {
                String errMsg = "Error doing entity query by condition: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new IllegalArgumentException(errMsg);
            }
        }
    }

    /**
     * Models the &lt;entity-one&gt; element.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class EntityOne extends AbstractModelAction {
        private final PrimaryKeyFinder finder;

        public EntityOne(ModelWidget modelWidget, Element entityOneElement) {
            super(modelWidget, entityOneElement);
            finder = new PrimaryKeyFinder(entityOneElement);
        }

        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public PrimaryKeyFinder getFinder() {
            return this.finder;
        }

        @Override
        public void runAction(Map<String, Object> context) {
            try {
                finder.runFind(context, WidgetWorker.getDelegator(context));
            } catch (GeneralException e) {
                String errMsg = "Error doing entity query by condition: " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new IllegalArgumentException(errMsg);
            }
        }
    }

    /**
     * Models the &lt;get-related&gt; element.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class GetRelated extends AbstractModelAction {
        private final FlexibleMapAccessor<List<GenericValue>> listNameAcsr;
        private final FlexibleMapAccessor<Map<String, Object>> mapAcsr;
        private final FlexibleMapAccessor<List<String>> orderByListAcsr;
        private final String relationName;
        private final boolean useCache;
        private final FlexibleMapAccessor<Object> valueNameAcsr;

        public GetRelated(ModelWidget modelWidget, Element getRelatedElement) {
            super(modelWidget, getRelatedElement);
            this.valueNameAcsr = FlexibleMapAccessor.getInstance(getRelatedElement.getAttribute("value-field"));
            this.listNameAcsr = FlexibleMapAccessor.getInstance(getRelatedElement.getAttribute("list"));
            this.relationName = getRelatedElement.getAttribute("relation-name");
            this.mapAcsr = FlexibleMapAccessor.getInstance(getRelatedElement.getAttribute("map"));
            this.orderByListAcsr = FlexibleMapAccessor.getInstance(getRelatedElement.getAttribute("order-by-list"));
            this.useCache = "true".equals(getRelatedElement.getAttribute("use-cache"));
        }

        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public String getRelationName() {
            return this.relationName;
        }

        @Override
        public void runAction(Map<String, Object> context) {
            Object valueObject = valueNameAcsr.get(context);
            if (valueObject == null) {
                Debug.logVerbose("Value not found with name: " + valueNameAcsr + ", not getting related...", module);
                return;
            }
            if (!(valueObject instanceof GenericValue)) {
                String errMsg = "Env variable for value-name " + valueNameAcsr.toString()
                        + " is not a GenericValue object; for the relation-name: " + relationName + "]";
                Debug.logError(errMsg, module);
                throw new IllegalArgumentException(errMsg);
            }
            GenericValue value = (GenericValue) valueObject;
            List<String> orderByNames = null;
            if (!orderByListAcsr.isEmpty()) {
                orderByNames = orderByListAcsr.get(context);
            }
            Map<String, Object> constraintMap = null;
            if (!mapAcsr.isEmpty()) {
                constraintMap = mapAcsr.get(context);
            }
            try {
                listNameAcsr.put(context, value.getRelated(relationName, constraintMap, orderByNames, useCache));
            } catch (GenericEntityException e) {
                String errMsg = "Problem getting related from entity with name " + value.getEntityName()
                        + " for the relation-name: " + relationName + ": " + e.getMessage();
                Debug.logError(e, errMsg, module);
                throw new IllegalArgumentException(errMsg);
            }
        }

        public FlexibleMapAccessor<List<GenericValue>> getListNameAcsr() {
            return listNameAcsr;
        }

        public FlexibleMapAccessor<Map<String, Object>> getMapAcsr() {
            return mapAcsr;
        }

        public FlexibleMapAccessor<List<String>> getOrderByListAcsr() {
            return orderByListAcsr;
        }

        public boolean getUseCache() {
            return useCache;
        }

        public FlexibleMapAccessor<Object> getValueNameAcsr() {
            return valueNameAcsr;
        }
    }

    /**
     * Models the &lt;get-related-one&gt; element.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class GetRelatedOne extends AbstractModelAction {
        private final String relationName;
        private final FlexibleMapAccessor<Object> toValueNameAcsr;
        private final boolean useCache;
        private final FlexibleMapAccessor<Object> valueNameAcsr;

        public GetRelatedOne(ModelWidget modelWidget, Element getRelatedOneElement) {
            super(modelWidget, getRelatedOneElement);
            this.valueNameAcsr = FlexibleMapAccessor.getInstance(getRelatedOneElement.getAttribute("value-field"));
            this.toValueNameAcsr = FlexibleMapAccessor.getInstance(getRelatedOneElement.getAttribute("to-value-field"));
            this.relationName = getRelatedOneElement.getAttribute("relation-name");
            this.useCache = "true".equals(getRelatedOneElement.getAttribute("use-cache"));
        }

        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public String getRelationName() {
            return this.relationName;
        }

        @Override
        public void runAction(Map<String, Object> context) {
            Object valueObject = valueNameAcsr.get(context);
            if (valueObject == null) {
                Debug.logVerbose("Value not found with name: " + valueNameAcsr + ", not getting related...", module);
                return;
            }
            if (!(valueObject instanceof GenericValue)) {
                String errMsg = "Env variable for value-name " + valueNameAcsr.toString()
                        + " is not a GenericValue object; for the relation-name: " + relationName + "]";
                Debug.logError(errMsg, module);
                throw new IllegalArgumentException(errMsg);
            }
            GenericValue value = (GenericValue) valueObject;
            try {
                toValueNameAcsr.put(context, value.getRelatedOne(relationName, useCache));
            } catch (GenericEntityException e) {
                String errMsg = "Problem getting related one from entity with name " + value.getEntityName()
                        + " for the relation-name: " + relationName + ": " + e.getMessage();
                Debug.logError(e, errMsg, module);
                throw new IllegalArgumentException(errMsg);
            }
        }

        public FlexibleMapAccessor<Object> getToValueNameAcsr() {
            return toValueNameAcsr;
        }

        public boolean getUseCache() {
            return useCache;
        }

        public FlexibleMapAccessor<Object> getValueNameAcsr() {
            return valueNameAcsr;
        }
    }

    /**
     * Models the &lt;property-map&gt; element.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class PropertyMap extends AbstractModelAction {
        private final FlexibleStringExpander globalExdr;
        private final FlexibleMapAccessor<ResourceBundleMapWrapper> mapNameAcsr;
        private final FlexibleStringExpander resourceExdr;

        public PropertyMap(ModelWidget modelWidget, Element setElement) {
            super(modelWidget, setElement);
            this.resourceExdr = FlexibleStringExpander.getInstance(setElement.getAttribute("resource"));
            this.mapNameAcsr = FlexibleMapAccessor.getInstance(setElement.getAttribute("map-name"));
            this.globalExdr = FlexibleStringExpander.getInstance(setElement.getAttribute("global"));
        }

        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        @Override
        public void runAction(Map<String, Object> context) {
            String globalStr = this.globalExdr.expandString(context);
            // default to false
            boolean global = "true".equals(globalStr);
            Locale locale = (Locale) context.get("locale");
            String resource = this.resourceExdr.expandString(context, locale);
            ResourceBundleMapWrapper existingPropMap = this.mapNameAcsr.get(context);
            if (existingPropMap == null) {
                this.mapNameAcsr.put(context, UtilProperties.getResourceBundleMap(resource, locale, context));
            } else {
                try {
                    existingPropMap.addBottomResourceBundle(resource);
                } catch (IllegalArgumentException e) {
                    // log the error, but don't let it kill everything just for a typo or bad char in an l10n file
                    Debug.logError(e, "Error adding resource bundle [" + resource + "]: " + e.toString(), module);
                }
            }
            if (global) {
                Map<String, Object> globalCtx = UtilGenerics.checkMap(context.get("globalContext"));
                if (globalCtx != null) {
                    ResourceBundleMapWrapper globalExistingPropMap = this.mapNameAcsr.get(globalCtx);
                    if (globalExistingPropMap == null) {
                        this.mapNameAcsr.put(globalCtx, UtilProperties.getResourceBundleMap(resource, locale, context));
                    } else {
                        // is it the same object? if not add it in here too...
                        if (existingPropMap != globalExistingPropMap) {
                            try {
                                globalExistingPropMap.addBottomResourceBundle(resource);
                            } catch (IllegalArgumentException e) {
                                // log the error, but don't let it kill everything just for a typo or bad char in an l10n file
                                Debug.logError(e, "Error adding resource bundle [" + resource + "]: " + e.toString(), module);
                            }
                        }
                    }
                }
            }
        }

        public FlexibleStringExpander getGlobalExdr() {
            return globalExdr;
        }

        public FlexibleMapAccessor<ResourceBundleMapWrapper> getMapNameAcsr() {
            return mapNameAcsr;
        }

        public FlexibleStringExpander getResourceExdr() {
            return resourceExdr;
        }
    }

    /**
     * Models the &lt;property-to-field&gt; element.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class PropertyToField extends AbstractModelAction {
        private final FlexibleMapAccessor<List<? extends Object>> argListAcsr;
        private final FlexibleStringExpander defaultExdr;
        private final FlexibleMapAccessor<Object> fieldAcsr;
        private final FlexibleStringExpander globalExdr;
        private final boolean noLocale;
        private final FlexibleStringExpander propertyExdr;
        private final FlexibleStringExpander resourceExdr;

        public PropertyToField(ModelWidget modelWidget, Element setElement) {
            super(modelWidget, setElement);
            this.resourceExdr = FlexibleStringExpander.getInstance(setElement.getAttribute("resource"));
            this.propertyExdr = FlexibleStringExpander.getInstance(setElement.getAttribute("property"));
            this.fieldAcsr = FlexibleMapAccessor.getInstance(setElement.getAttribute("field"));
            this.defaultExdr = FlexibleStringExpander.getInstance(setElement.getAttribute("default"));
            this.noLocale = "true".equals(setElement.getAttribute("no-locale"));
            this.argListAcsr = FlexibleMapAccessor.getInstance(setElement.getAttribute("arg-list-name"));
            this.globalExdr = FlexibleStringExpander.getInstance(setElement.getAttribute("global"));
        }

        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        @Override
        public void runAction(Map<String, Object> context) {
            //String globalStr = this.globalExdr.expandString(context);
            // default to false
            //boolean global = "true".equals(globalStr);
            Locale locale = (Locale) context.get("locale");
            String resource = this.resourceExdr.expandString(context, locale);
            String property = this.propertyExdr.expandString(context, locale);
            String value = null;
            if (noLocale) {
                value = EntityUtilProperties.getPropertyValue(resource, property, WidgetWorker.getDelegator(context));
            } else {
                value = EntityUtilProperties.getMessage(resource, property, locale, WidgetWorker.getDelegator(context));
            }
            if (UtilValidate.isEmpty(value)) {
                value = this.defaultExdr.expandString(context);
            }
            // note that expanding the value string here will handle defaultValue and the string from
            //  the properties file; if we decide later that we don't want the string from the properties
            //  file to be expanded we should just expand the defaultValue at the beginning of this method.
            value = FlexibleStringExpander.expandString(value, context);
            if (!argListAcsr.isEmpty()) {
                List<? extends Object> argList = argListAcsr.get(context);
                if (UtilValidate.isNotEmpty(argList)) {
                    value = MessageFormat.format(value, argList.toArray());
                }
            }
            fieldAcsr.put(context, value);
        }

        public FlexibleMapAccessor<List<? extends Object>> getArgListAcsr() {
            return argListAcsr;
        }

        public FlexibleStringExpander getDefaultExdr() {
            return defaultExdr;
        }

        public FlexibleMapAccessor<Object> getFieldAcsr() {
            return fieldAcsr;
        }

        public FlexibleStringExpander getGlobalExdr() {
            return globalExdr;
        }

        public boolean getNoLocale() {
            return noLocale;
        }

        public FlexibleStringExpander getPropertyExdr() {
            return propertyExdr;
        }

        public FlexibleStringExpander getResourceExdr() {
            return resourceExdr;
        }
    }

    /**
     * Models the &lt;script&gt; element.
     * <p>
     * SCIPIO: 2016-11-10: Extended to support inline scripts using code from
     * {@link org.ofbiz.minilang.method.callops.CallScript}
     * <p>
     * TODO: The child CDATA text should use a proper cache key instead of the
     * script body as ofbiz(?) coded it (should use: widgetloc#widgetname@line,col).
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class Script extends AbstractModelAction {
        
        /**
         * SCIPIO: available languages. NOTE: may not same as XSD; this is more permissive.
         */
        public static final Set<String> supportedLangs;
        static {
            Set<String> langSet = new HashSet<>();
            langSet.addAll(ScriptUtil.SCRIPT_NAMES);
            langSet.add("simple-method");
            langSet.add("simple-map-processor");
            supportedLangs = Collections.unmodifiableSet(langSet);
        }
        
        // SCIPIO: these are patched for dynamic location support
        //private final String location;
        //private final String method;
        private final FlexibleStringExpander locationExdr;
        private final Scriptlet scriptlet; // SCIPIO: imported from CallScript

        public Script(ModelWidget modelWidget, Element scriptElement) {
            super(modelWidget, scriptElement);
            String scriptLocation = scriptElement.getAttribute("location");
            //this.location = WidgetWorker.getScriptLocation(scriptLocation);
            //this.method = WidgetWorker.getScriptMethodName(scriptLocation);
            this.locationExdr = FlexibleStringExpander.getInstance(scriptLocation);
            
            // SCIPIO: inline script preparation derived from (but no longer resembles):
            //   org.ofbiz.minilang.method.callops.CallScript.CallScript(Element, SimpleMethod)
            String lang = scriptElement.getAttribute("lang");
            if (!lang.isEmpty() && !supportedLangs.contains(lang)) {
                Debug.logError("script element: lang attribute unrecognized language: lang=\"" + lang + "\"", module);
            }
            
            String inlineScript = scriptElement.getAttribute("script");
            boolean hasScriptPrefix = startsWithLangPrefix(inlineScript);
            Scriptlet scriptlet = null;
            if (hasScriptPrefix || (!inlineScript.isEmpty() && !lang.isEmpty())) {
                // use script attribute
                if (!hasScriptPrefix) {
                    inlineScript = lang + ":" + inlineScript;
                }
                inlineScript = StringUtil.convertOperatorSubstitutions(inlineScript);
                scriptlet = makeScriptlet(inlineScript);
            } else {
                if (!inlineScript.isEmpty()) {
                    Debug.logError("script element: script attribute contains code of unspecified"
                            + " or unknown language: script=\"" + inlineScript + "\"", module);
                }
                inlineScript = UtilXml.elementValue(scriptElement);
                if (UtilValidate.isNotEmpty(inlineScript)) {
                    hasScriptPrefix = startsWithLangPrefix(inlineScript);
                    if (hasScriptPrefix || (!inlineScript.isEmpty() && !lang.isEmpty())) {
                        // use script child body
                        if (!hasScriptPrefix) {
                            inlineScript = lang + ":" + inlineScript;
                        }
                        boolean trimLines = "true".equals(scriptElement.getAttribute("trim-lines"));
                        if (trimLines) {
                            inlineScript = ScriptUtil.trimScriptLines(inlineScript);
                        }
                        // SCIPIO: NOTE: do NOT convert keyword operators when using elem body!
                        scriptlet = makeScriptlet(inlineScript);
                    } else {
                        if (!inlineScript.isEmpty()) {
                            Debug.logError("script element: script body/child contains code of unspecified"
                                    + " or unknown language: script=\"" + inlineScript + "\"", module);
                        }
                    }
                }
            }
            this.scriptlet = scriptlet;
        }
        
        /**
         * SCIPIO: Returns <code>true</code> if <code>str</code> starts with a recognized lang prefix.
         * @param str The string to test
         * @return <code>true</code> if <code>str</code> starts with a recognized lang prefix
         */
        public static boolean startsWithLangPrefix(String str) {
            if (str.length() > 0) {
                for (String scriptPrefix : supportedLangs) {
                    if (str.startsWith(scriptPrefix)) {
                        return true;
                    }
                }
            }
            return false;
        }
        
        /**
         * SCIPIO: makes scriptlet.
         */
        private static Scriptlet makeScriptlet(String inlineScript) {
            if (MiniLangUtil.startsWithScriptPrefix(inlineScript)) {
                return new Scriptlet(inlineScript);
            } else {
                // TODO: simple-method, simple-map-processor
                int colon = inlineScript.indexOf(':');
                String lang;
                lang = (colon > 0) ? inlineScript.substring(0, colon) : "unknown";
                throw new UnsupportedOperationException("script element: does not yet "
                        + "support inline scripts for lang [" + lang + "]");
            }
        }
        
        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        @Override
        public void runAction(Map<String, Object> context) throws GeneralException {
            if (!this.locationExdr.getOriginal().isEmpty()) {
                String scriptLocation = this.locationExdr.expandString(context);
                String location = WidgetWorker.getScriptLocation(scriptLocation);
                String method = WidgetWorker.getScriptMethodName(scriptLocation);
                
                if (location.endsWith(".xml")) {
                    Map<String, Object> localContext = new HashMap<String, Object>();
                    localContext.putAll(context);
                    DispatchContext ctx = WidgetWorker.getDispatcher(context).getDispatchContext();
                    MethodContext methodContext = new MethodContext(ctx, localContext, null);
                    try {
                        SimpleMethod.runSimpleMethod(location, method, methodContext);
                        context.putAll(methodContext.getResults());
                    } catch (MiniLangException e) {
                        throw new GeneralException("Error running simple method at location [" + location + "]", e);
                    }
                } else {
                    ScriptUtil.executeScript(location, method, context);
                }
            }
            
            if (this.scriptlet != null) {
                try {
                    this.scriptlet.executeScript(context);
                } catch (Exception e) {
                    throw new GeneralException("Error running inline script", e);
                }
            }
        }

        public String getLocation() {
            //return location;
            return WidgetWorker.getScriptLocation(locationExdr.getOriginal());
        }

        public String getMethod() {
            //return method;
            return WidgetWorker.getScriptMethodName(locationExdr.getOriginal());
        }
    }

    /**
     * Models the &lt;service&gt; element.
     * 
     * @see <code>widget-screen.xsd</code>
     */
    public static class Service extends AbstractModelAction {
        private final FlexibleStringExpander autoFieldMapExdr;
        private final Map<FlexibleMapAccessor<Object>, Object> fieldMap;
        private final FlexibleMapAccessor<Map<String, Object>> resultMapNameAcsr;
        private final FlexibleStringExpander serviceNameExdr;

        public Service(ModelWidget modelWidget, Element serviceElement) {
            super(modelWidget, serviceElement);
            this.serviceNameExdr = FlexibleStringExpander.getInstance(serviceElement.getAttribute("service-name"));
            this.resultMapNameAcsr = FlexibleMapAccessor.getInstance(serviceElement.getAttribute("result-map"));
            this.autoFieldMapExdr = FlexibleStringExpander.getInstance(serviceElement.getAttribute("auto-field-map"));
            this.fieldMap = EntityFinderUtil.makeFieldMap(serviceElement);
        }

        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleStringExpander getServiceNameExdr() {
            return this.serviceNameExdr;
        }

        @Override
        public void runAction(Map<String, Object> context) {
            String serviceNameExpanded = this.serviceNameExdr.expandString(context);
            if (UtilValidate.isEmpty(serviceNameExpanded)) {
                throw new IllegalArgumentException("Service name was empty, expanded from: " + this.serviceNameExdr.getOriginal());
            }
            String autoFieldMapString = this.autoFieldMapExdr.expandString(context);
            try {
                Map<String, Object> serviceContext = null;
                if ("true".equals(autoFieldMapString)) {
                    DispatchContext dc = WidgetWorker.getDispatcher(context).getDispatchContext();
                    // try a map called "parameters", try it first so values from here are overriden by values in the main context
                    Map<String, Object> combinedMap = new HashMap<String, Object>();
                    Map<String, Object> parametersObj = UtilGenerics.toMap(context.get("parameters"));
                    if (parametersObj != null) {
                        combinedMap.putAll(parametersObj);
                    }
                    combinedMap.putAll(context);
                    serviceContext = dc.makeValidContext(serviceNameExpanded, ModelService.IN_PARAM, combinedMap);
                } else if (UtilValidate.isNotEmpty(autoFieldMapString) && !"false".equals(autoFieldMapString)) {
                    FlexibleMapAccessor<Object> fieldFma = FlexibleMapAccessor.getInstance(autoFieldMapString);
                    Map<String, Object> autoFieldMap = UtilGenerics.toMap(fieldFma.get(context));
                    if (autoFieldMap != null) {
                        serviceContext = WidgetWorker.getDispatcher(context).getDispatchContext()
                                .makeValidContext(serviceNameExpanded, ModelService.IN_PARAM, autoFieldMap);
                    }
                }
                if (serviceContext == null) {
                    serviceContext = new HashMap<String, Object>();
                }
                if (this.fieldMap != null) {
                    EntityFinderUtil.expandFieldMapToContext(this.fieldMap, context, serviceContext);
                }
                Map<String, Object> result = WidgetWorker.getDispatcher(context).runSync(serviceNameExpanded, serviceContext);
                if (!this.resultMapNameAcsr.isEmpty()) {
                    this.resultMapNameAcsr.put(context, result);
                    String queryString = (String) result.get("queryString");
                    context.put("queryString", queryString);
                    context.put("queryStringMap", result.get("queryStringMap"));
                    if (UtilValidate.isNotEmpty(queryString)) {
                        try {
                            String queryStringEncoded = queryString.replaceAll("&", "%26");
                            context.put("queryStringEncoded", queryStringEncoded);
                        } catch (PatternSyntaxException e) {

                        }
                    }
                } else {
                    context.putAll(result);
                }
            } catch (GenericServiceException e) {
                String errMsg = "Error calling service with name " + serviceNameExpanded + ": " + e.toString();
                Debug.logError(e, errMsg, module);
                throw new IllegalArgumentException(errMsg);
            }
        }

        public FlexibleStringExpander getAutoFieldMapExdr() {
            return autoFieldMapExdr;
        }

        public Map<FlexibleMapAccessor<Object>, Object> getFieldMap() {
            return fieldMap;
        }

        public FlexibleMapAccessor<Map<String, Object>> getResultMapNameAcsr() {
            return resultMapNameAcsr;
        }
    }

    /**
     * Models the &lt;set&gt; element.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class SetField extends AbstractModelAction {
        private final FlexibleStringExpander defaultExdr;
        private final FlexibleMapAccessor<Object> field;
        private final FlexibleMapAccessor<Object> fromField;
        private final String fromScope;
        private final FlexibleStringExpander globalExdr;
        private final String toScope;
        private final String type;
        private final FlexibleStringExpander valueExdr;

        public SetField(ModelWidget modelWidget, Element setElement) {
            super(modelWidget, setElement);
            this.field = FlexibleMapAccessor.getInstance(setElement.getAttribute("field"));
            this.fromField = FlexibleMapAccessor.getInstance(setElement.getAttribute("from-field"));
            this.valueExdr = FlexibleStringExpander.getInstance(setElement.getAttribute("value"));
            this.defaultExdr = FlexibleStringExpander.getInstance(setElement.getAttribute("default-value"));
            this.globalExdr = FlexibleStringExpander.getInstance(setElement.getAttribute("global"));
            this.type = setElement.getAttribute("type");
            this.toScope = setElement.getAttribute("to-scope");
            this.fromScope = setElement.getAttribute("from-scope");
            if (!this.fromField.isEmpty() && !this.valueExdr.isEmpty()) {
                throw new IllegalArgumentException("Cannot specify a from-field [" + setElement.getAttribute("from-field")
                        + "] and a value [" + setElement.getAttribute("value") + "] on the set action in a widget");
            }
        }

        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public Object getInMemoryPersistedFromField(Object storeAgent, Map<String, Object> context) {
            Object newValue = null;
            String originalName = this.fromField.getOriginalName();
            List<String> currentWidgetTrail = UtilGenerics.toList(context.get("_WIDGETTRAIL_"));
            List<String> trailList = new ArrayList<String>();
            if (currentWidgetTrail != null) {
                trailList.addAll(currentWidgetTrail);
            }
            for (int i = trailList.size(); i >= 0; i--) {
                List<String> subTrail = trailList.subList(0, i);
                String newKey = null;
                if (subTrail.size() > 0)
                    newKey = StringUtil.join(subTrail, "|") + "|" + originalName;
                else
                    newKey = originalName;
                if (storeAgent instanceof ServletContext) {
                    newValue = ((ServletContext) storeAgent).getAttribute(newKey);
                } else if (storeAgent instanceof HttpSession) {
                    newValue = ((HttpSession) storeAgent).getAttribute(newKey);
                }
                if (newValue != null) {
                    break;
                }
            }
            return newValue;
        }

        @SuppressWarnings("rawtypes")
        @Override
        public void runAction(Map<String, Object> context) {
            String globalStr = this.globalExdr.expandString(context);
            // default to false
            boolean global = "true".equals(globalStr);
            Object newValue = null;
            if (this.fromScope != null && this.fromScope.equals("user")) {
                if (!this.fromField.isEmpty()) {
                    HttpSession session = (HttpSession) context.get("session");
                    newValue = getInMemoryPersistedFromField(session, context);
                    if (Debug.verboseOn())
                        Debug.logVerbose("In user getting value for field from [" + this.fromField.getOriginalName() + "]: "
                                + newValue, module);
                } else if (!this.valueExdr.isEmpty()) {
                    newValue = this.valueExdr.expand(context);
                }
            } else if (this.fromScope != null && this.fromScope.equals("application")) {
                if (!this.fromField.isEmpty()) {
                    ServletContext servletContext = (ServletContext) context.get("application");
                    newValue = getInMemoryPersistedFromField(servletContext, context);
                    if (Debug.verboseOn())
                        Debug.logVerbose("In application getting value for field from [" + this.fromField.getOriginalName()
                                + "]: " + newValue, module);
                } else if (!this.valueExdr.isEmpty()) {
                    newValue = this.valueExdr.expandString(context);
                }
            } else {
                if (!this.fromField.isEmpty()) {
                    newValue = this.fromField.get(context);
                    if (Debug.verboseOn())
                        Debug.logVerbose("Getting value for field from [" + this.fromField.getOriginalName() + "]: " + newValue,
                                module);
                } else if (!this.valueExdr.isEmpty()) {
                    newValue = this.valueExdr.expand(context);
                }
            }
            // If newValue is still empty, use the default value
            if (ObjectType.isEmpty(newValue) && !this.defaultExdr.isEmpty()) {
                newValue = this.defaultExdr.expand(context);
            }
            if (UtilValidate.isNotEmpty(this.type)) {
                if ("NewMap".equals(this.type)) {
                    newValue = new HashMap();
                } else if ("NewList".equals(this.type)) {
                    newValue = new LinkedList();
                } else {
                    try {
                        newValue = ObjectType.simpleTypeConvert(newValue, this.type, null, (TimeZone) context.get("timeZone"),
                                (Locale) context.get("locale"), true);
                    } catch (GeneralException e) {
                        String errMsg = "Could not convert field value for the field: [" + this.field.getOriginalName()
                                + "] to the [" + this.type + "] type for the value [" + newValue + "]: " + e.toString();
                        Debug.logError(e, errMsg, module);
                        throw new IllegalArgumentException(errMsg);
                    }
                }
            }
            if (this.toScope != null && this.toScope.equals("user")) {
                String originalName = this.field.getOriginalName();
                List<String> currentWidgetTrail = UtilGenerics.toList(context.get("_WIDGETTRAIL_"));
                String newKey = "";
                if (currentWidgetTrail != null) {
                    newKey = StringUtil.join(currentWidgetTrail, "|");
                }
                if (UtilValidate.isNotEmpty(newKey)) {
                    newKey += "|";
                }
                newKey += originalName;
                HttpSession session = (HttpSession) context.get("session");
                session.setAttribute(newKey, newValue);
                if (Debug.verboseOn())
                    Debug.logVerbose("In user setting value for field from [" + this.field.getOriginalName() + "]: " + newValue,
                            module);
            } else if (this.toScope != null && this.toScope.equals("application")) {
                String originalName = this.field.getOriginalName();
                List<String> currentWidgetTrail = UtilGenerics.toList(context.get("_WIDGETTRAIL_"));
                String newKey = "";
                if (currentWidgetTrail != null) {
                    newKey = StringUtil.join(currentWidgetTrail, "|");
                }
                if (UtilValidate.isNotEmpty(newKey)) {
                    newKey += "|";
                }
                newKey += originalName;
                ServletContext servletContext = (ServletContext) context.get("application");
                servletContext.setAttribute(newKey, newValue);
                if (Debug.verboseOn())
                    Debug.logVerbose("In application setting value for field from [" + this.field.getOriginalName() + "]: "
                            + newValue, module);
            } else {
                // only do this if it is not global, if global ONLY put it in the global context
                if (!global) {
                    if (Debug.verboseOn())
                        Debug.logVerbose("Setting field [" + this.field.getOriginalName() + "] to value: " + newValue, module);
                    this.field.put(context, newValue);
                }
            }
            if (global) {
                Map<String, Object> globalCtx = UtilGenerics.checkMap(context.get("globalContext"));
                if (globalCtx != null) {
                    this.field.put(globalCtx, newValue);
                } else {
                    this.field.put(context, newValue);
                }
            }
            // this is a hack for backward compatibility with the JPublish page object
            Map<String, Object> page = UtilGenerics.checkMap(context.get("page"));
            if (page != null) {
                this.field.put(page, newValue);
            }
        }

        public FlexibleStringExpander getDefaultExdr() {
            return defaultExdr;
        }

        public FlexibleMapAccessor<Object> getField() {
            return field;
        }

        public FlexibleMapAccessor<Object> getFromField() {
            return fromField;
        }

        public String getFromScope() {
            return fromScope;
        }

        public FlexibleStringExpander getGlobalExdr() {
            return globalExdr;
        }

        public String getToScope() {
            return toScope;
        }

        public String getType() {
            return type;
        }

        public FlexibleStringExpander getValueExdr() {
            return valueExdr;
        }
    }
    
    /**
     * SCIPIO: Models the &lt;include-xxx-actions&gt; elements.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static abstract class IncludeActions extends AbstractModelAction {
        private static final Map<String, String> elemTypeMap;
        static {
            Map<String, String> map = new HashMap<>();
            map.put("include-screen-actions", "screen");
            map.put("include-form-actions", "form");
            map.put("include-form-row-actions", "form-row");
            map.put("include-menu-actions", "menu");
            map.put("include-tree-actions", "tree");
            elemTypeMap = map;
        }

        protected IncludeActions(ModelWidget modelWidget, Element element) {
            super(modelWidget, element);
        }
        
        protected final String getDefaultLocation(Element element, String location) {
            String defaultLocation = WidgetDocumentInfo.retrieveAlways(element).getResourceLocation();
            if (defaultLocation == null || !defaultLocation.startsWith("component://")) {
                if (UtilValidate.isEmpty(location)) {
                    Debug.logError("include-xxx-actions: Could not determine an original/default document location (in component:// format) " +
                            "for actions include directive (found: " + defaultLocation + "), and the location " +
                            "attribute expression is empty; the include directive will fail", module);
                } else {
                    Debug.logWarning("include-xxx-actions: Could not determine an original/default document location (in component:// format) " +
                            "for actions include directive (found: " + defaultLocation + "); " +
                            "relative locations for the location attribute expression are unsupported in this case and will fail if used", module);
                }
                if (defaultLocation == null) {
                    defaultLocation = "";
                }
            }
            return defaultLocation;
        }
        
        public static IncludeActions newInstance(ModelWidget modelWidget, Element element, String type) {
            DynamicIncludeActions dynInclude = DynamicIncludeActions.newInstance(modelWidget, element, type);
            if (dynInclude.isVariableInclude()) {
                return dynInclude;
            } else {
                // if static include, use optimized
                return StaticIncludeActions.newInstance(modelWidget, element, dynInclude);
            }
        }
        
        public static IncludeActions newInstance(ModelWidget modelWidget, Element element) {
            return newInstance(modelWidget, element, getIncludeActionsType(element));
        }
        
        public static String getIncludeActionsType(Element element) {
            return elemTypeMap.get(element.getTagName());
        }
        
        public static boolean isIncludeActions(Element element) {
            return elemTypeMap.containsKey(element.getTagName());
        }

        public abstract List<ModelAction> getIncludedActions(Map<String, Object> context) throws GeneralException;
        
        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            // TODO
        }
    }
    
    /**
     * SCIPIO: Models the &lt;include-xxx-actions&gt; elements containing only static include expressions
     * (constants, no ${}).
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static abstract class StaticIncludeActions extends IncludeActions {

        protected StaticIncludeActions(ModelWidget modelWidget, Element element) {
            super(modelWidget, element);
        }
        
        public static StaticIncludeActions newInstance(ModelWidget modelWidget, Element element) {
            return LazyStaticIncludeActions.newInstance(modelWidget, element);
        }
        
        public static StaticIncludeActions newInstance(ModelWidget modelWidget, Element element, DynamicIncludeActions dynInclude) {
            return LazyStaticIncludeActions.newInstance(modelWidget, element, dynInclude);
        }

        @Override
        public void runAction(Map<String, Object> context) throws GeneralException {
            AbstractModelAction.runSubActions(getIncludedActions(context), context); // NOTE: wraps in RunTimeExceptions
        }
    }
    
    /**
     * SCIPIO: Models the &lt;include-xxx-actions&gt; elements containing only static includes
     * with a lazy caching static implementation.
     * <p>
     * optimizes xml includes that have only constant location#name.
     * Due to complications it delegates the lookup to first run and caches.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class LazyStaticIncludeActions extends StaticIncludeActions {
        private final DynamicIncludeActions dynInclude;
        private List<ModelAction> actions = null; // WARN: not volatile because using double-locking idiom
        
        protected LazyStaticIncludeActions(ModelWidget modelWidget, Element element) {
            super(modelWidget, element);
            this.dynInclude = DynamicIncludeActions.newInstance(modelWidget, element);
            if (this.dynInclude.isVariableInclude()) {
                throw new IllegalArgumentException("static include-xxx-actions implementation can't "
                        + "contain a dynamic location#name expression");
            }
        }
        
        protected LazyStaticIncludeActions(ModelWidget modelWidget, Element element, DynamicIncludeActions dynInclude) {
            super(modelWidget, element);
            this.dynInclude = dynInclude;
            if (this.dynInclude.isVariableInclude()) {
                throw new IllegalArgumentException("static include-xxx-actions implementation can't "
                        + "contain a dynamic location#name expression");
            }
        }
        
        public static LazyStaticIncludeActions newInstance(ModelWidget modelWidget, Element element) {
            return new LazyStaticIncludeActions(modelWidget, element);
        }
        
        public static LazyStaticIncludeActions newInstance(ModelWidget modelWidget, Element element, DynamicIncludeActions dynInclude) {
            return new LazyStaticIncludeActions(modelWidget, element, dynInclude);
        }
        
        @Override
        public List<ModelAction> getIncludedActions(Map<String, Object> context) throws GeneralException {
            // WARN: special double-locking idiom for thread safety
            List<ModelAction> actions = this.actions;
            if (actions == null) {
                synchronized (this) {
                    actions = this.actions;
                    if (actions == null) {
                        // WARN: unmodifiableList is REQUIRED for thread safety, but all of the
                        // classes already return it that way, so this could double-wrap it
                        //actions = Collections.unmodifiableList(dynInclude.getIncludedActions(context));
                        actions = dynInclude.getIncludedActions(context);
                        this.actions = actions;
                    }
                }
            }
            return actions;
        }
        
    }
    
    /**
     * SCIPIO: Models the &lt;include-xxx-actions&gt; elements supporting dynamic expressions (${}).
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static abstract class DynamicIncludeActions extends IncludeActions {
        private final FlexibleStringExpander nameExdr;
        private final FlexibleStringExpander locationExdr;
        private final String defaultLocation; // the file where this action was originally defined

        protected DynamicIncludeActions(ModelWidget modelWidget, Element element) {
            super(modelWidget, element);
            this.nameExdr = FlexibleStringExpander.getInstance(element.getAttribute("name"));
            this.locationExdr = FlexibleStringExpander.getInstance(element.getAttribute("location"));
            this.defaultLocation = getDefaultLocation(element, this.locationExdr.getOriginal());
        }
        
        public static DynamicIncludeActions newInstance(ModelWidget modelWidget, Element element, String type) {
            DynamicIncludeActions includeActions;
            if ("screen".equals(type)) {
                includeActions = new IncludeScreenActions(modelWidget, element);
            } else if ("form".equals(type)) {
                includeActions = new IncludeFormActions(modelWidget, element);
            } else if ("form-row".equals(type)) {
                includeActions = new IncludeFormRowActions(modelWidget, element);    
            } else if ("menu".equals(type)) {
                includeActions = new IncludeMenuActions(modelWidget, element);
            } else if ("tree".equals(type)) {
                includeActions = new IncludeTreeActions(modelWidget, element);
            } else {
                throw new IllegalArgumentException("include-xxx-actions: Unrecognized actions include type: " + type);
            }
            return includeActions;
        }
        
        public static DynamicIncludeActions newInstance(ModelWidget modelWidget, Element element) {
            return newInstance(modelWidget, element, getIncludeActionsType(element));
        }
        
        @Override
        public void runAction(Map<String, Object> context) throws GeneralException {
            List<ModelAction> actions;
            try {
                actions = getIncludedActions(context, getLocation(context), getName(context));
            } catch (GeneralException e) {
                throw e;
            } catch (RuntimeException e) {
                throw e;
            } catch (Exception e) {
                throw new GeneralException(e);
            }
            AbstractModelAction.runSubActions(actions, context); // NOTE: wraps in RunTimeExceptions
        }
        
        @Override
        public List<ModelAction> getIncludedActions(Map<String, Object> context) throws GeneralException {
            try {
                return getIncludedActions(context, getLocation(context), getName(context));
            } catch (GeneralException e) {
                throw e;
            } catch (RuntimeException e) {
                throw e;
            } catch (Exception e) {
                throw new GeneralException(e);
            }
        }
        
        protected abstract List<ModelAction> getIncludedActions(Map<String, Object> context,
                String location, String name) throws Exception;
        
        /**
         * Returns true if location#name contains ${} expression.
         */
        public boolean isVariableInclude() {
            return FlexibleStringExpander.containsExpression(nameExdr) ||
                    FlexibleStringExpander.containsExpression(locationExdr);
        }
        
        /**
         * Returns true if location#name is a constant.
         * <p>
         * If this returns true, then you could use a StaticIncludeActions instance instead.
         */
        public boolean isStaticInclude() {
            return !isVariableInclude();
        }
        
        public FlexibleStringExpander getNameExdr() {
            return this.nameExdr;
        }
        
        public String getName(Map<String, Object> context) {
            return this.nameExdr.expandString(context);
        }

        public FlexibleStringExpander getLocationExdr() {
            return this.locationExdr;
        }

        public String getDefaultLocation() {
            return this.defaultLocation;
        }
        
        public String getLocation(Map<String, Object> context) {
            String location = this.locationExdr.expandString(context);
            if (UtilValidate.isEmpty(location)) {
                location = this.getDefaultLocation();
            }
            return location;
        }
    }
    
    /**
     * SCIPIO: Models the &lt;include-screen-actions&gt; element.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class IncludeScreenActions extends DynamicIncludeActions {
        public IncludeScreenActions(ModelWidget modelWidget, Element element) {
            super(modelWidget, element);
        }

        @Override
        protected List<ModelAction> getIncludedActions(Map<String, Object> context, String location, String name) throws Exception {
            ModelScreen widget = ScreenFactory.getScreenFromLocation(location, name);
            return widget.getSection().getActions();
        }
    }
    
    /**
     * SCIPIO: Models the &lt;include-form-actions&gt; element.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class IncludeFormActions extends DynamicIncludeActions {
        public IncludeFormActions(ModelWidget modelWidget, Element element) {
            super(modelWidget, element);
        }

        @Override
        protected List<ModelAction> getIncludedActions(Map<String, Object> context, String location, String name) throws Exception {
            ModelForm widget = FormFactory.getFormFromLocation(location, name, 
                    ((Delegator) context.get("delegator")).getModelReader(), 
                    ((LocalDispatcher) context.get("dispatcher")).getDispatchContext());
            return widget.getActions();
        }
    }
    
    /**
     * SCIPIO: Models the &lt;include-form-row-actions&gt; element.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class IncludeFormRowActions extends DynamicIncludeActions {
        public IncludeFormRowActions(ModelWidget modelWidget, Element element) {
            super(modelWidget, element);
        }
        
        @Override
        protected List<ModelAction> getIncludedActions(Map<String, Object> context, String location, String name) throws Exception {
            ModelForm widget = FormFactory.getFormFromLocation(location, name, 
                    ((Delegator) context.get("delegator")).getModelReader(), 
                    ((LocalDispatcher) context.get("dispatcher")).getDispatchContext());
            return widget.getRowActions();
        }
    }
    
    /**
     * SCIPIO: Models the &lt;include-menu-actions&gt; element.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class IncludeMenuActions extends DynamicIncludeActions {
        public IncludeMenuActions(ModelWidget modelWidget, Element element) {
            super(modelWidget, element);
        }

        @Override
        protected List<ModelAction> getIncludedActions(Map<String, Object> context, String location, String name) throws Exception {
            ModelMenu widget = MenuFactory.getMenuFromLocation(location, name);
            return widget.getActions();
        }
    }
    
    /**
     * SCIPIO: Models the &lt;include-tree-actions&gt; element.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class IncludeTreeActions extends DynamicIncludeActions {

        public IncludeTreeActions(ModelWidget modelWidget, Element element) {
            super(modelWidget, element);
        }

        @Override
        protected List<ModelAction> getIncludedActions(Map<String, Object> context, String location, String name) throws Exception {
            ModelTree widget = TreeFactory.getTreeFromLocation(location, name, 
                    (Delegator) context.get("delegator"), (LocalDispatcher) context.get("dispatcher"));
            return widget.getRootActions();
        }
    }
    
    /**
     * SCIPIO: Models the &lt;condition-to-field&gt; element.
     * <p>
     * NOTE: largely derived from SetField.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class ConditionToField extends AbstractModelAction {
        
        private static final Map<String, Object> defaultPassValueMap;
        private static final Map<String, Object> defaultFailValueMap;
        static {
            Map<String, Object> map = new HashMap<>();
            map.put("Boolean", Boolean.TRUE);
            map.put("String", "true");
            map.put("PlainString", "true");
            map.put("Indicator", "Y");
            defaultPassValueMap = map;
            map = new HashMap<>();
            map.put("Boolean", Boolean.FALSE);
            map.put("String", "false");
            map.put("PlainString", "false");
            map.put("Indicator", "N");
            defaultFailValueMap = map;
        }
        
        private final ModelCondition condition;
        private final FlexibleMapAccessor<Object> field;
        private final FlexibleStringExpander globalExdr;
        private final String toScope;
        private final String type;
        private final FlexibleStringExpander passValueExdr;
        private final FlexibleStringExpander failValueExdr;
        private final FlexibleStringExpander useWhenExdr;
        private final boolean onlyIfFieldNull;
        private final boolean onlyIfFieldEmpty;

        public ConditionToField(ModelWidget modelWidget, Element condToFieldElement) {
            super(modelWidget, condToFieldElement);
            this.field = FlexibleMapAccessor.getInstance(condToFieldElement.getAttribute("field"));
            this.globalExdr = FlexibleStringExpander.getInstance(condToFieldElement.getAttribute("global"));
            this.type = condToFieldElement.getAttribute("type");
            this.toScope = condToFieldElement.getAttribute("to-scope");
            this.passValueExdr = FlexibleStringExpander.getInstance(condToFieldElement.getAttribute("pass-value"));
            this.failValueExdr = FlexibleStringExpander.getInstance(condToFieldElement.getAttribute("fail-value"));
            this.useWhenExdr = FlexibleStringExpander.getInstance(condToFieldElement.getAttribute("use-when"));
            List<? extends Element> children = UtilXml.childElementList(condToFieldElement);
            this.condition = ModelScreenCondition.SCREEN_CONDITION_FACTORY.newInstance(modelWidget, 
                    UtilValidate.isNotEmpty(children) ? children.get(0) : null);
            this.onlyIfFieldNull = "null".equals(condToFieldElement.getAttribute("only-if-field"));
            this.onlyIfFieldEmpty = "empty".equals(condToFieldElement.getAttribute("only-if-field"));
        }

        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            // TODO
        }

        private static Object getDefaultVal(boolean condResult, String type) {
            if (UtilValidate.isEmpty(type)) {
                type = "String";
            }
            return condResult ? defaultPassValueMap.get(type) : defaultFailValueMap.get(type);
        }
        
        @Override
        public void runAction(Map<String, Object> context) {
            String globalStr = this.globalExdr.expandString(context);
            // default to false
            boolean global = "true".equals(globalStr);
            
            if (!this.useWhenExdr.getOriginal().isEmpty()) {
                String setIf = this.useWhenExdr.expandString(context);
                if ("false".equals(setIf)) {
                    return;
                } else if (!"true".equals(setIf)) {
                    throw new IllegalArgumentException("use-when expression '" + 
                            this.useWhenExdr.getOriginal() + "' in condition-to-field evaluated to non-boolean expression: " + setIf);
                }
            }
            if (onlyIfFieldNull || onlyIfFieldEmpty) {
                Object currValue;
                if (global) {
                    Map<String, Object> globalCtx = UtilGenerics.checkMap(context.get("globalContext"));
                    if (globalCtx != null) {
                        currValue = this.field.get(globalCtx);
                    } else {
                        currValue = this.field.get(context);
                    }
                } else {
                    currValue = this.field.get(context);
                }
                if ((onlyIfFieldNull && currValue == null) || (onlyIfFieldEmpty && ObjectType.isEmpty(currValue))) {
                    ; // we're good
                } else {
                    return; // skip
                }
            }

            boolean condResult = condition.eval(context);
            
            Object newValue = null;
            boolean convertType;
            if (condResult) {
                if (!this.passValueExdr.getOriginal().isEmpty()) {
                    convertType = true;
                    newValue = this.passValueExdr.expandString(context);
                } else {
                    convertType = false; // optimized
                    newValue = getDefaultVal(condResult, this.type);
                }
            } else {
                if (!this.failValueExdr.getOriginal().isEmpty()) {
                    convertType = true;
                    newValue = this.failValueExdr.expandString(context);
                } else {
                    convertType = false; // optimized
                    newValue = getDefaultVal(condResult, this.type);
                }
            }
            
            // SCIPIO: NOTE: code below is copied from SetField.runAction
            // TODO: r
            
            if (convertType && UtilValidate.isNotEmpty(this.type)) {
                try {
                    newValue = ObjectType.simpleTypeConvert(newValue, this.type, null, (TimeZone) context.get("timeZone"),
                            (Locale) context.get("locale"), true);
                } catch (GeneralException e) {
                    String errMsg = "Could not convert field value for the field: [" + this.field.getOriginalName()
                            + "] to the [" + this.type + "] type for the value [" + newValue + "]: " + e.toString();
                    Debug.logError(e, errMsg, module);
                    throw new IllegalArgumentException(errMsg);
                }
                
            }
            if (this.toScope != null && this.toScope.equals("user")) {
                String originalName = this.field.getOriginalName();
                List<String> currentWidgetTrail = UtilGenerics.toList(context.get("_WIDGETTRAIL_"));
                String newKey = "";
                if (currentWidgetTrail != null) {
                    newKey = StringUtil.join(currentWidgetTrail, "|");
                }
                if (UtilValidate.isNotEmpty(newKey)) {
                    newKey += "|";
                }
                newKey += originalName;
                HttpSession session = (HttpSession) context.get("session");
                session.setAttribute(newKey, newValue);
                if (Debug.verboseOn())
                    Debug.logVerbose("In user setting value for field from [" + this.field.getOriginalName() + "]: " + newValue,
                            module);
            } else if (this.toScope != null && this.toScope.equals("application")) {
                String originalName = this.field.getOriginalName();
                List<String> currentWidgetTrail = UtilGenerics.toList(context.get("_WIDGETTRAIL_"));
                String newKey = "";
                if (currentWidgetTrail != null) {
                    newKey = StringUtil.join(currentWidgetTrail, "|");
                }
                if (UtilValidate.isNotEmpty(newKey)) {
                    newKey += "|";
                }
                newKey += originalName;
                ServletContext servletContext = (ServletContext) context.get("application");
                servletContext.setAttribute(newKey, newValue);
                if (Debug.verboseOn())
                    Debug.logVerbose("In application setting value for field from [" + this.field.getOriginalName() + "]: "
                            + newValue, module);
            } else {
                // only do this if it is not global, if global ONLY put it in the global context
                if (!global) {
                    if (Debug.verboseOn())
                        Debug.logVerbose("Setting field [" + this.field.getOriginalName() + "] to value: " + newValue, module);
                    this.field.put(context, newValue);
                }
            }
            if (global) {
                Map<String, Object> globalCtx = UtilGenerics.checkMap(context.get("globalContext"));
                if (globalCtx != null) {
                    this.field.put(globalCtx, newValue);
                } else {
                    this.field.put(context, newValue);
                }
            }
            // this is a hack for backward compatibility with the JPublish page object
            Map<String, Object> page = UtilGenerics.checkMap(context.get("page"));
            if (page != null) {
                this.field.put(page, newValue);
            }
        }

        public ModelCondition getCondition() {
            return condition;
        }

        public FlexibleMapAccessor<Object> getField() {
            return field;
        }

        public FlexibleStringExpander getGlobalExdr() {
            return globalExdr;
        }

        public String getToScope() {
            return toScope;
        }

        public String getType() {
            return type;
        }

        public FlexibleStringExpander getPassValueExdr() {
            return passValueExdr;
        }

        public FlexibleStringExpander getFailValueExdr() {
            return failValueExdr;
        }

        public FlexibleStringExpander getUseWhenExdr() {
            return useWhenExdr;
        }

        public boolean isOnlyIfFieldNull() {
            return onlyIfFieldNull;
        }

        public boolean isOnlyIfFieldEmpty() {
            return onlyIfFieldEmpty;
        }

    }
    
    /**
     * Models the &lt;if&gt; element.
     * <p>
     * SCIPIO: 2016-11-11: Adapted from org.ofbiz.minilang.method.conditional.MasterIf.
     * 
     * @see <code>widget-common.xsd</code>
     */
    public static class MasterIf extends AbstractModelAction {
        public static final String TAG_NAME = "if";
        public static final String THEN_TAG_NAME = "then";
        public static final String ELSE_IF_TAG_NAME = "else-if";
        public static final String ELSE_TAG_NAME = "else";

        private final ModelCondition condition;
        private final List<ElseIf> elseIfs;
        private final List<ModelAction> elseSubOps;
        private final List<ModelAction> thenSubOps;

        public MasterIf(ModelWidget modelWidget, Element element) {
            super(modelWidget, element);
            ElementHelper elem = UtilXml.getElementHelper(element);
            this.condition = readCondition(modelWidget, elem, TAG_NAME);
            Element thenElement = elem.firstChildElement(THEN_TAG_NAME);
            this.thenSubOps = Collections.unmodifiableList(AbstractModelAction.readSubActions(modelWidget, thenElement));
            List<? extends Element> elseIfElements = elem.childElementList(ELSE_IF_TAG_NAME);
            if (elseIfElements.isEmpty()) {
                this.elseIfs = Collections.emptyList();
            } else {
                List<ElseIf> elseIfs = new ArrayList<ElseIf>(elseIfElements.size());
                for (Element elseIfElement : elseIfElements) {
                    elseIfs.add(new ElseIf(modelWidget, elseIfElement));
                }
                this.elseIfs = Collections.unmodifiableList(elseIfs);
            }
            Element elseElement = elem.firstChildElement(ELSE_TAG_NAME);
            if (elseElement == null) {
                this.elseSubOps = Collections.emptyList();
            } else {
                this.elseSubOps = Collections.unmodifiableList(AbstractModelAction.readSubActions(modelWidget, elseElement));
            }
        }
        
        /**
         * SCIPIO: read the condition element, then tries the condition attribute.
         */
        ModelCondition readCondition(ModelWidget modelWidget, ElementHelper elem, String directiveName) {
            Element conditionElement = elem.firstChildElement("condition");
            ModelCondition condition = null;
            Element conditionChildElement = UtilXml.firstChildElement(conditionElement);
            if (conditionChildElement != null) {
                condition = ModelScreenCondition.SCREEN_CONDITION_FACTORY.newInstance(modelWidget, conditionChildElement);
            }
            String conditionExdrStr = elem.attr("condition");
            if (!conditionExdrStr.isEmpty()) {
                if (condition == null) {
                    condition = ModelScreenCondition.makeBooleanExprCondition(modelWidget, 
                            FlexibleStringExpander.getInstance(conditionExdrStr));
                } else {
                    Debug.logError("Scipio: " + directiveName + " directive cannot have both condition element and condition attribute, will use only condition element" +
                            getLogDirectiveLocationString(), module);
                }
            }
            if (condition == null) {
                condition = AbstractModelCondition.FALSE_CONDITION;
                Debug.logError("Scipio: " + directiveName + " directive is missing condition in widget - will always evaluate to false" + getLogDirectiveLocationString(), module);
            }
            return condition;
        }
        
        @Override
        public void runAction(Map<String, Object> context) throws GeneralException {
            // if conditions fails, always return true; if a sub-op returns false
            // return false and stop, otherwise return true
            // return true;
            // only run subOps if element is empty/null
            boolean runSubOps = condition.eval(context);
            if (runSubOps) {
                AbstractModelAction.runSubActions(this.thenSubOps, context);
                return;
            } else {
                // try the else-ifs
                if (elseIfs != null) {
                    for (ElseIf elseIf : elseIfs) {
                        if (elseIf.eval(context)) {
                            elseIf.runAction(context);
                            return;
                        }
                    }
                }
                if (elseSubOps != null) {
                    AbstractModelAction.runSubActions(this.elseSubOps, context);
                    return;
                }
            }
        }

        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            // TODO
        }
        
        public ModelCondition getCondition() {
            return condition;
        }

        public List<ElseIf> getElseIfs() {
            return elseIfs;
        }

        public List<ModelAction> getElseSubOps() {
            return elseSubOps;
        }

        public List<ModelAction> getThenSubOps() {
            return thenSubOps;
        }
        
        /**
         * Models the &lt;else-if&gt; element.
         * <p>
         * SCIPIO: 2016-11-11: Adapted fromorg.ofbiz.minilang.method.conditional.ElseIf.
         * 
         * @see <code>widget-common.xsd</code>
         */
        public class ElseIf extends AbstractModelAction implements ModelCondition {
            private final ModelCondition condition;
            private final List<ModelAction> thenSubOps;
            
            public ElseIf(ModelWidget modelWidget, Element element) {
                super(modelWidget, element);
                ElementHelper elem = UtilXml.getElementHelper(element);
                this.condition = readCondition(modelWidget, elem, ELSE_IF_TAG_NAME);
                Element thenElement = elem.firstChildElement("then");
                this.thenSubOps = Collections.unmodifiableList(AbstractModelAction.readSubActions(modelWidget, thenElement));
            }

            @Override
            public void runAction(Map<String, Object> context) throws GeneralException {
                AbstractModelAction.runSubActions(this.thenSubOps, context);
            }
            
            @Override
            public boolean eval(Map<String, Object> context) {
                return condition.eval(context);
            }
            
            @Override
            public void accept(ModelActionVisitor visitor) throws Exception {
                // TODO
            }

            public ModelCondition getCondition() {
                return condition;
            }

            public List<ModelAction> getThenSubOps() {
                return thenSubOps;
            }

            @Override
            public void accept(ModelConditionVisitor visitor) throws Exception {
                // TODO Auto-generated method stub
            }
        }
    }
  
}

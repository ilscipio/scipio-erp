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

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.TimeZone;

import javax.servlet.ServletContext;
import javax.servlet.http.HttpSession;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.collections.FlexibleMapAccessor;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.w3c.dom.Element;

/**
 * Abstract menu action.
 */
@SuppressWarnings("serial")
public abstract class ModelMenuAction implements Serializable {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static List<ModelAction> readSubActions(ModelMenu modelMenu, Element parentElement) {
        List<? extends Element> actionElementList = UtilXml.childElementList(parentElement);
        List<ModelAction> actions = new ArrayList<>(actionElementList.size());
        for (Element actionElement : actionElementList) {
            // SCIPIO: 2019-02-04: The SetField element below appears completely deprecated and buggy,
            // and provides nothing over org.ofbiz.widget.model.AbstractModelAction.SetField.
            //if ("set".equals(actionElement.getNodeName())) {
            //    actions.add(new SetField(modelMenu, actionElement));
            //} else {
            actions.add(AbstractModelAction.newInstance(modelMenu, actionElement));
            //}
        }
        return Collections.unmodifiableList(actions);
    }

    /**
     * Models the &lt;set&gt; element.
     * @deprecated SCIPIO: 2019-02-04: This appears poorly maintained, deprecated and buggy, and appeared
     * to do nothing useful over what {@link org.ofbiz.widget.model.AbstractModelAction.SetField} does.
     *
     * @see <code>widget-common.xsd</code>
     */
    @Deprecated
    public static class SetField extends AbstractModelAction {
        private final FlexibleMapAccessor<Object> field;
        private final FlexibleMapAccessor<Object> fromField;
        private final FlexibleStringExpander valueExdr;
        private final FlexibleStringExpander defaultExdr;
        private final FlexibleStringExpander globalExdr;
        private final String type;
        private final String toScope;
        private final String fromScope;

        public SetField(ModelMenu modelMenu, Element setElement) {
            super (modelMenu, setElement);
            this.field = FlexibleMapAccessor.getInstance(setElement.getAttribute("field"));
            this.fromField = FlexibleMapAccessor.getInstance(setElement.getAttribute("from-field"));
            this.valueExdr = FlexibleStringExpander.getInstance(setElement.getAttribute("value"));
            this.defaultExdr = UtilValidate.isNotEmpty(setElement.getAttribute("default-value")) ? FlexibleStringExpander.getInstance(setElement.getAttribute("default-value")) : null;
            this.globalExdr = FlexibleStringExpander.getInstance(setElement.getAttribute("global"));
            this.type = setElement.getAttribute("type");
            this.toScope = setElement.getAttribute("to-scope");
            this.fromScope = setElement.getAttribute("from-scope");
            if (!this.fromField.isEmpty() && !this.valueExdr.isEmpty()) {
                throw new IllegalArgumentException("Cannot specify a from-field [" + setElement.getAttribute("from-field") + "] and a value [" + setElement.getAttribute("value") + "] on the set action in a screen widget");
            }
        }

        @SuppressWarnings("rawtypes")
        @Override
        public void runAction(Map<String, Object> context) {
            String globalStr = this.globalExdr.expandString(context);
            // default to false
            boolean global = "true".equals(globalStr);

            Object newValue = null;
            if (this.fromScope != null && "user".equals(this.fromScope)) {
                if (!this.fromField.isEmpty()) {
                    String originalName = this.fromField.getOriginalName();
                    String currentWidgetTrail = (String)context.get("_WIDGETTRAIL_");
                    String newKey = currentWidgetTrail + "|" + originalName;
                    HttpSession session = (HttpSession)context.get("session");
                    newValue = session.getAttribute(newKey);
                    if (Debug.verboseOn()) {
                        Debug.logVerbose("In user getting value for field from [" + this.fromField.getOriginalName() + "]: " + newValue, module);
                    }
                } else if (!this.valueExdr.isEmpty()) {
                    newValue = this.valueExdr.expandString(context);
                }

            } else if (this.fromScope != null && "application".equals(this.fromScope)) {
                if (!this.fromField.isEmpty()) {
                    String originalName = this.fromField.getOriginalName();
                    String currentWidgetTrail = (String)context.get("_WIDGETTRAIL_");
                    String newKey = currentWidgetTrail + "|" + originalName;
                    ServletContext servletContext = (ServletContext)context.get("application");
                    newValue = servletContext.getAttribute(newKey);
                    if (Debug.verboseOn()) {
                        Debug.logVerbose("In application getting value for field from [" + this.fromField.getOriginalName() + "]: " + newValue, module);
                    }
                } else if (!this.valueExdr.isEmpty()) {
                    newValue = this.valueExdr.expandString(context);
                }

            } else {
                if (!this.fromField.isEmpty()) {
                    newValue = this.fromField.get(context);
                    if (Debug.verboseOn()) {
                        Debug.logVerbose("In screen getting value for field from [" + this.fromField.getOriginalName() + "]: " + newValue, module);
                    }
                } else if (!this.valueExdr.isEmpty()) {
                    newValue = this.valueExdr.expandString(context);
                }
            }

            // If newValue is still empty, use the default value
               if (this.defaultExdr != null) {
                   if (ObjectType.isEmpty(newValue)) {
                    newValue = this.defaultExdr.expandString(context);
                   }
            }

            if (UtilValidate.isNotEmpty(this.type)) {
                if ("NewMap".equals(this.type)) {
                    newValue = new HashMap();
                } else if ("NewList".equals(this.type)) {
                    newValue = new LinkedList();
                } else {
                    try {
                        newValue = ObjectType.simpleTypeConvert(newValue, this.type, null, (TimeZone) context.get("timeZone"), (Locale) context.get("locale"), true);
                    } catch (GeneralException e) {
                        String errMsg = "Could not convert field value for the field: [" + this.field.getOriginalName() + "] to the [" + this.type + "] type for the value [" + newValue + "]: " + e.toString();
                        Debug.logError(e, errMsg, module);
                        throw new IllegalArgumentException(errMsg);
                    }
                }
            }
            if (this.toScope != null && "user".equals(this.toScope)) {
                    String originalName = this.field.getOriginalName();
                    String currentWidgetTrail = (String)context.get("_WIDGETTRAIL_");
                    String newKey = currentWidgetTrail + "|" + originalName;
                    HttpSession session = (HttpSession)context.get("session");
                    session.setAttribute(newKey, newValue);
                    if (Debug.verboseOn()) {
                        Debug.logVerbose("In user setting value for field from [" + this.field.getOriginalName() + "]: " + newValue, module);
                    }

            } else if (this.toScope != null && "application".equals(this.toScope)) {
                    String originalName = this.field.getOriginalName();
                    String currentWidgetTrail = (String)context.get("_WIDGETTRAIL_");
                    String newKey = currentWidgetTrail + "|" + originalName;
                    ServletContext servletContext = (ServletContext)context.get("application");
                    servletContext.setAttribute(newKey, newValue);
                    if (Debug.verboseOn()) {
                        Debug.logVerbose("In application setting value for field from [" + this.field.getOriginalName() + "]: " + newValue, module);
                    }

            } else {
                if (Debug.verboseOn()) {
                    Debug.logVerbose("In screen setting field [" + this.field.getOriginalName() + "] to value: " + newValue, module);
                }
                this.field.put(context, newValue);
            }

            if (global) {
                Map<String, Object> globalCtx = UtilGenerics.checkMap(context.get("globalContext"));
                if (globalCtx != null) {
                    this.field.put(globalCtx, newValue);
                }
            }
        }

        @Override
        public void accept(ModelActionVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        public FlexibleMapAccessor<Object> getField() {
            return field;
        }

        public FlexibleMapAccessor<Object> getFromField() {
            return fromField;
        }

        public FlexibleStringExpander getValueExdr() {
            return valueExdr;
        }

        public FlexibleStringExpander getDefaultExdr() {
            return defaultExdr;
        }

        public FlexibleStringExpander getGlobalExdr() {
            return globalExdr;
        }

        public String getType() {
            return type;
        }

        public String getToScope() {
            return toScope;
        }

        public String getFromScope() {
            return fromScope;
        }
    }
}

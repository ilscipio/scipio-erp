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
import java.util.Map;

import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.collections.ValueAccessor;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.widget.model.AbstractModelCondition.DefaultConditionFactory;
import org.w3c.dom.Element;

/**
 * Models the &lt;condition&gt; element.
 *
 * @see <code>widget-screen.xsd</code>
 */
@SuppressWarnings("serial")
public final class ModelScreenCondition implements Serializable {

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

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    public static final ModelConditionFactory SCREEN_CONDITION_FACTORY = new ScreenConditionFactory();

    public static class IfEmptySection extends AbstractModelCondition {
        private final FlexibleStringExpander sectionExdr;

        private IfEmptySection(ModelConditionFactory factory, ModelWidget modelWidget, Element condElement) {
            super (factory, modelWidget, condElement);
            this.sectionExdr = FlexibleStringExpander.getInstance(condElement.getAttribute("section-name"));
        }

        @Override
        public void accept(ModelConditionVisitor visitor) throws Exception {
            visitor.visit(this);
        }

        @Override
        public boolean eval(Map<String, Object> context) {
            Map<String, Object> sectionsMap = UtilGenerics.toMap(context.get("sections"));
            return !sectionsMap.containsKey(this.sectionExdr.expandString(context));
        }

        public FlexibleStringExpander getSectionExdr() {
            return sectionExdr;
        }
    }

    private static class ScreenConditionFactory extends DefaultConditionFactory {

        @Override
        public ModelCondition newInstance(ModelWidget modelWidget, Element conditionElement) {
            if (conditionElement == null) {
                return DefaultConditionFactory.TRUE;
            }
            if ("if-empty-section".equals(conditionElement.getNodeName())) {
                return new IfEmptySection(this, modelWidget, conditionElement);
            }
            return super.newInstance(this, modelWidget,conditionElement);
        }
    }

    /**
     * SCIPIO: Quick method to make an if-true value="" -like condition dynamically which only passes
     * if result is explicit true boolean or string.
     */
    public static ModelCondition makeBooleanExprCondition(ModelWidget modelWidget, FlexibleStringExpander expr) {
        return new AbstractModelCondition.IfTrue(SCREEN_CONDITION_FACTORY, modelWidget,
                new ValueAccessor.ExpanderAccessor(expr), true, true);
    }
}

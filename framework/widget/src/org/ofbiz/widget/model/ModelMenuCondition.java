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

import org.ofbiz.base.util.UtilXml;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.w3c.dom.Element;

/**
 * Models the &lt;condition&gt; element.
 *
 * @see <code>widget-menu.xsd</code>
 */
@SuppressWarnings("serial")
public final class ModelMenuCondition implements Serializable {

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

    private final FlexibleStringExpander passStyleExdr;
    private final FlexibleStringExpander failStyleExdr;
    private final ModelCondition condition;
    private final FlexibleStringExpander mode; // SCIPIO: 3.0.0: Added

    public ModelMenuCondition(ModelWidget modelWidget, Element conditionElement) { // SCIPIO: 3.0.0: Switched to ModelWidget (generalized)
        this.passStyleExdr = FlexibleStringExpander.getInstance(conditionElement.getAttribute("pass-style"));
        this.failStyleExdr = FlexibleStringExpander.getInstance(conditionElement.getAttribute("disabled-style"));
        // SCIPIO: 3.0.0: Previously the inner condition was passed here - must pass the inner element instead
        //this.condition = AbstractModelCondition.DEFAULT_CONDITION_FACTORY.newInstance(modelWidget, conditionElement);
        Element innerConditionElement = UtilXml.firstChildElement(conditionElement);
        this.condition = AbstractModelCondition.DEFAULT_CONDITION_FACTORY.newInstance(modelWidget, innerConditionElement);
        this.mode = FlexibleStringExpander.getInstance(conditionElement.getAttribute("mode"));
    }

    public ModelCondition getCondition() {
        return condition;
    }

    public FlexibleStringExpander getFailStyleExdr() {
        return failStyleExdr;
    }

    public FlexibleStringExpander getPassStyleExdr() {
        return passStyleExdr;
    }

    public FlexibleStringExpander getMode() {
        return mode;
    }
}

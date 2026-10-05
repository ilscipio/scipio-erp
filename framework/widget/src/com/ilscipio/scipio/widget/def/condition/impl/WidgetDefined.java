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
package com.ilscipio.scipio.widget.def.condition.impl;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.Delegator;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.widget.model.FormFactory;
import org.ofbiz.widget.model.MenuFactory;
import org.ofbiz.widget.model.ScreenFactory;
import org.ofbiz.widget.model.TreeFactory;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that checks if a widget (screen, form, menu, tree) is defined.
 *
 * <p>Params: [name, location, type]</p>
 * <ul>
 *   <li>name (required): Widget name</li>
 *   <li>location (required): Widget location (resource)</li>
 *   <li>type (required): Widget type (screen, form, menu, tree)</li>
 * </ul>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class WidgetDefined implements WidgetCondition {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private String name;
    private String location;
    private String type;

    private FlexibleStringExpander nameExpander;
    private FlexibleStringExpander locationExpander;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 3) {
            throw new IllegalArgumentException("WidgetDefined requires 3 parameters: [name, location, type]");
        }
        this.name = params[0];
        this.location = params[1];
        this.type = params[2];

        this.nameExpander = FlexibleStringExpander.getInstance(name);
        this.locationExpander = FlexibleStringExpander.getInstance(location);
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        try {
            String expandedName = nameExpander.expandString(context);
            String expandedLocation = locationExpander.expandString(context);

            if (UtilValidate.isEmpty(expandedName) || UtilValidate.isEmpty(expandedLocation)) {
                return false;
            }

            switch (type.toLowerCase()) {
                case "screen":
                    return isScreenDefined(expandedName, expandedLocation);
                case "form":
                    return isFormDefined(expandedName, expandedLocation);
                case "menu":
                    return isMenuDefined(expandedName, expandedLocation);
                case "tree":
                    return isTreeDefined(expandedName, expandedLocation, context);
                default:
                    Debug.logWarning("WidgetDefined: unknown widget type: " + type, module);
                    return false;
            }
        } catch (Exception e) {
            Debug.logWarning(e, "Error evaluating WidgetDefined condition for [" + type + ":" + name + "@" + location + "]", module);
            return false;
        }
    }

    private boolean isScreenDefined(String name, String location) {
        try {
            return ScreenFactory.getScreenFromLocation(location, name) != null;
        } catch (Exception e) {
            return false;
        }
    }

    private boolean isFormDefined(String name, String location) {
        try {
            return FormFactory.getFormFromLocation(location, name, null, null) != null;
        } catch (Exception e) {
            return false;
        }
    }

    private boolean isMenuDefined(String name, String location) {
        try {
            return MenuFactory.getMenuFromLocation(location, name) != null;
        } catch (Exception e) {
            return false;
        }
    }

    private boolean isTreeDefined(String name, String location, Map<String, Object> context) {
        try {
            Delegator delegator = (Delegator) context.get("delegator");
            LocalDispatcher dispatcher = (LocalDispatcher) context.get("dispatcher");
            return TreeFactory.getTreeFromLocation(location, name, delegator, dispatcher) != null;
        } catch (Exception e) {
            return false;
        }
    }
}

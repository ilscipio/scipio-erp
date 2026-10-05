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

import org.ofbiz.base.component.ComponentConfig;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.string.FlexibleStringExpander;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that checks if a component is enabled.
 *
 * <p>Params: [componentName]</p>
 *
 * <p>Returns true if the component exists and is enabled.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ComponentEnabled implements WidgetCondition {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private String componentName;
    private FlexibleStringExpander componentExpander;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 1) {
            throw new IllegalArgumentException("ComponentEnabled requires 1 parameter: [componentName]");
        }
        this.componentName = params[0];
        this.componentExpander = FlexibleStringExpander.getInstance(componentName);
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        try {
            String expandedName = componentExpander.expandString(context);

            // Check if component exists and is enabled
            ComponentConfig config = ComponentConfig.getComponentConfig(expandedName);
            return config != null && config.enabled();

        } catch (Exception e) {
            // Component not found or other error
            Debug.logVerbose("ComponentEnabled: component not found or error: " + componentName + " - " + e.getMessage(), module);
            return false;
        }
    }
}

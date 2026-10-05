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
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.model.ModelEntity;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that checks if an entity is defined in the model.
 *
 * <p>Params: [entityName]</p>
 *
 * <p>Returns true if the entity exists in the entity model.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class EntityDefined implements WidgetCondition {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private String entityName;
    private FlexibleStringExpander entityExpander;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 1) {
            throw new IllegalArgumentException("EntityDefined requires 1 parameter: [entityName]");
        }
        this.entityName = params[0];
        this.entityExpander = FlexibleStringExpander.getInstance(entityName);
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        try {
            String expandedName = entityExpander.expandString(context);

            Delegator delegator = (Delegator) context.get("delegator");
            if (delegator == null) {
                Debug.logWarning("EntityDefined: delegator not found in context", module);
                return false;
            }

            ModelEntity modelEntity = delegator.getModelEntity(expandedName);
            return modelEntity != null;

        } catch (Exception e) {
            Debug.logVerbose("EntityDefined: entity not found or error: " + entityName + " - " + e.getMessage(), module);
            return false;
        }
    }
}

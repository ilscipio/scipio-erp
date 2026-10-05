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

import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.string.FlexibleStringExpander;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that checks if a field is NOT empty (has a value).
 *
 * <p>Params: [fieldName]</p>
 *
 * <p>Convenience condition - equivalent to Not(Empty(field)).</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class NotEmpty implements WidgetCondition {

    private String fieldName;
    private FlexibleStringExpander fieldExpander;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 1) {
            throw new IllegalArgumentException("NotEmpty requires 1 parameter: [fieldName]");
        }
        this.fieldName = params[0];
        this.fieldExpander = FlexibleStringExpander.getInstance(fieldName);
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        Object fieldValue;

        // Handle flexible expressions
        if (fieldExpander.getOriginal().contains("${") || fieldExpander.getOriginal().contains(".")) {
            String expandedValue = fieldExpander.expandString(context);
            fieldValue = expandedValue;
        } else {
            fieldValue = getFieldValue(context, fieldName);
        }

        return !ObjectType.isEmpty(fieldValue);
    }

    @SuppressWarnings("unchecked")
    private Object getFieldValue(Map<String, Object> context, String field) {
        if (field == null) {
            return null;
        }
        if (field.contains(".")) {
            String[] parts = field.split("\\.", 2);
            Object parent = context.get(parts[0]);
            if (parent instanceof Map) {
                return getFieldValue((Map<String, Object>) parent, parts[1]);
            }
            return null;
        }
        return context.get(field);
    }
}

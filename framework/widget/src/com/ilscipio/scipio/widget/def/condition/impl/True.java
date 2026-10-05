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

import org.ofbiz.base.util.string.FlexibleStringExpander;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that checks if a field/value is boolean true.
 *
 * <p>Params: [fieldOrValue]</p>
 *
 * <p>Returns true if the value equals Boolean.TRUE or string "true".</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class True implements WidgetCondition {

    private String fieldOrValue;
    private FlexibleStringExpander expander;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 1) {
            throw new IllegalArgumentException("True requires 1 parameter: [fieldOrValue]");
        }
        this.fieldOrValue = params[0];
        this.expander = FlexibleStringExpander.getInstance(fieldOrValue);
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        Object value;

        // Check if it's a field reference or direct value
        if (fieldOrValue.contains("${") || fieldOrValue.contains(".")) {
            // Expand as expression
            String expanded = expander.expandString(context);
            value = expanded;
        } else {
            // Try as field name first
            value = getFieldValue(context, fieldOrValue);
            if (value == null) {
                // Could be a literal value
                value = fieldOrValue;
            }
        }

        // Check for boolean true
        if (value instanceof Boolean) {
            return Boolean.TRUE.equals(value);
        }
        if (value instanceof String) {
            return "true".equals(value);
        }
        return false;
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

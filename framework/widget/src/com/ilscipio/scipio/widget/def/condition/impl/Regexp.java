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
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.string.FlexibleStringExpander;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that matches a field against a regular expression.
 *
 * <p>Params: [field, expression]</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class Regexp implements WidgetCondition {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private String field;
    private String expression;

    private FlexibleStringExpander fieldExpander;
    private FlexibleStringExpander exprExpander;
    private Pattern compiledPattern; // Pre-compiled if expression is static

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 2) {
            throw new IllegalArgumentException("Regexp requires 2 parameters: [field, expression]");
        }
        this.field = params[0];
        this.expression = params[1];

        this.fieldExpander = FlexibleStringExpander.getInstance(field);
        this.exprExpander = FlexibleStringExpander.getInstance(expression);

        // Pre-compile pattern if it's static (no expressions)
        if (!expression.contains("${")) {
            try {
                this.compiledPattern = Pattern.compile(expression);
            } catch (PatternSyntaxException e) {
                Debug.logWarning("Invalid regex pattern [" + expression + "]: " + e.getMessage(), module);
            }
        }
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        try {
            // Get field value
            String fieldStr = fieldExpander.expandString(context);
            Object fieldValue = getFieldValue(context, fieldStr);
            String valueStr = fieldValue != null ? fieldValue.toString() : "";

            // Get pattern (expand if needed)
            Pattern pattern = compiledPattern;
            if (pattern == null) {
                String expandedExpr = exprExpander.expandString(context);
                try {
                    pattern = Pattern.compile(expandedExpr);
                } catch (PatternSyntaxException e) {
                    Debug.logWarning("Invalid regex pattern [" + expandedExpr + "]: " + e.getMessage(), module);
                    return false;
                }
            }

            return pattern.matcher(valueStr).matches();

        } catch (Exception e) {
            Debug.logWarning(e, "Error evaluating Regexp condition for field [" + field + "]", module);
            return false;
        }
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

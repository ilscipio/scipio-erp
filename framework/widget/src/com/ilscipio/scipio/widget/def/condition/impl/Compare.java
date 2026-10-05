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
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.minilang.operation.BaseCompare;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that compares a field against a value.
 *
 * <p>Params: [field, operator, value, type?, format?]</p>
 * <ul>
 *   <li>field (required): Field name to compare</li>
 *   <li>operator (required): less, greater, less-equals, greater-equals, equals, not-equals, contains</li>
 *   <li>value (required): Value to compare against</li>
 *   <li>type (optional): PlainString, String, BigDecimal, Double, Float, Long, Integer, Date, Time, Timestamp, Boolean, Object (default: String)</li>
 *   <li>format (optional): Format for date/time types</li>
 * </ul>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class Compare implements WidgetCondition {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private String field;
    private String operator;
    private String value;
    private String type;
    private String format;

    private FlexibleStringExpander fieldExpander;
    private FlexibleStringExpander valueExpander;
    private FlexibleStringExpander formatExpander;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 3) {
            throw new IllegalArgumentException("Compare requires at least 3 parameters: [field, operator, value]");
        }
        this.field = params[0];
        this.operator = params[1];
        this.value = params[2];
        this.type = params.length > 3 ? params[3] : "String";
        this.format = params.length > 4 ? params[4] : null;

        this.fieldExpander = FlexibleStringExpander.getInstance(field);
        this.valueExpander = FlexibleStringExpander.getInstance(value);
        this.formatExpander = UtilValidate.isNotEmpty(format) ? FlexibleStringExpander.getInstance(format) : null;
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        try {
            // Get field value
            String fieldStr = fieldExpander.expandString(context);
            Object fieldValue = getFieldValue(context, fieldStr);

            // Expand value expression
            String compareValue = valueExpander.expandString(context);

            // Expand format if needed
            String expandedFormat = null;
            if (formatExpander != null) {
                expandedFormat = formatExpander.expandString(context);
            }

            // Convert field value to string for comparison
            String fieldValueStr = fieldValue != null ? fieldValue.toString() : "";

            // Use BaseCompare for consistent comparison logic
            Boolean result = BaseCompare.doRealCompare(fieldValueStr, compareValue, operator, type, expandedFormat, null, null, null, true);
            return result != null && result;

        } catch (Exception e) {
            Debug.logWarning(e, "Error evaluating Compare condition for field [" + field + "]", module);
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

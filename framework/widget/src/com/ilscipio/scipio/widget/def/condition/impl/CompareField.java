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
import org.ofbiz.minilang.operation.BaseCompare;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that compares two fields.
 *
 * <p>Params: [field, operator, toField, type?, format?]</p>
 * <ul>
 *   <li>field (required): First field to compare</li>
 *   <li>operator (required): less, greater, less-equals, greater-equals, equals, not-equals, contains</li>
 *   <li>toField (required): Second field to compare against</li>
 *   <li>type (optional): Type for comparison (default: String)</li>
 *   <li>format (optional): Format for date/time types</li>
 * </ul>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class CompareField implements WidgetCondition {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private String field;
    private String operator;
    private String toField;
    private String type;
    private String format;

    private FlexibleStringExpander fieldExpander;
    private FlexibleStringExpander toFieldExpander;
    private FlexibleStringExpander formatExpander;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 3) {
            throw new IllegalArgumentException("CompareField requires at least 3 parameters: [field, operator, toField]");
        }
        this.field = params[0];
        this.operator = params[1];
        this.toField = params[2];
        this.type = params.length > 3 ? params[3] : "String";
        this.format = params.length > 4 ? params[4] : null;

        this.fieldExpander = FlexibleStringExpander.getInstance(field);
        this.toFieldExpander = FlexibleStringExpander.getInstance(toField);
        this.formatExpander = UtilValidate.isNotEmpty(format) ? FlexibleStringExpander.getInstance(format) : null;
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        try {
            // Get first field value
            String fieldStr = fieldExpander.expandString(context);
            Object fieldValue = getFieldValue(context, fieldStr);

            // Get second field value
            String toFieldStr = toFieldExpander.expandString(context);
            Object toFieldValue = getFieldValue(context, toFieldStr);

            // Expand format if needed
            String expandedFormat = null;
            if (formatExpander != null) {
                expandedFormat = formatExpander.expandString(context);
            }

            // Convert to strings for comparison
            String fieldValueStr = fieldValue != null ? fieldValue.toString() : "";
            String toFieldValueStr = toFieldValue != null ? toFieldValue.toString() : "";

            // Use BaseCompare for consistent comparison logic
            Boolean result = BaseCompare.doRealCompare(fieldValueStr, toFieldValueStr, operator, type, expandedFormat, null, null, null, true);
            return result != null && result;

        } catch (Exception e) {
            Debug.logWarning(e, "Error evaluating CompareField condition for field [" + field + "]", module);
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

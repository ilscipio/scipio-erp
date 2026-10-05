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

import java.lang.reflect.Method;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.base.util.string.FlexibleStringExpander;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that validates a field using a validation method.
 *
 * <p>Params: [field, method, class?]</p>
 * <ul>
 *   <li>field (required): Field name to validate</li>
 *   <li>method (required): Method name (e.g., isEmail, isNotEmpty)</li>
 *   <li>class (optional): Full class name (default: org.ofbiz.base.util.UtilValidate)</li>
 * </ul>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ValidateMethod implements WidgetCondition {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());
    private static final String DEFAULT_CLASS = "org.ofbiz.base.util.UtilValidate";

    private String field;
    private String methodName;
    private String className;

    private FlexibleStringExpander fieldExpander;
    private Method method;
    private Class<?> validatorClass;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 2) {
            throw new IllegalArgumentException("ValidateMethod requires at least 2 parameters: [field, method]");
        }
        this.field = params[0];
        this.methodName = params[1];
        this.className = params.length > 2 ? params[2] : DEFAULT_CLASS;

        this.fieldExpander = FlexibleStringExpander.getInstance(field);

        // Pre-load the class and method
        try {
            this.validatorClass = ObjectType.loadClass(className);
            // Try to find method that takes String or Object parameter
            try {
                this.method = validatorClass.getMethod(methodName, String.class);
            } catch (NoSuchMethodException e) {
                try {
                    this.method = validatorClass.getMethod(methodName, Object.class);
                } catch (NoSuchMethodException e2) {
                    Debug.logWarning("Validation method not found: " + className + "." + methodName + "(String/Object)", module);
                }
            }
        } catch (ClassNotFoundException e) {
            Debug.logWarning("Validation class not found: " + className, module);
        }
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        if (method == null) {
            Debug.logWarning("ValidateMethod condition has no valid method configured", module);
            return false;
        }

        try {
            // Get field value
            String fieldStr = fieldExpander.expandString(context);
            Object fieldValue = getFieldValue(context, fieldStr);
            String valueStr = fieldValue != null ? fieldValue.toString() : "";

            // Invoke the validation method
            Object result = method.invoke(null, valueStr);

            // Convert result to boolean
            if (result instanceof Boolean) {
                return (Boolean) result;
            }
            return false;

        } catch (Exception e) {
            Debug.logWarning(e, "Error evaluating ValidateMethod condition for field [" + field + "]", module);
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

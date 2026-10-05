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
package org.ofbiz.service.eca;

import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.codehaus.groovy.runtime.InvokerHelper;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.UtilValidate;

import groovy.lang.Binding;
import groovy.lang.Script;

/**
 * SCIPIO: 4.0.0: Evaluates the Groovy {@code condition} expression of annotation-based SECA and EECA rules
 * ({@code @Seca(condition = "!empty(quoteId)")}).
 *
 * <p>The expression sees the service context or entity value fields as variables (missing variables read as
 * null) and the helpers {@code empty(x)}, {@code xor(a, b)} and {@code property(resource, name[, default])}.
 * Compiled scripts are cached per expression. An expression that fails to compile or throws evaluates to false
 * and is logged once per failure.</p>
 */
public final class EcaConditionExpression {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final String PRELUDE =
            "static boolean empty(Object v) { if (v == null) return true; if (v instanceof CharSequence) return v.length() == 0; "
            + "if (v instanceof java.util.Collection) return v.isEmpty(); if (v instanceof java.util.Map) return v.isEmpty(); "
            + "if (v.getClass().isArray()) return java.lang.reflect.Array.getLength(v) == 0; return false }\n"
            + "static boolean xor(Object a, Object b) { (a as boolean) ^ (b as boolean) }\n"
            + "static Object property(String resource, String name, Object dflt = null) { "
            + "String v = org.ofbiz.base.util.UtilProperties.getPropertyValue(resource, name); (v == null || v.isEmpty()) ? dflt : v }\n"
            + "return (";

    private static final Map<String, Class<?>> CACHE = new ConcurrentHashMap<>();

    private EcaConditionExpression() {}

    /** Binding that answers null for unknown variables instead of throwing MissingPropertyException. */
    private static final class NullSafeBinding extends Binding {
        NullSafeBinding(Map<String, Object> vars) {
            super(vars);
        }

        @Override
        public Object getVariable(String name) {
            if (!hasVariable(name)) return null;
            return super.getVariable(name);
        }
    }

    public static boolean isBlank(String expression) {
        return UtilValidate.isEmpty(expression) || expression.trim().isEmpty();
    }

    /**
     * Evaluates the expression against the variables. A blank expression is true.
     * @param ruleDesc short rule description for log messages
     */
    public static boolean eval(String expression, Map<String, Object> variables, String ruleDesc) {
        if (isBlank(expression)) return true;
        Class<?> cls;
        try {
            cls = CACHE.computeIfAbsent(expression.trim(), EcaConditionExpression::compile);
        } catch (RuntimeException e) {
            Debug.logError("ECA condition [" + expression + "] of " + ruleDesc + " does not compile: " + e.getMessage() + "; treating as false", module);
            return false;
        }
        try {
            Script script = InvokerHelper.createScript(cls, new NullSafeBinding(new java.util.HashMap<>(variables)));
            Object result = script.run();
            return truth(result);
        } catch (RuntimeException e) {
            Debug.logError("ECA condition [" + expression + "] of " + ruleDesc + " failed: " + e.getMessage() + "; treating as false", module);
            return false;
        }
    }

    private static Class<?> compile(String expression) {
        try {
            return GroovyUtil.parseClass(PRELUDE + expression + "\n)");
        } catch (IOException e) {
            throw new IllegalArgumentException(e.getMessage(), e);
        }
    }

    static boolean truth(Object result) {
        if (result == null) return false;
        if (result instanceof Boolean) return (Boolean) result;
        if (result instanceof CharSequence) return ((CharSequence) result).length() > 0;
        if (result instanceof Number) return ((Number) result).doubleValue() != 0d;
        if (result instanceof Collection) return !((Collection<?>) result).isEmpty();
        if (result instanceof Map) return !((Map<?, ?>) result).isEmpty();
        return true;
    }
}

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
package com.ilscipio.scipio.widget.def.condition;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.ofbiz.base.util.Debug;

/**
 * Evaluator for {@link Condition} annotations.
 *
 * <p>Handles instantiation, initialization, and caching of condition instances.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for annotation-based widget condition support.</p>
 */
public class ConditionEvaluator {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    // Cache of built condition instances by annotation hashcode
    private static final Map<Integer, WidgetCondition> conditionCache = new ConcurrentHashMap<>();

    private ConditionEvaluator() {
        // Utility class
    }

    /**
     * Evaluates a {@link Condition} annotation against the given context.
     *
     * @param condition The condition annotation
     * @param context The widget context map
     * @return true if the condition is satisfied
     */
    public static boolean evaluate(Condition condition, Map<String, Object> context) {
        if (condition == null) {
            return true; // No condition = always true
        }
        WidgetCondition widgetCondition = buildCondition(condition);
        return widgetCondition.evaluate(context);
    }

    /**
     * Builds a {@link WidgetCondition} instance from a {@link Condition} annotation.
     *
     * <p>Results are cached for performance.</p>
     *
     * @param condition The condition annotation
     * @return The built condition instance
     */
    public static WidgetCondition buildCondition(Condition condition) {
        // Use annotation's identity for caching
        int cacheKey = computeCacheKey(condition);
        WidgetCondition cached = conditionCache.get(cacheKey);
        if (cached != null) {
            return cached;
        }

        WidgetCondition built = createCondition(condition);
        conditionCache.put(cacheKey, built);
        return built;
    }

    /**
     * Creates a new condition instance (not cached).
     *
     * <p>Note: Composite conditions (And, Or, Xor, Not) are not supported at the
     * individual Condition annotation level due to Java's cyclic annotation limitations.
     * Use composite conditions at the container level (e.g., MenuItemCondition).</p>
     */
    private static WidgetCondition createCondition(Condition condition) {
        try {
            Class<? extends WidgetCondition> type = condition.type();
            WidgetCondition instance = type.getDeclaredConstructor().newInstance();

            // Initialize with parameters
            String[] params = condition.params();
            if (params != null && params.length > 0) {
                instance.init(params);
            }

            // Note: Composite conditions cannot have nested children in individual
            // Condition annotations due to Java cyclic annotation limitations.
            // Composite logic is handled at the container level (e.g., MenuItemCondition).

            return instance;

        } catch (Exception e) {
            Debug.logError(e, "Failed to create condition of type [" + condition.type().getName() + "]", module);
            // Return a condition that always evaluates to false on error
            return context -> {
                Debug.logWarning("Condition evaluation skipped due to initialization error", module);
                return false;
            };
        }
    }

    /**
     * Computes a cache key for a condition annotation.
     *
     * <p>Includes type and params.</p>
     */
    private static int computeCacheKey(Condition condition) {
        int result = condition.type().hashCode();
        for (String param : condition.params()) {
            result = 31 * result + (param != null ? param.hashCode() : 0);
        }
        return result;
    }

    /**
     * Clears the condition cache.
     *
     * <p>Mainly for testing or hot-reload scenarios.</p>
     */
    public static void clearCache() {
        conditionCache.clear();
    }
}

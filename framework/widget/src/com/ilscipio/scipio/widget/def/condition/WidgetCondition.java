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

import java.util.List;
import java.util.Map;

/**
 * Functional interface for widget conditions.
 *
 * <p>Implementations evaluate conditions at runtime against a context map containing
 * standard widget context variables (userLogin, security, delegator, dispatcher, etc.).</p>
 *
 * <p>SCIPIO: 4.0.0: Added for annotation-based widget condition support.</p>
 */
@FunctionalInterface
public interface WidgetCondition {

    /**
     * Evaluates the condition against the given context.
     *
     * @param context The widget context map containing userLogin, security, delegator, etc.
     * @return true if the condition is satisfied, false otherwise
     */
    boolean evaluate(Map<String, Object> context);

    /**
     * Initializes the condition with parameters from the annotation.
     *
     * <p>Called once when the condition is instantiated from an annotation.
     * Default implementation does nothing - override for parameterized conditions.</p>
     *
     * @param params The parameters from {@code @Condition(params = {...})}
     */
    default void init(String[] params) {
        // Default: no-op for conditions without parameters
    }

    /**
     * Sets child conditions for composite conditions (And, Or, Not, Xor).
     *
     * <p>Default implementation does nothing - override for composite conditions.</p>
     *
     * @param conditions The child conditions
     */
    default void setConditions(List<WidgetCondition> conditions) {
        // Default: no-op for non-composite conditions
    }

    /**
     * Returns true if this is a composite condition that requires child conditions.
     *
     * @return true for And, Or, Not, Xor conditions
     */
    default boolean isComposite() {
        return false;
    }
}

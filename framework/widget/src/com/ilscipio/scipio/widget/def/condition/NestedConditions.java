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

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Container annotation for multiple {@link Condition} annotations.
 *
 * <p>Used in container annotations like {@code MenuItemCondition.conditions()}
 * to hold an array of {@link Condition} annotations.</p>
 *
 * <p>Example usage in MenuItemCondition:</p>
 * <pre>
 * {@code condition = @MenuItemCondition(conditions = {
 *     @Condition(type = HasPermission.class, params = {"PARTYMGR", "_ADMIN"}),
 *     @Condition(type = NotEmpty.class, params = {"partyId"})
 * })}
 * </pre>
 *
 * <p>For composite conditions like And/Or/Not, the children are specified
 * at the MenuItemCondition level, not within individual Condition annotations,
 * to avoid cyclic annotation references.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for annotation-based widget condition support.</p>
 *
 * @see Condition
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({}) // Used as nested annotation only
public @interface NestedConditions {

    /**
     * The condition annotations.
     *
     * @return Array of Condition annotations
     */
    Condition[] value() default {};
}

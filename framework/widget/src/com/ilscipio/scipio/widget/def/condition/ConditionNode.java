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
 * One node of the flat condition tree carried by {@link Condition#tree()}.
 *
 * <p>A Java annotation cannot contain itself, so a condition tree cannot be written as nested
 * annotations. The tree is therefore stored flat: every node names its parent by index, and a
 * node with {@code parent = -1} is a direct member of the enclosing {@link Condition}. This
 * carries any depth, unlike a fixed chain of NestedCondition types.</p>
 *
 * <p>{@code or(a, and(b, not c))} becomes:</p>
 * <pre>
 * {@code @Condition(type = Or.class, tree = {
 *     @ConditionNode(type = HasPermission.class, params = {"PARTYMGR", "_ADMIN"}),
 *     @ConditionNode(type = And.class),
 *     @ConditionNode(parent = 1, type = Empty.class, params = {"partyId"}),
 *     @ConditionNode(parent = 1, not = true, type = True.class, params = {"showAll"})
 * })}
 * </pre>
 *
 * <p>Nodes are written in pre-order, so a parent always precedes its children.</p>
 *
 * <p>SCIPIO: 4.0.0: Added; composite conditions the previous representation could not hold
 * fell back to an always-true condition, silently removing the guard.</p>
 *
 * @see Condition
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({}) // Used as nested annotation only
public @interface ConditionNode {

    /**
     * Index of this node's parent within the same {@link Condition#tree()}; -1 makes it a
     * direct member of the enclosing condition.
     *
     * @return The parent index, or -1
     */
    int parent() default -1;

    /**
     * The condition implementation class; a composite type (And, Or, Xor, Not) may have
     * children pointing at this node's index.
     *
     * @return The condition class
     */
    Class<? extends WidgetCondition> type();

    /**
     * Parameters for the condition.
     *
     * @return The parameters array
     */
    String[] params() default {};

    /**
     * Negates this node.
     *
     * @return true to negate the condition
     */
    boolean not() default false;
}

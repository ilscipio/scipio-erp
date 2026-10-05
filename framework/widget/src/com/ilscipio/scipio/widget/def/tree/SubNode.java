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
package com.ilscipio.scipio.widget.def.tree;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines a sub-node, equivalent to widget-tree.xsd sub-node element.
 *
 * <p>SCIPIO: 4.0.0: Added for tree annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface SubNode {

    /**
     * Referenced node name; required.
     */
    String nodeName();

    /**
     * Actions for this sub-node.
     */
    TreeActions actions() default @TreeActions(UNSET = true);

    /**
     * Entity-and action for this sub-node.
     */
    EntityAnd entityAnd() default @EntityAnd(UNSET = true);

    /**
     * Service action for this sub-node.
     */
    TreeService service() default @TreeService(UNSET = true);

    /**
     * Entity-condition action for this sub-node.
     */
    EntityCondition entityCondition() default @EntityCondition(UNSET = true);

    /**
     * Output field mappings.
     */
    OutFieldMap[] outFieldMaps() default {};
}

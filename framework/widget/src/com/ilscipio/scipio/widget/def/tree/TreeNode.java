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
 * Defines a tree node, equivalent to widget-tree.xsd node element.
 *
 * <p>SCIPIO: 4.0.0: Added for tree annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface TreeNode {

    /**
     * Node name; required.
     */
    String name();

    /**
     * CSS class for wrapping.
     */
    String wrapStyle() default "";

    /**
     * Render style: simple, follow-trail, show-peers, expand-collapse.
     */
    RenderStyle renderStyle() default RenderStyle.SIMPLE;

    /**
     * Whether to use default render style (ignores renderStyle).
     */
    boolean useDefaultRenderStyle() default true;

    /**
     * Entry name for iterating values.
     */
    String entryName() default "";

    /**
     * Entity name for this node.
     */
    String entityName() default "";

    /**
     * Field name for joining to parent.
     */
    String joinFieldName() default "";

    /**
     * Node condition.
     */
    TreeNodeCondition condition() default @TreeNodeCondition(UNSET = true);

    /**
     * Node actions.
     */
    TreeActions actions() default @TreeActions(UNSET = true);

    /**
     * Entity-one action for this node.
     */
    TreeEntityOne entityOne() default @TreeEntityOne(UNSET = true);

    /**
     * Service action for this node.
     */
    TreeService service() default @TreeService(UNSET = true);

    /**
     * Include screen content for this node.
     */
    TreeIncludeScreen includeScreen() default @TreeIncludeScreen(UNSET = true);

    /**
     * Label content for this node.
     */
    TreeLabel label() default @TreeLabel(UNSET = true);

    /**
     * Link content for this node.
     */
    TreeLink link() default @TreeLink(UNSET = true);

    /**
     * Sub-nodes.
     */
    SubNode[] subNodes() default {};
}

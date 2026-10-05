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

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a Scipio tree widget, equivalent to widget-tree.xsd tree element.
 *
 * <p>This annotation can be applied to a class or method to define a tree widget.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}Tree(name = "MyTree", rootNodeName = "root",
 *     nodes = {
 *         {@literal @}TreeNode(name = "root", entryName = "item",
 *             label = {@literal @}TreeLabel(text = "${item.name}")),
 *         {@literal @}TreeNode(name = "child", entryName = "childItem",
 *             link = {@literal @}TreeLink(target = "ViewChild", text = "${childItem.name}"))
 *     })
 * public interface MyTreeDef {}
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for tree annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(TreeList.class)
public @interface Tree {

    /**
     * Tree name; required.
     */
    String name();

    /**
     * Name of the root node; required.
     */
    String rootNodeName();

    /**
     * Default render style: simple, follow-trail, show-peers, expand-collapse.
     */
    RenderStyle defaultRenderStyle() default RenderStyle.SIMPLE;

    /**
     * Default CSS class for wrapping.
     */
    String defaultWrapStyle() default "";

    /**
     * Request name for expand/collapse functionality.
     */
    String expandCollapseRequest() default "";

    /**
     * Name of trail parameter.
     */
    String trailName() default "";

    /**
     * How many levels to open by default.
     */
    String openDepth() default "0";

    /**
     * How many levels to open after the trail.
     */
    String postTrailOpenDepth() default "0";

    /**
     * Entity name for the tree.
     */
    String entityName() default "";

    /**
     * Whether to force child check.
     */
    boolean forceChildCheck() default true;

    /**
     * Tree nodes.
     */
    TreeNode[] nodes() default {};

    // ========================================================================
    // Location alias attributes for backward compatibility with XML references
    // ========================================================================

    /**
     * Single alias location for backward compatibility with XML references.
     *
     * <p>When specified, lookups for this component:// location will resolve to this
     * annotated tree instead of the XML file.</p>
     *
     * <p>Example: "component://setup/widget/Trees.xml"</p>
     *
     * <p>SCIPIO: 4.0.0: Added for XML-to-annotation migration support.</p>
     */
    String location() default "";

    /**
     * Multiple alias locations for backward compatibility with XML references.
     *
     * <p>When specified, lookups for any of these component:// locations will resolve
     * to this annotated tree instead of the XML file.</p>
     *
     * <p>Example: {"component://setup/widget/Trees.xml", "component://setup/widget/OldTrees.xml"}</p>
     *
     * <p>SCIPIO: 4.0.0: Added for XML-to-annotation migration support.</p>
     */
    String[] locations() default {};
}

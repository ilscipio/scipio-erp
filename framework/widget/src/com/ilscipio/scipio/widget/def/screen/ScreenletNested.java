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
package com.ilscipio.scipio.widget.def.screen;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a screenlet widget for use inside containers.
 *
 * <p>This is a simplified version of {@link Screenlet} that can be nested inside
 * Container annotations without creating cyclic type references. Unlike the regular
 * Screenlet annotation, this one uses Container2 for nested containers to break
 * the cycle.</p>
 *
 * <p>SCIPIO: 4.0.0: Added to support screenlets inside containers without cyclic references.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.ANNOTATION_TYPE})
public @interface ScreenletNested {

    /**
     * The screenlet title (supports FlexibleStringExpander).
     */
    String title() default "";

    /**
     * Unique name/ID for the screenlet.
     */
    String name() default "";

    /** SCIPIO: 4.0.0: Screenlet id (collapsible screenlets need a name or id). */
    String id() default "";

    /**
     * Whether the screenlet is collapsible.
     */
    boolean collapsible() default false;

    /**
     * Whether the screenlet starts collapsed.
     */
    boolean initiallyCollapsed() default false;

    /**
     * Whether to save collapsed state.
     */
    boolean saveCollapsed() default true;

    /**
     * Whether the screenlet has padding.
     */
    boolean padded() default true;

    /**
     * CSS style/class for the screenlet title.
     */
    String titleStyle() default "";

    /**
     * Navigation menu name reference.
     */
    String navigationMenuName() default "";

    /**
     * Navigation form name reference.
     */
    String navigationFormName() default "";

    /**
     * Tab menu name reference.
     */
    String tabMenuName() default "";

    /**
     * Scipio targeting expression for conditional rendering.
     */
    String contains() default "";

    /**
     * Widgets without typed arrays here (link, content, include-tree, iterate-section, include-portal-page, ...).
     * <p>SCIPIO: 4.0.0: the converter dropped them, e.g. the export links of the financial reports.</p>
     */
    Widget[] widgets() default {};

    /**
     * Forms to include inside the screenlet.
     */
    IncludeForm[] includeForms() default {};

    /**
     * Screens to include inside the screenlet.
     */
    IncludeScreen[] includeScreens() default {};

    /**
     * Menus to include inside the screenlet.
     */
    IncludeMenu[] includeMenus() default {};

    /**
     * HTML templates to include inside the screenlet.
     */
    HtmlTemplate[] htmlTemplates() default {};

    /**
     * Labels to include inside the screenlet.
     */
    Label[] labels() default {};

    /**
     * Containers inside the screenlet.
     * Uses ContainerInScreenlet to avoid cyclic type references with Container.
     * SCIPIO: 4.0.0: Added to support nested containers inside screenlets.
     */
    ContainerInScreenlet[] containers() default {};

    /**
     * Sections at the end of the chain, where no SectionNested level is left.
     *
     * <p>SCIPIO: 4.0.0: Added; such a section was dropped or flattened, losing its condition
     * and merging its actions with its siblings'.</p>
     */
    SectionLeaf[] sections() default {};


    /** SCIPIO: 4.0.0: Decorator section includes nested directly in this screenlet. */
    DecoratorSectionInclude[] decoratorSectionIncludes() default {};

    /**
     * SCIPIO: 4.0.0: Slot of this child among all children of its widgets block, counting every typed
     * array. -1 (the default) keeps the declaration order of its own array. Set it when a child of
     * another type must render between two children of this type; every widgets block honours it.
     */
    int position() default -1;
}

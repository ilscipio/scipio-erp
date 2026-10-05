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
 * Defines an include-menu widget, equivalent to widget-screen.xsd include-menu element.
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}IncludeMenu(name = "ProductTabBar", location = "component://product/widget/ProductMenus.xml")
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for screen annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
public @interface IncludeMenu {

    /**
     * Menu name to include; required.
     */
    String name();

    /**
     * Menu location; required.
     *
     * <p>Component-style location (e.g., "component://product/widget/ProductMenus.xml").</p>
     */
    String location();

    /**
     * Whether to share scope with the menu.
     */
    boolean shareScope() default false;

    /**
     * Maximum depth of submenus to render (Scipio extension).
     */
    int maxDepth() default -1;

    /**
     * Submenu rendering mode: "none", "all", or "current" (Scipio extension).
     */
    String subMenus() default "";

    /**
     * Menu item condition mode (Scipio extension).
     */
    String itemConditionMode() default "";

    /**
     * SCIPIO: 4.0.0: Slot of this child among all children of its widgets block, counting every typed
     * array. -1 (the default) keeps the declaration order of its own array. Set it when a child of
     * another type must render between two children of this type; every widgets block honours it.
     */
    int position() default -1;
}

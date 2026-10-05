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
package com.ilscipio.scipio.widget.def.menu;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

/**
 * Defines a sub-menu in a menu item, equivalent to widget-menu.xsd sub-menu element.
 *
 * <p>SCIPIO: 4.0.0: Added for menu annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface SubMenu {

    /**
     * Marker for "not set" - used to distinguish between default and unset.
     */
    boolean UNSET() default false;

    /**
     * Unique sub-menu name within the top-level menu.
     */
    String name() default "";

    /**
     * HTML id attribute.
     */
    String id() default "";

    /**
     * CSS style class.
     */
    String style() default "";

    /**
     * Sub-menu title.
     */
    String title() default "";

    /**
     * Location in resource#name format of a menu to use as model.
     */
    String model() default "";

    /**
     * Scope of the sub-menu model: style, func, full, none.
     */
    String modelScope() default "";

    /**
     * Location in resource#name format of a menu to include.
     */
    String include() default "";

    /**
     * Items sort mode.
     */
    String itemsSortMode() default "";

    /**
     * Whether to share scope with parent.
     */
    String shareScope() default "";

    /**
     * Whether the sub-menu is expanded.
     * Supports flexible expressions.
     */
    String expanded() default "";

    /**
     * Sub-menu condition.
     */
    MenuItemCondition condition() default @MenuItemCondition(UNSET = true);

    /**
     * Sub-menu actions.
     */
    MenuActions actions() default @MenuActions(UNSET = true);

    /**
     * Menu items in this sub-menu.
     * Uses SubMenuItem to avoid cyclic annotation references.
     */
    SubMenuItem[] items() default {};
}

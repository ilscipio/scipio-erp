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
import java.lang.annotation.Target;

/**
 * Includes another menu's elements (actions and menu items) into a menu.
 *
 * <p>This is a convenience directive equivalent to declaring both include-actions
 * and include-menu-items individually. Can be used to include all elements from
 * another menu, then later split into more precise directives as needed.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}Menu(name = "MySideBar",
 *     extendsMenu = "CommonSideBarMenu",
 *     extendsResource = "component://common/widget/CommonMenus.xml",
 *     includeElements = {
 *         {@literal @}IncludeElements(menuName = "MyTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
 *     })
 * public interface MySideBar {}
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for menu annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({})
public @interface IncludeElements {

    /**
     * The name of the menu to include elements from.
     * Required if menuRef is not specified.
     */
    String menuName() default "";

    /**
     * The resource location of the menu to include (e.g., "component://workeffort/widget/WorkEffortMenus.xml").
     * If not specified, the menu is looked up in the same resource/class.
     */
    String resource() default "";

    /**
     * Special reference to another menu.
     * Possible values:
     * - "sub-menu-model": When this include is child of a menu-item (as sub-menu),
     *   refers to the sub-menu-model or sub-menu-include specified on that sub-menu element.
     * Required if menuName is not specified.
     */
    String menuRef() default "";

    /**
     * Defines how to recurse when included menus have their own extends and includes.
     * Default is FULL.
     */
    RecursiveMode recursive() default RecursiveMode.FULL;

    /**
     * Filter for which sub-menus to include (from menu-items).
     * Values: "none" (exclude all sub-menus) or "all" (include all sub-menus, default).
     */
    String subMenus() default "";

    /**
     * Forces the sub-menu model scope of all sub-menus of all menu items included
     * with this directive, recursively.
     * Values: "style", "func", "full", "none"
     */
    String forceSubMenuModelScope() default "";

    /**
     * Whether to include menu-item aliases as well.
     * Default: true
     */
    boolean includeMenuItemAliases() default true;

    /**
     * Menu items to exclude from the include.
     * Helps as workaround to odd merging behavior. Works recursively.
     */
    String[] excludeItems() default {};
}

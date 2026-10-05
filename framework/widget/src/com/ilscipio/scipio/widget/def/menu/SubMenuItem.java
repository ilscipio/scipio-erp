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
 * Defines a menu item for sub-menus, equivalent to widget-menu.xsd menu-item element.
 *
 * <p>This is a simplified version of MenuItem without subMenus to avoid cyclic annotation references.</p>
 *
 * <p>SCIPIO: 4.0.0: Added for menu annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
public @interface SubMenuItem {

    /**
     * Menu item name; required.
     */
    String name();

    /**
     * Menu item title.
     */
    String title() default "";

    /**
     * Tooltip text.
     */
    String tooltip() default "";

    // Styles

    /**
     * CSS class for title.
     */
    String titleStyle() default "";

    /**
     * CSS class for widget.
     */
    String widgetStyle() default "";

    /**
     * SCIPIO: CSS class for link.
     */
    String linkStyle() default "";

    /**
     * CSS class for alignment.
     */
    String alignStyle() default "";

    /**
     * CSS class for tooltip.
     */
    String tooltipStyle() default "";

    /**
     * CSS class for selected state.
     */
    String selectedStyle() default "";

    /**
     * SCIPIO: CSS class for selected ancestor state.
     */
    String selectedAncestorStyle() default "";

    /**
     * CSS class for disabled title.
     */
    String disabledTitleStyle() default "";

    // Position and layout

    /**
     * Position in menu.
     */
    String position() default "1";

    /**
     * Alignment: left or right.
     */
    Align align() default Align.LEFT;

    /**
     * Cell width.
     */
    String cellWidth() default "";

    /**
     * Associated content ID.
     */
    String associatedContentId() default "";

    /**
     * Whether to hide if selected.
     */
    String hideIfSelected() default "";

    /**
     * Target window for link.
     */
    String targetWindow() default "";

    // Behavior

    /**
     * SCIPIO: Whether the item is disabled.
     * Supports flexible expressions.
     */
    String disabled() default "";

    /**
     * Disable the item if specified field is empty.
     */
    String disableIfEmpty() default "";

    /**
     * SCIPIO: Override mode: merge, replace, remove-replace.
     */
    String overrideMode() default "";

    /**
     * SCIPIO: Sort mode: auto or off.
     */
    String sortMode() default "";

    /**
     * SCIPIO: Always expand selected or ancestor.
     */
    String alwaysExpandSelectedOrAncestor() default "";

    // Content

    /**
     * Menu item condition.
     */
    MenuItemCondition condition() default @MenuItemCondition(UNSET = true);

    /**
     * Item-level actions.
     */
    MenuActions itemActions() default @MenuActions(UNSET = true);

    /**
     * Link definition.
     */
    MenuLink link() default @MenuLink(UNSET = true);
}

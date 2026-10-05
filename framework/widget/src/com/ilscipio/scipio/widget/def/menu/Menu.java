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

import java.lang.annotation.ElementType;
import java.lang.annotation.Repeatable;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Defines a Scipio menu widget, equivalent to widget-menu.xsd menu element.
 *
 * <p>This annotation can be applied to a class or method to define a menu.</p>
 *
 * <p>Example usage:</p>
 * <pre>
 * {@literal @}Menu(name = "MyMenu", type = MenuType.SIMPLE,
 *     items = {
 *         {@literal @}MenuItem(name = "home", title = "Home",
 *             link = {@literal @}MenuLink(target = "main")),
 *         {@literal @}MenuItem(name = "admin", title = "Admin",
 *             link = {@literal @}MenuLink(target = "admin"))
 *     })
 * public interface MyMenuDef {}
 * </pre>
 *
 * <p>SCIPIO: 4.0.0: Added for menu annotations support.</p>
 */
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD})
@Repeatable(MenuList.class)
public @interface Menu {

    /**
     * Menu name; required.
     */
    String name();

    /**
     * Menu type: simple or cascade.
     */
    MenuType type() default MenuType.SIMPLE;

    /**
     * HTML id attribute.
     */
    String id() default "";

    /**
     * Menu title.
     */
    String title() default "";

    /**
     * SCIPIO: Title style for dropdown menus.
     */
    String titleStyle() default "";

    /**
     * Tooltip text.
     */
    String tooltip() default "";

    /**
     * Default entity name for field type derivation.
     */
    String defaultEntityName() default "";

    // Default styles

    /**
     * Default CSS class for titles.
     */
    String defaultTitleStyle() default "";

    /**
     * Default CSS class for widgets.
     */
    String defaultWidgetStyle() default "";

    /**
     * SCIPIO: Default link style for menu items.
     */
    String defaultLinkStyle() default "";

    /**
     * Default CSS class for tooltips.
     */
    String defaultTooltipStyle() default "";

    /**
     * Default CSS class for selected items.
     */
    String defaultSelectedStyle() default "";

    /**
     * SCIPIO: Default CSS class for selected ancestor items.
     */
    String defaultSelectedAncestorStyle() default "";

    /**
     * Default alignment style.
     */
    String defaultAlignStyle() default "";

    /**
     * Default CSS class for disabled title.
     */
    String defaultDisabledTitleStyle() default "";

    // Layout

    /**
     * Menu orientation: horizontal or vertical.
     */
    Orientation orientation() default Orientation.HORIZONTAL;

    /**
     * Default alignment: left or right.
     */
    Align defaultAlign() default Align.LEFT;

    /**
     * Menu width.
     */
    String menuWidth() default "";

    /**
     * Default cell width.
     */
    String defaultCellWidth() default "";

    /**
     * CSS class for menu container.
     */
    String menuContainerStyle() default "";

    /**
     * Fill style.
     */
    String fillStyle() default "";

    /**
     * Extra index.
     */
    String extraIndex() default "";

    // Extension

    /**
     * Menu to extend.
     */
    String extendsMenu() default "";

    /**
     * Resource location of menu to extend.
     */
    String extendsResource() default "";

    /**
     * Include elements (actions and menu items) from other menus.
     *
     * <p>This is equivalent to the XML include-elements directive. It includes
     * another menu's elements into this menu.</p>
     *
     * <p>SCIPIO: 4.0.0: Added for menu annotations support.</p>
     */
    IncludeElements[] includeElements() default {};

    // Selection

    /**
     * Default menu item name.
     */
    String defaultMenuItemName() default "";

    /**
     * Default associated content ID.
     */
    String defaultAssociatedContentId() default "";

    /**
     * Whether to hide if selected by default.
     */
    boolean defaultHideIfSelected() default false;

    /**
     * Context field name for selected menu item.
     */
    String selectedMenuItemContextFieldName() default "";

    /**
     * SCIPIO: Context field name for selected (sub-)menu.
     */
    String selectedMenuContextFieldName() default "";

    // Permissions

    /**
     * Default permission operation.
     */
    String defaultPermissionOperation() default "";

    /**
     * Default permission entity action.
     */
    String defaultPermissionEntityAction() default "";

    // SCIPIO-specific

    /**
     * SCIPIO: Items sort mode.
     */
    String itemsSortMode() default "";

    /**
     * SCIPIO: Auto sub-menu names mode.
     */
    String autoSubMenuNames() default "";

    /**
     * SCIPIO: Default sub-menu model scope.
     */
    String defaultSubMenuModelScope() default "";

    /**
     * SCIPIO: Default sub-menu include scope.
     */
    String defaultSubMenuIncludeScope() default "";

    /**
     * SCIPIO: Always expand selected or ancestor.
     */
    String alwaysExpandSelectedOrAncestor() default "";

    /**
     * SCIPIO: Separate menu type.
     */
    String separateMenuType() default "";

    /**
     * SCIPIO: Separate menu target style.
     */
    String separateMenuTargetStyle() default "";

    /**
     * SCIPIO: Item condition mode.
     */
    String itemConditionMode() default "";

    /**
     * SCIPIO: Force extends sub-menu model scope.
     */
    String forceExtendsSubMenuModelScope() default "";

    /**
     * SCIPIO: Force all sub-menu model scope.
     */
    String forceAllSubMenuModelScope() default "";

    /**
     * SCIPIO: Separate menu target preference.
     */
    String separateMenuTargetPreference() default "";

    /**
     * SCIPIO: Separate menu target original action.
     */
    String separateMenuTargetOriginalAction() default "";

    // Content

    /**
     * Menu-level actions.
     */
    MenuActions actions() default @MenuActions(UNSET = true);

    /**
     * Menu items.
     */
    MenuItem[] items() default {};

    // ========================================================================
    // Location alias attributes for backward compatibility with XML references
    // ========================================================================

    /**
     * Single alias location for backward compatibility with XML references.
     *
     * <p>When specified, lookups for this component:// location will resolve to this
     * annotated menu instead of the XML file.</p>
     *
     * <p>Example: "component://setup/widget/Menus.xml"</p>
     *
     * <p>SCIPIO: 4.0.0: Added for XML-to-annotation migration support.</p>
     */
    String location() default "";

    /**
     * Multiple alias locations for backward compatibility with XML references.
     *
     * <p>When specified, lookups for any of these component:// locations will resolve
     * to this annotated menu instead of the XML file.</p>
     *
     * <p>Example: {"component://setup/widget/Menus.xml", "component://setup/widget/OldMenus.xml"}</p>
     *
     * <p>SCIPIO: 4.0.0: Added for XML-to-annotation migration support.</p>
     */
    String[] locations() default {};
}

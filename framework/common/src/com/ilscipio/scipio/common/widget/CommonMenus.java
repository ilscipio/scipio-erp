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
package com.ilscipio.scipio.common.widget;

import com.ilscipio.scipio.widget.def.menu.*;
import com.ilscipio.scipio.widget.def.screen.*;
import com.ilscipio.scipio.widget.def.condition.Condition;
import com.ilscipio.scipio.widget.def.condition.ConditionNode;
import com.ilscipio.scipio.widget.def.condition.NestedCondition;
import com.ilscipio.scipio.widget.def.condition.NestedCondition2;
import com.ilscipio.scipio.widget.def.condition.impl.*;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CommonMenus {

    @Menu(
        name = "CommonEmptyAppBarMenu",
        location = "component://common/widget/CommonMenus.xml",
        id = "app-navigation",
        title = "${applicationTitle}&nbsp;",
        menuContainerStyle = "+menu-type-${groovy:context.menuCfgType?:'main'}",
        defaultMenuItemName = "main",
        selectedMenuItemContextFieldName = "activeMainMenuItem",
        itemsSortMode = "displaytext-ignorecase"
    )
    public interface CommonEmptyAppBarMenu {}

    @Menu(
        name = "CommonAppBarMenu",
        location = "component://common/widget/CommonMenus.xml",
        extendsMenu = "CommonEmptyAppBarMenu",
        items = {
            @MenuItem(name = "main", title = "${uiLabelMap.CommonDashboard}", widgetStyle = "+${styles.menu_sidebar_itemdashboard}", sortMode = "off", condition = @MenuItemCondition(mode = "omit", not = true, conditions = {@Condition(type = Empty.class, params = {"userLogin"})}), link = @MenuLink(target = "main", linkType = LinkType.ANCHOR))
        }
    )
    public interface CommonAppBarMenu {}

    @Menu(
        name = "CommonEmptyAppBarDashboardMenu",
        location = "component://common/widget/CommonMenus.xml",
        id = "app-navigation",
        title = "${applicationTitle}&nbsp;",
        menuContainerStyle = "+menu-type-${groovy:context.menuCfgType?:'main'}",
        defaultMenuItemName = "main",
        selectedMenuItemContextFieldName = "activeMainMenuItem",
        itemsSortMode = "displaytext-ignorecase"
    )
    public interface CommonEmptyAppBarDashboardMenu {}

    @Menu(
        name = "CommonAppBarDashboardMenu",
        location = "component://common/widget/CommonMenus.xml",
        extendsMenu = "CommonEmptyAppBarDashboardMenu",
        items = {
            @MenuItem(name = "main", title = "${uiLabelMap.CommonDashboard}", widgetStyle = "+${styles.menu_sidebar_itemdashboard}", sortMode = "off", condition = @MenuItemCondition(mode = "omit", not = true, conditions = {@Condition(type = Empty.class, params = {"userLogin"})}), link = @MenuLink(target = "main", linkType = LinkType.ANCHOR))
        }
    )
    public interface CommonAppBarDashboardMenu {}

    @Menu(
        name = "CommonEmptyAppSideBarMenu",
        location = "component://common/widget/CommonMenus.xml",
        menuContainerStyle = "+menu-type-${groovy:context.menuCfgType?:'sidebar'}",
        selectedMenuItemContextFieldName = "activeMainMenuItem",
        selectedMenuContextFieldName = "activeMainSubMenu",
        itemsSortMode = "displaytext-ignorecase",
        separateMenuType = "${groovy:context.menuCfgSepMenuType?:'default-sidebar'}",
        separateMenuTargetStyle = "${groovy:context.menuCfgSepMenuTargetStyle?:'scipio-nav-actions-menu'}",
        separateMenuTargetPreference = "${groovy:context.menuCfgSepMenuTargetPref?:'greatest-ancestor'}",
        separateMenuTargetOriginalAction = "${groovy:context.menuCfgSepMenuTargetOrigAction?:'remove-selected'}"
    )
    public interface CommonEmptyAppSideBarMenu {}

    @Menu(
        name = "CommonAppSideBarMenu",
        location = "component://common/widget/CommonMenus.xml",
        extendsMenu = "CommonEmptyAppSideBarMenu",
        items = {
            @MenuItem(name = "main", title = "${uiLabelMap.CommonDashboard}", widgetStyle = "+${styles.menu_sidebar_itemdashboard}", sortMode = "off", condition = @MenuItemCondition(mode = "omit", not = true, conditions = {@Condition(type = Empty.class, params = {"userLogin"})}), link = @MenuLink(target = "main", linkType = LinkType.ANCHOR))
        }
    )
    public interface CommonAppSideBarMenu {}

    @Menu(
        name = "CommonEmptyTabBarMenu",
        location = "component://common/widget/CommonMenus.xml",
        menuContainerStyle = "+menu-type-${groovy:context.menuCfgType?:'tab'}",
        selectedMenuItemContextFieldName = "activeSubMenuItem"
    )
    public interface CommonEmptyTabBarMenu {}

    @Menu(
        name = "CommonTabBarMenu",
        location = "component://common/widget/CommonMenus.xml",
        extendsMenu = "CommonEmptyTabBarMenu"
    )
    public interface CommonTabBarMenu {}

    @Menu(
        name = "CommonEmptySubTabBarMenu",
        location = "component://common/widget/CommonMenus.xml",
        menuContainerStyle = "+menu-type-${groovy:context.menuCfgType?:'tab'}"
    )
    public interface CommonEmptySubTabBarMenu {}

    @Menu(
        name = "CommonSubTabBarMenu",
        location = "component://common/widget/CommonMenus.xml",
        extendsMenu = "CommonEmptySubTabBarMenu"
    )
    public interface CommonSubTabBarMenu {}

    @Menu(
        name = "CommonEmptyButtonBarMenu",
        location = "component://common/widget/CommonMenus.xml",
        menuContainerStyle = "+menu-type-${groovy:context.menuCfgType?:'button'}"
    )
    public interface CommonEmptyButtonBarMenu {}

    @Menu(
        name = "CommonButtonBarMenu",
        location = "component://common/widget/CommonMenus.xml",
        extendsMenu = "CommonEmptyButtonBarMenu"
    )
    public interface CommonButtonBarMenu {}

    @Menu(
        name = "CommonEmptySideBarMenu",
        location = "component://common/widget/CommonMenus.xml",
        menuContainerStyle = "+menu-type-${groovy:context.menuCfgType?:'sidebar'}",
        selectedMenuItemContextFieldName = "activeSubMenuItem",
        itemsSortMode = "displaytext-ignorecase"
    )
    public interface CommonEmptySideBarMenu {}

    @Menu(
        name = "CommonSideBarMenu",
        location = "component://common/widget/CommonMenus.xml",
        extendsMenu = "CommonEmptySideBarMenu",
        items = {
            @MenuItem(name = "main", title = "${uiLabelMap.CommonDashboard}", widgetStyle = "+${styles.menu_sidebar_itemdashboard}", sortMode = "off", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = NotEmpty.class, params = {"userLogin"}), @Condition(type = Compare.class, params = {"currentMenuRenderState.currentDepth", "equals", "1", "Integer"})}), link = @MenuLink(target = "main", linkType = LinkType.ANCHOR))
        }
    )
    public interface CommonSideBarMenu {}

    @Menu(
        name = "CommonEmptyButtonDropdownMenu",
        location = "component://common/widget/CommonMenus.xml",
        menuContainerStyle = "+menu-type-${groovy:context.menuCfgType?:'button-dropdown'}"
    )
    public interface CommonEmptyButtonDropdownMenu {}

    @Menu(
        name = "CommonButtonDropdownMenu",
        location = "component://common/widget/CommonMenus.xml",
        extendsMenu = "CommonEmptyButtonDropdownMenu"
    )
    public interface CommonButtonDropdownMenu {}

}

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
package com.ilscipio.scipio.webtools.widget;

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
public class MiscMenus {

    @Menu(
        name = "LayoutDemoButton",
        location = "component://webtools/widget/MiscMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "Selected", title = "${uiLabelMap.CommonSelected}", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")})),
            @MenuItem(name = "Enabled", title = "${uiLabelMap.CommonEnabled}", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")}))
        }
    )
    public interface LayoutDemoButton {}

    @Menu(
        name = "LayoutDemoNestedButton",
        location = "component://webtools/widget/MiscMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        actions = @MenuActions(script = {@ScriptAction(location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/LayoutDemoNestedButton_script1.groovy")}),
        items = {
            @MenuItem(name = "top1", title = "Toplevel1", itemActions = @MenuActions(script = {@ScriptAction(location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/LayoutDemoNestedButton_top1_script2.groovy")}), link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")})),
            @MenuItem(name = "top2", title = "Toplevel2", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")})),
            @MenuItem(name = "top3", title = "Toplevel3", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")}, image = @MenuImage(src = "/images/products/GZ-1000/small.png")))
        }
    )
    public interface LayoutDemoNestedButton {}

    @Menu(
        name = "LayoutDemoButtonDropdownMenuModel",
        location = "component://webtools/widget/MiscMenus.xml",
        defaultLinkStyle = "+my-dropdown-item-link",
        extendsMenu = "CommonButtonDropdownMenu",
        extendsResource = "component://common/widget/CommonMenus.xml"
    )
    public interface LayoutDemoButtonDropdownMenuModel {}

    @Menu(
        name = "LayoutDemoButton2",
        location = "component://webtools/widget/MiscMenus.xml",
        defaultWidgetStyle = "+my-list-item",
        menuContainerStyle = "+my-special-button-menu",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "top1", title = "Toplevel1", widgetStyle = "my-list-item-override-style", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")})),
            @MenuItem(name = "top2", title = "Toplevel2", widgetStyle = "+my-list-item-extra-style", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")})),
            @MenuItem(name = "top3", subMenus = {@SubMenu(model = "component://common/widget/CommonMenus.xml#CommonButtonDropdownMenu")}),
            @MenuItem(name = "top3b"),
            @MenuItem(name = "top4", title = "Toplevel4", disableIfEmpty = "nonExistentVar", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")})),
            @MenuItem(name = "top5", subMenus = {@SubMenu(model = "component://webtools/widget/MiscMenus.xml#LayoutDemoButtonDropdownMenuModel")}),
            @MenuItem(name = "top6", subMenus = {@SubMenu(include = "component://webtools/widget/MiscMenus.xml#LayoutDemoButtonDropdown", modelScope = "full")}),
            @MenuItem(name = "top7", subMenus = {@SubMenu(name = "LayoutDemoButtonDropdownTest2", include = "component://webtools/widget/MiscMenus.xml#LayoutDemoButtonDropdown", modelScope = "full")}),
            @MenuItem(name = "top6", subMenus = {@SubMenu(name = "LayoutDemoButtonDropdownTest3", include = "component://webtools/widget/MiscMenus.xml#LayoutDemoButtonDropdown", modelScope = "full"), @SubMenu(name = "LayoutDemoButtonDropdownTest4", include = "component://webtools/widget/MiscMenus.xml#LayoutDemoButtonDropdown", modelScope = "full")})
        }
    )
    public interface LayoutDemoButton2 {}

    @Menu(
        name = "LayoutDemoButtonDropdown",
        location = "component://webtools/widget/MiscMenus.xml",
        title = "Dropdown button menu",
        extendsMenu = "CommonButtonDropdownMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "Selected", title = "${uiLabelMap.CommonSelected}", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")})),
            @MenuItem(name = "Enabled", title = "${uiLabelMap.CommonEnabled}", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")}))
        }
    )
    public interface LayoutDemoButtonDropdown {}

    @Menu(
        name = "LayoutDemoTest1",
        location = "component://webtools/widget/MiscMenus.xml",
        actions = @MenuActions(set = {@SetAction(field = "demoTestVar1", value = "demoTestValue1")}),
        items = {
            @MenuItem(name = "DemoTest1", title = "Demo Test 1", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")})),
            @MenuItem(name = "DemoTest2", title = "Demo Test 2", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")}))
        }
    )
    public interface LayoutDemoTest1 {}

    @Menu(
        name = "LayoutDemoTest2",
        location = "component://webtools/widget/MiscMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "LayoutDemoTest1")
        },
        items = {
            @MenuItem(name = "DemoTest3", title = "Demo Test 3", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1")})),
            @MenuItem(name = "DemoTest4", title = "Demo Test 4", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1")}))
        }
    )
    public interface LayoutDemoTest2 {}

    @Menu(
        name = "LayoutDemoButton2NoSubMenus",
        location = "component://webtools/widget/MiscMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "LayoutDemoButton2", subMenus = "none")
        },
        items = {
            @MenuItem(name = "top7", subMenus = {@SubMenu(name = "LayoutDemoButtonDropdownTest2", include = "component://webtools/widget/MiscMenus.xml#LayoutDemoButtonDropdown", modelScope = "full")})
        }
    )
    public interface LayoutDemoButton2NoSubMenus {}

    @Menu(
        name = "LayoutDemoTest3",
        location = "component://webtools/widget/MiscMenus.xml",
        items = {
            @MenuItem(name = "DemoTest1", title = "Demo Test 1", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1")})),
            @MenuItem(name = "DemoTest2", title = "Demo Test 2", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1")}), subMenus = {@SubMenu()}),
            @MenuItem(name = "myMenuPassedVar1", title = "${groovy: context.myMenuPassedVar1 ?: 'missing'}", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1")})),
            @MenuItem(name = "myMenuPassedGlobalVar1", title = "${groovy: context.myMenuPassedGlobalVar1 ?: 'missing'}", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1")})),
            @MenuItem(name = "myMenuPassedReqAttrib1", title = "${groovy: request.getAttribute('myMenuPassedReqAttrib1') ?: 'missing'}", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1")}))
        }
    )
    public interface LayoutDemoTest3 {}

    @Menu(
        name = "WebtoolsPlainSiteMapDemo",
        location = "component://webtools/widget/MiscMenus.xml",
        selectedMenuItemContextFieldName = "activeMainMenuItem",
        selectedMenuContextFieldName = "activeMainMenu"
    )
    public interface WebtoolsPlainSiteMapDemo {}

    @Menu(
        name = "WebtoolsPlainSiteMapDemo2",
        location = "component://webtools/widget/MiscMenus.xml"
    )
    public interface WebtoolsPlainSiteMapDemo2 {}

    @Menu(
        name = "WebtoolsPlainSiteMapDemo3",
        location = "component://webtools/widget/MiscMenus.xml",
        extendsMenu = "WebtoolsPlainSiteMapDemo2",
        forceAllSubMenuModelScope = "func",
        items = {
            @MenuItem(name = "extraInsertedItem", title = "Extra inserted item", subMenus = {@SubMenu(model = "component://webtools/widget/MiscMenus.xml#LayoutDemoButtonDropdownMenuModel")})
        }
    )
    public interface WebtoolsPlainSiteMapDemo3 {}

    @Menu(
        name = "WebtoolsInlineSectionMenuDemo",
        location = "component://webtools/widget/MiscMenus.xml",
        items = {
            @MenuItem(name = "item1", title = "Inline-Section Menu Item 1", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")})),
            @MenuItem(name = "item2", title = "Inline-Section Menu Item 2", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")}))
        }
    )
    public interface WebtoolsInlineSectionMenuDemo {}

    @Menu(
        name = "WebtoolsMenuActionsDemo1",
        location = "component://webtools/widget/MiscMenus.xml",
        actions = @MenuActions(set = {@SetAction(field = "commonActionField6", value = "This value 6 was set in WebtoolsMenuActionsDemo1. [SUCCESS]"), @SetAction(field = "commonActionField7", value = "This value 7 was set in WebtoolsMenuActionsDemo1 but should be overridden... [ERROR]"), @SetAction(field = "commonActionField8", value = "This value 8 was set in WebtoolsMenuActionsDemo1 but should be overridden... [ERROR]"), @SetAction(field = "commonActionField9", value = "This value 9 was set in WebtoolsMenuActionsDemo1 but should be overridden... [ERROR]")})
    )
    public interface WebtoolsMenuActionsDemo1 {}

    @Menu(
        name = "WebtoolsMenuActionsDemo2",
        location = "component://webtools/widget/MiscMenus.xml",
        actions = @MenuActions(set = {@SetAction(field = "commonActionField7", value = "This value 7 was set in WebtoolsMenuActionsDemo2. [SUCCESS]"), @SetAction(field = "commonActionField8", value = "This value 8 was set in WebtoolsMenuActionsDemo2 but should be overridden... [ERROR]"), @SetAction(field = "commonActionField9", value = "This value 9 was set in WebtoolsMenuActionsDemo2 but should be overridden... [ERROR]")})
    )
    public interface WebtoolsMenuActionsDemo2 {}

    @Menu(
        name = "WebtoolsMenuActionsDemo3",
        location = "component://webtools/widget/MiscMenus.xml",
        actions = @MenuActions(set = {@SetAction(field = "commonActionField8", value = "This value 8 was set in WebtoolsMenuActionsDemo3. [SUCCESS]"), @SetAction(field = "commonActionField9", value = "This value 9 was set in WebtoolsMenuActionsDemo3 but should be overridden... [ERROR]")})
    )
    public interface WebtoolsMenuActionsDemo3 {}

    @Menu(
        name = "WebtoolsMenuActionsDemo4",
        location = "component://webtools/widget/MiscMenus.xml",
        extendsMenu = "WebtoolsMenuActionsDemo1",
        actions = @MenuActions(set = {@SetAction(field = "commonActionField9", value = "This value 9 was set in WebtoolsMenuActionsDemo4. [SUCCESS]")})
    )
    public interface WebtoolsMenuActionsDemo4 {}

    @Menu(
        name = "TargetedRenderingTestMenu1",
        location = "component://webtools/widget/MiscMenus.xml",
        id = "TargetedRenderingTestMenu1",
        items = {
            @MenuItem(name = "item1", title = "Inline-Section Menu Item 1", link = @MenuLink(target = "${demoTargetUrl}", parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")})),
            @MenuItem(name = "item2", title = "Inline-Section Menu Item 2", link = @MenuLink(target = "${demoTargetUrl}", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "demoParam1", fromField = "demoParam1"), @MenuParameter(paramName = "demoParam2", fromField = "demoParam2"), @MenuParameter(paramName = "demoParam3", fromField = "demoParam3")}))
        }
    )
    public interface TargetedRenderingTestMenu1 {}

}

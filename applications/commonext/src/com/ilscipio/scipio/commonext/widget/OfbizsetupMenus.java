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
package com.ilscipio.scipio.commonext.widget;

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
public class OfbizsetupMenus {

    @Menu(
        name = "SetupAppBar",
        location = "component://commonext/widget/ofbizsetup/Menus.xml",
        title = "${uiLabelMap.SetupApp}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "main", title = "${uiLabelMap.SetupInitialSetup}", sortMode = "off", link = @MenuLink(target = "initialsetup")),
            @MenuItem(name = "export", title = "${uiLabelMap.PageTitleEntityExportAll}", link = @MenuLink(target = "EntityExportAll"))
        }
    )
    public interface SetupAppBar {}

    @Menu(
        name = "SetupAppSideBar",
        location = "component://commonext/widget/ofbizsetup/Menus.xml",
        title = "${uiLabelMap.SetupApp}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "SetupAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true"
    )
    public interface SetupAppSideBar {}

    @Menu(
        name = "SetupTabBar",
        location = "component://commonext/widget/ofbizsetup/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        selectedMenuItemContextFieldName = "activeSubMenuItemTop",
        items = {
            @MenuItem(name = "organization", title = "${uiLabelMap.SetupOrganization}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"partyId"})}), link = @MenuLink(target = "initialsetup")),
            @MenuItem(name = "facility", title = "${uiLabelMap.SetupFacility}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"partyId"})}), link = @MenuLink(target = "EditFacility", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "productstore", title = "${uiLabelMap.SetupProductStore}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"partyId"})}), link = @MenuLink(target = "EditProductStore", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "website", title = "${uiLabelMap.SetupWebSite}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"partyId"})}), link = @MenuLink(target = "EditWebSite", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "firstcustomer", title = "${uiLabelMap.SetupFirstCustomer}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"partyId"})}), link = @MenuLink(target = "firstcustomer")),
            @MenuItem(name = "firstproduct", title = "${uiLabelMap.SetupFirstProduct}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"partyId"})}), link = @MenuLink(target = "firstproduct", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")}))
        }
    )
    public interface SetupTabBar {}

    @Menu(
        name = "SetupSideBar",
        location = "component://commonext/widget/ofbizsetup/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "SetupTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        selectedMenuItemContextFieldName = "activeSubMenuItemTop"
    )
    public interface SetupSideBar {}

    @Menu(
        name = "FirstProductTabBar",
        location = "component://commonext/widget/ofbizsetup/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "productcatalog",
        items = {
            @MenuItem(name = "productcatalog", title = "${uiLabelMap.SetupProductCatalog}", link = @MenuLink(target = "firstproduct", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "productcategory", title = "${uiLabelMap.ProductCategory}", link = @MenuLink(target = "EditCategory", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")})),
            @MenuItem(name = "product", title = "${uiLabelMap.ProductProduct}", link = @MenuLink(target = "EditProduct", parameters = {@MenuParameter(paramName = "partyId", fromField = "partyId")}))
        }
    )
    public interface FirstProductTabBar {}

    @Menu(
        name = "personUpdate",
        location = "component://commonext/widget/ofbizsetup/Menus.xml",
        items = {
            @MenuItem(name = "update", title = "${uiLabelMap.CommonUpdate}", link = @MenuLink(target = "editperson", parameters = {@MenuParameter(paramName = "customerPartyId", fromField = "customerPartyId"), @MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")}))
        }
    )
    public interface personUpdate {}

    @Menu(
        name = "groupUpdate",
        location = "component://commonext/widget/ofbizsetup/Menus.xml",
        items = {
            @MenuItem(name = "update", title = "${uiLabelMap.CommonUpdate}", link = @MenuLink(target = "editpartygroup", parameters = {@MenuParameter(paramName = "partyId", fromField = "organizationPartyId")}))
        }
    )
    public interface groupUpdate {}

}

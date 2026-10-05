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
package com.ilscipio.scipio.setup.widget;

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
public class Menus {

    @Menu(
        name = "SetupAppBar",
        location = "component://setup/widget/Menus.xml",
        title = "${uiLabelMap.SetupApp}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "wizard", title = "${uiLabelMap.SetupSetupWizard}", sortMode = "off", link = @MenuLink(target = "setupWizard", linkType = LinkType.ANCHOR, useWhen = "/* TODO: parameter-map from-field=setupStepStates.finished.stepParams */")),
            @MenuItem(name = "export", title = "${uiLabelMap.PageTitleEntityExportAll}", link = @MenuLink(target = "EntityExportAll"))
        }
    )
    public interface SetupAppBar {}

    @Menu(
        name = "SetupAppSideBar",
        location = "component://setup/widget/Menus.xml",
        title = "${uiLabelMap.SetupApp}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "SetupAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true",
        items = {
            @MenuItem(name = "wizard", subMenus = {@SubMenu(name = "SetupSteps", include = "component://setup/widget/Menus.xml#SetupStepsSideBar")})
        }
    )
    public interface SetupAppSideBar {}

    @Menu(
        name = "SetupTabBar",
        location = "component://setup/widget/Menus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
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
        location = "component://setup/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "SetupTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface SetupSideBar {}

    @Menu(
        name = "FirstProductTabBar",
        location = "component://setup/widget/Menus.xml",
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
        location = "component://setup/widget/Menus.xml",
        items = {
            @MenuItem(name = "update", title = "${uiLabelMap.CommonUpdate}", link = @MenuLink(target = "editperson", parameters = {@MenuParameter(paramName = "customerPartyId", fromField = "customerPartyId"), @MenuParameter(paramName = "organizationPartyId", fromField = "organizationPartyId")}))
        }
    )
    public interface personUpdate {}

    @Menu(
        name = "groupUpdate",
        location = "component://setup/widget/Menus.xml",
        items = {
            @MenuItem(name = "update", title = "${uiLabelMap.CommonUpdate}", link = @MenuLink(target = "editpartygroup", parameters = {@MenuParameter(paramName = "partyId", fromField = "organizationPartyId")}))
        }
    )
    public interface groupUpdate {}

    @Menu(
        name = "SetupStepsSideBar",
        location = "component://setup/widget/Menus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        itemsSortMode = "off",
        actions = @MenuActions(script = {@ScriptAction(location = "component://setup/webapp/setup/WEB-INF/actions/generated/SetupStepsSideBar_script1.groovy")}),
        items = {
            @MenuItem(name = "organization", title = "${uiLabelMap[setupStepTitlePropMap['organization']]}", widgetStyle = "+${context.stepStyles.organization}", disabled = "${context.setupStepDisabledMap.organization == true}", link = @MenuLink(target = "setupOrganization", linkType = LinkType.ANCHOR, useWhen = "/* TODO: parameter-map from-field=setupStepStates.organization.stepParams */")),
            @MenuItem(name = "accounting", title = "${uiLabelMap[setupStepTitlePropMap['accounting']]}", widgetStyle = "+${context.stepStyles.accounting}", disabled = "${context.setupStepDisabledMap.accounting == true}", link = @MenuLink(target = "setupAccounting", linkType = LinkType.ANCHOR, useWhen = "/* TODO: parameter-map from-field=setupStepStates.accounting.stepParams */")),
            @MenuItem(name = "facility", title = "${uiLabelMap[setupStepTitlePropMap['facility']]}", widgetStyle = "+${context.stepStyles.facility}", disabled = "${context.setupStepDisabledMap.facility == true}", link = @MenuLink(target = "setupFacility", linkType = LinkType.ANCHOR, useWhen = "/* TODO: parameter-map from-field=setupStepStates.facility.stepParams */")),
            @MenuItem(name = "store", title = "${uiLabelMap[setupStepTitlePropMap['store']]}", widgetStyle = "+${context.stepStyles.store}", disabled = "${context.setupStepDisabledMap.store == true}", link = @MenuLink(target = "setupStore", linkType = LinkType.ANCHOR, useWhen = "/* TODO: parameter-map from-field=setupStepStates.store.stepParams */")),
            @MenuItem(name = "catalog", title = "${uiLabelMap[setupStepTitlePropMap['catalog']]}", widgetStyle = "+${context.stepStyles.catalog}", disabled = "${context.setupStepDisabledMap.catalog == true}", link = @MenuLink(target = "setupCatalog", linkType = LinkType.ANCHOR, useWhen = "/* TODO: parameter-map from-field=setupStepStates.catalog.stepParams */")),
            @MenuItem(name = "user", title = "${uiLabelMap[setupStepTitlePropMap['user']]}", widgetStyle = "+${context.stepStyles.user}", disabled = "${context.setupStepDisabledMap.user == true}", link = @MenuLink(target = "setupUser", linkType = LinkType.ANCHOR, useWhen = "/* TODO: parameter-map from-field=setupStepStates.user.stepParams */"))
        }
    )
    public interface SetupStepsSideBar {}

}

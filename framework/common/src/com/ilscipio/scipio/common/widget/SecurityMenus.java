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
public class SecurityMenus {

    @Menu(
        name = "SecurityGroupTabBar",
        location = "component://common/widget/SecurityMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "EditSecurityGroup",
        items = {
            @MenuItem(name = "FindUserLogin", title = "${uiLabelMap.FindUserLogin}", link = @MenuLink(target = "FindUserLogin")),
            @MenuItem(name = "FindSecurityGroup", title = "${uiLabelMap.PageTitleFindSecurityGroup}", link = @MenuLink(target = "FindSecurityGroup")),
            @MenuItem(name = "EditUserLogin", title = "${uiLabelMap.UserLogin}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"userLoginId"})}), link = @MenuLink(target = "editlogin", parameters = {@MenuParameter(paramName = "userLoginId", fromField = "userLoginId")})),
            @MenuItem(name = "EditSecurityGroup", title = "${uiLabelMap.SecurityGroups}", alwaysExpandSelectedOrAncestor = "false", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"groupId"})}), link = @MenuLink(target = "EditSecurityGroup", parameters = {@MenuParameter(paramName = "groupId", fromField = "groupId")})),
            @MenuItem(name = "EditUserLoginSecurityGroups", title = "${uiLabelMap.SecurityGroups}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"userLoginId"})}), link = @MenuLink(target = "EditUserLoginSecurityGroups", parameters = {@MenuParameter(paramName = "userLoginId", fromField = "userLoginId")})),
            @MenuItem(name = "EditSecurityGroupPermissions", title = "${uiLabelMap.Permissions}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"groupId"})}), link = @MenuLink(target = "EditSecurityGroupPermissions", parameters = {@MenuParameter(paramName = "groupId", fromField = "groupId")})),
            @MenuItem(name = "EditSecurityGroupUserLogins", title = "${uiLabelMap.UserLogins}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"groupId"})}), link = @MenuLink(target = "EditSecurityGroupUserLogins", parameters = {@MenuParameter(paramName = "groupId", fromField = "groupId")})),
            @MenuItem(name = "EditSecurityGroupProtectedViews", title = "${uiLabelMap.ProtectedViews}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"groupId"})}), link = @MenuLink(target = "EditSecurityGroupProtectedViews", parameters = {@MenuParameter(paramName = "groupId", fromField = "groupId")})),
            @MenuItem(name = "EditCertIssuerProvisions", title = "${uiLabelMap.CertIssuers}", link = @MenuLink(target = "EditCertIssuerProvisions"))
        }
    )
    public interface SecurityGroupTabBar {}

    @Menu(
        name = "SecurityGroupSideBar",
        location = "component://common/widget/SecurityMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "SecurityGroupTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "EditSecurityGroup"
    )
    public interface SecurityGroupSideBar {}

}

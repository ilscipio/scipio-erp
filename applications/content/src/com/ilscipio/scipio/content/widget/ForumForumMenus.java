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
package com.ilscipio.scipio.content.widget;

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
public class ForumForumMenus {

    @Menu(
        name = "ForumGroupTabBar",
        location = "component://content/widget/forum/ForumMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "forumGroups", title = "${uiLabelMap.FormFieldTitle_forumGroups}", link = @MenuLink(target = "findForumGroups")),
            @MenuItem(name = "forums", title = "${uiLabelMap.FormFieldTitle_forums}", link = @MenuLink(target = "findForums", parameters = {@MenuParameter(paramName = "forumGroupId", fromField = "parameters.forumGroupId")})),
            @MenuItem(name = "forums", title = "${uiLabelMap.FormFieldTitle_forums}"),
            @MenuItem(name = "purposes", title = "${uiLabelMap.FormFieldTitle_purposes}", link = @MenuLink(target = "forumGroupPurposes", parameters = {@MenuParameter(paramName = "forumGroupId", fromField = "parameters.forumGroupId")})),
            @MenuItem(name = "purposes", title = "${uiLabelMap.FormFieldTitle_purposes}"),
            @MenuItem(name = "roles", title = "${uiLabelMap.FormFieldTitle_roles}", link = @MenuLink(target = "forumGroupRoles", parameters = {@MenuParameter(paramName = "forumGroupId", fromField = "parameters.forumGroupId")})),
            @MenuItem(name = "roles", title = "${uiLabelMap.FormFieldTitle_roles}")
        }
    )
    public interface ForumGroupTabBar {}

    @Menu(
        name = "ForumGroupSideBar",
        location = "component://content/widget/forum/ForumMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ForumGroupTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ForumGroupSideBar {}

    @Menu(
        name = "ForumTabBar",
        location = "component://content/widget/forum/ForumMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "Survey"
    )
    public interface ForumTabBar {}

    @Menu(
        name = "ForumSideBar",
        location = "component://content/widget/forum/ForumMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ForumTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "Survey"
    )
    public interface ForumSideBar {}

    @Menu(
        name = "ForumMessagesTabBar",
        location = "component://content/widget/forum/ForumMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "forumGroups", title = "${uiLabelMap.FormFieldTitle_forumGroups}", link = @MenuLink(target = "findForumGroups")),
            @MenuItem(name = "forums", title = "${uiLabelMap.FormFieldTitle_forums}", link = @MenuLink(target = "findForums", parameters = {@MenuParameter(paramName = "forumGroupId", fromField = "parameters.forumGroupId")})),
            @MenuItem(name = "forums", title = "${uiLabelMap.FormFieldTitle_forums}"),
            @MenuItem(name = "messageList", title = "${uiLabelMap.FormFieldTitle_messageList}", link = @MenuLink(target = "findForumMessages", parameters = {@MenuParameter(paramName = "forumGroupId", fromField = "parameters.forumGroupId"), @MenuParameter(paramName = "forumId", fromField = "parameters.forumId")})),
            @MenuItem(name = "messageThread", title = "${uiLabelMap.FormFieldTitle_messageThread}", link = @MenuLink(target = "findForumThreads", parameters = {@MenuParameter(paramName = "forumGroupId", fromField = "parameters.forumGroupId"), @MenuParameter(paramName = "forumId", fromField = "parameters.forumId")}))
        }
    )
    public interface ForumMessagesTabBar {}

    @Menu(
        name = "ForumMessagesSideBar",
        location = "component://content/widget/forum/ForumMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ForumMessagesTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ForumMessagesSideBar {}

}

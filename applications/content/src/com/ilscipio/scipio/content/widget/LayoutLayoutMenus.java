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
public class LayoutLayoutMenus {

    @Menu(
        name = "LayoutButtonBar",
        location = "component://content/widget/layout/LayoutMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        items = {
            @MenuItem(name = "ListLayout", title = "${uiLabelMap.ContentListOwnCreatedTemplates}", link = @MenuLink(target = "ListLayout", targetWindow = "_top")),
            @MenuItem(name = "FindLayout", title = "${uiLabelMap.CommonFind}", widgetStyle = "+${styles.action_nav} ${styles.action_find}", link = @MenuLink(target = "FindLayout", targetWindow = "_top")),
            @MenuItem(name = "EditLayout", title = "${uiLabelMap.CommonEdit}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"parameters.contentId"}), @Condition(type = NotEmpty.class, params = {"parameters.drDataResourceId"})}), link = @MenuLink(target = "EditLayout", targetWindow = "_top", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId"), @MenuParameter(paramName = "drDataResourceId", fromField = "parameters.drDataResourceId")})),
            @MenuItem(name = "EditLayoutSubContent", title = "${uiLabelMap.ContentSubContent}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"parameters.contentId"}), @Condition(type = NotEmpty.class, params = {"parameters.drDataResourceId"})}), link = @MenuLink(target = "EditLayoutSubContent", targetWindow = "_top", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId"), @MenuParameter(paramName = "drDataResourceId", fromField = "parameters.drDataResourceId")})),
            @MenuItem(name = "EditLayoutText", title = "${uiLabelMap.ContentText}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"parameters.contentId"}), @Condition(type = NotEmpty.class, params = {"parameters.drDataResourceId"})}), link = @MenuLink(target = "EditLayoutText", targetWindow = "_top", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId"), @MenuParameter(paramName = "drDataResourceId", fromField = "parameters.drDataResourceId")})),
            @MenuItem(name = "EditLayoutHtml", title = "${uiLabelMap.ContentHtml}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"parameters.contentId"}), @Condition(type = NotEmpty.class, params = {"parameters.drDataResourceId"})}), link = @MenuLink(target = "EditLayoutHtml", targetWindow = "_top", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId"), @MenuParameter(paramName = "drDataResourceId", fromField = "parameters.drDataResourceId")})),
            @MenuItem(name = "EditLayoutImage", title = "${uiLabelMap.ContentImage}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"parameters.contentId"}), @Condition(type = NotEmpty.class, params = {"parameters.drDataResourceId"})}), link = @MenuLink(target = "EditLayoutImage", targetWindow = "_top", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId"), @MenuParameter(paramName = "drDataResourceId", fromField = "parameters.drDataResourceId")})),
            @MenuItem(name = "EditLayoutUrl", title = "${uiLabelMap.ContentUrl}", condition = @MenuItemCondition(conditions = {@Condition(type = NotEmpty.class, params = {"parameters.contentId"}), @Condition(type = NotEmpty.class, params = {"parameters.drDataResourceId"})}), link = @MenuLink(target = "EditLayoutUrl", targetWindow = "_top", parameters = {@MenuParameter(paramName = "contentId", fromField = "parameters.contentId"), @MenuParameter(paramName = "drDataResourceId", fromField = "parameters.drDataResourceId")}))
        }
    )
    public interface LayoutButtonBar {}

    @Menu(
        name = "LayoutSideBar",
        location = "component://content/widget/layout/LayoutMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "LayoutButtonBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}"
    )
    public interface LayoutSideBar {}

}

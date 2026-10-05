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
public class CmsCMSMenus {

    @Menu(
        name = "CmsTabBar",
        location = "component://content/widget/cms/CMSMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        selectedMenuItemContextFieldName = "currentCMSMenuItemName",
        defaultPermissionOperation = "HAS_AUTHOR_ROLE|CONTENT_ADMIN",
        defaultPermissionEntityAction = "_ADMIN",
        items = {
            @MenuItem(name = "contentfind", title = "${uiLabelMap.ContentContent}", link = @MenuLink(target = "CMSContentFind", id = "contentfind")),
            @MenuItem(name = "subsites", title = "${uiLabelMap.ContentSubSites}", link = @MenuLink(target = "CMSSites", id = "sites", parameters = {@MenuParameter(paramName = "forumId", fromField = "subsites.forumId")}))
        }
    )
    public interface CmsTabBar {}

    @Menu(
        name = "CmsSideBar",
        location = "component://content/widget/cms/CMSMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "CmsTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        selectedMenuItemContextFieldName = "currentCMSMenuItemName",
        defaultPermissionOperation = "HAS_AUTHOR_ROLE|CONTENT_ADMIN",
        defaultPermissionEntityAction = "_ADMIN"
    )
    public interface CmsSideBar {}

}

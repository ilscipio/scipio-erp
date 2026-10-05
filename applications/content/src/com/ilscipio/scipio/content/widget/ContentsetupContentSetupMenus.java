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
public class ContentsetupContentSetupMenus {

    @Menu(
        name = "ContentSetupButtonBar",
        location = "component://content/widget/contentsetup/ContentSetupMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        selectedMenuItemContextFieldName = "currentMenuItemName",
        items = {
            @MenuItem(name = "contentPurposeOp", title = "${uiLabelMap.PageTitleEditContentPurposeOperation}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", link = @MenuLink(target = "EditContentPurposeOperation", targetWindow = "_top")),
            @MenuItem(name = "contentOp", title = "${uiLabelMap.PageTitleEditContentOperation}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", link = @MenuLink(target = "EditContentOperation", targetWindow = "_top")),
            @MenuItem(name = "assocPred", title = "${uiLabelMap.PageTitleEditContentAssocPredicate}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", link = @MenuLink(target = "EditContentAssocPredicate", targetWindow = "_top")),
            @MenuItem(name = "typeAttr", title = "${uiLabelMap.PageTitleEditContentTypeAttribute}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", link = @MenuLink(target = "EditContentTypeAttr", targetWindow = "_top")),
            @MenuItem(name = "purposeType", title = "${uiLabelMap.PageTitleEditContentPurposeType}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", link = @MenuLink(target = "EditContentPurposeType", targetWindow = "_top")),
            @MenuItem(name = "assocType", title = "${uiLabelMap.PageTitleEditContentAssocType}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", link = @MenuLink(target = "EditContentAssocType", targetWindow = "_top")),
            @MenuItem(name = "type", title = "${uiLabelMap.PageTitleEditContentType}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", link = @MenuLink(target = "EditContentType", targetWindow = "_top")),
            @MenuItem(name = "userpermissions", title = "${uiLabelMap.PageTitleEditContentUserPermissions}", widgetStyle = "+${styles.action_nav} ${styles.action_update}", link = @MenuLink(target = "UserPermissions", targetWindow = "_top", name = "UserPermissions"))
        }
    )
    public interface ContentSetupButtonBar {}

    @Menu(
        name = "ContentSetupSideBar",
        location = "component://content/widget/contentsetup/ContentSetupMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ContentSetupButtonBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        selectedMenuItemContextFieldName = "currentMenuItemName"
    )
    public interface ContentSetupSideBar {}

}

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
public class DatasetupDataResourceSetupMenus {

    @Menu(
        name = "DataResourceSetupButtonBar",
        location = "component://content/widget/datasetup/DataResourceSetupMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}",
        items = {
            @MenuItem(name = "EditDataResourceType", title = "${uiLabelMap.CommonType}", link = @MenuLink(target = "EditDataResourceType", targetWindow = "_top")),
            @MenuItem(name = "EditCharacterSet", title = "${uiLabelMap.ContentCharacterSet}", link = @MenuLink(target = "EditCharacterSet", targetWindow = "_top")),
            @MenuItem(name = "EditDataCategory", title = "${uiLabelMap.ContentCategory}", link = @MenuLink(target = "EditDataCategory", targetWindow = "_top")),
            @MenuItem(name = "EditDataResourceTypeAttr", title = "${uiLabelMap.ContentTypeAttr}", link = @MenuLink(target = "EditDataResourceTypeAttr", targetWindow = "_top")),
            @MenuItem(name = "EditFileExtension", title = "${uiLabelMap.ContentFileExt}", link = @MenuLink(target = "EditFileExtension", targetWindow = "_top")),
            @MenuItem(name = "EditMetaDataPredicate", title = "${uiLabelMap.ContentMetaDataPred}", link = @MenuLink(target = "EditMetaDataPredicate", targetWindow = "_top")),
            @MenuItem(name = "EditMimeType", title = "${uiLabelMap.ContentMimeType}", link = @MenuLink(target = "EditMimeType", targetWindow = "_top")),
            @MenuItem(name = "EditMimeTypeHtmlTemplate", title = "${uiLabelMap.ContentMimeTypeHtmlTemplate}", link = @MenuLink(target = "EditMimeTypeHtmlTemplate", targetWindow = "_top"))
        }
    )
    public interface DataResourceSetupButtonBar {}

    @Menu(
        name = "DataResourceSetupSideBar",
        location = "component://content/widget/datasetup/DataResourceSetupMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "DataResourceSetupButtonBar", recursive = RecursiveMode.INCLUDES_ONLY)
        },
        defaultMenuItemName = "content",
        defaultAssociatedContentId = "${userLogin.userLoginId}"
    )
    public interface DataResourceSetupSideBar {}

}

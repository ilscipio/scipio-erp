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
package com.ilscipio.scipio.cms.widget;

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
public class WebsiteCommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://cms/widget/website/CommonScreens.xml")
    public interface webapp_common_actions {}

    @Screen(name = "static-common-actions", location = "component://cms/widget/website/CommonScreens.xml")
    public interface static_common_actions {}

    @Screen(name = "main-decorator", location = "component://cms/widget/website/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CMSUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.CMSCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.CMSCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "cms", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "MainAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://cms/widget/website/WebsiteMenus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.CMSApplication}", global = true)
    @DecoratorScreen(
        name = "GlobalDecorator",
        location = "component://common/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "content-full-screen", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "column-main", location = "component://common/widget/CommonScreens.xml"
            )})
        }
    )
    public interface main_decorator {}

}

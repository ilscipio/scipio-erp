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

import com.ilscipio.scipio.widget.def.screen.*;

/**
 * Screens of the server root webapp ("/"): the application launcher.
 *
 * <p>The decorator sets {@code auNoSideColumn}: a theme that has a side column (Aurora) leaves it
 * out, because the launcher is the list of applications itself.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class RootScreens {

    @Screen(name = "main-decorator", location = "component://commonext/widget/RootScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "root", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.CommonApplications}", global = true)
    @Action(type = ActionType.SET, field = "auNoSideColumn", value = "true", valueType = "Boolean", global = true)
    @DecoratorScreen(
        name = "ApplicationDecorator",
        location = "component://commonext/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")
            })
        }
    )
    public interface main_decorator {}

    @Screen(name = "main", location = "component://commonext/widget/RootScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CommonApplications")
    @Action(type = ActionType.SET, field = "noTitle", value = "true", global = true)
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://commonext/webapp/root/launcher.ftl")
            })
        }
    )
    public interface main {}
}

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
package com.ilscipio.scipio.marketing.widget;

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
public class SfaCalendarScreens {

    @Screen(name = "Calendar", location = "component://marketing/widget/sfa/CalendarScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "parameters.period", fromField = "parameters.period", defaultValue = "${initialView}")
    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/calendar/CreateUrlParam.groovy")
    @Action(type = ActionType.SET, field = "parentTypeId", fromField = "parameters.parentTypeId", defaultValue = "EVENT")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CalendarOnly")}))
    public interface Calendar {}

    @Screen(name = "CalendarOnly", location = "component://marketing/widget/sfa/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "eventDetailWidgetName", fromField = "eventDetailWidgetName", defaultValue = "eventDetail")
    @Action(type = ActionType.SET, field = "eventDetailWidgetLocation", fromField = "eventDetailWidgetLocation", defaultValue = "component://marketing/widget/sfa/CalendarScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CalendarOnly", location = "component://workeffort/widget/CalendarScreens.xml")}))
    public interface CalendarOnly {}

    @Screen(name = "CalendarWithDecorator", location = "component://marketing/widget/sfa/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "parameters.period", fromField = "parameters.period", defaultValue = "${initialView}")
    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/calendar/CreateUrlParam.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WorkEffortCalendar")
    @DecoratorScreen(
        name = "CommonCalendarDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CalendarOnly", location = "component://marketing/widget/sfa/CalendarScreens.xml"
            )})
        }
    )
    public interface CalendarWithDecorator {}

    @Screen(name = "eventDetail", location = "component://marketing/widget/sfa/CalendarScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "eventDetail", location = "component://workeffort/widget/CalendarScreens.xml")}))
    public interface eventDetail {}

}

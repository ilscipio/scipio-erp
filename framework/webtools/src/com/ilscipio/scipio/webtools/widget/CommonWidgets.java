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
package com.ilscipio.scipio.webtools.widget;

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
public class CommonWidgets {

    @Screen(name = "DashboardServerTraffic", location = "component://webtools/widget/CommonWidgets.xml")
    @Action(type = ActionType.SET, field = "chartType", value = "bar")
    @Action(type = ActionType.SET, field = "chartValue", value = "count")
    @Action(type = ActionType.SET, field = "chartData", value = "day")
    @Action(type = ActionType.SET, field = "chartLibrary", value = "chart")
    @Action(type = ActionType.SET, field = "chartIntervalScope", value = "day")
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "1")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/script/com/ilscipio/data/StatsServerTraffic.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonRequests} (${uiLabelMap.CommonPerDay})")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/stats/statsServerTraffic.ftl")}))
    public interface DashboardServerTraffic {}

    @Screen(name = "DashboardWSLiveRequests", location = "component://webtools/widget/CommonWidgets.xml")
    @Action(type = ActionType.SET, field = "xlabel", value = "Requests")
    @Action(type = ActionType.SET, field = "ylabel", value = "${uiLabelMap.CommonHour}")
    @Action(type = ActionType.SET, field = "label1", value = "Website Traffic")
    @Action(type = ActionType.SET, field = "chartType", value = "bar")
    @Action(type = ActionType.SET, field = "chartValue", value = "count")
    @Action(type = ActionType.SET, field = "chartIntervalScope", value = "minute")
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "0")
    @Action(type = ActionType.SET, field = "maxRequestsEntries", fromField = "parameters.maxRequestsEntries", valueType = "Integer", defaultValue = "60")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/script/com/ilscipio/data/StatsServerTraffic.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.CommonRequests} (${uiLabelMap.CommonPerHour})")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/dashboard/wsLiveRequests.ftl")}))
    public interface DashboardWSLiveRequests {}

}

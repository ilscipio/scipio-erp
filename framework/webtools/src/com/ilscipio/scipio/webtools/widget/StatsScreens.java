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
public class StatsScreens {

    @Screen(name = "StatsSinceStart", location = "component://webtools/widget/StatsScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsStatsMainPageTitle")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "stats")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/stats/StatsSinceStart.groovy")
    @DecoratorScreen(
        name = "StatsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "StatsSinceStart", location = "component://webtools/widget/Menus.xml"
            ),
            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsStatsCurrentTime} ${nowTimestamp}"
            )}, containers = {
                @Container(style = "page-title", labels = {
                    @Label(text = "${uiLabelMap[titleProperty]}")}, position = 0)
                }, screenlets = {
                    @Screenlet(title = "${uiLabelMap.WebtoolsStatsRequestStats}", includeForms = {
                        @IncludeForm(name = "ListRequestStats", location = "component://webtools/widget/StatsForms.xml"
                    )}, position = 3),
                    @Screenlet(title = "${uiLabelMap.WebtoolsStatsEventStats}", includeForms = {
                        @IncludeForm(name = "ListEventStats", location = "component://webtools/widget/StatsForms.xml"
                    )}, position = 4),
                    @Screenlet(title = "${uiLabelMap.WebtoolsStatsViewStats}", includeForms = {
                        @IncludeForm(name = "ListViewStats", location = "component://webtools/widget/StatsForms.xml"
                    )}, position = 5)})
        }
    )
    public interface StatsSinceStart {}

    @Screen(name = "StatBinsHistory", location = "component://webtools/widget/StatsScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsStatsBinsPageTitle")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "stats")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/stats/StatBinsHistory.groovy")
    @DecoratorScreen(
        name = "StatsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "StatsBinHistory", location = "component://webtools/widget/Menus.xml"
            ),
            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsStatsCurrentTime} ${nowTimestamp}"
            ),
            @Widget(type = WidgetType.INCLUDE_FORM, name = "ListRequestBins", location = "component://webtools/widget/StatsForms.xml"
            )}, containers = {
                @Container(style = "page-title", labels = {
                    @Label(text = "${uiLabelMap[titleProperty]}")}, position = 0)
                })
        }
    )
    public interface StatBinsHistory {}

    @Screen(name = "ViewMetrics", location = "component://webtools/widget/StatsScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsViewMetrics")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "metrics")
    @DecoratorScreen(
        name = "StatsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_FORM, name = "ListMetrics", location = "component://webtools/widget/StatsForms.xml"
            )}, containers = {
                @Container(style = "page-title", labels = {
                    @Label(text = "${uiLabelMap[titleProperty]}")}, position = 0)
                })
        }
    )
    public interface ViewMetrics {}

}

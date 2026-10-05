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
public class DemoDataGeneratorScreens {

    @Screen(name = "ListDemoDataGeneratorServices", location = "component://webtools/widget/DemoDataGeneratorScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleDemoDataGenerator")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListDemoDataGeneratorServices")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/script/com/ilscipio/demoDataGenerator/DemoDataGeneratorList.groovy")
    @DecoratorScreen(
        name = "CommonDemoDataGeneratorDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/demoDataGenerator/DemoDataGeneratorList.ftl"
            )})
        }
    )
    public interface ListDemoDataGeneratorServices {}

    @Screen(name = "RunDemoDataGeneratorService", location = "component://webtools/widget/DemoDataGeneratorScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleDemoDataGenerator")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListDemoDataGeneratorServices")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "maxRecords", resource = "demosuite", property = "demosuite.test.data.max.records", defaultValue = "50")
    @Action(type = ActionType.SET, field = "maxRecords", fromField = "maxRecords", valueType = "Integer")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/script/com/ilscipio/demoDataGenerator/RunDemoDataGenerator.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/ScheduleJob.groovy")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WebtoolsUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "infoMessage", value = "${uiLabelMap.WebtoolsDataGeneratorMaxRecordsInfo} ${uiLabelMap.WebtoolsDataGeneratorMaxRecordsCurrentValue}", valueType = "PlainString")
    @DecoratorScreen(
        name = "CommonDemoDataGeneratorDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/demoDataGenerator/RunDemoDataGeneratorService.ftl"
                )})})
        }
    )
    public interface RunDemoDataGeneratorService {}

    @Screen(name = "DemoDataGeneratorResult", location = "component://webtools/widget/DemoDataGeneratorScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleDemoDataGenerator")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListDemoDataGeneratorServices")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/script/com/ilscipio/demoDataGenerator/DemoDataGeneratorResult.groovy")
    @DecoratorScreen(
        name = "CommonDemoDataGeneratorDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/demoDataGenerator/DemoDataGeneratedResult.ftl"
                )})})
        }
    )
    public interface DemoDataGeneratorResult {}

    @Screen(name = "ListDemoDataGeneratorProviders", location = "component://webtools/widget/DemoDataGeneratorScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleDemoDataGenerator")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "ListDemoDataGeneratorServices")
    @DecoratorScreen(
        name = "CommonDemoDataGeneratorDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.LABEL, text = "TODO")})
        }
    )
    public interface ListDemoDataGeneratorProviders {}

}

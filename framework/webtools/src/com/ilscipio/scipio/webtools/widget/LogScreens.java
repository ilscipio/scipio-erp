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
public class LogScreens {

    @Screen(name = "log-decorator", location = "component://webtools/widget/LogScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "Server")
    @DecoratorScreen(
        name = "CommonWebtoolsAppDecorator",
        location = "${parameters.mainDecoratorLocation}"
    )
    public interface log_decorator {}

    @Screen(name = "ServiceLog", location = "component://webtools/widget/LogScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleServiceList")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "serviceLog")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/service/Services.groovy")
    @DecoratorScreen(
        name = "log-decorator",
        location = "component://webtools/widget/LogScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListServices", location = "component://webtools/widget/ServiceForms.xml"
                )})})
        }
    )
    public interface ServiceLog {}

    @Screen(name = "LogView", location = "component://webtools/widget/LogScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleLogView")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "logging")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "logFileName", resource = "debug", property = "log4j.appender.css.File", defaultValue = "runtime/logs/ofbiz.log", noLocale = true)
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/log/LogView.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/log/LogConfiguration.groovy")
    @DecoratorScreen(
        name = "log-decorator",
        location = "component://webtools/widget/LogScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.WebtoolsDebuggingLevelFormDescription}", includeForms = {
                    @IncludeForm(name = "LevelSwitch", location = "component://webtools/widget/LogForms.xml"
                )}),
                @Screenlet(htmlTemplates = {
                    @HtmlTemplate(location = "component://webtools/webapp/webtools/log/logContent.ftl", position = 1
                )}, containers = {
                    @Container(labels = {
                        @Label(text = "${uiLabelMap.WebtoolsLogFileName}:"),
                        @Label(text = "${logFileName}")}, position = 0)})})
        }
    )
    public interface LogView {}

}

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
public class ArtifactInfoScreens {

    @Screen(name = "ArtifactInfo", location = "component://webtools/widget/ArtifactInfoScreens.xml", transactionTimeout = "180", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = HasPermission.class, params = {"ARTIFACT_INFO_VIEW"})}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsCannotViewArtifactInfoPages}", style = "common-msg-error-perm")}))
    @Action(type = ActionType.SET, field = "titleProperty", value = "WebtoolsArtifactInfo")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "artifactInfo")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/artifactinfo/ArtifactInfo.groovy")
    @DecoratorScreen(
        name = "CommonArtifactDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://webtools/webapp/webtools/artifactinfo/ArtifactInfo.ftl"
            )})
        }
    )
    public interface ArtifactInfo {}

    @Screen(name = "ViewComponents", location = "component://webtools/widget/ArtifactInfoScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CommonComponents")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewents")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/artifactinfo/ComponentList.groovy")
    @Action(type = ActionType.SET, field = "viewSize", value = "30")
    @DecoratorScreen(
        name = "CommonArtifactDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ComponentList", location = "component://webtools/widget/ArtifactInfoForms.xml"
                )})})
        }
    )
    public interface ViewComponents {}

    @Screen(name = "ViewComponent", location = "component://webtools/widget/ArtifactInfoScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "CommonComponent")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "viewents")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/generated/ViewComponent_script1.groovy")
    @Action(type = ActionType.SCRIPT, location = "component://webtools/webapp/webtools/WEB-INF/actions/artifactinfo/TestSuiteInfo.groovy")
    @Action(type = ActionType.SET, field = "titleFormat", value = "\\${finalTitle}: ${compName}")
    @DecoratorScreen(
        name = "CommonArtifactDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"compEnabled"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(title = "Test Suites", includeForms = {
                            @IncludeForm(name = "TestSuiteInfo", location = "component://webtools/widget/ArtifactInfoForms.xml"
                        )})}), failWidgets = @InlineWidgets(value = {
                            @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WebtoolsComponentNotFoundOrNotEnabled}", style = "common-msg-error"
                        )}))})
        }
    )
    public interface ViewComponent {}

    @Screen(name = "TestSuiteInfo", location = "component://webtools/widget/ArtifactInfoScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ViewComponent")}))
    public interface TestSuiteInfo {}

}

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
package com.ilscipio.scipio.party.widget;

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
public class PartymgrVisitScreens {

    @Screen(name = "FindVisits", location = "component://party/widget/partymgr/VisitScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "visits")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleVisitList")
    @Action(type = ActionType.SET, field = "noConditionFind", value = "Y")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindVisits", location = "component://party/widget/partymgr/PartyVisitForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListVisits", location = "component://party/widget/partymgr/PartyVisitForms.xml"
                    )}))})})
        }
    )
    public interface FindVisits {}

    @Screen(name = "ListLoggedInUsers", location = "component://party/widget/partymgr/VisitScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "loggedinusers")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListLoggedInUsers")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListLoggedInUsers", location = "component://party/widget/partymgr/PartyVisitForms.xml"
                )})})
        }
    )
    public interface ListLoggedInUsers {}

    @Screen(name = "LoggedInUsersScreen", location = "component://party/widget/partymgr/VisitScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.PageTitleListLoggedInUsers}", includeForms = {@IncludeForm(name = "ListLoggedInUsers", location = "component://party/widget/partymgr/PartyVisitForms.xml")})}))
    public interface LoggedInUsersScreen {}

    @Screen(name = "visitdetail", location = "component://party/widget/partymgr/VisitScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleVisitDetail")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "visits")
    @Action(type = ActionType.SCRIPT, location = "component://party/webapp/partymgr/WEB-INF/actions/visit/VisitDetails.groovy")
    @DecoratorScreen(
        name = "CommonPartyAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://party/webapp/partymgr/visit/visitdetail.ftl"
            )})
        }
    )
    public interface visitdetail {}

}

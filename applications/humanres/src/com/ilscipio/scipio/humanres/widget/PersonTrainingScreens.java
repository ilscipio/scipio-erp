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
package com.ilscipio.scipio.humanres.widget;

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
public class PersonTrainingScreens {

    @Screen(name = "TrainingCalendar", location = "component://humanres/widget/PersonTrainingScreens.xml")
    @Action(type = ActionType.SET, field = "parameters.period", fromField = "parameters.period", defaultValue = "${initialView}")
    @Action(type = ActionType.SET, field = "eventDetailWidgetName", fromField = "eventDetailWidgetName", defaultValue = "trainingCalendarDetail")
    @Action(type = ActionType.SET, field = "eventDetailWidgetLocation", fromField = "eventDetailWidgetLocation", defaultValue = "component://humanres/widget/PersonTrainingScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.period", "equals", "day"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCalendarDay"), @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "day")}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CalendarMenu", location = "component://workeffort/widget/CalendarScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "dayCalendar", location = "component://workeffort/widget/CalendarScreens.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifCompare = {@IfCompare(field = "parameters.period", operator = "equals", value = "week")})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenuItem", value = "week")}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CalendarMenu", location = "component://workeffort/widget/CalendarScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "weekCalendar", location = "component://workeffort/widget/CalendarScreens.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifEmpty = {"parameters.period"}, ifCompare = {@IfCompare(field = "parameters.period", operator = "equals", value = "month")})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenuItem", value = "month")}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CalendarMenu", location = "component://workeffort/widget/CalendarScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "monthCalendar", location = "component://workeffort/widget/CalendarScreens.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.period", "equals", "upcoming"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenuItem", value = "upcoming")}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CalendarMenu", location = "component://workeffort/widget/CalendarScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "upcomingCalendar", location = "component://workeffort/widget/CalendarScreens.xml")}))
    public interface TrainingCalendar {}

    @Screen(name = "TrainingCalendarWithDecorator", location = "component://humanres/widget/PersonTrainingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TrainingCalendar")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindTrainingCalendar")
    @DecoratorScreen(
        name = "CommonTrainingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "TrainingCalendar", location = "component://humanres/widget/PersonTrainingScreens.xml"
            )})
        }
    )
    public interface TrainingCalendarWithDecorator {}

    @Screen(name = "trainingCalendarDetail", location = "component://humanres/widget/PersonTrainingScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.SCRIPT, location = "component://workeffort/script/isCalOwner.groovy")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.SET, field = "trainingClassTypeId", fromField = "workEffort.workEffortName")
    @Action(type = ActionType.SET, field = "workEffortTypeId", fromField = "workEffort.workEffortTypeId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "workEffort.estimatedStartDate")
    @Action(type = ActionType.SET, field = "thruDate", fromField = "workEffort.estimatedCompletionDate")
    @Action(type = ActionType.SET, field = "loginPartyId", fromField = "parameters.userLogin.partyId")
    @Action(type = ActionType.SET, field = "approvalStatus", fromField = "workEffort.status")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Or.class, tree = {@ConditionNode(type = And.class), @ConditionNode(parent = 0, not = true, type = Empty.class, params = {"workEffort"}), @ConditionNode(parent = 0, type = Compare.class, params = {"workEffort.currentStatusId", "not-equals", "CAL_CANCELLED"}), @ConditionNode(type = Empty.class, params = {"workEffort"}), @ConditionNode(type = HasPermission.class, params = {"WORKEFFORTMGR", "_ADMIN"})}), @Condition(type = Compare.class, params = {"parameters.form", "equals", "edit"})}), widgets = @Widgets(sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifEmpty = {"workEffort"}, ifTrue = {"isCalOwner"})}), widgets = @WidgetsForContainer(containers = {@Container2(style = "${styles.grid_large}6", includeForms = {@IncludeForm(name = "editTrainingCalendar", location = "component://humanres/widget/forms/PersonTrainingForms.xml", position = 1)}, labels = {@Label(text = "${uiLabelMap.WorkEffortAddCalendarEvent}", style = "heading", position = 0)}), @Container2(style = "${styles.grid_large}6", includeForms = {@IncludeForm(name = "ListTrainingParticipants", location = "component://humanres/widget/forms/PersonTrainingForms.xml", position = 1)}, labels = {@Label(text = "${uiLabelMap.WorkEffortParticipants}", style = "heading", position = 0)}, sections = {@SectionNested2(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = NotEmpty.class, params = {"workEffort"}), @Condition(type = Compare.class, params = {"workEffortTypeId", "not-equals", "WES_PUBLIC"})}), widgets = @WidgetsForContainer2(containers = {@Container3(includeForms = {@IncludeForm(name = "AssignTraining", location = "component://humanres/widget/forms/PersonTrainingForms.xml", position = 1)}, labels = {@Label(position = 0)})}), position = 2)})}), failWidgets = @WidgetsForContainer(containers = {@Container2(style = "${styles.grid_large}6", includeForms = {@IncludeForm(name = "showTrainingCalendar", location = "component://humanres/widget/forms/PersonTrainingForms.xml", position = 1)}, labels = {@Label(text = "${uiLabelMap.WorkEffortSummary}", style = "heading", position = 0)})}))}))
    public interface trainingCalendarDetail {}

    @Screen(name = "FindTrainingApprovals", location = "component://humanres/widget/PersonTrainingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindTrainingApprovals")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindTrainingApprovals")
    @Action(type = ActionType.SERVICE, serviceName = "humanResManagerPermission", resultMapName = "permResult", fieldMaps = {@FieldMap(fieldName = "mainAction", value = "ADMIN")})
    @Action(type = ActionType.SET, field = "hasAdminPermission", fromField = "permResult.hasPermission")
    @Action(type = ActionType.SET, field = "loginPartyId", fromField = "parameters.userLogin.partyId")
    @DecoratorScreen(
        name = "CommonTrainingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindTrainingApprovals", location = "component://humanres/widget/forms/PersonTrainingForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListTrainingApprovals", location = "component://humanres/widget/forms/PersonTrainingForms.xml"
                    )}))})})
        }
    )
    public interface FindTrainingApprovals {}

    @Screen(name = "EditTrainingApprovals", location = "component://humanres/widget/PersonTrainingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindTrainingApprovals")
    @Action(type = ActionType.SET, field = "partyId", fromField = "parameters.partyId")
    @Action(type = ActionType.SET, field = "trainingClassTypeId", fromField = "parameters.trainingClassTypeId")
    @Action(type = ActionType.SET, field = "fromDate", fromField = "parameters.fromDate")
    @Action(type = ActionType.ENTITY_ONE, entityName = "PersonTraining", valueField = "personTraining")
    @DecoratorScreen(
        name = "CommonTrainingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.CommonEdit} ${uiLabelMap.HumanResTrainingApproval}", name = "EditTrainingApprovals", collapsible = true, includeForms = {
                    @IncludeForm(name = "EditTrainingApprovals", location = "component://humanres/widget/forms/PersonTrainingForms.xml"
                )})})
        }
    )
    public interface EditTrainingApprovals {}

    @Screen(name = "FindTrainingStatus", location = "component://humanres/widget/PersonTrainingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindTrainingStatus")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindTrainingStatus")
    @Action(type = ActionType.SERVICE, serviceName = "humanResManagerPermission", resultMapName = "permResult", fieldMaps = {@FieldMap(fieldName = "mainAction", value = "ADMIN")})
    @Action(type = ActionType.SET, field = "hasAdminPermission", fromField = "permResult.hasPermission")
    @Action(type = ActionType.SET, field = "loginPartyId", fromField = "parameters.userLogin.partyId")
    @DecoratorScreen(
        name = "CommonTrainingDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", decorators = {
                @DecoratorScreenNested(name = "FindScreenDecorator", location = "component://common/widget/CommonScreens.xml", sections = {
                    @DecoratorSectionNested(name = "search-options", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "FindTrainingStatus", location = "component://humanres/widget/forms/PersonTrainingForms.xml"
                    )})),
                    @DecoratorSectionNested(name = "search-results", widgets = @WidgetsForContainer4(value = {
                        @Widget(type = WidgetType.INCLUDE_FORM, name = "ListTrainingStatus", location = "component://humanres/widget/forms/PersonTrainingForms.xml"
                    )}))})})
        }
    )
    public interface FindTrainingStatus {}

    @Screen(name = "ListTrainingStatus", location = "component://humanres/widget/PersonTrainingScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "EditPersonTrainings")
    @DecoratorScreen(
        name = "CommonPartyDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.HumanResTrainingStatus}", name = "TrainingStatus", collapsible = true, includeForms = {
                    @IncludeForm(name = "ListTrainingStatus", location = "component://humanres/widget/forms/PersonTrainingForms.xml"
                )})})
        }
    )
    public interface ListTrainingStatus {}

}

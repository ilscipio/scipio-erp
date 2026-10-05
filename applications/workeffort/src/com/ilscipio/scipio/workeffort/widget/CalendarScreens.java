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
package com.ilscipio.scipio.workeffort.widget;

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
public class CalendarScreens {

    @Screen(name = "Calendar", location = "component://workeffort/widget/CalendarScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "parameters.period", fromField = "parameters.period", defaultValue = "${initialView}")
    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/calendar/CreateUrlParam.groovy")
    @Action(type = ActionType.SET, field = "parentTypeId", fromField = "parameters.parentTypeId", defaultValue = "EVENT")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CalendarOnly")}))
    public interface Calendar {}

    @Screen(name = "CalendarOnly", location = "component://workeffort/widget/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "eventDetailWidgetName", fromField = "eventDetailWidgetName", defaultValue = "eventDetail")
    @Action(type = ActionType.SET, field = "eventDetailWidgetLocation", fromField = "eventDetailWidgetLocation", defaultValue = "component://workeffort/widget/CalendarScreens.xml")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.period", "equals", "day"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleCalendarDay"), @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "day")}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "calendarMenu", location = "component://workeffort/widget/CalendarScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "dayCalendar", location = "component://workeffort/widget/CalendarScreens.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifCompare = {@IfCompare(field = "parameters.period", operator = "equals", value = "week")})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenuItem", value = "week")}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "calendarMenu", location = "component://workeffort/widget/CalendarScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "weekCalendar", location = "component://workeffort/widget/CalendarScreens.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(or = {@OrCondition(ifEmpty = {"parameters.period"}, ifCompare = {@IfCompare(field = "parameters.period", operator = "equals", value = "month")})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenuItem", value = "month")}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "calendarMenu", location = "component://workeffort/widget/CalendarScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "monthCalendar", location = "component://workeffort/widget/CalendarScreens.xml")}))
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.period", "equals", "upcoming"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "activeSubMenuItem", value = "upcoming")}), widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "calendarMenu", location = "component://workeffort/widget/CalendarScreens.xml"), @Widget(type = WidgetType.INCLUDE_SCREEN, name = "upcomingCalendar", location = "component://workeffort/widget/CalendarScreens.xml")}))
    public interface CalendarOnly {}

    @Screen(name = "calendarMenu", location = "component://workeffort/widget/CalendarScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "Calendar", location = "component://workeffort/widget/WorkEffortMenus.xml")}))
    public interface calendarMenu {}

    @Screen(name = "dayCalendar", location = "component://workeffort/widget/CalendarScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/calendar/Days.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://workeffort/webapp/workeffort/calendar/day.ftl", position = 1)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"eventDetailWidgetName"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "${eventDetailWidgetName}", location = "${eventDetailWidgetLocation}")}), position = 0)}))
    public interface dayCalendar {}

    @Screen(name = "weekCalendar", location = "component://workeffort/widget/CalendarScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/calendar/Week.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://workeffort/webapp/workeffort/calendar/week.ftl", position = 1)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"eventDetailWidgetName"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "${eventDetailWidgetName}", location = "${eventDetailWidgetLocation}")}), position = 0)}))
    public interface weekCalendar {}

    @Screen(name = "monthCalendar", location = "component://workeffort/widget/CalendarScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/calendar/Month.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://workeffort/webapp/workeffort/calendar/month.ftl", position = 1)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"eventDetailWidgetName"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "${eventDetailWidgetName}", location = "${eventDetailWidgetLocation}")}), position = 0)}))
    public interface monthCalendar {}

    @Screen(name = "upcomingCalendar", location = "component://workeffort/widget/CalendarScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/calendar/Upcoming.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://workeffort/webapp/workeffort/calendar/upcoming.ftl", position = 1)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"eventDetailWidgetName"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "${eventDetailWidgetName}", location = "${eventDetailWidgetLocation}")}), position = 0)}))
    public interface upcomingCalendar {}

    @Screen(name = "CalendarWithDecorator", location = "component://workeffort/widget/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "parameters.period", fromField = "parameters.period", defaultValue = "${initialView}")
    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/calendar/CreateUrlParam.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WorkEffortCalendar")
    @DecoratorScreen(
        name = "CommonCalendarDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CalendarOnly", location = "component://workeffort/widget/CalendarScreens.xml"
            )})
        }
    )
    public interface CalendarWithDecorator {}

    @Screen(name = "eventDetail", location = "component://workeffort/widget/CalendarScreens.xml", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"parameters.form", "equals", "edit"})}))
    @Action(order = 0, type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(order = 1, type = ActionType.SET, field = "calEventIdNum", fromField = "calEventIdNum", defaultValue = "0")
    @Action(order = 2, type = ActionType.SET, field = "calEventFormPeriod", fromField = "parameters.period")
    @IfAction(order = 3, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"workEffort"})}), then = @Actions(value = {@Action(type = ActionType.SCRIPT, location = "component://workeffort/script/isCalOwner.groovy")}))
    @IfAction(order = 4, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Or.class, tree = {@ConditionNode(type = And.class), @ConditionNode(parent = 0, not = true, type = Empty.class, params = {"workEffort"}), @ConditionNode(parent = 0, type = Compare.class, params = {"workEffort.currentStatusId", "not-equals", "CAL_CANCELLED"}), @ConditionNode(parent = 0, type = True.class, params = {"isCalOwner"}), @ConditionNode(type = Empty.class, params = {"workEffort"}), @ConditionNode(type = HasPermission.class, params = {"WORKEFFORTMGR", "_ADMIN"})})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "useEditForm", value = "true")}))
    @Action(order = 5, type = ActionType.SET, field = "eventDetailCols", fromField = "eventDetailCols", defaultValue = "6")
    @Section(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Compare.class, params = {"useEditForm", "equals", "true"})}), actions = @Actions(value = {@Action(type = ActionType.SET, field = "cancelEventFormId", value = "cancelEvent_${calEventIdNum}")}), widgets = @Widgets(containers = {@Container(style = "${styles.grid_row}", sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"workEffort"})}), widgets = @WidgetsForContainer(containers = {@Container2(style = "${styles.grid_large}${eventDetailCols} ${styles.grid_cell}", screenlets = {@ScreenletNested(title = "${uiLabelMap.WorkEffortParticipants}", includeForms = {
                    @IncludeForm(name = "showCalEventRolesDel", location = "component://workeffort/widget/CalendarForms.xml", position = 0
                )}, sections = {
                    @SectionLeaf(condition = @Condition(type = NotEmpty.class, params = {"workEffort"
                }), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "calEventFormAction", value = "edit"
                )}), widgets = @WidgetsLeaf(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "eventDetail-part1", location = "component://workeffort/widget/CalendarScreens.xml", shareScope = true
                )}), position = 1)})})}))}, containers = {@Container2(style = "${styles.grid_large}${eventDetailCols} ${styles.grid_cell}", screenlets = {@ScreenletNested(title = "${uiLabelMap.WorkEffortAddCalendarEvent}", sections = {
                    @SectionLeaf(condition = @Condition(type = NotEmpty.class, params = {"workEffort"
                }), actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "calEventFormAction", value = "${groovy:''}"
                )}), widgets = @WidgetsLeaf(includeForms = {
                    @IncludeForm(name = "cancelEventHidden", location = "component://workeffort/widget/CalendarForms.xml"
                )})),
                @SectionLeaf(actions = @Actions(value = {
                    @Action(type = ActionType.SET, field = "calEventFormAction", value = "edit"
                )}), widgets = @WidgetsLeaf(includeForms = {
                    @IncludeForm(name = "editCalEvent", location = "component://workeffort/widget/CalendarForms.xml"
                )}))})})})}), failWidgets = @Widgets(containers = {@Container(style = "${styles.grid_row}", containers = {@Container2(style = "${styles.grid_large}${eventDetailCols} ${styles.grid_cell}", screenlets = {@ScreenletNested(title = "${uiLabelMap.WorkEffortSummary}", sections = {
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "calEventFormAction", value = "edit"
                    )}), widgets = @WidgetsLeaf(includeForms = {
                        @IncludeForm(name = "showCalEvent", location = "component://workeffort/widget/CalendarForms.xml"
                    )}))})}), @Container2(style = "${styles.grid_large}${eventDetailCols} ${styles.grid_cell}", screenlets = {@ScreenletNested(title = "${uiLabelMap.WorkEffortParticipants}", sections = {
                    @SectionLeaf(actions = @Actions(value = {
                        @Action(type = ActionType.SET, field = "calEventFormAction", value = "edit"
                    )}), widgets = @WidgetsLeaf(includeForms = {
                        @IncludeForm(name = "showCalEventRoles", location = "component://workeffort/widget/CalendarForms.xml"
                    )}))})})})}))
    public interface eventDetail {}

    @Screen(name = "eventDetail-part1", location = "component://workeffort/widget/CalendarScreens.xml")
    @Section(widgets = @Widgets(screenlets = {@Screenlet(includeForms = {@IncludeForm(name = "addCalEventRole", location = "component://workeffort/widget/CalendarForms.xml")})}))
    public interface eventDetail_part1 {}

    @Screen(name = "calendarEventContent", location = "component://workeffort/widget/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "periodType", value = "${groovy: request.getAttribute('periodType');}")
    @Action(type = ActionType.SET, field = "workEffortId", value = "${groovy: request.getAttribute('workEffortId');}")
    @Action(type = ActionType.SET, field = "calEventVerbose", value = "${groovy: request.getAttribute('calEventVerbose');}", valueType = "Boolean")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.GET_RELATED_ONE, valueField = "workEffort", relationName = "ParentWorkEffort", toValueField = "parentWorkEffort")
    @Action(type = ActionType.GET_RELATED, valueField = "workEffort", relationName = "WorkOrderItemFulfillment", list = "workOrderItemFulfillments")
    @Action(type = ActionType.GET_RELATED, valueField = "parentWorkEffort", relationName = "WorkOrderItemFulfillment", list = "parentWorkOrderItemFulfillments")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://workeffort/webapp/workeffort/calendar/calendarEventContent.ftl")}))
    public interface calendarEventContent {}

}

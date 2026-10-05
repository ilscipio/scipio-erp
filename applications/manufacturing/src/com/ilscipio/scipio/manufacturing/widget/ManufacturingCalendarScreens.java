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
package com.ilscipio.scipio.manufacturing.widget;

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
public class ManufacturingCalendarScreens {

    @Screen(name = "CommonCalendarDecorator", location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml#Calendar")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "Calendar")
    @DecoratorScreen(
        name = "CommonManufacturingAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonCalendarDecorator {}

    @Screen(name = "EditCalendar", location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "calendar")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/routing/EditCalendar.groovy")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.techDataCalendar ? 'PageTitleEditCalendar' : 'ManufacturingNewCalendar'}")
    @DecoratorScreen(
        name = "CommonCalendarDecorator",
        location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/routing/EditCalendar.ftl"
            )})
        }
    )
    public interface EditCalendar {}

    @Screen(name = "FindCalendar", location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindCalendar")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "Calendar")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "TechDataCalendar", list = "techDataCalendars")
    @DecoratorScreen(
        name = "CommonCalendarDecorator",
        location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListTechDataCalendars", location = "component://manufacturing/widget/manufacturing/CalendarForms.xml", position = 1
                )}, containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingNewCalendar}", style = "${styles.link_nav} ${styles.action_add}", target = "EditCalendar"
                    )}, position = 0)})})
        }
    )
    public interface FindCalendar {}

    @Screen(name = "ListCalendarWeek", location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleListCalendarWeek")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "CalendarWeek")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "TechDataCalendarWeek", list = "calendarWeeks")
    @DecoratorScreen(
        name = "CommonCalendarDecorator",
        location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListCalendarWeek", location = "component://manufacturing/widget/manufacturing/CalendarForms.xml", position = 1
                )}, containers = {
                    @Container(style = "button-bar", widgets = {
                        @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingNewCalendarWeek}", style = "${styles.link_nav} ${styles.action_add}", target = "EditCalendarWeek"
                    )}, position = 0)})})
        }
    )
    public interface ListCalendarWeek {}

    @Screen(name = "EditCalendarWeek", location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "CalendarWeek")
    @Action(type = ActionType.ENTITY_ONE, entityName = "TechDataCalendarWeek", valueField = "calendarWeek")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.calendarWeek ? 'PageTitleEditCalendarWeek' : 'ManufacturingNewCalendarWeek'}")
    @DecoratorScreen(
        name = "CommonCalendarDecorator",
        location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "UpdateCalendarWeek", location = "component://manufacturing/widget/manufacturing/CalendarForms.xml"
                )}, position = 1)}, sections = {
                    @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                        @Condition(type = Empty.class, params = {"calendarWeek"})}), widgets = @InlineWidgets(containers = {
                            @Container(style = "button-bar", widgets = {
                                @Widget(type = WidgetType.LINK, text = "${uiLabelMap.ManufacturingNewCalendarWeek}", style = "${styles.link_nav} ${styles.action_add}", target = "EditCalendarWeek"
                            )})}), position = 0)})
        }
    )
    public interface EditCalendarWeek {}

    @Screen(name = "EditCalendarExceptionWeek", location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditCalendarExceptionWeek")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "calendarExceptionWeek")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/routing/EditCalendarExceptionWeek.groovy")
    @DecoratorScreen(
        name = "CommonCalendarDecorator",
        location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/routing/EditCalendarExceptionWeek.ftl"
            )})
        }
    )
    public interface EditCalendarExceptionWeek {}

    @Screen(name = "EditCalendarExceptionDay", location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditCalendarExceptionDay")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "calendarExceptionDay")
    @Action(type = ActionType.SCRIPT, location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/routing/EditCalendarExceptionDay.groovy")
    @DecoratorScreen(
        name = "CommonCalendarDecorator",
        location = "component://manufacturing/widget/manufacturing/CalendarScreens.xml",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://manufacturing/webapp/manufacturing/routing/EditCalendarExceptionDay.ftl"
            )})
        }
    )
    public interface EditCalendarExceptionDay {}

}

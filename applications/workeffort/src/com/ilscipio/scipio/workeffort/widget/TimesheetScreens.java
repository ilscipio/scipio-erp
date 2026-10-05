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
public class TimesheetScreens {

    @Screen(name = "MyTimesheets", location = "component://workeffort/widget/TimesheetScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "MyTimesheets")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WorkEffortMyTimesheets")
    @Action(type = ActionType.SET, field = "queryString", fromField = "result.queryString")
    @Action(type = ActionType.SET, field = "viewIndex", fromField = "parameters.VIEW_INDEX", valueType = "Integer")
    @Action(type = ActionType.PROPERTY_TO_FIELD, field = "viewSizeDefaultValue", resource = "widget", property = "widget.form.defaultViewSize")
    @Action(type = ActionType.SET, field = "viewSize", fromField = "parameters.VIEW_SIZE", valueType = "Integer", defaultValue = "${viewSizeDefaultValue}")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Timesheet", list = "currentTimesheetList", conditions = {@ConditionExpr(fieldName = "partyId", fromField = "userLogin.partyId"), @ConditionExpr(fieldName = "fromDate", operator = "less-equals", fromField = "nowTimestamp")})
    @DecoratorScreen(
        name = "CommonTimesheetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.WorkEffortMyCurrentTimesheets}", widgets = {
                    @Widget(type = WidgetType.ITERATE_SECTION, list = "currentTimesheetList", entry = "currentTimesheet", name = "MyTimesheets-iterate1", location = "component://workeffort/widget/TimesheetScreens.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.WorkEffortMyRates}", includeForms = {
                    @IncludeForm(name = "ListMyRates", location = "component://workeffort/widget/TimesheetForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.WorkEffortMyTimesheets}", includeForms = {
                    @IncludeForm(name = "ListMyTimesheets", location = "component://workeffort/widget/TimesheetForms.xml"
                )})})
        }
    )
    public interface MyTimesheets {}

    @Screen(name = "MyTimesheets-iterate1", location = "component://workeffort/widget/TimesheetScreens.xml")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "TimeEntry", list = "currentTimeEntryList", conditions = {@ConditionExpr(fieldName = "partyId", fromField = "userLogin.partyId"), @ConditionExpr(fieldName = "timesheetId", fromField = "currentTimesheet.timesheetId")})
    @Section(widgets = @Widgets(screenlets = {@Screenlet(htmlTemplates = {@HtmlTemplate(location = "", content = "<@heading>${uiLabelMap.WorkEffortTimesheet}: ${currentTimesheet.fromDate!} - ${currentTimesheet.thruDate!}\n                                                    <a href=\"<@pageUrl uri=('EditTimesheet?timesheetId='+raw(currentTimesheet.timesheetId)) escapeAs='html'/>\">[${currentTimesheet.timesheetId}]</a></@heading>\n                                                <#if currentTimesheet.comments?has_content><p>${currentTimesheet.comments}</p></#if>", position = 0)}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = CompareField.class, params = {"parameters.showQuickEntry", "equals", "currentTimesheet.timesheetId"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_FORM, name = "QuickCreateTimeEntry", location = "component://workeffort/widget/TimesheetForms.xml")}), failWidgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.INCLUDE_MENU, name = "MyCurrentTimesheetSubTabBar", location = "component://workeffort/widget/TimesheetMenus.xml")}), position = 2)}, widgets = {@Widget(type = WidgetType.ITERATE_SECTION, list = "currentTimeEntryList", entry = "currentTimeEntry", name = "MyTimesheets-iterate1-iterate1", location = "component://workeffort/widget/TimesheetScreens.xml", position = 1)})}))
    public interface MyTimesheets_iterate1 {}

    @Screen(name = "MyTimesheets-iterate1-iterate1", location = "component://workeffort/widget/TimesheetScreens.xml")
    @Action(type = ActionType.ENTITY_ONE, entityName = "RateType", valueField = "currentRateType", autoFieldMap = false, fieldMaps = {@FieldMap(fieldName = "rateTypeId", fromField = "currentTimeEntry.rateTypeId")})
    @Section(widgets = @Widgets(containers = {@Container(labels = {@Label(text = "${uiLabelMap.WorkEffortTimesheetTimeEntry} ${uiLabelMap.CommonFor} ${currentTimeEntry.fromDate} "), @Label(text = "${currentTimeEntry.hours} ${uiLabelMap.WorkEffortTimesheetHours} ", style = "tableheadtext"), @Label(text = "${currentTimeEntry.comments} [${currentRateType.description}]")}, sections = {@SectionNested(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = Empty.class, params = {"currentTimeEntry.workEffortId"})}), widgets = @WidgetsForContainer(value = {@Widget(type = WidgetType.LINK, text = "${uiLabelMap.WorkEffortWorkEffort}: ${currentTimeEntry.workEffortId}", style = "${styles.link_nav_info_id_long}", target = "WorkEffortSummary")}))})}))
    public interface MyTimesheets_iterate1_iterate1 {}

    @Screen(name = "FindTimesheet", location = "component://workeffort/widget/TimesheetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleFindTimesheet")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "FindTimesheet")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleFindTimesheet")
    @Action(type = ActionType.SET, field = "timesheetId", fromField = "parameters.timesheetId")
    @DecoratorScreen(
        name = "CommonTimesheetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "FindTimesheet", location = "component://workeffort/widget/TimesheetForms.xml"
                )}),
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListFindTimesheet", location = "component://workeffort/widget/TimesheetForms.xml"
                )})})
        }
    )
    public interface FindTimesheet {}

    @Screen(name = "EditTimesheet", location = "component://workeffort/widget/TimesheetScreens.xml")
    @Action(type = ActionType.SET, field = "timesheetId", fromField = "parameters.timesheetId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Timesheet", valueField = "timesheet")
    @Action(type = ActionType.SET, field = "titleProperty", value = "${groovy: context.timesheet ? 'PageTitleEditTimesheet' : 'PageTitleAddTimesheet'}")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "${groovy: context.timesheet ? 'Timesheet' : 'NewTimesheet'}")
    @Action(type = ActionType.SET, field = "labelTitleProperty", fromField = "titleProperty")
    @Action(type = ActionType.SET, field = "isEditTimesheet", value = "true", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonTimesheetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Empty.class, params = {"timesheet"})}), widgets = @InlineWidgets(screenlets = {
                        @Screenlet(includeForms = {
                            @IncludeForm(name = "EditTimesheet", location = "component://workeffort/widget/TimesheetForms.xml"
                        )})}), failWidgets = @InlineWidgets(screenlets = {
                            @Screenlet(includeForms = {
                                @IncludeForm(name = "EditTimesheet", location = "component://workeffort/widget/TimesheetForms.xml"
                            )}),
                            @Screenlet(title = "${uiLabelMap.PageTitleAddTimesheetToInvoice}", includeForms = {
                                @IncludeForm(name = "AddTimesheetToInvoice", location = "component://workeffort/widget/TimesheetForms.xml"
                            )}),
                            @Screenlet(title = "${uiLabelMap.PageTitleDisplayTimesheetEntries}", includeForms = {
                                @IncludeForm(name = "DisplayTimesheetEntries", location = "component://workeffort/widget/TimesheetForms.xml"
                            )}),
                            @Screenlet(title = "${uiLabelMap.PageTitleAddTimesheetToNewInvoice}", includeForms = {
                                @IncludeForm(name = "AddTimesheetToNewInvoice", location = "component://workeffort/widget/TimesheetForms.xml"
                            )})}))})
        }
    )
    public interface EditTimesheet {}

    @Screen(name = "EditTimesheetRoles", location = "component://workeffort/widget/TimesheetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditTimesheetRoles")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TimesheetRoles")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditTimesheetRoles")
    @Action(type = ActionType.SET, field = "timesheetId", fromField = "parameters.timesheetId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Timesheet", valueField = "timesheet")
    @DecoratorScreen(
        name = "CommonTimesheetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListTimesheetRoles", location = "component://workeffort/widget/TimesheetForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleAddTimesheetRoles}", includeForms = {
                    @IncludeForm(name = "AddTimesheetRole", location = "component://workeffort/widget/TimesheetForms.xml"
                )})})
        }
    )
    public interface EditTimesheetRoles {}

    @Screen(name = "EditTimesheetEntries", location = "component://workeffort/widget/TimesheetScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "PageTitleEditTimesheetEntries")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "TimesheetEntries")
    @Action(type = ActionType.SET, field = "labelTitleProperty", value = "PageTitleEditTimesheetEntries")
    @Action(type = ActionType.SET, field = "timesheetId", fromField = "parameters.timesheetId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Timesheet", valueField = "timesheet")
    @DecoratorScreen(
        name = "CommonTimesheetDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(includeForms = {
                    @IncludeForm(name = "ListTimesheetEntries", location = "component://workeffort/widget/TimesheetForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.PageTitleAddTimesheetEntries}", includeForms = {
                    @IncludeForm(name = "AddTimesheetEntry", location = "component://workeffort/widget/TimesheetForms.xml"
                )})})
        }
    )
    public interface EditTimesheetEntries {}

}

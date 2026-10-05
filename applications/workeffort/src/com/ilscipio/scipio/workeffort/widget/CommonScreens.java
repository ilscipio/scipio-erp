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
public class CommonScreens {

    @Screen(name = "webapp-common-actions", location = "component://workeffort/widget/CommonScreens.xml")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "MainSideBarMenu")
    @Action(type = ActionType.SET, field = "mainSideBarMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "mainComplexMenuCfg", fromField = "menuCfg")
    @Action(type = ActionType.SET, field = "menuCfg")
    public interface webapp_common_actions {}

    @Screen(name = "main-decorator", location = "component://workeffort/widget/CommonScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "WorkEffortUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ContentUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "TemporalExpressionUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companyName", fromField = "uiLabelMap.WorkEffortCompanyName", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.companySubtitle", fromField = "uiLabelMap.WorkEffortCompanySubtitle", global = true)
    @Action(type = ActionType.SET, field = "activeApp", value = "workeffort", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuName", value = "WorkEffortAppBar", global = true)
    @Action(type = ActionType.SET, field = "applicationMenuLocation", value = "component://workeffort/widget/WorkEffortMenus.xml", global = true)
    @Action(type = ActionType.SET, field = "applicationTitle", value = "${uiLabelMap.CommonWorkEffort}", global = true)
    @Action(type = ActionType.SET, field = "menuCfg", fromField = "mainComplexMenuCfg")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "DeriveComplexSideBarMenuItems", location = "component://common/widget/CommonScreens.xml")
    @DecoratorScreen(
        name = "ApplicationDecorator",
        location = "component://commonext/widget/CommonScreens.xml",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {
                    @Condition(type = EmptySection.class, params = {"left-column"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "left-column"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.INCLUDE_SCREEN, name = "DefMainSideBarMenu", location = "${parameters.mainDecoratorLocation}"
                )}))}),
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface main_decorator {}

    @Screen(name = "CommonWorkEffortAppDecorator", location = "component://workeffort/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonWorkEffortAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"workEffortManagerPermission", "VIEW"})}))
    @DecoratorScreen(
        name = "main-decorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "left-column", useWhen = "${context.widePage != true}", overrideByAutoInclude = true, value = {
                @Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonWorkEffortAppSideBarMenu", location = "component://workeffort/widget/CommonScreens.xml"
            )}),
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = True.class, params = {"commonWorkEffortAppBasePermCond"
                })}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WorkEffortViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface CommonWorkEffortAppDecorator {}

    @Screen(name = "login-decorator", location = "component://workeffort/widget/CommonScreens.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "main-decorator")}))
    public interface login_decorator {}

    @Screen(name = "CommonWorkEffortDecorator", location = "component://workeffort/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://workeffort/widget/WorkEffortMenus.xml#WorkEffort")
    @Action(type = ActionType.SET, field = "workEffortId", fromField = "parameters.workEffortId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "WorkEffort", valueField = "workEffort")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.workEffortId}", valueType = "Boolean")
    @Action(type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${workEffortId} ${${extraFunctionName}}")
    @DecoratorScreen(
        name = "CommonWorkEffortAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {@Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body")})
        }
    )
    public interface CommonWorkEffortDecorator {}

    @Screen(name = "CommonTimesheetDecorator", location = "component://workeffort/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://workeffort/widget/TimesheetMenus.xml#Timesheet")
    @Action(type = ActionType.SET, field = "timesheetId", fromField = "parameters.timesheetId")
    @Action(type = ActionType.ENTITY_ONE, entityName = "Timesheet", valueField = "timesheet")
    @Action(type = ActionType.SET, field = "titleProperty", fromField = "labelTitleProperty", defaultValue = "${titleProperty}")
    @Action(type = ActionType.SET, field = "titleFormat", fromField = "titleFormat", defaultValue = "\\${finalTitle} ${timesheetId} ${${extraFunctionName}}")
    @DecoratorScreen(
        name = "CommonWorkEffortAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.INCLUDE_MENU, name = "TimesheetSubTabBar", location = "component://workeffort/widget/TimesheetMenus.xml"
            ),
            @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
            )})
        }
    )
    public interface CommonTimesheetDecorator {}

    @Screen(name = "CommonCalendarDecorator", location = "component://workeffort/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", fromField = "activeSubMenuItem", defaultValue = "calendar")
    @DecoratorScreen(
        name = "CommonWorkEffortAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(actions = @Actions(value = {
                    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/calendar/Days.groovy"
                )}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}))})
        }
    )
    public interface CommonCalendarDecorator {}

    @Screen(name = "iCalendarDecorator", location = "component://workeffort/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenu", fromField = "activeSubMenu", defaultValue = "component://workeffort/widget/WorkEffortMenus.xml#ICalendar")
    @Action(type = ActionType.SET, field = "commonSideBarMenu.condList[]", value = "${not empty context.workEffort}", valueType = "Boolean")
    @DecoratorScreen(
        name = "CommonWorkEffortAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", sections = {
                @InlineSection(condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {
                    @Condition(type = Or.class, tree = {
                        @ConditionNode(type = ServicePermission.class, params = {"workEffortICalendarPermission", "CREATE", "parameters"
                    }),
                    @ConditionNode(type = ServicePermission.class, params = {"workEffortICalendarPermission", "UPDATE", "parameters"
                })})}), widgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.DECORATOR_SECTION_INCLUDE, name = "body"
                )}), failWidgets = @InlineWidgets(value = {
                    @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.WorkEffortViewPermissionError}", style = "common-msg-error-perm"
                )}))})
        }
    )
    public interface iCalendarDecorator {}

    @Screen(name = "main", location = "component://workeffort/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "main")
    @Action(type = ActionType.SET, field = "titleProperty", value = "WorkEffort")
    @Action(type = ActionType.SET, field = "eventDetailWidgetName", fromField = "eventDetailWidgetName", defaultValue = "eventDetail")
    @Action(type = ActionType.SET, field = "eventDetailWidgetLocation", fromField = "eventDetailWidgetLocation", defaultValue = "component://workeffort/widget/CalendarScreens.xml")
    @Action(type = ActionType.SET, field = "eventDetailCols", fromField = "eventDetailCols", defaultValue = "12")
    @Action(type = ActionType.SET, field = "parameters.period", fromField = "parameters.period", defaultValue = "${initialView}")
    @Action(type = ActionType.SCRIPT, location = "component://workeffort/webapp/workeffort/WEB-INF/actions/calendar/CreateUrlParam.groovy")
    @Action(type = ActionType.SET, field = "parentTypeId", fromField = "parameters.parentTypeId", defaultValue = "EVENT")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "TimeEntry", list = "currentTimeEntryList", conditions = {@ConditionExpr(fieldName = "partyId", fromField = "userLogin.partyId"), @ConditionExpr(fieldName = "timesheetId", fromField = "currentTimesheet.timesheetId")})
    @DecoratorScreen(
        name = "CommonWorkEffortAppDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.HTML_TEMPLATE, location = "component://workeffort/webapp/workeffort/main.ftl"
            )}, containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "weekCalendar", location = "component://workeffort/widget/CalendarScreens.xml"
                    )}),
                    @Container2(style = "${styles.grid_large}6 ${styles.grid_cell}", screenlets = {
                        @ScreenletNested(title = "${uiLabelMap.WorkEffortMyTimesheets}", includeForms = {
                    @IncludeForm(name = "ListMyTimesheets", location = "component://workeffort/widget/TimesheetForms.xml"
                
                    )})})})})
        }
    )
    public interface main {}

    @Screen(name = "MainSideBarMenu", location = "component://workeffort/widget/CommonScreens.xml")
    @Action(type = ActionType.SET, field = "menuCfg.location", value = "component://workeffort/widget/WorkEffortMenus.xml")
    @Action(type = ActionType.SET, field = "menuCfg.name", value = "WorkEffortAppSideBar")
    @Action(type = ActionType.SET, field = "menuCfg.defLocation", value = "component://workeffort/widget/WorkEffortMenus.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "ComplexSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface MainSideBarMenu {}

    @Screen(name = "DefMainSideBarMenu", location = "component://workeffort/widget/CommonScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://common/webcommon/WEB-INF/actions/includes/scipio/PrepareDefComplexSideBarMenu.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "MainSideBarMenu")}))
    public interface DefMainSideBarMenu {}

    @Screen(name = "CommonWorkEffortAppSideBarMenu", location = "component://workeffort/widget/CommonScreens.xml")
    @Action(type = ActionType.CONDITION_TO_FIELD, field = "commonWorkEffortAppBasePermCond", valueType = "Boolean", onlyIfField = "empty", condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = ServicePermission.class, params = {"workEffortManagerPermission", "VIEW"})}))
    @Action(type = ActionType.SET, field = "commonSideBarMenu.cond", fromField = "commonSideBarMenu.cond", valueType = "Boolean", defaultValue = "${commonWorkEffortAppBasePermCond}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.INCLUDE_SCREEN, name = "CommonSideBarMenu", location = "component://common/widget/CommonScreens.xml")}))
    public interface CommonWorkEffortAppSideBarMenu {}

}

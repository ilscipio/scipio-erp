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

import com.ilscipio.scipio.widget.def.menu.*;
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
public class TimesheetMenus {

    @Menu(
        name = "TimesheetTabBar",
        location = "component://workeffort/widget/TimesheetMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "Timesheet", title = "${uiLabelMap.WorkEffortTimesheet}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"timesheetId"})}), link = @MenuLink(target = "EditTimesheet", parameters = {@MenuParameter(paramName = "timesheetId", fromField = "timesheetId")})),
            @MenuItem(name = "TimesheetRoles", title = "${uiLabelMap.PartyParties}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"timesheetId"})}), link = @MenuLink(target = "EditTimesheetRoles", parameters = {@MenuParameter(paramName = "timesheetId", fromField = "timesheetId")})),
            @MenuItem(name = "TimesheetEntries", title = "${uiLabelMap.CommonEntries}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"timesheetId"})}), link = @MenuLink(target = "EditTimesheetEntries", parameters = {@MenuParameter(paramName = "timesheetId", fromField = "timesheetId")})),
            @MenuItem(name = "MyTimesheets", title = "${uiLabelMap.WorkEffortMyTimesheets}", widgetStyle = "+${styles.action_nav} ${styles.action_view}", link = @MenuLink(target = "MyTimesheets", parameters = {@MenuParameter(paramName = "partyId", fromField = "userLogin.partyId")}))
        }
    )
    public interface TimesheetTabBar {}

    @Menu(
        name = "TimesheetSideBar",
        location = "component://workeffort/widget/TimesheetMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "TimesheetTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface TimesheetSideBar {}

    @Menu(
        name = "TimesheetSubTabBar",
        location = "component://workeffort/widget/TimesheetMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditTimesheet", title = "${uiLabelMap.WorkEffortTimesheetCreate}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = Or.class, tree = {@ConditionNode(not = true, type = Empty.class, params = {"timesheet"}), @ConditionNode(not = true, type = True.class, params = {"isEditTimesheet"})})}), link = @MenuLink(target = "EditTimesheet", parameters = {@MenuParameter(paramName = "partyId", fromField = "userLogin.partyId")})),
            @MenuItem(name = "MyTimesheets", title = "${uiLabelMap.WorkEffortMyTimesheets}", widgetStyle = "+${styles.action_nav} ${styles.action_view}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"activeSubMenuItem", "not-equals", "MyTimesheets"})}), link = @MenuLink(target = "MyTimesheets", parameters = {@MenuParameter(paramName = "partyId", fromField = "userLogin.partyId")})),
            @MenuItem(name = "createTimesheetForThisWeek", title = "${uiLabelMap.PageTitleCreateWeekTimesheet}", widgetStyle = "+${styles.action_run_sys} ${styles.action_add}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"activeSubMenuItem", "equals", "MyTimesheets"})}), link = @MenuLink(target = "createTimesheetForThisWeek", parameters = {@MenuParameter(paramName = "partyId", fromField = "userLogin.partyId")}))
        }
    )
    public interface TimesheetSubTabBar {}

    @Menu(
        name = "MyCurrentTimesheetSubTabBar",
        location = "component://workeffort/widget/TimesheetMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "MyTimesheets", title = "${uiLabelMap.WorkEffortTimesheetQuickTimeEntry}", widgetStyle = "+${styles.action_nav} ${styles.action_view}", link = @MenuLink(target = "MyTimesheets", parameters = {@MenuParameter(paramName = "showQuickEntry", fromField = "currentTimesheet.timesheetId")})),
            @MenuItem(name = "EditTimesheetEntries", title = "${uiLabelMap.WorkEffortTimesheetTimeEntries}", widgetStyle = "+${styles.action_nav} ${styles.action_view}", link = @MenuLink(target = "EditTimesheetEntries", parameters = {@MenuParameter(paramName = "timesheetId", fromField = "currentTimesheet.timesheetId")}))
        }
    )
    public interface MyCurrentTimesheetSubTabBar {}

}

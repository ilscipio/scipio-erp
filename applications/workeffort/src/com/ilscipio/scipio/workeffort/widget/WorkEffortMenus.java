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
public class WorkEffortMenus {

    @Menu(
        name = "WorkEffortAppBar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        title = "${uiLabelMap.WorkEffortManager}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "task", title = "${uiLabelMap.WorkEffortTaskList}", link = @MenuLink(target = "mytasks")),
            @MenuItem(name = "calendar", title = "${uiLabelMap.WorkEffortCalendar}", link = @MenuLink(target = "calendar")),
            @MenuItem(name = "request", title = "${uiLabelMap.WorkEffortRequestList}", link = @MenuLink(target = "requestlist")),
            @MenuItem(name = "workeffort", title = "${uiLabelMap.WorkEffortWorkEffort}", link = @MenuLink(target = "FindWorkEffort")),
            @MenuItem(name = "timesheet", title = "${uiLabelMap.WorkEffortTimesheet}", link = @MenuLink(target = "FindTimesheet")),
            @MenuItem(name = "userJobs", title = "${uiLabelMap.WorkEffortJobList}", link = @MenuLink(target = "UserJobs")),
            @MenuItem(name = "WorkEffortICalendar", title = "${uiLabelMap.WorkEffortICalendar}", link = @MenuLink(target = "FindICalendars"))
        }
    )
    public interface WorkEffortAppBar {}

    @Menu(
        name = "WorkEffortAppSideBar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        title = "${uiLabelMap.WorkEffortManager}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "WorkEffortAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true",
        items = {
            @MenuItem(name = "workeffort", subMenus = {@SubMenu(name = "WorkEffort", include = "component://workeffort/widget/WorkEffortMenus.xml#WorkEffortSideBar")}),
            @MenuItem(name = "timesheet", subMenus = {@SubMenu(name = "Timesheet", include = "component://workeffort/widget/TimesheetMenus.xml#TimesheetSideBar")}),
            @MenuItem(name = "WorkEffortICalendar", subMenus = {@SubMenu(name = "ICalendar", include = "component://workeffort/widget/WorkEffortMenus.xml#ICalendarSideBar")})
        }
    )
    public interface WorkEffortAppSideBar {}

    @Menu(
        name = "WorkEffortTabBar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        defaultMenuItemName = "WorkEffort",
        items = {
            @MenuItem(name = "WorkEffortRelatedSummary", title = "${uiLabelMap.WorkEffortSummary}", link = @MenuLink(target = "WorkEffortSummary", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffort", title = "${uiLabelMap.WorkEffortWorkEffort}", link = @MenuLink(target = "EditWorkEffort", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortAssocs", title = "${uiLabelMap.CommonEntityChildren}", link = @MenuLink(target = "ChildWorkEfforts", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId"), @MenuParameter(paramName = "trail", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortPartyAssigns", title = "${uiLabelMap.WorkEffortParties}", link = @MenuLink(target = "ListWorkEffortPartyAssigns", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortRates", title = "${uiLabelMap.WorkEffortTimesheetRates}", link = @MenuLink(target = "EditWorkEffortRates", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortCommEvents", title = "${uiLabelMap.WorkEffortCommEvents}", link = @MenuLink(target = "ListWorkEffortCommEvents", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortShopLists", title = "${uiLabelMap.WorkEffortShopLists}", link = @MenuLink(target = "ListWorkEffortShopLists", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortRequests", title = "${uiLabelMap.WorkEffortRequests}", link = @MenuLink(target = "ListWorkEffortRequests", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortRequirements", title = "${uiLabelMap.WorkEffortRequirements}", link = @MenuLink(target = "ListWorkEffortRequirements", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortQuotes", title = "${uiLabelMap.WorkEffortQuotes}", link = @MenuLink(target = "ListWorkEffortQuotes", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortOrderHeaders", title = "${uiLabelMap.WorkEffortOrderHeaders}", link = @MenuLink(target = "ListWorkEffortOrderHeaders", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortTimeEntries", title = "${uiLabelMap.WorkEffortTimesheetTimeEntries}", link = @MenuLink(target = "EditWorkEffortTimeEntries", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortNotes", title = "${uiLabelMap.WorkEffortNotes}", link = @MenuLink(target = "EditWorkEffortNotes", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortContents", title = "${uiLabelMap.ContentContent}", link = @MenuLink(target = "EditWorkEffortContents", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortGoodStandards", title = "${uiLabelMap.ProductProduct}", link = @MenuLink(target = "EditWorkEffortGoodStandards", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortReviews", title = "${uiLabelMap.WorkEffortReviews}", link = @MenuLink(target = "EditWorkEffortReviews", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortKeywords", title = "${uiLabelMap.WorkEffortKeywords}", link = @MenuLink(target = "EditWorkEffortKeywords", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortContactMechs", title = "${uiLabelMap.WorkEffortContactMechs}", link = @MenuLink(target = "EditWorkEffortContactMechs", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortAgreementAppls", title = "${uiLabelMap.WorkEffortAgreementAppls}", link = @MenuLink(target = "EditAgreementWorkEffortApplics", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortFixedAssetAssigns", title = "${uiLabelMap.AccountingFixedAssets}", link = @MenuLink(target = "ListWorkEffortFixedAssetAssigns", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortEventReminders", title = "${uiLabelMap.WorkEffortEventReminders}", link = @MenuLink(target = "listWorkEffortEventReminders", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")}))
        }
    )
    public interface WorkEffortTabBar {}

    @Menu(
        name = "WorkEffortSideBar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "WorkEffortTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface WorkEffortSideBar {}

    @Menu(
        name = "Calendar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "CommonButtonBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        selectedMenuItemContextFieldName = "activeSubMenuItem",
        items = {
            @MenuItem(name = "upcoming", condition = @MenuItemCondition(conditions = {@Condition(type = Empty.class, params = {"parameters.fixedAssetId"})}), link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", text = "${uiLabelMap.WorkEffortUpcomingEvents}", parameters = {@MenuParameter(paramName = "period", value = "upcoming"), @MenuParameter(paramName = "partyId", fromField = "parameters.partyId"), @MenuParameter(paramName = "fixedAssetId", fromField = "parameters.fixedAssetId"), @MenuParameter(paramName = "workEffortTypeId", fromField = "parameters.workEffortTypeId"), @MenuParameter(paramName = "calendarType", fromField = "parameters.calendarType"), @MenuParameter(paramName = "facilityId", fromField = "parameters.facilityId"), @MenuParameter(paramName = "hideEvents", fromField = "parameters.hideEvents")})),
            @MenuItem(name = "month", link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", text = "${uiLabelMap.WorkEffortMonthView}", parameters = {@MenuParameter(paramName = "period", value = "month"), @MenuParameter(paramName = "partyId", fromField = "parameters.partyId"), @MenuParameter(paramName = "fixedAssetId", fromField = "parameters.fixedAssetId"), @MenuParameter(paramName = "workEffortTypeId", fromField = "parameters.workEffortTypeId"), @MenuParameter(paramName = "calendarType", fromField = "parameters.calendarType"), @MenuParameter(paramName = "facilityId", fromField = "parameters.facilityId"), @MenuParameter(paramName = "hideEvents", fromField = "parameters.hideEvents")})),
            @MenuItem(name = "week", link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", text = "${uiLabelMap.WorkEffortWeekView}", parameters = {@MenuParameter(paramName = "period", value = "week"), @MenuParameter(paramName = "partyId", fromField = "parameters.partyId"), @MenuParameter(paramName = "fixedAssetId", fromField = "parameters.fixedAssetId"), @MenuParameter(paramName = "workEffortTypeId", fromField = "parameters.workEffortTypeId"), @MenuParameter(paramName = "calendarType", fromField = "parameters.calendarType"), @MenuParameter(paramName = "facilityId", fromField = "parameters.facilityId"), @MenuParameter(paramName = "hideEvents", fromField = "parameters.hideEvents")})),
            @MenuItem(name = "day", link = @MenuLink(target = "${parameters._LAST_VIEW_NAME_}", text = "${uiLabelMap.WorkEffortDayView}", parameters = {@MenuParameter(paramName = "period", value = "day"), @MenuParameter(paramName = "partyId", fromField = "parameters.partyId"), @MenuParameter(paramName = "fixedAssetId", fromField = "parameters.fixedAssetId"), @MenuParameter(paramName = "workEffortTypeId", fromField = "parameters.workEffortTypeId"), @MenuParameter(paramName = "calendarType", fromField = "parameters.calendarType"), @MenuParameter(paramName = "facilityId", fromField = "parameters.facilityId"), @MenuParameter(paramName = "hideEvents", fromField = "parameters.hideEvents")}))
        }
    )
    public interface Calendar {}

    @Menu(
        name = "ICalendarTabBar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "WorkEffort", title = "${uiLabelMap.WorkEffortICalendar}", link = @MenuLink(target = "EditICalendar", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortAssocs", title = "${uiLabelMap.CommonEntityChildren}", link = @MenuLink(target = "ICalendarChildren", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId"), @MenuParameter(paramName = "trail", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortPartyAssigns", title = "${uiLabelMap.WorkEffortParties}", link = @MenuLink(target = "ICalendarParties", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "WorkEffortFixedAssetAssigns", title = "${uiLabelMap.AccountingFixedAssets}", link = @MenuLink(target = "ICalendarFixedAssets", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "ICalendarData", title = "${uiLabelMap.WorkEffortICalendarData}", link = @MenuLink(target = "EditICalendarData", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")})),
            @MenuItem(name = "ICalendarHelp", title = "${uiLabelMap.CommonHelp}", link = @MenuLink(target = "ICalendarHelp", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "workEffortId")}))
        }
    )
    public interface ICalendarTabBar {}

    @Menu(
        name = "ICalendarSideBar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ICalendarTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ICalendarSideBar {}

    @Menu(
        name = "FindWorkEffortSubTabBar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditWorkEffort", title = "${uiLabelMap.WorkEffortCreate}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditWorkEffort", parameters = {@MenuParameter(paramName = "DONE_PAGE", fromField = "donePage")})),
            @MenuItem(name = "WorkEffortAdvancedSearch", title = "${uiLabelMap.CommonAdvancedSearch}", widgetStyle = "+${styles.action_nav} ${styles.action_find}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"activeSubMenuItem", "not-equals", "WorkEffortAdvancedSearch"})}), link = @MenuLink(target = "WorkEffortSearchOptions")),
            @MenuItem(name = "FindWorkEffort", title = "${uiLabelMap.CommonFind}", widgetStyle = "+${styles.action_nav} ${styles.action_find}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"activeSubMenuItem", "equals", "WorkEffortAdvancedSearch"})}), link = @MenuLink(target = "FindWorkEffort"))
        }
    )
    public interface FindWorkEffortSubTabBar {}

    @Menu(
        name = "EditWorkEffortSubTabBar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditWorkEffort", title = "${uiLabelMap.WorkEffortCreate}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"workEffort"})}), link = @MenuLink(target = "EditWorkEffort", parameters = {@MenuParameter(paramName = "DONE_PAGE", fromField = "donePage")}))
        }
    )
    public interface EditWorkEffortSubTabBar {}

    @Menu(
        name = "ChildWorkEffortsSubTabBar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "AddWorkEffortAssoc", title = "${uiLabelMap.WorkEffortAddExistingWorkEffortChild}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "AddWorkEffortAssoc", parameters = {@MenuParameter(paramName = "workEffortIdFrom", fromField = "workEffortId")})),
            @MenuItem(name = "AddWorkEffortAndAssoc", title = "${uiLabelMap.WorkEffortAddChild}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "AddWorkEffortAndAssoc", parameters = {@MenuParameter(paramName = "workEffortIdFrom", fromField = "workEffortId")}))
        }
    )
    public interface ChildWorkEffortsSubTabBar {}

    @Menu(
        name = "FindICalendarsSubTabBar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditICalendar", title = "${uiLabelMap.CommonCreate}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", link = @MenuLink(target = "EditICalendar")),
            @MenuItem(name = "ICalendarHelp", title = "${uiLabelMap.CommonHelp}", widgetStyle = "+${styles.action_nav} ${styles.action_view}", link = @MenuLink(target = "ICalendarHelp"))
        }
    )
    public interface FindICalendarsSubTabBar {}

    @Menu(
        name = "EditICalendarsSubTabBar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "EditICalendar", title = "${uiLabelMap.CommonCreate}", widgetStyle = "+${styles.action_nav} ${styles.action_add}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"workEffort"})}), link = @MenuLink(target = "EditICalendar"))
        }
    )
    public interface EditICalendarsSubTabBar {}

    @Menu(
        name = "EditWorkEffortAssocSubTabBar",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "removeWorkEffortAssoc", title = "${uiLabelMap.CommonRemoveAssoc}", widgetStyle = "+${styles.action_run_sys} ${styles.action_remove}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"workEffortAssoc"})}), link = @MenuLink(target = "removeWorkEffortAssoc", linkType = LinkType.HIDDEN_FORM, parameters = {@MenuParameter(paramName = "workEffortIdFrom", fromField = "workEffortAssoc.workEffortIdFrom"), @MenuParameter(paramName = "workEffortIdTo", fromField = "workEffortAssoc.workEffortIdTo"), @MenuParameter(paramName = "workEffortAssocTypeId", fromField = "workEffortAssoc.workEffortAssocTypeId"), @MenuParameter(paramName = "fromDate", fromField = "workEffortAssoc.fromDate")}))
        }
    )
    public interface EditWorkEffortAssocSubTabBar {}

    @Menu(
        name = "Day",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "Calendar"
    )
    public interface Day {}

    @Menu(
        name = "Week",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "Calendar"
    )
    public interface Week {}

    @Menu(
        name = "Month",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "Calendar"
    )
    public interface Month {}

    @Menu(
        name = "Upcoming",
        location = "component://workeffort/widget/WorkEffortMenus.xml",
        extendsMenu = "Calendar"
    )
    public interface Upcoming {}

}

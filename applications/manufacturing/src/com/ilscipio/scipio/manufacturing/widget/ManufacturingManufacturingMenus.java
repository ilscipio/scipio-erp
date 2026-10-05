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
public class ManufacturingManufacturingMenus {

    @Menu(
        name = "ManufacturingAppBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        title = "${uiLabelMap.ManufacturingManager}",
        extendsMenu = "CommonAppBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        itemsSortMode = "off",
        items = {
            @MenuItem(name = "jobshop", title = "${uiLabelMap.ManufacturingJobShop}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_CREATE"})}), link = @MenuLink(target = "FindProductionRun")),
            @MenuItem(name = "fabricationOrders", title = "${uiLabelMap.ManufacturingFabricationOrders}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_VIEW"})}), link = @MenuLink(target = "FindFabricationOrders")),
            @MenuItem(name = "shopFloor", title = "${uiLabelMap.ManufacturingShopFloor}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_VIEW"})}), link = @MenuLink(target = "ShopFloor")),
            @MenuItem(name = "scan", title = "${uiLabelMap.ManufacturingScan}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_VIEW"})}), link = @MenuLink(target = "ScanTask")),
            @MenuItem(name = "workCenterLoad", title = "${uiLabelMap.ManufacturingWorkCenterLoad}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_VIEW"})}), link = @MenuLink(target = "WorkCenterLoad")),
            @MenuItem(name = "routing", title = "${uiLabelMap.ManufacturingRouting}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_CREATE"})}), link = @MenuLink(target = "FindRouting")),
            @MenuItem(name = "routingTask", title = "${uiLabelMap.ManufacturingRoutingTask}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_CREATE"})}), link = @MenuLink(target = "FindRoutingTask")),
            @MenuItem(name = "calendar", title = "${uiLabelMap.ManufacturingCalendar}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_CREATE"})}), link = @MenuLink(target = "FindCalendar")),
            @MenuItem(name = "costs", title = "${uiLabelMap.ManufacturingCostCalcs}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_CREATE"})}), link = @MenuLink(target = "EditCostCalcs")),
            @MenuItem(name = "bom", title = "${uiLabelMap.ManufacturingBillOfMaterials}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_CREATE"})}), link = @MenuLink(target = "FindBom")),
            @MenuItem(name = "mrp", title = "${uiLabelMap.ManufacturingMrp}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_CREATE"})}), link = @MenuLink(target = "MrpRuns")),
            @MenuItem(name = "ShipmentPlans", title = "${uiLabelMap.ManufacturingShipmentPlans}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_CREATE"})}), link = @MenuLink(target = "WorkWithShipmentPlans")),
            @MenuItem(name = "ManufacturingReports", title = "${uiLabelMap.ManufacturingReports}", condition = @MenuItemCondition(mode = "omit", conditions = {@Condition(type = HasPermission.class, params = {"MANUFACTURING", "_CREATE"})}), link = @MenuLink(target = "ReportsHub"))
        }
    )
    public interface ManufacturingAppBar {}

    @Menu(
        name = "ManufacturingAppSideBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        title = "${uiLabelMap.ManufacturingManager}",
        extendsMenu = "CommonAppSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        itemsSortMode = "off",
        includeElements = {
            @IncludeElements(menuName = "ManufacturingAppBar")
        },
        alwaysExpandSelectedOrAncestor = "true",
        items = {
            @MenuItem(name = "jobshop", subMenus = {@SubMenu(name = "ProductionRun", include = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml#ProductionRunSideBar")}),
            @MenuItem(name = "routing", subMenus = {@SubMenu(name = "Routing", include = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml#RoutingSideBar")}),
            @MenuItem(name = "routingTask", subMenus = {@SubMenu(name = "RoutingTask", include = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml#RoutingTaskSideBar")}),
            @MenuItem(name = "calendar", subMenus = {@SubMenu(name = "Calendar", include = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml#CalendarSideBar")}),
            @MenuItem(name = "bom", subMenus = {@SubMenu(name = "Bom", include = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml#BomSideBar")}),
            @MenuItem(name = "mrp", subMenus = {@SubMenu(name = "Mrp", include = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml#MrpSideBar")})
        }
    )
    public interface ManufacturingAppSideBar {}

    @Menu(
        name = "BomTabBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "bomSimulation", title = "${uiLabelMap.ManufacturingBomSimulation}", link = @MenuLink(target = "BomSimulation", parameters = {@MenuParameter(paramName = "productId", fromField = "parameters.productId"), @MenuParameter(paramName = "productAssocTypeId", fromField = "parameters.productAssocTypeId")})),
            @MenuItem(name = "EditProductBom", title = "${uiLabelMap.ManufacturingBillOfMaterials}", alwaysExpandSelectedOrAncestor = "false", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "EditProductBom", parameters = {@MenuParameter(paramName = "productId", fromField = "parameters.productId"), @MenuParameter(paramName = "productAssocTypeId", fromField = "parameters.productAssocTypeId")})),
            @MenuItem(name = "productManufacturingRules", title = "${uiLabelMap.ManufacturingManufacturingRules}", link = @MenuLink(target = "EditProductManufacturingRules", parameters = {@MenuParameter(paramName = "productId", fromField = "parameters.productId")})),
            @MenuItem(name = "productCost", title = "${uiLabelMap.ManufacturingProductStandardCost}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "ProductCost", parameters = {@MenuParameter(paramName = "productId", fromField = "parameters.productId")})),
            @MenuItem(name = "whereUsed", title = "${uiLabelMap.ManufacturingWhereUsed}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"parameters.productId"})}), link = @MenuLink(target = "ProductWhereUsed", parameters = {@MenuParameter(paramName = "productId", fromField = "parameters.productId")}))
        }
    )
    public interface BomTabBar {}

    @Menu(
        name = "BomSideBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "BomTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface BomSideBar {}

    @Menu(
        name = "ProductionRunTabBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "edit", title = "${uiLabelMap.ManufacturingProductionRun}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "productionRun.currentStatusId", operator = "equals", value = "PRUN_CREATED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "productionRun.currentStatusId", operator = "equals", value = "PRUN_SCHEDULED")})}), link = @MenuLink(target = "EditProductionRun", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "tasks", title = "${uiLabelMap.ManufacturingListOfProductionRunRoutingTasks}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "productionRun.currentStatusId", operator = "equals", value = "PRUN_CREATED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "productionRun.currentStatusId", operator = "equals", value = "PRUN_SCHEDULED")})}), link = @MenuLink(target = "ProductionRunTasks", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "components", title = "${uiLabelMap.ManufacturingMaterials}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "productionRun.currentStatusId", operator = "equals", value = "PRUN_CREATED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "productionRun.currentStatusId", operator = "equals", value = "PRUN_SCHEDULED")})}), link = @MenuLink(target = "ProductionRunComponents", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "fixedAssets", title = "${uiLabelMap.AccountingFixedAssets}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "productionRun.currentStatusId", operator = "equals", value = "PRUN_CREATED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "productionRun.currentStatusId", operator = "equals", value = "PRUN_SCHEDULED")})}), link = @MenuLink(target = "ProductionRunFixedAssets", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "declaration", title = "${uiLabelMap.ManufacturingProductionRunDeclaration}", condition = @MenuItemCondition(conditions = {@Condition(type = And.class, tree = {@ConditionNode(not = true, type = Compare.class, params = {"productionRun.currentStatusId", "equals", "PRUN_CREATED"}), @ConditionNode(not = true, type = Compare.class, params = {"productionRun.currentStatusId", "equals", "PRUN_SCHEDULED"})})}), link = @MenuLink(target = "ProductionRunDeclaration", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "actualComponents", title = "${uiLabelMap.ManufacturingActualMaterials}", condition = @MenuItemCondition(conditions = {@Condition(type = And.class, tree = {@ConditionNode(not = true, type = Compare.class, params = {"productionRun.currentStatusId", "equals", "PRUN_CREATED"}), @ConditionNode(not = true, type = Compare.class, params = {"productionRun.currentStatusId", "equals", "PRUN_SCHEDULED"})})}), link = @MenuLink(target = "ProductionRunActualComponents", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "assocs", title = "${uiLabelMap.ManufacturingProductionRunAssocs}", link = @MenuLink(target = "ProductionRunAssocs", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "content", title = "${uiLabelMap.CommonContent}", link = @MenuLink(target = "ProductionRunContent", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "costs", title = "${uiLabelMap.ManufacturingActualCosts}", link = @MenuLink(target = "ProductionRunCosts", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "labelsPdf", title = "${uiLabelMap.ManufacturingLabelsPdf}", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", link = @MenuLink(target = "ProductionRunLabelsPdf", targetWindow = "_BLANK", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")}))
        }
    )
    public interface ProductionRunTabBar {}

    @Menu(
        name = "ProductionRunSideBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ProductionRunTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ProductionRunSideBar {}

    @Menu(
        name = "ProductionRunStatusTabBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "cancel", title = "${uiLabelMap.ManufacturingCancel}", link = @MenuLink(target = "cancelProductionRun", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "quickChangeClose", title = "${uiLabelMap.ManufacturingQuickClose}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_CANCELLED"}), @Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_COMPLETED"}), @Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_CLOSED"})}), link = @MenuLink(target = "quickChangeProductionRunStatus", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId"), @MenuParameter(paramName = "statusId", value = "PRUN_CLOSED")})),
            @MenuItem(name = "quickChangeComplete", title = "${uiLabelMap.ManufacturingQuickComplete}", widgetStyle = "+${styles.action_run_sys} ${styles.action_complete}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_CANCELLED"}), @Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_COMPLETED"}), @Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_CLOSED"})}), link = @MenuLink(target = "quickChangeProductionRunStatus", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId"), @MenuParameter(paramName = "statusId", value = "PRUN_COMPLETED")})),
            @MenuItem(name = "changeStatusToPrinted", title = "${uiLabelMap.ManufacturingConfirmProductionRun}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "productionRun.currentStatusId", operator = "equals", value = "PRUN_CREATED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "productionRun.currentStatusId", operator = "equals", value = "PRUN_SCHEDULED")})}), link = @MenuLink(target = "changeProductionRunStatusToPrinted", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "quickRunAllProductionRunTasks", title = "${uiLabelMap.ManufacturingQuickRunAllTasks}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_CREATED"}), @Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_SCHEDULED"}), @Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_CANCELLED"}), @Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_COMPLETED"}), @Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_CLOSED"})}), link = @MenuLink(target = "quickStartAllProductionRunTasks", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "quickStartAllProductionRunTasks", title = "${uiLabelMap.ManufacturingQuickStartAllTasks}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_CREATED"}), @Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_SCHEDULED"}), @Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_CANCELLED"}), @Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_COMPLETED"}), @Condition(type = Compare.class, params = {"productionRun.currentStatusId", "not-equals", "PRUN_CLOSED"})}), link = @MenuLink(target = "quickStartAllProductionRunTasks", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "quickChangeComplete2Close", title = "${uiLabelMap.ManufacturingQuickClose}", widgetStyle = "+${styles.action_run_sys} ${styles.action_terminate}", condition = @MenuItemCondition(conditions = {@Condition(type = Compare.class, params = {"productionRun.currentStatusId", "equals", "PRUN_COMPLETED"})}), link = @MenuLink(target = "quickChangeProductionRunStatus", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId"), @MenuParameter(paramName = "statusId", value = "PRUN_CLOSED")})),
            @MenuItem(name = "schedule", title = "${uiLabelMap.ManufacturingSchedule}", condition = @MenuItemCondition(or = {@com.ilscipio.scipio.widget.def.screen.OrCondition(ifCompare = {@com.ilscipio.scipio.widget.def.screen.IfCompare(field = "productionRun.currentStatusId", operator = "equals", value = "PRUN_CREATED"), @com.ilscipio.scipio.widget.def.screen.IfCompare(field = "productionRun.currentStatusId", operator = "equals", value = "PRUN_SCHEDULED")})}), link = @MenuLink(target = "scheduleProductionRun", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId"), @MenuParameter(paramName = "statusId", value = "PRUN_SCHEDULED")})),
            @MenuItem(name = "link", title = "${uiLabelMap.ManufacturingLinkProductionRun}", link = @MenuLink(target = "LinkProductionRun", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")})),
            @MenuItem(name = "print", title = "${uiLabelMap.CommonPrint} (${uiLabelMap.CommonPdf})", widgetStyle = "+${styles.action_run_sys} ${styles.action_export}", link = @MenuLink(target = "PrintProductionRun", targetWindow = "_BLANK", parameters = {@MenuParameter(paramName = "productionRunId", fromField = "productionRunId")}))
        }
    )
    public interface ProductionRunStatusTabBar {}

    @Menu(
        name = "ProductionRunStatusSubTabBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        extendsMenu = "CommonSubTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "ProductionRunStatusTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface ProductionRunStatusSubTabBar {}

    @Menu(
        name = "MrpTabBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "RunMrp", title = "${uiLabelMap.ManufacturingRunMrp}", link = @MenuLink(target = "RunMrp")),
            @MenuItem(name = "MrpRuns", title = "${uiLabelMap.ManufacturingMrpRuns}", link = @MenuLink(target = "MrpRuns")),
            @MenuItem(name = "MrpProposals", title = "${uiLabelMap.ManufacturingMrpProposals}", link = @MenuLink(target = "MrpProposals")),
            @MenuItem(name = "findInventoryEventPlan", title = "${uiLabelMap.ManufacturingMrpLog}", link = @MenuLink(target = "MrpRuns"))
        }
    )
    public interface MrpTabBar {}

    @Menu(
        name = "MrpSideBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "MrpTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface MrpSideBar {}

    @Menu(
        name = "CalendarTabBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        actions = @MenuActions(script = {@ScriptAction(location = "component://manufacturing/webapp/manufacturing/WEB-INF/actions/generated/CalendarTabBar_script1.groovy")}),
        items = {
            @MenuItem(name = "CalendarWeek", title = "${uiLabelMap.ManufacturingCalendarWeeks}", link = @MenuLink(target = "ListCalendarWeek")),
            @MenuItem(name = "calendar", title = "${uiLabelMap.ManufacturingCalendar}", alwaysExpandSelectedOrAncestor = "false", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"techDataCalendar"})}), link = @MenuLink(target = "EditCalendar", parameters = {@MenuParameter(paramName = "calendarId", fromField = "techDataCalendar.calendarId")})),
            @MenuItem(name = "calendarExceptionDay", title = "${uiLabelMap.ManufacturingCalendarExceptionDate}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"techDataCalendar"})}), link = @MenuLink(target = "EditCalendarExceptionDay", parameters = {@MenuParameter(paramName = "calendarId", fromField = "techDataCalendar.calendarId")})),
            @MenuItem(name = "calendarExceptionWeek", title = "${uiLabelMap.ManufacturingCalendarExceptionWeek}", condition = @MenuItemCondition(not = true, conditions = {@Condition(type = Empty.class, params = {"techDataCalendar"})}), link = @MenuLink(target = "EditCalendarExceptionWeek", parameters = {@MenuParameter(paramName = "calendarId", fromField = "techDataCalendar.calendarId")}))
        }
    )
    public interface CalendarTabBar {}

    @Menu(
        name = "CalendarSideBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "CalendarTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface CalendarSideBar {}

    @Menu(
        name = "RoutingTabBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "editRouting", title = "${uiLabelMap.ManufacturingRouting}", link = @MenuLink(target = "EditRouting", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "routing.workEffortId")})),
            @MenuItem(name = "routingTaskAssoc", title = "${uiLabelMap.ManufacturingEditRoutingTaskAssoc}", link = @MenuLink(target = "EditRoutingTaskAssoc", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "routing.workEffortId")})),
            @MenuItem(name = "routingProductLink", title = "${uiLabelMap.ManufacturingEditRoutingProductLink}", link = @MenuLink(target = "EditRoutingProductLink", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "routing.workEffortId")}))
        }
    )
    public interface RoutingTabBar {}

    @Menu(
        name = "RoutingSideBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "RoutingTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface RoutingSideBar {}

    @Menu(
        name = "RoutingTaskTabBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        extendsMenu = "CommonTabBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        items = {
            @MenuItem(name = "editRoutingTask", title = "${uiLabelMap.ManufacturingRoutingTask}", link = @MenuLink(target = "EditRoutingTask", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "routingTask.workEffortId")})),
            @MenuItem(name = "editRoutingTaskCosts", title = "${uiLabelMap.ManufacturingRoutingTaskCosts}", link = @MenuLink(target = "EditRoutingTaskCosts", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "routingTask.workEffortId")})),
            @MenuItem(name = "listRoutingTaskProducts", title = "${uiLabelMap.ManufacturingListProducts}", link = @MenuLink(target = "ListRoutingTaskProducts", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "routingTask.workEffortId")})),
            @MenuItem(name = "editRoutingTaskFixedAssets", title = "${uiLabelMap.ManufacturingRoutingTaskFixedAssets}", link = @MenuLink(target = "EditRoutingTaskFixedAssets", parameters = {@MenuParameter(paramName = "workEffortId", fromField = "routingTask.workEffortId")}))
        }
    )
    public interface RoutingTaskTabBar {}

    @Menu(
        name = "RoutingTaskSideBar",
        location = "component://manufacturing/widget/manufacturing/ManufacturingMenus.xml",
        menuContainerStyle = "+scipio-nav-actions-menu",
        extendsMenu = "CommonSideBarMenu",
        extendsResource = "component://common/widget/CommonMenus.xml",
        includeElements = {
            @IncludeElements(menuName = "RoutingTaskTabBar", recursive = RecursiveMode.INCLUDES_ONLY)
        }
    )
    public interface RoutingTaskSideBar {}

}

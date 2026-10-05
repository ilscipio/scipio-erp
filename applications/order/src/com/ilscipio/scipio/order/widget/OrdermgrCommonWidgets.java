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
package com.ilscipio.scipio.order.widget;

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
public class OrdermgrCommonWidgets {

    @Screen(name = "DashboardStatsOrderTotal", location = "component://order/widget/ordermgr/CommonWidgets.xml")
    @Section(widgets = @Widgets(containers = {@Container(style = "${styles.grid_large}4 ${styles.grid_cell}", includeScreens = {@IncludeScreen(name = "DashboardStatsOrderTotalWeek", location = "component://order/widget/ordermgr/CommonWidgets.xml")}), @Container(style = "${styles.grid_large}4 ${styles.grid_cell}", includeScreens = {@IncludeScreen(name = "DashboardStatsOrderTotalMonth", location = "component://order/widget/ordermgr/CommonWidgets.xml")}), @Container(style = "${styles.grid_large}4 ${styles.grid_cell}", includeScreens = {@IncludeScreen(name = "DashboardStatsReturnReasonWeek", location = "component://order/widget/ordermgr/CommonWidgets.xml")})}))
    public interface DashboardStatsOrderTotal {}

    @Screen(name = "DashboardStatsOrderTotalDay", location = "component://order/widget/ordermgr/CommonWidgets.xml")
    @Action(type = ActionType.SET, field = "chartType", value = "line")
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "1", valueType = "Integer")
    @Action(type = ActionType.SET, field = "chartLibrary", value = "chart")
    @Action(type = ActionType.SET, field = "chartIntervalScope", value = "day")
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "6", valueType = "Integer")
    @Action(type = ActionType.SET, field = "xlabel", value = "${uiLabelMap.OrderNetSales}")
    @Action(type = ActionType.SET, field = "ylabel", value = "${uiLabelMap.CommonDay}")
    @Action(type = ActionType.SET, field = "label1", value = "${uiLabelMap.OrderTotal}")
    @Action(type = ActionType.SET, field = "label2", value = "${uiLabelMap.OrderOrders}")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/stats/StatsOrderTotal.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.OrderGrossSales} (${uiLabelMap.CommonPerDay})")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/dashboard/statsOrderTotal.ftl")}))
    public interface DashboardStatsOrderTotalDay {}

    @Screen(name = "DashboardStatsOrderTotalWeek", location = "component://order/widget/ordermgr/CommonWidgets.xml")
    @Action(type = ActionType.SET, field = "chartType", value = "line")
    @Action(type = ActionType.SET, field = "chartLibrary", value = "chart")
    @Action(type = ActionType.SET, field = "chartIntervalScope", value = "week")
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "4", valueType = "Integer")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.OrderGrossSales} (${uiLabelMap.CommonPerWeek})")
    @Action(type = ActionType.SET, field = "xlabel", value = "${uiLabelMap.OrderNetSales}")
    @Action(type = ActionType.SET, field = "ylabel", value = "${uiLabelMap.CommonWeek}")
    @Action(type = ActionType.SET, field = "label1", value = "${uiLabelMap.OrderTotal}")
    @Action(type = ActionType.SET, field = "label2", value = "${uiLabelMap.OrderOrders}")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/stats/StatsOrderTotal.groovy")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/dashboard/statsOrderTotal.ftl")}))
    public interface DashboardStatsOrderTotalWeek {}

    @Screen(name = "DashboardStatsOrderTotalMonth", location = "component://order/widget/ordermgr/CommonWidgets.xml")
    @Action(type = ActionType.SET, field = "chartType", value = "bar")
    @Action(type = ActionType.SET, field = "chartLibrary", value = "chart")
    @Action(type = ActionType.SET, field = "chartIntervalScope", value = "month")
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "3", valueType = "Integer")
    @Action(type = ActionType.SET, field = "xlabel", value = "${uiLabelMap.OrderNetSales}")
    @Action(type = ActionType.SET, field = "ylabel", value = "${uiLabelMap.CommonMonth}")
    @Action(type = ActionType.SET, field = "label1", value = "${uiLabelMap.OrderTotal}")
    @Action(type = ActionType.SET, field = "label2", value = "${uiLabelMap.OrderOrders}")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/stats/StatsOrderTotal.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.OrderGrossSales} (${uiLabelMap.CommonPerMonth})")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/dashboard/statsOrderTotal.ftl")}))
    public interface DashboardStatsOrderTotalMonth {}

    @Screen(name = "DashboardWSLiveOrders", location = "component://order/widget/ordermgr/CommonWidgets.xml")
    @Action(type = ActionType.SET, field = "chartIntervalScope", value = "day")
    @Action(type = ActionType.SET, field = "xlabel", value = "${uiLabelMap.OrderNetSales}")
    @Action(type = ActionType.SET, field = "ylabel", value = "${uiLabelMap.CommonHour}")
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "24", valueType = "Integer")
    @Action(type = ActionType.SET, field = "label1", value = "${uiLabelMap.OrderTotal}")
    @Action(type = ActionType.SET, field = "label2", value = "${uiLabelMap.OrderOrders}")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/stats/StatsOrderTotal.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.OrderGrossSales} (${uiLabelMap.CommonPerHour})")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/dashboard/wsLiveOrders.ftl")}))
    public interface DashboardWSLiveOrders {}

    @Screen(name = "DashboardWSLiveOrdersTable", location = "component://order/widget/ordermgr/CommonWidgets.xml")
    @Action(type = ActionType.SET, field = "todayDate", value = "${groovy:org.ofbiz.base.util.UtilDateTime.nowDate();}", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "inmap.minDate", value = "${groovy:org.ofbiz.base.util.UtilDateTime.getDayStart(todayDate,-1);}", valueType = "String")
    @Action(type = ActionType.SET, field = "inmap.orderStatusId", value = "[ORDER_CREATED, ORDER_APPROVED, ORDER_PICKED, ORDER_PACKED,ORDER_SENT,ORDER_HOLD,ORDER_PROCESSING]")
    @Action(type = ActionType.SET, field = "inmap.orderTypeId", value = "SALES_ORDER", valueType = "String")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "OrderHeader", list = "orderList", useCache = true, conditions = {@ConditionExpr(fieldName = "lastUpdatedStamp", operator = "greater-equals", fromField = "inmap.minDate"), @ConditionExpr(fieldName = "statusId", operator = "in", fromField = "inmap.orderStatusId"), @ConditionExpr(fieldName = "orderTypeId", operator = "equals", fromField = "inmap.orderTypeId")}, orderBy = {"orderDate"})
    @Action(type = ActionType.SET, field = "layoutSettings.javaScriptsFooter[]", value = "https://cdn.datatables.net/scroller/2.0.3/js/dataTables.scroller.min.js", global = true)
    @Action(type = ActionType.SET, field = "layoutSettings.styleSheets[+0]", value = "https://cdn.datatables.net/scroller/2.0.3/css/scroller.dataTables.min.css", global = true)
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/dashboard/wsLiveOrdersTable.ftl")}))
    public interface DashboardWSLiveOrdersTable {}

    @Screen(name = "DashboardWSLiveOrderItem", location = "component://order/widget/ordermgr/CommonWidgets.xml")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/dashboard/wsLiveOrderItem.ftl")}))
    public interface DashboardWSLiveOrderItem {}

    @Screen(name = "DashboardStatsReturnReasonWeek", location = "component://order/widget/ordermgr/CommonWidgets.xml")
    @Action(type = ActionType.SET, field = "chartType", value = "doughnut")
    @Action(type = ActionType.SET, field = "chartLibrary", value = "chart")
    @Action(type = ActionType.SET, field = "chartIntervalScope", value = "month")
    @Action(type = ActionType.SET, field = "xlabel", value = "${uiLabelMap.OrderNetSales}")
    @Action(type = ActionType.SET, field = "ylabel", value = "${uiLabelMap.CommonReason}")
    @Action(type = ActionType.SET, field = "label1", value = "${uiLabelMap.OrderTotal}")
    @Action(type = ActionType.SET, field = "label2", value = "${uiLabelMap.OrderOrders}")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/stats/StatsReturnReason.groovy")
    @Action(type = ActionType.SET, field = "title", value = "${uiLabelMap.ReasonForReturns}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://order/webapp/ordermgr/dashboard/statsReturnReason.ftl")}))
    public interface DashboardStatsReturnReasonWeek {}

}

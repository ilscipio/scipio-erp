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
package com.ilscipio.scipio.marketing.widget;

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
public class SfaAnalyticsScreens {

    @Screen(name = "CommonAnalyticsActions", location = "component://marketing/widget/sfa/AnalyticsScreens.xml")
    @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/generated/CommonAnalyticsActions_script1.groovy")
    public interface CommonAnalyticsActions {}

    @Screen(name = "AnalyticsSales", location = "component://marketing/widget/sfa/AnalyticsScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "MarketingAnalyticsSales")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AnalyticsSales")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonAnalyticsActions")
    @Action(type = ActionType.SET, field = "chartIntervalScope", value = "${parameters.intervalScope}", defaultValue = "month")
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "6")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "Enumeration", list = "salesChannelList", conditions = {@ConditionExpr(fieldName = "enumTypeId", operator = "equals", value = "ORDER_SALES_CHANNEL")})
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "StatusItem", list = "orderStatusList", conditions = {@ConditionExpr(fieldName = "statusId", operator = "in", fromField = "allowedOrderStatus")})
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "AnalyticsSalesStatsActions")
    @DecoratorScreen(
        name = "CommonAnalyticsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}4 ${styles.grid_cell}", htmlTemplates = {
                        @HtmlTemplate(location = "component://marketing/webapp/sfa/analytics/FindAnalyticsSales.ftl"
                    )}),
                    @Container2(style = "${styles.grid_large}8 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "AnalyticsSalesStats", location = "component://marketing/widget/sfa/AnalyticsScreens.xml"
                    )})})})
        }
    )
    public interface AnalyticsSales {}

    @Screen(name = "AnalyticsSalesStatsActions", location = "component://marketing/widget/sfa/AnalyticsScreens.xml")
    @IfAction(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = True.class, params = {"ranAnalyticsSalesStatsActions"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "chartType", value = "line"), @Action(type = ActionType.SET, field = "chartDatasets", value = "1"), @Action(type = ActionType.SET, field = "chartLibrary", value = "chart"), @Action(type = ActionType.SET, field = "chartIntervalScope", value = "${parameters.intervalScope}", defaultValue = "month"), @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/analytics/SalesChart.groovy"), @Action(type = ActionType.SET, field = "ranAnalyticsSalesStatsActions", value = "true", valueType = "Boolean")}))
    public interface AnalyticsSalesStatsActions {}

    @Screen(name = "AnalyticsSalesStats", location = "component://marketing/widget/sfa/AnalyticsScreens.xml")
    @Action(order = 0, type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "AnalyticsSalesStatsActions")
    @Action(order = 1, type = ActionType.SET, field = "totalsDesc", value = "${uiLabelMap.OrderOrders}: ${orderStats.totalOrderCount}; ${uiLabelMap.OrderTotal}: ${orderStats.totalGrandTotal?currency(${context.currencyUomId})}")
    @IfAction(order = 2, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"productStoreId"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "chartTitle", value = "${totalsDesc}")}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "chartTitle", value = "${productStoreId} ${totalsDesc}")}))
    @Action(order = 3, type = ActionType.SET, field = "xlabel", value = "${uiLabelMap.OrderGrandTotal} (${currencyUomId})")
    @Action(order = 4, type = ActionType.SET, field = "ylabel", value = "${uiLabelMap.CommonWeek}")
    @Action(order = 5, type = ActionType.SET, field = "label1", value = "${uiLabelMap.OrderGrandTotal} (${currencyUomId})")
    @Action(order = 6, type = ActionType.SET, field = "label2", value = "${uiLabelMap.OrderOrders}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://marketing/webapp/sfa/analytics/AnalyticsSalesChart.ftl")}))
    public interface AnalyticsSalesStats {}

    @Screen(name = "AnalyticsTracking", location = "component://marketing/widget/sfa/AnalyticsScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "MarketingAnalytics")
    @Action(type = ActionType.SET, field = "activeSubMenuItem", value = "AnalyticsTracking")
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "CommonAnalyticsActions")
    @Action(type = ActionType.SET, field = "chartIntervalScope", value = "${parameters.intervalScope}", defaultValue = "week")
    @Action(type = ActionType.SET, field = "chartIntervalCount", value = "6")
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "MarketingCampaign", list = "marketingCampaignList", filterByDate = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "TrackingCode", list = "trackingCodeList", conditions = {@ConditionExpr(fieldName = "marketingCampaignId", operator = "equals", fromField = "parameters.marketingCampaignId")})
    @Action(type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "AnalyticsTrackingStatsActions")
    @DecoratorScreen(
        name = "CommonAnalyticsDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", containers = {
                @Container(style = "${styles.grid_row}", containers = {
                    @Container2(style = "${styles.grid_large}4 ${styles.grid_cell}", htmlTemplates = {
                        @HtmlTemplate(location = "component://marketing/webapp/sfa/analytics/FindAnalyticsTracking.ftl"
                    )}),
                    @Container2(style = "${styles.grid_large}8 ${styles.grid_cell}", includeScreens = {
                        @IncludeScreen(name = "AnalyticsTrackingStats", location = "component://marketing/widget/sfa/AnalyticsScreens.xml"
                    )})})})
        }
    )
    public interface AnalyticsTracking {}

    @Screen(name = "AnalyticsTrackingStatsActions", location = "component://marketing/widget/sfa/AnalyticsScreens.xml")
    @IfAction(order = 0, condition = @com.ilscipio.scipio.widget.def.screen.Condition(not = true, functionalConditions = {@Condition(type = True.class, params = {"ranAnalyticsTrackingStatsActions"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "chartType", value = "line"), @Action(type = ActionType.SET, field = "chartData", value = "week"), @Action(type = ActionType.SET, field = "chartDatasets", value = "2"), @Action(type = ActionType.SET, field = "chartLibrary", value = "chart"), @Action(type = ActionType.SET, field = "chartIntervalScope", value = "${parameters.intervalScope}", defaultValue = "week"), @Action(type = ActionType.SCRIPT, location = "component://marketing/webapp/marketing/WEB-INF/actions/analytics/TrackingCodeChart.groovy"), @Action(type = ActionType.SET, field = "ranAnalyticsTrackingStatsActions", value = "true", valueType = "Boolean")}))
    public interface AnalyticsTrackingStatsActions {}

    @Screen(name = "AnalyticsTrackingStats", location = "component://marketing/widget/sfa/AnalyticsScreens.xml")
    @Action(order = 0, type = ActionType.INCLUDE_SCREEN_ACTIONS, name = "AnalyticsTrackingStatsActions")
    @Action(order = 1, type = ActionType.SET, field = "totalsDesc", value = "${uiLabelMap.OrderOrders}: ${trackingStats.totalOrders}; ${uiLabelMap.PartyVisits}: ${trackingStats.totalVisits}")
    @IfAction(order = 2, condition = @com.ilscipio.scipio.widget.def.screen.Condition(functionalConditions = {@Condition(type = Empty.class, params = {"productStoreId"})}), then = @Actions(value = {@Action(type = ActionType.SET, field = "chartTitle", value = "${totalsDesc}")}), elseActions = @Actions(value = {@Action(type = ActionType.SET, field = "chartTitle", value = "${productStoreId} ${totalsDesc}")}))
    @Action(order = 3, type = ActionType.SET, field = "xlabel", value = "${uiLabelMap.ProductSales}")
    @Action(order = 4, type = ActionType.SET, field = "ylabel", value = "${uiLabelMap.CommonWeek}")
    @Action(order = 5, type = ActionType.SET, field = "label1", value = "${uiLabelMap.PartyVisits}")
    @Action(order = 6, type = ActionType.SET, field = "label2", value = "${uiLabelMap.OrderOrders}")
    @Section(widgets = @Widgets(value = {@Widget(type = WidgetType.HTML_TEMPLATE, location = "component://marketing/webapp/sfa/analytics/AnalyticsTrackingChart.ftl")}))
    public interface AnalyticsTrackingStats {}

}

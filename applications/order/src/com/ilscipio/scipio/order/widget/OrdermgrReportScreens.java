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
public class OrdermgrReportScreens {

    @Screen(name = "OrderPurchaseReportOptions", location = "component://order/widget/ordermgr/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderReports")
    @Action(type = ActionType.PROPERTY_MAP, resource = "BirtUiLabels", mapName = "uiLabelMap", global = true)
    @DecoratorScreen(
        name = "CommonReportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", value = {
                @Widget(type = WidgetType.LABEL, text = "${uiLabelMap.BirtOrderReportsWarning}", style = "common-msg-info-important"
            )}, containers = {
                @Container(style = "${styles.grid_large}12 ${styles.grid_cell}", widgets = {
                    @Widget(type = WidgetType.INCLUDE_PORTAL_PAGE, id = "OrderReportPage"
                )})})
        }
    )
    public interface OrderPurchaseReportOptions {}

    @Screen(name = "OrderReportSalesByStore", location = "component://order/widget/ordermgr/ReportScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReportSalesByStore}", includeForms = {@IncludeForm(name = "SalesByStoreReport", location = "component://order/widget/ordermgr/ReportForms.xml")})}))
    public interface OrderReportSalesByStore {}

    @Screen(name = "OrderReportOpenOrderItems", location = "component://order/widget/ordermgr/ReportScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReportOpenOrderItems}", includeForms = {@IncludeForm(name = "OpenOrderItemsReport", location = "component://order/widget/ordermgr/ReportForms.xml")})}))
    public interface OrderReportOpenOrderItems {}

    @Screen(name = "OrderReportPurchasesByOrganization", location = "component://order/widget/ordermgr/ReportScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReportPurchasesByOrganization}", includeForms = {@IncludeForm(name = "PurchasesByOrganizationReport", location = "component://order/widget/ordermgr/ReportForms.xml")})}))
    public interface OrderReportPurchasesByOrganization {}

    @Screen(name = "OrderReportPurchasesByProduct", location = "component://order/widget/ordermgr/ReportScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReportPurchasesByProduct}", includeForms = {@IncludeForm(name = "OrderPurchaseProductOptions", location = "component://order/widget/ordermgr/ReportForms.xml")})}))
    public interface OrderReportPurchasesByProduct {}

    @Screen(name = "OrderReportPurchasesByPaymentMethod", location = "component://order/widget/ordermgr/ReportScreens.xml")
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Section(widgets = @Widgets(screenlets = {@Screenlet(title = "${uiLabelMap.OrderReportPurchasesByPaymentMethod}", includeForms = {@IncludeForm(name = "OrderPurchasePaymentOptions", location = "component://order/widget/ordermgr/ReportForms.xml")})}))
    public interface OrderReportPurchasesByPaymentMethod {}

    @Screen(name = "OrderPurchaseReportPayment", location = "component://order/widget/ordermgr/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderReportPurchasesByPaymentMethod")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "AccountingUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "OrderPurchasePaymentSummary", list = "orderPurchasePaymentSummaryList", conditions = {@ConditionExpr(fieldName = "productStoreId", operator = "equals", fromField = "parameters.productStoreId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "originFacilityId", operator = "equals", fromField = "parameters.originFacilityId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "terminalId", operator = "equals", fromField = "parameters.terminalId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "statusId", operator = "equals", fromField = "parameters.statusId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "orderDate", operator = "greater-equals", fromField = "parameters.fromOrderDate", ignoreIfEmpty = true), @ConditionExpr(fieldName = "orderDate", operator = "less", fromField = "parameters.thruOrderDate", ignoreIfEmpty = true)}, orderBy = {"productStoreId", "originFacilityId", "terminalId", "paymentMethodTypeId"}, selectFields = {"productStoreId", "originFacilityId", "terminalId", "statusId", "paymentMethodTypeId", "description", "maxAmount"})
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://order/webapp/ordermgr/reports/OrderPurchaseReportPayment.fo.ftl", platform = "xsl-fo")}))
    public interface OrderPurchaseReportPayment {}

    @Screen(name = "OrderPurchaseReportProduct", location = "component://order/widget/ordermgr/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderReportPurchasesByProduct")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "OrderPurchaseProductSummary", list = "orderPurchaseProductSummaryList", conditions = {@ConditionExpr(fieldName = "productStoreId", operator = "equals", fromField = "parameters.productStoreId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "orderTypeId", operator = "equals", fromField = "parameters.orderTypeId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "originFacilityId", operator = "equals", fromField = "parameters.originFacilityId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "terminalId", operator = "equals", fromField = "parameters.terminalId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "statusId", operator = "equals", fromField = "parameters.statusId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "orderDate", operator = "greater-equals", fromField = "parameters.fromOrderDate", ignoreIfEmpty = true), @ConditionExpr(fieldName = "orderDate", operator = "less", fromField = "parameters.thruOrderDate", ignoreIfEmpty = true)}, orderBy = {"productStoreId", "originFacilityId", "terminalId", "productId"}, selectFields = {"productStoreId", "originFacilityId", "terminalId", "statusId", "productId", "internalName", "quantity", "cancelQuantity"})
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://order/webapp/ordermgr/reports/OrderPurchaseReportProduct.fo.ftl", platform = "xsl-fo")}))
    public interface OrderPurchaseReportProduct {}

    @Screen(name = "SalesByStoreReport", location = "component://order/widget/ordermgr/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderReportSalesByStore")
    @Action(type = ActionType.SET, field = "toPartyId", fromField = "parameters.toPartyId")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "OrderReportSalesGroupByProduct", list = "productReportList", conditions = {@ConditionExpr(fieldName = "productStoreId", operator = "equals", fromField = "parameters.productStoreId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "partyId", operator = "equals", fromField = "parameters.toPartyId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "roleTypeId", operator = "equals", value = "BILL_TO_CUSTOMER"), @ConditionExpr(fieldName = "orderTypeId", operator = "equals", value = "SALES_ORDER"), @ConditionExpr(fieldName = "orderStatusId", operator = "equals", value = "${parameters.orderStatusId}", ignoreIfEmpty = true), @ConditionExpr(fieldName = "orderDate", operator = "greater-equals", fromField = "parameters.fromOrderDate", ignoreIfEmpty = true), @ConditionExpr(fieldName = "orderDate", operator = "less", fromField = "parameters.thruOrderDate", ignoreIfEmpty = true)}, orderBy = {"storeName", "internalName"}, selectFields = {"productStoreId", "storeName", "productId", "internalName", "quantityOrdered", "unitPrice"})
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://order/webapp/ordermgr/reports/SalesByStoreReport.fo.ftl", platform = "xsl-fo")}))
    public interface SalesByStoreReport {}

    @Screen(name = "OpenOrderItemsReport", location = "component://order/widget/ordermgr/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderReportOpenOrderItems")
    @Action(type = ActionType.SET, field = "viewSize", value = "${parameters.VIEW_SIZE}", valueType = "Integer", defaultValue = "20")
    @Action(type = ActionType.SET, field = "viewIndex", value = "${parameters.VIEW_INDEX}", valueType = "Integer", defaultValue = "0")
    @Action(type = ActionType.SET, field = "fromOrderDate", fromField = "parameters.fromOrderDate", valueType = "Timestamp")
    @Action(type = ActionType.SET, field = "thruOrderDate", fromField = "parameters.thruOrderDate", valueType = "Timestamp")
    @Action(type = ActionType.SCRIPT, location = "component://order/webapp/ordermgr/WEB-INF/actions/reports/OpenOrderItemsReport.groovy")
    @DecoratorScreen(
        name = "CommonReportDecorator",
        location = "${parameters.mainDecoratorLocation}",
        sections = {
            @DecoratorSection(name = "body", screenlets = {
                @Screenlet(title = "${uiLabelMap.OrderReportOpenOrderItems} - ${productStore.storeName}", includeForms = {
                    @IncludeForm(name = "OpenOrderItemsList", location = "component://order/widget/ordermgr/ReportForms.xml"
                )}),
                @Screenlet(title = "${uiLabelMap.OrderReportOpenOrderItems}", includeForms = {
                    @IncludeForm(name = "OpenOrderItemsTotal", location = "component://order/widget/ordermgr/ReportForms.xml"
                )})})
        }
    )
    public interface OpenOrderItemsReport {}

    @Screen(name = "PurchasesByOrganizationReport", location = "component://order/widget/ordermgr/ReportScreens.xml")
    @Action(type = ActionType.SET, field = "titleProperty", value = "OrderReportPurchasesByOrganization")
    @Action(type = ActionType.SET, field = "toPartyId", fromField = "parameters.toPartyId")
    @Action(type = ActionType.SET, field = "fromPartyId", fromField = "parameters.fromPartyId")
    @Action(type = ActionType.PROPERTY_MAP, resource = "OrderUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "ProductUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "PartyUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.PROPERTY_MAP, resource = "CommonUiLabels", mapName = "uiLabelMap", global = true)
    @Action(type = ActionType.ENTITY_CONDITION, entityName = "OrderReportPurchasesGroupByProduct", list = "productReportList", conditions = {@ConditionExpr(fieldName = "toPartyId", operator = "equals", fromField = "parameters.toPartyId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "toRoleTypeId", operator = "equals", value = "BILL_TO_CUSTOMER"), @ConditionExpr(fieldName = "fromPartyId", operator = "equals", fromField = "parameters.fromPartyId", ignoreIfEmpty = true), @ConditionExpr(fieldName = "fromRoleTypeId", operator = "equals", value = "BILL_FROM_VENDOR"), @ConditionExpr(fieldName = "orderTypeId", operator = "equals", value = "PURCHASE_ORDER"), @ConditionExpr(fieldName = "orderStatusId", operator = "equals", value = "${parameters.orderStatusId}", ignoreIfEmpty = true), @ConditionExpr(fieldName = "orderDate", operator = "greater-equals", fromField = "parameters.fromOrderDate", ignoreIfEmpty = true), @ConditionExpr(fieldName = "orderDate", operator = "less", fromField = "parameters.thruOrderDate", ignoreIfEmpty = true)}, orderBy = {"internalName"}, selectFields = {"productId", "internalName", "quantity", "unitPrice"})
    @Section(widgets = @Widgets(htmlTemplates = {@HtmlTemplate(location = "component://order/webapp/ordermgr/reports/PurchasesByOrganizationReport.fo.ftl", platform = "xsl-fo")}))
    public interface PurchasesByOrganizationReport {}

}

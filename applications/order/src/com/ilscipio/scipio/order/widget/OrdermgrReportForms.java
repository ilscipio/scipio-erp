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

import com.ilscipio.scipio.widget.def.form.*;
import com.ilscipio.scipio.widget.def.screen.SetAction;
import com.ilscipio.scipio.widget.def.screen.ServiceAction;
import com.ilscipio.scipio.widget.def.screen.FieldMap;
import com.ilscipio.scipio.widget.def.screen.EntityOneAction;
import com.ilscipio.scipio.widget.def.screen.EntityConditionAction;
import com.ilscipio.scipio.widget.def.screen.ScriptAction;
import com.ilscipio.scipio.widget.def.screen.PropertyToFieldAction;

/**
 * Auto-generated annotation-based widget definitions.
 *
 * <p>Generated from XML by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OrdermgrReportForms {

    @Form(
        name = "OrderPurchaseReportOptions",
        location = "component://order/widget/ordermgr/ReportForms.xml",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productStoreId", dropDown = @DropDownField(options = {@Option(description = "- ${uiLabelMap.CommonAny} -")}, entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName} [${productStoreId}]", orderBy = {@EntityOrderBy(fieldName = "storeName")}))),
            @FormField(name = "orderTypeId", dropDown = @DropDownField(options = {@Option(description = "- ${uiLabelMap.CommonAny} -")}, entityOptions = @EntityOptions(entityName = "OrderType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "orderTypeId")}))),
            @FormField(name = "originFacilityId", dropDown = @DropDownField(options = {@Option(description = "- ${uiLabelMap.CommonAny} -")}, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "terminalId", text = @TextField(size = 10, maxlength = 20)),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(options = {@Option(description = "- ${uiLabelMap.CommonAny} -")}, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")})))
        }
    )
    public interface OrderPurchaseReportOptions {}

    @Form(
        name = "OrderPurchaseProductOptions",
        location = "component://order/widget/ordermgr/ReportForms.xml",
        target = "OrderPurchaseReportProduct.pdf",
        targetWindow = "_BLANK",
        extendsForm = "OrderPurchaseReportOptions",
        fields = {
            @FormField(name = "fromOrderDate", title = "${uiLabelMap.OrderReportFromDate}", dateTime = @DateTimeField),
            @FormField(name = "thruOrderDate", title = "${uiLabelMap.OrderReportThruDate}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonRun}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", submit = @SubmitField)
        }
    )
    public interface OrderPurchaseProductOptions {}

    @Form(
        name = "OrderPurchasePaymentOptions",
        location = "component://order/widget/ordermgr/ReportForms.xml",
        target = "OrderPurchaseReportPayment.pdf",
        targetWindow = "_BLANK",
        extendsForm = "OrderPurchaseReportOptions",
        fields = {
            @FormField(name = "fromOrderDate", title = "${uiLabelMap.OrderReportFromDate}", dateTime = @DateTimeField),
            @FormField(name = "thruOrderDate", title = "${uiLabelMap.OrderReportThruDate}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonRun}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", submit = @SubmitField)
        }
    )
    public interface OrderPurchasePaymentOptions {}

    @Form(
        name = "SalesByStoreReport",
        location = "component://order/widget/ordermgr/ReportForms.xml",
        target = "SalesByStoreReport.pdf",
        targetWindow = "_BLANK",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productStoreId", dropDown = @DropDownField(options = {@Option(description = "- ${uiLabelMap.CommonSelectAny} -")}, entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName} [${productStoreId}]", orderBy = {@EntityOrderBy(fieldName = "storeName")}))),
            @FormField(name = "toPartyId", title = "${uiLabelMap.AccountingToPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "orderStatusId", dropDown = @DropDownField(options = {@Option(description = "- ${uiLabelMap.CommonSelectAny} -")}, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_STATUS")}))),
            @FormField(name = "fromOrderDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruOrderDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonRun}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", submit = @SubmitField)
        }
    )
    public interface SalesByStoreReport {}

    @Form(
        name = "OpenOrderItemsReport",
        location = "component://order/widget/ordermgr/ReportForms.xml",
        target = "OpenOrderItemsReport",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productStoreId", dropDown = @DropDownField(options = {@Option(description = "- ${uiLabelMap.CommonSelectAny} -")}, entityOptions = @EntityOptions(entityName = "ProductStore", description = "${storeName} [${productStoreId}]", orderBy = {@EntityOrderBy(fieldName = "storeName")}))),
            @FormField(name = "orderTypeId", dropDown = @DropDownField(options = {@Option(key = "SALES_ORDER", description = "${uiLabelMap.OrderSalesOrder}"), @Option(key = "PURCHASE_ORDER", description = "${uiLabelMap.OrderPurchaseOrder}")})),
            @FormField(name = "orderStatusId", dropDown = @DropDownField(options = {@Option(description = "- ${uiLabelMap.CommonSelectAny} -")}, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_STATUS")}))),
            @FormField(name = "fromOrderDate", title = "${uiLabelMap.OrderReportFromDate}", dateTime = @DateTimeField),
            @FormField(name = "thruOrderDate", title = "${uiLabelMap.OrderReportThruDate}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonRun}", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", submit = @SubmitField)
        }
    )
    public interface OpenOrderItemsReport {}

    @Form(
        name = "OpenOrderItemsList",
        location = "component://order/widget/ordermgr/ReportForms.xml",
        type = FormType.LIST,
        listName = "orderItemList",
        paginateTarget = "OpenOrderItemsReport",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "orderDate", title = "${uiLabelMap.OrderDate}", display = @DisplayField),
            @FormField(name = "orderId", title = "${uiLabelMap.OrderOrderId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "orderview", description = "${orderId}", parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProduct}", display = @DisplayField),
            @FormField(name = "itemDescription", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "quantityOrdered", title = "${uiLabelMap.ProductQuantity}", display = @DisplayField),
            @FormField(name = "quantityIssued", title = "${uiLabelMap.OrderQtyShipped}", display = @DisplayField),
            @FormField(name = "quantityOpen", title = "${uiLabelMap.ProductOpenQuantity}", display = @DisplayField),
            @FormField(name = "shipAfterDate", title = "${uiLabelMap.OrderShipAfterDate}", display = @DisplayField),
            @FormField(name = "shipBeforeDate", title = "${uiLabelMap.OrderShipBeforeDate}", display = @DisplayField),
            @FormField(name = "comments", title = "${uiLabelMap.CommonComments}", display = @DisplayField),
            @FormField(name = "costPrice", title = "${uiLabelMap.ProductCostPrice}", display = @DisplayField),
            @FormField(name = "listPrice", title = "${uiLabelMap.ProductListPrice}", display = @DisplayField),
            @FormField(name = "retailPrice", title = "${uiLabelMap.ProductRetailPrice}", display = @DisplayField),
            @FormField(name = "discount", title = "${uiLabelMap.ProductDiscount}", display = @DisplayField),
            @FormField(name = "calculatedMarkup", title = "${uiLabelMap.OrderCalculatedMarkup}", display = @DisplayField),
            @FormField(name = "percentMarkup", title = "${uiLabelMap.OrderPercentageMarkup}", display = @DisplayField)
        }
    )
    public interface OpenOrderItemsList {}

    @Form(
        name = "OpenOrderItemsTotal",
        location = "component://order/widget/ordermgr/ReportForms.xml",
        type = FormType.LIST,
        listName = "totalAmountList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "total", title = "${uiLabelMap.CommonTotal}", display = @DisplayField),
            @FormField(name = "totalQuantityOrdered", display = @DisplayField),
            @FormField(name = "totalQuantityOpen", display = @DisplayField),
            @FormField(name = "totalCostPrice", display = @DisplayField),
            @FormField(name = "totalListPrice", display = @DisplayField),
            @FormField(name = "totalRetailPrice", display = @DisplayField),
            @FormField(name = "totalDiscount", display = @DisplayField),
            @FormField(name = "totalMarkup", display = @DisplayField),
            @FormField(name = "totalPercentMarkup", display = @DisplayField)
        }
    )
    public interface OpenOrderItemsTotal {}

    @Form(
        name = "PurchasesByOrganizationReport",
        location = "component://order/widget/ordermgr/ReportForms.xml",
        target = "PurchasesByOrganizationReport.pdf",
        targetWindow = "_BLANK",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fromPartyId", title = "${uiLabelMap.AccountingFromParty}", dropDown = @DropDownField(options = {@Option(description = "- ${uiLabelMap.CommonSelectAny} -")}, entityOptions = @EntityOptions(entityName = "PartyRoleAndPartyDetail", description = "${groupName} ${firstName} ${lastName} [${partyId}]", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "SUPPLIER")}))),
            @FormField(name = "toPartyId", title = "${uiLabelMap.AccountingToPartyId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyAcctgPrefAndGroup", description = "${groupName} ${firstName} ${lastName} [${partyId}]", keyFieldName = "partyId"))),
            @FormField(name = "orderStatusId", dropDown = @DropDownField(options = {@Option(description = "- ${uiLabelMap.CommonSelectAny} -")}, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_STATUS")}))),
            @FormField(name = "fromOrderDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruOrderDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonRun}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", submit = @SubmitField)
        }
    )
    public interface PurchasesByOrganizationReport {}

}

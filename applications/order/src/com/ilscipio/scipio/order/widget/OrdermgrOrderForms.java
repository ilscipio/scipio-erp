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
public class OrdermgrOrderForms {

    @Form(
        name = "EditOrderHeader",
        location = "component://order/widget/ordermgr/OrderForms.xml",
        target = "updateOrderHeader",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "OrderHeader")
        },
        fields = {
            @FormField(name = "orderId", useWhen = "orderHeader!=null", display = @DisplayField),
            @FormField(name = "orderId", useWhen = "orderHeader==null", ignored = @IgnoredField),
            @FormField(name = "orderTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "OrderType", description = "${description}", keyFieldName = "orderTypeId"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "orderHeader==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_STATUS")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "orderHeader!=null", dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", value = "${orderHeader.statusId}")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "salesChannelEnumId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "ORDER_SALES_CHANNEL")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "currencyUom", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "firstAttemptOrderId", lookup = @LookupField(targetFormName = "/ordermgr/control/LookupOrderHeader")),
            @FormField(name = "productStoreId", lookup = @LookupField(targetFormName = "/marketing/control/LookupProductStore")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "orderHeader==null", target = "createOrderHeader")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "StatusItem", valueField = "currentStatus", autoFieldMap = false)})
    )
    public interface EditOrderHeader {}

    @Form(
        name = "ListOrderHeaders",
        location = "component://order/widget/ordermgr/OrderForms.xml",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "OrderHeader", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditOrderHeader", description = "[${orderId}]", parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "productStoreId", displayEntity = @DisplayEntityField(entityName = "ProductStore"))
        }
    )
    public interface ListOrderHeaders {}

    @Form(
        name = "ListOrderTerms",
        location = "component://order/widget/ordermgr/OrderForms.xml",
        type = FormType.LIST,
        target = "updateOrderTerm",
        listName = "orderTerms",
        paginateTarget = "ListOrderTerms",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "OrderTerm", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "termTypeId", displayEntity = @DisplayEntityField(entityName = "TermType")),
            @FormField(name = "orderId", hidden = @HiddenField),
            @FormField(name = "orderItemSeqId", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeOrderTerm", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "orderId"), @ParameterDef(paramName = "termTypeId"), @ParameterDef(paramName = "orderItemSeqId")}))
        }
    )
    public interface ListOrderTerms {}

    @Form(
        name = "AddOrderTerm",
        location = "component://order/widget/ordermgr/OrderForms.xml",
        target = "createOrderTerm",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "OrderTerm", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "orderId", hidden = @HiddenField(value = "${parameters.orderId}")),
            @FormField(name = "termTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TermType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "addAction", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddOrderTerm {}

    @Form(
        name = "LookupBulkAddSupplierProductsInApprovedOrder",
        location = "component://order/widget/ordermgr/OrderForms.xml",
        type = FormType.MULTI,
        target = "bulkAddProductsInApprovedOrder",
        listName = "productList",
        paginateTarget = "LookupBulkAddSupplierProductsInApprovedOrder",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        viewSize = 10,
        fields = {
            @FormField(name = "orderId", hidden = @HiddenField),
            @FormField(name = "shipGroupSeqId", hidden = @HiddenField),
            @FormField(name = "productId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/EditProductInventoryItems", urlMode = UrlMode.INTER_APP, description = "${productId}", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "supplierProductId", display = @DisplayField),
            @FormField(name = "supplierProductName", display = @DisplayField),
            @FormField(name = "lastPrice", display = @DisplayField),
            @FormField(name = "quantity", title = "${uiLabelMap.OrderQuantity}", text = @TextField(size = 5, maxlength = 10)),
            @FormField(name = "itemDesiredDeliveryDate", title = "${uiLabelMap.OrderDesiredDeliveryDate}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.OrderAddToOrder}", widgetStyle = "${styles.link_run_sys} ${styles.action_select}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "orderId", fromField = "parameters.orderId")})
    )
    public interface LookupBulkAddSupplierProductsInApprovedOrder {}

    @Form(
        name = "OrderShipmentMethodHistory",
        location = "component://order/widget/ordermgr/OrderForms.xml",
        type = FormType.LIST,
        listName = "orderShipmentHistories",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "shipmentMethod", title = "${uiLabelMap.ProductShipmentMethod}", display = @DisplayField),
            @FormField(name = "changedDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField),
            @FormField(name = "changedByUser", title = "${uiLabelMap.OrderChangedByUser}", display = @DisplayField)
        }
    )
    public interface OrderShipmentMethodHistory {}

    @Form(
        name = "OrderUnitPriceHistory",
        location = "component://order/widget/ordermgr/OrderForms.xml",
        type = FormType.LIST,
        listName = "orderUnitPriceHistories",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "oldValue", display = @DisplayField(type = "currency")),
            @FormField(name = "newValue", display = @DisplayField(type = "currency")),
            @FormField(name = "changedDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField),
            @FormField(name = "changedByUser", title = "${uiLabelMap.OrderChangedByUser}", display = @DisplayField)
        }
    )
    public interface OrderUnitPriceHistory {}

    @Form(
        name = "OrderQuantityHistory",
        location = "component://order/widget/ordermgr/OrderForms.xml",
        type = FormType.LIST,
        listName = "orderQuantityHistories",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "oldValue", display = @DisplayField),
            @FormField(name = "newValue", display = @DisplayField),
            @FormField(name = "changedDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField),
            @FormField(name = "changedByUser", title = "${uiLabelMap.OrderChangedByUser}", display = @DisplayField)
        }
    )
    public interface OrderQuantityHistory {}

    @Form(
        name = "EditOrderByCustomer",
        location = "component://order/widget/ordermgr/OrderForms.xml",
        extendsForm = "CommonPortletEdit",
        extendsResource = "component://common/widget/PortletEditForms.xml",
        fields = {
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(options = {@Option(key = "ORDER_CREATED", description = "${uiLabelMap.CommonCreated}"), @Option(key = "ORDER_PROCESSING", description = "${uiLabelMap.CommonProcessing}"), @Option(key = "ORDER_APPROVED", description = "${uiLabelMap.CommonApproved}"), @Option(key = "ORDER_SENT", description = "${uiLabelMap.CommonSent}"), @Option(key = "ORDER_HELD", description = "${uiLabelMap.CommonHeld}"), @Option(key = "ORDER_COMPLETED", description = "${uiLabelMap.CommonCompleted}"), @Option(key = "ORDER_REJECTED", description = "${uiLabelMap.CommonRejected}"), @Option(key = "ORDER_CANDELLED", description = "${uiLabelMap.CommonCancelled}")})),
            @FormField(name = "roleTypeId", dropDown = @DropDownField(options = {@Option(key = "PLACING_CUSTOMER", description = "${uiLabelMap.MyPortalPlacingCustomer}"), @Option(key = "SHIP_TO_CUSTOMER", description = "${uiLabelMap.MyPortalShipToCustomer}"), @Option(key = "END_USER_CUSTOMER", description = "${uiLabelMap.MyPortalEndUserCustomer}"), @Option(key = "BILL_TO_CUSTOMER", description = "${uiLabelMap.MyPortalBillToCustomer}")})),
            @FormField(name = "updateAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditOrderByCustomer {}

    @Form(
        name = "ListCustomerOrders",
        location = "component://order/widget/ordermgr/OrderForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "orderTypeId", title = "${uiLabelMap.FormFieldTitle_orderTypeId}", display = @DisplayField),
            @FormField(name = "orderId", title = "${uiLabelMap.OrderOrderId}", display = @DisplayField),
            @FormField(name = "orderName", title = "${uiLabelMap.OrderOrderName}", display = @DisplayField),
            @FormField(name = "remainingSubTotal", title = "${uiLabelMap.FormFieldTitle_remainingSubTotal}", display = @DisplayField),
            @FormField(name = "grandTotal", title = "${uiLabelMap.OrderGrandTotal}", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", display = @DisplayField),
            @FormField(name = "orderDate", title = "${uiLabelMap.OrderOrderDate}", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", display = @DisplayField),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.FormFieldTitle_roleTypeId}", display = @DisplayField)
        }
    )
    public interface ListCustomerOrders {}

    @Form(
        name = "ListPurchaseOrders",
        location = "component://order/widget/ordermgr/OrderForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "orderTypeId", title = "${uiLabelMap.FormFieldTitle_orderTypeId}", displayEntity = @DisplayEntityField(entityName = "OrderType", description = "${description}")),
            @FormField(name = "orderId", title = "${uiLabelMap.OrderOrderId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "orderview", description = "${orderId}", parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "orderName", title = "${uiLabelMap.OrderOrderName}", display = @DisplayField),
            @FormField(name = "remainingSubTotal", title = "${uiLabelMap.FormFieldTitle_remainingSubTotal}", display = @DisplayField),
            @FormField(name = "grandTotal", title = "${uiLabelMap.OrderGrandTotal}", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "orderDate", title = "${uiLabelMap.OrderOrderDate}", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${groupName}")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.FormFieldTitle_roleTypeId}", displayEntity = @DisplayEntityField(entityName = "RoleType", description = "${description}"))
        }
    )
    public interface ListPurchaseOrders {}

    @Form(
        name = "OrderNewNote",
        location = "component://order/widget/ordermgr/OrderForms.xml",
        target = "createordernote",
        fields = {
            @FormField(name = "orderId", hidden = @HiddenField),
            @FormField(name = "note", title = "${uiLabelMap.OrderNote}", textarea = @TextareaField(cols = 70, rows = 5)),
            @FormField(name = "internalNote", title = "${uiLabelMap.OrderInternalNote}", tooltip = "${uiLabelMap.OrderInternalNoteMessage}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface OrderNewNote {}

}

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
package com.ilscipio.scipio.product.widget;

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
public class CatalogProductStoreForms {

    @Form(
        name = "EditProductStore",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        target = "updateProductStore",
        defaultMapName = "productStore",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductStore")
        },
        fields = {
            @FormField(name = "productStoreId", useWhen = "productStore!=null", display = @DisplayField),
            @FormField(name = "storeName", title = "${uiLabelMap.ProductStoreName}", position = 2, requiredField = true, text = @TextField(size = 30, maxlength = 100)),
            @FormField(name = "primaryStoreGroupId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductStoreGroup", description = "${productStoreGroupName} [${productStoreGroupId}]", keyFieldName = "productStoreGroupId", orderBy = {@EntityOrderBy(fieldName = "productStoreGroupName")}))),
            @FormField(name = "isCreate", useWhen = "productStore==null", hidden = @HiddenField(value = "true")),
            @FormField(name = "title", title = "${uiLabelMap.ProductTitle}", text = @TextField(size = 30, maxlength = 100)),
            @FormField(name = "subtitle", title = "${uiLabelMap.ProductSubTitle}", text = @TextField(size = 60)),
            @FormField(name = "payToPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "oneInventoryFacility", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "inventoryFacilityId", position = 2, requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "inventoryFacilityAction", title = " ", useWhen = "productStore!=null&&productStore.getString(\"inventoryFacilityId\")!=null", widgetStyle = "${styles.link_nav} ${styles.action_update}", position = 2, hyperlink = @HyperlinkField(target = "/facility/control/EditFacility", urlMode = UrlMode.INTER_APP, description = "${uiLabelMap.CommonEdit} ${uiLabelMap.ProductFacility} ${productStore.inventoryFacilityId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "facilityId", fromField = "productStore.inventoryFacilityId")})),
            @FormField(name = "manualAuthIsCapture", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "prorateShipping", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "prorateTaxes", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "viewCartOnAdd", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "autoSaveCart", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "autoApproveReviews", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "reviewsPurchased", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "multipleReviews", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "autoInvoiceDigitalItems", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "reqShipAddrForDigItems", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "saveAbandonedCart", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "sendAbandonedCartReminder", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "maxAbandonedCartReminderRetry", position = 2, text = @TextField),
            @FormField(name = "abandonedCartReminderDayOffset", position = 2, text = @TextField),
            @FormField(name = "isDemoStore", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "isImmediatelyFulfilled", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "checkInventory", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "requireInventory", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "reserveInventory", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "reserveOrderEnumId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "INV_RES_ORDER")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "balanceResOnOrderCreation", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "showOutOfStockProducts", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "useVariantStockCalc", position = 2, dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "showDiscontinuedProducts", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "requirementMethodEnumId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PROD_REQ_METHOD")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "addToCartReplaceUpsell", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "addToCartRemoveIncompat", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "defaultCurrencyUomId", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "isContentReference", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "defaultPriority", text = @TextField),
            @FormField(name = "defaultSalesChannelEnumId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "ORDER_SALES_CHANNEL")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "allowPassword", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "retryFailedAuths", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "headerApprovedStatus", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description} [${statusCode}]", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "headerCancelStatus", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description} [${statusCode}]", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "headerDeclinedStatus", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description} [${statusCode}]", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "itemApprovedStatus", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description} [${statusCode}]", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_ITEM_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "itemCancelStatus", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description} [${statusCode}]", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_ITEM_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "itemDeclinedStatus", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description} [${statusCode}]", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_ITEM_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "digitalItemApprovedStatus", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description} [${statusCode}]", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_ITEM_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "visualThemeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "VisualTheme", description = "${visualThemeId} - ${description}", keyFieldName = "visualThemeId", constraints = {@EntityConstraint(name = "visualThemeSetId", value = "ECOMMERCE")}))),
            @FormField(name = "storeCreditAccountEnumId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "STR_CRDT_ACT")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "oldStyleSheet", hidden = @HiddenField),
            @FormField(name = "managedByLot", title = "${uiLabelMap.ProductManagedByLot}", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "oldHeaderLogo", hidden = @HiddenField),
            @FormField(name = "oldHeaderMiddleBackground", hidden = @HiddenField),
            @FormField(name = "oldHeaderRightBackground", hidden = @HiddenField),
            @FormField(name = "explodeOrderItems", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "checkGcBalance", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "usePrimaryEmailUsername", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "requireCustomerRole", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "showCheckoutGiftOptions", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "selectPaymentTypePerItem", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "showPricesWithVatTax", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "showTaxIsExempt", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "vatTaxAuthGeoId", lookup = @LookupField(targetFormName = "LookupGeo")),
            @FormField(name = "vatTaxAuthPartyId", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "prodSearchExcludeVariants", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "enableDigProdUpload", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "digProdUploadCategoryId", lookup = @LookupField(targetFormName = "LookupProductCategory")),
            @FormField(name = "enableAutoSuggestionList", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "autoOrderCcTryExp", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "autoOrderCcTryOtherCards", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "autoOrderCcTryLaterNsf", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "autoApproveOrder", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "autoApproveInvoice", position = 2, dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "shipIfCaptureFails", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "setOwnerUponIssuance", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "reqPayMethForFreeOrders", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "reqReturnInventoryReceive", position = 2, dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "orderDecimalQuantity", tooltip = "${uiLabelMap.ProductOrderDecimalQuantityExistsToOverride}", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "reqReturnInventoryReceive", position = 2, dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productStore==null", target = "createProductStore")
        },
        sortOrder = @SortOrder()
    )
    public interface EditProductStore {}

    @Form(
        name = "ListProductStoreFinAccountSettings",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        type = FormType.LIST,
        target = "UpdateProductStoreFinAccountSettings",
        listName = "productStoreFinActSettings",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductStoreFinActSetting", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productStoreId", ignored = @IgnoredField),
            @FormField(name = "finAccountTypeId", displayEntity = @DisplayEntityField(entityName = "FinAccountType", keyFieldName = "finAccountTypeId", description = "${description}")),
            @FormField(name = "replenishMethodEnumId", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}")),
            @FormField(name = "removeAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "RemoveProductStoreFinAccountSettings", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "finAccountTypeId")})),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditProductStoreFinAccountSettings", description = "${uiLabelMap.CommonEdit}", parameters = {@ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "finAccountTypeId")}))
        }
    )
    public interface ListProductStoreFinAccountSettings {}

    @Form(
        name = "EditProductStoreFinAccountSettings",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        target = "UpdateProductStoreFinAccountSettings",
        defaultMapName = "finAccountSetting",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductStoreFinActSetting")
        },
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "finAccountTypeId", useWhen = "finAccountSetting!=null", displayEntity = @DisplayEntityField(entityName = "FinAccountType", keyFieldName = "finAccountTypeId", description = "${description}")),
            @FormField(name = "finAccountTypeId", useWhen = "finAccountSetting==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "FinAccountType", description = "${description}", keyFieldName = "finAccountTypeId"))),
            @FormField(name = "replenishMethodEnumId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "FARP_METHOD")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "spacer", title = " ", display = @DisplayField),
            @FormField(name = "submit", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "finAccountSetting==null", target = "CreateProductStoreFinAccountSettings")
        }
    )
    public interface EditProductStoreFinAccountSettings {}

    @Form(
        name = "CreateProductStoreCatalog",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        target = "createProductStoreCatalog",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductStoreCatalog")
        },
        fields = {
            @FormField(name = "productStoreId", mapName = "productStore", hidden = @HiddenField),
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.ProductCatalog}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProdCatalog", description = "${catalogName}", orderBy = {@EntityOrderBy(fieldName = "catalogName")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateProductStoreCatalog {}

    @Form(
        name = "UpdateProductStoreCatalog",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        type = FormType.LIST,
        target = "updateProductStoreCatalog",
        listName = "productStoreCatalogs",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductStoreCatalog")
        },
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "prodCatalogId", title = "${uiLabelMap.ProductCatalogId}", displayEntity = @DisplayEntityField(entityName = "ProdCatalog", description = "${catalogName}", subHyperlink = @SubHyperlink(target = "EditProdCatalog", description = "${prodCatalogId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "prodCatalogId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductStoreCatalog", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "prodCatalogId"), @ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateProductStoreCatalog {}

    @Form(
        name = "createProductStoreEmail",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        target = "createProductStoreEmail",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductStoreEmailSetting")
        },
        fields = {
            @FormField(name = "productStoreId", mapName = "productStore", hidden = @HiddenField),
            @FormField(name = "emailType", title = "${uiLabelMap.CommonEmailType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PRDS_EMAIL,PARTY_EMAIL", operator = "in")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "bodyScreenLocation", title = "${uiLabelMap.ProductBodyScreenLocation}"),
            @FormField(name = "xslfoAttachScreenLocation", title = "${uiLabelMap.ProductAttachmentScreenLocation}"),
            @FormField(name = "subject", title = "${uiLabelMap.ProductSubject}"),
            @FormField(name = "fromAddress", title = "${uiLabelMap.CommonFromAddress}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface createProductStoreEmail {}

    @Form(
        name = "updateProductStoreEmail",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        type = FormType.LIST,
        target = "updateProductStoreEmail",
        listName = "productStoreEmailSettings",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductStoreEmailSetting")
        },
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "emailType", title = "${uiLabelMap.CommonEmailType}", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}", cache = true)),
            @FormField(name = "sendAs", title = "${uiLabelMap.ProductEmailSendAs}"),
            @FormField(name = "bodyScreenLocation", title = "${uiLabelMap.ProductBodyScreenLocation}"),
            @FormField(name = "xslfoAttachScreenLocation", title = "${uiLabelMap.ProductAttachmentScreenLocation}"),
            @FormField(name = "subject", title = "${uiLabelMap.ProductSubject}"),
            @FormField(name = "fromAddress", title = "${uiLabelMap.CommonFromAddress}"),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeProductStoreEmail", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "emailType")}))
        }
    )
    public interface updateProductStoreEmail {}

    @Form(
        name = "CreateproductStorekeywordOvrdForm",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        target = "createProductStoreKeywordOvrd",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createProductStoreKeywordOvrd")
        },
        fields = {
            @FormField(name = "productStoreId", mapName = "productStore", hidden = @HiddenField),
            @FormField(name = "targetTypeEnumId", title = "${uiLabelMap.ProductTargetTypeEnumId}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "KWOVRD_TRGT_TYPE")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateproductStorekeywordOvrdForm {}

    @Form(
        name = "UpdateproductStorekeywordOvrdForm",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        type = FormType.LIST,
        target = "updateProductStoreKeywordOvrd",
        listName = "productStorekeywordOvrdList",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateProductStoreKeywordOvrd")
        },
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "keyword", title = "${uiLabelMap.ProductKeyword}", display = @DisplayField(description = "${keyword}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(description = "${fromDate}")),
            @FormField(name = "targetTypeEnumId", title = "${uiLabelMap.ProductTargetType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "KWOVRD_TRGT_TYPE")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteProductStoreKeywordOvrd", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "keyword"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface UpdateproductStorekeywordOvrdForm {}

    @Form(
        name = "ViewProductStoreSegments",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "segmentGroupId", title = "${uiLabelMap.ProductSegmentGroupId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/marketing/control/viewSegmentGroup", urlMode = UrlMode.INTER_APP, description = "${segmentGroupId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "segmentGroupId")})),
            @FormField(name = "segmentGroupTypeId", title = "${uiLabelMap.ProductSegmentGroupTypeId}", displayEntity = @DisplayEntityField(entityName = "SegmentGroupType")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "/marketing/control/deleteSegmentGroup", urlMode = UrlMode.INTER_APP, description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "segmentGroupId")}))
        }
    )
    public interface ViewProductStoreSegments {}

    @Form(
        name = "ListProductStoreShipmentMeths",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        type = FormType.LIST,
        listName = "storeShipMethods",
        paginateTarget = "EditProductStoreShipSetup",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ProductStoreShipmentMeth", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "productStoreShipMethId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductStoreShipSetup", description = "${productStoreShipMethId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "productStoreShipMethId")})),
            @FormField(name = "shipmentMethodTypeId", title = "${uiLabelMap.ProductMethod}", displayEntity = @DisplayEntityField(entityName = "ShipmentMethodType", description = "${description}", cache = true)),
            @FormField(name = "roleTypeId", hidden = @HiddenField),
            @FormField(name = "includeGeoId", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName} [${geoId}]")),
            @FormField(name = "excludeGeoId", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName} [${geoId}]")),
            @FormField(name = "serviceName", title = "${uiLabelMap.FacilityShipmentServiceName}", display = @DisplayField),
            @FormField(name = "configProps", title = "${uiLabelMap.FacilityShipmentConfigProps}", display = @DisplayField),
            @FormField(name = "shipmentCustomMethodId", title = "${uiLabelMap.FacilityShipmentCustomMethod}", displayEntity = @DisplayEntityField(entityName = "CustomMethod", keyFieldName = "customMethodId", description = "${description} (${customMethodName})")),
            @FormField(name = "shipmentGatewayConfigId", title = "${uiLabelMap.FacilityShipmentGatewayConfigId}", displayEntity = @DisplayEntityField(entityName = "ShipmentGatewayConfig", keyFieldName = "shipmentGatewayConfigId", description = "${description}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "storeRemoveShipMeth", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "productStoreShipMethId")}))
        }
    )
    public interface ListProductStoreShipmentMeths {}

    @Form(
        name = "EditProductStoreShipmentMeth",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        target = "storeUpdateShipMeth",
        defaultMapName = "productStoreShipmentMeth",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "productStoreShipMethId", useWhen = "productStoreShipmentMeth!=null", display = @DisplayField),
            @FormField(name = "shipmentMethodTypeId", displayEntity = @DisplayEntityField(entityName = "ShipmentMethodType", description = "${description}", cache = true)),
            @FormField(name = "roleTypeId", display = @DisplayField),
            @FormField(name = "partyId", display = @DisplayField),
            @FormField(name = "minSize", tooltip = "${uiLabelMap.ProductMinSizeMessage}", text = @TextField),
            @FormField(name = "maxSize", tooltip = "${uiLabelMap.ProductMaxSizeMessage}", text = @TextField),
            @FormField(name = "minWeight", tooltip = "${uiLabelMap.ProductMinWeightMessage}", text = @TextField),
            @FormField(name = "maxWeight", tooltip = "${uiLabelMap.ProductMaxWeightMessage}", text = @TextField),
            @FormField(name = "minTotal", tooltip = "${uiLabelMap.ProductMinTotalMessage}", text = @TextField),
            @FormField(name = "maxTotal", tooltip = "${uiLabelMap.ProductMaxTotalMessage}", text = @TextField),
            @FormField(name = "allowUspsAddr", tooltip = "${uiLabelMap.ProductAllowUSPSAddr}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "requireUspsAddr", tooltip = "${uiLabelMap.ProductRequireMessage}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "allowCompanyAddr", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "requireCompanyAddr", tooltip = "${uiLabelMap.ProductRequireMessage}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "companyPartyId", tooltip = "${uiLabelMap.ProductAllowMessage}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "includeNoChargeItems", title = "${uiLabelMap.ProductIncludeFreeship}", tooltip = "${uiLabelMap.ProductIncludeFreeshipMessage}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "includeGeoId", title = "${uiLabelMap.ProductIncludeGeo}", tooltip = "${uiLabelMap.ProductIncludeGeoMessage}", lookup = @LookupField(targetFormName = "LookupGeo")),
            @FormField(name = "excludeGeoId", title = "${uiLabelMap.ProductExcludeGeo}", tooltip = "${uiLabelMap.ProductExcludeGeoMessage}", lookup = @LookupField(targetFormName = "LookupGeo")),
            @FormField(name = "includeFeatureGroup", title = "${uiLabelMap.ProductIncludeFeature}", tooltip = "${uiLabelMap.ProductIncludeFeatureMessage}", text = @TextField),
            @FormField(name = "excludeFeatureGroup", title = "${uiLabelMap.ProductExcludeFeature}", tooltip = "${uiLabelMap.ProductExcludeFeatureMessage}", text = @TextField),
            @FormField(name = "serviceName", title = "${uiLabelMap.FacilityShipmentServiceName}", text = @TextField),
            @FormField(name = "configProps", title = "${uiLabelMap.FacilityShipmentConfigProps}", text = @TextField),
            @FormField(name = "shipmentCustomMethodId", title = "${uiLabelMap.FacilityShipmentCustomMethod}", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "shipmentCustomMethods", keyName = "customMethodId", description = "${description} (${customMethodName})"))),
            @FormField(name = "shipmentGatewayConfigId", title = "${uiLabelMap.FacilityShipmentGatewayConfigId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ShipmentGatewayConfig", description = "${description}", keyFieldName = "shipmentGatewayConfigId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "sequenceNumber", title = "${uiLabelMap.FormFieldTitle_sequenceNum}", tooltip = "${uiLabelMap.ProductUsedForDisplayOrdering}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productStoreShipmentMeth==null", target = "storeCreateShipMeth")
        }
    )
    public interface EditProductStoreShipmentMeth {}

    @Form(
        name = "ListShipmentCostEstimates",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        type = FormType.LIST,
        listName = "estimates",
        paginateTarget = "EditProductStoreShipmentCostEstimates",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShipmentCostEstimate", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "carrierRoleTypeId", hidden = @HiddenField),
            @FormField(name = "shipmentCostEstimateId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductStoreShipmentCostEstimates", description = "${shipmentCostEstimateId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "shipmentCostEstimateId")})),
            @FormField(name = "shipmentMethodTypeId", title = "${uiLabelMap.ProductMethod}", displayEntity = @DisplayEntityField(entityName = "ShipmentMethodType", description = "${description}", cache = true)),
            @FormField(name = "geoIdFrom", title = "${uiLabelMap.ProductFromGeo}", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName} [${geoId}]")),
            @FormField(name = "geoIdTo", title = "${uiLabelMap.ProductToGeo}", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName} [${geoId}]")),
            @FormField(name = "weightBreakId", displayEntity = @DisplayEntityField(entityName = "QuantityBreak", keyFieldName = "quantityBreakId", description = "${fromQuantity} - ${thruQuantity} [${quantityBreakId}]")),
            @FormField(name = "weightUomId", title = "${uiLabelMap.ProductUnitOfMeasure}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${description}")),
            @FormField(name = "quantityBreakId", displayEntity = @DisplayEntityField(entityName = "QuantityBreak", keyFieldName = "quantityBreakId", description = "${fromQuantity} - ${thruQuantity} [${quantityBreakId}]")),
            @FormField(name = "quantityUomId", title = "${uiLabelMap.ProductUnitOfMeasure}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${description}")),
            @FormField(name = "priceBreakId", displayEntity = @DisplayEntityField(entityName = "QuantityBreak", keyFieldName = "quantityBreakId", description = "${fromQuantity} - ${thruQuantity} [${quantityBreakId}]")),
            @FormField(name = "priceUomId", title = "${uiLabelMap.ProductUnitOfMeasure}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${description}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "storeRemoveShipRate", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "shipmentCostEstimateId")}))
        }
    )
    public interface ListShipmentCostEstimates {}

    @Form(
        name = "AddShipmentCostEstimate",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        target = "storeCreateShipRate",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "productStoreShipMethId", title = "${uiLabelMap.ProductShipmentMethod}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductStoreShipmentMethView", description = "${productStoreShipMethId} ${description} ${partyId}", constraints = {@EntityConstraint(name = "productStoreId", envName = "productStoreId")}, orderBy = {@EntityOrderBy(fieldName = "sequenceNumber")}))),
            @FormField(name = "fromGeo", title = "${uiLabelMap.ProductFromGeo}", lookup = @LookupField(targetFormName = "LookupGeo")),
            @FormField(name = "toGeo", title = "${uiLabelMap.ProductToGeo}", lookup = @LookupField(targetFormName = "LookupGeo")),
            @FormField(name = "partyId", text = @TextField),
            @FormField(name = "roleTypeId", text = @TextField),
            @FormField(name = "flatPercent", title = "${uiLabelMap.ProductFlatBasePercent}", tooltip = "${uiLabelMap.ProductShipamountOrderTotalPercent}", text = @TextField),
            @FormField(name = "flatPrice", title = "${uiLabelMap.ProductFlatBasePrice}", tooltip = "${uiLabelMap.ProductShipamountPrice}", text = @TextField),
            @FormField(name = "flatItemPrice", title = "${uiLabelMap.ProductFlatItemPrice}", tooltip = "${uiLabelMap.ProductShipamountTotalQuantityPrice}", text = @TextField),
            @FormField(name = "shippingPricePercent", title = "${uiLabelMap.ProductFlatShippingPercent}", tooltip = "${uiLabelMap.ProductShipamountShippingTotalPercent}", text = @TextField),
            @FormField(name = "productFeatureGroupId", title = "${uiLabelMap.ProductFeatureGroup}", tooltip = "${uiLabelMap.ProductFeatureMessage}", text = @TextField),
            @FormField(name = "featurePercent", title = "${uiLabelMap.ProductFeaturePerFeaturePercent}", tooltip = "${uiLabelMap.ProductShipamount} : ${uiLabelMap.ProductShipamount} + ((${uiLabelMap.ProductOrderTotal} * ${uiLabelMap.ProductPercent}) * ${uiLabelMap.ProductTotalFeaturesApplied})", text = @TextField),
            @FormField(name = "featurePrice", title = "${uiLabelMap.ProductFeaturePerFeaturePrice}", tooltip = "${uiLabelMap.ProductShipamount} : ${uiLabelMap.ProductShipamount} + (${uiLabelMap.ProductPrice} * ${uiLabelMap.ProductTotalFeaturesApplied})", text = @TextField),
            @FormField(name = "oversizeUnit", title = "${uiLabelMap.ProductOversizeUnit}", tooltip = "${uiLabelMap.ProductEach} ((${uiLabelMap.ProductHeight} * 2) + (${uiLabelMap.ProductWidth} * 2) + ${uiLabelMap.ProductDepth}) >= ${uiLabelMap.CommonThis} ${uiLabelMap.ProductNumber}", text = @TextField),
            @FormField(name = "oversizePrice", title = "${uiLabelMap.ProductOversizeSurcharge}", tooltip = "${uiLabelMap.ProductShipamount} : ${uiLabelMap.ProductShipamount} + (${uiLabelMap.ProductOversizeNumber} * ${uiLabelMap.ProductSurcharge})", text = @TextField),
            @FormField(name = "WeightTitle2", title = " ", tooltip = "${uiLabelMap.ProductMinMax}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "wmin", title = "${uiLabelMap.ProductMinWt}", text = @TextField),
            @FormField(name = "wmax", title = "${uiLabelMap.ProductMaxWt}", text = @TextField),
            @FormField(name = "weightBreakId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "QuantityBreak", description = "${fromQuantity} - ${thruQuantity}", keyFieldName = "quantityBreakId", constraints = {@EntityConstraint(name = "quantityBreakTypeId", value = "SHIP_WEIGHT")}, orderBy = {@EntityOrderBy(fieldName = "fromQuantity")}))),
            @FormField(name = "wuom", title = "${uiLabelMap.ProductUnitOfMeasure}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "WEIGHT_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "wprice", title = "${uiLabelMap.ProductPerUnitPrice}", tooltip = "${uiLabelMap.ProductOnlyAppliesWithinSpan}", text = @TextField),
            @FormField(name = "QuantityTitle2", title = " ", tooltip = "${uiLabelMap.ProductMinMax}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "qmin", title = "${uiLabelMap.ProductMinQt}", text = @TextField),
            @FormField(name = "qmax", title = "${uiLabelMap.ProductMaxQt}", text = @TextField),
            @FormField(name = "quantityBreakId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "QuantityBreak", description = "${fromQuantity} - ${thruQuantity}", keyFieldName = "quantityBreakId", constraints = {@EntityConstraint(name = "quantityBreakTypeId", value = "SHIP_QUANTITY")}, orderBy = {@EntityOrderBy(fieldName = "fromQuantity")}))),
            @FormField(name = "quom", title = "${uiLabelMap.ProductUnitOfMeasure}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE", operator = "not-equals")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "qprice", title = "${uiLabelMap.ProductPerUnitPrice}", tooltip = "${uiLabelMap.ProductOnlyAppliesWithinSpan}", text = @TextField),
            @FormField(name = "PriceTitle2", title = " ", tooltip = "${uiLabelMap.ProductMinMax}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "pmin", title = "${uiLabelMap.ProductMinPr}", text = @TextField),
            @FormField(name = "pmax", title = "${uiLabelMap.ProductMaxPr}", text = @TextField),
            @FormField(name = "priceBreakId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "QuantityBreak", description = "${fromQuantity} - ${thruQuantity}", keyFieldName = "quantityBreakId", constraints = {@EntityConstraint(name = "quantityBreakTypeId", value = "SHIP_PRICE")}, orderBy = {@EntityOrderBy(fieldName = "fromQuantity")}))),
            @FormField(name = "puom", title = "${uiLabelMap.ProductUnitOfMeasure}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "pprice", title = "${uiLabelMap.ProductPerUnitPrice}", tooltip = "${uiLabelMap.ProductOnlyAppliesWithinSpan}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        sortOrder = @SortOrder()
    )
    public interface AddShipmentCostEstimate {}

    @Form(
        name = "ViewShipmentCostEstimate",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        defaultMapName = "estimate",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "shipmentCostEstimateId", display = @DisplayField),
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "shipmentMethodTypeId", display = @DisplayField(description = "${estimate.shipmentMethodTypeId} (${estimate.carrierPartyId})")),
            @FormField(name = "geoIdFrom", title = "${uiLabelMap.ProductFromGeo}", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName} [${geoId}]")),
            @FormField(name = "geoIdTo", title = "${uiLabelMap.ProductToGeo}", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName} [${geoId}]")),
            @FormField(name = "partyId", display = @DisplayField),
            @FormField(name = "roleTypeId", display = @DisplayField),
            @FormField(name = "FlatTitle", title = "${uiLabelMap.ProductFlatTitle}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "orderPricePercent", title = "${uiLabelMap.ProductFlatBasePercent}", tooltip = "${uiLabelMap.ProductShipamountOrderTotalPercent}", display = @DisplayField),
            @FormField(name = "orderFlatPrice", title = "${uiLabelMap.ProductFlatBasePrice}", tooltip = "${uiLabelMap.ProductShipamountPrice}", display = @DisplayField),
            @FormField(name = "orderItemFlatPrice", title = "${uiLabelMap.ProductFlatItemPrice}", tooltip = "${uiLabelMap.ProductShipamountTotalQuantityPrice}", display = @DisplayField),
            @FormField(name = "shippingPricePercent", title = "${uiLabelMap.ProductFlatShippingPercent}", tooltip = "${uiLabelMap.ProductShipamountShippingTotalPercent}", display = @DisplayField),
            @FormField(name = "FeatureTitle", title = "${uiLabelMap.ProductFeatureTitle}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "productFeatureGroupId", title = "${uiLabelMap.ProductFeatureGroup}", tooltip = "${uiLabelMap.ProductFeatureMessage}", display = @DisplayField),
            @FormField(name = "featurePercent", title = "${uiLabelMap.ProductFeaturePerFeaturePercent}", tooltip = "${uiLabelMap.ProductShipamount} : ${uiLabelMap.ProductShipamount} + ((${uiLabelMap.ProductOrderTotal} * ${uiLabelMap.ProductPercent}) * ${uiLabelMap.ProductTotalFeaturesApplied})", display = @DisplayField),
            @FormField(name = "featurePrice", title = "${uiLabelMap.ProductFeaturePerFeaturePrice}", tooltip = "${uiLabelMap.ProductShipamount} : ${uiLabelMap.ProductShipamount} + (${uiLabelMap.ProductPrice} * ${uiLabelMap.ProductTotalFeaturesApplied})", display = @DisplayField),
            @FormField(name = "OversizeTitle", title = "${uiLabelMap.ProductOversizeTitle}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "oversizeUnit", title = "${uiLabelMap.ProductOversizeUnit}", tooltip = "${uiLabelMap.ProductEach} ((${uiLabelMap.ProductHeight} * 2) + (${uiLabelMap.ProductWidth} * 2) + ${uiLabelMap.ProductDepth}) >= ${uiLabelMap.CommonThis} ${uiLabelMap.ProductNumber}", display = @DisplayField),
            @FormField(name = "oversizePrice", title = "${uiLabelMap.ProductOversizeSurcharge}", tooltip = "${uiLabelMap.ProductShipamount} : ${uiLabelMap.ProductShipamount} + (${uiLabelMap.ProductNumber} ${uiLabelMap.ProductOversize} ${uiLabelMap.ProductProducts} * ${uiLabelMap.ProductSurcharge})", display = @DisplayField),
            @FormField(name = "WeightTitle1", title = "${uiLabelMap.ProductWeightTitle1}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "WeightTitle2", title = " ", tooltip = "${uiLabelMap.ProductMinMax}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "weightBreakId", displayEntity = @DisplayEntityField(entityName = "QuantityBreak", keyFieldName = "quantityBreakId", description = "${fromQuantity} - ${thruQuantity}")),
            @FormField(name = "weightUomId", title = "${uiLabelMap.ProductUnitOfMeasure}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${description}")),
            @FormField(name = "weightUnitPrice", title = "${uiLabelMap.ProductPerUnitPrice}", tooltip = "${uiLabelMap.ProductOnlyAppliesWithinSpan}", display = @DisplayField),
            @FormField(name = "QuantityTitle1", title = "${uiLabelMap.ProductQuantityTitle1}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "QuantityTitle2", title = " ", tooltip = "${uiLabelMap.ProductMinMax}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "quantityBreakId", displayEntity = @DisplayEntityField(entityName = "QuantityBreak", keyFieldName = "quantityBreakId", description = "${fromQuantity} - ${thruQuantity}")),
            @FormField(name = "quantityUomId", title = "${uiLabelMap.ProductUnitOfMeasure}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${description}")),
            @FormField(name = "quantityUnitPrice", title = "${uiLabelMap.ProductPerUnitPrice}", tooltip = "${uiLabelMap.ProductOnlyAppliesWithinSpan}", display = @DisplayField),
            @FormField(name = "PriceTitle1", title = "${uiLabelMap.ProductPriceTitle1}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "PriceTitle2", title = " ", tooltip = "${uiLabelMap.ProductMinMax}", titleAreaStyle = "group-label", display = @DisplayField(description = " ", alsoHidden = false)),
            @FormField(name = "priceBreakId", displayEntity = @DisplayEntityField(entityName = "QuantityBreak", keyFieldName = "quantityBreakId", description = "${fromQuantity} - ${thruQuantity}")),
            @FormField(name = "priceUomId", title = "${uiLabelMap.ProductUnitOfMeasure}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${description}")),
            @FormField(name = "priceUnitPrice", title = "${uiLabelMap.ProductPerUnitPrice}", tooltip = "${uiLabelMap.ProductOnlyAppliesWithinSpan}", display = @DisplayField)
        }
    )
    public interface ViewShipmentCostEstimate {}

    @Form(
        name = "ListProductStoreVendorPayments",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        type = FormType.LIST,
        target = "deleteProductStoreVendorPayment",
        listName = "productStoreVendorPaymentList",
        paginateTarget = "EditProductStoreVendorPayments",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "vendorPartyId", display = @DisplayField),
            @FormField(name = "paymentMethodTypeId", displayEntity = @DisplayEntityField(entityName = "PaymentMethodType", keyFieldName = "paymentMethodTypeId", description = "${description}")),
            @FormField(name = "creditCardEnumId", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface ListProductStoreVendorPayments {}

    @Form(
        name = "EditProductStoreVendorPayment",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        target = "createProductStoreVendorPayment",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "vendorPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "paymentMethodTypeId", dropDown = @DropDownField(current = "selected", entityOptions = @EntityOptions(entityName = "PaymentMethodType", description = "${description}", keyFieldName = "paymentMethodTypeId"))),
            @FormField(name = "creditCardEnumId", dropDown = @DropDownField(current = "selected", entityOptions = @EntityOptions(entityName = "Enumeration", description = "${enumCode} - ${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "CREDIT_CARD_TYPE")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditProductStoreVendorPayment {}

    @Form(
        name = "ListProductStoreVendorShipments",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        type = FormType.LIST,
        target = "deleteProductStoreVendorShipment",
        listName = "productStoreVendorShipmentList",
        paginateTarget = "EditProductStoreVendorShipments",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "carrierPartyId", display = @DisplayField),
            @FormField(name = "vendorPartyId", display = @DisplayField),
            @FormField(name = "shipmentMethodTypeId", displayEntity = @DisplayEntityField(entityName = "ShipmentMethodType", keyFieldName = "shipmentMethodTypeId", description = "${description}")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface ListProductStoreVendorShipments {}

    @Form(
        name = "EditProductStoreVendorShipment",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        target = "createProductStoreVendorShipment",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "carrierPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "vendorPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "shipmentMethodTypeId", dropDown = @DropDownField(current = "selected", entityOptions = @EntityOptions(entityName = "ShipmentMethodType", description = "${description}", keyFieldName = "shipmentMethodTypeId"))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditProductStoreVendorShipment {}

    @Form(
        name = "ListProductStorePromos",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        type = FormType.LIST,
        target = "updateProductStorePromoAppl",
        listName = "productStorePromoAndAppls",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "productPromoId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditProductPromo", description = "${productPromoId}", parameters = {@ParameterDef(paramName = "productPromoId")})),
            @FormField(name = "promoName", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "sequenceNum", hidden = @HiddenField),
            @FormField(name = "manualOnly", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListProductStorePromos {}

    @Form(
        name = "CreateProductStorePromo",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        target = "createProductStorePromoAppl",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "productPromoId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ProductPromo", description = "[${productPromoId}] ${promoName}"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", requiredField = true, dateTime = @DateTimeField),
            @FormField(name = "thruDate", position = 2, dateTime = @DateTimeField),
            @FormField(name = "manualOnly", dropDown = @DropDownField(options = {@Option(key = "N", description = "${uiLabelMap.CommonN}"), @Option(key = "Y", description = "${uiLabelMap.CommonY}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface CreateProductStorePromo {}

    @Form(
        name = "ListProductStorePaymentSettings",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        type = FormType.LIST,
        listName = "productStorePaymentSettings",
        paginateTarget = "EditProductStorePaySetup",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.CommonPaymentMethodType}", displayEntity = @DisplayEntityField(entityName = "PaymentMethodType", description = "${description}")),
            @FormField(name = "paymentServiceTypeEnumId", title = "${uiLabelMap.ProductServiceType}", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}")),
            @FormField(name = "paymentService", title = "${uiLabelMap.ProductServiceName}", display = @DisplayField),
            @FormField(name = "paymentCustomMethodId", title = "${uiLabelMap.ProductCustomMethod}", displayEntity = @DisplayEntityField(entityName = "CustomMethod", keyFieldName = "customMethodId", description = "${description} (${customMethodName})")),
            @FormField(name = "paymentGatewayConfigId", title = "${uiLabelMap.AccountingPaymentGatewayConfigId}", displayEntity = @DisplayEntityField(entityName = "PaymentGatewayConfig", keyFieldName = "paymentGatewayConfigId", description = "${description}")),
            @FormField(name = "paymentPropertiesPath", title = "${uiLabelMap.ProductPaymentProps}", display = @DisplayField),
            @FormField(name = "applyToAllProducts", title = "${uiLabelMap.ProductApplyToAll}", display = @DisplayField),
            @FormField(name = "editAction", title = " ", useWhen = "security.hasEntityPermission(\"CATALOG\", \"_UPDATE\", session)", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditProductStorePaySetup", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "paymentMethodTypeId"), @ParameterDef(paramName = "paymentServiceTypeEnumId")})),
            @FormField(name = "editAction", title = " ", useWhen = "!security.hasEntityPermission(\"CATALOG\", \"_UPDATE\", session)", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", useWhen = "security.hasEntityPermission(\"CATALOG\", \"_DELETE\", session)", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "storeRemovePaySetting", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productStoreId"), @ParameterDef(paramName = "paymentMethodTypeId"), @ParameterDef(paramName = "paymentServiceTypeEnumId")})),
            @FormField(name = "deleteAction", title = " ", useWhen = "!security.hasEntityPermission(\"CATALOG\", \"_DELETE\", session)", display = @DisplayField)
        }
    )
    public interface ListProductStorePaymentSettings {}

    @Form(
        name = "EditProductStorePaymentSetting",
        location = "component://product/widget/catalog/ProductStoreForms.xml",
        target = "storeUpdatePaySetting",
        defaultMapName = "productStorePaymentSetting",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "productStoreId", hidden = @HiddenField),
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.CommonPaymentMethodType}", useWhen = "productStorePaymentSetting!=null", displayEntity = @DisplayEntityField(entityName = "PaymentMethodType", keyFieldName = "paymentMethodTypeId", description = "${description}")),
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.CommonPaymentMethodType}", useWhen = "productStorePaymentSetting==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentMethodType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "paymentServiceTypeEnumId", title = "${uiLabelMap.ProductServiceType}", useWhen = "productStorePaymentSetting!=null", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}")),
            @FormField(name = "paymentServiceTypeEnumId", title = "${uiLabelMap.ProductServiceType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PRDS_PAYSVC")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "paymentService", title = "${uiLabelMap.ProductServiceName}", text = @TextField),
            @FormField(name = "paymentCustomMethodId", title = "${uiLabelMap.ProductCustomMethod}", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "paymentCustomMethods", keyName = "customMethodId", description = "${description} (${customMethodName})"))),
            @FormField(name = "paymentGatewayConfigId", title = "${uiLabelMap.AccountingPaymentGatewayConfigId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PaymentGatewayConfig", description = "${description}", keyFieldName = "paymentGatewayConfigId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "paymentPropertiesPath", title = "${uiLabelMap.ProductPaymentProps}", text = @TextField),
            @FormField(name = "applyToAllProducts", title = "${uiLabelMap.ProductApplyToAll} ${uiLabelMap.ProductProducts}", dropDown = @DropDownField(options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "productStorePaymentSetting==null", target = "storeCreatePaySetting")
        }
    )
    public interface EditProductStorePaymentSetting {}

}

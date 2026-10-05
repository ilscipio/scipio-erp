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
public class FacilityFacilityForms {

    @Form(
        name = "FindFacility",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindFacility",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "facilityId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditFacility", description = "${facilityId}", parameters = {@ParameterDef(paramName = "facilityId")})),
            @FormField(name = "facilityName", display = @DisplayField),
            @FormField(name = "facilityTypeId", title = "${uiLabelMap.ProductFacilityType}", displayEntity = @DisplayEntityField(entityName = "FacilityType", description = "${description}")),
            @FormField(name = "ownerPartyId", title = "${uiLabelMap.ProductFacilityOwner}", display = @DisplayField),
            @FormField(name = "FacilitySize", title = "${uiLabelMap.ProductFacilitySize}", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Facility"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        onEventUpdateAreas = {
            @OnEventUpdateArea(eventType = "paginate", areaId = "search-results", areaTarget = "FacilitySearchResults")
        }
    )
    public interface FindFacility {}

    @Form(
        name = "FindFacilityOptions",
        location = "component://product/widget/facility/FacilityForms.xml",
        target = "FindFacility",
        extendsForm = "lookupFacility",
        extendsResource = "component://product/widget/facility/FieldLookupForms.xml",
        fields = {
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindFacilityOptions {}

    @Form(
        name = "SearchInventoryItemsParams",
        location = "component://product/widget/facility/FacilityForms.xml",
        target = "EditFacilityInventoryItems",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "facilityId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "datetimeReceived", dateFind = @DateFindField),
            @FormField(name = "productId", textFind = @TextFindField),
            @FormField(name = "internalName", textFind = @TextFindField),
            @FormField(name = "inventoryItemId", textFind = @TextFindField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "INV_NON_SER_STTS")}))),
            @FormField(name = "serialNumber", textFind = @TextFindField),
            @FormField(name = "softIdentifier", text = @TextField),
            @FormField(name = "partyId", title = "${uiLabelMap.FormFieldTitle_manufacturerPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "lotId", title = "${uiLabelMap.ProductLotId}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", event = "onclick", action = "javascript:var field=document.SearchInventoryItemsParams.softIdentifier;var tmp=field.value;if (tmp.substring(0, 2) == '0x') {tmp=parseInt(tmp, 16)};if (!isNaN(tmp)) {field.value=tmp};return true;", submit = @SubmitField)
        }
    )
    public interface SearchInventoryItemsParams {}

    @Form(
        name = "ListFacilityInventoryItems",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "EditFacilityInventoryItems",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "inventoryItemId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditInventoryItem", description = "${inventoryItemId}", parameters = {@ParameterDef(paramName = "inventoryItemId"), @ParameterDef(paramName = "facilityId")})),
            @FormField(name = "inventoryItemTypeId", title = "${uiLabelMap.ProductInventoryItemTypeId}", displayEntity = @DisplayEntityField(entityName = "InventoryItemType")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "datetimeReceived", display = @DisplayField),
            @FormField(name = "expireDate", title = "${uiLabelMap.ProductExpireDate}", display = @DisplayField),
            @FormField(name = "productId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/ViewProduct", urlMode = UrlMode.INTER_APP, description = "${productId}", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "internalName", display = @DisplayField),
            @FormField(name = "partyId", display = @DisplayField),
            @FormField(name = "locationSeqId", widgetStyle = "${styles.link_nav_info_id_long}", hyperlink = @HyperlinkField(target = "EditFacilityLocation", description = "${areaId}:${aisleId}:${sectionId}:${levelId}:${positionId} [${locationSeqId}]", parameters = {@ParameterDef(paramName = "facilityId"), @ParameterDef(paramName = "locationSeqId")})),
            @FormField(name = "enumId", entryName = "locationTypeEnumId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "Enumeration")),
            @FormField(name = "lotId", title = "${uiLabelMap.ProductLotId}", display = @DisplayField),
            @FormField(name = "binNumber", title = "${uiLabelMap.ProductBinNumber}", display = @DisplayField),
            @FormField(name = "serialNumber", display = @DisplayField),
            @FormField(name = "softIdentifier", display = @DisplayField),
            @FormField(name = "quantityOnHandTotal", display = @DisplayField(description = "${availableToPromiseTotal} / ${quantityOnHandTotal}")),
            @FormField(name = "transferAction", entryName = "inventoryItemId", title = "${uiLabelMap.ProductTransfer}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "TransferInventoryItem", description = "${uiLabelMap.ProductTransfer}", parameters = {@ParameterDef(paramName = "facilityId"), @ParameterDef(paramName = "inventoryItemId")}))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "results", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "InventoryItemAndLocation"), @FieldMap(fieldName = "orderBy", value = "statusId|quantityOnHandTotal|serialNumber"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListFacilityInventoryItems {}

    @Form(
        name = "ListFacilityInventoryItemsNoLocations",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "inventoryItems",
        paginateTarget = "SearchInventoryItemsByLabels",
        overrideListSize = "${inventoryItemsSize}",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "inventoryItemId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditInventoryItem", description = "${inventoryItemId}", parameters = {@ParameterDef(paramName = "inventoryItemId"), @ParameterDef(paramName = "facilityId")})),
            @FormField(name = "inventoryItemTypeId", title = "${uiLabelMap.ProductInventoryItemTypeId}", displayEntity = @DisplayEntityField(entityName = "InventoryItemType")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", display = @DisplayField),
            @FormField(name = "datetimeReceived", display = @DisplayField),
            @FormField(name = "expireDate", title = "${uiLabelMap.ProductExpireDate}", display = @DisplayField),
            @FormField(name = "productId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/ViewProduct", urlMode = UrlMode.INTER_APP, description = "${productId}", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "lotId", title = "${uiLabelMap.ProductLotId}", display = @DisplayField),
            @FormField(name = "binNumber", title = "${ProductBinNumber}", display = @DisplayField),
            @FormField(name = "serialNumber", display = @DisplayField),
            @FormField(name = "softIdentifier", display = @DisplayField),
            @FormField(name = "quantityOnHandTotal", display = @DisplayField(description = "${availableToPromiseTotal} / ${quantityOnHandTotal}"))
        }
    )
    public interface ListFacilityInventoryItemsNoLocations {}

    @Form(
        name = "SearchInventoryItemsDetailsParams",
        location = "component://product/widget/facility/FacilityForms.xml",
        target = "ViewFacilityInventoryItemsDetails",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "facilityId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "effectiveDate", dateFind = @DateFindField),
            @FormField(name = "productId", textFind = @TextFindField),
            @FormField(name = "inventoryItemId", textFind = @TextFindField),
            @FormField(name = "serialNumber", textFind = @TextFindField),
            @FormField(name = "softIdentifier", text = @TextField),
            @FormField(name = "manufacturerPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "orderId", lookup = @LookupField(targetFormName = "LookupOrderHeader")),
            @FormField(name = "shipmentId", text = @TextField),
            @FormField(name = "returnId", text = @TextField),
            @FormField(name = "workEffortId", text = @TextField),
            @FormField(name = "reasonEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "IID_REASON")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "quantityOnHandDiff", tooltip = "${uiLabelMap.ProductMessageQoh}", textFind = @TextFindField(defaultValue = "0", defaultOption = "notEqual", ignoreCase = false)),
            @FormField(name = "reportType", dropDown = @DropDownField(options = {@Option(key = "BY_ITEM", description = "${uiLabelMap.ProductByInventoryItem}"), @Option(key = "BY_PRODUCT", description = "${uiLabelMap.ProductByProduct}"), @Option(key = "BY_DATE", description = "${uiLabelMap.ProductByDate}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", event = "onclick", action = "javascript:var field=document.SearchInventoryItemsParams.softIdentifier;var tmp=field.value;if (tmp.substring(0, 2) == '0x') {tmp=parseInt(tmp, 16)};if (!isNaN(tmp)) {field.value=tmp};return true;", submit = @SubmitField)
        }
    )
    public interface SearchInventoryItemsDetailsParams {}

    @Form(
        name = "ListFacilityInventoryItemsDetailsByItem",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "inventoryItemId", useWhen = "showPosition1", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditInventoryItem", description = "${inventoryItemId}", parameters = {@ParameterDef(paramName = "inventoryItemId"), @ParameterDef(paramName = "facilityId")})),
            @FormField(name = "productId", useWhen = "showPosition1", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/ViewProduct", urlMode = UrlMode.INTER_APP, description = "${productId}", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "quantityOnHandTotal", useWhen = "showPosition1", display = @DisplayField(description = "${quantityOnHandTotal}")),
            @FormField(name = "availableToPromiseTotal", useWhen = "showPosition1", display = @DisplayField(description = "${availableToPromiseTotal}")),
            @FormField(name = "serialNumber", useWhen = "showPosition1", display = @DisplayField),
            @FormField(name = "softIdentifier", useWhen = "showPosition1", display = @DisplayField),
            @FormField(name = "inventoryItemDetailSeqId", position = 2, display = @DisplayField),
            @FormField(name = "effectiveDate", position = 2, display = @DisplayField),
            @FormField(name = "quantityOnHandDiff", position = 2, display = @DisplayField),
            @FormField(name = "availableToPromiseDiff", position = 2, display = @DisplayField),
            @FormField(name = "reasonEnumId", position = 2, displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}")),
            @FormField(name = "description", position = 2, display = @DisplayField),
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "orderItemSeqId", position = 2, display = @DisplayField),
            @FormField(name = "shipmentId", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "EditShipment", description = "${shipmentId}", parameters = {@ParameterDef(paramName = "shipmentId")})),
            @FormField(name = "shipmentItemSeqId", position = 2, display = @DisplayField),
            @FormField(name = "workEffortId", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "/workeffort/control/WorkEffortSummary", urlMode = UrlMode.INTER_APP, description = "${workEffortId}", parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "returnId", position = 2, display = @DisplayField),
            @FormField(name = "returnItemSeqId", position = 2, display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "results", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "InventoryItemAndDetail"), @FieldMap(fieldName = "orderBy", value = "productId|inventoryItemId|-inventoryItemDetailSeqId|-effectiveDate|quantityOnHandTotal"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(set = {@SetAction(field = "showPosition1", value = "${groovy: String prev = (String) previousItem.get('inventoryItemId');return new Boolean(!(prev!=null && prev.equals(inventoryItemId)));}", type = "Boolean")})
    )
    public interface ListFacilityInventoryItemsDetailsByItem {}

    @Form(
        name = "ListFacilityInventoryItemsDetailsByProduct",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", useWhen = "showPosition1", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/ViewProduct", urlMode = UrlMode.INTER_APP, description = "${productId}", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "effectiveDate", position = 2, display = @DisplayField),
            @FormField(name = "inventoryItemId", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "EditInventoryItem", description = "${inventoryItemId}", parameters = {@ParameterDef(paramName = "inventoryItemId"), @ParameterDef(paramName = "facilityId")})),
            @FormField(name = "inventoryItemDetailSeqId", position = 2, display = @DisplayField),
            @FormField(name = "quantityOnHandDiff", position = 2, display = @DisplayField),
            @FormField(name = "availableToPromiseDiff", position = 2, display = @DisplayField),
            @FormField(name = "reasonEnumId", position = 2, displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}")),
            @FormField(name = "description", position = 2, display = @DisplayField),
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "orderItemSeqId", position = 2, display = @DisplayField),
            @FormField(name = "shipmentId", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "EditShipment", description = "${shipmentId}", parameters = {@ParameterDef(paramName = "shipmentId")})),
            @FormField(name = "shipmentItemSeqId", position = 2, display = @DisplayField),
            @FormField(name = "workEffortId", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "/workeffort/control/WorkEffortSummary", urlMode = UrlMode.INTER_APP, description = "${workEffortId}", parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "returnId", position = 2, display = @DisplayField),
            @FormField(name = "returnItemSeqId", position = 2, display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "results", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "InventoryItemAndDetail"), @FieldMap(fieldName = "orderBy", value = "productId|-effectiveDate|inventoryItemId|-inventoryItemDetailSeqId|quantityOnHandTotal"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(set = {@SetAction(field = "showPosition1", value = "${groovy:String prev = (String) previousItem.get('productId'); return new Boolean(!(prev!=null && prev.equals(productId)));}", type = "Boolean")})
    )
    public interface ListFacilityInventoryItemsDetailsByProduct {}

    @Form(
        name = "ListFacilityInventoryItemsDetailsByDate",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "effectiveDate", display = @DisplayField),
            @FormField(name = "productId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/ViewProduct", urlMode = UrlMode.INTER_APP, description = "${productId}", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "inventoryItemId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditInventoryItem", description = "${inventoryItemId}", parameters = {@ParameterDef(paramName = "inventoryItemId"), @ParameterDef(paramName = "facilityId")})),
            @FormField(name = "inventoryItemDetailSeqId", display = @DisplayField),
            @FormField(name = "quantityOnHandDiff", display = @DisplayField),
            @FormField(name = "availableToPromiseDiff", display = @DisplayField),
            @FormField(name = "reasonEnumId", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}")),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "orderItemSeqId", display = @DisplayField),
            @FormField(name = "shipmentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditShipment", description = "${shipmentId}", parameters = {@ParameterDef(paramName = "shipmentId")})),
            @FormField(name = "shipmentItemSeqId", display = @DisplayField),
            @FormField(name = "workEffortId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/workeffort/control/WorkEffortSummary", urlMode = UrlMode.INTER_APP, description = "${workEffortId}", parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "returnId", display = @DisplayField),
            @FormField(name = "returnItemSeqId", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "results", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "InventoryItemAndDetail"), @FieldMap(fieldName = "orderBy", value = "-effectiveDate|productId|inventoryItemId|-inventoryItemDetailSeqId|quantityOnHandTotal"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListFacilityInventoryItemsDetailsByDate {}

    @Form(
        name = "FindFacilityInventoryByProduct",
        location = "component://product/widget/facility/FacilityForms.xml",
        target = "${facilityInventoryByProductScreen}",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "facilityId", hidden = @HiddenField),
            @FormField(name = "action", hidden = @HiddenField(value = "SEARCH")),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", text = @TextField),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductInternalName}", text = @TextField),
            @FormField(name = "productTypeId", title = "${uiLabelMap.ProductProductType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductType", description = "${description}", constraints = {@EntityConstraint(name = "isPhysical", value = "Y"), @EntityConstraint(name = "parentTypeId", value = "GOOD")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "searchInProductCategoryId", title = "${uiLabelMap.ProductCategory}", lookup = @LookupField(targetFormName = "LookupProductCategory")),
            @FormField(name = "productSupplierId", title = "${uiLabelMap.ProductSupplier}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRoleAndPartyDetail", description = "${groupName} [${partyId}]", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "SUPPLIER")}, orderBy = {@EntityOrderBy(fieldName = "partyId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "INV_NON_SER_STTS")}))),
            @FormField(name = "offsetQOHQty", title = "${uiLabelMap.ProductQtyOffsetQOHBelow}", text = @TextField),
            @FormField(name = "offsetATPQty", title = "${uiLabelMap.ProductQtyOffsetATPBelow}", text = @TextField),
            @FormField(name = "productsSoldThruTimestamp", title = "${uiLabelMap.ProductShowProductsSoldThruTimestamp}", dateTime = @DateTimeField(defaultValue = "${groovy: org.ofbiz.base.util.UtilDateTime.nowTimestamp()}")),
            @FormField(name = "VIEW_SIZE_1", entryName = "viewSize", title = "${uiLabelMap.ProductShowProductsPerPage}", text = @TextField),
            @FormField(name = "monthsInPastLimit", entryName = "monthsInPastLimit", text = @TextField),
            @FormField(name = "fromDateSellThrough", title = "${uiLabelMap.ProductFromDateSellThrough}", dateTime = @DateTimeField),
            @FormField(name = "thruDateSellThrough", title = "${uiLabelMap.ProductThruDateSellThrough}", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindFacilityInventoryByProduct {}

    @Form(
        name = "ListFacilityInventoryByProduct",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "inventoryByProduct",
        paginateTarget = "${facilityInventoryByProductScreen}",
        overrideListSize = "${overrideListSize}",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "items", title = "${uiLabelMap.ProductProductId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditFacilityInventoryItems", description = "${productId}", parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "facilityId")})),
            @FormField(name = "productId", title = "${uiLabelMap.CommonDescription}", displayEntity = @DisplayEntityField(entityName = "Product", description = "${internalName}", subHyperlink = @SubHyperlink(target = "/catalog/control/ViewProduct", description = "${uiLabelMap.ProductCatalog}", linkStyle = "${styles.link_nav} ${styles.action_update}", parameters = {@ParameterDef(paramName = "productId")}))),
            @FormField(name = "totalAvailableToPromise", title = "${uiLabelMap.CommonTotal} ${uiLabelMap.ProductAtp}", display = @DisplayField),
            @FormField(name = "totalQuantityOnHand", title = "${uiLabelMap.CommonTotal} ${uiLabelMap.ProductQoh}", display = @DisplayField),
            @FormField(name = "quantityOnOrder", title = "${uiLabelMap.ProductOrderedQuantity}", display = @DisplayField),
            @FormField(name = "minimumStock", title = "${uiLabelMap.ProductMinimumStock}", display = @DisplayField),
            @FormField(name = "reorderQuantity", title = "${uiLabelMap.ProductReorderQuantity}", display = @DisplayField),
            @FormField(name = "daysToShip", title = "${uiLabelMap.ProductDaysToShip}", display = @DisplayField),
            @FormField(name = "offsetQOHQtyAvailable", title = "${uiLabelMap.ProductQtyOffsetQOH}", display = @DisplayField),
            @FormField(name = "offsetATPQtyAvailable", title = "${uiLabelMap.ProductQtyOffsetATP}", display = @DisplayField),
            @FormField(name = "quantityUom", title = "${uiLabelMap.ProductQuantityUomId}", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${abbreviation}")),
            @FormField(name = "usageQuantity", title = "${uiLabelMap.ProductUsage}", display = @DisplayField),
            @FormField(name = "defaultPrice", title = "${uiLabelMap.ProductDefaultPrice}", display = @DisplayField),
            @FormField(name = "listPrice", title = "${uiLabelMap.ProductListPrice}", display = @DisplayField),
            @FormField(name = "wholeSalePrice", title = "${uiLabelMap.ProductWholeSalePrice}", display = @DisplayField),
            @FormField(name = "fromDateSellThrough", entryName = "parameters.fromDateSellThrough", display = @DisplayField),
            @FormField(name = "sellThroughInitialInventory", display = @DisplayField),
            @FormField(name = "sellThroughInventorySold", display = @DisplayField),
            @FormField(name = "sellThroughPercentage", display = @DisplayField)
        },
        rowActions = @RowActions(script = {@ScriptAction(location = "component://product/webapp/facility/WEB-INF/actions/facility/ComputeProductSellThroughData.groovy")})
    )
    public interface ListFacilityInventoryByProduct {}

    @Form(
        name = "SchedulingList",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.MULTI,
        target = "BatchScheduleShipmentRouteSegments?facilityId=${facilityId}",
        title = "${uiLabelMap.PageTitlePackageShipmentScheduling}",
        listName = "shipmentRouteSegments",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        useRowSubmit = true,
        fields = {
            @FormField(name = "shipmentId", title = "${uiLabelMap.ProductShipmentId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditShipment", description = "${shipmentId}", parameters = {@ParameterDef(paramName = "shipmentId")})),
            @FormField(name = "primaryOrderId", title = "${uiLabelMap.ProductOrderId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${primaryOrderId}", parameters = {@ParameterDef(paramName = "orderId", fromField = "primaryOrderId")})),
            @FormField(name = "shipmentRouteSegmentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditShipmentRouteSegments", description = "${shipmentRouteSegmentId}", parameters = {@ParameterDef(paramName = "shipmentId")})),
            @FormField(name = "carrierPartyId", title = "${uiLabelMap.ProductCarrier}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}")),
            @FormField(name = "shipmentMethodTypeId", title = "${uiLabelMap.ProductShipmentMethodType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ShipmentMethodType", description = "${description}"))),
            @FormField(name = "billingWeight", title = "${uiLabelMap.CommonWeight}", text = @TextField(size = 10)),
            @FormField(name = "billingWeightUomId", title = "${uiLabelMap.ProductWeightUomId}", dropDown = @DropDownField(options = {@Option(key = "${defaultWeightUom.uomId}", description = "${defaultWeightUom.description}")}, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "WEIGHT_MEASURE")}))),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.ProductScheduleTheseRouteSegments}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface SchedulingList {}

    @Form(
        name = "Labels",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.MULTI,
        target = "BatchPrintShippingLabels",
        listName = "shipmentPackageRouteSegments",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        useRowSubmit = true,
        fields = {
            @FormField(name = "shipmentId", title = "${uiLabelMap.ProductShipmentId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditShipment", description = "${shipmentId}", parameters = {@ParameterDef(paramName = "shipmentId")})),
            @FormField(name = "primaryOrderId", title = "${uiLabelMap.ProductOrderId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${primaryOrderId}", parameters = {@ParameterDef(paramName = "orderId", fromField = "primaryOrderId")})),
            @FormField(name = "shipmentRouteSegmentId", display = @DisplayField),
            @FormField(name = "shipmentPackageSeqId", display = @DisplayField),
            @FormField(name = "carrierPartyId", title = "${uiLabelMap.ProductCarrier}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}")),
            @FormField(name = "shipmentMethodTypeId", title = "${uiLabelMap.ProductShipmentMethodType}", displayEntity = @DisplayEntityField(entityName = "ShipmentMethodType")),
            @FormField(name = "label", title = "${uiLabelMap.ProductLabel}", widgetStyle = "${styles.link_nav} ${styles.action_view}", hyperlink = @HyperlinkField(target = "viewShipmentPackageRouteSegLabelImage", description = "${uiLabelMap.ProductLabel}", parameters = {@ParameterDef(paramName = "shipmentId"), @ParameterDef(paramName = "shipmentRouteSegmentId"), @ParameterDef(paramName = "shipmentPackageSeqId")})),
            @FormField(name = "carrierServiceStatusId", hidden = @HiddenField(value = "SHRSCS_ACCEPTED")),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonPrint}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", submit = @SubmitField)
        }
    )
    public interface Labels {}

    @Form(
        name = "FindPhysicalInventory",
        location = "component://product/widget/facility/FacilityForms.xml",
        target = "FindFacilityPhysicalInventory",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "facilityId", hidden = @HiddenField),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "internalName", title = "${uiLabelMap.ProductInternalName}", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPhysicalInventory {}

    @Form(
        name = "ListInventoryItemTotals",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "inventoryItemTotals",
        paginateTarget = "InventoryItemTotals",
        overrideListSize = "${overrideListSize}",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "inventoryItemGrandTotals", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", display = @DisplayField),
            @FormField(name = "quantityOnHand", title = "${uiLabelMap.ProductQoh}", display = @DisplayField),
            @FormField(name = "availableToPromise", title = "${uiLabelMap.ProductAtp}", display = @DisplayField),
            @FormField(name = "costPrice", title = "${uiLabelMap.ProductCostPrice}", display = @DisplayField),
            @FormField(name = "retailPrice", title = "${uiLabelMap.ProductRetailPrice}", display = @DisplayField),
            @FormField(name = "totalCostPrice", title = "${uiLabelMap.CommonTotal} ${uiLabelMap.ProductCostPrice}", display = @DisplayField),
            @FormField(name = "totalRetailPrice", title = "${uiLabelMap.CommonTotal} ${uiLabelMap.ProductRetailPrice}", display = @DisplayField)
        }
    )
    public interface ListInventoryItemTotals {}

    @Form(
        name = "ListInventoryAverageCosts",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "inventoryAverageCosts",
        paginateTarget = "InventoryAverageCosts",
        overrideListSize = "${overrideListSize}",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId2", entryName = "productId", title = "${uiLabelMap.ProductProductId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/ViewProduct", urlMode = UrlMode.INTER_APP, description = "${productId}", parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "totalQuantityOnHand", title = "${uiLabelMap.CommonTotal} ${uiLabelMap.ProductQoh}", display = @DisplayField),
            @FormField(name = "productAverageCost", title = "${uiLabelMap.ProductAverageCost}", useWhen = "currencyUomId!=null", display = @DisplayField(type = "currency")),
            @FormField(name = "productAverageCost", title = "${uiLabelMap.ProductAverageCost}", useWhen = "currencyUomId==null", display = @DisplayField(description = "${uiLabelMap.ProductDifferentCurrencies}")),
            @FormField(name = "totalInventoryCost", title = "${uiLabelMap.CommonTotalCost}", useWhen = "currencyUomId!=null", display = @DisplayField(type = "currency")),
            @FormField(name = "totalInventoryCost", title = "${uiLabelMap.CommonTotalCost}", useWhen = "currencyUomId==null", display = @DisplayField(description = "${uiLabelMap.ProductDifferentCurrencies}"))
        }
    )
    public interface ListInventoryAverageCosts {}

    @Form(
        name = "ListInventoryItemGrandTotals",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "inventoryItemGrandTotals",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "qohGrandTotal", title = "${uiLabelMap.ProductQoh} ${uiLabelMap.CommonTotal} ${uiLabelMap.CommonQty}", display = @DisplayField),
            @FormField(name = "atpGrandTotal", title = "${uiLabelMap.ProductAtp} ${uiLabelMap.CommonTotal} ${uiLabelMap.CommonQty}", display = @DisplayField),
            @FormField(name = "totalCostPriceGrandTotal", title = "${uiLabelMap.CommonTotal} ${uiLabelMap.ProductCostPrice}", display = @DisplayField),
            @FormField(name = "totalRetailPriceGrandTotal", title = "${uiLabelMap.CommonTotal} ${uiLabelMap.ProductRetailPrice}", display = @DisplayField)
        }
    )
    public interface ListInventoryItemGrandTotals {}

    @Form(
        name = "InventoryItemTotalsExport",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "inventoryItemTotals",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", display = @DisplayField),
            @FormField(name = "quantityOnHand", title = "${uiLabelMap.ProductQoh}", display = @DisplayField),
            @FormField(name = "availableToPromise", title = "${uiLabelMap.ProductAtp}", display = @DisplayField),
            @FormField(name = "costPrice", title = "${uiLabelMap.ProductCostPrice}", display = @DisplayField),
            @FormField(name = "retailPrice", title = "${uiLabelMap.ProductRetailPrice}", display = @DisplayField),
            @FormField(name = "totalCostPrice", title = "${uiLabelMap.CommonTotal} ${uiLabelMap.ProductCostPrice}", display = @DisplayField),
            @FormField(name = "totalRetailPrice", title = "${uiLabelMap.CommonTotal} ${uiLabelMap.ProductRetailPrice}", display = @DisplayField)
        }
    )
    public interface InventoryItemTotalsExport {}

    @Form(
        name = "InventoryItemGrandTotalsExport",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "inventoryItemGrandTotals",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "qohGrandTotal", title = "${uiLabelMap.ProductQoh} ${uiLabelMap.CommonTotal} ${uiLabelMap.CommonQty}", display = @DisplayField),
            @FormField(name = "atpGrandTotal", title = "${uiLabelMap.ProductAtp} ${uiLabelMap.CommonTotal} ${uiLabelMap.CommonQty}", display = @DisplayField),
            @FormField(name = "totalCostPriceGrandTotal", title = "${uiLabelMap.CommonTotal} ${uiLabelMap.ProductCostPrice}", display = @DisplayField),
            @FormField(name = "totalRetailPriceGrandTotal", title = "${uiLabelMap.CommonTotal} ${uiLabelMap.ProductRetailPrice}", display = @DisplayField)
        }
    )
    public interface InventoryItemGrandTotalsExport {}

    @Form(
        name = "FromFacilityTransfersComplete",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.MULTI,
        target = "CompleteRequestedTransfers?completeRequested=true&facilityId=${facilityId}",
        listName = "fromTransfers",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "inventoryTransferId", title = "${uiLabelMap.ProductInventoryTransfer}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "TransferInventoryItem", description = "${inventoryTransferId}", parameters = {@ParameterDef(paramName = "inventoryTransferId")})),
            @FormField(name = "inventoryItemId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditInventoryItem", description = "${inventoryItemId}", parameters = {@ParameterDef(paramName = "inventoryItemId")})),
            @FormField(name = "facilityIdTo", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditFacility", description = "${facilityIdTo}", alsoHidden = false, parameters = {@ParameterDef(paramName = "facilityId", fromField = "facilityIdTo")})),
            @FormField(name = "facilityName", entryName = "facilityIdTo", displayEntity = @DisplayEntityField(entityName = "Facility", keyFieldName = "facilityId", description = "${facilityName}", alsoHidden = false)),
            @FormField(name = "locationSeqIdTo", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "productId", entryName = "product.productId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/ViewProduct", urlMode = UrlMode.INTER_APP, description = "${product.productId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId", fromField = "product.productId")})),
            @FormField(name = "productName", entryName = "product.internalName", title = "${uiLabelMap.ProductProductName}", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "serialNumber", entryName = "inventoryItem.serialNumber", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "atpQoh", title = "${uiLabelMap.ProductAtpQoh}", display = @DisplayField(description = "${inventoryItem.availableToPromiseTotal}/${inventoryItem.quantityOnHandTotal}", alsoHidden = false)),
            @FormField(name = "locationSeqId", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "sendDate", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "receiveDate", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", hidden = @HiddenField(value = "IXF_COMPLETE")),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_complete}", submit = @SubmitField)
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "InventoryItem", valueField = "inventoryItem"), @EntityOneAction(entityName = "Product", valueField = "product")})
    )
    public interface FromFacilityTransfersComplete {}

    @Form(
        name = "EditFacilityGeoPoint",
        location = "component://product/widget/facility/FacilityForms.xml",
        target = "createUpdateFacilityGeoPoint",
        defaultMapName = "geoPoint",
        fields = {
            @FormField(name = "facilityId", hidden = @HiddenField(value = "${facilityId}")),
            @FormField(name = "geoPointId", hidden = @HiddenField),
            @FormField(name = "dataSourceId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "DataSource", description = "${description}", constraints = {@EntityConstraint(name = "dataSourceTypeId", value = "GEOPOINT_SUPPLIER")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "information", title = "${uiLabelMap.CommonDescription}", text = @TextField(size = 50, maxlength = 60)),
            @FormField(name = "latitude", text = @TextField),
            @FormField(name = "longitude", position = 2, text = @TextField),
            @FormField(name = "elevation", text = @TextField),
            @FormField(name = "elevationUomId", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "LENGTH_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "selectAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditFacilityGeoPoint {}

    @Form(
        name = "ListFacilityAgreements",
        location = "component://product/widget/facility/FacilityForms.xml",
        type = FormType.LIST,
        listName = "facilityAgreements",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", widgetStyle = "${styles.link_nav_info_id_long}", hyperlink = @HyperlinkField(target = "/accounting/control/EditAgreementItemFacility", urlMode = UrlMode.INTER_APP, description = "${agreementId}/${agreementItemSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "faclityId")})),
            @FormField(name = "agreementText", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField)
        }
    )
    public interface ListFacilityAgreements {}

}

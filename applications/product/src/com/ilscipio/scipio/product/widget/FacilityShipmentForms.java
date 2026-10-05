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
public class FacilityShipmentForms {

    @Form(
        name = "EditShipment",
        location = "component://product/widget/facility/ShipmentForms.xml",
        target = "updateShipment",
        defaultMapName = "shipment",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "shipmentId", title = "${uiLabelMap.ProductShipmentId}", tooltip = "${uiLabelMap.ProductNotModificationRecreatingProductShipment}", useWhen = "shipment!=null", display = @DisplayField),
            @FormField(name = "shipmentId", title = "${uiLabelMap.ProductShipmentId}", tooltip = "${uiLabelMap.ProductCouldNotFindProductShipmentWithId} [${shipmentId}]", useWhen = "shipment==null&&shipmentId!=null", display = @DisplayField(alsoHidden = false)),
            @FormField(name = "shipmentId", title = "${uiLabelMap.ProductShipmentId}", useWhen = "shipment==null&&shipmentId==null", ignored = @IgnoredField),
            @FormField(name = "shipmentTypeId", title = "${uiLabelMap.ProductShipmentTypeId}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ShipmentType", description = "${description}"))),
            @FormField(name = "primaryShipGroupSeqId", title = "${uiLabelMap.ProductPrimaryShipGroupSeqId}", text = @TextField),
            @FormField(name = "statusId", title = "${uiLabelMap.ProductStatusId}", useWhen = "shipment==null", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "${statusItemTypeId}")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.ProductStatusId}", useWhen = "shipment!=null", position = 2, dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", value = "${shipment.statusId}")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "primaryOrderId", title = "${uiLabelMap.ProductPrimaryOrderId}", lookup = @LookupField(targetFormName = "LookupOrderHeader")),
            @FormField(name = "primaryReturnId", title = "${uiLabelMap.ProductPrimaryReturnId}", position = 2, text = @TextField),
            @FormField(name = "estimatedReadyDate", title = "${uiLabelMap.ProductEstimatedReadyDate}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "estimatedShipDate", title = "${uiLabelMap.ProductEstimatedShipDate}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "estimatedArrivalDate", title = "${uiLabelMap.ProductEstimatedArrivalDate}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "latestCancelDate", title = "${uiLabelMap.ProductLatestCancelDate}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "originFacilityId", title = "${uiLabelMap.ProductOriginFacility} [${shipment.primaryOrderId}]", useWhen = "productStoreId!=null", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductStoreFacilityByOrder", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId", constraints = {@EntityConstraint(name = "orderId", value = "${orderHeader.orderId}")}, orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "originFacilityId", title = "${uiLabelMap.ProductOriginFacility}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "destinationFacilityId", title = "${uiLabelMap.ProductDestinationFacility}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.ProductFromParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.ProductToParty}", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "originContactMechId", title = "${uiLabelMap.ProductOriginPostalAddressId}", tooltip = "${uiLabelMap.CommonTo}: ${originPostalAddress.toName}, ${uiLabelMap.CommonAttn}: ${originPostalAddress.attnName}, ${originPostalAddress.address1}, ${originPostalAddress.address2}, ${originPostalAddress.city}, ${originPostalAddress.stateProvinceGeoId}, ${originPostalAddress.postalCode}, ${originPostalAddress.countryGeoId}", text = @TextField),
            @FormField(name = "destinationContactMechId", title = "${uiLabelMap.ProductDestinationPostalAddressId}", tooltip = "${uiLabelMap.CommonTo}: ${destinationPostalAddress.toName}, ${uiLabelMap.CommonAttn}: ${destinationPostalAddress.attnName}, ${destinationPostalAddress.address1}, ${destinationPostalAddress.address2}, ${destinationPostalAddress.city}, ${destinationPostalAddress.stateProvinceGeoId}, ${destinationPostalAddress.postalCode}, ${destinationPostalAddress.countryGeoId}", position = 2, text = @TextField),
            @FormField(name = "originTelecomNumberId", title = "${uiLabelMap.ProductOriginPhoneNumberId}", tooltip = "${originTelecomNumber.countryCode}  ${originTelecomNumber.areaCode} ${originTelecomNumber.contactNumber}", text = @TextField),
            @FormField(name = "destinationTelecomNumberId", title = "${uiLabelMap.ProductDestinationPhoneNumberId}", tooltip = "${destinationTelecomNumber.countryCode}  ${destinationTelecomNumber.areaCode} ${destinationTelecomNumber.contactNumber}", position = 2, text = @TextField),
            @FormField(name = "estimatedShipWorkEffId", title = "${uiLabelMap.ProductEstimatedShipWorkEffId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/workeffort/control/WorkEffortSummary", urlMode = UrlMode.INTER_APP, description = "${shipment.estimatedShipWorkEffId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId", fromField = "shipment.estimatedShipWorkEffId")})),
            @FormField(name = "estimatedArrivalWorkEffId", title = "${uiLabelMap.ProductEstimatedArrivalWorkEffId}", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "/workeffort/control/WorkEffortSummary", urlMode = UrlMode.INTER_APP, description = "${shipment.estimatedArrivalWorkEffId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId", fromField = "shipment.estimatedArrivalWorkEffId")})),
            @FormField(name = "estimatedShipCost", title = "${uiLabelMap.ProductEstimatedShipCost}", text = @TextField),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.ProductCurrencyUomId}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "additionalShippingCharge", title = "${uiLabelMap.ProductAdditionalShippingCharge}", position = 2, text = @TextField),
            @FormField(name = "handlingInstructions", title = "${uiLabelMap.ProductHandlingInstructions}", textarea = @TextareaField),
            @FormField(name = "createdByUserLogin", useWhen = "shipment!=null", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${firstName} ${lastName}")),
            @FormField(name = "lastModifiedByUserLogin", useWhen = "shipment!=null", position = 2, displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${firstName} ${lastName}")),
            @FormField(name = "createdDate", title = "${uiLabelMap.ProductCreatedDate}", useWhen = "shipment!=null", display = @DisplayField(description = "${shipment.createdDate}", alsoHidden = false)),
            @FormField(name = "lastModifiedDate", title = "${uiLabelMap.ProductLastModifiedDate}", useWhen = "shipment!=null", position = 2, display = @DisplayField(description = "${shipment.lastModifiedDate}", alsoHidden = false)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "shipment==null&&shipmentTypeId==null", target = "createShipment"),
            @AltTarget(useWhen = "shipment==null&&shipmentTypeId!=null&&shipmentTypeId.equals(\"PURCHASE_RETURN\")", target = "createShipmentAndItemsForVendorReturn")
        }
    )
    public interface EditShipment {}

    @Form(
        name = "FindShipment",
        location = "component://product/widget/facility/ShipmentForms.xml",
        target = "FindShipment",
        defaultMapName = "Shipment",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "shipmentId", textFind = @TextFindField),
            @FormField(name = "shipmentTypeId", title = "${uiLabelMap.ProductShipmentTypeId}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ShipmentType", description = "${description}"))),
            @FormField(name = "originFacilityId", title = "${uiLabelMap.ProductOriginFacility}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "destinationFacilityId", title = "${uiLabelMap.ProductDestinationFacility}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName} [${facilityId}]", keyFieldName = "facilityId", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, options = {@Option(description = "${uiLabelMap.ProductSalesShipmentStatus}"), @Option(description = "---"), @Option(description = "${uiLabelMap.ProductPurchaseShipmentStatus}"), @Option(description = "---"), @Option(description = "${uiLabelMap.ProductOrderReturnStatus}")}, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "SHIPMENT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateFind = @DateFindField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateFind = @DateFindField),
            @FormField(name = "search", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindShipment {}

    @Form(
        name = "ListShipment",
        location = "component://product/widget/facility/ShipmentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindShipment",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "shipmentId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id} ${styles.action_view}", hyperlink = @HyperlinkField(target = "EditShipment", description = "${shipmentId}", parameters = {@ParameterDef(paramName = "shipmentId")})),
            @FormField(name = "shipmentTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "ShipmentType")),
            @FormField(name = "originFacilityId", title = "${uiLabelMap.ProductOriginFacility}", displayEntity = @DisplayEntityField(entityName = "Facility", keyFieldName = "facilityId", description = "${facilityName} [${facilityId}]")),
            @FormField(name = "destinationFacilityId", title = "${uiLabelMap.ProductDestinationFacility}", displayEntity = @DisplayEntityField(entityName = "Facility", keyFieldName = "facilityId", description = "${facilityName} [${facilityId}]")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "estimatedShipDate", title = "${uiLabelMap.ProductEstimatedShipDate}", titleAreaStyle = "text-right", widgetAreaStyle = "text-right", display = @DisplayField(type = "date")),
            @FormField(name = "estimatedArrivalDate", title = "${uiLabelMap.ProductEstimatedArrivalDate}", titleAreaStyle = "text-right", widgetAreaStyle = "text-right", display = @DisplayField(type = "date"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "results", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Shipment"), @FieldMap(fieldName = "orderBy", value = "shipmentId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize"), @FieldMap(fieldName = "noConditionFind", value = "Y")})})
    )
    public interface ListShipment {}

    @Form(
        name = "listShipmentPlan",
        location = "component://product/widget/facility/ShipmentForms.xml",
        type = FormType.LIST,
        listName = "listShipmentPlanRows",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "shipmentItemSeqId", title = "${uiLabelMap.ProductShipmentItemSeqId}", display = @DisplayField),
            @FormField(name = "orderId", title = "${uiLabelMap.ProductOrderId}", display = @DisplayField),
            @FormField(name = "orderItemSeqId", title = "${uiLabelMap.ProductOrderItem}", display = @DisplayField),
            @FormField(name = "shipGroupSeqId", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", display = @DisplayField),
            @FormField(name = "quantity", title = "${uiLabelMap.ProductQuantity}", display = @DisplayField),
            @FormField(name = "issuedQuantity", title = "${uiLabelMap.ProductIssuedQuantity}", display = @DisplayField),
            @FormField(name = "totOrderedQuantity", title = "${uiLabelMap.ProductTotOrderedQuantity}", display = @DisplayField),
            @FormField(name = "notAvailableQuantity", title = "${uiLabelMap.ProductNotAvailable}", display = @DisplayField),
            @FormField(name = "totPlannedQuantity", title = "${uiLabelMap.ProductTotPlannedQuantity}", display = @DisplayField),
            @FormField(name = "totIssuedQuantity", title = "${uiLabelMap.ProductTotIssuedQuantity}", display = @DisplayField),
            @FormField(name = "weight", title = "${uiLabelMap.ProductWeight}", display = @DisplayField),
            @FormField(name = "weightUom", title = "${uiLabelMap.CommonUom}", display = @DisplayField),
            @FormField(name = "volume", title = "${uiLabelMap.CommonVolume}", display = @DisplayField),
            @FormField(name = "volumeUom", title = "${uiLabelMap.CommonUom}", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeOrderShipmentFromShipment", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shipmentId"), @ParameterDef(paramName = "shipmentItemSeqId"), @ParameterDef(paramName = "orderId"), @ParameterDef(paramName = "orderItemSeqId"), @ParameterDef(paramName = "shipGroupSeqId")}))
        }
    )
    public interface listShipmentPlan {}

    @Form(
        name = "addToShipmentPlan",
        location = "component://product/widget/facility/ShipmentForms.xml",
        type = FormType.MULTI,
        target = "addToShipmentPlan?shipmentId=${shipmentId}",
        listName = "addToShipmentPlanRows",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "shipmentId", hidden = @HiddenField),
            @FormField(name = "orderId", hidden = @HiddenField),
            @FormField(name = "orderItemSeqId", hidden = @HiddenField),
            @FormField(name = "orderId", title = "${uiLabelMap.ProductOrderId}", display = @DisplayField),
            @FormField(name = "orderItemSeqId", title = "${uiLabelMap.ProductOrderItem}", display = @DisplayField),
            @FormField(name = "shipGroupSeqId", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.ProductProductId}", display = @DisplayField),
            @FormField(name = "orderedQuantity", title = "${uiLabelMap.ProductOrderedQuantity}", display = @DisplayField),
            @FormField(name = "plannedQuantity", title = "${uiLabelMap.ProductPlannedQuantity}", display = @DisplayField),
            @FormField(name = "issuedQuantity", title = "${uiLabelMap.ProductIssuedQuantity}", display = @DisplayField),
            @FormField(name = "quantity", title = "${uiLabelMap.ProductQuantity}", text = @TextField),
            @FormField(name = "weight", title = "${uiLabelMap.ProductWeight}", display = @DisplayField),
            @FormField(name = "weightUom", title = "${uiLabelMap.CommonUom}", display = @DisplayField),
            @FormField(name = "volume", title = "${uiLabelMap.CommonVolume}", display = @DisplayField),
            @FormField(name = "volumeUom", title = "${uiLabelMap.CommonUom}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface addToShipmentPlan {}

    @Form(
        name = "findOrderItems",
        location = "component://product/widget/facility/ShipmentForms.xml",
        target = "EditShipmentPlan",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "action", hidden = @HiddenField(value = "search")),
            @FormField(name = "shipmentId", hidden = @HiddenField),
            @FormField(name = "orderId", entryName = "shipment.primaryOrderId", title = "${uiLabelMap.ProductOrderId}", lookup = @LookupField(targetFormName = "LookupOrderHeaderAndShipInfo")),
            @FormField(name = "shipGroupSeqId", entryName = "shipment.primaryShipGroupSeqId", text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface findOrderItems {}

    @Form(
        name = "ShipmentReceipts",
        location = "component://product/widget/facility/ShipmentForms.xml",
        type = FormType.LIST,
        listName = "shipmentReceiptList",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "ShipmentReceipt", mapName = "shipmentReceipt", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "shipmentId", hidden = @HiddenField),
            @FormField(name = "orderItemSeqId", hidden = @HiddenField),
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id_long}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId} - ${orderItemSeqId}", parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "inventoryItemId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditInventoryItem", description = "${inventoryItemId}", parameters = {@ParameterDef(paramName = "inventoryItemId")}))
        }
    )
    public interface ShipmentReceipts {}

}

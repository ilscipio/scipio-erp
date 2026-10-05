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
public class FacilityFieldLookupForms {

    @Form(
        name = "lookupFacility",
        location = "component://product/widget/facility/FieldLookupForms.xml",
        target = "LookupFacility",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "facilityId", textFind = @TextFindField),
            @FormField(name = "facilityName", textFind = @TextFindField),
            @FormField(name = "parentFacilityId", textFind = @TextFindField),
            @FormField(name = "facilityTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FacilityType", description = "${description}", keyFieldName = "facilityTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupFacility {}

    @Form(
        name = "listLookupFacility",
        location = "component://product/widget/facility/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupFacility",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "facilityId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_values('${facilityId}', '${facilityName}')", urlMode = UrlMode.PLAIN, description = "${facilityId}", alsoHidden = false)),
            @FormField(name = "facilityName", display = @DisplayField),
            @FormField(name = "facilityTypeId", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Facility"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupFacility {}

    @Form(
        name = "lookupFacilityLocation",
        location = "component://product/widget/facility/FieldLookupForms.xml",
        target = "LookupFacilityLocation",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "FacilityLocation", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "facilityId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName}", keyFieldName = "facilityId", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "locationTypeEnumId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "FACLOC_TYPE")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupFacilityLocation {}

    @Form(
        name = "listLookupFacilityLocation",
        location = "component://product/widget/facility/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupFacilityLocation",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "FacilityLocation", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "locationSeqId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${locationSeqId}')", urlMode = UrlMode.PLAIN, description = "${locationSeqId}", alsoHidden = false)),
            @FormField(name = "facilityId", hidden = @HiddenField),
            @FormField(name = "locationTypeEnumId", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "FacilityLocation"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupFacilityLocation {}

    @Form(
        name = "lookupShipment",
        location = "component://product/widget/facility/FieldLookupForms.xml",
        target = "LookupShipment",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "shipmentId", textFind = @TextFindField),
            @FormField(name = "shipmentTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ShipmentType", description = "${description}"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "SHIPMENT_STATUS")}))),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", textFind = @TextFindField),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyTo}", textFind = @TextFindField),
            @FormField(name = "Datefrom", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "DateThru", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupShipment {}

    @Form(
        name = "listShipment",
        location = "component://product/widget/facility/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupShipment",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "shipmentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${shipmentId}')", urlMode = UrlMode.PLAIN, description = "${shipmentId}", alsoHidden = false)),
            @FormField(name = "shipmentTypeId", displayEntity = @DisplayEntityField(entityName = "ShipmentType")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.PartyPartyFrom}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${lastName}[${partyId}]")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyTo}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${lastName}[${partyId}]"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Shipment"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listShipment {}

    @Form(
        name = "ListProductInventoryLocation",
        location = "component://product/widget/facility/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "LocationList",
        paginateTarget = "ListProductInventoryLocation",
        oddRowStyle = "alternate-row",
        viewSize = 20,
        fields = {
            @FormField(name = "inventoryItemId", display = @DisplayField),
            @FormField(name = "locationSeqId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${locationSeqId}')", urlMode = UrlMode.PLAIN, description = "${locationSeqId}", alsoHidden = false)),
            @FormField(name = "locationTypeEnumId", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId", description = "${description}")),
            @FormField(name = "quantityOnHandTotal", title = "${uiLabelMap.ProductQoh}", display = @DisplayField),
            @FormField(name = "availableToPromiseTotal", title = "${uiLabelMap.ProductAtp}", display = @DisplayField)
        }
    )
    public interface ListProductInventoryLocation {}

}

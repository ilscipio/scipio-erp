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
package com.ilscipio.scipio.accounting.widget;

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
public class AssetsFixedAssetForms {

    @Form(
        name = "ListFixedAssets",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "ListFixedAssets",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "EditFixedAsset", description = "${fixedAssetId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetId")})),
            @FormField(name = "fixedAssetName", title = "${uiLabelMap.CommonName}", sortField = true, display = @DisplayField),
            @FormField(name = "fixedAssetTypeId", title = "${uiLabelMap.CommonType}", sortField = true, displayEntity = @DisplayEntityField(entityName = "FixedAssetType")),
            @FormField(name = "parentFixedAssetId", title = "${uiLabelMap.CommonParent}", sortField = true, displayEntity = @DisplayEntityField(entityName = "FixedAsset", keyFieldName = "fixedAssetId", description = "${fixedAssetName}", subHyperlink = @SubHyperlink(target = "EditFixedAsset", description = "${parentFixedAssetId}", parameters = {@ParameterDef(paramName = "fixedAssetId", fromField = "parentFixedAssetId")}))),
            @FormField(name = "dateAcquired", sortField = true, display = @DisplayField(type = "date")),
            @FormField(name = "expectedEndOfLife", sortField = true, display = @DisplayField(type = "date")),
            @FormField(name = "purchaseCost", titleAreaStyle = "align-right", widgetAreaStyle = "amount", sortField = true, display = @DisplayField(type = "currency")),
            @FormField(name = "salvageValue", titleAreaStyle = "align-right", widgetAreaStyle = "amount", sortField = true, display = @DisplayField(type = "currency")),
            @FormField(name = "depreciation", titleAreaStyle = "align-right", widgetAreaStyle = "amount", sortField = true, display = @DisplayField(type = "currency")),
            @FormField(name = "plannedPastDepreciationTotal", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(description = "${assetDepreciationResultMap.plannedPastDepreciationTotal}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "FixedAsset"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(service = {@ServiceAction(serviceName = "calculateFixedAssetDepreciation", resultMapName = "assetDepreciationResultMap", fieldMaps = {@FieldMap(fieldName = "fixedAssetId", fromField = "fixedAssetId")})}),
        onEventUpdateAreas = {
            @OnEventUpdateArea(eventType = "paginate", areaId = "search-results", areaTarget = "FixedAssetSearchResults")
        }
    )
    public interface ListFixedAssets {}

    @Form(
        name = "FindFixedAssetOptions",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        target = "ListFixedAssets",
        extendsForm = "lookupFixedAsset",
        extendsResource = "component://accounting/widget/FieldLookupForms.xml",
        fields = {
            @FormField(name = "searchOptions_collapsed", hidden = @HiddenField(value = "true")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindFixedAssetOptions {}

    @Form(
        name = "EditFixedAsset",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        target = "updateFixedAsset",
        defaultMapName = "fixedAsset",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fixedAssetId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "fixedAsset!=null", display = @DisplayField),
            @FormField(name = "fixedAssetId", useWhen = "fixedAsset==null&&fixedAssetId==null", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "fixedAssetId", tooltip = "${uiLabelMap.CommonCannotBeFound}:[${fixedAssetId}]", useWhen = "fixedAsset==null&&fixedAssetId!=null", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "fixedAssetTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "FixedAssetType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fixedAssetName", title = "${uiLabelMap.CommonName}", text = @TextField),
            @FormField(name = "parentFixedAssetId", title = "${uiLabelMap.CommonParent}", position = 2, lookup = @LookupField(targetFormName = "LookupFixedAsset")),
            @FormField(name = "instanceOfProductId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "classEnumId", title = "${uiLabelMap.CommonClass}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "FXAST_CLASS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "locatedAtFacilityId", lookup = @LookupField(targetFormName = "LookupFacility")),
            @FormField(name = "locatedAtLocationSeqId", position = 2, lookup = @LookupField(targetFormName = "LookupFacilityLocation")),
            @FormField(name = "productionCapacity", title = "${uiLabelMap.CommonCapacity}", text = @TextField),
            @FormField(name = "uomId", title = "${uiLabelMap.CommonUom}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "UomAndType", description = "${description} [${typeDescription}]", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE", operator = "not-equals")}, orderBy = {@EntityOrderBy(fieldName = "uomTypeId"), @EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "calendarId", title = "${uiLabelMap.CommonCalendar}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "TechDataCalendar", description = "[${calendarId}] ${description}", orderBy = {@EntityOrderBy(fieldName = "calendarId")}))),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPerson")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "dateAcquired", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "acquireOrderId", text = @TextField),
            @FormField(name = "acquireOrderItemSeqId", position = 2, text = @TextField),
            @FormField(name = "dateLastServiced", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "dateNextService", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "expectedEndOfLife", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "actualEndOfLife", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "purchaseCostUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${uomId} - ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "uomTypeId")}))),
            @FormField(name = "purchaseCost", position = 2, text = @TextField),
            @FormField(name = "depreciation", title = "${uiLabelMap.AccountingDepreciation}", position = 2, text = @TextField),
            @FormField(name = "salvageValue", position = 2, text = @TextField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "fixedAssetId!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", useWhen = "fixedAssetId==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "fixedAsset==null", target = "createFixedAsset")
        }
    )
    public interface EditFixedAsset {}

    @Form(
        name = "ListFixedAssetProducts",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        target = "updateFixedAssetProduct",
        listName = "fixedAssetProducts",
        paginateTarget = "ListFixedAssetProducts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", displayEntity = @DisplayEntityField(entityName = "Product", description = "${description}[${productId}]")),
            @FormField(name = "fixedAssetProductTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "FixedAssetProductType")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeFixedAssetProduct", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "fixedAssetProductTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListFixedAssetProducts {}

    @Form(
        name = "AddFixedAssetProduct",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        target = "addFixedAssetProduct",
        defaultMapName = "fixedAssetProduct",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "fixedAssetProductTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAssetProductType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "quantity", title = "${uiLabelMap.CommonQuantity}", text = @TextField),
            @FormField(name = "quantityUomId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "UomAndType", description = "[${typeDescription}] ${description}", keyFieldName = "uomId", orderBy = {@EntityOrderBy(fieldName = "uomTypeId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", text = @TextField),
            @FormField(name = "comments", position = 2, textarea = @TextareaField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface AddFixedAssetProduct {}

    @Form(
        name = "WorkEffortSummary",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        defaultMapName = "workEffort",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "workEffortId", display = @DisplayField),
            @FormField(name = "workEffortName", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "workEffortTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "WorkEffortType", description = "${description}")),
            @FormField(name = "workEffortPurposeTypeId", title = "${uiLabelMap.CommonPurpose}", displayEntity = @DisplayEntityField(entityName = "WorkEffortPurposeType", description = "${description}")),
            @FormField(name = "currentStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}")),
            @FormField(name = "percentComplete", display = @DisplayField),
            @FormField(name = "estimatedStartDate", display = @DisplayField),
            @FormField(name = "estimatedCompletionDate", display = @DisplayField),
            @FormField(name = "actualStartDate", display = @DisplayField),
            @FormField(name = "actualCompletionDate", display = @DisplayField)
        }
    )
    public interface WorkEffortSummary {}

    @Form(
        name = "ListFixedAssetStdCosts",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        target = "updateFixedAssetStdCost",
        listName = "fixedAssetStdCosts",
        paginateTarget = "EditFixedAssetStdCosts",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "FixedAssetStdCost")
        },
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "fixedAssetStdCostTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "FixedAssetStdCostType")),
            @FormField(name = "amountUomId", title = "${uiLabelMap.CommonCurrency}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${uomId} - ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "cancel", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", hyperlink = @HyperlinkField(target = "cancelFixedAssetStdCost", description = "${uiLabelMap.CommonCancel}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "fixedAssetStdCostTypeId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListFixedAssetStdCosts {}

    @Form(
        name = "EditFixedAssetStdCost",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        target = "createFixedAssetStdCost",
        defaultMapName = "fixedAssetStdCost",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "fixedAssetStdCostTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAssetStdCostType", description = "${description}", keyFieldName = "fixedAssetStdCostTypeId"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", text = @TextField),
            @FormField(name = "amountUomId", title = "${uiLabelMap.CommonCurrency}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${uomId} - ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditFixedAssetStdCost {}

    @Form(
        name = "ListFixedAssetIdents",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        target = "updateFixedAssetIdent",
        listName = "fixedAssetIdents",
        paginateTarget = "EditFixedAssetIdents",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateFixedAssetIdent")
        },
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "fixedAssetIdentTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "FixedAssetIdentType")),
            @FormField(name = "idValue", title = "${uiLabelMap.CommonValue}"),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeFixedAssetIdent", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "fixedAssetIdentTypeId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListFixedAssetIdents {}

    @Form(
        name = "AddFixedAssetIdent",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        target = "createFixedAssetIdent",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createFixedAssetIdent")
        },
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "fixedAssetIdentTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "FixedAssetIdentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "idValue", title = "${uiLabelMap.CommonValue}", position = 2, text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFixedAssetIdent {}

    @Form(
        name = "ListFixedAssetRegistrations",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        target = "updateFixedAssetRegistration",
        listName = "fixedAssetRegistrations",
        paginateTarget = "EditFixedAssetRegistrations",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "registrationDate", display = @DisplayField(type = "date")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "registrationNumber", title = "${uiLabelMap.AccountingFixedAssetRegNumber}", text = @TextField(size = 20)),
            @FormField(name = "licenseNumber", title = "${uiLabelMap.AccountingFixedAssetLicenseNumber}", text = @TextField(size = 20)),
            @FormField(name = "govAgencyPartyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteFixedAssetRegistration", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListFixedAssetRegistrations {}

    @Form(
        name = "AddFixedAssetRegistration",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        target = "createFixedAssetRegistration",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.CommonFixedAsset}", hidden = @HiddenField),
            @FormField(name = "registrationDate", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "govAgencyPartyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "registrationNumber", title = "${uiLabelMap.AccountingFixedAssetRegNumber}", position = 2, text = @TextField(size = 20)),
            @FormField(name = "licenseNumber", title = "${uiLabelMap.AccountingFixedAssetLicenseNumber}", position = 2, text = @TextField(size = 20)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFixedAssetRegistration {}

    @Form(
        name = "ListFixedAssetMaints",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginate = "true",
        paginateTarget = "ListFixedAssetMaints",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "maintHistSeqId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditFixedAssetMaint", description = "${maintHistSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "maintHistSeqId")})),
            @FormField(name = "productMaintTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "ProductMaintType")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "intervalMeterTypeId", title = "${uiLabelMap.AccountingFixedAssetMaintIntervalMeterType}", display = @DisplayField),
            @FormField(name = "intervalQuantity", title = "${uiLabelMap.AccountingFixedAssetMaintIntervalQuantity}", display = @DisplayField),
            @FormField(name = "intervalUomId", title = "${uiLabelMap.AccountingFixedAssetMaintIntervalUom}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "FixedAssetMaint"), @FieldMap(fieldName = "orderBy", value = "-maintHistSeqId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListFixedAssetMaints {}

    @Form(
        name = "EditFixedAssetMaint",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        target = "updateFixedAssetMaint",
        defaultMapName = "fixedAssetMaint",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "estimatedStartDate", hidden = @HiddenField(value = "${parameters.estimatedStartDate}")),
            @FormField(name = "estimatedCompletionDate", hidden = @HiddenField(value = "${parameters.estimatedCompletionDate}")),
            @FormField(name = "maintHistSeqId", useWhen = "fixedAssetMaint==null", ignored = @IgnoredField),
            @FormField(name = "maintHistSeqId", useWhen = "fixedAssetMaint!=null", display = @DisplayField),
            @FormField(name = "createdStamp", title = "${uiLabelMap.CommonCreated}", useWhen = "fixedAssetMaint!=null", position = 2, display = @DisplayField),
            @FormField(name = "productMaintSeqId", tooltip = "${uiLabelMap.AccountingFixedAssetMaintMessage2}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductMaint", description = "${maintName}", constraints = {@EntityConstraint(name = "productId", value = "${fixedAsset.instanceOfProductId}")}, orderBy = {@EntityOrderBy(fieldName = "productMaintSeqId")}))),
            @FormField(name = "productMaintTypeId", title = "${uiLabelMap.CommonType}", tooltip = "${uiLabelMap.AccountingFixedAssetMaintMessage1}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductMaintType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "productMaintTypeId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "fixedAssetMaint==null", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "FIXEDAST_MNT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "fixedAssetMaint!=null", position = 2, dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", envName = "fixedAssetMaint.statusId")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "maintTemplateWorkEffortId", title = "${uiLabelMap.AccountingFixedAssetMaintenanceTemplate}", tooltip = "${uiLabelMap.AccountingFixedAssetMaintMessage3}", useWhen = "fixedAssetMaint==null", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "intervalMeterTypeId", title = "${uiLabelMap.AccountingFixedAssetMaintIntervalMeterType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductMeterType", description = "${description}", keyFieldName = "productMeterTypeId", orderBy = {@EntityOrderBy(fieldName = "productMeterTypeId")}))),
            @FormField(name = "intervalQuantity", title = "${uiLabelMap.AccountingFixedAssetMaintIntervalQuantity}", text = @TextField(size = 10)),
            @FormField(name = "intervalUomId", title = "${uiLabelMap.AccountingFixedAssetMaintIntervalUom}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${uiLabelMap.CommonTime}: ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "TIME_FREQ_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "scheduleWorkEffortId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/workeffort/control/WorkEffortSummary", urlMode = UrlMode.INTER_APP, description = "${fixedAssetMaint.scheduleWorkEffortId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId", fromField = "fixedAssetMaint.scheduleWorkEffortId")})),
            @FormField(name = "purchaseOrderId", position = 2, lookup = @LookupField(targetFormName = "LookupOrderHeader")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "fixedAssetMaint==null", target = "createFixedAssetMaint")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "StatusItem", valueField = "currentStatus", autoFieldMap = false)})
    )
    public interface EditFixedAssetMaint {}

    @Form(
        name = "ListFixedAssetMeters",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        target = "updateFixedAssetMeter",
        listName = "listIt",
        paginateTarget = "EditFixedAssetMeters",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "productMeterTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "ProductMeterType", keyFieldName = "productMeterTypeId", description = "${description}")),
            @FormField(name = "readingDate", display = @DisplayField(type = "date")),
            @FormField(name = "meterValue", title = "${uiLabelMap.CommonValue}", text = @TextField),
            @FormField(name = "readingReasonEnumId", title = "${uiLabelMap.CommonReason}", text = @TextField(size = 20)),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "maintHistSeqId", title = "${uiLabelMap.AccountingFixedAssetMaint}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditFixedAssetMaint", description = "${maintHistSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "maintHistSeqId")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonRemove}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteFixedAssetMeter", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "productMeterTypeId"), @ParameterDef(paramName = "readingDate"), @ParameterDef(paramName = "maintHistSeqId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "findParams.fixedAssetId", fromField = "parameters.fixedAssetId"), @SetAction(field = "findParams.maintHistSeqId", fromField = "parameters.maintHistSeqId")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "findParams"), @FieldMap(fieldName = "entityName", value = "FixedAssetMeter"), @FieldMap(fieldName = "orderBy", value = "-readingDate"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListFixedAssetMeters {}

    @Form(
        name = "AddFixedAssetMeter",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        target = "createFixedAssetMeter",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "maintHistSeqId", useWhen = "maintHistSeqId != null", hidden = @HiddenField),
            @FormField(name = "maintHistSeqId", useWhen = "maintHistSeqId == null", ignored = @IgnoredField),
            @FormField(name = "readingDate", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "productMeterTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductMeterType", description = "${description}", keyFieldName = "productMeterTypeId", orderBy = {@EntityOrderBy(fieldName = "productMeterTypeId")}))),
            @FormField(name = "meterValue", title = "${uiLabelMap.CommonValue}", text = @TextField(size = 20)),
            @FormField(name = "readingReasonEnumId", title = "${uiLabelMap.CommonReason}", position = 2, text = @TextField(size = 20)),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", position = 2, lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFixedAssetMeter {}

    @Form(
        name = "ListFixedAssetMaintOrders",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        target = "updateFixedAssetMaintOrder",
        listName = "fixedAssetMaintOrders",
        paginateTarget = "EditFixedAssetMaintOrders",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "FixedAssetMaintOrder")
        },
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "maintHistSeqId", hidden = @HiddenField),
            @FormField(name = "orderId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "orderItemSeqId", display = @DisplayField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteFixedAssetMaintOrder", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "maintHistSeqId"), @ParameterDef(paramName = "orderId"), @ParameterDef(paramName = "orderItemSeqId")}))
        }
    )
    public interface ListFixedAssetMaintOrders {}

    @Form(
        name = "AddFixedAssetMaintOrder",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        target = "createFixedAssetMaintOrder",
        defaultMapName = "fixedAssetMaintOrder",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "maintHistSeqId", hidden = @HiddenField),
            @FormField(name = "orderId", lookup = @LookupField(targetFormName = "LookupOrderHeader")),
            @FormField(name = "orderItemSeqId", text = @TextField(size = 20)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFixedAssetMaintOrder {}

    @Form(
        name = "ListPartyFixedAssetAssignments",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        target = "updatePartyFixedAssetAssignment",
        listName = "listPartyFixedAssets",
        paginateTarget = "EditPartyFixedAssetAssignments",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePartyFixedAssetAssignment")
        },
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${firstName} ${middleName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${partyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "allocatedDate", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PRTYASGN_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "statusId")}))),
            @FormField(name = "comments", title = "${uiLabelMap.CommonComments}", text = @TextField(size = 35)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deletePartyFixedAssetAssignment", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListPartyFixedAssetAssignments {}

    @Form(
        name = "AddPartyFixedAssetAssignment",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        target = "createPartyFixedAssetAssignment",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "roleTypeId")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PRTYASGN_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "statusId")}))),
            @FormField(name = "allocatedDate", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPartyFixedAssetAssignment {}

    @Form(
        name = "AddFixedAssetDepMethod",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        target = "createFixedAssetDepMethod",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createFixedAssetDepMethod", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "depreciationCustomMethodId", title = "${uiLabelMap.CommonMethod}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "CustomMethod", description = "${description}", keyFieldName = "customMethodId", constraints = {@EntityConstraint(name = "customMethodTypeId", value = "DEPRECIATION_FORMULA")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFixedAssetDepMethod {}

    @Form(
        name = "ListFixedAssetDepMethods",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        listName = "fixedAssetDepMethods",
        paginateTarget = "showFixedAssetDepreciation",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "FixedAssetDepMethod", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "depreciationCustomMethodId", title = "${uiLabelMap.CommonMethod}", displayEntity = @DisplayEntityField(entityName = "CustomMethod", keyFieldName = "customMethodId")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteFixedAssetDepMethod", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "depreciationCustomMethodId"), @ParameterDef(paramName = "fromDate")}))
        }
    )
    public interface ListFixedAssetDepMethods {}

    @Form(
        name = "ListFixedAssetDepreciations",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        listName = "assetDepreciationInfoList",
        defaultMapName = "assetDepreciationInfo",
        paginateTarget = "showFixedAssetDepreciation",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "index", display = @DisplayField(description = "${itemIndex + 1}")),
            @FormField(name = "year", display = @DisplayField),
            @FormField(name = "depreciation", title = "${uiLabelMap.AccountingDepreciation}", display = @DisplayField(type = "currency")),
            @FormField(name = "depreciationTotal", display = @DisplayField(type = "currency")),
            @FormField(name = "nbv", title = "Net Book Value", display = @DisplayField(type = "currency"))
        }
    )
    public interface ListFixedAssetDepreciations {}

    @Form(
        name = "FixedAssetTransactions",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        listName = "fixedAssetTransactions",
        paginateTarget = "showFixedAssetDepreciation",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "acctgTransId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditAcctgTrans", description = "${acctgTransId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "acctgTransId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "transTypeDescription", display = @DisplayField),
            @FormField(name = "transactionDate", display = @DisplayField),
            @FormField(name = "accountCode", display = @DisplayField),
            @FormField(name = "accountName", display = @DisplayField),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "debitCreditFlag", display = @DisplayField),
            @FormField(name = "isPosted", display = @DisplayField)
        }
    )
    public interface FixedAssetTransactions {}

    @Form(
        name = "AddFixedAssetTypeGlAccount",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        target = "createFixedAssetTypeGlAccountForFixedAsset",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createFixedAssetTypeGlAccount", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "fixedAssetId", hidden = @HiddenField),
            @FormField(name = "fixedAssetTypeId", hidden = @HiddenField(value = "_NA_")),
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${fixedAsset.partyId}")),
            @FormField(name = "assetGlAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${fixedAsset.partyId}"), @EntityConstraint(name = "glAccountClassId", value = "LONGTERM_ASSET")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "accDepGlAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${fixedAsset.partyId}"), @EntityConstraint(name = "glAccountClassId", value = "ACCUM_DEPRECIATION")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "depGlAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${fixedAsset.partyId}"), @EntityConstraint(name = "glAccountClassId", value = "DEPRECIATION")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "profitGlAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${fixedAsset.partyId}"), @EntityConstraint(name = "glAccountClassId", value = "CASH_INCOME")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "lossGlAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", value = "${fixedAsset.partyId}"), @EntityConstraint(name = "glAccountClassId", value = "SGA_EXPENSE")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddFixedAssetTypeGlAccount {}

    @Form(
        name = "GlobalFixedAssetTypeGlAccounts",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        listName = "globalFixedAssetTypeGlAccounts",
        paginateTarget = "showFixedAssetDepreciation",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "fixedAssetTypeId", displayEntity = @DisplayEntityField(entityName = "FixedAssetType")),
            @FormField(name = "assetGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "accDepGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "depGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "profitGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "lossGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]"))
        }
    )
    public interface GlobalFixedAssetTypeGlAccounts {}

    @Form(
        name = "FixedAssetTypeGlAccounts",
        location = "component://accounting/widget/assets/FixedAssetForms.xml",
        type = FormType.LIST,
        listName = "fixedAssetTypeGlAccounts",
        paginateTarget = "showFixedAssetDepreciation",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "assetGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "accDepGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "depGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "profitGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "lossGlAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", keyFieldName = "glAccountId", description = "${accountCode} - ${accountName} [${glAccountId}]")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteFixedAssetTypeGlAccountForFixedAsset", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "fixedAssetTypeId"), @ParameterDef(paramName = "fixedAssetId"), @ParameterDef(paramName = "organizationPartyId")}))
        }
    )
    public interface FixedAssetTypeGlAccounts {}

}

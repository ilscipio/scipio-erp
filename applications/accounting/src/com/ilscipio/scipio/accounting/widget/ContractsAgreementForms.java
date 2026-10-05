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
public class ContractsAgreementForms {

    @Form(
        name = "FindAgreements",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "FindAgreement",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "agreementTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "AgreementType", description = "${description}"))),
            @FormField(name = "agreementName", title = "${uiLabelMap.CommonName}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", position = 2, textFind = @TextFindField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.CommonParty} ${uiLabelMap.CommonFrom}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.CommonParty} ${uiLabelMap.CommonTo}", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeIdFrom", title = "${uiLabelMap.CommonRole} ${uiLabelMap.CommonFrom}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeIdTo", title = "${uiLabelMap.CommonRole} ${uiLabelMap.CommonTo}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "agreementDate", title = "${uiLabelMap.AccountingAgreementDate}", hidden = @HiddenField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateFind = @DateFindField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateFind = @DateFindField(type = "date")),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindAgreements {}

    @Form(
        name = "ListAgreements",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindAgreement",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", sortField = true, hyperlink = @HyperlinkField(target = "EditAgreement", description = "${agreementId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId")})),
            @FormField(name = "agreementTypeId", title = "${uiLabelMap.CommonType}", sortField = true, displayEntity = @DisplayEntityField(entityName = "AgreementType")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.CommonFrom} ${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_name} ${styles.action_view}", sortField = true, hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyNameResultFrom.fullName}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdFrom")})),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.CommonTo} ${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_name} ${styles.action_view}", sortField = true, hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyNameResultTo.fullName}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdTo")})),
            @FormField(name = "roleTypeIdFrom", title = "${uiLabelMap.CommonFrom} ${uiLabelMap.CommonRole} ", hidden = @HiddenField),
            @FormField(name = "roleTypeIdTo", title = "${uiLabelMap.CommonTo} ${uiLabelMap.CommonRole}", sortField = true, displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", sortField = true, display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", sortField = true, display = @DisplayField(type = "date")),
            @FormField(name = "agreementDate", title = "${uiLabelMap.AccountingAgreementDate}", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", sortField = true, displayEntity = @DisplayEntityField(entityName = "Product", description = "${internalName}")),
            @FormField(name = "textData", title = "${uiLabelMap.AccountingTextData}", hidden = @HiddenField),
            @FormField(name = "description", sortField = true, display = @DisplayField),
            @FormField(name = "cancelAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_terminate}", hyperlink = @HyperlinkField(target = "cancelAgreement", description = "${uiLabelMap.CommonExpire}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId")}))
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "Agreement")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(script = {@ScriptAction(location = "component://accounting/webapp/accounting/WEB-INF/actions/agreement/GetPartyNameForDate.groovy")})
    )
    public interface ListAgreements {}

    @Form(
        name = "EditAgreement",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "updateAgreement",
        defaultMapName = "agreement",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateAgreement", mapName = "agreement", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "agreementId", useWhen = "agreementId!=null", display = @DisplayField),
            @FormField(name = "agreementId", useWhen = "agreement==null&&agreementId==null", ignored = @IgnoredField),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProductId}", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "agreementTypeId", title = "${uiLabelMap.CommonType}", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "AgreementType", description = "${description}", keyFieldName = "agreementTypeId"))),
            @FormField(name = "agreementDate", position = 2, requiredField = true, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.AccountingPartyIdFrom}", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.AccountingPartyIdTo}", position = 2, requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeIdFrom", title = "${uiLabelMap.AccountingRoleTypeIdFrom}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId"))),
            @FormField(name = "roleTypeIdTo", title = "${uiLabelMap.AccountingRoleTypeIdTo}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId"))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "agreement==null", target = "createAgreement")
        }
    )
    public interface EditAgreement {}

    @Form(
        name = "ListAgreementItems",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        listName = "agreementItems",
        paginateTarget = "ListAgreementItems",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "AgreementItem", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.CommonItem} ${uiLabelMap.ProductSeqId}", titleAreaStyle = "align-right", widgetStyle = "${styles.link_nav_info_id}", widgetAreaStyle = "align-right", hyperlink = @HyperlinkField(target = "EditAgreementItem", description = "${agreementItemSeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")})),
            @FormField(name = "agreementItemTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "AgreementItemType")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeAgreementItem", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "agreementId")}))
        }
    )
    public interface ListAgreementItems {}

    @Form(
        name = "EditAgreementItem",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "updateAgreementItem",
        defaultMapName = "agreementItem",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "AgreementItem", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.AccountingAgreementItemSeqId}", useWhen = "agreementItem!=null", display = @DisplayField),
            @FormField(name = "agreementItemSeqId", useWhen = "agreementItem==null", ignored = @IgnoredField),
            @FormField(name = "agreementItemTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "AgreementItemType", description = "${description}", keyFieldName = "agreementItemTypeId"))),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${uomId} - ${description}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "agreementItem==null", target = "createAgreementItem")
        }
    )
    public interface EditAgreementItem {}

    @Form(
        name = "ListAgreementTerms",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        target = "updateAgreementTerm",
        listName = "agreementTerms",
        paginateTarget = "EditAgreementTerms",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", hidden = @HiddenField),
            @FormField(name = "agreementTermId", display = @DisplayField),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.AccountingAgreementItemSeqId}", display = @DisplayField),
            @FormField(name = "termTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "TermType")),
            @FormField(name = "invoiceItemTypeId", title = "${uiLabelMap.AccountingInvoice} ${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "InvoiceItemType", description = "${description}")),
            @FormField(name = "minQuantity", title = "${uiLabelMap.Qty}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "maxQuantity", title = " ", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "termValue", title = "${uiLabelMap.CommonValue}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "termDays", title = "${uiLabelMap.CommonDays}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "textValue", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "editAction", title = " ", widgetStyle = "${styles.link_nav} ${styles.action_update}", hyperlink = @HyperlinkField(target = "EditAgreementItemTerm", description = "${uiLabelMap.CommonEdit}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementTermId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "agreementId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteAgreementTerm", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementTermId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "agreementId")}))
        }
    )
    public interface ListAgreementTerms {}

    @Form(
        name = "AddAgreementTerm",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "createAgreementTerm",
        defaultMapName = "agreementTerm",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "agreementTermId", title = "${uiLabelMap.AccountingAgreementTermId}", hidden = @HiddenField),
            @FormField(name = "termTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TermType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "invoiceItemTypeId", title = "${uiLabelMap.AccountingInvoice} ${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "InvoiceItemType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "minQuantity", text = @TextField),
            @FormField(name = "maxQuantity", position = 2, text = @TextField),
            @FormField(name = "termDays", text = @TextField),
            @FormField(name = "termValue", position = 2, text = @TextField),
            @FormField(name = "textValue", text = @TextField),
            @FormField(name = "description", textarea = @TextareaField(cols = 6)),
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.AccountingAgreementItemSeqId}", hidden = @HiddenField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddAgreementTerm {}

    @Form(
        name = "EditAgreementGeographicalApplic",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "updateAgreementGeographicalApplic",
        defaultMapName = "agreementGeographicalApplic",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "AgreementGeographicalApplic", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "agreementId", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", hidden = @HiddenField),
            @FormField(name = "geoId", useWhen = "agreementGeographicalApplic==null", requiredField = true, lookup = @LookupField(targetFormName = "LookupGeo")),
            @FormField(name = "geoId", useWhen = "agreementGeographicalApplic!=null", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "agreementGeographicalApplic==null", target = "createAgreementGeographicalApplic")
        }
    )
    public interface EditAgreementGeographicalApplic {}

    @Form(
        name = "ListAgreementGeographicalApplic",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        listName = "agreementGeographicalApplics",
        paginateTarget = "ListAgreementGeographicalApplic",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "AgreementGeographicalApplic", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "agreementId", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", hidden = @HiddenField),
            @FormField(name = "geoId", title = "${uiLabelMap.CommonDescription}", displayEntity = @DisplayEntityField(entityName = "Geo", keyFieldName = "geoId", description = "${geoName} [${geoId}]")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeAgreementGeographicalApplic", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "geoId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")}))
        }
    )
    public interface ListAgreementGeographicalApplic {}

    @Form(
        name = "EditAgreementItemFacility",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "updateAgreementItemFacility",
        defaultMapName = "agreementFacilityAppl",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "AgreementFacilityAppl", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "agreementId", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", hidden = @HiddenField),
            @FormField(name = "facilityId", useWhen = "agreementFacilityAppl==null", lookup = @LookupField(targetFormName = "LookupFacility")),
            @FormField(name = "facilityId", useWhen = "agreementFacilityAppl!=null", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "agreementFacilityAppl==null", target = "createAgreementItemFacility")
        }
    )
    public interface EditAgreementItemFacility {}

    @Form(
        name = "ListAgreementItemFacilities",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        listName = "agreementFacilities",
        paginateTarget = "ListAgreementItemFacilities",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.AccountingAgreementItemSeqId}", hidden = @HiddenField),
            @FormField(name = "facilityId", title = "${uiLabelMap.Facility}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "EditAgreementItemFacility", description = "${facilityId} - ${facility.facilityName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "facilityId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeAgreementItemFacility", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "facilityId")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "Facility", valueField = "facility")})
    )
    public interface ListAgreementItemFacilities {}

    @Form(
        name = "ViewAgreementInfoForReport",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        defaultMapName = "agreement",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "agreementId", text = @TextField),
            @FormField(name = "partyIdFrom", text = @TextField),
            @FormField(name = "partyIdTo", displayEntity = @DisplayEntityField(entityName = "PartyGroup", keyFieldName = "partyId", description = "${partyId} - ${groupName}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.AccountingThruDate}", position = 2, display = @DisplayField(type = "date")),
            @FormField(name = "description", display = @DisplayField)
        }
    )
    public interface ViewAgreementInfoForReport {}

    @Form(
        name = "ViewAgreementItemInfoForReport",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        defaultMapName = "agreementItem",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "agreementItemSeqId", display = @DisplayField),
            @FormField(name = "currencyUomId", display = @DisplayField)
        }
    )
    public interface ViewAgreementItemInfoForReport {}

    @Form(
        name = "EditAgreementItemParty",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "updateAgreementItemParty",
        defaultMapName = "agreementPartyApplic",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "AgreementPartyApplic", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "agreementId", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", hidden = @HiddenField),
            @FormField(name = "partyId", useWhen = "agreementPartyApplic==null", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyId", useWhen = "agreementPartyApplic!=null", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "agreementPartyApplic==null", target = "createAgreementItemParty")
        }
    )
    public interface EditAgreementItemParty {}

    @Form(
        name = "ListAgreementItemParties",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        listName = "agreementParties",
        paginateTarget = "ListAgreementItemParties",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.AccountingAgreementItemSeqId}", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_idname_long}", hyperlink = @HyperlinkField(target = "EditAgreementItemParty", description = "${partyId} - ${party.groupName} ${party.firstName} ${party.lastName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")})),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeAgreementItemParty", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "PartyNameView", valueField = "party")})
    )
    public interface ListAgreementItemParties {}

    @Form(
        name = "EditAgreementItemProduct",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "updateAgreementItemProduct",
        defaultMapName = "agreementProductAppl",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "AgreementProductAppl", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "agreementId", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", useWhen = "agreementProductAppl==null", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", useWhen = "agreementProductAppl!=null", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "agreementProductAppl==null", target = "createAgreementItemProduct")
        }
    )
    public interface EditAgreementItemProduct {}

    @Form(
        name = "ListAgreementItemProducts",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        listName = "agreementProducts",
        paginateTarget = "ListAgreementItemProducts",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.AccountingAgreementItemSeqId}", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", displayEntity = @DisplayEntityField(entityName = "Product", description = " ", subHyperlink = @SubHyperlink(target = "EditAgreementItemProduct", description = "${productId} - ${product.internalName}", linkStyle = "${styles.link_nav_info_idname}", parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "productId")}))),
            @FormField(name = "price", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeAgreementItemProduct", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "Product", valueField = "product")})
    )
    public interface ListAgreementItemProducts {}

    @Form(
        name = "ListAgreementItemProductsForReport",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        listName = "agreementProducts",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", titleAreaStyle = "tableheadmedium", display = @DisplayField),
            @FormField(name = "internalName", entryName = "productId", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${internalName}")),
            @FormField(name = "price", titleAreaStyle = "tableheadmedium", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ListAgreementItemProductsForReport {}

    @Form(
        name = "EditAgreementPromoAppl",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "updateAgreementPromoAppl",
        defaultMapName = "agreementPromoAppl",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "AgreementPromoAppl", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.AccountingAgreementItemSeqId}", hidden = @HiddenField),
            @FormField(name = "productPromoId", title = "${uiLabelMap.FormFieldTitle_productPromoId}", useWhen = "agreementPromoAppl!=null", displayEntity = @DisplayEntityField(entityName = "ProductPromo", keyFieldName = "productPromoId", description = "${promoName}")),
            @FormField(name = "productPromoId", useWhen = "agreementPromoAppl==null", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ProductPromo", description = "${promoName}", keyFieldName = "productPromoId"))),
            @FormField(name = "fromDate", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", positionSpan = 1, text = @TextField),
            @FormField(name = "fromDate", useWhen = "agreementPromoAppl!=null", display = @DisplayField),
            @FormField(name = "fromDate", useWhen = "agreementPromoAppl==null", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "agreementPromoAppl==null", target = "createAgreementPromoAppl")
        }
    )
    public interface EditAgreementPromoAppl {}

    @Form(
        name = "ListAgreementPromoAppls",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        listName = "agreementPromoAppls",
        paginateTarget = "ListAgreementPromoAppls",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.AccountingAgreementItemSeqId}", hidden = @HiddenField),
            @FormField(name = "productPromoId", title = "${uiLabelMap.ProductPromotion}", widgetStyle = "${styles.link_nav_info_idname}", hyperlink = @HyperlinkField(target = "EditAgreementPromoAppl", description = "${productPromoId} - ${productPromo.promoName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "productPromoId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "sequenceNum", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeAgreementPromoAppl", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productPromoId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "fromDate")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "ProductPromo", valueField = "productPromo")})
    )
    public interface ListAgreementPromoAppls {}

    @Form(
        name = "EditAgreementItemSupplierProduct",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "updateAgreementItemSupplierProduct",
        defaultMapName = "agreementProductAppl",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SupplierProduct", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "agreementId", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", hidden = @HiddenField),
            @FormField(name = "partyId", hidden = @HiddenField(value = "${agreement.partyIdTo}")),
            @FormField(name = "currencyUomId", hidden = @HiddenField(value = "${agreementItem.currencyUomId}")),
            @FormField(name = "availableFromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "agreementProductAppl==null", dateTime = @DateTimeField(defaultValue = "${agreement.fromDate}")),
            @FormField(name = "availableFromDate", title = "${uiLabelMap.CommonFrom}", useWhen = "agreementProductAppl!=null", display = @DisplayField),
            @FormField(name = "minimumOrderQuantity", useWhen = "agreementProductAppl==null", text = @TextField(size = 5, defaultValue = "0")),
            @FormField(name = "minimumOrderQuantity", useWhen = "agreementProductAppl!=null", display = @DisplayField),
            @FormField(name = "productId", useWhen = "agreementProductAppl==null", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productId", useWhen = "agreementProductAppl!=null", display = @DisplayField),
            @FormField(name = "supplierPrefOrderId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SupplierPrefOrder", description = "${description}", keyFieldName = "supplierPrefOrderId", orderBy = {@EntityOrderBy(fieldName = "supplierPrefOrderId")}))),
            @FormField(name = "supplierRatingTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "SupplierRatingType", description = "${description}", keyFieldName = "supplierRatingTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "quantityUomId", title = "${uiLabelMap.ProductQuantityUomId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "UomAndType", description = "${typeDescription}: ${description} (${abbreviation})", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE", operator = "not-equals")}, orderBy = {@EntityOrderBy(fieldName = "typeDescription"), @EntityOrderBy(fieldName = "uomId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "agreementProductAppl==null", target = "createAgreementItemSupplierProduct")
        }
    )
    public interface EditAgreementItemSupplierProduct {}

    @Form(
        name = "ListAgreementItemSupplierProducts",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        listName = "agreementProducts",
        paginateTarget = "ListAgreementItemSupplierProducts",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "SupplierProduct", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "agreementId", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", hidden = @HiddenField),
            @FormField(name = "partyId", hidden = @HiddenField),
            @FormField(name = "currencyUomId", hidden = @HiddenField),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditAgreementItemSupplierProduct", description = "${productId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "minimumOrderQuantity"), @ParameterDef(paramName = "currencyUomId"), @ParameterDef(paramName = "availableFromDate"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")})),
            @FormField(name = "internalName", entryName = "productId", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${internalName}")),
            @FormField(name = "lastPrice", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeAgreementItemSupplierProduct", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "minimumOrderQuantity"), @ParameterDef(paramName = "currencyUomId"), @ParameterDef(paramName = "availableFromDate"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "productId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")}))
        }
    )
    public interface ListAgreementItemSupplierProducts {}

    @Form(
        name = "ListAgreementItemSupplierProductsForReport",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        listName = "agreementProducts",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "productId", titleAreaStyle = "tableheadmedium", display = @DisplayField),
            @FormField(name = "supplierProductId", titleAreaStyle = "tableheadmedium", display = @DisplayField),
            @FormField(name = "internalName", entryName = "productId", displayEntity = @DisplayEntityField(entityName = "Product", keyFieldName = "productId", description = "${internalName}")),
            @FormField(name = "lastPrice", titleAreaStyle = "tableheadmedium", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ListAgreementItemSupplierProductsForReport {}

    @Form(
        name = "EditAgreementItemTerm",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "updateAgreementItemTerm",
        defaultMapName = "agreementTerm",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", display = @DisplayField),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.AccountingAgreementItemSeqId}", position = 2, display = @DisplayField),
            @FormField(name = "agreementTermId", title = "${uiLabelMap.AccountingAgreementTermId}", display = @DisplayField),
            @FormField(name = "termTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TermType", description = "${description}"))),
            @FormField(name = "invoiceItemTypeId", title = "${uiLabelMap.AccountingInvoice} ${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "InvoiceItemType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "minQuantity", text = @TextField),
            @FormField(name = "maxQuantity", position = 2, text = @TextField),
            @FormField(name = "termDays", text = @TextField),
            @FormField(name = "termValue", position = 2, text = @TextField),
            @FormField(name = "textValue", text = @TextField),
            @FormField(name = "description", textarea = @TextareaField(cols = 5)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "agreementTerm==null", target = "createAgreementItemTerm")
        }
    )
    public interface EditAgreementItemTerm {}

    @Form(
        name = "ListAgreementItemTerms",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        listName = "agreementTerms",
        paginateTarget = "ListAgreementItemTerms",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", title = "${uiLabelMap.AccountingAgreementId}", hidden = @HiddenField),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.AccountingAgreementItemSeqId}", hidden = @HiddenField),
            @FormField(name = "agreementTermId", title = " ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditAgreementItemTerm", description = "${agreementTermId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementTermId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")})),
            @FormField(name = "termTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "TermType")),
            @FormField(name = "invoiceItemTypeId", title = "${uiLabelMap.AccountingInvoice} ${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "InvoiceItemType")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeAgreementItemTerm", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementTermId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")}))
        }
    )
    public interface ListAgreementItemTerms {}

    @Form(
        name = "AddAgreementWorkEffortApplic",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "createAgreementWorkEffortApplic",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "agreementId", hidden = @HiddenField(value = "${agreementId}")),
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.CommonItem} ${uiLabelMap.ProductSeqId}", dropDown = @DropDownField(options = {@Option(key = "_NA_", description = "${uiLabelMap.CommonNA}")}, entityOptions = @EntityOptions(entityName = "AgreementItem", description = "${agreementItemSeqId}", constraints = {@EntityConstraint(name = "agreementId", envName = "agreementId")}, orderBy = {@EntityOrderBy(fieldName = "agreementItemSeqId")}))),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", position = 2, lookup = @LookupField(targetFormName = "LookupWorkEffort")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddAgreementWorkEffortApplic {}

    @Form(
        name = "ListAgreementWorkEffortApplics",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        target = "updateAgreementWorkEffortApplic",
        listName = "agreementWorkEffortApplics",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementItemSeqId", title = "${uiLabelMap.CommonItem} ${uiLabelMap.ProductSeqId}", display = @DisplayField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", displayEntity = @DisplayEntityField(entityName = "WorkEffort", description = "${workEffortName}")),
            @FormField(name = "workEffortTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "WorkEffortType", description = "${description}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteAgreementWorkEffortApplic", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId"), @ParameterDef(paramName = "workEffortId")}))
        },
        rowActions = @RowActions(set = {@SetAction(field = "statusId", fromField = "workEffort.currentStatusId"), @SetAction(field = "workEffortTypeId", fromField = "workEffort.workEffortTypeId")}, entityOne = {@EntityOneAction(entityName = "WorkEffort", valueField = "workEffort")})
    )
    public interface ListAgreementWorkEffortApplics {}

    @Form(
        name = "ListAgreementRoles",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        type = FormType.LIST,
        listName = "agreementRoles",
        paginateTarget = "EditAgreementRoles",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "AgreementRole", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "agreementId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${firstName} ${lastName}")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", displayEntity = @DisplayEntityField(entityName = "RoleType", description = "${description}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteAgreementRole", description = "${uiLabelMap.CommonRemove}", alsoHidden = false, parameters = {@ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId")}))
        }
    )
    public interface ListAgreementRoles {}

    @Form(
        name = "AddAgreementRole",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "createAgreementRole",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "AgreementRole", defaultFieldType = DefaultFieldType.EDIT)
        },
        fields = {
            @FormField(name = "agreementId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddAgreementRole {}

    @Form(
        name = "CopyAgreement",
        location = "component://accounting/widget/contracts/AgreementForms.xml",
        target = "copyAgreement",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "agreementId", hidden = @HiddenField),
            @FormField(name = "copyAgreementTerms", title = "${uiLabelMap.AccountingAgreementTerms}", check = @CheckField(noCurrentSelectedKey = "Y", options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "copyAgreementProducts", title = "${uiLabelMap.ProductProducts}", position = 2, check = @CheckField(noCurrentSelectedKey = "Y", options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "copyAgreementParties", title = "${uiLabelMap.Party}", check = @CheckField(noCurrentSelectedKey = "Y", options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "copyAgreementFacilities", title = "${uiLabelMap.ProductFacilities}", position = 2, check = @CheckField(noCurrentSelectedKey = "Y", options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCopy}", widgetStyle = "${styles.link_run_sys} ${styles.action_copy}", submit = @SubmitField)
        }
    )
    public interface CopyAgreement {}

}

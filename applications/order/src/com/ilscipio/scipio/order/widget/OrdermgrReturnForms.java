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
public class OrdermgrReturnForms {

    @Form(
        name = "EditReturn",
        location = "component://order/widget/ordermgr/ReturnForms.xml",
        target = "updateReturn",
        id = "editReturn",
        defaultMapName = "returnHeader",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateReturnHeader")
        },
        fields = {
            @FormField(name = "returnId", tooltip = "${uiLabelMap.OrderReturnCannotBeChanged}", useWhen = "returnHeader!=null", display = @DisplayField),
            @FormField(name = "returnId", tooltip = "${uiLabelMap.OrderReturnCannotBeFound} ${returnId}", useWhen = "returnHeader==null&&returnId!=null", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "returnId", useWhen = "returnHeader==null&&returnId==null", ignored = @IgnoredField),
            @FormField(name = "returnHeaderTypeId", idName = "returnHeaderTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "ReturnHeaderType", description = "${description}"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "returnHeader==null", idName = "statusId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_RETURN_STTS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}), listOptions = @ListOptions(listName = "returnStatus", keyName = "statusIdTo", description = "${transitionName}"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "returnHeader!=null", dropDown = @DropDownField(currentDescription = "${currentStatus.description}", entityOptions = @EntityOptions(entityName = "StatusValidChangeToDetail", description = "${transitionName} (${description})", keyFieldName = "statusIdTo", constraints = {@EntityConstraint(name = "statusId", envName = "returnHeader.statusId")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "currencyUomId", useWhen = "returnHeader==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "currencyUomId", useWhen = "returnHeader!=null", displayEntity = @DisplayEntityField(entityName = "Uom", keyFieldName = "uomId", description = "${abbreviation} - ${description}")),
            @FormField(name = "destinationFacilityId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Facility", description = "${facilityName}", keyFieldName = "facilityId", orderBy = {@EntityOrderBy(fieldName = "facilityName")}))),
            @FormField(name = "fromPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "toPartyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "originContactMechId", useWhen = "returnHeader!=null", displayEntity = @DisplayEntityField(entityName = "PostalAddress", keyFieldName = "contactMechId", description = "${toName} ${attnName} ${address1} ${address2} ${city} ${stateProvinceGeoId} ${postalCode} ${countryGeoId}")),
            @FormField(name = "originContactMechId", useWhen = "returnHeader!=null&&\"RETURN_REQUESTED\".equals(returnHeader.getString(\"statusId\"))", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "addresses", keyName = "postalAddress.contactMechId", description = "${postalAddress.toName} ${postalAddress.attnName} ${postalAddress.address1} ${postalAddress.address2} ${postalAddress.city} ${postalAddress.stateProvinceGeoId} ${postalAddress.postalCode} ${postalAddress.countryGeoId}"))),
            @FormField(name = "billingAccountId", useWhen = "billingAccountList!=null&&billingAccountList.size()>0", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "billingAccountList", keyName = "billingAccountId", description = "${billingAccountId}: ${description}"))),
            @FormField(name = "finAccountId", useWhen = "finAccounts!=null&&finAccounts.size()>0", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "finAccounts", keyName = "finAccountId", description = "${finAccountId}: ${finAccountName}"))),
            @FormField(name = "paymentMethodId", useWhen = "creditCardList!=null&&creditCardList.size()>0", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "creditCardList", listEntryName = "creditCardPm", keyName = "creditCardPm.paymentMethodId", description = "${groovy:org.ofbiz.party.contact.ContactHelper.formatCreditCard(creditCardPm.getRelatedOne(\"CreditCard\", false))}"))),
            @FormField(name = "newCreditCardAction", useWhen = "returnHeader!=null&&returnHeader.getString(\"fromPartyId\")!=null", widgetStyle = "${styles.link_nav} ${styles.action_add}", hyperlink = @HyperlinkField(target = "/partymgr/control/editcreditcard", urlMode = UrlMode.INTER_APP, description = "${uiLabelMap.AccountingCreateNewCreditCard}", alsoHidden = false, targetWindow = "partymgr", parameters = {@ParameterDef(paramName = "partyId", fromField = "returnHeader.fromPartyId")})),
            @FormField(name = "needsInventoryReceive", tooltip = "${uiLabelMap.OrderReturnNecessaryReceiveInventoryMessage}", dropDown = @DropDownField(options = {@Option(key = "N"), @Option(key = "Y")})),
            @FormField(name = "createdBy", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "returnHeader==null", target = "createReturn")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "StatusItem", valueField = "currentStatus", autoFieldMap = false)})
    )
    public interface EditReturn {}

    @Form(
        name = "FindReturns",
        location = "component://order/widget/ordermgr/ReturnForms.xml",
        target = "findreturn",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "returnHeaderTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "ReturnHeaderType", description = "${description}"))),
            @FormField(name = "returnId", title = "${uiLabelMap.OrderReturnId}", textFind = @TextFindField),
            @FormField(name = "fromPartyId", title = "${uiLabelMap.OrderReturnFromParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "billingAccountId", title = "${uiLabelMap.AccountingBillingAccount}", textFind = @TextFindField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "ORDER_RETURN_STTS")}))),
            @FormField(name = "entryDate", title = "${uiLabelMap.OrderEntryDate}", dateFind = @DateFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindReturns {}

    @Form(
        name = "ListReturns",
        location = "component://order/widget/ordermgr/ReturnForms.xml",
        type = FormType.LIST,
        target = "findreturn",
        listName = "listIt",
        paginateTarget = "findreturn",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "returnId", title = "${uiLabelMap.OrderReturnId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "returnMain", description = "${returnId}", parameters = {@ParameterDef(paramName = "returnId")})),
            @FormField(name = "entryDate", title = "${uiLabelMap.OrderEntryDate}", display = @DisplayField),
            @FormField(name = "fromPartyId", title = "${uiLabelMap.OrderReturnFromParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${firstName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "${fromPartyId}", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "fromPartyId")}))),
            @FormField(name = "destinationFacilityId", title = "${uiLabelMap.OrderReturnDestinationFacility}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/facility/control/EditFacility", urlMode = UrlMode.INTER_APP, description = "${destinationFacilityId}", parameters = {@ParameterDef(paramName = "facilityId", fromField = "destinationFacilityId")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId", description = "${description}"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", value = "ReturnHeader"), @FieldMap(fieldName = "orderBy", value = "entryDate DESC"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListReturns {}

    @Form(
        name = "ReturnStatusHistory",
        location = "component://order/widget/ordermgr/ReturnForms.xml",
        type = FormType.LIST,
        listName = "orderReturnStatusHistories",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "returnId", display = @DisplayField),
            @FormField(name = "returnItemSeqId", display = @DisplayField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "statusDatetime", title = "${uiLabelMap.CommonDate}", display = @DisplayField),
            @FormField(name = "changeByUserLoginId", title = "${uiLabelMap.FormFieldTitle_modifiedByUserLoginId}", display = @DisplayField)
        }
    )
    public interface ReturnStatusHistory {}

    @Form(
        name = "ReturnAndReceivedQuantityHistory",
        location = "component://order/widget/ordermgr/ReturnForms.xml",
        type = FormType.LIST,
        listName = "orderReturnItemHistories",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "returnId", display = @DisplayField(description = "${returnId}")),
            @FormField(name = "oldValueText", display = @DisplayField),
            @FormField(name = "newValueText", display = @DisplayField),
            @FormField(name = "changedDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField),
            @FormField(name = "changedByInfo", title = "${uiLabelMap.FormFieldTitle_modifiedByUserLoginId}", display = @DisplayField)
        }
    )
    public interface ReturnAndReceivedQuantityHistory {}

    @Form(
        name = "ReturnReasonHistory",
        location = "component://order/widget/ordermgr/ReturnForms.xml",
        type = FormType.LIST,
        listName = "orderReturnItemHistories",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "returnId", display = @DisplayField(description = "${returnId}")),
            @FormField(name = "oldValueText", displayEntity = @DisplayEntityField(entityName = "ReturnReason", keyFieldName = "returnReasonId", description = "${description}")),
            @FormField(name = "newValueText", displayEntity = @DisplayEntityField(entityName = "ReturnReason", keyFieldName = "returnReasonId", description = "${description}")),
            @FormField(name = "changedDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField),
            @FormField(name = "changedByInfo", title = "${uiLabelMap.FormFieldTitle_modifiedByUserLoginId}", display = @DisplayField)
        }
    )
    public interface ReturnReasonHistory {}

    @Form(
        name = "ReturnTypeHistory",
        location = "component://order/widget/ordermgr/ReturnForms.xml",
        type = FormType.LIST,
        listName = "orderReturnItemHistories",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "returnId", display = @DisplayField(description = "${returnId}")),
            @FormField(name = "oldValueText", displayEntity = @DisplayEntityField(entityName = "ReturnType", keyFieldName = "returnTypeId", description = "${description}")),
            @FormField(name = "newValueText", displayEntity = @DisplayEntityField(entityName = "ReturnType", keyFieldName = "returnTypeId", description = "${description}")),
            @FormField(name = "changedDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField),
            @FormField(name = "changedByInfo", title = "${uiLabelMap.FormFieldTitle_modifiedByUserLoginId}", display = @DisplayField)
        }
    )
    public interface ReturnTypeHistory {}

    @Form(
        name = "ReturnPriceHistory",
        location = "component://order/widget/ordermgr/ReturnForms.xml",
        type = FormType.LIST,
        listName = "orderReturnItemHistories",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "returnId", display = @DisplayField(description = "${returnId}")),
            @FormField(name = "oldValueText", display = @DisplayField(type = "currency")),
            @FormField(name = "newValueText", display = @DisplayField(type = "currency")),
            @FormField(name = "changedDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField),
            @FormField(name = "changedByInfo", title = "${uiLabelMap.FormFieldTitle_modifiedByUserLoginId}", display = @DisplayField)
        }
    )
    public interface ReturnPriceHistory {}

}

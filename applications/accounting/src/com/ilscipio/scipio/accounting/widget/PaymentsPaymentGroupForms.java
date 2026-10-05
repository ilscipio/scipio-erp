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
public class PaymentsPaymentGroupForms {

    @Form(
        name = "FindPaymentGroup",
        location = "component://accounting/widget/payments/PaymentGroupForms.xml",
        target = "FindPaymentGroup",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGroup", defaultFieldType = DefaultFieldType.FIND)
        },
        fields = {
            @FormField(name = "paymentGroupId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "paymentGroupTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PaymentGroupType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPaymentGroup {}

    @Form(
        name = "ListPaymentGroup",
        location = "component://accounting/widget/payments/PaymentGroupForms.xml",
        type = FormType.LIST,
        listName = "paymentGroupList",
        defaultEntityName = "PaymentGroup",
        paginate = "true",
        paginateTarget = "FindPaymentGroup",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "paymentGroupId", title = "${uiLabelMap.CommonId} - ", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "PaymentGroupOverview", description = "${paymentGroupId}", parameters = {@ParameterDef(paramName = "paymentGroupId")})),
            @FormField(name = "paymentGroupName", title = "${uiLabelMap.CommonName}", display = @DisplayField(description = "${paymentGroupName}")),
            @FormField(name = "paymentGroupTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "PaymentGroupType")),
            @FormField(name = "depositSlipAction", title = "${uiLabelMap.AccountingDepositSlip}", useWhen = "${paymentGroupTypeId == 'BATCH_PAYMENT'} @and ${groovy:org.ofbiz.base.util.UtilValidate.isNotEmpty(paymentGroupMembers)}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", hyperlink = @HyperlinkField(target = "DepositSlip.pdf", description = "${uiLabelMap.AccountingInvoicePDF}", alsoHidden = false, targetWindow = "_BLANK", parameters = {@ParameterDef(paramName = "paymentGroupId")})),
            @FormField(name = "printCheckAction", title = "${uiLabelMap.CommonPdf}", useWhen = "${paymentGroupTypeId == 'CHECK_RUN'} @and ${groovy:org.ofbiz.base.util.UtilValidate.isNotEmpty(paymentGroupMembers)}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", hyperlink = @HyperlinkField(target = "printChecks.pdf", description = "${uiLabelMap.AccountingInvoicePDF}", alsoHidden = false, targetWindow = "_BLANK", parameters = {@ParameterDef(paramName = "paymentGroupId")})),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonCancel}", useWhen = "${paymentGroupTypeId == 'BATCH_PAYMENT'} @and ${paymentGroupMemberAndTransList[0].finAccountTransStatusId != 'FINACT_TRNS_APPROVED'} @and ${groovy:org.ofbiz.base.util.UtilValidate.isNotEmpty(paymentGroupMembers)}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "cancelPaymentGroup", description = "${uiLabelMap.CommonCancel}", alsoHidden = false, parameters = {@ParameterDef(paramName = "paymentGroupId")})),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonCancel}", useWhen = "${paymentGroupTypeId == 'CHECK_RUN'} @and ${paymentGroupMemberAndTransList[0].finAccountTransStatusId != 'FINACT_TRNS_APPROVED'} @and ${groovy:org.ofbiz.base.util.UtilValidate.isNotEmpty(paymentGroupMembers)}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "cancelCheckRunPayments", description = "${uiLabelMap.CommonCancel}", alsoHidden = false, parameters = {@ParameterDef(paramName = "paymentGroupId")}))
        }
    )
    public interface ListPaymentGroup {}

    @Form(
        name = "AddPaymentGroup",
        location = "component://accounting/widget/payments/PaymentGroupForms.xml",
        target = "createPaymentGroup",
        defaultMapName = "paymentGroup",
        fields = {
            @FormField(name = "paymentGroupName", title = "${uiLabelMap.AccountingPaymentGroupName}", text = @TextField),
            @FormField(name = "paymentGroupTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentGroupType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPaymentGroup {}

    @Form(
        name = "EditPaymentGroup",
        location = "component://accounting/widget/payments/PaymentGroupForms.xml",
        target = "updatePaymentGroup",
        defaultMapName = "paymentGroup",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updatePaymentGroup")
        },
        fields = {
            @FormField(name = "paymentGroupId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "paymentGroup!=null", display = @DisplayField),
            @FormField(name = "paymentGroupId", useWhen = "paymentGroup==null @and paymentGroupId!=null", display = @DisplayField(description = "${uiLabelMap.CommonCannotBeFound}: [${paymentGroupId}]", alsoHidden = false)),
            @FormField(name = "paymentGroupId", useWhen = "display==true", display = @DisplayField),
            @FormField(name = "paymentGroupTypeId", title = "${uiLabelMap.CommonType}", position = 2, displayEntity = @DisplayEntityField(entityName = "PaymentGroupType", description = "${description}")),
            @FormField(name = "paymentGroupTypeId", title = "${uiLabelMap.CommonType}", useWhen = "paymentGroup==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentGroupType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "finAccountName", title = "${uiLabelMap.FormFieldTitle_finAccountName}", useWhen = "finAccount!=null", position = 2, display = @DisplayField(description = "${finAccount.finAccountName}")),
            @FormField(name = "ownerPartyId", title = "${uiLabelMap.FormFieldTitle_ownerPartyId}", useWhen = "finAccount!=null", position = 2, display = @DisplayField(description = "${finAccount.ownerPartyId}")),
            @FormField(name = "paymentGroupName", useWhen = "display==true", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "paymentGroup!=null @and display==false", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "paymentGroup==null", target = "createPaymentGroup")
        },
        actions = @FormActions(entityOne = {@EntityOneAction(entityName = "FinAccount", valueField = "finAccount")})
    )
    public interface EditPaymentGroup {}

    @Form(
        name = "ListPaymentGroupMember",
        location = "component://accounting/widget/payments/PaymentGroupForms.xml",
        type = FormType.LIST,
        target = "updatePaymentGroupMember",
        listName = "paymentGroupMembers",
        paginateTarget = "EditPaymentGroupMember",
        headerRowStyle = "header-row",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "paymentGroupId", hidden = @HiddenField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "paymentOverview", description = "${paymentId}", parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "paymentRefNum", title = "${uiLabelMap.AccountingReferenceNumber}", display = @DisplayField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.CommonFrom}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${lastName} ${firstName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${partyIdFrom}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdFrom")}))),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.CommonTo}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${lastName} ${firstName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${partyIdTo}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdTo")}))),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "PaymentType", description = "${description}")),
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.CommonMethod}", useWhen = "cardType!=null", displayEntity = @DisplayEntityField(entityName = "PaymentMethodType", description = "${description} (${cardType})")),
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.CommonMethod}", useWhen = "cardType==null", displayEntity = @DisplayEntityField(entityName = "PaymentMethodType", description = "${description}")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "expirePaymentGroupMember", description = "${uiLabelMap.CommonCancel}", alsoHidden = false, parameters = {@ParameterDef(paramName = "paymentGroupId"), @ParameterDef(paramName = "paymentId"), @ParameterDef(paramName = "fromDate")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        rowActions = @RowActions(set = {@SetAction(field = "paymentTypeId", fromField = "payment.paymentTypeId"), @SetAction(field = "partyIdFrom", fromField = "payment.partyIdFrom"), @SetAction(field = "partyIdTo", fromField = "payment.partyIdTo"), @SetAction(field = "paymentMethodTypeId", fromField = "payment.paymentMethodTypeId"), @SetAction(field = "cardType", fromField = "creditCard.cardType"), @SetAction(field = "amount", fromField = "payment.amount", type = "BigDecimal"), @SetAction(field = "paymentRefNum", fromField = "payment.paymentRefNum")}, entityOne = {@EntityOneAction(entityName = "Payment", valueField = "payment"), @EntityOneAction(entityName = "CreditCard", valueField = "creditCard")})
    )
    public interface ListPaymentGroupMember {}

    @Form(
        name = "AddPaymentGroupMember",
        location = "component://accounting/widget/payments/PaymentGroupForms.xml",
        target = "createPaymentGroupMember",
        fields = {
            @FormField(name = "paymentGroupId", hidden = @HiddenField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", lookup = @LookupField(targetFormName = "LookupPayment")),
            @FormField(name = "sequenceNum", position = 2, text = @TextField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(defaultValue = "${nowTimestamp}", type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(defaultValue = "${nowTimestamp}", type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddPaymentGroupMember {}

    @Form(
        name = "PaymentGroupMembers",
        location = "component://accounting/widget/payments/PaymentGroupForms.xml",
        type = FormType.LIST,
        listName = "paymentGroupMembers",
        paginateTarget = "PaymentGroupOverview",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "paymentOverview", description = "${paymentId}", parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.AccountingFromParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${lastName} ${firstName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${partyIdFrom}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdFrom")}))),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.AccountingToParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${lastName} ${firstName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${partyIdTo}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdTo")}))),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "PaymentType", description = "${description}")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency", alsoHidden = false)),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField)
        },
        rowActions = @RowActions(set = {@SetAction(field = "statusId", fromField = "payment.statusId"), @SetAction(field = "amount", fromField = "payment.amount"), @SetAction(field = "paymentTypeId", fromField = "payment.paymentTypeId"), @SetAction(field = "partyIdFrom", fromField = "payment.partyIdFrom"), @SetAction(field = "partyIdTo", fromField = "payment.partyIdTo")}, entityOne = {@EntityOneAction(entityName = "Payment", valueField = "payment")})
    )
    public interface PaymentGroupMembers {}

}

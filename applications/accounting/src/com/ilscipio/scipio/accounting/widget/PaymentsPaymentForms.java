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
public class PaymentsPaymentForms {

    @Form(
        name = "FindPayments",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "findPayments",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "comments", position = 2, textFind = @TextFindField),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PaymentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "PMNT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "partyIdFrom", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "amount", text = @TextField),
            @FormField(name = "paymentRefNum", position = 2, textFind = @TextFindField),
            @FormField(name = "paymentGatewayResponseId", position = 2, text = @TextField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPayments {}

    @Form(
        name = "ListPayments",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "Payment",
        paginate = "true",
        paginateTarget = "findPayments",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "effectiveDate", display = @DisplayField(type = "date")),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "paymentOverview", description = "${paymentId}", parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "isReduced==false", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.AccountingFromParty}", useWhen = "isReduced==false", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyFrom.groupName} ${partyFrom.firstName} ${partyFrom.lastName}", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdFrom")})),
            @FormField(name = "currencyUomId", hidden = @HiddenField),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency", alsoHidden = false))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PaymentAndTypeAndCreditCard"), @FieldMap(fieldName = "orderBy", value = "effectiveDate DESC"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(set = {@SetAction(field = "amountToApply", value = "${groovy:org.ofbiz.accounting.payment.PaymentWorker.getPaymentNotApplied(delegator,paymentId);}")}, entityOne = {@EntityOneAction(entityName = "PartyNameView", valueField = "partyFrom"), @EntityOneAction(entityName = "PartyNameView", valueField = "partyTo")})
    )
    public interface ListPayments {}

    @Form(
        name = "ListPaymentsReduced",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "Payment",
        paginate = "false",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        viewSize = 10,
        fields = {
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "paymentOverview", description = "${paymentId}", parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.AccountingFromParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${firstName} ${lastName}")),
            @FormField(name = "effectiveDate", display = @DisplayField(type = "date")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency", alsoHidden = false)),
            @FormField(name = "amountToApply", title = "${uiLabelMap.CommonOutstanding}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency", alsoHidden = false))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PaymentAndTypeAndCreditCard"), @FieldMap(fieldName = "orderBy", value = "effectiveDate ASC"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListPaymentsReduced {}

    @Form(
        name = "EditPaymentAttributes",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        extendsForm = "CommonPortletEdit",
        extendsResource = "component://common/widget/PortletEditForms.xml",
        fields = {
            @FormField(name = "partyIdFrom", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "statusId", entryName = "attributeMap.statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "PMNT_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "saveAction", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface EditPaymentAttributes {}

    @Form(
        name = "NewPaymentOut",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "createPayment",
        defaultMapName = "payment",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "statusId", hidden = @HiddenField(value = "PMNT_NOT_PAID")),
            @FormField(name = "currencyUomId", hidden = @HiddenField(value = "${defaultOrganizationPartyCurrencyUomId}")),
            @FormField(name = "organizationPartyId", parameterName = "partyIdFrom", title = "${uiLabelMap.CommonFrom} ${uiLabelMap.CommonParty}", event = "onchange", action = "javascript:(document.NewPaymentOut.action = 'newPayment'),(document.NewPaymentOut.submit())", dropDown = @DropDownField(options = {@Option(key = "${parameters.partyIdFrom}", description = "${partyGroupName}")}, entityOptions = @EntityOptions(entityName = "PartyAcctgPrefAndGroup", description = "${groupName}", keyFieldName = "partyId", orderBy = {@EntityOrderBy(fieldName = "groupName")}))),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.CommonTo} ${uiLabelMap.CommonParty}", position = 2, requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.CommonType}", requiredField = true, dropDown = @DropDownField(listOptions = @ListOptions(listName = "paymentTypes", keyName = "paymentTypeId", description = "${description}"))),
            @FormField(name = "paymentMethodId", title = "${uiLabelMap.CommonMethod}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentMethod", description = "${description}", constraints = {@EntityConstraint(name = "partyId", envName = "defaultOrganizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "paymentRefNum", text = @TextField),
            @FormField(name = "overrideGlAccountId", position = 2, lookup = @LookupField(targetFormName = "LookupGlAccount")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", requiredField = true, text = @TextField),
            @FormField(name = "comments", position = 2, text = @TextField(size = 70)),
            @FormField(name = "isDepositWithDrawPayment", hidden = @HiddenField(value = "Y")),
            @FormField(name = "finAccountTransTypeId", requiredField = true, hidden = @HiddenField(value = "WITHDRAWAL")),
            @FormField(name = "createAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "partyGroupName", fromField = "partyGroup.groupName"), @SetAction(field = "paymentPartyId", fromField = "parameters.partyIdFrom", defaultValue = "${defaultOrganizationPartyId}")}, entityOne = {@EntityOneAction(entityName = "PartyGroup", valueField = "partyGroup", useCache = true)})
    )
    public interface NewPaymentOut {}

    @Form(
        name = "NewPaymentIn",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "createPayment",
        defaultMapName = "payment",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "statusId", hidden = @HiddenField(value = "PMNT_NOT_PAID")),
            @FormField(name = "currencyUomId", hidden = @HiddenField(value = "${defaultOrganizationPartyCurrencyUomId}")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.CommonFrom} ${uiLabelMap.CommonParty}", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "organizationPartyId", parameterName = "partyIdTo", title = "${uiLabelMap.CommonTo} ${uiLabelMap.CommonParty}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyAcctgPrefAndGroup", description = "${groupName}", keyFieldName = "partyId", orderBy = {@EntityOrderBy(fieldName = "groupName")}))),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.CommonType}", requiredField = true, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", value = "RECEIPT")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "paymentMethodId", title = "${uiLabelMap.CommonMethod}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentMethod", description = "${description}", constraints = {@EntityConstraint(name = "partyId", envName = "defaultOrganizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "paymentRefNum", text = @TextField),
            @FormField(name = "overrideGlAccountId", position = 2, lookup = @LookupField(targetFormName = "LookupGlAccount")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", requiredField = true, text = @TextField),
            @FormField(name = "comments", position = 2, text = @TextField(size = 70)),
            @FormField(name = "finAccountId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FinAccount", description = "${finAccountName} [${finAccountId}]", filterByDate = "true", constraints = {@EntityConstraint(name = "finAccountTypeId", value = "BANK_ACCOUNT"), @EntityConstraint(name = "statusId", value = "FNACT_MANFROZEN", operator = "not-equals"), @EntityConstraint(name = "statusId", value = "FNACT_CANCELLED", operator = "not-equals")}))),
            @FormField(name = "isDepositWithDrawPayment", hidden = @HiddenField(value = "Y")),
            @FormField(name = "finAccountTransTypeId", hidden = @HiddenField(value = "DEPOSIT")),
            @FormField(name = "createAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface NewPaymentIn {}

    @Form(
        name = "EditPayment",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "updatePayment",
        defaultMapName = "payment",
        fields = {
            @FormField(name = "paymentId", hidden = @HiddenField),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", value = "${groovy:isDisbursement==true?\"DISBURSEMENT\":\"RECEIPT\"}")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "paymentMethodId", title = "${uiLabelMap.CommonMethod}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PaymentMethod", description = "${paymentMethodTypeId} (${paymentMethodId})", keyFieldName = "paymentMethodId", constraints = {@EntityConstraint(name = "partyId", value = "${groovy:isDisbursement==true?payment.partyIdFrom:payment.partyIdTo}")}, orderBy = {@EntityOrderBy(fieldName = "paymentMethodTypeId")}))),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.CommonFrom} ${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.CommonTo} ${uiLabelMap.CommonParty}", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", text = @TextField),
            @FormField(name = "currencyUomId", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "actualCurrencyAmount", title = "${uiLabelMap.AccountingActualCurrencyAmount}", text = @TextField),
            @FormField(name = "actualCurrencyUomId", title = "${uiLabelMap.AccountingActualCurrencyUomId}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "effectiveDate", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "paymentRefNum", text = @TextField),
            @FormField(name = "comments", position = 2, text = @TextField),
            @FormField(name = "paymentPreferenceId", ignored = @IgnoredField),
            @FormField(name = "paymentGatewayResponseId", ignored = @IgnoredField),
            @FormField(name = "finAccountTransId", text = @TextField),
            @FormField(name = "overrideGlAccountId", position = 2, lookup = @LookupField(targetFormName = "LookupGlAccount")),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "isDisbursement", value = "${groovy:org.ofbiz.accounting.util.UtilAccounting.isDisbursement(payment);}", type = "Boolean")})
    )
    public interface EditPayment {}

    @Form(
        name = "editPaymentApplicationsInv",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.LIST,
        target = "removePaymentApplication",
        listName = "paymentApplicationsInv",
        defaultEntityName = "PaymentApplication",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "paymentApplicationId", hidden = @HiddenField),
            @FormField(name = "paymentId", hidden = @HiddenField),
            @FormField(name = "invoiceId", title = "${uiLabelMap.CommonInvoice}", displayEntity = @DisplayEntityField(entityName = "Invoice", description = "${description}", subHyperlink = @SubHyperlink(target = "invoiceOverview", description = "[${invoiceId}]", parameters = {@ParameterDef(paramName = "invoiceId")}))),
            @FormField(name = "invoiceItemSeqId", display = @DisplayField),
            @FormField(name = "amountApplied", display = @DisplayField),
            @FormField(name = "removeAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface editPaymentApplicationsInv {}

    @Form(
        name = "editPaymentApplicationsPay",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.LIST,
        target = "removePaymentApplication",
        listName = "paymentApplicationsPay",
        defaultEntityName = "PaymentApplication",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "paymentApplicationId", hidden = @HiddenField),
            @FormField(name = "paymentId", hidden = @HiddenField),
            @FormField(name = "toPaymentId", display = @DisplayField),
            @FormField(name = "amountApplied", display = @DisplayField),
            @FormField(name = "removeAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface editPaymentApplicationsPay {}

    @Form(
        name = "editPaymentApplicationsBil",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.LIST,
        target = "removePaymentApplication",
        listName = "paymentApplicationsBil",
        defaultEntityName = "PaymentApplication",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "paymentApplicationId", hidden = @HiddenField),
            @FormField(name = "paymentId", hidden = @HiddenField),
            @FormField(name = "billingAccountId", display = @DisplayField),
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "amountApplied", display = @DisplayField),
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonRemove}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface editPaymentApplicationsBil {}

    @Form(
        name = "editPaymentApplicationsTax",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.LIST,
        target = "removePaymentApplication",
        listName = "paymentApplicationsTax",
        defaultEntityName = "PaymentApplication",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "paymentApplicationId", hidden = @HiddenField),
            @FormField(name = "paymentId", hidden = @HiddenField),
            @FormField(name = "taxAuthGeoId", display = @DisplayField),
            @FormField(name = "amountApplied", display = @DisplayField),
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonRemove}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", submit = @SubmitField)
        }
    )
    public interface editPaymentApplicationsTax {}

    @Form(
        name = "listInvoicesNotApplied",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.LIST,
        target = "createPaymentApplication",
        listName = "invoices",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "paymentId", hidden = @HiddenField),
            @FormField(name = "invoiceId", title = "${uiLabelMap.CommonInvoice}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "invoiceOverview", description = "${invoiceId}", parameters = {@ParameterDef(paramName = "invoiceId")})),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "invoiceDate", display = @DisplayField(type = "date")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "amountApplied", parameterName = "dummy", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "amountToApply", parameterName = "amountApplied", title = "${uiLabelMap.CommonOutStanding}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", text = @TextField(size = 10)),
            @FormField(name = "invoiceProcessing", useWhen = "\"${uiConfigMap.invoiceProcessing}\".equals(\"Y\")", check = @CheckField),
            @FormField(name = "invoiceProcessing", useWhen = "\"${uiConfigMap.invoiceProcessing}\".equals(\"N\")", check = @CheckField),
            @FormField(name = "applyAction", title = "${uiLabelMap.CommonApply}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface listInvoicesNotApplied {}

    @Form(
        name = "listInvoicesNotAppliedOtherCurrency",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        listName = "invoicesOtherCurrency",
        extendsForm = "listInvoicesNotApplied"
    )
    public interface listInvoicesNotAppliedOtherCurrency {}

    @Form(
        name = "listPaymentsNotApplied",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.LIST,
        target = "createPaymentApplication",
        listName = "payments",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "paymentId", hidden = @HiddenField),
            @FormField(name = "toPaymentId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "paymentOverview", description = "[${toPaymentId}]", parameters = {@ParameterDef(paramName = "paymentId", fromField = "toPaymentId")})),
            @FormField(name = "effectiveDate", display = @DisplayField),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "amountApplied", parameterName = "dummy", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "amountToApply", parameterName = "amountApplied", title = "${uiLabelMap.CommonOutstanding}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", text = @TextField(size = 10)),
            @FormField(name = "applyAction", title = "${uiLabelMap.CommonApply}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface listPaymentsNotApplied {}

    @Form(
        name = "addPaymentApplication",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "createPaymentApplication",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "paymentId", hidden = @HiddenField),
            @FormField(name = "invoiceId", title = "${uiLabelMap.CommonInvoice}", lookup = @LookupField(targetFormName = "LookupInvoice")),
            @FormField(name = "invoiceItemSeqId", useWhen = "\"${uiConfigMap.invoiceProcessing}\".equals(\"YY\")", text = @TextField(size = 10)),
            @FormField(name = "toPaymentId", lookup = @LookupField(targetFormName = "LookupPayment")),
            @FormField(name = "billingAccountId", lookup = @LookupField(targetFormName = "LookupBillingAccount")),
            @FormField(name = "taxAuthGeoId", lookup = @LookupField(targetFormName = "LookupGeo")),
            @FormField(name = "amountToApply", parameterName = "amountApplied", tooltip = "${uiLabelMap.AccountingLeaveEmptyForMaximumAmount}", text = @TextField),
            @FormField(name = "invoiceProcessing", useWhen = "\"${uiConfigMap.invoiceProcessing}\".equals(\"Y\")", check = @CheckField),
            @FormField(name = "invoiceProcessing", useWhen = "\"${uiConfigMap.invoiceProcessing}\".equals(\"N\")", check = @CheckField),
            @FormField(name = "applyAction", title = "${uiLabelMap.CommonApply}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface addPaymentApplication {}

    @Form(
        name = "AcctgTransAndEntries",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.LIST,
        listName = "AcctgTransAndEntries",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "invoiceId", title = "${uiLabelMap.CommonInvoice}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "invoiceOverview?invoiceId=${invoiceId}", description = "${invoiceId}")),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "paymentOverview?paymentId=${paymentId}", description = "${paymentId}")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "origAmount", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "acctgTransId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditAcctgTrans?acctgTransId=${acctgTransId}&organizationPartyId=${organizationPartyId}", description = "${acctgTransId}")),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "AcctgTransType")),
            @FormField(name = "glJournalId", title = "${uiLabelMap.AccountingGlJournal}", displayEntity = @DisplayEntityField(entityName = "GlJournal", description = "${glJournalName}")),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.FormFieldTitle_glAccountType}", displayEntity = @DisplayEntityField(entityName = "GlAccountType")),
            @FormField(name = "glAccountClassId", title = "${uiLabelMap.AccountingGlAccountClass}", displayEntity = @DisplayEntityField(entityName = "GlAccountClass", description = "${description}")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", description = "${lastName} ${groupName}")),
            @FormField(name = "reconcileStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "origCurrencyUomId", ignored = @IgnoredField),
            @FormField(name = "currencyUomId", ignored = @IgnoredField),
            @FormField(name = "shipmentId", ignored = @IgnoredField),
            @FormField(name = "receiptId", ignored = @IgnoredField),
            @FormField(name = "inventoryItemId", ignored = @IgnoredField),
            @FormField(name = "workEffortId", ignored = @IgnoredField),
            @FormField(name = "physicalInventoryId", ignored = @IgnoredField),
            @FormField(name = "transDescription", ignored = @IgnoredField),
            @FormField(name = "paymentId", hidden = @HiddenField)
        },
        sortOrder = @SortOrder(sortFields = {@SortField(name = "acctgTransId"), @SortField(name = "acctgTransEntrySeqId")})
    )
    public interface AcctgTransAndEntries {}

    @Form(
        name = "ListChecksToPrint",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.MULTI,
        target = "printChecks",
        targetWindow = "_blank",
        listName = "payments",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "paymentOverview", description = "${paymentId}", parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyTo}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "effectiveDate", display = @DisplayField),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonPrint}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", submit = @SubmitField)
        }
    )
    public interface ListChecksToPrint {}

    @Form(
        name = "ListChecksToSend",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.MULTI,
        target = "quickSendPayment?organizationPartyId=${organizationPartyId}",
        listName = "payments",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        useRowSubmit = true,
        fields = {
            @FormField(name = "paymentId", hidden = @HiddenField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "paymentOverview", description = "${paymentId}", parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.PartyPartyTo}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${firstName} ${middleName} ${lastName} ${groupName}")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "effectiveDate", display = @DisplayField),
            @FormField(name = "paymentRefNum", text = @TextField),
            @FormField(name = "_rowSubmit", title = "${uiLabelMap.CommonSelect}", check = @CheckField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSend}", widgetStyle = "${styles.link_run_sys} ${styles.action_send}", submit = @SubmitField)
        }
    )
    public interface ListChecksToSend {}

    @Form(
        name = "FindSalesInvoicesByDueDate",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "FindSalesInvoicesByDueDate",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "invoiceTypeId", hidden = @HiddenField(value = "SALES_INVOICE")),
            @FormField(name = "organizationPartyId", parameterName = "partyIdFrom", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRole", description = "${partyId}", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "partyId")}))),
            @FormField(name = "partyId", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "daysOffset", text = @TextField(defaultValue = "0")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonSelect}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindSalesInvoicesByDueDate {}

    @Form(
        name = "FindPurchaseInvoicesByDueDate",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "FindPurchaseInvoicesByDueDate",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "invoiceTypeId", hidden = @HiddenField(value = "PURCHASE_INVOICE")),
            @FormField(name = "organizationPartyId", parameterName = "partyId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRole", description = "${partyId}", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "partyId")}))),
            @FormField(name = "partyIdFrom", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "daysOffset", text = @TextField(defaultValue = "0")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonSelect}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindPurchaseInvoicesByDueDate {}

    @Form(
        name = "ListInvoicesByDueDate",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.LIST,
        listName = "invoicePaymentInfoList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "invoiceId", title = "${uiLabelMap.CommonInvoice}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "invoiceOverview", description = "${invoiceId}", parameters = {@ParameterDef(paramName = "invoiceId")})),
            @FormField(name = "termTypeId", title = "${uiLabelMap.CommonTerm}", displayEntity = @DisplayEntityField(entityName = "TermType", description = "${description}")),
            @FormField(name = "dueDate", title = "${uiLabelMap.CommonDue}", display = @DisplayField(type = "date")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "paidAmount", title = "${uiLabelMap.CommonPaid}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "outstandingAmount", title = "${uiLabelMap.CommonOutstanding}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "Invoice", valueField = "invoice")})
    )
    public interface ListInvoicesByDueDate {}

    @Form(
        name = "FindBatchPayments",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "batchPayments",
        fields = {
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.FormFieldTitle_paymentMethodTypeId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PaymentMethodType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "noConditionFind", hidden = @HiddenField),
            @FormField(name = "cardType", tooltip = "Select Credit Card from above list of Payment Method Types.", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${enumCode}", keyFieldName = "enumCode", constraints = {@EntityConstraint(name = "enumTypeId", value = "CREDIT_CARD_TYPE")}, orderBy = {@EntityOrderBy(fieldName = "enumId")}))),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.AccountingPartyIdFrom}", lookup = @LookupField(targetFormName = "LookupCustomerName")),
            @FormField(name = "fromDate", dateTime = @DateTimeField),
            @FormField(name = "thruDate", dateTime = @DateTimeField),
            @FormField(name = "submitButton", title = "${uiLabelMap.CommonFind}", widgetStyle = "smallSubmit", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "organizationPartyId", fromField = "organizationPartyId", defaultValue = "${defaultOrganizationPartyId}"), @SetAction(field = "noConditionFind", value = "Y")})
    )
    public interface FindBatchPayments {}

    @Form(
        name = "FindBatchPaymentsForDepositSlip",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "NewDepositSlip",
        extendsForm = "FindBatchPayments",
        extendsResource = "component://accounting/widget/payments/PaymentForms.xml",
        fields = {
            @FormField(name = "finAccountId", hidden = @HiddenField(value = "${finAccountId}"))
        }
    )
    public interface FindBatchPaymentsForDepositSlip {}

    @Form(
        name = "FindArPayments",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "findArPayments",
        extendsForm = "FindPayments",
        extendsResource = "component://accounting/widget/payments/PaymentForms.xml",
        fields = {
            @FormField(name = "parentTypeId", hidden = @HiddenField(value = "RECEIPT")),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PaymentType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", value = "RECEIPT")})))
        }
    )
    public interface FindArPayments {}

    @Form(
        name = "FindArPaymentGroups",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "FindArPaymentGroups",
        extendsForm = "FindPaymentGroup",
        extendsResource = "component://accounting/widget/payments/PaymentGroupForms.xml",
        fields = {
            @FormField(name = "paymentGroupTypeId", hidden = @HiddenField(value = "BATCH_PAYMENT"))
        }
    )
    public interface FindArPaymentGroups {}

    @Form(
        name = "FindGatewayResponses",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "FindGatewayResponses",
        headerRowStyle = "header-row",
        defaultTableStyle = "basic-table",
        fields = {
            @FormField(name = "paymentGatewayResponseId", title = "${uiLabelMap.AccountingPaymentGatewayResponseId}", textFind = @TextFindField),
            @FormField(name = "paymentServiceTypeEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PRDS_PAYSVC")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "orderPaymentPreferenceId", title = "${uiLabelMap.AccountingOrderPaymentPreferenceId}", textFind = @TextFindField),
            @FormField(name = "paymentMethodTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PaymentMethodType", keyFieldName = "paymentMethodTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "transCodeEnumId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "PGT_CODE")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "referenceNum", textFind = @TextFindField(size = 60, maxlength = 60)),
            @FormField(name = "performSearch", hidden = @HiddenField(value = "Y")),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitButton", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindGatewayResponses {}

    @Form(
        name = "ListGatewayResponses",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "FindGatewayResponses",
        oddRowStyle = "alternate-row",
        defaultTableStyle = "basic-table hover-bar",
        useRowSubmit = true,
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayResponse", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "paymentGatewayResponseId", widgetStyle = "buttontext", hyperlink = @HyperlinkField(target = "ViewGatewayResponse", description = "${paymentGatewayResponseId}", parameters = {@ParameterDef(paramName = "paymentGatewayResponseId")})),
            @FormField(name = "paymentServiceTypeEnumId", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId")),
            @FormField(name = "paymentMethodTypeId", displayEntity = @DisplayEntityField(entityName = "PaymentMethodType", keyFieldName = "paymentMethodTypeId")),
            @FormField(name = "transCodeEnumId", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId"))
        },
        actions = @FormActions(set = {@SetAction(field = "entityName", value = "PaymentGatewayResponse"), @SetAction(field = "orderBy", value = "transactionDate DESC")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "requestParameters"), @FieldMap(fieldName = "entityName", fromField = "entityName"), @FieldMap(fieldName = "orderBy", fromField = "orderBy"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListGatewayResponses {}

    @Form(
        name = "ViewGatewayResponseRelations",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        headerRowStyle = "header-row",
        defaultTableStyle = "basic-table",
        fields = {
            @FormField(name = "orderId", widgetStyle = "buttontext", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "orderPaymentPreferenceId", display = @DisplayField)
        }
    )
    public interface ViewGatewayResponseRelations {}

    @Form(
        name = "ViewGatewayResponsePayments",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        type = FormType.LIST,
        listName = "payments",
        headerRowStyle = "header-row-2",
        defaultTableStyle = "basic-table hover-bar",
        fields = {
            @FormField(name = "paymentId", widgetStyle = "buttontext", hyperlink = @HyperlinkField(target = "paymentOverview", description = "${paymentId}", parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.AccountingPaymentType}", displayEntity = @DisplayEntityField(entityName = "PaymentType")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "comments", display = @DisplayField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.AccountingFromParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${lastName},${firstName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${partyIdFrom}]", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdFrom")}))),
            @FormField(name = "partyIdTo", title = "${uiLabelMap.AccountingToParty}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${lastName},${firstName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${partyIdTo}]", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdTo")}))),
            @FormField(name = "effectiveDate", display = @DisplayField),
            @FormField(name = "currencyUomId", hidden = @HiddenField),
            @FormField(name = "amount", display = @DisplayField(type = "currency", alsoHidden = false))
        }
    )
    public interface ViewGatewayResponsePayments {}

    @Form(
        name = "ViewGatewayResponse",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "processCaptureTransaction",
        defaultMapName = "paymentGatewayResponse",
        headerRowStyle = "header-row",
        defaultTableStyle = "basic-table",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "PaymentGatewayResponse", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "paymentServiceTypeEnumId", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId")),
            @FormField(name = "paymentMethodTypeId", displayEntity = @DisplayEntityField(entityName = "PaymentMethodType", keyFieldName = "paymentMethodTypeId")),
            @FormField(name = "transCodeEnumId", displayEntity = @DisplayEntityField(entityName = "Enumeration", keyFieldName = "enumId"))
        }
    )
    public interface ViewGatewayResponse {}

    @Form(
        name = "AuthorizeTransaction",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "processAuthorizeTransaction",
        headerRowStyle = "header-row-2",
        defaultTableStyle = "basic-table",
        fields = {
            @FormField(name = "orderId", lookup = @LookupField(targetFormName = "LookupOrderHeader")),
            @FormField(name = "orderPaymentPreferenceId", lookup = @LookupField(targetFormName = "LookupOrderPaymentPreference")),
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.CommonPaymentMethodType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentMethodType"))),
            @FormField(name = "overrideAmount", title = "${uiLabelMap.AccountingAmount}", text = @TextField),
            @FormField(name = "submitButton", title = "${uiLabelMap.AccountingAuthorize}", widgetStyle = "smallSubmit", submit = @SubmitField)
        }
    )
    public interface AuthorizeTransaction {}

    @Form(
        name = "CaptureTransaction",
        location = "component://accounting/widget/payments/PaymentForms.xml",
        target = "processCaptureTransaction",
        headerRowStyle = "header-row-2",
        defaultTableStyle = "basic-table",
        fields = {
            @FormField(name = "orderId", lookup = @LookupField(targetFormName = "LookupOrderHeader")),
            @FormField(name = "orderPaymentPreferenceId", lookup = @LookupField(targetFormName = "LookupOrderPaymentPreference")),
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.CommonPaymentMethodType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentMethodType"))),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.AccountingPaymentType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentType", keyFieldName = "paymentTypeId", constraints = {@EntityConstraint(name = "parentTypeId", value = "RECEIPT")}))),
            @FormField(name = "captureAmount", title = "${uiLabelMap.AccountingAmount}", text = @TextField),
            @FormField(name = "submitButton", title = "${uiLabelMap.AccountingCapture}", widgetStyle = "smallSubmit", submit = @SubmitField)
        }
    )
    public interface CaptureTransaction {}

}

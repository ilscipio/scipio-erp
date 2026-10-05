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
public class BillingBillingAccountForms {

    @Form(
        name = "FindBillingAccounts",
        location = "component://accounting/widget/billing/BillingAccountForms.xml",
        target = "FindBillingAccount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "billingAccountId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "accountLimit", position = 2, text = @TextField),
            @FormField(name = "description", textFind = @TextFindField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "noConditionFind", position = 2, hidden = @HiddenField(value = "Y")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindBillingAccounts {}

    @Form(
        name = "ListBillingAccounts",
        location = "component://accounting/widget/billing/BillingAccountForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        defaultEntityName = "BillingAccount",
        paginateTarget = "FindBillingAccount",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "billingAccountId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditBillingAccount", description = "${billingAccountId}", parameters = {@ParameterDef(paramName = "billingAccountId")})),
            @FormField(name = "accountLimit", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "externalAccountId", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "BillingAccount"), @FieldMap(fieldName = "orderBy", value = "billingAccountId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListBillingAccounts {}

    @Form(
        name = "ListBillingAccountsByParty",
        location = "component://accounting/widget/billing/BillingAccountForms.xml",
        type = FormType.LIST,
        listName = "billingAccounts",
        paginateTarget = "FindBillingAccount",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "billingAccountId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditBillingAccount", description = "${billingAccountId}", parameters = {@ParameterDef(paramName = "billingAccountId")})),
            @FormField(name = "accountLimit", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "accountBalance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyParty}", display = @DisplayField(description = "${parameters.partyId}")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", displayEntity = @DisplayEntityField(entityName = "RoleType"))
        }
    )
    public interface ListBillingAccountsByParty {}

    @Form(
        name = "ListBillingAccountInvoices",
        location = "component://accounting/widget/billing/BillingAccountForms.xml",
        type = FormType.LIST,
        listName = "billingAccountInvoices",
        defaultEntityName = "Invoice",
        paginateTarget = "BillingAccountInvoices",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "billingAccountId", hidden = @HiddenField),
            @FormField(name = "invoiceId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "invoiceOverview", description = "${invoiceId}", parameters = {@ParameterDef(paramName = "invoiceId")})),
            @FormField(name = "invoiceTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "InvoiceType")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "partyIdFrom", display = @DisplayField(description = "${partyNameResultFrom.fullName} [${partyIdFrom}]")),
            @FormField(name = "partyIdTo", parameterName = "partyId", display = @DisplayField(description = "${partyNameResultTo.fullName} [${partyId}]")),
            @FormField(name = "invoiceDate", display = @DisplayField(description = "${groovy:invoiceDate.toString().substring(0,10)}")),
            @FormField(name = "total", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "amountToApply", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "capture", useWhen = "${groovy:!paidInvoice}", widgetStyle = "${styles.link_run_sys} ${styles.action_copy}", hyperlink = @HyperlinkField(target = "capturePaymentsByInvoice", description = "${uiLabelMap.AccountingCapture}", parameters = {@ParameterDef(paramName = "invoiceId"), @ParameterDef(paramName = "billingAccountId")})),
            @FormField(name = "capture", useWhen = "${groovy:paidInvoice}", display = @DisplayField)
        },
        rowActions = @RowActions(set = {@SetAction(field = "paidInvoice", value = "${groovy: org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceNotApplied(delegator,invoiceId).compareTo(java.math.BigDecimal.ZERO)==0}", type = "Boolean"), @SetAction(field = "amountToApply", value = "${groovy:                 import java.text.NumberFormat;                 return(NumberFormat.getNumberInstance(context.get(\"locale\")).format(org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceNotApplied(delegator,invoiceId)));}"), @SetAction(field = "total", value = "${groovy:                 import java.text.NumberFormat;                 return(NumberFormat.getNumberInstance(context.get(\"locale\")).format(org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceTotal(delegator,invoiceId)));}")}, service = {@ServiceAction(serviceName = "getPartyNameForDate", resultMapName = "partyNameResultFrom", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyIdFrom"), @FieldMap(fieldName = "compareDate", fromField = "invoiceDate"), @FieldMap(fieldName = "lastNameFirst", value = "Y")}), @ServiceAction(serviceName = "getPartyNameForDate", resultMapName = "partyNameResultTo", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId"), @FieldMap(fieldName = "compareDate", fromField = "invoiceDate"), @FieldMap(fieldName = "lastNameFirst", value = "Y")})})
    )
    public interface ListBillingAccountInvoices {}

    @Form(
        name = "EditBillingAccount",
        location = "component://accounting/widget/billing/BillingAccountForms.xml",
        target = "updateBillingAccount",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateBillingAccount", mapName = "billingAccount")
        },
        fields = {
            @FormField(name = "description", text = @TextField(size = 60)),
            @FormField(name = "billingAccountId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", useWhen = "billingAccount!=null", display = @DisplayField),
            @FormField(name = "partyId", useWhen = "partyId != null", hidden = @HiddenField),
            @FormField(name = "roleTypeId", useWhen = "roleTypeId != null", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.AccountingPartyBilledTo}", useWhen = "partyId == null", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", useWhen = "roleTypeId == null", hidden = @HiddenField(value = "BILL_TO_CUSTOMER")),
            @FormField(name = "accountCurrencyUomId", title = "${uiLabelMap.CommonCurrency}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "contactMechId", tooltip = "${uiLabelMap.AccountingBillingContactMechIdMessage}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "BillingAccountRoleAndAddress", description = "[${partyId}][${contactMechId}] ${toName}, ${attnName}, ${address1}, ${stateProvinceGeoId} ${postalCode}", keyFieldName = "contactMechId", filterByDate = "true", constraints = {@EntityConstraint(name = "billingAccountId", envName = "billingAccountId")}, orderBy = {@EntityOrderBy(fieldName = "partyId"), @EntityOrderBy(fieldName = "contactMechId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "availableBalance", title = "${uiLabelMap.AccountingBillingAvailableBalance}", tooltip = "${uiLabelMap.AccountingBillingAvailableBalanceMessage}", position = 2, display = @DisplayField(type = "currency")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", useWhen = "billingAccount == null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "billingAccount!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "billingAccount==null", target = "createBillingAccount")
        },
        actions = @FormActions(set = {@SetAction(field = "availableBalance", value = "${groovy:billingAccount != null ? org.ofbiz.order.order.OrderReadHelper.getBillingAccountBalance(billingAccount) : 0}", type = "BigDecimal")})
    )
    public interface EditBillingAccount {}

    @Form(
        name = "ListBillingAccountRoles",
        location = "component://accounting/widget/billing/BillingAccountForms.xml",
        type = FormType.MULTI,
        target = "updateBillingAccountRole?billingAccountId=${billingAccountId}",
        listName = "billingAccountRoleList",
        paginateTarget = "EditBillingAccountRoles",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "billingAccountId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_idname_long}", hyperlink = @HyperlinkField(target = "EditAgreementItemParty", description = "${partyId} - ${party.groupName} ${party.firstName} ${party.lastName}", parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")})),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", displayEntity = @DisplayEntityField(entityName = "RoleType", description = "${description}")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonDelete}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteBillingAccountRole", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "billingAccountId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "fromDate")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "PartyNameView", valueField = "party")})
    )
    public interface ListBillingAccountRoles {}

    @Form(
        name = "AddBillingAccountRole",
        location = "component://accounting/widget/billing/BillingAccountForms.xml",
        target = "createBillingAccountRole",
        defaultMapName = "billingAccountRole",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "createBillingAccountRole")
        },
        fields = {
            @FormField(name = "billingAccountId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface AddBillingAccountRole {}

    @Form(
        name = "ListBillingAccountTerms",
        location = "component://accounting/widget/billing/BillingAccountForms.xml",
        type = FormType.LIST,
        listName = "billingAccountTermsList",
        defaultEntityName = "BillingAccountTerm",
        paginateTarget = "EditBillingAccountTerms",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "billingAccountId", hidden = @HiddenField),
            @FormField(name = "billingAccountTermId", title = "${uiLabelMap.CommonTerm}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditBillingAccountTerms", description = "${billingAccountTermId}", parameters = {@ParameterDef(paramName = "billingAccountId"), @ParameterDef(paramName = "billingAccountTermId")})),
            @FormField(name = "termTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "TermType", description = "${description}")),
            @FormField(name = "termValue", title = "${uiLabelMap.CommonValue}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "uomId", title = "${uiLabelMap.CommonUom}", displayEntity = @DisplayEntityField(entityName = "Uom", description = "${description}")),
            @FormField(name = "deleteAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeBillingAccountTerm", description = "${uiLabelMap.CommonDelete}", parameters = {@ParameterDef(paramName = "billingAccountId"), @ParameterDef(paramName = "billingAccountTermId")}))
        }
    )
    public interface ListBillingAccountTerms {}

    @Form(
        name = "EditBillingAccountTerms",
        location = "component://accounting/widget/billing/BillingAccountForms.xml",
        target = "updateBillingAccountTerm",
        defaultMapName = "billingAccountTerm",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "billingAccountId", hidden = @HiddenField),
            @FormField(name = "billingAccountTermId", useWhen = "billingAccountTermId!=null", hidden = @HiddenField),
            @FormField(name = "termTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TermType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", value = "FINANCIAL_TERM")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "uomId", title = "${uiLabelMap.CommonUom}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "termValue", title = "${uiLabelMap.PartyTermValue}", text = @TextField(size = 10)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "billingAccountTermId==null", target = "createBillingAccountTerm")
        }
    )
    public interface EditBillingAccountTerms {}

    @Form(
        name = "ListBillingAccountPayments",
        location = "component://accounting/widget/billing/BillingAccountForms.xml",
        type = FormType.LIST,
        listName = "payments",
        paginateTarget = "BillingAccountPayments",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "paymentOverview", description = "${paymentId}", parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "PaymentMethodType", description = "${description}")),
            @FormField(name = "invoiceId", title = "${uiLabelMap.CommonInvoice}", display = @DisplayField),
            @FormField(name = "invoiceItemSeqId", display = @DisplayField),
            @FormField(name = "effectiveDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField(type = "date")),
            @FormField(name = "amountApplied", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "amount", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ListBillingAccountPayments {}

    @Form(
        name = "CreateIncomingBillingAccountPayment",
        location = "component://accounting/widget/billing/BillingAccountForms.xml",
        target = "createPaymentAndAssociateToBillingAccount",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "billingAccountId", hidden = @HiddenField),
            @FormField(name = "currencyUomId", hidden = @HiddenField(value = "${billingAccount.accountCurrencyUomId}")),
            @FormField(name = "statusId", hidden = @HiddenField(value = "PMNT_NOT_PAID")),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.CommonFrom} ${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "organizationPartyId", parameterName = "partyIdTo", title = "${uiLabelMap.CommonTo} ${uiLabelMap.CommonParty}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyRole", description = "${partyId}", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "partyId")}))),
            @FormField(name = "paymentTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.CommonMethod}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PaymentMethodType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", position = 2, text = @TextField),
            @FormField(name = "createAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "partyIdFrom", fromField = "billToCustomer.partyId")})
    )
    public interface CreateIncomingBillingAccountPayment {}

    @Form(
        name = "ListBillingAccountOrders",
        location = "component://accounting/widget/billing/BillingAccountForms.xml",
        type = FormType.LIST,
        listName = "orderPaymentPreferencesList",
        paginateTarget = "BillingAccountOrders",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "billingAccountId", hidden = @HiddenField),
            @FormField(name = "orderId", title = "${uiLabelMap.CommonOrder}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/ordermgr/control/orderview", urlMode = UrlMode.INTER_APP, description = "${orderId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "orderId")})),
            @FormField(name = "orderDate", title = "${uiLabelMap.CommonDate}", display = @DisplayField(type = "date")),
            @FormField(name = "paymentMethodTypeId", title = "${uiLabelMap.CommonMethod}", displayEntity = @DisplayEntityField(entityName = "PaymentMethodType", keyFieldName = "paymentMethodTypeId")),
            @FormField(name = "paymentStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "maxAmount", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface ListBillingAccountOrders {}

}

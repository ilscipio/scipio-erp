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
public class InvoiceInvoiceForms {

    @Form(
        name = "FindInvoices",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "findInvoices",
        title = "Find and list invoices",
        headerRowStyle = "header-row",
        defaultPositionSpan = 1,
        fields = {
            @FormField(name = "invoiceId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "invoiceDate", title = "${uiLabelMap.CommonDate}", position = 2, dateFind = @DateFindField(type = "date")),
            @FormField(name = "description", textFind = @TextFindField),
            @FormField(name = "invoiceTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "InvoiceType", description = "${description}"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "INVOICE_STATUS")}))),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.CommonFrom} ${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", parameterName = "partyId", title = "${uiLabelMap.CommonTo} ${uiLabelMap.CommonParty}", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "billingAccountId", lookup = @LookupField(targetFormName = "LookupBillingAccount")),
            @FormField(name = "referenceNumber", position = 2, textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface FindInvoices {}

    @Form(
        name = "ListInvoices",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        type = FormType.LIST,
        title = "Invoice List",
        listName = "listIt",
        defaultEntityName = "Invoice",
        paginateTarget = "findInvoices",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "invoiceId", title = "${uiLabelMap.CommonInvoice}", titleAreaStyle = "align-right", widgetStyle = "${styles.link_nav_info_id}", widgetAreaStyle = "align-right", sortField = true, hyperlink = @HyperlinkField(target = "invoiceOverview", description = "${invoiceId}", parameters = {@ParameterDef(paramName = "invoiceId")})),
            @FormField(name = "invoiceTypeId", title = "${uiLabelMap.CommonType}", sortField = true, displayEntity = @DisplayEntityField(entityName = "InvoiceType")),
            @FormField(name = "invoiceDate", title = "${uiLabelMap.CommonDate}", sortField = true, display = @DisplayField(type = "date")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", sortField = true, displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "description", sortField = true, display = @DisplayField),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.AccountingFromParty}", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", sortField = true, hyperlink = @HyperlinkField(target = "/partymgr/control/PartyFinancialHistory", urlMode = UrlMode.INTER_APP, description = "${partyNameResultFrom.fullName} [${partyIdFrom}]", parameters = {@ParameterDef(paramName = "partyId", fromField = "partyIdFrom")})),
            @FormField(name = "partyIdTo", parameterName = "partyId", title = "${uiLabelMap.AccountingToParty}", widgetStyle = "${styles.link_nav_info_idname} ${styles.action_view}", hyperlink = @HyperlinkField(target = "/partymgr/control/PartyFinancialHistory", urlMode = UrlMode.INTER_APP, description = "${partyNameResultTo.fullName} [${partyId}]", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "total", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "amountToApply", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        },
        actions = @FormActions(set = {@SetAction(field = "parameters.sortField", fromField = "parameters.sortField", defaultValue = "-invoiceDate")}, service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "InvoiceAndType"), @FieldMap(fieldName = "orderBy", fromField = "parameters.sortField"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})}),
        rowActions = @RowActions(set = {@SetAction(field = "amountToApply", value = "${groovy:org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceNotApplied(delegator,invoiceId)                 .multiply(org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceCurrencyConversionRate(delegator,invoiceId))}"), @SetAction(field = "total", value = "${groovy:org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceTotal(delegator,invoiceId)                 .multiply(org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceCurrencyConversionRate(delegator,invoiceId))}")}, service = {@ServiceAction(serviceName = "getPartyNameForDate", resultMapName = "partyNameResultFrom", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyIdFrom"), @FieldMap(fieldName = "compareDate", fromField = "invoiceDate"), @FieldMap(fieldName = "lastNameFirst", value = "Y")}), @ServiceAction(serviceName = "getPartyNameForDate", resultMapName = "partyNameResultTo", fieldMaps = {@FieldMap(fieldName = "partyId", fromField = "partyId"), @FieldMap(fieldName = "compareDate", fromField = "invoiceDate"), @FieldMap(fieldName = "lastNameFirst", value = "Y")})})
    )
    public interface ListInvoices {}

    @Form(
        name = "invoiceRoles",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        type = FormType.LIST,
        listName = "invoiceRoles",
        paginateTarget = "invoiceRoles",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_idname_long}", hyperlink = @HyperlinkField(target = "EditAgreementItemParty", description = "${partyId} - ${party.groupName} ${party.firstName} ${party.lastName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")})),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", displayEntity = @DisplayEntityField(entityName = "RoleType", alsoHidden = false)),
            @FormField(name = "percentage", display = @DisplayField),
            @FormField(name = "datetimePerformed", display = @DisplayField)
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "PartyNameView", valueField = "party")})
    )
    public interface invoiceRoles {}

    @Form(
        name = "AcctgTransAndEntries",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        type = FormType.LIST,
        title = "Accounting Transactions",
        listName = "acctgTransAndEntries",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "acctgTransId", title = "${uiLabelMap.CommonId}", titleAreaStyle = "align-right", widgetStyle = "${styles.link_nav_info_id}", widgetAreaStyle = "align-right", hyperlink = @HyperlinkField(target = "EditAcctgTrans?acctgTransId=${acctgTransId}&organizationPartyId=${organizationPartyId}", description = "${acctgTransId}")),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "AcctgTransType")),
            @FormField(name = "transactionDate", display = @DisplayField),
            @FormField(name = "postedDate", display = @DisplayField),
            @FormField(name = "glJournalId", title = "${uiLabelMap.AccountingGlJournal}", displayEntity = @DisplayEntityField(entityName = "GlJournal", description = "${glJournalId}")),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", display = @DisplayField),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.FormFieldTitle_glAccountType}", displayEntity = @DisplayEntityField(entityName = "GlAccountType")),
            @FormField(name = "glAccountClassId", title = "${uiLabelMap.AccountingGlAccountClass}", displayEntity = @DisplayEntityField(entityName = "GlAccountClass", description = "${description}")),
            @FormField(name = "reconcileStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "debitCreditFlag", widgetAreaStyle = "aling-center", display = @DisplayField),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        }
    )
    public interface AcctgTransAndEntries {}

    @Form(
        name = "NewSalesInvoice",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "createInvoice",
        title = "Edit Invoice Header",
        defaultMapName = "invoice",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "statusId", hidden = @HiddenField(value = "INVOICE_IN_PROCESS")),
            @FormField(name = "currencyUomId", hidden = @HiddenField(value = "${defaultOrganizationPartyCurrencyUomId}")),
            @FormField(name = "invoiceTypeId", position = 2, dropDown = @DropDownField(listOptions = @ListOptions(listName = "invoiceTypeList", keyName = "invoiceTypeId", description = "${description}"))),
            @FormField(name = "organizationPartyId", parameterName = "partyIdFrom", title = "${uiLabelMap.CommonFrom} ${uiLabelMap.CommonParty}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyAcctgPrefAndGroup", description = "${groupName}", keyFieldName = "partyId", orderBy = {@EntityOrderBy(fieldName = "groupName")}))),
            @FormField(name = "partyIdTo", parameterName = "partyId", title = "${uiLabelMap.CommonTo} ${uiLabelMap.CommonParty}", position = 2, requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "createAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface NewSalesInvoice {}

    @Form(
        name = "NewPurchaseInvoice",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "createInvoice",
        title = "Edit Invoice Header",
        defaultMapName = "invoice",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "statusId", hidden = @HiddenField(value = "INVOICE_IN_PROCESS")),
            @FormField(name = "currencyUomId", hidden = @HiddenField(value = "${defaultOrganizationPartyCurrencyUomId}")),
            @FormField(name = "invoiceTypeId", position = 2, dropDown = @DropDownField(listOptions = @ListOptions(listName = "invoiceTypeList", keyName = "invoiceTypeId", description = "${description}"))),
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.CommonFrom} ${uiLabelMap.CommonParty}", requiredField = true, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "organizationPartyId", parameterName = "partyId", title = "${uiLabelMap.CommonTo} ${uiLabelMap.CommonParty}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "PartyAcctgPrefAndGroup", description = "${groupName}", keyFieldName = "partyId", orderBy = {@EntityOrderBy(fieldName = "groupName")}))),
            @FormField(name = "createAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface NewPurchaseInvoice {}

    @Form(
        name = "EditInvoice",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "updateInvoice",
        title = "Edit Invoice Header",
        defaultMapName = "invoice",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "invoiceDate", dateTime = @DateTimeField),
            @FormField(name = "dueDate", position = 2, dateTime = @DateTimeField),
            @FormField(name = "invoiceTypeId", useWhen = "invoice!=null", displayEntity = @DisplayEntityField(entityName = "InvoiceType", description = "${description}")),
            @FormField(name = "invoiceTypeId", useWhen = "invoice==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "InvoiceType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "invoice==null", position = 2, hidden = @HiddenField(value = "INVOICE_IN_PROCESS")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "invoice!=null", position = 2, displayEntity = @DisplayEntityField(entityName = "StatusItem")),
            @FormField(name = "description", text = @TextField(size = 100)),
            @FormField(name = "partyIdFrom", useWhen = "${groovy:invoiceType.getString(\"parentTypeId\").equals(\"SALES_INVOICE\") || invoiceType.getString(\"invoiceTypeId\").equals(\"SALES_INVOICE\")}", position = 2, display = @DisplayField(description = "${invoice.partyIdFrom}")),
            @FormField(name = "partyIdFrom", useWhen = "${groovy:invoiceType.getString(\"parentTypeId\").equals(\"PURCHASE_INVOICE\") || invoiceType.getString(\"invoiceTypeId\").equals(\"PURCHASE_INVOICE\")}", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", entryName = "partyId", parameterName = "partyId", useWhen = "${groovy:invoiceType.getString(\"parentTypeId\").equals(\"PURCHASE_INVOICE\") || invoiceType.getString(\"invoiceTypeId\").equals(\"PURCHASE_INVOICE\")}", display = @DisplayField(description = "${invoice.partyId}")),
            @FormField(name = "partyIdTo", entryName = "partyId", parameterName = "partyId", useWhen = "${groovy:invoiceType.getString(\"parentTypeId\").equals(\"SALES_INVOICE\") || invoiceType.getString(\"invoiceTypeId\").equals(\"SALES_INVOICE\")}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", useWhen = "invoice!=null&&invoice.getString(\"invoiceTypeId\").equals(\"SALES_INVOICE\")", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", value = "CUSTOMER")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "roleTypeId", useWhen = "invoice!=null&&invoice.getString(\"invoiceTypeId\").equals(\"PURCHASE_INVOICE\")", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", constraints = {@EntityConstraint(name = "parentTypeId", value = "VENDOR")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "billingAccountId", lookup = @LookupField(targetFormName = "LookupBillingAccount")),
            @FormField(name = "currencyUomId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "recurrenceInfoId", text = @TextField(size = 10)),
            @FormField(name = "invoiceMessage", position = 2, text = @TextField(size = 100)),
            @FormField(name = "referenceNumber", position = 2, text = @TextField),
            @FormField(name = "updateAction", useWhen = "invoice!=null&&invoice.getString(\"statusId\").equals(\"INVOICE_IN_PROCESS\")", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        altTargets = {
            @AltTarget(useWhen = "invoice==null", target = "createInvoice")
        }
    )
    public interface EditInvoice {}

    @Form(
        name = "EditInvoiceItems",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        type = FormType.MULTI,
        target = "updateInvoiceItem?invoiceId=${invoiceId}",
        title = "Edit Invoice Items",
        listName = "invoiceItems",
        defaultEntityName = "InvoiceItem",
        paginateTarget = "listInvoiceItems",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "viewSize", hidden = @HiddenField(value = "${viewSize}")),
            @FormField(name = "viewIndex", hidden = @HiddenField(value = "${viewIndex}")),
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "uomId", hidden = @HiddenField),
            @FormField(name = "taxableFlag", hidden = @HiddenField),
            @FormField(name = "invoiceItemSeqId", display = @DisplayField),
            @FormField(name = "quantity", text = @TextField(size = 10)),
            @FormField(name = "invoiceItemTypeId", dropDown = @DropDownField(listOptions = @ListOptions(listName = "invoiceItemTypes", keyName = "invoiceItemTypeId", description = "${description}"))),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct", size = 20)),
            @FormField(name = "description", text = @TextField(size = 50)),
            @FormField(name = "overrideGlAccountId", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "glAccountOrganizationAndClassList", keyName = "glAccountId", description = "${glAccountId} ${accountName}"))),
            @FormField(name = "amount", title = "${uiLabelMap.AccountingUnitPrice}", text = @TextField(size = 10)),
            @FormField(name = "total", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", idName = "updateInvoiceItem", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonRemove}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeInvoiceItem", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "invoiceId"), @ParameterDef(paramName = "invoiceItemSeqId"), @ParameterDef(paramName = "viewIndex"), @ParameterDef(paramName = "viewSize")}))
        },
        rowActions = @RowActions(set = {@SetAction(field = "total", value = "${groovy: (quantity ?: 1) * (amount ?: 0)}", type = "BigDecimal")})
    )
    public interface EditInvoiceItems {}

    @Form(
        name = "EditInvoiceItem",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "createInvoiceItem",
        defaultMapName = "invoiceItem",
        defaultEntityName = "InvoiceItem",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "invoiceItemTypeId", dropDown = @DropDownField(listOptions = @ListOptions(listName = "invoiceItemTypes", keyName = "invoiceItemTypeId", description = "${description}"))),
            @FormField(name = "description", position = 2, text = @TextField(size = 80)),
            @FormField(name = "overrideGlAccountId", dropDown = @DropDownField(allowEmpty = true, listOptions = @ListOptions(listName = "glAccountOrganizationAndClassList", keyName = "glAccountId", description = "${glAccountId} ${accountName}"))),
            @FormField(name = "inventoryItemId", position = 2, text = @TextField),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "productFeatureId", position = 2, lookup = @LookupField(targetFormName = "LookupProductFeature")),
            @FormField(name = "quantity", text = @TextField(size = 10)),
            @FormField(name = "uomId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE", operator = "not-equals")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "amount", title = "${uiLabelMap.AccountingUnitPrice}", text = @TextField(size = 10)),
            @FormField(name = "taxableFlag", position = 2, dropDown = @DropDownField(current = "selected", options = {@Option(key = "Y", description = "${uiLabelMap.CommonYes}"), @Option(key = "N", description = "${uiLabelMap.CommonNo}")})),
            @FormField(name = "addAction", title = "${uiLabelMap.CommonAdd}", useWhen = "invoiceItem==null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonAdd}", useWhen = "invoiceItem!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditInvoiceItem {}

    @Form(
        name = "EditInvoiceApplications",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        type = FormType.MULTI,
        target = "updateInvoiceApplication",
        title = "Apply payments to invoices",
        listName = "invoiceApplications",
        defaultEntityName = "InvoiceItem",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "statusId", hidden = @HiddenField),
            @FormField(name = "paymentApplicationId", hidden = @HiddenField),
            @FormField(name = "invoiceItemSeqId", display = @DisplayField),
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "total", display = @DisplayField(type = "currency")),
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", lookup = @LookupField(targetFormName = "LookupPayment")),
            @FormField(name = "billingAccountId", hidden = @HiddenField),
            @FormField(name = "amountToApply", text = @TextField(size = 10, disabled = true)),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link")),
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonRemove}", useWhen = "paymentApplicationId!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeInvoiceApplication", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "paymentApplicationId"), @ParameterDef(paramName = "invoiceId"), @ParameterDef(paramName = "viewIndex"), @ParameterDef(paramName = "viewSize")}))
        }
    )
    public interface EditInvoiceApplications {}

    @Form(
        name = "AddPayment",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "updateInvoiceApplication",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", lookup = @LookupField(targetFormName = "LookupPayment")),
            @FormField(name = "amountToApply", parameterName = "amountApplied", text = @TextField(size = 10)),
            @FormField(name = "invoiceProcessing", useWhen = "\"${uiConfigMap.invoiceProcessing}\".equals(\"Y\")", check = @CheckField),
            @FormField(name = "applyAction", title = "${uiLabelMap.CommonApply}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface AddPayment {}

    @Form(
        name = "ListPaymentsNotApplied",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        type = FormType.LIST,
        target = "updateInvoiceApplication",
        listName = "payments",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        fields = {
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "paymentOverview", description = "[${paymentId}]", parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "effectiveDate", display = @DisplayField(type = "date")),
            @FormField(name = "amountApplied", parameterName = "dummy", display = @DisplayField(type = "currency")),
            @FormField(name = "amountToApply", parameterName = "amountApplied", text = @TextField(size = 10)),
            @FormField(name = "applyAction", title = "${uiLabelMap.CommonApply}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        }
    )
    public interface ListPaymentsNotApplied {}

    @Form(
        name = "ListPaymentsNotAppliedForeignCurrency",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        listName = "paymentsActualCurrency",
        extendsForm = "ListPaymentsNotApplied"
    )
    public interface ListPaymentsNotAppliedForeignCurrency {}

    @Form(
        name = "ListInvoiceRoles",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        type = FormType.LIST,
        listName = "invoiceRoles",
        paginateTarget = "invoiceRoles",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", widgetStyle = "${styles.link_nav_info_idname_long}", hyperlink = @HyperlinkField(target = "EditAgreementItemParty", description = "${partyId} - ${party.groupName} ${party.firstName} ${party.lastName}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "agreementId"), @ParameterDef(paramName = "agreementItemSeqId")})),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", displayEntity = @DisplayEntityField(entityName = "RoleType", alsoHidden = false)),
            @FormField(name = "percentage", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "datetimePerformed", display = @DisplayField),
            @FormField(name = "removeAction", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "removeInvoiceRole", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "invoiceId"), @ParameterDef(paramName = "partyId"), @ParameterDef(paramName = "roleTypeId"), @ParameterDef(paramName = "viewIndex"), @ParameterDef(paramName = "viewSize")}))
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "PartyNameView", valueField = "party")})
    )
    public interface ListInvoiceRoles {}

    @Form(
        name = "EditInvoiceRole",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "createInvoiceRole",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "percentage", text = @TextField),
            @FormField(name = "datetimePerformed", position = 2, dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonAdd}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        }
    )
    public interface EditInvoiceRole {}

    @Form(
        name = "SendPerEmail",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "executeSendPerEmail",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "emailAddressFrom", entryName = "mapFrom.emailAddress", parameterName = "sendFrom", useWhen = "\"${invoice.invoiceTypeId}\".equals(\"SALES_INVOICE\")", text = @TextField),
            @FormField(name = "emailAddressFrom", entryName = "mapTo.emailAddress", parameterName = "sendFrom", useWhen = "\"${invoice.invoiceTypeId}\".equals(\"PURCHASE_INVOICE\")", text = @TextField),
            @FormField(name = "emailAddressTo", entryName = "mapTo.emailAddress", parameterName = "sendTo", useWhen = "\"${invoice.invoiceTypeId}\".equals(\"SALES_INVOICE\")", text = @TextField),
            @FormField(name = "emailAddressTo", entryName = "mapFrom.emailAddress", parameterName = "sendTo", useWhen = "\"${invoice.invoiceTypeId}\".equals(\"PURCHASE_INVOICE\")", text = @TextField),
            @FormField(name = "emailAddressCc", entryName = "ccEmailAddress", parameterName = "sendCc", text = @TextField),
            @FormField(name = "subject", text = @TextField(defaultValue = "Please find attached invoice.")),
            @FormField(name = "otherCurrency", entryName = "parameters.other", parameterName = "other", check = @CheckField),
            @FormField(name = "bodyText", textarea = @TextareaField),
            @FormField(name = "webSiteId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "WebSite", description = "${siteName} [${webSiteId}]", keyFieldName = "webSiteId", orderBy = {@EntityOrderBy(fieldName = "siteName")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_send}", submit = @SubmitField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "getPartyEmail", resultMapName = "mapFrom", fieldMaps = {@FieldMap(fieldName = "partyId", value = "${invoice.partyIdFrom}")}), @ServiceAction(serviceName = "getPartyEmail", resultMapName = "mapTo", fieldMaps = {@FieldMap(fieldName = "partyId", value = "${invoice.partyId}")})})
    )
    public interface SendPerEmail {}

    @Form(
        name = "EditTimeEntries",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        type = FormType.LIST,
        listName = "timeEntries",
        oddRowStyle = "alternate-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateTimeEntry", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "deleteAction", title = "${uiLabelMap.CommonRemove}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "unlinkInvoiceFromTimeEntry", description = "${uiLabelMap.CommonDelete}", alsoHidden = false, parameters = {@ParameterDef(paramName = "timeEntryId"), @ParameterDef(paramName = "invoiceId"), @ParameterDef(paramName = "viewIndex"), @ParameterDef(paramName = "viewSize")}))
        }
    )
    public interface EditTimeEntries {}

    @Form(
        name = "ListTimeEntries",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        type = FormType.LIST,
        listName = "timeEntries",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "invoiceItemSeqId", display = @DisplayField),
            @FormField(name = "timeEntryId", display = @DisplayField),
            @FormField(name = "timesheetId", entryName = "timesheet.timesheetId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/workeffort/control/EditTimesheet", urlMode = UrlMode.INTER_APP, description = "${timesheetId}", parameters = {@ParameterDef(paramName = "timesheetId")})),
            @FormField(name = "partyId", entryName = "partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${firstName} ${middleName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${partyId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId")}))),
            @FormField(name = "timesheetPartyId", entryName = "timesheet.partyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName} ${firstName} ${middleName} ${lastName}", subHyperlink = @SubHyperlink(target = "/partymgr/control/viewprofile", description = "[${timesheet.partyId}]", linkStyle = "${styles.link_nav_info_id}", parameters = {@ParameterDef(paramName = "partyId", fromField = "timesheet.partyId")}))),
            @FormField(name = "hours", display = @DisplayField),
            @FormField(name = "rateTypeId", displayEntity = @DisplayEntityField(entityName = "RateType", description = "${description}")),
            @FormField(name = "workEffortId", displayEntity = @DisplayEntityField(entityName = "WorkEffort", description = "${workEffortName} [${workEffortId}]", subHyperlink = @SubHyperlink(target = "/workeffort/control/WorkEffortSummary", description = " [${workEffortId}]", parameters = {@ParameterDef(paramName = "workEffortId")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField(type = "date")),
            @FormField(name = "comments", display = @DisplayField)
        },
        rowActions = @RowActions(entityOne = {@EntityOneAction(entityName = "Timesheet", valueField = "timesheet")})
    )
    public interface ListTimeEntries {}

    @Form(
        name = "lookupInvoicesStatus",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "BillingAccountInvoices",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "billingAccountId", hidden = @HiddenField),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "INVOICE_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupInvoicesStatus {}

    @Form(
        name = "ListCustomerInvoices",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        listName = "invoices",
        extendsForm = "ListInvoices",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyIdFrom", ignored = @IgnoredField),
            @FormField(name = "partyIdTo", ignored = @IgnoredField)
        }
    )
    public interface ListCustomerInvoices {}

    @Form(
        name = "ListSupplierInvoices",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        listName = "invoiceslistexternal",
        extendsForm = "ListInvoices",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "partyIdFrom", ignored = @IgnoredField),
            @FormField(name = "partyIdTo", ignored = @IgnoredField)
        }
    )
    public interface ListSupplierInvoices {}

    @Form(
        name = "FindApInvoices",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "FindApInvoices",
        extendsForm = "FindInvoices",
        extendsResource = "component://accounting/widget/invoice/InvoiceForms.xml",
        fields = {
            @FormField(name = "invoiceTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "invoiceTypeList", keyName = "invoiceTypeId", description = "${description}")))
        }
    )
    public interface FindApInvoices {}

    @Form(
        name = "FindPurchaseInvoices",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "FindPurchaseInvoices",
        fields = {
            @FormField(name = "partyIdFrom", title = "${uiLabelMap.AccountingVendorParty}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "INVOICE_STATUS")}))),
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "fromInvoiceDate", dateTime = @DateTimeField),
            @FormField(name = "thruInvoiceDate", position = 2, dateTime = @DateTimeField),
            @FormField(name = "fromDueDate", dateTime = @DateTimeField),
            @FormField(name = "thruDueDate", position = 2, dateTime = @DateTimeField),
            @FormField(name = "invoiceTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "invoiceTypeList", keyName = "invoiceTypeId", description = "${description}"))),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "organizationPartyId", fromField = "parameters.organizationPartyId", defaultValue = "${defaultOrganizationPartyId}")})
    )
    public interface FindPurchaseInvoices {}

    @Form(
        name = "CommissionRun",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "CommissionRun",
        fields = {
            @FormField(name = "partyIds", fieldName = "partyId", title = "${uiLabelMap.PartyPartyId}", dropDown = @DropDownField(allowEmpty = true, allowMulti = true, entityOptions = @EntityOptions(entityName = "PartyRoleAndPartyDetail", description = "${firstName} ${middleName} ${lastName} ${groupName}(${partyId})", constraints = {@EntityConstraint(name = "roleTypeId", value = "SALES_REP")}))),
            @FormField(name = "invoiceTypeId", hidden = @HiddenField(value = "SALES_INVOICE")),
            @FormField(name = "statusId", hidden = @HiddenField(value = "INVOICE_PAID")),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface CommissionRun {}

    @Form(
        name = "CommissionReport",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "FindCommissions",
        fields = {
            @FormField(name = "isSearch", hidden = @HiddenField(value = "Y")),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct")),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRoleAndPartyDetail", description = "${firstName} ${middleName} ${lastName} ${groupName}(${partyId})", constraints = {@EntityConstraint(name = "roleTypeId", value = "SALES_REP")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface CommissionReport {}

    @Form(
        name = "FindArInvoices",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "findArInvoices",
        extendsForm = "FindInvoices",
        extendsResource = "component://accounting/widget/invoice/InvoiceForms.xml",
        defaultPositionSpan = 1,
        fields = {
            @FormField(name = "invoiceTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(listOptions = @ListOptions(listName = "invoiceTypeList", keyName = "invoiceTypeId", description = "${description}")))
        }
    )
    public interface FindArInvoices {}

    @Form(
        name = "ListInvoiceTerms",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        type = FormType.LIST,
        listName = "invoiceTerms",
        oddRowStyle = "alternate-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "InvoiceTerm", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "invoiceTermId", display = @DisplayField),
            @FormField(name = "termTypeId", displayEntity = @DisplayEntityField(entityName = "TermType")),
            @FormField(name = "termDays", titleAreaStyle = "align-right", widgetAreaStyle = "align-right", display = @DisplayField),
            @FormField(name = "uomId", title = "${uiLabelMap.Uom}", displayEntity = @DisplayEntityField(entityName = "Uom"))
        }
    )
    public interface ListInvoiceTerms {}

    @Form(
        name = "EditInvoiceTerm",
        location = "component://accounting/widget/invoice/InvoiceForms.xml",
        target = "createInvoiceTerm",
        title = "${uiLabelMap.PageTitleNewInvoiceTerm}",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "InvoiceTerm")
        },
        fields = {
            @FormField(name = "invoiceId", hidden = @HiddenField),
            @FormField(name = "termTypeId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "TermType", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "description", textarea = @TextareaField),
            @FormField(name = "uomId", title = "${uiLabelMap.Uom}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "submitButton", widgetStyle = "smallSubmit", submit = @SubmitField)
        }
    )
    public interface EditInvoiceTerm {}

}

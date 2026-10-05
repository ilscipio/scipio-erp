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
public class FieldLookupForms {

    @Form(
        name = "lookupFixedAsset",
        location = "component://accounting/widget/FieldLookupForms.xml",
        target = "LookupFixedAsset",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "FixedAsset", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.CommonId}", textFind = @TextFindField),
            @FormField(name = "fixedAssetName", title = "${uiLabelMap.CommonName}", textFind = @TextFindField),
            @FormField(name = "fixedAssetTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAssetType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupFixedAsset {}

    @Form(
        name = "listLookupFixedAsset",
        location = "component://accounting/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupFixedAsset",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "fixedAssetId", title = "uiLabelMap.CommonId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${fixedAssetId}')", urlMode = UrlMode.PLAIN, description = "${fixedAssetId}", alsoHidden = false)),
            @FormField(name = "fixedAssetName", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "fixedAssetTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "FixedAssetType"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "FixedAsset"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupFixedAsset {}

    @Form(
        name = "lookupBudget",
        location = "component://accounting/widget/FieldLookupForms.xml",
        target = "LookupBudget",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Budget", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "budgetId", textFind = @TextFindField),
            @FormField(name = "budgetTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "BudgetType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "customTimePeriodId", textFind = @TextFindField),
            @FormField(name = "comments", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupBudget {}

    @Form(
        name = "listLookupBudget",
        location = "component://accounting/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupBudget",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "fixedAssetId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${fixedAssetId}')", urlMode = UrlMode.PLAIN, description = "${fixedAssetId}", alsoHidden = false)),
            @FormField(name = "fixedAssetName", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "fixedAssetTypeId", title = "${uiLabelMap.AccountingFixedAssetTypeId}", displayEntity = @DisplayEntityField(entityName = "FixedAssetType"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "FixedAsset"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupBudget {}

    @Form(
        name = "lookupBillingAccount",
        location = "component://accounting/widget/FieldLookupForms.xml",
        target = "LookupBillingAccount",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "BillingAccount", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "billingAccountId", title = "${uiLabelMap.AccountingBillingAccountId}", textFind = @TextFindField),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "externalAccountId", title = "${uiLabelMap.AccountingExternalAccountId}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupBillingAccount {}

    @Form(
        name = "listBillingAccount",
        location = "component://accounting/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupBillingAccount",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "billingAccountId", title = "${uiLabelMap.AccountingBillingAccountId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${billingAccountId}')", urlMode = UrlMode.PLAIN, description = "${billingAccountId}", alsoHidden = false)),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "externalAccountId", title = "${uiLabelMap.AccountingExternalAccountId}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "BillingAccount"), @FieldMap(fieldName = "orderBy", value = "billingAccountId"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listBillingAccount {}

    @Form(
        name = "lookupGlAccount",
        location = "component://accounting/widget/FieldLookupForms.xml",
        target = "LookupGlAccount",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "GlAccount", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "glAccountId", title = "${uiLabelMap.AccountingGlAccountId}", textFind = @TextFindField),
            @FormField(name = "accountName", title = "${uiLabelMap.CommonName}", textFind = @TextFindField),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glAccountClassId", title = "${uiLabelMap.CommonClass}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountClass", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupGlAccount {}

    @Form(
        name = "listLookupGlAccount",
        location = "component://accounting/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupGlAccount",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "glAccountId", title = "${uiLabelMap.AccountingGlAccountId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${glAccountId}')", urlMode = UrlMode.PLAIN, description = "${glAccountId}", alsoHidden = false)),
            @FormField(name = "accountName", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "GlAccountType")),
            @FormField(name = "glAccountClassId", title = "${uiLabelMap.AccountingGlAccountClass}", displayEntity = @DisplayEntityField(entityName = "GlAccountClass"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "GlAccount"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupGlAccount {}

    @Form(
        name = "lookupPayment",
        location = "component://accounting/widget/FieldLookupForms.xml",
        target = "LookupPayment",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Payment", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", textFind = @TextFindField),
            @FormField(name = "partyIdFrom", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyIdTo", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "amountApplied", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupPayment {}

    @Form(
        name = "listPayment",
        location = "component://accounting/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupPayment",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${paymentId}')", urlMode = UrlMode.PLAIN, description = "${paymentId}", alsoHidden = false)),
            @FormField(name = "partyIdFrom", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${lastName}[${partyId}]")),
            @FormField(name = "partyIdTo", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${lastName}[${partyId}]")),
            @FormField(name = "effectiveDate", display = @DisplayField),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", display = @DisplayField),
            @FormField(name = "currencyUomId", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Payment"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listPayment {}

    @Form(
        name = "lookupInvoice",
        location = "component://accounting/widget/FieldLookupForms.xml",
        target = "LookupInvoice",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", textFind = @TextFindField),
            @FormField(name = "invoiceTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "InvoiceType", description = "${description}"))),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "INVOICE_STATUS")}))),
            @FormField(name = "partyIdFrom", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "partyId", title = "${uiLabelMap.AccountingPartyIdTo}", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "Datefrom", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "DateThru", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupInvoice {}

    @Form(
        name = "listInvoice",
        location = "component://accounting/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupInvoice",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${invoiceId}')", urlMode = UrlMode.PLAIN, description = "${invoiceId}", alsoHidden = false)),
            @FormField(name = "invoiceTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "InvoiceType")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", description = "${description}")),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "partyIdFrom", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${lastName}[${partyId}]")),
            @FormField(name = "partyId", title = "${uiLabelMap.AccountingPartyIdTo}", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${lastName}[${partyId}]")),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.CommonCurrency}", display = @DisplayField),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency"))
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Invoice"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listInvoice {}

    @Form(
        name = "lookupAgreement",
        location = "component://accounting/widget/FieldLookupForms.xml",
        target = "LookupAgreement",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "agreementId", textFind = @TextFindField),
            @FormField(name = "productId", textFind = @TextFindField),
            @FormField(name = "partyIdFrom", textFind = @TextFindField),
            @FormField(name = "partyId", title = "${uiLabelMap.AccountingPartyIdTo}", textFind = @TextFindField),
            @FormField(name = "agreementDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupAgreement {}

    @Form(
        name = "listAgreements",
        location = "component://accounting/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        target = "LookupAgreement",
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${agreementId}')", urlMode = UrlMode.PLAIN, description = "${agreementId}", alsoHidden = false)),
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "partyIdFrom", display = @DisplayField),
            @FormField(name = "partyIdTo", display = @DisplayField),
            @FormField(name = "agreementDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "agreementTypeId", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "Agreement")})})
    )
    public interface listAgreements {}

    @Form(
        name = "lookupAgreementItem",
        location = "component://accounting/widget/FieldLookupForms.xml",
        target = "LookupAgreementItem",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "agreementId", textFind = @TextFindField),
            @FormField(name = "agreementItemSeqId", textFind = @TextFindField),
            @FormField(name = "agreementItemTypeId", title = "${uiLabelMap.CommonType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "AgreementItemType", description = "${description}"))),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupAgreementItem {}

    @Form(
        name = "listAgreementItems",
        location = "component://accounting/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        target = "LookupAgreementItem",
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "agreementId", display = @DisplayField),
            @FormField(name = "agreementItemSeqId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${agreementItemSeqId}')", urlMode = UrlMode.PLAIN, description = "${agreementItemSeqId}", alsoHidden = false)),
            @FormField(name = "agreementItemTypeId", title = "${uiLabelMap.CommonType}", display = @DisplayField),
            @FormField(name = "currencyUomId", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "AgreementItem"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listAgreementItems {}

    @Form(
        name = "lookupPaymentGroupMember",
        location = "component://accounting/widget/FieldLookupForms.xml",
        target = "LookupPaymentGroupMember",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "paymentGroupId", textFind = @TextFindField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", textFind = @TextFindField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "sequenceNum", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupPaymentGroupMember {}

    @Form(
        name = "listPaymentGroupMember",
        location = "component://accounting/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        target = "LookupPaymentGroupMember",
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "paymentGroupId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${paymentGroupId}')", urlMode = UrlMode.PLAIN, description = "${paymentGroupId}", alsoHidden = false)),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", display = @DisplayField),
            @FormField(name = "sequenceNum", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "PaymentGroupMember"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listPaymentGroupMember {}

    @Form(
        name = "LookupGlReconciliation",
        location = "component://accounting/widget/FieldLookupForms.xml",
        target = "LookupGlReconciliation",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "glReconciliationId", title = "${uiLabelMap.FormFieldTitle_glReconciliationId}", textFind = @TextFindField),
            @FormField(name = "glReconciliationName", title = "${uiLabelMap.FormFieldTitle_glReconciliationName}", textFind = @TextFindField),
            @FormField(name = "organizationPartyId", title = "${uiLabelMap.FormFieldTitle_organizationPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface LookupGlReconciliation {}

    @Form(
        name = "ListLookupReconciliation",
        location = "component://accounting/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        target = "LookupGlReconciliation",
        listName = "listIt",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "glReconciliationId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${glReconciliationId}')", urlMode = UrlMode.PLAIN, description = "${glReconciliationId}", alsoHidden = false)),
            @FormField(name = "glReconciliationName", display = @DisplayField),
            @FormField(name = "organizationPartyId", displayEntity = @DisplayEntityField(entityName = "PartyNameView", keyFieldName = "partyId", description = "${groupName}${firstName} ${lastName}[${partyId}]")),
            @FormField(name = "description", title = "${uiLabelMap.CommonDescription}", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "GlReconciliation"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface ListLookupReconciliation {}

    @Form(
        name = "lookupCustomTimePeriod",
        location = "component://accounting/widget/FieldLookupForms.xml",
        target = "LookupCustomTimePeriod",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "CustomTimePeriod", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "customTimePeriodId", textFind = @TextFindField),
            @FormField(name = "parentPeriodId", textFind = @TextFindField),
            @FormField(name = "periodTypeId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PeriodType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "periodNum", textFind = @TextFindField),
            @FormField(name = "periodName", textFind = @TextFindField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "isClosed", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "Y", description = "${uiLabelMap.CommonY}"), @Option(key = "N", description = "${uiLabelMap.CommonN}")})),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupCustomTimePeriod {}

    @Form(
        name = "listLookupCustomTimePeriod",
        location = "component://accounting/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupCustomTimePeriod",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "customTimePeriodId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${customTimePeriodId}')", urlMode = UrlMode.PLAIN, description = "${customTimePeriodId}", alsoHidden = false)),
            @FormField(name = "parentPeriodId", display = @DisplayField),
            @FormField(name = "periodTypeId", displayEntity = @DisplayEntityField(entityName = "PeriodType")),
            @FormField(name = "periodNum", display = @DisplayField),
            @FormField(name = "periodName", display = @DisplayField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", display = @DisplayField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", display = @DisplayField),
            @FormField(name = "isClosed", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "CustomTimePeriod"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listLookupCustomTimePeriod {}

    @Form(
        name = "lookupOrderPaymentPreference",
        location = "component://accounting/widget/FieldLookupForms.xml",
        target = "LookupOrderPaymentPreference",
        headerRowStyle = "header-row",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "Payment", defaultFieldType = DefaultFieldType.HIDDEN)
        },
        fields = {
            @FormField(name = "orderPaymentPreferenceId", title = "${uiLabelMap.CommonPayment}", textFind = @TextFindField),
            @FormField(name = "orderId", lookup = @LookupField(targetFormName = "LookupOrderHeader")),
            @FormField(name = "paymentMethodTypeId"),
            @FormField(name = "maxAmount", textFind = @TextFindField),
            @FormField(name = "noConditionFind", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField)
        }
    )
    public interface lookupOrderPaymentPreference {}

    @Form(
        name = "listOrderPaymentPreference",
        location = "component://accounting/widget/FieldLookupForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginateTarget = "LookupOrderPaymentPreference",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "orderPaymentPreferenceId", title = "${uiLabelMap.CommonOrderPaymentPreference}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "javascript:set_value('${orderPaymentPreferenceId}')", urlMode = UrlMode.PLAIN, description = "${orderPaymentPreferenceId}", alsoHidden = false)),
            @FormField(name = "paymentMethodTypeId", display = @DisplayField),
            @FormField(name = "paymentMethodId", display = @DisplayField),
            @FormField(name = "maxAmount", display = @DisplayField),
            @FormField(name = "statusId", display = @DisplayField)
        },
        actions = @FormActions(service = {@ServiceAction(serviceName = "performFind", resultMapName = "result", resultMapList = "listIt", fieldMaps = {@FieldMap(fieldName = "inputFields", fromField = "parameters"), @FieldMap(fieldName = "entityName", value = "OrderPaymentPreference"), @FieldMap(fieldName = "viewIndex", fromField = "viewIndex"), @FieldMap(fieldName = "viewSize", fromField = "viewSize")})})
    )
    public interface listOrderPaymentPreference {}

}

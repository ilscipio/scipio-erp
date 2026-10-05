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
public class LedgerGlForms {

    @Form(
        name = "CreateAcctgTrans",
        location = "component://accounting/widget/ledger/GlForms.xml",
        target = "createAcctgTrans",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "AcctgTransType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", keyFieldName = "glFiscalTypeId", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "glJournalId", title = "${uiLabelMap.AccountingGlJournal}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlJournal", description = "${glJournalName} [${glJournalId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "glJournalName")}))),
            @FormField(name = "transactionDate", dateTime = @DateTimeField),
            @FormField(name = "scheduledPostingDate", position = 2, dateTime = @DateTimeField),
            @FormField(name = "isPosted", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "postedDate", position = 2, dateTime = @DateTimeField),
            @FormField(name = "finAccountTransId", text = @TextField),
            @FormField(name = "groupStatusId", title = "${uiLabelMap.FormFieldTitle_groupStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ACCTG_ENREC_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "description", textarea = @TextareaField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName", size = 20, maxlength = 20)),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", lookup = @LookupField(targetFormName = "LookupInvoice", size = 20, maxlength = 20)),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", position = 2, lookup = @LookupField(targetFormName = "LookupPayment", size = 20, maxlength = 20)),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", lookup = @LookupField(targetFormName = "LookupProduct", size = 20, maxlength = 20)),
            @FormField(name = "shipmentId", title = "${uiLabelMap.CommonShipment}", position = 2, lookup = @LookupField(targetFormName = "LookupShipment", size = 20, maxlength = 20)),
            @FormField(name = "inventoryItemId", text = @TextField),
            @FormField(name = "physicalInventoryId", position = 2, text = @TextField),
            @FormField(name = "receiptId", title = "${uiLabelMap.CommonReceipt}", text = @TextField),
            @FormField(name = "theirAcctgTransId", position = 2, text = @TextField),
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.CommonFixedAsset}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAsset", description = "${fixedAssetId}", orderBy = {@EntityOrderBy(fieldName = "fixedAssetId")}))),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", position = 2, lookup = @LookupField(targetFormName = "LookupWorkEffort", size = 20, maxlength = 20)),
            @FormField(name = "voucherRef", text = @TextField),
            @FormField(name = "voucherDate", position = 2, dateTime = @DateTimeField),
            @FormField(name = "createAction", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        sortOrder = @SortOrder()
    )
    public interface CreateAcctgTrans {}

    @Form(
        name = "EditAcctgTrans",
        location = "component://accounting/widget/ledger/GlForms.xml",
        target = "updateAcctgTrans",
        defaultMapName = "acctgTrans",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "acctgTransId", tooltip = "${uiLabelMap.CommonNotModifRecreat}", display = @DisplayField),
            @FormField(name = "organizationPartyId", mapName = "parameter", hidden = @HiddenField),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.CommonType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "AcctgTransType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "acctgTransTypeId")}))),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "glJournalId", title = "${uiLabelMap.AccountingGlJournal}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlJournal", description = "${glJournalName} [${glJournalId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "glJournalName")}))),
            @FormField(name = "transactionDate", dateTime = @DateTimeField),
            @FormField(name = "scheduledPostingDate", position = 2, dateTime = @DateTimeField),
            @FormField(name = "isPosted", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "postedDate", position = 2, dateTime = @DateTimeField),
            @FormField(name = "finAccountTransId", text = @TextField),
            @FormField(name = "groupStatusId", title = "${uiLabelMap.FormFieldTitle_groupStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ACCTG_ENREC_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "description", position = 2, textarea = @TextareaField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName", size = 20, maxlength = 20)),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "roleTypeId")}))),
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", lookup = @LookupField(targetFormName = "LookupInvoice", size = 20, maxlength = 20)),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", position = 2, lookup = @LookupField(targetFormName = "LookupPayment", size = 20, maxlength = 20)),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct", size = 20, maxlength = 20)),
            @FormField(name = "shipmentId", position = 2, lookup = @LookupField(targetFormName = "LookupShipment", size = 20, maxlength = 20)),
            @FormField(name = "inventoryItemId", text = @TextField),
            @FormField(name = "physicalInventoryId", position = 2, text = @TextField),
            @FormField(name = "receiptId", text = @TextField),
            @FormField(name = "theirAcctgTransId", position = 2, text = @TextField),
            @FormField(name = "fixedAssetId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAsset", description = "${fixedAssetId}", orderBy = {@EntityOrderBy(fieldName = "fixedAssetId")}))),
            @FormField(name = "workEffortId", position = 2, lookup = @LookupField(targetFormName = "LookupWorkEffort", size = 20, maxlength = 20)),
            @FormField(name = "voucherRef", text = @TextField),
            @FormField(name = "voucherDate", position = 2, dateTime = @DateTimeField),
            @FormField(name = "createdDate", display = @DisplayField),
            @FormField(name = "lastModifiedDate", position = 2, display = @DisplayField),
            @FormField(name = "createdByUserLogin", display = @DisplayField),
            @FormField(name = "lastModifiedByUserLogin", position = 2, display = @DisplayField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField)
        },
        sortOrder = @SortOrder()
    )
    public interface EditAcctgTrans {}

    @Form(
        name = "ViewAcctgTrans",
        location = "component://accounting/widget/ledger/GlForms.xml",
        defaultMapName = "acctgTrans",
        autoFieldsEntity = {
            @AutoFieldsEntity(entityName = "AcctgTrans", defaultFieldType = DefaultFieldType.DISPLAY)
        },
        fields = {
            @FormField(name = "acctgTransId"),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.CommonType}", position = 2, displayEntity = @DisplayEntityField(entityName = "AcctgTransType")),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", displayEntity = @DisplayEntityField(entityName = "GlFiscalType")),
            @FormField(name = "glJournalId", title = "${uiLabelMap.AccountingGlJournal}", position = 2, displayEntity = @DisplayEntityField(entityName = "GlJournal", description = "${glJournalName}")),
            @FormField(name = "transactionDate"),
            @FormField(name = "scheduledPostingDate", position = 2),
            @FormField(name = "isPosted"),
            @FormField(name = "postedDate", position = 2),
            @FormField(name = "groupStatusId", title = "${uiLabelMap.FormFieldTitle_groupStatus}", position = 2, displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "finAccountTransId"),
            @FormField(name = "description"),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}"),
            @FormField(name = "roleTypeId", title = "${uiLabelMap.CommonRole}", position = 2, displayEntity = @DisplayEntityField(entityName = "RoleType")),
            @FormField(name = "inventoryItemId"),
            @FormField(name = "physicalInventoryId", position = 2),
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "invoiceOverview?invoiceId=${acctgTrans.invoiceId}", description = "${acctgTrans.invoiceId}")),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "paymentOverview?paymentId=${acctgTrans.paymentId}", description = "${acctgTrans.paymentId}")),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", display = @DisplayField),
            @FormField(name = "shipmentId", title = "${uiLabelMap.CommonShipment}", position = 2),
            @FormField(name = "receiptId", title = "${uiLabelMap.CommonReceipt}"),
            @FormField(name = "theirAcctgTransId", position = 2),
            @FormField(name = "fixedAssetId", title = "${uiLabelMap.AccountingFixedAsset}"),
            @FormField(name = "workEffortId", position = 2),
            @FormField(name = "voucherRef"),
            @FormField(name = "voucherDate", position = 2),
            @FormField(name = "createdByUserLogin", hidden = @HiddenField),
            @FormField(name = "lastModifiedByUserLogin", hidden = @HiddenField)
        },
        sortOrder = @SortOrder()
    )
    public interface ViewAcctgTrans {}

    @Form(
        name = "FindAcctgTrans",
        location = "component://accounting/widget/ledger/GlForms.xml",
        target = "FindAcctgTrans",
        defaultMapName = "journal",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "acctgTransId", entityName = "AcctgTrans", title = "${uiLabelMap.FormFieldTitle_acctgTransId}", text = @TextField(size = 20)),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.AccountingTransactionType}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "AcctgTransType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "acctgTransTypeId")}))),
            @FormField(name = "glAccountId", title = "${uiLabelMap.FormFieldTitle_glAccountId}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccount", description = "${accountCode} ${accountName}", orderBy = {@EntityOrderBy(fieldName = "glAccountId")}))),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", position = 2, lookup = @LookupField(targetFormName = "LookupPartyName")),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "glJournalId", title = "${uiLabelMap.AccountingGlJournal}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlJournal", description = "${glJournalName} [${glJournalId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "glJournalName")}))),
            @FormField(name = "isPosted", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", position = 2, lookup = @LookupField(targetFormName = "LookupInvoice", size = 20, maxlength = 20)),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", lookup = @LookupField(targetFormName = "LookupPayment", size = 20, maxlength = 20)),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", position = 2, lookup = @LookupField(targetFormName = "LookupProduct", size = 20, maxlength = 20)),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", lookup = @LookupField(targetFormName = "LookupWorkEffort", size = 20, maxlength = 20)),
            @FormField(name = "shipmentId", title = "${uiLabelMap.CommonShipment}", position = 2, lookup = @LookupField(targetFormName = "LookupShipment", size = 20, maxlength = 20)),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "performSearch", hidden = @HiddenField(value = "Y")),
            @FormField(name = "searchAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindAcctgTrans {}

    @Form(
        name = "ListAcctgTrans",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.LIST,
        title = "List Accounting Transactions",
        listName = "listIt",
        defaultEntityName = "AcctgTransAndEntries",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "acctgTransId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditAcctgTrans", description = "${acctgTransId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "acctgTransId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "transactionDate", display = @DisplayField),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "AcctgTransType")),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", displayEntity = @DisplayEntityField(entityName = "GlFiscalType")),
            @FormField(name = "invoiceId", useWhen = "invoiceId!=null", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "editInvoice", description = "${invoiceId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "invoiceId")})),
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", useWhen = "invoiceId==null", display = @DisplayField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", useWhen = "paymentId!=null", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "editPayment", description = "${paymentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", useWhen = "paymentId==null", display = @DisplayField),
            @FormField(name = "workEffortId", useWhen = "workEffortId!=null", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/workeffort/control/EditWorkEffort", urlMode = UrlMode.INTER_APP, description = "${workEffortId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "workEffortId", useWhen = "workEffortId==null", display = @DisplayField),
            @FormField(name = "shipmentId", useWhen = "shipmentId!=null", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/facility/control/EditShipment", urlMode = UrlMode.INTER_APP, description = "${shipmentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shipmentId")})),
            @FormField(name = "shipmentId", useWhen = "shipmentId==null", display = @DisplayField),
            @FormField(name = "isPosted", display = @DisplayField),
            @FormField(name = "postedDate", display = @DisplayField),
            @FormField(name = "postAcctgTrans", title = " ", useWhen = "\"N\".equals(isPosted)", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "postAcctgTrans", description = "${uiLabelMap.AccountingPostTransaction}", parameters = {@ParameterDef(paramName = "acctgTransId")})),
            @FormField(name = "createTransactionDetailReportPDF", title = "${uiLabelMap.AccountingInvoicePDF}", useWhen = "${isPosted=='Y'}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", hyperlink = @HyperlinkField(target = "acctgTransDetailReportPdf.pdf", description = "${uiLabelMap.AccountingInvoicePDF}", targetWindow = "_BLANK", parameters = {@ParameterDef(paramName = "acctgTransId")})),
            @FormField(name = "postAcctgTrans", title = "${uiLabelMap.AccountingPostTransaction}", useWhen = "!\"N\".equals(isPosted)", display = @DisplayField)
        }
    )
    public interface ListAcctgTrans {}

    @Form(
        name = "ListUnpostedAcctgTrans",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.LIST,
        title = "Unposted Accounting Transactions",
        listName = "transactions",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "acctgTransId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditAcctgTrans", description = "${acctgTransId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "acctgTransId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "transactionDate", display = @DisplayField),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.FormFieldTitle_acctgTransType}", displayEntity = @DisplayEntityField(entityName = "AcctgTransType")),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", displayEntity = @DisplayEntityField(entityName = "GlFiscalType")),
            @FormField(name = "invoiceId", useWhen = "invoiceId!=null", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "editInvoice", description = "${invoiceId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "invoiceId")})),
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", useWhen = "invoiceId==null", display = @DisplayField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", useWhen = "paymentId!=null", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "editPayment", description = "${paymentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", useWhen = "paymentId==null", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", display = @DisplayField),
            @FormField(name = "workEffortId", display = @DisplayField),
            @FormField(name = "shipmentId", title = "${uiLabelMap.CommonShipment}", display = @DisplayField),
            @FormField(name = "verifyAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_view}", hyperlink = @HyperlinkField(target = "postAcctgTrans", description = "${uiLabelMap.AccountingVerifyTransaction}", alsoHidden = false, parameters = {@ParameterDef(paramName = "acctgTransId"), @ParameterDef(paramName = "organizationPartyId"), @ParameterDef(paramName = "verifyOnly", value = "Y")})),
            @FormField(name = "postAction", title = " ", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "postAcctgTrans", description = "${uiLabelMap.AccountingPostTransaction}", alsoHidden = false, parameters = {@ParameterDef(paramName = "acctgTransId"), @ParameterDef(paramName = "organizationPartyId")}))
        }
    )
    public interface ListUnpostedAcctgTrans {}

    @Form(
        name = "ListAcctgTransOverview",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.LIST,
        title = "List Accounting Transactions",
        listName = "listIt",
        defaultEntityName = "AcctgTransAndEntries",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "acctgTransId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditAcctgTrans", description = "${acctgTransId} - ${acctgTransEntrySeqId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "acctgTransId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountCode", widgetStyle = "${styles.link_nav_info_id} ${styles.action_find}", hyperlink = @HyperlinkField(target = "FindAcctgTransEntries", description = "${accountCode}", parameters = {@ParameterDef(paramName = "glAccountId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "accountName", display = @DisplayField),
            @FormField(name = "postedDebits", title = "${uiLabelMap.AccountingDebitFlag}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(description = "${groovy:if(debitCreditFlag.equals('D'))return(amount);if(debitCreditFlag.equals('C'))return('');}", type = "currency")),
            @FormField(name = "postedCredits", title = "${uiLabelMap.AccountingCreditFlag}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(description = "${groovy:if(debitCreditFlag.equals('C'))return(amount);if(debitCreditFlag.equals('D'))return('');}", type = "currency")),
            @FormField(name = "transactionDate", display = @DisplayField(type = "date-time")),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "AcctgTransType")),
            @FormField(name = "transDescription", display = @DisplayField),
            @FormField(name = "reconcileStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "isPosted", display = @DisplayField),
            @FormField(name = "postAcctgTrans", title = " ", useWhen = "\"N\".equals(isPosted)", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", hyperlink = @HyperlinkField(target = "postAcctgTrans", description = "${uiLabelMap.AccountingPostTransaction}", parameters = {@ParameterDef(paramName = "acctgTransId")})),
            @FormField(name = "createTransactionDetailReportPDF", title = "${uiLabelMap.AccountingInvoicePDF}", useWhen = "${isPosted=='Y'}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", hyperlink = @HyperlinkField(target = "acctgTransDetailReportPdf.pdf", description = "${uiLabelMap.AccountingInvoicePDF}", targetWindow = "_BLANK", parameters = {@ParameterDef(paramName = "acctgTransId")})),
            @FormField(name = "postAcctgTrans", title = "${uiLabelMap.AccountingPostTransaction}", useWhen = "!\"N\".equals(isPosted)", display = @DisplayField)
        }
    )
    public interface ListAcctgTransOverview {}

    @Form(
        name = "EditAcctgTransEntry",
        location = "component://accounting/widget/ledger/GlForms.xml",
        target = "createAcctgTransEntry",
        defaultEntityName = "AcctgTransEntry",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "acctgTransId", hidden = @HiddenField),
            @FormField(name = "acctgTransEntrySeqId", hidden = @HiddenField),
            @FormField(name = "acctgTransEntryTypeId", hidden = @HiddenField(value = "_NA_")),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.FormFieldTitle_glAccountType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "glAccountTypeId")}))),
            @FormField(name = "glAccountId", entryName = "resetFieldValue", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "parameters.organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "debitCreditFlag", entryName = "resetFieldValue", dropDown = @DropDownField(current = "selected", options = {@Option(key = "C", description = "${uiLabelMap.FormFieldTitle_credit}"), @Option(key = "D", description = "${uiLabelMap.FormFieldTitle_debit}")})),
            @FormField(name = "partyId", position = 2, text = @TextField(size = 30)),
            @FormField(name = "origAmount", entryName = "resetFieldValue", text = @TextField(size = 30)),
            @FormField(name = "origCurrencyUomId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Uom", description = "${description} - ${abbreviation}", keyFieldName = "uomId", constraints = {@EntityConstraint(name = "uomTypeId", value = "CURRENCY_MEASURE")}, orderBy = {@EntityOrderBy(fieldName = "description")}))),
            @FormField(name = "purposeEnumId", title = "${uiLabelMap.CommonPurpose}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "Enumeration", description = "${description}", keyFieldName = "enumId", constraints = {@EntityConstraint(name = "enumTypeId", value = "CONVERSION_PURPOSE")}, orderBy = {@EntityOrderBy(fieldName = "sequenceId")}))),
            @FormField(name = "voucherRef", position = 2, text = @TextField(size = 30)),
            @FormField(name = "productId", text = @TextField(size = 20)),
            @FormField(name = "reconcileStatusId", title = "${uiLabelMap.FormFieldTitle_reconcileStatus}", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ACCTG_ENREC_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "statusId")}))),
            @FormField(name = "settlementTermId", text = @TextField(size = 20)),
            @FormField(name = "isSummary", position = 2, text = @TextField(size = 10)),
            @FormField(name = "description", entryName = "resetFieldValue", position = 2, textarea = @TextareaField),
            @FormField(name = "createAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface EditAcctgTransEntry {}

    @Form(
        name = "ListAcctgTransEntries",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.LIST,
        target = "updateAcctgTransEntry",
        listName = "acctgTransEntries",
        defaultEntityName = "AcctgTransEntry",
        oddRowStyle = "alternate-row",
        separateColumns = true,
        useRowSubmit = true,
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "acctgTransId", hidden = @HiddenField),
            @FormField(name = "acctgTransEntrySeqId", display = @DisplayField),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.FormFieldTitle_glAccountType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "glAccountTypeId")}))),
            @FormField(name = "glAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "parameters.organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "voucherRef", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName", size = 20, maxlength = 20)),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", lookup = @LookupField(targetFormName = "LookupProduct", size = 20, maxlength = 20)),
            @FormField(name = "reconcileStatusId", title = "${uiLabelMap.CommonStatus}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", keyFieldName = "statusId", constraints = {@EntityConstraint(name = "statusTypeId", value = "ACCTG_ENREC_STATUS")}, orderBy = {@EntityOrderBy(fieldName = "statusId")}))),
            @FormField(name = "isSummary", display = @DisplayField),
            @FormField(name = "debitCreditFlag", dropDown = @DropDownField(current = "selected", options = {@Option(key = "C", description = "${uiLabelMap.FormFieldTitle_credit}"), @Option(key = "D", description = "${uiLabelMap.FormFieldTitle_debit}")})),
            @FormField(name = "origAmount", display = @DisplayField(type = "currency")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", display = @DisplayField(type = "currency")),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField),
            @FormField(name = "removeAction", title = "${uiLabelMap.CommonRemove}", widgetStyle = "${styles.link_run_sys} ${styles.action_remove}", hyperlink = @HyperlinkField(target = "deleteAcctgTransEntry", description = "${uiLabelMap.CommonRemove}", parameters = {@ParameterDef(paramName = "acctgTransId"), @ParameterDef(paramName = "acctgTransEntrySeqId"), @ParameterDef(paramName = "organizationPartyId")}))
        }
    )
    public interface ListAcctgTransEntries {}

    @Form(
        name = "FindAcctgTransEntries",
        location = "component://accounting/widget/ledger/GlForms.xml",
        target = "FindAcctgTransEntries",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "acctgTransId", entityName = "AcctgTrans", title = "${uiLabelMap.FormFieldTitle_acctgTransId}", text = @TextField(size = 20)),
            @FormField(name = "glAccountId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.AccountingTransactionType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "AcctgTransType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "acctgTransTypeId")}))),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "glJournalId", title = "${uiLabelMap.AccountingGlJournal}", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "GlJournal", description = "${glJournalName} [${glJournalId}]", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "glJournalName")}))),
            @FormField(name = "isPosted", dropDown = @DropDownField(allowEmpty = true, options = {@Option(key = "N", description = "${uiLabelMap.CommonNo}"), @Option(key = "Y", description = "${uiLabelMap.CommonYes}")})),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", lookup = @LookupField(targetFormName = "LookupPartyName", size = 20, maxlength = 20)),
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", lookup = @LookupField(targetFormName = "LookupInvoice", size = 20, maxlength = 20)),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", lookup = @LookupField(targetFormName = "LookupPayment", size = 20, maxlength = 20)),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", lookup = @LookupField(targetFormName = "LookupProduct", size = 20, maxlength = 20)),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", lookup = @LookupField(targetFormName = "LookupWorkEffort", size = 20, maxlength = 20)),
            @FormField(name = "shipmentId", title = "${uiLabelMap.CommonShipment}", lookup = @LookupField(targetFormName = "LookupShipment", size = 20, maxlength = 20)),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "performSearch", hidden = @HiddenField(value = "Y")),
            @FormField(name = "reportType", dropDown = @DropDownField(options = {@Option(key = "byAccount", description = "${uiLabelMap.AccountingByAccount}"), @Option(key = "byDate", description = "${uiLabelMap.AccountingByDate}")})),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonFind}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindAcctgTransEntries {}

    @Form(
        name = "ListFindAcctgTransEntriesByAccount",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.LIST,
        title = "List Accounting Transaction Entries",
        listName = "acctgTransEntryList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "accountCode", useWhen = "showPosition1", display = @DisplayField),
            @FormField(name = "glAccountDescription", title = "${uiLabelMap.CommonDescription}", useWhen = "showPosition1", display = @DisplayField(description = "${accountName}")),
            @FormField(name = "glAccountClassId", useWhen = "showPosition1", displayEntity = @DisplayEntityField(entityName = "GlAccountClass")),
            @FormField(name = "acctgTransId", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "EditAcctgTrans", description = "${acctgTransId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "acctgTransId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "acctgTransEntrySeqId", widgetStyle = "${styles.link_nav_info_id}", position = 2, display = @DisplayField),
            @FormField(name = "transactionDate", position = 2, display = @DisplayField),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.FormFieldTitle_acctgTransType}", position = 2, displayEntity = @DisplayEntityField(entityName = "AcctgTransType")),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", position = 2, displayEntity = @DisplayEntityField(entityName = "GlFiscalType")),
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", useWhen = "invoiceId!=null", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "editInvoice", description = "${invoiceId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "invoiceId")})),
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", useWhen = "invoiceId==null", position = 2, display = @DisplayField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", useWhen = "paymentId!=null", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "editPayment", description = "${paymentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", useWhen = "paymentId==null", position = 2, display = @DisplayField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", useWhen = "workEffortId!=null", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "/workeffort/control/EditWorkEffort", urlMode = UrlMode.INTER_APP, description = "${workEffortId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", useWhen = "workEffortId==null", position = 2, display = @DisplayField),
            @FormField(name = "shipmentId", title = "${uiLabelMap.CommonShipment}", useWhen = "shipmentId!=null", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "/facility/control/EditShipment", urlMode = UrlMode.INTER_APP, description = "${shipmentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shipmentId")})),
            @FormField(name = "shipmentId", title = "${uiLabelMap.CommonShipment}", useWhen = "shipmentId==null", position = 2, display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", useWhen = "partyId!=null", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", useWhen = "partyId==null", position = 2, display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", useWhen = "productId!=null", widgetStyle = "${styles.link_nav_info_id}", position = 2, hyperlink = @HyperlinkField(target = "/catalog/control/ViewProduct", urlMode = UrlMode.INTER_APP, description = "${productId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", useWhen = "productId==null", position = 2, display = @DisplayField),
            @FormField(name = "isPosted", position = 2, display = @DisplayField),
            @FormField(name = "postedDate", position = 2, display = @DisplayField),
            @FormField(name = "debitCreditFlag", position = 2, display = @DisplayField),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", position = 2, display = @DisplayField(type = "currency"))
        },
        rowActions = @RowActions(set = {@SetAction(field = "showPosition1", value = "${groovy:String prev=(String)previousItem.get(\"glAccountId\");return new Boolean(!(prev!=null&&prev.equals(glAccountId)));}", type = "Boolean")})
    )
    public interface ListFindAcctgTransEntriesByAccount {}

    @Form(
        name = "ListFindAcctgTransEntriesByDate",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.LIST,
        title = "List Accounting Transaction Entries",
        listName = "acctgTransEntryList",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "transactionDate", display = @DisplayField),
            @FormField(name = "acctgTransId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditAcctgTrans", description = "${acctgTransId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "acctgTransId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "acctgTransEntrySeqId", widgetStyle = "${styles.link_nav_info_id}", display = @DisplayField),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.FormFieldTitle_acctgTransType}", displayEntity = @DisplayEntityField(entityName = "AcctgTransType")),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", displayEntity = @DisplayEntityField(entityName = "GlFiscalType")),
            @FormField(name = "glAccountId", title = "${uiLabelMap.CommonAccount}", display = @DisplayField),
            @FormField(name = "glAccountDescription", title = "${uiLabelMap.CommonDescription}", display = @DisplayField(description = "${accountCode} ${accountName}")),
            @FormField(name = "glAccountClassId", title = "${uiLabelMap.CommonClass}", displayEntity = @DisplayEntityField(entityName = "GlAccountClass")),
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", useWhen = "invoiceId!=null", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "editInvoice", description = "${invoiceId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "invoiceId")})),
            @FormField(name = "invoiceId", title = "${uiLabelMap.AccountingInvoice}", useWhen = "invoiceId==null", display = @DisplayField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", useWhen = "paymentId!=null", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "editPayment", description = "${paymentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "paymentId")})),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", useWhen = "paymentId==null", display = @DisplayField),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", useWhen = "workEffortId!=null", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/workeffort/control/EditWorkEffort", urlMode = UrlMode.INTER_APP, description = "${workEffortId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "workEffortId")})),
            @FormField(name = "workEffortId", title = "${uiLabelMap.WorkEffort}", useWhen = "workEffortId==null", display = @DisplayField),
            @FormField(name = "shipmentId", title = "${uiLabelMap.CommonShipment}", useWhen = "shipmentId!=null", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/facility/control/EditShipment", urlMode = UrlMode.INTER_APP, description = "${shipmentId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "shipmentId")})),
            @FormField(name = "shipmentId", title = "${uiLabelMap.CommonShipment}", useWhen = "shipmentId==null", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", useWhen = "partyId!=null", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", useWhen = "partyId==null", display = @DisplayField),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", useWhen = "productId!=null", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/catalog/control/ViewProduct", urlMode = UrlMode.INTER_APP, description = "${productId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "productId")})),
            @FormField(name = "productId", title = "${uiLabelMap.AccountingProduct}", useWhen = "productId==null", display = @DisplayField),
            @FormField(name = "isPosted", display = @DisplayField),
            @FormField(name = "postedDate", display = @DisplayField),
            @FormField(name = "debitCreditFlag", display = @DisplayField),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", display = @DisplayField(type = "currency"))
        }
    )
    public interface ListFindAcctgTransEntriesByDate {}

    @Form(
        name = "CreateAcctgTransAndEntries",
        location = "component://accounting/widget/ledger/GlForms.xml",
        target = "quickCreateAcctgTransAndEntries",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.FormFieldTitle_acctgTransType}", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "AcctgTransType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "acctgTransTypeId")}))),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", position = 2, dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlFiscalType", description = "${description}", orderBy = {@EntityOrderBy(fieldName = "glFiscalTypeId")}))),
            @FormField(name = "partyId", title = "${uiLabelMap.PartyPartyId}", lookup = @LookupField(targetFormName = "LookupPartyName", size = 20, maxlength = 20)),
            @FormField(name = "roleTypeId", parameterName = "roleTypeId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "RoleType", description = "${description}", keyFieldName = "roleTypeId", orderBy = {@EntityOrderBy(fieldName = "roleTypeId")}))),
            @FormField(name = "invoiceId", lookup = @LookupField(targetFormName = "LookupInvoice", size = 20, maxlength = 20)),
            @FormField(name = "paymentId", position = 2, lookup = @LookupField(targetFormName = "LookupPayment", size = 20, maxlength = 20)),
            @FormField(name = "productId", lookup = @LookupField(targetFormName = "LookupProduct", size = 20, maxlength = 20)),
            @FormField(name = "workEffortId", position = 2, lookup = @LookupField(targetFormName = "LookupWorkEffort", size = 20, maxlength = 20)),
            @FormField(name = "shipmentId", lookup = @LookupField(targetFormName = "LookupShipment", size = 20, maxlength = 20)),
            @FormField(name = "fixedAssetId", position = 2, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "FixedAsset", description = "${fixedAssetId}", orderBy = {@EntityOrderBy(fieldName = "fixedAssetId")}))),
            @FormField(name = "debitGlAccountId", useWhen = "debitGlAccountClassId!=null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "parameters.organizationPartyId"), @EntityConstraint(name = "glAccountClassId", envName = "debitGlAccountClassId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "debitGlAccountId", useWhen = "debitGlAccountClassId==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "parameters.organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "creditGlAccountId", useWhen = "creditGlAccountClassId!=null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "parameters.organizationPartyId"), @EntityConstraint(name = "glAccountClassId", envName = "creditGlAccountClassId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "creditGlAccountId", useWhen = "creditGlAccountClassId==null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccountOrganizationAndClass", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", constraints = {@EntityConstraint(name = "organizationPartyId", envName = "parameters.organizationPartyId")}, orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", text = @TextField(size = 20, maxlength = 20)),
            @FormField(name = "transactionDate", dateTime = @DateTimeField),
            @FormField(name = "description", text = @TextField(size = 60)),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonCreate}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField)
        },
        actions = @FormActions(set = {@SetAction(field = "debitGlAccountClassId", fromField = "parameters.debitGlAccountClassId"), @SetAction(field = "creditGlAccountClassId", fromField = "parameters.creditGlAccountClassId")})
    )
    public interface CreateAcctgTransAndEntries {}

    @Form(
        name = "ViewAcctgTransEntries",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.LIST,
        listName = "acctgTransEntries",
        defaultEntityName = "AcctgTransEntry",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "acctgTransId", hidden = @HiddenField),
            @FormField(name = "acctgTransEntrySeqId", display = @DisplayField),
            @FormField(name = "glAccountId", displayEntity = @DisplayEntityField(entityName = "GlAccount", description = "${glAccountId} ${accountName}")),
            @FormField(name = "debitCreditFlag", titleAreaStyle = "align-center", widgetAreaStyle = "align-center", display = @DisplayField),
            @FormField(name = "origAmount", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "glAccountTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "GlAccountType")),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "reconcileStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId"))
        }
    )
    public interface ViewAcctgTransEntries {}

    @Form(
        name = "FindGlAccountReconciliation",
        location = "component://accounting/widget/ledger/GlForms.xml",
        target = "findGlAccountReconciliation",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "glAccountId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccount", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "performSearch", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSearch}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindGlAccountReconciliation {}

    @Form(
        name = "ListGlAccountReconciliation",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.MULTI,
        target = "EditGlReconciliation?organizationPartyId=${organizationPartyId}&activeSubMenuItem=AccountReconciliation",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "acctgTransId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditAcctgTrans", description = "${acctgTransId}", parameters = {@ParameterDef(paramName = "acctgTransId"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "acctgTransEntrySeqId", display = @DisplayField),
            @FormField(name = "glAccountId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "GlAccountNavigate", description = "${glAccountId}", parameters = {@ParameterDef(paramName = "glAccountId")})),
            @FormField(name = "partyId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${partyIdTo}", parameters = {@ParameterDef(paramName = "partyId")})),
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "organizationPartyId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${organizationPartyId}", parameters = {@ParameterDef(paramName = "partyId", fromField = "organizationPartyId")})),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", display = @DisplayField),
            @FormField(name = "submitAction", title = "${uiLabelMap.AccountingCreateAcctRecons}", widgetStyle = "${styles.link_run_sys} ${styles.action_add}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface ListGlAccountReconciliation {}

    @Form(
        name = "EditGlReconciliation",
        location = "component://accounting/widget/ledger/GlForms.xml",
        target = "updateGlReconciliation?activeSubMenuItem=${activeSubMenuItem}",
        defaultMapName = "glReconciliation",
        headerRowStyle = "header-row",
        autoFieldsService = {
            @AutoFieldsService(serviceName = "updateGlReconciliation")
        },
        fields = {
            @FormField(name = "glReconciliationId", display = @DisplayField),
            @FormField(name = "glReconciliationName", text = @TextField),
            @FormField(name = "description", text = @TextField),
            @FormField(name = "glAccountId", display = @DisplayField),
            @FormField(name = "statusId", useWhen = "glReconciliationId == null", hidden = @HiddenField(value = "GLREC_CREATED")),
            @FormField(name = "statusId", title = "${uiLabelMap.CommonStatus}", useWhen = "glReconciliationId != null", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "StatusItem", description = "${description}", constraints = {@EntityConstraint(name = "statusTypeId", value = "GLREC_STATUS")}))),
            @FormField(name = "reconciledDate", dateTime = @DateTimeField),
            @FormField(name = "organizationPartyId", display = @DisplayField),
            @FormField(name = "openingBalance", useWhen = "\"GLREC_RECONCILED\".equals(\"${glReconciliation.statusId}\")", display = @DisplayField),
            @FormField(name = "reconciledBalance", position = 2, display = @DisplayField),
            @FormField(name = "updateAction", title = "${uiLabelMap.CommonUpdate}", useWhen = "glReconciliationId!=null", widgetStyle = "${styles.link_run_sys} ${styles.action_update}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface EditGlReconciliation {}

    @Form(
        name = "ListGlReconciliationEntries",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.LIST,
        oddRowStyle = "alternate-row",
        useRowSubmit = true,
        fields = {
            @FormField(name = "glReconciliationId", display = @DisplayField),
            @FormField(name = "acctgTransId", display = @DisplayField),
            @FormField(name = "acctgTransEntrySeqId", display = @DisplayField),
            @FormField(name = "reconciledAmount", display = @DisplayField),
            @FormField(name = "lastUpdatedStamp", display = @DisplayField)
        }
    )
    public interface ListGlReconciliationEntries {}

    @Form(
        name = "FindGlAccountReconciliations",
        location = "component://accounting/widget/ledger/GlForms.xml",
        target = "findGlAccountReconciliations",
        headerRowStyle = "header-row",
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField),
            @FormField(name = "glAccountId", dropDown = @DropDownField(entityOptions = @EntityOptions(entityName = "GlAccount", description = "${accountCode} - ${accountName} [${glAccountId}]", keyFieldName = "glAccountId", orderBy = {@EntityOrderBy(fieldName = "accountCode")}))),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField(type = "date")),
            @FormField(name = "performSearch", hidden = @HiddenField(value = "Y")),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSearch}", widgetStyle = "${styles.link_run_sys} ${styles.action_find}", submit = @SubmitField(buttonType = "text-link"))
        }
    )
    public interface FindGlAccountReconciliations {}

    @Form(
        name = "ListGlAccountReconciliations",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.LIST,
        defaultEntityName = "GlReconciliation",
        oddRowStyle = "alternate-row",
        fields = {
            @FormField(name = "glReconciliationId", title = "${uiLabelMap.CommonId}", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "EditGlReconciliations", description = "${glReconciliationId}", alsoHidden = false, parameters = {@ParameterDef(paramName = "glReconciliationId"), @ParameterDef(paramName = "activeSubMenuItem", fromField = "AccountReconciliations"), @ParameterDef(paramName = "organizationPartyId")})),
            @FormField(name = "glReconciliationName", title = "${uiLabelMap.CommonName}", display = @DisplayField),
            @FormField(name = "description", display = @DisplayField),
            @FormField(name = "glAccountId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "GlAccountNavigate", description = "${glAccountId}", parameters = {@ParameterDef(paramName = "glAccountId")})),
            @FormField(name = "organizationPartyId", widgetStyle = "${styles.link_nav_info_id}", hyperlink = @HyperlinkField(target = "/partymgr/control/viewprofile", urlMode = UrlMode.INTER_APP, description = "${organizationPartyId}", parameters = {@ParameterDef(paramName = "partyId", fromField = "organizationPartyId")})),
            @FormField(name = "reconciledBalance", titleAreaStyle = "align-right", widgetAreaStyle = "amount", display = @DisplayField),
            @FormField(name = "createdByUserLogin", display = @DisplayField),
            @FormField(name = "lastModifiedByUserLogin", display = @DisplayField)
        }
    )
    public interface ListGlAccountReconciliations {}

    @Form(
        name = "AcctgTransEntriesSearchResultsCsv",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.LIST,
        title = "List Accounting Transaction Entries",
        listName = "acctgTransEntryList",
        paginate = "false",
        fields = {
            @FormField(name = "acctgTransId", display = @DisplayField),
            @FormField(name = "acctgTransEntrySeqId", display = @DisplayField(description = "${acctgTransEntrySeqId}")),
            @FormField(name = "transactionDate", display = @DisplayField),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.AccountingTransactionType}", displayEntity = @DisplayEntityField(entityName = "AcctgTransType")),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", displayEntity = @DisplayEntityField(entityName = "GlFiscalType")),
            @FormField(name = "glAccountId", display = @DisplayField),
            @FormField(name = "glAccountDescription", display = @DisplayField(description = "${accountName}")),
            @FormField(name = "glAccountClassId", title = "${uiLabelMap.AccountingGlAccountClass}", displayEntity = @DisplayEntityField(entityName = "GlAccountClass")),
            @FormField(name = "invoiceId", title = "${uiLabelMap.CommonAmount}", display = @DisplayField),
            @FormField(name = "paymentId", display = @DisplayField),
            @FormField(name = "workEffortId", display = @DisplayField),
            @FormField(name = "shipmentId", display = @DisplayField),
            @FormField(name = "partyId", display = @DisplayField),
            @FormField(name = "productId", display = @DisplayField),
            @FormField(name = "isPosted", display = @DisplayField),
            @FormField(name = "postedDate", display = @DisplayField),
            @FormField(name = "debitCreditFlag", display = @DisplayField),
            @FormField(name = "amount", title = "${uiLabelMap.CommonAmount}", display = @DisplayField)
        }
    )
    public interface AcctgTransEntriesSearchResultsCsv {}

    @Form(
        name = "AcctgTransSearchResultsCsv",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.LIST,
        title = "List Accounting Transactions",
        listName = "acctgTransList",
        paginate = "false",
        fields = {
            @FormField(name = "acctgTransId", display = @DisplayField),
            @FormField(name = "transactionDate", display = @DisplayField),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.AccountingTransactionType}", displayEntity = @DisplayEntityField(entityName = "AcctgTransType")),
            @FormField(name = "glFiscalTypeId", title = "${uiLabelMap.FormFieldTitle_glFiscalType}", displayEntity = @DisplayEntityField(entityName = "GlFiscalType")),
            @FormField(name = "invoiceId", display = @DisplayField),
            @FormField(name = "paymentId", display = @DisplayField),
            @FormField(name = "workEffortId", display = @DisplayField),
            @FormField(name = "shipmentId", display = @DisplayField),
            @FormField(name = "isPosted", display = @DisplayField),
            @FormField(name = "postedDate", display = @DisplayField)
        }
    )
    public interface AcctgTransSearchResultsCsv {}

    @Form(
        name = "AcctgTransDetailReportPdf",
        location = "component://accounting/widget/ledger/GlForms.xml",
        defaultMapName = "acctgTrans",
        fields = {
            @FormField(name = "acctgTransId", display = @DisplayField),
            @FormField(name = "acctgTransTypeId", useWhen = "${acctgTrans.acctgTransTypeId!=null}", displayEntity = @DisplayEntityField(entityName = "AcctgTransType", keyFieldName = "acctgTransTypeId")),
            @FormField(name = "description", useWhen = "${acctgTrans.description!=null}", display = @DisplayField),
            @FormField(name = "transactionDate", useWhen = "${acctgTrans.transactionDate!=null}", display = @DisplayField),
            @FormField(name = "isPosted", useWhen = "${acctgTrans.isPosted!=null}", display = @DisplayField),
            @FormField(name = "postedDate", useWhen = "${acctgTrans.postedDate!=null}", display = @DisplayField),
            @FormField(name = "scheduledPostingDate", useWhen = "${acctgTrans.scheduledPostingDate!=null}", display = @DisplayField),
            @FormField(name = "glJournalId", title = "${uiLabelMap.AccountingGlJournal}", useWhen = "${acctgTrans.glJournalId!=null}", display = @DisplayField),
            @FormField(name = "glFiscalTypeId", useWhen = "${acctgTrans.glFiscalTypeId!=null}", displayEntity = @DisplayEntityField(entityName = "GlFiscalType", keyFieldName = "glFiscalTypeId")),
            @FormField(name = "voucherRef", useWhen = "${acctgTrans.voucherRef!=null}", display = @DisplayField),
            @FormField(name = "voucherDate", useWhen = "${acctgTrans.voucherDate!=null}", display = @DisplayField),
            @FormField(name = "groupStatusId", useWhen = "${acctgTrans.groupStatusId!=null}", display = @DisplayField),
            @FormField(name = "fixedAssetId", useWhen = "${acctgTrans.fixedAssetId!=null}", display = @DisplayField),
            @FormField(name = "inventoryItemId", useWhen = "${acctgTrans.inventoryItemId!=null}", display = @DisplayField),
            @FormField(name = "physicalInventoryId", useWhen = "${acctgTrans.physicalInventoryId!=null}", display = @DisplayField),
            @FormField(name = "partyId", useWhen = "${acctgTrans.partyId!=null}", display = @DisplayField),
            @FormField(name = "roleTypeId", useWhen = "${acctgTrans.roleTypeId!=null}", displayEntity = @DisplayEntityField(entityName = "RoleType", keyFieldName = "roleTypeId")),
            @FormField(name = "invoiceId", useWhen = "${acctgTrans.invoiceId!=null}", display = @DisplayField),
            @FormField(name = "paymentId", useWhen = "${acctgTrans.paymentId!=null}", display = @DisplayField),
            @FormField(name = "finAccountTransId", useWhen = "${acctgTrans.finAccountTransId!=null}", display = @DisplayField),
            @FormField(name = "shipmentId", useWhen = "${acctgTrans.shipmentId!=null}", display = @DisplayField),
            @FormField(name = "receiptId", useWhen = "${acctgTrans.receiptId!=null}", display = @DisplayField),
            @FormField(name = "workEffortId", useWhen = "${acctgTrans.workEffortId!=null}", display = @DisplayField),
            @FormField(name = "theirAcctgTransId", useWhen = "${acctgTrans.theirAcctgTransId!=null}", display = @DisplayField),
            @FormField(name = "createdByUserLogin", useWhen = "${acctgTrans.createdByUserLogin!=null}", display = @DisplayField),
            @FormField(name = "lastModifiedByUserLogin", useWhen = "${acctgTrans.lastModifiedByUserLogin!=null}", display = @DisplayField)
        }
    )
    public interface AcctgTransDetailReportPdf {}

    @Form(
        name = "AcctgTransEntriesDetailReportPdf",
        location = "component://accounting/widget/ledger/GlForms.xml",
        type = FormType.LIST,
        listName = "acctgTransEntries",
        fields = {
            @FormField(name = "accountCode", display = @DisplayField),
            @FormField(name = "accountName", display = @DisplayField),
            @FormField(name = "currencyUomId", title = "${uiLabelMap.AccountingOriginalCurrency}", display = @DisplayField(description = "${origCurrencyUomId}")),
            @FormField(name = "exchangeRate", useWhen = "${origCurrencyUomId!=currencyUomId}", display = @DisplayField(description = "${origAmount/amount} ${origCurrencyUomId}/${currencyUomId}")),
            @FormField(name = "debitAmount", display = @DisplayField(description = "${groovy:if(debitCreditFlag.equals('D'))return(amount);if(debitCreditFlag.equals('C'))return(0);}", type = "currency")),
            @FormField(name = "creditAmount", display = @DisplayField(description = "${groovy:if(debitCreditFlag.equals('C'))return(amount);if(debitCreditFlag.equals('D'))return(0);}", type = "currency"))
        },
        rowActions = @RowActions(set = {@SetAction(field = "accountName", fromField = "glAccount.accountName"), @SetAction(field = "accountCode", fromField = "glAccount.accountCode")}, entityOne = {@EntityOneAction(entityName = "GlAccount", valueField = "glAccount")})
    )
    public interface AcctgTransEntriesDetailReportPdf {}

}

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
public class ToolsImportExportForms {

    @Form(
        name = "ExportInvoice",
        location = "component://accounting/widget/tools/ImportExportForms.xml",
        target = "ExportInvoiceCsv.csv",
        fields = {
            @FormField(name = "invoiceId", requiredField = true, lookup = @LookupField(targetFormName = "LookupInvoice")),
            @FormField(name = "organizationPartyId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRoleNameDetail", description = "${groupName} [${partyId}]", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "groupName")}))),
            @FormField(name = "startDate", dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", submit = @SubmitField)
        }
    )
    public interface ExportInvoice {}

    @Form(
        name = "ExportInvoiceCsv",
        location = "component://accounting/widget/tools/ImportExportForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginate = "false",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        viewSize = 99999,
        fields = {
            @FormField(name = "organizationPartyId", hidden = @HiddenField(value = "${organizationPartyId}")),
            @FormField(name = "invoiceId", title = "invoiceId", display = @DisplayField),
            @FormField(name = "invoiceTypeId", title = "invoiceTypeId", display = @DisplayField),
            @FormField(name = "invoiceDate", title = "invoiceDate", display = @DisplayField),
            @FormField(name = "dueDate", title = "dueDate", display = @DisplayField),
            @FormField(name = "partyIdFrom", title = "partyIdFrom", display = @DisplayField),
            @FormField(name = "partyIdFromTrans", title = "partyIdFromTrans", display = @DisplayField),
            @FormField(name = "partyId", title = "partyId", display = @DisplayField),
            @FormField(name = "partyIdTrans", title = "partyIdTrans", display = @DisplayField),
            @FormField(name = "currencyUomId", title = "currencyUomId", display = @DisplayField),
            @FormField(name = "description", title = "description", display = @DisplayField),
            @FormField(name = "referenceNumber", title = "referenceNumber", display = @DisplayField),
            @FormField(name = "invoiceItemSeqId", title = "invoiceItemSeqId", display = @DisplayField),
            @FormField(name = "invoiceItemTypeId", title = "invoiceItemTypeId", display = @DisplayField),
            @FormField(name = "productId", title = "productId", display = @DisplayField),
            @FormField(name = "productIdTrans", title = "productIdTrans", display = @DisplayField),
            @FormField(name = "itemDescription", title = "itemDescription", display = @DisplayField),
            @FormField(name = "quantity", title = "quantity", display = @DisplayField),
            @FormField(name = "amount", title = "amount", display = @DisplayField)
        }
    )
    public interface ExportInvoiceCsv {}

    @Form(
        name = "ImportInvoice",
        location = "component://accounting/widget/tools/ImportExportForms.xml",
        type = FormType.UPLOAD,
        target = "ImportInvoice",
        fields = {
            @FormField(name = "uploadedFile", requiredField = true, file = @FileField),
            @FormField(name = "organizationPartyId", requiredField = true, dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRoleNameDetail", description = "${groupName} [${partyId}]", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "groupName")}))),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonUpload}", widgetStyle = "${styles.link_run_sys} ${styles.action_import}", submit = @SubmitField)
        }
    )
    public interface ImportInvoice {}

    @Form(
        name = "ExportTransactions",
        location = "component://accounting/widget/tools/ImportExportForms.xml",
        target = "ExportTransaction.csv",
        fields = {
            @FormField(name = "organizationPartyId", dropDown = @DropDownField(allowEmpty = true, entityOptions = @EntityOptions(entityName = "PartyRoleNameDetail", description = "${groupName} [${partyId}]", keyFieldName = "partyId", constraints = {@EntityConstraint(name = "roleTypeId", value = "INTERNAL_ORGANIZATIO")}, orderBy = {@EntityOrderBy(fieldName = "groupName")}))),
            @FormField(name = "acctgTransId", text = @TextField),
            @FormField(name = "fromDate", title = "${uiLabelMap.CommonFrom}", dateTime = @DateTimeField),
            @FormField(name = "thruDate", title = "${uiLabelMap.CommonThru}", position = 2, dateTime = @DateTimeField),
            @FormField(name = "submitAction", title = "${uiLabelMap.CommonSubmit}", widgetStyle = "${styles.link_run_sys} ${styles.action_export}", submit = @SubmitField)
        }
    )
    public interface ExportTransactions {}

    @Form(
        name = "ExportTransactionCsv",
        location = "component://accounting/widget/tools/ImportExportForms.xml",
        type = FormType.LIST,
        listName = "listIt",
        paginate = "false",
        headerRowStyle = "header-row-2",
        oddRowStyle = "alternate-row",
        viewSize = 99999,
        fields = {
            @FormField(name = "acctgTransId", display = @DisplayField),
            @FormField(name = "accountCode", display = @DisplayField),
            @FormField(name = "accountName", display = @DisplayField),
            @FormField(name = "debitCreditFlag", display = @DisplayField),
            @FormField(name = "amount", display = @DisplayField(type = "currency")),
            @FormField(name = "transactionDate", display = @DisplayField(type = "date-time")),
            @FormField(name = "acctgTransTypeId", title = "${uiLabelMap.CommonType}", displayEntity = @DisplayEntityField(entityName = "AcctgTransType")),
            @FormField(name = "transDescription", display = @DisplayField),
            @FormField(name = "reconcileStatusId", title = "${uiLabelMap.CommonStatus}", displayEntity = @DisplayEntityField(entityName = "StatusItem", keyFieldName = "statusId")),
            @FormField(name = "isPosted", display = @DisplayField),
            @FormField(name = "postedDate", display = @DisplayField(type = "date-time")),
            @FormField(name = "postAcctgTrans", title = "${uiLabelMap.AccountingPostTransaction}", display = @DisplayField),
            @FormField(name = "invoiceId", title = "${uiLabelMap.CommonInvoice}", display = @DisplayField),
            @FormField(name = "paymentId", title = "${uiLabelMap.CommonPayment}", display = @DisplayField),
            @FormField(name = "partyId", title = "${uiLabelMap.CommonParty}", display = @DisplayField),
            @FormField(name = "workEffortId", display = @DisplayField),
            @FormField(name = "shipmentId", title = "${uiLabelMap.CommonShipment}", display = @DisplayField)
        }
    )
    public interface ExportTransactionCsv {}

}

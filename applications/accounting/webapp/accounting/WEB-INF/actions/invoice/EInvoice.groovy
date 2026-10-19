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
/*
 * E-invoice tab of an invoice: the check result for the selected format, the available formats, the kept files.
 */
import com.ilscipio.scipio.accounting.einvoice.EInvoiceArchive
import com.ilscipio.scipio.accounting.einvoice.EInvoiceServices
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.util.EntityQuery
import org.ofbiz.entity.util.EntityTypeUtil
import org.ofbiz.service.ServiceUtil

invoice = context.invoice
if (!invoice) {
    return
}
boolean isSales = EntityTypeUtil.hasParentType(delegator, "InvoiceType", "invoiceTypeId", invoice.invoiceTypeId, "parentTypeId", "SALES_INVOICE")
boolean isCreditNote = EntityTypeUtil.hasParentType(delegator, "InvoiceType", "invoiceTypeId", invoice.invoiceTypeId, "parentTypeId", "CUST_RTN_INVOICE")
context.einvOutgoing = isSales || isCreditNote

if (context.einvOutgoing) {
    context.einvCustomerId = isCreditNote ? invoice.partyIdFrom : invoice.partyId
    context.einvOrganizationId = isCreditNote ? invoice.partyId : invoice.partyIdFrom
    context.einvCustomerFormatId = EInvoiceServices.resolveFormat(delegator, invoice, null, false)?.getId()
    String formatId = parameters.formatId ?: (EInvoiceServices.resolveFormat(delegator, invoice, null, true)?.getId() ?: "XRECHNUNG_UBL")
    context.einvFormatId = formatId
    context.einvFormats = EntityQuery.use(delegator).from("Enumeration").where("enumTypeId", "EINV_FORMAT").orderBy("sequenceId").cache().queryList()

    Map check = dispatcher.runSync("checkEInvoice", [invoiceId: invoice.invoiceId, formatId: formatId, userLogin: userLogin, locale: locale])
    if (ServiceUtil.isSuccess(check)) {
        context.einvValid = check.valid
        context.einvErrors = check.errors
        context.einvWarnings = check.warnings
    } else {
        context.einvValid = false
        context.einvErrors = [ServiceUtil.getErrorMessage(check)]
    }

    archived = []
    EntityQuery.use(delegator).from("InvoiceContent").where(EntityCondition.makeCondition([
            EntityCondition.makeCondition("invoiceId", invoice.invoiceId),
            EntityCondition.makeCondition("invoiceContentTypeId", EntityOperator.IN, [EInvoiceArchive.SENT_XML, EInvoiceArchive.SENT_PDF])
    ])).orderBy("-fromDate").queryList().each { ic ->
        content = ic.getRelatedOne("Content", false)
        archived.add([fromDate: ic.fromDate, formatId: content?.description, fileName: content?.contentName])
    }
    context.einvArchived = archived
} else {
    context.einvHasOriginal = EInvoiceArchive.find(delegator, invoice.invoiceId, EInvoiceArchive.ORIGINAL, null) != null
    formatAttr = EntityQuery.use(delegator).from("InvoiceAttribute").where("invoiceId", invoice.invoiceId, "attrName", "EINV_FORMAT").queryOne()
    context.einvReceivedSyntax = formatAttr?.attrValue
}

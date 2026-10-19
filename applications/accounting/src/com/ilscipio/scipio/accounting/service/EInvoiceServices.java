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
package com.ilscipio.scipio.accounting.service;

import com.ilscipio.scipio.service.def.*;

/**
 * E-invoice (EN 16931) service definitions: XRechnung, ZUGFeRD / Factur-X, Peppol BIS Billing 3.0.
 * Implementation: {@link com.ilscipio.scipio.accounting.einvoice.EInvoiceServices}.
 */
public class EInvoiceServices {

    /**
     * Checks if the e-invoice of an invoice can be made, and returns the problems for the user.
     */
    @Service(
        name = "checkEInvoice",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.einvoice.EInvoiceServices",
        invoke = "checkEInvoice",
        description = "Checks if the e-invoice of an invoice can be made; errors and warnings are user messages",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "formatId", type = "String", mode = "INOUT", optional = "true", description = "Empty = the format of the customer or the default"),
            @Attribute(name = "valid", type = "Boolean", mode = "OUT"),
            @Attribute(name = "errors", type = "List", mode = "OUT"),
            @Attribute(name = "warnings", type = "List", mode = "OUT")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "VIEW")
    )
    public interface CheckEInvoice {}

    /**
     * Makes the e-invoice file of an invoice. When the invoice has left INVOICE_IN_PROCESS, the first file is kept
     * with the invoice and returned again.
     */
    @Service(
        name = "generateEInvoice",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.einvoice.EInvoiceServices",
        invoke = "generateEInvoice",
        description = "Makes the e-invoice file (XML or hybrid PDF) of a sales invoice or customer return credit note",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "formatId", type = "String", mode = "INOUT", optional = "true", description = "Empty = the format of the customer or the default"),
            @Attribute(name = "archive", type = "Boolean", mode = "IN", optional = "true", defaultValue = "true", description = "Keep the file with a final invoice"),
            @Attribute(name = "content", type = "Object", mode = "OUT", description = "byte[]"),
            @Attribute(name = "fileName", type = "String", mode = "OUT"),
            @Attribute(name = "contentType", type = "String", mode = "OUT"),
            @Attribute(name = "warnings", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "VIEW")
    )
    public interface GenerateEInvoice {}

    /**
     * Returns the e-invoice attachment for the invoice email (used by sendInvoicePerEmail).
     */
    @Service(
        name = "getEInvoiceEmailAttachments",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.einvoice.EInvoiceServices",
        invoke = "getEInvoiceEmailAttachments",
        description = "The e-invoice attachment of the invoice email, if the customer receives e-invoices by email; replacesPdf = a hybrid PDF replaces the plain PDF",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "formatId", type = "String", mode = "IN", optional = "true", description = "Empty = the format of the customer; NONE = no e-invoice"),
            @Attribute(name = "attachments", type = "List", mode = "OUT"),
            @Attribute(name = "replacesPdf", type = "Boolean", mode = "OUT"),
            @Attribute(name = "warnings", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetEInvoiceEmailAttachments {}

    /**
     * Imports a received e-invoice (CII, UBL, or ZUGFeRD / Factur-X PDF) as a purchase invoice in process.
     */
    @Service(
        name = "importEInvoice",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.einvoice.EInvoiceServices",
        invoke = "importEInvoice",
        description = "Imports a received e-invoice as a purchase invoice in process and keeps the original file",
        auth = "true",
        attributes = {
            @Attribute(name = "uploadedFile", type = "java.nio.ByteBuffer", mode = "IN"),
            @Attribute(name = "_uploadedFile_fileName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_uploadedFile_contentType", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "_uploadedFile_size", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "supplierPartyId", type = "String", mode = "IN", optional = "true", description = "Empty = find the supplier from the file"),
            @Attribute(name = "invoiceId", type = "String", mode = "OUT"),
            @Attribute(name = "warnings", type = "List", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "CREATE")
    )
    public interface ImportEInvoice {}

    /**
     * Creates or updates the e-invoice settings of a party.
     */
    @Service(
        name = "updatePartyEInvoicePref",
        engine = "java",
        location = "com.ilscipio.scipio.accounting.einvoice.EInvoiceServices",
        invoke = "updatePartyEInvoicePref",
        description = "Creates or updates the e-invoice settings (PartyEInvoicePref) of a party; empty fields are stored as null",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "formatEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deliveryChannelEnumId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "einvoiceEmail", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "buyerReference", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "electronicAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "electronicAddressScheme", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactPhone", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactEmail", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "smallBusinessFlag", type = "String", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdatePartyEInvoicePref {}
}

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
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class InvoiceServices {

    /**
     * Get the Next Invoice ID According to Settings on the PartyAcctgPreference Entity for the given Party
     */
    @Service(
        name = "getNextInvoiceId",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "getNextInvoiceId",
        description = "Get the Next Invoice ID According to Settings on the PartyAcctgPreference Entity for the given Party",
        implemented = {@Implements(service = "createInvoice")},
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "OUT")
        }
    )
    public interface GetNextInvoiceId {}

    @Service(
        name = "invoiceSequenceEnforced",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "invoiceSequenceEnforced",
        implemented = {@Implements(service = "getNextInvoiceId")},
        attributes = {
            @Attribute(name = "partyAcctgPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "invoiceId", type = "Long", mode = "OUT")
        }
    )
    public interface InvoiceSequenceEnforced {}

    @Service(
        name = "invoiceSequenceRestart",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "invoiceSequenceRestart",
        implemented = {@Implements(service = "getNextInvoiceId")},
        attributes = {
            @Attribute(name = "partyAcctgPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        }
    )
    public interface InvoiceSequenceRestart {}

    /**
     * Create Invoice Record
     */
    @Service(
        name = "createInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "createInvoice",
        description = "Create Invoice Record",
        defaultEntityName = "Invoice",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "CREATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "invoiceTypeId", mode = "IN", optional = "false"),
            @OverrideAttribute(name = "partyIdFrom", mode = "IN", optional = "false"),
            @OverrideAttribute(name = "partyId", mode = "IN", optional = "false"),
            @OverrideAttribute(name = "description", allowHtml = "any"),
            @OverrideAttribute(name = "invoiceMessage", allowHtml = "any")
        }
    )
    public interface CreateInvoice {}

    /**
     * Create Invoice Record/items from an existing invoice
     */
    @Service(
        name = "copyInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "copyInvoice",
        description = "Create Invoice Record/items from an existing invoice",
        defaultEntityName = "Invoice",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "invoiceIdToCopyFrom", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "CREATE")
    )
    public interface CopyInvoice {}

    /**
     * Retrieve an existing Invoice/Items
     */
    @Service(
        name = "getInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "getInvoice",
        description = "Retrieve an existing Invoice/Items",
        defaultEntityName = "Invoice",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "invoice", type = "org.ofbiz.entity.GenericValue", mode = "OUT"),
            @Attribute(name = "invoiceItems", type = "List", mode = "OUT")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "VIEW")
    )
    public interface GetInvoice {}

    /**
     * Update an existing Invoice Record
     */
    @Service(
        name = "updateInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "updateInvoice",
        description = "Update an existing Invoice Record",
        defaultEntityName = "Invoice",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", allowHtml = "any"),
            @OverrideAttribute(name = "invoiceMessage", allowHtml = "any")
        }
    )
    public interface UpdateInvoice {}

    /**
     * Set the Invoice  Status
     */
    @Service(
        name = "setInvoiceStatus",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "setInvoiceStatus",
        description = "Set the Invoice  Status",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "statusDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "paidDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceTypeId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "UPDATE")
    )
    public interface SetInvoiceStatus {}

    /**
     * Save a Invoice data to a template .
     */
    @Service(
        name = "copyInvoiceToTemplate",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "copyInvoiceToTemplate",
        description = "Save a Invoice data to a template .",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "INOUT"),
            @Attribute(name = "invoiceTypeId", type = "String", mode = "IN")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "CREATE")
    )
    public interface CopyInvoiceToTemplate {}

    /**
     * Create a new Invoice Item Record
     */
    @Service(
        name = "createInvoiceItem",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "createInvoiceItem",
        description = "Create a new Invoice Item Record",
        defaultEntityName = "InvoiceItem",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "invoiceItemSeqId", mode = "INOUT", optional = "true"),
            @OverrideAttribute(name = "description", allowHtml = "any")
        }
    )
    public interface CreateInvoiceItem {}

    /**
     * Update existing Invoice Item Record
     */
    @Service(
        name = "updateInvoiceItem",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "updateInvoiceItem",
        description = "Update existing Invoice Item Record",
        defaultEntityName = "InvoiceItem",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "UPDATE"),
        overrideAttributes = {
            @OverrideAttribute(name = "description", allowHtml = "any")
        }
    )
    public interface UpdateInvoiceItem {}

    /**
     * Remove an existing Invoice Item Record
     */
    @Service(
        name = "removeInvoiceItem",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "removeInvoiceItem",
        description = "Remove an existing Invoice Item Record",
        defaultEntityName = "InvoiceItem",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "UPDATE")
    )
    public interface RemoveInvoiceItem {}

    /**
     * Create a Invoice Status Record
     */
    @Service(
        name = "createInvoiceStatus",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Invoice Status Record",
        defaultEntityName = "InvoiceStatus",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "statusDate", mode = "IN", optional = "true")
        }
    )
    public interface CreateInvoiceStatus {}

    /**
     * Create a new Invoice Role Record
     */
    @Service(
        name = "createInvoiceRole",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "createInvoiceRole",
        description = "Create a new Invoice Role Record",
        defaultEntityName = "InvoiceRole",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "UPDATE")
    )
    public interface CreateInvoiceRole {}

    /**
     * Remove an existing Invoice Role Record
     */
    @Service(
        name = "removeInvoiceRole",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "removeInvoiceRole",
        description = "Remove an existing Invoice Role Record",
        defaultEntityName = "InvoiceRole",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "UPDATE")
    )
    public interface RemoveInvoiceRole {}

    /**
     * Create Invoice (Item) Term Record
     */
    @Service(
        name = "createInvoiceTerm",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "createInvoiceTerm",
        description = "Create Invoice (Item) Term Record",
        defaultEntityName = "InvoiceTerm",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "UPDATE")
    )
    public interface CreateInvoiceTerm {}

    /**
     * Update Invoice (Item) Term Record
     */
    @Service(
        name = "updateInvoiceTerm",
        engine = "entity-auto",
        invoke = "update",
        description = "Update Invoice (Item) Term Record",
        defaultEntityName = "InvoiceTerm",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "UPDATE")
    )
    public interface UpdateInvoiceTerm {}

    /**
     * Delete Invoice (Item) Term Record
     */
    @Service(
        name = "deleteInvoiceTerm",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete Invoice (Item) Term Record",
        defaultEntityName = "InvoiceTerm",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteInvoiceTerm {}

    /**
     *              Create an invoice from existing order using all order items             orderId = The orderId to associate the invoice with         
     */
    @Service(
        name = "createInvoiceForOrderAllItems",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "createInvoiceForOrderAllItems",
        description = "\n            Create an invoice from existing order using all order items\n            orderId = The orderId to associate the invoice with\n        ",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateInvoiceForOrderAllItems {}

    /**
     *              Create an invoice from existing order             orderId = The orderId to associate the invoice with             billItems = List of ItemIssuance records to use for creating the invoice         
     */
    @Service(
        name = "createInvoiceForOrder",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "createInvoiceForOrder",
        description = "\n            Create an invoice from existing order\n            orderId = The orderId to associate the invoice with\n            billItems = List of ItemIssuance records to use for creating the invoice\n        ",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "billItems", type = "List", mode = "IN"),
            @Attribute(name = "eventDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "invoiceTypeId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateInvoiceForOrder {}

    /**
     *              Create an invoice from a return             returnId = The returnId to associate the invoice with             billItems = List of ShipmentReceipts (for sales return) or ItemIssuance (for purchase return) to use for creating the invoice         
     */
    @Service(
        name = "createInvoiceFromReturn",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "createInvoiceFromReturn",
        description = "\n            Create an invoice from a return\n            returnId = The returnId to associate the invoice with\n            billItems = List of ShipmentReceipts (for sales return) or ItemIssuance (for purchase return) to use for creating the invoice\n        ",
        attributes = {
            @Attribute(name = "returnId", type = "String", mode = "IN"),
            @Attribute(name = "billItems", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateInvoiceFromReturn {}

    /**
     *              Create commission invoice for the list of sales invoices.               Returns a List of Maps, one for each invoice created containing:                  commissionInvoiceId: the invoiceId of the invoice created                  salesRepresentative: the invoice partyIdFrom          
     */
    @Service(
        name = "createCommissionInvoices",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "createCommissionInvoices",
        description = "\n            Create commission invoice for the list of sales invoices.  \n            Returns a List of Maps, one for each invoice created containing: \n                commissionInvoiceId: the invoiceId of the invoice created \n                salesRepresentative: the invoice partyIdFrom \n        ",
        attributes = {
            @Attribute(name = "partyIds", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceIds", type = "List", mode = "IN"),
            @Attribute(name = "invoicesCreated", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface CreateCommissionInvoices {}

    /**
     *              A sample/example service to calculate an affiliate commission (direct relationship to customer) and create             and invoice for it on behalf of the affiliate, ie an invoice from the affiliate to the company that can             then be paid by the company to balance it.         
     */
    @Service(
        name = "sampleInvoiceAffiliateCommission",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/SampleCommissionServices.xml",
        invoke = "sampleCalculateAffiliateCommission",
        description = "\n            A sample/example service to calculate an affiliate commission (direct relationship to customer) and create\n            and invoice for it on behalf of the affiliate, ie an invoice from the affiliate to the company that can\n            then be paid by the company to balance it.\n        ",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "OUT")
        }
    )
    public interface SampleInvoiceAffiliateCommission {}

    /**
     *              Sets status of each invoice in the list of invoices to INVOICE_READY.         
     */
    @Service(
        name = "readyInvoices",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "readyInvoices",
        description = "\n            Sets status of each invoice in the list of invoices to INVOICE_READY.\n        ",
        attributes = {
            @Attribute(name = "invoicesCreated", type = "List", mode = "IN")
        }
    )
    public interface ReadyInvoices {}

    /**
     *              Create invoice(s) from a Shipment             All the order items associated with the shipment will be selected and             one invoice for each order in the shipment will be created.             invoicesCreated = List of invoiceIds which were created by this service         
     */
    @Service(
        name = "createInvoicesFromShipment",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "createInvoicesFromShipment",
        description = "\n            Create invoice(s) from a Shipment\n            All the order items associated with the shipment will be selected and\n            one invoice for each order in the shipment will be created.\n            invoicesCreated = List of invoiceIds which were created by this service\n        ",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN"),
            @Attribute(name = "eventDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "invoicesCreated", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface CreateInvoicesFromShipment {}

    /**
     * Set invoice(s) to Ready from Shipment
     */
    @Service(
        name = "setInvoicesToReadyFromShipment",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "setInvoicesToReadyFromShipment",
        description = "Set invoice(s) to Ready from Shipment",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN")
        }
    )
    public interface SetInvoicesToReadyFromShipment {}

    /**
     *              Create sales invoice(s) from a drop shipment by wrapping a call to             createInvoicesFromShipments with the createSalesInvoicesForDropShipments parameter         
     */
    @Service(
        name = "createSalesInvoicesFromDropShipment",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "createSalesInvoicesFromDropShipment",
        description = "\n            Create sales invoice(s) from a drop shipment by wrapping a call to\n            createInvoicesFromShipments with the createSalesInvoicesForDropShipments parameter\n        ",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN"),
            @Attribute(name = "invoicesCreated", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface CreateSalesInvoicesFromDropShipment {}

    /**
     *              Create invoice(s) from a shipment list.             All the order items associated with the shipments will be selected and             one invoice for each order will be created (each invoice could contain             items shipped in different shipments).             If the shipments are drop shipments, the type of invoices (purchase or sales) created             will be controlled by the createSalesInvoicesForDropShipments parameter (purchase by default).             invoicesCreated = List of invoiceIds which were created by this service         
     */
    @Service(
        name = "createInvoicesFromShipments",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "createInvoicesFromShipments",
        description = "\n            Create invoice(s) from a shipment list.\n            All the order items associated with the shipments will be selected and\n            one invoice for each order will be created (each invoice could contain\n            items shipped in different shipments).\n            If the shipments are drop shipments, the type of invoices (purchase or sales) created\n            will be controlled by the createSalesInvoicesForDropShipments parameter (purchase by default).\n            invoicesCreated = List of invoiceIds which were created by this service\n        ",
        attributes = {
            @Attribute(name = "shipmentIds", type = "List", mode = "IN"),
            @Attribute(name = "createSalesInvoicesForDropShipments", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "eventDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "invoicesCreated", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface CreateInvoicesFromShipments {}

    /**
     *              Create invoice(s) from a return Shipment             invoicesCreated = List of invoiceIds which were created by this service         
     */
    @Service(
        name = "createInvoicesFromReturnShipment",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "createInvoicesFromReturnShipment",
        description = "\n            Create invoice(s) from a return Shipment\n            invoicesCreated = List of invoiceIds which were created by this service\n        ",
        attributes = {
            @Attribute(name = "shipmentId", type = "String", mode = "IN"),
            @Attribute(name = "invoicesCreated", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface CreateInvoicesFromReturnShipment {}

    /**
     * Send an invoice per email
     */
    @Service(
        name = "sendInvoicePerEmail",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "sendInvoicePerEmail",
        description = "Send an invoice per email",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "sendFrom", type = "String", mode = "IN"),
            @Attribute(name = "sendTo", type = "String", mode = "IN"),
            @Attribute(name = "sendCc", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "subject", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "bodyText", type = "String", mode = "IN", optional = "true", allowHtml = "any"),
            @Attribute(name = "other", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface SendInvoicePerEmail {}

    /**
     * Checks to see if the payments applied to an invoice total up to the invoice total; if so sets to PAID
     */
    @Service(
        name = "checkInvoicePaymentApplications",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "checkInvoicePaymentApplications",
        description = "Checks to see if the payments applied to an invoice total up to the invoice total; if so sets to PAID",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN")
        }
    )
    public interface CheckInvoicePaymentApplications {}

    /**
     * Create a ContactMech for an invoice
     */
    @Service(
        name = "createInvoiceContactMech",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "createInvoiceContactMech",
        description = "Create a ContactMech for an invoice",
        entityAttributes = {
            @EntityAttributes(entityName = "InvoiceContactMech", mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "CREATE")
    )
    public interface CreateInvoiceContactMech {}

    /**
     * Delete a ContactMech for an invoice
     */
    @Service(
        name = "deleteInvoiceContactMech",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a ContactMech for an invoice",
        defaultEntityName = "InvoiceContactMech",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "DELETE")
    )
    public interface DeleteInvoiceContactMech {}

    /**
     * Calculate the previously invoiced amount for an OrderAdjustment
     */
    @Service(
        name = "calculateInvoicedAdjustmentTotal",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "calculateInvoicedAdjustmentTotal",
        description = "Calculate the previously invoiced amount for an OrderAdjustment",
        attributes = {
            @Attribute(name = "orderAdjustment", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "invoicedTotal", type = "BigDecimal", mode = "OUT")
        }
    )
    public interface CalculateInvoicedAdjustmentTotal {}

    /**
     * Update Invoice Item Type Record
     */
    @Service(
        name = "updateInvoiceItemType",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "updateInvoiceItemType",
        description = "Update Invoice Item Type Record",
        defaultEntityName = "InvoiceItemType",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInvoiceItemType {}

    /**
     * Scheduled service to generate Invoice from an existing Invoice
     */
    @Service(
        name = "autoGenerateInvoiceFromExistingInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "autoGenerateInvoiceFromExistingInvoice",
        description = "Scheduled service to generate Invoice from an existing Invoice",
        attributes = {
            @Attribute(name = "recurrenceInfoId", type = "String", mode = "IN")
        }
    )
    public interface AutoGenerateInvoiceFromExistingInvoice {}

    /**
     * Cancel Invoice
     */
    @Service(
        name = "cancelInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "cancelInvoice",
        description = "Cancel Invoice",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceTypeId", type = "String", mode = "OUT")
        }
    )
    public interface CancelInvoice {}

    /**
     * calculate running total for selected Invoices
     */
    @Service(
        name = "getInvoiceRunningTotal",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "getInvoiceRunningTotal",
        description = "calculate running total for selected Invoices",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceIds", type = "List", mode = "IN"),
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceRunningTotal", type = "String", mode = "OUT")
        }
    )
    public interface GetInvoiceRunningTotal {}

    /**
     * Call Tax Calculate Service
     */
    @Service(
        name = "addtax",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "addtax",
        description = "Call Tax Calculate Service",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN")
        }
    )
    public interface Addtax {}

    /**
     * Filter invoices by invoiceItemAssocTypeId
     */
    @Service(
        name = "getInvoicesFilterByAssocType",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "getInvoicesFilterByAssocType",
        description = "Filter invoices by invoiceItemAssocTypeId",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceList", type = "List", mode = "IN"),
            @Attribute(name = "invoiceItemAssocTypeId", type = "String", mode = "IN"),
            @Attribute(name = "filteredInvoiceList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetInvoicesFilterByAssocType {}

    /**
     * Create a InvoiceItemAssoc
     */
    @Service(
        name = "createInvoiceItemAssoc",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a InvoiceItemAssoc",
        defaultEntityName = "InvoiceItemAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateInvoiceItemAssoc {}

    /**
     * Update a InvoiceItemAssoc
     */
    @Service(
        name = "updateInvoiceItemAssoc",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a InvoiceItemAssoc",
        defaultEntityName = "InvoiceItemAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInvoiceItemAssoc {}

    /**
     * Delete a InvoiceItemAssoc
     */
    @Service(
        name = "deleteInvoiceItemAssoc",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a InvoiceItemAssoc",
        defaultEntityName = "InvoiceItemAssoc",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInvoiceItemAssoc {}

    /**
     * Remove invoiceItemAssoc record on cancel invoice
     */
    @Service(
        name = "removeInvoiceItemAssocOnCancelInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "removeInvoiceItemAssocOnCancelInvoice",
        description = "Remove invoiceItemAssoc record on cancel invoice",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN")
        }
    )
    public interface RemoveInvoiceItemAssocOnCancelInvoice {}

    /**
     * Reset OrderItemBilling and OrderAdjustmentBilling records on cancel invoice, so it is isn't considered invoiced any more by createInvoiceForOrder service
     */
    @Service(
        name = "resetOrderItemBillingAndOrderAdjustmentBillingOnCancelInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "resetOrderItemBillingAndOrderAdjustmentBillingOnCancelInvoice",
        description = "Reset OrderItemBilling and OrderAdjustmentBilling records on cancel invoice, so it is isn't considered invoiced any more by createInvoiceForOrder service",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN")
        }
    )
    public interface ResetOrderItemBillingAndOrderAdjustmentBillingOnCancelInvoice {}

    /**
     * Set status of invoices in bulk.
     */
    @Service(
        name = "massChangeInvoiceStatus",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "massChangeInvoiceStatus",
        description = "Set status of invoices in bulk.",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceIds", type = "List", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "errorMessage", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface MassChangeInvoiceStatus {}

    /**
     * Create an invoice from existing order when invoicePerShipment is N
     */
    @Service(
        name = "createInvoiceFromOrder",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "createInvoiceFromOrder",
        description = "Create an invoice from existing order when invoicePerShipment is N",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreateInvoiceFromOrder {}

    /**
     * Add Content To Invoice
     */
    @Service(
        name = "createInvoiceContent",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "createInvoiceContent",
        description = "Add Content To Invoice",
        defaultEntityName = "InvoiceContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateInvoiceContent {}

    /**
     * Update Content To Invoice
     */
    @Service(
        name = "updateInvoiceContent",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "updateInvoiceContent",
        description = "Update Content To Invoice",
        defaultEntityName = "InvoiceContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInvoiceContent {}

    /**
     * Remove Content From Invoice
     */
    @Service(
        name = "removeInvoiceContent",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "removeInvoiceContent",
        description = "Remove Content From Invoice",
        defaultEntityName = "InvoiceContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemoveInvoiceContent {}

    @Service(
        name = "createSimpleTextContentForInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "createSimpleTextContentForInvoice",
        defaultEntityName = "InvoiceContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "text", type = "String", mode = "IN", allowHtml = "any")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "contentId", optional = "true"),
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreateSimpleTextContentForInvoice {}

    @Service(
        name = "updateSimpleTextContentForInvoice",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "updateSimpleTextContentForInvoice",
        defaultEntityName = "InvoiceContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", optional = "true")
        },
        attributes = {
            @Attribute(name = "textDataResourceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "text", type = "String", mode = "IN", optional = "true", allowHtml = "any")
        }
    )
    public interface UpdateSimpleTextContentForInvoice {}

    /**
     * check if a invoice is in a foreign currency related to the accounting company.
     */
    @Service(
        name = "isInvoiceInForeignCurrency",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "isInvoiceInForeignCurrency",
        description = "check if a invoice is in a foreign currency related to the accounting company.",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "isForeign", type = "Boolean", mode = "OUT")
        }
    )
    public interface IsInvoiceInForeignCurrency {}

    /**
     * Import an invoice with invoiceitems in csv format
     */
    @Service(
        name = "importInvoice",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "importInvoice",
        description = "Import an invoice with invoiceitems in csv format",
        auth = "true",
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "INOUT"),
            @Attribute(name = "uploadedFile", type = "java.nio.ByteBuffer", mode = "IN", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "CREATE")
    )
    public interface ImportInvoice {}

    /**
     * Create a InvoiceItemAttribute record
     */
    @Service(
        name = "createInvoiceItemAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a InvoiceItemAttribute record",
        defaultEntityName = "InvoiceItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateInvoiceItemAttribute {}

    /**
     * Update a InvoiceItemAttribute record
     */
    @Service(
        name = "updateInvoiceItemAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a InvoiceItemAttribute record",
        defaultEntityName = "InvoiceItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInvoiceItemAttribute {}

    /**
     * Delete a InvoiceItemAttribute record
     */
    @Service(
        name = "deleteInvoiceItemAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a InvoiceItemAttribute record",
        defaultEntityName = "InvoiceItemAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInvoiceItemAttribute {}

    /**
     * Create a InvoiceTermAttribute record
     */
    @Service(
        name = "createInvoiceTermAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a InvoiceTermAttribute record",
        defaultEntityName = "InvoiceTermAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateInvoiceTermAttribute {}

    /**
     * Update a InvoiceTermAttribute record
     */
    @Service(
        name = "updateInvoiceTermAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a InvoiceTermAttribute record",
        defaultEntityName = "InvoiceTermAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInvoiceTermAttribute {}

    /**
     * Delete a InvoiceTermAttribute record
     */
    @Service(
        name = "deleteInvoiceTermAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a InvoiceTermAttribute record",
        defaultEntityName = "InvoiceTermAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInvoiceTermAttribute {}

    /**
     * Create a InvoiceTypeAttr record
     */
    @Service(
        name = "createInvoiceTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a InvoiceTypeAttr record",
        defaultEntityName = "InvoiceTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateInvoiceTypeAttr {}

    /**
     * Update a InvoiceTypeAttr record
     */
    @Service(
        name = "updateInvoiceTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a InvoiceTypeAttr record",
        defaultEntityName = "InvoiceTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInvoiceTypeAttr {}

    /**
     * Delete a InvoiceTypeAttr record
     */
    @Service(
        name = "deleteInvoiceTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a InvoiceTypeAttr record",
        defaultEntityName = "InvoiceTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInvoiceTypeAttr {}

    /**
     * Create a InvoiceAttribute
     */
    @Service(
        name = "createInvoiceAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a InvoiceAttribute",
        defaultEntityName = "InvoiceAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateInvoiceAttribute {}

    /**
     * Update a InvoiceAttribute
     */
    @Service(
        name = "updateInvoiceAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a InvoiceAttribute",
        defaultEntityName = "InvoiceAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInvoiceAttribute {}

    /**
     * Delete a InvoiceAttribute
     */
    @Service(
        name = "deleteInvoiceAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a InvoiceAttribute",
        defaultEntityName = "InvoiceAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInvoiceAttribute {}

    /**
     * Create a InvoiceItemAssocType
     */
    @Service(
        name = "createInvoiceItemAssocType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a InvoiceItemAssocType",
        defaultEntityName = "InvoiceItemAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateInvoiceItemAssocType {}

    /**
     * Update a InvoiceItemAssocType
     */
    @Service(
        name = "updateInvoiceItemAssocType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a InvoiceItemAssocType",
        defaultEntityName = "InvoiceItemAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInvoiceItemAssocType {}

    /**
     * Delete a InvoiceItemAssocType
     */
    @Service(
        name = "deleteInvoiceItemAssocType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a InvoiceItemAssocType",
        defaultEntityName = "InvoiceItemAssocType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInvoiceItemAssocType {}

    /**
     * Create a InvoiceNote
     */
    @Service(
        name = "createInvoiceNote",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a InvoiceNote",
        defaultEntityName = "InvoiceNote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk")
        }
    )
    public interface CreateInvoiceNote {}

    /**
     * Delete a InvoiceNote
     */
    @Service(
        name = "deleteInvoiceNote",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a InvoiceNote",
        defaultEntityName = "InvoiceNote",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInvoiceNote {}

    /**
     * Create a InvoiceContentType
     */
    @Service(
        name = "createInvoiceContentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a InvoiceContentType",
        defaultEntityName = "InvoiceContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateInvoiceContentType {}

    /**
     * Update a InvoiceContentType
     */
    @Service(
        name = "updateInvoiceContentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a InvoiceContentType",
        defaultEntityName = "InvoiceContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInvoiceContentType {}

    /**
     * Delete a InvoiceContentType
     */
    @Service(
        name = "deleteInvoiceContentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a InvoiceContentType",
        defaultEntityName = "InvoiceContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInvoiceContentType {}

    /**
     * Create InvoiceItemTypeAttr
     */
    @Service(
        name = "createInvoiceItemTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create InvoiceItemTypeAttr",
        defaultEntityName = "InvoiceItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateInvoiceItemTypeAttr {}

    /**
     * Update InvoiceItemTypeAttr
     */
    @Service(
        name = "updateInvoiceItemTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update InvoiceItemTypeAttr",
        defaultEntityName = "InvoiceItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInvoiceItemTypeAttr {}

    /**
     * Delete InvoiceItemTypeAttr
     */
    @Service(
        name = "deleteInvoiceItemTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete InvoiceItemTypeAttr",
        defaultEntityName = "InvoiceItemTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInvoiceItemTypeAttr {}

    /**
     * Create InvoiceItemTypeMap
     */
    @Service(
        name = "createInvoiceItemTypeMap",
        engine = "entity-auto",
        invoke = "create",
        description = "Create InvoiceItemTypeMap",
        defaultEntityName = "InvoiceItemTypeMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateInvoiceItemTypeMap {}

    /**
     * Update InvoiceItemTypeMap
     */
    @Service(
        name = "updateInvoiceItemTypeMap",
        engine = "entity-auto",
        invoke = "update",
        description = "Update InvoiceItemTypeMap",
        defaultEntityName = "InvoiceItemTypeMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInvoiceItemTypeMap {}

    /**
     * Delete InvoiceItemTypeMap
     */
    @Service(
        name = "deleteInvoiceItemTypeMap",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete InvoiceItemTypeMap",
        defaultEntityName = "InvoiceItemTypeMap",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInvoiceItemTypeMap {}

    /**
     * Create a InvoiceType
     */
    @Service(
        name = "createInvoiceType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a InvoiceType",
        defaultEntityName = "InvoiceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        }
    )
    public interface CreateInvoiceType {}

    /**
     * Update a InvoiceType
     */
    @Service(
        name = "updateInvoiceType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a InvoiceType",
        defaultEntityName = "InvoiceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateInvoiceType {}

    /**
     * Delete a InvoiceType
     */
    @Service(
        name = "deleteInvoiceType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a InvoiceType",
        defaultEntityName = "InvoiceType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteInvoiceType {}

}

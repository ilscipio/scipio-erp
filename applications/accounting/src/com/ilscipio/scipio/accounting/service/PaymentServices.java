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
public class PaymentServices {

    /**
     * Create a Payment.  If a paymentMethodId is supplied, paymentMethodTypeId is gotten from paymentMethod.  Otherwise, it must be supplied.  If no         paymentMethodTypeId and no paymentMethodId is supplied, then an error will be returned. 
     */
    @Service(
        name = "createPayment",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "createPayment",
        description = "Create a Payment.  If a paymentMethodId is supplied, paymentMethodTypeId is gotten from paymentMethod.  Otherwise, it must be supplied.  If no\n        paymentMethodTypeId and no paymentMethodId is supplied, then an error will be returned. ",
        defaultEntityName = "Payment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "paymentTypeId", optional = "false"),
            @OverrideAttribute(name = "partyIdFrom", optional = "false"),
            @OverrideAttribute(name = "partyIdTo", optional = "false"),
            @OverrideAttribute(name = "amount", optional = "false")
        }
    )
    public interface CreatePayment {}

    /**
     * Update a Payment
     */
    @Service(
        name = "updatePayment",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "updatePayment",
        description = "Update a Payment",
        defaultEntityName = "Payment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePayment {}

    /**
     * Change the status of a Payment
     */
    @Service(
        name = "setPaymentStatus",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "setPaymentStatus",
        description = "Change the status of a Payment",
        defaultEntityName = "Payment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        },
        attributes = {
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "oldStatusId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgPaymentPermissionCheck", mainAction = "UPDATE")
    )
    public interface SetPaymentStatus {}

    /**
     * Checks to see if each invoice to which a payment is applied has been fully paid up.  If so, sets the invoice status to PAID.
     */
    @Service(
        name = "checkPaymentInvoices",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "checkPaymentInvoices",
        description = "Checks to see if each invoice to which a payment is applied has been fully paid up.  If so, sets the invoice status to PAID.",
        attributes = {
            @Attribute(name = "paymentId", type = "String", mode = "IN")
        }
    )
    public interface CheckPaymentInvoices {}

    /**
     * Updates a Payment and then marks it as PMNT_SENT
     */
    @Service(
        name = "quickSendPayment",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "quickSendPayment",
        description = "Updates a Payment and then marks it as PMNT_SENT",
        defaultEntityName = "Payment",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface QuickSendPayment {}

    /**
     * Create a payment application
     */
    @Service(
        name = "createPaymentApplication",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "createPaymentApplication",
        description = "Create a payment application",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentId", type = "String", mode = "IN"),
            @Attribute(name = "toPaymentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billingAccountId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "taxAuthGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amountApplied", type = "BigDecimal", mode = "INOUT", optional = "true"),
            @Attribute(name = "paymentApplicationId", type = "String", mode = "OUT"),
            @Attribute(name = "paymentTypeId", type = "String", mode = "OUT")
        }
    )
    public interface CreatePaymentApplication {}

    /**
     *              Apply a payment to a Invoice or other payment or Billing account or  Taxauthority,             create/update paymentApplication records.         
     */
    @Service(
        name = "updatePaymentApplication",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "updatePaymentApplication",
        description = "\n            Apply a payment to a Invoice or other payment or Billing account or  Taxauthority,\n            create/update paymentApplication records.\n        ",
        defaultEntityName = "PaymentApplication",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "invoiceProcessing", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "paymentId", optional = "false")
        }
    )
    public interface UpdatePaymentApplication {}

    /**
     *              Apply a payment to a Invoice or other payment or Billing account or Taxauthority,             If no ammountApplied is supplied the system will calculate and use the maximum possible value.         
     */
    @Service(
        name = "updatePaymentApplicationDef",
        engine = "java",
        location = "org.ofbiz.accounting.invoice.InvoiceServices",
        invoke = "updatePaymentApplicationDef",
        description = "\n            Apply a payment to a Invoice or other payment or Billing account or Taxauthority,\n            If no ammountApplied is supplied the system will calculate and use the maximum possible value.\n        ",
        defaultEntityName = "PaymentApplication",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "invoiceProcessing", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "paymentId", optional = "false")
        }
    )
    public interface UpdatePaymentApplicationDef {}

    /**
     * Delete a paymentApplication record.
     */
    @Service(
        name = "removePaymentApplication",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml",
        invoke = "removePaymentApplication",
        description = "Delete a paymentApplication record.",
        defaultEntityName = "PaymentApplication",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "OUT", include = "nonpk", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgInvoicePermissionCheck", mainAction = "UPDATE")
    )
    public interface RemovePaymentApplication {}

    /**
     * Create a payment and a payment application for the full amount
     */
    @Service(
        name = "createPaymentAndApplication",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "createPaymentAndApplication",
        description = "Create a payment and a payment application for the full amount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "Payment", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Payment", mode = "INOUT", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceItemSeqId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billingAccountId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "overrideGlAccountId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "taxAuthGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentApplicationId", type = "String", mode = "OUT")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "paymentTypeId", optional = "false"),
            @OverrideAttribute(name = "partyIdFrom", optional = "false"),
            @OverrideAttribute(name = "partyIdTo", optional = "false"),
            @OverrideAttribute(name = "statusId", optional = "false"),
            @OverrideAttribute(name = "amount", optional = "false")
        }
    )
    public interface CreatePaymentAndApplication {}

    /**
     * Create a list with information on payment due dates and amounts for the invoice; one of invoiceId or invoice must be provided.
     */
    @Service(
        name = "getInvoicePaymentInfoList",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "getInvoicePaymentInfoList",
        description = "Create a list with information on payment due dates and amounts for the invoice; one of invoiceId or invoice must be provided.",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "invoice", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "invoicePaymentInfoList", type = "List", mode = "OUT")
        }
    )
    public interface GetInvoicePaymentInfoList {}

    /**
     * Create a list with information on payment due dates and amounts.
     */
    @Service(
        name = "getInvoicePaymentInfoListByDueDateOffset",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "getInvoicePaymentInfoListByDueDateOffset",
        description = "Create a list with information on payment due dates and amounts.",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceTypeId", type = "String", mode = "IN"),
            @Attribute(name = "daysOffset", type = "Long", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyIdFrom", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "invoicePaymentInfoList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetInvoicePaymentInfoListByDueDateOffset {}

    /**
     * Sets payment status to PMNT_VOID, removes all PaymentApplications, changes related invoice statuses to              INVOICE_READY if status is INVOICE_PAID, and reverses related AcctgTrans by calling copyAcctgTransAndEntries service
     */
    @Service(
        name = "voidPayment",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "voidPayment",
        description = "Sets payment status to PMNT_VOID, removes all PaymentApplications, changes related invoice statuses to \n            INVOICE_READY if status is INVOICE_PAID, and reverses related AcctgTrans by calling copyAcctgTransAndEntries service",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentId", type = "String", mode = "IN"),
            @Attribute(name = "finAccountTransId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "statusId", type = "String", mode = "OUT", optional = "true")
        },
        permissionService = @PermissionService(service = "acctgPaymentPermissionCheck", mainAction = "UPDATE")
    )
    public interface VoidPayment {}

    /**
     * calculate running total for payments
     */
    @Service(
        name = "getPaymentRunningTotal",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "getPaymentRunningTotal",
        description = "calculate running total for payments",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentIds", type = "List", mode = "IN"),
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentRunningTotal", type = "String", mode = "OUT")
        }
    )
    public interface GetPaymentRunningTotal {}

    /**
     * cancel payment batch
     */
    @Service(
        name = "cancelPaymentBatch",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "cancelPaymentBatch",
        description = "cancel payment batch",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentGroupId", type = "String", mode = "IN")
        }
    )
    public interface CancelPaymentBatch {}

    /**
     * Creates Payments, Payment Application and Payment Group for the same
     */
    @Service(
        name = "createPaymentAndPaymentGroupForInvoices",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "createPaymentAndPaymentGroupForInvoices",
        description = "Creates Payments, Payment Application and Payment Group for the same",
        auth = "true",
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "IN"),
            @Attribute(name = "checkStartNumber", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceIds", type = "List", mode = "IN"),
            @Attribute(name = "paymentMethodTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN"),
            @Attribute(name = "paymentGroupId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "errorMessage", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CreatePaymentAndPaymentGroupForInvoices {}

    /**
     * create Payment and PaymentApplications for multiple invoices for one party
     */
    @Service(
        name = "createPaymentAndApplicationForParty",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "createPaymentAndApplicationForParty",
        description = "create Payment and PaymentApplications for multiple invoices for one party",
        auth = "true",
        attributes = {
            @Attribute(name = "organizationPartyId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN"),
            @Attribute(name = "invoices", type = "List", mode = "IN"),
            @Attribute(name = "paymentMethodTypeId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN"),
            @Attribute(name = "finAccountId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "checkStartNumber", type = "Long", mode = "IN", optional = "true"),
            @Attribute(name = "paymentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "invoiceIds", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "OUT", optional = "true")
        }
    )
    public interface CreatePaymentAndApplicationForParty {}

    @Service(
        name = "createPaymentGroupAndMember",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "createPaymentGroupAndMember",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentIds", type = "List", mode = "IN"),
            @Attribute(name = "paymentGroupTypeId", type = "String", mode = "IN"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "paymentGroupName", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentGroupId", type = "String", mode = "OUT")
        }
    )
    public interface CreatePaymentGroupAndMember {}

    /**
     * Cancel all payments for payment group
     */
    @Service(
        name = "cancelCheckRunPayments",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "cancelCheckRunPayments",
        description = "Cancel all payments for payment group",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentGroupId", type = "String", mode = "IN")
        }
    )
    public interface CancelCheckRunPayments {}

    @Service(
        name = "createFinAccoutnTransFromPayment",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "createFinAccoutnTransFromPayment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "FinAccountTrans", mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "FinAccountTrans", mode = "INOUT", include = "pk", optional = "true")
        },
        attributes = {
            @Attribute(name = "invoiceIds", type = "List", mode = "IN", optional = "true")
        }
    )
    public interface CreateFinAccoutnTransFromPayment {}

    /**
     * Get list of payment
     */
    @Service(
        name = "getPayments",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "getPayments",
        description = "Get list of payment",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentGroupId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "finAccountTransId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "payments", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface GetPayments {}

    /**
     * Get ReconciliationId associated to paymentGroup
     */
    @Service(
        name = "getPaymentGroupReconciliationId",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "getPaymentGroupReconciliationId",
        description = "Get ReconciliationId associated to paymentGroup",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentGroupId", type = "String", mode = "IN"),
            @Attribute(name = "glReconciliationId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface GetPaymentGroupReconciliationId {}

    /**
     * Check the valid(unbatched) payment and create batch for same
     */
    @Service(
        name = "checkAndCreateBatchForValidPayments",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "checkAndCreateBatchForValidPayments",
        description = "Check the valid(unbatched) payment and create batch for same",
        auth = "true",
        implemented = {@Implements(service = "createPaymentGroupAndMember")}
    )
    public interface CheckAndCreateBatchForValidPayments {}

    /**
     * Set status of Payments in bulk.
     */
    @Service(
        name = "massChangePaymentStatus",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "massChangePaymentStatus",
        description = "Set status of Payments in bulk.",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentIds", type = "List", mode = "IN"),
            @Attribute(name = "statusId", type = "String", mode = "IN"),
            @Attribute(name = "errorMessage", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface MassChangePaymentStatus {}

    /**
     * Create Payment from Order when payment does exist yet and not disabled by accountingconfig
     */
    @Service(
        name = "createPaymentFromOrder",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "createPaymentFromOrder",
        description = "Create Payment from Order when payment does exist yet and not disabled by accountingconfig",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "paymentId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentRefNum", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "comments", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreatePaymentFromOrder {}

    /**
     * Create a payment application if either the invoice of payment could be found
     */
    @Service(
        name = "createMatchingPaymentApplication",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "createMatchingPaymentApplication",
        description = "Create a payment application if either the invoice of payment could be found",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CreateMatchingPaymentApplication {}

    /**
     * Add Content To Payment
     */
    @Service(
        name = "createPaymentContent",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "createPaymentContent",
        description = "Add Content To Payment",
        defaultEntityName = "PaymentContent",
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
    public interface CreatePaymentContent {}

    /**
     * Update Content To Payment
     */
    @Service(
        name = "updatePaymentContent",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "updatePaymentContent",
        description = "Update Content To Payment",
        defaultEntityName = "PaymentContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true"),
            @EntityAttributes(entityName = "Content", mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentContent {}

    /**
     * Remove Content From Payment
     */
    @Service(
        name = "removePaymentContent",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml",
        invoke = "removePaymentContent",
        description = "Remove Content From Payment",
        defaultEntityName = "PaymentContent",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface RemovePaymentContent {}

    @Service(
        name = "createBillingAccountTermAttr",
        engine = "entity-auto",
        invoke = "create",
        defaultEntityName = "BillingAccountTermAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateBillingAccountTermAttr {}

    @Service(
        name = "updateBillingAccountTermAttr",
        engine = "entity-auto",
        invoke = "update",
        defaultEntityName = "BillingAccountTermAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateBillingAccountTermAttr {}

    @Service(
        name = "deleteBillingAccountTermAttr",
        engine = "entity-auto",
        invoke = "delete",
        defaultEntityName = "BillingAccountTermAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteBillingAccountTermAttr {}

    /**
     * Create a Deduction record
     */
    @Service(
        name = "createDeduction",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Deduction record",
        defaultEntityName = "Deduction",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateDeduction {}

    /**
     * Update a Deduction record
     */
    @Service(
        name = "updateDeduction",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Deduction record",
        defaultEntityName = "Deduction",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateDeduction {}

    /**
     * Delete a Deduction record
     */
    @Service(
        name = "deleteDeduction",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Deduction record",
        defaultEntityName = "Deduction",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteDeduction {}

    /**
     * Create a Deduction Type record
     */
    @Service(
        name = "createDeductionType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Deduction Type record",
        defaultEntityName = "DeductionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreateDeductionType {}

    /**
     * Update a Deduction Type record
     */
    @Service(
        name = "updateDeductionType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Deduction Type record",
        defaultEntityName = "DeductionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdateDeductionType {}

    /**
     * Delete a Deduction Type record
     */
    @Service(
        name = "deleteDeductionType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Deduction Type record",
        defaultEntityName = "DeductionType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteDeductionType {}

    /**
     * Create a PaymentAttribute record
     */
    @Service(
        name = "createPaymentAttribute",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PaymentAttribute record",
        defaultEntityName = "PaymentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePaymentAttribute {}

    /**
     * Update a PaymentAttribute record
     */
    @Service(
        name = "updatePaymentAttribute",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PaymentAttribute record",
        defaultEntityName = "PaymentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentAttribute {}

    /**
     * Delete a PaymentAttribute record
     */
    @Service(
        name = "deletePaymentAttribute",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PaymentAttribute record",
        defaultEntityName = "PaymentAttribute",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePaymentAttribute {}

    /**
     * Create a PaymentBudgetAllocation record
     */
    @Service(
        name = "createPaymentBudgetAllocation",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PaymentBudgetAllocation record",
        defaultEntityName = "PaymentBudgetAllocation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePaymentBudgetAllocation {}

    /**
     * Update a PaymentBudgetAllocation record
     */
    @Service(
        name = "updatePaymentBudgetAllocation",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PaymentBudgetAllocation record",
        defaultEntityName = "PaymentBudgetAllocation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentBudgetAllocation {}

    /**
     * Delete a PaymentBudgetAllocation record
     */
    @Service(
        name = "deletePaymentBudgetAllocation",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PaymentBudgetAllocation record",
        defaultEntityName = "PaymentBudgetAllocation",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePaymentBudgetAllocation {}

    /**
     * Create a PaymentContentType record
     */
    @Service(
        name = "createPaymentContentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PaymentContentType record",
        defaultEntityName = "PaymentContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePaymentContentType {}

    /**
     * Update a PaymentContentType record
     */
    @Service(
        name = "updatePaymentContentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PaymentContentType record",
        defaultEntityName = "PaymentContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentContentType {}

    /**
     * Delete a PaymentContentType record
     */
    @Service(
        name = "deletePaymentContentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PaymentContentType record",
        defaultEntityName = "PaymentContentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePaymentContentType {}

    /**
     * Create a PaymentGroupType record
     */
    @Service(
        name = "createPaymentGroupType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PaymentGroupType record",
        defaultEntityName = "PaymentGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePaymentGroupType {}

    /**
     * Update a PaymentGroupType record
     */
    @Service(
        name = "updatePaymentGroupType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PaymentGroupType record",
        defaultEntityName = "PaymentGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGroupType {}

    /**
     * Delete a PaymentGroupType record
     */
    @Service(
        name = "deletePaymentGroupType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PaymentGroupType record",
        defaultEntityName = "PaymentGroupType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePaymentGroupType {}

    /**
     * Create a PaymentType record
     */
    @Service(
        name = "createPaymentType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PaymentType record",
        defaultEntityName = "PaymentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePaymentType {}

    /**
     * Update a PaymentType record
     */
    @Service(
        name = "updatePaymentType",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PaymentType record",
        defaultEntityName = "PaymentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentType {}

    /**
     * Delete a PaymentType record
     */
    @Service(
        name = "deletePaymentType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PaymentType record",
        defaultEntityName = "PaymentType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePaymentType {}

    /**
     * Create a PaymentTypeAttr record
     */
    @Service(
        name = "createPaymentTypeAttr",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PaymentTypeAttr record",
        defaultEntityName = "PaymentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePaymentTypeAttr {}

    /**
     * Update a PaymentTypeAttr record
     */
    @Service(
        name = "updatePaymentTypeAttr",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a PaymentTypeAttr record",
        defaultEntityName = "PaymentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentTypeAttr {}

    /**
     * Delete a PaymentTypeAttr record
     */
    @Service(
        name = "deletePaymentTypeAttr",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PaymentTypeAttr record",
        defaultEntityName = "PaymentTypeAttr",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePaymentTypeAttr {}

}

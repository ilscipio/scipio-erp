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
package com.ilscipio.scipio.accounting.seca;

import com.ilscipio.scipio.service.def.seca.*;

/**
 * Auto-generated annotation-based service ECA definitions.
 *
 * <p>Generated from secas.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class LedgerSecas {

    /**
     * SECA for service createAcctgTransAndEntries on event commit.
     */
    @Seca(
        service = "createAcctgTransAndEntries",
        event = "commit",
        condition = "!empty(acctgTransId)",
        actions = {
            @SecaAction(
                service = "postAcctgTrans",
                mode = "sync"
            )
        }
    )
    public interface CreateAcctgTransAndEntriescommitSeca1 {}

    /**
     * SECA for service createItemIssuance on event commit.
     */
    @Seca(
        service = "createItemIssuance",
        event = "commit",
        condition = "!empty(orderId) && !empty(inventoryItemId) && affectAccounting == 'true'",
        actions = {
            @SecaAction(
                service = "createAcctgTransForSalesShipmentIssuance",
                mode = "sync"
            )
        }
    )
    public interface CreateItemIssuancecommitSeca2 {}

    /**
     * SECA for service cancelOrderItemIssuanceFromSalesShipment on event commit.
     */
    @Seca(
        service = "cancelOrderItemIssuanceFromSalesShipment",
        event = "commit",
        condition = "canceledQuantity > 0",
        actions = {
            @SecaAction(
                service = "createAcctgTransForCanceledSalesShipmentIssuance",
                mode = "sync"
            )
        }
    )
    public interface CancelOrderItemIssuanceFromSalesShipmentcommitSeca3 {}

    /**
     * SECA for service createShipmentReceipt on event commit.
     */
    @Seca(
        service = "createShipmentReceipt",
        event = "commit",
        condition = "affectAccounting == 'true'",
        actions = {
            @SecaAction(
                service = "createAcctgTransForShipmentReceipt",
                mode = "sync"
            )
        }
    )
    public interface CreateShipmentReceiptcommitSeca4 {}

    /**
     * SECA for service assignInventoryToWorkEffort on event commit.
     */
    @Seca(
        service = "assignInventoryToWorkEffort",
        event = "commit",
        actions = {
            @SecaAction(
                service = "createAcctgTransForWorkEffortIssuance",
                mode = "sync"
            )
        }
    )
    public interface AssignInventoryToWorkEffortcommitSeca5 {}

    /**
     * SECA for service createWorkEffortInventoryProduced on event commit.
     */
    @Seca(
        service = "createWorkEffortInventoryProduced",
        event = "commit",
        actions = {
            @SecaAction(
                service = "createAcctgTransForWorkEffortInventoryProduced",
                mode = "sync"
            )
        }
    )
    public interface CreateWorkEffortInventoryProducedcommitSeca6 {}

    /**
     * SECA for service createCostComponent on event commit.
     */
    @Seca(
        service = "createCostComponent",
        event = "commit",
        condition = "!empty(workEffortId) && costComponentTypeId != 'ACTUAL_MAT_COST'",
        actions = {
            @SecaAction(
                service = "createAcctgTransForWorkEffortCost",
                mode = "sync"
            )
        }
    )
    public interface CreateCostComponentcommitSeca7 {}

    /**
     * SECA for service updateInventoryItem on event commit.
     */
    @Seca(
        service = "updateInventoryItem",
        event = "commit",
        condition = "!empty(ownerPartyId) && ownerPartyId != oldOwnerPartyId",
        actions = {
            @SecaAction(
                service = "createAcctgTransForInventoryItemOwnerChange",
                mode = "sync"
            )
        }
    )
    public interface UpdateInventoryItemcommitSeca8 {}

    /**
     * SECA for service createInventoryItemDetail on event commit.
     */
    @Seca(
        service = "createInventoryItemDetail",
        event = "commit",
        condition = "!empty(unitCost) && empty(receiptId)",
        actions = {
            @SecaAction(
                service = "createAcctgTransForInventoryItemCostChange",
                mode = "sync"
            )
        }
    )
    public interface CreateInventoryItemDetailcommitSeca9 {}

    /**
     * SECA for service createPhysicalInventoryAndVariance on event commit.
     */
    @Seca(
        service = "createPhysicalInventoryAndVariance",
        event = "commit",
        condition = "!empty(physicalInventoryId)",
        actions = {
            @SecaAction(
                service = "createAcctgTransForPhysicalInventoryVariance",
                mode = "sync"
            )
        }
    )
    public interface CreatePhysicalInventoryAndVariancecommitSeca10 {}

    /**
     * SECA for service createItemIssuance on event commit.
     */
    @Seca(
        service = "createItemIssuance",
        event = "commit",
        condition = "!empty(fixedAssetId)",
        actions = {
            @SecaAction(
                service = "createAcctgTransForFixedAssetMaintIssuance",
                mode = "sync"
            )
        }
    )
    public interface CreateItemIssuancecommitSeca11 {}

    /**
     * SECA for service setInvoiceStatus on event commit.
     */
    @Seca(
        service = "setInvoiceStatus",
        event = "commit",
        condition = "!empty(invoiceId) && invoiceTypeId != 'CUST_RTN_INVOICE' && statusId == 'INVOICE_READY' && oldStatusId != 'INVOICE_READY' && oldStatusId != 'INVOICE_PAID'",
        actions = {
            @SecaAction(
                service = "createAcctgTransForPurchaseInvoice",
                mode = "sync"
            ),
            @SecaAction(
                service = "createAcctgTransForSalesInvoice",
                mode = "sync"
            )
        }
    )
    public interface SetInvoiceStatuscommitSeca12 {}

    /**
     * SECA for service setInvoiceStatus on event commit.
     */
    @Seca(
        service = "setInvoiceStatus",
        event = "commit",
        condition = "!empty(invoiceId) && invoiceTypeId == 'CUST_RTN_INVOICE' && statusId == 'INVOICE_READY' && oldStatusId != 'INVOICE_READY' && oldStatusId != 'INVOICE_PAID'",
        actions = {
            @SecaAction(
                service = "createAcctgTransForCustomerReturnInvoice",
                mode = "sync"
            )
        }
    )
    public interface SetInvoiceStatuscommitSeca13 {}

    /**
     * SECA for service setInvoiceStatus on event commit.
     */
    @Seca(
        service = "setInvoiceStatus",
        event = "commit",
        condition = "!empty(invoiceId) && statusId == 'INVOICE_CANCELLED'",
        actions = {
            @SecaAction(
                service = "cancelInvoice",
                mode = "sync"
            )
        }
    )
    public interface SetInvoiceStatuscommitSeca14 {}

    /**
     * SECA for service createPayment on event commit.
     */
    @Seca(
        service = "createPayment",
        event = "commit",
        condition = "statusId == 'PMNT_RECEIVED'",
        actions = {
            @SecaAction(
                service = "createAcctgTransAndEntriesForIncomingPayment",
                mode = "sync"
            )
        }
    )
    public interface CreatePaymentcommitSeca15 {}

    /**
     * SECA for service setPaymentStatus on event commit.
     */
    @Seca(
        service = "setPaymentStatus",
        event = "commit",
        condition = "statusId == 'PMNT_RECEIVED' && oldStatusId != 'PMNT_RECEIVED' && oldStatusId != 'PMNT_CONFIRMED'",
        actions = {
            @SecaAction(
                service = "createAcctgTransAndEntriesForIncomingPayment",
                mode = "sync"
            )
        }
    )
    public interface SetPaymentStatuscommitSeca16 {}

    /**
     * SECA for service createPayment on event commit.
     */
    @Seca(
        service = "createPayment",
        event = "commit",
        condition = "statusId == 'PMNT_SENT'",
        actions = {
            @SecaAction(
                service = "createAcctgTransAndEntriesForOutgoingPayment",
                mode = "sync"
            )
        }
    )
    public interface CreatePaymentcommitSeca17 {}

    /**
     * SECA for service setPaymentStatus on event commit.
     */
    @Seca(
        service = "setPaymentStatus",
        event = "commit",
        condition = "statusId == 'PMNT_SENT' && oldStatusId != 'PMNT_SENT' && oldStatusId != 'PMNT_CONFIRMED'",
        actions = {
            @SecaAction(
                service = "createAcctgTransAndEntriesForOutgoingPayment",
                mode = "sync"
            )
        }
    )
    public interface SetPaymentStatuscommitSeca18 {}

    /**
     * SECA for service createPaymentApplication on event commit.
     */
    @Seca(
        service = "createPaymentApplication",
        event = "commit",
        condition = "!empty(invoiceId) && paymentTypeId != 'CUSTOMER_REFUND'",
        actions = {
            @SecaAction(
                service = "createAcctgTransAndEntriesForPaymentApplication",
                mode = "sync"
            )
        }
    )
    public interface CreatePaymentApplicationcommitSeca19 {}

    /**
     * SECA for service createPaymentApplication on event commit.
     */
    @Seca(
        service = "createPaymentApplication",
        event = "commit",
        condition = "!empty(invoiceId) && paymentTypeId == 'CUSTOMER_REFUND'",
        actions = {
            @SecaAction(
                service = "createAcctgTransAndEntriesForCustomerRefundPaymentApplication",
                mode = "sync"
            )
        }
    )
    public interface CreatePaymentApplicationcommitSeca20 {}

    /**
     * SECA for service createGlReconciliationEntry on event commit.
     */
    @Seca(
        service = "createGlReconciliationEntry",
        event = "commit",
        condition = "!empty(statusId)",
        actions = {
            @SecaAction(
                service = "setGlReconciliationStatus",
                mode = "sync"
            )
        }
    )
    public interface CreateGlReconciliationEntrycommitSeca21 {}

    /**
     * SECA for service postAcctgTrans on event commit.
     */
    @Seca(
        service = "postAcctgTrans",
        event = "commit",
        condition = "verifyOnly != 'true'",
        actions = {
            @SecaAction(
                service = "checkUpdateFixedAssetDepreciation",
                mode = "sync"
            )
        }
    )
    public interface PostAcctgTranscommitSeca22 {}

}

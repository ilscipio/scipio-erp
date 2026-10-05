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
public class InvoiceSecas {

    /**
     * SECA for service cancelInvoice on event invoke.
     */
    @Seca(
        service = "cancelInvoice",
        event = "invoke",
        actions = {
            @SecaAction(
                service = "revertAcctgTransOnCancelInvoice",
                mode = "sync"
            )
        }
    )
    public interface CancelInvoiceinvokeSeca1 {}

    /**
     * SECA for service cancelInvoice on event commit.
     */
    @Seca(
        service = "cancelInvoice",
        event = "commit",
        condition = "invoiceTypeId == 'COMMISSION_INVOICE'",
        actions = {
            @SecaAction(
                service = "removeInvoiceItemAssocOnCancelInvoice",
                mode = "sync"
            )
        }
    )
    public interface CancelInvoicecommitSeca2 {}

    /**
     * SECA for service cancelInvoice on event commit.
     */
    @Seca(
        service = "cancelInvoice",
        event = "commit",
        actions = {
            @SecaAction(
                service = "resetOrderItemBillingAndOrderAdjustmentBillingOnCancelInvoice",
                mode = "sync"
            )
        }
    )
    public interface CancelInvoicecommitSeca3 {}

    /**
     * SECA for service setInvoiceStatus on event commit.
     */
    @Seca(
        service = "setInvoiceStatus",
        event = "commit",
        condition = "statusId == 'INVOICE_APPROVED' && oldStatusId != 'INVOICE_APPROVED'",
        actions = {
            @SecaAction(
                service = "createMatchingPaymentApplication",
                mode = "sync"
            )
        }
    )
    public interface SetInvoiceStatuscommitSeca4 {}

    /**
     * SECA for service setInvoiceStatus on event return.
     */
    @Seca(
        service = "setInvoiceStatus",
        event = "return",
        condition = "statusId == 'INVOICE_READY' && oldStatusId == 'INVOICE_IN_PROCESS'",
        actions = {
            @SecaAction(
                service = "createMatchingPaymentApplication",
                mode = "sync"
            )
        }
    )
    public interface SetInvoiceStatusreturnSeca5 {}

    /**
     * SECA for service setInvoiceStatus on event commit.
     */
    @Seca(
        service = "setInvoiceStatus",
        event = "commit",
        condition = "!empty(invoiceId) && statusId == 'INVOICE_READY' && oldStatusId != 'INVOICE_READY' && oldStatusId != 'INVOICE_PAID'",
        actions = {
            @SecaAction(
                service = "checkInvoicePaymentApplications",
                mode = "sync"
            ),
            @SecaAction(
                service = "capturePaymentsByInvoice",
                mode = "sync"
            )
        }
    )
    public interface SetInvoiceStatuscommitSeca6 {}

}

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
public class PaymentSecas {

    /**
     * SECA for service setPaymentStatus on event commit.
     */
    @Seca(
        service = "setPaymentStatus",
        event = "commit",
        condition = "statusId == 'PMNT_RECEIVED' && oldStatusId != 'PMNT_RECEIVED'",
        actions = {
            @SecaAction(
                service = "checkPaymentInvoices",
                mode = "sync"
            )
        }
    )
    public interface SetPaymentStatuscommitSeca1 {}

    /**
     * SECA for service setPaymentStatus on event commit.
     */
    @Seca(
        service = "setPaymentStatus",
        event = "commit",
        condition = "statusId == 'PMNT_SENT' && oldStatusId != 'PMNT_SENT'",
        actions = {
            @SecaAction(
                service = "checkPaymentInvoices",
                mode = "sync"
            )
        }
    )
    public interface SetPaymentStatuscommitSeca2 {}

    /**
     * SECA for service createPaymentAndApplicationForParty on event commit.
     */
    @Seca(
        service = "createPaymentAndApplicationForParty",
        event = "commit",
        condition = "!empty(finAccountId)",
        actions = {
            @SecaAction(
                service = "createFinAccoutnTransFromPayment",
                mode = "sync"
            )
        }
    )
    public interface CreatePaymentAndApplicationForPartycommitSeca3 {}

    /**
     * SECA for service setFinAccountTransStatus on event commit.
     */
    @Seca(
        service = "setFinAccountTransStatus",
        event = "commit",
        condition = "!empty(finAccountTransId) && statusId == 'FINACT_TRNS_CANCELED'",
        actions = {
            @SecaAction(
                service = "expirePaymentAssociationsOnFinAccountTransCancel",
                mode = "sync"
            ),
            @SecaAction(
                service = "updatePaymentOnFinAccTransStatusSetToCancel",
                mode = "sync"
            ),
            @SecaAction(
                service = "updateFinAccountBalancesFromTrans",
                mode = "sync"
            )
        }
    )
    public interface SetFinAccountTransStatuscommitSeca4 {}

    /**
     * SECA for service removePaymentApplication on event invoke.
     */
    @Seca(
        service = "removePaymentApplication",
        event = "invoke",
        actions = {
            @SecaAction(
                service = "revertAcctgTransOnRemovePaymentApplications",
                mode = "sync"
            )
        }
    )
    public interface RemovePaymentApplicationinvokeSeca5 {}

    /**
     * SECA for service voidPayment on event commit.
     */
    @Seca(
        service = "voidPayment",
        event = "commit",
        condition = "!empty(finAccountTransId) && statusId == 'FINACT_TRNS_CANCELED'",
        actions = {
            @SecaAction(
                service = "setFinAccountTransStatus",
                mode = "sync"
            )
        }
    )
    public interface VoidPaymentcommitSeca6 {}

    /**
     * SECA for service setPaymentStatus on event commit.
     */
    @Seca(
        service = "setPaymentStatus",
        event = "commit",
        condition = "statusId == 'PMNT_RECEIVED' && oldStatusId != 'PMNT_RECEIVED'",
        actions = {
            @SecaAction(
                service = "createMatchingPaymentApplication",
                mode = "sync"
            )
        }
    )
    public interface SetPaymentStatuscommitSeca7 {}

    /**
     * SECA for service setPaymentStatus on event commit.
     */
    @Seca(
        service = "setPaymentStatus",
        event = "commit",
        condition = "statusId == 'PMNT_SENT' && oldStatusId != 'PMNT_SENT'",
        actions = {
            @SecaAction(
                service = "createMatchingPaymentApplication",
                mode = "sync"
            )
        }
    )
    public interface SetPaymentStatuscommitSeca8 {}

}

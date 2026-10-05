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
public class Secas {

    /**
     * SECA for service createPaymentApplication on event commit.
     */
    @Seca(
        service = "createPaymentApplication",
        event = "commit",
        condition = "!empty(invoiceId)",
        actions = {
            @SecaAction(
                service = "checkInvoicePaymentApplications",
                mode = "sync"
            )
        }
    )
    public interface CreatePaymentApplicationcommitSeca1 {}

    /**
     * SECA for service createPartyPostalAddress on event commit.
     */
    @Seca(
        service = "createPartyPostalAddress",
        event = "commit",
        condition = "!empty(paymentMethodId)",
        actions = {
            @SecaAction(
                service = "setPaymentMethodAddress",
                mode = "sync"
            )
        }
    )
    public interface CreatePartyPostalAddresscommitSeca2 {}

    /**
     * SECA for service updatePostalAddress on event return.
     */
    @Seca(
        service = "updatePostalAddress",
        event = "return",
        actions = {
            @SecaAction(
                service = "updatePaymentMethodAddress",
                mode = "sync"
            )
        }
    )
    public interface UpdatePostalAddressreturnSeca3 {}

    /**
     * SECA for service createBillingAccount on event return.
     */
    @Seca(
        service = "createBillingAccount",
        event = "return",
        condition = "!empty(roleTypeId) && !empty(partyId)",
        actions = {
            @SecaAction(
                service = "createBillingAccountRole",
                mode = "sync"
            )
        }
    )
    public interface CreateBillingAccountreturnSeca4 {}

    /**
     * SECA for service createBillingAccountRole on event invoke.
     */
    @Seca(
        service = "createBillingAccountRole",
        event = "invoke",
        condition = "!empty(roleTypeId) && !empty(partyId)",
        actions = {
            @SecaAction(
                service = "ensurePartyRole",
                mode = "sync"
            )
        }
    )
    public interface CreateBillingAccountRoleinvokeSeca5 {}

    /**
     * SECA for service createCreditCard on event in-validate.
     */
    @Seca(
        service = "createCreditCard",
        event = "in-validate",
        condition = "!empty(expMonth) && !empty(expYear)",
        actions = {
            @SecaAction(
                service = "buildCcExpireDate",
                mode = "sync"
            )
        }
    )
    public interface CreateCreditCardinvalidateSeca6 {}

    /**
     * SECA for service updateCreditCard on event in-validate.
     */
    @Seca(
        service = "updateCreditCard",
        event = "in-validate",
        condition = "!empty(expMonth) && !empty(expYear)",
        actions = {
            @SecaAction(
                service = "buildCcExpireDate",
                mode = "sync"
            )
        }
    )
    public interface UpdateCreditCardinvalidateSeca7 {}

    /**
     * SECA for service createCreditCard on event commit.
     */
    @Seca(
        service = "createCreditCard",
        event = "commit",
        assignments = {
            @SecaSet(fieldName = "mode", value = "CREATE")
        },
        actions = {
            @SecaAction(
                service = "verifyCreditCard",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateCreditCardcommitSeca8 {}

    /**
     * SECA for service updateCreditCard on event commit.
     */
    @Seca(
        service = "updateCreditCard",
        event = "commit",
        condition = "oldPaymentMethodId != paymentMethodId",
        assignments = {
            @SecaSet(fieldName = "mode", value = "UPDATE")
        },
        actions = {
            @SecaAction(
                service = "verifyCreditCard",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateCreditCardcommitSeca9 {}

    /**
     * SECA for service createCreditCardAndAddress on event in-validate.
     */
    @Seca(
        service = "createCreditCardAndAddress",
        event = "in-validate",
        condition = "!empty(expMonth) && !empty(expYear)",
        actions = {
            @SecaAction(
                service = "buildCcExpireDate",
                mode = "sync"
            )
        }
    )
    public interface CreateCreditCardAndAddressinvalidateSeca10 {}

    /**
     * SECA for service updateCreditCardAndAddress on event in-validate.
     */
    @Seca(
        service = "updateCreditCardAndAddress",
        event = "in-validate",
        condition = "!empty(expMonth) && !empty(expYear)",
        actions = {
            @SecaAction(
                service = "buildCcExpireDate",
                mode = "sync"
            )
        }
    )
    public interface UpdateCreditCardAndAddressinvalidateSeca11 {}

    /**
     * SECA for service createGiftCard on event in-validate.
     */
    @Seca(
        service = "createGiftCard",
        event = "in-validate",
        condition = "!empty(expMonth) && !empty(expYear)",
        actions = {
            @SecaAction(
                service = "buildCcExpireDate",
                mode = "sync"
            )
        }
    )
    public interface CreateGiftCardinvalidateSeca12 {}

    /**
     * SECA for service updateGiftCard on event in-validate.
     */
    @Seca(
        service = "updateGiftCard",
        event = "in-validate",
        condition = "!empty(expMonth) && !empty(expYear)",
        actions = {
            @SecaAction(
                service = "buildCcExpireDate",
                mode = "sync"
            )
        }
    )
    public interface UpdateGiftCardinvalidateSeca13 {}

    /**
     * SECA for service authOrderPayments on event global-rollback.
     */
    @Seca(
        service = "authOrderPayments",
        event = "global-rollback",
        actions = {
            @SecaAction(
                service = "releaseOrderPayments",
                mode = "sync"
            )
        }
    )
    public interface AuthOrderPaymentsglobalrollbackSeca14 {}

    /**
     * SECA for service retryFailedOrderAuth on event commit.
     */
    @Seca(
        service = "retryFailedOrderAuth",
        event = "commit",
        condition = "processResult != 'ERROR'",
        actions = {
            @SecaAction(
                service = "sendOrderPayRetryNotification",
                mode = "async",
                persist = "true"
            )
        }
    )
    public interface RetryFailedOrderAuthcommitSeca15 {}

    /**
     * SECA for service createFinAccountRole on event invoke.
     */
    @Seca(
        service = "createFinAccountRole",
        event = "invoke",
        actions = {
            @SecaAction(
                service = "ensurePartyRole",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface CreateFinAccountRoleinvokeSeca16 {}

    /**
     * SECA for service finAccountWithdraw on event return.
     */
    @Seca(
        service = "finAccountWithdraw",
        event = "return",
        runOnError = "true",
        condition = "!empty(productStoreId)",
        actions = {
            @SecaAction(
                service = "finAccountReplenish",
                mode = "async",
                runAsUser = "system",
                persist = "true"
            )
        }
    )
    public interface FinAccountWithdrawreturnSeca17 {}

    /**
     * SECA for service updateFinAccount on event commit.
     */
    @Seca(
        service = "updateFinAccount",
        event = "commit",
        condition = "oldReplenishPaymentId != replenishPaymentId && !empty(replenishLevel)",
        actions = {
            @SecaAction(
                service = "finAccountReplenish",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateFinAccountcommitSeca18 {}

    /**
     * SECA for service updateFinAccount on event commit.
     */
    @Seca(
        service = "updateFinAccount",
        event = "commit",
        condition = "oldReplenishLevel != replenishLevel && !empty(replenishPaymentId)",
        actions = {
            @SecaAction(
                service = "finAccountReplenish",
                mode = "sync",
                runAsUser = "system"
            )
        }
    )
    public interface UpdateFinAccountcommitSeca19 {}

    /**
     * SECA for service createFinAccountTrans on event commit.
     */
    @Seca(
        service = "createFinAccountTrans",
        event = "commit",
        condition = "!empty(glAccountId)",
        actions = {
            @SecaAction(
                service = "postFinAccountTransToGl",
                mode = "sync"
            )
        }
    )
    public interface CreateFinAccountTranscommitSeca20 {}

    /**
     * SECA for service createProduct on event commit.
     */
    @Seca(
        service = "createProduct",
        event = "commit",
        condition = "productTypeId == 'ASSET_USAGE'",
        actions = {
            @SecaAction(
                service = "createFixedAssetAndLinkToProduct",
                mode = "sync"
            )
        }
    )
    public interface CreateProductcommitSeca21 {}

}

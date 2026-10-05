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
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PaymentmethodServices {

    /**
     * Delete PaymentMethod
     */
    @Service(
        name = "deletePaymentMethod",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentMethodServices",
        invoke = "deletePaymentMethod",
        description = "Delete PaymentMethod",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN")
        }
    )
    public interface DeletePaymentMethod {}

    /**
     * Create CreditCard
     */
    @Service(
        name = "createCreditCard",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentMethodServices",
        invoke = "createCreditCard",
        description = "Create CreditCard",
        defaultEntityName = "CreditCard",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "expMonth", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expYear", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "firstNameOnCard", optional = "false"),
            @OverrideAttribute(name = "lastNameOnCard", optional = "false"),
            @OverrideAttribute(name = "cardType", optional = "false"),
            @OverrideAttribute(name = "cardNumber", optional = "false"),
            @OverrideAttribute(name = "expireDate", optional = "false")
        }
    )
    public interface CreateCreditCard {}

    /**
     * Creates a CreditCard and PostalAddress
     */
    @Service(
        name = "createCreditCardAndAddress",
        engine = "group",
        location = "createCreditCardAndAddress",
        description = "Creates a CreditCard and PostalAddress",
        auth = "true",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateCreditCardAndAddress {}

    /**
     * Updates a CreditCard and PostalAddress
     */
    @Service(
        name = "updateCreditCardAndAddress",
        engine = "group",
        location = "updateCreditCardAndAddress",
        description = "Updates a CreditCard and PostalAddress",
        auth = "true",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface UpdateCreditCardAndAddress {}

    /**
     * Update CreditCard
     */
    @Service(
        name = "updateCreditCard",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentMethodServices",
        invoke = "updateCreditCard",
        description = "Update CreditCard",
        defaultEntityName = "CreditCard",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expMonth", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expYear", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "OUT"),
            @Attribute(name = "oldPaymentMethodId", type = "String", mode = "OUT"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdateCreditCard {}

    /**
     * Clears the credit card number out of the value object and expires the payment method
     */
    @Service(
        name = "clearCreditCardDataAndExpire",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentMethodServices",
        invoke = "clearCreditCardData",
        description = "Clears the credit card number out of the value object and expires the payment method",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN")
        }
    )
    public interface ClearCreditCardDataAndExpire {}

    /**
     * Makes a Expire Date
     */
    @Service(
        name = "buildCcExpireDate",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentMethodServices",
        invoke = "makeExpireDate",
        description = "Makes a Expire Date",
        attributes = {
            @Attribute(name = "expMonth", type = "String", mode = "IN"),
            @Attribute(name = "expYear", type = "String", mode = "IN"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT")
        }
    )
    public interface BuildCcExpireDate {}

    /**
     * Create GiftCard
     */
    @Service(
        name = "createGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentMethodServices",
        invoke = "createGiftCard",
        description = "Create GiftCard",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pinNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expMonth", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expYear", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateGiftCard {}

    /**
     * Update GiftCard
     */
    @Service(
        name = "updateGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentMethodServices",
        invoke = "updateGiftCard",
        description = "Update GiftCard",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "INOUT"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pinNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expMonth", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expYear", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "oldPaymentMethodId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface UpdateGiftCard {}

    /**
     * SCIPIO: Verifies if the given gift card number and pin are valid (best-effort)             (based on: org.ofbiz.order.shoppingcart.CheckOutHelper#checkGiftCard)
     */
    @Service(
        name = "verifyGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentMethodServices",
        invoke = "verifyGiftCard",
        description = "SCIPIO: Verifies if the given gift card number and pin are valid (best-effort)\n            (based on: org.ofbiz.order.shoppingcart.CheckOutHelper#checkGiftCard)",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN", description = "Note: If cardNumber is masked (************XXXX), it is not verified for validity."),
            @Attribute(name = "pinNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expMonth", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expYear", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN")
        }
    )
    public interface VerifyGiftCard {}

    /**
     * SCIPIO: Verifies if the given gift card number and pin are valid (best-effort)             (based on: org.ofbiz.order.shoppingcart.CheckOutHelper#checkGiftCard), but ignores cardNumber if it             is in masked format (************XXXX)
     */
    @Service(
        name = "verifyGiftCardIgnoreMasked",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentMethodServices",
        invoke = "verifyGiftCardIgnoreMasked",
        description = "SCIPIO: Verifies if the given gift card number and pin are valid (best-effort)\n            (based on: org.ofbiz.order.shoppingcart.CheckOutHelper#checkGiftCard), but ignores cardNumber if it\n            is in masked format (************XXXX)",
        auth = "true",
        implemented = {@Implements(service = "verifyGiftCard")}
    )
    public interface VerifyGiftCardIgnoreMasked {}

    /**
     * SCIPIO: Create and verify GiftCard (best-effort)
     */
    @Service(
        name = "createVerifyGiftCard",
        engine = "group",
        description = "SCIPIO: Create and verify GiftCard (best-effort)",
        auth = "true",
        invokes = {@GroupInvoke(name = "verifyGiftCard", resultToContext = "false"), @GroupInvoke(name = "createGiftCard", resultToContext = "false")}
    )
    public interface CreateVerifyGiftCard {}

    /**
     * SCIPIO: Update and verify GiftCard (best-effort)
     */
    @Service(
        name = "updateVerifyGiftCard",
        engine = "group",
        description = "SCIPIO: Update and verify GiftCard (best-effort)",
        auth = "true",
        invokes = {@GroupInvoke(name = "verifyGiftCardIgnoreMasked", resultToContext = "false"), @GroupInvoke(name = "updateGiftCard", resultToContext = "false")}
    )
    public interface UpdateVerifyGiftCard {}

    /**
     * Create EftAccount
     */
    @Service(
        name = "createEftAccount",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentMethodServices",
        invoke = "createEftAccount",
        description = "Create EftAccount",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "bankName", type = "String", mode = "IN"),
            @Attribute(name = "routingNumber", type = "String", mode = "IN"),
            @Attribute(name = "accountType", type = "String", mode = "IN"),
            @Attribute(name = "accountNumber", type = "String", mode = "IN"),
            @Attribute(name = "nameOnAccount", type = "String", mode = "IN"),
            @Attribute(name = "companyNameOnAccount", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateEftAccount {}

    /**
     * Creates an EftAccount and PostalAddress
     */
    @Service(
        name = "createEftAccountAndAddress",
        engine = "group",
        location = "createEftAccountAndAddress",
        description = "Creates an EftAccount and PostalAddress",
        auth = "true",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface CreateEftAccountAndAddress {}

    /**
     * Updates an EftAccount and PostalAddress
     */
    @Service(
        name = "updateEftAccountAndAddress",
        engine = "group",
        location = "updateEftAccountAndAddress",
        description = "Updates an EftAccount and PostalAddress",
        auth = "true",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "INOUT", optional = "true")
        }
    )
    public interface UpdateEftAccountAndAddress {}

    /**
     * Update EftAccount
     */
    @Service(
        name = "updateEftAccount",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentMethodServices",
        invoke = "updateEftAccount",
        description = "Update EftAccount",
        auth = "true",
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentMethodId", type = "String", mode = "INOUT"),
            @Attribute(name = "bankName", type = "String", mode = "IN"),
            @Attribute(name = "routingNumber", type = "String", mode = "IN"),
            @Attribute(name = "accountType", type = "String", mode = "IN"),
            @Attribute(name = "accountNumber", type = "String", mode = "IN"),
            @Attribute(name = "nameOnAccount", type = "String", mode = "IN"),
            @Attribute(name = "companyNameOnAccount", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "oldPaymentMethodId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface UpdateEftAccount {}

    /**
     * Set the inital payment method address.
     */
    @Service(
        name = "setPaymentMethodAddress",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodServices.xml",
        invoke = "setPaymentMethodAddress",
        description = "Set the inital payment method address.",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN"),
            @Attribute(name = "contactMechId", type = "String", mode = "IN")
        }
    )
    public interface SetPaymentMethodAddress {}

    /**
     * Finds CreditCards and EftAccounts that use the oldContactMechId and updates to the contactMechId
     */
    @Service(
        name = "updatePaymentMethodAddress",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodServices.xml",
        invoke = "updatePaymentMethodAddress",
        description = "Finds CreditCards and EftAccounts that use the oldContactMechId and updates to the contactMechId",
        auth = "true",
        useTransaction = "false",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "IN"),
            @Attribute(name = "oldContactMechId", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpdatePaymentMethodAddress {}

    /**
     * Create a PayPal Payment Method
     */
    @Service(
        name = "createPayPalPaymentMethod",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodServices.xml",
        invoke = "createPayPalPaymentMethod",
        description = "Create a PayPal Payment Method",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PayPalPaymentMethod", mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(entityName = "PayPalPaymentMethod", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "fromDate", type = "Timestamp", mode = "IN", optional = "true"),
            @Attribute(name = "thruDate", type = "Timestamp", mode = "IN", optional = "true")
        }
    )
    public interface CreatePayPalPaymentMethod {}

    /**
     * Update a PayPal Payment Method
     */
    @Service(
        name = "updatePayPalPaymentMethod",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodServices.xml",
        invoke = "updatePayPalPaymentMethod",
        description = "Update a PayPal Payment Method",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "PayPalPaymentMethod", mode = "IN", include = "nonpk", optional = "true")
        },
        attributes = {
            @Attribute(name = "paymentMethodId", type = "String", mode = "INOUT")
        }
    )
    public interface UpdatePayPalPaymentMethod {}

    /**
     * Direct link to payment processors to force manual CC authorizations; not logged in system
     */
    @Service(
        name = "manualForcedCcAuthTransaction",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "processManualCcAuth",
        description = "Direct link to payment processors to force manual CC authorizations; not logged in system",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "securityCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN")
        }
    )
    public interface ManualForcedCcAuthTransaction {}

    /**
     * Direct link to payment processors to force manual transactions; not logged in system
     */
    @Service(
        name = "manualForcedCcTransaction",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "processManualCcTx",
        description = "Direct link to payment processors to force manual transactions; not logged in system",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentMethodTypeId", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "transactionType", type = "String", mode = "IN"),
            @Attribute(name = "companyNameOnCard", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "titleOnCard", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "firstNameOnCard", type = "String", mode = "IN"),
            @Attribute(name = "middleNameOnCard", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "lastNameOnCard", type = "String", mode = "IN"),
            @Attribute(name = "suffixOnCard", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cardType", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "cardSecurityCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "expMonth", type = "String", mode = "IN"),
            @Attribute(name = "expYear", type = "String", mode = "IN"),
            @Attribute(name = "infoString", type = "String", mode = "IN"),
            @Attribute(name = "address1", type = "String", mode = "IN"),
            @Attribute(name = "address2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "city", type = "String", mode = "IN"),
            @Attribute(name = "stateProvinceGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "postalCode", type = "String", mode = "IN"),
            @Attribute(name = "countryGeoId", type = "String", mode = "IN"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "referenceCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "orderPaymentPreferenceId", type = "String", mode = "IN"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT"),
            @Attribute(name = "tranRespMsgs", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface ManualForcedCcTransaction {}

    /**
     * (Batch) Retries failed authorizations due to processor/connection problems
     */
    @Service(
        name = "retryFailedAuths",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "retryFailedAuths",
        description = "(Batch) Retries failed authorizations due to processor/connection problems",
        auth = "true"
    )
    public interface RetryFailedAuths {}

    /**
     * Retries failed authorization due to processor/connection problems, or NSF (Not Sufficient Funds) failure, for an order
     */
    @Service(
        name = "retryFailedOrderAuth",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "retryFailedOrderAuth",
        description = "Retries failed authorization due to processor/connection problems, or NSF (Not Sufficient Funds) failure, for an order",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "processResult", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "authResultMsgs", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface RetryFailedOrderAuth {}

    /**
     * (Batch) Retries failed authorizations due to NSF (Not Sufficient Funds); these are for auto-orders
     */
    @Service(
        name = "retryFailedAuthNsfs",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "retryFailedAuthNsfs",
        description = "(Batch) Retries failed authorizations due to NSF (Not Sufficient Funds); these are for auto-orders",
        auth = "true"
    )
    public interface RetryFailedAuthNsfs {}

    /**
     * Process (authorizes/re-authorizes) a single payment for an order with an optional overrideAmount
     */
    @Service(
        name = "authOrderPaymentPreference",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "authOrderPaymentPreference",
        description = "Process (authorizes/re-authorizes) a single payment for an order with an optional overrideAmount",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreferenceId", type = "String", mode = "IN"),
            @Attribute(name = "overrideAmount", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "finished", type = "Boolean", mode = "OUT"),
            @Attribute(name = "errors", type = "Boolean", mode = "OUT"),
            @Attribute(name = "messages", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "processAmount", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "authCode", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface AuthOrderPaymentPreference {}

    /**
     * Process (authorizes/re-authorizes) payments for an order
     */
    @Service(
        name = "authOrderPayments",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "authOrderPayments",
        description = "Process (authorizes/re-authorizes) payments for an order",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "processResult", type = "String", mode = "OUT"),
            @Attribute(name = "authResultMsgs", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "reAuth", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "authCode", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface AuthOrderPayments {}

    /**
     * Releases all payment authorizations for an order
     */
    @Service(
        name = "releaseOrderPayments",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "releaseOrderPayments",
        description = "Releases all payment authorizations for an order",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "processResult", type = "String", mode = "OUT"),
            @Attribute(name = "orderPaymentPreferenceId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ReleaseOrderPayments {}

    /**
     * Releases payment authorization for a single OrderPaymentPreference
     */
    @Service(
        name = "releaseOrderPaymentPreference",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "releaseOrderPaymentPreference",
        description = "Releases payment authorization for a single OrderPaymentPreference",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreferenceId", type = "String", mode = "IN")
        }
    )
    public interface ReleaseOrderPaymentPreference {}

    /**
     * Captures (settles) pre-authorized order payments by invoice
     */
    @Service(
        name = "capturePaymentsByInvoice",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "capturePaymentsByInvoice",
        description = "Captures (settles) pre-authorized order payments by invoice",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "processResult", type = "String", mode = "OUT")
        }
    )
    public interface CapturePaymentsByInvoice {}

    /**
     * Captures (settles) pre-authorized order payments, re-authorizing any remaining balance.  If the order involves billing accounts,             capture to it in full before proceeding to other payment preferences.
     */
    @Service(
        name = "captureOrderPayments",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "captureOrderPayments",
        description = "Captures (settles) pre-authorized order payments, re-authorizing any remaining balance.  If the order involves billing accounts,\n            capture to it in full before proceeding to other payment preferences.",
        auth = "true",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billingAccountId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "captureAmount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "processResult", type = "String", mode = "OUT")
        }
    )
    public interface CaptureOrderPayments {}

    /**
     * Records a settlement or payment of an invoice by a billing account for the given captureAmount
     */
    @Service(
        name = "captureBillingAccountPayment",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "captureBillingAccountPayment",
        description = "Records a settlement or payment of an invoice by a billing account for the given captureAmount",
        auth = "true",
        attributes = {
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "billingAccountId", type = "String", mode = "IN"),
            @Attribute(name = "captureAmount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentId", type = "String", mode = "OUT"),
            @Attribute(name = "paymentGatewayResponseId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface CaptureBillingAccountPayment {}

    /**
     * Applies (part of) the unapplied payment applications associated to the billing account to the given invoice.
     */
    @Service(
        name = "captureBillingAccountPayments",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "captureBillingAccountPayments",
        description = "Applies (part of) the unapplied payment applications associated to the billing account to the given invoice.",
        auth = "true",
        attributes = {
            @Attribute(name = "billingAccountId", type = "String", mode = "IN"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN"),
            @Attribute(name = "captureAmount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CaptureBillingAccountPayments {}

    /**
     * Handles the creation of new OrderPaymentPreference record (and Auth) for partial captures
     */
    @Service(
        name = "processCaptureSplitPayment",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "processCaptureSplitPayment",
        description = "Handles the creation of new OrderPaymentPreference record (and Auth) for partial captures",
        auth = "true",
        requireNewTransaction = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "splitAmount", type = "BigDecimal", mode = "IN")
        }
    )
    public interface ProcessCaptureSplitPayment {}

    /**
     * Refund payment authorization for a single OrderPaymentPreference
     */
    @Service(
        name = "refundOrderPaymentPreference",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "refundOrderPaymentPreference",
        description = "Refund payment authorization for a single OrderPaymentPreference",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreferenceId", type = "String", mode = "IN"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "refundAmount", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "paymentId", type = "String", mode = "OUT")
        }
    )
    public interface RefundOrderPaymentPreference {}

    /**
     * Refunds A Payment
     */
    @Service(
        name = "refundPayment",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "refundPayment",
        description = "Refunds A Payment",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "refundAmount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "paymentId", type = "String", mode = "OUT")
        }
    )
    public interface RefundPayment {}

    /**
     * Process the payment authorization result(s)
     */
    @Service(
        name = "processAuthResult",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "processAuthResult",
        description = "Process the payment authorization result(s)",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "processAmount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "authResult", type = "Boolean", mode = "IN"),
            @Attribute(name = "serviceTypeEnum", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "authCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "authAltRefNum", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "authRefNum", type = "String", mode = "IN"),
            @Attribute(name = "authFlag", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "authMessage", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cvCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "avsCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "scoreCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "internalRespMsgs", type = "List", mode = "IN", optional = "true")
        }
    )
    public interface ProcessAuthResult {}

    /**
     * Process the payment capture result(s)
     */
    @Service(
        name = "processCaptureResult",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "processCaptureResult",
        description = "Process the payment capture result(s)",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "serviceTypeEnum", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "payToPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "captureAmount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "captureResult", type = "Boolean", mode = "IN"),
            @Attribute(name = "captureAltRefNum", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "captureRefNum", type = "String", mode = "IN"),
            @Attribute(name = "captureCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "captureFlag", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "captureMessage", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "internalRespMsgs", type = "List", mode = "IN", optional = "true")
        }
    )
    public interface ProcessCaptureResult {}

    /**
     * Process the payment release result(s)
     */
    @Service(
        name = "processReleaseResult",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "processReleaseResult",
        description = "Process the payment release result(s)",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "releaseAmount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "releaseResult", type = "Boolean", mode = "IN"),
            @Attribute(name = "releaseAltRefNum", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "releaseRefNum", type = "String", mode = "IN"),
            @Attribute(name = "releaseCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "releaseFlag", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "releaseMessage", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "internalRespMsgs", type = "List", mode = "IN", optional = "true")
        }
    )
    public interface ProcessReleaseResult {}

    /**
     * Process the payment credit result(s)
     */
    @Service(
        name = "processCreditResult",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "processCreditResult",
        description = "Process the payment credit result(s)",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "creditAmount", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "creditResult", type = "Boolean", mode = "IN"),
            @Attribute(name = "creditAltRefNum", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "creditRefNum", type = "String", mode = "IN"),
            @Attribute(name = "creditCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "creditFlag", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "creditMessage", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "internalRespMsgs", type = "List", mode = "IN", optional = "true")
        }
    )
    public interface ProcessCreditResult {}

    /**
     * Process the payment refund result(s)
     */
    @Service(
        name = "processRefundResult",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "processRefundResult",
        description = "Process the payment refund result(s)",
        auth = "true",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "serviceTypeEnum", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "payFromPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "payToPartyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "invoiceId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "refundAmount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "currencyUomId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "refundResult", type = "Boolean", mode = "IN"),
            @Attribute(name = "refundAltRefNum", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "refundRefNum", type = "String", mode = "IN"),
            @Attribute(name = "refundCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "refundFlag", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "refundMessage", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "internalRespMsgs", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "paymentId", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface ProcessRefundResult {}

    /**
     * Process (store) error messages from payment service failures
     */
    @Service(
        name = "processPaymentServiceError",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "storePaymentErrorMessage",
        description = "Process (store) error messages from payment service failures",
        auth = "true",
        requireNewTransaction = "true",
        maxRetry = "5",
        attributes = {
            @Attribute(name = "paymentServiceTypeEnumId", type = "String", mode = "IN"),
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "transCodeEnumId", type = "String", mode = "IN"),
            @Attribute(name = "serviceResultMap", type = "Map", mode = "IN")
        }
    )
    public interface ProcessPaymentServiceError {}

    /**
     * Method to make sure PaymentGatewayResponse records get stored (uses XA wrapper on rollback)
     */
    @Service(
        name = "savePaymentGatewayResponse",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "savePaymentGatewayResponse",
        description = "Method to make sure PaymentGatewayResponse records get stored (uses XA wrapper on rollback)",
        attributes = {
            @Attribute(name = "paymentGatewayResponse", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        }
    )
    public interface SavePaymentGatewayResponse {}

    /**
     * Method to make sure PaymentGatewayResponse records get stored (uses XA wrapper on rollback)
     */
    @Service(
        name = "savePaymentGatewayResponseAndMessages",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "savePaymentGatewayResponseAndMessages",
        description = "Method to make sure PaymentGatewayResponse records get stored (uses XA wrapper on rollback)",
        attributes = {
            @Attribute(name = "paymentGatewayResponse", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "messages", type = "List", mode = "IN")
        }
    )
    public interface SavePaymentGatewayResponseAndMessages {}

    /**
     * Generic Payment Processing Interface
     */
    @Service(
        name = "paymentProcessInterface",
        engine = "interface",
        description = "Generic Payment Processing Interface",
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "processAmount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "orderItems", type = "List", mode = "IN"),
            @Attribute(name = "billToParty", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "billToEmail", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "billingAddress", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "shippingAddress", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "customerIpAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "currency", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentConfig", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentGatewayConfigId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "authResult", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "captureResult", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "resultDeclined", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "resultNsf", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "resultBadExpire", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "resultBadCardNumber", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "authCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "authAltRefNum", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "authRefNum", type = "String", mode = "OUT"),
            @Attribute(name = "authFlag", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "authMessage", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cvCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "avsCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "scoreCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "captureAltRefNum", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "captureRefNum", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "captureCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "captureFlag", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "captureMessage", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "internalRespMsgs", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "customerRespMsgs", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface PaymentProcessInterface {}

    /**
     * Generic Payment Release (reverse) Interface
     */
    @Service(
        name = "paymentReleaseInterface",
        engine = "interface",
        description = "Generic Payment Release (reverse) Interface",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "releaseAmount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "currency", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentConfig", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "authTrans", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "paymentGatewayConfigId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "releaseResult", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "releaseAltRefNum", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "releaseRefNum", type = "String", mode = "OUT"),
            @Attribute(name = "releaseCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "releaseFlag", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "releaseMessage", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "internalRespMsgs", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface PaymentReleaseInterface {}

    /**
     * Generic Payment Credit Interface
     */
    @Service(
        name = "paymentCreditInterface",
        engine = "interface",
        description = "Generic Payment Credit Interface",
        attributes = {
            @Attribute(name = "referenceCode", type = "String", mode = "IN"),
            @Attribute(name = "creditAmount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "orderItems", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "billToParty", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "billToEmail", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "billingAddress", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "currency", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentConfig", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentGatewayConfigId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "creditResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "creditAltRefNum", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "creditRefNum", type = "String", mode = "OUT"),
            @Attribute(name = "creditCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "creditFlag", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "creditMessage", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "internalRespMsgs", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface PaymentCreditInterface {}

    /**
     * Generic Payment Refund Interface
     */
    @Service(
        name = "paymentRefundInterface",
        engine = "interface",
        description = "Generic Payment Refund Interface",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "refundAmount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "currency", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentConfig", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentGatewayConfigId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "refundResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "refundAltRefNum", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "refundRefNum", type = "String", mode = "OUT"),
            @Attribute(name = "refundCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "refundFlag", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "refundMessage", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "internalRespMsgs", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface PaymentRefundInterface {}

    /**
     * Credit Card Authorization Interface
     */
    @Service(
        name = "ccAuthInterface",
        engine = "interface",
        description = "Credit Card Authorization Interface",
        implemented = {@Implements(service = "paymentProcessInterface")},
        attributes = {
            @Attribute(name = "creditCard", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "cardSecurityCode", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CcAuthInterface {}

    /**
     * Credit Card Capture Interface
     */
    @Service(
        name = "ccCaptureInterface",
        engine = "interface",
        description = "Credit Card Capture Interface",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "captureAmount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "currency", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentConfig", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "authTrans", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "paymentGatewayConfigId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "captureResult", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "captureAltRefNum", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "captureRefNum", type = "String", mode = "OUT"),
            @Attribute(name = "captureCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "captureFlag", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "captureMessage", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "internalRespMsgs", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface CcCaptureInterface {}

    /**
     * Credit Card 'Credit' Inteface
     */
    @Service(
        name = "ccCreditInterface",
        engine = "interface",
        description = "Credit Card 'Credit' Inteface",
        implemented = {@Implements(service = "paymentCreditInterface")},
        attributes = {
            @Attribute(name = "creditCard", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "cardSecurityCode", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface CcCreditInterface {}

    /**
     * EFT Account Processing Interface
     */
    @Service(
        name = "eftProcessInterface",
        engine = "interface",
        description = "EFT Account Processing Interface",
        implemented = {@Implements(service = "paymentProcessInterface")},
        attributes = {
            @Attribute(name = "eftAccount", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        }
    )
    public interface EftProcessInterface {}

    /**
     * PayPal authorize Interface
     */
    @Service(
        name = "payPalProcessInterface",
        engine = "interface",
        description = "PayPal authorize Interface",
        implemented = {@Implements(service = "paymentProcessInterface")},
        attributes = {
            @Attribute(name = "payPalPaymentMethod", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        }
    )
    public interface PayPalProcessInterface {}

    /**
     * PayPal Capture Interface
     */
    @Service(
        name = "payPalCaptureInterface",
        engine = "interface",
        description = "PayPal Capture Interface",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "org.ofbiz.entity.GenericValue", mode = "IN"),
            @Attribute(name = "captureAmount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "currency", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "paymentConfig", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "authTrans", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "paymentGatewayConfigId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "captureResult", type = "Boolean", mode = "OUT", optional = "true"),
            @Attribute(name = "captureAltRefNum", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "captureRefNum", type = "String", mode = "OUT"),
            @Attribute(name = "captureCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "captureFlag", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "captureMessage", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "internalRespMsgs", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface PayPalCaptureInterface {}

    /**
     * Interface for services performing the PayPal getExpressCheckout operation
     */
    @Service(
        name = "payPalSetExpressCheckoutInterface",
        engine = "interface",
        description = "Interface for services performing the PayPal getExpressCheckout operation",
        attributes = {
            @Attribute(name = "cart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN")
        }
    )
    public interface PayPalSetExpressCheckoutInterface {}

    /**
     * Interface for services performing the PayPal getExpressCheckoutDetails operation
     */
    @Service(
        name = "payPalGetExpressCheckoutInterface",
        engine = "interface",
        description = "Interface for services performing the PayPal getExpressCheckoutDetails operation",
        attributes = {
            @Attribute(name = "cart", type = "org.ofbiz.order.shoppingcart.ShoppingCart", mode = "IN")
        }
    )
    public interface PayPalGetExpressCheckoutInterface {}

    /**
     * Interface for services performing the PayPal doExpressCheckout operation
     */
    @Service(
        name = "payPalDoExpressCheckoutInterface",
        engine = "interface",
        description = "Interface for services performing the PayPal doExpressCheckout operation",
        attributes = {
            @Attribute(name = "orderPaymentPreference", type = "GenericValue", mode = "IN")
        }
    )
    public interface PayPalDoExpressCheckoutInterface {}

    /**
     * Gift Card Processing Interface
     */
    @Service(
        name = "giftCardProcessInterface",
        engine = "interface",
        description = "Gift Card Processing Interface",
        implemented = {@Implements(service = "paymentProcessInterface")},
        attributes = {
            @Attribute(name = "giftCard", type = "org.ofbiz.entity.GenericValue", mode = "IN")
        }
    )
    public interface GiftCardProcessInterface {}

    /**
     * Test Credit Card Auth Processing: declines auth requests for all orders less than 100.00; approves auth requests for all orders greater than or equal to 100.00
     */
    @Service(
        name = "testCCProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "testProcessor",
        description = "Test Credit Card Auth Processing: declines auth requests for all orders less than 100.00; approves auth requests for all orders greater than or equal to 100.00",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface TestCCProcessor {}

    /**
     * Test Credit Card Auth Processing: declines auth requests for all orders less than 100.00; approves auth requests for all orders greater than or equal to 100.00
     */
    @Service(
        name = "testCCProcessorWithCapture",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "testProcessorWithCapture",
        description = "Test Credit Card Auth Processing: declines auth requests for all orders less than 100.00; approves auth requests for all orders greater than or equal to 100.00",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface TestCCProcessorWithCapture {}

    /**
     * Test Credit Card Auth Processing: does random auth declines
     */
    @Service(
        name = "testRandomAuthorize",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "testRandomAuthorize",
        description = "Test Credit Card Auth Processing: does random auth declines",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface TestRandomAuthorize {}

    /**
     * Test Credit Card Auth Processing: always approve the auth request
     */
    @Service(
        name = "alwaysApproveCCProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "alwaysApproveProcessor",
        description = "Test Credit Card Auth Processing: always approve the auth request",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface AlwaysApproveCCProcessor {}

    /**
     * Test Credit Card Auth Processing: always approve with capture auth request
     */
    @Service(
        name = "alwaysApproveWithCaptureCCProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "alwaysApproveWithCapture",
        description = "Test Credit Card Auth Processing: always approve with capture auth request",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface AlwaysApproveWithCaptureCCProcessor {}

    /**
     * Test Credit Card Auth Processing: always decline the auth request
     */
    @Service(
        name = "alwaysDeclineCCProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "alwaysDeclineProcessor",
        description = "Test Credit Card Auth Processing: always decline the auth request",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface AlwaysDeclineCCProcessor {}

    /**
     * Test Credit Card Auth Processing: always decline for NSF (not sufficient funds) auth request
     */
    @Service(
        name = "alwaysNsfCCProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "alwaysNsfProcessor",
        description = "Test Credit Card Auth Processing: always decline for NSF (not sufficient funds) auth request",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface AlwaysNsfCCProcessor {}

    /**
     * Test Credit Card Auth Processing: always fail/bad expire date processor
     */
    @Service(
        name = "alwaysBadExpireCCProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "alwaysBadExpireProcessor",
        description = "Test Credit Card Auth Processing: always fail/bad expire date processor",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface AlwaysBadExpireCCProcessor {}

    /**
     * Test Credit Card Auth Processing: fail/bad expire date when year is even processor
     */
    @Service(
        name = "badExpireEvenCCProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "badExpireEvenProcessor",
        description = "Test Credit Card Auth Processing: fail/bad expire date when year is even processor",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface BadExpireEvenCCProcessor {}

    /**
     * Test Credit Card Auth Processing: always decline the auth request for bad card number
     */
    @Service(
        name = "alwaysBadCardNumberCCProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "alwaysBadCardNumberProcessor",
        description = "Test Credit Card Auth Processing: always decline the auth request for bad card number",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface AlwaysBadCardNumberCCProcessor {}

    /**
     * Test Credit Card Auth Processing: always fail (error) the auth transaction request
     */
    @Service(
        name = "alwaysFailCCProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "alwaysFailProcessor",
        description = "Test Credit Card Auth Processing: always fail (error) the auth transaction request",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface AlwaysFailCCProcessor {}

    /**
     * Test Credit Card Capture Processing: always approve the capture request
     */
    @Service(
        name = "testCCCapture",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "testCapture",
        description = "Test Credit Card Capture Processing: always approve the capture request",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface TestCCCapture {}

    /**
     * Test Credit Card Capture Processing: always approve with reauth the capture request
     */
    @Service(
        name = "testCCCaptureWithReAuth",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "testCaptureWithReAuth",
        description = "Test Credit Card Capture Processing: always approve with reauth the capture request",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface TestCCCaptureWithReAuth {}

    /**
     * Test Credit Card Capture Processing: always decline a cc capture request
     */
    @Service(
        name = "testCCProcessorCaptureAlwaysDecline",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "testCCProcessorCaptureAlwaysDecline",
        description = "Test Credit Card Capture Processing: always decline a cc capture request",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface TestCCProcessorCaptureAlwaysDecline {}

    /**
     * Test Credit Card Release Processing: always approve the release request
     */
    @Service(
        name = "testCCRelease",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "testRelease",
        description = "Test Credit Card Release Processing: always approve the release request",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface TestCCRelease {}

    /**
     * Test Credit Card Refund Processing: always approve the refund request
     */
    @Service(
        name = "testCCRefund",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "testRefund",
        description = "Test Credit Card Refund Processing: always approve the refund request",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface TestCCRefund {}

    /**
     * Credit Card Test Refund Failure
     */
    @Service(
        name = "testCCRefundFailure",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "testRefundFailure",
        description = "Credit Card Test Refund Failure",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface TestCCRefundFailure {}

    /**
     * Gift Card Processing
     */
    @Service(
        name = "alwaysApproveGCProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "alwaysApproveWithCapture",
        description = "Gift Card Processing",
        implemented = {@Implements(service = "giftCardProcessInterface")}
    )
    public interface AlwaysApproveGCProcessor {}

    /**
     * Gift Card Processing
     */
    @Service(
        name = "alwaysDeclineGCProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "alwaysDeclineProcessor",
        description = "Gift Card Processing",
        implemented = {@Implements(service = "giftCardProcessInterface")}
    )
    public interface AlwaysDeclineGCProcessor {}

    /**
     * Gift Card Test Release
     */
    @Service(
        name = "testGCRelease",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "testRelease",
        description = "Gift Card Test Release",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface TestGCRelease {}

    /**
     * EFT Account Processing
     */
    @Service(
        name = "testEFTProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "testProcessor",
        description = "EFT Account Processing",
        implemented = {@Implements(service = "eftProcessInterface")}
    )
    public interface TestEFTProcessor {}

    /**
     * EFT Account Processing
     */
    @Service(
        name = "alwaysApproveEFTProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "alwaysApproveProcessor",
        description = "EFT Account Processing",
        implemented = {@Implements(service = "eftProcessInterface")}
    )
    public interface AlwaysApproveEFTProcessor {}

    /**
     * EFT Account Processing
     */
    @Service(
        name = "alwaysDeclineEFTProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "alwaysDeclineProcessor",
        description = "EFT Account Processing",
        implemented = {@Implements(service = "eftProcessInterface")}
    )
    public interface AlwaysDeclineEFTProcessor {}

    /**
     * EFT Account Test Release
     */
    @Service(
        name = "testEFTRelease",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "testRelease",
        description = "EFT Account Test Release",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface TestEFTRelease {}

    @Service(
        name = "verifyCreditCard",
        engine = "java",
        location = "org.ofbiz.accounting.payment.PaymentGatewayServices",
        invoke = "verifyCreditCard",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentMethodId", type = "String", mode = "IN"),
            @Attribute(name = "oldPaymentMethodId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "mode", type = "String", mode = "IN")
        }
    )
    public interface VerifyCreditCard {}

    /**
     * create a Credit Card Gl Account
     */
    @Service(
        name = "createCreditCardTypeGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodServices.xml",
        invoke = "createCreditCardTypeGlAccount",
        description = "create a Credit Card Gl Account",
        defaultEntityName = "CreditCardTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        }
    )
    public interface CreateCreditCardTypeGlAccount {}

    /**
     * Update a Credit Card Gl Account 
     */
    @Service(
        name = "updateCreditCardTypeGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodServices.xml",
        invoke = "updateCreditCardTypeGlAccount",
        description = "Update a Credit Card Gl Account ",
        defaultEntityName = "CreditCardTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk")
        }
    )
    public interface UpdateCreditCardTypeGlAccount {}

    /**
     * delete a Credit Card Gl Account
     */
    @Service(
        name = "deleteCreditCardTypeGlAccount",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodServices.xml",
        invoke = "deleteCreditCardTypeGlAccount",
        description = "delete a Credit Card Gl Account",
        defaultEntityName = "CreditCardTypeGlAccount",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeleteCreditCardTypeGlAccount {}

    /**
     * Create a PaymentMethodType record
     */
    @Service(
        name = "createPaymentMethodType",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a PaymentMethodType record",
        defaultEntityName = "PaymentMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "INOUT", include = "pk", optional = "true"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface CreatePaymentMethodType {}

    /**
     * Update a Payment Method Type
     */
    @Service(
        name = "updatePaymentMethodType",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodServices.xml",
        invoke = "updatePaymentMethodType",
        description = "Update a Payment Method Type",
        defaultEntityName = "PaymentMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentMethodType {}

    /**
     * Delete a PaymentMethodType record
     */
    @Service(
        name = "deletePaymentMethodType",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a PaymentMethodType record",
        defaultEntityName = "PaymentMethodType",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePaymentMethodType {}

    /**
     * Create a Payment Group
     */
    @Service(
        name = "createPaymentGroup",
        engine = "entity-auto",
        invoke = "create",
        description = "Create a Payment Group",
        defaultEntityName = "PaymentGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "OUT", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "paymentGroupTypeId", optional = "false"),
            @OverrideAttribute(name = "paymentGroupName", optional = "false")
        }
    )
    public interface CreatePaymentGroup {}

    /**
     * Update a Payment Group
     */
    @Service(
        name = "updatePaymentGroup",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Payment Group",
        defaultEntityName = "PaymentGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGroup {}

    /**
     * Delete a Payment Group
     */
    @Service(
        name = "deletePaymentGroup",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Payment Group",
        defaultEntityName = "PaymentGroup",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePaymentGroup {}

    /**
     * Check For Outgoing/Incoming Payment And Create Payment Group Member
     */
    @Service(
        name = "createPaymentGroupMember",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodServices.xml",
        invoke = "createPaymentGroupMember",
        description = "Check For Outgoing/Incoming Payment And Create Payment Group Member",
        defaultEntityName = "PaymentGroupMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        },
        overrideAttributes = {
            @OverrideAttribute(name = "fromDate", optional = "true")
        }
    )
    public interface CreatePaymentGroupMember {}

    /**
     * Update a Payment Group Member
     */
    @Service(
        name = "updatePaymentGroupMember",
        engine = "entity-auto",
        invoke = "update",
        description = "Update a Payment Group Member",
        defaultEntityName = "PaymentGroupMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk"),
            @EntityAttributes(mode = "IN", include = "nonpk", optional = "true")
        }
    )
    public interface UpdatePaymentGroupMember {}

    /**
     * Delete a Payment Group Member
     */
    @Service(
        name = "deletePaymentGroupMember",
        engine = "entity-auto",
        invoke = "delete",
        description = "Delete a Payment Group Member",
        defaultEntityName = "PaymentGroupMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface DeletePaymentGroupMember {}

    /**
     * expire a Payment Group Member
     */
    @Service(
        name = "expirePaymentGroupMember",
        engine = "simple",
        location = "component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodServices.xml",
        invoke = "expirePaymentGroupMember",
        description = "expire a Payment Group Member",
        defaultEntityName = "PaymentGroupMember",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(mode = "IN", include = "pk")
        }
    )
    public interface ExpirePaymentGroupMember {}

}

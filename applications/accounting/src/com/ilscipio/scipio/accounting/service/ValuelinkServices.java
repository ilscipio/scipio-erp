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
public class ValuelinkServices {

    /**
     * Gift Card Payment Processing
     */
    @Service(
        name = "valueLinkProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "giftCardProcessor",
        description = "Gift Card Payment Processing",
        auth = "true",
        implemented = {@Implements(service = "giftCardProcessInterface")}
    )
    public interface ValueLinkProcessor {}

    /**
     * Gift Card Release (reverse) Payment
     */
    @Service(
        name = "valueLinkRelease",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "giftCardRelease",
        description = "Gift Card Release (reverse) Payment",
        auth = "true",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface ValueLinkRelease {}

    /**
     * Gift Card Refund Payment
     */
    @Service(
        name = "valueLinkRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "giftCardRefund",
        description = "Gift Card Refund Payment",
        auth = "true",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface ValueLinkRefund {}

    /**
     * Gift Card Payment Processing
     */
    @Service(
        name = "valueLinkGcPurchase",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "giftCardPurchase",
        description = "Gift Card Payment Processing",
        auth = "true",
        implemented = {@Implements(service = "itemFulfillmentInterface")}
    )
    public interface ValueLinkGcPurchase {}

    /**
     * Gift Card Payment Processing
     */
    @Service(
        name = "valueLinkGcReload",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "giftCardReload",
        description = "Gift Card Payment Processing",
        auth = "true",
        implemented = {@Implements(service = "itemFulfillmentInterface")}
    )
    public interface ValueLinkGcReload {}

    /**
     * Create Public/Private Keys + KEK and Display Them
     */
    @Service(
        name = "createVLKeys",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "createKeys",
        description = "Create Public/Private Keys + KEK and Display Them",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "kekOnly", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "kekTest", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "output", type = "String", mode = "OUT")
        }
    )
    public interface CreateVLKeys {}

    /**
     * Test KEK Encryption
     */
    @Service(
        name = "testKekEncryption",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "testKekEncryption",
        description = "Test KEK Encryption",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "kekTest", type = "String", mode = "IN"),
            @Attribute(name = "mode", type = "Integer", mode = "IN"),
            @Attribute(name = "output", type = "String", mode = "OUT")
        }
    )
    public interface TestKekEncryption {}

    /**
     * Assign a new working key to ValueLink
     */
    @Service(
        name = "assignWorkingKey",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "assignWorkingKey",
        description = "Assign a new working key to ValueLink",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "desHexString", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface AssignWorkingKey {}

    /**
     * Activate (create new) gift card
     */
    @Service(
        name = "activateGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "activate",
        description = "Activate (create new) gift card",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "vlPromoCode", type = "String", mode = "IN"),
            @Attribute(name = "currency", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "authCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardNumber", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "pin", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardClass", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface ActivateGiftCard {}

    /**
     * Void a Activate (create new) gift card
     */
    @Service(
        name = "voidActivateGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "voidActivate",
        description = "Void a Activate (create new) gift card",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "currency", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "authCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardNumber", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "pin", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardClass", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface VoidActivateGiftCard {}

    /**
     * Redeem (take money off) gift card
     */
    @Service(
        name = "redeemGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "redeem",
        description = "Redeem (take money off) gift card",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pin", type = "String", mode = "IN"),
            @Attribute(name = "currency", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "previousAmount", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "authCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cashBack", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardClass", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface RedeemGiftCard {}

    /**
     * Void a Redeem (take money off) gift card
     */
    @Service(
        name = "voidRedeemGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "voidRedeem",
        description = "Void a Redeem (take money off) gift card",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pin", type = "String", mode = "IN"),
            @Attribute(name = "currency", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "previousAmount", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "authCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cashBack", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardClass", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface VoidRedeemGiftCard {}

    /**
     * Reload (add money to) gift card
     */
    @Service(
        name = "reloadGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "reload",
        description = "Reload (add money to) gift card",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pin", type = "String", mode = "IN"),
            @Attribute(name = "currency", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "previousAmount", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "authCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardClass", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface ReloadGiftCard {}

    /**
     * Void a Reload (add money to) gift card
     */
    @Service(
        name = "voidReloadGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "voidReload",
        description = "Void a Reload (add money to) gift card",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pin", type = "String", mode = "IN"),
            @Attribute(name = "currency", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "previousAmount", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "authCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardClass", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface VoidReloadGiftCard {}

    /**
     * Inquire current card balance
     */
    @Service(
        name = "balanceInquireGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "balanceInquire",
        description = "Inquire current card balance",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pin", type = "String", mode = "IN"),
            @Attribute(name = "currency", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "balance", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardClass", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface BalanceInquireGiftCard {}

    /**
     * Obtain card transaction history
     */
    @Service(
        name = "transactionHistoryGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "transactionHistory",
        description = "Obtain card transaction history",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pin", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "balance", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "history", type = "List", mode = "OUT", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardClass", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface TransactionHistoryGiftCard {}

    /**
     * Refund a gift card
     */
    @Service(
        name = "refundGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "refund",
        description = "Refund a gift card",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pin", type = "String", mode = "IN"),
            @Attribute(name = "currency", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "previousAmount", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "authCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardClass", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface RefundGiftCard {}

    /**
     * Refund a gift card
     */
    @Service(
        name = "voidRefundGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "voidRefund",
        description = "Refund a gift card",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pin", type = "String", mode = "IN"),
            @Attribute(name = "currency", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "INOUT"),
            @Attribute(name = "previousAmount", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "authCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardClass", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface VoidRefundGiftCard {}

    /**
     * Link a physical card to a virtual account
     */
    @Service(
        name = "linkPhysicalGiftCard",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "linkPhysicalCard",
        description = "Link a physical card to a virtual account",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "virtualCard", type = "String", mode = "IN"),
            @Attribute(name = "virtualPin", type = "String", mode = "IN"),
            @Attribute(name = "physicalCard", type = "String", mode = "IN"),
            @Attribute(name = "physicalPin", type = "String", mode = "IN"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "authCode", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "amount", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardClass", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface LinkPhysicalGiftCard {}

    /**
     * Disable a gift card PIN
     */
    @Service(
        name = "disableGiftCardPin",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "disablePin",
        description = "Disable a gift card PIN",
        auth = "true",
        attributes = {
            @Attribute(name = "paymentConfig", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "pin", type = "String", mode = "IN"),
            @Attribute(name = "orderId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "partyId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "processResult", type = "Boolean", mode = "OUT"),
            @Attribute(name = "responseCode", type = "String", mode = "OUT"),
            @Attribute(name = "balance", type = "BigDecimal", mode = "OUT", optional = "true"),
            @Attribute(name = "expireDate", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cardClass", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "referenceNum", type = "String", mode = "OUT")
        }
    )
    public interface DisableGiftCardPin {}

    @Service(
        name = "vlTimeOutReversal",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.valuelink.ValueLinkServices",
        invoke = "timeOutReversal",
        auth = "true",
        validate = "false"
    )
    public interface VlTimeOutReversal {}

}

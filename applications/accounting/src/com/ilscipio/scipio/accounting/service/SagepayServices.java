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
public class SagepayServices {

    /**
     * SagePay Payment Authorization Service
     */
    @Service(
        name = "sagepayCCAuth",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.sagepay.SagePayPaymentServices",
        invoke = "ccAuth",
        description = "SagePay Payment Authorization Service",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface SagepayCCAuth {}

    /**
     * SagePay Payment Capture Service
     */
    @Service(
        name = "sagepayCCCapture",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.sagepay.SagePayPaymentServices",
        invoke = "ccCapture",
        description = "SagePay Payment Capture Service",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface SagepayCCCapture {}

    /**
     * SagePay Payment Release
     */
    @Service(
        name = "sagepayCCRelease",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.sagepay.SagePayPaymentServices",
        invoke = "ccRelease",
        description = "SagePay Payment Release",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface SagepayCCRelease {}

    /**
     * SagePay Payment Refund Service
     */
    @Service(
        name = "sagepayCCRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.sagepay.SagePayPaymentServices",
        invoke = "ccRefund",
        description = "SagePay Payment Refund Service",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface SagepayCCRefund {}

    /**
     * For payment authentication
     */
    @Service(
        name = "SagePayPaymentAuthentication",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.sagepay.SagePayServices",
        invoke = "paymentAuthentication",
        description = "For payment authentication",
        attributes = {
            @Attribute(name = "paymentGatewayConfigId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "transactionType", type = "String", mode = "OUT"),
            @Attribute(name = "vendorTxCode", type = "String", mode = "INOUT"),
            @Attribute(name = "cardHolder", type = "String", mode = "IN"),
            @Attribute(name = "cardNumber", type = "String", mode = "IN"),
            @Attribute(name = "expiryDate", type = "String", mode = "IN"),
            @Attribute(name = "cardType", type = "String", mode = "IN"),
            @Attribute(name = "amount", type = "String", mode = "INOUT"),
            @Attribute(name = "currency", type = "String", mode = "IN"),
            @Attribute(name = "billingSurname", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billingFirstnames", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billingAddress", type = "String", mode = "IN"),
            @Attribute(name = "billingAddress2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billingCity", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billingPostCode", type = "String", mode = "IN"),
            @Attribute(name = "billingCountry", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billingState", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "billingPhone", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "isBillingSameAsDelivery", type = "Boolean", mode = "IN", optional = "true"),
            @Attribute(name = "deliverySurname", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deliveryFirstnames", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deliveryAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deliveryAddress2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deliveryCity", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deliveryPostCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deliveryCountry", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deliveryState", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "deliveryPhone", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "cv2", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "startDate", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "issueNumber", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "basket", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "description", type = "String", mode = "IN"),
            @Attribute(name = "clientIPAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "status", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "statusDetail", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "vpsTxId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "securityKey", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "txAuthNo", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "avsCv2", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "addressResult", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "postCodeResult", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cv2Result", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "cavv", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SagePayPaymentAuthentication {}

    /**
     * For capturing the payment
     */
    @Service(
        name = "SagePayPaymentAuthorisation",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.sagepay.SagePayServices",
        invoke = "paymentAuthorisation",
        description = "For capturing the payment",
        attributes = {
            @Attribute(name = "paymentGatewayConfigId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "vendorTxCode", type = "String", mode = "IN"),
            @Attribute(name = "vpsTxId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "securityKey", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "txAuthNo", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "amount", type = "String", mode = "IN"),
            @Attribute(name = "status", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "statusDetail", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SagePayPaymentAuthorisation {}

    /**
     * For releasing (cancel) the payment
     */
    @Service(
        name = "SagePayPaymentRelease",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.sagepay.SagePayServices",
        invoke = "paymentRelease",
        description = "For releasing (cancel) the payment",
        attributes = {
            @Attribute(name = "paymentGatewayConfigId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "vendorTxCode", type = "String", mode = "IN"),
            @Attribute(name = "vpsTxId", type = "String", mode = "IN"),
            @Attribute(name = "securityKey", type = "String", mode = "IN"),
            @Attribute(name = "txAuthNo", type = "String", mode = "IN"),
            @Attribute(name = "releaseAmount", type = "String", mode = "IN"),
            @Attribute(name = "status", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "statusDetail", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SagePayPaymentRelease {}

    /**
     * For voiding the payment
     */
    @Service(
        name = "SagePayPaymentVoid",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.sagepay.SagePayServices",
        invoke = "paymentVoid",
        description = "For voiding the payment",
        attributes = {
            @Attribute(name = "paymentGatewayConfigId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "vendorTxCode", type = "String", mode = "IN"),
            @Attribute(name = "vpsTxId", type = "String", mode = "IN"),
            @Attribute(name = "securityKey", type = "String", mode = "IN"),
            @Attribute(name = "txAuthNo", type = "String", mode = "IN"),
            @Attribute(name = "status", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "statusDetail", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SagePayPaymentVoid {}

    /**
     * For refunding the payment
     */
    @Service(
        name = "SagePayPaymentRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.sagepay.SagePayServices",
        invoke = "paymentRefund",
        description = "For refunding the payment",
        attributes = {
            @Attribute(name = "paymentGatewayConfigId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "vendorTxCode", type = "String", mode = "IN"),
            @Attribute(name = "amount", type = "String", mode = "IN"),
            @Attribute(name = "currency", type = "String", mode = "IN"),
            @Attribute(name = "description", type = "String", mode = "IN"),
            @Attribute(name = "relatedVPSTxId", type = "String", mode = "IN"),
            @Attribute(name = "relatedVendorTxCode", type = "String", mode = "IN"),
            @Attribute(name = "relatedSecurityKey", type = "String", mode = "IN"),
            @Attribute(name = "relatedTxAuthNo", type = "String", mode = "IN"),
            @Attribute(name = "status", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "statusDetail", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "vpsTxId", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "txAuthNo", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface SagePayPaymentRefund {}

}

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
public class VerisignServices {

    /**
     * Credit Card Processing
     */
    @Service(
        name = "payflowCCProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.verisign.PayflowPro",
        invoke = "ccProcessor",
        description = "Credit Card Processing",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface PayflowCCProcessor {}

    /**
     * Credit Card Capture
     */
    @Service(
        name = "payflowCCCapture",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.verisign.PayflowPro",
        invoke = "ccCapture",
        description = "Credit Card Capture",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface PayflowCCCapture {}

    /**
     * Credit Card Void
     */
    @Service(
        name = "payflowCCVoid",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.verisign.PayflowPro",
        invoke = "ccVoid",
        description = "Credit Card Void",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface PayflowCCVoid {}

    /**
     * Credit Card Refund
     */
    @Service(
        name = "payflowCCRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.verisign.PayflowPro",
        invoke = "ccRefund",
        description = "Credit Card Refund",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface PayflowCCRefund {}

    @Service(
        name = "payflowSetExpressCheckout",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.verisign.PayflowPro",
        invoke = "setExpressCheckout",
        implemented = {@Implements(service = "payPalSetExpressCheckoutInterface")}
    )
    public interface PayflowSetExpressCheckout {}

    @Service(
        name = "payflowGetExpressCheckout",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.verisign.PayflowPro",
        invoke = "getExpressCheckout",
        implemented = {@Implements(service = "payPalGetExpressCheckoutInterface")}
    )
    public interface PayflowGetExpressCheckout {}

    @Service(
        name = "payflowDoExpressCheckout",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.verisign.PayflowPro",
        invoke = "doExpressCheckout",
        implemented = {@Implements(service = "payPalDoExpressCheckoutInterface")}
    )
    public interface PayflowDoExpressCheckout {}

    @Service(
        name = "payflowPayPalProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.verisign.PayflowPro",
        invoke = "ccProcessor",
        implemented = {@Implements(service = "payPalProcessInterface")}
    )
    public interface PayflowPayPalProcessor {}

    @Service(
        name = "payflowPayPalCapture",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.verisign.PayflowPro",
        invoke = "ccCapture",
        implemented = {@Implements(service = "payPalCaptureInterface")}
    )
    public interface PayflowPayPalCapture {}

    /**
     * Credit Card Void
     */
    @Service(
        name = "payflowPayPalVoid",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.verisign.PayflowPro",
        invoke = "ccVoid",
        description = "Credit Card Void",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface PayflowPayPalVoid {}

    @Service(
        name = "payflowPayPalRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.verisign.PayflowPro",
        invoke = "ccRefund",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface PayflowPayPalRefund {}

}

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
public class PaypalServices {

    @Service(
        name = "payPalSetExpressCheckout",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.paypal.PayPalServices",
        invoke = "setExpressCheckout",
        implemented = {@Implements(service = "payPalSetExpressCheckoutInterface")}
    )
    public interface PayPalSetExpressCheckout {}

    @Service(
        name = "payPalGetExpressCheckout",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.paypal.PayPalServices",
        invoke = "getExpressCheckout",
        implemented = {@Implements(service = "payPalGetExpressCheckoutInterface")}
    )
    public interface PayPalGetExpressCheckout {}

    @Service(
        name = "payPalDoExpressCheckout",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.paypal.PayPalServices",
        invoke = "doExpressCheckout",
        implemented = {@Implements(service = "payPalDoExpressCheckoutInterface")}
    )
    public interface PayPalDoExpressCheckout {}

    @Service(
        name = "payPalCheckoutUpdate",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.paypal.PayPalServices",
        invoke = "payPalCheckoutUpdate",
        attributes = {
            @Attribute(name = "request", type = "javax.servlet.http.HttpServletRequest", mode = "IN"),
            @Attribute(name = "response", type = "javax.servlet.http.HttpServletResponse", mode = "IN")
        }
    )
    public interface PayPalCheckoutUpdate {}

    @Service(
        name = "payPalProcessor",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.paypal.PayPalServices",
        invoke = "doAuthorization",
        implemented = {@Implements(service = "payPalProcessInterface")}
    )
    public interface PayPalProcessor {}

    @Service(
        name = "payPalCapture",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.paypal.PayPalServices",
        invoke = "doCapture",
        implemented = {@Implements(service = "payPalCaptureInterface")}
    )
    public interface PayPalCapture {}

    /**
     * PayPal Order Payment Void
     */
    @Service(
        name = "payPalVoid",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.paypal.PayPalServices",
        invoke = "doVoid",
        description = "PayPal Order Payment Void",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface PayPalVoid {}

    @Service(
        name = "payPalRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.paypal.PayPalServices",
        invoke = "doRefund",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface PayPalRefund {}

}

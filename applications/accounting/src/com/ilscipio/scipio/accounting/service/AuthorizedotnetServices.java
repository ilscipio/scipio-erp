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
public class AuthorizedotnetServices {

    /**
     * Authorize.NET Payment Authorization
     */
    @Service(
        name = "aimCCAuth",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.authorizedotnet.AIMPaymentServices",
        invoke = "ccAuth",
        description = "Authorize.NET Payment Authorization",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface AimCCAuth {}

    /**
     * Authorize.NET Payment Authorize and Capture service
     */
    @Service(
        name = "aimCCAuthCapture",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.authorizedotnet.AIMPaymentServices",
        invoke = "ccAuthCapture",
        description = "Authorize.NET Payment Authorize and Capture service",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface AimCCAuthCapture {}

    /**
     * Authorize.NET Payment Capture Service
     */
    @Service(
        name = "aimCCCapture",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.authorizedotnet.AIMPaymentServices",
        invoke = "ccCapture",
        description = "Authorize.NET Payment Capture Service",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface AimCCCapture {}

    /**
     * Authorize.NET Payment Release Service - NOT IMPLEMENTED YET
     */
    @Service(
        name = "aimCCRelease",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.authorizedotnet.AIMPaymentServices",
        invoke = "ccRelease",
        description = "Authorize.NET Payment Release Service - NOT IMPLEMENTED YET",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface AimCCRelease {}

    /**
     * Authorize.NET Payment Refund Service
     */
    @Service(
        name = "aimCCRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.authorizedotnet.AIMPaymentServices",
        invoke = "ccRefund",
        description = "Authorize.NET Payment Refund Service",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface AimCCRefund {}

    /**
     * Authorize.NET Credit Service - NOT IMPLEMENTED YET
     */
    @Service(
        name = "aimCCCredit",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.authorizedotnet.AIMPaymentServices",
        invoke = "ccCredit",
        description = "Authorize.NET Credit Service - NOT IMPLEMENTED YET",
        implemented = {@Implements(service = "ccCreditInterface")}
    )
    public interface AimCCCredit {}

}

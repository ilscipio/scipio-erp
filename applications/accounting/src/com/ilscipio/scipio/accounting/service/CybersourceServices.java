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
public class CybersourceServices {

    /**
     * CyberSource Credit Card Authorization
     */
    @Service(
        name = "cyberSourceCCAuth",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.cybersource.IcsPaymentServices",
        invoke = "ccAuth",
        description = "CyberSource Credit Card Authorization",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface CyberSourceCCAuth {}

    /**
     * CyberSource Credit Card Capture
     */
    @Service(
        name = "cyberSourceCCCapture",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.cybersource.IcsPaymentServices",
        invoke = "ccCapture",
        description = "CyberSource Credit Card Capture",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface CyberSourceCCCapture {}

    /**
     * CyberSource Credit Card Credit
     */
    @Service(
        name = "cyberSourceCCRelease",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.cybersource.IcsPaymentServices",
        invoke = "ccRelease",
        description = "CyberSource Credit Card Credit",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface CyberSourceCCRelease {}

    /**
     * CyberSource Credit Card Refund
     */
    @Service(
        name = "cyberSourceCCRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.cybersource.IcsPaymentServices",
        invoke = "ccRefund",
        description = "CyberSource Credit Card Refund",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface CyberSourceCCRefund {}

    /**
     * CyberSource Credit Card Credit
     */
    @Service(
        name = "cyberSourceCCCredit",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.cybersource.IcsPaymentServices",
        invoke = "ccCredit",
        description = "CyberSource Credit Card Credit",
        implemented = {@Implements(service = "ccCreditInterface")}
    )
    public interface CyberSourceCCCredit {}

}

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
public class OrbitalServices {

    /**
     * Orbital Payment Authorization
     */
    @Service(
        name = "orbitalCCAuth",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.orbital.OrbitalPaymentServices",
        invoke = "ccAuth",
        description = "Orbital Payment Authorization",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface OrbitalCCAuth {}

    /**
     * Orbital Payment Authorize and Capture service
     */
    @Service(
        name = "orbitalCCAuthCapture",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.orbital.OrbitalPaymentServices",
        invoke = "ccAuthCapture",
        description = "Orbital Payment Authorize and Capture service",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface OrbitalCCAuthCapture {}

    /**
     * Orbital Payment Capture Service
     */
    @Service(
        name = "orbitalCCCapture",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.orbital.OrbitalPaymentServices",
        invoke = "ccCapture",
        description = "Orbital Payment Capture Service",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface OrbitalCCCapture {}

    /**
     * Orbital Payment Refund Service
     */
    @Service(
        name = "orbitalCCRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.orbital.OrbitalPaymentServices",
        invoke = "ccRefund",
        description = "Orbital Payment Refund Service",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface OrbitalCCRefund {}

    /**
     * Orbital Payment Release Service
     */
    @Service(
        name = "orbitalCCRelease",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.orbital.OrbitalPaymentServices",
        invoke = "ccRelease",
        description = "Orbital Payment Release Service",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface OrbitalCCRelease {}

}

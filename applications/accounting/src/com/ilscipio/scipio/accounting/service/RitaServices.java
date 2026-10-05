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
public class RitaServices {

    /**
     * RiTA Credit Card Pre-Authorization/Sale
     */
    @Service(
        name = "ritaCCAuth",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.gosoftware.RitaServices",
        invoke = "ccAuth",
        description = "RiTA Credit Card Pre-Authorization/Sale",
        export = "true",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface RitaCCAuth {}

    /**
     * RiTA Credit Card Post-Authorization
     */
    @Service(
        name = "ritaCCCapture",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.gosoftware.RitaServices",
        invoke = "ccCapture",
        description = "RiTA Credit Card Post-Authorization",
        export = "true",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface RitaCCCapture {}

    /**
     * RiTA Credit Card Void
     */
    @Service(
        name = "ritaCCRelease",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.gosoftware.RitaServices",
        invoke = "ccVoidRelease",
        description = "RiTA Credit Card Void",
        export = "true",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface RitaCCRelease {}

    /**
     * RiTA Credit Card Refund - Main Service
     */
    @Service(
        name = "ritaCCRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.gosoftware.RitaServices",
        invoke = "ccRefund",
        description = "RiTA Credit Card Refund - Main Service",
        export = "true",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface RitaCCRefund {}

    /**
     * RiTA Credit Card Void Refund - Called from ritaCCRefund
     */
    @Service(
        name = "ritaCCVoidRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.gosoftware.RitaServices",
        invoke = "ccVoidRefund",
        description = "RiTA Credit Card Void Refund - Called from ritaCCRefund",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface RitaCCVoidRefund {}

    /**
     * RiTA Credit Card Credit Refund - Called from ritaCCRefund
     */
    @Service(
        name = "ritaCCCreditRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.gosoftware.RitaServices",
        invoke = "ccCreditRefund",
        description = "RiTA Credit Card Credit Refund - Called from ritaCCRefund",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface RitaCCCreditRefund {}

    /**
     * RiTA Credit Card Pre-Authorization/Sale
     */
    @Service(
        name = "ritaCCAuthRemote",
        engine = "rmi",
        location = "rita-rmi",
        invoke = "ritaCCAuth",
        description = "RiTA Credit Card Pre-Authorization/Sale",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface RitaCCAuthRemote {}

    /**
     * RiTA Credit Card Post-Authorization
     */
    @Service(
        name = "ritaCCCaptureRemote",
        engine = "rmi",
        location = "rita-rmi",
        invoke = "ritaCCCapture",
        description = "RiTA Credit Card Post-Authorization",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface RitaCCCaptureRemote {}

    /**
     * RiTA Credit Card Void
     */
    @Service(
        name = "ritaCCReleaseRemote",
        engine = "rmi",
        location = "rita-rmi",
        invoke = "ritaCCRelease",
        description = "RiTA Credit Card Void",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface RitaCCReleaseRemote {}

    /**
     * RiTA Credit Card Refund
     */
    @Service(
        name = "ritaCCRefundRemote",
        engine = "rmi",
        location = "rita-rmi",
        invoke = "ritaCCRefund",
        description = "RiTA Credit Card Refund",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface RitaCCRefundRemote {}

}

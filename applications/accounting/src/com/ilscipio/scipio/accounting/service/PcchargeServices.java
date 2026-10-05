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
public class PcchargeServices {

    /**
     * PCCharge Credit Card Pre-Authorization/Sale
     */
    @Service(
        name = "pcChargeCCAuth",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.gosoftware.PcChargeServices",
        invoke = "ccAuth",
        description = "PCCharge Credit Card Pre-Authorization/Sale",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface PcChargeCCAuth {}

    /**
     * PCCharge Credit Card Post-Authorization
     */
    @Service(
        name = "pcChargeCCCapture",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.gosoftware.PcChargeServices",
        invoke = "ccCapture",
        description = "PCCharge Credit Card Post-Authorization",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface PcChargeCCCapture {}

    /**
     * PCCharge Credit Card Void
     */
    @Service(
        name = "pcChargeCCRelease",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.gosoftware.PcChargeServices",
        invoke = "ccRelease",
        description = "PCCharge Credit Card Void",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface PcChargeCCRelease {}

    /**
     * PCCharge Credit Card Refund
     */
    @Service(
        name = "pcChargeCCRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.gosoftware.PcChargeServices",
        invoke = "ccRefund",
        description = "PCCharge Credit Card Refund",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface PcChargeCCRefund {}

}

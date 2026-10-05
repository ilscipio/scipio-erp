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
public class ClearcommerceServices {

    /**
     * ClearCommerce Credit Card Authorization
     */
    @Service(
        name = "clearCommerceCCAuth",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.clearcommerce.CCPaymentServices",
        invoke = "ccAuth",
        description = "ClearCommerce Credit Card Authorization",
        implemented = {@Implements(service = "ccAuthInterface")},
        attributes = {
            @Attribute(name = "ccAction", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ClearCommerceCCAuth {}

    /**
     * ClearCommerce Credit Card Capture
     */
    @Service(
        name = "clearCommerceCCCapture",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.clearcommerce.CCPaymentServices",
        invoke = "ccCapture",
        description = "ClearCommerce Credit Card Capture",
        implemented = {@Implements(service = "ccCaptureInterface")}
    )
    public interface ClearCommerceCCCapture {}

    /**
     * ClearCommerce Credit Card Release
     */
    @Service(
        name = "clearCommerceCCRelease",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.clearcommerce.CCPaymentServices",
        invoke = "ccRelease",
        description = "ClearCommerce Credit Card Release",
        implemented = {@Implements(service = "paymentReleaseInterface")}
    )
    public interface ClearCommerceCCRelease {}

    /**
     * ClearCommerce Credit Card Refund
     */
    @Service(
        name = "clearCommerceCCRefund",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.clearcommerce.CCPaymentServices",
        invoke = "ccRefund",
        description = "ClearCommerce Credit Card Refund",
        implemented = {@Implements(service = "paymentRefundInterface")}
    )
    public interface ClearCommerceCCRefund {}

    /**
     * ClearCommerce Credit Card Credit
     */
    @Service(
        name = "clearCommerceCCCredit",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.clearcommerce.CCPaymentServices",
        invoke = "ccCredit",
        description = "ClearCommerce Credit Card Credit",
        implemented = {@Implements(service = "ccCreditInterface")},
        attributes = {
            @Attribute(name = "orderId", type = "String", mode = "IN"),
            @Attribute(name = "pbOrder", type = "Map", mode = "IN", optional = "true"),
            @Attribute(name = "OrderFrequencyCycle", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "OrderFrequencyInterval", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "TotalNumberPayments", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface ClearCommerceCCCredit {}

    /**
     * Reporting facility
     */
    @Service(
        name = "clearCommerceCCReport",
        engine = "java",
        location = "org.ofbiz.accounting.thirdparty.clearcommerce.CCPaymentServices",
        invoke = "ccReport",
        description = "Reporting facility",
        implemented = {@Implements(service = "ccAuthInterface")}
    )
    public interface ClearCommerceCCReport {}

}

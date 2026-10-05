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
package com.ilscipio.scipio.product.service;

import com.ilscipio.scipio.service.def.*;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class Shipment_dhlServices {

    /**
     * DHL ShipIt Register Account inquire tool
     */
    @Service(
        name = "dhlRegisterAccount",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.dhl.DhlServices",
        invoke = "dhlRegisterInquire",
        description = "DHL ShipIt Register Account inquire tool",
        attributes = {
            @Attribute(name = "postalCode", type = "String", mode = "IN"),
            @Attribute(name = "shipmentGatewayConfigId", type = "String", mode = "IN"),
            @Attribute(name = "configProps", type = "String", mode = "IN"),
            @Attribute(name = "shippingKey", type = "String", mode = "OUT")
        }
    )
    public interface DhlRegisterAccount {}

    /**
     * DHL ShipIt rate inquire tool
     */
    @Service(
        name = "dhlRateEstimate",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.dhl.DhlServices",
        invoke = "dhlRateEstimate",
        description = "DHL ShipIt rate inquire tool",
        implemented = {@Implements(service = "calcShipmentEstimateInterface")},
        attributes = {
            @Attribute(name = "dhlRateCodeMap", type = "Map", mode = "OUT")
        }
    )
    public interface DhlRateEstimate {}

    /**
     * DHL Shipment Confirm
     */
    @Service(
        name = "dhlShipmentConfirm",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.dhl.DhlServices",
        invoke = "dhlShipmentConfirm",
        description = "DHL Shipment Confirm",
        auth = "true",
        maxRetry = "3",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentRouteSegment", mode = "IN", include = "pk")
        }
    )
    public interface DhlShipmentConfirm {}

}

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
public class Shipment_uspsServices {

    @Service(
        name = "uspsRateInquire",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.usps.UspsServices",
        invoke = "uspsRateInquire",
        implemented = {@Implements(service = "calcShipmentEstimateInterface")}
    )
    public interface UspsRateInquire {}

    @Service(
        name = "uspsInternationalRateInquire",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.usps.UspsServices",
        invoke = "uspsInternationalRateInquire",
        implemented = {@Implements(service = "calcShipmentEstimateInterface")}
    )
    public interface UspsInternationalRateInquire {}

    @Service(
        name = "uspsTrackConfirm",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.usps.UspsServices",
        invoke = "uspsTrackConfirm",
        attributes = {
            @Attribute(name = "trackingId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentGatewayConfigId", type = "String", mode = "IN"),
            @Attribute(name = "configProps", type = "String", mode = "IN"),
            @Attribute(name = "trackingSummary", type = "String", mode = "OUT"),
            @Attribute(name = "trackingDetailList", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface UspsTrackConfirm {}

    @Service(
        name = "uspsAddressValidation",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.usps.UspsServices",
        invoke = "uspsAddressValidation",
        attributes = {
            @Attribute(name = "firmName", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "address1", type = "String", mode = "INOUT"),
            @Attribute(name = "address2", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "city", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "state", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "zip5", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "zip4", type = "String", mode = "INOUT", optional = "true"),
            @Attribute(name = "shipmentGatewayConfigId", type = "String", mode = "IN"),
            @Attribute(name = "configProps", type = "String", mode = "IN"),
            @Attribute(name = "returnText", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface UspsAddressValidation {}

    @Service(
        name = "uspsCityStateLookup",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.usps.UspsServices",
        invoke = "uspsCityStateLookup",
        attributes = {
            @Attribute(name = "zip5", type = "String", mode = "IN"),
            @Attribute(name = "shipmentGatewayConfigId", type = "String", mode = "IN"),
            @Attribute(name = "configProps", type = "String", mode = "IN"),
            @Attribute(name = "city", type = "String", mode = "OUT"),
            @Attribute(name = "state", type = "String", mode = "OUT")
        }
    )
    public interface UspsCityStateLookup {}

    @Service(
        name = "uspsPriorityMailStandard",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.usps.UspsServices",
        invoke = "uspsPriorityMailStandard",
        attributes = {
            @Attribute(name = "originZip", type = "String", mode = "IN"),
            @Attribute(name = "destinationZip", type = "String", mode = "IN"),
            @Attribute(name = "shipmentGatewayConfigId", type = "String", mode = "IN"),
            @Attribute(name = "configProps", type = "String", mode = "IN"),
            @Attribute(name = "days", type = "String", mode = "OUT")
        }
    )
    public interface UspsPriorityMailStandard {}

    @Service(
        name = "uspsPackageServicesStandard",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.usps.UspsServices",
        invoke = "uspsPackageServicesStandard",
        attributes = {
            @Attribute(name = "originZip", type = "String", mode = "IN"),
            @Attribute(name = "destinationZip", type = "String", mode = "IN"),
            @Attribute(name = "shipmentGatewayConfigId", type = "String", mode = "IN"),
            @Attribute(name = "configProps", type = "String", mode = "IN"),
            @Attribute(name = "days", type = "String", mode = "OUT")
        }
    )
    public interface UspsPackageServicesStandard {}

    @Service(
        name = "uspsDomesticRate",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.usps.UspsServices",
        invoke = "uspsDomesticRate",
        attributes = {
            @Attribute(name = "service", type = "String", mode = "IN"),
            @Attribute(name = "originZip", type = "String", mode = "IN"),
            @Attribute(name = "destinationZip", type = "String", mode = "IN"),
            @Attribute(name = "pounds", type = "String", mode = "IN"),
            @Attribute(name = "ounces", type = "String", mode = "IN"),
            @Attribute(name = "container", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "size", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "machinable", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentGatewayConfigId", type = "String", mode = "IN"),
            @Attribute(name = "configProps", type = "String", mode = "IN"),
            @Attribute(name = "zone", type = "String", mode = "OUT"),
            @Attribute(name = "postage", type = "String", mode = "OUT"),
            @Attribute(name = "restrictionCodes", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "restrictionDesc", type = "String", mode = "OUT", optional = "true")
        }
    )
    public interface UspsDomesticRate {}

    @Service(
        name = "uspsUpdateShipmentRateInfo",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.usps.UspsServices",
        invoke = "uspsUpdateShipmentRateInfo",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentRouteSegment", mode = "IN", include = "pk")
        }
    )
    public interface UspsUpdateShipmentRateInfo {}

    @Service(
        name = "uspsDeliveryConfirmation",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.usps.UspsServices",
        invoke = "uspsDeliveryConfirmation",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentRouteSegment", mode = "IN", include = "pk")
        }
    )
    public interface UspsDeliveryConfirmation {}

}

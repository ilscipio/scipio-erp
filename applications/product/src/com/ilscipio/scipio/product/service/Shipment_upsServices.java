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
public class Shipment_upsServices {

    /**
     * UPS On-Line rate inquire tool.  Also supports rate shopping by setting upsRateInquireMode to 'Shop', and upsRateCodeMap                 will return a Map of serviceCode -> rate
     */
    @Service(
        name = "upsRateEstimate",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.ups.UpsServices",
        invoke = "upsRateInquire",
        description = "UPS On-Line rate inquire tool.  Also supports rate shopping by setting upsRateInquireMode to 'Shop', and upsRateCodeMap\n                will return a Map of serviceCode -> rate",
        implemented = {@Implements(service = "calcShipmentEstimateInterface")},
        attributes = {
            @Attribute(name = "upsRateInquireMode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "packageWeights", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "upsRateCodeMap", type = "Map", mode = "OUT")
        }
    )
    public interface UpsRateEstimate {}

    /**
     * UPS Shipment Confirm
     */
    @Service(
        name = "upsShipmentConfirm",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.ups.UpsServices",
        invoke = "upsShipmentConfirm",
        description = "UPS Shipment Confirm",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentRouteSegment", mode = "IN", include = "pk")
        }
    )
    public interface UpsShipmentConfirm {}

    /**
     * UPS Shipment Accept
     */
    @Service(
        name = "upsShipmentAccept",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.ups.UpsServices",
        invoke = "upsShipmentAccept",
        description = "UPS Shipment Accept",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentRouteSegment", mode = "IN", include = "pk")
        }
    )
    public interface UpsShipmentAccept {}

    /**
     * UPS Void Shipment
     */
    @Service(
        name = "upsVoidShipment",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.ups.UpsServices",
        invoke = "upsVoidShipment",
        description = "UPS Void Shipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentRouteSegment", mode = "IN", include = "pk")
        }
    )
    public interface UpsVoidShipment {}

    /**
     * UPS Track Shipment
     */
    @Service(
        name = "upsTrackShipment",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.ups.UpsServices",
        invoke = "upsTrackShipment",
        description = "UPS Track Shipment",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentRouteSegment", mode = "IN", include = "pk")
        }
    )
    public interface UpsTrackShipment {}

    /**
     * Email UPS Retrun Label
     */
    @Service(
        name = "upsEmailReturnLabel",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.ups.UpsServices",
        invoke = "upsEmailReturnLabel",
        description = "Email UPS Retrun Label",
        auth = "true",
        entityAttributes = {
            @EntityAttributes(entityName = "ShipmentRouteSegment", mode = "IN", include = "pk")
        }
    )
    public interface UpsEmailReturnLabel {}

    /**
     * UPS On-Line rate inquire tool.  Also supports rate shopping by setting upsRateInquireMode to 'Shop', and upsRateCodeMap             will return a Map of serviceCode -> rate
     */
    @Service(
        name = "upsRateEstimateByPostalCode",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.ups.UpsServices",
        invoke = "upsRateInquireByPostalCode",
        description = "UPS On-Line rate inquire tool.  Also supports rate shopping by setting upsRateInquireMode to 'Shop', and upsRateCodeMap\n            will return a Map of serviceCode -> rate",
        attributes = {
            @Attribute(name = "serviceConfigProps", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "initialEstimateAmt", type = "BigDecimal", mode = "IN", optional = "true"),
            @Attribute(name = "shippingPostalCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentMethodTypeId", type = "String", mode = "IN"),
            @Attribute(name = "carrierPartyId", type = "String", mode = "IN"),
            @Attribute(name = "carrierRoleTypeId", type = "String", mode = "IN"),
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "packageWeights", type = "List", mode = "IN", optional = "true"),
            @Attribute(name = "shippableItemInfo", type = "List", mode = "IN"),
            @Attribute(name = "shippableWeight", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "shippableQuantity", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "shippableTotal", type = "BigDecimal", mode = "IN"),
            @Attribute(name = "shippingEstimateAmount", type = "BigDecimal", mode = "OUT"),
            @Attribute(name = "upsRateInquireMode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "upsRateCodeMap", type = "Map", mode = "OUT"),
            @Attribute(name = "isResidentialAddress", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shippingCountryCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shipFromAddress", type = "org.ofbiz.entity.GenericValue", mode = "IN", optional = "true"),
            @Attribute(name = "shipmentGatewayConfigId", type = "String", mode = "IN", optional = "true")
        }
    )
    public interface UpsRateEstimateByPostalCode {}

    @Service(
        name = "upsAddressValidation",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.ups.UpsServices",
        invoke = "upsAddressValidation",
        attributes = {
            @Attribute(name = "city", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "stateProvinceGeoId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "postalCode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "matches", type = "List", mode = "OUT", optional = "true")
        }
    )
    public interface UpsAddressValidation {}

    /**
     * UPS On-Line rate inquire tool. Supports rate shopping where  upsRateInquireMode is set to 'Shop', and shippingRates                  will return a List of Maps, of serviceCode -> rate for the shipping methods which are configured in ProductStoreShipmentMeth entity
     */
    @Service(
        name = "upsShipmentAlternateRatesEstimate",
        engine = "java",
        location = "org.ofbiz.shipment.thirdparty.ups.UpsServices",
        invoke = "upsShipmentAlternateRatesInquiry",
        description = "UPS On-Line rate inquire tool. Supports rate shopping where  upsRateInquireMode is set to 'Shop', and shippingRates \n                will return a List of Maps, of serviceCode -> rate for the shipping methods which are configured in ProductStoreShipmentMeth entity",
        attributes = {
            @Attribute(name = "productStoreId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentId", type = "String", mode = "IN"),
            @Attribute(name = "shipmentRouteSegmentId", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "shippingRates", type = "List", mode = "OUT")
        }
    )
    public interface UpsShipmentAlternateRatesEstimate {}

}

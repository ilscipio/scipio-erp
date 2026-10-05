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
package com.ilscipio.scipio.common.service;

import com.ilscipio.scipio.service.def.*;
import com.ilscipio.scipio.service.def.Service.GroupInvoke;

/**
 * Auto-generated annotation-based service definitions.
 *
 * <p>Generated from services.xml by convertXmlToAnnotation Gradle task.</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CdyneServices {

    /**
     * Use the CdyneReturnCityState service to fill in the County on a PostalAddress. Can be called as with a SECA rule.
     */
    @Service(
        name = "cdynePostalAddressFillInCounty",
        engine = "simple",
        location = "component://common/script/org/ofbiz/common/CdyneServices.xml",
        invoke = "cdynePostalAddressFillInCounty",
        description = "Use the CdyneReturnCityState service to fill in the County on a PostalAddress. Can be called as with a SECA rule.",
        attributes = {
            @Attribute(name = "contactMechId", type = "String", mode = "IN")
        }
    )
    public interface CdynePostalAddressFillInCounty {}

    /**
     * CDyne ReturnCityState
     */
    @Service(
        name = "cdyneReturnCityState",
        location = "org.ofbiz.common.CdyneServices",
        invoke = "cdyneReturnCityState",
        description = "CDyne ReturnCityState",
        attributes = {
            @Attribute(name = "zipcode", type = "String", mode = "IN", optional = "true"),
            @Attribute(name = "ServiceError", type = "String", mode = "OUT"),
            @Attribute(name = "AddressError", type = "String", mode = "OUT"),
            @Attribute(name = "AddressFoundBeMoreSpecific", type = "String", mode = "OUT"),
            @Attribute(name = "NeededCorrection", type = "String", mode = "OUT"),
            @Attribute(name = "DeliveryAddress", type = "String", mode = "OUT"),
            @Attribute(name = "City", type = "String", mode = "OUT"),
            @Attribute(name = "StateAbbrev", type = "String", mode = "OUT"),
            @Attribute(name = "ZipCode", type = "String", mode = "OUT"),
            @Attribute(name = "County", type = "String", mode = "OUT"),
            @Attribute(name = "CountyNum", type = "String", mode = "OUT"),
            @Attribute(name = "PreferredCityName", type = "String", mode = "OUT"),
            @Attribute(name = "DeliveryPoint", type = "String", mode = "OUT"),
            @Attribute(name = "CheckDigit", type = "String", mode = "OUT"),
            @Attribute(name = "CSKey", type = "String", mode = "OUT"),
            @Attribute(name = "FIPS", type = "String", mode = "OUT"),
            @Attribute(name = "FromLongitude", type = "String", mode = "OUT"),
            @Attribute(name = "FromLatitude", type = "String", mode = "OUT"),
            @Attribute(name = "ToLongitude", type = "String", mode = "OUT"),
            @Attribute(name = "ToLatitude", type = "String", mode = "OUT"),
            @Attribute(name = "AvgLongitude", type = "String", mode = "OUT"),
            @Attribute(name = "AvgLatitude", type = "String", mode = "OUT"),
            @Attribute(name = "CMSA", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "PMSA", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "MSA", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "MA", type = "String", mode = "OUT", optional = "true"),
            @Attribute(name = "TimeZone", type = "String", mode = "OUT"),
            @Attribute(name = "hasDaylightSavings", type = "String", mode = "OUT"),
            @Attribute(name = "AreaCode", type = "String", mode = "OUT"),
            @Attribute(name = "LLCertainty", type = "String", mode = "OUT"),
            @Attribute(name = "CensusBlockNum", type = "String", mode = "OUT"),
            @Attribute(name = "CensusTractNum", type = "String", mode = "OUT")
        }
    )
    public interface CdyneReturnCityState {}

}

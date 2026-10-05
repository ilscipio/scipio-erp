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


import org.ofbiz.base.util.*

htmlString = new StringBuffer();

if (destinationPostalAddress != null){
    contactMechId = destinationPostalAddress.contactMechId
    attnName = destinationPostalAddress.attnName
    toName = destinationPostalAddress.toName
    address1 = destinationPostalAddress.address1
    address2 = destinationPostalAddress.address2
    postalCode = destinationPostalAddress.postalCode
    city = destinationPostalAddress.city
    stateProvinceGeoId = destinationPostalAddress.stateProvinceGeoId
    countryGeoId = destinationPostalAddress.countryGeoId
    geoPointId = destinationPostalAddress
    
    if(contactMechId){
         htmlString.append(contactMechId + "<br/>")
    }
    if(toName){
        htmlString.append("To: " + toName + "<br/>")
    }
    if(attnName){
        htmlString.append("Attn: " + attnName + "<br/>")
    }
     if(address1){
        htmlString.append(address1 + "<br/>")
    }
    if(address2){
        htmlString.append(address2 + "<br/>")
    }
    if(city){
        htmlString.append(city + "<br/>")
    }
    if(stateProvinceGeoId){
        htmlString.append(stateProvinceGeoId + "<br/>")
    }
    if(countryGeoId){
        htmlString.append(countryGeoId + "<br/>")
    }
}

 context.destAddressFormatted = htmlString
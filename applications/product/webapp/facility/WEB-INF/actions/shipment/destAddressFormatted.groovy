/*
* Licensed to the Apache Software Foundation (ASF) under one
* or more contributor license agreements.  See the NOTICE file
* distributed with this work for additional information
* regarding copyright ownership.  The ASF licenses this file
* to you under the Apache License, Version 2.0 (the
* "License"); you may not use this file except in compliance
* with the License.  You may obtain a copy of the License at
*
* http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing,
* software distributed under the License is distributed on an
* "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
* KIND, either express or implied.  See the License for the
* specific language governing permissions and limitations
* under the License.
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
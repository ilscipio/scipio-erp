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

import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupFacility.groovy";

facilityData = context.facilityData ?: [:];
organizationData = context.setupStepStates?.organization.stepData ?: [:];

facilityInfo = null; // contains ALL facility record information
defaultParams = [:];

facilityId = null;
facility = facilityData.facility;

context.facility = facility;
if (facility) {
    facilityId = facility.facilityId;
    facilityType = facility.getRelatedOne("FacilityType", false);
    context.facilityType = facilityType;
    facilityInfo = new HashMap(facility);
}
context.facilityId = facilityId;

makeShipAddressMap = { postalAddress, contactMech, includeId ->
    def prefix = "shipAddress_";
    def res = [
        (prefix+"toName"): postalAddress.toName,
        (prefix+"attnName"): postalAddress.attnName,
        (prefix+"stateProvinceGeoId"): postalAddress.stateProvinceGeoId,
        (prefix+"countryGeoId"): postalAddress.countryGeoId,
        (prefix+"address1"): postalAddress.address1,
        (prefix+"address2"): postalAddress.address2,
        (prefix+"city"): postalAddress.city,
        (prefix+"postalCode"): postalAddress.postalCode
    ];
    if (includeId) {
        res[prefix+"contactMechId"] = contactMech?.contactMechId;
    }
    return res;
};


shipAddressContactMech = facilityData.shipAddressContactMech;
context.shipAddressContactMech = shipAddressContactMech;
context.shipAddressContactMechPurposes = facilityData.shipAddressContactMechPurposes;
context.shipAddressStandaloneCompleted = facilityData.shipAddressStandaloneCompleted;
context.locationAddressesCompleted = facilityData.locationAddressesCompleted;
context.locationPurposes = facilityData.locationPurposes;
shipPostalAddress = null;
if (shipAddressContactMech) {
    postalAddress = delegator.findOne("PostalAddress", [contactMechId:shipAddressContactMech.contactMechId], false);
    if (postalAddress) {
        shipPostalAddress = makeShipAddressMap(postalAddress, shipAddressContactMech, true);
    } else {
        Debug.logError("Setup: Configuration error: Facility ship address contact mech '"
            + shipAddressContactMech.contactMechId + " has no PostalAddress record! Invalid data configuration!", module)
    }
}
//context.shipPostalAddress = shipPostalAddress;
if (shipPostalAddress != null && facilityInfo != null) {
    facilityInfo.putAll(shipPostalAddress);
}

if (facility != null) {
    Debug.logInfo("Setup: Setting up existing warehouse '" + facilityId + "'", module); // ; info: " + facilityInfo
} else {
    Debug.logInfo("Setup: Setting up new warehouse", module);
}

getSrcPostalAddresses = { contactMechPurposeList, generalAddressContactMechId, generalAddressContactMechPurposes ->
    def srcContactMechPurposeMap = [:];
    def srcPostalAddressList = [];
    
    def shipAddressList = [];
    def genAddressList = [];
    def restList = [];
    for(cmp in contactMechPurposeList) {
        def contactMechId = cmp.contactMechId;
        def postalAddress = delegator.findOne("PostalAddress", [contactMechId:contactMechId], false);
        if (postalAddress) {
            def purpose = cmp.contactMechPurposeTypeId;
            if (contactMechId == generalAddressContactMechId) {
                genAddressList.add(postalAddress);
            } else if (purpose == "SHIPPING_LOCATION" || purpose == "SHIP_ORIG_LOCATION") {
                shipAddressList.add(postalAddress);
            } else {
                restList.add(postalAddress);
            }
            addrSet = srcContactMechPurposeMap[contactMechId];
            if (addrSet == null) {
                addrSet = new HashSet();
                srcContactMechPurposeMap[contactMechId] = addrSet;
            }
            addrSet.add(purpose);
        }
    }
    
    if (generalAddressContactMechPurposes?.contains("SHIPPING_LOCATION") || generalAddressContactMechPurposes?.contains("SHIP_ORIG_LOCATION")) {
        srcPostalAddressList.addAll(genAddressList);
        srcPostalAddressList.addAll(shipAddressList);
    } else {
        srcPostalAddressList.addAll(shipAddressList);
        srcPostalAddressList.addAll(genAddressList);
    }
    srcPostalAddressList.addAll(restList);
    
    // now make unique
    addressMap = new LinkedHashMap();
    for(postalAddress in srcPostalAddressList) {
        addressMap[postalAddress.contactMechId] = postalAddress;
    }
    return [srcPostalAddressList:new ArrayList(addressMap.values()), srcContactMechPurposeMap:srcContactMechPurposeMap];
};


srcPostalAddressList = [];
srcPostalAddress = null;
srcContactMechPurposeMap = [:];

// add facility addresses first
facilityContactMechPurposeList = facilityData.facilityContactMechPurposeList;
if (facilityContactMechPurposeList) {
    shipAddressContactMechId = facilityData.shipAddressContactMech?.contactMechId;
    shipAddressContactMechPurposes = facilityData.shipAddressContactMechPurposes ?: [];
    
    def srcPostalAddressInfo = getSrcPostalAddresses(facilityContactMechPurposeList, shipAddressContactMechId, shipAddressContactMechPurposes);
    srcPostalAddressList.addAll(srcPostalAddressInfo.srcPostalAddressList);
    srcContactMechPurposeMap.putAll(srcPostalAddressInfo.srcContactMechPurposeMap);
}

partyContactMechPurposeList = organizationData.partyContactMechPurposeList;
// pick out SHIPPING_LOCATION and SHIP_ORIG_LOCATIONs first, followed by general address, then the rest
if (partyContactMechPurposeList) {
    generalAddressContactMechId = organizationData.generalAddressContactMech?.contactMechId;
    generalAddressContactMechPurposes = organizationData.generalAddressContactMechPurposes ?: [];
    
    def srcPostalAddressInfo = getSrcPostalAddresses(partyContactMechPurposeList, generalAddressContactMechId, generalAddressContactMechPurposes);
    srcPostalAddressList.addAll(srcPostalAddressInfo.srcPostalAddressList);
    srcContactMechPurposeMap.putAll(srcPostalAddressInfo.srcContactMechPurposeMap);
}

if (srcPostalAddressList) {
    srcPostalAddress = srcPostalAddressList[0];
}

context.srcPostalAddressList = srcPostalAddressList;
context.srcPostalAddress = srcPostalAddress;
context.srcContactMechPurposeMap = srcContactMechPurposeMap;

defaultShipAddress = null;
if (srcPostalAddress) {
    defaultShipAddress = makeShipAddressMap(srcPostalAddress, srcPostalAddress.getRelatedOne("ContactMech", false), false);
    defaultParams.putAll(defaultShipAddress);
}
context.defaultShipAddress = defaultShipAddress;

context.facilityInfo = facilityInfo;
context.defaultParams = defaultParams;

context.facilitySubmitOk = (facility != null); // || (srcPostalAddress)

defaultDefaultWeightUomId = UtilProperties.getPropertyValue("scipiosetup", "facility.defaultWeightUomId");
context.defaultDefaultWeightUomId = defaultDefaultWeightUomId;


// NOTE: some of these below from EditFacility.groovy

//Facility types
facilityTypes = from("FacilityType").cache(true).queryList();
if (facilityTypes) {
    context.facilityTypes = facilityTypes;
}

// all possible inventory item types
context.inventoryItemTypes = from("InventoryItemType").orderBy("description").cache(true).queryList();

// weight unit of measures
context.weightUomList = from("Uom").where("uomTypeId", "WEIGHT_MEASURE").cache(true).queryList();

// area unit of measures
context.areaUomList = from("Uom").where("uomTypeId", "AREA_MEASURE").cache(true).queryList();

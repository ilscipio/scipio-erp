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
    Debug.logInfo("Setup: Setting up existing warehouse '" + facilityId + "'", module);
} else {
    Debug.logInfo("Setup: Setting up new warehouse", module);
}

partyAddressContactMech = context.setupStepStates?.organization.stepData.generalAddressContactMech;
partyPostalAddress = null;
if (partyAddressContactMech) {
    partyPostalAddress = delegator.findOne("PostalAddress", [contactMechId:partyAddressContactMech.contactMechId], false);
}
if (partyPostalAddress == null) {
    // fallback (legacy ofbiz behavior)
    listPartyPostalAddress = delegator.findByAnd("PartyAndPostalAddress", [partyId: context.partyId], null, false);
    partyPostalAddress = EntityUtil.getFirst(EntityUtil.filterByDate(listPartyPostalAddress));
}
context.partyPostalAddress = partyPostalAddress;

defaultShipAddress = null;
if (partyPostalAddress) {
    defaultShipAddress = makeShipAddressMap(partyPostalAddress, partyPostalAddress.getRelatedOne("ContactMech", false), false);
    defaultParams.putAll(defaultShipAddress);
}
context.defaultShipAddress = defaultShipAddress;

context.facilityInfo = facilityInfo;
context.defaultParams = defaultParams;

context.facilitySubmitOk = (facility != null); // || (partyPostalAddress)

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

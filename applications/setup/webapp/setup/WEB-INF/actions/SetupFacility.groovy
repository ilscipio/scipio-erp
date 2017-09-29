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

final module = "SetupFacility.groovy";

facilityData = context.facilityData ?: [:];

facilityId = null;
facility = facilityData.facility;
context.facility = facility;
if (facility) {
    facilityId = facility.facilityId;
    facilityType = facility.getRelatedOne("FacilityType", false);
    context.facilityType = facilityType;
}
context.facilityId = facilityId;

facilityContactMechPurposeList = facilityData.facilityContactMechPurposeList;
context.facilityContactMechPurposeList = facilityContactMechPurposeList;

facilityContactMechsByPurpose = [:];
facilityContactMechsById = [:];
facilityContactMechPurposes = [:];
for(purpose in facilityContactMechPurposeList) {
    // WARN: can't use findByOne because the fromDate may not match!
    def contactMech = EntityUtil.getFirst(delegator.findByAnd("FacilityAndContactMech", 
        [facilityId:purpose.facilityId, contactMechId:purpose.contactMechId], null, false));
    facilityContactMechsByPurpose[purpose.contactMechPurposeTypeId] = contactMech;
    facilityContactMechsById[purpose.contactMechId] = contactMech;
    purposeSet = facilityContactMechPurposes[purpose.contactMechId];
    if (!purposeSet) {
        purposeSet = new HashSet();
        facilityContactMechPurposes[purpose.contactMechId] = purposeSet;
    }
    purposeSet.add(purpose.contactMechPurposeTypeId);
}
context.facilityContactMechsByPurpose = facilityContactMechsByPurpose;
context.facilityContactMechsById = facilityContactMechsById;
context.facilityContactMechs = facilityContactMechsById.values() as List;
context.facilityContactMechPurposes = facilityContactMechPurposes;

if (facility != null) {
    Debug.logInfo("Setup: Setting up existing warehouse '" + facilityId + "' with contact mechs and purposes: " + facilityContactMechPurposes, module);
} else {
    Debug.logInfo("Setup: Setting up new warehouse", module);
}

listPartyPostalAddress = delegator.findByAnd("PartyAndPostalAddress", [partyId: context.partyId], null, false);
partyPostalAddress = EntityUtil.getFirst(EntityUtil.filterByDate(listPartyPostalAddress));
context.partyPostalAddress = partyPostalAddress;

context.facilitySubmitOk = (facility != null) || (partyPostalAddress);

defaultDefaultWeightUomId = UtilProperties.getPropertyValue("scipiosetup", "facility.defaultWeightUomId");
context.defaultDefaultWeightUomId = defaultDefaultWeightUomId;


// NOTE: some of these below from EditFacility.groovy

//Facility types
facilityTypes = from("FacilityType").queryList();
if (facilityTypes) {
    context.facilityTypes = facilityTypes;
}

// all possible inventory item types
context.inventoryItemTypes = from("InventoryItemType").orderBy("description").cache(true).queryList();

// weight unit of measures
context.weightUomList = from("Uom").where("uomTypeId", "WEIGHT_MEASURE").cache(true).queryList();

// area unit of measures
context.areaUomList = from("Uom").where("uomTypeId", "AREA_MEASURE").cache(true).queryList();

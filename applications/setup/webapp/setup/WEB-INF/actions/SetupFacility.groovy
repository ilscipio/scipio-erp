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



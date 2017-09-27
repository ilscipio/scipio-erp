import org.ofbiz.entity.util.*;

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
for(purpose in facilityContactMechPurposeList) {
    facilityContactMechsByPurpose[purpose.contactMechPurposeTypeId] = delegator.findOne("FacilityAndContactMech", 
        [facilityId:purpose.facilityId, contactMechId:purpose.contactMechId, fromDate:purpose.fromDate], false);
}
context.facilityContactMechsByPurpose = facilityContactMechsByPurpose;

listPartyPostalAddress = delegator.findByAnd("PartyAndPostalAddress", [partyId: context.partyId], null, false);
partyPostalAddress = EntityUtil.getFirst(EntityUtil.filterByDate(listPartyPostalAddress));
context.partyPostalAddress = partyPostalAddress;
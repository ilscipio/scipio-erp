import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupOrganization.groovy";

SetupWorker setupWorker = context.setupWorker;
setupStep = context.setupStep;
partyId = context.orgPartyId;

organizationData = context.organizationData ?: [:];

organizationInfo = null; // contains record data for the WHOLE form

if (context.party) {
    organizationInfo = [:];
    organizationInfo.putAll(context.party);
    organizationInfo.putAll(context.partyGroup);
}

//context.groupName = context.partyGroup?.groupName;

// FIXME: bad names: the organizationData.*ContactMech are actually PartyContactMech records

generalAddressContactMech = organizationData.generalAddressContactMech;
context.generalAddressContactMech = generalAddressContactMech;
context.generalAddressContactMechPurposes = organizationData.generalAddressContactMechPurposes;
context.generalAddressStandaloneCompleted = organizationData.generalAddressStandaloneCompleted;
context.locationAddressesCompleted = organizationData.locationAddressesCompleted;
context.locationPurposes = organizationData.locationPurposes;
generalPostalAddress = null;
if (generalAddressContactMech) {
    postalAddress = delegator.findOne("PostalAddress", [contactMechId:generalAddressContactMech.contactMechId], false);
    if (postalAddress) {
        generalPostalAddress = [
            "USER_ADDRESS_CONTACTMECHID": generalAddressContactMech.contactMechId,
            "USER_STATE": postalAddress.stateProvinceGeoId,
            "USER_COUNTRY": postalAddress.countryGeoId,
            "USER_ADDRESS1": postalAddress.address1,
            "USER_ADDRESS2": postalAddress.address2,
            "USER_CITY": postalAddress.city,
            "USER_POSTAL_CODE": postalAddress.postalCode
        ];
        if (organizationInfo != null) {
            organizationInfo.putAll(generalPostalAddress);
        }
    } else {
        Debug.logError("Setup: Configuration error: Mail/ship address contact mech '"
            + generalAddressContactMech.contactMechId + " has no PostalAddress record! Invalid data configuration!", module)
    }
    
}
//context.generalPostalAddress = generalPostalAddress;

workPhoneContactMech = organizationData.workPhoneContactMech;
context.workPhoneContactMech = workPhoneContactMech;
workPhoneNumber = null;
if (workPhoneContactMech) {
    telecomNumber = delegator.findOne("TelecomNumber", [contactMechId:workPhoneContactMech.contactMechId], false);
    if (telecomNumber) {
        workPhoneNumber = [
            "USER_WORK_CONTACTMECHID": workPhoneContactMech.contactMechId,
            "USER_WORK_COUNTRY": telecomNumber.countryCode,
            "USER_WORK_AREA": telecomNumber.areaCode,
            "USER_WORK_CONTACT": telecomNumber.contactNumber,
            "USER_WORK_EXT": workPhoneContactMech.extension
        ];
        if (organizationInfo != null) {
            organizationInfo.putAll(workPhoneNumber);
        }
    } else {
        Debug.logError("Setup: Configuration error: Work phone contact mech '"
            + workPhoneContactMech.contactMechId + " has no TelecomNumber record! Invalid data configuration!", module)
    }
}
//context.workPhoneNumber = workPhoneNumber;

faxPhoneContactMech = organizationData.faxPhoneContactMech;
context.faxPhoneContactMech = faxPhoneContactMech;
faxPhoneNumber = null;
if (faxPhoneContactMech) {
    telecomNumber = delegator.findOne("TelecomNumber", [contactMechId:faxPhoneContactMech.contactMechId], false);
    if (telecomNumber) {
        faxPhoneNumber = [
            "USER_FAX_CONTACTMECHID": faxPhoneContactMech.contactMechId,
            "USER_FAX_COUNTRY": telecomNumber.countryCode,
            "USER_FAX_AREA": telecomNumber.areaCode,
            "USER_FAX_CONTACT": telecomNumber.contactNumber,
            "USER_FAX_EXT": faxPhoneContactMech.extension
        ];
        if (organizationInfo != null) {
            organizationInfo.putAll(faxPhoneNumber);
        }
    } else {
        Debug.logError("Setup: Configuration error: Fax phone contact mech '" 
            + faxPhoneContactMech.contactMechId + " has no TelecomNumber record! Invalid data configuration!", module)
    }
}
//context.faxPhoneNumber = faxPhoneNumber;

primaryEmailContactMech = organizationData.primaryEmailContactMech;
context.primaryEmailContactMech = primaryEmailContactMech;
primaryEmailAddress = null;
if (primaryEmailContactMech) {
    primaryEmailAddress = [
        "USER_EMAIL_CONTACTMECHID": primaryEmailContactMech.contactMechId,
        "USER_EMAIL": primaryEmailContactMech.getRelatedOne("ContactMech", false)?.infoString
    ];
    if (organizationInfo != null) {
        organizationInfo.putAll(primaryEmailAddress);
    }
}
//context.primaryEmailAddress = primaryEmailAddress;

context.contactMechsCompleted = organizationData.contactMechsCompleted;

// true if explicit orgPartyId OR explicit newOrganization=Y flag OR failed create
organizationSelected = partyId || setupWorker?.isEffectiveNewRecordRequest(setupStep);
context.organizationSelected = organizationSelected;

context.organizationInfo = organizationInfo;

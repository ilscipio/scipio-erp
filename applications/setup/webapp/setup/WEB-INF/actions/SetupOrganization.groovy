import org.ofbiz.base.util.*;
import org.ofbiz.entity.util.*;
import com.ilscipio.scipio.setup.*;

final module = "SetupOrganization.groovy";

SetupWorker setupWorker = context.setupWorker;
setupStep = context.setupStep;
partyId = context.orgPartyId;

organizationData = context.organizationData ?: [:];

//context.groupName = context.partyGroup?.groupName;

// FIXME: bad names: the organizationData.*ContactMech are actually PartyContactMech records

mailShipAddressContactMech = organizationData.mailShipAddressContactMech;
context.mailShipAddressContactMech = mailShipAddressContactMech;
context.mailShipAddressContactMechPurposes = organizationData.mailShipAddressContactMechPurposes;
context.mailShipAddressStandaloneCompleted = organizationData.mailShipAddressStandaloneCompleted;
context.locationAddressesCompleted = organizationData.locationAddressesCompleted;
context.locationPurposes = organizationData.locationPurposes;
mailShipPostalAddress = null;
if (mailShipAddressContactMech) {
    postalAddress = delegator.findOne("PostalAddress", [contactMechId:mailShipAddressContactMech.contactMechId], false);
    if (postalAddress) {
        mailShipPostalAddress = [
            "USER_STATE": postalAddress.stateProvinceGeoId,
            "USER_COUNTRY": postalAddress.countryGeoId,
            "USER_ADDRESS1": postalAddress.address1,
            "USER_ADDRESS2": postalAddress.address2,
            "USER_CITY": postalAddress.city,
            "USER_POSTAL_CODE": postalAddress.postalCode
        ];
    } else {
        Debug.logError("Setup: Configuration error: Mail/ship address contact mech '"
            + mailShipAddressContactMech.contactMechId + " has no PostalAddress record! Invalid data configuration!", module)
    }
    
}
context.mailShipPostalAddress = mailShipPostalAddress;

workPhoneContactMech = organizationData.workPhoneContactMech;
context.workPhoneContactMech = workPhoneContactMech;
workPhoneNumber = null;
if (workPhoneContactMech) {
    telecomNumber = delegator.findOne("TelecomNumber", [contactMechId:workPhoneContactMech.contactMechId], false);
    if (telecomNumber) {
        workPhoneNumber = [
            "USER_WORK_COUNTRY": telecomNumber.countryCode,
            "USER_WORK_AREA": telecomNumber.areaCode,
            "USER_WORK_CONTACT": telecomNumber.contactNumber,
            "USER_WORK_EXT": workPhoneContactMech.extension
        ];
    } else {
        Debug.logError("Setup: Configuration error: Work phone contact mech '"
            + workPhoneContactMech.contactMechId + " has no TelecomNumber record! Invalid data configuration!", module)
    }
}
context.workPhoneNumber = workPhoneNumber;

faxPhoneContactMech = organizationData.faxPhoneContactMech;
context.faxPhoneContactMech = faxPhoneContactMech;
faxPhoneNumber = null;
if (faxPhoneContactMech) {
    telecomNumber = delegator.findOne("TelecomNumber", [contactMechId:faxPhoneContactMech.contactMechId], false);
    if (telecomNumber) {
        faxPhoneNumber = [
            "USER_FAX_COUNTRY": telecomNumber.countryCode,
            "USER_FAX_AREA": telecomNumber.areaCode,
            "USER_FAX_CONTACT": telecomNumber.contactNumber,
            "USER_FAX_EXT": faxPhoneContactMech.extension
        ];
    } else {
        Debug.logError("Setup: Configuration error: Fax phone contact mech '" 
            + faxPhoneContactMech.contactMechId + " has no TelecomNumber record! Invalid data configuration!", module)
    }
}
context.faxPhoneNumber = faxPhoneNumber;

primaryEmailContactMech = organizationData.primaryEmailContactMech;
context.primaryEmailContactMech = primaryEmailContactMech;
primaryEmailAddress = null;
if (primaryEmailContactMech) {
    primaryEmailAddress = [USER_EMAIL: primaryEmailContactMech.getRelatedOne("ContactMech", false)?.infoString];
}
context.primaryEmailAddress = primaryEmailAddress;

context.contactMechsCompleted = organizationData.contactMechsCompleted;

// true if explicit orgPartyId OR explicit newOrganization=Y flag OR failed create
organizationSelected = partyId || setupWorker?.isEffectiveNewRecordRequest(setupStep);
context.organizationSelected = organizationSelected;


import org.apache.commons.lang.StringUtils
import org.ofbiz.base.util.Debug
import org.ofbiz.entity.GenericValue
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.util.EntityQuery
import org.ofbiz.entity.util.EntityUtil
import org.ofbiz.product.store.ProductStoreWorker

import com.ilscipio.scipio.setup.*;

final module = "SetupUser.groovy";

SetupWorker setupWorker = context.setupWorker;
setupStep = context.setupStep;

userData = context.userData ?: [:];

userInfo = null;

if (context.productStoreId) {
    productStore = ProductStoreWorker.getProductStore(context.productStoreId, delegator);

    context.createAllowPassword = "Y".equals(productStore.allowPassword);
    context.getUsername = !"Y".equals(productStore.usePrimaryEmailUsername);
}

userParty = userData.userParty;
context.userParty = userParty;
userPartyId = userData.userPartyId;
context.userPartyId = userPartyId;
orgPartyId = context.orgPartyId;

if (context.userParty) {
    userInfo = [:];
    userInfo.putAll(context.userParty);    
    if (userData.userUserLogin) {
        userInfo.putAll(userData.userUserLogin);
        context.userUserLogin = userData.userUserLogin;
    }
    if (userData.userPerson) {
        userInfo.putAll(userData.userPerson);
    }

    // this can't possibly work properly
    //partyRole = EntityUtil.getFirst(delegator.findByAnd("PartyRole", ["partyId" : userParty.partyId], null, false));
    //context.userPartyRole = partyRole;
    //if (partyRole) {
    //    context.userPartyRelationship = EntityUtil.getFirst(EntityUtil.filterByDate(delegator.findByAnd("PartyRelationship", ["partyIdTo" : userParty.partyId, "roleTypeIdTo" : partyRole.roleTypeId], null, false)));
    //}
    if (orgPartyId) {
        userPartyRelationship = EntityUtil.getFirst(EntityUtil.filterByDate(delegator.findByAnd("PartyRelationship", 
            ["partyIdFrom" : orgPartyId, "partyIdTo" : userParty.partyId], null, false)));
        if (userPartyRelationship) {
            context.userPartyRelationship = userPartyRelationship;
            context.userPartyRole = userPartyRelationship.getRelatedOne("ToPartyRole", false);
        } 
    }
}

generalAddressContactMech = userData.generalAddressContactMech;
context.generalAddressContactMech = generalAddressContactMech;
context.generalAddressContactMechPurposes = userData.generalAddressContactMechPurposes;
context.generalAddressStandaloneCompleted = userData.generalAddressStandaloneCompleted;
context.locationAddressesCompleted = userData.locationAddressesCompleted;
context.locationPurposes = userData.locationPurposes;
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
        if (userInfo != null) {
            userInfo.putAll(generalPostalAddress);
        }
    } else {
        Debug.logError("Setup: Configuration error: Mail/ship address contact mech '"
            + generalAddressContactMech.contactMechId + " has no PostalAddress record! Invalid data configuration!", module)
    }
    
}

workPhoneContactMech = userData.workPhoneContactMech;
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
        if (userInfo != null) {
            userInfo.putAll(workPhoneNumber);
        }
    } else {
        Debug.logError("Setup: Configuration error: Work phone contact mech '"
            + workPhoneContactMech.contactMechId + " has no TelecomNumber record! Invalid data configuration!", module)
    }
}

mobilePhoneContactMech = userData.mobilePhoneContactMech;
context.mobilePhoneContactMech = mobilePhoneContactMech;
mobilePhoneNumber = null;
if (mobilePhoneContactMech) {
    telecomNumber = delegator.findOne("TelecomNumber", [contactMechId:mobilePhoneContactMech.contactMechId], false);
    if (telecomNumber) {
        mobilePhoneNumber = [
            "USER_MOBILE_CONTACTMECHID": mobilePhoneContactMech.contactMechId,
            "USER_MOBILE_COUNTRY": telecomNumber.countryCode,
            "USER_MOBILE_AREA": telecomNumber.areaCode,
            "USER_MOBILE_CONTACT": telecomNumber.contactNumber,
            "USER_MOBILE_EXT": mobilePhoneContactMech.extension
        ];
        if (userInfo != null) {
            userInfo.putAll(mobilePhoneNumber);
        }
    } else {
        Debug.logError("Setup: Configuration error: Mobile phone contact mech '"
            + mobilePhoneContactMech.contactMechId + " has no TelecomNumber record! Invalid data configuration!", module)
    }
}

faxPhoneContactMech = userData.faxPhoneContactMech;
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
        if (userInfo != null) {
            userInfo.putAll(faxPhoneNumber);
        }
    } else {
        Debug.logError("Setup: Configuration error: Fax phone contact mech '"
            + faxPhoneContactMech.contactMechId + " has no TelecomNumber record! Invalid data configuration!", module)
    }
}


primaryEmailContactMech = userData.primaryEmailContactMech;
context.primaryEmailContactMech = primaryEmailContactMech;
primaryEmailAddress = null;
if (primaryEmailContactMech) {
    primaryEmailAddress = [
        "USER_EMAIL_CONTACTMECHID": primaryEmailContactMech.contactMechId,
        "USER_EMAIL": primaryEmailContactMech.getRelatedOne("ContactMech", false)?.infoString
    ];
    if (userInfo != null) {
        userInfo.putAll(primaryEmailAddress);
    }
}


// true if explicit userPartyId OR explicit newUser=Y flag OR failed create
userSelected = userPartyId || setupWorker?.isEffectiveNewRecordRequest(StringUtils.capitalize(setupStep));
context.userSelected = userSelected;

context.contactMechsCompleted = userData.contactMechsCompleted;

// FIXME: this omits important roles such as EMPLOYEE
List<GenericValue> userPartyRoles = EntityQuery.use(delegator).from("RoleType").where(EntityCondition.makeCondition("parentTypeId", EntityOperator.EQUALS, null)).orderBy(["description"]).query();
context.userPartyRoles = userPartyRoles;
List<GenericValue> userPartyRelationshipTypes = EntityQuery.use(delegator).from("PartyRelationshipType").orderBy(["partyRelationshipName"]).query();
context.userPartyRelationshipTypes = userPartyRelationshipTypes;

context.userInfo = userInfo;
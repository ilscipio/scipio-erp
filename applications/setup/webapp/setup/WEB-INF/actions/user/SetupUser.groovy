import org.ofbiz.base.util.UtilMisc
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

if (context.userParty) {
    userInfo = [:];
    userInfo.putAll(context.userParty);
    userInfo.putAll(userData.userUserLogin);
    userInfo.putAll(userData.userPerson);
}

//
//generalAddressContactMech = organizationData.generalAddressContactMech;
//context.generalAddressContactMech = generalAddressContactMech;
//context.generalAddressContactMechPurposes = organizationData.generalAddressContactMechPurposes;
//context.generalAddressStandaloneCompleted = organizationData.generalAddressStandaloneCompleted;
//context.locationAddressesCompleted = organizationData.locationAddressesCompleted;
//context.locationPurposes = organizationData.locationPurposes;
//generalPostalAddress = null;
//if (generalAddressContactMech) {
//    postalAddress = delegator.findOne("PostalAddress", [contactMechId:generalAddressContactMech.contactMechId], false);
//    if (postalAddress) {
//        generalPostalAddress = [
//            "USER_ADDRESS_CONTACTMECHID": generalAddressContactMech.contactMechId,
//            "USER_STATE": postalAddress.stateProvinceGeoId,
//            "USER_COUNTRY": postalAddress.countryGeoId,
//            "USER_ADDRESS1": postalAddress.address1,
//            "USER_ADDRESS2": postalAddress.address2,
//            "USER_CITY": postalAddress.city,
//            "USER_POSTAL_CODE": postalAddress.postalCode
//        ];
//        if (organizationInfo != null) {
//            organizationInfo.putAll(generalPostalAddress);
//        }
//    } else {
//        Debug.logError("Setup: Configuration error: Mail/ship address contact mech '"
//            + generalAddressContactMech.contactMechId + " has no PostalAddress record! Invalid data configuration!", module)
//    }
//    
//}

userContactMechPurposeList = userData.userContactMechPurposeList;
context.userContactMechPurposeList = userContactMechPurposeList;

userContactMechsByPurpose = [:];
userContactMechsById = [:];
userContactMechPurposes = [:];
for(purpose in userContactMechPurposeList) {
    // WARN: can't use findByOne because the fromDate may not match!
    def contactMech = EntityUtil.getFirst(delegator.findByAnd("PartyAndContactMech",
            [partyId: purpose.partyId, contactMechId:purpose.contactMechId], null, false));
    userContactMechsByPurpose[purpose.contactMechPurposeTypeId] = contactMech;
    userContactMechsById[purpose.contactMechId] = contactMech;
    purposeSet = userContactMechPurposes[purpose.contactMechId];
    if (!purposeSet) {
        purposeSet = new HashSet();
        userContactMechPurposes[purpose.contactMechId] = purposeSet;
    }
    purposeSet.add(purpose.contactMechPurposeTypeId);
}

context.userContactMechsByPurpose = userContactMechsByPurpose;
context.userContactMechsById = userContactMechsById;
context.userContactMechs = userContactMechsById.values() as List;
context.userContactMechPurposes = userContactMechPurposes;

if (userContactMechsByPurpose["PHONE_WORK"]) {
    context.userWorkNumber = userContactMechsByPurpose["PHONE_WORK"].getRelatedOne("TelecomNumber", false);
}
if (userContactMechsByPurpose["PHONE_MOBILE"]) {
    context.userMobileNumber = userContactMechsByPurpose["PHONE_MOBILE"].getRelatedOne("TelecomNumber", false);
}
if (userContactMechsByPurpose["FAX_NUMBER"]) {
    context.userFaxNumber = userContactMechsByPurpose["FAX_NUMBER"].getRelatedOne("TelecomNumber", false);
}
if (userContactMechsByPurpose["PRIMARY_EMAIL"]) {
    context.userEmailAddress = userContactMechsByPurpose["PRIMARY_EMAIL"];
}
if (userContactMechsByPurpose["GENERAL_LOCATION"]) {
    context.userPostalAddress = userContactMechsByPurpose["GENERAL_LOCATION"].getRelatedOne("PostalAddress", false);
}

// true if explicit userPartyId OR explicit newUser=Y flag OR failed create
userSelected = userPartyId || setupWorker?.isEffectiveNewRecordRequest(setupStep);
context.userSelected = userSelected;

//List<GenericValue> roleTypes = delegator.findByAnd("RoleType", UtilMisc.toMap("parentTypeId", null), UtilMisc.toList("description"), true);

List<GenericValue> userPartyRoles = EntityQuery.use(delegator).from("RoleType").where(EntityCondition.makeCondition("parentTypeId", EntityOperator.EQUALS, null)).orderBy(["description"]).query();
context.userPartyRoles = userPartyRoles;

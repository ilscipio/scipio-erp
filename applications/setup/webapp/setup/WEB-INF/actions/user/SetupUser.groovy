import org.ofbiz.base.util.Debug
import org.ofbiz.entity.GenericValue
import org.ofbiz.entity.util.EntityUtil
import org.ofbiz.product.store.ProductStoreWorker
import com.ilscipio.scipio.setup.*;

final module = "SetupUser.groovy";

userData = context.userData ?: [:];

if (context.productStoreId) {
    productStore = ProductStoreWorker.getProductStore(context.productStoreId, delegator);

    context.createAllowPassword = "Y".equals(productStore.allowPassword);
    context.getUsername = !"Y".equals(productStore.usePrimaryEmailUsername);
}

userParty = userData.userParty;
context.userParty = userParty;
userPartyId = userData.userPartyId;
context.userPartyId = userPartyId;


context.userUserLogin = userData.userUserLogin;
context.userPerson = userData.userPerson;

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

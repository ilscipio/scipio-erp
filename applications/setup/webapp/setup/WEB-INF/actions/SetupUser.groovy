import org.ofbiz.base.util.Debug
import org.ofbiz.entity.GenericValue
import org.ofbiz.entity.util.EntityUtil
import org.ofbiz.product.store.ProductStoreWorker

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
context.userPostalAddress = userData.userPostalAddress;
context.userEmailAddress = userData.userEmailAddress;
context.userTelecomNumber = userData.userTelecomNumber;

//if (userParty) {
//    Debug.log("userParty =================> " + userParty);
//
//    userUserLogin = EntityUtil.getFirst(delegator.findByAnd("UserLogin", ["partyId" : userData.userPartyId], null, false));
//    userPerson = delegator.findOne("Person", ["partyId" : userData.userPartyId], false);
//    userPartyContactMechList = delegator.findByAnd("PartyContactMech", ["partyId" : userData.userPartyId], null, false);
//    
//    Debug.log("userUserLogin ====================> " + userUserLogin);
//    Debug.log("userPerson ====================> " + userPerson);
//    for (GenericValue userPartyContactMech in userPartyContactMechList) {
//        Debug.log("userPartyContactMech ======================> " + userPartyContactMech);
//    }
//    
//}

//Debug.log("userData =========> " + userData);
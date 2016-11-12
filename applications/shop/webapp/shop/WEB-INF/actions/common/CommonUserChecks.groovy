/**
 * SCIPIO: Common permission checks that any store screen may need.
 */

import org.ofbiz.base.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.entity.util.*;
 
final module = "CommonUserChecksGroovy";

userLogin = context.userLogin; // (could be missing entirely)
permChecksSetGlobal = context.remove("permChecksSetGlobal");
if (permChecksSetGlobal == null) {
    permChecksSetGlobal = true; // default is TRUE to prevent accidental scope conflicts with globals which wreak havoc
}
targetCtx = context;
if (permChecksSetGlobal == true) {
    targetCtx = globalContext;
}

// Full login
userHasAccount = ((userLogin) && (userLogin.userLoginId != "anonymous"));
targetCtx.userHasAccount = userHasAccount;

// Anon user (whether recognized or not)
userIsAnon = ((!userLogin) || (userLogin.userLoginId == "anonymous"));
targetCtx.userIsAnon = userIsAnon;

// Recognized anon user (associated with a party in session)
userIsAnonKnown = ((userLogin) && userIsAnon && (userLogin.partyId));
targetCtx.userIsAnonKnown = userIsAnonKnown;

// Unrecognized anon user (no party in session yet)
userIsAnonUnknown = (userIsAnon && ((!userLogin) || (!userLogin.partyId)));
targetCtx.userIsAnonUnknown = userIsAnonUnknown;

// Either full login or recognized anon user
userIsKnown = (userHasAccount || userIsAnonKnown);
targetCtx.userIsKnown = userIsKnown;

// Get the user party
userParty = null;
if (userLogin?.partyId) {
    userParty = EntityQuery.use(delegator).from("Party").where("partyId", userLogin.partyId).queryOne();
}
targetCtx.userParty = userParty;

userIsBusiness = false;
if (userParty?.partyTypeId) {
    // FIXME?: Currently do a simplistic partyTypeId check to see if business rep.
    // By default we assume personal unless has a partyTypeId that is non-PERSON
    if (userParty.partyTypeId != "PERSON" && userParty.partyTypeId != "FAMILY") {
        userIsBusiness = true;
    }
}
targetCtx.userIsBusiness = userIsBusiness;

userIsPersonal = !userIsBusiness;
targetCtx.userIsPersonal = userIsPersonal;




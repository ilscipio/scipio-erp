/**
 * Cato: Common permission checks that any store screen may need.
 */

final module = "CommonPermChecks.groovy";

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






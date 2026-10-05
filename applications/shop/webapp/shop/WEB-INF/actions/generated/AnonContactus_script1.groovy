import org.ofbiz.base.util.*;
                    userLogin = context.userLogin;
                    if (userLogin?.partyId) {
                        try {
                            servRes = runService("getPartyEmail", [partyId:userLogin.partyId, userLogin:userLogin]);
                            context.partyEmailAddress = servRes.emailAddress;
                        } catch(Exception e) {
                            Debug.logError(e, "AnonContactusScreen.groovy");
                        }
                    }
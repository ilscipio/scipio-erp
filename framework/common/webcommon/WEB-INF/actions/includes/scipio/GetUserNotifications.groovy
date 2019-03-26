import org.ofbiz.base.util.*;

final module = "GetUserNotifications.groovy"

userLogin = context.userLogin;
def result = null;
if (userLogin?.partyId) {
    try {
        result = runService("getSystemMessages", [partyId:userLogin.partyId, userLogin:userLogin]);
    } catch(Exception e) {
        Debug.logError("Error getting SystemMessages: " + e.toString(), module);
    }
}
context.systemNotifications = result?.messages ?: [];
context.systemNotificationsCount = result?.count ?: 0L;

import javolution.util.FastList

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityJoinOperator
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.model.DynamicViewEntity
import org.ofbiz.entity.model.ModelKeyMap
import org.ofbiz.webapp.stats.ServerHitBin;


String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year

Calendar calendar = Calendar.getInstance();
if (iScope.equals("day")) {
    calendar.set(Calendar.DAY_OF_YEAR, calendar.get(Calendar.DAY_OF_YEAR) - 30);
} else if (iScope.equals("week")) {
    calendar.set(Calendar.DAY_OF_WEEK, 1);
    calendar.set(Calendar.WEEK_OF_YEAR, calendar.get(Calendar.WEEK_OF_YEAR) - 12);
} else if (iScope.equals("month")) {
    calendar.set(Calendar.DAY_OF_MONTH, 1);
    calendar.set(Calendar.MONTH, calendar.get(Calendar.MONTH) - 6);
} else if (iScope.equals("year")) {
    calendar.set(Calendar.DAY_OF_YEAR, 1);
    calendar.set(Calendar.MONTH, 1);
    calendar.set(Calendar.YEAR, calendar.get(Calendar.YEAR) - 5);
}
fromDate = UtilDateTime.toTimestamp(calendar.getTime());

List exprsList = FastList.newInstance();
exprsList.add(EntityCondition.makeCondition("enabled", EntityOperator.EQUALS, "N"));
exprsList.add(EntityCondition.makeCondition("successfulLogin", EntityOperator.EQUALS, 'N'));

DynamicViewEntity dve = new DynamicViewEntity();
dve.addMemberEntity("UL", "UserLogin");
dve.addMemberEntity("ULH", "UserLoginHistory");
dve.addAlias("UL", "userLoginId", null, null, null, true, null);
dve.addAlias("UL", "enabled", null, null, null, true, null);
dve.addAlias("UL", "disabledDateTime", null, null, null, true, null);
dve.addAlias("ULH", "fromDate", null, null, null, true, null);
dve.addAlias("ULH", "successfulLogin", null, null, null, true, null);
dve.addAlias("ULH", "lastUpdatedStamp", null, null, null, true, null);
dve.addAlias("ULH", "visitId", null, null, null, true, null);
dve.addViewLink("UL", "ULH", Boolean.FALSE, UtilMisc.toList(new ModelKeyMap("userLoginId", "userLoginId")));
dve.addRelation("many", "", "ServerHit", [
    new ModelKeyMap("visitId", "visitId")
]);
userLoginAndHistoryList = from(dve).where(EntityCondition.makeCondition(
        EntityCondition.makeCondition(exprsList, EntityJoinOperator.OR),
        EntityOperator.AND,
        EntityCondition.makeCondition("fromDate", EntityOperator.GREATER_THAN, fromDate)
        )).orderBy("disabledDateTime", "lastUpdatedStamp").queryList();

securityAlerts = [];
for (userLoginAndHistory in userLoginAndHistoryList) {
    serverHits = userLoginAndHistory.getRelated("ServerHit", UtilMisc.toMap("hitTypeId", "REQUEST"), null, false);
    for (serverHit in serverHits) {
        // FIXME: Unfortunately this is the only way I can match userLoginHistory with its corresponding serverHit and I feel it might be error prone since I'm skipping miliseconds
        hsDate = UtilDateTime.toCalendar(serverHit.hitStartDateTime);
        hitStartDate = Calendar.getInstance();
        hitStartDate.set(hsDate.get(Calendar.YEAR), hsDate.get(Calendar.MONTH), hsDate.get(Calendar.DATE), hsDate.get(Calendar.HOUR_OF_DAY), hsDate.get(Calendar.MINUTE), hsDate.get(Calendar.SECOND));

        fDate = UtilDateTime.toCalendar(userLoginAndHistory.fromDate);
        fromDate = Calendar.getInstance();
        fromDate.set(fDate.get(Calendar.YEAR), fDate.get(Calendar.MONTH), fDate.get(Calendar.DATE), fDate.get(Calendar.HOUR_OF_DAY), fDate.get(Calendar.MINUTE), fDate.get(Calendar.SECOND));        
        if (hitStartDate.equals(fromDate)) {
            securityAlert = UtilMisc.toMap("userLoginId", userLoginAndHistory.userLoginId, "enabled", userLoginAndHistory.enabled, 
                "successfulLogin", userLoginAndHistory.successfulLogin, "contentId", serverHit.contentId, "requestUrl", serverHit.requestUrl,
                "disabledDateTime", userLoginAndHistory.disabledDateTime, "serverIpAddress", serverHit.serverIpAddress, "fromDate", fromDate.getTime());
            securityAlerts.add(securityAlert);
            
            Debug.log("userLoginId ==========> " + userLoginAndHistory.userLoginId +  " enabled ========>  " + userLoginAndHistory.enabled + " successfulLogin ========> " + userLoginAndHistory.successfulLogin
                    + "   visitId =========> " + userLoginAndHistory.visitId
                    + "   fromDate ========> " + userLoginAndHistory.fromDate);
                
            break;
        }
    }
}
context.securityAlerts = securityAlerts;
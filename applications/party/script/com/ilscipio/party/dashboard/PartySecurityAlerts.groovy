import javolution.util.FastList

import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityJoinOperator
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.model.DynamicViewEntity
import org.ofbiz.entity.model.ModelKeyMap
import org.ofbiz.entity.util.EntityUtilProperties
import org.ofbiz.base.util.*;

final module = "PartySecurityAlerts.groovy"

iCount = (context.chartIntervalCount != null) ? context.chartIntervalCount : 0;
String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year
if (iCount <= 0)
    iCount = UtilDateTime.getIntervalDefaultCount(iScope);
fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);
dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);

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

Debug.logInfo("Running main query...", module);
userLoginAndHistoryList = from(dve).where(EntityCondition.makeCondition(
        EntityCondition.makeCondition(exprsList, EntityJoinOperator.OR),
        EntityOperator.AND,
        EntityCondition.makeCondition("fromDate", EntityOperator.GREATER_THAN, dateIntervals.getDateBegin())
        )).orderBy("lastUpdatedStamp DESC").queryList();
    
Debug.logInfo("Main query executed; processing data and related queries...", module);
securityAlerts = [];
for (userLoginAndHistory in userLoginAndHistoryList) {
    serverHits = userLoginAndHistory.getRelated("ServerHit", UtilMisc.toMap("hitTypeId", "REQUEST"), null, true);
    for (serverHit in serverHits) {
        // FIXME: Unfortunately this is the only way I can match userLoginHistory with its corresponding serverHit and I feel it might be error prone since I'm skipping miliseconds
        hsDate = UtilDateTime.toCalendar(serverHit.hitStartDateTime);
        hitStartDate = Calendar.getInstance();
        hitStartDate.set(hsDate.get(Calendar.YEAR), hsDate.get(Calendar.MONTH), hsDate.get(Calendar.DATE), hsDate.get(Calendar.HOUR_OF_DAY), hsDate.get(Calendar.MINUTE), hsDate.get(Calendar.SECOND));
        hitStartDate.set(Calendar.MILLISECOND, 0);

        fDate = UtilDateTime.toCalendar(userLoginAndHistory.fromDate);
        fromDateUserLoginHistory = Calendar.getInstance();
        fromDateUserLoginHistory.set(fDate.get(Calendar.YEAR), fDate.get(Calendar.MONTH), fDate.get(Calendar.DATE), fDate.get(Calendar.HOUR_OF_DAY), fDate.get(Calendar.MINUTE), fDate.get(Calendar.SECOND));
        fromDateUserLoginHistory.set(Calendar.MILLISECOND, 0);
        visit = serverHit.getRelatedOne("Visit", true);
        if (hitStartDate.getTimeInMillis() == fromDateUserLoginHistory.getTimeInMillis()) {
            securityAlert = UtilMisc.toMap("userLoginId", userLoginAndHistory.userLoginId, "enabled", userLoginAndHistory.enabled,
                "successfulLogin", userLoginAndHistory.successfulLogin, "contentId", serverHit.contentId, "requestUrl", serverHit.requestUrl,
                "disabledDateTime", userLoginAndHistory.disabledDateTime, "serverIpAddress", serverHit.serverIpAddress, "clientIpAddress", visit.clientIpAddress,"fromDate", fromDateUserLoginHistory.getTime());
            securityAlerts.add(securityAlert);
            break;
        }
    }
}

// pagination for the security alerts list
viewIndex = Integer.valueOf(parameters.VIEW_INDEX  ?: 0);
viewSize = 0;
viewSize = Integer.valueOf(parameters.VIEW_SIZE ?: EntityUtilProperties.getPropertyValue("widget", "widget.form.defaultViewSize", "10", delegator));
listSize = securityAlerts ? securityAlerts.size() : 0;

lowIndex = viewIndex * viewSize;
highIndex = (viewIndex + 1) * viewSize;
highIndex = highIndex > listSize ? listSize : highIndex;
lowIndex = lowIndex > highIndex ? highIndex : lowIndex;

resultPartialList = securityAlerts.subList(lowIndex, highIndex);

Debug.logInfo("Data processed", module);

context.viewIndex = viewIndex;
context.viewSize = viewSize;
context.listSize = listSize;
context.lowIndex = lowIndex;
context.highIndex = highIndex;

context.securityAlerts = resultPartialList;

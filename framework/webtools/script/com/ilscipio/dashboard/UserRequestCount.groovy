import java.text.SimpleDateFormat

import javolution.util.FastList

import org.apache.xmlrpc.util.HttpUtil
import org.ofbiz.accounting.util.UtilAccounting
import org.ofbiz.base.util.*
import org.ofbiz.base.util.cache.UtilCache
import org.ofbiz.entity.*
import org.ofbiz.entity.condition.*
import org.ofbiz.entity.util.*
import org.ofbiz.party.party.PartyWorker


Map<Date, Map<String, BigDecimal>> processResults() {
    List mainAndExprs = FastList.newInstance();
    mainAndExprs.add(EntityCondition.makeCondition("hitTypeId", EntityOperator.EQUALS, "REQUEST"));

    int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : 6;
    String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "hour"; //day|week|month|year
    
    iCount = UtilDateTime.getIntervalDefaultCount(iScope);
    fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);
    
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
    
    Map<Date, Long> totalRequests = [:];
    for (int i = 0; i <= iCount; i++) {      
        
        List serverHitDateAndExprs = FastList.newInstance(mainAndExprs);
        serverHitDateAndExprs.add(EntityCondition.makeCondition("hitStartDateTime", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals["dateBegin"]));
        serverHitDateAndExprs.add(EntityCondition.makeCondition("hitStartDateTime", EntityOperator.LESS_THAN, dateIntervals["dateEnd"]));
       
        serverRequestHits = from("ServerHit").where(serverHitDateAndExprs).queryCount();
        Debug.log("serverRequestHits ===========> " + serverRequestHits);
        totalRequests.put(dateIntervals["dateFormatter"].format(dateIntervals["dateBegin"]), serverRequestHits);
        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntervals["dateEnd"], context.locale, context.timeZone);
    }
    return totalRequests;
}

context.userRequestCount = processResults();
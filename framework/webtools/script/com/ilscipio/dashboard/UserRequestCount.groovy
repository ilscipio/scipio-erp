import javolution.util.FastList

import org.ofbiz.base.util.*
import org.ofbiz.base.util.UtilDateTime.TimeInterval
import org.ofbiz.entity.*
import org.ofbiz.entity.condition.*
import org.ofbiz.entity.util.*
import org.ofbiz.base.util.cache.UtilCache

contentCache = UtilCache.getOrCreateUtilCache("dashboard.webtools", 0, 0, 1800000, true, false);
cacheId = "webtools_user_requestcount";

Map<Date, Map<String, BigDecimal>> processResults() {
    List mainAndExprs = FastList.newInstance();
    mainAndExprs.add(EntityCondition.makeCondition("hitTypeId", EntityOperator.EQUALS, "REQUEST"));

    int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : 6;
    String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "hour"; //day|week|month|year
    
    iCount = UtilDateTime.getIntervalDefaultCount(iScope);
    fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);
    
    TimeInterval dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
    
    Map<Date, Long> totalRequests = [:];
    for (int i = 0; i <= iCount; i++) {
        List serverHitDateAndExprs = FastList.newInstance(mainAndExprs);
        serverHitDateAndExprs.add(EntityCondition.makeCondition("hitStartDateTime", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()));
        serverHitDateAndExprs.add(EntityCondition.makeCondition("hitStartDateTime", EntityOperator.LESS_THAN, dateIntervals.getDateEnd()));
       
        serverRequestHits = from("ServerHit").where(serverHitDateAndExprs).queryCount();
        totalRequests.put(dateIntervals.getDateFormatter().format(dateIntervals.getDateBegin()), serverRequestHits);
        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntervals.getDateEnd(), context.locale, context.timeZone);
    }
    return totalRequests;
}

Map cacheMap = [:];
// TODO: Not sure wether I should leave or not the result to be cached, for now let's not cache it
if (contentCache.get(cacheId)==null) {
    cacheMap = processResults();
    contentCache.put(cacheId, cacheMap);
    Debug.log("adding totalMap to cache");
} else {
    cacheMap = contentCache.get(cacheId);
    Debug.log("taking totalMap from cache");
}
context.userRequestCount = cacheMap;

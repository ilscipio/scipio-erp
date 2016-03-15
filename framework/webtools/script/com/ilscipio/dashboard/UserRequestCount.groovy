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
    
    Calendar calendar = Calendar.getInstance();
    if (iScope.equals("hour")) {
        calendar.set(Calendar.HOUR_OF_DAY, calendar.get(Calendar.HOUR_OF_DAY) - 12);
    } else if (iScope.equals("day")) {
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
    
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDate, context.locale, context.timeZone);
   
    
    Map<Date, Long> totalRequests = [:];
    for (int i = 0; i <= iCount; i++) {      
        List serverHitDateAndExprs = FastList.newInstance(mainAndExprs);
        serverHitDateAndExprs.add(EntityCondition.makeCondition("hitStartDateTime", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals["dateBegin"]));
        serverHitDateAndExprs.add(EntityCondition.makeCondition("hitStartDateTime", EntityOperator.LESS_THAN, dateIntervals["dateEnd"]));
       
        serverRequestHits = from("ServerHit").where(serverHitDateAndExprs).queryCount();
        totalRequests.put(dateIntervals["dateFormatter"].format(dateIntervals["dateBegin"]) + "h", serverRequestHits);
        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, dateIntervals["dateEnd"], context.locale, context.timeZone);
    }
    return totalRequests;
}

context.userRequestCount = processResults();
import javolution.util.FastList

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator

int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : 6;
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
dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDate, context.locale, context.timeZone);

Map<Date, Long> totalMap = [:];
for (int i = 0; i <= iCount; i++) {
    List createdDateAndExprs = FastList.newInstance();
    createdDateAndExprs.add(EntityCondition.makeCondition("createdDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals["dateBegin"]));
    createdDateAndExprs.add(EntityCondition.makeCondition("createdDate", EntityOperator.LESS_THAN, dateIntervals["dateEnd"]));
    
    createdPartiesCount = from("Party").where(createdDateAndExprs).queryCount();
    Debug.log("createdPartiesCount ==========> " + createdPartiesCount);
    
    totalMap.put(dateIntervals["dateFormatter"].format(dateIntervals["dateBegin"]), createdPartiesCount);
    
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, dateIntervals["dateEnd"] + 1, context.locale, context.timeZone);
    
}

context.totalMap = totalMap;
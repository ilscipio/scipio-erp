import javolution.util.FastList

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator

int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : 6;
String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year

iCount = UtilDateTime.getIntervalDefaultCount(iScope);
fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);
dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone)

Map<Date, Long> totalMap = [:];
for (int i = 0; i <= iCount; i++) {
    List createdDateAndExprs = FastList.newInstance();
    createdDateAndExprs.add(EntityCondition.makeCondition("createdDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()));
    createdDateAndExprs.add(EntityCondition.makeCondition("createdDate", EntityOperator.LESS_THAN, dateIntervals.getDateEnd()));
    
    createdPartiesCount = from("Party").select("partyId").where(createdDateAndExprs).queryCount();
    
    totalMap.put(dateIntervals.getDateFormatter().format(dateIntervals.getDateBegin()), createdPartiesCount);    
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntervals.getDateEnd(), context.locale, context.timeZone);    
}

context.totalMap = totalMap;
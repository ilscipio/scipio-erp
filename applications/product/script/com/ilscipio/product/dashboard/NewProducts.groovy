
import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator


//dateIntervals = UtilDateTime.getPeriodInterval("month", null, locale, timeZone);
int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : 6;
String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year

iCount = UtilDateTime.getIntervalDefaultCount(iScope);
fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);
dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);

List introductionDateAndExprs = [];
introductionDateAndExprs.add(EntityCondition.makeCondition("introductionDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()));
introductionDateAndExprs.add(EntityCondition.makeCondition("introductionDate", EntityOperator.LESS_THAN, dateIntervals.getDateEnd()));

newProducts = from("Product").where(introductionDateAndExprs).queryList();
newProducts.each { product ->
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, fromDateTimestamp, context.locale, context.timeZone);
} 

context.products = newProducts;
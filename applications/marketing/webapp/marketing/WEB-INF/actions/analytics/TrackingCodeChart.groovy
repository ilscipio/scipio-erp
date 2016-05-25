import java.sql.Timestamp
import java.text.SimpleDateFormat

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityJoinOperator
import org.ofbiz.entity.condition.EntityOperator


//// use this helper to build a List of visits, orders, order totals, and conversion rates
//trackingCodeVisitAndOrders = ReportHelper.calcConversionRates(visits, orders, "trackingCodeId");
//context.trackingCodeVisitAndOrders = trackingCodeVisitAndOrders;
SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
currentYearBegin = UtilDateTime.getYearStart(nowTimestamp, timeZone, locale);
currentYearEnd  = UtilDateTime.getYearEnd(nowTimestamp, timeZone, locale);
currentYearBeginText = sdf.format(currentYearBegin);
currentYearEndText = sdf.format(currentYearEnd);
cacheId = "marketingTracking_" + currentYearBeginText + "-" + currentYearEndText;

Map<Date, Integer> processResults() {
    Map<Date, Integer> visits = [:];
    if (!context.trackingCodeId && !parameters.trackingCodeId)
        return visits;
    
    int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : -1;
    String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year
    
    // Check and sanitize fromDate/thruDate params 
    fromDate = parameters.fromDate;
    thruDate = parameters.thruDate;
    Timestamp fromDateTimestamp = null;
    Timestamp thruDateTimestamp = null;
    
    if (fromDate)
        fromDateTimestamp = UtilDateTime.toTimestamp(fromDate);
    if (thruDate)
        thruDateTimestamp = UtilDateTime.toTimestamp(thruDate);   
    
    if (fromDateTimestamp && fromDateTimestamp < thruDateTimeStamp) {
        fromDate = null;
        thruDate = null;
    }
    
    if (!fromDateTimestamp) {
        Calendar calendar = Calendar.getInstance();
        if (iScope.equals("day")) {
            if (iCount == -1)
                iCount = 30;
            calendar.set(Calendar.DAY_OF_YEAR, calendar.get(Calendar.DAY_OF_YEAR) - iCount);
        } else if (iScope.equals("week")) {
            if (iCount == -1)
                iCount = 4;
            calendar.set(Calendar.DAY_OF_WEEK, 1);
            calendar.set(Calendar.WEEK_OF_YEAR, calendar.get(Calendar.WEEK_OF_YEAR) - iCount);
        } else if (iScope.equals("month")) {
            if (iCount == -1)
                iCount = 12;
            calendar.set(Calendar.DAY_OF_MONTH, 1);
            calendar.set(Calendar.MONTH, calendar.get(Calendar.MONTH) - iCount);
        } else if (iScope.equals("year")) {
            if (iCount == -1)
                iCount = 5;
            calendar.set(Calendar.DAY_OF_YEAR, 1);
            calendar.set(Calendar.MONTH, 1);
            calendar.set(Calendar.YEAR, calendar.get(Calendar.YEAR) - iCount);
        }
        fromDateTimestamp = UtilDateTime.toTimestamp(calendar.getTime());        
    }
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
    if (thruDateTimestamp && dateIntervals["dateEnd"] < thruDateTimestamp)
        dateIntervals["dateEnd"] = thruDate;
        
    exprList = EntityCondition.makeCondition("trackingCodeId", EntityOperator.EQUALS, context.trackingCodeId);        
        
    for (int i = 0; i <= iCount; i++) {
        int totalVisits = 0;
        fromDateAndExprs =  EntityCondition.makeCondition([EntityCondition.makeCondition("fromDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals["dateBegin"]),
            EntityCondition.makeCondition("fromDate", EntityOperator.LESS_THAN, dateIntervals["dateEnd"])], EntityJoinOperator.AND);        
        visitList = select("visitId").from("TrackingCodeAndVisit").where(EntityCondition.makeCondition([exprList, fromDateAndExprs], EntityOperator.AND)).queryList();
        for (v in visitList) {
            totalVisits += v.visitId;           
        }
        visits.put(dateIntervals["dateFormatter"].format(dateIntervals["dateBegin"]), totalVisits);            
        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, dateIntervals["dateEnd"] + 1, context.locale, context.timeZone);
        if (thruDateTimestamp && dateIntervals["dateEnd"] < thruDateTimestamp)
            dateIntervals["dateEnd"] = thruDateTimestamp;
    }
    return visits;
    
}
visits = processResults();

context.visits = visits;
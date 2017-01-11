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

Map processResults() {
    Map resultMap = [:];
        
    trackingCodeId = parameters.trackingCodeId;
    marketingCampaignId = parameters.marketingCampaignId;
    
    int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : -1;
    String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year
    
    // Check and sanitize fromDate/thruDate params
    fromDate = parameters.fromDate;
    thruDate = parameters.thruDate;
    Timestamp fromDateTimestamp = null;
    Timestamp thruDateTimestamp = null;
    if (fromDate)
        fromDateTimestamp = UtilDateTime.stringToTimeStamp(fromDate, "yyyy-MM-dd HH:mm:ss", context.timeZone, context.locale);    
    if (thruDate)
        thruDateTimestamp = UtilDateTime.stringToTimeStamp(thruDate, "yyyy-MM-dd HH:mm:ss", context.timeZone, context.locale);           
    if (fromDateTimestamp && fromDateTimestamp < thruDateTimestamp) {
        fromDate = null;
        thruDate = null;
    }  
    if (!fromDateTimestamp) {
        iCount = UtilDateTime.getIntervalDefaultCount(iScope);        
        fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);        
    }

    if (iScope.equals("quarter")) iCount = Math.round(iCount / 3);
    if (iScope.equals("semester")) iCount = Math.round(iCount / 6);
   
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);        
    if (thruDateTimestamp && dateIntervals["dateEnd"] < thruDateTimestamp)
        dateIntervals["dateEnd"] = thruDateTimestamp;
        
    exprList = [];
    if (marketingCampaignId && !trackingCodeId) {
        trackingCodeList = select("trackingCodeId").from("TrackingCode").where(["marketingCampaignId" : marketingCampaignId]).queryList();
        trackingCodes = [];
        for (trackingCode in trackingCodeList) 
            trackingCodes += trackingCode.trackingCodeId;
        if (trackingCodes)
            exprList.add(EntityCondition.makeCondition("trackingCodeId", EntityOperator.IN, trackingCodes));        
    } else if (trackingCodeId) {
        exprList.add(EntityCondition.makeCondition("trackingCodeId", EntityOperator.EQUALS, trackingCodeId));
    }
     
    if ((marketingCampaignId || trackingCodeId) && exprList) {
        
        for (int i = 0; i < iCount; i++) {            
            int totalVisits = 0;
            int totalOrders = 0;
            conditionList = [];
            
            // Get visits
            conditionList = EntityCondition.makeCondition(EntityCondition.makeCondition(exprList), EntityJoinOperator.AND, 
                EntityCondition.makeCondition([EntityCondition.makeCondition("fromDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()),
                EntityCondition.makeCondition("fromDate", EntityOperator.LESS_THAN, dateIntervals.getDateEnd())], EntityJoinOperator.AND));            
            visitList = select("visitId").from("TrackingCodeAndVisit").where(conditionList).queryList();
            for (v in visitList)
                totalVisits += v.visitId;                
            // Get orders
            conditionList = EntityCondition.makeCondition(EntityCondition.makeCondition(exprList), EntityJoinOperator.AND, 
                    EntityCondition.makeCondition([EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()),
                    EntityCondition.makeCondition("orderDate", EntityOperator.LESS_THAN, dateIntervals.getDateEnd())], EntityJoinOperator.AND));
            orderList = select("orderId").from("TrackingCodeAndOrderHeader").where(conditionList).queryList();
            for (o in orderList) {
                totalOrders += o.orderId;
            }
            
            dateBeginFormatted = dateIntervals.getDateFormatter().format(dateIntervals.getDateBegin());           
            Map newMap = [:];
            newMap.put("totalOrders", totalOrders);                
            newMap.put("totalVisits", totalVisits);
            resultMap.put(dateBeginFormatted, newMap);
            
            dateEnd = dateIntervals.getDateEnd();
            if (thruDateTimestamp && dateIntervals.getDateEnd() < thruDateTimestamp)
                dateEnd = thruDateTimestamp;
            dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateEnd, context.locale, context.timeZone);
        }
    }
    return resultMap;
    
}
result = processResults();
context.result = result;

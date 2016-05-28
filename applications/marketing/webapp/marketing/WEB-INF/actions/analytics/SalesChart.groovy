import java.sql.Timestamp
import java.text.SimpleDateFormat

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilMisc

//salesOrderItemFacts = null;
//
//if (parameters.salesChannel) {
//    salesOrderItemFacts = delegator.findByAnd("SalesOrderItemFact", UtilMisc.toMap("saleChannel", parameters.salesChannel), null, false);
//} else {
//    salesOrderItemFacts = delegator.findList("SalesOrderItemFact", null, null, null, null, false);
//}
//
//context.salesOrderItemFacts = salesOrderItemFacts;
//
//for (item in salesOrderItemFacts) {
//    Debug.log("item ================> " + item);
//}
//contentCache = UtilCache.getOrCreateUtilCache("stats.order", 0, 0, 0, true, false);


//def begin, end,dailyStats,weeklyStats,monthlyStats;
//SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
//if(context.chartIntervalScope != null){
//    String iscope = context.chartIntervalScope; //day|week|month|year
//    int icount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : 0;
//    icount = icount *(-1);
//    if(iscope=="day"){
//        begin = UtilDateTime.getDayStart(nowTimestamp, icount, timeZone, locale);
//    }
//    if(iscope=="week"){
//        begin = UtilDateTime.getWeekStart(nowTimestamp, 0, icount, timeZone, locale);
//    }
//    if(iscope=="month"){
//        begin = UtilDateTime.getMonthStart(nowTimestamp, 0, icount, timeZone, locale);
//    }
//    if(iscope=="year"){
//        begin = UtilDateTime.getYearStart(nowTimestamp, 0, icount, timeZone, locale);
//    }
//}else{
//    begin = UtilDateTime.getYearStart(nowTimestamp, timeZone, locale);
//}

//end = UtilDateTime.getYearEnd(nowTimestamp, timeZone, locale);
//beginText = sdf.format(begin);
//endText = sdf.format(end);
//cacheId = "order_"+begin+"-"+end;

Map processResult() {
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
        iCount = UtilDateTime.getIntervalDefaultCount(iScope);        
        fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);
        if (iScope.equals("quarter")) iCount = Math.round(iCount / 3);
        if (iScope.equals("semester")) iCount = Math.round(iCount / 6);
    }
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
    Debug.log("dateBegin ===========> " + dateIntervals["dateBegin"] + "  dateEnd =================> " + dateIntervals["dateEnd"]);
    
    if (thruDateTimestamp && dateIntervals["dateEnd"] < thruDateTimestamp)
        dateIntervals["dateEnd"] = thruDate;

    fromDateText = UtilDateTime.timeStampToString(fromDateTimestamp, "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);
    thruDateText = null;
    if (thruDateTimestamp)
        thruDateText = UtilDateTime.timeStampToString(thruDateTimestamp, "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);
        
    Map resultMap = new TreeMap<String, Object>();
    for (int i = 0; i <= iCount; i++) {
        Map findOrderMap = dispatcher.runSync("findOrdersFull", UtilMisc.toMap("minDate", fromDateText, "maxDate", thruDateText,"userLogin",userLogin));
        List orderList = findOrderMap.orderList;

        orderList.each { header ->        
            String date = dateIntervals["dateFormatter"].format(header.orderDate);
            if (resultMap.get(date) != null) {
                Map newMap = resultMap.get(date);
                BigDecimal total = newMap.get("total");
                total = total.plus(header.grandTotal ?: BigDecimal.ZERO);
                newMap.put("total", total);
                int count = newMap.get("count");
                newMap.put("count", count+1);
                newMap.put("pos", date);
                resultMap.put(date, newMap);
            } else {
                Map newMap = [:];
                BigDecimal total = BigDecimal.ZERO;
                total = total.plus(header.grandTotal ?: BigDecimal.ZERO);
                newMap.put("total", total);
                newMap.put("count", 1);
                newMap.put("pos", date);
                resultMap.put(date,newMap);
            }
        }
        
        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntervals["dateBegin"], context.locale, context.timeZone);
        Debug.log("dateBegin ===========> " + dateIntervals["dateBegin"] + "  dateEnd =================> " + dateIntervals["dateEnd"]);
        fromDateText = UtilDateTime.timeStampToString(dateIntervals["dateBegin"], "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);
        thruDateText = null;
        if (thruDateTimestamp)
            thruDateText = UtilDateTime.timeStampToString(dateIntervals["dateEnd"], "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);
    }
    return resultMap;
}


//if (contentCache.get(cacheId) == null) {
//    GenericValue userLogin = context.get("userLogin");
//    Map cacheMap = [:];
//    dailyStats = processResult();    
//    cacheMap.dailyStats = processResult();

//    contentCache.put(cacheId,cacheMap);
//} else {
//    cacheMap = contentCache.get(cacheId);
//    dailyStats = cacheMap.dailyStats;
//}
sales = processResult();
Debug.log("sales ===========> " + sales);
context.sales = sales;

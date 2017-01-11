import java.sql.Timestamp
import java.text.SimpleDateFormat

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilMisc


Map processResult() {
    salesChannels = (parameters.salesChannel) ? [parameters.salesChannel] : [];
    
    int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : -1;
    String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year
    
    // Check and sanitize fromDate/thruDate params
    fromDate = parameters.fromDate;
    thruDate = parameters.thruDate;
    Timestamp fromDateTimestamp = null;
    Timestamp thruDateTimestamp = null;
    if (fromDate)
        fromDateTimestamp = UtilDateTime.stringToTimeStamp(fromDate, "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);    
    if (thruDate)
        thruDateTimestamp = UtilDateTime.stringToTimeStamp(thruDate, "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);           
    if (fromDateTimestamp && !fromDateTimestamp.before(thruDateTimestamp)) {
        fromDateTimestamp = null;
        thruDateTimestamp = null;
    }  
    if (!fromDateTimestamp) {
        iCount = UtilDateTime.getIntervalDefaultCount(iScope);        
        fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);        
    }
    if (iScope.equals("quarter")) iCount = Math.round(iCount / 3);
    if (iScope.equals("semester")) iCount = Math.round(iCount / 6);    
   
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
    dateEnd = dateIntervals.getDateEnd();
    if (thruDateTimestamp && dateEnd.before(thruDateTimestamp))
        dateEnd = thruDateTimestamp;

    fromDateText = UtilDateTime.timeStampToString(dateIntervals.getDateBegin(), "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);
    thruDateText = UtilDateTime.timeStampToString(dateEnd, "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);
        
    Map resultMap = new TreeMap<String, Object>();
    for (int i = 0; i < iCount; i++) {
        Map findOrderMap = dispatcher.runSync("findOrdersFull", UtilMisc.toMap("salesChannelEnumId", salesChannels, "minDate", fromDateText, "maxDate", thruDateText,"userLogin",userLogin));
        List orderList = findOrderMap.orderList;
        orderList.each { header ->        
            String date = dateIntervals.getDateFormatter().format(header.orderDate);
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
        
        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntervals.getDateBegin(), context.locale, context.timeZone);
        dateEnd = dateIntervals.getDateEnd();
        if (thruDateTimestamp && dateEnd.before(thruDateTimestamp))
            dateEnd = thruDateTimestamp;
        fromDateText = UtilDateTime.timeStampToString(dateIntervals.getDateBegin(), "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);
        thruDateText = UtilDateTime.timeStampToString(dateEnd, "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);
    }
    return resultMap;
}

sales = processResult();
context.sales = sales;

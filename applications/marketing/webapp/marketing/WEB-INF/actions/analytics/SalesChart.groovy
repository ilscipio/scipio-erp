import java.sql.Timestamp
import java.text.SimpleDateFormat

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.base.util.UtilMisc


Map processResult() {
    final def module = "SalesChart.groovy";
    def debugMode = Debug.verboseOn();
    //debugMode = true;
    
    salesChannels = (parameters.salesChannel) ? [parameters.salesChannel] : [];
    
    String iScope = context.chartIntervalScope;
    if (!iScope) { // Screen must set this, either from user or a default
        return null;
    }
    int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : -1;
    fromDate = parameters.fromDate;
    thruDate = parameters.thruDate;

    if (debugMode) {
        Debug.logInfo("Fields (input): fromDate: " + fromDate
            + "; thruDate: " + thruDate
            + "; iCount: " + iCount
            + "; iScope: " + iScope
            , module);
    }
    
    // Check and sanitize fromDate/thruDate params
    Timestamp fromDateTimestamp = null;
    Timestamp thruDateTimestamp = null;
    Timestamp nowTimestamp = context.nowTimestamp;
    if (fromDate)
        fromDateTimestamp = UtilDateTime.stringToTimeStamp(fromDate, "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);    
    if (thruDate)
        thruDateTimestamp = UtilDateTime.stringToTimeStamp(thruDate, "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);           
    if (fromDateTimestamp && thruDateTimestamp && fromDateTimestamp.after(thruDateTimestamp)) {
        // switch these automatically for the user... saves him time this way
        oldThruDateTimestamp = thruDateTimestamp
        thruDateTimestamp = fromDateTimestamp;
        fromDateTimestamp = oldThruDateTimestamp;
    }
    // Make sure we have a fromDateTimestamp, and either iCount or thruDateTimestamp (otherwise endless loop!)
    if (!fromDateTimestamp) {
        // Determine an appropriate fromDateTimestamp and iCount (if not set)
        if (iCount < 0) {
            iCount = UtilDateTime.getIntervalDefaultCount(iScope);
            if (iCount >= 0) {
                // FIXME?: why isn't this taken into account by values in UtilDateTime?
                if (iScope.equals("quarter")) iCount = Math.round(iCount / 3);
                if (iScope.equals("semester")) iCount = Math.round(iCount / 6);
            }
        }      
        fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);        
    } else if (!thruDateTimestamp && iCount < 0) {
        if (fromDateTimestamp.after(nowTimestamp)) {
            // fallback: If fromDate in the future, select an appropriate default iCount...
            // but having fromDate in future doesn't make much sense anyway
            iCount = UtilDateTime.getIntervalDefaultCount(iScope);
            if (iCount >= 0) {
                // FIXME?: why isn't this taken into account by values in UtilDateTime?
                if (iScope.equals("quarter")) iCount = Math.round(iCount / 3);
                if (iScope.equals("semester")) iCount = Math.round(iCount / 6);
            }
        } else {
            // If fromDate in the past, user probably wanted all orders up until today
            // (we must have a cutoff point, due to way loop is written)
            thruDateTimestamp = nowTimestamp;
        }
    }
    
    if (debugMode) {
        Debug.logInfo("Fields (adjusted): fromDate: " + fromDateTimestamp
            + "; thruDate: " + thruDateTimestamp
            + "; iCount: " + iCount
            + "; iScope: " + iScope
            , module);
    }
    
    // Calculate the max thruDate, which is either thruDateTimestamp or (fromDateTimestamp+(iCount*iScope))
    maxThruDateTimestamp = thruDateTimestamp;
    if (!maxThruDateTimestamp) {
        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, iCount, fromDateTimestamp, context.locale, context.timeZone);
        maxThruDateTimestamp = dateIntervals.getDateEnd();
    }
    
    // Perform main DB query
    queryMinDate = UtilDateTime.timeStampToString(fromDateTimestamp, "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);
    queryMaxDate = UtilDateTime.timeStampToString(maxThruDateTimestamp, "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);
    if (debugMode) {
        Debug.logInfo("Fields (query): minDate: " + queryMinDate
            + "; maxDate: " + queryMaxDate
            + "; maxThruDateTimestamp: " + maxThruDateTimestamp
            , module);
    }
    Map findOrderMap = dispatcher.runSync("findOrdersFull", UtilMisc.toMap("salesChannelEnumId", salesChannels,
        "minDate", queryMinDate, "maxDate", queryMaxDate, "userLogin", userLogin));
    List orderList = findOrderMap.orderList;
    
    // Organize results
    //Map resultMap = new TreeMap<String, Object>();
    Map resultMap = new LinkedHashMap<String, Object>(); // preserve the insertion order; the results are ordered by -orderDate
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
    dateFormatter = dateIntervals.getDateFormatter();
    
    // create zero-value data points (ON default)
    createZeroEntries = context.createZeroEntries;
    if (createZeroEntries == null) {
        createZeroEntries = true;
    }
    int i = 0;
    if (createZeroEntries) {
        // Loop intervals until reach iCount (if set) or until pass thruDate (if set)
        while ((iCount < 0 || i < iCount) && !(thruDateTimestamp && dateIntervals.getDateBegin().after(thruDateTimestamp))) {
            String date = dateFormatter.format(dateIntervals.getDateBegin());
            //Debug.logInfo("DATE: " + date, module);
            resultMap.put(date, ["total": BigDecimal.ZERO, "count": 0, "pos": date]);
    
            // Get next date interval; NOTE: duplicated above
            dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntervals.getDateBegin(), context.locale, context.timeZone);
            i++;
        }
    }
    
    // create first and last zero-value data points (OFF default)
    createFirstLastZeroEntries = context.createFirstLastZeroEntries;
    if (createFirstLastZeroEntries == null) {
        createFirstLastZeroEntries = false
    }
    if (createFirstLastZeroEntries) {
        String date = dateFormatter.format(dateIntervals.getDateBegin());
        resultMap.put(date, ["total": BigDecimal.ZERO, "count": 0, "pos": date]);
        date = dateFormatter.format(maxThruDateTimestamp);
        resultMap.put(date, ["total": BigDecimal.ZERO, "count": 0, "pos": date]);
        
        Debug.logInfo("resultMap: " + resultMap, module);
    }
    
    if (orderList) {
        // iterate in reverse order because the service orders by -orderDate
        orderList.reverseEach { header ->
            String date = dateFormatter.format(header.orderDate);
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
                resultMap.put(date, newMap);
            }
            //Debug.logInfo("DATE: " + date, module);
        }
    }
    
    if (debugMode) {
        Debug.logInfo("Number of results: " + resultMap.size() 
            + "; number of zero-entry loop iterations: " + i, module);
    }
    return resultMap;
}

sales = processResult();
context.sales = sales;

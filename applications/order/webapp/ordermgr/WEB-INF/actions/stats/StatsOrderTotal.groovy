import java.math.BigDecimal;
import java.util.*;
import java.sql.Timestamp;
import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.base.util.*;

import com.ibm.icu.text.SimpleDateFormat;
import org.ofbiz.base.util.cache.UtilCache;

beginText = "";
endText = "";
contentCache = UtilCache.getOrCreateUtilCache("stats.order", 0, 0, 0, true);
Map processResult() {
    iCount = (context.chartIntervalCount != null) ? context.chartIntervalCount : 0;
    String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year    
    if (iCount <= 0)
        iCount = UtilDateTime.getIntervalDefaultCount(iScope);
    fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
    
    Map resultMap = new TreeMap<String, Object>();
    GenericValue userLogin = context.get("userLogin");
    for (int i = 0; i < iCount; i++) {
        beginText = UtilDateTime.timeStampToString(dateIntervals.getDateBegin(), "yyyy-MM-dd HH:mm:ss", context.timeZone, context.locale);
        endText = UtilDateTime.timeStampToString(dateIntervals.getDateEnd(), "yyyy-MM-dd HH:mm:ss", context.timeZone, context.locale);
        
        
        Map findOrderMap = dispatcher.runSync("findOrdersFull", UtilMisc.toMap("minDate", beginText, "maxDate", endText, "userLogin", userLogin));
        orderList = findOrderMap.orderList;
        Map newMap = [:];
        if (orderList) {
            orderList.each { header ->          
                BigDecimal total = (newMap.get("total") ?: BigDecimal.ZERO);
                total = total.plus(header.grandTotal ?: BigDecimal.ZERO);
                newMap.put("total", total);
            }
        } else {
            newMap.put("total", BigDecimal.ZERO);
        }
        newMap.put("count", findOrderMap.orderListSize);
        resultMap.put(dateIntervals.getDateFormatter().format(dateIntervals.getDateBegin()), newMap);
        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntervals.getDateBegin(), context.locale, context.timeZone);
    }
    return resultMap;
}
orderStats = processResult();

context.orderStats = orderStats;
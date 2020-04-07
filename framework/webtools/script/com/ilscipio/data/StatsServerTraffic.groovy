/**
 * This is based on the new SCIPIO getServerRequests services. Sample implementation only
 */
import java.sql.Timestamp;
import org.ofbiz.base.util.*;

Map findDataMap = [:];
Timestamp nowTimestamp = UtilDateTime.nowTimestamp();
switch (context.chartIntervalScope ?: "day") {
    case "hour":
        begin = UtilDateTime.getHourStart(nowTimestamp, 0, timeZone, locale);
        break;

    case "day":
        begin = UtilDateTime.getDayStart(nowTimestamp, 0, timeZone, locale);
        break;

    case "week":
        begin = UtilDateTime.getWeekStart(nowTimestamp, 0, timeZone, locale);
        break;

    case "month":
        begin = UtilDateTime.getMonthStart(nowTimestamp, 0, timeZone, locale);
        break;

    case "year":
        begin = UtilDateTime.getYearStart(nowTimestamp, 0, timeZone, locale);
        break;

    default:
        begin = UtilDateTime.getDayStart(nowTimestamp, 0, timeZone, locale);
}

try {
    //findDataMap = dispatcher.runSync("getServerRequests", UtilMisc.toMap("fromDate",begin,"thruDate",nowTimestamp,"dateInterval",context.chartIntervalScope?context.chartIntervalScope:"day","userLogin",userLogin));
    findDataMap = dispatcher.runSync("getSavedHitBinLiveData", ["fromDate": begin, "useCache": true, "maxRequests": context.maxRequestsEntries, "userLogin": userLogin]);
    chartData = findDataMap.requests;
    context.chartData = chartData;
} catch(Exception e) {
    Debug.logError(e, "Cannot fetch request data", module); // NOTE: Because call is switched this shouldn't happen anymore...
}


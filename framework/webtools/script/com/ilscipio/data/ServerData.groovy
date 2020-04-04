/**
 * Implements websocket services under framework/webtools/servicedef/services.xml
 */

import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.model.DynamicViewEntity;
import org.ofbiz.entity.util.*;
import org.ofbiz.base.util.*;
import org.ofbiz.service.ServiceUtil;
import java.text.SimpleDateFormat;
import java.sql.Timestamp;
import com.ibm.icu.util.Calendar;

delegator = request.getAttribute("delegator");


/**
 * Looks up current server stats and returns a list of maps containing current information for the day
 * @return Map with the result of the service, the output parameters.
 * */
public Map getServerRequestsThisHour() {
    Map result = ServiceUtil.returnSuccess();
    Timestamp startDate = UtilDateTime.getHourStart(context.nowTimestamp, context.timeZone, context.locale); // Today (Begin)
    Timestamp endDate = UtilDateTime.getHourEnd(startDate, context.timeZone, context.locale);    // Today (End)
    
    try {
        Map findDataMap = dispatcher.runSync("getServerRequests", UtilMisc.toMap("fromDate",startDate,"thruDate",endDate,"dateInterval","minute","userLogin",userLogin));
        result = findDataMap;
    }catch(Exception e){
        result = ServiceUtil.returnError("Cannot fetch request data");
    }
    return result;
}

/**
 * Looks up current server stats and returns a list of maps containing current information for the day
 * @return Map with the result of the service, the output parameters.
 * */
public Map getServerRequestsToday() {
    Map result = ServiceUtil.returnSuccess();
    Timestamp startDate = UtilDateTime.getDayStart(context.nowTimestamp, context.timeZone, context.locale); // Today (Begin)
    Timestamp endDate = UtilDateTime.getDayEnd(startDate, context.timeZone, context.locale);    // Today (End)
    
    try {
        Map findDataMap = dispatcher.runSync("getServerRequests", UtilMisc.toMap("fromDate",startDate,"thruDate",endDate,"dateInterval","hour","userLogin",userLogin));
        result = findDataMap;
    }catch(Exception e){
        result = ServiceUtil.returnError("Cannot fetch request data");
    }
    return result;
}

/**
 * Looks up current server stats and returns a list of maps containing current information for the week
 * @return Map with the result of the service, the output parameters.
 * */
public Map getServerRequestsThisWeek() {
    Map result = ServiceUtil.returnSuccess();
    Timestamp startDate = UtilDateTime.getWeekStart(context.nowTimestamp, context.timeZone, context.locale); // This week (Begin)
    Timestamp endDate = UtilDateTime.getWeekEnd(startDate, context.timeZone, context.locale); // This week (End)
    
    try {
        Map findDataMap = dispatcher.runSync("getServerRequests", UtilMisc.toMap("fromDate",startDate,"thruDate",endDate,"dateInterval","day","userLogin",userLogin));
        result = findDataMap;
    }catch(Exception e){
        result = ServiceUtil.returnError("Cannot fetch request data");
    }
    return result;
}

/**
 * Looks up current server stats and returns a list of maps containing current information for the month
 * @return Map with the result of the service, the output parameters.
 * */
public Map getServerRequestsThisMonth() {
    Map result = ServiceUtil.returnSuccess();
    Timestamp startDate = UtilDateTime.getMonthStart(context.nowTimestamp, context.timeZone, context.locale); // This month (Begin)
    Timestamp endDate = UtilDateTime.getMonthEnd(startDate, context.timeZone, context.locale); // This month (End)
    
    try {
        Map findDataMap = dispatcher.runSync("getServerRequests", UtilMisc.toMap("fromDate",startDate,"thruDate",endDate,"dateInterval","day","userLogin",userLogin));
        result = findDataMap;
    }catch(Exception e){
        result = ServiceUtil.returnError("Cannot fetch request data");
    }
    return result;
}

/**
 * Looks up current server stats and returns a list of maps containing current information for the last day
 * @return Map with the result of the service, the output parameters.
 * */
public Map getServerRequestsThisYear() {
    Map result = ServiceUtil.returnSuccess();
    Timestamp startDate = UtilDateTime.getYearStart(context.nowTimestamp, timeZone, locale);  // This Year (Begin)
    Timestamp endDate = UtilDateTime.getYearEnd(startDate, timeZone, locale); // This Year (End)
    
    try {
        Map findDataMap = dispatcher.runSync("getServerRequests", UtilMisc.toMap("fromDate",startDate,"thruDate",endDate,"dateInterval","month","userLogin",userLogin));
        result = findDataMap;
    }catch(Exception e){
        result = ServiceUtil.returnError("Cannot fetch request data");
    }
    return result;
}

/**
 * Looks up current server stats and returns a list of maps containing current information.
 * Parameter "dateInterval" can be set to specify the date interval (hour,day,week,month,year)
 * @return Map with the result of the service, the output parameters.
 */
public Map getServerRequests() {
    Map result = ServiceUtil.returnSuccess();
    String dateInterval = context.dateInterval != null ? context.dateInterval : "day";
    Integer bucketMinutes = context.bucketMinutes;
    // SPECIAL: we must remove seconds otherwise the returned values might be outside since they remove seconds
    Timestamp fromDate = UtilDateTime.getMinuteBasedTimestamp(context.fromDate);
    if (fromDate.getTime() != context.fromDate.getTime()) {
        Debug.logWarning("getServerRequests: Invalid fromDate, should not have seconds or milliseconds, stripped: " + context.fromDate, module);
    }
    Timestamp thruDate = UtilDateTime.getMinuteBasedTimestamp(context.thruDate);
    if (thruDate != null && thruDate.getTime() != context.thruDate.getTime()) {
        Debug.logWarning("getServerRequests: Invalid thruDate, should not have seconds or milliseconds, stripped: " + context.thruDate, module);
    }
    if (thruDate == null) {
        thruDate = UtilDateTime.nowTimestamp();
    }
    try {
        EntityListIterator dataList = null;
        try {
            try {
                // For most SQL Databases use this
                Map sqlFunctionMap = UtilMisc.toMap("year", "extract-year", "month", "extract-month", "day", "extract-day", "hour", "extract-hour", "minute", "extract-minute");
                dataList = getDataFromDB(sqlFunctionMap, fromDate, thruDate);
            } catch (Exception e) { // FIXME: swallows obscure errors
                //Fallback for derby
                Map sqlFunctionMap = UtilMisc.toMap("year", "year", "month", "month", "day", "day", "hour", "hour", "minute", "minute");
                dataList = getDataFromDB(sqlFunctionMap, fromDate, thruDate);
            }
            result.put("requests", processResult(dataList, fromDate, thruDate, dateInterval, bucketMinutes));
        } finally {
            if (dataList != null) {
                dataList.close();
            }
        }
    } catch(Exception e) {
        result = ServiceUtil.returnError("Cannot fetch request data");
    }
    return result;
}

public EntityListIterator getDataFromDB(Map sqlFunctionMap, Timestamp fromDate, Timestamp thruDate) {
    //SQL magic
    DynamicViewEntity dve = new DynamicViewEntity();
    dve.addMemberEntity("SH", "ServerHit");
    dve.addAlias("SH", "contentId", null, null, null, Boolean.TRUE, null);
    dve.addAlias("SH", "hitStartDateTime", null, null, null, Boolean.TRUE, null);
    dve.addAlias("SH", "hitTypeId", null, null, null, Boolean.TRUE, null);
    dve.addAlias("SH", "referrerUrl", null, null, null, Boolean.FALSE, null);

    // Split date in order to group at a later stage
    dve.addAlias("SH", "year", "hitStartDateTime",  null, null, Boolean.TRUE, sqlFunctionMap.year);
    dve.addAlias("SH", "month", "hitStartDateTime",  null, null, Boolean.TRUE, sqlFunctionMap.month);
    dve.addAlias("SH", "day", "hitStartDateTime",  null, null, Boolean.TRUE, sqlFunctionMap.day);
    dve.addAlias("SH", "hour", "hitStartDateTime",  null, null, Boolean.TRUE, sqlFunctionMap.hour);
    dve.addAlias("SH", "minute", "hitStartDateTime",  null, null, Boolean.TRUE, sqlFunctionMap.minute);
    dve.addAlias("SH", "contentIdCount", "contentId", null, null, null, "count");

    def ecl = [
            EntityCondition.makeCondition("hitTypeId", EntityOperator.EQUALS, "REQUEST"),
            EntityCondition.makeCondition("hitStartDateTime", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate)];
    if (thruDate != null) {
        ecl.add(EntityCondition.makeCondition("hitStartDateTime", EntityOperator.LESS_THAN, thruDate))
    }
    return select("contentIdCount","contentId","year","month","day","hour","minute").from(dve).where(ecl).queryIterator();
}

public Map processResult(EntityListIterator resultList, Timestamp fromDate, Timestamp thruDate, String dateInterval, Integer bucketMinutes) {
    long fromDateMs = fromDate.getTime();
    Long bucketMs = (bucketMinutes != null) ? bucketMinutes * 60 * 1000 : null;
    if (bucketMs == null) {
        Debug.logError("MISSING bucketMs", module)
    }
    Map dateMap = new TreeMap<Date, Object>();
    //Calendar startD = UtilDateTime.toCalendar(fromDate);
    //Calendar endD = UtilDateTime.toCalendar(thruDate);
    SimpleDateFormat sdf;

    // create new ordered Map of all upcoming dates
    /*
    if(dateInterval.equals("minute")){sdf = new SimpleDateFormat("hh:mm");}
    if(dateInterval.equals("hour")){sdf = new SimpleDateFormat("MM-dd hh");}
    if(dateInterval.equals("day")){sdf = new SimpleDateFormat("MM-dd");}
    if(dateInterval.equals("week")){sdf = new SimpleDateFormat("yy-w")}
    if(dateInterval.equals("month")){sdf = new SimpleDateFormat("yy-MM")}
    if(dateInterval.equals("year")){sdf = new SimpleDateFormat("yy")}
    */
    sdf = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm")

    /*
    while(!startD.after(endD))
    {
        int year = startD.get(Calendar.YEAR);
        int month = startD.get(Calendar.MONTH) + 1;
        int day = startD.get(Calendar.DAY_OF_MONTH);
        int hour = startD.get(Calendar.HOUR_OF_DAY);
        int minute = startD.get(Calendar.MINUTE);
        
        // Create new Map
        Map newMap = [:];
        newMap.put("count", 0);
        newMap.put("contentIds",new ArrayList());
        dateMap.put(sdf.format(startD.getTime()),newMap);
        
        if(dateInterval.equals("minute")){startD.add(Calendar.MINUTE, 1);}
        if(dateInterval.equals("hour")){startD.add(Calendar.HOUR, 1);}
        if(dateInterval.equals("day")){startD.add(Calendar.DATE, 1);}
        if(dateInterval.equals("week")){startD.add(Calendar.WEEK_OF_MONTH, 1);}
        if(dateInterval.equals("month")){startD.add(Calendar.MONTH, 1);}
        if(dateInterval.equals("year")){startD.add(Calendar.YEAR, 1);}
    }*/
    GenericValue p;
    while((p = resultList.next()) != null) {
        String year = p.get("year");
        String month = p.get("month");
        String day = p.get("day");
        String hour = p.get("hour");
        String minute = p.get("minute");

        Date pDate = UtilDateTime.toDate(month, day, year, hour, minute, "0");
        if (bucketMs != null) {
            // readjust the date to nearest bucket (NOTE: the seconds removal does not matter, have to do this anyway)
            pDate = UtilDateTime.getTimestamp(fromDateMs + ((pDate.getTime() - fromDateMs).intdiv(bucketMs) * bucketMs));
        }
        String dateString = sdf.format(pDate);
        
        if(dateMap.get(dateString) != null){
            Map currentMap = dateMap.get(dateString);
            long currCount = currentMap.get("count"); // fixed, not ints
            long addCount = p.get("contentIdCount");
            if(addCount!=null){
                currentMap.put("count",(currCount+addCount));
            }
            List currentContentIdList = currentMap.get("contentIds");
            if(currentContentIdList!=null){
                String addContentId = p.get("contentId");
                if(!currentContentIdList.contains(addContentId))currentContentIdList.add(addContentId);
                currentMap.put("contentIds",currentContentIdList);
            }
        }else{
            Map newMap = [:];
            newMap.put("count", p.get("contentIdCount"));
            List contentIds = new ArrayList();
            contentIds.add(p.get("contentId"));
            newMap.put("contentIds", contentIds);
            dateMap.put(dateString,newMap);
        }
    }

    return dateMap;
}

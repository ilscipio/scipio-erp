import org.ofbiz.security.Security;
import java.util.*;
import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.model.DynamicViewEntity;
import org.ofbiz.entity.model.ModelViewEntity.ComplexAlias;
import org.ofbiz.entity.model.ModelViewEntity.ComplexAliasField;
import org.ofbiz.base.util.*;
import org.ofbiz.webapp.stats.*;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.base.util.Debug;
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
    try {
        List dataList = new ArrayList();
        try{
            // For most SQL Databases use this
            Map sqlFunctionMap = UtilMisc.toMap("year","extract-year","month","extract-month","day","extract-day","hour","extract-hour","minute","extract-minute");
            dataList = getDataFromDB(sqlFunctionMap,context.fromDate, context.thruDate);
        }catch(Exception e){
            //Fallback for derby
            Map sqlFunctionMap = UtilMisc.toMap("year","year","month","month","day","day","hour","hour","minute","minute");
            dataList = getDataFromDB(sqlFunctionMap,context.fromDate, context.thruDate);
        }
        result.put("requests",processResult(dataList,context.fromDate,context.thruDate,dateInterval));
    }catch(Exception e){
        result = ServiceUtil.returnError("Cannot fetch request data");
    }
    return result;
}

public List getDataFromDB(Map sqlFunctionMap, Timestamp fromdate, Timestamp thrudate){
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
    
    EntityCondition ecl = EntityCondition.makeCondition([
        EntityCondition.makeCondition("hitTypeId", EntityOperator.EQUALS, "REQUEST"),
        EntityCondition.makeCondition("hitStartDateTime", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate),
        EntityCondition.makeCondition("hitStartDateTime", EntityOperator.LESS_THAN_EQUAL_TO, thruDate)
    ],
    EntityOperator.AND);
    return select("contentIdCount","contentId","year","month","day","hour","minute").from(dve).where(ecl).queryList();
    
}


public Map processResult(List resultList,Timestamp startDate,Timestamp endDate,String dateInterval) {
    Map dateMap = new TreeMap<Date, Object>();
    Calendar startD = UtilDateTime.toCalendar(startDate);
    Calendar endD = UtilDateTime.toCalendar(endDate);
    SimpleDateFormat sdf;

    // create new ordered Map of all upcoming dates
    if(dateInterval.equals("minute")){sdf = new SimpleDateFormat("hh:mm");}
    if(dateInterval.equals("hour")){sdf = new SimpleDateFormat("MM-dd hh");}
    if(dateInterval.equals("day")){sdf = new SimpleDateFormat("MM-dd");}
    if(dateInterval.equals("week")){sdf = new SimpleDateFormat("yy-w")}
    if(dateInterval.equals("month")){sdf = new SimpleDateFormat("yy-MM")}
    if(dateInterval.equals("year")){sdf = new SimpleDateFormat("yy")}
    
        
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
    }

    resultList.each {
        Map p = it;
        String year = p.get("year");
        String month = p.get("month");
        String day = p.get("day");
        String hour = p.get("hour");
        String minute = p.get("minute");
        
        Date pDate = UtilDateTime.toDate(month,day,year,hour,minute,"0");
        
        String dateString = sdf.format(pDate);
        
        if(dateMap.get(dateString) != null){
            Map currentMap = dateMap.get(dateString);
            int currCount = currentMap.get("count");
            int addCount = p.get("contentIdCount");
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

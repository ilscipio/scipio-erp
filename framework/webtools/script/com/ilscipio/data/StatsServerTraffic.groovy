import org.ofbiz.security.Security;

import org.ofbiz.webapp.stats.*;
import java.util.Date;
import java.text.SimpleDateFormat;
import java.sql.Timestamp;
import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.base.util.*;
import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.model.DynamicViewEntity;
import org.ofbiz.entity.model.ModelViewEntity.ComplexAlias;
import org.ofbiz.entity.model.ModelViewEntity.ComplexAliasField;



/**
 * This is based on the new SCIPIO getServerRequests services. Sample implementation only
 * */
Map findDataMap = [:];
Timestamp nowTimestamp = UtilDateTime.nowTimestamp();
Timestamp begin = nowTimestamp;
switch (context.chartIntervalScope?context.chartIntervalScope:"day"){
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
    findDataMap = dispatcher.runSync("getServerRequests", UtilMisc.toMap("fromDate",begin,"thruDate",nowTimestamp,"dateInterval",context.chartIntervalScope?context.chartIntervalScope:"day","userLogin",userLogin));
    result = findDataMap;
}catch(Exception e){
    result = ServiceUtil.returnError("Cannot fetch request data");
}

context.chartData = findDataMap.requests;

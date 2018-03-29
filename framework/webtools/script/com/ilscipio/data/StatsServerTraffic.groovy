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
Timestamp startDate = UtilDateTime.getMonthStart(context.nowTimestamp, context.timeZone, context.locale); // This month (Begin)
Timestamp endDate = UtilDateTime.getMonthEnd(startDate, context.timeZone, context.locale); // This month (End)

try {
    findDataMap = dispatcher.runSync("getServerRequests", UtilMisc.toMap("fromDate",startDate,"thruDate",endDate,"dateInterval","day","userLogin",userLogin));
    result = findDataMap;
}catch(Exception e){
    result = ServiceUtil.returnError("Cannot fetch request data");
}

context.chartData = findDataMap.requests;

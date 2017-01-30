import java.sql.Timestamp
import java.text.SimpleDateFormat

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityJoinOperator
import org.ofbiz.entity.condition.EntityOperator
import org.ofbiz.entity.util.EntityUtilProperties;

import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.common.uom.UomWorker;
import org.ofbiz.common.uom.SimpleUomRateConverter;

// NOTE: See SalesChart.groovy for more complete comments

// FIXME: revisit reuse pattern
asutil = GroovyUtil.getScriptFromLocation("component://marketing/webapp/marketing/WEB-INF/actions/analytics/AnalyticsScriptUtil.groovy", binding);
// also works here:
//asutil = GroovyUtil.getScriptClassFromLocation("component://marketing/webapp/marketing/WEB-INF/actions/analytics/AnalyticsScriptUtil.groovy").newInstance();
//asutil.setBinding(binding);

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
    final def module = "TrackingCodeChart.groovy";
    asutil.readDebugMode();
    asutil.readZeroEntriesParams();
     
    trackingCodeId = parameters.trackingCodeId;
    marketingCampaignId = parameters.marketingCampaignId;

    //asutil.readProductStoreParams();
    //asutil.readCurrencyUomParams();
    asutil.readChartTimeParams();
    asutil.readDateIntervalsFormatter();
        
    Map resultMap = new LinkedHashMap<String, Object>();
    
    // Do main queries
    // FIXME: QUERY AND RESULT PROCESSING NON-FUNCTIONAL
    
    trackCondList = [];
    if (marketingCampaignId && !trackingCodeId) {
        trackingCodeList = select("trackingCodeId").from("TrackingCode").where(["marketingCampaignId" : marketingCampaignId]).queryList();
        trackingCodes = [];
        for (trackingCode in trackingCodeList)
            trackingCodes += trackingCode.trackingCodeId;
        if (trackingCodes)
            trackCondList.add(EntityCondition.makeCondition("trackingCodeId", EntityOperator.IN, trackingCodes));
    } else if (trackingCodeId) {
        trackCondList.add(EntityCondition.makeCondition("trackingCodeId", EntityOperator.EQUALS, trackingCodeId));
    } else {
        return null;
    }

    //asutil.checkCreateZeroEntries(resultMap, ["totalOrders": 0, "totalVisits": 0]);
    
    def trackingStats = [ "totalOrders": 0, "totalVisits": 0];
    
    def dateIntv = dateIntervals;
    // Loop intervals until reach iCount (if set) or until pass thruDate (if set) (NOTE: thruDate is inclusive)
    int i = 0;
    while ((iCount < 0 || i < iCount) && !(thruDateTimestamp && dateIntv.getDateBegin().after(thruDateTimestamp))) {
        dateEnd = dateIntv.getDateEnd();
        if (thruDateTimestamp && thruDateTimestamp.before(dateIntv.getDateEnd()))
            dateEnd = thruDateTimestamp;
        
        int totalVisits = 0;
        int totalOrders = 0;
        conditionList = [];
        
        // Get visits
        conditionList = EntityCondition.makeCondition(EntityCondition.makeCondition(trackCondList), EntityJoinOperator.AND,
            EntityCondition.makeCondition([EntityCondition.makeCondition("fromDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntv.getDateBegin()),
            EntityCondition.makeCondition("fromDate", EntityOperator.LESS_THAN, dateEnd)], EntityJoinOperator.AND));
        visitList = select("visitId").from("TrackingCodeAndVisit").where(conditionList).queryList();
        for (v in visitList)
            totalVisits += v.visitId;
        // Get orders
        conditionList = EntityCondition.makeCondition(EntityCondition.makeCondition(trackCondList), EntityJoinOperator.AND,
                EntityCondition.makeCondition([EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntv.getDateBegin()),
                EntityCondition.makeCondition("orderDate", EntityOperator.LESS_THAN, dateEnd)], EntityJoinOperator.AND));
        orderList = select("orderId").from("TrackingCodeAndOrderHeader").where(conditionList).queryList();
        for (o in orderList) {
            totalOrders += o.orderId;
        }
        
        dateBeginFormatted = dateFormatter.format(dateIntv.getDateBegin());
        Map newMap = [:];
        newMap.put("totalOrders", totalOrders);
        newMap.put("totalVisits", totalVisits);
        newMap.put("pos", totalVisits);
        resultMap.put(dateBeginFormatted, newMap);
        
        dateIntv = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntv.getDateBegin(), context.locale, context.timeZone);
        i++;
    }
    
    if (debugMode) {
        Debug.logInfo("Number of results: " + resultMap.size(), module);
    }
    
    context.trackingStats = trackingStats;
    
    //asutil.storeProductStoreParams();
    //asutil.storeCurrencyUomParams();
    
    return resultMap;
}

result = processResults();
context.result = result;

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

//// use this helper to build a List of visits, orders, order totals, and conversion rates
//trackingCodeVisitAndOrders = ReportHelper.calcConversionRates(visits, orders, "trackingCodeId");
//context.trackingCodeVisitAndOrders = trackingCodeVisitAndOrders;
SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
currentYearBegin = UtilDateTime.getYearStart(nowTimestamp, timeZone, locale);
currentYearEnd  = UtilDateTime.getYearEnd(nowTimestamp, timeZone, locale);
currentYearBeginText = sdf.format(currentYearBegin);
currentYearEndText = sdf.format(currentYearEnd);
cacheId = "marketingTracking_" + currentYearBeginText + "-" + currentYearEndText;

def getIntervalDefaultCount(iScope) {
    def iCount = UtilDateTime.getIntervalDefaultCount(iScope);
    if (iCount >= 0) {
        // FIXME?: why isn't this taken into account by values in UtilDateTime?
        if (iScope.equals("quarter")) iCount = Math.round(iCount / 3);
        if (iScope.equals("semester")) iCount = Math.round(iCount / 6);
    }
    return iCount;
};

Map processResults() {
    final def module = "TrackingCodeChart.groovy";
    def debugMode = Debug.verboseOn();
    //debugMode = true;
    
    Map resultMap = [:];
        
    trackingCodeId = parameters.trackingCodeId;
    marketingCampaignId = parameters.marketingCampaignId;
    
    // FIXME: preparation duplicated from SalesChart.groovy

    productStoreId = parameters.productStoreId;
    productStore = null;
    if (productStoreId) {
        productStore = from("ProductStore").where("productStoreId", productStoreId).cache(true).queryOne();
        if (!productStore) {
            productStoreId = null;
        }
    }
    
    currencyUomId = parameters.currencyUomId;
    currencyUom = null;
    if (currencyUomId) {
        currencyUom = from("Uom").where("uomId", currencyUomId).cache(true).queryOne();
        if (!currencyUom) {
            currencyUomId = null;
        }
    }
    if (!currencyUomId) {
        // check productStore default currency
        currencyUomId = productStore?.defaultCurrencyUomId;
        if (!currencyUomId) {
            // global default lookup, based on org.ofbiz.order.shoppingcart.ShoppingCart
            currencyUomId = EntityUtilProperties.getPropertyValue("general.properties", "currency.uom.id.default", "USD", delegator);
        }
        currencyUom = from("Uom").where("uomId", currencyUomId).cache(true).queryOne();
    }
    
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
            iCount = getIntervalDefaultCount(iScope);
        }      
        fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);        
    } else if (!thruDateTimestamp && iCount < 0) {
        if (fromDateTimestamp.after(nowTimestamp)) {
            // fallback: If fromDate in the future, select an appropriate default iCount...
            // but having fromDate in future doesn't make much sense anyway
            iCount = getIntervalDefaultCount(iScope);
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
    
    
   
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);        
    if (thruDateTimestamp && dateIntervals["dateEnd"] < thruDateTimestamp)
        dateIntervals["dateEnd"] = thruDateTimestamp;
        
    exprList = [];
    if (marketingCampaignId && !trackingCodeId) {
        trackingCodeList = select("trackingCodeId").from("TrackingCode").where(["marketingCampaignId" : marketingCampaignId]).queryList();
        trackingCodes = [];
        for (trackingCode in trackingCodeList) 
            trackingCodes += trackingCode.trackingCodeId;
        if (trackingCodes)
            exprList.add(EntityCondition.makeCondition("trackingCodeId", EntityOperator.IN, trackingCodes));        
    } else if (trackingCodeId) {
        exprList.add(EntityCondition.makeCondition("trackingCodeId", EntityOperator.EQUALS, trackingCodeId));
    }
     
    if ((marketingCampaignId || trackingCodeId) && exprList) {
        
        for (int i = 0; i < iCount; i++) {            
            int totalVisits = 0;
            int totalOrders = 0;
            conditionList = [];
            
            // Get visits
            conditionList = EntityCondition.makeCondition(EntityCondition.makeCondition(exprList), EntityJoinOperator.AND, 
                EntityCondition.makeCondition([EntityCondition.makeCondition("fromDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()),
                EntityCondition.makeCondition("fromDate", EntityOperator.LESS_THAN, dateIntervals.getDateEnd())], EntityJoinOperator.AND));            
            visitList = select("visitId").from("TrackingCodeAndVisit").where(conditionList).queryList();
            for (v in visitList)
                totalVisits += v.visitId;                
            // Get orders
            conditionList = EntityCondition.makeCondition(EntityCondition.makeCondition(exprList), EntityJoinOperator.AND, 
                    EntityCondition.makeCondition([EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()),
                    EntityCondition.makeCondition("orderDate", EntityOperator.LESS_THAN, dateIntervals.getDateEnd())], EntityJoinOperator.AND));
            orderList = select("orderId").from("TrackingCodeAndOrderHeader").where(conditionList).queryList();
            for (o in orderList) {
                totalOrders += o.orderId;
            }
            
            dateBeginFormatted = dateIntervals.getDateFormatter().format(dateIntervals.getDateBegin());           
            Map newMap = [:];
            newMap.put("totalOrders", totalOrders);                
            newMap.put("totalVisits", totalVisits);
            resultMap.put(dateBeginFormatted, newMap);
            
            dateEnd = dateIntervals.getDateEnd();
            if (thruDateTimestamp && dateIntervals.getDateEnd() < thruDateTimestamp)
                dateEnd = thruDateTimestamp;
            dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateEnd, context.locale, context.timeZone);
        }
    }
    
    if (debugMode) {
        Debug.logInfo("Number of results: " + resultMap.size(), module);
    }
    
    context.productStoreId = productStoreId;
    context.productStore = productStore;
    context.currencyUomId = currencyUomId;
    context.currencyUom = currencyUom;
    
    return resultMap;
}
result = processResults();
context.result = result;

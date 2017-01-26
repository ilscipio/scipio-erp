/**
 * SCIPIO: Analytics chart script utility and common code.
 * 
 * Include this class from other scripts using:
 *   asutil = GroovyUtil.getScriptFromLocation("component://marketing/webapp/marketing/WEB-INF/actions/analytics/AnalyticsScriptUtil.groovy", binding);
 * OR
 *   asutil = GroovyUtil.getScriptClassFromLocation("component://marketing/webapp/marketing/WEB-INF/actions/analytics/AnalyticsScriptUtil.groovy")
 *              .newInstance();
 *   asutil.setBinding(binding);
 * Its methods may populate the caller's binding.
 */

import java.sql.Timestamp
import java.text.SimpleDateFormat

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityUtilProperties;

import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.common.uom.UomWorker;
import org.ofbiz.common.uom.SimpleUomRateConverter;
import org.ofbiz.service.engine.GroovyBaseScript

import groovy.transform.Field;

@Field BigDecimal ZERO = BigDecimal.ZERO;
@Field int scale = OrderReadHelper.scale;
@Field int rounding = OrderReadHelper.rounding;

//public AnalyticsScriptUtil(Binding binding) {
//    this.setBinding(binding);
//}

def getIntervalDefaultCount(iScope) {
    def iCount = UtilDateTime.getIntervalDefaultCount(iScope);
    if (iCount >= 0) {
        // FIXME?: why isn't this taken into account by values in UtilDateTime?
        if (iScope.equals("quarter")) iCount = (int) Math.round(iCount / 3);
        if (iScope.equals("semester")) iCount = (int) Math.round(iCount / 6);
    }
    return iCount;
}

def readDebugMode() {
    debugMode = Debug.verboseOn();
    //debugMode = true;
}

def readZeroEntriesParams() {
    createZeroEntries = context.createZeroEntries;
    if (createZeroEntries == null) {
        createZeroEntries = true;
    }
    createFirstLastZeroEntries = context.createFirstLastZeroEntries;
    if (createFirstLastZeroEntries == null) {
        createFirstLastZeroEntries = false
    }
}

def readProductStoreParams() {
    productStoreId = parameters.productStoreId;
    productStore = null;
    if (productStoreId) {
        productStore = from("ProductStore").where("productStoreId", productStoreId).cache(true).queryOne();
        if (!productStore) {
            productStoreId = null;
        }
    }
}

def storeProductStoreParams() {
    context.productStoreId = productStoreId;
    context.productStore = productStore;
}

def readCurrencyUomParams() {
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
}

def storeCurrencyUomParams() {
    context.currencyUomId = currencyUomId;
    context.currencyUom = currencyUom;
}

def readChartTimeParams() {
    iScope = context.chartIntervalScope;
    if (!iScope) { // Screen must set this, either from user or a default
        return null;
    }
    iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : -1;
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
    fromDateTimestamp = null;
    thruDateTimestamp = null;
    nowTimestamp = context.nowTimestamp;
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
}

def readDateIntervalsFormatter() {
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
    dateFormatter = dateIntervals.getDateFormatter();
}

def checkCreateZeroEntries(resultMap, initialValues) {
    
    def dateIntv = dateIntervals;
    
    // Create zero-value data points (if requested) 
    // NOTE: this also establishes the LinkedHashMap order for unordered query
    int i = 0;
    if (createZeroEntries) {
        // Loop intervals until reach iCount (if set) or until pass thruDate (if set) (NOTE: thruDate is inclusive due to query above)
        while ((iCount < 0 || i < iCount) && !(thruDateTimestamp && dateIntv.getDateBegin().after(thruDateTimestamp))) {
            String date = dateFormatter.format(dateIntv.getDateBegin());
            //Debug.logInfo("Interval date: " + date + " (" + dateIntv.getDateBegin() + " - " + dateIntv.getDateEnd() + ")", module);
            resultMap.put(date, initialValues + ["pos": date]);
    
            // Get next date interval; NOTE: duplicated above
            dateIntv = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntv.getDateBegin(), context.locale, context.timeZone);
            i++;
        }
    }
    
    // Create first and last zero-value data points (if requested)
    if (createFirstLastZeroEntries) {
        String date = dateFormatter.format(dateIntv.getDateBegin());
        resultMap.put(date, initialValues + ["pos": date]);
        date = dateFormatter.format(maxThruDateTimestamp);
        resultMap.put(date, initialValues + ["pos": date]);
    }   
}


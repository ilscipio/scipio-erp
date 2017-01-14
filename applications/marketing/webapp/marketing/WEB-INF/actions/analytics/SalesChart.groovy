import java.sql.Timestamp
import java.text.SimpleDateFormat

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityUtilProperties;

import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.common.uom.UomWorker;
import org.ofbiz.common.uom.SimpleUomRateConverter;

def getIntervalDefaultCount(iScope) {
    def iCount = UtilDateTime.getIntervalDefaultCount(iScope);
    if (iCount >= 0) {
        // FIXME?: why isn't this taken into account by values in UtilDateTime?
        if (iScope.equals("quarter")) iCount = Math.round(iCount / 3);
        if (iScope.equals("semester")) iCount = Math.round(iCount / 6);
    }
    return iCount;
};

Map processResult() {
    final def module = "SalesChart.groovy";
    def debugMode = Debug.verboseOn();
    //debugMode = true;
    
    createZeroEntries = context.createZeroEntries;
    if (createZeroEntries == null) {
        createZeroEntries = true;
    }
    createFirstLastZeroEntries = context.createFirstLastZeroEntries;
    if (createFirstLastZeroEntries == null) {
        createFirstLastZeroEntries = false
    }
    
    salesChannel = parameters.salesChannel;
    salesChannels = (parameters.salesChannel) ? [parameters.salesChannel] : [];
    
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
    
    // Perform main DB query
    // OLD: we'll do the query manually because of the performance impacts, and use EntityListIterator
//    queryMinDate = UtilDateTime.timeStampToString(fromDateTimestamp, "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);
//    queryMaxDate = UtilDateTime.timeStampToString(maxThruDateTimestamp, "yyyy-MM-dd HH:mm:ss.SSS", context.timeZone, context.locale);
//    if (debugMode) {
//        Debug.logInfo("Fields (query): minDate: " + queryMinDate
//            + "; maxDate: " + queryMaxDate
//            + "; maxThruDateTimestamp: " + maxThruDateTimestamp
//            , module);
//    }
//    Map findOrderMap = dispatcher.runSync("findOrdersFull", UtilMisc.toMap("salesChannelEnumId", salesChannels,
//        "minDate", queryMinDate, "maxDate", queryMaxDate, "userLogin", userLogin));
//    List orderList = findOrderMap.orderList;
    
    // NOTE: conditions derived from findOrders service
    conditions = [];
    conditions.add(EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, fromDateTimestamp));
    // IMPORTANT: we're using LESS_THAN_EQUAL_TO currently, not LESS_THAN, 
    // which can in theory affect some of the logic below...
    conditions.add(EntityCondition.makeCondition("orderDate", EntityOperator.LESS_THAN_EQUAL_TO, maxThruDateTimestamp));
    if (salesChannel) {
        conditions.add(EntityCondition.makeCondition("salesChannelEnumId", salesChannel));
    }
    if (productStoreId) {
        conditions.add(EntityCondition.makeCondition("productStoreId", productStoreId));
    }
    
    def orderListIterator;
    // OPTIMIZATION: if we're creating zero-entries, we don't need to sort the query (which may be huge)
    if (createZeroEntries) {
        orderListIterator = from("OrderHeader").where(conditions).cache(false).queryIterator();
    } else {
        orderListIterator = from("OrderHeader").where(conditions).orderBy("orderDate").cache(false).queryIterator();
    }
    
    // Organize results
    //Map resultMap = new TreeMap<String, Object>(); // TreeMap doesn't properly order because the keys are strings...
    Map resultMap = new LinkedHashMap<String, Object>(); // preserve the insertion order; the results are ordered by -orderDate
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
    dateFormatter = dateIntervals.getDateFormatter();
    try {
        def ZERO = BigDecimal.ZERO;
        def scale = OrderReadHelper.scale;
        def rounding = OrderReadHelper.rounding;
        
        // Create zero-value data points (if requested) 
        // NOTE: this also establishes the LinkedHashMap order for unordered query
        int i = 0;
        if (createZeroEntries) {
            // Loop intervals until reach iCount (if set) or until pass thruDate (if set) (NOTE: thruDate is inclusive due to query above)
            while ((iCount < 0 || i < iCount) && !(thruDateTimestamp && dateIntervals.getDateBegin().after(thruDateTimestamp))) {
                String date = dateFormatter.format(dateIntervals.getDateBegin());
                //Debug.logInfo("Interval date: " + date + " (" + dateIntervals.getDateBegin() + " - " + dateIntervals.getDateEnd() + ")", module);
                resultMap.put(date, ["total": ZERO, "count": 0, "pos": date]);
        
                // Get next date interval; NOTE: duplicated above
                dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntervals.getDateBegin(), context.locale, context.timeZone);
                i++;
            }
        }
        
        // Create first and last zero-value data points (if requested)
        if (createFirstLastZeroEntries) {
            String date = dateFormatter.format(dateIntervals.getDateBegin());
            resultMap.put(date, ["total": ZERO, "count": 0, "pos": date]);
            date = dateFormatter.format(maxThruDateTimestamp);
            resultMap.put(date, ["total": ZERO, "count": 0, "pos": date]);
        }
        
        def orderStats = [ "currencyConvertCount": 0, "currencyFailCount": 0, 
            "totalOrderCount": 0, "totalGrandTotal": ZERO];
        def converter = new SimpleUomRateConverter(delegator, dispatcher);
        
        // Process orders, organized by date interval
        if (orderListIterator != null) {
            getOrderTotal = { header ->
                def sourceUomId = header.currencyUom;
                if (currencyUomId == sourceUomId) {
                    // hopefully most cases fall here...
                    // (don't count this in currencyConvertCount)
                    return header.grandTotal;
                }
                // if for some reason the OrderHeader didn't store uom ID, get it through product store
                if (!sourceUomId) {
                    // TODO: REVIEW: if this is not a valid case in ofbiz OrderHeader, then should not
                    // do this and should log error and return zero instead...
                    //Debug.logWarning("SalesChart: No currencyUom for order '"
                    //        + header.orderId + "' header; using product store default", module);
                    if (header.productStoreId) {
                        if (header.productStoreId == productStoreId) {
                            headerProductStore = productStore;
                        } else {
                            headerProductStore = from("ProductStore").where("productStoreId", productStoreId).cache(true).queryOne();
                        }
                        sourceUomId = headerProductStore.defaultCurrencyUomId;
                    }
                    if (!sourceUomId) {
                        Debug.logError("SalesChart: Could not determine a currency uom for order '"
                            + header.orderId + "'; omitting order from results", module);
                        orderStats.currencyFailCount++;
                        return ZERO;
                    }
                }

                def result = converter.convertUom(header.grandTotal, sourceUomId, currencyUomId);
                if (result != null) {
                    orderStats.currencyConvertCount++;
                    return result;
                } else {
                    orderStats.currencyFailCount++;
                    return ZERO;
                }
            }
            
            while ((header = orderListIterator.next())) {
                String date = dateFormatter.format(header.orderDate);
                if (resultMap.get(date) != null) {
                    Map newMap = resultMap.get(date);
                    BigDecimal total = newMap.get("total");
                    if (header.grandTotal != null) {
                        // TODO: REVIEW: it is unclear where the setScale call is really supposed to be done...
                        // here (while summing), or at the very end after all totals were summed?
                        // mostly it is the currency conversions that are affected.
                        // for now, doing it here (while summing) instead of at the end...
                        BigDecimal orderTotal = getOrderTotal(header).setScale(scale, rounding);
                        total = total.plus(orderTotal);
                        orderStats.totalGrandTotal = orderStats.totalGrandTotal.plus(orderTotal);
                        newMap.put("total", total);
                    }
                    int count = newMap.get("count");
                    newMap.put("count", count+1);
                    newMap.put("pos", date);
                    resultMap.put(date, newMap);
                } else {
                    Map newMap = [:];
                    BigDecimal total = ZERO;
                    if (header.grandTotal != null) {
                        BigDecimal orderTotal = getOrderTotal(header).setScale(scale, rounding);
                        total = total.plus(orderTotal);
                        orderStats.totalGrandTotal = orderStats.totalGrandTotal.plus(orderTotal);
                    }
                    newMap.put("total", total);
                    newMap.put("count", 1);
                    newMap.put("pos", date);
                    resultMap.put(date, newMap);
                }
                //Debug.logInfo("Order date: " + date, module);
                orderStats.totalOrderCount++;
            }
        }
        context.orderStats = orderStats;
        
        // TODO: REVIEW: do we need to do a .setScale(scale, rounding) call on each of the totals
        // at the end of summing?? (and also whether to do it during summing, above)
        
        Debug.logInfo("SalesChart stats: " + orderStats, module);
        
        if (orderStats.currencyFailCount > 0) {
            errorMessageList = context.errorMessageList;
            if (errorMessageList == null) {
                errorMessageList = [];
            }
            errorMessageList.add(UtilProperties.getMessage("MarketingUiLabels", "SfaOrderTotalsConvertFail", 
                orderStats + ["currencyUomId":currencyUomId], context.locale));
            context.errorMessageList = errorMessageList;
        }
        if (orderStats.currencyConvertCount > 0) {
            eventMessageList = context.eventMessageList;
            if (eventMessageList == null) {
                eventMessageList = [];
            }
            eventMessageList.add(UtilProperties.getMessage("MarketingUiLabels", "SfaOrderTotalsConvertedExchangeNotice", 
                orderStats + ["currencyUomId":currencyUomId], context.locale));
            context.eventMessageList = eventMessageList;
        }
        
    } finally {
        if (orderListIterator != null) {
            orderListIterator.close();
        }
    }    
       
    if (debugMode) {
        Debug.logInfo("Number of results: " + resultMap.size() 
            + "; number of zero-entry loop iterations: " + i, module);
    }
    
    context.productStoreId = productStoreId;
    context.productStore = productStore;
    context.currencyUomId = currencyUomId;
    context.currencyUom = currencyUom;
    
    return resultMap;
}

sales = processResult();
context.sales = sales;

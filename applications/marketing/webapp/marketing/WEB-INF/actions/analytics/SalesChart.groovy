import java.sql.Timestamp
import java.text.SimpleDateFormat

import org.ofbiz.base.util.*;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityUtilProperties;

import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.common.uom.UomWorker;
import org.ofbiz.common.uom.SimpleUomRateConverter;

// FIXME: revisit reuse pattern
GroovyUtil.runScriptAtLocation("component://marketing/webapp/marketing/WEB-INF/actions/analytics/CommonAnalytics.groovy", context);
// New util instance that sets variables in our binding
asutil = context.analyticsScriptUtilClass.newInstance(binding);
// old:
//asutil = context.analyticsUtil;
//analyticsUtil.setBinding(binding);
// possible future construct
//this.metaClass.mixin context.analyticsClass

Map processResult() {
    final def module = "SalesChart.groovy";
    asutil.readDebugMode();
    asutil.readZeroEntriesParams();
    
    salesChannel = parameters.salesChannel;
    salesChannels = (parameters.salesChannel) ? [parameters.salesChannel] : [];
    
    asutil.readProductStoreParams();
    asutil.readCurrencyUomParams();
    asutil.readChartTimeParams();
    asutil.readDateIntervalsFormatter();
    
    //Map resultMap = new TreeMap<String, Object>(); // TreeMap doesn't properly order because the keys are strings...
    Map resultMap = new LinkedHashMap<String, Object>(); // preserve the insertion order
    
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
    
    // Process results
    try {
        def ZERO = asutil.ZERO;
        def scale = asutil.scale;
        def rounding = asutil.rounding;
        
        asutil.checkCreateZeroEntries(resultMap, ["total": ZERO, "count": 0]);
        
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
        Debug.logInfo("Number of results: " + resultMap.size(), module);
    }
    
    asutil.storeProductStoreParams();
    asutil.storeCurrencyUomParams();
    
    return resultMap;
}

sales = processResult();
context.sales = sales;

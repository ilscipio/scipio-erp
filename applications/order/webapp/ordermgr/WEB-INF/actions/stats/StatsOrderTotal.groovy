import java.math.BigDecimal
import java.math.RoundingMode;
import java.util.*;
import java.sql.Timestamp;
import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.base.util.*;

import com.ibm.icu.text.SimpleDateFormat;
import org.ofbiz.base.util.cache.UtilCache;


contentCache = UtilCache.getOrCreateUtilCache("stats.order", 0, 0, 60000, true);
Map processResult() {
    int decimals = UtilNumber.getBigDecimalScale("invoice.decimals");
    RoundingMode rounding = UtilNumber.getRoundingMode("invoice.rounding");
    iCount = (context.chartIntervalCount != null) ? context.chartIntervalCount : 0;
    String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year    
    if (iCount <= 0)
        iCount = UtilDateTime.getIntervalDefaultCount(iScope);
    fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
    
    Map resultMap = new TreeMap<String, Object>();
    GenericValue userLogin = context.get("userLogin");
    for (int i = 0; i < iCount; i++) {
        List<EntityCondition> curConditions = UtilMisc.toList(
                EntityCondition.makeCondition("statusId", EntityOperator.NOT_EQUAL, "ORDER_CANCELLED"),
                EntityCondition.makeCondition("orderTypeId", EntityOperator.EQUALS, "SALES_ORDER"),
                EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()),
                EntityCondition.makeCondition("orderDate", EntityOperator.LESS_THAN_EQUAL_TO, dateIntervals.getDateEnd())
        );

        if(UtilValidate.isNotEmpty(context.productStoreId)){
            curConditions.add(EntityCondition.makeCondition("productStoreId", EntityOperator.EQUALS, context.productStoreId));
        }

        if(UtilValidate.isNotEmpty(context.webSiteId)){
            curConditions.add(EntityCondition.makeCondition("webSiteId", EntityOperator.EQUALS, context.webSiteId));
        }

        if(UtilValidate.isNotEmpty(context.terminalId)){
            curConditions.add(EntityCondition.makeCondition("terminalId", EntityOperator.EQUALS, context.terminalId));
        }

        EntityCondition ecl = EntityCondition.makeCondition(curConditions, EntityOperator.AND);
        List<GenericValue> sales = delegator.findList("OrderStats",ecl,
                UtilMisc.toSet("totalGrandAmount","totalOrders","totalSubRemainingAmount")
                ,null
                ,null,true);
        BigDecimal totalAmount =  BigDecimal.ZERO;
        BigDecimal totalSubRemainingAmount = BigDecimal.ZERO;
        Long totalOrders = Long.valueOf(0);
        Map newMap = [:];
        for(GenericValue sale : sales) {
            if (sale.get("totalGrandAmount") != null) {
                BigDecimal amount = sale.getBigDecimal("totalGrandAmount");
                totalAmount = totalAmount.add(amount).setScale(decimals, rounding);
            }
            if (sale.get("totalSubRemainingAmount") != null){
                BigDecimal subamount = sale.getBigDecimal("totalSubRemainingAmount");
                totalSubRemainingAmount = totalSubRemainingAmount.add(subamount).setScale(decimals, rounding);
            }
            totalOrders+= sale.getLong("totalOrders");
        }

        newMap.put("total", totalAmount);
        newMap.put("subamount", totalSubRemainingAmount);
        newMap.put("count", totalOrders);
        newMap.put("dateTime", dateIntervals.getDateBegin().toLocalDateTime());
        resultMap.put(dateIntervals.getDateFormatter().format(dateIntervals.getDateBegin()), newMap);

        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntervals.getDateBegin(), context.locale, context.timeZone);
    }
    return resultMap;
}
orderStats = processResult();

context.orderStats = orderStats;
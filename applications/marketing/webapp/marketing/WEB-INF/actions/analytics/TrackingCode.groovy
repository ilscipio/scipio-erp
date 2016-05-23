/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

import java.text.SimpleDateFormat

import javolution.util.FastList

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilDateTime
import org.ofbiz.entity.condition.EntityCondition
import org.ofbiz.entity.condition.EntityOperator

// query for both number of visits and number of orders

//visitConditionList = [] as LinkedList;
//orderConditionList = [] as LinkedList;
//
//if (fromDate) {
//    visitConditionList.add(EntityCondition.makeCondition("fromDate", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate));
//    orderConditionList.add(EntityCondition.makeCondition("orderDate", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate));
//}
//if (thruDate) {
//     visitConditionList.add(EntityCondition.makeCondition("fromDate", EntityOperator.LESS_THAN_EQUAL_TO, thruDate));
//     orderConditionList.add(EntityCondition.makeCondition("orderDate", EntityOperator.LESS_THAN_EQUAL_TO, thruDate));
//}
//if (trackingCodeId) {
//     visitConditionList.add(EntityCondition.makeCondition("trackingCodeId", EntityOperator.EQUALS, trackingCodeId));
//     orderConditionList.add(EntityCondition.makeCondition("trackingCodeId", EntityOperator.EQUALS, trackingCodeId));
//}
//
//visits = select("trackingCodeId", "visitId").from("TrackingCodeAndVisit").where(visitConditionList).orderBy("trackingCodeId").queryList();
//orders = select("trackingCodeId", "orderId", "grandTotal").from("TrackingCodeAndOrderHeader").where(orderConditionList).orderBy("trackingCodeId").queryList();
//
//
//
//// use this helper to build a List of visits, orders, order totals, and conversion rates
//trackingCodeVisitAndOrders = ReportHelper.calcConversionRates(visits, orders, "trackingCodeId");
//context.trackingCodeVisitAndOrders = trackingCodeVisitAndOrders;

SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
currentYearBegin = UtilDateTime.getYearStart(nowTimestamp, timeZone, locale);
currentYearEnd  = UtilDateTime.getYearEnd(nowTimestamp, timeZone, locale);
currentYearBeginText = sdf.format(currentYearBegin);
currentYearEndText = sdf.format(currentYearEnd);
cacheId = "accounting_" + currentYearBeginText + "-" + currentYearEndText;

Map<Date, BigDecimal> processResults() {
    
//    visitConditionList.add(EntityCondition.makeCondition("trackingCodeId", EntityOperator.EQUALS, trackingCodeId));
//    
//    
//    orderConditionList.add(EntityCondition.makeCondition("trackingCodeId", EntityOperator.EQUALS, trackingCodeId));   
//    
//    currencyUomId = UtilHttp.getCurrencyUom(request);
//    context.currencyUomId = currencyUomId;
//    mainAndExprs.add(EntityCondition.makeCondition("currencyUomId", EntityOperator.EQUALS, currencyUomId));
    
    int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : 6;
    String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year
    
    Calendar calendar = Calendar.getInstance();
    if (iScope.equals("day")) {
        calendar.set(Calendar.DAY_OF_YEAR, calendar.get(Calendar.DAY_OF_YEAR) - 30);
    } else if (iScope.equals("week")) {
        calendar.set(Calendar.DAY_OF_WEEK, 1);
        calendar.set(Calendar.WEEK_OF_YEAR, calendar.get(Calendar.WEEK_OF_YEAR) - 12);
    } else if (iScope.equals("month")) {
        calendar.set(Calendar.DAY_OF_MONTH, 1);
        calendar.set(Calendar.MONTH, calendar.get(Calendar.MONTH) - 6);
    } else if (iScope.equals("year")) {
        calendar.set(Calendar.DAY_OF_YEAR, 1);
        calendar.set(Calendar.MONTH, 1);
        calendar.set(Calendar.YEAR, calendar.get(Calendar.YEAR) - 5);
    }
    fromDate = UtilDateTime.toTimestamp(calendar.getTime());
    
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDate, context.locale, context.timeZone);
    
    Map<Date, BigDecimal> visits = [:];
    Debug.log("count ===============> " + iCount);
    for (int i = 0; i <= iCount; i++) {       
        List fromDateAndExprs = FastList.newInstance();
        fromDateAndExprs.add(EntityCondition.makeCondition("fromDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals["dateBegin"]));
        fromDateAndExprs.add(EntityCondition.makeCondition("fromDate", EntityOperator.LESS_THAN, dateIntervals["dateEnd"]));
        
        visit = select("trackingCodeId", "visitId").from("TrackingCodeAndVisit").where(fromDateAndExprs).orderBy("trackingCodeId").queryList();
        Debug.log("dateBegin ========> " + dateIntervals["dateBegin"] + "     dateEnd ========> " + dateIntervals["dateEnd"] + "  visit ==============> " + visit);
        if (visit)
            visits.put(dateIntervals["dateBegin"], visit.visitId);
    }
    return visits;
    
}
visits = processResults();

context.visits = visits;
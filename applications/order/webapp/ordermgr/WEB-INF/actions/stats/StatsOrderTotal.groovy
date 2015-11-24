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

import java.math.BigDecimal;
import java.util.*;
import java.sql.Timestamp;
import org.ofbiz.entity.*;
import org.ofbiz.entity.condition.*;
import org.ofbiz.entity.util.*;
import org.ofbiz.base.util.*;

import com.ibm.icu.text.SimpleDateFormat;
import org.ofbiz.base.util.cache.UtilCache;


contentCache = UtilCache.getOrCreateUtilCache("stats.order", 0, 0, 0, true, false);


def begin, end,dailyStats,weeklyStats,monthlyStats;
SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
if(context.chartIntervalScope != null){
	String iscope = context.chartIntervalScope; //day|week|month|year
	int icount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : 0;
	icount = icount *(-1);
	if(iscope=="day"){
		begin = UtilDateTime.getDayStart(nowTimestamp, icount, timeZone, locale);
	}
	if(iscope=="week"){
		begin = UtilDateTime.getWeekStart(nowTimestamp, 0, icount, timeZone, locale);
	}
	if(iscope=="month"){
		begin = UtilDateTime.getMonthStart(nowTimestamp, 0, icount, timeZone, locale);
	}
	if(iscope=="year"){
		begin = UtilDateTime.getYearStart(nowTimestamp, 0, icount, timeZone, locale);
	}
}else{
	begin = UtilDateTime.getYearStart(nowTimestamp, timeZone, locale);
}

end = UtilDateTime.getYearEnd(nowTimestamp, timeZone, locale);
beginText = sdf.format(begin);
endText = sdf.format(end);
cacheId = "order_"+begin+"-"+end;

Map	processResult(List orderList,String dateType){
	Map dateFormats = ["day":new SimpleDateFormat("YYYY-MM-dd"),
		"week":new SimpleDateFormat("YYYY-'W'ww"),
		"month":new SimpleDateFormat("YYYY-MM"),
		"year":new SimpleDateFormat("YYYY"),
		"singleday":new SimpleDateFormat("D"),
		"singleweek":new SimpleDateFormat("'W'ww"),
		"singlemonth":new SimpleDateFormat("MM"),
		"singleyear":new SimpleDateFormat("YYYY")];
	Map resultMap = new TreeMap<String, Object>();
	orderList.each { header ->
		SimpleDateFormat df = dateFormats.get(dateType);
		String date = df.format(header.orderDate);
		if(resultMap.get(date) != null){
			Map newMap = resultMap.get(date);
			BigDecimal total = newMap.get("total");
			total = total.plus(header.grandTotal ?: BigDecimal.ZERO);
			newMap.put("total", total);
			int count = newMap.get("count");
			newMap.put("count", count+1);
			newMap.put("pos", ((SimpleDateFormat) dateFormats.get("singleday")).format(header.orderDate));
			resultMap.put(date, newMap);
		}else{
			Map newMap = [:];
			BigDecimal total = BigDecimal.ZERO;
			total = total.plus(header.grandTotal ?: BigDecimal.ZERO);
			newMap.put("total", total);
			newMap.put("count", 1);
			newMap.put("pos", ((SimpleDateFormat) dateFormats.get("singleday")).format(header.orderDate));
			resultMap.put(date,newMap);
		}
	}
	return resultMap;
}


if(contentCache.get(cacheId)==null){
	GenericValue userLogin = context.get("userLogin");
	Map cacheMap = [:];
	// Lookup results
	Map findOrderMap = dispatcher.runSync("findOrdersFull", UtilMisc.toMap("minDate",beginText,"maxDate",endText,"userLogin",userLogin));
	// Result-set processing
	List orderList = findOrderMap.orderList;
	
	dailyStats = processResult(orderList,"day");
	weeklyStats = processResult(orderList,"week");
	monthlyStats = processResult(orderList,"month");
	yearlyStats = processResult(orderList,"year");
	
	cacheMap.dailyStats = processResult(orderList,"day");
	cacheMap.weeklyStats = processResult(orderList,"week");
	cacheMap.monthlyStats = processResult(orderList,"month");
	cacheMap.yearlyStats = processResult(orderList,"year");
	contentCache.put(cacheId,cacheMap);
}else{
	cacheMap = contentCache.get(cacheId);
	dailyStats = cacheMap.dailyStats;
	weeklyStats = cacheMap.weeklyStats;
	monthlyStats = cacheMap.monthlyStats;
	yearlyStats = cacheMap.yearlyStats;
}
context.dailyStats = dailyStats;						  
context.weeklyStats = weeklyStats;
context.monthlyStats = monthlyStats;
context.yearlyStats = yearlyStats;
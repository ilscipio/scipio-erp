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

SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
yearBegin = UtilDateTime.getYearStart(nowTimestamp, timeZone, locale);
yearBeginText = sdf.format(yearBegin);
yearEnd = UtilDateTime.getYearEnd(nowTimestamp, timeZone, locale);
yearEndText = sdf.format(yearEnd);
GenericValue userLogin = context.get("userLogin");

// Lookup results
Map findOrderMap = dispatcher.runSync("findOrdersFull", UtilMisc.toMap("minDate",yearBeginText,"maxDate",yearEndText,"userLogin",userLogin));
// Result-set processing
List orderList = findOrderMap.orderList;

Map	processResult(List orderList,String dateType){
	Map dateFormats = ["day":new SimpleDateFormat("YYYY-MM-DD"),
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
			newMap.put("day", ((SimpleDateFormat) dateFormats.get("singleday")).format(header.orderDate));
			resultMap.put(date, newMap);
		}else{
			Map newMap = [:];
			BigDecimal total = BigDecimal.ZERO;
			total = total.plus(header.grandTotal ?: BigDecimal.ZERO);
			newMap.put("total", total);
			newMap.put("count", 1);
			newMap.put("day", ((SimpleDateFormat) dateFormats.get("singleday")).format(header.orderDate));
			resultMap.put(date,newMap);
		}
	}
	return resultMap;
}

context.dailyStats = processResult(orderList,"day");						  
context.weeklyStats = processResult(orderList,"week");
context.monthlyStats = processResult(orderList,"month");
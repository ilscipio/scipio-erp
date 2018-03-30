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

import java.sql.Date;

import org.ofbiz.accounting.util.UtilAccounting;


contentCache = UtilCache.getOrCreateUtilCache("stats.accounting", 0, 0, 0, true, false);

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
cacheId = "accounting_"+begin+"-"+end;

Map findLastClosedDateOutMap = context.findLastClosedDateOutMap;
Timestamp lastClosedDate = (Timestamp)findLastClosedDateOutMap.lastClosedDate;


// POSTED AND UNPOSTED
// Posted and unposted transactions totals and grand totals
andExprs = [];
andExprs.add(EntityCondition.makeCondition("organizationPartyId", EntityOperator.IN, partyIds));
andExprs.add(EntityCondition.makeCondition("glFiscalTypeId", EntityOperator.EQUALS, glFiscalTypeId));
andExprs.add(EntityCondition.makeCondition("transactionDate", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate));
andExprs.add(EntityCondition.makeCondition("transactionDate", EntityOperator.LESS_THAN_EQUAL_TO, thruDate));
andCond = EntityCondition.makeCondition(andExprs, EntityOperator.AND);
List allTransactionTotals = select("acctgTransTypeId", "debitCreditFlag", "amount").from("AcctgTransSums").where(andExprs).queryList();
List allTransactionDebit = [];
List allTransactionCredit = [];
if (allTransactionTotals) {    
    allTransactionTotals.each { allTransactionTotal ->
        accountMap = [:];
        accountMap.put("amount", allTransactionTotal.amount);
        acctgTransType = select("description").from("AcctgTransType").where(["acctgTransTypeId" : allTransactionTotal.acctgTransTypeId]).cache(true).queryOne();
        accountMap.put("type", acctgTransType.description);

        if (allTransactionTotal.debitCreditFlag == "C") {
            allTransactionCredit.add(accountMap);
        } else if (allTransactionTotal.debitCreditFlag == "D") {
            allTransactionDebit.add(accountMap);
        }        
    }
}

Map    processResult(List transactionList) {
    Map resultMap = new TreeMap<String, Object>();
    transactionList.each { header ->        
            Map newMap = [:];
            BigDecimal total = BigDecimal.ZERO;
            total = total.plus(header.amount ?: BigDecimal.ZERO);
            newMap.put("total", total);
            newMap.put("count", 1);
            newMap.put("pos", header.type);
            resultMap.put(header.type, newMap);
//        }
    }
    return resultMap;
}


//if (contentCache.get(cacheId)==null){
//    GenericValue userLogin = context.get("userLogin");
    Map cacheMap = [:];
    // Lookup results
    debitStats = processResult(allTransactionDebit);
    creditStats = processResult(allTransactionCredit);
    contentCache.put(cacheId, cacheMap);
//} else {
//    cacheMap = contentCache.get(cacheId);
//    debitStats = cacheMap.debitStats;
//    creditStats = cacheMap.creditStats;
//}
context.debitStats = debitStats;        
context.creditStats = creditStats;
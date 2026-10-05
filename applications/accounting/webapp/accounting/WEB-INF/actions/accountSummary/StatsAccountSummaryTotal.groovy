/*
 * Scipio Commerce
 * Copyright (C) Ilscipio GmbH
 *
 * This file is part of Scipio Commerce. Scipio Commerce is free software: you
 * can redistribute it and modify it under the terms of the GNU Affero General
 * Public License, version 3, as published by the Free Software Foundation.
 * Scipio Commerce is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License
 * for more details. You should have received a copy of the license with this
 * work (file LICENSE). If not, see <https://www.gnu.org/licenses/agpl-3.0.html>.
 * A commercial license is available from Ilscipio GmbH.
 *
 * SPDX-License-Identifier: AGPL-3.0-only
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


contentCache = UtilCache.getOrCreateUtilCache("stats.accounting", 0, 0, 60000, true);

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
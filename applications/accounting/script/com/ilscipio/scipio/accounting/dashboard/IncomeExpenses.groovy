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
import java.text.SimpleDateFormat

import org.ofbiz.accounting.util.UtilAccounting
import org.ofbiz.base.util.*
import org.ofbiz.base.util.cache.UtilCache
import org.ofbiz.entity.*
import org.ofbiz.entity.condition.*
import org.ofbiz.party.party.PartyWorker

contentCache = UtilCache.getOrCreateUtilCache("dashboard.accounting", 0, 0, 60000, true);

SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
currentYearBegin = UtilDateTime.getYearStart(nowTimestamp, timeZone, locale);
currentYearEnd  = UtilDateTime.getYearEnd(nowTimestamp, timeZone, locale);
currentYearBeginText = sdf.format(currentYearBegin);
currentYearEndText = sdf.format(currentYearEnd);
cacheId = "accounting_" + currentYearBeginText + "-" + currentYearEndText;

Map<Date, Map<String, BigDecimal>> processResults() {
    // Setup the divisions for which the report is executed
    List partyIds = PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, context.organizationPartyId, 'GROUP_ROLLUP');
    partyIds.add(context.organizationPartyId);  
    GenericValue incomeGlAccountClass = from("GlAccountClass").where("glAccountClassId", "INCOME").cache(true).queryOne();
    List incomeAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds(incomeGlAccountClass);    
    GenericValue expenseGlAccountClass = from("GlAccountClass").where("glAccountClassId", "EXPENSE").cache(true).queryOne();
    List expenseAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds(expenseGlAccountClass);
    List mainAndExprs = [];
    mainAndExprs.add(EntityCondition.makeCondition("organizationPartyId", EntityOperator.IN, partyIds));
    mainAndExprs.add(EntityCondition.makeCondition("isPosted", EntityOperator.EQUALS, "Y"));
    mainAndExprs.add(EntityCondition.makeCondition("glFiscalTypeId", EntityOperator.EQUALS, glFiscalTypeId));
    mainAndExprs.add(EntityCondition.makeCondition("acctgTransTypeId", EntityOperator.NOT_EQUAL, "PERIOD_CLOSING"));
    currencyUomId = UtilHttp.getCurrencyUom(request);
    context.currencyUomId = currencyUomId;    
    mainAndExprs.add(EntityCondition.makeCondition("currencyUomId", EntityOperator.EQUALS, currencyUomId));
    
    int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : 6;
    String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year
    
    iCount = UtilDateTime.getIntervalDefaultCount(iScope);
    fromDateTimestamp = UtilDateTime.getTimeStampFromIntervalScope(iScope, iCount);    
    dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, fromDateTimestamp, context.locale, context.timeZone);
    
    Map<Date, Map<String, BigDecimal>> totalMap = [:];
    for (int i = 0; i <= iCount; i++) {
        Map<String, BigDecimal> auxMap = [:];
        List transactionDateAndExprs = [];
        transactionDateAndExprs.add(EntityCondition.makeCondition("transactionDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals.getDateBegin()));
        transactionDateAndExprs.add(EntityCondition.makeCondition("transactionDate", EntityOperator.LESS_THAN, dateIntervals.getDateEnd()));
        
        List balanceTotalList = [];
        // EXPENSE
        List expenseAndExprs = new ArrayList(mainAndExprs);
        expenseAndExprs.add(EntityCondition.makeCondition("glAccountClassId", EntityOperator.IN, expenseAccountClassIds));
        expenseAndExprs.addAll(transactionDateAndExprs);
    
        expenseTransactionTotals = select("glAccountId", "debitCreditFlag", "amount", "currencyUomId").from("AcctgTransAndEntries").where(expenseAndExprs).queryList();
        balanceTotalCredit = BigDecimal.ZERO;
        balanceTotalDebit = BigDecimal.ZERO;
        expenseTransactionTotals.each { transactionTotal ->
            if ("D".equals(transactionTotal.debitCreditFlag)) {
                balanceTotalDebit = balanceTotalDebit.add(transactionTotal.amount);
            } else {
                balanceTotalCredit = balanceTotalCredit.add(transactionTotal.amount);
            }
        }
        auxMap.put("expense", balanceTotalDebit);
    
    
        // INCOME
        List incomeAndExprs = new ArrayList(mainAndExprs);
        incomeAndExprs.add(EntityCondition.makeCondition("glAccountClassId", EntityOperator.IN, incomeAccountClassIds));
        //    mainAndExprs.add(EntityCondition.makeCondition("debitCreditFlag", EntityOperator.EQUALS, "C"));
        incomeAndExprs.addAll(transactionDateAndExprs)
        
        incomeTransactionTotals = select("glAccountId", "debitCreditFlag", "amount").from("AcctgTransAndEntries").where(incomeAndExprs).queryList();
        balanceTotalCredit = BigDecimal.ZERO;
        balanceTotalDebit = BigDecimal.ZERO;
        incomeTransactionTotals.each { transactionTotal ->
            if ("D".equals(transactionTotal.debitCreditFlag)) {
                balanceTotalDebit = balanceTotalDebit.add(transactionTotal.amount);
            } else {
                balanceTotalCredit = balanceTotalCredit.add(transactionTotal.amount);
            }
        }
        auxMap.put("income", balanceTotalCredit);

        totalMap.put(dateIntervals.getDateFormatter().format(dateIntervals.getDateBegin()), auxMap);        
        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, 1, dateIntervals.getDateEnd(), context.locale, context.timeZone);
    }
    return totalMap;
}

Map cacheMap = [:];
// TODO: Not sure wether I should leave or not the result to be cached, for now let's not cache it
//if (contentCache.get(cacheId)==null) {
    cacheMap = processResults();    
//    contentCache.put(cacheId, cacheMap);
//    Debug.log("adding totalMap to cache");
//} else {
//    cacheMap = contentCache.get(cacheId);
//    Debug.log("taking totalMap from cache");    
//}
context.totalMap = cacheMap;

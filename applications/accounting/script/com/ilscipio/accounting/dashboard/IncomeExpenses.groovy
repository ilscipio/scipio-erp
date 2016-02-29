import java.text.SimpleDateFormat

import javolution.util.FastList

import org.ofbiz.accounting.util.UtilAccounting
import org.ofbiz.base.util.*
import org.ofbiz.base.util.cache.UtilCache
import org.ofbiz.entity.*
import org.ofbiz.entity.condition.*
import org.ofbiz.entity.util.*
import org.ofbiz.party.party.PartyWorker

contentCache = UtilCache.getOrCreateUtilCache("dashboard.accounting", 0, 0, 0, true, false);

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
    Debug.log("organizationPartyId ===========> " + context.organizationPartyId);
    GenericValue incomeGlAccountClass = from("GlAccountClass").where("glAccountClassId", "INCOME").cache(true).queryOne();
    List incomeAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds(incomeGlAccountClass);
    Debug.log("incomeAccountClassIds =============> " + incomeAccountClassIds);
    GenericValue expenseGlAccountClass = from("GlAccountClass").where("glAccountClassId", "EXPENSE").cache(true).queryOne();
    List expenseAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds(expenseGlAccountClass);
    List mainAndExprs = FastList.newInstance();
    mainAndExprs.add(EntityCondition.makeCondition("organizationPartyId", EntityOperator.IN, partyIds));
    mainAndExprs.add(EntityCondition.makeCondition("isPosted", EntityOperator.EQUALS, "Y"));
    mainAndExprs.add(EntityCondition.makeCondition("glFiscalTypeId", EntityOperator.EQUALS, glFiscalTypeId));
    mainAndExprs.add(EntityCondition.makeCondition("acctgTransTypeId", EntityOperator.NOT_EQUAL, "PERIOD_CLOSING"));
    mainAndExprs.add(EntityCondition.makeCondition("currencyUomId", EntityOperator.EQUALS, context.currencyUomId));
    
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
    Debug.log("dateIntervals ========> " + dateIntervals);
    //Debug.log("dateIntervals ========> " + dateIntervals);
    
    Map<Date, Map<String, BigDecimal>> totalMap = [:];
    for (int i = 0; i <= iCount; i++) {
        Debug.log("dateIntervals " + i + " ========> " + dateIntervals);
        
        Map<String, BigDecimal> auxMap = [:];
        List transactionDateAndExprs = FastList.newInstance();
        transactionDateAndExprs.add(EntityCondition.makeCondition("transactionDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals["dateBegin"]));
        transactionDateAndExprs.add(EntityCondition.makeCondition("transactionDate", EntityOperator.LESS_THAN, dateIntervals["dateEnd"]));
        
        List balanceTotalList = [];
        // EXPENSE
        List expenseAndExprs = FastList.newInstance(mainAndExprs);
        expenseAndExprs.add(EntityCondition.makeCondition("glAccountClassId", EntityOperator.IN, expenseAccountClassIds));
        // mainAndExprs.add(EntityCondition.makeCondition("debitCreditFlag", EntityOperator.EQUALS, "D"));
        expenseAndExprs.addAll(transactionDateAndExprs);
    
        expenseTransactionTotals = select("glAccountId", "debitCreditFlag", "amount").from("AcctgTransAndEntries").where(expenseAndExprs).queryList();
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
        List incomeAndExprs = FastList.newInstance(mainAndExprs);
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

        totalMap.put(dateIntervals["dateFormatter"].format(dateIntervals["dateBegin"]), auxMap);
        dateIntervals = UtilDateTime.getPeriodIntervalAndFormatter(iScope, dateIntervals["dateEnd"] + 1, context.locale, context.timeZone);
    }
//    Debug.log("totalMap ==========> " + totalMap);
    return totalMap;
}

Map cacheMap = [:];
if (contentCache.get(cacheId)==null) {
    cacheMap = processResults();    
    contentCache.put(cacheId, cacheMap);
    Debug.log("adding totalMap to cache");
} else {
    cacheMap = contentCache.get(cacheId);
    Debug.log("taking totalMap from cache");    
}
context.totalMap = cacheMap;

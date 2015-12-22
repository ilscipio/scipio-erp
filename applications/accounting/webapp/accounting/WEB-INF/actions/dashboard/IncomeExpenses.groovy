import java.sql.Timestamp
import java.text.SimpleDateFormat

import javolution.util.FastList

import org.ofbiz.accounting.util.UtilAccounting
import org.ofbiz.base.util.*
import org.ofbiz.base.util.cache.UtilCache
import org.ofbiz.entity.*
import org.ofbiz.entity.condition.*
import org.ofbiz.entity.util.*
import org.ofbiz.party.party.PartyWorker

contentCache = UtilCache.getOrCreateUtilCache("dashboard.accounting", 0, 0, 0, true, false, null);

// Setup the divisions for which the report is executed
List partyIds = PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, organizationPartyId, 'GROUP_ROLLUP');
partyIds.add(organizationPartyId);
GenericValue incomeGlAccountClass = from("GlAccountClass").where("glAccountClassId", "INCOME").cache(true).queryOne();
List incomeAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds(incomeGlAccountClass);
GenericValue expenseGlAccountClass = from("GlAccountClass").where("glAccountClassId", "EXPENSE").cache(true).queryOne();
List expenseAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds(expenseGlAccountClass);
List mainAndExprs = FastList.newInstance();
mainAndExprs.add(EntityCondition.makeCondition("organizationPartyId", EntityOperator.IN, partyIds));
mainAndExprs.add(EntityCondition.makeCondition("isPosted", EntityOperator.EQUALS, "Y"));
mainAndExprs.add(EntityCondition.makeCondition("glFiscalTypeId", EntityOperator.EQUALS, glFiscalTypeId));
mainAndExprs.add(EntityCondition.makeCondition("acctgTransTypeId", EntityOperator.NOT_EQUAL, "PERIOD_CLOSING"));


def beginDate, endDate, dailyStats, weeklyStats, monthlyStats;
SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
currentYearBegin = UtilDateTime.getYearStart(nowTimestamp, timeZone, locale);
currentYearEnd  = UtilDateTime.getYearEnd(nowTimestamp, timeZone, locale);
currentYearBeginText = sdf.format(currentYearBegin);
currentYearEndText = sdf.format(currentYearEnd);
Debug.log("currentYearBegin ===========> " + currentYearBeginText + "  currentYearEnd =============> " + currentYearEndText);
int iStartDay = context.chartIntervalStartDay != null ? Integer.parseInt(context.chartIntervalStartDay) : 0;
int iStartWeek = context.chartIntervalStartWeek != null ? Integer.parseInt(context.chartIntervalStartWeek) : 0;
int iStartMonth = context.chartIntervalStartMonth != null ? Integer.parseInt(context.chartIntervalStartMonth) : 5;
int iStartYear = context.chartIntervalStartYear != null ? Integer.parseInt(context.chartIntervalStartYear) : 0;
int iCount = context.chartIntervalCount != null ? Integer.parseInt(context.chartIntervalCount) : 6;
String iScope = context.chartIntervalScope != null ? context.chartIntervalScope : "month"; //day|week|month|year
Debug.log("icount ==============> " + iCount);
Debug.log("istartDay ==============> " + iStartDay);
Debug.log("istartMonth ==============> " + iStartMonth);
Debug.log("iScope ==============> " + iScope);


dateIntervals = getIntervalDates(currentYearBegin, iStartDay, iStartWeek, iStartMonth, iStartYear, 0, iScope);

cacheId = "accounting_" + currentYearBeginText + "-" + currentYearEndText;
Debug.log("organizationPartyId ===========> " + organizationPartyId);


Map<Date, Map<String, BigDecimal>> totalMap = [:];
for (int i = 0; i <= iCount; i++) {
	Debug.log("index ====> " + i + "   currentDateBegin ======> " + dateIntervals["beginDate"] + "    currentDateEnd ==========> " + dateIntervals["endDate"]);
	
	Map<String, BigDecimal> auxMap = [:];
	List transactionDateAndExprs = FastList.newInstance();
	transactionDateAndExprs.add(EntityCondition.makeCondition("transactionDate", EntityOperator.GREATER_THAN_EQUAL_TO, dateIntervals["beginDate"]));
	transactionDateAndExprs.add(EntityCondition.makeCondition("transactionDate", EntityOperator.LESS_THAN, dateIntervals["endDate"]));
	
	List balanceTotalList = [];
	// EXPENSE
	List expenseAndExprs = FastList.newInstance(mainAndExprs);
	expenseAndExprs.add(EntityCondition.makeCondition("glAccountClassId", EntityOperator.IN, expenseAccountClassIds));
	// mainAndExprs.add(EntityCondition.makeCondition("debitCreditFlag", EntityOperator.EQUALS, "D"));
	expenseAndExprs.addAll(transactionDateAndExprs);

	expenseTransactionTotals = select("glAccountId", "accountName", "accountCode", "debitCreditFlag", "amount").from("AcctgTransEntrySums").where(expenseAndExprs).queryList();
	
	Debug.log("expenseTransactionTotals ========> " + expenseTransactionTotals);
	if (expenseTransactionTotals) {
		balanceTotalCredit = BigDecimal.ZERO;
		balanceTotalDebit = BigDecimal.ZERO;
		expenseTransactionTotals.each { transactionTotal ->
			Map accountMap = (Map)transactionTotalsMap.get(transactionTotal.glAccountId);
			if (!accountMap) {
				accountMap = UtilMisc.makeMapWritable(transactionTotal);
				accountMap.remove("debitCreditFlag");
				accountMap.remove("amount");
//				accountMap.put("D", BigDecimal.ZERO);
//				accountMap.put("C", BigDecimal.ZERO);
			}
			UtilMisc.addToBigDecimalInMap(accountMap, transactionTotal.debitCreditFlag, transactionTotal.amount);
			if ("D".equals(transactionTotal.debitCreditFlag)) {
				balanceTotalDebit = balanceTotalDebit.add(transactionTotal.amount);
			} else {
				balanceTotalCredit = balanceTotalCredit.add(transactionTotal.amount);
			}
//			BigDecimal debitAmount = (BigDecimal)accountMap.get("D");
//			BigDecimal creditAmount = (BigDecimal)accountMap.get("C");
		}
		auxMap.put("expense", balanceTotalDebit);
	}
	
	

	// INCOME
	List incomeAndExprs = FastList.newInstance(mainAndExprs);
	incomeAndExprs.add(EntityCondition.makeCondition("glAccountClassId", EntityOperator.IN, incomeAccountClassIds));
	//	mainAndExprs.add(EntityCondition.makeCondition("debitCreditFlag", EntityOperator.EQUALS, "C"));
	incomeAndExprs.addAll(transactionDateAndExprs)
	
	incomeTransactionTotals = select("glAccountId", "accountName", "accountCode", "debitCreditFlag", "amount").from("AcctgTransEntrySums").where(incomeAndExprs).orderBy("glAccountId").queryList();
	Debug.log("incomeTransactionTotals ========> " + incomeTransactionTotals);
	if (incomeTransactionTotals) {
		Map transactionTotalsMap = [:];
		balanceTotalCredit = BigDecimal.ZERO;
		balanceTotalDebit = BigDecimal.ZERO;
		incomeTransactionTotals.each { transactionTotal ->
			Map accountMap = (Map)transactionTotalsMap.get(transactionTotal.glAccountId);
			if (!accountMap) {
				accountMap = UtilMisc.makeMapWritable(transactionTotal);
				accountMap.remove("debitCreditFlag");
				accountMap.remove("amount");
//				accountMap.put("D", BigDecimal.ZERO);
//				accountMap.put("C", BigDecimal.ZERO);
			}
			UtilMisc.addToBigDecimalInMap(accountMap, transactionTotal.debitCreditFlag, transactionTotal.amount);
			if ("D".equals(transactionTotal.debitCreditFlag)) {
				balanceTotalDebit = balanceTotalDebit.add(transactionTotal.amount);
			} else {
				balanceTotalCredit = balanceTotalCredit.add(transactionTotal.amount);
			}
//			BigDecimal debitAmount = (BigDecimal)accountMap.get("D");
//			BigDecimal creditAmount = (BigDecimal)accountMap.get("C");
		}
		auxMap.put("income", balanceTotalCredit);
	}
	
	totalMap.put(dateIntervals["dateFormatter"].format(dateIntervals["beginDate"]), auxMap);
	
	Debug.log("index ======> " + i + "   income balanceTotalDebit ==========> " + auxMap.income);
	Debug.log("index ======> " + i + "   expense balanceTotalCredit ========> " + auxMap.expense);
	
	
	dateIntervals = getIntervalDates(currentYearBegin, iStartDay, iStartWeek, iStartMonth, iStartYear, i + 1, iScope);
}

context.totalMap = totalMap;
Debug.log("totalMap ============> " + totalMap);

//Map	processResult(List transactionList) {
//	Map resultMap = new TreeMap<String, Object>();
//	transactionList.each { header ->
//		Debug.log("header ==========> " + header);
//		Map newMap = [:];
//		BigDecimal total = BigDecimal.ZERO;
//		total = total.plus(header.amount ?: BigDecimal.ZERO);
//		newMap.put("total", total);
//		newMap.put("count", 1);
//		newMap.put("pos", header.type);
//		resultMap.put(header.type, newMap);
////		}
//	}
//	return resultMap;
//}

Map<String, Object> getIntervalDates(Timestamp sDate, int iStartDay, int iStartWeek, int iStartMonth, int iStartYear, int iCount, String iScope) {
	Map<String, Timestamp> result = [:];
	if (iScope != null) {	
		Debug.log("iScope ==============> " + iScope);
		if (iScope == "day") {
			result.put("beginDate", UtilDateTime.getDayStart(sDate, iStartDay + iCount, timeZone, locale));
			result.put("endDate", UtilDateTime.getDayEnd(result["beginDate"], timeZone, locale));
			result.put("dateFormatter", new SimpleDateFormat("yyyy-MM-dd"));
		}
		if (iScope == "week") {
			result.put("beginDate", UtilDateTime.getWeekStart(sDate, 0, iStartWeek + iCount, timeZone, locale));
			result.put("endDate", UtilDateTime.getWeekEnd(result["beginDate"], timeZone, locale));
			result.put("dateFormatter", new SimpleDateFormat("yyyy-MM"));
		}
		if (iScope == "month") {
			result.put("beginDate", UtilDateTime.getMonthStart(sDate, 0, iStartMonth + iCount, timeZone, locale));
			result.put("endDate", UtilDateTime.getMonthEnd(result["beginDate"], timeZone, locale));
			result.put("dateFormatter", new SimpleDateFormat("yyyy-MM"));
		}
		if (iScope == "year") {
			result.put("beginDate", UtilDateTime.getYearStart(sDate, 0, 0, iStartYear + iCount, timeZone, locale));
			result.put("endDate", UtilDateTime.getYearEnd(result["beginDate"], timeZone, locale));
			result.put("dateFormatter", new SimpleDateFormat("yyyy"));
		}
	} else {
		result.put("beginDate", UtilDateTime.getYearStart(sDate, 0, 0, iCount, timeZone, locale));
		result.put("endDate", UtilDateTime.getYearEnd(result["beginDate"], timeZone, locale));
		result.put("dateFormatter", new SimpleDateFormat("yyyy"));
	}
	return result;
}



//if (contentCache.get(cacheId)==null) {
//	GenericValue userLogin = context.get("userLogin");
	Map cacheMap = [:];
	// Lookup results
//	debitStats = processResult(allTransactionDebit);
//	creditStats = processResult(allTransactionCredit);
//	contentCache.put(cacheId, cacheMap);
//} else {
//	cacheMap = contentCache.get(cacheId);
//	debitStats = cacheMap.debitStats;
//	creditStats = cacheMap.creditStats;
//}
//context.debitStats = debitStats;		
//context.creditStats = creditStats;
//Debug.log("debitStats ===========> " + debitStats);
//Debug.log("creditStats ===========> " + creditStats);
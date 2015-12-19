import javolution.util.FastList

import org.ofbiz.accounting.util.UtilAccounting
import org.ofbiz.base.util.*
import org.ofbiz.base.util.cache.UtilCache
import org.ofbiz.entity.*
import org.ofbiz.entity.condition.*
import org.ofbiz.entity.util.*
import org.ofbiz.party.party.PartyWorker

import com.ibm.icu.text.SimpleDateFormat


contentCache = UtilCache.getOrCreateUtilCache("dashboard.accounting", 0, 0, 0, true, false, null);

def begin, end,dailyStats,weeklyStats,monthlyStats;
SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss.SSS");
if(context.chartIntervalScope != null) {
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

Debug.log("organizationPartyId ===========> " + organizationPartyId);
Debug.log("fromDate ===========> " + fromDate + "  thruDate =============> " + thruDate);


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
mainAndExprs.add(EntityCondition.makeCondition("transactionDate", EntityOperator.GREATER_THAN_EQUAL_TO, fromDate));
mainAndExprs.add(EntityCondition.makeCondition("transactionDate", EntityOperator.LESS_THAN, thruDate));

List balanceTotalList = [];

// EXPENSE
// account balances
accountBalanceList = [];
transactionTotals = [];
balanceTotal = BigDecimal.ZERO;
List expenseAndExprs = FastList.newInstance(mainAndExprs);
expenseAndExprs.add(EntityCondition.makeCondition("glAccountClassId", EntityOperator.IN, expenseAccountClassIds));
transactionTotals = select("glAccountId", "accountName", "accountCode", "debitCreditFlag", "amount").from("AcctgTransEntrySums").where(expenseAndExprs).queryList();
if (transactionTotals) {
	Map transactionTotalsMap = [:];
	balanceTotalCredit = BigDecimal.ZERO;
	balanceTotalDebit = BigDecimal.ZERO;
	transactionTotals.each { transactionTotal ->
		Map accountMap = (Map)transactionTotalsMap.get(transactionTotal.glAccountId);
		if (!accountMap) {
			accountMap = UtilMisc.makeMapWritable(transactionTotal);
			accountMap.remove("debitCreditFlag");
			accountMap.remove("amount");
			accountMap.put("D", BigDecimal.ZERO);
			accountMap.put("C", BigDecimal.ZERO);
			accountMap.put("balance", BigDecimal.ZERO);
		}
		UtilMisc.addToBigDecimalInMap(accountMap, transactionTotal.debitCreditFlag, transactionTotal.amount);
		if ("D".equals(transactionTotal.debitCreditFlag)) {
			balanceTotalDebit = balanceTotalDebit.add(transactionTotal.amount);
		} else {
			balanceTotalCredit = balanceTotalCredit.add(transactionTotal.amount);
		}
		BigDecimal debitAmount = (BigDecimal)accountMap.get("D");
		BigDecimal creditAmount = (BigDecimal)accountMap.get("C");
		// expenses are accounts of class DEBIT: the balance is given by debits minus credits
		BigDecimal balance = debitAmount.subtract(creditAmount);
		accountMap.put("balance", balance);
		transactionTotalsMap.put(transactionTotal.glAccountId, accountMap);
	}
	accountBalanceList = UtilMisc.sortMaps(transactionTotalsMap.values().asList(), UtilMisc.toList("accountCode"));
	// expenses are accounts of class DEBIT: the balance is given by debits minus credits
	balanceTotal = balanceTotalDebit.subtract(balanceTotalCredit);
}
context.expenseAccountBalanceList = accountBalanceList;
context.expenseAccountBalanceList.add(UtilMisc.toMap("accountName", "TOTAL EXPENSES", "balance", balanceTotal));
context.expenseBalanceTotal = balanceTotal;



// INCOME
// account balances
accountBalanceList = [];
transactionTotals = [];
balanceTotal = BigDecimal.ZERO;
List incomeAndExprs = FastList.newInstance(mainAndExprs);
incomeAndExprs.add(EntityCondition.makeCondition("glAccountClassId", EntityOperator.IN, incomeAccountClassIds));
transactionTotals = select("glAccountId", "accountName", "accountCode", "debitCreditFlag", "amount").from("AcctgTransEntrySums").where(incomeAndExprs).orderBy("glAccountId").queryList();
if (transactionTotals) {
	Map transactionTotalsMap = [:];
	balanceTotalCredit = BigDecimal.ZERO;
	balanceTotalDebit = BigDecimal.ZERO;
	transactionTotals.each { transactionTotal ->
		Map accountMap = (Map)transactionTotalsMap.get(transactionTotal.glAccountId);
		if (!accountMap) {
			accountMap = UtilMisc.makeMapWritable(transactionTotal);
			accountMap.remove("debitCreditFlag");
			accountMap.remove("amount");
			accountMap.put("D", BigDecimal.ZERO);
			accountMap.put("C", BigDecimal.ZERO);
			accountMap.put("balance", BigDecimal.ZERO);
		}
		UtilMisc.addToBigDecimalInMap(accountMap, transactionTotal.debitCreditFlag, transactionTotal.amount);
		if ("D".equals(transactionTotal.debitCreditFlag)) {
			balanceTotalDebit = balanceTotalDebit.add(transactionTotal.amount);
		} else {
			balanceTotalCredit = balanceTotalCredit.add(transactionTotal.amount);
		}
		BigDecimal debitAmount = (BigDecimal)accountMap.get("D");
		BigDecimal creditAmount = (BigDecimal)accountMap.get("C");
		// income are accounts of class CREDIT: the balance is given by credits minus debits
		BigDecimal balance = creditAmount.subtract(debitAmount);
		accountMap.put("balance", balance);
		transactionTotalsMap.put(transactionTotal.glAccountId, accountMap);
	}
	accountBalanceList = UtilMisc.sortMaps(transactionTotalsMap.values().asList(), UtilMisc.toList("accountCode"));
	// incomes are accounts of class CREDIT: the balance is given by credits minus debits
	balanceTotal = balanceTotalCredit.subtract(balanceTotalDebit);
}
context.incomeAccountBalanceList = accountBalanceList;
context.incomeAccountBalanceList.add(UtilMisc.toMap("accountName", "TOTAL INCOME", "balance", balanceTotal));
context.incomeBalanceTotal = balanceTotal;



Debug.log("expenseBalanceTotal ==========> " + context.expenseBalanceTotal);
Debug.log("incomeBalanceTotal ==========> " + context.incomeBalanceTotal);

Map	processResult(List transactionList) {
	Map resultMap = new TreeMap<String, Object>();
	transactionList.each { header ->
		Debug.log("header ==========> " + header);
			Map newMap = [:];
			BigDecimal total = BigDecimal.ZERO;
			total = total.plus(header.amount ?: BigDecimal.ZERO);
			newMap.put("total", total);
			newMap.put("count", 1);
			newMap.put("pos", header.type);
			resultMap.put(header.type, newMap);
//		}
	}
	return resultMap;
}


//if (contentCache.get(cacheId)==null){
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
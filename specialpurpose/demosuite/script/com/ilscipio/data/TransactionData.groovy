//	121800 	INTRSTINC_RECEIVABLE 	CURRENT_ASSET
//	125000 	INVENTORY_XFER_OUT 	CURRENT_ASSET
//	150000 	PREPAID_EXPENSES 	CURRENT_ASSET
//	213000 	CUSTOMER_CREDIT 	CURRENT_LIABILITY
//	213100 	CUSTOMER_CREDIT 	CURRENT_LIABILITY
//	213200 	CUSTOMER_CREDIT 	CURRENT_LIABILITY
//	213300 	CUSTOMER_DEPOSIT 	CURRENT_LIABILITY
//	213400 	CUSTOMER_DEPOSIT 	CURRENT_LIABILITY
//	213500 	CUSTOMER_DEPOSIT 	CURRENT_LIABILITY
//	215000 	INVENTORY_XFER_IN 	CURRENT_LIABILITY
//	221100 	COMMISSIONS_PAYABLE 	CURRENT_LIABILITY
//	310000 	OWNERS_EQUITY 	OWNERS_EQUITY
//	311000 	OWNERS_EQUITY 	RETURN_OF_CAPITAL
//	320000 	OWNERS_EQUITY 	OWNERS_EQUITY
//	320100 	OWNERS_EQUITY 	OWNERS_EQUITY
//	320200 	OWNERS_EQUITY 	OWNERS_EQUITY
//	320300 	OWNERS_EQUITY 	OWNERS_EQUITY
//	321000 	OWNERS_EQUITY 	RETURN_OF_CAPITAL
//	321100 	OWNERS_EQUITY 	RETURN_OF_CAPITAL
//	321200 	OWNERS_EQUITY 	RETURN_OF_CAPITAL
//	321300 	OWNERS_EQUITY 	RETURN_OF_CAPITAL
//	330000 	OWNERS_EQUITY 	OWNERS_EQUITY
//	331000 	OWNERS_EQUITY 	OWNERS_EQUITY
//	332000 	OWNERS_EQUITY 	OWNERS_EQUITY
//	333000 	OWNERS_EQUITY 	OWNERS_EQUITY
//	334000 	OWNERS_EQUITY 	RETURN_OF_CAPITAL
//	335000 	OWNERS_EQUITY 	RETURN_OF_CAPITAL
//	336000 	OWNERS_EQUITY 	RETAINED_EARNINGS
//	340000 	OWNERS_EQUITY 	OWNERS_EQUITY
//	341000 	OWNERS_EQUITY 	OWNERS_EQUITY
//	342000 	OWNERS_EQUITY 	DIVIDEND
//	342100 	OWNERS_EQUITY 	DIVIDEND
//	342200 	OWNERS_EQUITY 	DIVIDEND
//	342300 	OWNERS_EQUITY 	DIVIDEND
//	343000 	OWNERS_EQUITY 	RETURN_OF_CAPITAL
//	343100 	OWNERS_EQUITY 	RETURN_OF_CAPITAL
//	343200 	OWNERS_EQUITY 	RETURN_OF_CAPITAL
//	343300 	OWNERS_EQUITY 	RETURN_OF_CAPITAL
//	344000 	OWNERS_EQUITY 	RETAINED_EARNINGS
//	517000 	WRITEOFF 	NON_CASH_EXPENSE
//	517100 	ACCTRECV_WRITEOFF 	NON_CASH_EXPENSE
//	517200 	ACCTPAY_WRITEOFF 	NON_CASH_EXPENSE
//	517300 	COMMISSIONS_WRITEOFF 	NON_CASH_EXPENSE
//	517400 	INTRSTINC_WRITEOFF 	NON_CASH_EXPENSE
//	518000 	FX_GAIN_LOSS_ACCT 	NON_CASH_EXPENSE
//	518100 	FX_GAIN_ACCOUNT 	NON_CASH_EXPENSE
//	518200 	FX_LOSS_ACCOUNT 	NON_CASH_EXPENSE
//	810000 	INTEREST_INCOME 	CASH_INCOME

import java.sql.Timestamp
import java.text.SimpleDateFormat

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.entity.*
import org.ofbiz.entity.model.DynamicViewEntity;
import org.ofbiz.entity.model.ModelKeyMap;
import org.ofbiz.entity.util.*
import org.ofbiz.service.ServiceUtil

import java.math.BigDecimal

public Map createDemoTransaction() {
	List<String> acctgTransTypeIds = [
		"AMORTIZATION",
		"CAPITALIZATION",
		"CREDIT_LINE",
		"CREDIT_MEMO",
		"DEPRECIATION",
		"DISBURSEMENT",
		"EXTERNAL_ACCTG_TRANS",
		"INCOMING_PAYMENT",
		"INTERNAL_ACCTG_TRANS",
		"INVENTORY",
		"INVENTORY_RETURN",
		"ITEM_VARIANCE",
		"MANUFACTURING",
		"NOTE",
		"OBLIGATION_ACCTG_TRA",
		"OTHER_INTERNAL",
		"OTHER_OBLIGATION",
		"OUTGOING_PAYMENT",
		"PAYMENT_ACCTG_TRANS",
		"PAYMENT_APPL",
		"PERIOD_CLOSING",
		"PURCHASE_INVOICE",
		"RECEIPT",
		"SALES",
		"SALES_INVOICE",
		"SALES_SHIPMENT",
		"SHIPMENT_RECEIPT",
		"TAX_DUE"
	]
	
	List<String> glAccountTypeIds = [
		'EXPENSE',
		'OPERATING_EXPENSE',
		'OTHER_EXPENSE',
		'OWNERS_EQUITY',
		'RETAINED_EARNINGS',
		'CUSTOMER_DEPOSIT',
		'CUSTOMER_CREDIT',
		'CUSTOMER_GC_DEPOSIT',
		'UNINVOICED_SHIP_RCPT',
		'PURCHASE_PRICE_VAR',
		'PREPAID_EXPENSES',
		'INCOME',
		'OTHER_INCOME',
		'INTEREST_INCOME',
		'INTRSTINC_RECEIVABLE',
		'INVENTORY_XFER_OUT',
		'INVENTORY_XFER_IN',
		'COMMISSION_EXPENSE',
		'COMMISSIONS_PAYABLE',
		'WRITEOFF',
		'ACCTRECV_WRITEOFF',
		'ACCTPAY_WRITEOFF',
		'COMMISSIONS_WRITEOFF',
		'INTRSTINC_WRITEOFF',
		'FX_GAIN_LOSS_ACCT',
		'FX_GAIN_ACCOUNT',
		'FX_LOSS_ACCOUNT'
	]

	
	Map<String, List<String>> glIncomeAccountClassIds = [
		"INCOME" : ["800000"],
		"CASH_INCOME" : ["801000", "802000", "803000", "804000", "805000", "806000", "810000", "811000", "812000", "813000", "814000", "819000"]
	]
	
	Map<String, List<String>> glExpenseAccountClassIds = [
		"EXPENSE" : ["513000", "513100", "513200", "600000"],
		"NON_CASH_EXPENSE" : ["515000", "515100", "515200", "516000", "516100", "517000", "517100", "517200", "517300", "517400", "518000", "518100", "518200", "519000", "519100", "519200"]
	]
	

//	
//	List<String> products = [
//			 ["productId":"GZ-2644","itemDescription":"Round Gizmo","unitPrice":38.4,"unitListPrice":48.0],
//			 ["productId":"WG-1111","itemDescription":"Micro Chrome Widget","unitPrice":59.99,"unitListPrice":60.0]
//		];
	
	Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - TX DATA-=-=-=-", "");
	Map result = ServiceUtil.returnSuccess();
	
	
	List<GenericValue> toBeStored = new ArrayList<GenericValue>();
	List<GenericValue> acctgTransEntrys = new ArrayList<GenericValue>();
	int num = context.num;
	
	for(int i = 0; i <num; i++){
		// Create AcctgTrans
		String acctgTransId = "GEN_" + delegator.getNextSeqId("demo-acctgTransId");
		// Create AcctgTransEntry (2,4,6)
		int acctgTransEntryCount = getRandomEvenInt(2,6);
		Debug.log("acctgTransEntryCount ====> " + acctgTransEntryCount);
		
		List<GenericValue> glAccounts = null;
		for (int acctgTransEntrySeqId = 1; acctgTransEntrySeqId <= acctgTransEntryCount; acctgTransEntrySeqId++) {
			
			if (acctgTransEntrySeqId % 2 == 0) {				
				debitCreditFlag = "D";
				Debug.log("debit acctg entry");
			} else {
				debitCreditFlag = "C";
				DynamicViewEntity dve = new DynamicViewEntity();
				dve.addMemberEntity("GA", "GlAccount");
				dve.addMemberEntity("GAO", "GlAccountOrganization");
				dve.addAliasAll("GA", "", null); // no prefix
				dve.addAlias("GAO", "organizationPartyId");
				dve.addViewLink("GA", "GAO", Boolean.FALSE, UtilMisc.toList(new ModelKeyMap("glAccountId", "glAccountId")));				
						
				int glAccountTypeIdIndex = random(glAccountTypeIds);
				Debug.log("glAccountTypeIdIndex =========> " + glAccountTypeIdIndex + " glAccountTypeIdList size " + glAccountTypeIds.size() + "         glAccountTypeId ==========> " + glAccountTypeIds.get(glAccountTypeIdIndex));
				
				glAccounts = from(dve).where("glAccountTypeId", glAccountTypeIds.get(glAccountTypeIdIndex), "organizationPartyId", "Company").cache(true).queryList();
				while (glAccounts == null || UtilValidate.isEmpty(glAccounts)) {
					int incomeExpense = getRandomInt(1,2);
					if (incomeExpense == 1)
						int incomeAccountClassId = random(glIncomeAccountClassIds.keySet());
					else
					
										
					glAccounts = from(dve).where("glAccountTypeId", glAccountTypeIds.get(glAccountTypeIdIndex), "organizationPartyId", "Company").cache(true).queryList();
				}
										
				amount = new BigDecimal(getRandomInt(10, 10000));
				Debug.log("credit acctg entry");
			}
			
			
			
			GenericValue glAccount = glAccounts.get(0);
//			Debug.log("glAccount ========> " + glAccount + " amount ========> " + amount);
//			fields = UtilMisc.toMap("acctgTransId", acctgTransId, "acctgTransEntrySeqId", "0000" + acctgTransEntrySeqId, "acctgTransEntryTypeId", "_NA_", "description",
//										"Automatically generated transaction (for demo purposes)", "glAccountId", glAccount.glAccountId, "glAccountTypeId", glAccount.glAccountTypeId, "organizationPartyId", "Company",
//										"reconcileStatusId", "AES_NOT_RECONCILED", "amount", amount, "currencyUomId", "USD", "debitCreditFlag", debitCreditFlag);
			
//			GenericValue acctgTransEntry = delegator.makeValue("AcctgTransEntry", fields);
//			acctgTransEntrys.add(acctgTransEntry);
		}
		
		String acctgTransTypeId = acctgTransTypeIds.get(random(acctgTransTypeIds));
		String description = "Demo Transaction " + acctgTransId;		
		Timestamp transactionDate = Timestamp.valueOf(generateRandomDate());
		isPosted = "Y";
		postedDate =  Timestamp.valueOf(generateRandomDate(transactionDate));
		glFiscalTypeId = "ACTUAL";
		
		Debug.log("acctgTransTypeId ==========> " + acctgTransTypeId);
		 
		Map fields = UtilMisc.toMap("acctgTransId", acctgTransId, "acctgTransTypeId", acctgTransTypeId, "description", description, "transactionDate", transactionDate,
									"isPosted", isPosted, "postedDate", postedDate, "glFiscalTypeId", glFiscalTypeId);
	
//		GenericValue acctgTrans = delegator.makeValue("AcctgTrans", fields);
//		toBeStored.add(acctgTrans);
//		toBeStored.addAll(acctgTransEntrys);
	}
	
	// store the changes
	if (toBeStored.size() > 0) {
		try {
			delegator.storeAll(toBeStored);
		} catch (GenericEntityException e) {
			return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,
			"acctgTransErrorCannotStoreChanges", locale) + e.getMessage());
		}
	}
	
	return result;
}

/**
 * Method should generate random number that represents
 * a time between two dates.
 *
 * @return
 */
private long getRandomTimeBetweenTwoDates (Timestamp beginDate) {
	long beginTime,endTime;
	Calendar cal = Calendar.getInstance();
	
	if(context.maxDate != null){
		endTime = ((Timestamp)context.endTime).getTime();
	}else{
		endTime = cal.getTimeInMillis();
	}
		
	if(beginDate != null){
		beginTime = beginDate.getTime();
	} else if (context.minDate != null) {
		beginTime = ((Timestamp)context.minDate).getTime();
	} else {
		cal.add(Calendar.DATE, -180);
		beginTime = cal.getTimeInMillis();
	}
	long diff = endTime - beginTime + 1;
	beginTime = beginTime + (long) (Math.random() * diff);
	return beginTime;
}



public String generateRandomDate() {
	SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
	Date randomDate = new Date(getRandomTimeBetweenTwoDates(null));
	return dateFormat.format(randomDate);
}

public String generateRandomDate(Date beginDate) {
	SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
	Date randomDate = new Date(getRandomTimeBetweenTwoDates(beginDate));
	return dateFormat.format(randomDate);
}


public int random(List myList){
	int size = myList.size();
	int index = new Random().nextInt(size - 1);
	return index;
}

public static boolean getRandomBoolean() {
	return Math.random() < 0.5;
}

public static int getRandomEvenInt(int min, int max) {
	int x = getRandomInt(min, max);
	while (x % 2 != 0) {
		x =  getRandomInt(min, max);
	}
	return x;
}

public static int getRandomInt(int min, int max) {
	Random rand = new Random();
	int x = rand.nextInt(max - min + 1) + min;
	x = rand.nextInt(max - min + 1) + min;	
	return x;
}
import java.sql.Timestamp
import java.text.SimpleDateFormat

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.entity.*
import org.ofbiz.entity.model.DynamicViewEntity
import org.ofbiz.entity.model.ModelKeyMap
import org.ofbiz.entity.util.*
import org.ofbiz.service.ServiceUtil

public Map createDemoTransaction() {
    
    // AcctgTransTypeId
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

    // GlAccountType
//    Map<String, String> glAccountTypeIds = ["503000" : "COGS_ADJ_AVG_COST", "517000" : "WRITEOFF", "517100" : "ACCTRECV_WRITEOFF", "517200" : "ACCTPAY_WRITEOFF", "517300" : "COMMISSIONS_WRITEOFF",
//        "517400" : "INTRSTINC_WRITEOFF", "518000" : "FX_GAIN_LOSS_ACCT", "518100" : "FX_GAIN_ACCOUNT", "518200" : "FX_LOSS_ACCOUNT", "810000" : "INTEREST_INCOME"];
    
    // Income GlAccounts per GlAccountClass    
    Map<String, List<String>> glIncomeAccountClassIds = [
        "INCOME" : ["800000"],
        "CASH_INCOME" : ["801000", "802000", "803000", "804000", "805000", "806000", "810000", "811000", "812000", "813000", "814000", "819000"]
    ]
    
    // Expense GlAccounts per GlAccountClass
    Map<String, List<String>> glExpenseAccountClassIds = [
        "EXPENSE" : ["513000", "513100", "513200", "600000"],
        "CASH_EXPENSE" : ["820000", "824000", "829000"],
        "NON_CASH_EXPENSE" : ["515000", "515100", "515200", "516000", "516100", "517000", "517100", "517200", "517300", "517400", "518000", "518100", "518200", "519000", "519100", "519200", "823000"],
        "COGS_EXPENSE" : ["410000", "500000", "501000" , "502000", "503000", "510000" ,"510200", "511000", "511100", "511200", "512000", "512050", "512100", "512200"],
        "INVENTORY_ADJUST" : ["514000"],
        "SGA_EXPENSE" : ["601000", "601100", "601200", "601300", "601400", "602000", "602100", "602200", "603000", "603100", "603200", "604000", "604100", "604200", "604500", "604600", "604700", "605000", 
            "605100", "605200", "605800", "605900", "606000", "607000", "607100", "607200", "607400", "607500", "607600", "607700", "607900", "608000", "609000", "609100", "609200", "609500", "610000", "611000",
            "611100", "611200", "611300", "612000", "612100", "612200", "612300", "612400", "613000", "613100", "613200", "613300", "620000", "621000", "621100", "621200", "621300", "621400", "621500", "622000", 
            "622100", "622200", "623000", "623100", "623200", "624000", "624100", "624200", "624300", "624400", "625000", "626000", "626100", "626200", "626300", "626400", "630000", "631000", "631100", "631200", 
            "631300", "631400", "632000", "640000", "641000", "642000", "643000", "649000", "650000", "651000", "652000", "653000", "654000", "659000", "660000", "661000", "662000", "663000", "669000", "680000",
            "681000", "682000", "683000", "690000", "691000", "692000", "693000", "694000", "700000", "701000", "701100", "701200", "701300", "701400", "702000", "709000", "710000", "711000", "712000", "712100", 
            "712200", "712300", "720000", "721000", "730000", "731000", "731100", "739000", "740000", "741000", "742000", "743000", "744000", "750000", "751000", "752000", "753000", "754000", "760000", "761000",
            "762000", "763000", "770000", "771000", "772000", "773000", "774000", "779000", "780000", "781000", "782000", "783000", "784000", "785000", "786000", "788000", "789000", "790000"],         
        "DEPRECIATION" : ["670000", "671000", "672000", "673000", "674000", "675000", "675100", "675200", "675300", "675400"],
        "AMORTIZATION" : ["787000"],
        "INTEREST_EXPENSE" : ["821000", "823000"]
    ]

    
    Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - TX DATA-=-=-=-", "");
    Map result = ServiceUtil.returnSuccess();
    
    
    List<GenericValue> toBeStored = new ArrayList<GenericValue>();
    List<GenericValue> acctgTransEntrys = new ArrayList<GenericValue>();
    Integer num = UtilProperties.getPropertyAsInteger("general", "data.generator.max.records", 50);
    if (context.num && context.num < num)
        num = context.num;
    
    for(int i = 0; i <num; i++) {
        // Create AcctgTrans
        String acctgTransId = "GEN_" + delegator.getNextSeqId("demo-acctgTransId");
        // Create AcctgTransEntry (2,4,6)
        int acctgTransEntryCount = UtilRandom.getRandomEvenInt(2,6);        
        // Determine if it is an income or an expense
        int incomeExpense = UtilRandom.getRandomInt(1,2);
        
        List<GenericValue> glAccounts = null;
        for (int acctgTransEntrySeqId = 1; acctgTransEntrySeqId <= acctgTransEntryCount; acctgTransEntrySeqId++) {            
            if (acctgTransEntrySeqId % 2 == 0) {
                debitCreditFlag = "D";
                Debug.log("debit acctg entry");
            } else {
                debitCreditFlag = "C";            
                List<String> glAccountClassIdList = null;
                if (incomeExpense == 1) {
                    keys = new ArrayList(glExpenseAccountClassIds.keySet());
                    index = UtilRandom.random(keys);
                    glAccountClassIdList = glExpenseAccountClassIds.get(keys.get(index));
                } else {
                    keys = new ArrayList(glIncomeAccountClassIds.keySet());
                    index = UtilRandom.random(keys);
                    glAccountClassIdList = glIncomeAccountClassIds.get(keys.get(index));
                }                    
                
                amount = new BigDecimal(UtilRandom.getRandomInt(10, 10000));
                String glAccountId = glAccountClassIdList.get(UtilRandom.random(glAccountClassIdList));
                
                DynamicViewEntity dve = new DynamicViewEntity();
                dve.addMemberEntity("GA", "GlAccount");
                dve.addMemberEntity("GAO", "GlAccountOrganization");
                dve.addAliasAll("GA", "", null); // no prefix
                dve.addAlias("GAO", "organizationPartyId");
                dve.addViewLink("GA", "GAO", Boolean.FALSE, UtilMisc.toList(new ModelKeyMap("glAccountId", "glAccountId")));                
                glAccount = from(dve).where("glAccountId", glAccountId).cache(true).queryOne();
                Debug.log("credit acctg entry - glAccountId ===> " + glAccountId);
            }

            
            
            if (glAccount) {
                currencyUomId = UtilProperties.getProperties("general.properties").getProperty("currency.uom.id.default", "USD");
                Debug.log("glAccountId ========> " + glAccount.glAccountId + " amount ========> " + amount);
                fields = UtilMisc.toMap("acctgTransId", acctgTransId, "acctgTransEntrySeqId", "0000" + acctgTransEntrySeqId, "acctgTransEntryTypeId", "_NA_", "description",
                    "Automatically generated transaction (for demo purposes)", "glAccountId", glAccount.glAccountId, "glAccountTypeId", glAccount.glAccountTypeId, "organizationPartyId", "Company",
                    "reconcileStatusId", "AES_NOT_RECONCILED", "amount", amount, "currencyUomId", currencyUomId, "debitCreditFlag", debitCreditFlag);                
                GenericValue acctgTransEntry = delegator.makeValue("AcctgTransEntry", fields);
                acctgTransEntrys.add(acctgTransEntry);
            }
        }
        
        // FIXME: This may be inconsistent with the GlAccount selected, it may affect the accuracy of some reports
        String acctgTransTypeId = acctgTransTypeIds.get(UtilRandom.random(acctgTransTypeIds)); 
        String description = "Demo Transaction " + acctgTransId;        
        Timestamp transactionDate = Timestamp.valueOf(UtilRandom.generateRandomDate(context));
        isPosted = "Y";
        postedDate =  Timestamp.valueOf(UtilRandom.generateRandomDate(transactionDate, context));
        glFiscalTypeId = "ACTUAL";
        
        Map fields = UtilMisc.toMap("acctgTransId", acctgTransId, "acctgTransTypeId", acctgTransTypeId, "description", description, "transactionDate", transactionDate,
                                    "isPosted", isPosted, "postedDate", postedDate, "glFiscalTypeId", glFiscalTypeId);
    
        GenericValue acctgTrans = delegator.makeValue("AcctgTrans", fields);
        toBeStored.add(acctgTrans);
        toBeStored.addAll(acctgTransEntrys);
    }
    
    // store the changes
    if (toBeStored.size() > 0) {
        try {
            Debug.log("Storing transactions")
            delegator.storeAll(toBeStored);
            result.put("generatedData", toBeStored);
        } catch (GenericEntityException e) {
            return ServiceUtil.returnError(UtilProperties.getMessage(resource_error,
            "acctgTransErrorCannotStoreChanges", locale) + e.getMessage());
        }
    }
    
    return result;
}
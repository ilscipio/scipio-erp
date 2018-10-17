import java.sql.Timestamp
import java.util.Map

import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.base.util.UtilProperties
import org.ofbiz.base.util.UtilRandom
import org.ofbiz.entity.*
import org.ofbiz.entity.model.DynamicViewEntity
import org.ofbiz.entity.model.ModelKeyMap
import org.ofbiz.entity.util.*

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.AbstractDemoDataHelper.DataTypeEnum
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript

public class TransactionData extends DataGeneratorGroovyBaseScript {

    TransactionData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - TX DATA-=-=-=-", "");
    }

    public String getDataType() {
        return DataTypeEnum.TRANSACTION;
    }


    void init() {
    }

    List prepareData(int index, AbstractDataObject transactionData) throws Exception {
        List<GenericValue> toBeStored = new ArrayList<GenericValue>();

        Map<String, Object> transactionFields = UtilMisc.toMap("acctgTransId", transactionData.getId(), "acctgTransTypeId", transactionData.getType(), "description", transactionData.getDescription(), "transactionDate",
                transactionData.getDate(), "isPosted", isPosted, "postedDate", postedDate, "glFiscalTypeId", glFiscalTypeId);
        GenericValue acctgTrans = delegator.makeValue("AcctgTrans", transactionFields);


        Map<String, Object> fields = UtilMisc.toMap("acctgTransId", transaction.getId(), "acctgTransEntrySeqId", "0000" + acctgTransEntrySeqId, "acctgTransEntryTypeId", "_NA_",
                "description", "Automatically generated transaction (for demo purposes)", "glAccountId", glAccount.get("glAccountId"), "glAccountTypeId",
                glAccount.get("glAccountTypeId"), "organizationPartyId", "Company", "reconcileStatusId", "AES_NOT_RECONCILED", "amount", amount, "currencyUomId",
                currencyUomId, "debitCreditFlag", debitCreditFlag);
        GenericValue acctgTransEntry = delegator.makeValue("AcctgTransEntry", fields);
        acctgTransEntries.add(acctgTransEntry);


        toBeStored.add(acctgTrans);
        toBeStored.addAll(acctgTransEntrys);

        return toBeStored;
    }
}
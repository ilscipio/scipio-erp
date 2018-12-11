import org.ofbiz.base.util.Debug
import org.ofbiz.base.util.UtilMisc
import org.ofbiz.entity.*
import org.ofbiz.entity.util.*

import com.ilscipio.scipio.ce.demoSuite.dataGenerator.DataGeneratorProvider
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.AbstractDataObject
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.dataObject.DemoDataTransaction.DemoDataTransactionEntry
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.helper.AbstractDemoDataHelper.DataTypeEnum
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.service.DataGeneratorGroovyBaseScript
import com.ilscipio.scipio.ce.demoSuite.dataGenerator.util.DemoSuiteDataGeneratorUtil.DataGeneratorProviders

@DataGeneratorProvider(providers=[DataGeneratorProviders.LOCAL])
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
                transactionData.getDate(), "isPosted", transactionData.isPosted(), "postedDate", transactionData.getPostedDate(), "glFiscalTypeId", transactionData.getFiscalType());
        GenericValue acctgTrans = delegator.makeValue("AcctgTrans", transactionFields);

        acctgTransEntries = [];
        for (DemoDataTransactionEntry entry in transactionData.getEntries()) {
            Map<String, Object> fields = UtilMisc.toMap("acctgTransId", transactionData.getId(), "acctgTransEntrySeqId", "0000" + entry.getSequenceId(), "acctgTransEntryTypeId", "_NA_",
                    "description", "Automatically generated transaction (for demo purposes)", "glAccountId", entry.getGlAccount(), "glAccountTypeId",
                    entry.getGlAccountType(), "organizationPartyId", entry.getOrgParty(), "reconcileStatusId", "AES_NOT_RECONCILED", "amount", entry.getAmount(), "currencyUomId",
                    entry.getCurrency(), "debitCreditFlag", entry.getDebitCreditFlag());
            GenericValue acctgTransEntry = delegator.makeValue("AcctgTransEntry", fields);
            acctgTransEntries.add(acctgTransEntry);
        }

        toBeStored.add(acctgTrans);
        toBeStored.addAll(acctgTransEntries);

        return toBeStored;
    }
}
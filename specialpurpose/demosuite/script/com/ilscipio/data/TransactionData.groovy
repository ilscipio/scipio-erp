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
    private static final String module = "TransactionData.groovy";
    
    TransactionData() {
        Debug.logInfo("-=-=-=- DEMO DATA CREATION SERVICE - TX DATA-=-=-=-", module);
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
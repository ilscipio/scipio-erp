/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
package com.ilscipio.scipio.accounting.ledger;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

public class AcctgAdminServices {

    private static final String module = AcctgAdminServices.class.getName();

    /**
     * Custom update PartyAcctgPreference only meant to be used during setup.
     * Allows fields not allowed in the stock service to be safely updated.
     * 
     * @param dctx
     * @param context
     * @return
     * @throws GenericEntityException
     */
    public static Map<String, Object> updatePartyAcctgPreference(DispatchContext dctx, Map<String, ? extends Object> context) throws GenericEntityException {
        Map<String, Object> result = ServiceUtil.returnSuccess();
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();

        long acctgTransCount = EntityQuery.use(delegator).from("AcctgTrans").queryCount();
        long orderCount = EntityQuery.use(delegator).from("OrderHeader").queryCount();
        long invoiceCount = EntityQuery.use(delegator).from("Invoice").queryCount();
        long quoteCount = EntityQuery.use(delegator).from("Quote").queryCount();

        try {
            result = dispatcher.runSync("updatePartyAcctgPreference", context);
        } catch (GenericServiceException e) {
            Debug.logError(e.getMessage(), module);
        }
        if (ServiceUtil.isSuccess(result)) {
            // Update more fields if there's no data in their respective main
            // entities.
            GenericValue partyAcctgPreference = delegator.findOne("PartyAcctgPreference", UtilMisc.toMap("partyId", context.get("partyId")), false);
            if (acctgTransCount == 0) {
                partyAcctgPreference.set("fiscalYearStartMonth", context.get("fiscalYearStartMonth"));
                partyAcctgPreference.set("fiscalYearStartDay", context.get("fiscalYearStartDay"));
                partyAcctgPreference.set("taxFormId", context.get("taxFormId"));
                partyAcctgPreference.set("cogsMethodId", context.get("cogsMethodId"));
                // This may have an undesired impact
                // partyAcctgPreference.set("baseCurrencyUomId",
                // context.get("baseCurrencyUomId"));
            } else if (orderCount == 0) {
                partyAcctgPreference.set("orderSeqCustMethId", context.get("orderSeqCustMethId"));
                partyAcctgPreference.set("orderIdPrefix", context.get("orderIdPrefix"));
                partyAcctgPreference.set("lastOrderNumber", context.get("lastOrderNumber"));
            } else if (invoiceCount == 0) {
                partyAcctgPreference.set("invoiceSeqCustMethId", context.get("invoiceSeqCustMethId"));
                partyAcctgPreference.set("invoiceIdPrefix", context.get("invoiceIdPrefix"));
                partyAcctgPreference.set("lastInvoiceNumber", context.get("lastInvoiceNumber"));
                partyAcctgPreference.set("lastInvoiceRestartDate", context.get("lastInvoiceRestartDate"));
                partyAcctgPreference.set("useInvoiceIdForReturns", context.get("useInvoiceIdForReturns"));
            } else if (quoteCount == 0) {
                partyAcctgPreference.set("quoteSeqCustMethId", context.get("quoteSeqCustMethId"));
                partyAcctgPreference.set("quoteIdPrefix", context.get("quoteIdPrefix"));
                partyAcctgPreference.set("lastQuoteNumber", context.get("lastQuoteNumber"));
            }
            partyAcctgPreference.store();
        }

        return result;

    }

}

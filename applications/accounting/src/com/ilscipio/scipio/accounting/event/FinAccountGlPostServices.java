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
package com.ilscipio.scipio.accounting.event;

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountGlPostServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class FinAccountGlPostServices {

    private static final String MODULE = FinAccountGlPostServices.class.getName();


    /**
     * Post a Financial Account Transaction to the General Ledger
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String postFinAccountTransToGl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object glAccountId = null;
        GenericValue finAccountTypeGlAccount = null;
        Map<String, Object> quickCreateAcctgTransAndEntries = null;
        Object requiredField = null;
        Object creditDebit = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        GenericValue finAccountTrans = null;
        try {
            finAccountTrans = EntityQuery.use(delegator)
                    .from("FinAccountTrans")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object finAccountId = ((Map<String, Object>) finAccountTrans).get("finAccountId");
        GenericValue finAccount = null;
        try {
            finAccount = EntityQuery.use(delegator)
                    .from("FinAccount")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object organizationPartyId = ((Map<String, Object>) finAccount).get("organizationPartyId");
        if (UtilValidate.isNotEmpty(((Map<String, Object>) finAccount).get("postToGlAccountId"))) {
            glAccountId = ((Map<String, Object>) finAccount).get("postToGlAccountId");
        } else {
            try {
                finAccountTypeGlAccount = EntityQuery.use(delegator)
                        .from("FinAccountTypeGlAccount")
                        .where(UtilMisc.toMap("organizationPartyId", organizationPartyId, "finAccountTypeId", ((Map<String, Object>) finAccount).get("finAccountTypeId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FinAccountTypeGlAccount: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(finAccountTypeGlAccount)) {
                glAccountId = ((Map<String, Object>) finAccountTypeGlAccount).get("glAccountId");
            }
        }
        quickCreateAcctgTransAndEntries.put("finAccountTransId", context.get("finAccountTransId"));
        quickCreateAcctgTransAndEntries.put("transactionDate", nowTimestamp);
        quickCreateAcctgTransAndEntries.put("glFiscalTypeId", "ACTUAL");
        quickCreateAcctgTransAndEntries.put("partyId", ((Map<String, Object>) finAccountTrans).get("partyId"));
        quickCreateAcctgTransAndEntries.put("isPosted", "N");
        quickCreateAcctgTransAndEntries.put("organizationPartyId", ((Map<String, Object>) finAccount).get("organizationPartyId"));
        quickCreateAcctgTransAndEntries.put("amount", ((Map<String, Object>) finAccountTrans).get("amount"));
        quickCreateAcctgTransAndEntries.put("acctgTransEntryTypeId", "_NA_");
        Object quickCreateAcctgTransAndEntries_creditGlAccountId = null;
        Object quickCreateAcctgTransAndEntries_debitGlAccountId = null;
        Object quickCreateAcctgTransAndEntries_acctgTransTypeId = null;
        if ("DEPOSIT".equals(((Map<String, Object>) finAccountTrans).get("finAccountTransTypeId"))) {
            quickCreateAcctgTransAndEntries.put("creditGlAccountId", context.get("glAccountId"));
            quickCreateAcctgTransAndEntries.put("debitGlAccountId", glAccountId);
            quickCreateAcctgTransAndEntries.put("acctgTransTypeId", "RECEIPT");
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) quickCreateAcctgTransAndEntries).get("debitGlAccountId"))) {
            creditDebit = "Debit";
            requiredField = "glAccountId";
            {
                String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingFinAccountCannotPost", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) quickCreateAcctgTransAndEntries).get("creditGlAccountId"))) {
            creditDebit = "credit";
            requiredField = "glAccountId";
            {
                String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingFinAccountCannotPost", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(organizationPartyId)) {
            creditDebit = null;
            requiredField = "organizationPartyId";
            {
                String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingFinAccountCannotPost", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("quickCreateAcctgTransAndEntries", quickCreateAcctgTransAndEntries);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling quickCreateAcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }

}

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

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.accounting.invoice.InvoiceWorker;
import org.ofbiz.accounting.util.UtilAccounting;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/ledger/GeneralLedgerServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class GeneralLedgerServices {

    private static final String MODULE = GeneralLedgerServices.class.getName();


    /**
     * Create an GlAccount
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("GlAccount");
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(context.get("glAccountId"))) {
            ((GenericValue) newEntity).put("glAccountId", delegator.getNextSeqId("GlAccount"));
        } else {
            newEntity.setPKFields((Map<String, Object>) context);
        }
        result.put("glAccountId", ((Map<String, Object>) newEntity).get("glAccountId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update an GlAccount
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("GlAccount");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete an GlAccount
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("GlAccount");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create GlAccountOrganization
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createGlAccountOrganization(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("GlAccountOrganization");
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            Timestamp parameters_fromDate = new Timestamp(System.currentTimeMillis());
        }
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update GlAccountOrganization
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateGlAccountOrganization(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("GlAccountOrganization");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete GlAccountOrganization
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteGlAccountOrganization(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("GlAccountOrganization");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Creates an AcctgTrans and two offsetting AcctgTransEntry records
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String quickCreateAcctgTransAndEntries(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createAcctgTransParams = new HashMap<>();
        // set-service-fields from "parameters" to "createAcctgTransParams" for service "createAcctgTrans"
        createAcctgTransParams.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isEmpty(((Map<String, Object>) createAcctgTransParams).get("transactionDate"))) {
            Timestamp createAcctgTransParams_transactionDate = new Timestamp(System.currentTimeMillis());
        }
        Object acctgTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTrans", createAcctgTransParams);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acctgTransId = serviceResult.get("acctgTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createAcctgTransEntryParams = new HashMap<>();
        // set-service-fields from "parameters" to "createAcctgTransEntryParams" for service "createAcctgTransEntry"
        createAcctgTransEntryParams.putAll(UtilMisc.toMap(context));
        createAcctgTransEntryParams.put("acctgTransId", acctgTransId);
        createAcctgTransEntryParams.put("glAccountId", context.get("debitGlAccountId"));
        createAcctgTransEntryParams.put("debitCreditFlag", "D");
        createAcctgTransEntryParams.put("acctgTransEntryTypeId", "_NA_");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransEntry", createAcctgTransEntryParams);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // set-service-fields from "parameters" to "createAcctgTransEntryParams" for service "createAcctgTransEntry"
        createAcctgTransEntryParams.putAll(UtilMisc.toMap(context));
        createAcctgTransEntryParams.put("acctgTransId", acctgTransId);
        createAcctgTransEntryParams.put("glAccountId", context.get("creditGlAccountId"));
        createAcctgTransEntryParams.put("debitCreditFlag", "C");
        createAcctgTransEntryParams.put("acctgTransEntryTypeId", "_NA_");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransEntry", createAcctgTransEntryParams);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * Create an GlJournal
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createGlJournal(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("GlJournal");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(context.get("glJournalId"))) {
            ((GenericValue) newEntity).put("glJournalId", delegator.getNextSeqId("GlJournal"));
        }
        result.put("glJournalId", ((Map<String, Object>) newEntity).get("glJournalId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update an GlJournal
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateGlJournal(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("GlJournal")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlJournal: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete an GlJournal
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteGlJournal(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("GlJournal")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlJournal: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Calculate Trial Balance for a GlJournal
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String calculateGlJournalTrialBalance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object creditTotal = null;
        Object debitCreditDifference = null;
        Object serviceResults = null;
        Object callServiceMap = null;
        Object debitTotal = null;
        List<GenericValue> acctgTransList = null;
        try {
            acctgTransList = EntityQuery.use(delegator)
                    .from("AcctgTrans")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (acctgTransList != null) {
            for (GenericValue acctgTrans : acctgTransList) {
                callServiceMap = null;
                serviceResults = null;
                ((Map<String, Object>) callServiceMap).put("acctgTransId", ((Map<String, Object>) acctgTrans).get("acctgTransId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("calculateAcctgTransTrialBalance", (Map<String, Object>) callServiceMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    serviceResults = serviceResult;
                } catch (Exception e) {
                    Debug.logError(e, "Error calling calculateAcctgTransTrialBalance: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                debitTotal = new BigDecimal(debitTotal.toString());
                creditTotal = new BigDecimal(creditTotal.toString());
                debitCreditDifference = new BigDecimal(debitCreditDifference.toString());
            }
        }
        result.put("debitTotal", debitTotal);
        result.put("creditTotal", creditTotal);
        result.put("debitCreditDifference", debitCreditDifference);

        return "success";
    }


    /**
     * Post a GlJournal
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String postGlJournal(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object callServiceMap = null;
        Map<String, Object> trialBalanceCallMap = new HashMap<>();
        trialBalanceCallMap.put("glJournalId", context.get("glJournalId"));
        Map<String, Object> trialBalanceResultMap = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("calculateGlJournalTrialBalance", trialBalanceCallMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            trialBalanceResultMap = serviceResult;
        } catch (Exception e) {
            Debug.logError(e, "Error calling calculateGlJournalTrialBalance: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (!"0".equals(((Map<String, Object>) trialBalanceResultMap).get("debitCreditDifference"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotPostingGlJournalTrialBalanceFailed", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        List<GenericValue> acctgTransList = null;
        try {
            acctgTransList = EntityQuery.use(delegator)
                    .from("AcctgTrans")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (acctgTransList != null) {
            for (GenericValue acctgTrans : acctgTransList) {
                callServiceMap = null;
                ((Map<String, Object>) callServiceMap).put("acctgTransId", ((Map<String, Object>) acctgTrans).get("acctgTransId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("postAcctgTrans", (Map<String, Object>) callServiceMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling postAcctgTrans: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Create an GlReconciliation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createGlReconciliation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        newEntity = delegator.makeValue("GlReconciliation");
        newEntity.setNonPKFields((Map<String, Object>) context);
        ((GenericValue) newEntity).put("glReconciliationId", delegator.getNextSeqId("GlReconciliation"));
        result.put("glReconciliationId", ((Map<String, Object>) newEntity).get("glReconciliationId"));
        newEntity.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        newEntity.put("createdByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("statusId"))) {
            newEntity.put("statusId", "GLREC_CREATED");
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update an GlReconciliation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateGlReconciliation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> setGlReconciliationStatusMap = new HashMap<>();
        // set-service-fields from "parameters" to "setGlReconciliationStatusMap" for service "setGlReconciliationStatus"
        setGlReconciliationStatusMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("setGlReconciliationStatus", setGlReconciliationStatusMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling setGlReconciliationStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("GlReconciliation")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlReconciliation: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        lookedUpValue.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete an GlReconciliation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteGlReconciliation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("GlReconciliation")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlReconciliation: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update GlReconciliation LastModified Info
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateGlReconciliationLastModified(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpGLR = null;
        try {
            lookedUpGLR = EntityQuery.use(delegator)
                    .from("GlReconciliation")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlReconciliation: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpGLR.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        try {
            delegator.store(lookedUpGLR);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Add Entry To GlReconciliation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createGlReconciliationEntry(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object statusId = null;
        GenericValue acctgTransEntry = null;
        try {
            acctgTransEntry = EntityQuery.use(delegator)
                    .from("AcctgTransEntry")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTransEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("AES_RECONCILED".equals(((Map<String, Object>) acctgTransEntry).get("reconcileStatusId"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotReconcilingTransEntryAlreadyReconciled", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        GenericValue newEntity = delegator.makeValue("GlReconciliationEntry");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> updateAcctgTransEntryInMap = new HashMap<>();
        updateAcctgTransEntryInMap.put("acctgTransId", context.get("acctgTransId"));
        updateAcctgTransEntryInMap.put("acctgTransEntrySeqId", context.get("acctgTransEntrySeqId"));
        updateAcctgTransEntryInMap.put("reconcileStatusId", "AES_RECONCILED");
        updateAcctgTransEntryInMap.put("amount", context.get("reconciledAmount"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateAcctgTransEntry", updateAcctgTransEntryInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateAcctgTransEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue glReconciliation = null;
        try {
            glReconciliation = EntityQuery.use(delegator)
                    .from("GlReconciliation")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlReconciliation: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("GLREC_CREATED".equals(((Map<String, Object>) glReconciliation).get("statusId"))) {
            statusId = "GLREC_RECONCILED";
            result.put("statusId", statusId);
        }
        String inlineResult = updateGlReconciliationLastModified(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }

        return "success";
    }


    /**
     * Update Entry To GlReconciliation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateGlReconciliationEntry(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("GlReconciliationEntry")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlReconciliationEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        String result = updateGlReconciliationLastModified(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Remove Entry From GlReconciliation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteGlReconciliationEntry(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("GlReconciliationEntry")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlReconciliationEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        String result = updateGlReconciliationLastModified(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Completes, if possible, the AcctgTransEntries using the mappings defined in the gl setup
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String completeAcctgTransEntries(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue glAccountType = null;
        Object getGlAccountFromAccountTypeInMap = null;
        GenericValue acctgTrans = null;
        try {
            acctgTrans = EntityQuery.use(delegator)
                    .from("AcctgTrans")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("Y".equals(((Map<String, Object>) acctgTrans).get("isPosted"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingTransactionHasBeenAlreadyPosted", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        List<GenericValue> acctgTransEntries = null;
        try {
            acctgTransEntries = acctgTrans.getRelated("AcctgTransEntry", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related AcctgTransEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (acctgTransEntries != null) {
            for (GenericValue acctgTransEntry : acctgTransEntries) {
                if (UtilValidate.isEmpty(((Map<String, Object>) acctgTransEntry).get("glAccountId"))) {
                    getGlAccountFromAccountTypeInMap = null;
                    ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("organizationPartyId", ((Map<String, Object>) acctgTransEntry).get("organizationPartyId"));
                    ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("acctgTransTypeId", ((Map<String, Object>) acctgTrans).get("acctgTransTypeId"));
                    ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("glAccountTypeId", ((Map<String, Object>) acctgTransEntry).get("glAccountTypeId"));
                    ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("debitCreditFlag", ((Map<String, Object>) acctgTransEntry).get("debitCreditFlag"));
                    ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("productId", ((Map<String, Object>) acctgTransEntry).get("productId"));
                    ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("partyId", ((Map<String, Object>) acctgTrans).get("partyId"));
                    ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("roleTypeId", ((Map<String, Object>) acctgTrans).get("roleTypeId"));
                    ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("invoiceId", ((Map<String, Object>) acctgTrans).get("invoiceId"));
                    ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("paymentId", ((Map<String, Object>) acctgTrans).get("paymentId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("getGlAccountFromAccountType", (Map<String, Object>) getGlAccountFromAccountTypeInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        acctgTransEntry.put("glAccountId", serviceResult.get("glAccountId"));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling getGlAccountFromAccountType: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                if (UtilValidate.isEmpty(((Map<String, Object>) acctgTransEntry).get("origAmount"))) {
                    acctgTransEntry.put("origAmount", ((Map<String, Object>) acctgTransEntry).get("amount"));
                }
                try {
                    glAccountType = EntityQuery.use(delegator)
                            .from("GlAccountType")
                            .where(UtilMisc.toMap("glAccountTypeId", ((Map<String, Object>) acctgTransEntry).get("glAccountTypeId")))
                            .cache()
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying GlAccountType: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(glAccountType)) {
                    acctgTransEntry.remove("glAccountTypeId");
                }
                try {
                    delegator.store(acctgTransEntry);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Verifies and posts a set of AcctgTransEntries
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransAndEntries(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue glAccountType = null;
        Map<String, Object> partyAccountingPreferencesCallMap = null;
        Object convertUomInMap = null;
        Object getGlAccountFromAccountTypeInMap = null;
        GenericValue partyRole = null;
        GenericValue acctgTransEntry = null;
        List<Object> normalizedAcctgTransEntries = null;
        Object partyAcctgPreference = null;
        Object acctgTransId = null;
        Map<String, Object> createAcctgTransEntryParams = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        Map<String, Object> createAcctgTransParams = new HashMap<>();
        // set-service-fields from "parameters" to "createAcctgTransParams" for service "createAcctgTrans"
        createAcctgTransParams.putAll(UtilMisc.toMap(context));
        if (context.get("acctgTransEntries") != null) {
            for (GenericValue acctgTransEntry_iter : (List<GenericValue>) context.get("acctgTransEntries")) {
                acctgTransEntry = acctgTransEntry_iter;
                try {
                    partyRole = EntityQuery.use(delegator)
                            .from("PartyRole")
                            .where(UtilMisc.toMap("partyId", ((Map<String, Object>) acctgTransEntry).get("organizationPartyId"), "roleTypeId", "INTERNAL_ORGANIZATIO"))
                            .cache()
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PartyRole: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(partyRole)) {
                    Debug.logWarning("The party with id [" + ((Map<String, Object>) acctgTransEntry).get("organizationPartyId") + "] is not an internal organization; the following accounting transaction will be ignored: " + acctgTransEntry, MODULE);
                } else {
                    partyAccountingPreferencesCallMap.put("organizationPartyId", ((Map<String, Object>) acctgTransEntry).get("organizationPartyId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("getPartyAccountingPreferences", partyAccountingPreferencesCallMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        partyAcctgPreference = serviceResult.get("partyAccountingPreference");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling getPartyAccountingPreferences: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isEmpty(partyAcctgPreference)) {
                        Debug.logWarning("The internal organization with id [" + ((Map<String, Object>) acctgTransEntry).get("organizationPartyId") + "] has no PartyAcctgPreference setting; the following accounting transaction will be ignored: " + acctgTransEntry, MODULE);
                    } else {
                        if (UtilValidate.isEmpty(((Map<String, Object>) acctgTransEntry).get("amount"))) {
                            if (UtilValidate.isNotEmpty(((Map<String, Object>) acctgTransEntry).get("origAmount"))) {
                                if (UtilValidate.isEmpty(((Map<String, Object>) acctgTransEntry).get("origCurrencyUomId"))) {
                                    acctgTransEntry.put("origCurrencyUomId", ((Map<String, Object>) partyAcctgPreference).get("baseCurrencyUomId"));
                                }
                                acctgTransEntry.put("currencyUomId", ((Map<String, Object>) partyAcctgPreference).get("baseCurrencyUomId"));
                                if (!java.util.Objects.equals(((Map<String, Object>) acctgTransEntry).get("origCurrencyUomId"), ((Map<String, Object>) acctgTransEntry).get("currencyUomId"))) {
                                    convertUomInMap = null;
                                    ((Map<String, Object>) convertUomInMap).put("originalValue", ((Map<String, Object>) acctgTransEntry).get("origAmount"));
                                    ((Map<String, Object>) convertUomInMap).put("uomId", ((Map<String, Object>) acctgTransEntry).get("origCurrencyUomId"));
                                    ((Map<String, Object>) convertUomInMap).put("uomIdTo", ((Map<String, Object>) acctgTransEntry).get("currencyUomId"));
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) createAcctgTransParams).get("transactionDate"))) {
                                        ((Map<String, Object>) convertUomInMap).put("asOfDate", ((Map<String, Object>) createAcctgTransParams).get("transactionDate"));
                                    }
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("convertUom", (Map<String, Object>) convertUomInMap);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                            return "error";
                                        }
                                        acctgTransEntry.put("amount", serviceResult.get("convertedValue"));
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling convertUom: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                } else {
                                    acctgTransEntry.put("amount", ((Map<String, Object>) acctgTransEntry).get("origAmount"));
                                }
                            }
                        }
                        if (UtilValidate.isEmpty(((Map<String, Object>) acctgTransEntry).get("glAccountId"))) {
                            getGlAccountFromAccountTypeInMap = null;
                            ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("organizationPartyId", ((Map<String, Object>) acctgTransEntry).get("organizationPartyId"));
                            ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("acctgTransTypeId", context.get("acctgTransTypeId"));
                            ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("glAccountTypeId", ((Map<String, Object>) acctgTransEntry).get("glAccountTypeId"));
                            ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("debitCreditFlag", ((Map<String, Object>) acctgTransEntry).get("debitCreditFlag"));
                            ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("productId", ((Map<String, Object>) acctgTransEntry).get("productId"));
                            ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("partyId", context.get("partyId"));
                            ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("roleTypeId", context.get("roleTypeId"));
                            ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("invoiceId", context.get("invoiceId"));
                            ((Map<String, Object>) getGlAccountFromAccountTypeInMap).put("paymentId", context.get("paymentId"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("getGlAccountFromAccountType", (Map<String, Object>) getGlAccountFromAccountTypeInMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                                acctgTransEntry.put("glAccountId", serviceResult.get("glAccountId"));
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling getGlAccountFromAccountType: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                        if (UtilValidate.isEmpty(((Map<String, Object>) acctgTransEntry).get("origAmount"))) {
                            acctgTransEntry.put("origAmount", ((Map<String, Object>) acctgTransEntry).get("amount"));
                        }
                        try {
                            glAccountType = EntityQuery.use(delegator)
                                    .from("GlAccountType")
                                    .where(UtilMisc.toMap("glAccountTypeId", ((Map<String, Object>) acctgTransEntry).get("glAccountTypeId")))
                                    .cache()
                                    .queryOne();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying GlAccountType: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (UtilValidate.isEmpty(glAccountType)) {
                            acctgTransEntry.remove("glAccountTypeId");
                        }
                        normalizedAcctgTransEntries.add(acctgTransEntry);
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(normalizedAcctgTransEntries)) {
            if (UtilValidate.isEmpty(((Map<String, Object>) createAcctgTransParams).get("transactionDate"))) {
                Timestamp createAcctgTransParams_transactionDate = new Timestamp(System.currentTimeMillis());
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTrans", createAcctgTransParams);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                acctgTransId = serviceResult.get("acctgTransId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createAcctgTrans: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (normalizedAcctgTransEntries != null) {
                for (Object acctgTransEntry_iter : normalizedAcctgTransEntries) {
                    acctgTransEntry = (GenericValue) acctgTransEntry_iter;
                    if (((Comparable) ((Map<String, Object>) acctgTransEntry).get("origAmount")).compareTo("0") < 0) {
                        Debug.logVerbose(acctgTransEntry + " is going to get inverted", MODULE);
                        acctgTransEntry.set("origAmount", new BigDecimal(((Map<String, Object>) acctgTransEntry).get("origAmount").toString()));
                        acctgTransEntry.set("amount", new BigDecimal(((Map<String, Object>) acctgTransEntry).get("amount").toString()));
                        if ("D".equals(((Map<String, Object>) acctgTransEntry).get("debitCreditFlag"))) {
                            acctgTransEntry.put("debitCreditFlag", "C");
                        } else {
                            if ("C".equals(((Map<String, Object>) acctgTransEntry).get("debitCreditFlag"))) {
                                acctgTransEntry.put("debitCreditFlag", "D");
                            }
                        }
                    }
                    createAcctgTransEntryParams = null;
                    // set-service-fields from "acctgTransEntry" to "createAcctgTransEntryParams" for service "createAcctgTransEntry"
                    createAcctgTransEntryParams.putAll(UtilMisc.toMap(acctgTransEntry));
                    createAcctgTransEntryParams.put("acctgTransId", acctgTransId);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransEntry", createAcctgTransEntryParams);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createAcctgTransEntry: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        } else {
            Debug.logWarning("Cannot process an accounting transactions with empty list of entries.", MODULE);
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * Look up a GlAccountId from GlAccountTypeId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getGlAccountFromAccountType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue lookedUpValue = null;
        List<GenericValue> fixedAssetTypeGlAccounts = null;
        GenericValue fixedAssetTypeGlAccount = null;
        GenericValue fixedAsset = null;
        GenericValue paymentMethod = null;
        GenericValue payment = null;
        GenericValue creditCard = null;
        List<GenericValue> productCategoryMembers = null;
        Object glAccountTypeDefault = null;
        GenericValue invoiceItemType = null;
        if ("ITEM_VARIANCE".equals(context.get("acctgTransTypeId"))) {
            getVarianceReasonGlAccountInline(request, response);
            Object varianceReasonGlAccount = null;
            if (UtilValidate.isNotEmpty(((Map<String, Object>) varianceReasonGlAccount).get("glAccountId"))) {
                result.put("glAccountId", ((Map<String, Object>) varianceReasonGlAccount).get("glAccountId"));
                return "success";
            }
        }
        if ("DEPRECIATION".equals(context.get("acctgTransTypeId"))) {
            if (UtilValidate.isNotEmpty(context.get("fixedAssetId"))) {
                try {
                    fixedAssetTypeGlAccounts = EntityQuery.use(delegator)
                            .from("FixedAssetTypeGlAccount")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying FixedAssetTypeGlAccount: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(fixedAssetTypeGlAccounts)) {
                    try {
                        fixedAsset = EntityQuery.use(delegator)
                                .from("FixedAsset")
                                .where(UtilMisc.toMap("fixedAssetId", context.get("fixedAssetId")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying FixedAsset: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    try {
                        fixedAssetTypeGlAccounts = EntityQuery.use(delegator)
                                .from("FixedAssetTypeGlAccount")
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying FixedAssetTypeGlAccount: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                fixedAssetTypeGlAccount = EntityUtil.getFirst((List<GenericValue>) fixedAssetTypeGlAccounts);
                if ((!(UtilValidate.isEmpty(((Map<String, Object>) fixedAssetTypeGlAccount).get("accDepGlAccountId"))) && "C".equals(context.get("debitCreditFlag")))) {
                    result.put("glAccountId", ((Map<String, Object>) fixedAssetTypeGlAccount).get("accDepGlAccountId"));
                    return "success";
                }
                if ((!(UtilValidate.isEmpty(((Map<String, Object>) fixedAssetTypeGlAccount).get("depGlAccountId"))) && "D".equals(context.get("debitCreditFlag")))) {
                    result.put("glAccountId", ((Map<String, Object>) fixedAssetTypeGlAccount).get("depGlAccountId"));
                    return "success";
                }
            }
        }
        if ((!(UtilValidate.isEmpty(context.get("glAccountTypeId"))) && !(UtilValidate.isEmpty(context.get("partyId"))) && !(UtilValidate.isEmpty(context.get("roleTypeId"))))) {
            getPartyGlAccountInline(request, response);
            Object partyGlAccount = null;
            if (UtilValidate.isNotEmpty(((Map<String, Object>) partyGlAccount).get("glAccountId"))) {
                result.put("glAccountId", ((Map<String, Object>) partyGlAccount).get("glAccountId"));
                return "success";
            }
        }
        if (((("OUTGOING_PAYMENT".equals(context.get("acctgTransTypeId")) && "C".equals(context.get("debitCreditFlag"))) || ("INCOMING_PAYMENT".equals(context.get("acctgTransTypeId")) && "D".equals(context.get("debitCreditFlag")))) && !(UtilValidate.isEmpty(context.get("paymentId"))))) {
            try {
                payment = EntityQuery.use(delegator)
                        .from("Payment")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                paymentMethod = payment.getRelatedOne("PaymentMethod", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one PaymentMethod: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) paymentMethod).get("glAccountId"))) {
                result.put("glAccountId", ((Map<String, Object>) paymentMethod).get("glAccountId"));
                return "success";
            }
            if ("CREDIT_CARD".equals(((Map<String, Object>) payment).get("paymentMethodTypeId"))) {
                try {
                    creditCard = payment.getRelatedOne("CreditCard", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one CreditCard: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                getCreditCardTypeGlAccountInline(request, response);
                Object creditCardTypeGlAccount = null;
                if (UtilValidate.isNotEmpty(((Map<String, Object>) creditCardTypeGlAccount).get("glAccountId"))) {
                    result.put("glAccountId", ((Map<String, Object>) creditCardTypeGlAccount).get("glAccountId"));
                    return "success";
                }
            }
            getPaymentMethodTypeGlAccountInline(request, response);
            Object paymentMethodTypeGlAccount = null;
            if (UtilValidate.isNotEmpty(((Map<String, Object>) paymentMethodTypeGlAccount).get("glAccountId"))) {
                result.put("glAccountId", ((Map<String, Object>) paymentMethodTypeGlAccount).get("glAccountId"));
                return "success";
            }
            return "success";
        }
        if (UtilValidate.isNotEmpty(context.get("productId"))) {
            getProductGlAccountInline(request, response);
            GenericValue productCategoryMember = null;
            Object productGlAccount = null;
            if (UtilValidate.isEmpty(((Map<String, Object>) productGlAccount).get("glAccountId"))) {
                try {
                    productCategoryMembers = EntityQuery.use(delegator)
                            .from("ProductCategoryMember")
                            .where(UtilMisc.toMap("productId", context.get("productId")))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProductCategoryMember: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (productCategoryMembers != null) {
                    for (GenericValue productCategoryMemberEntry : productCategoryMembers) {
                        getProductCategoryGlAccountInline(request, response);
                        Object productCategoryGlAccount = null;
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) productCategoryGlAccount).get("glAccountId"))) {
                            result.put("glAccountId", ((Map<String, Object>) productCategoryGlAccount).get("glAccountId"));
                            return "success";
                        }
                    }
                }
            } else {
                lookedUpValue.put("glAccountId", ((Map<String, Object>) productGlAccount).get("glAccountId"));
            }
        }
        Object parameters_glAccountTypeId = null;
        if (((("PURCHASE_INVOICE".equals(context.get("acctgTransTypeId")) && "D".equals(context.get("debitCreditFlag"))) || ("CUST_RTN_INVOICE".equals(context.get("acctgTransTypeId")) && "D".equals(context.get("debitCreditFlag"))) || ("SALES_INVOICE".equals(context.get("acctgTransTypeId")) && "C".equals(context.get("debitCreditFlag")))) && !(UtilValidate.isEmpty(context.get("invoiceId"))) && !(UtilValidate.isEmpty(context.get("glAccountTypeId"))))) {
            getInvoiceItemTypeGlAccountInline(request, response);
            Object invoiceItemTypeGlAccount = null;
            if (UtilValidate.isNotEmpty(((Map<String, Object>) invoiceItemTypeGlAccount).get("glAccountId"))) {
                result.put("glAccountId", ((Map<String, Object>) invoiceItemTypeGlAccount).get("glAccountId"));
                return "success";
            }
            try {
                invoiceItemType = EntityQuery.use(delegator)
                        .from("InvoiceItemType")
                        .where(UtilMisc.toMap("invoiceItemTypeId", context.get("glAccountTypeId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InvoiceItemType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) invoiceItemType).get("defaultGlAccountId"))) {
                result.put("glAccountId", ((Map<String, Object>) invoiceItemType).get("defaultGlAccountId"));
                return "success";
            }
            if (UtilValidate.isNotEmpty(context.get("productId"))) {
                if ("PURCHASE_INVOICE".equals(context.get("acctgTransTypeId"))) {
                    context.put("glAccountTypeId", "UNINVOICED_SHIP_RCPT");
                    // getGlAccountTypeDefaultInline: Look up GlAccountTypeDefault
                    try {
                        lookedUpValue = EntityQuery.use(delegator)
                                .from("GlAccountTypeDefault")
                                .where(UtilMisc.toMap("organizationPartyId", context.get("organizationPartyId"), "glAccountTypeId", context.get("glAccountTypeId")))
                                .cache()
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying GlAccountTypeDefault: " + e.getMessage(), MODULE);
                    }
                    glAccountTypeDefault = lookedUpValue;
                }
                if ("CUST_RTN_INVOICE".equals(context.get("acctgTransTypeId"))) {
                    context.put("glAccountTypeId", "SALES_RETURNS");
                    // getGlAccountTypeDefaultInline: Look up GlAccountTypeDefault
                    try {
                        lookedUpValue = EntityQuery.use(delegator)
                                .from("GlAccountTypeDefault")
                                .where(UtilMisc.toMap("organizationPartyId", context.get("organizationPartyId"), "glAccountTypeId", context.get("glAccountTypeId")))
                                .cache()
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying GlAccountTypeDefault: " + e.getMessage(), MODULE);
                    }
                    glAccountTypeDefault = lookedUpValue;
                }
                if ("SALES_INVOICE".equals(context.get("acctgTransTypeId"))) {
                    context.put("glAccountTypeId", "SALES_ACCOUNT");
                    // getGlAccountTypeDefaultInline: Look up GlAccountTypeDefault
                    try {
                        lookedUpValue = EntityQuery.use(delegator)
                                .from("GlAccountTypeDefault")
                                .where(UtilMisc.toMap("organizationPartyId", context.get("organizationPartyId"), "glAccountTypeId", context.get("glAccountTypeId")))
                                .cache()
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying GlAccountTypeDefault: " + e.getMessage(), MODULE);
                    }
                    glAccountTypeDefault = lookedUpValue;
                }
                if (UtilValidate.isNotEmpty(((Map<String, Object>) glAccountTypeDefault).get("glAccountId"))) {
                    result.put("glAccountId", ((Map<String, Object>) glAccountTypeDefault).get("glAccountId"));
                    return "success";
                }
            }
            return "success";
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) lookedUpValue).get("glAccountId"))) {
            // getGlAccountTypeDefaultInline: Look up GlAccountTypeDefault
            try {
                lookedUpValue = EntityQuery.use(delegator)
                        .from("GlAccountTypeDefault")
                        .where(UtilMisc.toMap("organizationPartyId", context.get("organizationPartyId"), "glAccountTypeId", context.get("glAccountTypeId")))
                        .cache()
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying GlAccountTypeDefault: " + e.getMessage(), MODULE);
            }
        }
        result.put("glAccountId", ((Map<String, Object>) lookedUpValue).get("glAccountId"));

        return "success";
    }


    /**
     * Get an ownerPartyId from inventoryItemId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getInventoryItemOwner(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue facility = null;
        GenericValue inventoryItem = null;
        try {
            inventoryItem = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) inventoryItem).get("ownerPartyId"))) {
            try {
                facility = inventoryItem.getRelatedOne("Facility", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one Facility: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("ownerPartyId", ((Map<String, Object>) facility).get("ownerPartyId"));
        } else {
            result.put("ownerPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        }

        return "success";
    }


    /**
     * Close a financial CustomTimePeriod
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String closeFinancialTimePeriod(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object amount = null;
        Object isExpenseAccount = null;
        Object totalCreditAmount = null;
        Object isDebitAccount = null;
        GenericValue glAccount = null;
        Object totalDebitAmount = null;
        Object isCreditAccount = null;
        Map<String, Object> createAcctgTransAndEntriesInMap = null;
        GenericValue creditEntry = null;
        GenericValue debitEntry = null;
        List<Object> acctgTransEntries = null;
        Object acctgTransId = null;
        Map<String, Object> inMap = null;
        GenericValue glAccountHistory = null;
        GenericValue customTimePeriod = null;
        try {
            customTimePeriod = EntityQuery.use(delegator)
                    .from("CustomTimePeriod")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustomTimePeriod: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> openTimePeriodCondition = new HashMap<>();
        openTimePeriodCondition.put("isClosed", "N");
        List<GenericValue> openChildTimePeriods = null;
        try {
            openChildTimePeriods = customTimePeriod.getRelated("ChildCustomTimePeriod", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related ChildCustomTimePeriod: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (openChildTimePeriods != null) {
            for (GenericValue openChildTimePeriod : openChildTimePeriods) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNoCustomTimePeriodClosedChild", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> findLastClosedDateInMap = new HashMap<>();
        findLastClosedDateInMap.put("organizationPartyId", ((Map<String, Object>) customTimePeriod).get("organizationPartyId"));
        findLastClosedDateInMap.put("periodTypeId", ((Map<String, Object>) customTimePeriod).get("periodTypeId"));
        Object lastClosedDate = null;
        Object lastClosedTimePeriod = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("findLastClosedDate", findLastClosedDateInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            lastClosedDate = serviceResult.get("lastClosedDate");
            lastClosedTimePeriod = serviceResult.get("lastClosedTimePeriod");
        } catch (Exception e) {
            Debug.logError(e, "Error calling findLastClosedDate: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(lastClosedDate)) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNoCustomTimePeriodClosedForClosedDate", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue expenseGlAccountClass = null;
        try {
            expenseGlAccountClass = EntityQuery.use(delegator)
                    .from("GlAccountClass")
                    .where(UtilMisc.toMap("glAccountClassId", "EXPENSE"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountClass: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object expenseAccountClassIds = null;
        try {
            expenseAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds((GenericValue) expenseGlAccountClass);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.getDescendantGlAccountClassIds: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue revenueGlAccountClass = null;
        try {
            revenueGlAccountClass = EntityQuery.use(delegator)
                    .from("GlAccountClass")
                    .where(UtilMisc.toMap("glAccountClassId", "REVENUE"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountClass: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object revenueAccountClassIds = null;
        try {
            revenueAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds((GenericValue) revenueGlAccountClass);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.getDescendantGlAccountClassIds: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue incomeGlAccountClass = null;
        try {
            incomeGlAccountClass = EntityQuery.use(delegator)
                    .from("GlAccountClass")
                    .where(UtilMisc.toMap("glAccountClassId", "INCOME"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountClass: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object incomeAccountClassIds = null;
        try {
            incomeAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds((GenericValue) incomeGlAccountClass);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.getDescendantGlAccountClassIds: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue assetGlAccountClass = null;
        try {
            assetGlAccountClass = EntityQuery.use(delegator)
                    .from("GlAccountClass")
                    .where(UtilMisc.toMap("glAccountClassId", "ASSET"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountClass: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object assetAccountClassIds = null;
        try {
            assetAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds((GenericValue) assetGlAccountClass);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.getDescendantGlAccountClassIds: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue contraAssetGlAccountClass = null;
        try {
            contraAssetGlAccountClass = EntityQuery.use(delegator)
                    .from("GlAccountClass")
                    .where(UtilMisc.toMap("glAccountClassId", "CONTRA_ASSET"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountClass: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object contraAssetAccountClassIds = null;
        try {
            contraAssetAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds((GenericValue) contraAssetGlAccountClass);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.getDescendantGlAccountClassIds: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue liabilityGlAccountClass = null;
        try {
            liabilityGlAccountClass = EntityQuery.use(delegator)
                    .from("GlAccountClass")
                    .where(UtilMisc.toMap("glAccountClassId", "LIABILITY"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountClass: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object liabilityAccountClassIds = null;
        try {
            liabilityAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds((GenericValue) liabilityGlAccountClass);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.getDescendantGlAccountClassIds: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue equityGlAccountClass = null;
        try {
            equityGlAccountClass = EntityQuery.use(delegator)
                    .from("GlAccountClass")
                    .where(UtilMisc.toMap("glAccountClassId", "EQUITY"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountClass: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object equityAccountClassIds = null;
        try {
            equityAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds((GenericValue) equityGlAccountClass);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.getDescendantGlAccountClassIds: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> acctgTransAndEntries = null;
        try {
            acctgTransAndEntries = EntityQuery.use(delegator)
                    .from("AcctgTransAndEntries")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        BigDecimal totalAmount = new BigDecimal("0.0");
        totalCreditAmount = new BigDecimal("0.0");
        totalDebitAmount = new BigDecimal("0.0");
        if (acctgTransAndEntries != null) {
            for (GenericValue acctgTransAndEntry : acctgTransAndEntries) {
                try {
                    glAccount = acctgTransAndEntry.getRelatedOne("GlAccount", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one GlAccount: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                try {
                    isCreditAccount = UtilAccounting.isCreditAccount((GenericValue) glAccount);
                } catch (Exception e) {
                    Debug.logError(e, "Error calling UtilAccounting.isCreditAccount: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                try {
                    isDebitAccount = UtilAccounting.isDebitAccount((GenericValue) glAccount);
                } catch (Exception e) {
                    Debug.logError(e, "Error calling UtilAccounting.isDebitAccount: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                try {
                    isExpenseAccount = UtilAccounting.isExpenseAccount((GenericValue) glAccount);
                } catch (Exception e) {
                    Debug.logError(e, "Error calling UtilAccounting.isExpenseAccount: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                amount = ((Map<String, Object>) acctgTransAndEntry).get("amount");
                if ("D".equals(((Map<String, Object>) acctgTransAndEntry).get("debitCreditFlag"))) {
                    totalDebitAmount = new BigDecimal(amount.toString());
                } else {
                    totalCreditAmount = new BigDecimal(amount.toString());
                }
            }
        }
        totalAmount = new BigDecimal(totalDebitAmount.toString());
        Map<String, Object> partyAccountingPreferencesCallMap = new HashMap<>();
        partyAccountingPreferencesCallMap.put("organizationPartyId", ((Map<String, Object>) customTimePeriod).get("organizationPartyId"));
        Object partyAcctgPreference = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getPartyAccountingPreferences", partyAccountingPreferencesCallMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            partyAcctgPreference = serviceResult.get("partyAccountingPreference");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getPartyAccountingPreferences: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue profitLossAccount = null;
        try {
            profitLossAccount = EntityQuery.use(delegator)
                    .from("GlAccountTypeDefault")
                    .where(UtilMisc.toMap("organizationPartyId", ((Map<String, Object>) customTimePeriod).get("organizationPartyId"), "glAccountTypeId", "PROFIT_LOSS_ACCOUNT"))
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountTypeDefault: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue profitLossAccountHistory = null;
        try {
            profitLossAccountHistory = EntityQuery.use(delegator)
                    .from("GlAccountHistory")
                    .where(UtilMisc.toMap("organizationPartyId", ((Map<String, Object>) customTimePeriod).get("organizationPartyId"), "customTimePeriodId", ((Map<String, Object>) customTimePeriod).get("customTimePeriodId"), "glAccountId", ((Map<String, Object>) profitLossAccount).get("glAccountId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountHistory: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(profitLossAccountHistory)) {
            if (!java.util.Objects.equals(((Map<String, Object>) profitLossAccountHistory).get("endingBalance"), totalAmount)) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPostedBalanceAlreadyPresent", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        } else {
            creditEntry = delegator.makeValue("AcctgTransEntry");
            creditEntry.put("debitCreditFlag", "C");
            creditEntry.put("glAccountTypeId", "RETAINED_EARNINGS");
            creditEntry.put("organizationPartyId", ((Map<String, Object>) customTimePeriod).get("organizationPartyId"));
            creditEntry.put("origAmount", totalAmount);
            creditEntry.put("origCurrencyUomId", ((Map<String, Object>) partyAcctgPreference).get("baseCurrencyUomId"));
            acctgTransEntries.add(creditEntry);
            debitEntry = delegator.makeValue("AcctgTransEntry");
            debitEntry.put("debitCreditFlag", "D");
            debitEntry.put("glAccountTypeId", "PROFIT_LOSS_ACCOUNT");
            debitEntry.put("organizationPartyId", ((Map<String, Object>) customTimePeriod).get("organizationPartyId"));
            debitEntry.put("origAmount", totalAmount);
            debitEntry.put("origCurrencyUomId", ((Map<String, Object>) partyAcctgPreference).get("baseCurrencyUomId"));
            acctgTransEntries.add(debitEntry);
            createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
            createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "PERIOD_CLOSING");
            // TODO: Convert <set-calendar> element
            createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                acctgTransId = serviceResult.get("acctgTransId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        List<GenericValue> organizationGlAccounts = null;
        try {
            organizationGlAccounts = EntityQuery.use(delegator)
                    .from("GlAccountOrganization")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountOrganization: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (organizationGlAccounts != null) {
            for (GenericValue organizationGlAccount : organizationGlAccounts) {
                try {
                    glAccountHistory = EntityQuery.use(delegator)
                            .from("GlAccountHistory")
                            .where(UtilMisc.toMap("customTimePeriodId", ((Map<String, Object>) customTimePeriod).get("customTimePeriodId"), "organizationPartyId", ((Map<String, Object>) customTimePeriod).get("organizationPartyId"), "glAccountId", ((Map<String, Object>) organizationGlAccount).get("glAccountId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying GlAccountHistory: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(glAccountHistory)) {
                    glAccountHistory = delegator.makeValue("GlAccountHistory");
                    glAccountHistory.put("customTimePeriodId", ((Map<String, Object>) customTimePeriod).get("customTimePeriodId"));
                    glAccountHistory.put("organizationPartyId", ((Map<String, Object>) customTimePeriod).get("organizationPartyId"));
                    glAccountHistory.put("glAccountId", ((Map<String, Object>) organizationGlAccount).get("glAccountId"));
                    try {
                        delegator.create(glAccountHistory);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                inMap.put("customTimePeriodId", ((Map<String, Object>) glAccountHistory).get("customTimePeriodId"));
                inMap.put("organizationPartyId", ((Map<String, Object>) glAccountHistory).get("organizationPartyId"));
                inMap.put("glAccountId", ((Map<String, Object>) glAccountHistory).get("glAccountId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("computeAndStoreGlAccountHistoryBalance", inMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling computeAndStoreGlAccountHistoryBalance: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        Map<String, Object> updateCustomTimePeriodInMap = new HashMap<>();
        updateCustomTimePeriodInMap.put("customTimePeriodId", ((Map<String, Object>) customTimePeriod).get("customTimePeriodId"));
        updateCustomTimePeriodInMap.put("organizationPartyId", ((Map<String, Object>) customTimePeriod).get("organizationPartyId"));
        updateCustomTimePeriodInMap.put("isClosed", "Y");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateCustomTimePeriod", updateCustomTimePeriodInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateCustomTimePeriod: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Compute the total debits, total credits, opening, ending balances of an account in a financial period
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String computeGlAccountBalanceForTimePeriod(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        BigDecimal endingBalance = null;
        BigDecimal openingBalance = null;
        GenericValue customTimePeriod = null;
        try {
            customTimePeriod = EntityQuery.use(delegator)
                    .from("CustomTimePeriod")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustomTimePeriod: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue glAccount = null;
        try {
            glAccount = EntityQuery.use(delegator)
                    .from("GlAccount")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> totalDebitsToOpeningDates = null;
        try {
            totalDebitsToOpeningDates = EntityQuery.use(delegator)
                    .from("AcctgTransEntrySums")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTransEntrySums: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object totalDebitsToOpeningDate = ((List<?>) totalDebitsToOpeningDates).get(0);
        List<GenericValue> totalDebitsToEndingDates = null;
        try {
            totalDebitsToEndingDates = EntityQuery.use(delegator)
                    .from("AcctgTransEntrySums")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTransEntrySums: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object totalDebitsToEndingDate = ((List<?>) totalDebitsToEndingDates).get(0);
        List<GenericValue> totalCreditsToOpeningDates = null;
        try {
            totalCreditsToOpeningDates = EntityQuery.use(delegator)
                    .from("AcctgTransEntrySums")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTransEntrySums: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object totalCreditsToOpeningDate = ((List<?>) totalCreditsToOpeningDates).get(0);
        List<GenericValue> totalCreditsToEndingDates = null;
        try {
            totalCreditsToEndingDates = EntityQuery.use(delegator)
                    .from("AcctgTransEntrySums")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTransEntrySums: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object totalCreditsToEndingDate = ((List<?>) totalCreditsToEndingDates).get(0);
        BigDecimal totalDebitsInTimePeriod = (BigDecimal) ((BigDecimal) ((Map<String, Object>) totalDebitsToEndingDate).get("amount")).subtract((BigDecimal) ((Map<String, Object>) totalDebitsToOpeningDate).get("amount"));
        BigDecimal totalCreditsInTimePeriod = (BigDecimal) ((BigDecimal) ((Map<String, Object>) totalCreditsToEndingDate).get("amount")).subtract((BigDecimal) ((Map<String, Object>) totalCreditsToOpeningDate).get("amount"));
        Object isDebit = GroovyUtil.eval("org.ofbiz.accounting.util.UtilAccounting.isDebitAccount(glAccount)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        if ("true".equals(isDebit)) {
            openingBalance = (BigDecimal) ((BigDecimal) ((Map<String, Object>) totalDebitsToOpeningDate).get("amount")).subtract((BigDecimal) ((Map<String, Object>) totalCreditsToOpeningDate).get("amount"));
            endingBalance = (BigDecimal) ((BigDecimal) ((Map<String, Object>) totalDebitsToEndingDate).get("amount")).subtract((BigDecimal) ((Map<String, Object>) totalCreditsToEndingDate).get("amount"));
        } else {
            openingBalance = (BigDecimal) ((BigDecimal) ((Map<String, Object>) totalCreditsToOpeningDate).get("amount")).subtract((BigDecimal) ((Map<String, Object>) totalDebitsToOpeningDate).get("amount"));
            endingBalance = (BigDecimal) ((BigDecimal) ((Map<String, Object>) totalCreditsToEndingDate).get("amount")).subtract((BigDecimal) ((Map<String, Object>) totalDebitsToEndingDate).get("amount"));
        }
        result.put("openingBalance", openingBalance);
        result.put("endingBalance", endingBalance);
        result.put("postedDebits", totalDebitsInTimePeriod);
        result.put("postedCredits", totalCreditsInTimePeriod);

        return "success";
    }


    /**
     * Compute and store the total debits, total credits, opening, ending balances of an account in a financial period
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String computeAndStoreGlAccountHistoryBalance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue glAccountHistory = null;
        try {
            glAccountHistory = EntityQuery.use(delegator)
                    .from("GlAccountHistory")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountHistory: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> inMap = new HashMap<>();
        inMap.put("organizationPartyId", context.get("organizationPartyId"));
        inMap.put("customTimePeriodId", context.get("customTimePeriodId"));
        inMap.put("glAccountId", context.get("glAccountId"));
        Object openingBalance = null;
        Object endingBalance = null;
        Object postedDebits = null;
        Object postedCredits = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("computeGlAccountBalanceForTimePeriod", inMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            openingBalance = serviceResult.get("openingBalance");
            endingBalance = serviceResult.get("endingBalance");
            postedDebits = serviceResult.get("postedDebits");
            postedCredits = serviceResult.get("postedCredits");
        } catch (Exception e) {
            Debug.logError(e, "Error calling computeGlAccountBalanceForTimePeriod: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        glAccountHistory.put("openingBalance", openingBalance);
        glAccountHistory.put("endingBalance", endingBalance);
        glAccountHistory.put("postedDebits", postedDebits);
        glAccountHistory.put("postedCredits", postedCredits);
        try {
            delegator.store(glAccountHistory);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Prepare data for the Income Statement
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String prepareIncomeStatement(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object totalNetIncome = null;
        Object amount = null;
        Object isExpenseAccount = null;
        Map<String, Object> glAccountTotalsExpenseMap = null;
        Object isDebitAccount = null;
        GenericValue glAccount = null;
        Map<String, Object> glAccountTotalsProfitMap = null;
        Object isCreditAccount = null;
        Object totalOfCurrentFiscalPeriod = null;
        Object creditTotal = null;
        Map<String, Object> acctgTransEntriesAndTransTotalMap = null;
        Object glAccountTotalMap = null;
        List<Object> glAccountIncomeList = null;
        Object debitTotal = null;
        List<Object> glAccountExpenseList = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        GenericValue expenseGlAccountClass = null;
        try {
            expenseGlAccountClass = EntityQuery.use(delegator)
                    .from("GlAccountClass")
                    .where(UtilMisc.toMap("glAccountClassId", "EXPENSE"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountClass: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object expenseAccountClassIds = null;
        try {
            expenseAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds((GenericValue) expenseGlAccountClass);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.getDescendantGlAccountClassIds: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue revenueGlAccountClass = null;
        try {
            revenueGlAccountClass = EntityQuery.use(delegator)
                    .from("GlAccountClass")
                    .where(UtilMisc.toMap("glAccountClassId", "REVENUE"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountClass: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object revenueAccountClassIds = null;
        try {
            revenueAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds((GenericValue) revenueGlAccountClass);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.getDescendantGlAccountClassIds: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue incomeGlAccountClass = null;
        try {
            incomeGlAccountClass = EntityQuery.use(delegator)
                    .from("GlAccountClass")
                    .where(UtilMisc.toMap("glAccountClassId", "INCOME"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountClass: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object incomeAccountClassIds = null;
        try {
            incomeAccountClassIds = UtilAccounting.getDescendantGlAccountClassIds((GenericValue) incomeGlAccountClass);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.getDescendantGlAccountClassIds: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object organizationPartyId = context.get("organizationPartyId");
        Object partyIds = GroovyUtil.eval("org.ofbiz.party.party.PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, organizationPartyId, 'GROUP_ROLLUP')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        ((List<Object>) partyIds).add(organizationPartyId);
        List<GenericValue> acctgTransAndEntries = null;
        try {
            acctgTransAndEntries = EntityQuery.use(delegator)
                    .from("AcctgTransAndEntries")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> findCustomTimePeriodsMap = new HashMap<>();
        findCustomTimePeriodsMap.put("findDate", context.get("fromDate"));
        findCustomTimePeriodsMap.put("organizationPartyId", organizationPartyId);
        Object customTimePeriodList = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("findCustomTimePeriods", findCustomTimePeriodsMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            customTimePeriodList = serviceResult.get("customTimePeriodList");
        } catch (Exception e) {
            Debug.logError(e, "Error calling findCustomTimePeriods: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue customTimePeriod = EntityUtil.getFirst((List<GenericValue>) customTimePeriodList);
        acctgTransEntriesAndTransTotalMap.put("isPosted", "Y");
        acctgTransEntriesAndTransTotalMap.put("organizationPartyId", organizationPartyId);
        acctgTransEntriesAndTransTotalMap.put("customTimePeriodStartDate", ((Map<String, Object>) customTimePeriod).get("fromDate"));
        acctgTransEntriesAndTransTotalMap.put("customTimePeriodEndDate", context.get("thruDate"));
        totalNetIncome = new BigDecimal("0.0");
        if (acctgTransAndEntries != null) {
            for (GenericValue acctgTransAndEntry : acctgTransAndEntries) {
                try {
                    glAccount = acctgTransAndEntry.getRelatedOne("GlAccount", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one GlAccount: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                try {
                    isCreditAccount = UtilAccounting.isCreditAccount((GenericValue) glAccount);
                } catch (Exception e) {
                    Debug.logError(e, "Error calling UtilAccounting.isCreditAccount: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                try {
                    isDebitAccount = UtilAccounting.isDebitAccount((GenericValue) glAccount);
                } catch (Exception e) {
                    Debug.logError(e, "Error calling UtilAccounting.isDebitAccount: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                try {
                    isExpenseAccount = UtilAccounting.isExpenseAccount((GenericValue) glAccount);
                } catch (Exception e) {
                    Debug.logError(e, "Error calling UtilAccounting.isExpenseAccount: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                amount = ((Map<String, Object>) acctgTransAndEntry).get("amount");
                if ((("D".equals(((Map<String, Object>) acctgTransAndEntry).get("debitCreditFlag")) && Boolean.TRUE.equals(isCreditAccount)) || ("C".equals(((Map<String, Object>) acctgTransAndEntry).get("debitCreditFlag")) && Boolean.TRUE.equals(isDebitAccount)))) {
                    amount = new BigDecimal(amount.toString());
                }
                if (Boolean.TRUE.equals(isExpenseAccount)) {
                    amount = new BigDecimal(amount.toString());
                }
                totalNetIncome = new BigDecimal(amount.toString());
                if (Boolean.TRUE.equals(isExpenseAccount)) {
                    if (UtilValidate.isEmpty(((Map<String, Object>) glAccountTotalsExpenseMap).get(((Map<String, Object>) glAccount).get("glAccountId")))) {
                        glAccountTotalsExpenseMap.put((String) ((Map<String, Object>) glAccount).get("glAccountId"), new BigDecimal("0.0"));
                    }
                    ((Map<String, Object>) glAccountTotalsExpenseMap).put("glAccount.glAccountId", new BigDecimal(amount.toString()));
                } else {
                    if (UtilValidate.isEmpty(((Map<String, Object>) glAccountTotalsProfitMap).get(((Map<String, Object>) glAccount).get("glAccountId")))) {
                        glAccountTotalsProfitMap.put((String) ((Map<String, Object>) glAccount).get("glAccountId"), new BigDecimal("0.0"));
                    }
                    ((Map<String, Object>) glAccountTotalsProfitMap).put("glAccount.glAccountId", new BigDecimal(amount.toString()));
                }
            }
        }
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) glAccountTotalsProfitMap).entrySet()) {
            String glAccountId = entry.getKey();
            Object totalAmount = entry.getValue();
            glAccountTotalMap = null;
            ((Map<String, Object>) glAccountTotalMap).put("glAccountId", glAccountId);
            ((Map<String, Object>) glAccountTotalMap).put("totalAmount", totalAmount);
            acctgTransEntriesAndTransTotalMap.put("glAccountId", glAccountId);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getAcctgTransEntriesAndTransTotal", acctgTransEntriesAndTransTotalMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                debitTotal = serviceResult.get("debitTotal");
                creditTotal = serviceResult.get("creditTotal");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getAcctgTransEntriesAndTransTotal: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            totalOfCurrentFiscalPeriod = (BigDecimal) ((BigDecimal) debitTotal).subtract((BigDecimal) creditTotal);
            totalOfCurrentFiscalPeriod = new BigDecimal(totalOfCurrentFiscalPeriod.toString());
            ((Map<String, Object>) glAccountTotalMap).put("totalOfCurrentFiscalPeriod", totalOfCurrentFiscalPeriod);
            glAccountIncomeList.add(glAccountTotalMap);
        }
        Map<String, Object> glAccountTotalsMap = new HashMap<>();
        glAccountTotalsMap.put("income", glAccountIncomeList);
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) glAccountTotalsExpenseMap).entrySet()) {
            String glAccountId = entry.getKey();
            Object totalAmount = entry.getValue();
            glAccountTotalMap = null;
            ((Map<String, Object>) glAccountTotalMap).put("glAccountId", glAccountId);
            ((Map<String, Object>) glAccountTotalMap).put("totalAmount", totalAmount);
            acctgTransEntriesAndTransTotalMap.put("glAccountId", glAccountId);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getAcctgTransEntriesAndTransTotal", acctgTransEntriesAndTransTotalMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                debitTotal = serviceResult.get("debitTotal");
                creditTotal = serviceResult.get("creditTotal");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getAcctgTransEntriesAndTransTotal: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            totalOfCurrentFiscalPeriod = (BigDecimal) ((BigDecimal) debitTotal).subtract((BigDecimal) creditTotal);
            totalOfCurrentFiscalPeriod = new BigDecimal(totalOfCurrentFiscalPeriod.toString());
            ((Map<String, Object>) glAccountTotalMap).put("totalOfCurrentFiscalPeriod", totalOfCurrentFiscalPeriod);
            glAccountExpenseList.add(glAccountTotalMap);
        }
        glAccountTotalsMap.put("expenses", glAccountExpenseList);
        result.put("totalNetIncome", totalNetIncome);
        result.put("glAccountTotalsMap", glAccountTotalsMap);

        return "success";
    }


    /**
     * Create an accounting transactions for a sales shipment issuance (D: INVENTORY_ACCOUNT, C: COGS_ACCOUNT)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForSalesShipmentIssuance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        BigDecimal remainingQuantity = null;
        Object totalAmount = null;
        GenericValue creditEntry = null;
        Object costInventoryItemQuantity = null;
        Object costInventoryItemAmount = null;
        Object unitCost = null;
        Object orderByString = null;
        List<Object> acctgTransEntries = null;
        List<GenericValue> costInventoryItems = null;
        Map<String, Object> getProdAvgCostMap = null;
        Map<String, Object> createDetailMap = null;
        GenericValue debitEntry = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        GenericValue itemIssuance = null;
        try {
            itemIssuance = EntityQuery.use(delegator)
                    .from("ItemIssuance")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue inventoryItem = null;
        try {
            inventoryItem = itemIssuance.getRelatedOne("InventoryItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> billToCustomers = null;
        try {
            billToCustomers = EntityQuery.use(delegator)
                    .from("OrderRole")
                    .where(UtilMisc.toMap("orderId", ((Map<String, Object>) itemIssuance).get("orderId"), "roleTypeId", "BILL_TO_CUSTOMER"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue billToCustomer = EntityUtil.getFirst((List<GenericValue>) billToCustomers);
        Map<String, Object> partyAccountingPreferencesCallMap = new HashMap<>();
        partyAccountingPreferencesCallMap.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        Object partyAcctgPreference = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getPartyAccountingPreferences", partyAccountingPreferencesCallMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            partyAcctgPreference = serviceResult.get("partyAccountingPreference");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getPartyAccountingPreferences: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        totalAmount = new BigDecimal("0.0");
        Object getProdAvgCostMap_inventoryItem = null;
        Object creditEntry_debitCreditFlag = null;
        Object creditEntry_glAccountTypeId = null;
        Object creditEntry_organizationPartyId = null;
        Object creditEntry_productId = null;
        Object creditEntry_inventoryItemId = null;
        Object creditEntry_origAmount = null;
        Object creditEntry_origCurrencyUomId = null;
        Object creditEntry_partyId = null;
        Object creditEntry_roleTypeId = null;
        Object acctgTransEntries__ = null;
        Object createDetailMap_inventoryItemId = null;
        BigDecimal createDetailMap_accountingQuantityDiff = null;
        if (("COGS_INV_COST".equals(((Map<String, Object>) partyAcctgPreference).get("cogsMethodId")) || "COGS_AVG_COST".equals(((Map<String, Object>) partyAcctgPreference).get("cogsMethodId")))) {
            if ("COGS_AVG_COST".equals(((Map<String, Object>) partyAcctgPreference).get("cogsMethodId"))) {
                getProdAvgCostMap.put("inventoryItem", inventoryItem);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("getProductAverageCost", getProdAvgCostMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    unitCost = serviceResult.get("unitCost");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling getProductAverageCost: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                unitCost = ((Map<String, Object>) inventoryItem).get("unitCost");
            }
            totalAmount = (new BigDecimal(((Map<String, Object>) itemIssuance).get("quantity").toString())).multiply(new BigDecimal(unitCost.toString()));
            creditEntry = delegator.makeValue("AcctgTransEntry");
            creditEntry.put("debitCreditFlag", "C");
            creditEntry.put("glAccountTypeId", "INVENTORY_ACCOUNT");
            creditEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
            creditEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
            creditEntry.put("inventoryItemId", ((Map<String, Object>) inventoryItem).get("inventoryItemId"));
            creditEntry.put("origAmount", totalAmount);
            creditEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
            if (UtilValidate.isNotEmpty(billToCustomer)) {
                creditEntry.put("partyId", ((Map<String, Object>) billToCustomer).get("partyId"));
                creditEntry.put("roleTypeId", ((Map<String, Object>) billToCustomer).get("roleTypeId"));
            }
            acctgTransEntries.add(creditEntry);
        } else {
            if ("COGS_FIFO".equals(((Map<String, Object>) partyAcctgPreference).get("cogsMethodId"))) {
                orderByString = "+datetimeReceived";
            }
            if ("COGS_LIFO".equals(((Map<String, Object>) partyAcctgPreference).get("cogsMethodId"))) {
                orderByString = "-datetimeReceived";
            }
            if (UtilValidate.isEmpty(orderByString)) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingCOGSCostingMethodIsNotSupported", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            try {
                costInventoryItems = EntityQuery.use(delegator)
                        .from("InventoryItem")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            remainingQuantity = (BigDecimal) ((Map<String, Object>) itemIssuance).get("quantity");
            if (costInventoryItems != null) {
                for (GenericValue costInventoryItem : costInventoryItems) {
                    if (((Comparable) remainingQuantity).compareTo(new BigDecimal("0.0")) > 0) {
                        if (remainingQuantity != null /* TODO: field compare operator less-equals */) {
                            costInventoryItemQuantity = remainingQuantity;
                            remainingQuantity = new BigDecimal("0.0");
                        } else {
                            costInventoryItemQuantity = ((Map<String, Object>) costInventoryItem).get("accountingQuantityTotal");
                            remainingQuantity = (BigDecimal) ((BigDecimal) remainingQuantity).subtract((BigDecimal) ((Map<String, Object>) costInventoryItem).get("accountingQuantityTotal"));
                        }
                        createDetailMap.put("inventoryItemId", ((Map<String, Object>) costInventoryItem).get("inventoryItemId"));
                        createDetailMap.put("accountingQuantityDiff", (BigDecimal) (new BigDecimal("-1")).multiply((BigDecimal) costInventoryItemQuantity));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        costInventoryItemAmount = (new BigDecimal(costInventoryItemQuantity.toString())).multiply(new BigDecimal(((Map<String, Object>) costInventoryItem).get("unitCost").toString()));
                        totalAmount = (new BigDecimal(costInventoryItemAmount.toString())).add(new BigDecimal(totalAmount.toString()));
                        creditEntry = delegator.makeValue("AcctgTransEntry");
                        creditEntry.put("debitCreditFlag", "C");
                        creditEntry.put("glAccountTypeId", "INVENTORY_ACCOUNT");
                        creditEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
                        creditEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
                        creditEntry.put("inventoryItemId", ((Map<String, Object>) costInventoryItem).get("inventoryItemId"));
                        creditEntry.put("origAmount", costInventoryItemAmount);
                        creditEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
                        if (UtilValidate.isNotEmpty(billToCustomer)) {
                            creditEntry.put("partyId", ((Map<String, Object>) billToCustomer).get("partyId"));
                            creditEntry.put("roleTypeId", ((Map<String, Object>) billToCustomer).get("roleTypeId"));
                        }
                        acctgTransEntries.add(creditEntry);
                        creditEntry = null;
                    }
                }
            }
            if (((Comparable) remainingQuantity).compareTo(new BigDecimal("0.0")) > 0) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotFindAccountingInventory", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        debitEntry = delegator.makeValue("AcctgTransEntry");
        debitEntry.put("debitCreditFlag", "D");
        debitEntry.put("glAccountTypeId", "COGS_ACCOUNT");
        debitEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        debitEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        debitEntry.put("origAmount", totalAmount);
        debitEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        if (UtilValidate.isNotEmpty(billToCustomer)) {
            debitEntry.put("partyId", ((Map<String, Object>) billToCustomer).get("partyId"));
            debitEntry.put("roleTypeId", ((Map<String, Object>) billToCustomer).get("roleTypeId"));
        }
        acctgTransEntries.add(debitEntry);
        Map<String, Object> createAcctgTransAndEntriesInMap = new HashMap<>();
        createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
        createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "SALES_SHIPMENT");
        createAcctgTransAndEntriesInMap.put("shipmentId", ((Map<String, Object>) itemIssuance).get("shipmentId"));
        createAcctgTransAndEntriesInMap.put("inventoryItemId", ((Map<String, Object>) inventoryItem).get("inventoryItemId"));
        createAcctgTransAndEntriesInMap.put("transactionDate", ((Map<String, Object>) itemIssuance).get("issuedDateTime"));
        createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
        Object acctgTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acctgTransId = serviceResult.get("acctgTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * Create an accounting transactions for a canceled sales shipment issuance (D: INVENTORY_ACCOUNT, C: COGS_ACCOUNT
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForCanceledSalesShipmentIssuance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue creditEntry = null;
        GenericValue debitEntry = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        GenericValue itemIssuance = null;
        try {
            itemIssuance = EntityQuery.use(delegator)
                    .from("ItemIssuance")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue inventoryItem = null;
        try {
            inventoryItem = itemIssuance.getRelatedOne("InventoryItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> billToCustomers = null;
        try {
            billToCustomers = EntityQuery.use(delegator)
                    .from("OrderRole")
                    .where(UtilMisc.toMap("orderId", ((Map<String, Object>) itemIssuance).get("orderId"), "roleTypeId", "BILL_TO_CUSTOMER"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue billToCustomer = EntityUtil.getFirst((List<GenericValue>) billToCustomers);
        Map<String, Object> getProdAvgCostMap = new HashMap<>();
        getProdAvgCostMap.put("inventoryItem", inventoryItem);
        Object unitCost = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getProductAverageCost", getProdAvgCostMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            unitCost = serviceResult.get("unitCost");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getProductAverageCost: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object origAmount = (new BigDecimal(context.get("canceledQuantity").toString())).multiply(new BigDecimal(unitCost.toString()));
        creditEntry = delegator.makeValue("AcctgTransEntry");
        creditEntry.put("debitCreditFlag", "C");
        creditEntry.put("glAccountTypeId", "COGS_ACCOUNT");
        creditEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        creditEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        creditEntry.put("origAmount", origAmount);
        creditEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        if (UtilValidate.isNotEmpty(billToCustomer)) {
            creditEntry.put("partyId", ((Map<String, Object>) billToCustomer).get("partyId"));
            creditEntry.put("roleTypeId", ((Map<String, Object>) billToCustomer).get("roleTypeId"));
        }
        List<Object> acctgTransEntries = new LinkedList<>();
        acctgTransEntries.add(creditEntry);
        debitEntry = delegator.makeValue("AcctgTransEntry");
        debitEntry.put("debitCreditFlag", "D");
        debitEntry.put("glAccountTypeId", "INVENTORY_ACCOUNT");
        debitEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        debitEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        debitEntry.put("origAmount", origAmount);
        debitEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        if (UtilValidate.isNotEmpty(billToCustomer)) {
            debitEntry.put("partyId", ((Map<String, Object>) billToCustomer).get("partyId"));
            debitEntry.put("roleTypeId", ((Map<String, Object>) billToCustomer).get("roleTypeId"));
        }
        acctgTransEntries.add(debitEntry);
        Map<String, Object> createAcctgTransAndEntriesInMap = new HashMap<>();
        createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
        createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "SALES_SHIPMENT");
        createAcctgTransAndEntriesInMap.put("shipmentId", ((Map<String, Object>) itemIssuance).get("shipmentId"));
        createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
        Object acctgTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acctgTransId = serviceResult.get("acctgTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * Create an accounting transactions for a shipment receipt (D: INVENTORY_ACCOUNT, C: UNINVOICED_SHIP_RCPT or COGS_ACCOUNT for returns)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForShipmentReceipt(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object creditAccountTypeId = null;
        Object unitCost = null;
        Map<String, Object> getProdAvgCostMap = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        GenericValue shipmentReceipt = null;
        try {
            shipmentReceipt = EntityQuery.use(delegator)
                    .from("ShipmentReceipt")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShipmentReceipt: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue inventoryItem = null;
        try {
            inventoryItem = shipmentReceipt.getRelatedOne("InventoryItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue shipment = null;
        try {
            shipment = shipmentReceipt.getRelatedOne("Shipment", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one Shipment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) shipmentReceipt).get("returnId"))) {
            creditAccountTypeId = "COGS_ACCOUNT";
        } else {
            creditAccountTypeId = "UNINVOICED_SHIP_RCPT";
        }
        Map<String, Object> partyAccountingPreferencesCallMap = new HashMap<>();
        partyAccountingPreferencesCallMap.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        Object partyAcctgPreference = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getPartyAccountingPreferences", partyAccountingPreferencesCallMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            partyAcctgPreference = serviceResult.get("partyAccountingPreference");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getPartyAccountingPreferences: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) shipmentReceipt).get("returnId"))) {
            Object getProdAvgCostMap_inventoryItem = null;
            if (("COGS_INV_COST".equals(((Map<String, Object>) partyAcctgPreference).get("cogsMethodId")) || "COGS_AVG_COST".equals(((Map<String, Object>) partyAcctgPreference).get("cogsMethodId")))) {
                if ("COGS_AVG_COST".equals(((Map<String, Object>) partyAcctgPreference).get("cogsMethodId"))) {
                    getProdAvgCostMap.put("inventoryItem", inventoryItem);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("getProductAverageCost", getProdAvgCostMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        unitCost = serviceResult.get("unitCost");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling getProductAverageCost: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                } else {
                    unitCost = ((Map<String, Object>) inventoryItem).get("unitCost");
                }
            } else {
                unitCost = ((Map<String, Object>) inventoryItem).get("unitCost");
            }
        } else {
            unitCost = ((Map<String, Object>) inventoryItem).get("unitCost");
        }
        Object origAmount = (new BigDecimal(((Map<String, Object>) shipmentReceipt).get("quantityAccepted").toString())).multiply(new BigDecimal(unitCost.toString()));
        Map<String, Object> createDetailMap = new HashMap<>();
        createDetailMap.put("inventoryItemId", ((Map<String, Object>) inventoryItem).get("inventoryItemId"));
        createDetailMap.put("accountingQuantityDiff", ((Map<String, Object>) shipmentReceipt).get("quantityAccepted"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue creditEntry = delegator.makeValue("AcctgTransEntry");
        creditEntry.put("debitCreditFlag", "C");
        creditEntry.put("glAccountTypeId", creditAccountTypeId);
        creditEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        creditEntry.put("partyId", ((Map<String, Object>) shipment).get("partyIdFrom"));
        creditEntry.put("roleTypeId", "BILL_FROM_VENDOR");
        creditEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        creditEntry.put("origAmount", origAmount);
        creditEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        List<Object> acctgTransEntries = new LinkedList<>();
        acctgTransEntries.add(creditEntry);
        GenericValue debitEntry = delegator.makeValue("AcctgTransEntry");
        debitEntry.put("debitCreditFlag", "D");
        debitEntry.put("glAccountTypeId", "INVENTORY_ACCOUNT");
        debitEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        debitEntry.put("partyId", ((Map<String, Object>) shipment).get("partyIdFrom"));
        debitEntry.put("roleTypeId", "BILL_FROM_VENDOR");
        debitEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        debitEntry.put("origAmount", origAmount);
        debitEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        acctgTransEntries.add(debitEntry);
        Map<String, Object> createAcctgTransAndEntriesInMap = new HashMap<>();
        createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
        createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "SHIPMENT_RECEIPT");
        createAcctgTransAndEntriesInMap.put("shipmentId", ((Map<String, Object>) shipmentReceipt).get("shipmentId"));
        createAcctgTransAndEntriesInMap.put("partyId", ((Map<String, Object>) shipment).get("partyIdFrom"));
        createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
        Object acctgTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acctgTransId = serviceResult.get("acctgTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * Create accounting transaction when item cost is changed (D: INV_ADJ_VAL, C: INVENTORY_ACCOUNT)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForInventoryItemCostChange(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createAcctgTransAndEntriesInMap = null;
        GenericValue creditEntry = null;
        Object origAmount = null;
        GenericValue debitEntry = null;
        List<Object> acctgTransEntries = null;
        Object acctgTransId = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        GenericValue newInventoryItemDetail = null;
        try {
            newInventoryItemDetail = EntityQuery.use(delegator)
                    .from("InventoryItemDetail")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemDetail: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue inventoryItem = null;
        try {
            inventoryItem = newInventoryItemDetail.getRelatedOne("InventoryItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> inventoryItemDetails = null;
        try {
            inventoryItemDetails = EntityQuery.use(delegator)
                    .from("InventoryItemDetail")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemDetail: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue oldInventoryItemDetail = EntityUtil.getFirst((List<GenericValue>) inventoryItemDetails);
        if (UtilValidate.isNotEmpty(oldInventoryItemDetail)) {
            origAmount = (new BigDecimal(((Map<String, Object>) oldInventoryItemDetail).get("unitCost").toString())).subtract(new BigDecimal(((Map<String, Object>) newInventoryItemDetail).get("unitCost").toString()));
            if (!"0".equals(origAmount)) {
                creditEntry = delegator.makeValue("AcctgTransEntry");
                creditEntry.put("debitCreditFlag", "C");
                creditEntry.put("glAccountTypeId", "INVENTORY_ACCOUNT");
                creditEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
                creditEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
                creditEntry.put("origAmount", origAmount);
                creditEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
                debitEntry = delegator.makeValue("AcctgTransEntry");
                debitEntry.put("debitCreditFlag", "D");
                debitEntry.put("glAccountTypeId", "INV_ADJ_VAL");
                debitEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
                debitEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
                debitEntry.put("origAmount", origAmount);
                debitEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
                acctgTransEntries.add(creditEntry);
                acctgTransEntries.add(debitEntry);
                createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
                createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "INVENTORY");
                createAcctgTransAndEntriesInMap.put("inventoryItemId", context.get("inventoryItemId"));
                createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    acctgTransId = serviceResult.get("acctgTransId");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                result.put("acctgTransId", acctgTransId);
            }
        }

        return "success";
    }


    /**
     * Create an Account Transaction For Physical Inventory Variance
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForPhysicalInventoryVariance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue inventoryItem = null;
        GenericValue creditEntry = null;
        Object origAmount = null;
        GenericValue debitEntry = null;
        List<Object> acctgTransEntries = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        List<GenericValue> inventoryItemDetails = null;
        try {
            inventoryItemDetails = EntityQuery.use(delegator)
                    .from("InventoryItemDetail")
                    .where(UtilMisc.toMap("physicalInventoryId", context.get("physicalInventoryId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItemDetail: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (inventoryItemDetails != null) {
            for (GenericValue inventoryItemDetail : inventoryItemDetails) {
                try {
                    inventoryItem = inventoryItemDetail.getRelatedOne("InventoryItem", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                origAmount = (new BigDecimal(((Map<String, Object>) inventoryItemDetail).get("quantityOnHandDiff").toString())).multiply(new BigDecimal(((Map<String, Object>) inventoryItem).get("unitCost").toString()));
                creditEntry = delegator.makeValue("AcctgTransEntry");
                creditEntry.put("debitCreditFlag", "C");
                creditEntry.put("glAccountTypeId", ((Map<String, Object>) inventoryItemDetail).get("reasonEnumId"));
                creditEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
                creditEntry.put("origAmount", origAmount);
                creditEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
                creditEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
                debitEntry = delegator.makeValue("AcctgTransEntry");
                debitEntry.put("debitCreditFlag", "D");
                debitEntry.put("glAccountTypeId", "INVENTORY_ACCOUNT");
                debitEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
                debitEntry.put("origAmount", origAmount);
                debitEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
                debitEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
                acctgTransEntries.add(creditEntry);
                acctgTransEntries.add(debitEntry);
            }
        }
        Map<String, Object> createAcctgTransAndEntriesInMap = new HashMap<>();
        createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
        createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
        createAcctgTransAndEntriesInMap.put("physicalInventoryId", context.get("physicalInventoryId"));
        createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "ITEM_VARIANCE");
        Object acctgTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acctgTransId = serviceResult.get("acctgTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * Create an accounting transactions for a Work Effort Inventory Produced (D: INVENTORY_ACCOUNT, C: WIP_INVENTORY)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForWorkEffortInventoryProduced(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        GenericValue workEffortInventoryProduced = null;
        try {
            workEffortInventoryProduced = EntityQuery.use(delegator)
                    .from("WorkEffortInventoryProduced")
                    .where(UtilMisc.toMap("workEffortId", context.get("workEffortId"), "inventoryItemId", context.get("inventoryItemId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortInventoryProduced: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue inventoryItem = null;
        try {
            inventoryItem = workEffortInventoryProduced.getRelatedOne("InventoryItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object origAmount = (new BigDecimal(((Map<String, Object>) inventoryItem).get("quantityOnHandTotal").toString())).multiply(new BigDecimal(((Map<String, Object>) inventoryItem).get("unitCost").toString()));
        Map<String, Object> createDetailMap = new HashMap<>();
        createDetailMap.put("inventoryItemId", ((Map<String, Object>) inventoryItem).get("inventoryItemId"));
        createDetailMap.put("accountingQuantityDiff", ((Map<String, Object>) inventoryItem).get("quantityOnHandTotal"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInventoryItemDetail", createDetailMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInventoryItemDetail: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue creditEntry = delegator.makeValue("AcctgTransEntry");
        creditEntry.put("debitCreditFlag", "C");
        creditEntry.put("glAccountTypeId", "WIP_INVENTORY");
        creditEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        creditEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        creditEntry.put("origAmount", origAmount);
        creditEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        List<Object> acctgTransEntries = new LinkedList<>();
        acctgTransEntries.add(creditEntry);
        GenericValue debitEntry = delegator.makeValue("AcctgTransEntry");
        debitEntry.put("debitCreditFlag", "D");
        debitEntry.put("glAccountTypeId", "INVENTORY_ACCOUNT");
        debitEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        debitEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        debitEntry.put("origAmount", origAmount);
        debitEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        acctgTransEntries.add(debitEntry);
        Map<String, Object> createAcctgTransAndEntriesInMap = new HashMap<>();
        createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
        createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "INVENTORY");
        createAcctgTransAndEntriesInMap.put("workEffortId", context.get("workEffortId"));
        createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
        Object acctgTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acctgTransId = serviceResult.get("acctgTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * Create an accounting transaction for inventory that is issued to a work effort cost (Type: INVENTORY D: INVENTORY_ACCOUNT , C: UNINVOICED_SHIP_RCPT or COGS_ACCOUNT)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForWorkEffortCost(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue workEffortGoodStandard = null;
        List<GenericValue> workEffortGoodStandards = null;
        GenericValue creditEntry = null;
        GenericValue debitEntry = null;
        GenericValue costComponent = null;
        try {
            costComponent = EntityQuery.use(delegator)
                    .from("CostComponent")
                    .where(UtilMisc.toMap("costComponentId", context.get("costComponentId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CostComponent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue costComponentCalc = null;
        try {
            costComponentCalc = costComponent.getRelatedOne("CostComponentCalc", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one CostComponentCalc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue workEffort = null;
        try {
            workEffort = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(UtilMisc.toMap("workEffortId", context.get("workEffortId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue facility = null;
        try {
            facility = workEffort.getRelatedOne("Facility", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one Facility: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("PROD_ORDER_TASK".equals(((Map<String, Object>) workEffort).get("workEffortTypeId"))) {
            if (UtilValidate.isNotEmpty(((Map<String, Object>) workEffort).get("workEffortParentId"))) {
                try {
                    workEffortGoodStandards = EntityQuery.use(delegator)
                            .from("WorkEffortGoodStandard")
                            .where(UtilMisc.toMap("workEffortId", ((Map<String, Object>) workEffort).get("workEffortParentId"), "workEffortGoodStdTypeId", "PRUN_PROD_DELIV"))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying WorkEffortGoodStandard: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                workEffortGoodStandard = EntityUtil.getFirst((List<GenericValue>) workEffortGoodStandards);
            }
        }
        if ("PROD_ORDER_HEADER".equals(((Map<String, Object>) workEffort).get("workEffortTypeId"))) {
            try {
                workEffortGoodStandards = EntityQuery.use(delegator)
                        .from("WorkEffortGoodStandard")
                        .where(UtilMisc.toMap("workEffortId", ((Map<String, Object>) workEffort).get("workEffortId"), "workEffortGoodStdTypeId", "PRUN_PROD_DELIV"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffortGoodStandard: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            workEffortGoodStandard = EntityUtil.getFirst((List<GenericValue>) workEffortGoodStandards);
        }
        creditEntry = delegator.makeValue("AcctgTransEntry");
        creditEntry.put("debitCreditFlag", "C");
        if (UtilValidate.isNotEmpty(((Map<String, Object>) costComponentCalc).get("costGlAccountTypeId"))) {
            creditEntry.put("glAccountTypeId", ((Map<String, Object>) costComponentCalc).get("costGlAccountTypeId"));
        } else {
            if (UtilValidate.isNotEmpty(((Map<String, Object>) costComponent).get("fixedAssetId"))) {
                creditEntry.put("glAccountTypeId", "OPERATING_EXPENSE");
            }
        }
        creditEntry.put("organizationPartyId", ((Map<String, Object>) facility).get("ownerPartyId"));
        creditEntry.put("productId", ((Map<String, Object>) workEffortGoodStandard).get("productId"));
        creditEntry.put("origAmount", ((Map<String, Object>) costComponent).get("cost"));
        creditEntry.put("origCurrencyUomId", ((Map<String, Object>) costComponent).get("costUomId"));
        List<Object> acctgTransEntries = new LinkedList<>();
        acctgTransEntries.add(creditEntry);
        debitEntry = delegator.makeValue("AcctgTransEntry");
        debitEntry.put("debitCreditFlag", "D");
        if (UtilValidate.isNotEmpty(((Map<String, Object>) costComponentCalc).get("offsettingGlAccountTypeId"))) {
            debitEntry.put("glAccountTypeId", "costComponentCalc.offsettingGlAccountTypeId");
        } else {
            debitEntry.put("glAccountTypeId", "WIP_INVENTORY");
        }
        debitEntry.put("organizationPartyId", ((Map<String, Object>) facility).get("ownerPartyId"));
        debitEntry.put("productId", ((Map<String, Object>) workEffortGoodStandard).get("productId"));
        debitEntry.put("origAmount", ((Map<String, Object>) costComponent).get("cost"));
        debitEntry.put("origCurrencyUomId", ((Map<String, Object>) costComponent).get("costUomId"));
        acctgTransEntries.add(debitEntry);
        Map<String, Object> createAcctgTransAndEntriesInMap = new HashMap<>();
        createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
        createAcctgTransAndEntriesInMap.put("workEffortId", context.get("workEffortId"));
        createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "MANUFACTURING");
        createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
        Object acctgTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acctgTransId = serviceResult.get("acctgTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * Create an accounting transactions for Inventory Item Owner Change (D: INVENTORY_ACCOUNT(old Owner) INVENTORY_ACCOUNT(new Owner), C: INVENTORY_XFER_IN(oldOwner) INVENTORY_XFER_OUT(new Owner))
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForInventoryItemOwnerChange(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object origAmount = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        GenericValue inventoryItem = null;
        try {
            inventoryItem = EntityQuery.use(delegator)
                    .from("InventoryItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) inventoryItem).get("quantityOnHandTotal"))) {
            origAmount = (new BigDecimal(((Map<String, Object>) inventoryItem).get("quantityOnHandTotal").toString())).multiply(new BigDecimal(((Map<String, Object>) inventoryItem).get("unitCost").toString()));
        }
        GenericValue oldPartyCreditEntry = delegator.makeValue("AcctgTransEntry");
        oldPartyCreditEntry.put("debitCreditFlag", "C");
        oldPartyCreditEntry.put("glAccountTypeId", "INVENTORY_XFER_IN");
        oldPartyCreditEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        oldPartyCreditEntry.put("origAmount", origAmount);
        oldPartyCreditEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        oldPartyCreditEntry.put("organizationPartyId", context.get("oldOwnerPartyId"));
        GenericValue oldPartyDebitEntry = delegator.makeValue("AcctgTransEntry");
        oldPartyDebitEntry.put("debitCreditFlag", "D");
        oldPartyDebitEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        oldPartyDebitEntry.put("origAmount", origAmount);
        oldPartyDebitEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        oldPartyDebitEntry.put("organizationPartyId", context.get("oldOwnerPartyId"));
        oldPartyDebitEntry.put("glAccountTypeId", "INVENTORY_ACCOUNT");
        GenericValue newPartyCreditEntry = delegator.makeValue("AcctgTransEntry");
        newPartyCreditEntry.put("debitCreditFlag", "C");
        newPartyCreditEntry.put("glAccountTypeId", "INVENTORY_XFER_IN");
        newPartyCreditEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        newPartyCreditEntry.put("origAmount", origAmount);
        newPartyCreditEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        newPartyCreditEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        GenericValue newPartyDebitEntry = delegator.makeValue("AcctgTransEntry");
        newPartyDebitEntry.put("debitCreditFlag", "D");
        newPartyDebitEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        newPartyDebitEntry.put("origAmount", origAmount);
        newPartyDebitEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        newPartyDebitEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        newPartyDebitEntry.put("glAccountTypeId", "INVENTORY_ACCOUNT");
        List<Object> acctgTransEntries = new LinkedList<>();
        acctgTransEntries.add(oldPartyCreditEntry);
        acctgTransEntries.add(oldPartyDebitEntry);
        acctgTransEntries.add(newPartyCreditEntry);
        acctgTransEntries.add(newPartyDebitEntry);
        Map<String, Object> createAcctgTransAndEntriesInMap = new HashMap<>();
        createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "INVENTORY");
        createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
        createAcctgTransAndEntriesInMap.put("inventoryItemId", context.get("inventoryItemId"));
        createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
        Object acctgTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acctgTransId = serviceResult.get("acctgTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * Create an accounting transaction for inventory that is issued to a work effort (Type: INVENTORY D: RAWMAT_INVENTORY, C: WIP_INVENTORY)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForWorkEffortIssuance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue workEffortGoodStandard = null;
        List<GenericValue> workEffortGoodStandards = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        GenericValue workEffort = null;
        try {
            workEffort = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(UtilMisc.toMap("workEffortId", context.get("workEffortId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("PROD_ORDER_TASK".equals(((Map<String, Object>) workEffort).get("workEffortTypeId"))) {
            if (UtilValidate.isNotEmpty(((Map<String, Object>) workEffort).get("workEffortParentId"))) {
                try {
                    workEffortGoodStandards = EntityQuery.use(delegator)
                            .from("WorkEffortGoodStandard")
                            .where(UtilMisc.toMap("workEffortId", ((Map<String, Object>) workEffort).get("workEffortParentId"), "workEffortGoodStdTypeId", "PRUN_PROD_DELIV"))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying WorkEffortGoodStandard: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                workEffortGoodStandard = EntityUtil.getFirst((List<GenericValue>) workEffortGoodStandards);
            }
        }
        GenericValue workEffortInventoryAssign = null;
        try {
            workEffortInventoryAssign = EntityQuery.use(delegator)
                    .from("WorkEffortInventoryAssign")
                    .where(UtilMisc.toMap("workEffortId", context.get("workEffortId"), "inventoryItemId", context.get("inventoryItemId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffortInventoryAssign: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue inventoryItem = null;
        try {
            inventoryItem = workEffortInventoryAssign.getRelatedOne("InventoryItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object origAmount = (new BigDecimal(((Map<String, Object>) workEffortInventoryAssign).get("quantity").toString())).multiply(new BigDecimal(((Map<String, Object>) inventoryItem).get("unitCost").toString()));
        GenericValue debitEntry = delegator.makeValue("AcctgTransEntry");
        debitEntry.put("debitCreditFlag", "D");
        debitEntry.put("glAccountTypeId", "WIP_INVENTORY");
        debitEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        debitEntry.put("productId", ((Map<String, Object>) workEffortGoodStandard).get("productId"));
        debitEntry.put("origAmount", origAmount);
        debitEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        List<Object> acctgTransEntries = new LinkedList<>();
        acctgTransEntries.add(debitEntry);
        GenericValue creditEntry = delegator.makeValue("AcctgTransEntry");
        creditEntry.put("debitCreditFlag", "C");
        creditEntry.put("glAccountTypeId", "RAWMAT_INVENTORY");
        creditEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        creditEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        creditEntry.put("origAmount", origAmount);
        creditEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        acctgTransEntries.add(creditEntry);
        Map<String, Object> createAcctgTransAndEntriesInMap = new HashMap<>();
        createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
        createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "INVENTORY");
        createAcctgTransAndEntriesInMap.put("workEffortId", context.get("workEffortId"));
        createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
        Object acctgTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acctgTransId = serviceResult.get("acctgTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * Create an accounting transaction for inventory that is issued for fixed asset maintenance (Type: INVENTORY D: INVENTORY_ACCOUNT, C: FIXED_ASSET_MAINT)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForFixedAssetMaintIssuance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        GenericValue itemIssuance = null;
        try {
            itemIssuance = EntityQuery.use(delegator)
                    .from("ItemIssuance")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ItemIssuance: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue inventoryItem = null;
        try {
            inventoryItem = itemIssuance.getRelatedOne("InventoryItem", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InventoryItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object origAmount = (new BigDecimal(((Map<String, Object>) itemIssuance).get("quantity").toString())).multiply(new BigDecimal(((Map<String, Object>) inventoryItem).get("unitCost").toString()));
        GenericValue creditEntry = delegator.makeValue("AcctgTransEntry");
        creditEntry.put("debitCreditFlag", "C");
        creditEntry.put("glAccountTypeId", "INVENTORY_ACCOUNT");
        creditEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        creditEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        creditEntry.put("origAmount", origAmount);
        creditEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        List<Object> acctgTransEntries = new LinkedList<>();
        acctgTransEntries.add(creditEntry);
        GenericValue debitEntry = delegator.makeValue("AcctgTransEntry");
        debitEntry.put("debitCreditFlag", "D");
        debitEntry.put("glAccountTypeId", "FIXED_ASSET_MAINT");
        debitEntry.put("organizationPartyId", ((Map<String, Object>) inventoryItem).get("ownerPartyId"));
        debitEntry.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        debitEntry.put("origAmount", origAmount);
        debitEntry.put("origCurrencyUomId", ((Map<String, Object>) inventoryItem).get("currencyUomId"));
        acctgTransEntries.add(debitEntry);
        Map<String, Object> createAcctgTransAndEntriesInMap = new HashMap<>();
        createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
        createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "INVENTORY");
        createAcctgTransAndEntriesInMap.put("fixedAssetId", ((Map<String, Object>) itemIssuance).get("fixedAssetId"));
        createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
        Object acctgTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acctgTransId = serviceResult.get("acctgTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * Create an accounting transaction for an incoming payment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransAndEntriesForIncomingPayment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createAcctgTransAndEntriesInMap = null;
        Object amount = null;
        GenericValue debitEntry = null;
        Object creditGlAccountTypeId = null;
        List<Object> acctgTransEntries = null;
        Map<String, Object> createAcctgTransAndEntriesForPaymentApplicationInMap = null;
        Object acctgTransId = null;
        GenericValue paymentGlAccountTypeMap = null;
        List<GenericValue> paymentApplications = null;
        Object currencyUomId = null;
        Object origAmount = null;
        GenericValue creditEntryWithDiffAmount = null;
        Object paymentId = null;
        Object origCurrencyUomId = null;
        Object organizationPartyId = null;
        Object partyId = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        Object amountAppliedTotal = 0;
        Object diffAmount = 0;
        GenericValue payment = null;
        try {
            payment = EntityQuery.use(delegator)
                    .from("Payment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object isReceiptValue = null;
        try {
            isReceiptValue = UtilAccounting.isReceipt(payment);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.isReceipt: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (Boolean.TRUE.equals(isReceiptValue)) {
            origAmount = ((Map<String, Object>) payment).get("actualCurrencyAmount");
            origCurrencyUomId = ((Map<String, Object>) payment).get("actualCurrencyUomId");
            currencyUomId = ((Map<String, Object>) payment).get("currencyUomId");
            amount = ((Map<String, Object>) payment).get("amount");
            organizationPartyId = ((Map<String, Object>) payment).get("partyIdTo");
            partyId = ((Map<String, Object>) payment).get("partyIdFrom");
            paymentId = ((Map<String, Object>) payment).get("paymentId");
            debitEntry = delegator.makeValue("AcctgTransEntry");
            debitEntry.put("debitCreditFlag", "D");
            debitEntry.put("amount", amount);
            debitEntry.put("currencyUomId", currencyUomId);
            debitEntry.put("origAmount", origAmount);
            debitEntry.put("origCurrencyUomId", origCurrencyUomId);
            debitEntry.put("organizationPartyId", organizationPartyId);
            acctgTransEntries.add(debitEntry);
            try {
                paymentGlAccountTypeMap = EntityQuery.use(delegator)
                        .from("PaymentGlAccountTypeMap")
                        .where(UtilMisc.toMap("paymentTypeId", ((Map<String, Object>) payment).get("paymentTypeId"), "organizationPartyId", organizationPartyId))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentGlAccountTypeMap: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            creditGlAccountTypeId = ((Map<String, Object>) paymentGlAccountTypeMap).get("glAccountTypeId");
            creditEntryWithDiffAmount = delegator.makeValue("AcctgTransEntry");
            creditEntryWithDiffAmount.put("debitCreditFlag", "C");
            creditEntryWithDiffAmount.put("amount", amount);
            creditEntryWithDiffAmount.put("currencyUomId", currencyUomId);
            creditEntryWithDiffAmount.put("origAmount", origAmount);
            creditEntryWithDiffAmount.put("origCurrencyUomId", origCurrencyUomId);
            creditEntryWithDiffAmount.put("glAccountId", ((Map<String, Object>) payment).get("overrideGlAccountId"));
            creditEntryWithDiffAmount.put("glAccountTypeId", creditGlAccountTypeId);
            creditEntryWithDiffAmount.put("organizationPartyId", organizationPartyId);
            acctgTransEntries.add(creditEntryWithDiffAmount);
            createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
            createAcctgTransAndEntriesInMap.put("partyId", partyId);
            createAcctgTransAndEntriesInMap.put("roleTypeId", "BILL_TO_CUSTOMER");
            createAcctgTransAndEntriesInMap.put("paymentId", paymentId);
            createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "INCOMING_PAYMENT");
            createAcctgTransAndEntriesInMap.put("transactionDate", ((Map<String, Object>) payment).get("effectiveDate"));
            createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                acctgTransId = serviceResult.get("acctgTransId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("acctgTransId", acctgTransId);
            try {
                paymentApplications = payment.getRelated("PaymentApplication", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related PaymentApplication: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (paymentApplications != null) {
                for (GenericValue paymentApplication : paymentApplications) {
                    createAcctgTransAndEntriesForPaymentApplicationInMap.put("paymentApplicationId", ((Map<String, Object>) paymentApplication).get("paymentApplicationId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntriesForPaymentApplication", createAcctgTransAndEntriesForPaymentApplicationInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        acctgTransId = serviceResult.get("acctgTransId");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createAcctgTransAndEntriesForPaymentApplication: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    Debug.logInfo("Accounting transaction " + acctgTransId + " created for payment application " + ((Map<String, Object>) paymentApplication).get("paymentApplicationId"), MODULE);
                }
            }
        }

        return "success";
    }


    /**
     * Create an accounting transaction for a Customer Return Invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForCustomerReturnInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> createAcctgTransAndEntriesInMap = null;
        GenericValue creditEntry = null;
        BigDecimal quantity = null;
        Object acctgTransTypeId = null;
        GenericValue debitEntry = null;
        Object taxAuthPartyAndGeos = null;
        List<Object> acctgTransEntries = null;
        Object transPartyRoleTypeId = null;
        Object acctgTransId = null;
        BigDecimal amountFromOrder = null;
        List<GenericValue> invoiceItems = null;
        Object amountFromInvoice = null;
        BigDecimal totalAmountFromInvoice = null;
        Object taxAmount = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        GenericValue invoice = null;
        try {
            invoice = EntityQuery.use(delegator)
                    .from("Invoice")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue invoiceType = null;
        try {
            invoiceType = invoice.getRelatedOne("InvoiceType", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one InvoiceType: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue invoiceItem = null;
        GenericValue taxAuthGeoId = null;
        if ("CUST_RTN_INVOICE".equals(((Map<String, Object>) invoiceType).get("invoiceTypeId"))) {
            totalAmountFromInvoice = BigDecimal.ZERO;
            transPartyRoleTypeId = "BILL_TO_CUSTOMER";
            acctgTransTypeId = "CUST_RTN_INVOICE";
            try {
                invoiceItems = EntityQuery.use(delegator)
                        .from("InvoiceItem")
                        .cache()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InvoiceItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (invoiceItems != null) {
                for (GenericValue invoiceItemEntry : invoiceItems) {
                    amountFromOrder = BigDecimal.ZERO;
                    amountFromInvoice = BigDecimal.ZERO;
                    quantity = BigDecimal.ONE;
                    if (UtilValidate.isNotEmpty(((Map<String, Object>) invoiceItemEntry).get("quantity"))) {
                        quantity = (BigDecimal) ((Map<String, Object>) invoiceItemEntry).get("quantity");
                    }
                    amountFromInvoice = (new BigDecimal(quantity.toString())).multiply(new BigDecimal(((Map<String, Object>) invoiceItemEntry).get("amount").toString()));
                    totalAmountFromInvoice = (new BigDecimal(totalAmountFromInvoice.toString())).add(new BigDecimal(amountFromInvoice.toString()));
                    debitEntry = delegator.makeValue("AcctgTransEntry");
                    debitEntry.put("debitCreditFlag", "D");
                    debitEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyId"));
                    debitEntry.put("partyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
                    debitEntry.put("roleTypeId", transPartyRoleTypeId);
                    debitEntry.put("productId", ((Map<String, Object>) invoiceItemEntry).get("productId"));
                    debitEntry.put("glAccountTypeId", ((Map<String, Object>) invoiceItemEntry).get("invoiceItemTypeId"));
                    debitEntry.put("glAccountId", ((Map<String, Object>) invoiceItemEntry).get("overrideGlAccountId"));
                    debitEntry.put("origAmount", amountFromInvoice);
                    debitEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
                    acctgTransEntries.add(debitEntry);
                }
            }
            try {
                taxAuthPartyAndGeos = InvoiceWorker.getInvoiceTaxAuthPartyAndGeos(invoice);
            } catch (Exception e) {
                Debug.logError(e, "Error calling InvoiceWorker.getInvoiceTaxAuthPartyAndGeos: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            for (Map.Entry<String, Object> entry : ((Map<String, Object>) taxAuthPartyAndGeos).entrySet()) {
                String taxAuthPartyId = entry.getKey();
                Object taxAuthGeoIds = entry.getValue();
                if (taxAuthGeoIds != null) {
                    for (Object taxAuthGeoIdEntry : (List<Object>) taxAuthGeoIds) {
                        debitEntry = null;
                        debitEntry = delegator.makeValue("AcctgTransEntry");
                        debitEntry.put("debitCreditFlag", "D");
                        debitEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyIdTo"));
                        try {
                            taxAmount = InvoiceWorker.getInvoiceTaxTotalForTaxAuthPartyAndGeo((GenericValue) invoice, (String) taxAuthPartyId, (String) taxAuthGeoIdEntry);
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling InvoiceWorker.getInvoiceTaxTotalForTaxAuthPartyAndGeo: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        debitEntry.put("origAmount", taxAmount);
                        debitEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
                        debitEntry.put("partyId", taxAuthPartyId);
                        debitEntry.put("roleTypeId", "TAX_AUTHORITY");
                        acctgTransEntries.add(debitEntry);
                    }
                }
            }
            debitEntry = null;
            debitEntry = delegator.makeValue("AcctgTransEntry");
            debitEntry.put("debitCreditFlag", "D");
            debitEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
            try {
                taxAmount = InvoiceWorker.getInvoiceUnattributedTaxTotal((GenericValue) invoice);
            } catch (Exception e) {
                Debug.logError(e, "Error calling InvoiceWorker.getInvoiceUnattributedTaxTotal: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            debitEntry.put("origAmount", taxAmount);
            debitEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
            acctgTransEntries.add(debitEntry);
            creditEntry = delegator.makeValue("AcctgTransEntry");
            creditEntry.put("debitCreditFlag", "C");
            creditEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyId"));
            creditEntry.put("glAccountTypeId", "ACCOUNTS_RECEIVABLE");
            totalAmountFromInvoice = ((new BigDecimal(totalAmountFromInvoice.toString())).add(new BigDecimal(context.get("invoiceTaxTotal").toString()))).setScale(((Number) ledgerDecimals).intValue(), RoundingMode.valueOf(String.valueOf(roundingMode).toUpperCase().replace("-", "_")));
            creditEntry.put("origAmount", totalAmountFromInvoice);
            creditEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
            creditEntry.put("partyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
            creditEntry.put("roleTypeId", transPartyRoleTypeId);
            acctgTransEntries.add(creditEntry);
            createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
            createAcctgTransAndEntriesInMap.put("acctgTransTypeId", acctgTransTypeId);
            createAcctgTransAndEntriesInMap.put("invoiceId", ((Map<String, Object>) invoice).get("invoiceId"));
            createAcctgTransAndEntriesInMap.put("partyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
            createAcctgTransAndEntriesInMap.put("roleTypeId", "BILL_TO_CUSTOMER");
            createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                acctgTransId = serviceResult.get("acctgTransId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("acctgTransId", acctgTransId);
        } else {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingInvoiceBadInvoiceType", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }

        return "success";
    }


    /**
     * Create an accounting transaction for a purchase invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForPurchaseInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createAcctgTransAndEntriesInMap = null;
        List<GenericValue> orderItemBillings = null;
        GenericValue creditEntry = null;
        BigDecimal quantity = null;
        GenericValue orderItem = null;
        GenericValue debitEntry = null;
        Object taxAuthPartyAndGeos = null;
        List<Object> acctgTransEntries = null;
        Map<String, Object> createAcctgTransAndEntriesForPaymentApplicationInMap = null;
        Object acctgTransId = null;
        Object amountFromOrder = null;
        List<GenericValue> paymentApplications = null;
        List<GenericValue> invoiceItems = null;
        Object origAmount = null;
        Object amountFromInvoice = null;
        GenericValue payment = null;
        BigDecimal totalAmountFromInvoice = null;
        Object taxAmount = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        totalAmountFromInvoice = BigDecimal.ZERO;
        GenericValue invoice = null;
        try {
            invoice = EntityQuery.use(delegator)
                    .from("Invoice")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object isPurchaseInvoice = (Boolean) GroovyUtil.eval("org.ofbiz.entity.util.EntityTypeUtil.hasParentType(delegator, 'InvoiceType', 'invoiceTypeId', invoice.getString('invoiceTypeId'), 'parentTypeId', 'PURCHASE_INVOICE')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        if (Boolean.TRUE.equals(isPurchaseInvoice)) {
            try {
                invoiceItems = EntityQuery.use(delegator)
                        .from("InvoiceItem")
                        .cache()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InvoiceItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (invoiceItems != null) {
                for (GenericValue invoiceItem : invoiceItems) {
                    amountFromOrder = BigDecimal.ZERO;
                    amountFromInvoice = BigDecimal.ZERO;
                    quantity = BigDecimal.ONE;
                    if (UtilValidate.isNotEmpty(((Map<String, Object>) invoiceItem).get("quantity"))) {
                        quantity = (BigDecimal) ((Map<String, Object>) invoiceItem).get("quantity");
                    }
                    amountFromInvoice = (new BigDecimal(quantity.toString())).multiply(new BigDecimal(((Map<String, Object>) invoiceItem).get("amount").toString()));
                    totalAmountFromInvoice = (new BigDecimal(totalAmountFromInvoice.toString())).add(new BigDecimal(amountFromInvoice.toString()));
                    try {
                        orderItemBillings = invoiceItem.getRelated("OrderItemBilling", null, null, false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related OrderItemBilling: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (orderItemBillings != null) {
                        for (GenericValue orderItemBilling : orderItemBillings) {
                            try {
                                orderItem = orderItemBilling.getRelatedOne("OrderItem", false);
                            } catch (Exception e) {
                                Debug.logError(e, "Error getting related one OrderItem: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            amountFromOrder = (new BigDecimal(((Map<String, Object>) orderItemBilling).get("quantity").toString())).multiply(new BigDecimal(((Map<String, Object>) orderItem).get("unitPrice").toString()));
                        }
                    }
                    Object debitEntry_debitCreditFlag = null;
                    Object debitEntry_organizationPartyId = null;
                    Object debitEntry_partyId = null;
                    Object debitEntry_roleTypeId = null;
                    Object debitEntry_productId = null;
                    Object debitEntry_glAccountTypeId = null;
                    Object debitEntry_origAmount = null;
                    Object debitEntry_origCurrencyUomId = null;
                    Object acctgTransEntries__ = null;
                    if ((!java.util.Objects.equals(amountFromInvoice, amountFromOrder) && ((Comparable) amountFromOrder).compareTo(BigDecimal.ZERO) > 0)) {
                        debitEntry = delegator.makeValue("AcctgTransEntry");
                        debitEntry.put("debitCreditFlag", "D");
                        debitEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyId"));
                        debitEntry.put("partyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
                        debitEntry.put("roleTypeId", "BILL_FROM_VENDOR");
                        debitEntry.put("productId", ((Map<String, Object>) invoiceItem).get("productId"));
                        debitEntry.put("glAccountTypeId", "PURCHASE_PRICE_VAR");
                        origAmount = (new BigDecimal(amountFromInvoice.toString())).subtract(new BigDecimal(amountFromOrder.toString()));
                        debitEntry.put("origAmount", origAmount);
                        debitEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
                        acctgTransEntries.add(debitEntry);
                    }
                    debitEntry = delegator.makeValue("AcctgTransEntry");
                    debitEntry.put("debitCreditFlag", "D");
                    debitEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyId"));
                    debitEntry.put("partyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
                    debitEntry.put("roleTypeId", "BILL_FROM_VENDOR");
                    debitEntry.put("productId", ((Map<String, Object>) invoiceItem).get("productId"));
                    debitEntry.put("glAccountTypeId", ((Map<String, Object>) invoiceItem).get("invoiceItemTypeId"));
                    debitEntry.put("glAccountId", ((Map<String, Object>) invoiceItem).get("overrideGlAccountId"));
                    if (((Comparable) amountFromOrder).compareTo(BigDecimal.ZERO) > 0) {
                        origAmount = amountFromOrder;
                    } else {
                        origAmount = amountFromInvoice;
                    }
                    debitEntry.put("origAmount", origAmount);
                    debitEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
                    acctgTransEntries.add(debitEntry);
                }
            }
            try {
                taxAuthPartyAndGeos = InvoiceWorker.getInvoiceTaxAuthPartyAndGeos(invoice);
            } catch (Exception e) {
                Debug.logError(e, "Error calling InvoiceWorker.getInvoiceTaxAuthPartyAndGeos: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            for (Map.Entry<String, Object> entry : ((Map<String, Object>) taxAuthPartyAndGeos).entrySet()) {
                String taxAuthPartyId = entry.getKey();
                Object taxAuthGeoIds = entry.getValue();
                if (taxAuthGeoIds != null) {
                    for (Object taxAuthGeoId : (List<Object>) taxAuthGeoIds) {
                        debitEntry = null;
                        debitEntry = delegator.makeValue("AcctgTransEntry");
                        debitEntry.put("debitCreditFlag", "D");
                        debitEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyId"));
                        try {
                            taxAmount = InvoiceWorker.getInvoiceTaxTotalForTaxAuthPartyAndGeo((GenericValue) invoice, (String) taxAuthPartyId, (String) taxAuthGeoId);
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling InvoiceWorker.getInvoiceTaxTotalForTaxAuthPartyAndGeo: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        debitEntry.put("origAmount", taxAmount);
                        debitEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
                        debitEntry.put("partyId", taxAuthPartyId);
                        debitEntry.put("roleTypeId", "TAX_AUTHORITY");
                        acctgTransEntries.add(debitEntry);
                    }
                }
            }
            try {
                taxAmount = InvoiceWorker.getInvoiceUnattributedTaxTotal((GenericValue) invoice);
            } catch (Exception e) {
                Debug.logError(e, "Error calling InvoiceWorker.getInvoiceUnattributedTaxTotal: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if ((((Comparable) taxAmount).compareTo(BigDecimal.ZERO) > 0)) {
                debitEntry = null;
                debitEntry = delegator.makeValue("AcctgTransEntry");
                debitEntry.put("debitCreditFlag", "D");
                debitEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyId"));
                debitEntry.put("origAmount", taxAmount);
                debitEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
                acctgTransEntries.add(debitEntry);
            }
            creditEntry = delegator.makeValue("AcctgTransEntry");
            creditEntry.put("debitCreditFlag", "C");
            creditEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyId"));
            creditEntry.put("glAccountTypeId", "ACCOUNTS_PAYABLE");
            totalAmountFromInvoice = ((new BigDecimal(totalAmountFromInvoice.toString())).add(new BigDecimal(context.get("invoiceTaxTotal").toString()))).setScale(((Number) ledgerDecimals).intValue(), RoundingMode.valueOf(String.valueOf(roundingMode).toUpperCase().replace("-", "_")));
            creditEntry.put("origAmount", totalAmountFromInvoice);
            creditEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
            creditEntry.put("partyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
            creditEntry.put("roleTypeId", "BILL_FROM_VENDOR");
            acctgTransEntries.add(creditEntry);
            createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
            createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "PURCHASE_INVOICE");
            createAcctgTransAndEntriesInMap.put("invoiceId", ((Map<String, Object>) invoice).get("invoiceId"));
            createAcctgTransAndEntriesInMap.put("partyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
            createAcctgTransAndEntriesInMap.put("roleTypeId", "BILL_FROM_VENDOR");
            createAcctgTransAndEntriesInMap.put("transactionDate", ((Map<String, Object>) invoice).get("invoiceDate"));
            createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                acctgTransId = serviceResult.get("acctgTransId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("acctgTransId", acctgTransId);
            try {
                paymentApplications = EntityQuery.use(delegator)
                        .from("PaymentApplication")
                        .where(UtilMisc.toMap("invoiceId", ((Map<String, Object>) invoice).get("invoiceId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (paymentApplications != null) {
                for (GenericValue paymentApplication : paymentApplications) {
                    try {
                        payment = paymentApplication.getRelatedOne("Payment", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one Payment: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    Object createAcctgTransAndEntriesForPaymentApplicationInMap_paymentApplicationId = null;
                    if (("PMNT_SENT".equals(((Map<String, Object>) payment).get("statusId")) || "PMNT_CONFIRMED".equals(((Map<String, Object>) payment).get("statusId")))) {
                        createAcctgTransAndEntriesForPaymentApplicationInMap.put("paymentApplicationId", ((Map<String, Object>) paymentApplication).get("paymentApplicationId"));
                        if ("CUSTOMER_REFUND".equals(((Map<String, Object>) payment).get("paymentTypeId"))) {
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntriesForCustomerRefundPaymentApplication", createAcctgTransAndEntriesForPaymentApplicationInMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                                acctgTransId = serviceResult.get("acctgTransId");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createAcctgTransAndEntriesForCustomerRefundPaymentApplication: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        } else {
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntriesForPaymentApplication", createAcctgTransAndEntriesForPaymentApplicationInMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                                acctgTransId = serviceResult.get("acctgTransId");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createAcctgTransAndEntriesForPaymentApplication: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                        Debug.logInfo("Accounting transaction " + acctgTransId + " created for payment application " + ((Map<String, Object>) paymentApplication).get("paymentApplicationId"), MODULE);
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Create an accounting transaction for a sales invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransForSalesInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createAcctgTransAndEntriesInMap = null;
        GenericValue creditEntry = null;
        Object origAmountWithoutTax = null;
        GenericValue debitEntry = null;
        Object invoiceTaxTotal = null;
        Object InvoiceItemTaxAlreadyIncluded = null;
        List<Object> acctgTransEntries = null;
        Object acctgTransId = null;
        List<GenericValue> taxInvoiceItems = null;
        List<GenericValue> invoiceItems = null;
        Object taxRateProducts = null;
        Object origAmount = null;
        GenericValue taxInvoiceItem = null;
        GenericValue taxRateProduct = null;
        Object taxAmount = null;
        BigDecimal totalOrigAmount = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        totalOrigAmount = BigDecimal.ZERO;
        GenericValue invoice = null;
        try {
            invoice = EntityQuery.use(delegator)
                    .from("Invoice")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("SALES_INVOICE".equals(((Map<String, Object>) invoice).get("invoiceTypeId"))) {
            try {
                invoiceItems = EntityQuery.use(delegator)
                        .from("InvoiceItem")
                        .cache()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InvoiceItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (invoiceItems != null) {
                for (GenericValue invoiceItem : invoiceItems) {
                    if (UtilValidate.isEmpty(((Map<String, Object>) invoiceItem).get("quantity"))) {
                        invoiceItem.put("quantity", BigDecimal.ONE);
                    }
                    origAmount = (new BigDecimal(((Map<String, Object>) invoiceItem).get("quantity").toString())).multiply(new BigDecimal(((Map<String, Object>) invoiceItem).get("amount").toString()));
                    Debug.logInfo("** origAmount " + origAmount, MODULE);
                    try {
                        InvoiceItemTaxAlreadyIncluded = InvoiceWorker.getInvoiceItemTaxIncluded((GenericValue) invoiceItem);
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling InvoiceWorker.getInvoiceItemTaxIncluded: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    origAmountWithoutTax = (new BigDecimal(origAmount.toString())).subtract(new BigDecimal(InvoiceItemTaxAlreadyIncluded.toString()));
                    Debug.logInfo("** origAmountWithoutTax " + origAmountWithoutTax, MODULE);
                    totalOrigAmount = (new BigDecimal(totalOrigAmount.toString())).add(new BigDecimal(origAmount.toString()));
                    Debug.logInfo("** totalOrigAmount " + totalOrigAmount, MODULE);
                    creditEntry = delegator.makeValue("AcctgTransEntry");
                    creditEntry.put("debitCreditFlag", "C");
                    creditEntry.put("glAccountTypeId", "SALES_ACCOUNT");
                    creditEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
                    creditEntry.put("productId", ((Map<String, Object>) invoiceItem).get("productId"));
                    creditEntry.put("origAmount", origAmountWithoutTax);
                    creditEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
                    creditEntry.put("glAccountId", ((Map<String, Object>) invoiceItem).get("overrideGlAccountId"));
                    Debug.logInfo("*** creditEntry.glAccountId: " + ((Map<String, Object>) creditEntry).get("glAccountId"), MODULE);
                    if (UtilValidate.isEmpty(((Map<String, Object>) creditEntry).get("glAccountId"))) {
                        Debug.logInfo("*** parentInvoiceId=" + ((Map<String, Object>) invoiceItem).get("invoiceId") + ", parentInvoiceItemSeqId=" + ((Map<String, Object>) invoiceItem).get("invoiceItemSeqId"), MODULE);
                        try {
                            taxInvoiceItems = EntityQuery.use(delegator)
                                    .from("InvoiceItem")
                                    .where(UtilMisc.toMap("parentInvoiceId", ((Map<String, Object>) invoiceItem).get("invoiceId"), "parentInvoiceItemSeqId", ((Map<String, Object>) invoiceItem).get("invoiceItemSeqId")))
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying InvoiceItem: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        Debug.logInfo("*** taxInvoiceItems: " + taxInvoiceItems, MODULE);
                        if (UtilValidate.isNotEmpty(taxInvoiceItems)) {
                            taxInvoiceItem = EntityUtil.getFirst((List<GenericValue>) taxInvoiceItems);
                            try {
                                taxRateProduct = taxInvoiceItem.getRelatedOne("TaxAuthorityRateProduct", false);
                            } catch (Exception e) {
                                Debug.logError(e, "Error getting related one TaxAuthorityRateProduct: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            try {
                                Map<String, Object> scriptContext = new HashMap<>();
                                scriptContext.put("delegator", delegator);
                                scriptContext.put("dispatcher", dispatcher);
                                scriptContext.put("locale", locale);
                                scriptContext.put("userLogin", userLogin);
                                scriptContext.put("context", context);
                                scriptContext.put("parameters", context);
                                scriptContext.put("request", request);
                                scriptContext.put("response", response);
                                Object scriptResult = GroovyUtil.eval("groovy:\n                            if (taxRateProduct != null && taxRateProduct.getModelEntity().isField(\"revenueGlAccountId\")) {\n                                creditEntry.glAccountId = taxRateProduct.revenueGlAccountId;\n                            } else {\n                                creditEntry.glAccountId = null;\n                            }", scriptContext);
                            } catch (Exception e) {
                                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
                            }
                        }
                    }
                    if (UtilValidate.isNotEmpty(((Map<String, Object>) invoiceItem).get("taxAuthPartyId"))) {
                        creditEntry.put("partyId", ((Map<String, Object>) invoiceItem).get("taxAuthPartyId"));
                        creditEntry.put("roleTypeId", "TAX_AUTHORITY");
                    }
                    acctgTransEntries.add(creditEntry);
                }
            }
            try {
                taxRateProducts = InvoiceWorker.getInvoiceTaxRateProducts(invoice);
            } catch (Exception e) {
                Debug.logError(e, "Error calling InvoiceWorker.getInvoiceTaxRateProducts: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("** taxRateProducts " + taxRateProducts, MODULE);
            for (Map.Entry<String, Object> entry : ((Map<String, Object>) taxRateProducts).entrySet()) {
                String taxAuthorityRateSeqId = entry.getKey();
                Object glAccountId = entry.getValue();
                creditEntry = null;
                creditEntry = delegator.makeValue("AcctgTransEntry");
                creditEntry.put("debitCreditFlag", "C");
                creditEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
                try {
                    taxAmount = InvoiceWorker.getInvoiceTaxTotalForTaxGlAccount((GenericValue) invoice, (String) glAccountId);
                } catch (Exception e) {
                    Debug.logError(e, "Error calling InvoiceWorker.getInvoiceTaxTotalForTaxGlAccount: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                Debug.logInfo("** taxAmount for " + glAccountId + ": " + taxAmount, MODULE);
                taxRateProduct = null;
                try {
                    taxRateProduct = EntityQuery.use(delegator)
                            .from("TaxAuthorityRateProduct")
                            .where(UtilMisc.toMap("taxAuthorityRateSeqId", taxAuthorityRateSeqId))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying TaxAuthorityRateProduct: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                creditEntry.put("origAmount", taxAmount);
                creditEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
                creditEntry.put("partyId", ((Map<String, Object>) taxRateProduct).get("taxAuthPartyId"));
                creditEntry.put("roleTypeId", "TAX_AUTHORITY");
                creditEntry.put("glAccountId", glAccountId);
                acctgTransEntries.add(creditEntry);
            }
            try {
                taxAmount = InvoiceWorker.getInvoiceUnattributedTaxTotal((GenericValue) invoice);
            } catch (Exception e) {
                Debug.logError(e, "Error calling InvoiceWorker.getInvoiceUnattributedTaxTotal: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (!"0".equals(taxAmount)) {
                creditEntry = null;
                creditEntry = delegator.makeValue("AcctgTransEntry");
                creditEntry.put("debitCreditFlag", "C");
                creditEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
                creditEntry.put("origAmount", taxAmount);
                creditEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
                creditEntry.put("glAccountTypeId", "TAX_ACCOUNT");
                acctgTransEntries.add(creditEntry);
            }
            debitEntry = delegator.makeValue("AcctgTransEntry");
            debitEntry.put("debitCreditFlag", "D");
            debitEntry.put("glAccountTypeId", "ACCOUNTS_RECEIVABLE");
            debitEntry.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
            try {
                invoiceTaxTotal = InvoiceWorker.getInvoiceTaxTotal((GenericValue) invoice);
            } catch (Exception e) {
                Debug.logError(e, "Error calling InvoiceWorker.getInvoiceTaxTotal: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("** getInvoiceTaxTotal " + invoiceTaxTotal, MODULE);
            totalOrigAmount = ((new BigDecimal(totalOrigAmount.toString())).add(new BigDecimal(invoiceTaxTotal.toString()))).setScale(((Number) ledgerDecimals).intValue(), RoundingMode.valueOf(String.valueOf(roundingMode).toUpperCase().replace("-", "_")));
            Debug.logInfo("** totalOrigAmount " + totalOrigAmount, MODULE);
            debitEntry.put("origAmount", totalOrigAmount);
            debitEntry.put("origCurrencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
            debitEntry.put("partyId", ((Map<String, Object>) invoice).get("partyId"));
            debitEntry.put("roleTypeId", "BILL_TO_CUSTOMER");
            acctgTransEntries.add(debitEntry);
            createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
            createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "SALES_INVOICE");
            createAcctgTransAndEntriesInMap.put("invoiceId", context.get("invoiceId"));
            createAcctgTransAndEntriesInMap.put("partyId", ((Map<String, Object>) invoice).get("partyId"));
            createAcctgTransAndEntriesInMap.put("roleTypeId", "BILL_TO_CUSTOMER");
            createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
            createAcctgTransAndEntriesInMap.put("transactionDate", ((Map<String, Object>) invoice).get("invoiceDate"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                acctgTransId = serviceResult.get("acctgTransId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("acctgTransId", acctgTransId);
        }

        return "success";
    }


    /**
     * create accounting transactions and accounting transaction entries for outgoing payment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransAndEntriesForOutgoingPayment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createAcctgTransAndEntriesInMap = null;
        Object roleTypeId = null;
        GenericValue creditEntry = null;
        Object amount = null;
        List<Object> acctgTransEntries = null;
        Map<String, Object> createAcctgTransAndEntriesForPaymentApplicationInMap = null;
        Object acctgTransId = null;
        GenericValue paymentGlAccountTypeMap = null;
        GenericValue debitEntryWithDiffAmount = null;
        List<GenericValue> paymentApplications = null;
        Object debitGlAccountTypeId = null;
        Object organizationPartyId = null;
        GenericValue invoice = null;
        Object partyId = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        BigDecimal amountAppliedTotal = BigDecimal.ZERO;
        GenericValue payment = null;
        try {
            payment = EntityQuery.use(delegator)
                    .from("Payment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object isDisbursement = null;
        try {
            isDisbursement = UtilAccounting.isDisbursement(payment);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.isDisbursement: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (Boolean.TRUE.equals(isDisbursement)) {
            organizationPartyId = ((Map<String, Object>) payment).get("partyIdFrom");
            partyId = ((Map<String, Object>) payment).get("partyIdTo");
            roleTypeId = "BILL_FROM_VENDOR";
            try {
                paymentGlAccountTypeMap = EntityQuery.use(delegator)
                        .from("PaymentGlAccountTypeMap")
                        .where(UtilMisc.toMap("paymentTypeId", ((Map<String, Object>) payment).get("paymentTypeId"), "organizationPartyId", organizationPartyId))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentGlAccountTypeMap: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            debitGlAccountTypeId = ((Map<String, Object>) paymentGlAccountTypeMap).get("glAccountTypeId");
            creditEntry = delegator.makeValue("AcctgTransEntry");
            creditEntry.put("debitCreditFlag", "C");
            creditEntry.put("origAmount", ((Map<String, Object>) payment).get("amount"));
            creditEntry.put("origCurrencyUomId", ((Map<String, Object>) payment).get("currencyUomId"));
            creditEntry.put("organizationPartyId", ((Map<String, Object>) payment).get("partyIdFrom"));
            creditEntry.put("partyId", ((Map<String, Object>) payment).get("partyIdTo"));
            creditEntry.put("roleTypeId", "BILL_FROM_VENDOR");
            acctgTransEntries.add(creditEntry);
            amount = (new BigDecimal(((Map<String, Object>) payment).get("amount").toString())).subtract(new BigDecimal(amountAppliedTotal.toString()));
            if (((Comparable) amount).compareTo(BigDecimal.ZERO) > 0) {
                debitEntryWithDiffAmount = delegator.makeValue("AcctgTransEntry");
                debitEntryWithDiffAmount.put("debitCreditFlag", "D");
                debitEntryWithDiffAmount.put("origAmount", amount);
                debitEntryWithDiffAmount.put("origCurrencyUomId", ((Map<String, Object>) payment).get("currencyUomId"));
                debitEntryWithDiffAmount.put("glAccountId", ((Map<String, Object>) payment).get("overrideGlAccountId"));
                debitEntryWithDiffAmount.put("glAccountTypeId", debitGlAccountTypeId);
                debitEntryWithDiffAmount.put("organizationPartyId", organizationPartyId);
                acctgTransEntries.add(debitEntryWithDiffAmount);
            }
            createAcctgTransAndEntriesInMap.put("roleTypeId", roleTypeId);
            createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
            createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "OUTGOING_PAYMENT");
            createAcctgTransAndEntriesInMap.put("partyId", partyId);
            createAcctgTransAndEntriesInMap.put("paymentId", ((Map<String, Object>) payment).get("paymentId"));
            createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                acctgTransId = serviceResult.get("acctgTransId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("acctgTransId", acctgTransId);
            try {
                paymentApplications = EntityQuery.use(delegator)
                        .from("PaymentApplication")
                        .where(UtilMisc.toMap("paymentId", ((Map<String, Object>) payment).get("paymentId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (paymentApplications != null) {
                for (GenericValue paymentApplication : paymentApplications) {
                    try {
                        invoice = paymentApplication.getRelatedOne("Invoice", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one Invoice: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    Object createAcctgTransAndEntriesForPaymentApplicationInMap_paymentApplicationId = null;
                    if (("INVOICE_READY".equals(((Map<String, Object>) invoice).get("statusId")) || "INVOICE_PAID".equals(((Map<String, Object>) invoice).get("statusId")))) {
                        createAcctgTransAndEntriesForPaymentApplicationInMap.put("paymentApplicationId", ((Map<String, Object>) paymentApplication).get("paymentApplicationId"));
                        if ("CUSTOMER_REFUND".equals(((Map<String, Object>) payment).get("paymentTypeId"))) {
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntriesForCustomerRefundPaymentApplication", createAcctgTransAndEntriesForPaymentApplicationInMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                                acctgTransId = serviceResult.get("acctgTransId");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createAcctgTransAndEntriesForCustomerRefundPaymentApplication: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        } else {
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntriesForPaymentApplication", createAcctgTransAndEntriesForPaymentApplicationInMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                                acctgTransId = serviceResult.get("acctgTransId");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createAcctgTransAndEntriesForPaymentApplication: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                        Debug.logInfo("Accounting transaction " + acctgTransId + " created for payment application " + ((Map<String, Object>) paymentApplication).get("paymentApplicationId"), MODULE);
                    }
                }
            }
        }

        return "success";
    }


    /**
     * copy AcctgTransAndEntries
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String copyAcctgTransAndEntries(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createAcctgTransAndEntryInMap = null;
        GenericValue newAcctgTransEntry = null;
        GenericValue acctgTrans = null;
        try {
            acctgTrans = EntityQuery.use(delegator)
                    .from("AcctgTrans")
                    .where(UtilMisc.toMap("acctgTransId", context.get("fromAcctgTransId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue newAcctgTrans = GenericValue.create((GenericValue) acctgTrans);
        newAcctgTrans.remove("acctgTransId");
        Map<String, Object> createAcctgTransInMap = new HashMap<>();
        // set-service-fields from "newAcctgTrans" to "createAcctgTransInMap" for service "createAcctgTrans"
        createAcctgTransInMap.putAll(UtilMisc.toMap(newAcctgTrans));
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        createAcctgTransInMap.put("transactionDate", nowTimestamp);
        createAcctgTransInMap.put("isPosted", "N");
        Object originalAcctgTransId = context.get("fromAcctgTransId");
        result.put("acctgTransId", originalAcctgTransId);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTrans", createAcctgTransInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            newAcctgTrans.put("acctgTransId", serviceResult.get("acctgTransId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> acctgTransEntries = null;
        try {
            acctgTransEntries = acctgTrans.getRelated("AcctgTransEntry", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related AcctgTransEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (acctgTransEntries != null) {
            for (GenericValue acctgTransEntry : acctgTransEntries) {
                newAcctgTransEntry = GenericValue.create((GenericValue) acctgTransEntry);
                newAcctgTransEntry.remove("acctgTransId");
                // set-service-fields from "newAcctgTransEntry" to "createAcctgTransAndEntryInMap" for service "createAcctgTransEntry"
                createAcctgTransAndEntryInMap.putAll(UtilMisc.toMap(newAcctgTransEntry));
                createAcctgTransAndEntryInMap.put("acctgTransId", ((Map<String, Object>) newAcctgTrans).get("acctgTransId"));
                if ("Y".equals(context.get("revert"))) {
                    if ("D".equals(((Map<String, Object>) newAcctgTransEntry).get("debitCreditFlag"))) {
                        createAcctgTransAndEntryInMap.put("debitCreditFlag", "C");
                    }
                    if ("C".equals(((Map<String, Object>) newAcctgTransEntry).get("debitCreditFlag"))) {
                        createAcctgTransAndEntryInMap.put("debitCreditFlag", "D");
                    }
                } else {
                    createAcctgTransAndEntryInMap.put("debitCreditFlag", ((Map<String, Object>) newAcctgTransEntry).get("debitCreditFlag"));
                }
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransEntry", createAcctgTransAndEntryInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createAcctgTransEntry: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * create AcctgTransAndEntries For Customer Refund PaymentApplication
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransAndEntriesForCustomerRefundPaymentApplication(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue debitEntry = null;
        GenericValue paymentApplication = null;
        try {
            paymentApplication = EntityQuery.use(delegator)
                    .from("PaymentApplication")
                    .where(UtilMisc.toMap("paymentApplicationId", context.get("paymentApplicationId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue payment = null;
        try {
            payment = paymentApplication.getRelatedOne("Payment", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("PMNT_NOT_PAID".equals(((Map<String, Object>) payment).get("statusId"))) {
            return "success";
        }
        GenericValue paymentGlAccountTypeMap = null;
        try {
            paymentGlAccountTypeMap = EntityQuery.use(delegator)
                    .from("PaymentGlAccountTypeMap")
                    .where(UtilMisc.toMap("paymentTypeId", ((Map<String, Object>) payment).get("paymentTypeId"), "organizationPartyId", ((Map<String, Object>) payment).get("partyIdFrom")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGlAccountTypeMap: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("ACCOUNTS_RECEIVABLE".equals(((Map<String, Object>) paymentGlAccountTypeMap).get("glAccountTypeId"))) {
            return "success";
        }
        GenericValue creditEntry = delegator.makeValue("AcctgTransEntry");
        creditEntry.put("debitCreditFlag", "C");
        creditEntry.put("organizationPartyId", ((Map<String, Object>) payment).get("partyIdFrom"));
        creditEntry.put("partyId", ((Map<String, Object>) payment).get("partyIdTo"));
        creditEntry.put("roleTypeId", "BILL_TO_CUSTOMER");
        creditEntry.put("origAmount", ((Map<String, Object>) paymentApplication).get("amountApplied"));
        creditEntry.put("origCurrencyUomId", ((Map<String, Object>) payment).get("currencyUomId"));
        creditEntry.put("glAccountId", ((Map<String, Object>) payment).get("overrideGlAccountId"));
        creditEntry.put("glAccountTypeId", ((Map<String, Object>) paymentGlAccountTypeMap).get("glAccountTypeId"));
        debitEntry = delegator.makeValue("AcctgTransEntry");
        debitEntry.put("debitCreditFlag", "D");
        debitEntry.put("organizationPartyId", ((Map<String, Object>) payment).get("partyIdFrom"));
        debitEntry.put("partyId", ((Map<String, Object>) payment).get("partyIdTo"));
        debitEntry.put("roleTypeId", "BILL_TO_CUSTOMER");
        debitEntry.put("origAmount", ((Map<String, Object>) paymentApplication).get("amountApplied"));
        debitEntry.put("origCurrencyUomId", ((Map<String, Object>) payment).get("currencyUomId"));
        debitEntry.put("glAccountTypeId", "ACCOUNTS_RECEIVABLE");
        if (UtilValidate.isNotEmpty(((Map<String, Object>) paymentApplication).get("overrideGlAccountId"))) {
            debitEntry.put("glAccountId", ((Map<String, Object>) paymentApplication).get("overrideGlAccountId"));
        }
        List<Object> acctgTransEntries = new LinkedList<>();
        acctgTransEntries.add(debitEntry);
        acctgTransEntries.add(creditEntry);
        Map<String, Object> createAcctgTransAndEntriesInMap = new HashMap<>();
        createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
        createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "PAYMENT_APPL");
        createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
        createAcctgTransAndEntriesInMap.put("paymentId", ((Map<String, Object>) paymentApplication).get("paymentId"));
        createAcctgTransAndEntriesInMap.put("invoiceId", ((Map<String, Object>) paymentApplication).get("invoiceId"));
        createAcctgTransAndEntriesInMap.put("partyId", ((Map<String, Object>) payment).get("partyIdTo"));
        createAcctgTransAndEntriesInMap.put("roleTypeId", "BILL_TO_CUSTOMER");
        Object acctgTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acctgTransId = serviceResult.get("acctgTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * create AcctgTransAndEntries For PaymentApplication
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransAndEntriesForPaymentApplication(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue paymentGlAccountTypeMap = null;
        GenericValue creditEntry = null;
        Object invoiceExchangeRate = null;
        GenericValue debitEntry = null;
        GenericValue taxAuthorityGlAccount = null;
        List<Object> acctgTransEntries = null;
        Object paymentExchangeRate = null;
        Map<String, Object> createAcctgTransAndEntriesInMap = null;
        GenericValue paymentApplication = null;
        try {
            paymentApplication = EntityQuery.use(delegator)
                    .from("PaymentApplication")
                    .where(UtilMisc.toMap("paymentApplicationId", context.get("paymentApplicationId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue payment = null;
        try {
            payment = paymentApplication.getRelatedOne("Payment", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("PMNT_NOT_PAID".equals(((Map<String, Object>) payment).get("statusId"))) {
            return "success";
        }
        Object isReceipt = null;
        try {
            isReceipt = UtilAccounting.isReceipt(payment);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilAccounting.isReceipt: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (Boolean.TRUE.equals(isReceipt)) {
            try {
                paymentGlAccountTypeMap = EntityQuery.use(delegator)
                        .from("PaymentGlAccountTypeMap")
                        .where(UtilMisc.toMap("paymentTypeId", ((Map<String, Object>) payment).get("paymentTypeId"), "organizationPartyId", ((Map<String, Object>) payment).get("partyIdTo")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentGlAccountTypeMap: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if ("ACCOUNTS_RECEIVABLE".equals(((Map<String, Object>) paymentGlAccountTypeMap).get("glAccountTypeId"))) {
                return "success";
            }
            debitEntry = delegator.makeValue("AcctgTransEntry");
            debitEntry.put("debitCreditFlag", "D");
            debitEntry.put("organizationPartyId", ((Map<String, Object>) payment).get("partyIdTo"));
            debitEntry.put("partyId", ((Map<String, Object>) payment).get("partyIdFrom"));
            debitEntry.put("roleTypeId", "BILL_TO_CUSTOMER");
            debitEntry.put("origAmount", ((Map<String, Object>) paymentApplication).get("amountApplied"));
            debitEntry.put("origCurrencyUomId", ((Map<String, Object>) payment).get("currencyUomId"));
            debitEntry.put("glAccountId", ((Map<String, Object>) payment).get("overrideGlAccountId"));
            debitEntry.put("glAccountTypeId", ((Map<String, Object>) paymentGlAccountTypeMap).get("glAccountTypeId"));
            creditEntry = delegator.makeValue("AcctgTransEntry");
            creditEntry.put("debitCreditFlag", "C");
            creditEntry.put("organizationPartyId", ((Map<String, Object>) payment).get("partyIdTo"));
            creditEntry.put("partyId", ((Map<String, Object>) payment).get("partyIdFrom"));
            creditEntry.put("roleTypeId", "BILL_TO_CUSTOMER");
            creditEntry.put("origAmount", ((Map<String, Object>) paymentApplication).get("amountApplied"));
            creditEntry.put("origCurrencyUomId", ((Map<String, Object>) payment).get("currencyUomId"));
            creditEntry.put("glAccountTypeId", "ACCOUNTS_RECEIVABLE");
        } else {
            try {
                paymentGlAccountTypeMap = EntityQuery.use(delegator)
                        .from("PaymentGlAccountTypeMap")
                        .where(UtilMisc.toMap("paymentTypeId", ((Map<String, Object>) payment).get("paymentTypeId"), "organizationPartyId", ((Map<String, Object>) payment).get("partyIdFrom")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentGlAccountTypeMap: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if ("ACCOUNTS_PAYABLE".equals(((Map<String, Object>) paymentGlAccountTypeMap).get("glAccountTypeId"))) {
                return "success";
            }
            try {
                invoiceExchangeRate = UtilAccounting.getGlExchangeRateOfPurchaseInvoice(paymentApplication);
            } catch (Exception e) {
                Debug.logError(e, "Error calling UtilAccounting.getGlExchangeRateOfPurchaseInvoice: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                paymentExchangeRate = UtilAccounting.getGlExchangeRateOfOutgoingPayment(paymentApplication);
            } catch (Exception e) {
                Debug.logError(e, "Error calling UtilAccounting.getGlExchangeRateOfOutgoingPayment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            creditEntry = delegator.makeValue("AcctgTransEntry");
            creditEntry.put("debitCreditFlag", "C");
            creditEntry.put("organizationPartyId", ((Map<String, Object>) payment).get("partyIdFrom"));
            creditEntry.put("partyId", ((Map<String, Object>) payment).get("partyIdTo"));
            creditEntry.put("roleTypeId", "BILL_FROM_VENDOR");
            creditEntry.put("origAmount", ((Map<String, Object>) paymentApplication).get("amountApplied"));
            creditEntry.put("origCurrencyUomId", ((Map<String, Object>) payment).get("currencyUomId"));
            creditEntry.put("amount", ((BigDecimal) ((Map<String, Object>) paymentApplication).get("amountApplied")).multiply((BigDecimal) paymentExchangeRate));
            creditEntry.put("glAccountId", ((Map<String, Object>) payment).get("overrideGlAccountId"));
            creditEntry.put("glAccountTypeId", ((Map<String, Object>) paymentGlAccountTypeMap).get("glAccountTypeId"));
            debitEntry = delegator.makeValue("AcctgTransEntry");
            if (!java.util.Objects.equals(invoiceExchangeRate, paymentExchangeRate)) {
                debitEntry.put("debitCreditFlag", "D");
                debitEntry.put("organizationPartyId", ((Map<String, Object>) payment).get("partyIdFrom"));
                debitEntry.put("partyId", ((Map<String, Object>) payment).get("partyIdTo"));
                debitEntry.put("roleTypeId", "BILL_FROM_VENDOR");
                debitEntry.put("amount", ((BigDecimal) ((Map<String, Object>) paymentApplication).get("amountApplied * (paymentExchangeRate")).subtract((BigDecimal) context.get("invoiceExchangeRate)")));
                debitEntry.put("glAccountTypeId", "FX_GAIN_LOSS_ACCT");
                acctgTransEntries.add(debitEntry);
                debitEntry = null;
            }
            debitEntry = delegator.makeValue("AcctgTransEntry");
            debitEntry.put("debitCreditFlag", "D");
            debitEntry.put("organizationPartyId", ((Map<String, Object>) payment).get("partyIdFrom"));
            debitEntry.put("partyId", ((Map<String, Object>) payment).get("partyIdTo"));
            debitEntry.put("roleTypeId", "BILL_FROM_VENDOR");
            debitEntry.put("origAmount", ((Map<String, Object>) paymentApplication).get("amountApplied"));
            debitEntry.put("amount", ((BigDecimal) ((Map<String, Object>) paymentApplication).get("amountApplied")).multiply((BigDecimal) invoiceExchangeRate));
            debitEntry.put("origCurrencyUomId", ((Map<String, Object>) payment).get("currencyUomId"));
            debitEntry.put("glAccountTypeId", "ACCOUNTS_PAYABLE");
            if (UtilValidate.isNotEmpty(((Map<String, Object>) paymentApplication).get("overrideGlAccountId"))) {
                debitEntry.put("glAccountId", ((Map<String, Object>) paymentApplication).get("overrideGlAccountId"));
            } else {
                if (UtilValidate.isNotEmpty(((Map<String, Object>) paymentApplication).get("taxAuthGeoId"))) {
                    try {
                        taxAuthorityGlAccount = EntityQuery.use(delegator)
                                .from("TaxAuthorityGlAccount")
                                .where(UtilMisc.toMap("organizationPartyId", ((Map<String, Object>) payment).get("partyIdFrom"), "taxAuthGeoId", ((Map<String, Object>) paymentApplication).get("taxAuthGeoId"), "taxAuthPartyId", ((Map<String, Object>) payment).get("partyIdTo")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying TaxAuthorityGlAccount: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    debitEntry.put("glAccountId", ((Map<String, Object>) taxAuthorityGlAccount).get("glAccountId"));
                }
            }
        }
        acctgTransEntries.add(debitEntry);
        acctgTransEntries.add(creditEntry);
        createAcctgTransAndEntriesInMap.put("acctgTransEntries", acctgTransEntries);
        createAcctgTransAndEntriesInMap.put("acctgTransTypeId", "PAYMENT_APPL");
        createAcctgTransAndEntriesInMap.put("glFiscalTypeId", "ACTUAL");
        createAcctgTransAndEntriesInMap.put("paymentId", ((Map<String, Object>) paymentApplication).get("paymentId"));
        createAcctgTransAndEntriesInMap.put("invoiceId", ((Map<String, Object>) paymentApplication).get("invoiceId"));
        if (Boolean.TRUE.equals(isReceipt)) {
            createAcctgTransAndEntriesInMap.put("partyId", ((Map<String, Object>) payment).get("partyIdFrom"));
            createAcctgTransAndEntriesInMap.put("roleTypeId", "BILL_TO_CUSTOMER");
        } else {
            createAcctgTransAndEntriesInMap.put("partyId", ((Map<String, Object>) payment).get("partyIdTo"));
            createAcctgTransAndEntriesInMap.put("roleTypeId", "BILL_FROM_VENDOR");
        }
        Object acctgTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acctgTransId = serviceResult.get("acctgTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("acctgTransId", acctgTransId);

        return "success";
    }


    /**
     * Associate a party to a General Ledger Account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("PartyGlAccount");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update an existing General Ledger Account of a Party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PartyGlAccount")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyGlAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete an existing General Ledger Account of a Party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deletePartyGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PartyGlAccount")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyGlAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Gets VarianceReasonGlAccount on the basis of primary key
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getVarianceReasonGlAccountInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue varianceReasonGlAccount = null;
        try {
            varianceReasonGlAccount = EntityQuery.use(delegator)
                    .from("VarianceReasonGlAccount")
                    .where(UtilMisc.toMap("organizationPartyId", context.get("organizationPartyId"), "varianceReasonId", context.get("glAccountTypeId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying VarianceReasonGlAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Gets PartyGlAccount on the basis of primary key
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPartyGlAccountInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue partyGlAccount = null;
        try {
            partyGlAccount = EntityQuery.use(delegator)
                    .from("PartyGlAccount")
                    .where(UtilMisc.toMap("organizationPartyId", context.get("organizationPartyId"), "partyId", context.get("partyId"), "roleTypeId", context.get("roleTypeId"), "glAccountTypeId", context.get("glAccountTypeId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyGlAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Gets CreditCardTypeGlAccount on the basis of primary key
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getCreditCardTypeGlAccountInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue creditCardTypeGlAccount = null;
        try {
            creditCardTypeGlAccount = EntityQuery.use(delegator)
                    .from("CreditCardTypeGlAccount")
                    .where(UtilMisc.toMap("cardType", ((Map<String, Object>) context.get("creditCard")).get("cardType"), "organizationPartyId", context.get("organizationPartyId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CreditCardTypeGlAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Gets PaymentMethodTypeGlAccount on the basis of primary key
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPaymentMethodTypeGlAccountInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue paymentMethodTypeGlAccount = null;
        try {
            paymentMethodTypeGlAccount = EntityQuery.use(delegator)
                    .from("PaymentMethodTypeGlAccount")
                    .where(UtilMisc.toMap("paymentMethodTypeId", ((Map<String, Object>) context.get("payment")).get("paymentMethodTypeId"), "organizationPartyId", context.get("organizationPartyId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentMethodTypeGlAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Gets ProductGlAccount on the basis of primary key
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getProductGlAccountInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue productGlAccount = null;
        try {
            productGlAccount = EntityQuery.use(delegator)
                    .from("ProductGlAccount")
                    .where(UtilMisc.toMap())
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductGlAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Gets ProductCategoryGlAccount on the basis of primary key
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getProductCategoryGlAccountInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue productCategoryGlAccount = null;
        try {
            productCategoryGlAccount = EntityQuery.use(delegator)
                    .from("ProductCategoryGlAccount")
                    .where(UtilMisc.toMap("productCategoryId", ((Map<String, Object>) context.get("productCategoryMember")).get("productCategoryId"), "glAccountTypeId", context.get("glAccountTypeId"), "organizationPartyId", context.get("organizationPartyId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCategoryGlAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Gets InvoiceItemTypeGlAccount on the basis of primary key
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getInvoiceItemTypeGlAccountInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue invoiceItemTypeGlAccount = null;
        try {
            invoiceItemTypeGlAccount = EntityQuery.use(delegator)
                    .from("InvoiceItemTypeGlAccount")
                    .where(UtilMisc.toMap("invoiceItemTypeId", context.get("glAccountTypeId"), "organizationPartyId", context.get("organizationPartyId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InvoiceItemTypeGlAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Gets GlAccountTypeDefault on the basis of primary key
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getGlAccountTypeDefaultInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("GlAccountTypeDefault")
                    .where(UtilMisc.toMap("organizationPartyId", context.get("organizationPartyId"), "glAccountTypeId", context.get("glAccountTypeId")))
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountTypeDefault: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create GlAccountCategroyMember from CostCenters
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createGlAcctCatMemFromCostCenters(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Map<String, Object> createGlAccountCategoryMemberMap = null;
        Map<String, Object> updateGlAccountCategoryMemberMap = null;
        Object glAccountId = context.get("glAccountId");
        Object glAccountCategoryId = context.get("glAccountCategoryId");
        BigDecimal amountPercentage = (BigDecimal) context.get("amountPercentage");
        BigDecimal totalAmountPercentage = (BigDecimal) context.get("totalAmountPercentage");
        List<GenericValue> glAccountCategoryMemberList = null;
        try {
            glAccountCategoryMemberList = EntityQuery.use(delegator)
                    .from("GlAccountCategoryMember")
                    .where(UtilMisc.toMap("glAccountId", glAccountId, "glAccountCategoryId", glAccountCategoryId))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountCategoryMember: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue glAccountCategoryMember = EntityUtil.getFirst((List<GenericValue>) glAccountCategoryMemberList);
        if ("100".equals(totalAmountPercentage)) {
            if (UtilValidate.isEmpty(glAccountCategoryMember)) {
                createGlAccountCategoryMemberMap.put("amountPercentage", amountPercentage);
                createGlAccountCategoryMemberMap.put("glAccountCategoryId", glAccountCategoryId);
                createGlAccountCategoryMemberMap.put("glAccountId", glAccountId);
                Timestamp createGlAccountCategoryMemberMap_fromDate = new Timestamp(System.currentTimeMillis());
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createGlAccountCategoryMember", createGlAccountCategoryMemberMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createGlAccountCategoryMember: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                Debug.logInfo("GlAccountCategoryMember created for [" + glAccountCategoryId + "] and [" + glAccountId + "]", MODULE);
            } else {
                // set-service-fields from "glAccountCategoryMember" to "updateGlAccountCategoryMemberMap" for service "updateGlAccountCategoryMember"
                updateGlAccountCategoryMemberMap.putAll(UtilMisc.toMap(glAccountCategoryMember));
                updateGlAccountCategoryMemberMap.put("amountPercentage", amountPercentage);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateGlAccountCategoryMember", updateGlAccountCategoryMemberMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateGlAccountCategoryMember: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        } else {
            if ("0".equals(totalAmountPercentage)) {
                if (UtilValidate.isNotEmpty(glAccountCategoryMember)) {
                    Timestamp glAccountCategoryMember_thruDate = new Timestamp(System.currentTimeMillis());
                    try {
                        delegator.store(glAccountCategoryMember);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    Debug.logInfo("GlAccountCategoryMember expired for [" + glAccountCategoryId + "] and [" + glAccountId + "]", MODULE);
                }
            } else {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingTotalAmountPercentageIsNotEqualOneHundred", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Update GL Account Category Member
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateGlAccountCategoryMember(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        GenericValue newLookedUpValue = null;
        if (UtilValidate.isNotEmpty(context.get("amountPercentage"))) {
            try {
                lookedUpValue = EntityQuery.use(delegator)
                        .from("GlAccountCategoryMember")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying GlAccountCategoryMember: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (!java.util.Objects.equals(((Map<String, Object>) lookedUpValue).get("amountPercentage"), context.get("amountPercentage"))) {
                newLookedUpValue = GenericValue.create((GenericValue) lookedUpValue);
                Timestamp lookedUpValue_thruDate = new Timestamp(System.currentTimeMillis());
                try {
                    delegator.store(lookedUpValue);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                newLookedUpValue.put("amountPercentage", context.get("amountPercentage"));
                Timestamp newLookedUpValue_fromDate = new Timestamp(System.currentTimeMillis());
                try {
                    delegator.create(newLookedUpValue);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                Debug.logInfo("GlAccountCategoryMember updated for [" + context.get("glAccountCategoryId") + "] and [" + context.get("glAccountId") + "]", MODULE);
            }
        }

        return "success";
    }


    /**
     * Get amount percentage and glAccount for cost center
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getGlAcctgAndAmountPercentage(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue organizationParty = null;
        List<GenericValue> glAccountCategoryMembers = null;
        List<Object> glAcctgAndAmountPercentageList = null;
        GenericValue glAccount = null;
        List<GenericValue> glAccountCategories = null;
        GenericValue glAccountCategoryMember = null;
        Map<String, Object> glAcctgOrgAndCostCenterMap = null;
        glAcctgAndAmountPercentageList = null;
        Object organizationPartyId = context.get("organizationPartyId");
        Object partyIds = GroovyUtil.eval("org.ofbiz.party.party.PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, organizationPartyId, 'GROUP_ROLLUP')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        ((List<Object>) partyIds).add(organizationPartyId);
        List<GenericValue> glAccountOrganizations = null;
        try {
            glAccountOrganizations = EntityQuery.use(delegator)
                    .from("GlAccountOrganization")
                    .cache()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccountOrganization: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(glAccountOrganizations)) {
            try {
                glAccountCategories = EntityQuery.use(delegator)
                        .from("GlAccountCategory")
                        .where(UtilMisc.toMap("glAccountCategoryTypeId", "COST_CENTER"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying GlAccountCategory: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(glAccountCategories)) {
                if (glAccountOrganizations != null) {
                    for (GenericValue glAccountOrganization : glAccountOrganizations) {
                        if (glAccountCategories != null) {
                            for (GenericValue glAccountCategory : glAccountCategories) {
                                try {
                                    organizationParty = EntityQuery.use(delegator)
                                            .from("PartyGroup")
                                            .where(UtilMisc.toMap("partyId", ((Map<String, Object>) glAccountOrganization).get("organizationPartyId")))
                                            .queryOne();
                                } catch (Exception e) {
                                    Debug.logError(e, "Error querying PartyGroup: " + e.getMessage(), MODULE);
                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                    return "error";
                                }
                                try {
                                    glAccountCategoryMembers = EntityQuery.use(delegator)
                                            .from("GlAccountCategoryMember")
                                            .where(UtilMisc.toMap("glAccountId", ((Map<String, Object>) glAccountOrganization).get("glAccountId"), "glAccountCategoryId", ((Map<String, Object>) glAccountCategory).get("glAccountCategoryId")))
                                            .filterByDate()
                                            .queryList();
                                } catch (Exception e) {
                                    Debug.logError(e, "Error querying GlAccountCategoryMember: " + e.getMessage(), MODULE);
                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                    return "error";
                                }
                                if (UtilValidate.isNotEmpty(glAccountCategoryMembers)) {
                                    glAccountCategoryMember = EntityUtil.getFirst((List<GenericValue>) glAccountCategoryMembers);
                                    glAcctgOrgAndCostCenterMap.put((String) ((Map<String, Object>) glAccountCategory).get("glAccountCategoryId"), ((Map<String, Object>) glAccountCategoryMember).get("amountPercentage"));
                                    try {
                                        glAccount = glAccountCategoryMember.getRelatedOne("GlAccount", false);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error getting related one GlAccount: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                    glAcctgOrgAndCostCenterMap.put("organizationPartyId", ((Map<String, Object>) organizationParty).get("groupName") + " [" + ((Map<String, Object>) glAccountOrganization).get("organizationPartyId") + "]");
                                    glAcctgOrgAndCostCenterMap.put("glAccountId", ((Map<String, Object>) glAccount).get("glAccountId"));
                                    glAcctgOrgAndCostCenterMap.put("accountCode", ((Map<String, Object>) glAccount).get("accountCode"));
                                    glAcctgOrgAndCostCenterMap.put("accountName", ((Map<String, Object>) glAccount).get("accountName"));
                                }
                            }
                        }
                        glAcctgAndAmountPercentageList.add(glAcctgOrgAndCostCenterMap);
                        glAcctgOrgAndCostCenterMap = null;
                    }
                }
                result.put("glAccountCategories", glAccountCategories);
            }
            result.put("glAcctgAndAmountPercentageList", glAcctgAndAmountPercentageList);
        }

        return "success";
    }


    /**
     * Retrieves list for Inventory Valuation Report
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getInventoryValuationList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> inventoryValuation = null;
        GenericValue inventoryItem = null;
        Object currencyUomId = null;
        BigDecimal totalQuantityOnHand = null;
        Object productIds = null;
        Object inventoryValuationList = null;
        BigDecimal totalInventoryCost = null;
        BigDecimal productAverageCost = null;
        Map<String, Object> getProdAvgCostMap = null;
        List<GenericValue> productInventoryItems = null;
        try {
            productInventoryItems = EntityQuery.use(delegator)
                    .from("ProductInventoryItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductInventoryItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(productInventoryItems)) {
            productIds = GroovyUtil.eval("org.ofbiz.entity.util.EntityUtil.getFieldListFromEntityList(productInventoryItems, 'productId', true);", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
            if (productIds != null) {
                for (Object productId : (List<Object>) productIds) {
                    totalQuantityOnHand = BigDecimal.ZERO;
                    totalInventoryCost = BigDecimal.ZERO;
                    productAverageCost = BigDecimal.ZERO;
                    if (productInventoryItems != null) {
                        for (GenericValue productInventoryItem : productInventoryItems) {
                            if (java.util.Objects.equals(productId, ((Map<String, Object>) productInventoryItem).get("productId"))) {
                                if ("COGS_AVG_COST".equals(context.get("cogsMethodId"))) {
                                    try {
                                        inventoryItem = EntityQuery.use(delegator)
                                                .from("InventoryItem")
                                                .where(UtilMisc.toMap("inventoryItemId", ((Map<String, Object>) productInventoryItem).get("inventoryItemId")))
                                                .queryOne();
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                    getProdAvgCostMap.put("inventoryItem", inventoryItem);
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("getProductAverageCost", getProdAvgCostMap);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                            return "error";
                                        }
                                        productAverageCost = (BigDecimal) serviceResult.get("unitCost");
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling getProductAverageCost: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                }
                                totalQuantityOnHand = (BigDecimal) ((BigDecimal) totalQuantityOnHand).add((BigDecimal) ((Map<String, Object>) productInventoryItem).get("quantityOnHandTotal"));
                                currencyUomId = ((Map<String, Object>) productInventoryItem).get("currencyUomId");
                                totalInventoryCost = (BigDecimal) ((BigDecimal) totalInventoryCost).add((BigDecimal) ((Map<String, Object>) productInventoryItem).get("quantityOnHandTotal * productAverageCost"));
                            }
                        }
                    }
                    inventoryValuation.put("productId", productId);
                    inventoryValuation.put("totalQuantityOnHand", totalQuantityOnHand);
                    inventoryValuation.put("totalInventoryCost", totalInventoryCost);
                    inventoryValuation.put("productAverageCost", productAverageCost);
                    inventoryValuation.put("currencyUomId", currencyUomId);
                    inventoryValuationList = inventoryValuation;
                    inventoryValuation = null;
                }
            }
            result.put("inventoryValuationList", inventoryValuationList);
        }

        return "success";
    }


    /**
     * getGlArithmeticSettingsInline
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getGlArithmeticSettingsInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String ledgerDecimals = UtilProperties.getMessage("arithmetic", "ledger.decimals", locale);
        String roundingMode = UtilProperties.getMessage("arithmetic", "ledger.rounding", locale);
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);

        return "success";
    }


    /**
     * Set Gl Reconciliation status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setGlReconciliationStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue statusChange = null;
        GenericValue glReconciliation = null;
        try {
            glReconciliation = EntityQuery.use(delegator)
                    .from("GlReconciliation")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlReconciliation: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("oldStatusId", ((Map<String, Object>) glReconciliation).get("statusId"));
        if (!java.util.Objects.equals(((Map<String, Object>) glReconciliation).get("statusId"), context.get("statusId"))) {
            try {
                statusChange = EntityQuery.use(delegator)
                        .from("StatusValidChange")
                        .where(UtilMisc.toMap("statusId", ((Map<String, Object>) glReconciliation).get("statusId"), "statusIdTo", context.get("statusId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(statusChange)) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPSInvalidStatusChange", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                Debug.logError("Cannot change from " + ((Map<String, Object>) glReconciliation).get("statusId") + " to " + context.get("statusId"), MODULE);
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            } else {
                glReconciliation.put("statusId", context.get("statusId"));
                try {
                    delegator.store(glReconciliation);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }

}

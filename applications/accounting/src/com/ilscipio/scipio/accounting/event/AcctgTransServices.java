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
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/ledger/AcctgTransServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class AcctgTransServices {

    private static final String MODULE = AcctgTransServices.class.getName();


    /**
     * Create an AcctgTrans
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("AcctgTrans");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.put("isPosted", "N");
        ((GenericValue) newEntity).put("acctgTransId", delegator.getNextSeqId("AcctgTrans"));
        result.put("acctgTransId", ((Map<String, Object>) newEntity).get("acctgTransId"));
        newEntity.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        newEntity.put("createdByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
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
     * Update an AcctgTrans
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAcctgTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("AcctgTrans")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("Y".equals(((Map<String, Object>) lookedUpValue).get("isPosted"))) {
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
     * Delete an AcctgTrans
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteAcctgTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("AcctgTrans")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("Y".equals(((Map<String, Object>) lookedUpValue).get("isPosted"))) {
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
     * Update AcctgTrans LastModified Info
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAcctgTransLastModified(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpACTX = null;
        try {
            lookedUpACTX = EntityQuery.use(delegator)
                    .from("AcctgTrans")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpACTX.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        try {
            delegator.store(lookedUpACTX);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Add Entry To AcctgTrans
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAcctgTransEntry(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        Object convertUomInMap = null;
        newEntity = delegator.makeValue("AcctgTransEntry");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
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
        delegator.setNextSubSeqId(newEntity, "acctgTransEntrySeqId", 5, 1);
        Object acctgTransEntrySeqId = newEntity.get("acctgTransEntrySeqId");
        result.put("acctgTransEntrySeqId", ((Map<String, Object>) newEntity).get("acctgTransEntrySeqId"));
        Map<String, Object> partyAccountingPreferencesCallMap = new HashMap<>();
        partyAccountingPreferencesCallMap.put("organizationPartyId", context.get("organizationPartyId"));
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
        if (UtilValidate.isEmpty(context.get("currencyUomId"))) {
            newEntity.put("currencyUomId", ((Map<String, Object>) partyAcctgPreference).get("baseCurrencyUomId"));
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("amount"))) {
            if (UtilValidate.isNotEmpty(((Map<String, Object>) newEntity).get("origAmount"))) {
                if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("origCurrencyUomId"))) {
                    newEntity.put("origCurrencyUomId", ((Map<String, Object>) partyAcctgPreference).get("baseCurrencyUomId"));
                }
                if (!java.util.Objects.equals(((Map<String, Object>) newEntity).get("origCurrencyUomId"), ((Map<String, Object>) newEntity).get("currencyUomId"))) {
                    convertUomInMap = null;
                    ((Map<String, Object>) convertUomInMap).put("originalValue", ((Map<String, Object>) newEntity).get("origAmount"));
                    ((Map<String, Object>) convertUomInMap).put("uomId", ((Map<String, Object>) newEntity).get("origCurrencyUomId"));
                    ((Map<String, Object>) convertUomInMap).put("uomIdTo", ((Map<String, Object>) newEntity).get("currencyUomId"));
                    ((Map<String, Object>) convertUomInMap).put("purposeEnumId", context.get("purposeEnumId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("convertUom", (Map<String, Object>) convertUomInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        newEntity.put("amount", serviceResult.get("convertedValue"));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling convertUom: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                } else {
                    newEntity.put("amount", ((Map<String, Object>) newEntity).get("origAmount"));
                }
            }
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("acctgTransEntryTypeId"))) {
            newEntity.put("acctgTransEntryTypeId", "_NA_");
        }
        newEntity.put("reconcileStatusId", "AES_NOT_RECONCILED");
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
     * Update Entry To AcctgTrans
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAcctgTransEntry(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue acctgTrans = null;
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("AcctgTransEntry")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTransEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue acctgTransEntry = delegator.makeValue("AcctgTransEntry");
        acctgTransEntry = lookedUpValue;
        acctgTransEntry.setNonPKFields((Map<String, Object>) context);
        lookedUpValue.put("reconcileStatusId", ((Map<String, Object>) acctgTransEntry).get("reconcileStatusId"));
        if (!java.util.Objects.equals(acctgTransEntry, lookedUpValue)) {
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
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        String result = updateAcctgTransLastModified(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Remove Entry From AcctgTrans
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteAcctgTransEntry(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("AcctgTransEntry")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTransEntry: " + e.getMessage(), MODULE);
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
        String result = updateAcctgTransLastModified(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Calculate Trial Balance for a AcctgTrans
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String calculateAcctgTransTrialBalance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        BigDecimal creditTotal = null;
        BigDecimal debitTotal = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        List<GenericValue> acctgTransEntryList = null;
        try {
            acctgTransEntryList = EntityQuery.use(delegator)
                    .from("AcctgTransEntry")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTransEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        debitTotal = new BigDecimal("0");
        creditTotal = new BigDecimal("0");
        if (acctgTransEntryList != null) {
            for (GenericValue acctgTransEntry : acctgTransEntryList) {
                if ("D".equals(((Map<String, Object>) acctgTransEntry).get("debitCreditFlag"))) {
                    debitTotal = ((new BigDecimal(debitTotal.toString())).add(new BigDecimal(((Map<String, Object>) acctgTransEntry).get("amount").toString()))).setScale(((Number) ledgerDecimals).intValue(), RoundingMode.valueOf(String.valueOf(roundingMode).toUpperCase().replace("-", "_")));
                } else {
                    if ("C".equals(((Map<String, Object>) acctgTransEntry).get("debitCreditFlag"))) {
                        creditTotal = ((new BigDecimal(creditTotal.toString())).add(new BigDecimal(((Map<String, Object>) acctgTransEntry).get("amount").toString()))).setScale(((Number) ledgerDecimals).intValue(), RoundingMode.valueOf(String.valueOf(roundingMode).toUpperCase().replace("-", "_")));
                    } else {
                        {
                            String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotPostingGlAccountTransactionBadDebitCreditFlag", locale);
                            error_list.add(errorMsg);
                            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                        }
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        BigDecimal debitCreditDifference = ((new BigDecimal(debitTotal.toString())).add(new BigDecimal(creditTotal.toString()))).setScale(((Number) ledgerDecimals).intValue(), RoundingMode.valueOf(String.valueOf(roundingMode).toUpperCase().replace("-", "_")));
        result.put("debitTotal", debitTotal);
        result.put("creditTotal", creditTotal);
        result.put("debitCreditDifference", debitCreditDifference);

        return "success";
    }


    /**
     * Post a AcctgTrans
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String postAcctgTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object beforeScheduled = null;
        Object scheduledPostingDate = null;
        Map<String, Object> customTimePeriodListByOrganizationPartyIdMap = null;
        Object customTimePeriodList = null;
        GenericValue acctgTransEntry = null;
        Object findCustomTimePeriodCallMap = null;
        Map<String, Object> partyAccountingPreferencesCallMap = null;
        Map<String, Object> updateAcctgTransParams = null;
        List<Object> warningMessage = null;
        Object partyAcctgPreference = null;
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
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotPostingGlAccountTransactionAlreadyPosted", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> trialBalanceCallMap = new HashMap<>();
        trialBalanceCallMap.put("acctgTransId", context.get("acctgTransId"));
        Map<String, Object> trialBalanceResultMap = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("calculateAcctgTransTrialBalance", trialBalanceCallMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            trialBalanceResultMap = serviceResult;
        } catch (Exception e) {
            Debug.logError(e, "Error calling calculateAcctgTransTrialBalance: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (((Comparable) ((Map<String, Object>) trialBalanceResultMap).get("debitCreditDifference")).compareTo(new BigDecimal("0.01")) >= 0) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotPostingGlAccountTransactionTrialBalanceFailed", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (((Comparable) ((Map<String, Object>) trialBalanceResultMap).get("debitCreditDifference")).compareTo(new BigDecimal("-0.01")) <= 0) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotPostingGlAccountTransactionTrialBalanceFailed", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (("0.00".equals(((Map<String, Object>) trialBalanceResultMap).get("debitTotal")) && !"0.00".equals(((Map<String, Object>) trialBalanceResultMap).get("creditTotal")))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotPostingGlAccountTransactionDebitZero", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (("0.00".equals(((Map<String, Object>) trialBalanceResultMap).get("creditTotal")) && !"0.00".equals(((Map<String, Object>) trialBalanceResultMap).get("debitTotal")))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotPostingGlAccountTransactionCreditZero", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        List<GenericValue> acctgTransEntryList = null;
        try {
            acctgTransEntryList = EntityQuery.use(delegator)
                    .from("AcctgTransEntry")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTransEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) acctgTrans).get("scheduledPostingDate"))) {
            scheduledPostingDate = ((Map<String, Object>) acctgTrans).get("scheduledPostingDate");
            beforeScheduled = GroovyUtil.eval("org.ofbiz.base.util.UtilDateTime.nowTimestamp().before(scheduledPostingDate)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
            if (Boolean.TRUE.equals(beforeScheduled)) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotPostingGlAccountTransactionNotScheduledToBePosted", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        List<Object> onlyIncludePeriodTypeIdList = new LinkedList<>();
        onlyIncludePeriodTypeIdList.add("FISCAL_YEAR");
        onlyIncludePeriodTypeIdList.add("FISCAL_QUARTER");
        onlyIncludePeriodTypeIdList.add("FISCAL_MONTH");
        onlyIncludePeriodTypeIdList.add("FISCAL_WEEK");
        onlyIncludePeriodTypeIdList.add("FISCAL_BIWEEK");
        if (acctgTransEntryList != null) {
            for (GenericValue acctgTransEntry_iter : acctgTransEntryList) {
                acctgTransEntry = acctgTransEntry_iter;
                if (UtilValidate.isEmpty(((Map<String, Object>) customTimePeriodListByOrganizationPartyIdMap).get(((Map<String, Object>) acctgTransEntry).get("organizationPartyId")))) {
                    findCustomTimePeriodCallMap = null;
                    customTimePeriodList = null;
                    ((Map<String, Object>) findCustomTimePeriodCallMap).put("findDate", ((Map<String, Object>) acctgTrans).get("transactionDate"));
                    ((Map<String, Object>) findCustomTimePeriodCallMap).put("organizationPartyId", ((Map<String, Object>) acctgTransEntry).get("organizationPartyId"));
                    ((Map<String, Object>) findCustomTimePeriodCallMap).put("onlyIncludePeriodTypeIdList", onlyIncludePeriodTypeIdList);
                    ((Map<String, Object>) findCustomTimePeriodCallMap).put("excludeNoOrganizationPeriods", "Y");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("findCustomTimePeriods", (Map<String, Object>) findCustomTimePeriodCallMap);
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
                    if (UtilValidate.isEmpty(customTimePeriodList)) {
                        {
                            String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNoCustomTimePeriodFoundForTransactionDate", locale);
                            error_list.add(errorMsg);
                            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                        }
                    }
                    if (customTimePeriodList != null) {
                        for (Object customTimePeriod : (List<Object>) customTimePeriodList) {
                            if ("Y".equals(((Map<String, Object>) customTimePeriod).get("isClosed"))) {
                                {
                                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNoCustomTimePeriodClosed", locale);
                                    error_list.add(errorMsg);
                                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                                }
                            }
                        }
                    }
                    customTimePeriodListByOrganizationPartyIdMap.put((String) ((Map<String, Object>) acctgTransEntry).get("organizationPartyId"), customTimePeriodList);
                }
                if (UtilValidate.isEmpty(((Map<String, Object>) acctgTransEntry).get("glAccountId"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingGlAccountNotSetForAccountType", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
                if (UtilValidate.isEmpty(((Map<String, Object>) acctgTransEntry).get("amount"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingGlAccountAmountNotSet", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
            }
        }
        String trans_entry_error = null;
        if ("Y".equals(context.get("verifyOnly"))) {
            if (UtilValidate.isNotEmpty(error_list)) {
                if (error_list != null) {
                    for (String trans_entry_errorEntry : error_list) {
                        Debug.logInfo("postAcctgTrans error: " + trans_entry_errorEntry, MODULE);
                    }
                }
                result.put("successMessageList", error_list);
            }
            return "success";
        } else {
            if (UtilValidate.isNotEmpty(error_list)) {
                if (acctgTransEntryList != null) {
                    for (GenericValue acctgTransEntry_iter : acctgTransEntryList) {
                        acctgTransEntry = acctgTransEntry_iter;
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
                        if (UtilValidate.isEmpty(((Map<String, Object>) partyAcctgPreference).get("errorGlJournalId"))) {
                            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                                return "error";
                            }
                        } else {
                            acctgTrans.put("glJournalId", ((Map<String, Object>) partyAcctgPreference).get("errorGlJournalId"));
                            try {
                                delegator.store(acctgTrans);
                            } catch (Exception e) {
                                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            warningMessage.add("The accounting transaction [" + ((Map<String, Object>) acctgTrans).get("acctgTransId") + "] has been posted to the Error Journal [" + ((Map<String, Object>) partyAcctgPreference).get("errorGlJournalId") + "].");
                            result.put("successMessageList", warningMessage);
                            return "success";
                        }
                    }
                }
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            updateAcctgTransParams.put("acctgTransId", ((Map<String, Object>) acctgTrans).get("acctgTransId"));
            Timestamp updateAcctgTransParams_postedDate = new Timestamp(System.currentTimeMillis());
            updateAcctgTransParams.put("isPosted", "Y");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateAcctgTrans", updateAcctgTransParams);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateAcctgTrans: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Calculate total of credit and debit and difference between both for passed party and group rollup parties
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getAcctgTransEntriesAndTransTotal(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        BigDecimal creditTotal = null;
        BigDecimal debitTotal = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        Object organizationPartyId = context.get("organizationPartyId");
        Object partyIds = GroovyUtil.eval("org.ofbiz.party.party.PartyWorker.getAssociatedPartyIdsByRelationshipType(delegator, organizationPartyId, 'GROUP_ROLLUP')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        ((List<Object>) partyIds).add(organizationPartyId);
        List<GenericValue> acctgTransAndEntries = null;
        try {
            acctgTransAndEntries = EntityQuery.use(delegator)
                    .from("AcctgTransAndEntries")
                    .distinct()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTransAndEntries: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        debitTotal = BigDecimal.ZERO;
        creditTotal = BigDecimal.ZERO;
        BigDecimal debitCreditDifference = BigDecimal.ZERO;
        if (acctgTransAndEntries != null) {
            for (GenericValue acctgTransEntry : acctgTransAndEntries) {
                if ("D".equals(((Map<String, Object>) acctgTransEntry).get("debitCreditFlag"))) {
                    debitTotal = (BigDecimal) ((BigDecimal) debitTotal).add((BigDecimal) ((Map<String, Object>) acctgTransEntry).get("amount"));
                } else {
                    if ("C".equals(((Map<String, Object>) acctgTransEntry).get("debitCreditFlag"))) {
                        creditTotal = (BigDecimal) ((BigDecimal) creditTotal).add((BigDecimal) ((Map<String, Object>) acctgTransEntry).get("amount"));
                    }
                }
            }
        }
        debitTotal = new BigDecimal(debitTotal.toString());
        creditTotal = new BigDecimal(creditTotal.toString());
        debitCreditDifference = (BigDecimal) ((BigDecimal) debitTotal).subtract((BigDecimal) creditTotal);
        result.put("acctgTransAndEntries", acctgTransAndEntries);
        result.put("debitTotal", debitTotal);
        result.put("creditTotal", creditTotal);
        result.put("debitCreditDifference", debitCreditDifference);

        return "success";
    }


    /**
     * Calculate Trial Balance for a GlAccount
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String calculateGlAccountTrialBalance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        BigDecimal openingBalanceCredit = null;
        BigDecimal openingBalanceDebit = null;
        // getGlArithmeticSettingsInline: Load GL arithmetic settings
        Object ledgerDecimals = UtilProperties.getPropertyValue("arithmetic", "ledger.decimals", "4");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "ledger.rounding", "HalfUp");
        Debug.logInfo("Got settings from arithmetic.properties: ledgerDecimals=" + ledgerDecimals + ", roundingMode=" + roundingMode, MODULE);
        openingBalanceDebit = BigDecimal.ZERO;
        openingBalanceCredit = BigDecimal.ZERO;
        BigDecimal debitCreditDifference = BigDecimal.ZERO;
        List<GenericValue> glAccOrgAndAcctgTransAndEntries = null;
        try {
            glAccOrgAndAcctgTransAndEntries = EntityQuery.use(delegator)
                    .from("GlAccOrgAndAcctgTransAndEntry")
                    .cache()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlAccOrgAndAcctgTransAndEntry: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (glAccOrgAndAcctgTransAndEntries != null) {
            for (GenericValue glAccOrgAndAcctgTransAndEntry : glAccOrgAndAcctgTransAndEntries) {
                if ("D".equals(((Map<String, Object>) glAccOrgAndAcctgTransAndEntry).get("debitCreditFlag"))) {
                    openingBalanceDebit = (BigDecimal) ((BigDecimal) openingBalanceDebit).add((BigDecimal) ((Map<String, Object>) glAccOrgAndAcctgTransAndEntry).get("totalAmount"));
                } else {
                    openingBalanceCredit = (BigDecimal) ((BigDecimal) openingBalanceCredit).add((BigDecimal) ((Map<String, Object>) glAccOrgAndAcctgTransAndEntry).get("totalAmount"));
                }
            }
        }
        openingBalanceDebit = new BigDecimal(openingBalanceDebit.toString());
        openingBalanceCredit = new BigDecimal(openingBalanceCredit.toString());
        debitCreditDifference = (BigDecimal) ((BigDecimal) openingBalanceDebit).subtract((BigDecimal) openingBalanceCredit);
        result.put("openingBalanceDebit", openingBalanceDebit);
        result.put("openingBalanceCredit", openingBalanceCredit);
        result.put("debitCreditDifference", debitCreditDifference);

        return "success";
    }


    /**
     * Create Reverse Accounting Transaction and Entries on removing PaymentApplication records.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String revertAcctgTransOnRemovePaymentApplications(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> copyAcctgTransCtx = null;
        GenericValue paymentApplication = null;
        try {
            paymentApplication = EntityQuery.use(delegator)
                    .from("PaymentApplication")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> acctgTransPaymentList = null;
        try {
            acctgTransPaymentList = EntityQuery.use(delegator)
                    .from("AcctgTrans")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (acctgTransPaymentList != null) {
            for (GenericValue acctgTransPayment : acctgTransPaymentList) {
                copyAcctgTransCtx.put("fromAcctgTransId", ((Map<String, Object>) acctgTransPayment).get("acctgTransId"));
                copyAcctgTransCtx.put("revert", "Y");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("copyAcctgTransAndEntries", copyAcctgTransCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling copyAcctgTransAndEntries: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                copyAcctgTransCtx = null;
            }
        }

        return "success";
    }


    /**
     * Reverting Accounting Transaction And Entries on Canceling an Invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String revertAcctgTransOnCancelInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> copyAcctgTransCtx = null;
        List<GenericValue> acctgTransInvoiceList = null;
        try {
            acctgTransInvoiceList = EntityQuery.use(delegator)
                    .from("AcctgTrans")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (acctgTransInvoiceList != null) {
            for (GenericValue acctgTransInvoice : acctgTransInvoiceList) {
                copyAcctgTransCtx.put("fromAcctgTransId", ((Map<String, Object>) acctgTransInvoice).get("acctgTransId"));
                copyAcctgTransCtx.put("revert", "Y");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("copyAcctgTransAndEntries", copyAcctgTransCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling copyAcctgTransAndEntries: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                copyAcctgTransCtx = null;
            }
        }

        return "success";
    }

}

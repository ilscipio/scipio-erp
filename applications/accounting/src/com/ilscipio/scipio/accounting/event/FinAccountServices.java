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
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/finaccount/FinAccountServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class FinAccountServices {

    private static final String MODULE = FinAccountServices.class.getName();


    /**
     * getArithmeticSettingsInline
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getArithmeticSettingsInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String roundingDecimals = UtilProperties.getMessage("arithmetic", "finaccount.decimals", locale);
        String roundingMode = UtilProperties.getMessage("arithmetic", "finaccount.roundingSimpleMethod", locale);
        Debug.logVerbose("Got settings from arithmetic.properties: roundingDecimals=" + roundingDecimals + ", roundingMode=" + roundingMode, MODULE);

        return "success";
    }


    /**
     * Create a Financial Account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFinAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        String finAccountId = null;
        GenericValue newEntity = null;
        String defaultCurrency = null;
        GenericValue finAccountType = null;
        // getArithmeticSettingsInline: Load FinAccount arithmetic settings
        Object roundingDecimals = UtilProperties.getPropertyValue("arithmetic", "finaccount.decimals", "2");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "finaccount.roundingSimpleMethod", "HalfUp");
        Debug.logVerbose("Got settings from arithmetic.properties: roundingDecimals=" + roundingDecimals + ", roundingMode=" + roundingMode, MODULE);
        if (UtilValidate.isEmpty(context.get("statusId"))) {
            context.put("statusId", "FNACT_ACTIVE");
        }
        newEntity = delegator.makeValue("FinAccount");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("finAccountId"))) {
            finAccountId = delegator.getNextSeqId("FinAccount");
            finAccountId = finAccountId != null ? finAccountId.toString() : null;
            newEntity.put("finAccountId", finAccountId);
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("currencyUomId"))) {
            defaultCurrency = UtilProperties.getMessage("general", "currency.uom.id.default", locale);
            newEntity.put("currencyUomId", defaultCurrency);
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("isRefundable"))) {
            try {
                finAccountType = EntityQuery.use(delegator)
                        .from("FinAccountType")
                        .where(UtilMisc.toMap("finAccountTypeId", context.get("finAccountTypeId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FinAccountType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Object newEntity_isRefundable = null;
            if ((!(UtilValidate.isEmpty(((Map<String, Object>) finAccountType).get("isRefundable"))) && "Y".equals(((Map<String, Object>) finAccountType).get("isRefundable")))) {
                newEntity.put("isRefundable", "Y");
            }
        }
        newEntity.set("replenishLevel", new BigDecimal(((Map<String, Object>) newEntity).get("replenishLevel").toString()));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("finAccountId", ((Map<String, Object>) newEntity).get("finAccountId"));
        Map<String, Object> createFinAccountStatusMap = new HashMap<>();
        // set-service-fields from "newEntity" to "createFinAccountStatusMap" for service "createFinAccountStatus"
        createFinAccountStatusMap.putAll(UtilMisc.toMap(newEntity));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createFinAccountStatus", createFinAccountStatusMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createFinAccountStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update a Financial Account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFinAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue statusValidChange = null;
        Map<String, Object> createFinAccountStatusMap = null;
        // getArithmeticSettingsInline: Load FinAccount arithmetic settings
        Object roundingDecimals = UtilProperties.getPropertyValue("arithmetic", "finaccount.decimals", "2");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "finaccount.roundingSimpleMethod", "HalfUp");
        Debug.logVerbose("Got settings from arithmetic.properties: roundingDecimals=" + roundingDecimals + ", roundingMode=" + roundingMode, MODULE);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FinAccount")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("oldReplenishPaymentId", ((Map<String, Object>) lookedUpValue).get("replenishPaymentId"));
        result.put("oldReplenishLevel", ((Map<String, Object>) lookedUpValue).get("replenishLevel"));
        result.put("oldStatusId", ((Map<String, Object>) lookedUpValue).get("statusId"));
        if ((!(UtilValidate.isEmpty(context.get("statusId"))) && !java.util.Objects.equals(((Map<String, Object>) lookedUpValue).get("statusId"), context.get("statusId")))) {
            if (UtilValidate.isNotEmpty(((Map<String, Object>) lookedUpValue).get("statusId"))) {
                try {
                    statusValidChange = EntityQuery.use(delegator)
                            .from("StatusValidChange")
                            .where(UtilMisc.toMap("statusId", ((Map<String, Object>) lookedUpValue).get("statusId"), "statusIdTo", context.get("statusId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(statusValidChange)) {
                    {
                        String errorMsg = UtilProperties.getMessage("CommonUiLabels", "CommonErrorNoStatusValidChange", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                    if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                            UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                        return "error";
                    }
                }
            }
            // set-service-fields from "parameters" to "createFinAccountStatusMap" for service "createFinAccountStatus"
            createFinAccountStatusMap.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createFinAccountStatus", createFinAccountStatusMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createFinAccountStatus: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            if ("FNACT_MANFROZEN".equals(((Map<String, Object>) lookedUpValue).get("statusId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingFinAccountInactiveStatusError", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if ("FNACT_CANCELLED".equals(((Map<String, Object>) lookedUpValue).get("statusId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingFinAccountStatusNotValidError", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        lookedUpValue.set("replenishLevel", new BigDecimal(((Map<String, Object>) lookedUpValue).get("replenishLevel").toString()));
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("replenishPaymentId", ((Map<String, Object>) lookedUpValue).get("replenishPaymentId"));
        result.put("replenishLevel", ((Map<String, Object>) lookedUpValue).get("replenishLevel"));
        result.put("finAccountId", ((Map<String, Object>) lookedUpValue).get("finAccountId"));

        return "success";
    }


    /**
     * Delete a Financial Account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteFinAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

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
        try {
            delegator.removeValue(finAccount);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create a Financial Account Transaction
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFinAccountTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
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
        if ("FNACT_MANFROZEN".equals(((Map<String, Object>) finAccount).get("statusId"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingFinAccountInactiveStatusError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if ("FNACT_CANCELLED".equals(((Map<String, Object>) finAccount).get("statusId"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingFinAccountStatusNotValidError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        // getArithmeticSettingsInline: Load FinAccount arithmetic settings
        Object roundingDecimals = UtilProperties.getPropertyValue("arithmetic", "finaccount.decimals", "2");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "finaccount.roundingSimpleMethod", "HalfUp");
        Debug.logVerbose("Got settings from arithmetic.properties: roundingDecimals=" + roundingDecimals + ", roundingMode=" + roundingMode, MODULE);
        newEntity = delegator.makeValue("FinAccountTrans");
        newEntity.setNonPKFields((Map<String, Object>) context);
        ((GenericValue) newEntity).put("finAccountTransId", delegator.getNextSeqId("FinAccountTrans"));
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("transactionDate"))) {
            newEntity.put("transactionDate", nowTimestamp);
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("entryDate"))) {
            newEntity.put("entryDate", nowTimestamp);
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("statusId"))) {
            newEntity.put("statusId", "FINACT_TRNS_APPROVED");
        }
        newEntity.put("performedByPartyId", ((Map<String, Object>) userLogin).get("partyId"));
        Object originalAmount = ((Map<String, Object>) newEntity).get("amount");
        newEntity.set("amount", new BigDecimal(((Map<String, Object>) newEntity).get("amount").toString()));
        if (!java.util.Objects.equals(((Map<String, Object>) newEntity).get("amount"), originalAmount)) {
            Debug.logWarning("In createFinAccountTrans had to round the amount from [" + originalAmount + "] to [" + ((Map<String, Object>) newEntity).get("amount") + "]", MODULE);
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("finAccountTransId", ((Map<String, Object>) newEntity).get("finAccountTransId"));

        return "success";
    }


    /**
     * Create a Financial Account Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFinAccountRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

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
        if ("FNACT_MANFROZEN".equals(((Map<String, Object>) finAccount).get("statusId"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingFinAccountInactiveStatusError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if ("FNACT_CANCELLED".equals(((Map<String, Object>) finAccount).get("statusId"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingFinAccountStatusNotValidError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("FinAccountRole");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
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
     * Update a Financial Account Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFinAccountRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FinAccountRole")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountRole: " + e.getMessage(), MODULE);
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
     * Delete a Financial Account Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteFinAccountRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FinAccountRole")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountRole: " + e.getMessage(), MODULE);
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
     * Create a Financial Account Authorization
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFinAccountAuth(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        // getArithmeticSettingsInline: Load FinAccount arithmetic settings
        Object roundingDecimals = UtilProperties.getPropertyValue("arithmetic", "finaccount.decimals", "2");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "finaccount.roundingSimpleMethod", "HalfUp");
        Debug.logVerbose("Got settings from arithmetic.properties: roundingDecimals=" + roundingDecimals + ", roundingMode=" + roundingMode, MODULE);
        newEntity = delegator.makeValue("FinAccountAuth");
        newEntity.setNonPKFields((Map<String, Object>) context);
        ((GenericValue) newEntity).put("finAccountAuthId", delegator.getNextSeqId("FinAccountAuth"));
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("authorizationDate"))) {
            newEntity.put("authorizationDate", nowTimestamp);
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fromDate"))) {
            newEntity.put("fromDate", nowTimestamp);
        }
        Object originalAmount = ((Map<String, Object>) newEntity).get("amount");
        newEntity.set("amount", new BigDecimal(((Map<String, Object>) newEntity).get("amount").toString()));
        if (!java.util.Objects.equals(((Map<String, Object>) newEntity).get("amount"), originalAmount)) {
            Debug.logWarning("In createFinAccountAuth had to round the amount from [" + originalAmount + "] to [" + ((Map<String, Object>) newEntity).get("amount") + "]", MODULE);
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("finAccountAuthId", ((Map<String, Object>) newEntity).get("finAccountAuthId"));

        return "success";
    }


    /**
     * Expire a Financial Account Authorization
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String expireFinAccountAuth(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue finAccountAuth = null;
        try {
            finAccountAuth = EntityQuery.use(delegator)
                    .from("FinAccountAuth")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountAuth: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("expireDateTime"))) {
            Timestamp finAccountAuth_thruDate = new Timestamp(System.currentTimeMillis());
        } else {
            finAccountAuth.put("thruDate", context.get("expireDatetime"));
        }
        try {
            delegator.store(finAccountAuth);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFinAccountBalancesFromTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue mainFinAccountTrans = null;
        Object finAccountId = null;
        if (UtilValidate.isNotEmpty(context.get("finAccountId"))) {
            finAccountId = context.get("finAccountId");
        } else {
            try {
                mainFinAccountTrans = EntityQuery.use(delegator)
                        .from("FinAccountTrans")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            finAccountId = ((Map<String, Object>) mainFinAccountTrans).get("finAccountId");
        }
        String result = inlineUpdateFinAccountActualAndAvailableBalance(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFinAccountBalancesFromAuth(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object finAccountId = null;
        GenericValue mainFinAccountAuth = null;
        if (UtilValidate.isNotEmpty(context.get("finAccountId"))) {
            finAccountId = context.get("finAccountId");
        } else {
            try {
                mainFinAccountAuth = EntityQuery.use(delegator)
                        .from("FinAccountAuth")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FinAccountAuth: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            finAccountId = ((Map<String, Object>) mainFinAccountAuth).get("finAccountId");
        }
        String result = inlineUpdateFinAccountActualAndAvailableBalance(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String inlineUpdateFinAccountActualAndAvailableBalance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object amountForCalc = null;
        BigDecimal actualBalanceSum = null;
        BigDecimal availableBalanceSum = null;
        // getArithmeticSettingsInline: Load FinAccount arithmetic settings
        Object roundingDecimals = UtilProperties.getPropertyValue("arithmetic", "finaccount.decimals", "2");
        Object roundingMode = UtilProperties.getPropertyValue("arithmetic", "finaccount.roundingSimpleMethod", "HalfUp");
        Debug.logVerbose("Got settings from arithmetic.properties: roundingDecimals=" + roundingDecimals + ", roundingMode=" + roundingMode, MODULE);
        List<GenericValue> finAccountTransList = null;
        try {
            finAccountTransList = EntityQuery.use(delegator)
                    .from("FinAccountTrans")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        actualBalanceSum = BigDecimal.ZERO;
        if (finAccountTransList != null) {
            for (GenericValue finAccountTrans : finAccountTransList) {
                if ("FINACT_TRNS_APPROVED".equals(((Map<String, Object>) finAccountTrans).get("statusId"))) {
                    if ("DEPOSIT".equals(((Map<String, Object>) finAccountTrans).get("finAccountTransTypeId"))) {
                        amountForCalc = ((Map<String, Object>) finAccountTrans).get("amount");
                    }
                    actualBalanceSum = (BigDecimal) ((BigDecimal) actualBalanceSum).add((BigDecimal) amountForCalc);
                }
            }
        }
        actualBalanceSum = new BigDecimal(actualBalanceSum.toString());
        availableBalanceSum = actualBalanceSum;
        List<GenericValue> finAccountAuthList = null;
        try {
            finAccountAuthList = EntityQuery.use(delegator)
                    .from("FinAccountAuth")
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountAuth: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (finAccountAuthList != null) {
            for (GenericValue finAccountAuth : finAccountAuthList) {
                availableBalanceSum = (new BigDecimal(((Map<String, Object>) finAccountAuth).get("amount").toString())).setScale(((Number) roundingDecimals).intValue(), RoundingMode.valueOf(String.valueOf(roundingMode).toUpperCase().replace("-", "_")));
            }
        }
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
        Debug.logVerbose("In updateFinAccountBalancesFromTrans/Auth updating FinAccount with ID [" + context.get("finAccountId") + "] with actualBalance: " + ((Map<String, Object>) finAccount).get("actualBalance") + " -> " + actualBalanceSum + ", and availableBalance: " + ((Map<String, Object>) finAccount).get("availableBalance") + " -> " + availableBalanceSum, MODULE);
        finAccount.put("actualBalance", actualBalanceSum);
        finAccount.put("availableBalance", availableBalanceSum);
        try {
            delegator.store(finAccount);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create an FinAccountTypeGlAccount
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFinAccountTypeGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("FinAccountTypeGlAccount");
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
     * Update an FinAccountTypeGlAccount
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFinAccountTypeGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FinAccountTypeGlAccount")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountTypeGlAccount: " + e.getMessage(), MODULE);
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
     * Delete an FinAccountTypeGlAccount
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteFinAccountTypeGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FinAccountTypeGlAccount")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountTypeGlAccount: " + e.getMessage(), MODULE);
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
     * Create a Variance Reason Gl Account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createVarianceReasonGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("VarianceReasonGlAccount");
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
     * Update an Variance Reason Gl Account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateVarianceReasonGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("VarianceReasonGlAccount")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying VarianceReasonGlAccount: " + e.getMessage(), MODULE);
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
     * Delete an Variance Reason Gl Account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteVarianceReasonGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("VarianceReasonGlAccount")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying VarianceReasonGlAccount: " + e.getMessage(), MODULE);
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
     * Deposit withdraw payments
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String depositWithdrawPayments(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue payment = null;
        Boolean isValidStatus = null;
        BigDecimal paymentRunningTotal = null;
        Object finAccountTransId = null;
        Map<String, Object> updatePaymentCtx = null;
        Map<String, Object> createFinAccountTransMap = null;
        Boolean isDisbursement = null;
        Boolean isReceipt = null;
        Map<String, Object> checkAndCreateBatchForValidPaymentsMap = null;
        Object paymentIds = context.get("paymentIds");
        Object finAccountId = context.get("finAccountId");
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
        if (("FNACT_MANFROZEN".equals(((Map<String, Object>) finAccount).get("statusId")) || "FNACT_CANCELLED".equals(((Map<String, Object>) finAccount).get("statusId")))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingFinAccountInactiveStatusError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        paymentRunningTotal = BigDecimal.ZERO;
        List<GenericValue> payments = null;
        try {
            payments = EntityQuery.use(delegator)
                    .from("Payment")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (payments != null) {
            for (GenericValue payment_iter : payments) {
                payment = payment_iter;
                paymentRunningTotal = (BigDecimal) ((BigDecimal) paymentRunningTotal).add((BigDecimal) ((Map<String, Object>) payment).get("amount"));
                if (UtilValidate.isNotEmpty(((Map<String, Object>) payment).get("finAccountTransId"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentAlreadyAssociatedToFinAccountError", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
                isValidStatus = (Boolean) ((Map<String, Object>) payment.get("statusId == 'PMNT_SENT' @or payment")).get("statusId == 'PMNT_RECEIVED'");
                if ("false".equals(isValidStatus)) {
                    {
                        String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentStatusIsNotReceivedOrSentError", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        if ("Y".equals(context.get("groupInOneTransaction"))) {
            createFinAccountTransMap.put("finAccountId", finAccountId);
            createFinAccountTransMap.put("finAccountTransTypeId", "DEPOSIT");
            createFinAccountTransMap.put("partyId", ((Map<String, Object>) finAccount).get("ownerPartyId"));
            createFinAccountTransMap.put("amount", paymentRunningTotal);
            createFinAccountTransMap.put("statusId", "FINACT_TRNS_CREATED");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createFinAccountTrans", createFinAccountTransMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                finAccountTransId = serviceResult.get("finAccountTransId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createFinAccountTrans: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (payments != null) {
                for (GenericValue payment_iter : payments) {
                    payment = payment_iter;
                    isReceipt = (Boolean) GroovyUtil.eval("org.ofbiz.accounting.util.UtilAccounting.isReceipt(payment)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                    if (Boolean.FALSE.equals(isReceipt)) {
                        {
                            String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingCannotIncludeApPaymentError", locale);
                            error_list.add(errorMsg);
                            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                        }
                    }
                    if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                            UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                        return "error";
                    }
                    updatePaymentCtx.put("paymentId", ((Map<String, Object>) payment).get("paymentId"));
                    updatePaymentCtx.put("finAccountTransId", finAccountTransId);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updatePayment", updatePaymentCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updatePayment: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    updatePaymentCtx = null;
                }
            }
            // set-service-fields from "parameters" to "checkAndCreateBatchForValidPaymentsMap" for service "checkAndCreateBatchForValidPayments"
            checkAndCreateBatchForValidPaymentsMap.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("checkAndCreateBatchForValidPayments", checkAndCreateBatchForValidPaymentsMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling checkAndCreateBatchForValidPayments: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            if (payments != null) {
                for (GenericValue payment_iter : payments) {
                    payment = payment_iter;
                    isReceipt = (Boolean) GroovyUtil.eval("org.ofbiz.accounting.util.UtilAccounting.isReceipt(payment)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                    isDisbursement = (Boolean) GroovyUtil.eval("org.ofbiz.accounting.util.UtilAccounting.isDisbursement(payment)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                    if (Boolean.TRUE.equals(isReceipt)) {
                        createFinAccountTransMap.put("finAccountTransTypeId", "DEPOSIT");
                    } else {
                        if (Boolean.TRUE.equals(isDisbursement)) {
                            createFinAccountTransMap.put("finAccountTransTypeId", "WITHDRAWAL");
                        }
                    }
                    createFinAccountTransMap.put("finAccountId", finAccountId);
                    createFinAccountTransMap.put("partyId", ((Map<String, Object>) finAccount).get("ownerPartyId"));
                    createFinAccountTransMap.put("paymentId", ((Map<String, Object>) payment).get("paymentId"));
                    createFinAccountTransMap.put("amount", ((Map<String, Object>) payment).get("amount"));
                    createFinAccountTransMap.put("statusId", "FINACT_TRNS_CREATED");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createFinAccountTrans", createFinAccountTransMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        finAccountTransId = serviceResult.get("finAccountTransId");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createFinAccountTrans: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    updatePaymentCtx.put("paymentId", ((Map<String, Object>) payment).get("paymentId"));
                    updatePaymentCtx.put("finAccountTransId", finAccountTransId);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updatePayment", updatePaymentCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updatePayment: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    updatePaymentCtx = null;
                    createFinAccountTransMap = null;
                }
            }
        }

        return "success";
    }


    /**
     * set the financial account transaction status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setFinAccountTransStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue statusChange = null;
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
        result.put("oldStatusId", ((Map<String, Object>) finAccountTrans).get("statusId"));
        if (!java.util.Objects.equals(((Map<String, Object>) finAccountTrans).get("statusId"), context.get("statusId"))) {
            try {
                statusChange = EntityQuery.use(delegator)
                        .from("StatusValidChange")
                        .where(UtilMisc.toMap("statusId", ((Map<String, Object>) finAccountTrans).get("statusId"), "statusIdTo", context.get("statusId")))
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
                Debug.logError("Cannot change from " + ((Map<String, Object>) finAccountTrans).get("statusId") + " to " + context.get("statusId"), MODULE);
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            } else {
                finAccountTrans.put("statusId", context.get("statusId"));
                try {
                    delegator.store(finAccountTrans);
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
     * remove field finAccountTransId from Payment entity.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentOnFinAccTransStatusSetToCancel(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> payments = null;
        Map<String, Object> updatePaymentMap = null;
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
        GenericValue payment = null;
        if (UtilValidate.isEmpty(((Map<String, Object>) finAccountTrans).get("paymentId"))) {
            try {
                payments = EntityQuery.use(delegator)
                        .from("Payment")
                        .where(UtilMisc.toMap("finAccountTransId", ((Map<String, Object>) finAccountTrans).get("finAccountTransId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            try {
                payment = finAccountTrans.getRelatedOne("Payment", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            payments.add(payment);
        }
        if (payments != null) {
            for (GenericValue paymentEntry : payments) {
                updatePaymentMap.put("paymentId", ((Map<String, Object>) paymentEntry).get("paymentId"));
                updatePaymentMap.remove("finAccountTransId");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePayment", updatePaymentMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePayment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                updatePaymentMap = null;
            }
        }

        return "success";
    }


    /**
     * expire payment associations with paymentGroup on finAccountTrans cancel
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String expirePaymentAssociationsOnFinAccountTransCancel(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> payments = null;
        GenericValue paymentGroupMember = null;
        List<GenericValue> paymentGroupMembers = null;
        Map<String, Object> expirePaymentGroupMemberMap = null;
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
        GenericValue payment = null;
        if (UtilValidate.isEmpty(((Map<String, Object>) finAccountTrans).get("paymentId"))) {
            try {
                payments = EntityQuery.use(delegator)
                        .from("Payment")
                        .where(UtilMisc.toMap("finAccountTransId", ((Map<String, Object>) finAccountTrans).get("finAccountTransId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            try {
                payment = finAccountTrans.getRelatedOne("Payment", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            payments.add(payment);
        }
        if (payments != null) {
            for (GenericValue paymentEntry : payments) {
                try {
                    paymentGroupMembers = EntityQuery.use(delegator)
                            .from("PaymentGroupMember")
                            .where(UtilMisc.toMap("paymentId", ((Map<String, Object>) paymentEntry).get("paymentId")))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PaymentGroupMember: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(paymentGroupMembers)) {
                    paymentGroupMember = EntityUtil.getFirst((List<GenericValue>) paymentGroupMembers);
                    // set-service-fields from "paymentGroupMember" to "expirePaymentGroupMemberMap" for service "expirePaymentGroupMember"
                    expirePaymentGroupMemberMap.putAll(UtilMisc.toMap(paymentGroupMember));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("expirePaymentGroupMember", expirePaymentGroupMemberMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling expirePaymentGroupMember: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    expirePaymentGroupMemberMap = null;
                }
            }
        }

        return "success";
    }


    /**
     * Retrieve Financial Account Transaction List and Totals
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getFinAccountTransListAndTotals(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        BigDecimal glReconciliationApprovedGrandTotal = null;
        BigDecimal approvedGrandTotal = null;
        BigDecimal createdApprovedGrandTotal = null;
        BigDecimal createdGrandTotal = null;
        Long totalCreatedTransactions = null;
        Long totalApprovedTransactions = null;
        Boolean isConditionalStatusId = null;
        Object conditionalStatusId = null;
        List<GenericValue> finAccountTransList = null;
        BigDecimal grandTotal = null;
        List<GenericValue> finAccountTransactions = null;
        try {
            finAccountTransactions = EntityQuery.use(delegator)
                    .from("FinAccountTrans")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        grandTotal = BigDecimal.ZERO;
        createdGrandTotal = BigDecimal.ZERO;
        totalCreatedTransactions = 0L;
        approvedGrandTotal = BigDecimal.ZERO;
        totalApprovedTransactions = 0L;
        createdApprovedGrandTotal = BigDecimal.ZERO;
        glReconciliationApprovedGrandTotal = BigDecimal.ZERO;
        if (finAccountTransactions != null) {
            for (GenericValue finAccountTransaction : finAccountTransactions) {
                if ("WITHDRAWAL".equals(((Map<String, Object>) finAccountTransaction).get("finAccountTransTypeId"))) {
                    if ("FINACT_TRNS_CREATED".equals(((Map<String, Object>) finAccountTransaction).get("statusId"))) {
                        totalCreatedTransactions = totalCreatedTransactions + 1L;
                        createdGrandTotal = (BigDecimal) ((BigDecimal) createdGrandTotal).subtract((BigDecimal) ((Map<String, Object>) finAccountTransaction).get("amount"));
                        createdApprovedGrandTotal = (BigDecimal) ((BigDecimal) createdApprovedGrandTotal).subtract((BigDecimal) ((Map<String, Object>) finAccountTransaction).get("amount"));
                    }
                    if ("FINACT_TRNS_APPROVED".equals(((Map<String, Object>) finAccountTransaction).get("statusId"))) {
                        totalApprovedTransactions = totalApprovedTransactions + 1L;
                        approvedGrandTotal = (BigDecimal) ((BigDecimal) approvedGrandTotal).subtract((BigDecimal) ((Map<String, Object>) finAccountTransaction).get("amount"));
                        createdApprovedGrandTotal = (BigDecimal) ((BigDecimal) createdApprovedGrandTotal).subtract((BigDecimal) ((Map<String, Object>) finAccountTransaction).get("amount"));
                        if (java.util.Objects.equals(context.get("glReconciliationId"), ((Map<String, Object>) finAccountTransaction).get("glReconciliationId"))) {
                            glReconciliationApprovedGrandTotal = (BigDecimal) ((BigDecimal) glReconciliationApprovedGrandTotal).subtract((BigDecimal) ((Map<String, Object>) finAccountTransaction).get("amount"));
                        }
                    }
                } else {
                    if ("FINACT_TRNS_CREATED".equals(((Map<String, Object>) finAccountTransaction).get("statusId"))) {
                        totalCreatedTransactions = totalCreatedTransactions + 1L;
                        createdGrandTotal = (BigDecimal) ((BigDecimal) createdGrandTotal).add((BigDecimal) ((Map<String, Object>) finAccountTransaction).get("amount"));
                        createdApprovedGrandTotal = (BigDecimal) ((BigDecimal) createdApprovedGrandTotal).add((BigDecimal) ((Map<String, Object>) finAccountTransaction).get("amount"));
                    }
                    if ("FINACT_TRNS_APPROVED".equals(((Map<String, Object>) finAccountTransaction).get("statusId"))) {
                        totalApprovedTransactions = totalApprovedTransactions + 1L;
                        approvedGrandTotal = (BigDecimal) ((BigDecimal) approvedGrandTotal).add((BigDecimal) ((Map<String, Object>) finAccountTransaction).get("amount"));
                        createdApprovedGrandTotal = (BigDecimal) ((BigDecimal) createdApprovedGrandTotal).add((BigDecimal) ((Map<String, Object>) finAccountTransaction).get("amount"));
                        if (java.util.Objects.equals(context.get("glReconciliationId"), ((Map<String, Object>) finAccountTransaction).get("glReconciliationId"))) {
                            glReconciliationApprovedGrandTotal = (BigDecimal) ((BigDecimal) glReconciliationApprovedGrandTotal).add((BigDecimal) ((Map<String, Object>) finAccountTransaction).get("amount"));
                        }
                    }
                }
            }
        }
        if ("_NA_".equals(context.get("glReconciliationId"))) {
            isConditionalStatusId = (Boolean) ((Map<String, Object>) context.get("statusId == null @or parameters")).get("statusId != 'FINACT_TRNS_CANCELED'");
            if (Boolean.TRUE.equals(isConditionalStatusId)) {
                conditionalStatusId = "FINACT_TRNS_CANCELED";
            }
            try {
                finAccountTransList = EntityQuery.use(delegator)
                        .from("FinAccountTrans")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            try {
                finAccountTransList = EntityQuery.use(delegator)
                        .from("FinAccountTrans")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (finAccountTransList != null) {
            for (GenericValue finAccountTrans : finAccountTransList) {
                if ("WITHDRAWAL".equals(((Map<String, Object>) finAccountTrans).get("finAccountTransTypeId"))) {
                    grandTotal = (BigDecimal) ((BigDecimal) grandTotal).subtract((BigDecimal) ((Map<String, Object>) finAccountTrans).get("amount"));
                } else {
                    grandTotal = (BigDecimal) ((BigDecimal) grandTotal).add((BigDecimal) ((Map<String, Object>) finAccountTrans).get("amount"));
                }
            }
        }
        Object searchedNumberOfRecords = finAccountTransList.size();
        Long totalCreatedApprovedTransactions = totalCreatedTransactions + totalApprovedTransactions;
        result.put("finAccountTransList", finAccountTransList);
        result.put("searchedNumberOfRecords", searchedNumberOfRecords);
        result.put("grandTotal", grandTotal);
        result.put("createdGrandTotal", createdGrandTotal);
        result.put("totalCreatedTransactions", totalCreatedTransactions);
        result.put("approvedGrandTotal", approvedGrandTotal);
        result.put("totalApprovedTransactions", totalApprovedTransactions);
        result.put("createdApprovedGrandTotal", createdApprovedGrandTotal);
        result.put("totalCreatedApprovedTransactions", totalCreatedApprovedTransactions);
        if (UtilValidate.isNotEmpty(context.get("openingBalance"))) {
            glReconciliationApprovedGrandTotal = (BigDecimal) ((BigDecimal) glReconciliationApprovedGrandTotal).add((BigDecimal) context.get("openingBalance"));
        }
        result.put("glReconciliationApprovedGrandTotal", glReconciliationApprovedGrandTotal);

        return "success";
    }


    /**
     * Calculate running total and Balances of Financial Account Transactions
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getFinAccountTransRunningTotalAndBalances(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        BigDecimal runningTotal = null;
        String currencyUomId = null;
        runningTotal = (BigDecimal) context.get("runningTotal");
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
        if ("WITHDRAWAL".equals(((Map<String, Object>) finAccountTrans).get("finAccountTransTypeId"))) {
            runningTotal = (BigDecimal) ((BigDecimal) runningTotal).subtract((BigDecimal) ((Map<String, Object>) finAccountTrans).get("amount"));
        } else {
            runningTotal = (BigDecimal) ((BigDecimal) runningTotal).add((BigDecimal) ((Map<String, Object>) finAccountTrans).get("amount"));
        }
        result.put("runningTotal", runningTotal);
        Long numberOfTransactions = (Long) context.get("numberOfTransactions");
        numberOfTransactions = numberOfTransactions + 1L;
        result.put("numberOfTransactions", numberOfTransactions);
        BigDecimal openingBalance = (BigDecimal) context.get("openingBalance");
        BigDecimal reconciledBalance = (BigDecimal) context.get("reconciledBalance");
        Object endingBalance = (BigDecimal) ((BigDecimal) openingBalance).add((BigDecimal) context.get("reconciledBalance + runningTotal"));
        Map<String, Object> getPartyAccountingPreferencesMap = new HashMap<>();
        // set-service-fields from "parameters" to "getPartyAccountingPreferencesMap" for service "getPartyAccountingPreferences"
        getPartyAccountingPreferencesMap.putAll(UtilMisc.toMap(context));
        Object partyAccountingPreference = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getPartyAccountingPreferences", getPartyAccountingPreferencesMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            partyAccountingPreference = serviceResult.get("partyAccountingPreference");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getPartyAccountingPreferences: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        currencyUomId = (String) ((Map<String, Object>) partyAccountingPreference).get("baseCurrencyUomId");
        if (UtilValidate.isEmpty(currencyUomId)) {
            currencyUomId = UtilProperties.getMessage("general", "currency.uom.id.default", locale);
        }
        Object finAccountTransRunningTotal = GroovyUtil.eval("org.ofbiz.base.util.UtilFormatOut.formatCurrency(runningTotal, currencyUomId, parameters.locale)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        endingBalance = GroovyUtil.eval("org.ofbiz.base.util.UtilFormatOut.formatCurrency(endingBalance, currencyUomId, parameters.locale)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        result.put("finAccountTransRunningTotal", finAccountTransRunningTotal);
        result.put("endingBalance", endingBalance);

        return "success";
    }


    /**
     * Reconcile Financial Accounting Transaction
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String reconcileFinAccountTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object isReconciled = null;
        Map<String, Object> reconcileWithdrawalFinAcctgTransMap = null;
        Map<String, Object> updateGlReconciliationMap = null;
        Map<String, Object> setFinAccountTransStatusMap = null;
        Map<String, Object> isGlReconciliationReconciledMap = null;
        Map<String, Object> reconcileAdjustmentFinAcctgTransMap = null;
        Boolean isAdjustmentOrDeposit = null;
        Map<String, Object> reconcileDepositFinAcctgTransMap = null;
        GenericValue glReconciliation = null;
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
        if (UtilValidate.isNotEmpty(((Map<String, Object>) finAccountTrans).get("glReconciliationId"))) {
            if ("ADJUSTMENT".equals(((Map<String, Object>) finAccountTrans).get("finAccountTransTypeId"))) {
                // set-service-fields from "parameters" to "reconcileAdjustmentFinAcctgTransMap" for service "reconcileAdjustmentFinAcctgTrans"
                reconcileAdjustmentFinAcctgTransMap.putAll(UtilMisc.toMap(context));
                reconcileAdjustmentFinAcctgTransMap.put("finAccountTrans", finAccountTrans);
                reconcileAdjustmentFinAcctgTransMap.put("organizationPartyId", context.get("organizationPartyId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("reconcileAdjustmentFinAcctgTrans", reconcileAdjustmentFinAcctgTransMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling reconcileAdjustmentFinAcctgTrans: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            if ("DEPOSIT".equals(((Map<String, Object>) finAccountTrans).get("finAccountTransTypeId"))) {
                // set-service-fields from "parameters" to "reconcileDepositFinAcctgTransMap" for service "reconcileDepositFinAcctgTrans"
                reconcileDepositFinAcctgTransMap.putAll(UtilMisc.toMap(context));
                reconcileDepositFinAcctgTransMap.put("finAccountTrans", finAccountTrans);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("reconcileDepositFinAcctgTrans", reconcileDepositFinAcctgTransMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling reconcileDepositFinAcctgTrans: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            if ("WITHDRAWAL".equals(((Map<String, Object>) finAccountTrans).get("finAccountTransTypeId"))) {
                // set-service-fields from "parameters" to "reconcileWithdrawalFinAcctgTransMap" for service "reconcileWithdrawalFinAcctgTrans"
                reconcileWithdrawalFinAcctgTransMap.putAll(UtilMisc.toMap(context));
                reconcileWithdrawalFinAcctgTransMap.put("finAccountTrans", finAccountTrans);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("reconcileWithdrawalFinAcctgTrans", reconcileWithdrawalFinAcctgTransMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling reconcileWithdrawalFinAcctgTrans: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            // set-service-fields from "finAccountTrans" to "setFinAccountTransStatusMap" for service "setFinAccountTransStatus"
            setFinAccountTransStatusMap.putAll(UtilMisc.toMap(finAccountTrans));
            setFinAccountTransStatusMap.put("statusId", "FINACT_TRNS_APPROVED");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("setFinAccountTransStatus", setFinAccountTransStatusMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling setFinAccountTransStatus: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                glReconciliation = finAccountTrans.getRelatedOne("GlReconciliation", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one GlReconciliation: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // set-service-fields from "glReconciliation" to "updateGlReconciliationMap" for service "updateGlReconciliation"
            updateGlReconciliationMap.putAll(UtilMisc.toMap(glReconciliation));
            isAdjustmentOrDeposit = (Boolean) ((Map<String, Object>) finAccountTrans.get("finAccountTransTypeId == 'ADJUSTMENT' @or finAccountTrans")).get("finAccountTransTypeId == 'DEPOSIT'");
            if (Boolean.TRUE.equals(isAdjustmentOrDeposit)) {
                updateGlReconciliationMap.put("reconciledBalance", (BigDecimal) ((BigDecimal) ((Map<String, Object>) glReconciliation).get("reconciledBalance")).add((BigDecimal) ((Map<String, Object>) finAccountTrans).get("amount")));
            } else {
                updateGlReconciliationMap.put("reconciledBalance", (BigDecimal) ((BigDecimal) ((Map<String, Object>) glReconciliation).get("reconciledBalance")).subtract((BigDecimal) ((Map<String, Object>) finAccountTrans).get("amount")));
            }
            isGlReconciliationReconciledMap.put("glReconciliationId", ((Map<String, Object>) finAccountTrans).get("glReconciliationId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("isGlReconciliationReconciled", isGlReconciliationReconciledMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                isReconciled = serviceResult.get("isReconciled");
            } catch (Exception e) {
                Debug.logError(e, "Error calling isGlReconciliationReconciled: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (Boolean.TRUE.equals(isReconciled)) {
                if (UtilValidate.isEmpty(((Map<String, Object>) updateGlReconciliationMap).get("reconciledDate"))) {
                    updateGlReconciliationMap.put("reconciledDate", nowTimestamp);
                }
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateGlReconciliation", updateGlReconciliationMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateGlReconciliation: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingReconciliationError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }

        return "success";
    }


    /**
     * Reconcile financial accounting transaction of type adjustment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String reconcileAdjustmentFinAcctgTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createGlReconciliationEntryMap = null;
        Map<String, Object> postAcctgTransMap = null;
        Map<String, Object> updateAcctgTransEntryMap = null;
        List<GenericValue> acctgTransList = null;
        String errorMessage = null;
        List<GenericValue> acctgTransEntries = null;
        GenericValue acctgTrans = null;
        Object finAccountTrans = context.get("finAccountTrans");
        GenericValue acctgTransEntry = null;
        if ("ADJUSTMENT".equals(((Map<String, Object>) finAccountTrans).get("finAccountTransTypeId"))) {
            try {
                acctgTransList = EntityQuery.use(delegator)
                        .from("AcctgTrans")
                        .where(UtilMisc.toMap("finAccountTransId", ((Map<String, Object>) finAccountTrans).get("finAccountTransId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(acctgTransList)) {
                acctgTrans = EntityUtil.getFirst((List<GenericValue>) acctgTransList);
                if ("N".equals(((Map<String, Object>) acctgTrans).get("isPosted"))) {
                    // set-service-fields from "acctgTrans" to "postAcctgTransMap" for service "postAcctgTrans"
                    postAcctgTransMap.putAll(UtilMisc.toMap(acctgTrans));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("postAcctgTrans", postAcctgTransMap);
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
                try {
                    acctgTransEntries = acctgTrans.getRelated("AcctgTransEntry", null, null, false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related AcctgTransEntry: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (acctgTransEntries != null) {
                    for (GenericValue acctgTransEntryEntry : acctgTransEntries) {
                        // set-service-fields from "acctgTransEntry" to "createGlReconciliationEntryMap" for service "createGlReconciliationEntry"
                        createGlReconciliationEntryMap.putAll(UtilMisc.toMap(acctgTransEntryEntry));
                        createGlReconciliationEntryMap.put("glReconciliationId", ((Map<String, Object>) finAccountTrans).get("glReconciliationId"));
                        createGlReconciliationEntryMap.put("reconciledAmount", ((Map<String, Object>) acctgTransEntryEntry).get("amount"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createGlReconciliationEntry", createGlReconciliationEntryMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createGlReconciliationEntry: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        // set-service-fields from "acctgTransEntry" to "updateAcctgTransEntryMap" for service "updateAcctgTransEntry"
                        updateAcctgTransEntryMap.putAll(UtilMisc.toMap(acctgTransEntryEntry));
                        updateAcctgTransEntryMap.put("reconcileStatusId", "AES_RECONCILED");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("updateAcctgTransEntry", updateAcctgTransEntryMap);
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
                    }
                }
            }
        } else {
            errorMessage = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotAdjustmentFinAccountTrans", locale);
            result.put("errorMessage", errorMessage);
        }

        return "success";
    }


    /**
     * Reconcile financial accounting transaction of type deposit
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String reconcileDepositFinAcctgTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createGlReconciliationEntryMap = null;
        Map<String, Object> postAcctgTransMap = null;
        GenericValue newAcctgTransEntry = null;
        List<GenericValue> payments = null;
        List<GenericValue> acctgTransList = null;
        Map<String, Object> createAcctgTransAndEntriesMap = null;
        BigDecimal entryAmount = null;
        String errorMessage = null;
        List<Object> createAcctgTransAndEntriesMap_acctgTransEntries = null;
        List<GenericValue> acctgTransEntries = null;
        Object acctgTransId = null;
        Map<String, Object> updateAcctgTransEntryMap = null;
        Object organizationPartyId = null;
        GenericValue acctgTrans = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        Object finAccountTrans = context.get("finAccountTrans");
        GenericValue finAccount = null;
        try {
            finAccount = ((GenericValue) finAccountTrans).getRelatedOne("FinAccount", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one FinAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue oldAcctgTransEntry = null;
        GenericValue acctgTransEntry = null;
        GenericValue payment = null;
        if ("DEPOSIT".equals(((Map<String, Object>) finAccountTrans).get("finAccountTransTypeId"))) {
            if (UtilValidate.isEmpty(((Map<String, Object>) finAccountTrans).get("paymentId"))) {
                try {
                    payments = EntityQuery.use(delegator)
                            .from("Payment")
                            .where(UtilMisc.toMap("finAccountTransId", ((Map<String, Object>) finAccountTrans).get("finAccountTransId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                try {
                    payment = ((GenericValue) finAccountTrans).getRelatedOne("Payment", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one Payment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                payments.add(payment);
            }
            if (UtilValidate.isEmpty(payments)) {
                try {
                    acctgTransList = EntityQuery.use(delegator)
                            .from("AcctgTrans")
                            .where(UtilMisc.toMap("finAccountTransId", ((Map<String, Object>) finAccountTrans).get("finAccountTransId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                acctgTrans = EntityUtil.getFirst((List<GenericValue>) acctgTransList);
                if ("N".equals(((Map<String, Object>) acctgTrans).get("isPosted"))) {
                    // set-service-fields from "acctgTrans" to "postAcctgTransMap" for service "postAcctgTrans"
                    postAcctgTransMap.putAll(UtilMisc.toMap(acctgTrans));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("postAcctgTrans", postAcctgTransMap);
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
            if (payments != null) {
                for (GenericValue paymentEntry : payments) {
                    createAcctgTransAndEntriesMap = null;
                    try {
                        acctgTransList = paymentEntry.getRelated("AcctgTrans", null, null, false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related AcctgTrans: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    acctgTrans = EntityUtil.getFirst((List<GenericValue>) acctgTransList);
                    // set-service-fields from "acctgTrans" to "createAcctgTransAndEntriesMap" for service "createAcctgTransAndEntries"
                    createAcctgTransAndEntriesMap.putAll(UtilMisc.toMap(acctgTrans));
                    entryAmount = BigDecimal.ZERO;
                    if (acctgTransList != null) {
                        for (GenericValue acctgTrans_iter : acctgTransList) {
                            acctgTrans = acctgTrans_iter;
                            if (!"PAYMENT_APPL".equals(((Map<String, Object>) acctgTrans).get("acctgTransTypeId"))) {
                                try {
                                    acctgTransEntries = acctgTrans.getRelated("AcctgTransEntry", null, null, false);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error getting related AcctgTransEntry: " + e.getMessage(), MODULE);
                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                    return "error";
                                }
                            }
                            if (acctgTransEntries != null) {
                                for (GenericValue oldAcctgTransEntryEntry : acctgTransEntries) {
                                    if ("D".equals(((Map<String, Object>) oldAcctgTransEntryEntry).get("debitCreditFlag"))) {
                                        newAcctgTransEntry = delegator.makeValue("AcctgTransEntry");
                                        newAcctgTransEntry.put("glAccountId", ((Map<String, Object>) oldAcctgTransEntryEntry).get("glAccountId"));
                                        organizationPartyId = ((Map<String, Object>) oldAcctgTransEntryEntry).get("organizationPartyId");
                                        newAcctgTransEntry.put("organizationPartyId", organizationPartyId);
                                        newAcctgTransEntry.put("partyId", ((Map<String, Object>) oldAcctgTransEntryEntry).get("partyId"));
                                        newAcctgTransEntry.put("amount", ((Map<String, Object>) oldAcctgTransEntryEntry).get("amount"));
                                        newAcctgTransEntry.put("acctgTransEntryTypeId", ((Map<String, Object>) oldAcctgTransEntryEntry).get("acctgTransEntryTypeId"));
                                        newAcctgTransEntry.put("debitCreditFlag", "C");
                                        entryAmount = (BigDecimal) ((BigDecimal) entryAmount).add((BigDecimal) ((Map<String, Object>) newAcctgTransEntry).get("amount"));
                                        createAcctgTransAndEntriesMap_acctgTransEntries.add(newAcctgTransEntry);
                                    }
                                    // set-service-fields from "oldAcctgTransEntry" to "updateAcctgTransEntryMap" for service "updateAcctgTransEntry"
                                    updateAcctgTransEntryMap.putAll(UtilMisc.toMap(oldAcctgTransEntryEntry));
                                    updateAcctgTransEntryMap.put("reconcileStatusId", "AES_RECONCILED");
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("updateAcctgTransEntry", updateAcctgTransEntryMap);
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
                                }
                            }
                        }
                    }
                    createAcctgTransAndEntriesMap.remove("acctgTransId");
                    createAcctgTransAndEntriesMap.put("transactionDate", nowTimestamp);
                    createAcctgTransAndEntriesMap.put("postedDate", nowTimestamp);
                    newAcctgTransEntry = delegator.makeValue("AcctgTransEntry");
                    newAcctgTransEntry.put("glAccountId", ((Map<String, Object>) finAccount).get("postToGlAccountId"));
                    newAcctgTransEntry.put("organizationPartyId", organizationPartyId);
                    newAcctgTransEntry.put("partyId", ((Map<String, Object>) oldAcctgTransEntry).get("partyId"));
                    newAcctgTransEntry.put("amount", entryAmount);
                    newAcctgTransEntry.put("acctgTransEntryTypeId", "_NA_");
                    newAcctgTransEntry.put("debitCreditFlag", "D");
                    createAcctgTransAndEntriesMap_acctgTransEntries.add(newAcctgTransEntry);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesMap);
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
                    try {
                        acctgTransEntries = EntityQuery.use(delegator)
                                .from("AcctgTransEntry")
                                .where(UtilMisc.toMap("acctgTransId", acctgTransId))
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying AcctgTransEntry: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (acctgTransEntries != null) {
                        for (GenericValue acctgTransEntryEntry : acctgTransEntries) {
                            // set-service-fields from "acctgTransEntry" to "createGlReconciliationEntryMap" for service "createGlReconciliationEntry"
                            createGlReconciliationEntryMap.putAll(UtilMisc.toMap(acctgTransEntryEntry));
                            createGlReconciliationEntryMap.put("glReconciliationId", ((Map<String, Object>) finAccountTrans).get("glReconciliationId"));
                            createGlReconciliationEntryMap.put("reconciledAmount", ((Map<String, Object>) acctgTransEntryEntry).get("amount"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createGlReconciliationEntry", createGlReconciliationEntryMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createGlReconciliationEntry: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                    }
                }
            }
        } else {
            errorMessage = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotDepositFinAccountTrans", locale);
            result.put("errorMessage", errorMessage);
        }

        return "success";
    }


    /**
     * Reconcile financial accounting transaction of type withdrawl
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String reconcileWithdrawalFinAcctgTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createGlReconciliationEntryMap = null;
        Map<String, Object> postAcctgTransMap = null;
        GenericValue newAcctgTransEntry = null;
        List<GenericValue> payments = null;
        List<GenericValue> acctgTransList = null;
        Map<String, Object> createAcctgTransAndEntriesMap = null;
        BigDecimal entryAmount = null;
        String errorMessage = null;
        List<Object> createAcctgTransAndEntriesMap_acctgTransEntries = null;
        List<GenericValue> acctgTransEntries = null;
        Object acctgTransId = null;
        Map<String, Object> updateAcctgTransEntryMap = null;
        Object organizationPartyId = null;
        Map<String, Object> updateAcctgTransEntry = null;
        GenericValue acctgTrans = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        Object finAccountTrans = context.get("finAccountTrans");
        GenericValue finAccount = null;
        try {
            finAccount = ((GenericValue) finAccountTrans).getRelatedOne("FinAccount", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one FinAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue oldAcctgTransEntry = null;
        GenericValue acctgTransEntry = null;
        GenericValue payment = null;
        if ("WITHDRAWAL".equals(((Map<String, Object>) finAccountTrans).get("finAccountTransTypeId"))) {
            if (UtilValidate.isEmpty(((Map<String, Object>) finAccountTrans).get("paymentId"))) {
                try {
                    payments = EntityQuery.use(delegator)
                            .from("Payment")
                            .where(UtilMisc.toMap("finAccountTransId", ((Map<String, Object>) finAccountTrans).get("finAccountTransId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                try {
                    payment = ((GenericValue) finAccountTrans).getRelatedOne("Payment", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one Payment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                payments.add(payment);
            }
            if (UtilValidate.isEmpty(payments)) {
                try {
                    acctgTransList = EntityQuery.use(delegator)
                            .from("AcctgTrans")
                            .where(UtilMisc.toMap("finAccountTransId", ((Map<String, Object>) finAccountTrans).get("finAccountTransId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                acctgTrans = EntityUtil.getFirst((List<GenericValue>) acctgTransList);
                if ("N".equals(((Map<String, Object>) acctgTrans).get("isPosted"))) {
                    // set-service-fields from "acctgTrans" to "postAcctgTransMap" for service "postAcctgTrans"
                    postAcctgTransMap.putAll(UtilMisc.toMap(acctgTrans));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("postAcctgTrans", postAcctgTransMap);
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
            if (payments != null) {
                for (GenericValue paymentEntry : payments) {
                    createAcctgTransAndEntriesMap = null;
                    try {
                        acctgTransList = paymentEntry.getRelated("AcctgTrans", null, null, false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related AcctgTrans: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    acctgTrans = EntityUtil.getFirst((List<GenericValue>) acctgTransList);
                    // set-service-fields from "acctgTrans" to "createAcctgTransAndEntriesMap" for service "createAcctgTransAndEntries"
                    createAcctgTransAndEntriesMap.putAll(UtilMisc.toMap(acctgTrans));
                    entryAmount = BigDecimal.ZERO;
                    if (acctgTransList != null) {
                        for (GenericValue acctgTrans_iter : acctgTransList) {
                            acctgTrans = acctgTrans_iter;
                            if (!"PAYMENT_APPL".equals(((Map<String, Object>) acctgTrans).get("acctgTransTypeId"))) {
                                try {
                                    acctgTransEntries = acctgTrans.getRelated("AcctgTransEntry", null, null, false);
                                } catch (Exception e) {
                                    Debug.logError(e, "Error getting related AcctgTransEntry: " + e.getMessage(), MODULE);
                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                    return "error";
                                }
                            }
                            if (acctgTransEntries != null) {
                                for (GenericValue oldAcctgTransEntryEntry : acctgTransEntries) {
                                    if ("C".equals(((Map<String, Object>) oldAcctgTransEntryEntry).get("debitCreditFlag"))) {
                                        newAcctgTransEntry = delegator.makeValue("AcctgTransEntry");
                                        newAcctgTransEntry.put("glAccountId", ((Map<String, Object>) oldAcctgTransEntryEntry).get("glAccountId"));
                                        organizationPartyId = ((Map<String, Object>) oldAcctgTransEntryEntry).get("organizationPartyId");
                                        newAcctgTransEntry.put("organizationPartyId", organizationPartyId);
                                        newAcctgTransEntry.put("partyId", ((Map<String, Object>) oldAcctgTransEntryEntry).get("partyId"));
                                        newAcctgTransEntry.put("amount", ((Map<String, Object>) oldAcctgTransEntryEntry).get("amount"));
                                        newAcctgTransEntry.put("acctgTransEntryTypeId", ((Map<String, Object>) oldAcctgTransEntryEntry).get("acctgTransEntryTypeId"));
                                        newAcctgTransEntry.put("debitCreditFlag", "D");
                                        entryAmount = (BigDecimal) ((BigDecimal) entryAmount).add((BigDecimal) ((Map<String, Object>) newAcctgTransEntry).get("amount"));
                                        createAcctgTransAndEntriesMap_acctgTransEntries.add(newAcctgTransEntry);
                                    }
                                    // set-service-fields from "oldAcctgTransEntry" to "updateAcctgTransEntryMap" for service "updateAcctgTransEntry"
                                    updateAcctgTransEntryMap.putAll(UtilMisc.toMap(oldAcctgTransEntryEntry));
                                    updateAcctgTransEntry.put("reconcileStatusId", "AES_RECONCILED");
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("updateAcctgTransEntry", updateAcctgTransEntryMap);
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
                                }
                            }
                        }
                    }
                    createAcctgTransAndEntriesMap.remove("acctgTransId");
                    createAcctgTransAndEntriesMap.put("transactionDate", nowTimestamp);
                    createAcctgTransAndEntriesMap.put("postedDate", nowTimestamp);
                    newAcctgTransEntry = delegator.makeValue("AcctgTransEntry");
                    newAcctgTransEntry.put("glAccountId", ((Map<String, Object>) finAccount).get("postToGlAccountId"));
                    newAcctgTransEntry.put("organizationPartyId", organizationPartyId);
                    newAcctgTransEntry.put("partyId", ((Map<String, Object>) oldAcctgTransEntry).get("partyId"));
                    newAcctgTransEntry.put("amount", entryAmount);
                    newAcctgTransEntry.put("acctgTransEntryTypeId", "_NA_");
                    newAcctgTransEntry.put("debitCreditFlag", "C");
                    createAcctgTransAndEntriesMap_acctgTransEntries.add(newAcctgTransEntry);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createAcctgTransAndEntries", createAcctgTransAndEntriesMap);
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
                    try {
                        acctgTransEntries = EntityQuery.use(delegator)
                                .from("AcctgTransEntry")
                                .where(UtilMisc.toMap("acctgTransId", acctgTransId))
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying AcctgTransEntry: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (acctgTransEntries != null) {
                        for (GenericValue acctgTransEntryEntry : acctgTransEntries) {
                            // set-service-fields from "acctgTransEntry" to "createGlReconciliationEntryMap" for service "createGlReconciliationEntry"
                            createGlReconciliationEntryMap.putAll(UtilMisc.toMap(acctgTransEntryEntry));
                            createGlReconciliationEntryMap.put("glReconciliationId", ((Map<String, Object>) finAccountTrans).get("glReconciliationId"));
                            createGlReconciliationEntryMap.put("reconciledAmount", ((Map<String, Object>) acctgTransEntryEntry).get("amount"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createGlReconciliationEntry", createGlReconciliationEntryMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createGlReconciliationEntry: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                    }
                }
            }
        } else {
            errorMessage = UtilProperties.getMessage("AccountingUiLabels", "AccountingNotWithdrawalFinAccountTrans", locale);
            result.put("errorMessage", errorMessage);
        }

        return "success";
    }


    /**
     * create new payment and associate with respective financial account in FinAccountTrans Entity.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPaymentAndFinAccountTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> createPaymentMap = null;
        GenericValue paymentMethod = null;
        GenericValue finAccount = null;
        Object finAccountTransId = null;
        Map<String, Object> updatePaymentCtx = null;
        Map<String, Object> createFinAccountTransMap = null;
        // set-service-fields from "parameters" to "createPaymentMap" for service "createPayment"
        createPaymentMap.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isNotEmpty(context.get("paymentMethodId"))) {
            try {
                paymentMethod = EntityQuery.use(delegator)
                        .from("PaymentMethod")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentMethod: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            createPaymentMap.put("paymentMethodTypeId", ((Map<String, Object>) paymentMethod).get("paymentMethodTypeId"));
        }
        Object paymentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPayment", createPaymentMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            paymentId = serviceResult.get("paymentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPayment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) paymentMethod).get("finAccountId"))) {
            try {
                finAccount = EntityQuery.use(delegator)
                        .from("FinAccount")
                        .where(UtilMisc.toMap("finAccountId", ((Map<String, Object>) paymentMethod).get("finAccountId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FinAccount: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if ("FNACT_MANFROZEN".equals(((Map<String, Object>) finAccount).get("statusId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingFinAccountInactiveStatusError", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if ("FNACT_CANCELLED".equals(((Map<String, Object>) finAccount).get("statusId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingFinAccountStatusNotValidError", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            if ("Y".equals(context.get("isDepositWithDrawPayment"))) {
                // set-service-fields from "parameters" to "createFinAccountTransMap" for service "createFinAccountTrans"
                createFinAccountTransMap.putAll(UtilMisc.toMap(context));
                createFinAccountTransMap.put("finAccountId", ((Map<String, Object>) paymentMethod).get("finAccountId"));
                createFinAccountTransMap.put("paymentId", paymentId);
                createFinAccountTransMap.put("statusId", "FINACT_TRNS_CREATED");
                createFinAccountTransMap.put("partyId", context.get("partyIdFrom"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createFinAccountTrans", createFinAccountTransMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    finAccountTransId = serviceResult.get("finAccountTransId");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createFinAccountTrans: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                updatePaymentCtx.put("paymentId", paymentId);
                updatePaymentCtx.put("finAccountTransId", finAccountTransId);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePayment", updatePaymentCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePayment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        result.put("paymentId", paymentId);

        return "success";
    }


    /**
     * Transaction Total By GlReconcile Id
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getTransactionTotalByGlReconcileId(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        BigDecimal reconciledBalance = null;
        BigDecimal grandTotal = null;
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
        List<GenericValue> finAccountTransList = null;
        try {
            finAccountTransList = EntityQuery.use(delegator)
                    .from("FinAccountTrans")
                    .where(UtilMisc.toMap("glReconciliationId", context.get("glReconciliationId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        reconciledBalance = (BigDecimal) ((Map<String, Object>) glReconciliation).get("reconciledBalance");
        grandTotal = BigDecimal.ZERO;
        if (UtilValidate.isEmpty(reconciledBalance)) {
            reconciledBalance = BigDecimal.ZERO;
        }
        if (finAccountTransList != null) {
            for (GenericValue finAccountTrans : finAccountTransList) {
                if ("WITHDRAWAL".equals(((Map<String, Object>) finAccountTrans).get("finAccountTransTypeId"))) {
                    grandTotal = (BigDecimal) ((BigDecimal) grandTotal).subtract((BigDecimal) ((Map<String, Object>) finAccountTrans).get("amount"));
                } else {
                    grandTotal = (BigDecimal) ((BigDecimal) grandTotal).add((BigDecimal) ((Map<String, Object>) finAccountTrans).get("amount"));
                }
            }
        }
        grandTotal = (BigDecimal) ((BigDecimal) grandTotal).add((BigDecimal) reconciledBalance);
        result.put("grandTotal", grandTotal);

        return "success";
    }


    /**
     * Assignment of Gl Reconciliation to Fin Account Trans
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String assignGlRecToFinAccTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        List<GenericValue> payments = null;
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
        Object glReconciliationId = context.get("glReconciliationId");
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
        GenericValue payment = null;
        if ("GLREC_CREATED".equals(((Map<String, Object>) glReconciliation).get("statusId"))) {
            if (!"FINACT_TRNS_CREATED".equals(((Map<String, Object>) finAccountTrans).get("statusId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingInvalidGlReconciliationAssignment", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            } else {
                finAccountTrans.put("glReconciliationId", glReconciliationId);
                try {
                    delegator.store(finAccountTrans);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            if (UtilValidate.isEmpty(((Map<String, Object>) finAccountTrans).get("paymentId"))) {
                try {
                    payments = EntityQuery.use(delegator)
                            .from("Payment")
                            .where(UtilMisc.toMap("finAccountTransId", ((Map<String, Object>) finAccountTrans).get("finAccountTransId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                try {
                    payment = EntityQuery.use(delegator)
                            .from("Payment")
                            .where(UtilMisc.toMap("paymentId", ((Map<String, Object>) finAccountTrans).get("paymentId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                payments.add(payment);
            }
            if (payments != null) {
                for (GenericValue paymentEntry : payments) {
                    if (!("PMNT_SENT".equals(((Map<String, Object>) paymentEntry).get("statusId")) || "PMNT_RECEIVED".equals(((Map<String, Object>) paymentEntry).get("statusId")) || "PMNT_CONFIRMED".equals(((Map<String, Object>) paymentEntry).get("statusId")))) {
                        {
                            String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentsAssociateWithFinAccountHasInvalidStatusError", locale);
                            error_list.add(errorMsg);
                            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                        }
                        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                            return "error";
                        }
                    }
                }
            }
        } else {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingInvalidGlReconciliation", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }

        return "success";
    }


    /**
     * Remove finAccountTrans from reconciliation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeFinAccountTransFromReconciliation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

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
        if ("FINACT_TRNS_CREATED".equals(((Map<String, Object>) finAccountTrans).get("statusId"))) {
            finAccountTrans.remove("glReconciliationId");
            try {
                delegator.store(finAccountTrans);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingFinAccountTransInvalidStatusError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }

        return "success";
    }


    /**
     * Check GlReconciliation is Reconciled or not
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String isGlReconciliationReconciled(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Boolean isReconciled = null;
        Object glReconciliationId = context.get("glReconciliationId");
        List<GenericValue> finAccountTransList = null;
        try {
            finAccountTransList = EntityQuery.use(delegator)
                    .from("FinAccountTrans")
                    .where(UtilMisc.toMap("glReconciliationId", glReconciliationId))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object finAccountTransIds = GroovyUtil.eval("org.ofbiz.entity.util.EntityUtil.getFieldListFromEntityList(finAccountTransList, 'finAccountTransId', true);", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        List<GenericValue> finAccountTransactions = null;
        try {
            finAccountTransactions = EntityQuery.use(delegator)
                    .from("FinAccountTrans")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(finAccountTransactions)) {
            isReconciled = Boolean.FALSE;
        } else {
            isReconciled = Boolean.TRUE;
        }
        result.put("isReconciled", isReconciled);

        return "success";
    }


    /**
     * Cancel bank reconciliation.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String cancelBankReconciliation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> removeFinAccountTransFromReconciliationMap = null;
        List<GenericValue> finAccountTransList = null;
        try {
            finAccountTransList = EntityQuery.use(delegator)
                    .from("FinAccountTrans")
                    .where(UtilMisc.toMap("glReconciliationId", context.get("glReconciliationId"), "statusId", "FINACT_TRNS_CREATED"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(finAccountTransList)) {
            if (finAccountTransList != null) {
                for (GenericValue finAccountTrans : finAccountTransList) {
                    removeFinAccountTransFromReconciliationMap.put("finAccountTransId", ((Map<String, Object>) finAccountTrans).get("finAccountTransId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("removeFinAccountTransFromReconciliation", removeFinAccountTransFromReconciliationMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling removeFinAccountTransFromReconciliation: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Get associated acctgTransEntries with finAccountTrans
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getAssociatedAcctgTransEntriesWithFinAccountTrans(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<GenericValue> payments = null;
        GenericValue payment = null;
        Object finAccountTransId = context.get("finAccountTransId");
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
        if (UtilValidate.isEmpty(((Map<String, Object>) finAccountTrans).get("paymentId"))) {
            try {
                payments = EntityQuery.use(delegator)
                        .from("Payment")
                        .where(UtilMisc.toMap("finAccountTransId", ((Map<String, Object>) finAccountTrans).get("finAccountTransId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            try {
                payment = EntityQuery.use(delegator)
                        .from("Payment")
                        .where(UtilMisc.toMap("paymentId", ((Map<String, Object>) finAccountTrans).get("paymentId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            payments.add(payment);
        }
        Object paymentIds = GroovyUtil.eval("org.ofbiz.entity.util.EntityUtil.getFieldListFromEntityList(payments, 'paymentId', true);", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
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
        result.put("acctgTransAndEntries", acctgTransAndEntries);

        return "success";
    }


    /**
     * Get Reconciliation Closing Balance.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getReconciliationClosingBalance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

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
        BigDecimal reconciledBalance = (BigDecimal) ((Map<String, Object>) glReconciliation).get("reconciledBalance");
        BigDecimal openingBalance = (BigDecimal) ((Map<String, Object>) glReconciliation).get("openingBalance");
        BigDecimal closingBalance = (BigDecimal) ((BigDecimal) reconciledBalance).add((BigDecimal) openingBalance);
        result.put("closingBalance", closingBalance);

        return "success";
    }


    /**
     * Auto Reconcile Financial(bank) Account Transactions
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String autoFinAccountReconciliation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> reconcileFinAccountTransMap = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        Object reconciliationDayStart = (Timestamp) GroovyUtil.eval("org.ofbiz.base.util.UtilDateTime.getDayStart(nowTimestamp)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        Object reconciliationDayEnd = (Timestamp) GroovyUtil.eval("org.ofbiz.base.util.UtilDateTime.getDayEnd(nowTimestamp)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        List<GenericValue> glReconciliation = null;
        try {
            glReconciliation = EntityQuery.use(delegator)
                    .from("GlReconciliation")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying GlReconciliation: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object glReconciliationIds = GroovyUtil.eval("org.ofbiz.entity.util.EntityUtil.getFieldListFromEntityList(glReconciliation, 'glReconciliationId', true);", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        List<GenericValue> finAccountTransList = null;
        try {
            finAccountTransList = EntityQuery.use(delegator)
                    .from("FinAccountTrans")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (finAccountTransList != null) {
            for (GenericValue finAccountTrans : finAccountTransList) {
                reconcileFinAccountTransMap.put("finAccountTransId", ((Map<String, Object>) finAccountTrans).get("finAccountTransId"));
                reconcileFinAccountTransMap.put("organizationPartyId", ((Map<String, Object>) finAccountTrans).get("partyId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("reconcileFinAccountTrans", reconcileFinAccountTransMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling reconcileFinAccountTrans: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }

}

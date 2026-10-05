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
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/rate/RateServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class RateServices {

    private static final String MODULE = RateServices.class.getName();


    /**
     * update/create a rate amount value
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateRateAmount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> delRateAmount = null;
        GenericValue rateAmount = null;
        if (UtilValidate.isEmpty(context.get("rateCurrencyUomId"))) {
            String parameters_rateCurrencyUomId = UtilProperties.getMessage("general.properties", "currency.uom.id.default", locale);
        }
        if (UtilValidate.isEmpty(context.get("periodTypeId"))) {
            context.put("periodTypeId", "RATE_HOUR");
        }
        if (UtilValidate.isEmpty(context.get("emplPositionTypeId"))) {
            context.put("emplPositionTypeId", "_NA_");
        }
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", "_NA_");
        }
        if (UtilValidate.isEmpty(context.get("workEffortId"))) {
            context.put("workEffortId", "_NA_");
        }
        List<GenericValue> rateAmounts = null;
        try {
            rateAmounts = EntityQuery.use(delegator)
                    .from("RateAmount")
                    .where(UtilMisc.toMap("rateTypeId", context.get("rateTypeId"), "workEffortId", context.get("workEffortId"), "rateCurrencyUomId", context.get("rateCurrencyUomId"), "emplPositionTypeId", context.get("emplPositionTypeId"), "partyId", context.get("partyId"), "periodTypeId", context.get("periodTypeId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying RateAmount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(rateAmounts)) {
            rateAmount = EntityUtil.getFirst((List<GenericValue>) rateAmounts);
            if (!java.util.Objects.equals(((Map<String, Object>) rateAmount).get("rateAmount"), context.get("rateAmount"))) {
                // set-service-fields from "rateAmount" to "delRateAmount" for service "expireRateAmount"
                delRateAmount.putAll(UtilMisc.toMap(rateAmount));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("expireRateAmount", delRateAmount);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling expireRateAmount: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        GenericValue newEntity = delegator.makeValue("RateAmount");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
        }
        newEntity.remove("thruDate");
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
     * expire a rate amount value
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String expireRateAmount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue rateAmount = null;
        Timestamp nowTimestamp = null;
        Timestamp previousDay = null;
        if (UtilValidate.isEmpty(context.get("rateCurrencyUomId"))) {
            String parameters_rateCurrencyUomId = UtilProperties.getMessage("general.properties", "currency.uom.id.default", locale);
        }
        if (UtilValidate.isEmpty(context.get("periodTypeId"))) {
            context.put("periodTypeId", "RATE_HOUR");
        }
        if (UtilValidate.isEmpty(context.get("emplPositionTypeId"))) {
            context.put("emplPositionTypeId", "_NA_");
        }
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", "_NA_");
        }
        if (UtilValidate.isEmpty(context.get("workEffortId"))) {
            context.put("workEffortId", "_NA_");
        }
        try {
            rateAmount = EntityQuery.use(delegator)
                    .from("RateAmount")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying RateAmount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(rateAmount)) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            previousDay = (Timestamp) GroovyUtil.eval("org.ofbiz.base.util.UtilDateTime.adjustTimestamp(nowTimestamp,5,-1)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
            rateAmount.put("thruDate", (Timestamp) GroovyUtil.eval("org.ofbiz.base.util.UtilDateTime.getDayEnd(previousDay)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context)));
            try {
                delegator.store(rateAmount);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            {
                String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels", "AccountingDeleteRateAmount", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }

        return "success";
    }


    /**
     * delete (expire) a rate amount value
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteRateAmount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = expireRateAmount(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Get the applicable rate amount value
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getRateAmount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object level = null;
        List<GenericValue> ratesList = null;
        GenericValue rateType = null;
        GenericValue amount = null;
        if (UtilValidate.isEmpty(context.get("rateCurrencyUomId"))) {
            String parameters_rateCurrencyUomId = UtilProperties.getMessage("general.properties", "currency.uom.id.default", locale);
        }
        if (UtilValidate.isEmpty(context.get("periodTypeId"))) {
            context.put("periodTypeId", "RATE_HOUR");
        }
        Object parameters_ratesList = null;
        if ((!(UtilValidate.isEmpty(context.get("workEffortId"))) && !"_NA_".equals(context.get("workEffortId")))) {
            level = "workEffort";
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getRatesAmountsFromWorkEffortId", context);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("ratesList", serviceResult.get("ratesList"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling getRatesAmountsFromWorkEffortId: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("filterRateAmountList", context);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("ratesList", serviceResult.get("filteredRatesList"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling filterRateAmountList: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isEmpty(context.get("ratesList"))) {
            try {
                ratesList = EntityQuery.use(delegator)
                        .from("RateAmount")
                        .where(UtilMisc.toMap("rateTypeId", context.get("rateTypeId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying RateAmount: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("filterRateAmountList", context);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("ratesList", serviceResult.get("filteredRatesList"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling filterRateAmountList: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isEmpty(context.get("ratesList"))) {
            try {
                rateType = EntityQuery.use(delegator)
                        .from("RateType")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying RateType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logError("A valid rate amount could not be found for rateType: " + ((Map<String, Object>) rateType).get("description"), MODULE);
        }
        if (UtilValidate.isNotEmpty(context.get("ratesList"))) {
            amount = EntityUtil.getFirst((List<GenericValue>) context.get("ratesList"));
            if (UtilValidate.isEmpty(((Map<String, Object>) amount).get("rateAmount"))) {
                amount.put("rateAmount", BigDecimal.ZERO);
            }
            result.put("rateAmount", ((Map<String, Object>) amount).get("rateAmount"));
            result.put("periodTypeId", ((Map<String, Object>) amount).get("periodTypeId"));
            result.put("rateCurrencyUomId", ((Map<String, Object>) amount).get("rateCurrencyUomId"));
            result.put("level", level);
            result.put("fromDate", ((Map<String, Object>) amount).get("fromDate"));
        }

        return "success";
    }


    /**
     * Get all the rateAmount for a given workEffortId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getRatesAmountsFromWorkEffortId(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue rateType = null;
        GenericValue workEffort = null;
        GenericValue currencyUomId = null;
        GenericValue periodType = null;
        GenericValue partyNameView = null;
        List<GenericValue> amounts = null;
        try {
            amounts = EntityQuery.use(delegator)
                    .from("RateAmount")
                    .where(UtilMisc.toMap("rateTypeId", context.get("rateTypeId"), "workEffortId", context.get("workEffortId"), "periodTypeId", context.get("periodTypeId"), "rateCurrencyUomId", context.get("rateCurrencyUomId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying RateAmount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(amounts)) {
            try {
                currencyUomId = EntityQuery.use(delegator)
                        .from("Uom")
                        .where(UtilMisc.toMap("uomId", context.get("rateCurrencyUomId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Uom: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                periodType = EntityQuery.use(delegator)
                        .from("PeriodType")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PeriodType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                rateType = EntityQuery.use(delegator)
                        .from("RateType")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying RateType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                workEffort = EntityQuery.use(delegator)
                        .from("WorkEffort")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                partyNameView = EntityQuery.use(delegator)
                        .from("PartyNameView")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyNameView: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logError("A valid rate entry could be found for rateType: " + ((Map<String, Object>) rateType).get("description") + ", workEffort: " + ((Map<String, Object>) workEffort).get("workEffortName") + ", party: " + ((Map<String, Object>) partyNameView).get("lastName") + " " + ((Map<String, Object>) partyNameView).get("middleName") + " " + ((Map<String, Object>) partyNameView).get("firstName") + ((Map<String, Object>) partyNameView).get("groupName") + " However.....not for the period: " + ((Map<String, Object>) context.get("period")).get("description") + " and currency: " + ((Map<String, Object>) currencyUomId).get("description"), MODULE);
        }
        result.put("ratesList", amounts);
        result.put("level", context.get("level"));

        return "success";
    }


    /**
     * Get all the rateAmount for a given partyId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getRatesAmountsFromPartyId(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue rateType = null;
        GenericValue currencyUomId = null;
        GenericValue periodType = null;
        GenericValue partyNameView = null;
        List<GenericValue> amounts = null;
        try {
            amounts = EntityQuery.use(delegator)
                    .from("RateAmount")
                    .where(UtilMisc.toMap("rateTypeId", context.get("rateTypeId"), "partyId", context.get("partyId"), "periodTypeId", context.get("periodTypeId"), "rateCurrencyUomId", context.get("rateCurrencyUomId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying RateAmount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(amounts)) {
            try {
                currencyUomId = EntityQuery.use(delegator)
                        .from("Uom")
                        .where(UtilMisc.toMap("uomId", context.get("rateCurrencyUomId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Uom: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                periodType = EntityQuery.use(delegator)
                        .from("PeriodType")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PeriodType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                rateType = EntityQuery.use(delegator)
                        .from("RateType")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying RateType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                partyNameView = EntityQuery.use(delegator)
                        .from("PartyNameView")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyNameView: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logError("A valid rate entry could be found for rateType: " + ((Map<String, Object>) rateType).get("description") + ", party: " + ((Map<String, Object>) partyNameView).get("lastName") + " " + ((Map<String, Object>) partyNameView).get("middleName") + " " + ((Map<String, Object>) partyNameView).get("firstName") + ((Map<String, Object>) partyNameView).get("groupName") + " However..... NOT   for the period: " + ((Map<String, Object>) context.get("period")).get("description") + " and currency: " + ((Map<String, Object>) currencyUomId).get("description"), MODULE);
        }
        result.put("ratesList", amounts);
        result.put("level", context.get("level"));

        return "success";
    }


    /**
     * Get all the rateAmount for a given emplPositionTypeId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getRatesAmountsFromEmplPositionTypeId(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue rateType = null;
        GenericValue currencyUomId = null;
        GenericValue periodType = null;
        GenericValue emplPositionType = null;
        List<GenericValue> amounts = null;
        try {
            amounts = EntityQuery.use(delegator)
                    .from("RateAmount")
                    .where(UtilMisc.toMap("rateTypeId", context.get("rateTypeId"), "emplPositionTypeId", context.get("emplPositionTypeId"), "periodTypeId", context.get("periodTypeId"), "rateCurrencyUomId", context.get("rateCurrencyUomId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying RateAmount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(amounts)) {
            try {
                currencyUomId = EntityQuery.use(delegator)
                        .from("Uom")
                        .where(UtilMisc.toMap("uomId", context.get("rateCurrencyUomId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Uom: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                periodType = EntityQuery.use(delegator)
                        .from("PeriodType")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PeriodType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                rateType = EntityQuery.use(delegator)
                        .from("RateType")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying RateType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                emplPositionType = EntityQuery.use(delegator)
                        .from("EmplPositionType")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying EmplPositionType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logError("A valid rate entry could be found for rateType: " + ((Map<String, Object>) rateType).get("description") + ", emplPositionType: " + ((Map<String, Object>) emplPositionType).get("description") + ".... However.....NOT for the period: " + ((Map<String, Object>) context.get("period")).get("description") + " and currency: " + ((Map<String, Object>) currencyUomId).get("description"), MODULE);
        }
        result.put("ratesList", amounts);
        result.put("level", context.get("level"));

        return "success";
    }


    /**
     * Filter a list of rateAmount. The result is the         most heavily-filtered non-empty list
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String filterRateAmountList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> filterMap = null;
        Object tempRatesFilteredList = null;
        if (UtilValidate.isEmpty(context.get("ratesList"))) {
            Debug.logWarning("The list parameters.ratesList was empty, not processing any further", MODULE);
            return "success";
        }
        filterMap = null;
        if (UtilValidate.isNotEmpty(context.get("workEffortId"))) {
            filterMap.put("workEffortId", context.get("workEffortId"));
            // TODO: Convert <filter-list-by-and> element
            if (UtilValidate.isNotEmpty(tempRatesFilteredList)) {
                context.put("ratesList", tempRatesFilteredList);
            }
            filterMap = null;
            tempRatesFilteredList = null;
        }
        if (UtilValidate.isNotEmpty(context.get("partyId"))) {
            filterMap.put("partyId", context.get("partyId"));
            // TODO: Convert <filter-list-by-and> element
            if (UtilValidate.isNotEmpty(tempRatesFilteredList)) {
                context.put("ratesList", tempRatesFilteredList);
            }
            filterMap = null;
            tempRatesFilteredList = null;
        }
        if (UtilValidate.isNotEmpty(context.get("emplPositionTypeId"))) {
            filterMap.put("emplPositionTypeId", context.get("emplPositionTypeId"));
            // TODO: Convert <filter-list-by-and> element
            if (UtilValidate.isNotEmpty(tempRatesFilteredList)) {
                context.put("ratesList", tempRatesFilteredList);
            }
            filterMap = null;
            tempRatesFilteredList = null;
        }
        if (UtilValidate.isNotEmpty(context.get("rateTypeId"))) {
            filterMap.put("rateTypeId", context.get("rateTypeId"));
            // TODO: Convert <filter-list-by-and> element
            if (UtilValidate.isNotEmpty(tempRatesFilteredList)) {
                context.put("ratesList", tempRatesFilteredList);
            }
            filterMap = null;
            tempRatesFilteredList = null;
        }
        result.put("filteredRatesList", context.get("ratesList"));

        return "success";
    }


    /**
     * Update/Create PartyRate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyRate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue partyRate = null;
        Map<String, Object> updRate = null;
        List<GenericValue> partyRates = null;
        try {
            partyRates = EntityQuery.use(delegator)
                    .from("PartyRate")
                    .where(UtilMisc.toMap("partyId", context.get("partyId"), "rateTypeId", context.get("rateTypeId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRate: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(partyRates)) {
            partyRate = EntityUtil.getFirst((List<GenericValue>) partyRates);
            Timestamp partyRate_thruDate = new Timestamp(System.currentTimeMillis());
            try {
                delegator.store(partyRate);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        GenericValue newEntity = delegator.makeValue("PartyRate");
        newEntity.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
        }
        newEntity.setNonPKFields((Map<String, Object>) context);
        String result = checkOtherDefaultRate(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(context.get("rateAmount"))) {
            // set-service-fields from "parameters" to "updRate" for service "updateRateAmount"
            updRate.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateRateAmount", updRate);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateRateAmount: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * remove an other defaultRate flag
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkOtherDefaultRate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue rate = null;
        List<GenericValue> rates = null;
        Object securityAction = "_CREATE";
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object newEntity = null;
        if ("Y".equals(((Map<String, Object>) newEntity).get("defaultRate"))) {
            try {
                rates = EntityQuery.use(delegator)
                        .from("PartyRate")
                        .where(UtilMisc.toMap("partyId", ((Map<String, Object>) newEntity).get("partyId"), "defaultRate", "Y"))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyRate: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(rates)) {
                rate = EntityUtil.getFirst((List<GenericValue>) rates);
                rate.put("defaultRate", "N");
                try {
                    delegator.store(rate);
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
     * Expire PartyRate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String expirePartyRate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PartyRate")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRate: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Timestamp lookedUpValue_thruDate = new Timestamp(System.currentTimeMillis());
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> delRateAmount = new HashMap<>();
        // set-service-fields from "parameters" to "delRateAmount" for service "expireRateAmount"
        delRateAmount.putAll(UtilMisc.toMap(context));
        delRateAmount.put("fromDate", context.get("rateAmountFromDate"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("expireRateAmount", delRateAmount);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling expireRateAmount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * migrate the several entities which were change in the rate refactor activity
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String migrateRateFactor(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue rateAmount = null;
        GenericValue emplPositionTypeRate = null;
        GenericValue partyRate = null;
        List<GenericValue> posRates = null;
        try {
            posRates = EntityQuery.use(delegator)
                    .from("OldEmplPositionTypeRate")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OldEmplPositionTypeRate: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (posRates != null) {
            for (GenericValue posRate : posRates) {
                emplPositionTypeRate = delegator.makeValue("EmplPositionTypeRate");
                posRate.setPKFields((Map<String, Object>) emplPositionTypeRate);
                posRate.setNonPKFields((Map<String, Object>) emplPositionTypeRate);
                try {
                    delegator.create(emplPositionTypeRate);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                rateAmount = delegator.makeValue("RateAmount");
                posRate.setPKFields((Map<String, Object>) rateAmount);
                posRate.setNonPKFields((Map<String, Object>) rateAmount);
                rateAmount.put("workeffortId", "_NA_");
                rateAmount.put("partyId", "_NA_");
                String rateAmount_rateCurrencyUomId = UtilProperties.getMessage("general.properties", "currency.uom.id.default", locale);
                try {
                    delegator.create(rateAmount);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        List<GenericValue> partyRates = null;
        try {
            partyRates = EntityQuery.use(delegator)
                    .from("OldPartyRate")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OldPartyRate: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (context.get("oldPartyRates") != null) {
            for (Object oldPartyRate : (List<Object>) context.get("oldPartyRates")) {
                partyRate = delegator.makeValue("PartyRate");
                ((GenericValue) oldPartyRate).setPKFields((Map<String, Object>) partyRate);
                ((GenericValue) oldPartyRate).setNonPKFields((Map<String, Object>) partyRate);
                try {
                    delegator.create(partyRate);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                rateAmount = delegator.makeValue("RateAmount");
                ((GenericValue) oldPartyRate).setPKFields((Map<String, Object>) rateAmount);
                ((GenericValue) oldPartyRate).setNonPKFields((Map<String, Object>) rateAmount);
                rateAmount.put("workeffortId", "_NA_");
                rateAmount.put("emplPositionTypeId", "_NA_");
                rateAmount.put("periodTypeId", "RATE_HOUR");
                try {
                    delegator.create(rateAmount);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }

}

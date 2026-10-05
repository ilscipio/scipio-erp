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
package com.ilscipio.scipio.order.event;

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
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.config.ProductConfigWorker;
import org.ofbiz.product.config.ProductConfigWrapper;
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.order.shoppingcart.ShoppingCartItem;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://order/script/org/ofbiz/order/quote/QuoteServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class QuoteServices {

    private static final String MODULE = QuoteServices.class.getName();


    /**
     * Get Next quoteId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getNextQuoteId(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue customMethod = null;
        Object customMethodName = null;
        String quoteIdTemp = null;
        Map<String, Object> customMethodMap = null;
        GenericValue partyAcctgPreference = null;
        try {
            partyAcctgPreference = EntityQuery.use(delegator)
                    .from("PartyAcctgPreference")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyAcctgPreference: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("In getNextQuoteId partyId is [" + context.get("partyId") + "], partyAcctgPreference: " + partyAcctgPreference, MODULE);
        if (UtilValidate.isNotEmpty(partyAcctgPreference)) {
            try {
                customMethod = partyAcctgPreference.getRelatedOne("QuoteCustomMethod", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one QuoteCustomMethod: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            Debug.logWarning("Acctg preference not defined for partyId [" + context.get("partyId") + "]", MODULE);
        }
        if (UtilValidate.isNotEmpty(customMethod)) {
            customMethodName = ((Map<String, Object>) customMethod).get("customMethodName");
        } else {
            if ("QUOSQ_ENF_SEQ".equals(((Map<String, Object>) partyAcctgPreference).get("oldQuoteSequenceEnumId"))) {
                customMethodName = "quoteSequenceEnforced";
            }
        }
        if (UtilValidate.isNotEmpty(customMethod)) {
            // set-service-fields from "parameters" to "customMethodMap" for service "${customMethodName}"
            customMethodMap.putAll(UtilMisc.toMap(context));
            customMethodMap.put("partyId", context.get("partyId"));
            customMethodMap.put("partyAcctgPreference", partyAcctgPreference);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("${customMethodName}", customMethodMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                quoteIdTemp = (String) serviceResult.get("quoteId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling ${customMethodName}: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            quoteIdTemp = (String) context.get("quoteId");
            if (UtilValidate.isEmpty(quoteIdTemp)) {
                quoteIdTemp = delegator.getNextSeqId("Quote");
            } else {
                // TODO: Convert <check-id> element
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        Object quoteId = ((Map<String, Object>) partyAcctgPreference).get("quoteIdPrefix") + quoteIdTemp;
        result.put("quoteId", quoteId);

        return "success";
    }


    /**
     * Enforced Sequence (no gaps, per organization)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String quoteSequenceEnforced(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Debug.logInfo("In getNextQuoteId sequence enum Enforced", MODULE);
        GenericValue partyAcctgPreference = (GenericValue) context.get("partyAcctgPreference");
        if (UtilValidate.isNotEmpty(((Map<String, Object>) partyAcctgPreference).get("lastQuoteNumber"))) {
            partyAcctgPreference.set("lastQuoteNumber", new BigDecimal(((Map<String, Object>) partyAcctgPreference).get("lastQuoteNumber").toString()));
        } else {
            partyAcctgPreference.set("lastQuoteNumber", 1);
        }
        try {
            delegator.store(partyAcctgPreference);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object quoteId = ((Map<String, Object>) partyAcctgPreference).get("lastQuoteNumber");
        result.put("quoteId", quoteId);

        return "success";
    }


    /**
     * Create a Quote
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuote(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        Map<String, Object> getNextQuoteIdContext = null;
        Map<String, Object> takerMap = null;
        if ((!(UtilValidate.isEmpty(context.get("partyId"))) && !(java.util.Objects.equals(context.get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) && !(true /* TODO: if-has-permission */))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderSecurityErrorToRunCreateQuote", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        newEntity = delegator.makeValue("Quote");
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("statusId"))) {
            newEntity.put("statusId", "QUO_CREATED");
        }
        GenericValue productStore = null;
        try {
            productStore = EntityQuery.use(delegator)
                    .from("ProductStore")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) productStore).get("payToPartyId"))) {
            // set-service-fields from "parameters" to "getNextQuoteIdContext" for service "getNextQuoteId"
            getNextQuoteIdContext.putAll(UtilMisc.toMap(context));
            getNextQuoteIdContext.put("partyId", ((Map<String, Object>) productStore).get("payToPartyId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getNextQuoteId", getNextQuoteIdContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                newEntity.put("quoteId", serviceResult.get("quoteId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling getNextQuoteId: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            ((GenericValue) newEntity).put("quoteId", delegator.getNextSeqId("Quote"));
        }
        result.put("quoteId", ((Map<String, Object>) newEntity).get("quoteId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (!java.util.Objects.equals(context.get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) {
            takerMap.put("quoteId", ((Map<String, Object>) newEntity).get("quoteId"));
            takerMap.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
            takerMap.put("roleTypeId", "REQ_TAKER");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createQuoteRole", takerMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createQuoteRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Update an existing Quote
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateQuote(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        List<GenericValue> validChange = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quote = null;
        try {
            quote = EntityQuery.use(delegator)
                    .from("Quote")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Quote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("statusId"))) {
            context.put("statusId", ((Map<String, Object>) quote).get("statusId"));
        }
        if (!java.util.Objects.equals(((Map<String, Object>) quote).get("statusId"), context.get("statusId"))) {
            try {
                validChange = EntityQuery.use(delegator)
                        .from("StatusValidChange")
                        .where(UtilMisc.toMap("statusId", ((Map<String, Object>) quote).get("statusId"), "statusIdTo", context.get("statusId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(validChange)) {
                {
                    String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderQuoteStatusChangeIsNotValid", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                Debug.logError("The status change from " + ((Map<String, Object>) quote).get("statusId") + " to " + context.get("statusId") + " is not a valid change", MODULE);
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        quote.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(quote);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Copy an existing Quote
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String copyQuote(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createQuoteItemInMap = null;
        List<GenericValue> quoteItems = null;
        List<GenericValue> quoteAdjustments = null;
        Map<String, Object> createQuoteAdjustmentInMap = null;
        List<GenericValue> quoteRoles = null;
        Map<String, Object> createQuoteRoleInMap = null;
        Map<String, Object> createQuoteAttributeInMap = null;
        List<GenericValue> quoteAttributes = null;
        List<GenericValue> quoteCoefficients = null;
        Map<String, Object> createQuoteCoefficientInMap = null;
        List<GenericValue> quoteTerms = null;
        Map<String, Object> createQuoteTermInMap = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quote = null;
        try {
            quote = EntityQuery.use(delegator)
                    .from("Quote")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Quote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> createQuoteInMap = new HashMap<>();
        // set-service-fields from "quote" to "createQuoteInMap" for service "createQuote"
        createQuoteInMap.putAll(UtilMisc.toMap(quote));
        createQuoteInMap.remove("statusId");
        Object quoteIdTo = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createQuote", createQuoteInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            quoteIdTo = serviceResult.get("quoteId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createQuote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("quoteId", quoteIdTo);
        if ("Y".equals(context.get("copyQuoteItems"))) {
            try {
                quoteItems = quote.getRelated("QuoteItem", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related QuoteItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (quoteItems != null) {
                for (GenericValue quoteItem : quoteItems) {
                    createQuoteItemInMap = null;
                    // set-service-fields from "quoteItem" to "createQuoteItemInMap" for service "createQuoteItem"
                    createQuoteItemInMap.putAll(UtilMisc.toMap(quoteItem));
                    createQuoteItemInMap.put("quoteIdTo", quoteIdTo);
                    createQuoteItemInMap.put("copyQuoteAdjustments", context.get("copyQuoteAdjustments"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("copyQuoteItem", createQuoteItemInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling copyQuoteItem: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        if ("Y".equals(context.get("copyQuoteAdjustments"))) {
            try {
                quoteAdjustments = quote.getRelated("QuoteAdjustment", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related QuoteAdjustment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (quoteAdjustments != null) {
                for (GenericValue quoteAdjustment : quoteAdjustments) {
                    if (UtilValidate.isEmpty(((Map<String, Object>) quoteAdjustment).get("quoteItemSeqId"))) {
                        createQuoteAdjustmentInMap = null;
                        // set-service-fields from "quoteAdjustment" to "createQuoteAdjustmentInMap" for service "createQuoteAdjustment"
                        createQuoteAdjustmentInMap.putAll(UtilMisc.toMap(quoteAdjustment));
                        createQuoteAdjustmentInMap.put("quoteId", quoteIdTo);
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createQuoteAdjustment", createQuoteAdjustmentInMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createQuoteAdjustment: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
            }
        }
        if ("Y".equals(context.get("copyQuoteRoles"))) {
            try {
                quoteRoles = quote.getRelated("QuoteRole", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related QuoteRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (quoteRoles != null) {
                for (GenericValue quoteRole : quoteRoles) {
                    if (!"REQ_TAKER".equals(((Map<String, Object>) quoteRole).get("roleTypeId"))) {
                        createQuoteRoleInMap = null;
                        // set-service-fields from "quoteRole" to "createQuoteRoleInMap" for service "createQuoteRole"
                        createQuoteRoleInMap.putAll(UtilMisc.toMap(quoteRole));
                        createQuoteRoleInMap.put("quoteId", quoteIdTo);
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createQuoteRole", createQuoteRoleInMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createQuoteRole: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
            }
        }
        if ("Y".equals(context.get("copyQuoteAttributes"))) {
            try {
                quoteAttributes = quote.getRelated("QuoteAttribute", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related QuoteAttribute: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (quoteAttributes != null) {
                for (GenericValue quoteAttribute : quoteAttributes) {
                    createQuoteAttributeInMap = null;
                    // set-service-fields from "quoteAttribute" to "createQuoteAttributeInMap" for service "createQuoteAttribute"
                    createQuoteAttributeInMap.putAll(UtilMisc.toMap(quoteAttribute));
                    createQuoteAttributeInMap.put("quoteId", quoteIdTo);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createQuoteAttribute", createQuoteAttributeInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createQuoteAttribute: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        if ("Y".equals(context.get("copyQuoteCoefficients"))) {
            try {
                quoteCoefficients = quote.getRelated("QuoteCoefficient", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related QuoteCoefficient: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (quoteCoefficients != null) {
                for (GenericValue quoteCoefficient : quoteCoefficients) {
                    createQuoteCoefficientInMap = null;
                    // set-service-fields from "quoteCoefficient" to "createQuoteCoefficientInMap" for service "createQuoteCoefficient"
                    createQuoteCoefficientInMap.putAll(UtilMisc.toMap(quoteCoefficient));
                    createQuoteCoefficientInMap.put("quoteId", quoteIdTo);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createQuoteCoefficient", createQuoteCoefficientInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createQuoteCoefficient: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        if ("Y".equals(context.get("copyQuoteTerms"))) {
            try {
                quoteTerms = quote.getRelated("QuoteTerm", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related QuoteTerm: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (quoteTerms != null) {
                for (GenericValue quoteTerm : quoteTerms) {
                    createQuoteTermInMap = null;
                    // set-service-fields from "quoteTerm" to "createQuoteTermInMap" for service "createQuoteTerm"
                    createQuoteTermInMap.putAll(UtilMisc.toMap(quoteTerm));
                    createQuoteTermInMap.put("quoteId", quoteIdTo);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createQuoteTerm", createQuoteTermInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createQuoteTerm: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Set the Quote status to ordered.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkUpdateQuoteStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quote = null;
        try {
            quote = EntityQuery.use(delegator)
                    .from("Quote")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Quote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        quote.put("statusId", "QUO_ORDERED");
        try {
            delegator.store(quote);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Create a QuoteRole
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuoteRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue partyRole = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> lookupPKMap = new HashMap<>();
        lookupPKMap.put("partyId", context.get("partyId"));
        lookupPKMap.put("roleTypeId", context.get("roleTypeId"));
        try {
            partyRole = EntityQuery.use(delegator)
                    .from("PartyRole")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key PartyRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) partyRole).get("partyId"))) {
            // TODO: Convert <check-permission> element
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            // TODO: Convert <check-permission> element
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            partyRole = delegator.makeValue("PartyRole");
            partyRole.setPKFields((Map<String, Object>) lookupPKMap);
            try {
                delegator.create(partyRole);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        GenericValue newEntity = delegator.makeValue("QuoteRole");
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Remove an existing QuoteRole
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeQuoteRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quoteRole = null;
        try {
            quoteRole = EntityQuery.use(delegator)
                    .from("QuoteRole")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(quoteRole);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Create a QuoteItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuoteItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        GenericValue product = null;
        Object totalPrice = null;
        ProductConfigWrapper configWrapper = null;
        Map<String, Object> calculateProductPriceMap = null;
        GenericValue quote = null;
        try {
            quote = EntityQuery.use(delegator)
                    .from("Quote")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Quote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ((!(UtilValidate.isEmpty(((Map<String, Object>) quote).get("partyId"))) && !(java.util.Objects.equals(((Map<String, Object>) quote).get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) && !(true /* TODO: if-has-permission */))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderSecurityErrorToRunCreateQuoteItem", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        newEntity = delegator.makeValue("QuoteItem");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.put("quoteId", context.get("quoteId"));
        newEntity.put("quoteItemSeqId", context.get("quoteItemSeqId"));
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("quoteItemSeqId"))) {
            delegator.setNextSubSeqId(newEntity, "quoteItemSeqId", 5, 1);
            Object quoteItemSeqId = newEntity.get("quoteItemSeqId");
            newEntity.put("quoteItemSeqId", quoteItemSeqId);
        }
        result.put("quoteId", ((Map<String, Object>) newEntity).get("quoteId"));
        result.put("quoteItemSeqId", ((Map<String, Object>) newEntity).get("quoteItemSeqId"));
        if (UtilValidate.isEmpty(context.get("quoteUnitPrice"))) {
            if (UtilValidate.isNotEmpty(context.get("productId"))) {
                try {
                    product = EntityQuery.use(delegator)
                            .from("Product")
                            .where(UtilMisc.toMap())
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if ("Y".equals(((Map<String, Object>) product).get("isVirtual"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderCannotAddVirtualProductToQuote", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
                Object newEntity_quoteUnitPrice = null;
                Object calculateProductPriceMap_product = null;
                Object calculateProductPriceMap_quantity = null;
                Object calculateProductPriceMap_amount = null;
                if ((("AGGREGATED".equals(((Map<String, Object>) product).get("productTypeId")) || "AGGREGATED_SERVICE".equals(((Map<String, Object>) product).get("productTypeId"))) && !(UtilValidate.isEmpty(context.get("configId"))))) {
                    try {
                        configWrapper = ProductConfigWorker.loadProductConfigWrapper(delegator, dispatcher, (String) context.get("configId"), (String) context.get("productId"), null, null, null, null, locale, userLogin);
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling ProductConfigWorker.loadProductConfigWrapper: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    totalPrice = configWrapper.getTotalPrice();
                    newEntity.put("quoteUnitPrice", totalPrice);
                } else {
                    calculateProductPriceMap.put("product", product);
                    calculateProductPriceMap.put("quantity", context.get("quantity"));
                    if (UtilValidate.isNotEmpty(context.get("selectedAmount"))) {
                        calculateProductPriceMap.put("amount", context.get("selectedAmount"));
                    }
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("calculateProductPrice", calculateProductPriceMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        newEntity.put("quoteUnitPrice", serviceResult.get("price"));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling calculateProductPrice: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Update an existing QuoteItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateQuoteItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quoteItem = null;
        try {
            quoteItem = EntityQuery.use(delegator)
                    .from("QuoteItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        quoteItem.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(quoteItem);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Remove a QuoteItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeQuoteItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        List<GenericValue> quoteTermList = null;
        try {
            quoteTermList = EntityQuery.use(delegator)
                    .from("QuoteTerm")
                    .where(UtilMisc.toMap("quoteId", context.get("quoteId"), "quoteItemSeqId", context.get("quoteItemSeqId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteTerm: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(quoteTermList)) {
            // TODO: Convert <remove-list> element
        }
        GenericValue quoteItem = null;
        try {
            quoteItem = EntityQuery.use(delegator)
                    .from("QuoteItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert <remove-related> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(quoteItem);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Copy an existing QuoteItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String copyQuoteItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> quoteAdjustments = null;
        Map<String, Object> createQuoteAdjustmentInMap = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quoteItem = null;
        try {
            quoteItem = EntityQuery.use(delegator)
                    .from("QuoteItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> createQuoteItemInMap = new HashMap<>();
        // set-service-fields from "quoteItem" to "createQuoteItemInMap" for service "createQuoteItem"
        createQuoteItemInMap.putAll(UtilMisc.toMap(quoteItem));
        createQuoteItemInMap.put("quoteId", context.get("quoteIdTo"));
        createQuoteItemInMap.put("quoteItemSeqId", context.get("quoteItemSeqId"));
        if (UtilValidate.isEmpty(context.get("quoteIdTo"))) {
            if (UtilValidate.isEmpty(context.get("quoteItemSeqIdTo"))) {
                createQuoteItemInMap.remove("quoteItemSeqId");
            }
        }
        Object quoteIdTo = null;
        Object quoteItemSeqId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createQuoteItem", createQuoteItemInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            quoteIdTo = serviceResult.get("quoteId");
            quoteItemSeqId = serviceResult.get("quoteItemSeqId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createQuoteItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if ("Y".equals(context.get("copyQuoteAdjustments"))) {
            try {
                quoteAdjustments = quoteItem.getRelated("QuoteAdjustment", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related QuoteAdjustment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (quoteAdjustments != null) {
                for (GenericValue quoteAdjustment : quoteAdjustments) {
                    createQuoteAdjustmentInMap = null;
                    // set-service-fields from "quoteAdjustment" to "createQuoteAdjustmentInMap" for service "createQuoteAdjustment"
                    createQuoteAdjustmentInMap.putAll(UtilMisc.toMap(quoteAdjustment));
                    createQuoteAdjustmentInMap.put("quoteId", quoteIdTo);
                    createQuoteAdjustmentInMap.put("quoteItemSeqId", quoteItemSeqId);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createQuoteAdjustment", createQuoteAdjustmentInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createQuoteAdjustment: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Create a QuoteAttribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuoteAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("QuoteAttribute");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Update an existing QuoteAttribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateQuoteAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quoteAttribute = null;
        try {
            quoteAttribute = EntityQuery.use(delegator)
                    .from("QuoteAttribute")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteAttribute: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        quoteAttribute.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(quoteAttribute);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Remove an existing QuoteAttribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeQuoteAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quoteAttribute = null;
        try {
            quoteAttribute = EntityQuery.use(delegator)
                    .from("QuoteAttribute")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteAttribute: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(quoteAttribute);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Create a QuoteCoefficient
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuoteCoefficient(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("QuoteCoefficient");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Update an existing QuoteCoefficient
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateQuoteCoefficient(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quoteCoefficient = null;
        try {
            quoteCoefficient = EntityQuery.use(delegator)
                    .from("QuoteCoefficient")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteCoefficient: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        quoteCoefficient.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(quoteCoefficient);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Remove an existing QuoteCoefficient
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeQuoteCoefficient(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quoteCoefficient = null;
        try {
            quoteCoefficient = EntityQuery.use(delegator)
                    .from("QuoteCoefficient")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteCoefficient: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(quoteCoefficient);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Create a Quote
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuoteAndQuoteItemForRequest(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue newQuote = null;
        Object cqrContext = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue custRequest = null;
        try {
            custRequest = EntityQuery.use(delegator)
                    .from("CustRequest")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustRequest: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue custRequestItem = null;
        try {
            custRequestItem = EntityQuery.use(delegator)
                    .from("CustRequestItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustRequestItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(custRequest)) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderErrorCustRequestWithIdDoesntExist", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        newQuote = delegator.makeValue("Quote");
        GenericValue newQuoteItem = delegator.makeValue("QuoteItem");
        newQuote.setNonPKFields((Map<String, Object>) context);
        newQuoteItem.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newQuote).get("statusId"))) {
            newQuote.put("statusId", "QUO_CREATED");
        }
        newQuote.put("quoteTypeId", "PROPOSAL");
        String quoteId = delegator.getNextSeqId("Quote");
        newQuote.put("quoteId", quoteId);
        newQuoteItem.put("quoteId", quoteId);
        delegator.setNextSubSeqId(newQuoteItem, "quoteItemSeqId", 5, 1);
        Object quoteItemSeqId = newQuoteItem.get("quoteItemSeqId");
        result.put("quoteId", quoteId);
        result.put("quoteItemSeqId", ((Map<String, Object>) newQuoteItem).get("quoteItemSeqId"));
        newQuote.put("partyId", ((Map<String, Object>) custRequest).get("fromPartyId"));
        newQuote.put("quoteName", ((Map<String, Object>) custRequest).get("custRequestName"));
        newQuote.put("description", ((Map<String, Object>) custRequest).get("description"));
        newQuote.put("currencyUomId", ((Map<String, Object>) custRequest).get("maximumAmountUomId"));
        newQuote.put("productStoreId", ((Map<String, Object>) custRequest).get("productStoreId"));
        newQuote.put("salesChannelEnumId", ((Map<String, Object>) custRequest).get("salesChannelEnumId"));
        newQuoteItem.put("comments", ((Map<String, Object>) custRequestItem).get("story"));
        try {
            delegator.create(newQuote);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.create(newQuoteItem);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue lookupRequestRole = delegator.makeValue("CustRequestParty");
        lookupRequestRole.put("custRequestId", context.get("custRequestId"));
        // TODO: Convert <find-by-and> element
        if (context.get("roles") != null) {
            for (Object role : (List<Object>) context.get("roles")) {
                cqrContext = null;
                ((Map<String, Object>) cqrContext).put("quoteId", quoteId);
                ((Map<String, Object>) cqrContext).put("partyId", ((Map<String, Object>) role).get("partyId"));
                ((Map<String, Object>) cqrContext).put("roleTypeId", ((Map<String, Object>) role).get("roleTypeId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createQuoteRole", (Map<String, Object>) cqrContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createQuoteRole: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Auto update a QuoteItem price
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String autoUpdateQuotePrice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue quoteItem = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            quoteItem = EntityQuery.use(delegator)
                    .from("QuoteItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isNotEmpty(context.get("manualQuoteUnitPrice"))) {
            quoteItem.put("quoteUnitPrice", context.get("manualQuoteUnitPrice"));
        }
        if (UtilValidate.isEmpty(context.get("manualQuoteUnitPrice"))) {
            if (UtilValidate.isNotEmpty(context.get("defaultQuoteUnitPrice"))) {
                quoteItem.put("quoteUnitPrice", context.get("defaultQuoteUnitPrice"));
            }
        }
        try {
            delegator.store(quoteItem);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        request.setAttribute("quoteId", ((Map<String, Object>) quoteItem).get("quoteId"));

        return "success";
    }


    /**
     * Auto create QuoteAdjustments
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String autoCreateQuoteAdjustments(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue quoteItem = null;
        Map<String, Object> removeQuoteItemInMap = null;
        Map<String, Object> removeQuoteAdjustmentInMap = null;
        Object orderItemSeqId = null;
        Object newOrderItemSeqId = null;
        Object emptyField = null;
        Map<String, Object> createQuoteItemInMap = new HashMap<>();
        Object quoteItemSeqId = null;
        Map<String, Object> updateQuoteItemInMap = new HashMap<>();
        ShoppingCartItem cartItem = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quote = null;
        try {
            quote = EntityQuery.use(delegator)
                    .from("Quote")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Quote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> quoteItems = null;
        try {
            quoteItems = quote.getRelated("QuoteItem", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related QuoteItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (quoteItems != null) {
            for (GenericValue quoteItem_iter : quoteItems) {
                quoteItem = quoteItem_iter;
                if ("Y".equals(((Map<String, Object>) quoteItem).get("isPromo"))) {
                    removeQuoteItemInMap = null;
                    // set-service-fields from "parameters" to "removeQuoteItemInMap" for service "removeQuoteItem"
                    removeQuoteItemInMap.putAll(UtilMisc.toMap(context));
                    removeQuoteItemInMap.put("quoteItemSeqId", ((Map<String, Object>) quoteItem).get("quoteItemSeqId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("removeQuoteItem", removeQuoteItemInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling removeQuoteItem: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        List<GenericValue> quoteAdjustments = null;
        try {
            quoteAdjustments = quote.getRelated("QuoteAdjustment", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related QuoteAdjustment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (quoteAdjustments != null) {
            for (GenericValue quoteAdjustment : quoteAdjustments) {
                if (UtilValidate.isNotEmpty(((Map<String, Object>) quoteAdjustment).get("productPromoId"))) {
                    removeQuoteAdjustmentInMap = null;
                    // set-service-fields from "parameters" to "removeQuoteAdjustmentInMap" for service "removeQuoteAdjustment"
                    removeQuoteAdjustmentInMap.putAll(UtilMisc.toMap(context));
                    removeQuoteAdjustmentInMap.put("quoteAdjustmentId", ((Map<String, Object>) quoteAdjustment).get("quoteAdjustmentId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("removeQuoteAdjustment", removeQuoteAdjustmentInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling removeQuoteAdjustment: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        Map<String, Object> loadCartFromQuoteInMap = new HashMap<>();
        // set-service-fields from "parameters" to "loadCartFromQuoteInMap" for service "loadCartFromQuote"
        loadCartFromQuoteInMap.putAll(UtilMisc.toMap(context));
        loadCartFromQuoteInMap.put("applyQuoteAdjustments", "false");
        ShoppingCart cart = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("loadCartFromQuote", loadCartFromQuoteInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            cart = (ShoppingCart) serviceResult.get("shoppingCart");
        } catch (Exception e) {
            Debug.logError(e, "Error calling loadCartFromQuote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<ShoppingCartItem> items = cart != null ? cart.items() : null;
        if (items != null) {
            for (ShoppingCartItem item : items) {
                orderItemSeqId = item.getOrderItemSeqId();
                if (UtilValidate.isEmpty(orderItemSeqId)) {
                    createQuoteItemInMap = null;
                    // set-service-fields from "parameters" to "createQuoteItemInMap" for service "createQuoteItem"
                    createQuoteItemInMap.putAll(UtilMisc.toMap(context));
                    Object createQuoteItemInMap_productId = item.getProductId();
                    Object createQuoteItemInMap_quantity = item.getQuantity();
                    createQuoteItemInMap.put("isPromo", "Y");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createQuoteItem", createQuoteItemInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        quoteItemSeqId = serviceResult.get("quoteItemSeqId");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createQuoteItem: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    item.setOrderItemSeqId((String) quoteItemSeqId);
                    newOrderItemSeqId = item.getOrderItemSeqId();
                }
            }
        }
        try {
            quoteItems = quote.getRelated("QuoteItem", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related QuoteItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (quoteItems != null) {
            for (GenericValue quoteItem_iter : quoteItems) {
                quoteItem = quoteItem_iter;
                Object updateQuoteItemInMap_quoteId = null;
                Object updateQuoteItemInMap_quoteItemSeqId = null;
                if ((UtilValidate.isEmpty(((Map<String, Object>) quoteItem).get("quoteUnitPrice")) || "0".equals(((Map<String, Object>) quoteItem).get("quoteUnitPrice")))) {
                    cartItem = cart.findCartItem((String) quoteItem.get("quoteItemSeqId"));
                    updateQuoteItemInMap = new HashMap<>();
                    updateQuoteItemInMap.put("quoteId", quoteItem.get("quoteId"));
                    updateQuoteItemInMap.put("quoteItemSeqId", quoteItem.get("quoteItemSeqId"));
                    if (cartItem != null) { updateQuoteItemInMap.put("quoteUnitPrice", cartItem.getBasePrice()); }
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updateQuoteItem", updateQuoteItemInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updateQuoteItem: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        List<GenericValue> adjustments = cart != null ? cart.makeAllQuoteAdjustments() : null;
        if (adjustments != null) {
            for (GenericValue adjustment : adjustments) {
                ((Map<String, Object>) adjustment).put("quoteId", context.get("quoteId"));
                ((GenericValue) adjustment).put("quoteAdjustmentId", delegator.getNextSeqId("QuoteAdjustment"));
                try {
                    delegator.create((GenericValue) adjustment);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Create a QuoteAdjustment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuoteAdjustment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("QuoteAdjustment");
        newEntity.setNonPKFields((Map<String, Object>) context);
        String quoteAdjustmentId = delegator.getNextSeqId("QuoteAdjustment");
        Timestamp newEntity_createdDate = new Timestamp(System.currentTimeMillis());
        newEntity.put("quoteAdjustmentId", quoteAdjustmentId);
        newEntity.put("createdByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        result.put("quoteAdjustmentId", quoteAdjustmentId);
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
     * Remove an existing QuoteAdjustment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeQuoteAdjustment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quoteAdjustment = null;
        try {
            quoteAdjustment = EntityQuery.use(delegator)
                    .from("QuoteAdjustment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteAdjustment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(quoteAdjustment);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Update an existing QuoteAdjustment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateQuoteAdjustment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue quoteAdjustment = null;
        try {
            quoteAdjustment = EntityQuery.use(delegator)
                    .from("QuoteAdjustment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying QuoteAdjustment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        quoteAdjustment.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(quoteAdjustment);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Create a Quote from a Cart
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuoteFromCart(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> createQuoteInMap = new HashMap<>();
        Object emptyField = null;
        Object configId = null;
        Map<String, Object> createQuoteItemInMap = new HashMap<>();
        ProductConfigWrapper configWrapper = null;
        Object quoteItemSeqId = null;
        Object isPromo = null;
        List<GenericValue> adjustments = null;
        ShoppingCart cartObj = (ShoppingCart) context.get("cart");
        createQuoteInMap.put("partyId", cartObj != null ? cartObj.getPartyId() : null);
        if ((!(UtilValidate.isEmpty(((Map<String, Object>) createQuoteInMap).get("partyId"))) && !(java.util.Objects.equals(((Map<String, Object>) createQuoteInMap).get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) && !(true /* TODO: if-has-permission */))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderSecurityErrorToRunCreateQuoteFromCart", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        createQuoteInMap.put("currencyUomId", cartObj != null ? cartObj.getCurrency() : null);
        createQuoteInMap.put("salesChannelEnumId", cartObj != null ? cartObj.getChannelType() : null);
        Object orderType = cartObj != null ? cartObj.getOrderType() : null;
        if ("SALES_ORDER".equals(orderType)) {
            createQuoteInMap.put("productStoreId", cartObj != null ? cartObj.getProductStoreId() : null);
            createQuoteInMap.put("quoteTypeId", "PRODUCT_QUOTE");
        }
        if ("PURCHASE_ORDER".equals(orderType)) {
            createQuoteInMap.put("quoteTypeId", "PURCHASE_QUOTE");
        }
        createQuoteInMap.put("statusId", "QUO_CREATED");
        Object quoteId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createQuote", createQuoteInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            quoteId = serviceResult.get("quoteId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createQuote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue quote = null;
        try {
            quote = EntityQuery.use(delegator)
                    .from("Quote")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Quote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<ShoppingCartItem> items2 = cartObj != null ? cartObj.items() : null;
        if (items2 != null) {
            for (ShoppingCartItem item : items2) {
                createQuoteItemInMap = new HashMap<>();
                isPromo = item.getIsPromo();
                if (Boolean.TRUE.equals(isPromo)) {
                    createQuoteItemInMap.put("isPromo", "Y");
                }
                configWrapper = item.getConfigWrapper();
                if (UtilValidate.isNotEmpty(configWrapper)) {
                    configId = configWrapper.getConfigId();
                    createQuoteItemInMap.put("configId", configId);
                }
                if ((!"N".equals(context.get("applyStorePromotions")) || !"Y".equals(createQuoteItemInMap.get("isPromo")))) {
                    createQuoteItemInMap.put("quoteId", quote != null ? quote.get("quoteId") : null);
                    createQuoteItemInMap.put("productId", item.getProductId());
                    createQuoteItemInMap.put("quantity", item.getQuantity());
                    createQuoteItemInMap.put("selectedAmount", item.getSelectedAmount());
                    createQuoteItemInMap.put("quoteUnitPrice", item.getBasePrice());
                    createQuoteItemInMap.put("comments", item.getItemComment());
                    createQuoteItemInMap.put("reservStart", item.getReservStart());
                    createQuoteItemInMap.put("reservLength", item.getReservLength());
                    createQuoteItemInMap.put("reservPersons", item.getReservPersons());
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createQuoteItem", createQuoteItemInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        quoteItemSeqId = serviceResult.get("quoteItemSeqId");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createQuoteItem: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    item.setOrderItemSeqId((String) quoteItemSeqId);
                }
            }
        }
        Object adjustment_quoteId = null;
        if (!"N".equals(context.get("applyStorePromotions"))) {
            adjustments = cartObj != null ? cartObj.makeAllQuoteAdjustments() : null;
            if (adjustments != null) {
                for (GenericValue adjustment : adjustments) {
                    ((Map<String, Object>) adjustment).put("quoteId", ((Map<String, Object>) quote).get("quoteId"));
                    ((GenericValue) adjustment).put("quoteAdjustmentId", delegator.getNextSeqId("QuoteAdjustment"));
                    try {
                        delegator.create((GenericValue) adjustment);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        result.put("quoteId", ((Map<String, Object>) quote).get("quoteId"));

        return "success";
    }


    /**
     * Create a Quote from a Shopping List
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuoteFromShoppingList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object cart = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("loadCartFromShoppingList", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            cart = serviceResult.get("shoppingCart");
        } catch (Exception e) {
            Debug.logError(e, "Error calling loadCartFromShoppingList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createQuoteFromCartInMap = new HashMap<>();
        createQuoteFromCartInMap.put("cart", cart);
        createQuoteFromCartInMap.put("applyStorePromotions", context.get("applyStorePromotions"));
        Object quoteId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createQuoteFromCart", createQuoteFromCartInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            quoteId = serviceResult.get("quoteId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createQuoteFromCart: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("quoteId", quoteId);

        return "success";
    }


    /**
     * Create a Quote from a CustRequest
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuoteFromCustRequest(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> createQuoteInMap = null;
        Object createQuoteItemInMap = null;
        Object createQuoteRoleInMap = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue custRequest = null;
        try {
            custRequest = EntityQuery.use(delegator)
                    .from("CustRequest")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustRequest: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ((!"RF_QUOTE".equals(((Map<String, Object>) custRequest).get("custRequestTypeId")) && !"RF_PUR_QUOTE".equals(((Map<String, Object>) custRequest).get("custRequestTypeId")))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderQuoteNotARequest", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        createQuoteInMap.put("partyId", ((Map<String, Object>) custRequest).get("fromPartyId"));
        createQuoteInMap.put("productStoreId", ((Map<String, Object>) custRequest).get("productStoreId"));
        createQuoteInMap.put("salesChannelEnumId", ((Map<String, Object>) custRequest).get("salesChannelEnumId"));
        createQuoteInMap.put("quoteName", ((Map<String, Object>) custRequest).get("custRequestName"));
        createQuoteInMap.put("description", ((Map<String, Object>) custRequest).get("description"));
        createQuoteInMap.put("currencyUomId", ((Map<String, Object>) custRequest).get("maximumAmountUomId"));
        createQuoteInMap.put("statusId", "QUO_CREATED");
        createQuoteInMap.put("quoteTypeId", context.get("quoteTypeId"));
        if (UtilValidate.isEmpty(((Map<String, Object>) createQuoteInMap).get("quoteTypeId"))) {
            if ("RF_QUOTE".equals(((Map<String, Object>) custRequest).get("custRequestTypeId"))) {
                createQuoteInMap.put("quoteTypeId", "PRODUCT_QUOTE");
            } else {
                createQuoteInMap.put("quoteTypeId", "PURCHASE_QUOTE");
            }
        }
        Object quoteId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createQuote", createQuoteInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            quoteId = serviceResult.get("quoteId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createQuote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        List<GenericValue> custRequestItems = null;
        try {
            custRequestItems = EntityQuery.use(delegator)
                    .from("CustRequestItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustRequestItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (custRequestItems != null) {
            for (GenericValue custRequestItem : custRequestItems) {
                createQuoteItemInMap = null;
                ((Map<String, Object>) createQuoteItemInMap).put("quoteId", quoteId);
                ((Map<String, Object>) createQuoteItemInMap).put("custRequestId", ((Map<String, Object>) custRequestItem).get("custRequestId"));
                ((Map<String, Object>) createQuoteItemInMap).put("custRequestItemSeqId", ((Map<String, Object>) custRequestItem).get("custRequestItemSeqId"));
                ((Map<String, Object>) createQuoteItemInMap).put("productId", ((Map<String, Object>) custRequestItem).get("productId"));
                ((Map<String, Object>) createQuoteItemInMap).put("quantity", ((Map<String, Object>) custRequestItem).get("quantity"));
                ((Map<String, Object>) createQuoteItemInMap).put("selectedAmount", ((Map<String, Object>) custRequestItem).get("selectedAmount"));
                ((Map<String, Object>) createQuoteItemInMap).put("reservStart", ((Map<String, Object>) custRequestItem).get("reservStart"));
                ((Map<String, Object>) createQuoteItemInMap).put("reservLength", ((Map<String, Object>) custRequestItem).get("reservLength"));
                ((Map<String, Object>) createQuoteItemInMap).put("reservPersons", ((Map<String, Object>) custRequestItem).get("reservPersons"));
                ((Map<String, Object>) createQuoteItemInMap).put("configId", ((Map<String, Object>) custRequestItem).get("configId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createQuoteItem", (Map<String, Object>) createQuoteItemInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createQuoteItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        List<GenericValue> custRequestParties = null;
        try {
            custRequestParties = custRequest.getRelated("CustRequestParty", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related CustRequestParty: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (custRequestParties != null) {
            for (GenericValue custRequestParty : custRequestParties) {
                createQuoteRoleInMap = null;
                if (!"REQ_TAKER".equals(((Map<String, Object>) custRequestParty).get("roleTypeId"))) {
                    ((Map<String, Object>) createQuoteRoleInMap).put("quoteId", quoteId);
                    ((Map<String, Object>) createQuoteRoleInMap).put("partyId", ((Map<String, Object>) custRequestParty).get("partyId"));
                    ((Map<String, Object>) createQuoteRoleInMap).put("roleTypeId", ((Map<String, Object>) custRequestParty).get("roleTypeId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createQuoteRole", (Map<String, Object>) createQuoteRoleInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createQuoteRole: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        result.put("quoteId", quoteId);

        return "success";
    }


    /**
     * Create a QuoteWorkEffort
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuoteWorkEffort(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue lookupMap = delegator.makeValue("QuoteWorkEffort");
        lookupMap.setPKFields((Map<String, Object>) context);
        GenericValue quoteWorkEffort = null;
        try {
            quoteWorkEffort = EntityQuery.use(delegator)
                    .from("QuoteWorkEffort")
                    .where(lookupMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key QuoteWorkEffort: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) quoteWorkEffort).get("quoteId"))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderQuoteNotAWorkEffort", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        lookupMap.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(lookupMap);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("quoteId", ((Map<String, Object>) lookupMap).get("quoteId"));
        result.put("workEffortId", ((Map<String, Object>) lookupMap).get("workEffortId"));

        return "success";
    }


    /**
     * Delete a QuoteWorkEffort
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteQuoteWorkEffort(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue lookupMap = delegator.makeValue("QuoteWorkEffort");
        lookupMap.setPKFields((Map<String, Object>) context);
        GenericValue quoteWorkEffort = null;
        try {
            quoteWorkEffort = EntityQuery.use(delegator)
                    .from("QuoteWorkEffort")
                    .where(lookupMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key QuoteWorkEffort: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) quoteWorkEffort).get("quoteId"))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderQuoteWorkEffortDoesNotExists", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) quoteWorkEffort).get("quoteId"))) {
            try {
                delegator.removeValue(quoteWorkEffort);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create a QuoteTerm
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuoteTerm(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("QuoteTerm");
        if (UtilValidate.isEmpty(context.get("quoteItemSeqId"))) {
            context.put("quoteItemSeqId", "_NA_");
        }
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
     * Update a QuoteTerm
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateQuoteTerm(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("QuoteTerm");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete a QuoteTerm
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteQuoteTerm(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("QuoteTerm");
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.removeValue(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create a new Note associated with a Quote
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createQuoteNote(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Map<String, Object> createNoteMap = new HashMap<>();
        // set-service-fields from "parameters" to "createNoteMap" for service "createNote"
        createNoteMap.putAll(UtilMisc.toMap(context));
        createNoteMap.put("note", context.get("noteInfo"));
        Object noteId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createNote", createNoteMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            noteId = serviceResult.get("noteId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createNote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(noteId)) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderProblemCreatingTheNoteNoNoteIdReturned", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newQuoteNote = delegator.makeValue("QuoteNote");
        newQuoteNote.put("quoteId", context.get("quoteId"));
        newQuoteNote.put("noteId", noteId);
        try {
            delegator.create(newQuoteNote);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }

}

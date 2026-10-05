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
 * <p>Generated from: component://order/script/org/ofbiz/order/order/OrderReturnServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OrderReturnServices {

    private static final String MODULE = OrderReturnServices.class.getName();


    /**
     * Create a ReturnHeader
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createReturnHeader(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue partyRole = null;
        GenericValue destinationFacility = null;
        Map<String, Object> getNextInvoiceIdMap = null;
        GenericValue newEntity = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        if ((!(true /* TODO: if-has-permission */) && !(java.util.Objects.equals(((Map<String, Object>) userLogin).get("partyId"), context.get("fromPartyId"))))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderSecurityErrorToRunCreateReturnHeader", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        Object returnHeaderTypeId = context.get("returnHeaderTypeId");
        if (UtilValidate.isEmpty(context.get("toPartyId"))) {
            if ("CUSTOMER_RETURN".equals(returnHeaderTypeId)) {
                if (UtilValidate.isNotEmpty(context.get("destinationFacilityId"))) {
                    try {
                        destinationFacility = EntityQuery.use(delegator)
                                .from("Facility")
                                .where(UtilMisc.toMap("facilityId", context.get("destinationFacilityId")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying Facility: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    context.put("toPartyId", ((Map<String, Object>) destinationFacility).get("ownerPartyId"));
                }
            }
        } else {
            if (returnHeaderTypeId != null /* TODO: operator contains */) {
                try {
                    partyRole = EntityQuery.use(delegator)
                            .from("PartyRole")
                            .where(UtilMisc.toMap("partyId", context.get("toPartyId"), "roleTypeId", "INTERNAL_ORGANIZATIO"))
                            .cache()
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PartyRole: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(partyRole)) {
                    {
                        String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderReturnRequestPartyRoleInternalOrg", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
            } else {
                try {
                    partyRole = EntityQuery.use(delegator)
                            .from("PartyRole")
                            .where(UtilMisc.toMap("partyId", context.get("toPartyId"), "roleTypeId", "SUPPLIER"))
                            .cache()
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PartyRole: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(partyRole)) {
                    {
                        String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderReturnRequestPartyRoleSupplier", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("needsInventoryReceive"))) {
            context.put("needsInventoryReceive", "N");
        }
        newEntity = delegator.makeValue("ReturnHeader");
        newEntity.setNonPKFields((Map<String, Object>) context);
        Map<String, Object> partyAccountingPreferencesCallMap = new HashMap<>();
        partyAccountingPreferencesCallMap.put("organizationPartyId", context.get("toPartyId"));
        Map<String, Object> systemMap = new HashMap<>();
        systemMap.put("userLoginId", "system");
        GenericValue systemLogin = null;
        try {
            systemLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(systemMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        partyAccountingPreferencesCallMap.put("userLogin", systemLogin);
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
        if ("Y".equals(((Map<String, Object>) partyAcctgPreference).get("useInvoiceIdForReturns"))) {
            getNextInvoiceIdMap.put("partyId", context.get("toPartyId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getNextInvoiceId", getNextInvoiceIdMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                newEntity.put("returnId", serviceResult.get("invoiceId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling getNextInvoiceId: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            ((GenericValue) newEntity).put("returnId", delegator.getNextSeqId("ReturnHeader"));
        }
        result.put("returnId", ((Map<String, Object>) newEntity).get("returnId"));
        Object newEntity_statusId = null;
        Object newEntity_entryDate = null;
        if (!(true /* TODO: if-has-permission */)) {
            if ("CUSTOMER_RETURN".equals(returnHeaderTypeId)) {
                newEntity.put("statusId", "RETURN_REQUESTED");
            } else {
                newEntity.put("statusId", "SUP_RETURN_REQUESTED");
            }
            newEntity.put("entryDate", nowTimestamp);
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("entryDate"))) {
            newEntity.put("entryDate", nowTimestamp);
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("statusId"))) {
            if ("CUSTOMER_RETURN".equals(returnHeaderTypeId)) {
                newEntity.put("statusId", "RETURN_REQUESTED");
            } else {
                newEntity.put("statusId", "SUP_RETURN_REQUESTED");
            }
        }
        newEntity.put("createdBy", ((Map<String, Object>) userLogin).get("userLoginId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object successMsg = "Return Request #" + ((Map<String, Object>) newEntity).get("returnId") + " was created successfully.";
        result.put("successMessage", successMsg);

        return "success";
    }


    /**
     * Update a ReturnHeader
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateReturnHeader(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> returnItems = null;
        Object returnTotalAmount = null;
        Map<String, Object> returnTotalCtx = null;
        Object statusId = null;
        Object returnTotal = null;
        Object statusIdTo = null;
        GenericValue statusValidChange = null;
        Object availableReturnTotal = null;
        Object orderTotal = null;
        List<GenericValue> returnAdjustments = null;
        GenericValue returnHeader = null;
        try {
            returnHeader = EntityQuery.use(delegator)
                    .from("ReturnHeader")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("RETURN_ACCEPTED".equals(context.get("statusId"))) {
            try {
                returnItems = EntityQuery.use(delegator)
                        .from("ReturnItem")
                        .distinct()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ReturnItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            returnTotalAmount = 0.0;
            if (returnItems != null) {
                for (GenericValue returnItem : returnItems) {
                    if ((UtilValidate.isEmpty(((Map<String, Object>) returnHeader).get("paymentMethodId")) && UtilValidate.isEmpty(context.get("paymentMethodId")) && ("RTN_CSREPLACE".equals(((Map<String, Object>) returnItem).get("returnTypeId")) || "RTN_REPAIR_REPLACE".equals(((Map<String, Object>) returnItem).get("returnTypeId"))))) {
                        {
                            String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderReturnPaymentMethodNeededForThisTypeOfReturn", locale);
                            error_list.add(errorMsg);
                            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                        }
                    }
                    if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                            UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                        return "error";
                    }
                    returnTotalAmount = (new BigDecimal(((Map<String, Object>) returnItem).get("returnPrice").toString())).multiply(new BigDecimal(((Map<String, Object>) returnItem).get("returnQuantity").toString()));
                    if (UtilValidate.isNotEmpty(((Map<String, Object>) returnItem).get("orderId"))) {
                        returnTotalCtx.put("orderId", ((Map<String, Object>) returnItem).get("orderId"));
                        ((Map<String, Object>) returnTotalCtx).put("adjustment", new BigDecimal("0.0"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("getOrderAvailableReturnedTotal", returnTotalCtx);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                            availableReturnTotal = serviceResult.get("availableReturnTotal");
                            returnTotal = serviceResult.get("returnTotal");
                            orderTotal = serviceResult.get("orderTotal");
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling getOrderAvailableReturnedTotal: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        Debug.logInfo("Available amount for return on order #" + ((Map<String, Object>) returnItem).get("orderId") + " is [" + availableReturnTotal + "] (orderTotal = [" + orderTotal + "] - returnTotal = [" + returnTotal + "]", MODULE);
                        if (((Comparable) availableReturnTotal).compareTo(new BigDecimal("-0.01")) < 0) {
                            {
                                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderReturnPriceCannotExceedTheOrderTotal", locale);
                                error_list.add(errorMsg);
                                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                            }
                        }
                        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                            return "error";
                        }
                    } else {
                        Debug.logInfo("Not an order based returnItem; unable to check valid amounts!", MODULE);
                    }
                }
            }
            if ((!(UtilValidate.isEmpty(context.get("statusId"))) && !java.util.Objects.equals(context.get("statusId"), ((Map<String, Object>) returnHeader).get("statusId")))) {
                statusIdTo = context.get("statusId");
                statusId = ((Map<String, Object>) returnHeader).get("statusId");
                try {
                    statusValidChange = EntityQuery.use(delegator)
                            .from("StatusValidChange")
                            .where(UtilMisc.toMap())
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(statusValidChange)) {
                    {
                        String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderErrorReturnHeaderItemStatusNotChangedIsNotAValidChange", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            try {
                returnAdjustments = EntityQuery.use(delegator)
                        .from("ReturnAdjustment")
                        .where(UtilMisc.toMap("returnId", ((Map<String, Object>) returnHeader).get("returnId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ReturnAdjustment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (returnAdjustments != null) {
                for (GenericValue returnAdjustment : returnAdjustments) {
                    returnTotalAmount = new BigDecimal(((Map<String, Object>) returnAdjustment).get("amount").toString());
                }
            }
            if (((Comparable) returnTotalAmount).compareTo(BigDecimal.ZERO) < 0) {
                {
                    String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderReturnTotalCannotLessThanZero", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        result.put("oldStatusId", ((Map<String, Object>) returnHeader).get("statusId"));
        returnHeader.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(returnHeader);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create Return Item
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createReturnItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue orderItem = null;
        GenericValue itemLookup = null;
        Object returnableQuantity = null;
        Object returnablePrice = null;
        Map<String, Object> serviceContext = null;
        Object orderItemSeqId = null;
        BigDecimal roundedValue = null;
        Object orderId = null;
        Object returnAdjCtx = null;
        List<GenericValue> orderAdjustments = null;
        Map<String, Object> lookupPKMap = new HashMap<>();
        lookupPKMap.put("returnId", context.get("returnId"));
        GenericValue returnHeader = null;
        try {
            returnHeader = EntityQuery.use(delegator)
                    .from("ReturnHeader")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ReturnHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ((!(true /* TODO: if-has-permission */) && !(java.util.Objects.equals(((Map<String, Object>) userLogin).get("partyId"), ((Map<String, Object>) returnHeader).get("fromPartyId"))))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderSecurityErrorToRunCreateReturnItem", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("returnItemTypeId"))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderReturnItemTypeIsNotDefined", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        if ((UtilValidate.isEmpty(((Map<String, Object>) returnHeader).get("paymentMethodId")) && "RETURN_ACCEPTED".equals(((Map<String, Object>) returnHeader).get("statusId")) && ("RTN_CSREPLACE".equals(context.get("returnTypeId")) || "RTN_REPAIR_REPLACE".equals(context.get("returnTypeId"))))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderReturnPaymentMethodNeededForThisTypeOfReturn", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if ("0".equals(context.get("returnQuantity"))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderNoReturnQuantityAvailablePreviousReturnsMayExist", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        returnableQuantity = BigDecimal.ZERO;
        returnablePrice = BigDecimal.ZERO;
        if (UtilValidate.isNotEmpty(context.get("orderItemSeqId"))) {
            itemLookup = delegator.makeValue("OrderItem");
            itemLookup.setPKFields((Map<String, Object>) context);
            if (UtilValidate.isNotEmpty(context.get("orderItemSeqId"))) {
                try {
                    orderItem = EntityQuery.use(delegator)
                            .from("OrderItem")
                            .where(itemLookup)
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error finding by primary key OrderItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                Debug.logInfo("Return item is an OrderItem - " + ((Map<String, Object>) orderItem).get("orderItemSeqId"), MODULE);
            }
        }
        if (UtilValidate.isNotEmpty(orderItem)) {
            serviceContext.put("orderItem", orderItem);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getReturnableQuantity", serviceContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                returnableQuantity = serviceResult.get("returnableQuantity");
                returnablePrice = serviceResult.get("returnablePrice");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getReturnableQuantity: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (((Comparable) returnableQuantity).compareTo(BigDecimal.ZERO) > 0) {
            Object parameters_returnPrice = null;
            if (!(true /* TODO: if-has-permission */)) {
                context.put("returnPrice", returnablePrice);
            }
            roundedValue = (new BigDecimal(returnablePrice.toString())).setScale(2, RoundingMode.HALF_UP);
            returnablePrice = roundedValue;
            if (context.get("returnQuantity") != null /* TODO: field compare operator greater */) {
                {
                    String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderRequestedReturnQuantityNotAvailablePreviousReturnsMayExist", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isNotEmpty(orderItem)) {
                if (context.get("returnQuantity") != null /* TODO: field compare operator greater */) {
                    {
                        String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderReturnQuantityCannotExceedTheOrderedQuantity", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
            }
            if (context.get("returnPrice") != null /* TODO: field compare operator greater */) {
                {
                    String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderReturnPriceCannotExceedThePurchasePrice", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        } else {
            orderId = context.get("orderId");
            orderItemSeqId = context.get("orderItemSeqId");
            Debug.logError("Order " + orderId + " item " + orderItemSeqId + " has been returned in full", MODULE);
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderIllegalReturnItemTypePassed", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        GenericValue newEntity = delegator.makeValue("ReturnItem");
        newEntity.put("returnId", context.get("returnId"));
        delegator.setNextSubSeqId(newEntity, "returnItemSeqId", 5, 1);
        Object returnItemSeqId = newEntity.get("returnItemSeqId");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.put("statusId", "RETURN_REQUESTED");
        result.put("returnItemSeqId", ((Map<String, Object>) newEntity).get("returnItemSeqId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert <refresh-value> element
        Object returnAdjCtx_returnId = null;
        Object returnAdjCtx_returnItemSeqId = null;
        Object returnAdjCtx_returnTypeId = null;
        Object returnAdjCtx_orderAdjustmentId = null;
        if ((UtilValidate.isEmpty(context.get("includeAdjustments")) || "Y".equals(context.get("includeAdjustments")))) {
            if (UtilValidate.isNotEmpty(orderItem)) {
                try {
                    orderAdjustments = orderItem.getRelated("OrderAdjustment", null, null, false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related OrderAdjustment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (orderAdjustments != null) {
                    for (GenericValue orderAdjustment : orderAdjustments) {
                        returnAdjCtx = null;
                        ((Map<String, Object>) returnAdjCtx).put("returnId", context.get("returnId"));
                        ((Map<String, Object>) returnAdjCtx).put("returnItemSeqId", ((Map<String, Object>) newEntity).get("returnItemSeqId"));
                        ((Map<String, Object>) returnAdjCtx).put("returnTypeId", ((Map<String, Object>) newEntity).get("returnTypeId"));
                        ((Map<String, Object>) returnAdjCtx).put("orderAdjustmentId", ((Map<String, Object>) orderAdjustment).get("orderAdjustmentId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createReturnAdjustment", (Map<String, Object>) returnAdjCtx);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createReturnAdjustment: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Update Return Item
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateReturnItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> ctx = null;
        Map<String, Object> lookupPKMap = new HashMap<>();
        lookupPKMap.put("returnId", context.get("returnId"));
        lookupPKMap.put("returnItemSeqId", context.get("returnItemSeqId"));
        GenericValue returnItem = null;
        try {
            returnItem = EntityQuery.use(delegator)
                    .from("ReturnItem")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ReturnItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object originalReturnPrice = ((Map<String, Object>) returnItem).get("returnPrice");
        Object originalReturnQuantity = ((Map<String, Object>) returnItem).get("returnQuantity");
        result.put("oldStatusId", ((Map<String, Object>) returnItem).get("statusId"));
        returnItem.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(returnItem);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert <refresh-value> element
        List<GenericValue> returnAdjustments = null;
        try {
            returnAdjustments = EntityQuery.use(delegator)
                    .from("ReturnAdjustment")
                    .where(UtilMisc.toMap("returnId", ((Map<String, Object>) returnItem).get("returnId"), "returnItemSeqId", ((Map<String, Object>) returnItem).get("returnItemSeqId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnAdjustment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (returnAdjustments != null) {
            for (GenericValue returnAdjustment : returnAdjustments) {
                Debug.logInfo("updating returnAdjustment with Id:[" + ((Map<String, Object>) returnAdjustment).get("returnAdjustmentId") + "]", MODULE);
                // set-service-fields from "returnAdjustment" to "ctx" for service "updateReturnAdjustment"
                ctx.putAll(UtilMisc.toMap(returnAdjustment));
                ctx.put("originalReturnPrice", originalReturnPrice);
                ctx.put("originalReturnQuantity", originalReturnQuantity);
                ctx.put("returnTypeId", ((Map<String, Object>) returnItem).get("returnTypeId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateReturnAdjustment", ctx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateReturnAdjustment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Update Return Items Status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateReturnItemsStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> serviceInMap = null;
        Map<String, Object> lookupPKMap = new HashMap<>();
        lookupPKMap.put("returnId", context.get("returnId"));
        // TODO: Convert <find-by-and> element
        if (context.get("returnItems") != null) {
            for (Object item : (List<Object>) context.get("returnItems")) {
                ((Map<String, Object>) item).put("statusId", context.get("statusId"));
                // set-service-fields from "item" to "serviceInMap" for service "updateReturnItem"
                serviceInMap.putAll(UtilMisc.toMap(item));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateReturnItem", serviceInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateReturnItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                serviceInMap = null;
                item = null;
            }
        }

        return "success";
    }


    /**
     * Remove Return Item
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeReturnItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Map<String, Object> removeCtx = null;
        GenericValue returnHeader = null;
        try {
            returnHeader = EntityQuery.use(delegator)
                    .from("ReturnHeader")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("CUSTOMER_RETURN".equals(((Map<String, Object>) returnHeader).get("returnHeaderTypeId"))) {
            if (!"RETURN_REQUESTED".equals(((Map<String, Object>) returnHeader).get("statusId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderCannotRemoveItemsOnceReturnIsApproved", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        } else {
            if (!"SUP_RETURN_REQUESTED".equals(((Map<String, Object>) returnHeader).get("statusId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderCannotRemoveItemsOnceReturnIsApproved", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> lookupPKMap = new HashMap<>();
        lookupPKMap.put("returnId", context.get("returnId"));
        lookupPKMap.put("returnItemSeqId", context.get("returnItemSeqId"));
        GenericValue returnItem = null;
        try {
            returnItem = EntityQuery.use(delegator)
                    .from("ReturnItem")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ReturnItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> returnAdjustments = null;
        try {
            returnAdjustments = EntityQuery.use(delegator)
                    .from("ReturnAdjustment")
                    .where(UtilMisc.toMap("returnItemSeqId", ((Map<String, Object>) returnItem).get("returnItemSeqId"), "returnId", ((Map<String, Object>) returnItem).get("returnId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnAdjustment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (returnAdjustments != null) {
            for (GenericValue returnAdjustment : returnAdjustments) {
                removeCtx.put("returnAdjustmentId", ((Map<String, Object>) returnAdjustment).get("returnAdjustmentId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("removeReturnAdjustment", removeCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling removeReturnAdjustment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        try {
            delegator.removeValue(returnItem);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create Return Adjustment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createReturnAdjustment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        if (!(true /* TODO: if-has-permission */)) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderErrorCreatePermissionError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("ReturnAdjustment");
        newEntity.setNonPKFields((Map<String, Object>) context);
        ((GenericValue) newEntity).put("returnAdjustmentId", delegator.getNextSeqId("ReturnAdjustment"));
        result.put("returnAdjustmentId", ((Map<String, Object>) newEntity).get("returnAdjustmentId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object successMsg = "Return Adjustment #" + ((Map<String, Object>) newEntity).get("returnAdjustmentId") + " was created successfully.";
        result.put("successMessage", successMsg);

        return "success";
    }


    /**
     * Remove Return Adjustment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeReturnAdjustment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> lookupPKMap = new HashMap<>();
        lookupPKMap.put("returnAdjustmentId", context.get("returnAdjustmentId"));
        GenericValue returnAdjustment = null;
        try {
            returnAdjustment = EntityQuery.use(delegator)
                    .from("ReturnAdjustment")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ReturnAdjustment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(returnAdjustment)) {
            try {
                delegator.removeValue(returnAdjustment);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Update Return Status From ShipmentReceipt
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateReturnStatusFromReceipt(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> totalsMap = null;
        GenericValue receipt = null;
        GenericValue item = null;
        Map<String, Object> filterMap = null;
        Map<String, Object> serviceInMap = null;
        Object allReceived = null;
        GenericValue shipment = null;
        Map<String, Object> serviceInput = null;
        Map<String, Object> returnHeaderCtx = null;
        Map<String, Object> lookupPKMap = new HashMap<>();
        lookupPKMap.put("returnId", context.get("returnId"));
        GenericValue returnHeader = null;
        try {
            returnHeader = EntityQuery.use(delegator)
                    .from("ReturnHeader")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ReturnHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert <find-by-and> element
        if (context.get("shipmentReceipts") != null) {
            for (Object receipt_iter : (List<Object>) context.get("shipmentReceipts")) {
                receipt = (GenericValue) receipt_iter;
                if (UtilValidate.isEmpty(((Map<String, Object>) totalsMap).get(((Map<String, Object>) receipt).get("returnItemSeqId")))) {
                    totalsMap.put((String) ((Map<String, Object>) receipt).get("returnItemSeqId"), BigDecimal.ZERO);
                }
                totalsMap.put((String) ((Map<String, Object>) receipt).get("returnItemSeqId"), null);
            }
        }
        List<GenericValue> returnItems = null;
        try {
            returnItems = returnHeader.getRelated("ReturnItem", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related ReturnItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) totalsMap).entrySet()) {
            String returnItemSeqId = entry.getKey();
            Object value = entry.getValue();
            filterMap.put("returnItemSeqId", null);
            // TODO: Convert <filter-list-by-and> element
            item = EntityUtil.getFirst((List<GenericValue>) context.get("items"));
            item.put("receivedQuantity", null);
            // set-service-fields from "item" to "serviceInMap" for service "updateReturnItem"
            serviceInMap.putAll(UtilMisc.toMap(item));
            if (value != null /* TODO: field compare operator greater-equals */) {
                serviceInMap.put("statusId", "RETURN_RECEIVED");
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateReturnItem", serviceInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateReturnItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            serviceInMap = null;
            filterMap = null;
        }
        allReceived = "true";
        // TODO: Convert <find-by-and> element
        if (context.get("allReturnItems") != null) {
            for (Object item_iter : (List<Object>) context.get("allReturnItems")) {
                item = (GenericValue) item_iter;
                if (!"RETURN_RECEIVED".equals(((Map<String, Object>) item).get("statusId"))) {
                    if (UtilValidate.isNotEmpty(((Map<String, Object>) item).get("orderItemSeqId"))) {
                        allReceived = "false";
                    }
                }
            }
        }
        if ("true".equals(allReceived)) {
            if (context.get("shipmentReceipts") != null) {
                for (Object receipt_iter : (List<Object>) context.get("shipmentReceipts")) {
                    receipt = (GenericValue) receipt_iter;
                    try {
                        shipment = receipt.getRelatedOne("Shipment", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one Shipment: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isNotEmpty(((Map<String, Object>) shipment).get("shipmentId"))) {
                        if (!"RETURN_RECEIVED".equals(((Map<String, Object>) shipment).get("statusId"))) {
                            serviceInput.put("shipmentId", ((Map<String, Object>) shipment).get("shipmentId"));
                            serviceInput.put("statusId", "PURCH_SHIP_RECEIVED");
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("updateShipment", serviceInput);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling updateShipment: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                    }
                }
            }
            returnHeaderCtx.put("statusId", "RETURN_RECEIVED");
            returnHeaderCtx.put("returnId", ((Map<String, Object>) returnHeader).get("returnId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateReturnHeader", returnHeaderCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateReturnHeader: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("returnHeaderStatus", ((Map<String, Object>) returnHeader).get("statusId"));

        return "success";
    }


    /**
     * Create a ReturnItemResponse
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createReturnItemResponse(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("ReturnItemResponse");
        ((GenericValue) newEntity).put("returnItemResponseId", delegator.getNextSeqId("ReturnItemResponse"));
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("returnItemResponseId", ((Map<String, Object>) newEntity).get("returnItemResponseId"));

        return "success";
    }


    /**
     * Create Quick Return From Order
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String quickReturnFromOrder(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object roleTypeId = null;
        Map<String, Object> createHeaderCtx = null;
        Map<String, Object> newItemCtx = null;
        GenericValue product = null;
        Map<String, Object> itemCheckMap = null;
        GenericValue returnItemTypeMapping = null;
        Object orderItemTypeId = null;
        Long returnCount = null;
        Object returnAdjCtx = null;
        Map<String, Object> balanceItemCtx = null;
        Map<String, Object> updateHeaderCtx = null;
        Map<String, Object> receiveCtx = null;
        if ((!(true /* TODO: if-has-permission */) && !(java.util.Objects.equals(((Map<String, Object>) userLogin).get("partyId"), context.get("fromPartyId"))))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderSecurityErrorToRunQuickReturnFromOrder", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue orderHeader = null;
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(UtilMisc.toMap("orderId", context.get("orderId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object returnHeaderTypeId = context.get("returnHeaderTypeId");
        if ("CUSTOMER_RETURN".equals(returnHeaderTypeId)) {
            roleTypeId = "BILL_TO_CUSTOMER";
        } else {
            roleTypeId = "BILL_FROM_VENDOR";
        }
        List<GenericValue> orderRoles = null;
        try {
            orderRoles = EntityQuery.use(delegator)
                    .from("OrderRole")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue orderRole = EntityUtil.getFirst((List<GenericValue>) orderRoles);
        createHeaderCtx.put("destinationFacilityId", ((Map<String, Object>) orderHeader).get("originFacilityId"));
        updateHeaderCtx.put("needsInventoryReceive", "Y");
        createHeaderCtx.put("returnHeaderTypeId", returnHeaderTypeId);
        GenericValue productStore = null;
        try {
            productStore = orderHeader.getRelatedOne("ProductStore", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one ProductStore: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("CUSTOMER_RETURN".equals(returnHeaderTypeId)) {
            createHeaderCtx.put("fromPartyId", ((Map<String, Object>) orderRole).get("partyId"));
            createHeaderCtx.put("toPartyId", ((Map<String, Object>) productStore).get("payToPartyId"));
            if (UtilValidate.isEmpty(((Map<String, Object>) createHeaderCtx).get("destinationFacilityId"))) {
                createHeaderCtx.put("destinationFacilityId", ((Map<String, Object>) productStore).get("inventoryFacilityId"));
            }
        } else {
            createHeaderCtx.put("fromPartyId", ((Map<String, Object>) productStore).get("payToPartyId"));
            createHeaderCtx.put("toPartyId", ((Map<String, Object>) orderRole).get("partyId"));
        }
        createHeaderCtx.put("currencyUomId", ((Map<String, Object>) orderHeader).get("currencyUom"));
        Object returnId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createReturnHeader", createHeaderCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            returnId = serviceResult.get("returnId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createReturnHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> orderItems = null;
        try {
            orderItems = EntityQuery.use(delegator)
                    .from("OrderItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("returnReasonId"))) {
            context.put("returnReasonId", "RTN_NOT_WANT");
        }
        if (UtilValidate.isEmpty(context.get("returnTypeId"))) {
            context.put("returnTypeId", "RTN_REFUND");
        }
        if (orderItems != null) {
            for (GenericValue orderItem : orderItems) {
                newItemCtx.put("returnId", returnId);
                newItemCtx.put("returnReasonId", context.get("returnReasonId"));
                newItemCtx.put("returnTypeId", context.get("returnTypeId"));
                if (UtilValidate.isNotEmpty(((Map<String, Object>) orderItem).get("productId"))) {
                    newItemCtx.put("productId", ((Map<String, Object>) orderItem).get("productId"));
                }
                newItemCtx.put("orderId", ((Map<String, Object>) orderItem).get("orderId"));
                newItemCtx.put("orderItemSeqId", ((Map<String, Object>) orderItem).get("orderItemSeqId"));
                newItemCtx.put("description", ((Map<String, Object>) orderItem).get("itemDescription"));
                itemCheckMap.put("orderItem", orderItem);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("getReturnableQuantity", itemCheckMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    newItemCtx.put("returnQuantity", serviceResult.get("returnableQuantity"));
                    newItemCtx.put("returnPrice", serviceResult.get("returnablePrice"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling getReturnableQuantity: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                orderItemTypeId = ((Map<String, Object>) orderItem).get("orderItemTypeId");
                if ("PRODUCT_ORDER_ITEM".equals(orderItemTypeId)) {
                    try {
                        product = EntityQuery.use(delegator)
                                .from("Product")
                                .where(UtilMisc.toMap("productId", ((Map<String, Object>) orderItem).get("productId")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    try {
                        returnItemTypeMapping = EntityQuery.use(delegator)
                                .from("ReturnItemTypeMap")
                                .where(UtilMisc.toMap("returnItemMapKey", ((Map<String, Object>) product).get("productTypeId"), "returnHeaderTypeId", returnHeaderTypeId))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ReturnItemTypeMap: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                } else {
                    Debug.logWarning("Trying to find returnItemtype from ReturnItemTypeMap with orderItemtypeId [" + ((Map<String, Object>) orderItem).get("orderItemTypeId") + "] for order item [" + orderItem + "]", MODULE);
                    try {
                        returnItemTypeMapping = EntityQuery.use(delegator)
                                .from("ReturnItemTypeMap")
                                .where(UtilMisc.toMap("returnItemMapKey", orderItemTypeId, "returnHeaderTypeId", returnHeaderTypeId))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ReturnItemTypeMap: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                if (UtilValidate.isEmpty(((Map<String, Object>) returnItemTypeMapping).get("returnItemTypeId"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderReturnItemTypeOrderItemNoMatching", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                    if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                            UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                        return "error";
                    }
                } else {
                    newItemCtx.put("returnItemTypeId", ((Map<String, Object>) returnItemTypeMapping).get("returnItemTypeId"));
                }
                if (UtilValidate.isNotEmpty(((Map<String, Object>) newItemCtx).get("orderAdjustmentId"))) {
                    Debug.logInfo("Found unexpected orderAdjustment:" + ((Map<String, Object>) newItemCtx).get("orderAdjustmentId"), MODULE);
                    newItemCtx.remove("orderAdjustmentId");
                }
                if (((Comparable) ((Map<String, Object>) newItemCtx).get("returnQuantity")).compareTo(BigDecimal.ZERO) > 0) {
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createReturnItem", newItemCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createReturnItem: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                } else {
                    Debug.logInfo("This return item is not going to be created because its returnQuantity is zero: " + newItemCtx, MODULE);
                }
            }
        }
        List<GenericValue> orderAdjustments = null;
        try {
            orderAdjustments = EntityQuery.use(delegator)
                    .from("OrderAdjustment")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderAdjustment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (orderAdjustments != null) {
            for (GenericValue orderAdjustment : orderAdjustments) {
                returnAdjCtx = null;
                ((Map<String, Object>) returnAdjCtx).put("orderAdjustmentId", ((Map<String, Object>) orderAdjustment).get("orderAdjustmentId"));
                ((Map<String, Object>) returnAdjCtx).put("returnId", returnId);
                returnCount = null;
                try {
                    returnCount = EntityQuery.use(delegator)
                            .from("ReturnAdjustment")
                            .where("orderAdjustmentId", ((Map<String, Object>) orderAdjustment).get("orderAdjustmentId"))
                            .queryCount();
                } catch (Exception e) {
                    Debug.logError(e, "Error counting ReturnAdjustment: " + e.getMessage(), MODULE);
                }
                if ("0".equals(returnCount)) {
                    Debug.logInfo("Create new return adjustment: " + returnAdjCtx, MODULE);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createReturnAdjustment", (Map<String, Object>) returnAdjCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createReturnAdjustment: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        Map<String, Object> orderAvailableCtx = new HashMap<>();
        orderAvailableCtx.put("orderId", ((Map<String, Object>) orderHeader).get("orderId"));
        orderAvailableCtx.put("countNewReturnItems", Boolean.TRUE);
        Object availableReturnTotal = null;
        Object returnTotal = null;
        Object orderTotal = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getOrderAvailableReturnedTotal", orderAvailableCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            availableReturnTotal = serviceResult.get("availableReturnTotal");
            returnTotal = serviceResult.get("returnTotal");
            orderTotal = serviceResult.get("orderTotal");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getOrderAvailableReturnedTotal: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("OrderTotal [" + orderTotal + "] - ReturnTotal [" + returnTotal + "] = available Return Total [" + availableReturnTotal + "]", MODULE);
        if (!"0.00".equals(availableReturnTotal)) {
            balanceItemCtx.put("description", "Balance Adjustment");
            balanceItemCtx.put("returnAdjustmentTypeId", "RET_MAN_ADJ");
            balanceItemCtx.put("returnId", returnId);
            balanceItemCtx.put("returnItemSeqId", "_NA_");
            balanceItemCtx.put("amount", availableReturnTotal);
            Debug.logWarning("Creating a balance adjustment of [" + availableReturnTotal + "] for return [" + returnId + "]", MODULE);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createReturnAdjustment", balanceItemCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createReturnAdjustment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if ("CUSTOMER_RETURN".equals(returnHeaderTypeId)) {
            updateHeaderCtx.put("statusId", "RETURN_ACCEPTED");
        } else {
            updateHeaderCtx.put("statusId", "SUP_RETURN_ACCEPTED");
        }
        updateHeaderCtx.put("returnId", returnId);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateReturnHeader", updateHeaderCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateReturnHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("CUSTOMER_RETURN".equals(returnHeaderTypeId)) {
            if (Boolean.TRUE.equals(context.get("receiveReturn"))) {
                receiveCtx.put("returnId", returnId);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("quickReceiveReturn", receiveCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling quickReceiveReturn: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                Debug.logInfo("Receive flag not set; will handle receiving on entity-sync", MODULE);
            }
        }
        result.put("returnId", returnId);

        return "success";
    }


    /**
     * If returnId is null, create a return; then create Return Item or Adjustment based on the parameters passed in
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createReturnAndItemOrAdjustment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> returnHeaderInMap = null;
        Object returnId = null;
        if (UtilValidate.isEmpty(context.get("returnId"))) {
            // set-service-fields from "parameters" to "returnHeaderInMap" for service "createReturnHeader"
            returnHeaderInMap.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createReturnHeader", returnHeaderInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                returnId = serviceResult.get("returnId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createReturnHeader: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            context.put("returnId", returnId);
            result.put("returnId", returnId);
        }
        Map<String, Object> createReturnItemOrAdjustmentInMap = new HashMap<>();
        // set-service-fields from "parameters" to "createReturnItemOrAdjustmentInMap" for service "createReturnItemOrAdjustment"
        createReturnItemOrAdjustmentInMap.putAll(UtilMisc.toMap(context));
        Object returnAdjustmentId = null;
        Object returnItemSeqId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createReturnItemOrAdjustment", createReturnItemOrAdjustmentInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            returnAdjustmentId = serviceResult.get("returnAdjustmentId");
            returnItemSeqId = serviceResult.get("returnItemSeqId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createReturnItemOrAdjustment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        result.put("returnAdjustmentId", returnAdjustmentId);
        result.put("returnItemSeqId", returnItemSeqId);

        return "success";
    }


    /**
     * Create a ReturnItemBilling
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createReturnItemBilling(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("ReturnItemBilling");
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
     * Update a ReturnItems
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String cancelReturnItems(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> returnItemMap = null;
        List<GenericValue> returnItems = null;
        try {
            returnItems = EntityQuery.use(delegator)
                    .from("ReturnItem")
                    .distinct()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (returnItems != null) {
            for (GenericValue returnItem : returnItems) {
                returnItemMap.put("returnId", context.get("returnId"));
                returnItemMap.put("returnItemSeqId", ((Map<String, Object>) returnItem).get("returnItemSeqId"));
                returnItemMap.put("statusId", "RETURN_CANCELLED");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateReturnItem", returnItemMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateReturnItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Cancel the associated OrderItems of the replacement order, if any.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String cancelReplacementOrderItems(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> orderItemMap = null;
        GenericValue orderItem = null;
        List<GenericValue> replacementOrderItems = null;
        Map<String, Object> oiaMap = null;
        GenericValue returnItem = null;
        try {
            returnItem = EntityQuery.use(delegator)
                    .from("ReturnItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object oiaMap_orderItemAssocTypeId = null;
        Object orderItemMap_orderId = null;
        Object orderItemMap_orderItemSeqId = null;
        if (("RTN_REPLACE".equals(((Map<String, Object>) returnItem).get("returnTypeId")) || "RTN_CSREPLACE".equals(((Map<String, Object>) returnItem).get("returnTypeId")) || "RTN_REPAIR_REPLACE".equals(((Map<String, Object>) returnItem).get("returnTypeId")))) {
            try {
                orderItem = returnItem.getRelatedOne("OrderItem", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one OrderItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            oiaMap.put("orderItemAssocTypeId", "REPLACEMENT");
            try {
                replacementOrderItems = orderItem.getRelated("FromOrderItemAssoc", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related FromOrderItemAssoc: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (replacementOrderItems != null) {
                for (GenericValue replacementOrderItem : replacementOrderItems) {
                    orderItemMap.put("orderId", ((Map<String, Object>) replacementOrderItem).get("toOrderId"));
                    orderItemMap.put("orderItemSeqId", ((Map<String, Object>) replacementOrderItem).get("toOrderItemSeqId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("cancelOrderItem", orderItemMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling cancelOrderItem: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Process the replacements in a wait return
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String processWaitReplacementReturn(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> inMap = new HashMap<>();
        inMap.put("returnId", context.get("returnId"));
        inMap.put("returnTypeId", "RTN_REPLACE");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("processReplacementReturn", inMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling processReplacementReturn: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Process the replacements in a cross-ship return
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String processCrossShipReplacementReturn(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> inMap = new HashMap<>();
        inMap.put("returnId", context.get("returnId"));
        inMap.put("returnTypeId", "RTN_CSREPLACE");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("processReplacementReturn", inMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling processReplacementReturn: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Process the replacements in a repair return
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String processRepairReplacementReturn(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> inMap = new HashMap<>();
        inMap.put("returnId", context.get("returnId"));
        inMap.put("returnTypeId", "RTN_REPAIR_REPLACE");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("processReplacementReturn", inMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling processReplacementReturn: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Process the replacements in a wait reserved return when the return is accepted and then received
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String processWaitReplacementReservedReturn(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> inMap = null;
        Map<String, Object> changeOrderStatusMap = null;
        List<GenericValue> returnItems = null;
        Map<String, Object> createOrderMap = null;
        GenericValue returnItemResponse = null;
        GenericValue orderHeader = null;
        GenericValue returnItem = null;
        GenericValue returnHeader = null;
        try {
            returnHeader = EntityQuery.use(delegator)
                    .from("ReturnHeader")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("RETURN_ACCEPTED".equals(((Map<String, Object>) returnHeader).get("statusId"))) {
            inMap.put("returnId", context.get("returnId"));
            inMap.put("returnTypeId", "RTN_WAIT_REPLACE_RES");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("processReplacementReturn", inMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling processReplacementReturn: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if ("RETURN_RECEIVED".equals(((Map<String, Object>) returnHeader).get("statusId"))) {
            try {
                returnItems = EntityQuery.use(delegator)
                        .from("ReturnItem")
                        .where(UtilMisc.toMap("returnId", ((Map<String, Object>) returnHeader).get("returnId"), "returnTypeId", "RTN_WAIT_REPLACE_RES"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ReturnItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(returnItems)) {
                returnItem = EntityUtil.getFirst((List<GenericValue>) returnItems);
                try {
                    returnItemResponse = returnItem.getRelatedOne("ReturnItemResponse", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one ReturnItemResponse: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                try {
                    orderHeader = EntityQuery.use(delegator)
                            .from("OrderHeader")
                            .where(UtilMisc.toMap("orderId", ((Map<String, Object>) returnItemResponse).get("replacementOrderId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(orderHeader)) {
                    if ("ORDER_HOLD".equals(((Map<String, Object>) orderHeader).get("statusId"))) {
                        changeOrderStatusMap.put("statusId", "ORDER_APPROVED");
                        changeOrderStatusMap.put("orderId", ((Map<String, Object>) returnItemResponse).get("replacementOrderId"));
                        changeOrderStatusMap.put("setItemStatus", "Y");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("changeOrderStatus", changeOrderStatusMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling changeOrderStatus: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                    if ("ORDER_CANCELLED".equals(((Map<String, Object>) orderHeader).get("statusId"))) {
                        createOrderMap.put("returnId", context.get("returnId"));
                        createOrderMap.put("returnTypeId", "RTN_WAIT_REPLACE_RES");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("processReplacementReturn", createOrderMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling processReplacementReturn: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Process the replacements in a immediate return
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String processReplaceImmediatelyReturn(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> inMap = new HashMap<>();
        inMap.put("returnId", context.get("returnId"));
        inMap.put("returnTypeId", "RTN_REPLACE_IMMEDIAT");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("processReplacementReturn", inMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling processReplacementReturn: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Process the refund in a return
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String processRefundOnlyReturn(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> inMap = new HashMap<>();
        inMap.put("returnId", context.get("returnId"));
        inMap.put("returnTypeId", "RTN_REFUND");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("processRefundReturn", inMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling processRefundReturn: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Process the Immediate refund in a return
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String processRefundImmediatelyReturn(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> inMap = new HashMap<>();
        inMap.put("returnId", context.get("returnId"));
        inMap.put("returnTypeId", "RTN_REFUND_IMMEDIATE");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("processRefundReturn", inMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling processRefundReturn: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create a ReturnItemShipment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createReturnItemShipment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("ReturnItemShipment");
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
     * Get the return status associated with customer vs. vendor return
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getStatusItemsForReturn(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<GenericValue> statusItems = null;
        if ("CUSTOMER_RETURN".equals(context.get("returnHeaderTypeId"))) {
            try {
                statusItems = EntityQuery.use(delegator)
                        .from("StatusItem")
                        .where(UtilMisc.toMap("statusTypeId", "ORDER_RETURN_STTS"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying StatusItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("statusItems", statusItems);
        } else {
            try {
                statusItems = EntityQuery.use(delegator)
                        .from("StatusItem")
                        .where(UtilMisc.toMap("statusTypeId", "PORDER_RETURN_STTS"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying StatusItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("statusItems", statusItems);
        }

        return "success";
    }


    /**
     * Associate exchange order with original order in OrderItemAssoc entity
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createExchangeOrderAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue orderItemAssocValue = null;
        GenericValue orderItemAssoc = null;
        Map<String, Object> orderItemAssocMap = null;
        Long orderItemCounter = null;
        Long returnItemCounter = null;
        List<GenericValue> returnItems = null;
        try {
            returnItems = EntityQuery.use(delegator)
                    .from("ReturnItem")
                    .where(UtilMisc.toMap("orderId", context.get("originOrderId"), "returnTypeId", "RTN_REFUND"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Long returnItemSize = (Long) (long) (returnItems != null ? ((java.util.List<?>) returnItems).size() : 0);
        List<GenericValue> orderItems = null;
        try {
            orderItems = EntityQuery.use(delegator)
                    .from("OrderItem")
                    .where(UtilMisc.toMap("orderId", context.get("orderId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Long orderItemSize = (Long) (long) (orderItems != null ? ((java.util.List<?>) orderItems).size() : 0);
        Object orderItemAssocMap_orderId = null;
        Object orderItemAssocMap_orderItemSeqId = null;
        Object orderItemAssocMap_toOrderId = null;
        Object orderItemAssocMap_toOrderItemSeqId = null;
        Object orderItemAssocMap_shipGroupSeqId = null;
        Object orderItemAssocMap_toShipGroupSeqId = null;
        Object orderItemAssocMap_orderItemAssocTypeId = null;
        if (returnItemSize != null /* TODO: field compare operator greater */) {
            returnItemCounter = 1L;
            if (returnItems != null) {
                for (GenericValue returnItem : returnItems) {
                    orderItemAssocMap.put("orderId", context.get("originOrderId"));
                    orderItemAssocMap.put("orderItemSeqId", ((Map<String, Object>) returnItem).get("orderItemSeqId"));
                    orderItemCounter = 1L;
                    if (orderItems != null) {
                        for (GenericValue orderItem : orderItems) {
                            if (java.util.Objects.equals(returnItemCounter, orderItemCounter)) {
                                orderItemAssocMap.put("toOrderId", context.get("orderId"));
                                orderItemAssocMap.put("toOrderItemSeqId", ((Map<String, Object>) orderItem).get("orderItemSeqId"));
                            }
                            orderItemCounter = (Long) context.get("orderItemCounter+1");
                        }
                    }
                    orderItemAssocMap.put("shipGroupSeqId", "_NA_");
                    orderItemAssocMap.put("toShipGroupSeqId", "_NA_");
                    orderItemAssocMap.put("orderItemAssocTypeId", "EXCHANGE");
                    orderItemAssoc = delegator.makeValue("OrderItemAssoc");
                    orderItemAssoc.setPKFields((Map<String, Object>) orderItemAssocMap);
                    try {
                        orderItemAssocValue = EntityQuery.use(delegator)
                                .from("OrderItemAssoc")
                                .where(orderItemAssoc)
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error finding by primary key OrderItemAssoc: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isEmpty(orderItemAssocValue)) {
                        try {
                            delegator.create(orderItemAssoc);
                        } catch (Exception e) {
                            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        orderItemAssoc = null;
                    }
                    returnItemCounter = (Long) context.get("returnItemCounter+1");
                }
            }
        } else {
            orderItemCounter = 1L;
            if (orderItems != null) {
                for (GenericValue orderItemEntry : orderItems) {
                    orderItemAssocMap.put("toOrderId", context.get("orderId"));
                    orderItemAssocMap.put("toOrderItemSeqId", ((Map<String, Object>) orderItemEntry).get("orderItemSeqId"));
                    returnItemCounter = 1L;
                    if (returnItems != null) {
                        for (GenericValue returnItemEntry : returnItems) {
                            if (java.util.Objects.equals(orderItemCounter, returnItemCounter)) {
                                orderItemAssocMap.put("orderId", context.get("originOrderId"));
                                orderItemAssocMap.put("orderItemSeqId", ((Map<String, Object>) returnItemEntry).get("orderItemSeqId"));
                            }
                            returnItemCounter = (Long) context.get("returnItemCounter+1");
                        }
                    }
                    orderItemAssocMap.put("shipGroupSeqId", "_NA_");
                    orderItemAssocMap.put("toShipGroupSeqId", "_NA_");
                    orderItemAssocMap.put("orderItemAssocTypeId", "EXCHANGE");
                    orderItemAssoc = delegator.makeValue("OrderItemAssoc");
                    orderItemAssoc.setPKFields((Map<String, Object>) orderItemAssocMap);
                    try {
                        orderItemAssocValue = EntityQuery.use(delegator)
                                .from("OrderItemAssoc")
                                .where(orderItemAssoc)
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error finding by primary key OrderItemAssoc: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isEmpty(orderItemAssocValue)) {
                        try {
                            delegator.create(orderItemAssoc);
                        } catch (Exception e) {
                            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        orderItemAssocMap = null;
                    }
                    orderItemCounter = (Long) context.get("orderItemCounter+1");
                }
            }
        }

        return "success";
    }


    /**
     * When one or more product is received directly through receive inventory or refund return then add these product(s) back to category, if they does not have any active category
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String addProductsBackToCategory(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue inventoryItem = null;
        GenericValue product = null;
        List<GenericValue> returnItems = null;
        List<Object> orderBy = null;
        List<GenericValue> productCategoryMembers = null;
        GenericValue pcm = null;
        Map<String, Object> updateProductToCategoryMap = null;
        List<GenericValue> pcms = null;
        GenericValue returnItem = null;
        if (UtilValidate.isNotEmpty(context.get("inventoryItemId"))) {
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
            try {
                product = inventoryItem.getRelatedOne("Product", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one Product: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            orderBy.add("-thruDate");
            try {
                productCategoryMembers = product.getRelated("ProductCategoryMember", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related ProductCategoryMember: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(productCategoryMembers)) {
                pcms = EntityUtil.filterByDate((List<GenericValue>) productCategoryMembers);
                if (UtilValidate.isEmpty(pcms)) {
                    pcm = EntityUtil.getFirst((List<GenericValue>) productCategoryMembers);
                    pcm.remove("thruDate");
                    // set-service-fields from "pcm" to "updateProductToCategoryMap" for service "updateProductToCategory"
                    updateProductToCategoryMap.putAll(UtilMisc.toMap(pcm));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updateProductToCategory", updateProductToCategoryMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updateProductToCategory: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        } else {
            if (UtilValidate.isNotEmpty(context.get("returnId"))) {
                try {
                    returnItems = EntityQuery.use(delegator)
                            .from("ReturnItem")
                            .where(UtilMisc.toMap("returnId", context.get("returnId"), "returnTypeId", "RTN_REFUND"))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ReturnItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(returnItems)) {
                    if (returnItems != null) {
                        for (GenericValue returnItemEntry : returnItems) {
                            try {
                                product = returnItemEntry.getRelatedOne("Product", false);
                            } catch (Exception e) {
                                Debug.logError(e, "Error getting related one Product: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            orderBy.add("-thruDate");
                            try {
                                productCategoryMembers = product.getRelated("ProductCategoryMember", null, null, false);
                            } catch (Exception e) {
                                Debug.logError(e, "Error getting related ProductCategoryMember: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            if (UtilValidate.isNotEmpty(productCategoryMembers)) {
                                pcms = EntityUtil.filterByDate((List<GenericValue>) productCategoryMembers);
                                if (UtilValidate.isEmpty(pcms)) {
                                    pcm = EntityUtil.getFirst((List<GenericValue>) productCategoryMembers);
                                    pcm.remove("thruDate");
                                    // set-service-fields from "pcm" to "updateProductToCategoryMap" for service "updateProductToCategory"
                                    updateProductToCategoryMap.putAll(UtilMisc.toMap(pcm));
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("updateProductToCategory", updateProductToCategoryMap);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                            return "error";
                                        }
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling updateProductToCategory: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Create ReturnHeader and ReturnItem Status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createReturnStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        GenericValue returnHeader = null;
        GenericValue returnItem = null;
        newEntity = delegator.makeValue("ReturnStatus");
        if (UtilValidate.isEmpty(context.get("returnItemSeqId"))) {
            try {
                returnHeader = EntityQuery.use(delegator)
                        .from("ReturnHeader")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ReturnHeader: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            newEntity.put("statusId", ((Map<String, Object>) returnHeader).get("statusId"));
        } else {
            try {
                returnItem = EntityQuery.use(delegator)
                        .from("ReturnItem")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ReturnItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            newEntity.put("returnItemSeqId", ((Map<String, Object>) returnItem).get("returnItemSeqId"));
            newEntity.put("statusId", ((Map<String, Object>) returnItem).get("statusId"));
        }
        ((GenericValue) newEntity).put("returnStatusId", delegator.getNextSeqId("ReturnStatus"));
        newEntity.put("returnId", context.get("returnId"));
        newEntity.put("changeByUserLoginId", ((Map<String, Object>) userLogin).get("userLoginId"));
        Timestamp newEntity_statusDatetime = new Timestamp(System.currentTimeMillis());
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
     * Update ReturnContactMech
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateReturnContactMech(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> deleteReturnContactMechMap = null;
        GenericValue returnHeader = null;
        GenericValue returnContactMechMap = delegator.makeValue("ReturnContactMech");
        returnContactMechMap.setPKFields((Map<String, Object>) context);
        try {
            returnHeader = EntityQuery.use(delegator)
                    .from("ReturnHeader")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ReturnHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createReturnContactMechMap = new HashMap<>();
        createReturnContactMechMap.put("returnId", context.get("returnId"));
        createReturnContactMechMap.put("contactMechPurposeTypeId", context.get("contactMechPurposeTypeId"));
        createReturnContactMechMap.put("contactMechId", context.get("contactMechId"));
        // TODO: Convert <find-by-and> element
        if (UtilValidate.isEmpty(context.get("returnContactMechList"))) {
            if ("SHIPPING_LOCATION".equals(context.get("contactMechPurposeTypeId"))) {
                returnHeader.put("originContactMechId", ((Map<String, Object>) createReturnContactMechMap).get("contactMechId"));
                try {
                    delegator.store(returnHeader);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createReturnContactMech", createReturnContactMechMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createReturnContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            deleteReturnContactMechMap.put("returnId", context.get("returnId"));
            deleteReturnContactMechMap.put("contactMechId", context.get("oldContactMechId"));
            deleteReturnContactMechMap.put("contactMechPurposeTypeId", context.get("contactMechPurposeTypeId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("deleteReturnContactMech", deleteReturnContactMechMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling deleteReturnContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        try {
            delegator.store(returnContactMechMap);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create the return item for rental (which items has product type is ASSET_USAGE_OUT_IN)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createReturnItemForRental(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> updateHeaderCtx = null;
        List<GenericValue> orderRoles = null;
        Map<String, Object> createReturnCtx = null;
        Object returnId = null;
        GenericValue orderRole = null;
        GenericValue productStore = null;
        List<GenericValue> orderItems = null;
        GenericValue orderHeader = null;
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(UtilMisc.toMap("orderId", context.get("orderId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("SALES_ORDER".equals(((Map<String, Object>) orderHeader).get("orderTypeId"))) {
            try {
                orderRoles = EntityQuery.use(delegator)
                        .from("OrderRole")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            orderRole = EntityUtil.getFirst((List<GenericValue>) orderRoles);
            try {
                productStore = orderHeader.getRelatedOne("ProductStore", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one ProductStore: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) productStore).get("inventoryFacilityId"))) {
                createReturnCtx.put("destinationFacilityId", ((Map<String, Object>) productStore).get("inventoryFacilityId"));
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) productStore).get("reqReturnInventoryReceive"))) {
                updateHeaderCtx.put("needsInventoryReceive", ((Map<String, Object>) productStore).get("reqReturnInventoryReceive"));
            } else {
                updateHeaderCtx.put("needsInventoryReceive", "N");
            }
            createReturnCtx.put("orderId", ((Map<String, Object>) orderHeader).get("orderId"));
            createReturnCtx.put("currencyUomId", ((Map<String, Object>) orderHeader).get("currencyUom"));
            createReturnCtx.put("fromPartyId", ((Map<String, Object>) orderRole).get("partyId"));
            createReturnCtx.put("toPartyId", ((Map<String, Object>) productStore).get("payToPartyId"));
            createReturnCtx.put("returnHeaderTypeId", "CUSTOMER_RETURN");
            createReturnCtx.put("returnReasonId", "RTN_NORMAL_RETURN");
            createReturnCtx.put("returnTypeId", "RTN_RENTAL");
            createReturnCtx.put("returnItemTypeId", "RET_FDPROD_ITEM");
            createReturnCtx.put("expectedItemStatus", "INV_RETURNED");
            createReturnCtx.put("returnPrice", new BigDecimal("0.00"));
            try {
                orderItems = EntityQuery.use(delegator)
                        .from("OrderItemAndProduct")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderItemAndProduct: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (orderItems != null) {
                for (GenericValue orderItem : orderItems) {
                    createReturnCtx.put("productId", ((Map<String, Object>) orderItem).get("productId"));
                    createReturnCtx.put("orderItemSeqId", ((Map<String, Object>) orderItem).get("orderItemSeqId"));
                    createReturnCtx.put("description", ((Map<String, Object>) orderItem).get("itemDescription"));
                    createReturnCtx.put("returnQuantity", ((Map<String, Object>) orderItem).get("quantity"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createReturnAndItemOrAdjustment", createReturnCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        returnId = serviceResult.get("returnId");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createReturnAndItemOrAdjustment: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isNotEmpty(returnId)) {
                        createReturnCtx.put("returnId", returnId);
                    }
                }
            }
        }

        return "success";
    }

}

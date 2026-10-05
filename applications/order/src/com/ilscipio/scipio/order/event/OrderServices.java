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
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.order.shoppingcart.ShoppingCartItem;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://order/script/org/ofbiz/order/order/OrderServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OrderServices {

    private static final String MODULE = OrderServices.class.getName();


    /**
     * Get Summary Information About Orders for a Customer
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getOrderedSummaryInformation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Timestamp nowTimestamp = null;
        if (UtilValidate.isNotEmpty(context.get("monthsToInclude"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
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
                Object scriptResult = GroovyUtil.eval("calendar = com.ibm.icu.util.Calendar.getInstance()\n                calendar.setTimeInMillis(nowTimestamp.getTime())\n                calendar.add(com.ibm.icu.util.Calendar.MONTH, -monthsToInclude.intValue())\n                parameters.put(\"fromDate\", new Timestamp(calendar.getTimeInMillis()))", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            context.put("thruDate", nowTimestamp);
        }
        if (UtilValidate.isEmpty(context.get("roleTypeId"))) {
            context.put("roleTypeId", "PLACING_CUSTOMER");
        }
        if (UtilValidate.isEmpty(context.get("orderTypeId"))) {
            context.put("orderTypeId", "SALES_ORDER");
        }
        if (UtilValidate.isEmpty(context.get("statusId"))) {
            context.put("statusId", "ORDER_COMPLETED");
        }
        List<GenericValue> orderInfoList = null;
        try {
            orderInfoList = EntityQuery.use(delegator)
                    .from("OrderHeaderAndRoleSummary")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeaderAndRoleSummary: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object plainDoubleZero = 0.0;
        Object plainLongZero = 0;
        result.put("totalGrandAmount", plainDoubleZero);
        result.put("totalSubRemainingAmount", plainDoubleZero);
        result.put("totalOrders", plainLongZero);
        GenericValue orderInfo = EntityUtil.getFirst((List<GenericValue>) orderInfoList);
        if (UtilValidate.isNotEmpty(orderInfo)) {
            result.put("totalGrandAmount", ((Map<String, Object>) orderInfo).get("totalGrandAmount"));
            result.put("totalSubRemainingAmount", ((Map<String, Object>) orderInfo).get("totalSubRemainingAmount"));
            result.put("totalOrders", ((Map<String, Object>) orderInfo).get("totalOrders"));
        }

        return "success";
    }


    /**
     * Create OrderShipment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOrderShipment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object operationName = "Create OrderShipment";
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("OrderShipment");
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
     * Update OrderShipment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateOrderShipment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object operationName = "Update OrderShipment";
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("OrderShipment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderShipment: " + e.getMessage(), MODULE);
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
     * Delete OrderShipment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteOrderShipment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object operationName = "Delete OrderShipment";
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("OrderShipment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderShipment: " + e.getMessage(), MODULE);
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
     * Create OrderRequirementCommitment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOrderRequirementCommitment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("OrderRequirementCommitment");
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
     * create a requirement and commitment for it
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createRequirementAndCommitment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> inputMap = null;
        inputMap.put("requirementTypeId", "PRODUCT_REQUIREMENT");
        GenericValue orderHeader = null;
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue productStore = null;
        try {
            productStore = orderHeader.getRelatedOne("ProductStore", true);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one ProductStore: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) productStore).get("inventoryFacilityId"))) {
            inputMap.put("facilityId", ((Map<String, Object>) productStore).get("inventoryFacilityId"));
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createRequirement", inputMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("requirementId", serviceResult.get("requirementId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createRequirement: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> orderReqCommitParams = new HashMap<>();
        // set-service-fields from "parameters" to "orderReqCommitParams" for service "createOrderRequirementCommitment"
        orderReqCommitParams.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createOrderRequirementCommitment", orderReqCommitParams);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createOrderRequirementCommitment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("requirementId", context.get("requirementId"));

        return "success";
    }


    /**
     * finds ProductFacility and QOH, ATP inventory for an inventoryItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getProductFacilityAndQuantities(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue productFacility = null;
        try {
            productFacility = EntityQuery.use(delegator)
                    .from("ProductFacility")
                    .where(UtilMisc.toMap("productId", ((Map<String, Object>) context.get("inventoryItem")).get("productId"), "facilityId", ((Map<String, Object>) context.get("inventoryItem")).get("facilityId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductFacility: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> inputMap = new HashMap<>();
        inputMap.put("productId", ((Map<String, Object>) context.get("inventoryItem")).get("productId"));
        inputMap.put("facilityId", ((Map<String, Object>) context.get("inventoryItem")).get("facilityId"));
        Object quantityOnHandTotal = null;
        Object availableToPromiseTotal = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getInventoryAvailableByFacility", inputMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            quantityOnHandTotal = serviceResult.get("quantityOnHandTotal");
            availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getInventoryAvailableByFacility: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        inputMap = null;

        return "success";
    }


    /**
     * finds the requirement method for the product
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getProductRequirementMethod(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue order = null;
        Object requirementMethodId = null;
        Boolean isMarketingPkg = null;
        GenericValue productStore = null;
        if (UtilValidate.isNotEmpty(context.get("orderId"))) {
            try {
                order = EntityQuery.use(delegator)
                        .from("OrderHeader")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        GenericValue product = null;
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
        requirementMethodId = ((Map<String, Object>) product).get("requirementMethodEnumId");
        if (UtilValidate.isEmpty(requirementMethodId)) {
            isMarketingPkg = (Boolean) GroovyUtil.eval("org.ofbiz.entity.util.EntityTypeUtil.hasParentType(delegator, 'ProductType', 'productTypeId', product.productTypeId, 'parentTypeId', 'MARKETING_PKG')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
            if ((Boolean.FALSE.equals(isMarketingPkg) && !"DIGITAL_GOOD".equals(((Map<String, Object>) product).get("productTypeId")) && !(UtilValidate.isEmpty(order)))) {
                try {
                    productStore = EntityQuery.use(delegator)
                            .from("ProductStore")
                            .where(UtilMisc.toMap("productStoreId", ((Map<String, Object>) order).get("productStoreId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                requirementMethodId = ((Map<String, Object>) productStore).get("requirementMethodEnumId");
            }
        }

        return "success";
    }


    /**
     * Create OrderRequirementCommitment and Requirement
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkCreateOrderRequirement(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> inputMap = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        String result = getProductRequirementMethod(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        Object order = null;
        if ("SALES_ORDER".equals(((Map<String, Object>) order).get("orderTypeId"))) {
            if ("PRODRQM_AUTO".equals(context.get("requirementMethodId"))) {
                inputMap.put("productId", context.get("productId"));
                inputMap.put("quantity", context.get("quantity"));
                String result2 = createRequirementAndCommitment(request, response);
                if (!"success".equals(result2)) {
                    return result2;
                }
            }
        }

        return "success";
    }


    /**
     * Create a Requirement if QOH goes under the minimum stock level
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkCreateStockRequirementQoh(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue inventoryItem = null;
        GenericValue itemIssuance = null;
        Object newQuantityOnHand = null;
        Map<String, Object> inputMap = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isNotEmpty(context.get("itemIssuanceId"))) {
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
            try {
                inventoryItem = EntityQuery.use(delegator)
                        .from("InventoryItem")
                        .where(UtilMisc.toMap("inventoryItemId", ((Map<String, Object>) itemIssuance).get("inventoryItemId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying InventoryItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
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
        }
        context.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        String inlineResult = getProductRequirementMethod(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        if ("PRODRQM_STOCK".equals(context.get("requirementMethodId"))) {
            String checkResult = getProductFacilityAndQuantities(request, response);
            if (!"success".equals(checkResult)) {
                return checkResult;
            }
            Object productFacility = null;
            if (UtilValidate.isNotEmpty(((Map<String, Object>) productFacility).get("minimumStock"))) {
                if (context.get("quantityOnHandTotal") != null /* TODO: field compare operator greater-equals */) {
                    newQuantityOnHand = new BigDecimal(context.get("quantity").toString());
                    if (newQuantityOnHand != null /* TODO: field compare operator less */) {
                        inputMap.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
                        inputMap.put("facilityId", ((Map<String, Object>) productFacility).get("facilityId"));
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) productFacility).get("reorderQuantity"))) {
                            inputMap.put("quantity", ((Map<String, Object>) productFacility).get("reorderQuantity"));
                        } else {
                            inputMap.put("quantity", context.get("quantity"));
                        }
                        inputMap.put("requirementTypeId", "PRODUCT_REQUIREMENT");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createRequirement", inputMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                            context.put("requirementId", serviceResult.get("requirementId"));
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createRequirement: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        result.put("requirementId", context.get("requirementId"));
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Create a Requirement if ATP goes under the minimum stock level
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkCreateStockRequirementAtp(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object oldAvailableToPromise = null;
        Map<String, Object> inputMap = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
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
        context.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        String inlineResult = getProductRequirementMethod(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        if ("PRODRQM_STOCK_ATP".equals(context.get("requirementMethodId"))) {
            String checkResult = getProductFacilityAndQuantities(request, response);
            if (!"success".equals(checkResult)) {
                return checkResult;
            }
            Object productFacility = null;
            if (UtilValidate.isNotEmpty(((Map<String, Object>) productFacility).get("minimumStock"))) {
                if (context.get("availableToPromiseTotal") != null /* TODO: field compare operator less */) {
                    oldAvailableToPromise = new BigDecimal(context.get("quantity").toString());
                    if (oldAvailableToPromise != null /* TODO: field compare operator greater-equals */) {
                        inputMap.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
                        inputMap.put("facilityId", ((Map<String, Object>) productFacility).get("facilityId"));
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) productFacility).get("reorderQuantity"))) {
                            inputMap.put("quantity", ((Map<String, Object>) productFacility).get("reorderQuantity"));
                        } else {
                            inputMap.put("quantity", context.get("quantity"));
                        }
                        inputMap.put("requirementTypeId", "PRODUCT_REQUIREMENT");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createRequirement", inputMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                            context.put("requirementId", serviceResult.get("requirementId"));
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createRequirement: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        result.put("requirementId", context.get("requirementId"));
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Create a Requirement for an item based on ATP inventory quantity and minimum
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createRequirementFromItemATP(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> requirements = null;
        Object existingRequirementTotal = null;
        Object newRequirementTotal = null;
        Object minimumStock = null;
        Map<String, Object> inputMap = null;
        Object quantityShortfall = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
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
        context.put("productId", ((Map<String, Object>) inventoryItem).get("productId"));
        String result = getProductRequirementMethod(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if ("PRODRQM_ATP".equals(context.get("requirementMethodId"))) {
            String checkResult1 = getProductFacilityAndQuantities(request, response);
            if (!"success".equals(checkResult1)) {
                return checkResult1;
            }
            if (UtilValidate.isEmpty(context.get("productFacility"))) {
                minimumStock = "0";
            } else {
                minimumStock = ((Map<String, Object>) context.get("productFacility")).get("minimumStock");
            }
            if (context.get("availableToPromiseTotal") != null /* TODO: field compare operator less */) {
                quantityShortfall = new BigDecimal(context.get("availableToPromiseTotal").toString());
                if (quantityShortfall != null /* TODO: field compare operator less */) {
                    inputMap.put("quantity", quantityShortfall);
                } else {
                    inputMap.put("quantity", context.get("quantity"));
                }
                inputMap.put("productId", context.get("productId"));
                inputMap.put("facilityId", ((Map<String, Object>) inventoryItem).get("facilityId"));
                try {
                    requirements = EntityQuery.use(delegator)
                            .from("Requirement")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Requirement: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (requirements != null) {
                    for (GenericValue requirement : requirements) {
                        existingRequirementTotal = new BigDecimal(existingRequirementTotal.toString());
                    }
                }
                newRequirementTotal = new BigDecimal(existingRequirementTotal.toString());
                if (((Comparable) newRequirementTotal).compareTo("0") >= 0) {
                    newRequirementTotal = ((Map<String, Object>) inputMap).get("quantity");
                    String checkResult2 = createRequirementAndCommitment(request, response);
                    if (!"success".equals(checkResult2)) {
                        return checkResult2;
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Create Requirements for all the products in a facility with QOH under the minimum stock level
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkCreateProductRequirementForFacility(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object quantityOnHandTotal = null;
        Object requirementMethodId = null;
        Object inputMap = null;
        Object requirementId = null;
        Object availableToPromiseTotal = null;
        Object currentQuantity = null;
        Object quantityShortfall = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        List<GenericValue> products = null;
        try {
            products = EntityQuery.use(delegator)
                    .from("ProductFacility")
                    .where(UtilMisc.toMap("facilityId", context.get("facilityId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductFacility: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (products != null) {
            for (GenericValue productFacility : products) {
                context.put("productId", ((Map<String, Object>) productFacility).get("productId"));
                requirementMethodId = null;
                String result = getProductRequirementMethod(request, response);
                if (!"success".equals(result)) {
                    return result;
                }
                if (UtilValidate.isEmpty(requirementMethodId)) {
                    requirementMethodId = context.get("defaultRequirementMethodId");
                }
                Object inputMap_productId = null;
                Object inputMap_facilityId = null;
                BigDecimal inputMap_quantity = null;
                Object inputMap_requirementTypeId = null;
                if (("PRODRQM_STOCK".equals(requirementMethodId) || "PRODRQM_STOCK_ATP".equals(requirementMethodId))) {
                    if (UtilValidate.isNotEmpty(((Map<String, Object>) productFacility).get("minimumStock"))) {
                        inputMap = null;
                        ((Map<String, Object>) inputMap).put("productId", ((Map<String, Object>) productFacility).get("productId"));
                        ((Map<String, Object>) inputMap).put("facilityId", ((Map<String, Object>) productFacility).get("facilityId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("getInventoryAvailableByFacility", (Map<String, Object>) inputMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                            quantityOnHandTotal = serviceResult.get("quantityOnHandTotal");
                            availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling getInventoryAvailableByFacility: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if ("PRODRQM_STOCK".equals(requirementMethodId)) {
                            currentQuantity = quantityOnHandTotal;
                        } else {
                            currentQuantity = availableToPromiseTotal;
                        }
                        if (currentQuantity != null /* TODO: field compare operator less */) {
                            inputMap = null;
                            ((Map<String, Object>) inputMap).put("productId", ((Map<String, Object>) productFacility).get("productId"));
                            if (UtilValidate.isNotEmpty(((Map<String, Object>) productFacility).get("reorderQuantity"))) {
                                ((Map<String, Object>) inputMap).put("quantity", ((Map<String, Object>) productFacility).get("reorderQuantity"));
                            } else {
                                ((Map<String, Object>) inputMap).put("quantity", BigDecimal.ZERO);
                            }
                            quantityShortfall = new BigDecimal(currentQuantity.toString());
                            if (((Map<String, Object>) inputMap).get("quantity") != null /* TODO: field compare operator less */) {
                                ((Map<String, Object>) inputMap).put("quantity", quantityShortfall);
                            }
                            ((Map<String, Object>) inputMap).put("requirementTypeId", "PRODUCT_REQUIREMENT");
                            ((Map<String, Object>) inputMap).put("facilityId", context.get("facilityId"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createRequirement", (Map<String, Object>) inputMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                                requirementId = serviceResult.get("requirementId");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createRequirement: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            Debug.logInfo("Requirement creted with id [" + requirementId + "] for product with id [" + ((Map<String, Object>) productFacility).get("productId") + "].", MODULE);
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Get Next orderId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getNextOrderId(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue customMethod = null;
        Object customMethodName = null;
        Map<String, Object> customMethodMap = null;
        String orderIdTemp = null;
        GenericValue productStore = null;
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
        Debug.logInfo("In getNextOrderId partyId is [" + context.get("partyId") + "], partyAcctgPreference: " + partyAcctgPreference, MODULE);
        if (UtilValidate.isNotEmpty(partyAcctgPreference)) {
            try {
                customMethod = partyAcctgPreference.getRelatedOne("OrderCustomMethod", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one OrderCustomMethod: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            Debug.logWarning("Acctg preference not defined for partyId [" + context.get("partyId") + "]", MODULE);
        }
        if (UtilValidate.isNotEmpty(customMethod)) {
            customMethodName = ((Map<String, Object>) customMethod).get("customMethodName");
        } else {
            if ("ODRSQ_ENF_SEQ".equals(((Map<String, Object>) partyAcctgPreference).get("oldOrderSequenceEnumId"))) {
                customMethodName = "orderSequence_enforced";
            }
        }
        if (UtilValidate.isNotEmpty(customMethodName)) {
            // set-service-fields from "parameters" to "customMethodMap" for service "${customMethodName}"
            customMethodMap.putAll(UtilMisc.toMap(context));
            customMethodMap.put("partyAcctgPreference", partyAcctgPreference);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("${customMethodName}", customMethodMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                orderIdTemp = (String) serviceResult.get("orderId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling ${customMethodName}: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            orderIdTemp = (String) context.get("orderId");
            if (UtilValidate.isEmpty(orderIdTemp)) {
                orderIdTemp = delegator.getNextSeqId("OrderHeader");
            } else {
                // TODO: Convert <check-id> element
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("productStoreId"))) {
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
        }
        Object orderId = "" + ((Map<String, Object>) productStore).get("orderNumberPrefix") + ((Map<String, Object>) partyAcctgPreference).get("orderIdPrefix") + String.valueOf(orderIdTemp);
        result.put("orderId", orderId);

        return "success";
    }


    /**
     * Enforced Sequence (no gaps, per organization)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String orderSequenceEnforced(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Debug.logInfo("In getNextOrderId sequence enum Enforced", MODULE);
        GenericValue partyAcctgPreference = (GenericValue) context.get("partyAcctgPreference");
        if (UtilValidate.isNotEmpty(((Map<String, Object>) partyAcctgPreference).get("lastOrderNumber"))) {
            partyAcctgPreference.set("lastOrderNumber", new BigDecimal(((Map<String, Object>) partyAcctgPreference).get("lastOrderNumber").toString()));
        } else {
            partyAcctgPreference.set("lastOrderNumber", 1);
        }
        try {
            delegator.store(partyAcctgPreference);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object orderId = ((Map<String, Object>) partyAcctgPreference).get("lastOrderNumber");
        result.put("orderId", orderId);

        return "success";
    }


    /**
     * Create OrderHeader
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOrderHeader(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        Object operationName = "Create OrderHeader";
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        newEntity = delegator.makeValue("OrderHeader");
        if (UtilValidate.isNotEmpty(context.get("orderId"))) {
            newEntity.put("orderId", context.get("orderId"));
        } else {
            ((GenericValue) newEntity).put("orderId", delegator.getNextSeqId("OrderHeader"));
        }
        result.put("orderId", ((Map<String, Object>) newEntity).get("orderId"));
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("statusId"))) {
            newEntity.put("statusId", "ORDER_CREATED");
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("orderDate"))) {
            newEntity.put("orderDate", nowTimestamp);
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("entryDate"))) {
            newEntity.put("entryDate", nowTimestamp);
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
     * Update OrderHeader
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateOrderHeader(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object operationName = "Update OrderHeader";
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue orderHeader = null;
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(orderHeader)) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderOrderIdDoesNotExists", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        orderHeader.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(orderHeader);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Auto create OrderAdjustments
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String recreateOrderAdjustments(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> cancelOrderItemInMap = null;
        Object existingOrderAdjustmentTotal = null;
        Object orderItemSeqId = null;
        Object emptyField = null;
        GenericValue newOrderItem = null;
        Object newOrderAdjustmentTotal = null;
        Map<String, Object> createOrderAdjContext = null;
        GenericValue order = null;
        try {
            order = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> orderItems = null;
        try {
            orderItems = order.getRelated("OrderItem", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related OrderItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (orderItems != null) {
            for (GenericValue orderItem : orderItems) {
                Object cancelOrderItemInMap_orderItemSeqId = null;
                if (("Y".equals(((Map<String, Object>) orderItem).get("isPromo")) && !"ITEM_CANCELLED".equals(((Map<String, Object>) orderItem).get("statusId")))) {
                    cancelOrderItemInMap = null;
                    // set-service-fields from "parameters" to "cancelOrderItemInMap" for service "cancelOrderItemNoActions"
                    cancelOrderItemInMap.putAll(UtilMisc.toMap(context));
                    cancelOrderItemInMap.put("orderItemSeqId", ((Map<String, Object>) orderItem).get("orderItemSeqId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("cancelOrderItemNoActions", cancelOrderItemInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling cancelOrderItemNoActions: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        List<GenericValue> orderAdjustments = null;
        try {
            orderAdjustments = order.getRelated("OrderAdjustment", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related OrderAdjustment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        existingOrderAdjustmentTotal = BigDecimal.ZERO;
        if (orderAdjustments != null) {
            for (GenericValue orderAdjustment : orderAdjustments) {
                if ((!(UtilValidate.isEmpty(((Map<String, Object>) orderAdjustment).get("orderAdjustmentTypeId"))) && "PROMOTION_ADJUSTMENT".equals(((Map<String, Object>) orderAdjustment).get("orderAdjustmentTypeId")))) {
                    existingOrderAdjustmentTotal = (new BigDecimal(((Map<String, Object>) orderAdjustment).get("amount").toString())).add(new BigDecimal(existingOrderAdjustmentTotal.toString()));
                }
            }
        }
        Map<String, Object> loadCartFromOrderInMap = new HashMap<>();
        // set-service-fields from "parameters" to "loadCartFromOrderInMap" for service "loadCartFromOrder"
        loadCartFromOrderInMap.putAll(UtilMisc.toMap(context));
        loadCartFromOrderInMap.put("skipInventoryChecks", Boolean.TRUE);
        loadCartFromOrderInMap.put("skipProductChecks", Boolean.TRUE);
        ShoppingCart cart = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("loadCartFromOrder", loadCartFromOrderInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            cart = (ShoppingCart) serviceResult.get("shoppingCart");
        } catch (Exception e) {
            Debug.logError(e, "Error calling loadCartFromOrder: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<ShoppingCartItem> items = cart != null ? cart.items() : null;
        if (items != null) {
            for (ShoppingCartItem item : items) {
                orderItemSeqId = item.getOrderItemSeqId();
                if (UtilValidate.isEmpty(orderItemSeqId)) {
                    newOrderItem = delegator.makeValue("OrderItem");
                    newOrderItem.put("orderId", context.get("orderId"));
                    newOrderItem.put("orderItemTypeId", item.getItemType());
                    newOrderItem.put("selectedAmount", item.getSelectedAmount());
                    newOrderItem.put("unitPrice", item.getBasePrice());
                    newOrderItem.put("unitListPrice", item.getListPrice());
                    newOrderItem.put("itemDescription", item.getName());
                    newOrderItem.put("statusId", item.getStatusId());
                    newOrderItem.put("productId", item.getProductId());
                    newOrderItem.put("quantity", item.getQuantity());
                    newOrderItem.put("isModifiedPrice", "N");
                    newOrderItem.put("isPromo", "Y");
                    if (UtilValidate.isEmpty(((Map<String, Object>) newOrderItem).get("statusId"))) {
                        newOrderItem.put("statusId", "ITEM_CREATED");
                    }
                    delegator.setNextSubSeqId(newOrderItem, "orderItemSeqId", 5, 1);
                    orderItemSeqId = newOrderItem.get("orderItemSeqId");
                    try {
                        delegator.create(newOrderItem);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    item.setOrderItemSeqId((String) newOrderItem.get("orderItemSeqId"));
                }
            }
        }
        List<GenericValue> adjustments = cart != null ? cart.makeAllAdjustments() : null;
        newOrderAdjustmentTotal = BigDecimal.ZERO;
        if (adjustments != null) {
            for (GenericValue adjustment : adjustments) {
                if ((!(UtilValidate.isEmpty(((Map<String, Object>) adjustment).get("productPromoId"))) && UtilValidate.isEmpty(((Map<String, Object>) adjustment).get("orderAdjustmentId")))) {
                    newOrderAdjustmentTotal = (new BigDecimal(((Map<String, Object>) adjustment).get("amount").toString())).add(new BigDecimal(newOrderAdjustmentTotal.toString()));
                }
            }
        }
        BigDecimal orderAdjustmentTotalDifference = (new BigDecimal(existingOrderAdjustmentTotal.toString())).setScale(3, RoundingMode.HALF_UP);
        if (!"0".equals(orderAdjustmentTotalDifference)) {
            createOrderAdjContext.put("orderAdjustmentTypeId", "PROMOTION_ADJUSTMENT");
            createOrderAdjContext.put("orderId", context.get("orderId"));
            createOrderAdjContext.put("orderItemSeqId", "_NA_");
            createOrderAdjContext.put("shipGroupSeqId", "_NA_");
            createOrderAdjContext.put("description", "Adjustment due to order change");
            createOrderAdjContext.put("amount", orderAdjustmentTotalDifference);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createOrderAdjustment", createOrderAdjContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createOrderAdjustment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }

        return "success";
    }


    /**
     * Update OrderContactMech
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateOrderContactMech(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> removeOrderContactMechMap = null;
        Map<String, Object> orderItemShipGroupMap = null;
        Map<String, Object> inputMap = null;
        Map<String, Object> orderContactMechLookupMap = null;
        Map<String, Object> createOrderContactMechMap = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue orderContactMechMap = delegator.makeValue("OrderContactMech");
        orderContactMechMap.setPKFields((Map<String, Object>) context);
        inputMap.put("orderId", context.get("orderId"));
        inputMap.put("contactMechPurposeTypeId", context.get("contactMechPurposeTypeId"));
        inputMap.put("contactMechId", context.get("contactMechId"));
        GenericValue shipGroup = null;
        if ("SHIPPING_LOCATION".equals(context.get("contactMechPurposeTypeId"))) {
            if (!"parameters.oldContactMechId".equals(context.get("contactMechId"))) {
                orderItemShipGroupMap.put("orderId", context.get("orderId"));
                orderItemShipGroupMap.put("contactMechId", context.get("oldContactMechId"));
                // TODO: Convert <find-by-and> element
                if (UtilValidate.isNotEmpty(context.get("shipGroupList"))) {
                    if (context.get("shipGroupList") != null) {
                        for (Object shipGroupEntry : (List<Object>) context.get("shipGroupList")) {
                            inputMap.put("shipGroupSeqId", ((Map<String, Object>) shipGroupEntry).get("shipGroupSeqId"));
                            inputMap.put("shipmentMethod", ((Map<String, Object>) shipGroupEntry).get("shipmentMethodTypeId") + "@" + ((Map<String, Object>) shipGroupEntry).get("carrierPartyId") + "@" + ((Map<String, Object>) shipGroupEntry).get("carrierRoleTypeId"));
                            inputMap.put("oldContactMechId", context.get("oldContactMechId"));
                            // set-service-fields from "inputMap" to "orderItemShipGroupMap" for service "updateOrderItemShipGroup"
                            orderItemShipGroupMap.putAll(UtilMisc.toMap(inputMap));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("updateOrderItemShipGroup", orderItemShipGroupMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling updateOrderItemShipGroup: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                    }
                }
            }
        } else {
            // TODO: Convert <find-by-and> element
            if (UtilValidate.isEmpty(context.get("orderContactMechList"))) {
                // set-service-fields from "parameters" to "createOrderContactMechMap" for service "createOrderContactMech"
                createOrderContactMechMap.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createOrderContactMech", createOrderContactMechMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createOrderContactMech: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                orderContactMechLookupMap.put("orderId", context.get("orderId"));
                orderContactMechLookupMap.put("contactMechId", context.get("oldContactMechId"));
                orderContactMechLookupMap.put("contactMechPurposeTypeId", context.get("contactMechPurposeTypeId"));
                // set-service-fields from "orderContactMechLookupMap" to "removeOrderContactMechMap" for service "removeOrderContactMech"
                removeOrderContactMechMap.putAll(UtilMisc.toMap(orderContactMechLookupMap));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("removeOrderContactMech", removeOrderContactMechMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling removeOrderContactMech: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            try {
                delegator.store(orderContactMechMap);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create OrderItemShipGroup
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOrderItemShipGroup(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue newEntity = delegator.makeValue("OrderItemShipGroup");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("shipGroupSeqId"))) {
            delegator.setNextSubSeqId(newEntity, "shipGroupSeqId", 5, 1);
            Object shipGroupSeqId = newEntity.get("shipGroupSeqId");
            result.put("shipGroupSeqId", ((Map<String, Object>) newEntity).get("shipGroupSeqId"));
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
     * Update OrderItemShipGroup
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateOrderItemShipGroup(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createOrderContactMechMap = null;
        Map<String, Object> removeOrderContactMechMap = null;
        Map<String, Object> inputMap = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue lookupPKMap = delegator.makeValue("OrderItemShipGroup");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("OrderItemShipGroup")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key OrderItemShipGroup: " + e.getMessage(), MODULE);
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
            Object scriptResult = GroovyUtil.eval("shipmentMethod = parameters.get(\"shipmentMethod\")\n            if (shipmentMethod != null) {\n               arr = shipmentMethod.split( \"@\" )\n               parameters.put(\"shipmentMethodTypeId\", arr[0])\n               parameters.put(\"carrierPartyId\", arr[1])\n               parameters.put(\"carrierRoleTypeId\", arr[2])\n            }", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        inputMap.put("orderId", context.get("orderId"));
        inputMap.put("contactMechPurposeTypeId", context.get("contactMechPurposeTypeId"));
        inputMap.put("contactMechId", context.get("contactMechId"));
        // TODO: Convert <find-by-and> element
        if (UtilValidate.isEmpty(context.get("orderContactMechList"))) {
            // set-service-fields from "parameters" to "createOrderContactMechMap" for service "createOrderContactMech"
            createOrderContactMechMap.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createOrderContactMech", createOrderContactMechMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createOrderContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> shipGroupLookupMap = new HashMap<>();
        shipGroupLookupMap.put("orderId", context.get("orderId"));
        shipGroupLookupMap.put("contactMechId", context.get("oldContactMechId"));
        // TODO: Convert <find-by-and> element
        if (UtilValidate.isEmpty(context.get("orderItemShipGroupList"))) {
            inputMap.put("orderId", context.get("orderId"));
            inputMap.put("contactMechPurposeTypeId", context.get("contactMechPurposeTypeId"));
            inputMap.put("contactMechId", context.get("oldContactMechId"));
            // TODO: Convert <find-by-and> element
            // set-service-fields from "inputMap" to "removeOrderContactMechMap" for service "createOrderContactMech"
            removeOrderContactMechMap.putAll(UtilMisc.toMap(inputMap));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("removeOrderContactMech", removeOrderContactMechMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling removeOrderContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Compute and return the OrderItemShipGroup estimated ship date based on the associated items.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getOrderItemShipGroupEstimatedShipDate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<Object> orderByList = null;
        GenericValue orderItemShipGroup = null;
        try {
            orderItemShipGroup = EntityQuery.use(delegator)
                    .from("OrderItemShipGroup")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemShipGroup: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("Y".equals(((Map<String, Object>) orderItemShipGroup).get("maySplit"))) {
            orderByList.add("+promisedDatetime");
        } else {
            orderByList.add("-promisedDatetime");
        }
        List<GenericValue> orderItemShipGroupInvResList = null;
        try {
            orderItemShipGroupInvResList = orderItemShipGroup.getRelated("OrderItemShipGrpInvRes", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue orderItemShipGroupInvRes = EntityUtil.getFirst((List<GenericValue>) orderItemShipGroupInvResList);
        result.put("estimatedShipDate", ((Map<String, Object>) orderItemShipGroupInvRes).get("promisedDatetime"));

        return "success";
    }


    /**
     * Create OrderContactMech
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOrderContactMech(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue newEntity = delegator.makeValue("OrderContactMech");
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
     * Remove OrderContactMech
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeOrderContactMech(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("OrderContactMech")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderContactMech: " + e.getMessage(), MODULE);
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
     * Update OrderNote
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateOrderNote(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue orderHeaderNote = null;
        try {
            orderHeaderNote = EntityQuery.use(delegator)
                    .from("OrderHeaderNote")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeaderNote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        orderHeaderNote.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(orderHeaderNote);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create an OrderTerm
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOrderTerm(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue newEntity = delegator.makeValue("OrderTerm");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.put("orderItemSeqId", "_NA_");
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
     * Update OrderTerm
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateOrderTerm(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("OrderTerm")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderTerm: " + e.getMessage(), MODULE);
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
     * Remove OrderTerm
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeOrderTerm(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("OrderTerm")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderTerm: " + e.getMessage(), MODULE);
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
     * Create an PaymentMethodToOrder
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String addPaymentMethodToOrder(HttpServletRequest request, HttpServletResponse response) {
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
        Map<String, Object> inputMap = new HashMap<>();
        inputMap.put("paymentMethodId", context.get("paymentMethodId"));
        inputMap.put("maxAmount", context.get("maxAmount"));
        inputMap.put("orderId", context.get("orderId"));
        GenericValue paymentMethod = null;
        try {
            paymentMethod = EntityQuery.use(delegator)
                    .from("PaymentMethod")
                    .where(UtilMisc.toMap("paymentMethodId", context.get("paymentMethodId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentMethod: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        inputMap.put("paymentMethodTypeId", ((Map<String, Object>) paymentMethod).get("paymentMethodTypeId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createOrderPaymentPreference", inputMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("orderPaymentPreferenceId", serviceResult.get("orderPaymentPreferenceId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createOrderPaymentPreference: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("orderPaymentPreferenceId", context.get("orderPaymentPreferenceId"));

        return "success";
    }


    /**
     * Gets an order status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getOrderStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue order = null;
        try {
            order = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(order)) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderOrderIdDoesNotExists", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        result.put("statusId", ((Map<String, Object>) order).get("statusId"));

        return "success";
    }


    /**
     * Check if an Order is on Back Order
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkOrderIsOnBackOrder(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Boolean isBackOrder = null;
        BigDecimal zeroEnv = BigDecimal.ZERO;
        List<GenericValue> orderItemShipGrpInvResList = null;
        try {
            orderItemShipGrpInvResList = EntityQuery.use(delegator)
                    .from("OrderItemShipGrpInvRes")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemShipGrpInvRes: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(orderItemShipGrpInvResList)) {
            isBackOrder = Boolean.FALSE;
        } else {
            isBackOrder = Boolean.TRUE;
        }
        result.put("isBackOrder", isBackOrder);

        return "success";
    }


    /**
     * Creates a new Order Item Change record
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOrderItemChange(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("OrderItemChange");
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(context.get("changeDatetime"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            newEntity.put("changeDatetime", nowTimestamp);
        }
        if (UtilValidate.isEmpty(context.get("changeUserLogin"))) {
            newEntity.put("changeUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        }
        ((GenericValue) newEntity).put("orderItemChangeId", delegator.getNextSeqId("OrderItemChange"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("orderItemChangeId", ((Map<String, Object>) newEntity).get("orderItemChangeId"));

        return "success";
    }


    /**
     * create and update Shipping address
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdateShippingAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createPartyPostalAddressCtx = null;
        GenericValue newValue = null;
        Map<String, Object> shipToAddressCtx = null;
        Map<String, Object> partyProfileDefaultsCtx = null;
        Map<String, Object> updatePartyPostalAddressCtx = null;
        List<GenericValue> pcmpShipList = null;
        GenericValue oldValue = null;
        Map<String, Object> serviceContext = null;
        List<GenericValue> pcmpList = null;
        Map<String, Object> serviceInMap = null;
        Object keepAddressBook = context.get("keepAddressBook");
        // TODO: Convert call-map-processor (in-map: parameters, out-map: shipToAddressCtx)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        shipToAddressCtx.put("partyId", context.get("partyId"));
        GenericValue pcmp = null;
        if (UtilValidate.isEmpty(((Map<String, Object>) shipToAddressCtx).get("contactMechId"))) {
            shipToAddressCtx.put("contactMechPurposeTypeId", "SHIPPING_LOCATION");
            // set-service-fields from "shipToAddressCtx" to "createPartyPostalAddressCtx" for service "createPartyPostalAddress"
            createPartyPostalAddressCtx.putAll(UtilMisc.toMap(shipToAddressCtx));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", createPartyPostalAddressCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("shipToContactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Shipping address created with contactMechId " + context.get("shipToContactMechId"), MODULE);
        } else {
            if ("Y".equals(keepAddressBook)) {
                newValue = delegator.makeValue("PostalAddress");
                newValue.setPKFields((Map<String, Object>) shipToAddressCtx);
                try {
                    oldValue = EntityQuery.use(delegator)
                            .from("PostalAddress")
                            .where(newValue)
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error finding by primary key PostalAddress: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                newValue.setNonPKFields((Map<String, Object>) shipToAddressCtx);
                if (!java.util.Objects.equals(oldValue, newValue)) {
                    shipToAddressCtx.remove("contactMechId");
                    // set-service-fields from "shipToAddressCtx" to "createPartyPostalAddressCtx" for service "createPartyPostalAddress"
                    createPartyPostalAddressCtx.putAll(UtilMisc.toMap(shipToAddressCtx));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", createPartyPostalAddressCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        context.put("shipToContactMechId", serviceResult.get("contactMechId"));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                try {
                    pcmpShipList = EntityQuery.use(delegator)
                            .from("PartyContactMechPurpose")
                            .where(UtilMisc.toMap("partyId", ((Map<String, Object>) userLogin).get("partyId"), "contactMechId", context.get("shipToContactMechId"), "contactMechPurposeTypeId", "SHIPPING_LOCATION"))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(pcmpShipList)) {
                    // set-service-fields from "parameters" to "serviceContext" for service "createPartyContactMechPurpose"
                    serviceContext.putAll(UtilMisc.toMap(context));
                    serviceContext.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                    try {
                        pcmpList = EntityQuery.use(delegator)
                                .from("PartyContactMechPurpose")
                                .where(UtilMisc.toMap("partyId", ((Map<String, Object>) userLogin).get("partyId"), "contactMechPurposeTypeId", "SHIPPING_LOCATION"))
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (pcmpList != null) {
                        for (GenericValue pcmpEntry : pcmpList) {
                            // set-service-fields from "pcmp" to "serviceInMap" for service "deletePartyContactMechPurposeIfExists"
                            serviceInMap.putAll(UtilMisc.toMap(pcmpEntry));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("deletePartyContactMechPurposeIfExists", serviceInMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling deletePartyContactMechPurposeIfExists: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            serviceInMap = null;
                        }
                    }
                    serviceContext.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                    serviceContext.put("contactMechId", context.get("shipToContactMechId"));
                    serviceContext.put("contactMechPurposeTypeId", "SHIPPING_LOCATION");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", serviceContext);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createPartyContactMechPurpose: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    pcmpList = null;
                    serviceContext = null;
                }
                if ("Y".equals(context.get("setDefaultShipping"))) {
                    // set-service-fields from "parameters" to "partyProfileDefaultsCtx" for service "setPartyProfileDefaults"
                    partyProfileDefaultsCtx.putAll(UtilMisc.toMap(context));
                    partyProfileDefaultsCtx.put("defaultShipAddr", context.get("shipToContactMechId"));
                    partyProfileDefaultsCtx.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("setPartyProfileDefaults", partyProfileDefaultsCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling setPartyProfileDefaults: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
            if ("N".equals(keepAddressBook)) {
                shipToAddressCtx.put("shipToContactMechId", ((Map<String, Object>) shipToAddressCtx).get("contactMechId"));
                if (java.util.Objects.equals(((Map<String, Object>) shipToAddressCtx).get("shipToContactMechId"), context.get("billToContactMechId"))) {
                    newValue = delegator.makeValue("PostalAddress");
                    newValue.setPKFields((Map<String, Object>) shipToAddressCtx);
                    try {
                        oldValue = EntityQuery.use(delegator)
                                .from("PostalAddress")
                                .where(newValue)
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error finding by primary key PostalAddress: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    newValue.setNonPKFields((Map<String, Object>) shipToAddressCtx);
                    if (!java.util.Objects.equals(oldValue, newValue)) {
                        try {
                            pcmpList = EntityQuery.use(delegator)
                                    .from("PartyContactMechPurpose")
                                    .where(UtilMisc.toMap("contactMechId", ((Map<String, Object>) shipToAddressCtx).get("shipToContactMechId"), "partyId", ((Map<String, Object>) userLogin).get("partyId"), "contactMechPurposeTypeId", "SHIPPING_LOCATION"))
                                    .filterByDate()
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (pcmpList != null) {
                            for (GenericValue pcmpEntry : pcmpList) {
                                // set-service-fields from "pcmp" to "serviceInMap" for service "deletePartyContactMechPurposeIfExists"
                                serviceInMap.putAll(UtilMisc.toMap(pcmpEntry));
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("deletePartyContactMechPurposeIfExists", serviceInMap);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                        return "error";
                                    }
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling deletePartyContactMechPurposeIfExists: " + e.getMessage(), MODULE);
                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                    return "error";
                                }
                            }
                        }
                        shipToAddressCtx.put("contactMechPurposeTypeId", "SHIPPING_LOCATION");
                        shipToAddressCtx.remove("contactMechId");
                        // set-service-fields from "shipToAddressCtx" to "createPartyPostalAddressCtx" for service "createPartyPostalAddress"
                        createPartyPostalAddressCtx.putAll(UtilMisc.toMap(shipToAddressCtx));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", createPartyPostalAddressCtx);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                            context.put("shipToContactMechId", serviceResult.get("contactMechId"));
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        Debug.logInfo("Shipping address updated with contactMechId " + ((Map<String, Object>) shipToAddressCtx).get("shipToContactMechId"), MODULE);
                    }
                } else {
                    shipToAddressCtx.put("userLogin", context.get("userLogin"));
                    // set-service-fields from "shipToAddressCtx" to "updatePartyPostalAddressCtx" for service "updatePartyPostalAddress"
                    updatePartyPostalAddressCtx.putAll(UtilMisc.toMap(shipToAddressCtx));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updatePartyPostalAddress", updatePartyPostalAddressCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        context.put("shipToContactMechId", serviceResult.get("contactMechId"));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updatePartyPostalAddress: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    Debug.logInfo("Shipping address updated with contactMechId " + ((Map<String, Object>) shipToAddressCtx).get("shipToContactMechId"), MODULE);
                }
            }
        }
        result.put("contactMechId", context.get("shipToContactMechId"));

        return "success";
    }


    /**
     * create and update billing address
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdateBillingAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> billToAddressCtx = null;
        Map<String, Object> serviceInMap = null;
        List<GenericValue> pcmpList = null;
        Map<String, Object> createPartyPostalAddressCtx = null;
        GenericValue newValue = null;
        Map<String, Object> updatePartyPostalAddressCtx = null;
        Map<String, Object> partyProfileDefaultsCtx = null;
        List<GenericValue> pcmpBillList = null;
        GenericValue oldValue = null;
        Map<String, Object> serviceContext = null;
        Object keepAddressBook = context.get("keepAddressBook");
        if (!"Y".equals(context.get("useShippingAddressForBilling"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: billToAddressCtx)
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object partyId = context.get("partyId");
        billToAddressCtx.put("partyId", partyId);
        if ("Y".equals(context.get("useShippingAddressForBilling"))) {
            GenericValue pcmp = null;
            if (UtilValidate.isEmpty(context.get("billToContactMechId"))) {
                billToAddressCtx.put("contactMechPurposeTypeId", "BILLING_LOCATION");
                // set-service-fields from "billToAddressCtx" to "serviceInMap" for service "createPartyContactMechPurpose"
                serviceInMap.putAll(UtilMisc.toMap(billToAddressCtx));
                serviceInMap.put("contactMechId", context.get("shipToContactMechId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", serviceInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyContactMechPurpose: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                if (!java.util.Objects.equals(context.get("shipToContactMechId"), context.get("billToContactMechId"))) {
                    try {
                        pcmpList = EntityQuery.use(delegator)
                                .from("PartyContactMechPurpose")
                                .where(UtilMisc.toMap("contactMechId", context.get("billToContactMechId"), "partyId", partyId, "contactMechPurposeTypeId", "BILLING_LOCATION"))
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (pcmpList != null) {
                        for (GenericValue pcmpEntry : pcmpList) {
                            // set-service-fields from "pcmp" to "serviceInMap" for service "deletePartyContactMechPurposeIfExists"
                            serviceInMap.putAll(UtilMisc.toMap(pcmpEntry));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("deletePartyContactMechPurposeIfExists", serviceInMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling deletePartyContactMechPurposeIfExists: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            serviceInMap = null;
                        }
                    }
                    if ("N".equals(keepAddressBook)) {
                        serviceInMap.put("contactMechId", context.get("billToContactMechId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("deletePartyContactMech", serviceInMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling deletePartyContactMech: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        serviceInMap = null;
                    }
                    try {
                        pcmpList = EntityQuery.use(delegator)
                                .from("PartyContactMechPurpose")
                                .where(UtilMisc.toMap("contactMechId", context.get("shipToContactMechId"), "partyId", partyId, "contactMechPurposeTypeId", "BILLING_LOCATION"))
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isEmpty(pcmpList)) {
                        billToAddressCtx.put("contactMechPurposeTypeId", "BILLING_LOCATION");
                        // set-service-fields from "billToAddressCtx" to "serviceInMap" for service "createPartyContactMechPurpose"
                        serviceInMap.putAll(UtilMisc.toMap(billToAddressCtx));
                        serviceInMap.put("contactMechId", context.get("shipToContactMechId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", serviceInMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createPartyContactMechPurpose: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                    Debug.logInfo("Billing address updated with contactMechId " + context.get("billToContactMechId"), MODULE);
                }
            }
            context.put("billToContactMechId", context.get("shipToContactMechId"));
        }
        if (!"Y".equals(context.get("useShippingAddressForBilling"))) {
            if (UtilValidate.isEmpty(context.get("billToContactMechId"))) {
                billToAddressCtx.put("contactMechPurposeTypeId", "BILLING_LOCATION");
                // set-service-fields from "billToAddressCtx" to "createPartyPostalAddressCtx" for service "createPartyPostalAddress"
                createPartyPostalAddressCtx.putAll(UtilMisc.toMap(billToAddressCtx));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", createPartyPostalAddressCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("billToContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                Debug.logInfo("Billing address created with contactmechId " + context.get("billToContactMechId"), MODULE);
            } else {
                if (java.util.Objects.equals(context.get("shipToContactMechId"), context.get("billToContactMechId"))) {
                    billToAddressCtx.remove("contactMechId");
                    // set-service-fields from "billToAddressCtx" to "createPartyPostalAddressCtx" for service "createPartyPostalAddress"
                    createPartyPostalAddressCtx.putAll(UtilMisc.toMap(billToAddressCtx));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", createPartyPostalAddressCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        context.put("billToContactMechId", serviceResult.get("contactMechId"));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    Debug.logInfo("Billing address updated with contactMechId " + context.get("billToContactMechId"), MODULE);
                } else {
                    if ("N".equals(keepAddressBook)) {
                        // set-service-fields from "billToAddressCtx" to "updatePartyPostalAddressCtx" for service "updatePartyPostalAddress"
                        updatePartyPostalAddressCtx.putAll(UtilMisc.toMap(billToAddressCtx));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("updatePartyPostalAddress", updatePartyPostalAddressCtx);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                            context.put("billToContactMechId", serviceResult.get("contactMechId"));
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling updatePartyPostalAddress: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                    if ("Y".equals(keepAddressBook)) {
                        newValue = delegator.makeValue("PostalAddress");
                        newValue.setPKFields((Map<String, Object>) billToAddressCtx);
                        newValue.setNonPKFields((Map<String, Object>) billToAddressCtx);
                        try {
                            oldValue = EntityQuery.use(delegator)
                                    .from("PostalAddress")
                                    .where(UtilMisc.toMap("contactMechId", ((Map<String, Object>) billToAddressCtx).get("contactMechId")))
                                    .queryOne();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying PostalAddress: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (!java.util.Objects.equals(oldValue, newValue)) {
                            billToAddressCtx.remove("contactMechId");
                            // set-service-fields from "billToAddressCtx" to "createPartyPostalAddressCtx" for service "createPartyPostalAddress"
                            createPartyPostalAddressCtx.putAll(UtilMisc.toMap(billToAddressCtx));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", createPartyPostalAddressCtx);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                                context.put("billToContactMechId", serviceResult.get("contactMechId"));
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                    }
                    Debug.logInfo("Billing Postal Address created billToContactMechId is " + context.get("billToContactMechId"), MODULE);
                }
                try {
                    pcmpBillList = EntityQuery.use(delegator)
                            .from("PartyContactMechPurpose")
                            .where(UtilMisc.toMap("partyId", ((Map<String, Object>) userLogin).get("partyId"), "contactMechId", context.get("billToContactMechId"), "contactMechPurposeTypeId", "BILLING_LOCATION"))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(pcmpBillList)) {
                    // set-service-fields from "parameters" to "serviceContext" for service "createPartyContactMechPurpose"
                    serviceContext.putAll(UtilMisc.toMap(context));
                    serviceContext.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                    try {
                        pcmpList = EntityQuery.use(delegator)
                                .from("PartyContactMechPurpose")
                                .where(UtilMisc.toMap("partyId", ((Map<String, Object>) userLogin).get("partyId"), "contactMechPurposeTypeId", "BILLING_LOCATION"))
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (pcmpList != null) {
                        for (GenericValue pcmpEntry : pcmpList) {
                            // set-service-fields from "pcmp" to "serviceInMap" for service "deletePartyContactMechPurposeIfExists"
                            serviceInMap.putAll(UtilMisc.toMap(pcmpEntry));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("deletePartyContactMechPurposeIfExists", serviceInMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling deletePartyContactMechPurposeIfExists: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            serviceInMap = null;
                        }
                    }
                    serviceContext.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                    serviceContext.put("contactMechId", context.get("billToContactMechId"));
                    serviceContext.put("contactMechPurposeTypeId", "BILLING_LOCATION");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", serviceContext);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createPartyContactMechPurpose: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    pcmpList = null;
                    serviceContext = null;
                }
                if ("Y".equals(context.get("setDefaultBilling"))) {
                    // set-service-fields from "parameters" to "partyProfileDefaultsCtx" for service "setPartyProfileDefaults"
                    partyProfileDefaultsCtx.putAll(UtilMisc.toMap(context));
                    partyProfileDefaultsCtx.put("defaultBillAddr", context.get("billToContactMechId"));
                    partyProfileDefaultsCtx.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("setPartyProfileDefaults", partyProfileDefaultsCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling setPartyProfileDefaults: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        result.put("contactMechId", context.get("billToContactMechId"));

        return "success";
    }


    /**
     * create and update credit card
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdateCreditCard(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> creditCardContext = null;
        GenericValue paymentMethod = null;
        List<GenericValue> paymentMethodList = null;
        // set-service-fields from "parameters" to "creditCardContext" for service "createCreditCard"
        creditCardContext.putAll(UtilMisc.toMap(context));
        creditCardContext.put("partyId", context.get("partyId"));
        creditCardContext.put("contactMechId", context.get("contactMechId"));
        if (UtilValidate.isEmpty(context.get("paymentMethodId"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createCreditCard", creditCardContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("paymentMethodId", serviceResult.get("paymentMethodId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createCreditCard: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            try {
                paymentMethodList = EntityQuery.use(delegator)
                        .from("PaymentMethod")
                        .where(UtilMisc.toMap("partyId", context.get("partyId"), "paymentMethodTypeId", "CREDIT_CARD"))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentMethod: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            paymentMethod = EntityUtil.getFirst((List<GenericValue>) paymentMethodList);
            creditCardContext.put("paymentMethodId", ((Map<String, Object>) paymentMethod).get("paymentMethodId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateCreditCard", creditCardContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("paymentMethodId", serviceResult.get("paymentMethodId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateCreditCard: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        Object paymentMethodId = context.get("paymentMethodId");
        result.put("paymentMethodId", context.get("paymentMethodId"));

        return "success";
    }


    /**
     * Set unitPrice as lastPrice on create purchase order, edit purchase order items and on receive inventory against a purchase order
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setUnitPriceAsLastPrice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue orderSupplier = null;
        List<GenericValue> supplierProducts = null;
        List<GenericValue> orderSuppliers = null;
        GenericValue newSupplierProduct = null;
        Timestamp nowTimestamp = null;
        List<GenericValue> orderItems = null;
        Object newSupplierProduct_availableFromDate = null;
        BigDecimal newSupplierProduct_lastPrice = null;
        Object supplierProduct_availableThruDate = null;
        BigDecimal orderItem_unitPrice = null;
        if ((!(UtilValidate.isEmpty(context.get("facilityId"))) && !(UtilValidate.isEmpty(context.get("orderId"))))) {
            try {
                orderSuppliers = EntityQuery.use(delegator)
                        .from("OrderHeaderItemAndRoles")
                        .where(UtilMisc.toMap("orderId", context.get("orderId"), "roleTypeId", "BILL_FROM_VENDOR", "orderTypeId", "PURCHASE_ORDER"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderHeaderItemAndRoles: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            orderSupplier = EntityUtil.getFirst((List<GenericValue>) orderSuppliers);
            try {
                supplierProducts = EntityQuery.use(delegator)
                        .from("SupplierProduct")
                        .where(UtilMisc.toMap("productId", context.get("productId"), "partyId", ((Map<String, Object>) orderSupplier).get("partyId"), "availableThruDate", null))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying SupplierProduct: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (supplierProducts != null) {
                for (GenericValue supplierProduct : supplierProducts) {
                    nowTimestamp = new Timestamp(System.currentTimeMillis());
                    if (UtilValidate.isNotEmpty(context.get("orderCurrencyUnitPrice"))) {
                        if (!java.util.Objects.equals(context.get("orderCurrencyUnitPrice"), ((Map<String, Object>) supplierProduct).get("lastPrice"))) {
                            newSupplierProduct = GenericValue.create((GenericValue) supplierProduct);
                            newSupplierProduct.put("availableFromDate", nowTimestamp);
                            newSupplierProduct.put("lastPrice", context.get("orderCurrencyUnitPrice"));
                            try {
                                delegator.create(newSupplierProduct);
                            } catch (Exception e) {
                                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            supplierProduct.put("availableThruDate", nowTimestamp);
                            try {
                                delegator.store(supplierProduct);
                            } catch (Exception e) {
                                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                    } else {
                        if (!java.util.Objects.equals(context.get("unitCost"), ((Map<String, Object>) supplierProduct).get("lastPrice"))) {
                            newSupplierProduct = GenericValue.create((GenericValue) supplierProduct);
                            newSupplierProduct.put("availableFromDate", nowTimestamp);
                            newSupplierProduct.put("lastPrice", context.get("unitCost"));
                            try {
                                delegator.create(newSupplierProduct);
                            } catch (Exception e) {
                                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            supplierProduct.put("availableThruDate", nowTimestamp);
                            try {
                                delegator.store(supplierProduct);
                            } catch (Exception e) {
                                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                    }
                }
            }
        } else {
            if ((UtilValidate.isEmpty(context.get("orderItems")) && !(UtilValidate.isEmpty(context.get("orderId"))))) {
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
                if (orderItems != null) {
                    for (GenericValue orderItem : orderItems) {
                        for (Map.Entry<String, Object> entry : ((Map<String, Object>) context.get("itemPriceMap")).entrySet()) {
                            String orderItemSeqId = entry.getKey();
                            Object unitPrice = entry.getValue();
                            if (java.util.Objects.equals(((Map<String, Object>) orderItem).get("orderItemSeqId"), orderItemSeqId)) {
                                for (Map.Entry<String, Object> entry2 : ((Map<String, Object>) context.get("overridePriceMap")).entrySet()) {
                                    orderItemSeqId = entry2.getKey();
                                    Object Y = entry2.getValue();
                                    if (java.util.Objects.equals(((Map<String, Object>) orderItem).get("orderItemSeqId"), orderItemSeqId)) {
                                        orderItem.put("unitPrice", unitPrice);
                                        try {
                                            supplierProducts = EntityQuery.use(delegator)
                                                    .from("SupplierProduct")
                                                    .where(UtilMisc.toMap("productId", ((Map<String, Object>) orderItem).get("productId"), "partyId", context.get("supplierPartyId"), "availableThruDate", null))
                                                    .queryList();
                                        } catch (Exception e) {
                                            Debug.logError(e, "Error querying SupplierProduct: " + e.getMessage(), MODULE);
                                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                            return "error";
                                        }
                                        if (supplierProducts != null) {
                                            for (GenericValue supplierProductEntry : supplierProducts) {
                                                nowTimestamp = new Timestamp(System.currentTimeMillis());
                                                if (!java.util.Objects.equals(((Map<String, Object>) orderItem).get("unitPrice"), ((Map<String, Object>) supplierProductEntry).get("lastPrice"))) {
                                                    newSupplierProduct = delegator.makeValue("SupplierProduct");
                                                    newSupplierProduct = GenericValue.create((GenericValue) supplierProductEntry);
                                                    newSupplierProduct.put("availableFromDate", nowTimestamp);
                                                    newSupplierProduct.put("lastPrice", ((Map<String, Object>) orderItem).get("unitPrice"));
                                                    try {
                                                        delegator.create(newSupplierProduct);
                                                    } catch (Exception e) {
                                                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                                        return "error";
                                                    }
                                                    supplierProductEntry.put("availableThruDate", nowTimestamp);
                                                    try {
                                                        delegator.store(supplierProductEntry);
                                                    } catch (Exception e) {
                                                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
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
                    }
                }
            } else {
                if (context.get("orderItems") != null) {
                    for (GenericValue orderItemEntry : (List<GenericValue>) context.get("orderItems")) {
                        try {
                            supplierProducts = EntityQuery.use(delegator)
                                    .from("SupplierProduct")
                                    .where(UtilMisc.toMap("productId", ((Map<String, Object>) orderItemEntry).get("productId"), "partyId", context.get("supplierPartyId"), "availableThruDate", null))
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying SupplierProduct: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (supplierProducts != null) {
                            for (GenericValue supplierProductEntry : supplierProducts) {
                                nowTimestamp = new Timestamp(System.currentTimeMillis());
                                if (!java.util.Objects.equals(((Map<String, Object>) orderItemEntry).get("unitPrice"), ((Map<String, Object>) supplierProductEntry).get("lastPrice"))) {
                                    newSupplierProduct = delegator.makeValue("SupplierProduct");
                                    newSupplierProduct = GenericValue.create((GenericValue) supplierProductEntry);
                                    newSupplierProduct.put("availableFromDate", nowTimestamp);
                                    newSupplierProduct.put("lastPrice", ((Map<String, Object>) orderItemEntry).get("unitPrice"));
                                    try {
                                        delegator.create(newSupplierProduct);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                    supplierProductEntry.put("availableThruDate", nowTimestamp);
                                    try {
                                        delegator.store(supplierProductEntry);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
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
     * Cancels those back orders from suppliers whose cancel back order date (cancelBackOrderDate) has passed the current date
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String cancelAllBackOrders(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> orderItemMap = null;
        Timestamp backOrderDate = null;
        List<GenericValue> orderItems = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        List<GenericValue> orders = null;
        try {
            orders = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (orders != null) {
            for (GenericValue currentOrder : orders) {
                try {
                    orderItems = EntityQuery.use(delegator)
                            .from("OrderItem")
                            .where(UtilMisc.toMap("orderId", ((Map<String, Object>) currentOrder).get("orderId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying OrderItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (orderItems != null) {
                    for (GenericValue currentOrderItem : orderItems) {
                        backOrderDate = (Timestamp) ((Map<String, Object>) currentOrderItem).get("cancelBackOrderDate");
                        Object orderItemMap_orderId = null;
                        Object orderItemMap_orderItemSeqId = null;
                        if ((!(UtilValidate.isEmpty(backOrderDate)) && nowTimestamp != null /* TODO: field compare operator greater */)) {
                            orderItemMap.put("orderId", ((Map<String, Object>) currentOrder).get("orderId"));
                            orderItemMap.put("orderItemSeqId", ((Map<String, Object>) currentOrderItem).get("orderItemSeqId"));
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
            }
        }

        return "success";
    }


    /**
     * Updates shipping method and shipping charges from Order View page when Shipment is in picked status and items of Order are packed
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateShippingMethodAndCharges(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        BigDecimal diffPercentage = null;
        Map<String, Object> updateOrderAdjustmentContext = null;
        Map<String, Object> updateOrderItemShipGroupContext = null;
        Map<String, Object> upsShipmentConfirmContext = null;
        Map<String, Object> updateShipmentRouteSegmentContext = null;
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
            Object scriptResult = GroovyUtil.eval("shipmentMethodAndAmount = parameters.get(\"shipmentMethodAndAmount\")\n            if (shipmentMethodAndAmount != null) {\n               parameters.put(\"shipmentMethod\", shipmentMethodAndAmount.substring(0, shipmentMethodAndAmount.indexOf(\"*\")))\n               parameters.put(\"amount\", shipmentMethodAndAmount.substring(shipmentMethodAndAmount.indexOf(\"*\")+1))\n               parameters.put(\"shipmentMethodTypeId\", shipmentMethodAndAmount.substring(0, shipmentMethodAndAmount.indexOf(\"@\")))\n            }", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        BigDecimal newAmount = (BigDecimal) context.get("amount");
        BigDecimal shippingAmount = (BigDecimal) context.get("shippingAmount");
        String percentAllowed = UtilProperties.getMessage("shipment", "shipment.default.cost_actual_over_estimated_percent_allowed", locale);
        if (newAmount != null /* TODO: field compare operator greater */) {
            diffPercentage = (BigDecimal) ((BigDecimal) context.get("(newAmount")).subtract((BigDecimal) context.get("shippingAmount/shippingAmount)*100"));
        } else {
            diffPercentage = (BigDecimal) ((BigDecimal) context.get("(shippingAmount")).subtract((BigDecimal) context.get("newAmount/newAmount)*100"));
        }
        if (diffPercentage != null /* TODO: field compare operator greater */) {
            // set-service-fields from "parameters" to "updateOrderItemShipGroupContext" for service "updateOrderItemShipGroup"
            updateOrderItemShipGroupContext.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateOrderItemShipGroup", updateOrderItemShipGroupContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateOrderItemShipGroup: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // set-service-fields from "parameters" to "updateOrderAdjustmentContext" for service "updateOrderAdjustment"
            updateOrderAdjustmentContext.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateOrderAdjustment", updateOrderAdjustmentContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateOrderAdjustment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // set-service-fields from "parameters" to "updateShipmentRouteSegmentContext" for service "updateShipmentRouteSegment"
            updateShipmentRouteSegmentContext.putAll(UtilMisc.toMap(context));
            updateShipmentRouteSegmentContext.remove("trackingIdNumber");
            updateShipmentRouteSegmentContext.remove("trackingDigest");
            updateShipmentRouteSegmentContext.remove("carrierServiceStatusId");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateShipmentRouteSegment", updateShipmentRouteSegmentContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateShipmentRouteSegment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // set-service-fields from "parameters" to "upsShipmentConfirmContext" for service "upsShipmentConfirm"
            upsShipmentConfirmContext.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("upsShipmentConfirm", upsShipmentConfirmContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling upsShipmentConfirm: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            // set-service-fields from "parameters" to "updateOrderItemShipGroupContext" for service "updateOrderItemShipGroup"
            updateOrderItemShipGroupContext.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateOrderItemShipGroup", updateOrderItemShipGroupContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateOrderItemShipGroup: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // set-service-fields from "parameters" to "updateShipmentRouteSegmentContext" for service "updateShipmentRouteSegment"
            updateShipmentRouteSegmentContext.putAll(UtilMisc.toMap(context));
            updateShipmentRouteSegmentContext.remove("trackingIdNumber");
            updateShipmentRouteSegmentContext.remove("trackingDigest");
            updateShipmentRouteSegmentContext.remove("carrierServiceStatusId");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateShipmentRouteSegment", updateShipmentRouteSegmentContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateShipmentRouteSegment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // set-service-fields from "parameters" to "upsShipmentConfirmContext" for service "upsShipmentConfirm"
            upsShipmentConfirmContext.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("upsShipmentConfirm", upsShipmentConfirmContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling upsShipmentConfirm: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Calculate ATP and Qoh According For each facility
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String productAvailabalityByFacility(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<Object> availabalityList = null;
        Object quantityOnHandTotal = null;
        Map<String, Object> availabalityMap = null;
        Map<String, Object> getInventoryAvailableByFacilityMap = null;
        Object availableToPromiseTotal = null;
        Map<String, Object> facilityMap = new HashMap<>();
        facilityMap.put("ownerPartyId", context.get("ownerPartyId"));
        // TODO: Convert <find-by-and> element
        if (context.get("facilityList") != null) {
            for (Object facility : (List<Object>) context.get("facilityList")) {
                getInventoryAvailableByFacilityMap.put("facilityId", ((Map<String, Object>) facility).get("facilityId"));
                getInventoryAvailableByFacilityMap.put("productId", context.get("productId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("getInventoryAvailableByFacility", getInventoryAvailableByFacilityMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    quantityOnHandTotal = serviceResult.get("quantityOnHandTotal");
                    availableToPromiseTotal = serviceResult.get("availableToPromiseTotal");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling getInventoryAvailableByFacility: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                availabalityMap.put("facilityId", ((Map<String, Object>) facility).get("facilityId"));
                availabalityMap.put("quantityOnHandTotal", quantityOnHandTotal);
                availabalityMap.put("availableToPromiseTotal", availableToPromiseTotal);
                availabalityList.add(availabalityMap);
                availabalityMap = null;
            }
        }
        result.put("availabalityList", availabalityList);

        return "success";
    }


    /**
     * Create Order Payment Application
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOrderPaymentApplication(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createCtx = null;
        GenericValue paymentMap = null;
        try {
            paymentMap = EntityQuery.use(delegator)
                    .from("Payment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        createCtx.put("amountApplied", ((Map<String, Object>) paymentMap).get("amount"));
        createCtx.put("paymentId", ((Map<String, Object>) paymentMap).get("paymentId"));
        GenericValue orderPaymentPreMap = null;
        try {
            orderPaymentPreMap = EntityQuery.use(delegator)
                    .from("OrderPaymentPreference")
                    .where(UtilMisc.toMap("orderPaymentPreferenceId", ((Map<String, Object>) paymentMap).get("paymentPreferenceId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderPaymentPreference: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> orderItemBilList = null;
        try {
            orderItemBilList = EntityQuery.use(delegator)
                    .from("OrderItemBilling")
                    .where(UtilMisc.toMap("orderId", ((Map<String, Object>) orderPaymentPreMap).get("orderId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemBilling: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(orderItemBilList)) {
            createCtx.put("invoiceId", ((GenericValue) ((List<?>) orderItemBilList).get(0)).get("invoiceId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPaymentApplication", createCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPaymentApplication: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Move order items between ship groups
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String MoveItemBetweenShipGroups(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Map<String, Object> map = null;
        GenericValue orderItemShipGroupAssoc = null;
        try {
            orderItemShipGroupAssoc = EntityQuery.use(delegator)
                    .from("OrderItemShipGroupAssoc")
                    .where(UtilMisc.toMap("orderId", context.get("orderId"), "orderItemSeqId", context.get("orderItemSeqId"), "shipGroupSeqId", context.get("toGroupIndex")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemShipGroupAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(orderItemShipGroupAssoc)) {
            // set-service-fields from "parameters" to "map" for service "addOrderItemShipGroupAssoc"
            map.putAll(UtilMisc.toMap(context));
            map.put("quantity", BigDecimal.ZERO);
            map.put("shipGroupSeqId", null);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("addOrderItemShipGroupAssoc", map);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling addOrderItemShipGroupAssoc: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                orderItemShipGroupAssoc = EntityQuery.use(delegator)
                        .from("OrderItemShipGroupAssoc")
                        .where(UtilMisc.toMap("orderId", context.get("orderId"), "orderItemSeqId", context.get("orderItemSeqId"), "shipGroupSeqId", context.get("toGroupIndex")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderItemShipGroupAssoc: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        map = null;
        map.put("orderId", context.get("orderId"));
        map.put("orderItemSeqId", context.get("orderItemSeqId"));
        map.put("shipGroupSeqId", context.get("toGroupIndex"));
        map.put("quantity", (BigDecimal) ((BigDecimal) ((Map<String, Object>) orderItemShipGroupAssoc).get("quantity")).add((BigDecimal) context.get("quantity")));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateOrderItemShipGroupAssoc", map);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateOrderItemShipGroupAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            orderItemShipGroupAssoc = EntityQuery.use(delegator)
                    .from("OrderItemShipGroupAssoc")
                    .where(UtilMisc.toMap("orderId", context.get("orderId"), "orderItemSeqId", context.get("orderItemSeqId"), "shipGroupSeqId", context.get("fromGroupIndex")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemShipGroupAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(orderItemShipGroupAssoc)) {
            error_list.add("The orderItemShipGroupAssoc qualified by orderId=${parameters.orderId} orderItemSeqId=${parameters.orderItemSeqId} shipGroupSeqId=${parameters.fromGroupIndex} does not exist");
            request.setAttribute("_ERROR_MESSAGE_", "The orderItemShipGroupAssoc qualified by orderId=${parameters.orderId} orderItemSeqId=${parameters.orderItemSeqId} shipGroupSeqId=${parameters.fromGroupIndex} does not exist");
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        map = null;
        map.put("orderId", context.get("orderId"));
        map.put("orderItemSeqId", context.get("orderItemSeqId"));
        map.put("shipGroupSeqId", context.get("fromGroupIndex"));
        map.put("quantity", (BigDecimal) ((BigDecimal) ((Map<String, Object>) orderItemShipGroupAssoc).get("quantity")).subtract((BigDecimal) context.get("quantity")));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateOrderItemShipGroupAssoc", map);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateOrderItemShipGroupAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }

}

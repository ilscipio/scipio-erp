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
 * <p>Generated from: component://order/script/org/ofbiz/order/shoppinglist/ShoppingListServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ShoppingListServices {

    private static final String MODULE = ShoppingListServices.class.getName();


    /**
     * Create a ShoppingList
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createShoppingList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        if ((!(UtilValidate.isEmpty(userLogin)) && !("anonymous".equals(((Map<String, Object>) userLogin).get("userLoginId"))) && !(UtilValidate.isEmpty(context.get("partyId"))) && !(java.util.Objects.equals(context.get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) && !(true /* TODO: if-has-permission */))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderSecurityErrorToRunCreateShoppingListForAnotherParty", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        newEntity = delegator.makeValue("ShoppingList");
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("partyId"))) {
            newEntity.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("shoppingListTypeId"))) {
            newEntity.put("shoppingListTypeId", "SLT_WISH_LIST");
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("listName"))) {
            String newEntity_listName = UtilProperties.getMessage("OrderUiLabels", "OrderNewShoppingList", locale);
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("isPublic"))) {
            newEntity.put("isPublic", "N");
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("isActive"))) {
            if ("SLT_AUTO_REODR".equals(((Map<String, Object>) newEntity).get("shoppingListTypeId"))) {
                newEntity.put("isActive", "N");
            } else {
                newEntity.put("isActive", "Y");
            }
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("partyId"))) {
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
                Object scriptResult = GroovyUtil.eval("newEntity.shoppingListAuthToken = org.ofbiz.order.shoppinglist.ShoppingListWorker.generateShoppingListAuthToken(delegator);", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            result.put("shoppingListAuthToken", ((Map<String, Object>) newEntity).get("shoppingListAuthToken"));
        }
        ((GenericValue) newEntity).put("shoppingListId", delegator.getNextSeqId("ShoppingList"));
        result.put("shoppingListId", ((Map<String, Object>) newEntity).get("shoppingListId"));
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
     * Update a ShoppingList
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateShoppingList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue shoppingList = null;
        try {
            shoppingList = EntityQuery.use(delegator)
                    .from("ShoppingList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object parentMethodName = "updateShoppingList";
        Object permissionAction = "UPDATE";
        String result = checkShoppingListSecurity(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (!(true /* TODO: if-has-permission */)) {
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
                Object scriptResult = GroovyUtil.eval("parameters.remove(\"partyId\");\n                    parameters.remove(\"shoppingListAuthToken\");", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
        }
        shoppingList.setNonPKFields((Map<String, Object>) context);
        Object shoppingList_isActive = null;
        if (("SLT_AUTO_REODR".equals(((Map<String, Object>) shoppingList).get("shoppingListTypeId")) && (UtilValidate.isEmpty(((Map<String, Object>) shoppingList).get("recurrenceInfoId")) || UtilValidate.isEmpty(((Map<String, Object>) shoppingList).get("paymentMethodId")) || UtilValidate.isEmpty(((Map<String, Object>) shoppingList).get("contactMechId")) || UtilValidate.isEmpty(((Map<String, Object>) shoppingList).get("shipmentMethodTypeId"))))) {
            shoppingList.put("isActive", "N");
        }
        try {
            delegator.store(shoppingList);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Remove a ShoppingList
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeShoppingList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue shoppingList = null;
        try {
            shoppingList = EntityQuery.use(delegator)
                    .from("ShoppingList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object parentMethodName = "removeShoppingList";
        Object permissionAction = "DELETE";
        String result = checkShoppingListSecurity(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(shoppingList);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create a ShoppingList Item
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createShoppingListItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object parentMethodName = null;
        GenericValue product = null;
        Map<String, Object> shoppingListItemParameters = null;
        GenericValue newEntity = null;
        GenericValue shoppingListItem = null;
        Object permissionAction = null;
        Object totalquantity = null;
        List<GenericValue> shoppingListItems = null;
        try {
            shoppingListItems = EntityQuery.use(delegator)
                    .from("ShoppingListItem")
                    .where(UtilMisc.toMap("productId", context.get("productId"), "shoppingListId", context.get("shoppingListId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingListItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(shoppingListItems)) {
            parentMethodName = "createShoppingListItem";
            permissionAction = "CREATE";
            String checkResult = checkShoppingListItemSecurity(request, response);
            if (!"success".equals(checkResult)) {
                return checkResult;
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
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
            if (UtilValidate.isEmpty(product)) {
                {
                    String errorMsg = UtilProperties.getMessage("ProductUiLabels", "ProductErrorProductNotFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            newEntity = delegator.makeValue("ShoppingListItem");
            newEntity.put("shoppingListId", context.get("shoppingListId"));
            delegator.setNextSubSeqId(newEntity, "shoppingListItemSeqId", 5, 1);
            Object shoppingListItemSeqId = newEntity.get("shoppingListItemSeqId");
            newEntity.setNonPKFields((Map<String, Object>) context);
            result.put("shoppingListId", ((Map<String, Object>) newEntity).get("shoppingListId"));
            result.put("shoppingListItemSeqId", ((Map<String, Object>) newEntity).get("shoppingListItemSeqId"));
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Object shoppingList = null;
            if (!java.util.Objects.equals(((Map<String, Object>) shoppingList).get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) {
                Timestamp shoppingList_lastAdminModified = new Timestamp(System.currentTimeMillis());
                try {
                    delegator.store((GenericValue) shoppingList);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        } else {
            shoppingListItem = EntityUtil.getFirst((List<GenericValue>) shoppingListItems);
            totalquantity = new BigDecimal(context.get("quantity").toString());
            result.put("shoppingListItemSeqId", ((Map<String, Object>) shoppingListItem).get("shoppingListItemSeqId"));
            // set-service-fields from "shoppingListItem" to "shoppingListItemParameters" for service "updateShoppingListItem"
            shoppingListItemParameters.putAll(UtilMisc.toMap(shoppingListItem));
            shoppingListItemParameters.put("quantity", totalquantity);
            shoppingListItemParameters.put("shoppingListAuthToken", context.get("shoppingListAuthToken"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateShoppingListItem", shoppingListItemParameters);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateShoppingListItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("shoppingListId", ((Map<String, Object>) shoppingListItem).get("shoppingListId"));
        }

        return "success";
    }


    /**
     * Update a ShoppingListItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateShoppingListItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object parentMethodName = "updateShoppingListItem";
        Object permissionAction = "UPDATE";
        String result = checkShoppingListItemSecurity(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue shoppingList = null;
        try {
            shoppingList = EntityQuery.use(delegator)
                    .from("ShoppingList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue shoppingListItem = null;
        try {
            shoppingListItem = EntityQuery.use(delegator)
                    .from("ShoppingListItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingListItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        shoppingListItem.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(shoppingListItem);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (!java.util.Objects.equals(((Map<String, Object>) shoppingList).get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) {
            Timestamp shoppingList_lastAdminModified = new Timestamp(System.currentTimeMillis());
            try {
                delegator.store(shoppingList);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Remove a ShoppingListItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeShoppingListItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object parentMethodName = "removeShoppingListItem";
        Object permissionAction = "DELETE";
        String result = checkShoppingListItemSecurity(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue shoppingList = null;
        try {
            shoppingList = EntityQuery.use(delegator)
                    .from("ShoppingList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue shoppingListItem = null;
        try {
            shoppingListItem = EntityQuery.use(delegator)
                    .from("ShoppingListItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingListItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(shoppingListItem);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (!java.util.Objects.equals(((Map<String, Object>) shoppingList).get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) {
            Timestamp shoppingList_lastAdminModified = new Timestamp(System.currentTimeMillis());
            try {
                delegator.store(shoppingList);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Adds a shopping list item if one with the same productId does not exist
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String addDistinctShoppingListItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<GenericValue> shoppingListItemList = null;
        try {
            shoppingListItemList = EntityQuery.use(delegator)
                    .from("ShoppingListItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingListItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (shoppingListItemList != null) {
            for (GenericValue shoppingListItem : shoppingListItemList) {
                if (java.util.Objects.equals(context.get("productId"), ((Map<String, Object>) shoppingListItem).get("productId"))) {
                    result.put("shoppingListItemSeqId", ((Map<String, Object>) shoppingListItem).get("shoppingListItemSeqId"));
                    return "success";
                }
            }
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createShoppingListItem", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createShoppingListItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Calculate Deep Total Price for a ShoppingList
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String calculateShoppingListDeepTotalPrice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue product = null;
        Object totalPrice = null;
        Object itemPrice = null;
        Map<String, Object> calcPriceInMap = null;
        Map<String, Object> calcChildPriceInMap = null;
        Map<String, Object> calcPriceOutMap = new HashMap<>();
        Object parentMethodName = "calculateShoppingListDeepTotalPrice";
        Object permissionAction = "VIEW";
        String inlineResult = checkShoppingListItemSecurity(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> calcPriceInBaseMap = new HashMap<>();
        calcPriceInBaseMap.put("prodCatalogId", context.get("prodCatalogId"));
        calcPriceInBaseMap.put("webSiteId", context.get("webSiteId"));
        calcPriceInBaseMap.put("partyId", context.get("partyId"));
        calcPriceInBaseMap.put("productStoreId", context.get("productStoreId"));
        calcPriceInBaseMap.put("productStoreGroupId", context.get("productStoreGroupId"));
        calcPriceInBaseMap.put("currencyUomId", context.get("currencyUomId"));
        calcPriceInBaseMap.put("autoUserLogin", context.get("autoUserLogin"));
        List<GenericValue> shoppingListItems = null;
        try {
            shoppingListItems = EntityQuery.use(delegator)
                    .from("ShoppingListItem")
                    .where(UtilMisc.toMap("shoppingListId", context.get("shoppingListId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingListItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        totalPrice = new BigDecimal("0.0");
        if (shoppingListItems != null) {
            for (GenericValue shoppingListItem : shoppingListItems) {
                try {
                    product = EntityQuery.use(delegator)
                            .from("Product")
                            .where(UtilMisc.toMap("productId", ((Map<String, Object>) shoppingListItem).get("productId")))
                            .cache()
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                calcPriceInMap.putAll((Map<String, Object>) calcPriceInBaseMap);
                calcPriceInMap.put("product", product);
                calcPriceInMap.put("quantity", ((Map<String, Object>) shoppingListItem).get("quantity"));
                if (UtilValidate.isNotEmpty(((Map<String, Object>) shoppingListItem).get("modifiedPrice"))) {
                    calcPriceOutMap = new HashMap<>();
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("calculateProductPrice", calcPriceInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        calcPriceOutMap.put("price", serviceResult.get("price"));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling calculateProductPrice: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                itemPrice = ((Map<String, Object>) shoppingListItem).get("modifiedPrice");
                totalPrice = new BigDecimal(totalPrice.toString());
                calcPriceInMap = null;
            }
        }
        List<GenericValue> childshoppingLists = null;
        try {
            childshoppingLists = EntityQuery.use(delegator)
                    .from("ShoppingList")
                    .where(UtilMisc.toMap("parentShoppingListId", context.get("shoppingListId"), "partyId", ((Map<String, Object>) userLogin).get("partyId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (childshoppingLists != null) {
            for (GenericValue childshoppingList : childshoppingLists) {
                calcChildPriceInMap.putAll((Map<String, Object>) calcPriceInBaseMap);
                calcChildPriceInMap.put("shoppingListId", ((Map<String, Object>) childshoppingList).get("shoppingListId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("calculateShoppingListDeepTotalPrice", calcChildPriceInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    calcPriceOutMap.put("totalPrice", serviceResult.get("totalPrice"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling calculateShoppingListDeepTotalPrice: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                totalPrice = new BigDecimal(totalPrice.toString());
                calcChildPriceInMap = null;
            }
        }
        result.put("totalPrice", totalPrice);

        return "success";
    }


    /**
     * Checks security on a ShoppingList
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkShoppingListSecurity(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object shoppingList = null;
        if ((!((!(UtilValidate.isEmpty(((Map<String, Object>) shoppingList).get("partyId"))) && java.util.Objects.equals(((Map<String, Object>) userLogin).get("partyId"), ((Map<String, Object>) shoppingList).get("partyId")))) && !(true /* TODO: if-has-permission */) && !((!(UtilValidate.isEmpty(((Map<String, Object>) shoppingList).get("shoppingListAuthToken"))) && java.util.Objects.equals(context.get("shoppingListAuthToken"), ((Map<String, Object>) shoppingList).get("shoppingListAuthToken")) && UtilValidate.isEmpty(((Map<String, Object>) shoppingList).get("partyId")))))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderSecurityErrorToRunForAnotherParty", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }

        return "success";
    }


    /**
     * Checks security on a ShoppingListItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkShoppingListItemSecurity(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue shoppingList = null;
        try {
            shoppingList = EntityQuery.use(delegator)
                    .from("ShoppingList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        String result = checkShoppingListSecurity(request, response);
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
     * Add suggestions to a shopping list
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String addSuggestionsToShoppingList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object shoppingListId = null;
        Map<String, Object> createShoppingListInMap = null;
        GenericValue virtualProductAssoc = null;
        GenericValue product = null;
        List<GenericValue> compProductAssocList = null;
        List<GenericValue> virtualProductAssocList = null;
        Object shoppingListParameters = null;
        GenericValue compProductAssoc = null;
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
        if (UtilValidate.isEmpty(((Map<String, Object>) orderHeader).get("productStoreId"))) {
            return "success";
        }
        GenericValue productStore = null;
        try {
            productStore = EntityQuery.use(delegator)
                    .from("ProductStore")
                    .where(UtilMisc.toMap("productStoreId", ((Map<String, Object>) orderHeader).get("productStoreId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (!"Y".equals(((Map<String, Object>) productStore).get("enableAutoSuggestionList"))) {
            return "success";
        }
        List<GenericValue> orderRoleList = null;
        try {
            orderRoleList = EntityQuery.use(delegator)
                    .from("OrderRole")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue orderRole = EntityUtil.getFirst((List<GenericValue>) orderRoleList);
        List<GenericValue> shoppingListList = null;
        try {
            shoppingListList = EntityQuery.use(delegator)
                    .from("ShoppingList")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShoppingList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue shoppingList = EntityUtil.getFirst((List<GenericValue>) shoppingListList);
        if (UtilValidate.isEmpty(shoppingList)) {
            createShoppingListInMap.put("partyId", ((Map<String, Object>) orderRole).get("partyId"));
            createShoppingListInMap.put("listName", "Auto Suggestions");
            createShoppingListInMap.put("shoppingListTypeId", "SLT_WISH_LIST");
            createShoppingListInMap.put("productStoreId", context.get("productStoreId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createShoppingList", createShoppingListInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                shoppingListId = serviceResult.get("shoppingListId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createShoppingList: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            shoppingListId = ((Map<String, Object>) shoppingList).get("shoppingListId");
        }
        List<GenericValue> orderItemList = null;
        try {
            orderItemList = EntityQuery.use(delegator)
                    .from("OrderItem")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (orderItemList != null) {
            for (GenericValue orderItem : orderItemList) {
                if (UtilValidate.isNotEmpty(((Map<String, Object>) orderItem).get("productId"))) {
                    try {
                        compProductAssocList = EntityQuery.use(delegator)
                                .from("ProductAssoc")
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductAssoc: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (compProductAssocList != null) {
                        for (GenericValue compProductAssoc_iter : compProductAssocList) {
                            compProductAssoc = compProductAssoc_iter;
                            shoppingListParameters = null;
                            ((Map<String, Object>) shoppingListParameters).put("productId", ((Map<String, Object>) compProductAssoc).get("productIdTo"));
                            ((Map<String, Object>) shoppingListParameters).put("shoppingListId", shoppingListId);
                            ((Map<String, Object>) shoppingListParameters).put("quantity", BigDecimal.ONE);
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("addDistinctShoppingListItem", (Map<String, Object>) shoppingListParameters);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling addDistinctShoppingListItem: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                    }
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
                    if ("Y".equals(((Map<String, Object>) product).get("isVariant"))) {
                        try {
                            virtualProductAssocList = EntityQuery.use(delegator)
                                    .from("ProductAssoc")
                                    .filterByDate()
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying ProductAssoc: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        virtualProductAssoc = EntityUtil.getFirst((List<GenericValue>) virtualProductAssocList);
                        if (UtilValidate.isNotEmpty(virtualProductAssoc)) {
                            try {
                                compProductAssocList = EntityQuery.use(delegator)
                                        .from("ProductAssoc")
                                        .filterByDate()
                                        .queryList();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying ProductAssoc: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            if (compProductAssocList != null) {
                                for (GenericValue compProductAssoc_iter : compProductAssocList) {
                                    compProductAssoc = compProductAssoc_iter;
                                    shoppingListParameters = null;
                                    ((Map<String, Object>) shoppingListParameters).put("productId", ((Map<String, Object>) compProductAssoc).get("productIdTo"));
                                    ((Map<String, Object>) shoppingListParameters).put("shoppingListId", shoppingListId);
                                    ((Map<String, Object>) shoppingListParameters).put("quantity", BigDecimal.ONE);
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("addDistinctShoppingListItem", (Map<String, Object>) shoppingListParameters);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                            return "error";
                                        }
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling addDistinctShoppingListItem: " + e.getMessage(), MODULE);
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

}

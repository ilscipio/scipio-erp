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
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://order/script/org/ofbiz/order/order/OrderSimpleMethods.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OrderSimpleMethods {

    private static final String MODULE = OrderSimpleMethods.class.getName();


    /**
     * Permission service for the creation and editing of order adjustments
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String orderAdjustmentPermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Boolean hasPermission = null;
        String resourceDescription = null;
        String failMessage = null;
        Object primaryPermission = "ORDERMGR";
        Object altPermission = "ORDERMGR_ROLE";
        Object mainAction = context.get("mainAction");
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        if (!"true".equals(hasPermission)) {
            resourceDescription = (String) context.get("resourceDescription");
            if (UtilValidate.isEmpty(resourceDescription)) {
                resourceDescription = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionThisOperation", locale);
            }
            if ("CREATE".equals(mainAction)) {
                failMessage = UtilProperties.getMessage("OrderErrorUiLabels", "OrderSecurityErrorToRunCreateOrderAdjustement", locale);
            }
            if ("UPDATE".equals(mainAction)) {
                failMessage = UtilProperties.getMessage("OrderErrorUiLabels", "OrderSecurityErrorToRunAutoCreateOrderAdjustments", locale);
            }
            hasPermission = Boolean.FALSE;
            result.put("failMessage", failMessage);
        } else {
            result.put("hasPermission", hasPermission);
        }

        return "success";
    }


    /**
     * Create an OrderAdjustment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOrderAdjustment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("OrderAdjustment");
        newEntity.setNonPKFields((Map<String, Object>) context);
        ((GenericValue) newEntity).put("orderAdjustmentId", delegator.getNextSeqId("OrderAdjustment"));
        result.put("orderAdjustmentId", ((Map<String, Object>) newEntity).get("orderAdjustmentId"));
        Timestamp newEntity_createdDate = new Timestamp(System.currentTimeMillis());
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
     * Update an OrderAdjustment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateOrderAdjustment(HttpServletRequest request, HttpServletResponse response) {
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
                    .from("OrderAdjustment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderAdjustment: " + e.getMessage(), MODULE);
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
     * Delete an OrderAdjustment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteOrderAdjustment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("OrderAdjustment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderAdjustment: " + e.getMessage(), MODULE);
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
        if (UtilValidate.isNotEmpty(context.get("productPromoCodeId"))) {
            try {
                lookedUpValue = EntityQuery.use(delegator)
                        .from("OrderProductPromoCode")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderProductPromoCode: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(lookedUpValue)) {
                try {
                    delegator.removeValue(lookedUpValue);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Create an OrderAdjustmentBilling
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOrderAdjustmentBilling(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("OrderAdjustmentBilling");
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
     * Create an OrderItemBilling
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOrderItemBilling(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("OrderItemBilling");
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
     * Log an order notification
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createNotificationLog(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue orderNotification = delegator.makeValue("OrderNotification");
        ((GenericValue) orderNotification).put("orderNotificationId", delegator.getNextSeqId("OrderNotification"));
        orderNotification.put("orderId", context.get("orderId"));
        orderNotification.put("emailType", context.get("emailType"));
        orderNotification.put("comments", context.get("comments"));
        Timestamp orderNotification_notificationDate = new Timestamp(System.currentTimeMillis());
        try {
            delegator.create(orderNotification);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("orderNotificationId", ((Map<String, Object>) orderNotification).get("orderNotificationId"));

        return "success";
    }


    /**
     * Update Order Status From ShipmentReceipt
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateOrderStatusFromReceipt(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newValue = null;
        GenericValue orderItem = null;
        Map<String, Object> newLookupMap = null;
        Map<String, Object> totalsMap = null;
        Object allCompleted = null;
        GenericValue orderHeader = null;
        Timestamp newValue_statusDatetime = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
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
        List<GenericValue> shipmentReceipts = null;
        try {
            shipmentReceipts = EntityQuery.use(delegator)
                    .from("ShipmentReceipt")
                    .where(UtilMisc.toMap("orderId", context.get("orderId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ShipmentReceipt: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (shipmentReceipts != null) {
            for (GenericValue receipt : shipmentReceipts) {
                if (UtilValidate.isEmpty(((Map<String, Object>) totalsMap).get(((Map<String, Object>) receipt).get("orderItemSeqId")))) {
                    totalsMap.put((String) ((Map<String, Object>) receipt).get("orderItemSeqId"), BigDecimal.ZERO);
                }
                ((Map<String, Object>) totalsMap).put("receipt.orderItemSeqId", (new BigDecimal(((Map<String, Object>) receipt).get("quantityAccepted").toString())).add(new BigDecimal(((Map<String, Object>) receipt).get("quantityRejected").toString())));
                newLookupMap.put("orderId", ((Map<String, Object>) receipt).get("orderId"));
                newLookupMap.put("orderItemSeqId", ((Map<String, Object>) receipt).get("orderItemSeqId"));
                try {
                    orderItem = EntityQuery.use(delegator)
                            .from("OrderItem")
                            .where(newLookupMap)
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error finding by primary key OrderItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (!"ITEM_COMPLETED".equals(((Map<String, Object>) orderItem).get("statusId"))) {
                    if (((Map<String, Object>) orderItem).get("quantity") != null /* TODO: field compare operator less-equals */) {
                        orderItem.put("statusId", "ITEM_COMPLETED");
                        try {
                            delegator.store(orderItem);
                        } catch (Exception e) {
                            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        newValue = delegator.makeValue("OrderStatus");
                        ((GenericValue) newValue).put("orderStatusId", delegator.getNextSeqId("OrderStatus"));
                        newValue.put("orderItemSeqId", ((Map<String, Object>) orderItem).get("orderItemSeqId"));
                        newValue.put("orderId", ((Map<String, Object>) orderItem).get("orderId"));
                        newValue.put("statusId", ((Map<String, Object>) orderItem).get("statusId"));
                        newValue.put("statusUserLogin", ((Map<String, Object>) context.get("userLogin")).get("userLoginId"));
                        newValue_statusDatetime = new Timestamp(System.currentTimeMillis());
                        try {
                            delegator.create(newValue);
                        } catch (Exception e) {
                            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
            }
        }
        List<GenericValue> allOrderItems = null;
        try {
            allOrderItems = EntityQuery.use(delegator)
                    .from("OrderItem")
                    .where(UtilMisc.toMap("orderId", context.get("orderId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        allCompleted = "true";
        if (allOrderItems != null) {
            for (GenericValue item : allOrderItems) {
                if (!"ITEM_COMPLETED".equals(((Map<String, Object>) item).get("statusId"))) {
                    allCompleted = "false";
                }
            }
        }
        if ("true".equals(allCompleted)) {
            orderHeader.put("statusId", "ORDER_COMPLETED");
            try {
                delegator.store(orderHeader);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            newValue = delegator.makeValue("OrderStatus");
            ((GenericValue) newValue).put("orderStatusId", delegator.getNextSeqId("OrderStatus"));
            newValue.put("orderId", ((Map<String, Object>) orderHeader).get("orderId"));
            newValue.put("statusId", ((Map<String, Object>) orderHeader).get("statusId"));
            newValue.put("statusUserLogin", ((Map<String, Object>) context.get("userLogin")).get("userLoginId"));
            newValue_statusDatetime = new Timestamp(System.currentTimeMillis());
            try {
                delegator.create(newValue);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("currentStatusId", ((Map<String, Object>) orderHeader).get("statusId"));

        return "success";
    }

}

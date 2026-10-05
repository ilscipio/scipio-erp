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
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://order/script/org/ofbiz/order/order/OrderDeliveryServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OrderDeliveryServices {

    private static final String MODULE = OrderDeliveryServices.class.getName();


    /**
     * Creates a new Purchase Order Schedule
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOrderDeliverySchedule(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue schedule = null;
        Object callingMethodName = "createOrderDeliverySchedule";
        Object checkAction = "CREATE";
        String result = checkSupplierRelatedPermission(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        schedule = delegator.makeValue("OrderDeliverySchedule");
        schedule.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) schedule).get("orderItemSeqId"))) {
            schedule.put("orderItemSeqId", "_NA_");
        }
        schedule.setNonPKFields((Map<String, Object>) context);
        // TODO: Convert <if-has-permission> element
        try {
            delegator.create(schedule);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Updates an existing Purchase Order Schedule
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateOrderDeliverySchedule(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue schedule = null;
        Object callingMethodName = "updateOrderDeliverySchedule";
        Object checkAction = "UPDATE";
        String result = checkSupplierRelatedPermission(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue lookupPkMap = delegator.makeValue("OrderDeliverySchedule");
        lookupPkMap.setPKFields((Map<String, Object>) context);
        try {
            schedule = EntityQuery.use(delegator)
                    .from("OrderDeliverySchedule")
                    .where(lookupPkMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key OrderDeliverySchedule: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object saveStatusId = ((Map<String, Object>) schedule).get("statusId");
        schedule.setNonPKFields((Map<String, Object>) context);
        // TODO: Convert <if-has-permission> element
        try {
            delegator.store(schedule);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Send Order Delivery Schedule Notification
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sendOrderDeliveryScheduleNotification(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> sendToPartyIdMap = null;
        Map<String, Object> sendToPartyPcmFindMap = null;
        Object callingMethodName = "sendOrderDeliveryScheduleNotification";
        Object checkAction = "UPDATE";
        String result = checkSupplierRelatedPermission(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("orderItemSeqId"))) {
            context.put("orderItemSeqId", "_NA_");
        }
        GenericValue orderDeliverySchedule = delegator.makeValue("OrderDeliverySchedule");
        orderDeliverySchedule.setPKFields((Map<String, Object>) context);
        try {
            orderDeliverySchedule = EntityQuery.use(delegator)
                    .from("")
                    .where(orderDeliverySchedule)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> curUserPcmFindMap = new HashMap<>();
        curUserPcmFindMap.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        curUserPcmFindMap.put("contactMechTypeId", "EMAIL_ADDRESS");
        // TODO: Convert <find-by-and> element
        GenericValue curUserPartyAndContactMech = EntityUtil.getFirst((List<GenericValue>) context.get("curUserPartyAndContactMechs"));
        // TODO: Convert <string-append> element
        Map<String, Object> shipmentClerkFindMap = new HashMap<>();
        shipmentClerkFindMap.put("roleTypeId", "SHIPMENT_CLERK");
        // TODO: Convert <find-by-and> element
        if (context.get("shipmentClerkRoles") != null) {
            for (Object shipmentClerkRole : (List<Object>) context.get("shipmentClerkRoles")) {
                sendToPartyIdMap.put((String) ((Map<String, Object>) shipmentClerkRole).get("partyId"), ((Map<String, Object>) shipmentClerkRole).get("partyId"));
            }
        }
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) sendToPartyIdMap).entrySet()) {
            String sendToPartyId = entry.getKey();
            Object sendToPartyIdValue = entry.getValue();
            sendToPartyPcmFindMap.put("partyId", sendToPartyId);
            sendToPartyPcmFindMap.put("contactMechTypeId", "EMAIL_ADDRESS");
            // TODO: Convert <find-by-and> element
            if (context.get("sendToPartyPartyAndContactMechs") != null) {
                for (Object sendToPartyPartyAndContactMech : (List<Object>) context.get("sendToPartyPartyAndContactMechs")) {
                    // TODO: Convert <string-append> element
                }
            }
        }
        Map<String, Object> sendEmailMap = new HashMap<>();
        sendEmailMap.put("subject", "Delivery Information Updated for Order #" + ((Map<String, Object>) orderDeliverySchedule).get("orderId"));
        if (!"_NA_".equals(((Map<String, Object>) orderDeliverySchedule).get("orderItemSeqId"))) {
            // TODO: Convert <string-append> element
        }
        sendEmailMap.put("contentType", "text/html");
        sendEmailMap.put("templateName", "default/OrderDeliveryUpdatedNotice.ftl");
        sendEmailMap.put("templateData.orderDeliverySchedule", orderDeliverySchedule);
        Debug.logInfo("Sending generic notification email (if all info is in place): " + sendEmailMap, MODULE);
        if ((!(UtilValidate.isEmpty(((Map<String, Object>) sendEmailMap).get("sendTo"))) && !(UtilValidate.isEmpty(((Map<String, Object>) sendEmailMap).get("sendFrom"))))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("sendGenericNotificationEmail", sendEmailMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling sendGenericNotificationEmail: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            Debug.logError("Insufficient data to send notice email: " + sendEmailMap, MODULE);
        }

        return "success";
    }


    /**
     * Check Supplier Related Permission Service
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkSupplierRelatedOrderPermissionService(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object checkAction = context.get("checkAction");
        Object callingMethodName = context.get("callingMethodName");
        String inlineResult = checkSupplierRelatedPermission(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        result.put("hasSupplierRelatedPermission", context.get("hasSupplierRelatedPermission"));

        return "success";
    }


    /**
     * Check Supplier Related Permission
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkSupplierRelatedPermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String callingMethodName = null;
        Object checkAction = null;
        Map<String, Object> lookupOrderRoleMap = null;
        Object hasSupplierRelatedPermission = null;
        GenericValue permOrderRole = null;
        List<String> error_list = null;
        if (UtilValidate.isEmpty(callingMethodName)) {
            callingMethodName = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionThisOperation", locale);
        }
        if (UtilValidate.isEmpty(checkAction)) {
            checkAction = "UPDATE";
        }
        hasSupplierRelatedPermission = "false";
        Object lookupOrderRoleMap_orderId = null;
        Object lookupOrderRoleMap_partyId = null;
        Object lookupOrderRoleMap_roleTypeId = null;
        if (true /* TODO: if-has-permission */) {
            hasSupplierRelatedPermission = "true";
        } else {
            lookupOrderRoleMap.put("orderId", context.get("orderId"));
            lookupOrderRoleMap.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
            lookupOrderRoleMap.put("roleTypeId", "SUPPLIER_AGENT");
            try {
                permOrderRole = EntityQuery.use(delegator)
                        .from("OrderRole")
                        .where(lookupOrderRoleMap)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key OrderRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(permOrderRole)) {
                hasSupplierRelatedPermission = "false";
                error_list.add("ERROR: You do not have permission to ${checkAction} Delivery Schedule Information; you must be associated with this order as a Supplier Agent or have the ORDERMGR_${checkAction} permission.");
            } else {
                hasSupplierRelatedPermission = "true";
            }
        }
        Debug.logInfo("hasSupplierRelatedPermission is: " + hasSupplierRelatedPermission, MODULE);

        return "success";
    }

}

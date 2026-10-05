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
package com.ilscipio.scipio.marketing.event;

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
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://marketing/script/org/ofbiz/marketing/tracking/TrackingCodeServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class TrackingCodeServices {

    private static final String MODULE = TrackingCodeServices.class.getName();


    /**
     * Create an TrackingCodeType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createTrackingCodeType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        GenericValue newEntity = delegator.makeValue("TrackingCodeType");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.put("createdStamp", nowStamp);
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
     * Update an TrackingCodeType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateTrackingCodeType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        GenericValue lookupPKMap = delegator.makeValue("TrackingCodeType");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("TrackingCodeType")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key TrackingCodeType: " + e.getMessage(), MODULE);
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
     * Delete an TrackingCodeType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteTrackingCodeType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("TrackingCodeType");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("TrackingCodeType")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key TrackingCodeType: " + e.getMessage(), MODULE);
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
     * Create an TrackingCodeOrder
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createTrackingCodeOrder(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        GenericValue newEntity = delegator.makeValue("TrackingCodeOrder");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.put("createdStamp", nowStamp);
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
     * Update an TrackingCodeOrder
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateTrackingCodeOrder(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        GenericValue lookupPKMap = delegator.makeValue("TrackingCodeOrder");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("TrackingCodeOrder")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key TrackingCodeOrder: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        Map<String, Object> newEntity = new HashMap<>();
        newEntity.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        newEntity.put("createdDate", context.get("lastModifiedDate"));
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
     * Create an TrackingCodeOrderReturn
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createTrackingCodeOrderReturn(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        GenericValue newEntity = delegator.makeValue("TrackingCodeOrderReturn");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.put("createdStamp", nowStamp);
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
     * Create TrackingCodeOrderReturn for all the Return Items with Orders that have trackingCodeOrder entry
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createTrackingCodeOrderReturns(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> returnHeaderFindContext = new HashMap<>();
        Object trackingCodeOrder = null;
        List<GenericValue> trackingCodeOrders = null;
        List<GenericValue> returnItems = null;
        GenericValue orderHeader = null;
        GenericValue returnHeader = null;
        Map<String, Object> trackingCodeOrderFindContext = new HashMap<>();
        Map<String, Object> trackingCodeOrderReturnContext = new HashMap<>();
        if (UtilValidate.isNotEmpty(context.get("returnId"))) {
            returnHeaderFindContext.put("returnId", context.get("returnId"));
            try {
                returnHeader = EntityQuery.use(delegator)
                        .from("ReturnHeader")
                        .where(returnHeaderFindContext)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key ReturnHeader: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                returnItems = returnHeader.getRelated("ReturnItem", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related ReturnItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Return items lists are : " + returnItems, MODULE);
            if (returnItems != null) {
                for (GenericValue returnItem : returnItems) {
                    try {
                        orderHeader = returnItem.getRelatedOne("OrderHeader", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one OrderHeader: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    trackingCodeOrderFindContext.put("orderId", ((Map<String, Object>) orderHeader).get("orderId"));
                    Debug.logInfo("Find Tracking Code Order For " + trackingCodeOrderFindContext, MODULE);
                    // TODO: Convert <find-by-and> element
                    trackingCodeOrder = (trackingCodeOrders != null && !trackingCodeOrders.isEmpty()) ? trackingCodeOrders.get(0) : null;
                    Debug.logInfo("Create  Tracking Code Order Return  For " + trackingCodeOrder, MODULE);
                    if (UtilValidate.isNotEmpty(trackingCodeOrder)) {
                        // set-service-fields from "trackingCodeOrder" to "trackingCodeOrderReturnContext" for service "createTrackingCodeOrderReturn"
                        trackingCodeOrderReturnContext.putAll(UtilMisc.toMap(trackingCodeOrder));
                        trackingCodeOrderReturnContext.put("returnId", context.get("returnId"));
                        trackingCodeOrderReturnContext.put("orderId", ((Map<String, Object>) trackingCodeOrder).get("orderId"));
                        trackingCodeOrderReturnContext.put("trackingCodeTypeId", "PARTNER_MGD");
                        trackingCodeOrderReturnContext.put("hasExported", "N");
                        trackingCodeOrderReturnContext.put("affiliateReferredTimeStamp", context.get("nowTimestamp"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createTrackingCodeOrderReturn", trackingCodeOrderReturnContext);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createTrackingCodeOrderReturn: " + e.getMessage(), MODULE);
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
     * update an TrackingCodeOrderReturn
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateTrackingCodeOrderReturn(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        GenericValue lookupPKMap = delegator.makeValue("TrackingCodeOrderReturn");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("TrackingCodeOrderReturn")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key TrackingCodeOrderReturn: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        Map<String, Object> newEntity = new HashMap<>();
        newEntity.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        newEntity.put("createdDate", context.get("lastModifiedDate"));
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
     * delete an TrackingCodeOrderReturn
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteTrackingCodeOrderReturn(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("TrackingCodeOrderReturn");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("TrackingCodeOrderReturn")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key TrackingCodeOrderReturn: " + e.getMessage(), MODULE);
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

}

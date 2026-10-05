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

import java.sql.Timestamp;
import java.util.ArrayList;
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
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;
import org.ofbiz.order.shoppingcart.ShoppingCart;
import org.ofbiz.order.shoppingcart.ShoppingCartItem;
import org.ofbiz.product.config.ProductConfigWrapper;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://order/script/org/ofbiz/order/request/CustRequestServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CustRequestServices {

    private static final String MODULE = CustRequestServices.class.getName();


    /**
     * Cust Request Permission Check
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String custRequestPermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object primaryPermission = null;
        Boolean hasPermission = null;
        String resourceDescription = null;
        String failMessage = null;
        if ((!(UtilValidate.isEmpty(context.get("fromPartyId"))) && !(java.util.Objects.equals(context.get("fromPartyId"), ((Map<String, Object>) userLogin).get("partyId"))))) {
            primaryPermission = "ORDERMGR_CRQ";
            // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
            // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
            if (!"true".equals(hasPermission)) {
                resourceDescription = (String) context.get("resourceDescription");
                if (UtilValidate.isEmpty(resourceDescription)) {
                    resourceDescription = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionThisOperation", locale);
                }
                failMessage = UtilProperties.getMessage("OrderErrorUiLabels", "OrderSecurityErrorToRunCreateCustRequest", locale);
                hasPermission = Boolean.FALSE;
                result.put("failMessage", failMessage);
            }
        } else {
            hasPermission = Boolean.TRUE;
        }
        result.put("hasPermission", hasPermission);

        return "success";
    }


    /**
     * Create Customer Request
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustRequest(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        Map<String, Object> setStat = null;
        Map<String, Object> createItem = null;
        newEntity = delegator.makeValue("CustRequest");
        newEntity.setNonPKFields((Map<String, Object>) context);
        Timestamp newEntity_lastModifiedDate = new Timestamp(System.currentTimeMillis());
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        newEntity.put("lastModifiedDate", nowTimestamp);
        newEntity.put("createdDate", nowTimestamp);
        newEntity.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        newEntity.put("createdByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        if (UtilValidate.isEmpty(context.get("custRequestDate"))) {
            newEntity.put("custRequestDate", nowTimestamp);
        }
        newEntity.put("statusId", "CRQ_DRAFT");
        if (UtilValidate.isNotEmpty(context.get("custRequestId"))) {
            newEntity.put("custRequestId", context.get("custRequestId"));
        } else {
            ((GenericValue) newEntity).put("custRequestId", delegator.getNextSeqId("CustRequest"));
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("custRequestId", ((Map<String, Object>) newEntity).get("custRequestId"));
        if (UtilValidate.isNotEmpty(context.get("statusId"))) {
            setStat.put("statusId", context.get("statusId"));
            setStat.put("custRequestId", ((Map<String, Object>) newEntity).get("custRequestId"));
            if (UtilValidate.isNotEmpty(context.get("webSiteId"))) {
                setStat.put("webSiteId", context.get("webSiteId"));
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("setCustRequestStatus", setStat);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling setCustRequestStatus: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        Object createItem_custRequestId = null;
        if ((!(UtilValidate.isEmpty(context.get("productId"))))) {
            // set-service-fields from "parameters" to "createItem" for service "createCustRequestItem"
            createItem.putAll(UtilMisc.toMap(context));
            createItem.put("custRequestId", ((Map<String, Object>) newEntity).get("custRequestId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createCustRequestItem", createItem);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createCustRequestItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Delete a draft Customer Request with no relations yet
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteCustRequest(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

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
        if (!"CRQ_DRAFT".equals(((Map<String, Object>) custRequest).get("statusId"))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderCheckCustRequestDraftStatusForDelete", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        // TODO: Convert <remove-related> element
        // TODO: Convert <remove-related> element
        // TODO: Convert <remove-related> element
        try {
            delegator.removeValue(custRequest);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update Customer Request
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCustRequest(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue custRequest = null;
        Map<String, Object> setStat = null;
        GenericValue lowInfo = null;
        String errorMessage = null;
        List<GenericValue> workEfforts = null;
        Object actualHours = null;
        Object successMessage = null;
        Map<String, Object> updTask = null;
        Object isShowEvent = null;
        List<GenericValue> custRequestItems = null;
        Map<String, Object> createItem = null;
        Map<String, Object> updateItem = null;
        GenericValue custRequestItem = null;
        String inlineResult = checkStatusCustRequest(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        Object oldStatusId = ((Map<String, Object>) custRequest).get("statusId");
        result.put("oldStatusId", oldStatusId);
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        custRequest.put("lastModifiedDate", nowTimestamp);
        custRequest.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        custRequest.setNonPKFields((Map<String, Object>) context);
        custRequest.put("statusId", oldStatusId);
        try {
            delegator.store(custRequest);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(context.get("statusId"))) {
            if (!java.util.Objects.equals(((Map<String, Object>) custRequest).get("statusId"), context.get("statusId"))) {
                if ("CRQ_CANCELLED".equals(context.get("statusId"))) {
                    try {
                        workEfforts = custRequest.getRelated("CustRequestWorkEffort", null, null, false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related CustRequestWorkEffort: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isNotEmpty(workEfforts)) {
                        if (workEfforts != null) {
                            for (GenericValue workEffort : workEfforts) {
                                try {
                                    lowInfo = EntityQuery.use(delegator)
                                            .from("WorkEffort")
                                            .where(UtilMisc.toMap("workEffortId", ((Map<String, Object>) workEffort).get("workEffortId")))
                                            .queryOne();
                                } catch (Exception e) {
                                    Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                    return "error";
                                }
                                // TODO: Call simple-method "getHours" from "component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml"
                                // Original: call-simple-method method-name="getHours" xml-resource="component://workeffort/script/org/ofbiz/workeffort/workeffort/WorkEffortSimpleServices.xml"
                                actualHours = ((Map<String, Object>) context.get("highInfo")).get("actualHours");
                                if (UtilValidate.isEmpty(actualHours)) {
                                    custRequest.put("statusId", context.get("statusId"));
                                    updTask.put("workEffortId", ((Map<String, Object>) workEffort).get("workEffortId"));
                                    updTask.put("currentStatusId", "PTS_CANCELLED");
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("updateWorkEffort", updTask);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                            return "error";
                                        }
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling updateWorkEffort: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                } else {
                                    context.put("statusId", ((Map<String, Object>) custRequest).get("statusId"));
                                    errorMessage = UtilProperties.getMessage("OrderUiLabels", "OrderCannotCancelRequestAlreadyWorkedOn", locale);
                                    result.put("errorMessage", errorMessage);
                                    isShowEvent = "N";
                                }
                            }
                        }
                    }
                }
                // set-service-fields from "parameters" to "setStat" for service "setCustRequestStatus"
                setStat.putAll(UtilMisc.toMap(context));
                if (UtilValidate.isNotEmpty(context.get("webSiteId"))) {
                    setStat.put("webSiteId", context.get("webSiteId"));
                }
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setCustRequestStatus", setStat);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling setCustRequestStatus: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(isShowEvent)) {
                    successMessage = null;
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("story"))) {
            try {
                custRequestItems = EntityQuery.use(delegator)
                        .from("CustRequestItem")
                        .where(UtilMisc.toMap("custRequestId", context.get("custRequestId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying CustRequestItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(custRequestItems)) {
                custRequestItem = EntityUtil.getFirst((List<GenericValue>) custRequestItems);
                // set-service-fields from "custRequestItem" to "updateItem" for service "updateCustRequestItem"
                updateItem.putAll(UtilMisc.toMap(custRequestItem));
                updateItem.put("story", context.get("story"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateCustRequestItem", updateItem);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateCustRequestItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                // set-service-fields from "custRequest" to "createItem" for service "createCustRequestItem"
                createItem.putAll(UtilMisc.toMap(custRequest));
                createItem.put("story", context.get("story"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createCustRequestItem", createItem);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createCustRequestItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Check StatusId CustRequest
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkStatusCustRequest(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

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
        if (UtilValidate.isEmpty(custRequest)) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderErrorCustRequestNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            Debug.logInfo("CustRequest not found, statusId Id: " + ((Map<String, Object>) custRequest).get("statusId"), MODULE);
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        if (("CRQ_CANCELLED".equals(((Map<String, Object>) custRequest).get("statusId")) || "CRQ_COMPLETED".equals(((Map<String, Object>) custRequest).get("statusId")))) {
            {
                String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderCheckCustRequest", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            Debug.logInfo("Can only update CustRequest, when status is in-process...is now: " + ((Map<String, Object>) custRequest).get("statusId"), MODULE);
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create Customer Request Attribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustRequestAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("CustRequestAttribute");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        String result = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Update Customer Request Attribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCustRequestAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> lookupPKMap = new HashMap<>();
        lookupPKMap.put("custRequestId", context.get("custRequestId"));
        lookupPKMap.put("attrName", context.get("attrName"));
        GenericValue custRequestAttr = null;
        try {
            custRequestAttr = EntityQuery.use(delegator)
                    .from("CustRequestAttribute")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CustRequestAttribute: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        custRequestAttr.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(custRequestAttr);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        String result = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Create Customer Request Item
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustRequestItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        String inlineResult = checkStatusCustRequest(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        newEntity = delegator.makeValue("CustRequestItem");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(context.get("custRequestItemSeqId"))) {
            delegator.setNextSubSeqId(newEntity, "custRequestItemSeqId", 5, 1);
            Object custRequestItemSeqId = newEntity.get("custRequestItemSeqId");
        }
        if (UtilValidate.isEmpty(context.get("statusId"))) {
            newEntity.put("statusId", "CRQ_SUBMITTED");
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("custRequestId", ((Map<String, Object>) newEntity).get("custRequestId"));
        result.put("custRequestItemSeqId", ((Map<String, Object>) newEntity).get("custRequestItemSeqId"));
        inlineResult = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }

        return "success";
    }


    /**
     * Update Customer Request Item
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCustRequestItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = checkStatusCustRequest(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        Map<String, Object> lookupPKMap = new HashMap<>();
        lookupPKMap.put("custRequestId", context.get("custRequestId"));
        lookupPKMap.put("custRequestItemSeqId", context.get("custRequestItemSeqId"));
        GenericValue custRequestItem = null;
        try {
            custRequestItem = EntityQuery.use(delegator)
                    .from("CustRequestItem")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CustRequestItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        custRequestItem.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(custRequestItem);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Remove Customer Request Item
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeCustRequestItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = checkStatusCustRequest(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        Map<String, Object> lookupPKMap = new HashMap<>();
        lookupPKMap.put("custRequestId", context.get("custRequestId"));
        lookupPKMap.put("custRequestItemSeqId", context.get("custRequestItemSeqId"));
        GenericValue custRequestItem = null;
        try {
            custRequestItem = EntityQuery.use(delegator)
                    .from("CustRequestItem")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CustRequestItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(custRequestItem);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Create Customer RequestParty
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustRequestParty(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue partyRole = null;
        String result = checkStatusCustRequest(request, response);
        if (!"success".equals(result)) {
            return result;
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
        GenericValue newEntity = delegator.makeValue("CustRequestParty");
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
        result = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Update an existing CustRequestParty
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCustRequestParty(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = checkStatusCustRequest(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("CustRequestParty")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustRequestParty: " + e.getMessage(), MODULE);
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
        result = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Expire Customer CustRequestParty
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String expireCustRequestParty(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = checkStatusCustRequest(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("CustRequestParty")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustRequestParty: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Timestamp thruDate = new Timestamp(System.currentTimeMillis());
        lookedUpValue.put("thruDate", thruDate);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Delete Customer CustRequestParty
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteCustRequestParty(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = checkStatusCustRequest(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("CustRequestParty")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustRequestParty: " + e.getMessage(), MODULE);
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
        result = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Create Customer Request Note
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustRequestNote(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("CustRequestNote");
        newEntity.put("custRequestId", context.get("custRequestId"));
        Map<String, Object> newNoteMap = new HashMap<>();
        newNoteMap.put("note", context.get("noteInfo"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createNote", newNoteMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            newEntity.put("noteId", serviceResult.get("noteId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createNote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("noteId", ((Map<String, Object>) newEntity).get("noteId"));
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
        result.put("fromPartyId", ((Map<String, Object>) custRequest).get("fromPartyId"));
        result.put("custRequestName", ((Map<String, Object>) custRequest).get("custRequestName"));
        String inlineResult = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }

        return "success";
    }


    /**
     * Update CustRequest Note
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCustRequestNote(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("CustRequestNote")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustRequestNote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue lookedUpValueForNoteData = null;
        try {
            lookedUpValueForNoteData = EntityQuery.use(delegator)
                    .from("NoteData")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying NoteData: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValueForNoteData.setNonPKFields((Map<String, Object>) context);
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.store(lookedUpValueForNoteData);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        String result = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Create Customer RequestItem Note
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustRequestItemNote(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        String inlineResult = checkStatusCustRequest(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        GenericValue newEntity = delegator.makeValue("CustRequestItemNote");
        newEntity.put("custRequestId", context.get("custRequestId"));
        newEntity.put("custRequestItemSeqId", context.get("custRequestItemSeqId"));
        Map<String, Object> newNoteMap = new HashMap<>();
        newNoteMap.put("note", context.get("note"));
        newNoteMap.put("partyId", context.get("partyId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createNote", newNoteMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            newEntity.put("noteId", serviceResult.get("noteId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createNote: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("noteId", ((Map<String, Object>) newEntity).get("noteId"));
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
        result.put("fromPartyId", ((Map<String, Object>) custRequest).get("fromPartyId"));
        result.put("custRequestName", ((Map<String, Object>) custRequest).get("custRequestName"));
        inlineResult = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }

        return "success";
    }


    /**
     * Create Customer RequestItem Note
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getCustRequestsByRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> lookupMap = new HashMap<>();
        lookupMap.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        lookupMap.put("roleTypeId", context.get("roleTypeId"));
        List<String> orderByList = new ArrayList<>();
        orderByList.add("priority");
        orderByList.add("-responseRequiredDate");
        orderByList.add("-custRequestDate");
        orderByList.add("-createdDate");
        // TODO: Convert <find-by-and> element
        result.put("custRequestAndRoles", context.get("custRequestAndRoles"));

        return "success";
    }


    /**
     * Create a CustRequest from a ShoppingCart
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustRequestFromCart(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        String custRequestName = null;
        Map<String, Object> createCustRequestInMap = new HashMap<>();
        Map<String, Object> createCustRequestItemInMap = new HashMap<>();
        Object configId = null;
        ProductConfigWrapper configWrapper = null;
        Object isPromo = null;
        Object custRequestItemSeqId = null;
        ShoppingCart cartObj = (ShoppingCart) context.get("cart");
        createCustRequestInMap.put("fromPartyId", cartObj != null ? cartObj.getPartyId() : null);
        createCustRequestInMap.put("custRequestTypeId", "RF_QUOTE");
        createCustRequestInMap.put("statusId", "CRQ_SUBMITTED");
        createCustRequestInMap.put("custRequestName", context.get("custRequestName"));
        if (UtilValidate.isEmpty(((Map<String, Object>) createCustRequestInMap).get("custRequestName"))) {
            custRequestName = UtilProperties.getMessage("OrderUiLabels", "OrderRequestCreatedFromShoppingCart", locale);
            createCustRequestInMap.put("custRequestName", custRequestName);
        }
        createCustRequestInMap.put("maximumAmountUomId", cartObj != null ? cartObj.getCurrency() : null);
        createCustRequestInMap.put("productStoreId", cartObj != null ? cartObj.getProductStoreId() : null);
        createCustRequestInMap.put("salesChannelEnumId", cartObj != null ? cartObj.getChannelType() : null);
        Object custRequestId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCustRequest", createCustRequestInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            custRequestId = serviceResult.get("custRequestId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCustRequest: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
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
        List<ShoppingCartItem> cartItems = cartObj != null ? cartObj.items() : null;
        if (cartItems != null) {
            for (ShoppingCartItem item : cartItems) {
                createCustRequestItemInMap = new HashMap<>();
                configWrapper = item.getConfigWrapper();
                if (UtilValidate.isNotEmpty(configWrapper)) {
                    configId = configWrapper.getConfigId();
                    createCustRequestItemInMap.put("configId", configId);
                }
                isPromo = item.getIsPromo();
                if (Boolean.FALSE.equals(isPromo)) {
                    createCustRequestItemInMap.put("custRequestId", custRequest != null ? custRequest.get("custRequestId") : null);
                    createCustRequestItemInMap.put("productId", item.getProductId());
                    createCustRequestItemInMap.put("quantity", item.getQuantity());
                    createCustRequestItemInMap.put("reservStart", item.getReservStart());
                    createCustRequestItemInMap.put("reservLength", item.getReservLength());
                    createCustRequestItemInMap.put("reservPersons", item.getReservPersons());
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createCustRequestItem", createCustRequestItemInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        custRequestItemSeqId = serviceResult.get("custRequestItemSeqId");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createCustRequestItem: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        result.put("custRequestId", ((Map<String, Object>) custRequest).get("custRequestId"));

        return "success";
    }


    /**
     * Create a CustRequest from a Shopping List
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustRequestFromShoppingList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

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
        Map<String, Object> createCustRequestFromCartInMap = new HashMap<>();
        createCustRequestFromCartInMap.put("cart", cart);
        createCustRequestFromCartInMap.put("custRequestName", ((Map<String, Object>) shoppingList).get("listName"));
        Object custRequestId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCustRequestFromCart", createCustRequestFromCartInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            custRequestId = serviceResult.get("custRequestId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCustRequestFromCart: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        result.put("custRequestId", custRequestId);

        return "success";
    }


    /**
     * Copy an existing CustRequestItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String copyCustRequestItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createQuoteItemInMap = null;
        List<GenericValue> quoteItems = null;
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
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
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> createCustRequestItemInMap = new HashMap<>();
        // set-service-fields from "custRequestItem" to "createCustRequestItemInMap" for service "createCustRequestItem"
        createCustRequestItemInMap.putAll(UtilMisc.toMap(custRequestItem));
        createCustRequestItemInMap.put("custRequestId", context.get("custRequestIdTo"));
        createCustRequestItemInMap.put("custRequestItemSeqId", context.get("custRequestItemSeqId"));
        if (UtilValidate.isEmpty(context.get("custRequestIdTo"))) {
            if (UtilValidate.isEmpty(context.get("custRequestItemSeqIdTo"))) {
                createCustRequestItemInMap.remove("custRequestItemSeqId");
            }
        }
        Object custRequestIdTo = null;
        Object custRequestItemSeqId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCustRequestItem", createCustRequestItemInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            custRequestIdTo = serviceResult.get("custRequestId");
            custRequestItemSeqId = serviceResult.get("custRequestItemSeqId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCustRequestItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if ("Y".equals(context.get("copyLinkedQuotes"))) {
            try {
                quoteItems = custRequestItem.getRelated("QuoteItem", null, null, false);
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
                    createQuoteItemInMap.put("custRequestId", custRequestIdTo);
                    createQuoteItemInMap.put("custRequestItemSeqId", custRequestItemSeqId);
                    createQuoteItemInMap.remove("quoteItemSeqId");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createQuoteItem", createQuoteItemInMap);
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
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Create Customer Request Status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustRequestStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("CustRequestStatus");
        ((GenericValue) newEntity).put("custRequestStatusId", delegator.getNextSeqId("CustRequestStatus"));
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("statusDatetime"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            newEntity.put("statusDatetime", nowTimestamp);
        }
        newEntity.put("changeByUserLoginId", ((Map<String, Object>) userLogin).get("userLoginId"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("custRequestStatusId", ((Map<String, Object>) newEntity).get("custRequestStatusId"));

        return "success";
    }


    /**
     * change the customer request Status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setCustRequestStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        String msg = null;
        GenericValue statusChange = null;
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
        if (UtilValidate.isNotEmpty(custRequest)) {
            result.put("oldStatusId", ((Map<String, Object>) custRequest).get("statusId"));
            result.put("custRequestId", ((Map<String, Object>) custRequest).get("custRequestId"));
            result.put("fromPartyId", ((Map<String, Object>) custRequest).get("fromPartyId"));
            result.put("custRequestName", ((Map<String, Object>) custRequest).get("custRequestName"));
            if (!java.util.Objects.equals(((Map<String, Object>) custRequest).get("statusId"), context.get("statusId"))) {
                try {
                    statusChange = EntityQuery.use(delegator)
                            .from("StatusValidChange")
                            .where(UtilMisc.toMap("statusId", ((Map<String, Object>) custRequest).get("statusId"), "statusIdTo", context.get("statusId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(statusChange)) {
                    msg = "Status is not a valid change: from " + ((Map<String, Object>) custRequest).get("statusId") + " to " + context.get("statusId");
                    Debug.logError(msg, MODULE);
                    {
                        String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "OrderErrorCouldNotChangeOrderStatusFromTo", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
            }
        }
        if (!"CRQ_DRAFT".equals(context.get("statusId"))) {
            if (UtilValidate.isEmpty(((Map<String, Object>) custRequest).get("fromPartyId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("OrderUiLabels", "OrderCustRequestShouldHaveFromPartyIdIfNotDraft", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isEmpty(((Map<String, Object>) custRequest).get("custRequestName"))) {
                {
                    String errorMsg = UtilProperties.getMessage("OrderUiLabels", "OrderCustRequestShouldHaveCustRequestNameIfNotDraft", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        custRequest.put("statusId", context.get("statusId"));
        if (UtilValidate.isNotEmpty(context.get("reason"))) {
            custRequest.put("reason", context.get("reason"));
        }
        Timestamp custRequest_lastModifiedDate = new Timestamp(System.currentTimeMillis());
        try {
            delegator.store(custRequest);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> rqMap = new HashMap<>();
        // set-service-fields from "parameters" to "rqMap" for service "createCustRequestStatus"
        rqMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCustRequestStatus", rqMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCustRequestStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create a Customer request from a commEvent(email)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustRequestFromCommEvent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> custRequest = null;
        Integer subjectLength = null;
        Map<String, Object> reqContent = null;
        GenericValue communicationEvent = null;
        try {
            communicationEvent = EntityQuery.use(delegator)
                    .from("CommunicationEvent")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> custRequests = null;
        try {
            custRequests = communicationEvent.getRelated("CustRequestCommEvent", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related CustRequestCommEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("COM_COMPLETE".equals(((Map<String, Object>) communicationEvent).get("statusId"))) {
            if (UtilValidate.isNotEmpty(custRequests)) {
                result.put("custRequestId", ((GenericValue) ((List<?>) custRequests).get(0)).get("custRequestId"));
                return "success";
            }
        }
        // set-service-fields from "parameters" to "custRequest" for service "createCustRequest"
        custRequest.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isEmpty(((Map<String, Object>) custRequest).get("custRequestName"))) {
            subjectLength = (Integer) GroovyUtil.eval("communicationEvent.subject.length()", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
            if (((Comparable) subjectLength).compareTo(100) < 0) {
                custRequest.put("custRequestName", ((Map<String, Object>) communicationEvent).get("subject"));
            } else {
                custRequest.put("custRequestName", GroovyUtil.eval("communicationEvent.subject.substring(0,95) + \".....\"", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context)));
            }
        }
        if (UtilValidate.isEmpty(context.get("custRequestTypeId"))) {
            custRequest.put("custRequestTypeId", "RF_SUPPORT");
        }
        if (UtilValidate.isEmpty(context.get("fromPartyId"))) {
            custRequest.put("fromPartyId", ((Map<String, Object>) communicationEvent).get("partyIdFrom"));
        }
        custRequest.put("custRequestDate", ((Map<String, Object>) communicationEvent).get("entryDate"));
        custRequest.put("statusId", "CRQ_ACCEPTED");
        if (UtilValidate.isEmpty(((Map<String, Object>) custRequest).get("story"))) {
            custRequest.put("story", ((Map<String, Object>) communicationEvent).get("content"));
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCustRequest", custRequest);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("custRequestId", serviceResult.get("custRequestId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCustRequest: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> custRequestCommEvent = new HashMap<>();
        // set-service-fields from "parameters" to "custRequestCommEvent" for service "createCustRequestCommEvent"
        custRequestCommEvent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCustRequestCommEvent", custRequestCommEvent);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCustRequestCommEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> custRequestContents = null;
        try {
            custRequestContents = EntityQuery.use(delegator)
                    .from("CommEventContentAssoc")
                    .where(UtilMisc.toMap("communicationEventId", context.get("communicationEventId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommEventContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (custRequestContents != null) {
            for (GenericValue custRequestContent : custRequestContents) {
                reqContent.put("custRequestId", context.get("custRequestId"));
                reqContent.put("contentId", ((Map<String, Object>) custRequestContent).get("contentId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createCustRequestContent", reqContent);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createCustRequestContent: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        Map<String, Object> updStat = new HashMap<>();
        // set-service-fields from "parameters" to "updStat" for service "setCommunicationEventStatus"
        updStat.putAll(UtilMisc.toMap(context));
        updStat.put("setRoleStatusToComplete", "Y");
        updStat.put("statusId", "COM_COMPLETE");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("setCommunicationEventStatus", updStat);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling setCommunicationEventStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("custRequestId", context.get("custRequestId"));
        List<String> successMessageList = new ArrayList<>();
        successMessageList.add("Customer request ${parameters.custRequestId} created");

        return "success";
    }


    /**
     * Create Customer request Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustRequestContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        String result = checkStatusCustRequest(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        newEntity = delegator.makeValue("CustRequestContent");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fromDate"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            newEntity.put("fromDate", nowTimestamp);
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Remove a Customer Request Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteCustRequestContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = checkStatusCustRequest(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("CustRequestContent")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustRequestContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) lookedUpValue).get("thruDate"))) {
            Timestamp lookedUpValue_thruDate = new Timestamp(System.currentTimeMillis());
        }
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result = updateCustRequestLastModifiedDate(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * update the modified date field in a customer request
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCustRequestLastModifiedDate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

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
        Timestamp custRequest_lastModifiedDate = new Timestamp(System.currentTimeMillis());
        try {
            delegator.store(custRequest);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }

}

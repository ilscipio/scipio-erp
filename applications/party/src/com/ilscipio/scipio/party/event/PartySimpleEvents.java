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
package com.ilscipio.scipio.party.event;

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
import org.ofbiz.content.layout.LayoutWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://party/script/org/ofbiz/party/party/PartySimpleEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PartySimpleEvents {

    private static final String MODULE = PartySimpleEvents.class.getName();


    /**
     * Delete Person
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteParty(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // simple-map-processor name: deleteParty
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteParty", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteParty: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create Person
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPerson(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPerson", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPerson: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update Person
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePerson(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        context.put("partyId", context.get("partyId"));
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePerson", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePerson: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create Party Group
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyGroup(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyGroup", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyGroup: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update Party Group
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyGroup(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        context.put("partyId", context.get("partyId"));
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePartyGroup", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePartyGroup: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Creates Party Associated Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> formInput = null;
        try {
            formInput = LayoutWorker.uploadImageAndParameters(request, "dataResourceName");
        } catch (Exception e) {
            Debug.logError(e, "Error calling LayoutWorker.uploadImageAndParameters: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        @SuppressWarnings("unchecked")
        Map<String, java.nio.ByteBuffer> byteData = (Map<String, java.nio.ByteBuffer>) (Map<?, ?>) formInput;
        java.nio.ByteBuffer byteWrap = null;
        try {
            byteWrap = LayoutWorker.returnByteBuffer(byteData);
        } catch (Exception e) {
            Debug.logError(e, "Error calling LayoutWorker.returnByteBuffer: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        @SuppressWarnings("unchecked")
        Map<String, Object> formInputMap = (Map<String, Object>) formInput.get("formInput");
        Map<String, Object> partyContentMap = new HashMap<>();
        // set-service-fields from "formInput.formInput" to "partyContentMap" for service "uploadPartyContentFile"
        partyContentMap.putAll(UtilMisc.toMap(formInputMap));
        partyContentMap.put("_uploadedFile_fileName", formInput.get("imageFileName"));
        partyContentMap.put("uploadedFile", byteWrap);
        partyContentMap.put("_uploadedFile_contentType", formInputMap.get("mimeTypeId"));
        Object contentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("uploadPartyContentFile", partyContentMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentId = serviceResult.get("contentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling uploadPartyContentFile: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        request.setAttribute("partyId", formInputMap.get("partyId"));
        request.setAttribute("contentId", contentId);

        return "success";
    }


    /**
     * Update Party Associated Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> formInput2 = null;
        try {
            formInput2 = LayoutWorker.uploadImageAndParameters(request, "dataResourceName");
        } catch (Exception e) {
            Debug.logError(e, "Error calling LayoutWorker.uploadImageAndParameters: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        @SuppressWarnings("unchecked")
        Map<String, java.nio.ByteBuffer> byteData2 = (Map<String, java.nio.ByteBuffer>) (Map<?, ?>) formInput2;
        java.nio.ByteBuffer byteWrap2 = null;
        try {
            byteWrap2 = LayoutWorker.returnByteBuffer(byteData2);
        } catch (Exception e) {
            Debug.logError(e, "Error calling LayoutWorker.returnByteBuffer: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        @SuppressWarnings("unchecked")
        Map<String, Object> formInputMap2 = (Map<String, Object>) formInput2.get("formInput");
        Map<String, Object> partyContentMap = new HashMap<>();
        // set-service-fields from "formInput.formInput" to "partyContentMap" for service "updateContentAndUploadedFile"
        partyContentMap.putAll(UtilMisc.toMap(formInputMap2));
        partyContentMap.put("uploadedFile", formInput2.get("imageData"));
        partyContentMap.put("_uploadedFile_fileName", formInput2.get("imageFileName"));
        Object contentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateContentAndUploadedFile", partyContentMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentId = serviceResult.get("contentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateContentAndUploadedFile: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        request.setAttribute("partyId", formInputMap2.get("partyId"));
        request.setAttribute("contentId", contentId);

        return "success";
    }


    /**
     * Edit GeoLocation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String editGeoLocation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue partyGeoPoint = null;
        Map<String, Object> updateGeoPointMap = null;
        Map<String, Object> createGeoPointMap = null;
        Object geoPointId = null;
        Timestamp nowTimestamp = null;
        if (UtilValidate.isEmpty(context.get("geoPointId"))) {
            createGeoPointMap.put("dataSourceId", "GEOPT_GOOGLE");
            createGeoPointMap.put("latitude", context.get("lat"));
            createGeoPointMap.put("longitude", context.get("lng"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createGeoPoint", createGeoPointMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                geoPointId = serviceResult.get("geoPointId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createGeoPoint: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // TODO: Convert <now> element
            partyGeoPoint = delegator.makeValue("PartyGeoPoint");
            partyGeoPoint.put("partyId", context.get("partyId"));
            partyGeoPoint.put("geoPointId", geoPointId);
            partyGeoPoint.put("fromDate", nowTimestamp);
            try {
                delegator.create(partyGeoPoint);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            updateGeoPointMap.put("geoPointId", context.get("geoPointId"));
            updateGeoPointMap.put("dataSourceId", "GEOPT_GOOGLE");
            updateGeoPointMap.put("latitude", context.get("lat"));
            updateGeoPointMap.put("longitude", context.get("lng"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateGeoPoint", updateGeoPointMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateGeoPoint: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }

}

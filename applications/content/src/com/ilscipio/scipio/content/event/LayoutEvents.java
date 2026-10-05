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
package com.ilscipio.scipio.content.event;

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
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.layout.LayoutWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://content/script/org/ofbiz/content/layout/LayoutEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class LayoutEvents {

    private static final String MODULE = LayoutEvents.class.getName();


    /**
     * Create Layout
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createLayout(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Debug.logInfo("in createLayout.", MODULE);
        GenericValue currentContent = delegator.makeValue("Content");
        currentContent.setPKFields((Map<String, Object>) context);
        currentContent.setNonPKFields((Map<String, Object>) context);
        context.putAll((Map<String, Object>) currentContent);
        context.put("contentPurposeTypeId", context.get("contentPurposeTypeId"));
        List<String> targetOperationList = new ArrayList<>();
        targetOperationList.add("CONTENT_CREATE");
        context.put("targetOperationList", targetOperationList);
        Object context_userLogin = request.getSession().getAttribute("userLogin");
        context.put("objectInfo", context.get("drObjectInfo"));
        context.put("dataResourceTypeId", "LOCAL_FILE");
        Object contentId = null;
        Object dataResourceId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentAndAssoc", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentId = serviceResult.get("contentId");
            dataResourceId = serviceResult.get("dataResourceId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentAndAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(contentId)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentPermissionNotGranted", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        request.setAttribute("contentId", contentId);
        request.setAttribute("drDataResourceId", dataResourceId);

        return "success";
    }


    /**
     * Update Layout
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateLayout(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Debug.logInfo("in updateLayout.", MODULE);
        GenericValue currentContent = delegator.makeValue("Content");
        currentContent.setPKFields((Map<String, Object>) context);
        currentContent.setNonPKFields((Map<String, Object>) context);
        context.put("currentContent", currentContent);
        context.put("contentPurposeTypeId", context.get("contentPurposeTypeId"));
        List<String> targetOperationList = new ArrayList<>();
        targetOperationList.add("CONTENT_CREATE");
        context.put("targetOperationList", targetOperationList);
        Object context_userLogin = request.getSession().getAttribute("userLogin");
        Object permissionStatus = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("checkContentPermission", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            permissionStatus = serviceResult.get("permissionStatus");
        } catch (Exception e) {
            Debug.logError(e, "Error calling checkContentPermission: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (!"granted".equals(permissionStatus)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentPermissionNotGranted", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue content = null;
        try {
            content = EntityQuery.use(delegator)
                    .from("Content")
                    .where(UtilMisc.toMap("contentId", context.get("contentId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key Content: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        content.setNonPKFields((Map<String, Object>) context);
        Debug.logInfo("content: " + content, MODULE);
        try {
            delegator.store(content);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        request.setAttribute("contentId", ((Map<String, Object>) content).get("contentId"));
        GenericValue dataResource = null;
        try {
            dataResource = EntityQuery.use(delegator)
                    .from("DataResource")
                    .where(UtilMisc.toMap("dataResourceId", context.get("dataResourceId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        dataResource.setNonPKFields((Map<String, Object>) context);
        dataResource.put("objectInfo", context.get("drObjectInfo"));
        Debug.logInfo("dataResource: " + dataResource, MODULE);
        try {
            delegator.store(dataResource);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        request.setAttribute("drDataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Create Layout Text
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createLayoutText(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object deactivateList = null;
        Debug.logInfo("in createLayoutText.", MODULE);
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        context.put("dataResourceName", ((Map<String, Object>) context).get("contentName"));
        context.put("contentPurposeTypeId", context.get("contentPurposeTypeId"));
        context.put("contentIdTo", context.get("contentIdTo"));
        context.put("textData", context.get("textData"));
        Object context_userLogin = request.getSession().getAttribute("userLogin");
        context.put("dataResourceTypeId", "ELECTRONIC_TEXT");
        context.put("mimeTypeId", "text/plain");
        context.put("contentAssocTypeId", "SUB_CONTENT");
        context.put("contentTypeId", "DOCUMENT");
        Map<String, Object> context2 = new HashMap<>();
        Object dataResourceId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentAndAssoc", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context2.put("activeContentId", serviceResult.get("contentId"));
            dataResourceId = serviceResult.get("dataResourceId");
            context2.put("contentAssocTypeId", serviceResult.get("contentAssocTypeId"));
            context2.put("fromDate", serviceResult.get("fromDate"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentAndAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        request.setAttribute("contentId", ((Map<String, Object>) context2).get("activeContentId"));
        request.setAttribute("drDataResourceId", dataResourceId);
        Object currentEntityName = "SubContentDataResourceView";
        request.setAttribute("currentEntityName", currentEntityName);
        context2.put("contentIdTo", context.get("contentIdTo"));
        context2.put("mapKey", context.get("mapKey"));
        if (UtilValidate.isNotEmpty(((Map<String, Object>) context2).get("activeContentId"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("deactivateAssocs", context2);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                deactivateList = serviceResult.get("deactivateList");
            } catch (Exception e) {
                Debug.logError(e, "Error calling deactivateAssocs: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Update Layout Text
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateLayoutText(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Debug.logInfo("in updateLayoutText.", MODULE);
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        context.put("dataResourceName", ((Map<String, Object>) context).get("contentName"));
        context.put("contentPurposeTypeId", context.get("contentPurposeTypeId"));
        context.put("contentIdTo", context.get("contentIdTo"));
        context.put("textData", context.get("textData"));
        Object context_userLogin = request.getSession().getAttribute("userLogin");
        Object contentId = null;
        Object dataResourceId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentAndAssoc", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentId = serviceResult.get("contentId");
            dataResourceId = serviceResult.get("dataResourceId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentAndAssoc: " + e.getMessage(), MODULE);
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
     * Create Layout Image
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createLayoutImage(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object deactivateList = null;
        Debug.logInfo("in createLayoutImage.", MODULE);
        Map<String, Object> formInput = null;
        try {
            formInput = LayoutWorker.uploadImageAndParameters(request, "imageData");
        } catch (Exception e) {
            Debug.logError(e, "Error calling LayoutWorker.uploadImageAndParameters: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object byteWrap = null;
        try {
            byteWrap = LayoutWorker.returnByteBuffer((Map) formInput);
        } catch (Exception e) {
            Debug.logError(e, "Error calling LayoutWorker.returnByteBuffer: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        formInput.remove("imageData");
        Debug.logInfo("formInput: " + formInput, MODULE);
        Debug.logInfo("byteWrap: " + byteWrap, MODULE);
        // TODO: Convert call-map-processor (in-map: formInput, out-map: context)
        // TODO: Convert call-map-processor (in-map: formInput, out-map: context)
        // TODO: Convert call-map-processor (in-map: formInput, out-map: context)
        context.put("dataResourceName", ((Map<String, Object>) context).get("contentName"));
        context.put("contentPurposeTypeId", ((Map<String, Object>) formInput).get("contentPurposeTypeId"));
        context.put("contentIdTo", ((Map<String, Object>) formInput).get("contentIdTo"));
        Object context_userLogin = request.getSession().getAttribute("userLogin");
        context.put("dataResourceTypeId", "IMAGE_OBJECT");
        context.put("mimeTypeId", "text/plain");
        context.put("contentAssocTypeId", "SUB_CONTENT");
        context.put("contentTypeId", "DOCUMENT");
        Map<String, Object> context2 = new HashMap<>();
        Object dataResourceId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentAndAssoc", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context2.put("activeContentId", serviceResult.get("contentId"));
            dataResourceId = serviceResult.get("dataResourceId");
            context2.put("contentAssocTypeId", serviceResult.get("contentAssocTypeId"));
            context2.put("fromDate", serviceResult.get("fromDate"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentAndAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        request.setAttribute("contentId", ((Map<String, Object>) context2).get("activeContentId"));
        request.setAttribute("drDataResourceId", dataResourceId);
        Object currentEntityName = "SubContentDataResourceView";
        request.setAttribute("currentEntityName", currentEntityName);
        context2.put("contentIdTo", ((Map<String, Object>) formInput).get("contentIdTo"));
        context2.put("mapKey", ((Map<String, Object>) formInput).get("mapKey"));
        if (UtilValidate.isNotEmpty(((Map<String, Object>) context2).get("activeContentId"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("deactivateAssocs", context2);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                deactivateList = serviceResult.get("deactivateList");
            } catch (Exception e) {
                Debug.logError(e, "Error calling deactivateAssocs: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create Layout URL
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createLayoutUrl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object deactivateList = null;
        Debug.logInfo("in createLayoutUrl", MODULE);
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        context.put("dataResourceName", ((Map<String, Object>) context).get("contentName"));
        context.put("contentPurposeTypeId", context.get("contentPurposeTypeId"));
        context.put("contentIdTo", context.get("contentIdTo"));
        Object context_userLogin = request.getSession().getAttribute("userLogin");
        context.put("dataResourceTypeId", "URL_RESOURCE");
        context.put("mimeTypeId", "text/plain");
        context.put("contentAssocTypeId", "SUB_CONTENT");
        context.put("contentTypeId", "DOCUMENT");
        Map<String, Object> context2 = new HashMap<>();
        Object dataResourceId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentAndAssoc", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context2.put("activeContentId", serviceResult.get("contentId"));
            dataResourceId = serviceResult.get("dataResourceId");
            context2.put("contentAssocTypeId", serviceResult.get("contentAssocTypeId"));
            context2.put("fromDate", serviceResult.get("fromDate"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentAndAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        request.setAttribute("contentId", ((Map<String, Object>) context2).get("activeContentId"));
        request.setAttribute("drDataResourceId", dataResourceId);
        Object currentEntityName = "SubContentDataResourceView";
        request.setAttribute("currentEntityName", currentEntityName);
        context2.put("contentIdTo", context.get("contentIdTo"));
        context2.put("mapKey", context.get("mapKey"));
        if (UtilValidate.isNotEmpty(((Map<String, Object>) context2).get("activeContentId"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("deactivateAssocs", context2);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                deactivateList = serviceResult.get("deactivateList");
            } catch (Exception e) {
                Debug.logError(e, "Error calling deactivateAssocs: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Update Layout URL
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateLayoutUrl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Debug.logInfo("in updateLayoutUrl.", MODULE);
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        context.put("dataResourceName", ((Map<String, Object>) context).get("contentName"));
        context.put("contentPurposeTypeId", context.get("contentPurposeTypeId"));
        context.put("contentIdTo", context.get("contentIdTo"));
        Object context_userLogin = request.getSession().getAttribute("userLogin");
        Object contentId = null;
        Object dataResourceId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentAndAssoc", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentId = serviceResult.get("contentId");
            dataResourceId = serviceResult.get("dataResourceId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentAndAssoc: " + e.getMessage(), MODULE);
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
     * Create Generic Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createGenericContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Map<String, Object> inMap = null;
        Map<String, Object> contentMap = null;
        List<GenericValue> contentAssoList = null;
        Map<String, Object> formInput = null;
        try {
            formInput = LayoutWorker.uploadImageAndParameters(request, "dataResourceName");
        } catch (Exception e) {
            Debug.logError(e, "Error calling LayoutWorker.uploadImageAndParameters: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ((UtilValidate.isEmpty(((Map<String, Object>) formInput.get("formInput")).get("contentId")) && UtilValidate.isEmpty(((Map<String, Object>) formInput).get("imageFileName")))) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentContentIdOrUploadFileIsMissing", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) formInput.get("formInput")).get("contentId"))) {
            Object inMap__uploadedFile_fileName = null;
            Object inMap_uploadedFile = null;
            Object inMap__uploadedFile_contentType = null;
            Object context_contentId = null;
            if ((java.util.Objects.equals(((Map<String, Object>) formInput).get("uploadMimeType"), ((Map<String, Object>) formInput.get("formInput")).get("mimeTypeId")) || "application/octet-stream".equals(((Map<String, Object>) formInput.get("formInput")).get("mimeTypeId")) || "".equals(((Map<String, Object>) formInput.get("formInput")).get("mimeTypeId")))) {
                // set-service-fields from "formInput.formInput" to "inMap" for service "createContentFromUploadedFile"
                inMap.putAll(UtilMisc.toMap(((Map<String, Object>) formInput).get("formInput")));
                inMap.put("_uploadedFile_fileName", ((Map<String, Object>) formInput).get("imageFileName"));
                inMap.put("uploadedFile", ((Map<String, Object>) formInput).get("imageData"));
                inMap.put("_uploadedFile_contentType", ((Map<String, Object>) formInput).get("uploadMimeType"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createContentFromUploadedFile", inMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("contentId", serviceResult.get("contentId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createContentFromUploadedFile: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentUploadFileTypeNotMatch", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        } else {
            context.put("contentId", ((Map<String, Object>) formInput.get("formInput")).get("contentId"));
        }
        // set-service-fields from "formInput.formInput" to "contentMap" for service "createContentAssoc"
        contentMap.putAll(UtilMisc.toMap(((Map<String, Object>) formInput).get("formInput")));
        if (UtilValidate.isNotEmpty(((Map<String, Object>) formInput.get("formInput")).get("contentIdFrom"))) {
            contentMap.put("contentAssocTypeId", "SUB_CONTENT");
            contentMap.put("contentIdFrom", ((Map<String, Object>) formInput.get("formInput")).get("contentIdFrom"));
            contentMap.put("contentId", ((Map<String, Object>) formInput.get("formInput")).get("contentIdFrom"));
            contentMap.put("contentIdTo", ((Map<String, Object>) context).get("contentId"));
            Timestamp contentMap_fromDate = new Timestamp(System.currentTimeMillis());
            try {
                contentAssoList = EntityQuery.use(delegator)
                        .from("ContentAssoc")
                        .where(UtilMisc.toMap("contentId", ((Map<String, Object>) contentMap).get("contentId"), "contentIdTo", ((Map<String, Object>) contentMap).get("contentIdTo")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(context.get("contentAssonList"))) {
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createContentAssoc", contentMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createContentAssoc: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }

}

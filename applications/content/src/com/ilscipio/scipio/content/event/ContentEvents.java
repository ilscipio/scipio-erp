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
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://content/script/org/ofbiz/content/content/ContentEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ContentEvents {

    private static final String MODULE = ContentEvents.class.getName();


    /**
     * Create Content And Purpose
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentAndPurpose(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object currentEntityMap = null;
        GenericValue newContentPurpose = null;
        Debug.logInfo("in createContentAndPurpose.", MODULE);
        GenericValue currentContent = delegator.makeValue("Content");
        currentContent.setPKFields((Map<String, Object>) context);
        currentContent.setNonPKFields((Map<String, Object>) context);
        // TODO: Convert call-map-processor (in-map: currentContent, out-map: currentContent)
        // simple-map-processor name: newDateContent
        context.putAll((Map<String, Object>) currentContent);
        Debug.logInfo("currentContent: " + currentContent, MODULE);
        context.put("contentPurposeTypeId", context.get("contentPurposeTypeId"));
        List<String> targetOperationList = new ArrayList<>();
        targetOperationList.add("CONTENT_CREATE");
        context.put("targetOperationList", targetOperationList);
        Object context_userLogin = request.getSession().getAttribute("userLogin");
        Object contentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContent", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentId = serviceResult.get("contentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContent: " + e.getMessage(), MODULE);
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
        Object entityMap = request.getSession().getAttribute("currentEntityMap");
        if (UtilValidate.isNotEmpty(entityMap)) {
            currentEntityMap = entityMap;
        }
        GenericValue contentPK = delegator.makeValue("Content");
        contentPK.put("contentId", contentId);
        Debug.logInfo("contentPK: " + contentPK, MODULE);
        ((Map<String, Object>) currentEntityMap).put("Content", contentPK);
        Debug.logInfo("currentEntityMap: " + currentEntityMap, MODULE);
        request.getSession().setAttribute("currentEntityMap", currentEntityMap);
        if (UtilValidate.isNotEmpty(context.get("contentPurposeTypeId"))) {
            Debug.logInfo("contentPurposeTypeId: " + context.get("contentPurposeTypeId"), MODULE);
            newContentPurpose = delegator.makeValue("ContentPurpose");
            newContentPurpose.put("contentPurposeTypeId", context.get("contentPurposeTypeId"));
            Debug.logInfo("contentId: " + contentId, MODULE);
            newContentPurpose.put("contentId", contentId);
            try {
                delegator.create(newContentPurpose);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("newContentPurpose: " + newContentPurpose, MODULE);
        }

        return "success";
    }


    /**
     * Update Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Debug.logInfo("/nin updateContent.", MODULE);
        Debug.logInfo("parameters: " + context, MODULE);
        GenericValue currentContent = delegator.makeValue("Content");
        currentContent.setPKFields((Map<String, Object>) context);
        currentContent.setNonPKFields((Map<String, Object>) context);
        // TODO: Convert call-map-processor (in-map: currentContent, out-map: currentContent)
        // simple-map-processor name: newDateContent
        Debug.logInfo("datesConverted: " + context.get("datesConverted"), MODULE);
        context.putAll((Map<String, Object>) currentContent);
        List<GenericValue> contentPurposeList = null;
        try {
            contentPurposeList = currentContent.getRelated("ContentPurpose", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related ContentPurpose: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        context.put("contentPurposeList", contentPurposeList);
        List<String> targetOperationList = new ArrayList<>();
        targetOperationList.add("CONTENT_UPDATE");
        context.put("targetOperationList", targetOperationList);
        Object context_userLogin = request.getSession().getAttribute("userLogin");
        Object contentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateContent", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentId = serviceResult.get("contentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateContent: " + e.getMessage(), MODULE);
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
     * Add Content Assoc
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String addContentAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Debug.logInfo("in addContentAssoc.", MODULE);
        Debug.logInfo("parameters: " + context, MODULE);
        Map<String, Object> context3 = new HashMap<>();
        context3.put("contentIdTo", context.get("contentIdTo"));
        context3.put("contentIdFrom", context.get("contentId"));
        context3.put("contentAssocTypeId", context.get("contentAssocTypeId"));
        Object context3_userLogin = request.getSession().getAttribute("userLogin");
        List<String> contentPurposeList = new ArrayList<>();
        contentPurposeList.add("_NA_");
        context3.put("contentPurposeList", contentPurposeList);
        List<String> targetOperationList = new ArrayList<>();
        targetOperationList.add("ASSOC_CONTENT");
        context3.put("targetOperationList", targetOperationList);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("assocContent", context3);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling assocContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create Document Tree
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createDocument(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> content = new HashMap<>();
        content.put("contentName", context.get("contentName"));
        content.put("contentTypeId", context.get("contentTypeId"));
        Object content_userLogin = request.getSession().getAttribute("userLogin");
        Map<String, Object> contentAssoc = new HashMap<>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContent", content);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentAssoc.put("contentIdTo", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        contentAssoc.put("contentId", context.get("contentId"));
        contentAssoc.put("contentAssocTypeId", context.get("contentAssocTypeId"));
        Object contentAssoc_userLogin = request.getSession().getAttribute("userLogin");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContentAssoc", contentAssoc);
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

        return "success";
    }

}

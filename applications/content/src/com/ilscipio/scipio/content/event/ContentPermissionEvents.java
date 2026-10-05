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
 * <p>Generated from: component://content/script/org/ofbiz/content/content/ContentPermissionEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ContentPermissionEvents {

    private static final String MODULE = ContentPermissionEvents.class.getName();


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

        GenericValue currentContent = null;
        String id = null;
        GenericValue newContentPurpose = null;
        currentContent = delegator.makeValue("Content");
        currentContent.setPKFields((Map<String, Object>) context);
        currentContent.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) currentContent).get("contentId"))) {
            delegator.setNextSubSeqId(currentContent, "contentId", 5, 1);
            Object contentId = currentContent.get("contentId");
            id = delegator.getNextSeqId("Content");
            currentContent.put("contentId", id);
        }
        Debug.logInfo("currentContent: " + currentContent, MODULE);
        List<Object> contentPurposeList = new LinkedList<>();
        contentPurposeList.add(context.get("contentPurposeTypeId"));
        List<String> targetOperationList = new ArrayList<>();
        targetOperationList.add("CONTENT_CREATE");
        context.put("currentContent", currentContent);
        context.put("contentPurposeList", contentPurposeList);
        context.put("targetOperationList", targetOperationList);
        context.put("currentContent", currentContent);
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
        Debug.logInfo("permissionStatus:" + permissionStatus, MODULE);
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
        if ("granted".equals(permissionStatus)) {
            try {
                delegator.create(currentContent);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(context.get("contentPurposeTypeId"))) {
                newContentPurpose = delegator.makeValue("ContentPurpose");
                newContentPurpose.put("contentPurposeTypeId", context.get("contentPurposeTypeId"));
                newContentPurpose.put("contentId", ((Map<String, Object>) currentContent).get("contentId"));
                try {
                    delegator.create(newContentPurpose);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }

}

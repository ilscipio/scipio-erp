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
 * <p>Generated from: component://order/script/org/ofbiz/order/request/CustRequestEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CustRequestEvents {

    private static final String MODULE = CustRequestEvents.class.getName();


    /**
     * Create Customer Request Content
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

        List<String> error_list = new LinkedList<>();

        Map<String, Object> inMap = new HashMap<>();
        Map<String, Object> contentMap = new HashMap<>();
        List<GenericValue> contentAssoList = null;
        Map<String, Object> formInput = null;
        try {
            formInput = LayoutWorker.uploadImageAndParameters(request, "dataResourceName");
        } catch (Exception e) {
            Debug.logError(e, "Error calling LayoutWorker.uploadImageAndParameters: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        @SuppressWarnings("unchecked")
        Map<String, Object> formInputMap = (Map<String, Object>) formInput.get("formInput");
        if ((UtilValidate.isEmpty(formInputMap.get("contentId")) && UtilValidate.isEmpty(formInput.get("imageFileName")))) {
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
        if (UtilValidate.isEmpty(formInputMap.get("contentId"))) {
            Object inMap__uploadedFile_fileName = null;
            Object inMap_uploadedFile = null;
            Object inMap__uploadedFile_contentType = null;
            Object context_contentId = null;
            if ((java.util.Objects.equals(formInput.get("uploadMimeType"), formInputMap.get("mimeTypeId")) || "".equals(formInputMap.get("mimeTypeId")))) {
                // set-service-fields from "formInput.formInput" to "inMap" for service "createContentFromUploadedFile"
                inMap.putAll(UtilMisc.toMap(formInput.get("formInput")));
                inMap.put("_uploadedFile_fileName", formInput.get("imageFileName"));
                inMap.put("uploadedFile", formInput.get("imageData"));
                inMap.put("_uploadedFile_contentType", formInput.get("uploadMimeType"));
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
            context.put("contentId", formInputMap.get("contentId"));
        }
        // set-service-fields from "formInput.formInput" to "contentMap" for service "createContentAssoc"
        contentMap.putAll(UtilMisc.toMap(formInput.get("formInput")));
        if (UtilValidate.isNotEmpty(formInputMap.get("contentIdFrom"))) {
            contentMap.put("contentAssocTypeId", "SUB_CONTENT");
            contentMap.put("contentIdFrom", formInputMap.get("contentIdFrom"));
            contentMap.put("contentId", formInputMap.get("contentIdFrom"));
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
        context.put("custRequestId", formInputMap.get("custRequestId"));
        // TODO: Convert call-map-processor (in-map: context, out-map: custRequestContext)
        // simple-map-processor name: newCustRequestContent
        Map<String, Object> custRequestContext = new HashMap<>();
        Object contentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCustRequestContent", (Map<String, Object>) custRequestContext);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentId = serviceResult.get("contentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCustRequestContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }

}

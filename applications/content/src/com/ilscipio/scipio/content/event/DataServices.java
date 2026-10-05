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

import java.nio.ByteBuffer;
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
import org.ofbiz.content.data.DataResourceWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://content/script/org/ofbiz/content/data/DataServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class DataServices {

    private static final String MODULE = DataServices.class.getName();


    /**
     * Create a Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        GenericValue statusItem = null;
        List<GenericValue> contentStatus = null;
        newEntity = delegator.makeValue("DataResource");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("dataResourceId"))) {
            ((GenericValue) newEntity).put("dataResourceId", delegator.getNextSeqId("DataResource"));
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        newEntity.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        newEntity.put("createdByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        newEntity.put("lastModifiedDate", nowTimestamp);
        newEntity.put("createdDate", nowTimestamp);
        if (UtilValidate.isEmpty(context.get("dataTemplateTypeId"))) {
            newEntity.put("dataTemplateTypeId", "NONE");
        }
        if (UtilValidate.isEmpty(context.get("statusId"))) {
            try {
                contentStatus = EntityQuery.use(delegator)
                        .from("StatusItem")
                        .where(UtilMisc.toMap("statusTypeId", "CONTENT_STATUS"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying StatusItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            statusItem = EntityUtil.getFirst((List<GenericValue>) contentStatus);
            newEntity.put("statusId", ((Map<String, Object>) statusItem).get("statusId"));
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("mimeTypeId"))) {
            if (UtilValidate.isNotEmpty(context.get("uploadedFile"))) {
                try {
                    ((Map<String, Object>) newEntity).put("mimeTypeId", DataResourceWorker.getMimeTypeWithByteBuffer((ByteBuffer) context.get("uploadedFile")));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling DataResourceWorker.getMimeTypeWithByteBuffer: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("dataResourceId", ((Map<String, Object>) newEntity).get("dataResourceId"));
        result.put("dataResource", newEntity);

        return "success";
    }


    /**
     * Update a Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResource")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        {
            Object _val = context.get("locale");
            context.put("locale", _val != null ? _val.toString() : null);
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        lookedUpValue.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        lookedUpValue.put("lastModifiedDate", nowTimestamp);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("dataResourceId", context.get("dataResourceId"));

        return "success";
    }


    /**
     * Delete a Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResource")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
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
     * Create a Data Resource and return the data resource type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createDataResourceAndAssocToContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> contentCtx = null;
        GenericValue content = null;
        try {
            content = EntityQuery.use(delegator)
                    .from("Content")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(content)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentErrorUiLabels", "layoutEvents.content_empty", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        Map<String, Object> dataResourceCtx = new HashMap<>();
        // set-service-fields from "parameters" to "dataResourceCtx" for service "createDataResource"
        dataResourceCtx.putAll(UtilMisc.toMap(context));
        Object dataResource = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createDataResource", dataResourceCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            dataResource = serviceResult.get("dataResource");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createDataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("Y".equals(context.get("templateDataResource"))) {
            contentCtx.put("templateDataResourceId", context.get("dataResourceId"));
        } else {
            contentCtx.put("dataResourceId", context.get("dataResourceId"));
        }
        contentCtx.put("contentId", context.get("contentId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateContent", contentCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contentId", context.get("contentId"));
        if ("ELECTRONIC_TEXT".equals(((Map<String, Object>) dataResource).get("dataResourceTypeId"))) {
            return "${dataResource.dataResourceTypeId}";
        }
        if ("IMAGE_OBJECT".equals(((Map<String, Object>) dataResource).get("dataResourceTypeId"))) {
            return "${dataResource.dataResourceTypeId}";
        }

        return "success";
    }


    /**
     * Create Data Resource Meta Data
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createDataResourceMetaData(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue newEntity = delegator.makeValue("DataResourceMetaData");
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
     * Update Data Resource Meta Data
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateDataResourceMetaData(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = delegator.makeValue("DataResourceMetaData");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResourceMetaData")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResourceMetaData: " + e.getMessage(), MODULE);
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
     * Remove Data Resource Meta Data
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeDataResourceMetaData(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = delegator.makeValue("DataResourceMetaData");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResourceMetaData")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResourceMetaData: " + e.getMessage(), MODULE);
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
     * Create Data Resource Purpose
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createDataResourcePurpose(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue newEntity = delegator.makeValue("DataResourcePurpose");
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
     * Update Data Resource Purpose
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateDataResourcePurpose(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = delegator.makeValue("DataResourcePurpose");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResourcePurpose")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResourcePurpose: " + e.getMessage(), MODULE);
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
     * Remove Data Resource Purpose
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeDataResourcePurpose(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = delegator.makeValue("DataResourcePurpose");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResourcePurpose")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResourcePurpose: " + e.getMessage(), MODULE);
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
     * Create Data Resource Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createDataResourceRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        if (UtilValidate.isNotEmpty(context.get("partyId"))) {
            newEntity = delegator.makeValue("DataResourceRole");
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
        }

        return "success";
    }


    /**
     * Update Data Resource Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateDataResourceRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("DataResourceRole");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResourceRole")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResourceRole: " + e.getMessage(), MODULE);
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
     * Remove Data Resource Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeDataResourceRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("DataResourceRole");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResourceRole")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResourceRole: " + e.getMessage(), MODULE);
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
     * Update Data Category
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateDataCategory(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("DataCategory");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataCategory")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataCategory: " + e.getMessage(), MODULE);
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
     * Remove Data DateCategory
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeDataCategory(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("DataCategory");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataCategory")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataCategory: " + e.getMessage(), MODULE);
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
     * Create Data Resource Type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createDataResourceType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("DataResourceType");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("dataResourceTypeId", ((Map<String, Object>) newEntity).get("dataResourceTypeId"));

        return "success";
    }


    /**
     * Update Data Resource Type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateDataResourceType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("DataResourceType");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResourceType")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResourceType: " + e.getMessage(), MODULE);
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
     * Remove Data Resource Type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeDataResourceType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("DataResourceType");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResourceType")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResourceType: " + e.getMessage(), MODULE);
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
     * Create Data Resource Attribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createDataResourceAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("DataResourceAttribute");
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
     * Update Data Resource Attribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateDataResourceAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("DataResourceAttribute");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResourceAttribute")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResourceAttribute: " + e.getMessage(), MODULE);
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
     * Remove Data Resource Attribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeDataResourceAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("DataResourceAttribute");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResourceAttribute")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResourceAttribute: " + e.getMessage(), MODULE);
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
     * Create Data Resource Type Attribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createDataResourceTypeAttr(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("DataResourceTypeAttr");
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
     * Update Data Resource Type Attribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateDataResourceTypeAttr(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("DataResourceTypeAttr");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResourceTypeAttr")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResourceTypeAttr: " + e.getMessage(), MODULE);
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
     * Remove Data Resource Type Attribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeDataResourceTypeAttr(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("DataResourceTypeAttr");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("DataResourceTypeAttr")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key DataResourceTypeAttr: " + e.getMessage(), MODULE);
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
     * Create Character Set
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCharacterSet(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("CharacterSet");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("characterSetId", ((Map<String, Object>) newEntity).get("characterSetId"));

        return "success";
    }


    /**
     * Update Character Set
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCharacterSet(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("CharacterSet");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("CharacterSet")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CharacterSet: " + e.getMessage(), MODULE);
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
     * Remove Character Set
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeCharacterSet(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("CharacterSet");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("CharacterSet")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CharacterSet: " + e.getMessage(), MODULE);
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
     * Create Data Resource Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFileExtension(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("FileExtension");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("fileExtensionId", ((Map<String, Object>) newEntity).get("fileExtensionId"));

        return "success";
    }


    /**
     * Update Data Resource Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFileExtension(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("FileExtension");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FileExtension")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key FileExtension: " + e.getMessage(), MODULE);
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
     * Remove Data Resource Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeFileExtension(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("FileExtension");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FileExtension")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key FileExtension: " + e.getMessage(), MODULE);
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
     * Create Data Resource Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createMetaDataPredicate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("MetaDataPredicate");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("metaDataPredicateId", ((Map<String, Object>) newEntity).get("metaDataPredicateId"));

        return "success";
    }


    /**
     * Update Data Resource Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateMetaDataPredicate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("MetaDataPredicate");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("MetaDataPredicate")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key MetaDataPredicate: " + e.getMessage(), MODULE);
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
     * Remove Data Resource Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeMetaDataPredicate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("MetaDataPredicate");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("MetaDataPredicate")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key MetaDataPredicate: " + e.getMessage(), MODULE);
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
     * Create MimeType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createMimeType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("MimeType");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("mimeTypeId", ((Map<String, Object>) newEntity).get("mimeTypeId"));

        return "success";
    }


    /**
     * Update MimeType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateMimeType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("MimeType");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("MimeType")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key MimeType: " + e.getMessage(), MODULE);
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
     * Remove MimeType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeMimeType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("MimeType");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("MimeType")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key MimeType: " + e.getMessage(), MODULE);
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
     * Create MimeTypeHtmlTemplate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createMimeTypeHtmlTemplate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("MimeTypeHtmlTemplate");
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
     * Update MimeTypeHtmlTemplate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateMimeTypeHtmlTemplate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("MimeTypeHtmlTemplate");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("MimeTypeHtmlTemplate")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key MimeTypeHtmlTemplate: " + e.getMessage(), MODULE);
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
     * Remove MimeTypeHtmlTemplate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeMimeTypeHtmlTemplate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("MimeTypeHtmlTemplate");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("MimeTypeHtmlTemplate")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key MimeTypeHtmlTemplate: " + e.getMessage(), MODULE);
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
     * Create Electronic Text
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createElectronicText(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("ElectronicText");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("dataResourceId", ((Map<String, Object>) newEntity).get("dataResourceId"));

        return "success";
    }


    /**
     * Update Electronic Text
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateElectronicText(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue lookupKeyValue = delegator.makeValue("ElectronicText");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ElectronicText")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ElectronicText: " + e.getMessage(), MODULE);
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
        result.put("dataResourceId", ((Map<String, Object>) lookedUpValue).get("dataResourceId"));

        return "success";
    }


    /**
     * Create Electronic Text with Form code
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createElectronicTextForm(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("ElectronicText");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("dataResourceId", ((Map<String, Object>) newEntity).get("dataResourceId"));

        return "success";
    }


    /**
     * Update Electronic Text with Form code
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateElectronicTextForm(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue lookupKeyValue = delegator.makeValue("ElectronicText");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ElectronicText")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ElectronicText: " + e.getMessage(), MODULE);
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
        result.put("dataResourceId", ((Map<String, Object>) lookedUpValue).get("dataResourceId"));

        return "success";
    }


    /**
     * Remove Electronic Text
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeElectronicText(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ElectronicText");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ElectronicText")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ElectronicText: " + e.getMessage(), MODULE);
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
     * Create Image Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createImageDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("ImageDataResource");
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
     * Update Image Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateImageDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ImageDataResource");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ImageDataResource")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ImageDataResource: " + e.getMessage(), MODULE);
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
     * Remove Image Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeImageDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ImageDataResource");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ImageDataResource")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ImageDataResource: " + e.getMessage(), MODULE);
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
     * Create Video Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createVideoDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("VideoDataResource");
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
     * Update Video Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateVideoDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("VideoDataResource");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("VideoDataResource")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key VideoDataResource: " + e.getMessage(), MODULE);
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
     * Remove Video Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeVideoDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("VideoDataResource");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("VideoDataResource")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key VideoDataResource: " + e.getMessage(), MODULE);
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
     * Create Audio Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAudioDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("AudioDataResource");
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
     * Update Audio Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAudioDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("AudioDataResource");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("AudioDataResource")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key AudioDataResource: " + e.getMessage(), MODULE);
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
     * Remove Audio Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeAudioDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("AudioDataResource");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("AudioDataResource")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key AudioDataResource: " + e.getMessage(), MODULE);
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
     * Create Other Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createOtherDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("OtherDataResource");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("created new OtherDataResource: " + newEntity, MODULE);

        return "success";
    }


    /**
     * Update Other Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateOtherDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("OtherDataResource");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("OtherDataResource")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key OtherDataResource: " + e.getMessage(), MODULE);
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
     * Remove Other Data Resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeOtherDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("OtherDataResource");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("OtherDataResource")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key OtherDataResource: " + e.getMessage(), MODULE);
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
     * Get Electronic Text
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getElectronicText(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue currentContent = null;
        userLogin = (GenericValue) context.get("userLogin");
        currentContent = (GenericValue) context.get("content");
        Debug.logInfo("GETELECTRONICTEXT, currentContent:" + currentContent, MODULE);
        if (UtilValidate.isEmpty(currentContent)) {
            if (UtilValidate.isNotEmpty(context.get("contentId"))) {
                try {
                    currentContent = EntityQuery.use(delegator)
                            .from("Content")
                            .where(UtilMisc.toMap())
                            .cache()
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            if (UtilValidate.isEmpty(currentContent)) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNeitherContentSupplied", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) currentContent).get("dataResourceId"))) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentDataResourceNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        result.put("dataResourceId", ((Map<String, Object>) currentContent).get("dataResourceId"));
        GenericValue eText = null;
        try {
            eText = EntityQuery.use(delegator)
                    .from("ElectronicText")
                    .where(UtilMisc.toMap("dataResourceId", ((Map<String, Object>) currentContent).get("dataResourceId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ElectronicText: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(eText)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentElectronicTextNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        result.put("textData", ((Map<String, Object>) eText).get("textData"));

        return "success";
    }


    /**
     * Attach an uploaded file to a data resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String attachUploadToDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue dataResObj = null;
        Object isUpdate = null;
        isUpdate = "false";
        String forceLocal = UtilProperties.getMessage("content.properties", "content.upload.always.local.file", locale);
        Object parameters_dataResourceTypeId = null;
        if (("true".equals(forceLocal) && !(("LOCAL_FILE".equals(context.get("dataResourceTypeId")) || "OFBIZ_FILE".equals(context.get("dataResourceTypeId")) || "CONTEXT_FILE".equals(context.get("dataResourceTypeId")) || "LOCAL_FILE_BIN".equals(context.get("dataResourceTypeId")) || "OFBIZ_FILE_BIN".equals(context.get("dataResourceTypeId")) || "CONTEXT_FILE_BIN".equals(context.get("dataResourceTypeId")))))) {
            context.put("dataResourceTypeId", "LOCAL_FILE");
        }
        if (UtilValidate.isEmpty(context.get("dataResourceTypeId"))) {
            if (!(UtilValidate.isEmpty(context.get("_uploadedFile_contentType")))) {
                if (true /* TODO: if-regexp */) {
                    context.put("dataResourceTypeId", "IMAGE_OBJECT");
                } else {
                    context.put("dataResourceTypeId", "OTHER_OBJECT");
                }
            } else {
                context.put("dataResourceTypeId", "OTHER_OBJECT");
            }
        }
        if (("LOCAL_FILE".equals(context.get("dataResourceTypeId")) || "LOCAL_FILE_BIN".equals(context.get("dataResourceTypeId")))) {
            String result = saveLocalFileDataResource(request, response);
            if (!"success".equals(result)) {
                return result;
            }
            return "success";
        }
        if (("OFBIZ_FILE".equals(context.get("dataResourceTypeId")) || "OFBIZ_FILE_BIN".equals(context.get("dataResourceTypeId")))) {
            String result = saveOfbizFileDataResource(request, response);
            if (!"success".equals(result)) {
                return result;
            }
            return "success";
        }
        if (("CONTEXT_FILE".equals(context.get("dataResourceTypeId")) || "CONTEXT_FILE_BIN".equals(context.get("dataResourceTypeId")))) {
            String result = saveContextFileDataResource(request, response);
            if (!"success".equals(result)) {
                return result;
            }
            return "success";
        }
        if ("IMAGE_OBJECT".equals(context.get("dataResourceTypeId"))) {
            try {
                dataResObj = EntityQuery.use(delegator)
                        .from("ImageDataResource")
                        .where(UtilMisc.toMap("dataResourceId", context.get("dataResourceId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ImageDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(dataResObj)) {
                isUpdate = "true";
            }
            String result = saveImageObjectDateResource(request, response);
            if (!"success".equals(result)) {
                return result;
            }
            return "success";
        }
        if ("VIDEO_OBJECT".equals(context.get("dataResourceTypeId"))) {
            try {
                dataResObj = EntityQuery.use(delegator)
                        .from("VideoDataResource")
                        .where(UtilMisc.toMap("dataResourceId", context.get("dataResourceId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying VideoDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(dataResObj)) {
                isUpdate = "true";
            }
            String result = saveVideoObjectDateResource(request, response);
            if (!"success".equals(result)) {
                return result;
            }
            return "success";
        }
        if ("AUDIO_OBJECT".equals(context.get("dataResourceTypeId"))) {
            try {
                dataResObj = EntityQuery.use(delegator)
                        .from("AudioDataResource")
                        .where(UtilMisc.toMap("dataResourceId", context.get("dataResourceId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying AudioDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(dataResObj)) {
                isUpdate = "true";
            }
            String result = saveAudioObjectDateResource(request, response);
            if (!"success".equals(result)) {
                return result;
            }
            return "success";
        }
        if ("OTHER_OBJECT".equals(context.get("dataResourceTypeId"))) {
            try {
                dataResObj = EntityQuery.use(delegator)
                        .from("OtherDataResource")
                        .where(UtilMisc.toMap("dataResourceId", context.get("dataResourceId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OtherDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(dataResObj)) {
                isUpdate = "true";
            }
            String result = saveOtherObjectDateResource(request, response);
            if (!"success".equals(result)) {
                return result;
            }
            return "success";
        }
        {
            String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentDataTypeNotYetSupported", locale);
            error_list.add(errorMsg);
            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Attach an uploaded file to a data resource as LOCAL_FILE
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String saveLocalFileDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object isUpdate = null;
        GenericValue dataResource = null;
        try {
            dataResource = EntityQuery.use(delegator)
                    .from("DataResource")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(dataResource)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentDataResourceNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        } else {
            if (UtilValidate.isNotEmpty(((Map<String, Object>) dataResource).get("objectInfo"))) {
                isUpdate = "Y";
            }
        }
        if (UtilValidate.isEmpty(context.get("_uploadedFile_fileName"))) {
            if ((UtilValidate.isEmpty(isUpdate) || !"Y".equals(isUpdate))) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoUploadedContentFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            } else {
                result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
                result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));
                return "success";
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Boolean absolute = Boolean.TRUE;
        Object uploadPath = null;
        try {
            uploadPath = DataResourceWorker.getDataResourceContentUploadPath(delegator, absolute);
        } catch (Exception e) {
            Debug.logError(e, "Error calling DataResourceWorker.getDataResourceContentUploadPath: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("[attachLocalFileToDataResource] - Found Subdir : " + uploadPath, MODULE);
        Map<String, Object> extenLookup = new HashMap<>();
        extenLookup.put("mimeTypeId", context.get("_uploadedFile_contentType"));
        // TODO: Convert <find-by-and> element
        GenericValue extension = EntityUtil.getFirst((List<GenericValue>) context.get("extensions"));
        dataResource.put("dataResourceName", context.get("_uploadedFile_fileName"));
        dataResource.put("objectInfo", uploadPath + "/" + ((Map<String, Object>) dataResource).get("dataResourceId"));
        if (UtilValidate.isNotEmpty(extension)) {
            dataResource.put("objectInfo", uploadPath + "/" + ((Map<String, Object>) dataResource).get("dataResourceId") + "." + ((Map<String, Object>) extension).get("fileExtensionId"));
        }
        dataResource.put("dataResourceTypeId", context.get("dataResourceTypeId"));
        try {
            delegator.store(dataResource);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> fileCtx = new HashMap<>();
        // set-service-fields from "dataResource" to "fileCtx" for service "createAnonFile"
        fileCtx.putAll(UtilMisc.toMap(dataResource));
        fileCtx.put("binData", context.get("uploadedFile"));
        fileCtx.put("dataResource", dataResource);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAnonFile", fileCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAnonFile: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
        result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));

        return "success";
    }


    /**
     * Attach an uploaded file to a data resource as OFBIZ_FILE
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String saveOfbizFileDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object isUpdate = null;
        GenericValue dataResource = null;
        try {
            dataResource = EntityQuery.use(delegator)
                    .from("DataResource")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(dataResource)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentDataResourceNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        } else {
            if (UtilValidate.isNotEmpty(((Map<String, Object>) dataResource).get("objectInfo"))) {
                isUpdate = "Y";
            }
        }
        if (UtilValidate.isEmpty(context.get("_uploadedFile_fileName"))) {
            if ((UtilValidate.isEmpty(isUpdate) || !"Y".equals(isUpdate))) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoUploadedContentFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            } else {
                result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
                result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));
                return "success";
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Boolean absolute = Boolean.FALSE;
        Object uploadPath = null;
        try {
            uploadPath = DataResourceWorker.getDataResourceContentUploadPath(delegator, absolute);
        } catch (Exception e) {
            Debug.logError(e, "Error calling DataResourceWorker.getDataResourceContentUploadPath: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("[attachLocalFileToDataResource] - Found Subdir : " + uploadPath, MODULE);
        Map<String, Object> extenLookup = new HashMap<>();
        extenLookup.put("mimeTypeId", context.get("_uploadedFile_contentType"));
        // TODO: Convert <find-by-and> element
        GenericValue extension = EntityUtil.getFirst((List<GenericValue>) context.get("extensions"));
        dataResource.put("dataResourceName", context.get("_uploadedFile_fileName"));
        dataResource.put("mimeTypeId", context.get("_uploadedFile_contentType"));
        dataResource.put("objectInfo", uploadPath + "/" + ((Map<String, Object>) dataResource).get("dataResourceId"));
        if (UtilValidate.isNotEmpty(extension)) {
            dataResource.put("objectInfo", uploadPath + "/" + ((Map<String, Object>) dataResource).get("dataResourceId") + "." + ((Map<String, Object>) extension).get("fileExtensionId"));
        }
        dataResource.put("dataResourceTypeId", context.get("dataResourceTypeId"));
        try {
            delegator.store(dataResource);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> fileCtx = new HashMap<>();
        // set-service-fields from "dataResource" to "fileCtx" for service "createAnonFile"
        fileCtx.putAll(UtilMisc.toMap(dataResource));
        fileCtx.put("binData", context.get("uploadedFile"));
        fileCtx.put("dataResource", dataResource);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAnonFile", fileCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAnonFile: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
        result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));

        return "success";
    }


    /**
     * Attach an uploaded file to a data resource as OTHER_OBJECT
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String saveOtherObjectDateResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue dataResource = null;
        try {
            dataResource = EntityQuery.use(delegator)
                    .from("DataResource")
                    .where(UtilMisc.toMap("dataResourceId", context.get("dataResourceId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(dataResource)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentDataResourceNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(context.get("_uploadedFile_fileName"))) {
            if ((UtilValidate.isEmpty(context.get("isUpdate")) || !"Y".equals(context.get("isUpdate")))) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoUploadedContentFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            } else {
                result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
                result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));
                return "success";
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        dataResource.put("dataResourceTypeId", context.get("dataResourceTypeId"));
        dataResource.put("dataResourceName", context.get("_uploadedFile_fileName"));
        dataResource.put("mimeTypeId", context.get("_uploadedFile_contentType"));
        try {
            delegator.store(dataResource);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> serviceContext = new HashMap<>();
        // set-service-fields from "dataResource" to "serviceContext" for service "createOtherDataResource"
        serviceContext.putAll(UtilMisc.toMap(dataResource));
        serviceContext.put("dataResourceContent", context.get("uploadedFile"));
        if ("true".equals(context.get("isUpdate"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateOtherDataResource", serviceContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateOtherDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createOtherDataResource", serviceContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createOtherDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
        result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));

        return "success";
    }


    /**
     * Attach an uploaded file to a data resource as IMAGE_OBJECT
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String saveImageObjectDateResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue dataResource = null;
        try {
            dataResource = EntityQuery.use(delegator)
                    .from("DataResource")
                    .where(UtilMisc.toMap("dataResourceId", context.get("dataResourceId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(dataResource)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentDataResourceNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(context.get("_uploadedFile_fileName"))) {
            if ((UtilValidate.isEmpty(context.get("isUpdate")) || !"Y".equals(context.get("isUpdate")))) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoUploadedContentFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            } else {
                result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
                result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));
                return "success";
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        dataResource.put("dataResourceTypeId", context.get("dataResourceTypeId"));
        dataResource.put("dataResourceName", context.get("_uploadedFile_fileName"));
        dataResource.put("mimeTypeId", context.get("_uploadedFile_contentType"));
        try {
            delegator.store(dataResource);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> serviceContext = new HashMap<>();
        // set-service-fields from "dataResource" to "serviceContext" for service "createImageDataResource"
        serviceContext.putAll(UtilMisc.toMap(dataResource));
        serviceContext.put("imageData", context.get("uploadedFile"));
        if ("true".equals(context.get("isUpdate"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateImageDataResource", serviceContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateImageDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createImageDataResource", serviceContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createImageDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
        result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));

        return "success";
    }


    /**
     * Attach an uploaded file to a data resource as VIDEO_OBJECT
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String saveVideoObjectDateResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue dataResource = null;
        try {
            dataResource = EntityQuery.use(delegator)
                    .from("DataResource")
                    .where(UtilMisc.toMap("dataResourceId", context.get("dataResourceId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(dataResource)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentDataResourceNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(context.get("_uploadedFile_fileName"))) {
            if ((UtilValidate.isEmpty(context.get("isUpdate")) || !"Y".equals(context.get("isUpdate")))) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoUploadedContentFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            } else {
                result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
                result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));
                return "success";
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        dataResource.put("dataResourceTypeId", context.get("dataResourceTypeId"));
        dataResource.put("dataResourceName", context.get("_uploadedFile_fileName"));
        dataResource.put("mimeTypeId", context.get("_uploadedFile_contentType"));
        try {
            delegator.store(dataResource);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> serviceContext = new HashMap<>();
        // set-service-fields from "dataResource" to "serviceContext" for service "createVideoDataResource"
        serviceContext.putAll(UtilMisc.toMap(dataResource));
        serviceContext.put("videoData", context.get("uploadedFile"));
        if ("true".equals(context.get("isUpdate"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateVideoDataResource", serviceContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateVideoDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createVideoDataResource", serviceContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createVideoDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
        result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));

        return "success";
    }


    /**
     * Attach an uploaded file to a data resource as AUDIO_OBJECT
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String saveAudioObjectDateResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue dataResource = null;
        try {
            dataResource = EntityQuery.use(delegator)
                    .from("DataResource")
                    .where(UtilMisc.toMap("dataResourceId", context.get("dataResourceId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(dataResource)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentDataResourceNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(context.get("_uploadedFile_fileName"))) {
            if ((UtilValidate.isEmpty(context.get("isUpdate")) || !"Y".equals(context.get("isUpdate")))) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoUploadedContentFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            } else {
                result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
                result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));
                return "success";
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        dataResource.put("dataResourceTypeId", context.get("dataResourceTypeId"));
        dataResource.put("dataResourceName", context.get("_uploadedFile_fileName"));
        dataResource.put("mimeTypeId", context.get("_uploadedFile_contentType"));
        try {
            delegator.store(dataResource);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> serviceContext = new HashMap<>();
        // set-service-fields from "dataResource" to "serviceContext" for service "createAudioDataResource"
        serviceContext.putAll(UtilMisc.toMap(dataResource));
        serviceContext.put("audioData", context.get("uploadedFile"));
        if ("true".equals(context.get("isUpdate"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateAudioDataResource", serviceContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateAudioDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createAudioDataResource", serviceContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createAudioDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
        result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));

        return "success";
    }


    /**
     * Attach an uploaded file to a data resource as CONTEXT_FILE
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String saveContextFileDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object isUpdate = null;
        GenericValue dataResource = null;
        try {
            dataResource = EntityQuery.use(delegator)
                    .from("DataResource")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(dataResource)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentDataResourceNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        } else {
            if (UtilValidate.isNotEmpty(((Map<String, Object>) dataResource).get("objectInfo"))) {
                isUpdate = "Y";
            }
        }
        if (UtilValidate.isEmpty(context.get("_uploadedFile_fileName"))) {
            if ((UtilValidate.isEmpty(isUpdate) || !"Y".equals(isUpdate))) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoUploadedContentFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            } else {
                result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
                result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));
                return "success";
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object uploadPath = context.get("rootDir");
        Debug.logInfo("[attachLocalFileToDataResource] - Found Subdir : " + uploadPath, MODULE);
        if (UtilValidate.isEmpty(uploadPath)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentErrorUiLabels", "uploadContentAndImage.noRootDirProvided", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        Debug.logInfo("[attachLocalFileToDataResource] - Found Subdir : " + uploadPath, MODULE);
        Map<String, Object> extenLookup = new HashMap<>();
        extenLookup.put("mimeTypeId", context.get("_uploadedFile_contentType"));
        // TODO: Convert <find-by-and> element
        GenericValue extension = EntityUtil.getFirst((List<GenericValue>) context.get("extensions"));
        dataResource.put("dataResourceName", context.get("_uploadedFile_fileName"));
        dataResource.put("mimeTypeId", context.get("_uploadedFile_contentType"));
        dataResource.put("objectInfo", uploadPath + "/" + ((Map<String, Object>) dataResource).get("dataResourceId"));
        if (UtilValidate.isNotEmpty(extension)) {
            dataResource.put("objectInfo", uploadPath + "/" + ((Map<String, Object>) dataResource).get("dataResourceId") + "." + ((Map<String, Object>) extension).get("fileExtensionId"));
        }
        dataResource.put("dataResourceTypeId", context.get("dataResourceTypeId"));
        try {
            delegator.store(dataResource);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> fileCtx = new HashMap<>();
        // set-service-fields from "dataResource" to "fileCtx" for service "createAnonFile"
        fileCtx.putAll(UtilMisc.toMap(dataResource));
        fileCtx.put("binData", context.get("uploadedFile"));
        fileCtx.put("dataResource", dataResource);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAnonFile", fileCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAnonFile: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
        result.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));

        return "success";
    }

}

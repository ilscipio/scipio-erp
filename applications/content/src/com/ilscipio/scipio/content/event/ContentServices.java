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

import java.lang.Math;
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
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.common.UrlServletHelper;
import org.ofbiz.content.content.ContentKeywordIndex;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://content/script/org/ofbiz/content/content/ContentServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ContentServices {

    private static final String MODULE = ContentServices.class.getName();


    /**
     * Create a Content Record
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue statusItem = null;
        GenericValue content = null;
        List<GenericValue> contentStatus = null;
        content = delegator.makeValue("Content");
        content.setNonPKFields((Map<String, Object>) context);
        content.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(context.get("contentId"))) {
            ((GenericValue) content).put("contentId", delegator.getNextSeqId("Content"));
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) content).get("statusId"))) {
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
            content.put("statusId", ((Map<String, Object>) statusItem).get("statusId"));
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        content.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        content.put("createdByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        content.put("lastModifiedDate", nowTimestamp);
        content.put("createdDate", nowTimestamp);
        try {
            delegator.create(content);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contentId", ((Map<String, Object>) content).get("contentId"));

        return "success";
    }


    /**
     * Update a Content Record
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

        Map<String, Object> result = new HashMap<>();

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
        content.setNonPKFields((Map<String, Object>) context);
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        content.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        content.put("lastModifiedDate", nowTimestamp);
        try {
            delegator.store(content);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contentId", ((Map<String, Object>) content).get("contentId"));

        return "success";
    }


    /**
     * Remove a Content Record
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("Content");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue content = null;
        try {
            content = EntityQuery.use(delegator)
                    .from("Content")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key Content: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(content);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Remove a Content Record, related resource(s) and assocs.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentAndRelated(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue documentDataResource = null;
        GenericValue otherDataResource = null;
        GenericValue imageDataResource = null;
        GenericValue audioDataResource = null;
        GenericValue videoDataResource = null;
        Object dataResourceTypeId = null;
        GenericValue electronicText = null;
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
        // TODO: Convert <remove-related> element
        // TODO: Convert <remove-related> element
        // TODO: Convert <remove-related> element
        // TODO: Convert <remove-related> element
        // TODO: Convert <remove-related> element
        try {
            delegator.removeValue(content);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue dataResource = null;
        try {
            dataResource = content.getRelatedOne("DataResource", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one DataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(dataResource)) {
            dataResourceTypeId = ((Map<String, Object>) dataResource).get("dataResourceTypeId");
            if ("IMAGE_OBJECT".equals(dataResourceTypeId)) {
                try {
                    imageDataResource = dataResource.getRelatedOne("ImageDataResource", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one ImageDataResource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(imageDataResource)) {
                    try {
                        delegator.removeValue(imageDataResource);
                    } catch (Exception e) {
                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
            if ("VIDEO_OBJECT".equals(dataResourceTypeId)) {
                try {
                    videoDataResource = dataResource.getRelatedOne("VideoDataResource", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one VideoDataResource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(videoDataResource)) {
                    try {
                        delegator.removeValue(videoDataResource);
                    } catch (Exception e) {
                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
            if ("AUDIO_OBJECT".equals(dataResourceTypeId)) {
                try {
                    audioDataResource = dataResource.getRelatedOne("AudioDataResource", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one AudioDataResource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(audioDataResource)) {
                    try {
                        delegator.removeValue(audioDataResource);
                    } catch (Exception e) {
                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
            if ("DOCUMENT_OBJECT".equals(dataResourceTypeId)) {
                try {
                    documentDataResource = dataResource.getRelatedOne("DocumentDataResource", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one DocumentDataResource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(documentDataResource)) {
                    try {
                        delegator.removeValue(documentDataResource);
                    } catch (Exception e) {
                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
            if ("OTHER_OBJECT".equals(dataResourceTypeId)) {
                try {
                    otherDataResource = dataResource.getRelatedOne("OtherDataResource", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one OtherDataResource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(otherDataResource)) {
                    try {
                        delegator.removeValue(otherDataResource);
                    } catch (Exception e) {
                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
            try {
                electronicText = dataResource.getRelatedOne("ElectronicText", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one ElectronicText: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(electronicText)) {
                try {
                    delegator.removeValue(electronicText);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            // TODO: Convert <remove-related> element
            // TODO: Convert <remove-related> element
            try {
                delegator.removeValue(dataResource);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Remove a Content Record, related resource(s) and assocs, and To content records recursively (SCIPIO)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentAndRelatedRecursiveTo(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> contentAssocList = null;
        Map<String, Object> assocRemoveCtx = null;
        if ("all".equals(context.get("recursiveTarget"))) {
            try {
                contentAssocList = EntityQuery.use(delegator)
                        .from("ContentAssoc")
                        .where(UtilMisc.toMap("contentId", context.get("contentId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if ("active".equals(context.get("recursiveTarget"))) {
            try {
                contentAssocList = EntityQuery.use(delegator)
                        .from("ContentAssoc")
                        .where(UtilMisc.toMap("contentId", context.get("contentId")))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (contentAssocList != null) {
            for (GenericValue contentAssoc : contentAssocList) {
                if (!java.util.Objects.equals(context.get("contentId"), ((Map<String, Object>) contentAssoc).get("contentIdTo"))) {
                    assocRemoveCtx = new HashMap<>();
                    // set-service-fields from "parameters" to "assocRemoveCtx" for service "removeContentAndRelatedRecursiveTo"
                    assocRemoveCtx.putAll(UtilMisc.toMap(context));
                    assocRemoveCtx.put("contentId", ((Map<String, Object>) contentAssoc).get("contentIdTo"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("removeContentAndRelatedRecursiveTo", assocRemoveCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling removeContentAndRelatedRecursiveTo: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        String result = removeContentAndRelated(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Create a ContntAssoc Record
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue assoc = null;
        assoc = delegator.makeValue("ContentAssoc");
        assoc.setNonPKFields((Map<String, Object>) context);
        assoc.setPKFields((Map<String, Object>) context);
        assoc.put("contentId", context.get("contentIdFrom"));
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        if (UtilValidate.isEmpty(((Map<String, Object>) assoc).get("fromDate"))) {
            assoc.put("fromDate", nowTimestamp);
        }
        assoc.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        assoc.put("createdByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        assoc.put("lastModifiedDate", nowTimestamp);
        assoc.put("createdDate", nowTimestamp);
        try {
            delegator.create(assoc);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("assoc: " + assoc, MODULE);
        result.put("fromDate", ((Map<String, Object>) assoc).get("fromDate"));

        return "success";
    }


    /**
     * Update a ContentAssoc Record
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object contentId = context.get("contentIdFrom");
        GenericValue assoc = null;
        try {
            assoc = EntityQuery.use(delegator)
                    .from("ContentAssoc")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assoc.setNonPKFields((Map<String, Object>) context);
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        Map<String, Object> content = new HashMap<>();
        content.put("lastModifiedByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        content.put("lastModifiedDate", nowTimestamp);
        try {
            delegator.store(assoc);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Remove a Content Assoc Record
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentAssoc");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue assoc = null;
        try {
            assoc = EntityQuery.use(delegator)
                    .from("ContentAssoc")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(assoc);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Set The Content Status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setContentStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue statusChange = null;
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
        result.put("oldStatusId", ((Map<String, Object>) content).get("statusId"));
        if (!java.util.Objects.equals(((Map<String, Object>) content).get("statusId"), context.get("statusId"))) {
            try {
                statusChange = EntityQuery.use(delegator)
                        .from("StatusValidChange")
                        .where(UtilMisc.toMap("statusId", ((Map<String, Object>) content).get("statusId"), "statusIdTo", context.get("statusId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(statusChange)) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentCannotChangeStatus", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                Debug.logError("Cannot change from " + ((Map<String, Object>) content).get("statusId") + " to " + context.get("statusId"), MODULE);
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            } else {
                content.put("statusId", context.get("statusId"));
                try {
                    delegator.store(content);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * copy a content, electronic text and assocs and set status in progress
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String copyContentAndElectronicTextandAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> getEt = null;
        GenericValue content = null;
        Map<String, Object> dataResource = null;
        Map<String, Object> assocS = null;
        Map<String, Object> assocTos = null;
        Map<String, Object> getC = new HashMap<>();
        // set-service-fields from "parameters" to "getC" for service "getContent"
        getC.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getContent", getC);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            content = (GenericValue) serviceResult.get("view");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        content = GenericValue.create((GenericValue) content);
        if (UtilValidate.isNotEmpty(((Map<String, Object>) content).get("dataResourceId"))) {
            // set-service-fields from "content" to "getEt" for service "getElectronicText"
            getEt.putAll(UtilMisc.toMap(content));
            Map<String, Object> et = new HashMap<>();
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getElectronicText", getEt);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                et.put("textData", serviceResult.get("textData"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling getElectronicText: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            dataResource.put("dataResourceTypeId", "ELECTRONIC_TEXT");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createDataResource", dataResource);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                et.put("dataResourceId", serviceResult.get("dataResourceId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createElectronicText", et);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createElectronicText: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            content.put("dataResourceId", ((Map<String, Object>) et).get("dataResourceId"));
        }
        content.remove("contentId");
        content.remove("statusId");
        Map<String, Object> createContent = new HashMap<>();
        // set-service-fields from "content" to "createContent" for service "createContent"
        createContent.putAll(UtilMisc.toMap(content));
        Object newContentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContent", createContent);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            newContentId = serviceResult.get("contentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> assocs = null;
        try {
            assocs = EntityQuery.use(delegator)
                    .from("ContentAssoc")
                    .where(UtilMisc.toMap("contentId", context.get("contentId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (assocs != null) {
            for (GenericValue assoc : assocs) {
                assoc.put("contentId", newContentId);
                // set-service-fields from "assoc" to "assocS" for service "createContentAssoc"
                assocS.putAll(UtilMisc.toMap(assoc));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createContentAssoc", assocS);
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
        List<GenericValue> assocsTo = null;
        try {
            assocsTo = EntityQuery.use(delegator)
                    .from("ContentAssoc")
                    .where(UtilMisc.toMap("contentIdTo", context.get("contentId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (assocsTo != null) {
            for (GenericValue assocTo : assocsTo) {
                assocTo.put("contentIdTo", newContentId);
                // set-service-fields from "assocTo" to "assocTos" for service "createContentAssoc"
                assocTos.putAll(UtilMisc.toMap(assocTo));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createContentAssoc", assocTos);
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
        result.put("contentId", newContentId);

        return "success";
    }


    /**
     * Associate Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String assocContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newContentAssoc = null;
        Debug.logInfo("assocContent, parameters:" + context, MODULE);
        Debug.logInfo("assocContent, context:" + context, MODULE);
        Object permissionStatus = null;
        Object rolesOut = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("checkAssocPermission", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            permissionStatus = serviceResult.get("permissionStatus");
            rolesOut = serviceResult.get("rolesOut");
        } catch (Exception e) {
            Debug.logError(e, "Error calling checkAssocPermission: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("permissionStatus:" + permissionStatus, MODULE);
        Map<String, Object> pk = new HashMap<>();
        pk.put("contentId", context.get("contentIdTo"));
        context.put("currentContent", context.get("currentContent"));
        pk.put("contentId", context.get("contentIdFrom"));
        context.put("userLogin", context.get("userLogin"));
        GenericValue currentContent = null;
        try {
            currentContent = EntityQuery.use(delegator)
                    .from("Content")
                    .where(pk)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key Content: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue fromContent = null;
        try {
            fromContent = EntityQuery.use(delegator)
                    .from("Content")
                    .where(pk)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key Content: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("granted".equals(permissionStatus)) {
            newContentAssoc = delegator.makeValue("ContentAssoc");
            newContentAssoc.put("contentIdTo", context.get("contentIdTo"));
            newContentAssoc.put("contentId", context.get("contentIdFrom"));
            newContentAssoc.put("contentAssocTypeId", context.get("contentAssocTypeId"));
            newContentAssoc.put("createdByUserLogin", ((Map<String, Object>) context.get("newUserLogin")).get("userLoginId"));
            newContentAssoc.put("lastModifiedByUserLogin", ((Map<String, Object>) context.get("newUserLogin")).get("userLoginId"));
            Timestamp newContentAssoc_createdDate = new Timestamp(System.currentTimeMillis());
            Timestamp newContentAssoc_lastModifiedDate = new Timestamp(System.currentTimeMillis());
            if (UtilValidate.isEmpty(context.get("fromDate"))) {
                Timestamp parameters_fromDate = new Timestamp(System.currentTimeMillis());
            }
            newContentAssoc.put("fromDate", context.get("fromDate"));
            if (UtilValidate.isNotEmpty(context.get("thruDate"))) {
                newContentAssoc.put("thruDate", context.get("thruDate"));
            }
            try {
                delegator.create(newContentAssoc);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Create Content Meta Data
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentMetaData(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("ContentMetaData");
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
     * Update Content Meta Data
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentMetaData(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentMetaData");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentMetaData")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentMetaData: " + e.getMessage(), MODULE);
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
     * Remove Content Meta Data
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentMetaData(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentMetaData");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentMetaData")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentMetaData: " + e.getMessage(), MODULE);
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
     * Create Content Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue partyRole = null;
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            Timestamp parameters_fromDate = new Timestamp(System.currentTimeMillis());
        }
        GenericValue partyRolePK = delegator.makeValue("PartyRole");
        partyRolePK.setPKFields((Map<String, Object>) context);
        try {
            partyRole = EntityQuery.use(delegator)
                    .from("PartyRole")
                    .where(partyRolePK)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key PartyRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(partyRole)) {
            // TODO: Convert <check-permission> element
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            // TODO: Convert <check-permission> element
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            partyRole = delegator.makeValue("PartyRole");
            try {
                delegator.create(partyRole);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("ContentRole");
        newEntity.setPKFields((Map<String, Object>) context);
        GenericValue contentRole = null;
        try {
            contentRole = EntityQuery.use(delegator)
                    .from("ContentRole")
                    .where(newEntity)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(contentRole)) {
            newEntity.setNonPKFields((Map<String, Object>) context);
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Update Content Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentRole");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentRole")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentRole: " + e.getMessage(), MODULE);
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
     * Update Content Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deactivateAllContentRoles(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue role = null;
        GenericValue lookupKeyValue = delegator.makeValue("ContentRole");
        lookupKeyValue.put("contentId", context.get("contentId"));
        lookupKeyValue.put("partyId", context.get("partyId"));
        lookupKeyValue.put("roleTypeId", context.get("roleTypeId"));
        // TODO: Convert <find-by-and> element
        if (context.get("roleList") != null) {
            for (Object contentRoleMap : (List<Object>) context.get("roleList")) {
                role = delegator.makeValue("ContentRole");
                Timestamp role_thruDate = new Timestamp(System.currentTimeMillis());
                try {
                    delegator.store(role);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Remove Content Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentRole");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentRole")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentRole: " + e.getMessage(), MODULE);
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
     * Create Content Type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("ContentType");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contentTypeId", ((Map<String, Object>) newEntity).get("contentTypeId"));

        return "success";
    }


    /**
     * Update Content Type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentType");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentType")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentType: " + e.getMessage(), MODULE);
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
     * Remove Content Type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentType");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentType")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentType: " + e.getMessage(), MODULE);
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
     * Create Content TypeAttr
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentTypeAttr(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("ContentTypeAttr");
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
     * Remove Content TypeAttr
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentTypeAttr(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentTypeAttr");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentTypeAttr")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentTypeAttr: " + e.getMessage(), MODULE);
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
     * Create Content AssocType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentAssocType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("ContentAssocType");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contentAssocTypeId", ((Map<String, Object>) newEntity).get("contentAssocTypeId"));

        return "success";
    }


    /**
     * Update Content AssocType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentAssocType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentAssocType");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentAssocType")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentAssocType: " + e.getMessage(), MODULE);
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
     * Remove Content AssocType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentAssocType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentAssocType");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentAssocType")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentAssocType: " + e.getMessage(), MODULE);
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
     * Create Content PurposeType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentPurposeType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("ContentPurposeType");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("contentPurposeTypeId"))) {
            delegator.setNextSubSeqId(newEntity, "contentPurposeTypeId", 5, 1);
            Object contentPurposeTypeId = newEntity.get("contentPurposeTypeId");
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contentPurposeTypeId", ((Map<String, Object>) newEntity).get("contentPurposeTypeId"));

        return "success";
    }


    /**
     * Update Content PurposeType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentPurposeType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentPurposeType");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentPurposeType")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentPurposeType: " + e.getMessage(), MODULE);
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
     * Remove Content PurposeType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentPurposeType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentPurposeType");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentPurposeType")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentPurposeType: " + e.getMessage(), MODULE);
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
     * Create Content AssocPredicate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentAssocPredicate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("ContentAssocPredicate");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contentAssocPredicateId", ((Map<String, Object>) newEntity).get("contentAssocPredicateId"));

        return "success";
    }


    /**
     * Update Content AssocPredicate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentAssocPredicate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentAssocPredicate");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentAssocPredicate")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentAssocPredicate: " + e.getMessage(), MODULE);
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
     * Remove Content AssocPredicate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentAssocPredicate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentAssocPredicate");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentAssocPredicate")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentAssocPredicate: " + e.getMessage(), MODULE);
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
     * Create Content PurposeOperation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentPurposeOperation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("ContentPurposeOperation");
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
     * Update Content PurposeOperation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentPurposeOperation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentPurposeOperation");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentPurposeOperation")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentPurposeOperation: " + e.getMessage(), MODULE);
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
     * Remove Content PurposeOperation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentPurposeOperation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentPurposeOperation");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentPurposeOperation")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentPurposeOperation: " + e.getMessage(), MODULE);
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
     * Create Content Purpose
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentPurpose(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("ContentPurpose");
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
     * Update Content Purpose
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentPurpose(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentPurpose");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentPurpose")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentPurpose: " + e.getMessage(), MODULE);
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
     * Remove Content Purpose
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentPurpose(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentPurpose");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentPurpose")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentPurpose: " + e.getMessage(), MODULE);
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
     * Updates the purpose making sure there is only one
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSingleContentPurpose(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> toRemove = new HashMap<>();
        toRemove.put("contentId", context.get("contentId"));
        // TODO: Convert <remove-by-and> element
        String result = createContentPurpose(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Create Content Operation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentOperation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("ContentOperation");
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
     * Update Content Operation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentOperation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentOperation");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentOperation")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentOperation: " + e.getMessage(), MODULE);
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
     * Remove Content Operation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentOperation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentOperation");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentOperation")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentOperation: " + e.getMessage(), MODULE);
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
     * Create Content Attribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("ContentAttribute");
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
     * Update Content Attribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentAttribute");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentAttribute")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentAttribute: " + e.getMessage(), MODULE);
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
     * Remove Content Attribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupKeyValue = delegator.makeValue("ContentAttribute");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentAttribute")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentAttribute: " + e.getMessage(), MODULE);
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
     * Creates Text and Optionally Uploaded (sub) Content records
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createTextAndUploadedContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> uploadContext = null;
        Map<String, Object> textContext = new HashMap<>();
        // set-service-fields from "parameters" to "textContext" for service "createTextContent"
        textContext.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createTextContent", textContext);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("parentContentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createTextContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("context: " + context, MODULE);
        if (UtilValidate.isNotEmpty(context.get("uploadedFile"))) {
            Debug.logInfo("Uploaded file found; processing sub-content", MODULE);
            // set-service-fields from "parameters" to "uploadContext" for service "createContentFromUploadedFile"
            uploadContext.putAll(UtilMisc.toMap(context));
            uploadContext.put("ownerContentId", context.get("parentContentId"));
            uploadContext.put("contentIdFrom", context.get("parentContentId"));
            uploadContext.put("contentAssocTypeId", "SUB_CONTENT");
            uploadContext.put("contentPurposeTypeId", "SECTION");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createContentFromUploadedFile", uploadContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createContentFromUploadedFile: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("contentId", context.get("parentContentId"));

        return "success";
    }


    /**
     * Find associated content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String findAssocContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> queryMap = null;
        List<GenericValue> validContent = null;
        queryMap.put("contentId", context.get("contentId"));
        Object mapKeys = context.get("mapKeys");
        ((List<Object>) mapKeys).add(context.get("mapKey"));
        if (mapKeys != null) {
            for (Object mapKey : (List<Object>) mapKeys) {
                queryMap.put("mapKey", mapKey);
                // TODO: Convert <find-by-and> element
                validContent = EntityUtil.filterByDate((List<GenericValue>) context.get("resultMap"));
                if (validContent != null) {
                    for (GenericValue contentAssoc : validContent) {
                        ((List<Object>) result).add(contentAssoc);
                    }
                }
            }
        }
        if (UtilValidate.isEmpty(context.get("mapKey"))) {
            result.put("contentAssocs", result);
        } else {
            result.put("contentAssoc", result);
        }

        return "success";
    }


    /**
     * Create Email as Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createEmailContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createSubjectContent = new HashMap<>();
        // set-service-fields from "parameters" to "createSubjectContent" for service "createContent"
        createSubjectContent.putAll(UtilMisc.toMap(context));
        Map<String, Object> createSubjectEtext = new HashMap<>();
        createSubjectEtext.put("textData", context.get("subject"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createElectronicText", createSubjectEtext);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            createSubjectContent.put("dataResourceId", serviceResult.get("dataResourceId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createElectronicText: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createBodyAssoc = new HashMap<>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContent", createSubjectContent);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            createBodyAssoc.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createPlainBodyEtext = new HashMap<>();
        createPlainBodyEtext.put("textData", context.get("plainBody"));
        Map<String, Object> createPlainBodyContent = new HashMap<>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createElectronicText", createPlainBodyEtext);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            createPlainBodyContent.put("dataResourceId", serviceResult.get("dataResourceId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createElectronicText: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContent", createPlainBodyContent);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            createBodyAssoc.put("contentIdTo", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        createBodyAssoc.put("contentAssocTypeId", "TREE_CHILD");
        createBodyAssoc.put("mapKey", "plainBody");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContentAssoc", createBodyAssoc);
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
        Map<String, Object> createHtmlBodyEtext = new HashMap<>();
        createHtmlBodyEtext.put("textData", context.get("htmlBody"));
        Map<String, Object> createHtmlBodyContent = new HashMap<>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createElectronicText", createHtmlBodyEtext);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            createHtmlBodyContent.put("dataResourceId", serviceResult.get("dataResourceId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createElectronicText: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContent", createHtmlBodyContent);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            createBodyAssoc.put("contentIdTo", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        createBodyAssoc.put("mapKey", "htmlBody");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContentAssoc", createBodyAssoc);
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
        result.put("contentId", ((Map<String, Object>) createBodyAssoc).get("contentId"));

        return "success";
    }


    /**
     * Update Email Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateEmailContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> updateSubjectEt = null;
        Map<String, Object> updatePlainBodyEt = null;
        Map<String, Object> updateHtmlBodyEt = null;
        if (UtilValidate.isNotEmpty(context.get("subjectDataResourceId"))) {
            updateSubjectEt.put("dataResourceId", context.get("subjectDataResourceId"));
            updateSubjectEt.put("textData", context.get("subject"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateElectronicText", updateSubjectEt);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateElectronicText: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(context.get("plainBodyDataResourceId"))) {
            updatePlainBodyEt.put("dataResourceId", context.get("plainBodyDataResourceId"));
            updatePlainBodyEt.put("textData", context.get("plainBody"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateElectronicText", updatePlainBodyEt);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateElectronicText: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(context.get("htmlBodyDataResourceId"))) {
            updateHtmlBodyEt.put("dataResourceId", context.get("htmlBodyDataResourceId"));
            updateHtmlBodyEt.put("textData", context.get("htmlBody"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateElectronicText", updateHtmlBodyEt);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateElectronicText: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create Download as Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createDownloadContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createDownloadContent = new HashMap<>();
        // set-service-fields from "parameters" to "createDownloadContent" for service "createContent"
        createDownloadContent.putAll(UtilMisc.toMap(context));
        Map<String, Object> createDownload = new HashMap<>();
        createDownload.put("dataResourceContent", context.get("file"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createOtherDataResource", createDownload);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            createDownloadContent.put("dataResourceId", serviceResult.get("dataResourceId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createOtherDataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContent", createDownloadContent);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update Download Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateDownloadContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> updateFile = null;
        if (UtilValidate.isNotEmpty(context.get("fileDataResourceId"))) {
            updateFile.put("dataResourceId", context.get("fileDataResourceId"));
            updateFile.put("dataResourceContent", context.get("file"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateOtherDataResource", updateFile);
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
        }

        return "success";
    }


    /**
     * Create Simple Text Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSimpleTextContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createSimpleTextContent = null;
        Map<String, Object> createSimpleTextDataResource = null;
        // set-service-fields from "parameters" to "createSimpleTextContent" for service "createContent"
        createSimpleTextContent.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isEmpty(((Map<String, Object>) createSimpleTextContent).get("contentTypeId"))) {
            createSimpleTextContent.put("contentTypeId", "DOCUMENT");
        }
        Map<String, Object> createSimpleText = new HashMap<>();
        createSimpleText.put("textData", null);
        // set-service-fields from "parameters" to "createSimpleTextDataResource" for service "createDataResource"
        createSimpleTextDataResource.putAll(UtilMisc.toMap(context));
        createSimpleTextDataResource.put("dataResourceTypeId", "ELECTRONIC_TEXT");
        if (UtilValidate.isEmpty(((Map<String, Object>) createSimpleTextDataResource).get("dataTemplateTypeId"))) {
            createSimpleTextDataResource.put("dataTemplateTypeId", "FTL");
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createDataResource", createSimpleTextDataResource);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            createSimpleText.put("dataResourceId", serviceResult.get("dataResourceId"));
            createSimpleTextContent.put("dataResourceId", serviceResult.get("dataResourceId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createDataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createElectronicText", createSimpleText);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createElectronicText: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContent", createSimpleTextContent);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update Simple Text Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSimpleTextContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> updateSimpleText = null;
        if (UtilValidate.isNotEmpty(context.get("textDataResourceId"))) {
            updateSimpleText.put("dataResourceId", context.get("textDataResourceId"));
            updateSimpleText.put("textData", context.get("text"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateElectronicText", updateSimpleText);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateElectronicText: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create TOPIC type Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createTopic(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue content = delegator.makeValue("Content");
        content.put("contentId", context.get("newTopicId"));
        content.put("contentName", context.get("newTopicId"));
        content.put("description", context.get("newTopicDescription"));
        content.put("contentTypeId", "TOPIC");
        Timestamp content_lastModifiedDate = new Timestamp(System.currentTimeMillis());
        Timestamp content_createdDate = new Timestamp(System.currentTimeMillis());
        content.put("lastModifiedByUserLogin", ((Map<String, Object>) context.get("userLogin")).get("userLoginId"));
        content.put("createdByUserLogin", ((Map<String, Object>) context.get("userLogin")).get("userLoginId"));
        try {
            delegator.create(content);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create Content from DataResource Object
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentFromDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> createContentMap = null;
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
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        // set-service-fields from "parameters" to "createContentMap" for service "createContent"
        createContentMap.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isEmpty(((Map<String, Object>) createContentMap).get("contentName"))) {
            createContentMap.put("contentName", ((Map<String, Object>) dataResource).get("dataResourceName"));
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) createContentMap).get("contentTypeId"))) {
            createContentMap.put("contentTypeId", "DOCUMENT");
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) createContentMap).get("statusId"))) {
            createContentMap.put("statusId", "CTNT_INITIAL_DRAFT");
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) createContentMap).get("mimeTypeId"))) {
            createContentMap.put("mimeTypeId", ((Map<String, Object>) dataResource).get("mimeTypeId"));
        }
        Object contentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContent", createContentMap);
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
        result.put("contentId", contentId);

        return "success";
    }


    /**
     * Create CommunicationEvent and Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCommContentDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> persistIn = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        // set-service-fields from "parameters" to "persistIn" for service "persistContentAndAssoc"
        persistIn.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isEmpty(((Map<String, Object>) persistIn).get("dataResourceTypeId"))) {
            Debug.logInfo("persistIn.drMimeTypeId: " + ((Map<String, Object>) persistIn).get("drMimeTypeId"), MODULE);
            // TODO: Convert <if-regexp> element
        }
        Debug.logInfo("persistIn.dataResourceTypeId: " + ((Map<String, Object>) persistIn).get("dataResourceTypeId"), MODULE);
        Map<String, Object> persistOut = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentAndAssoc", persistIn);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            persistOut = serviceResult;
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentAndAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contentId", ((Map<String, Object>) persistOut).get("contentId"));
        result.put("dataResourceId", ((Map<String, Object>) persistOut).get("dataResourceId"));
        result.put("drDataResourceId", ((Map<String, Object>) persistOut).get("drDataResourceId"));
        result.put("caContentIdTo", ((Map<String, Object>) persistOut).get("caContentIdTo"));
        result.put("caContentId", ((Map<String, Object>) persistOut).get("caContentId"));
        result.put("caContentAssocTypeId", ((Map<String, Object>) persistOut).get("caContentAssocTypeId"));
        result.put("caFromDate", ((Map<String, Object>) persistOut).get("caFromDate"));
        result.put("caSequenceNum", ((Map<String, Object>) persistOut).get("caSequenceNum"));
        result.put("roleTypeList", ((Map<String, Object>) persistOut).get("roleTypeList"));
        result.put("fromDate", ((Map<String, Object>) persistOut).get("fromDate"));
        Map<String, Object> mapIn = new HashMap<>();
        mapIn.put("contentId", ((Map<String, Object>) persistOut).get("contentId"));
        mapIn.put("communicationEventId", context.get("communicationEventId"));
        mapIn.put("sequenceNum", context.get("sequenceNum"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCommEventContentAssoc", mapIn);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCommEventContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update CommunicationEvent and Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCommContentDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> persistIn = new HashMap<>();
        // set-service-fields from "parameters" to "persistIn" for service "persistContentAndAssoc"
        persistIn.putAll(UtilMisc.toMap(context));
        Map<String, Object> persistOut = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentAndAssoc", persistIn);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            persistOut = serviceResult;
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentAndAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> mapIn = new HashMap<>();
        mapIn.put("contentId", ((Map<String, Object>) persistOut).get("contentId"));
        mapIn.put("fromDate", context.get("fromDate"));
        mapIn.put("communicationEventId", context.get("communicationEventId"));
        mapIn.put("sequenceNum", context.get("sequenceNum"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateCommEventContentAssoc", mapIn);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateCommEventContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contentId", ((Map<String, Object>) persistOut).get("contentId"));
        result.put("dataResourceId", ((Map<String, Object>) persistOut).get("dataResourceId"));
        result.put("drDataResourceId", ((Map<String, Object>) persistOut).get("drDataResourceId"));
        result.put("caContentIdTo", ((Map<String, Object>) persistOut).get("caContentIdTo"));
        result.put("caContentId", ((Map<String, Object>) persistOut).get("caContentId"));
        result.put("caContentAssocTypeId", ((Map<String, Object>) persistOut).get("caContentAssocTypeId"));
        result.put("caFromDate", ((Map<String, Object>) persistOut).get("caFromDate"));
        result.put("caSequenceNum", ((Map<String, Object>) persistOut).get("caSequenceNum"));
        result.put("roleTypeList", ((Map<String, Object>) persistOut).get("roleTypeList"));

        return "success";
    }


    /**
     * Create CommEventContentAssoc
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCommEventContentAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue commEventContentAssoc = delegator.makeValue("CommEventContentAssoc");
        commEventContentAssoc.setPKFields((Map<String, Object>) context);
        commEventContentAssoc.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) commEventContentAssoc).get("fromDate"))) {
            Timestamp commEventContentAssoc_fromDate = new Timestamp(System.currentTimeMillis());
        }
        try {
            delegator.create(commEventContentAssoc);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("fromDate", ((Map<String, Object>) commEventContentAssoc).get("fromDate"));

        return "success";
    }


    /**
     * Update CommEventContentAssoc
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCommEventContentAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue pkCommEventContentAssoc = delegator.makeValue("CommEventContentAssoc");
        pkCommEventContentAssoc.setPKFields((Map<String, Object>) context);
        GenericValue commEventContentAssoc = null;
        try {
            commEventContentAssoc = EntityQuery.use(delegator)
                    .from("CommEventContentAssoc")
                    .where(pkCommEventContentAssoc)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CommEventContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(commEventContentAssoc)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContenCommEventContentAssocNotFoundForUpdate", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        commEventContentAssoc.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(commEventContentAssoc);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete CommEventContentAssoc
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeCommEventContentAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue pkCommEventContentAssoc = delegator.makeValue("CommEventContentAssoc");
        pkCommEventContentAssoc.setPKFields((Map<String, Object>) context);
        GenericValue commEventContentAssoc = null;
        try {
            commEventContentAssoc = EntityQuery.use(delegator)
                    .from("CommEventContentAssoc")
                    .where(pkCommEventContentAssoc)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key CommEventContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(commEventContentAssoc)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContenCommEventContentAssocNotFoundForDelete", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        try {
            delegator.removeValue(commEventContentAssoc);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * get the content and relasted resource information
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue resultData_dataResource = null;
        try {
            resultData_dataResource = EntityQuery.use(delegator)
                    .from("DataResource")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> resultData = new HashMap<>();
        if (UtilValidate.isNotEmpty(((Map<String, Object>) resultData).get("dataResource"))) {
            if ("ELECTRONIC_TEXT".equals(((Map<String, Object>) ((Map<String, Object>) resultData).get("dataResource")).get("dataResourceTypeId"))) {
                GenericValue resultData_electronicText = null;
                try {
                    resultData_electronicText = ((GenericValue) ((Map<String, Object>) resultData).get("dataResource")).getRelatedOne("ElectronicText", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one ElectronicText: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            if ("IMAGE_OBJECT".equals(((Map<String, Object>) ((Map<String, Object>) resultData).get("dataResource")).get("dataResourceTypeId"))) {
                GenericValue resultData_imageDataResource = null;
                try {
                    resultData_imageDataResource = ((GenericValue) ((Map<String, Object>) resultData).get("dataResource")).getRelatedOne("ImageDataResource", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one ImageDataResource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        result.put("resultData", resultData);

        return "success";
    }


    /**
     * get the content and related resource information
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getContentAndDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> resultDataContent = null;
        GenericValue resultDataContent_content = null;
        try {
            resultDataContent_content = EntityQuery.use(delegator)
                    .from("Content")
                    .where(UtilMisc.toMap("contentId", context.get("contentId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) resultDataContent).get("content"))) {
            if (UtilValidate.isNotEmpty(((Map<String, Object>) resultDataContent.get("content")).get("dataResourceId"))) {
                context.put("dataResourceId", ((Map<String, Object>) resultDataContent.get("content")).get("dataResourceId"));
                String inlineResult = getDataResource(request, response);
                if (!"success".equals(inlineResult)) {
                    return inlineResult;
                }
                resultDataContent.put("dataResource", ((Map<String, Object>) context.get("resultData")).get("dataResource"));
                resultDataContent.put("electronicText", ((Map<String, Object>) context.get("resultData")).get("electronicText"));
                resultDataContent.put("imageDataResource", ((Map<String, Object>) context.get("resultData")).get("imageDataResource"));
            }
            result.put("resultData", resultDataContent);
        }

        return "success";
    }


    /**
     * get the content and related resource information without security
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPublicForumMessage(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object publicForumMessage = "true";
        String result = getContentAndDataResource(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Checks and prepares contentIdTo and contentId for ContentAssoc service
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkContentAssocIds(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        if ((!(UtilValidate.isEmpty(context.get("contentIdFrom"))) && !(UtilValidate.isEmpty(context.get("contentId"))) && UtilValidate.isEmpty(context.get("contentIdTo")))) {
            result.put("contentId", context.get("contentIdFrom"));
            result.put("contentIdTo", context.get("contentId"));
            Debug.logInfo("Converted 'contentId' to 'contentIdTo' and 'contentIdFrom' to 'contentId'", MODULE);
        } else {
            Debug.logWarning("Illegal values passed; should be either contentIdTo/contentId or contentIdFrom/contentId :: " + context, MODULE);
        }

        return "success";
    }


    /**
     * Post a new Content article Entry
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createArticleContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object ownerContentId = null;
        Object contentAssocTypeId = null;
        Object contentId = null;
        Object contentIdFrom = null;
        Map<String, Object> createMain = null;
        Map<String, Object> createImage = null;
        Object imageContentId = null;
        Map<String, Object> createText = null;
        Object textContentId = null;
        Map<String, Object> createSummary = null;
        Map<String, Object> contentAssocMap = null;
        contentAssocTypeId = context.get("contentAssocTypeId");
        Object origContentAssocTypeId = context.get("contentAssocTypeId");
        ownerContentId = context.get("threadContentId");
        if ("PUBLISH_LINK".equals(origContentAssocTypeId)) {
            ownerContentId = context.get("pubPtContentId");
        }
        contentIdFrom = context.get("contentIdFrom");
        Object pubPtContentId = context.get("pubPtContentId");
        int textDataLen = ((String) context.get("textData")).length();
        Debug.logInfo("textDataLen:" + textDataLen, MODULE);
        int descriptLen = Integer.parseInt(UtilProperties.getMessage("forum", "descriptLen", locale));
        Debug.logInfo("descriptLen:" + descriptLen, MODULE);
        int subStringLen = 0;
        try {
            subStringLen = Math.min(textDataLen, descriptLen);
        } catch (Exception e) {
            Debug.logError(e, "Error calling Math.min: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("subStringLen:" + subStringLen, MODULE);
        int zeroValue = 0;
        String subDescript = ((String) context.get("textData")).substring(zeroValue, subStringLen);
        Debug.logInfo("subDescript:" + subDescript, MODULE);
        if ("PUBLISH_LINK".equals(contentAssocTypeId)) {
            ownerContentId = pubPtContentId;
        }
        Object createMain_dataResourceId = null;
        Object createMain_contentAssocTypeId = null;
        Object createMain_contentName = null;
        Object createMain_description = null;
        Object createMain_statusId = null;
        Object createMain_contentIdFrom = null;
        Object createMain_partyId = null;
        Object createMain_ownerContentId = null;
        Object createMain_dataTemplateTypeId = null;
        Object createMain_mapKey = null;
        if ((!(UtilValidate.isEmpty(context.get("uploadedFile"))) && !(UtilValidate.isEmpty(context.get("textData"))))) {
            createMain.put("dataResourceId", context.get("dataResourceId"));
            createMain.put("contentAssocTypeId", contentAssocTypeId);
            createMain.put("contentName", context.get("contentName"));
            createMain.put("description", subDescript);
            createMain.put("statusId", context.get("statusId"));
            createMain.put("contentIdFrom", contentIdFrom);
            createMain.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
            createMain.put("ownerContentId", ownerContentId);
            createMain.put("dataTemplateTypeId", "SCREEN_COMBINED");
            createMain.put("mapKey", "MAIN");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createContent", createMain);
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
            contentAssocTypeId = "SUB_CONTENT";
            contentIdFrom = contentId;
        }
        Object createImage_dataResourceTypeId = null;
        Object createImage_dataTemplateTypeId = null;
        Object createImage_mapKey = null;
        Object createImage_contentName = null;
        Object createImage_description = null;
        Object createImage_statusId = null;
        Object createImage_contentAssocTypeId = null;
        Object createImage_contentIdFrom = null;
        Object createImage_partyId = null;
        Object createImage_uploadedFile = null;
        Object createImage__uploadedFile_fileName = null;
        Object createImage__uploadedFile_contentType = null;
        if (!(UtilValidate.isEmpty(context.get("uploadedFile")))) {
            createImage.put("dataResourceTypeId", "LOCAL_FILE");
            createImage.put("dataTemplateTypeId", "NONE");
            createImage.put("mapKey", "IMAGE");
            createMain.put("ownerContentId", ownerContentId);
            createImage.put("contentName", context.get("contentName"));
            createImage.put("description", subDescript);
            createImage.put("statusId", context.get("statusId"));
            createImage.put("contentAssocTypeId", contentAssocTypeId);
            createImage.put("contentIdFrom", contentIdFrom);
            createImage.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
            createImage.put("uploadedFile", context.get("uploadedFile"));
            createImage.put("_uploadedFile_fileName", context.get("_uploadedFile_fileName"));
            createImage.put("_uploadedFile_contentType", context.get("_uploadedFile_contentType"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createContentFromUploadedFile", createImage);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                imageContentId = serviceResult.get("contentId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createContentFromUploadedFile: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(contentId)) {
                contentIdFrom = imageContentId;
                contentId = imageContentId;
                contentAssocTypeId = "SUB_CONTENT";
            }
        }
        Object createText_dataResourceTypeId = null;
        Object createText_dataTemplateTypeId = null;
        Object createText_mapKey = null;
        Object createText_ownerContentId = null;
        Object createText_contentName = null;
        Object createText_description = null;
        Object createText_statusId = null;
        Object createText_contentAssocTypeId = null;
        Object createText_textData = null;
        Object createText_contentIdFrom = null;
        Object createText_partyId = null;
        if (!(UtilValidate.isEmpty(context.get("textData")))) {
            createText.put("dataResourceTypeId", "ELECTRONIC_TEXT");
            createText.put("dataTemplateTypeId", "NONE");
            createText.put("mapKey", "MAIN");
            createText.put("ownerContentId", ownerContentId);
            createText.put("contentName", context.get("contentName"));
            createText.put("description", subDescript);
            createText.put("statusId", context.get("statusId"));
            createText.put("contentAssocTypeId", contentAssocTypeId);
            createText.put("textData", context.get("textData"));
            createText.put("contentIdFrom", contentIdFrom);
            createText.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
            Debug.logInfo("calling createTextContent with map: " + createText, MODULE);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createTextContent", createText);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                textContentId = serviceResult.get("contentId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createTextContent: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(contentId)) {
                contentIdFrom = textContentId;
                contentId = textContentId;
                contentAssocTypeId = "SUB_CONTENT";
            }
        }
        Object createSummary_dataResourceTypeId = null;
        Object createSummary_dataTemplateTypeId = null;
        Object createSummary_mapKey = null;
        Object createSummary_ownerContentId = null;
        Object createSummary_contentName = null;
        Object createSummary_description = null;
        Object createSummary_statusId = null;
        Object createSummary_contentAssocTypeId = null;
        Object createSummary_textData = null;
        Object createSummary_contentIdFrom = null;
        Object createSummary_partyId = null;
        if ((!(UtilValidate.isEmpty(contentId)) && !(UtilValidate.isEmpty(context.get("summaryData"))))) {
            createSummary.put("dataResourceTypeId", "ELECTRONIC_TEXT");
            createSummary.put("dataTemplateTypeId", "NONE");
            createSummary.put("mapKey", "SUMMARY");
            createSummary.put("ownerContentId", ownerContentId);
            createSummary.put("contentName", context.get("contentName"));
            createSummary.put("description", context.get("description"));
            createSummary.put("statusId", context.get("statusId"));
            createSummary.put("contentAssocTypeId", contentAssocTypeId);
            createSummary.put("textData", context.get("summaryData"));
            createSummary.put("contentIdFrom", contentIdFrom);
            createSummary.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createTextContent", createSummary);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createTextContent: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if ("RESPONSE".equals(origContentAssocTypeId)) {
            contentAssocMap.put("contentId", pubPtContentId);
            contentAssocMap.put("contentIdTo", contentId);
            contentAssocMap.put("contentAssocTypeId", "RESPONSE");
            Debug.logInfo("contentAssocMap:" + ((Map<String, Object>) contentAssocMap).get("contentId"), MODULE);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createContentAssoc", contentAssocMap);
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
        result.put("contentId", contentId);

        return "success";
    }


    /**
     * Get sub content and perform permission check on each record
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getSubContentWithPermCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<Object> contentViewList = null;
        Boolean hasPermission = null;
        Map<String, Object> inMap = null;
        Boolean filterByDate = (Boolean) context.get("filterByDate");
        Boolean useCache = (Boolean) context.get("useCache");
        List<GenericValue> viewList = null;
        try {
            viewList = EntityQuery.use(delegator)
                    .from("ContentAssocViewTo")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentAssocViewTo: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (viewList != null) {
            for (GenericValue view : viewList) {
                hasPermission = Boolean.TRUE;
                Object inMap_contentId = null;
                Object inMap_mainAction = null;
                Object inMap_userLogin = null;
                Object inMap_contentOperationId = null;
                if ((!(UtilValidate.isEmpty(context.get("mainAction"))) && !(UtilValidate.isEmpty(context.get("userLogin"))))) {
                    inMap.put("contentId", context.get("contentId"));
                    inMap.put("mainAction", context.get("mainAction"));
                    inMap.put("userLogin", context.get("userLogin"));
                    inMap.put("contentOperationId", context.get("contentOperationId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("genericContentPermission", inMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        hasPermission = (Boolean) serviceResult.get("hasPermission");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling genericContentPermission: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                if (Boolean.TRUE.equals(hasPermission)) {
                    contentViewList.add(view);
                }
            }
        }
        result.put("subContentList", contentViewList);

        return "success";
    }


    /**
     * Get sub content and perform permission check on each record
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getSubSubContentWithPermCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<GenericValue> viewList = null;
        List<Object> contentViewList = null;
        GenericValue view2 = null;
        Object map = null;
        GenericValue electronicText = null;
        Map<String, Object> inMap = new HashMap<>();
        // set-service-fields from "parameters" to "inMap" for service "getSubContentWithPermCheck"
        inMap.putAll(UtilMisc.toMap(context));
        Object subContentList = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getSubContentWithPermCheck", inMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            subContentList = serviceResult.get("subContentList");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getSubContentWithPermCheck: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (subContentList != null) {
            for (Object view : (List<Object>) subContentList) {
                try {
                    viewList = EntityQuery.use(delegator)
                            .from("ContentAssocViewTo")
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ContentAssocViewTo: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                view2 = null;
                view2 = EntityUtil.getFirst((List<GenericValue>) viewList);
                map = null;
                ((Map<String, Object>) map).put("contentIdFrom", ((Map<String, Object>) view).get("contentId"));
                ((Map<String, Object>) map).put("dataResourceIdFrom", ((Map<String, Object>) view).get("dataResourceId"));
                ((Map<String, Object>) map).put("contentId", ((Map<String, Object>) view2).get("contentId"));
                ((Map<String, Object>) map).put("contentName", ((Map<String, Object>) view2).get("contentName"));
                ((Map<String, Object>) map).put("description", ((Map<String, Object>) view2).get("description"));
                try {
                    electronicText = EntityQuery.use(delegator)
                            .from("ElectronicText")
                            .where(UtilMisc.toMap("dataResourceId", ((Map<String, Object>) view2).get("dataResourceId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ElectronicText: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                ((Map<String, Object>) map).put("textData", ((Map<String, Object>) electronicText).get("textData"));
                contentViewList.add(map);
            }
        }
        result.put("subContentList", subContentList);
        result.put("subSubContentList", contentViewList);

        return "success";
    }


    /**
     * create a ContentKeyword
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentKeyword(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("ContentKeyword");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
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
     * update a ContentKeyword
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentKeyword(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentKeyword")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentKeyword: " + e.getMessage(), MODULE);
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
     * delete a ContentKeyword
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteContentKeyword(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentKeyword")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentKeyword: " + e.getMessage(), MODULE);
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
     * induce all the keywords of a content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String forceIndexContentKeywords(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

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
        try {
            ContentKeywordIndex.forceIndexKeywords(content);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ContentKeywordIndex.forceIndexKeywords: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * delete all the keywords of a content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteContentKeywords(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

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
        // TODO: Convert <remove-related> element

        return "success";
    }


    /**
     * Index the Keywords for a Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String indexContentKeywords(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> findContentMap = null;
        GenericValue contentInstance = null;
        contentInstance = (GenericValue) context.get("contentInstance");
        if (UtilValidate.isEmpty(contentInstance)) {
            findContentMap.put("contentId", context.get("contentId"));
            try {
                contentInstance = EntityQuery.use(delegator)
                        .from("Content")
                        .where(findContentMap)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key Content: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        try {
            ContentKeywordIndex.indexKeywords(contentInstance);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ContentKeywordIndex.indexKeywords: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create Content Alternative URLs.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentAlternativeUrl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<GenericValue> contents = null;
        Map<String, Object> dataResourceCtx = null;
        List<GenericValue> emptyField = null;
        Map<String, Object> createContentAssocCtx = null;
        Object contentCreated = null;
        Object localeString = null;
        Object contentIdTo = null;
        Object dataResourceId = null;
        Map<String, Object> contentCtx = null;
        List<GenericValue> contentAssocDataResources = null;
        Object uri = null;
        String defaultLocaleString = (String) context.get("locale");
        contents = null;
        if ((UtilValidate.isEmpty(context.get("contentId")) || "null".equals(context.get("contentId")))) {
            try {
                contents = EntityQuery.use(delegator)
                        .from("Content")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            try {
                contents = EntityQuery.use(delegator)
                        .from("Content")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (contents != null) {
            for (GenericValue content : contents) {
                localeString = ((Map<String, Object>) content).get("localeString");
                try {
                    contentAssocDataResources = EntityQuery.use(delegator)
                            .from("ContentAssocDataResourceViewTo")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ContentAssocDataResourceViewTo: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                emptyField = EntityUtil.filterByDate((List<GenericValue>) contentAssocDataResources);
                if (UtilValidate.isEmpty(contentAssocDataResources)) {
                    if (UtilValidate.isNotEmpty(((Map<String, Object>) content).get("contentName"))) {
                        try {
                            uri = UrlServletHelper.invalidCharacter((String) ((Map<String, Object>) content).get("contentName"));
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling UrlServletHelper.invalidCharacter: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (UtilValidate.isNotEmpty(uri)) {
                            ((GenericValue) dataResourceCtx).put("dataResourceId", delegator.getNextSeqId("DataResource"));
                            dataResourceCtx.put("dataResourceTypeId", "URL_RESOURCE");
                            dataResourceCtx.put("localeString", localeString);
                            dataResourceCtx.put("objectInfo", "/" + uri + "-" + ((Map<String, Object>) content).get("contentId") + "-content");
                            dataResourceCtx.put("statusId", "CTNT_IN_PROGRESS");
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createDataResource", dataResourceCtx);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                                dataResourceId = serviceResult.get("dataResourceId");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createDataResource: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            if (UtilValidate.isNotEmpty(dataResourceId)) {
                                contentCtx.put("dataResourceId", dataResourceId);
                                contentCtx.put("statusId", "CTNT_IN_PROGRESS");
                                contentCtx.put("localeString", localeString);
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("createContent", contentCtx);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                        return "error";
                                    }
                                    contentIdTo = serviceResult.get("contentId");
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling createContent: " + e.getMessage(), MODULE);
                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                    return "error";
                                }
                                if (UtilValidate.isNotEmpty(contentIdTo)) {
                                    createContentAssocCtx.put("contentId", ((Map<String, Object>) content).get("contentId"));
                                    createContentAssocCtx.put("contentIdTo", contentIdTo);
                                    createContentAssocCtx.put("contentAssocTypeId", "ALTERNATIVE_URL");
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("createContentAssoc", createContentAssocCtx);
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
                            contentCreated = "Y";
                            result.put("contentCreated", contentCreated);
                        }
                    }
                } else {
                    if (UtilValidate.isEmpty(((GenericValue) ((List<?>) contentAssocDataResources).get(0)).get("drObjectInfo"))) {
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) content).get("contentName"))) {
                            try {
                                uri = UrlServletHelper.invalidCharacter((String) ((Map<String, Object>) content).get("contentName"));
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling UrlServletHelper.invalidCharacter: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            if (UtilValidate.isNotEmpty(uri)) {
                                dataResourceCtx.put("dataResourceId", ((GenericValue) ((List<?>) contentAssocDataResources).get(0)).get("dataResourceId"));
                                dataResourceCtx.put("objectInfo", "/" + uri + "-" + ((Map<String, Object>) content).get("contentId") + "-content");
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("updateDataResource", dataResourceCtx);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                        return "error";
                                    }
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling updateDataResource: " + e.getMessage(), MODULE);
                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                    return "error";
                                }
                                contentCreated = "Y";
                                result.put("contentCreated", contentCreated);
                            }
                        }
                    } else {
                        contentCreated = "N";
                        result.put("contentCreated", contentCreated);
                    }
                }
            }
        }

        return "success";
    }


    /**
     * create missing content alternative urls.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createMissingContentAltUrls(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> product = null;
        List<GenericValue> productCategoryRollupList = null;
        Object contentsUpdated = null;
        Object rootProductCategoryId = null;
        Map<String, Object> createMissingProductContentAltUrlsMap = null;
        Map<String, Object> createMissingCategoryContentAltUrlsMap = null;
        Object contentsNotUpdated = null;
        List<GenericValue> productCategoryMemberList = null;
        Object contentCreated = null;
        List<GenericValue> productContentAndInfoList = null;
        List<GenericValue> prodCatalogCategoryList = null;
        List<GenericValue> productCategoryContentAndInfoList = null;
        List<GenericValue> contentAssocs = null;
        List<GenericValue> subContents = null;
        Map<String, Object> createMissingContentAltUrlsMap = null;
        Timestamp now = new Timestamp(System.currentTimeMillis());
        contentsNotUpdated = 0;
        contentsUpdated = 0;
        if (UtilValidate.isNotEmpty(context.get("prodCatalogId"))) {
            try {
                prodCatalogCategoryList = EntityQuery.use(delegator)
                        .from("ProdCatalogCategory")
                        .where(UtilMisc.toMap("prodCatalogId", context.get("prodCatalogId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProdCatalogCategory: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            context.put("productCategories", GroovyUtil.eval("[];", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context)));
            if (prodCatalogCategoryList != null) {
                for (GenericValue prodCatalogCategory : prodCatalogCategoryList) {
                    rootProductCategoryId = ((Map<String, Object>) prodCatalogCategory).get("productCategoryId");
                    try {
                        productCategoryRollupList = EntityQuery.use(delegator)
                                .from("ProductCategoryRollup")
                                .where(UtilMisc.toMap("parentProductCategoryId", rootProductCategoryId))
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductCategoryRollup: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    context.put("parentProductCategoryId", rootProductCategoryId);
                    createMissingCategoryContentAltUrlInline(request, response);
                }
            }
            if (context.get("productCategories") != null) {
                for (GenericValue productCategoryList : (List<GenericValue>) context.get("productCategories")) {
                    try {
                        productCategoryContentAndInfoList = EntityQuery.use(delegator)
                                .from("ProductCategoryContentAndInfo")
                                .cache()
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductCategoryContentAndInfo: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (productCategoryContentAndInfoList != null) {
                        for (GenericValue productCategoryContentAndInfo : productCategoryContentAndInfoList) {
                            createMissingCategoryContentAltUrlsMap.put("contentId", ((Map<String, Object>) productCategoryContentAndInfo).get("contentId"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createContentAlternativeUrl", createMissingCategoryContentAltUrlsMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                                contentCreated = serviceResult.get("contentCreated");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createContentAlternativeUrl: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            if ("Y".equals(contentCreated)) {
                                contentsUpdated = new BigDecimal(contentsUpdated.toString());
                            }
                            if ("N".equals(contentCreated)) {
                                contentsNotUpdated = new BigDecimal(contentsNotUpdated.toString());
                            }
                        }
                    }
                    try {
                        productCategoryMemberList = EntityQuery.use(delegator)
                                .from("ProductCategoryMember")
                                .cache()
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ProductCategoryMember: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (productCategoryMemberList != null) {
                        for (GenericValue productCategoryMember : productCategoryMemberList) {
                            product.put("productId", ((Map<String, Object>) productCategoryMember).get("productId"));
                            try {
                                productContentAndInfoList = EntityQuery.use(delegator)
                                        .from("ProductContentAndInfo")
                                        .cache()
                                        .filterByDate()
                                        .queryList();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying ProductContentAndInfo: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            if (productContentAndInfoList != null) {
                                for (GenericValue productContentAndInfo : productContentAndInfoList) {
                                    createMissingProductContentAltUrlsMap.put("contentId", ((Map<String, Object>) productContentAndInfo).get("contentId"));
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("createContentAlternativeUrl", createMissingProductContentAltUrlsMap);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                            return "error";
                                        }
                                        contentCreated = serviceResult.get("contentCreated");
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling createContentAlternativeUrl: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                    if ("Y".equals(contentCreated)) {
                                        contentsUpdated = new BigDecimal(contentsUpdated.toString());
                                    }
                                    if ("N".equals(contentCreated)) {
                                        contentsNotUpdated = new BigDecimal(contentsNotUpdated.toString());
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        List<GenericValue> webSiteContents = null;
        try {
            webSiteContents = EntityQuery.use(delegator)
                    .from("WebSiteContent")
                    .where(UtilMisc.toMap("webSiteId", context.get("webSiteId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WebSiteContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (webSiteContents != null) {
            for (GenericValue webSiteContent : webSiteContents) {
                try {
                    subContents = EntityQuery.use(delegator)
                            .from("ContentAssoc")
                            .where(UtilMisc.toMap("contentId", ((Map<String, Object>) webSiteContent).get("contentId")))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (subContents != null) {
                    for (GenericValue subContent : subContents) {
                        createMissingContentAltUrlsMap.put("contentId", ((Map<String, Object>) subContent).get("contentIdTo"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createContentAlternativeUrl", createMissingContentAltUrlsMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                            contentCreated = serviceResult.get("contentCreated");
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createContentAlternativeUrl: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if ("Y".equals(contentCreated)) {
                            contentsUpdated = new BigDecimal(contentsUpdated.toString());
                        }
                        if ("N".equals(contentCreated)) {
                            contentsNotUpdated = new BigDecimal(contentsNotUpdated.toString());
                        }
                        try {
                            contentAssocs = EntityQuery.use(delegator)
                                    .from("ContentAssoc")
                                    .where(UtilMisc.toMap("contentId", ((Map<String, Object>) subContent).get("contentIdTo")))
                                    .filterByDate()
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (contentAssocs != null) {
                            for (GenericValue contentAssoc : contentAssocs) {
                                createMissingContentAltUrlsMap.put("contentId", ((Map<String, Object>) contentAssoc).get("contentIdTo"));
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("createContentAlternativeUrl", createMissingContentAltUrlsMap);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                        return "error";
                                    }
                                    contentCreated = serviceResult.get("contentCreated");
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling createContentAlternativeUrl: " + e.getMessage(), MODULE);
                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                    return "error";
                                }
                                if ("Y".equals(contentCreated)) {
                                    contentsUpdated = new BigDecimal(contentsUpdated.toString());
                                }
                                if ("N".equals(contentCreated)) {
                                    contentsNotUpdated = new BigDecimal(contentsNotUpdated.toString());
                                }
                            }
                        }
                    }
                }
            }
        }
        result.put("contentsNotUpdated", contentsNotUpdated);
        result.put("contentsUpdated", contentsUpdated);

        return "success";
    }


    /**
     * create missing category alternative inline
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createMissingCategoryContentAltUrlInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<Object> parameters_productCategories = null;
        GenericValue productCategory = null;
        List<GenericValue> productCategoryRollups = null;
        try {
            productCategoryRollups = EntityQuery.use(delegator)
                    .from("ProductCategoryRollup")
                    .where(UtilMisc.toMap("parentProductCategoryId", context.get("parentProductCategoryId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductCategoryRollup: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (productCategoryRollups != null) {
            for (GenericValue productCategoryRollup : productCategoryRollups) {
                try {
                    productCategory = EntityQuery.use(delegator)
                            .from("ProductCategory")
                            .where(UtilMisc.toMap("productCategoryId", ((Map<String, Object>) productCategoryRollup).get("productCategoryId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProductCategory: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                parameters_productCategories.add(productCategory);
                context.put("parentProductCategoryId", ((Map<String, Object>) productCategoryRollup).get("productCategoryId"));
                createMissingCategoryContentAltUrlInline(request, response);
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }

        return "success";
    }

}

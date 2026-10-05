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
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://content/script/org/ofbiz/content/blog/BlogServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class BlogServices {

    private static final String MODULE = BlogServices.class.getName();


    /**
     * Create a new Blog Entry
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createBlogEntry(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> createImage = null;
        Object imageContentId = null;
        Map<String, Object> createText = null;
        Object textContentId = null;
        Map<String, Object> createSummary = null;
        Object contentAssocTypeId = "PUBLISH_LINK";
        Object ownerContentId = context.get("blogContentId");
        Object contentIdFrom = context.get("blogContentId");
        if (UtilValidate.isEmpty(context.get("statusId"))) {
            context.put("statusId", "CTNT_INITIAL_DRAFT");
        }
        if (UtilValidate.isEmpty(context.get("templateDataResourceId"))) {
            context.put("templateDataResourceId", "BLOG_TPL_TOPLEFT");
        }
        if (UtilValidate.isEmpty(context.get("contentName"))) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentArticleNameIsMissing", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> createMain = new HashMap<>();
        createMain.put("dataResourceId", context.get("templateDataResourceId"));
        createMain.put("contentAssocTypeId", contentAssocTypeId);
        createMain.put("contentName", context.get("contentName"));
        createMain.put("description", context.get("description"));
        createMain.put("statusId", context.get("statusId"));
        createMain.put("contentIdFrom", contentIdFrom);
        createMain.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        createMain.put("ownerContentId", ownerContentId);
        createMain.put("dataTemplateTypeId", "SCREEN_COMBINED");
        createMain.put("mapKey", "MAIN");
        Object contentId = null;
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
        if (UtilValidate.isNotEmpty(context.get("_uploadedFile_fileName"))) {
            createImage.put("dataResourceTypeId", "LOCAL_FILE");
            createImage.put("dataTemplateTypeId", "NONE");
            createImage.put("mapKey", "IMAGE");
            createImage.put("ownerContentId", ownerContentId);
            createImage.put("contentName", context.get("contentName"));
            createImage.put("description", context.get("description"));
            createImage.put("statusId", context.get("statusId"));
            createImage.put("contentAssocTypeId", contentAssocTypeId);
            createImage.put("contentIdFrom", contentIdFrom);
            createImage.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
            createImage.put("isPublic", "Y");
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
        }
        if (UtilValidate.isNotEmpty(context.get("articleData"))) {
            createText.put("dataResourceTypeId", "ELECTRONIC_TEXT");
            createText.put("contentPurposeTypeId", "ARTICLE");
            createText.put("dataTemplateTypeId", "NONE");
            createText.put("mapKey", "MAIN");
            createText.put("ownerContentId", ownerContentId);
            createText.put("contentName", context.get("contentName"));
            createText.put("description", context.get("description"));
            createText.put("statusId", context.get("statusId"));
            createText.put("contentAssocTypeId", contentAssocTypeId);
            createText.put("textData", context.get("articleData"));
            createText.put("contentIdFrom", contentIdFrom);
            createText.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
            createText.put("mapKey", "ARTICLE");
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
        }
        if (UtilValidate.isNotEmpty(contentId)) {
            if (UtilValidate.isNotEmpty(context.get("summaryData"))) {
                createSummary.put("dataResourceTypeId", "ELECTRONIC_TEXT");
                createSummary.put("contentPurposeTypeId", "ARTICLE");
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
        }
        result.put("contentId", contentIdFrom);
        result.put("blogContentId", context.get("blogContentId"));

        return "success";
    }


    /**
     * Update a existing Blog Entry
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateBlogEntry(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue imageContent = null;
        Map<String, Object> updContent = null;
        Object ownerContentId = null;
        Object contentAssocTypeId = null;
        Map<String, Object> createText = null;
        Object contentIdFrom = null;
        GenericValue articleText = null;
        Map<String, Object> createSummary = null;
        GenericValue summaryText = null;
        Map<String, Object> createImage = null;
        List<GenericValue> oldAssocs = null;
        GenericValue oldAssoc = null;
        Object showNoResult = "Y";
        String inlineResult = getBlogEntry(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        Object updContent_dataResourceId = null;
        Object imageContent_status_Id = null;
        if ((!java.util.Objects.equals(context.get("contentName"), context.get("contentName")) || !java.util.Objects.equals(context.get("description"), context.get("description")) || !java.util.Objects.equals(context.get("summaryData"), context.get("summaryData")) || !java.util.Objects.equals(context.get("templateDataResourceId"), context.get("templateDataResourceId")) || !java.util.Objects.equals(context.get("statusId"), context.get("statusId")))) {
            // set-service-fields from "parameters" to "updContent" for service "updateContent"
            updContent.putAll(UtilMisc.toMap(context));
            updContent.put("dataResourceId", context.get("templateDataResourceId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateContent", updContent);
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
            if (!java.util.Objects.equals(context.get("statusId"), context.get("statusId"))) {
                if (UtilValidate.isNotEmpty(imageContent)) {
                    imageContent.put("status.Id", context.get("statusId"));
                    try {
                        delegator.store(imageContent);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        if (UtilValidate.isEmpty(articleText)) {
            if (UtilValidate.isNotEmpty(context.get("articleData"))) {
                ownerContentId = context.get("blogContentId");
                contentAssocTypeId = "SUB_CONTENT";
                contentIdFrom = context.get("contentId");
                createText.put("dataResourceTypeId", "ELECTRONIC_TEXT");
                createText.put("contentPurposeTypeId", "ARTICLE");
                createText.put("dataTemplateTypeId", "NONE");
                createText.put("mapKey", "ARTICLE");
                createText.put("ownerContentId", ownerContentId);
                createText.put("contentName", context.get("contentName"));
                createText.put("description", context.get("description"));
                createText.put("statusId", context.get("statusId"));
                createText.put("contentAssocTypeId", contentAssocTypeId);
                createText.put("textData", context.get("articleData"));
                createText.put("contentIdFrom", contentIdFrom);
                createText.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createTextContent", createText);
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
        }
        if (UtilValidate.isNotEmpty(articleText)) {
            if (!java.util.Objects.equals(context.get("articleData"), context.get("articleData"))) {
                articleText.put("textData", context.get("articleData"));
                try {
                    delegator.store(articleText);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isEmpty(context.get("summaryData"))) {
            if (UtilValidate.isNotEmpty(context.get("summaryData"))) {
                ownerContentId = context.get("blogContentId");
                contentAssocTypeId = "SUB_CONTENT";
                contentIdFrom = context.get("contentId");
                createSummary.put("dataResourceTypeId", "ELECTRONIC_TEXT");
                createSummary.put("contentPurposeTypeId", "ARTICLE");
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
        }
        if (UtilValidate.isNotEmpty(context.get("summaryData"))) {
            if (!java.util.Objects.equals(context.get("summaryData"), context.get("summaryData"))) {
                summaryText.put("textData", context.get("summaryData"));
                try {
                    delegator.store(summaryText);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("_uploadedFile_fileName"))) {
            if (UtilValidate.isNotEmpty(imageContent)) {
                try {
                    oldAssocs = EntityQuery.use(delegator)
                            .from("ContentAssoc")
                            .where(UtilMisc.toMap("contentId", context.get("contentId"), "contentIdTo", ((Map<String, Object>) imageContent).get("contentId"), "mapKey", "IMAGE"))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                oldAssoc = EntityUtil.getFirst((List<GenericValue>) oldAssocs);
                Timestamp oldAssoc_thruDate = new Timestamp(System.currentTimeMillis());
                try {
                    delegator.store(oldAssoc);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            createImage.put("dataResourceTypeId", "LOCAL_FILE");
            createImage.put("dataTemplateTypeId", "NONE");
            createImage.put("mapKey", "IMAGE");
            createImage.put("ownerContentId", context.get("contentId"));
            createImage.put("contentName", context.get("contentName"));
            createImage.put("description", context.get("description"));
            createImage.put("statusId", context.get("statusId"));
            createImage.put("contentAssocTypeId", "SUB_CONTENT");
            createImage.put("contentIdFrom", context.get("contentId"));
            createImage.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
            createImage.put("isPublic", "Y");
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
            } catch (Exception e) {
                Debug.logError(e, "Error calling createContentFromUploadedFile: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("contentId", context.get("contentId"));
        result.put("blogContentId", context.get("blogContentId"));

        return "success";
    }


    /**
     * Get blog entries that the user owns or are published
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getOwnedOrPublishedBlogEntries(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object hasPermission = null;
        Map<String, Object> mapIn = null;
        List<Object> blogList = null;
        List<GenericValue> unfilteredList = null;
        try {
            unfilteredList = EntityQuery.use(delegator)
                    .from("ContentAssocViewTo")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentAssocViewTo: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> blogItems = EntityUtil.filterByDate((List<GenericValue>) unfilteredList);
        ((Map<String, Object>) blogList).put((String) context.get(""), null);
        if (blogItems != null) {
            for (Object blogItem : (List<?>) blogItems) {
                // set-service-fields from "blogItem" to "mapIn" for service "genericContentPermission"
                mapIn.putAll(UtilMisc.toMap(blogItem));
                mapIn.put("ownerContentId", context.get("contentId"));
                mapIn.put("mainAction", "VIEW");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("genericContentPermission", mapIn);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    hasPermission = serviceResult.get("hasPermission");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling genericContentPermission: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (Boolean.TRUE.equals(hasPermission)) {
                    blogList.add(blogItem);
                } else {
                    mapIn.put("mainAction", "UPDATE");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("genericContentPermission", mapIn);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        hasPermission = serviceResult.get("hasPermission");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling genericContentPermission: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (Boolean.TRUE.equals(hasPermission)) {
                        blogList.add(blogItem);
                    }
                }
            }
        }
        result.put("blogList", blogList);
        result.put("blogContentId", context.get("blogContentId"));

        return "success";
    }


    /**
     * Get all the info for a blog article
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getBlogEntry(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue summaryContent = null;
        GenericValue imageContent = null;
        GenericValue summaryText = null;
        GenericValue mainContent = null;
        GenericValue articleText = null;
        GenericValue dataResource = null;
        Object statusId = null;
        Object templateDataResourceId = null;
        Object contentId = null;
        Object description = null;
        Object articleData = null;
        Object summaryData = null;
        Object imageDataResourceId = null;
        Object contentName = null;
        if (UtilValidate.isEmpty(context.get("contentId"))) {
            result.put("blogContentId", context.get("blogContentId"));
            return "success";
        }
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
        List<GenericValue> rawAssocs = null;
        try {
            rawAssocs = content.getRelated("FromContentAssoc", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related FromContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> assocs = EntityUtil.filterByDate((List<GenericValue>) rawAssocs);
        if (assocs != null) {
            for (Object assoc : (List<?>) assocs) {
                if ("ARTICLE".equals(((Map<String, Object>) assoc).get("mapKey"))) {
                    try {
                        mainContent = ((GenericValue) assoc).getRelatedOne("ToContent", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one ToContent: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    try {
                        dataResource = mainContent.getRelatedOne("DataResource", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one DataResource: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    try {
                        articleText = dataResource.getRelatedOne("ElectronicText", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one ElectronicText: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                if ("SUMMARY".equals(((Map<String, Object>) assoc).get("mapKey"))) {
                    try {
                        summaryContent = ((GenericValue) assoc).getRelatedOne("ToContent", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one ToContent: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    try {
                        dataResource = summaryContent.getRelatedOne("DataResource", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one DataResource: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    try {
                        summaryText = dataResource.getRelatedOne("ElectronicText", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one ElectronicText: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                if ("IMAGE".equals(((Map<String, Object>) assoc).get("mapKey"))) {
                    try {
                        imageContent = ((GenericValue) assoc).getRelatedOne("ToContent", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one ToContent: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        if (UtilValidate.isEmpty(context.get("showNoResult"))) {
            result.put("contentId", ((Map<String, Object>) content).get("contentId"));
            result.put("contentName", ((Map<String, Object>) content).get("contentName"));
            result.put("description", ((Map<String, Object>) content).get("description"));
            result.put("statusId", ((Map<String, Object>) content).get("statusId"));
            if (UtilValidate.isNotEmpty(imageContent)) {
                result.put("templateDataResourceId", ((Map<String, Object>) content).get("dataResourceId"));
            }
            result.put("articleData", ((Map<String, Object>) articleText).get("textData"));
            result.put("summaryData", ((Map<String, Object>) summaryText).get("textData"));
            result.put("imageContentId", ((Map<String, Object>) imageContent).get("contentId"));
            result.put("articleContentId", ((Map<String, Object>) mainContent).get("contentId"));
            result.put("summaryContentId", ((Map<String, Object>) summaryContent).get("contentId"));
            result.put("blogContentId", context.get("blogContentId"));
        } else {
            contentId = ((Map<String, Object>) content).get("contentId");
            contentName = ((Map<String, Object>) content).get("contentName");
            description = ((Map<String, Object>) content).get("description");
            statusId = ((Map<String, Object>) content).get("statusId");
            templateDataResourceId = ((Map<String, Object>) content).get("dataResourceId");
            articleData = ((Map<String, Object>) articleText).get("textData");
            summaryData = ((Map<String, Object>) summaryText).get("textData");
            imageDataResourceId = ((Map<String, Object>) imageContent).get("dataResourceId");
        }

        return "success";
    }

}

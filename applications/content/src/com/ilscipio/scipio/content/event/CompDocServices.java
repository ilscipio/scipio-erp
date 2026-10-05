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
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.compdoc.CompDocEvents;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://content/script/org/ofbiz/content/compdoc/CompDocServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CompDocServices {

    private static final String MODULE = CompDocServices.class.getName();


    /**
     * Create CompDoc
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String genCompDocInstance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> contentRevisionList = null;
        Object contentRevisionSeqId = null;
        GenericValue existingContent = null;
        GenericValue rootInstanceContent = null;
        GenericValue rootTemplateContent = null;
        try {
            rootTemplateContent = EntityQuery.use(delegator)
                    .from("Content")
                    .where(UtilMisc.toMap("contentId", context.get("instanceOfContentId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("genCompDocInstance> rootTemplateContent: " + rootTemplateContent, MODULE);
        if (UtilValidate.isEmpty(context.get("contentRevisionSeqId"))) {
            try {
                contentRevisionList = EntityQuery.use(delegator)
                        .from("ContentRevision")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContentRevision: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(contentRevisionList)) {
                contentRevisionSeqId = ((GenericValue) ((List<?>) contentRevisionList).get(0)).get("contentRevisionSeqId");
            } else {
                contentRevisionSeqId = null;
            }
        } else {
            contentRevisionSeqId = context.get("contentRevisionSeqId");
        }
        Debug.logInfo("genCompDocInstance> contentRevisionSeqId: " + contentRevisionSeqId, MODULE);
        rootInstanceContent = delegator.makeValue("Content");
        if (UtilValidate.isEmpty(context.get("rootInstanceContentId"))) {
            ((GenericValue) rootInstanceContent).put("contentId", delegator.getNextSeqId("Content"));
            Debug.logInfo("genCompDocInstance 2> rootInstanceContent: " + rootInstanceContent, MODULE);
        } else {
            try {
                existingContent = EntityQuery.use(delegator)
                        .from("Content")
                        .where(UtilMisc.toMap("contentId", context.get("rootInstanceContentId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(existingContent)) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentCompDocInstanceAlreadyExists", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            rootInstanceContent.put("contentId", context.get("rootInstanceContentId"));
        }
        rootInstanceContent.put("contentName", context.get("contentName"));
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        rootInstanceContent.put("instanceOfContentId", context.get("instanceOfContentId"));
        rootInstanceContent.put("createdDate", nowTimestamp);
        rootInstanceContent.put("lastModifiedDate", nowTimestamp);
        context.put("userLogin.userLoginId", ((Map<String, Object>) rootInstanceContent).get("createdByUserLogin"));
        context.put("userLogin.userLoginId", ((Map<String, Object>) rootInstanceContent).get("lastModifiedByUserLogin"));
        rootInstanceContent.put("contentTypeId", "COMPDOC_INSTANCE");
        try {
            delegator.create(rootInstanceContent);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("genCompDocInstance 3> rootInstanceContent: " + rootInstanceContent, MODULE);
        result.put("contentId", ((Map<String, Object>) rootInstanceContent).get("contentId"));
        Object parentTemplateContentId = context.get("instanceOfContentId");
        Object parentInstanceContentId = ((Map<String, Object>) rootInstanceContent).get("contentId");
        Debug.logInfo("genCompDocInstance 4> parentTemplateContentId: " + parentTemplateContentId, MODULE);
        Debug.logInfo("genCompDocInstance 5> parentInstanceContentId: " + parentInstanceContentId, MODULE);
        Map<String, Object> revisionMap = new HashMap<>();
        revisionMap.put("contentId", parentInstanceContentId);
        revisionMap.put("itemContentId", parentInstanceContentId);
        revisionMap.put("userLogin", context.get("userLogin"));
        Debug.logInfo("revisionMap : " + revisionMap, MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentRevisionAndItem", revisionMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentRevisionSeqId = serviceResult.get("contentRevisionSeqId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentRevisionAndItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> cloneMap = new HashMap<>();
        cloneMap.put("contentId", ((Map<String, Object>) revisionMap).get("contentId"));
        cloneMap.put("contentRevisionSeqId", contentRevisionSeqId);
        cloneMap.put("userLogin", context.get("userLogin"));

        return "success";
    }


    /**
     * Create CompDoc
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String genInstanceChildCompDocs(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object parentInstanceContentId = null;
        Object parentTemplateContentId = null;
        GenericValue instanceContentAssoc = null;
        GenericValue instanceContent = null;
        Object thisTemplateContentId = parentTemplateContentId;
        Object thisInstanceContentId = parentInstanceContentId;
        Debug.logInfo("genInstanceChildCompDocs 0> thisTemplateContentId: " + thisTemplateContentId, MODULE);
        Debug.logInfo("genInstanceChildCompDocs 1> thisInstanceContentId: " + thisInstanceContentId, MODULE);
        List<GenericValue> contentAssocList = null;
        try {
            contentAssocList = EntityQuery.use(delegator)
                    .from("ContentAssoc")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("genInstanceChildCompDocs 1> contentAssocList: " + contentAssocList, MODULE);
        if (contentAssocList != null) {
            for (GenericValue templateContentAssoc : contentAssocList) {
                instanceContent = GenericValue.create((GenericValue) context.get("templateContent"));
                ((GenericValue) instanceContent).put("contentId", delegator.getNextSeqId("Content"));
                instanceContent.put("contentTypeId", "TEMPLATE");
                try {
                    delegator.create(instanceContent);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                instanceContentAssoc = delegator.makeValue("ContentAssoc");
                instanceContentAssoc.put("contentIdTo", thisInstanceContentId);
                instanceContentAssoc.put("contentId", ((Map<String, Object>) instanceContent).get("contentId"));
                instanceContentAssoc.put("contentAssocTypeId", "COMPDOC_PART");
                instanceContentAssoc.put("fromDate", context.get("nowTimestamp"));
                try {
                    delegator.create(instanceContent);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                parentTemplateContentId = ((Map<String, Object>) templateContentAssoc).get("contentId");
                parentInstanceContentId = ((Map<String, Object>) instanceContent).get("contentId");
                String result = genInstanceChildCompDocs(request, response);
                if (!"success".equals(result)) {
                    return result;
                }
            }
        }

        return "success";
    }


    /**
     * Create CompDoc
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String persistCompDoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> persistMap = null;
        Map<String, Object> resequenceMap = null;
        Map<String, Object> revisionMap = null;
        Map<String, Object> cloneMap = null;
        // set-service-fields from "parameters" to "persistMap" for service "persistContentAndAssoc"
        persistMap.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isNotEmpty(context.get("mimeTypeId"))) {
            Object persistMap_dataResourceTypeId = null;
            if (("application/msword".equals(context.get("mimeTypeId")) || "application/pdf".equals(context.get("mimeTypeId")) || "application/octet-stream".equals(context.get("mimeTypeId")))) {
                persistMap.put("dataResourceTypeId", "IMAGE_OBJECT");
            } else {
                persistMap.put("dataResourceTypeId", "ELECTRONIC_TEXT");
            }
        }
        persistMap.put("userLogin", context.get("userLogin"));
        Debug.logInfo("persistMap : " + persistMap, MODULE);
        Map<String, Object> pResults = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentAndAssoc", persistMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            pResults = serviceResult;
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentAndAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contentRevisionSeqId", ((Map<String, Object>) pResults).get("contentRevisionSeqId"));
        result.put("contentId", ((Map<String, Object>) pResults).get("contentId"));
        result.put("dataResourceId", ((Map<String, Object>) pResults).get("dataResourceId"));
        result.put("drDataResourceId", ((Map<String, Object>) pResults).get("drDataResourceId"));
        result.put("caContentIdTo", ((Map<String, Object>) pResults).get("caContentIdTo"));
        result.put("caContentId", ((Map<String, Object>) pResults).get("caContentId"));
        result.put("caContentAssocTypeId", ((Map<String, Object>) pResults).get("caContentAssocTypeId"));
        result.put("caFromDate", ((Map<String, Object>) pResults).get("caFromDate"));
        result.put("caSequenceNum", ((Map<String, Object>) pResults).get("caSequenceNum"));
        result.put("roleTypeList", ((Map<String, Object>) pResults).get("roleTypeList"));
        Debug.logInfo("pResults : " + pResults, MODULE);
        if (UtilValidate.isNotEmpty(((Map<String, Object>) pResults).get("contentIdTo"))) {
            resequenceMap.put("contentIdTo", ((Map<String, Object>) pResults).get("contentIdTo"));
            List<Object> resequenceMap_typeList = new LinkedList<>();
            resequenceMap_typeList.add("COMPDOC_PART");
            resequenceMap.put("seqInc", 10);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("resequence", resequenceMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling resequence: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        Object revisionMap_contentId = null;
        if (("COMPDOC_TEMPLATE".equals(((Map<String, Object>) persistMap).get("contentTypeId")) || "COMPDOC_INSTANCE".equals(((Map<String, Object>) persistMap).get("contentTypeId")) || "TEMPLATE".equals(((Map<String, Object>) persistMap).get("contentTypeId")) || "DOCUMENT".equals(((Map<String, Object>) persistMap).get("contentTypeId")))) {
            revisionMap.put("contentId", ((Map<String, Object>) pResults).get("contentId"));
        }
        revisionMap.put("contentId", context.get("rootContentId"));
        revisionMap.put("contentId", ((Map<String, Object>) revisionMap).get("contentId"));
        revisionMap.put("itemContentId", ((Map<String, Object>) pResults).get("contentId"));
        revisionMap.put("userLogin", context.get("userLogin"));
        Debug.logInfo("revisionMap : " + revisionMap, MODULE);
        Object contentRevisionSeqId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentRevisionAndItem", revisionMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentRevisionSeqId = serviceResult.get("contentRevisionSeqId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentRevisionAndItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object cloneMap_contentId = null;
        Object cloneMap_contentRevisionSeqId = null;
        Object cloneMap_userLogin = null;
        if (("COMPDOC_TEMPLATE".equals(((Map<String, Object>) persistMap).get("contentTypeId")) || "TEMPLATE".equals(((Map<String, Object>) persistMap).get("contentTypeId")))) {
            cloneMap.put("contentId", ((Map<String, Object>) revisionMap).get("contentId"));
            cloneMap.put("contentRevisionSeqId", contentRevisionSeqId);
            cloneMap.put("userLogin", context.get("userLogin"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("cloneTemplateContentApprovals", cloneMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling cloneTemplateContentApprovals: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Persist a CompDoc DataResource and data
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String persistCompDocContent(HttpServletRequest request, HttpServletResponse response) {
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
        Object oldDataResourceId = ((Map<String, Object>) content).get("dataResourceId");
        Debug.logInfo("persistCompDocContent(0).content : " + content, MODULE);
        Map<String, Object> persistMap = new HashMap<>();
        // set-service-fields from "parameters" to "persistMap" for service "persistDataResourceAndData"
        persistMap.putAll(UtilMisc.toMap(context));
        persistMap.remove("dataResourceId");
        persistMap.remove("drDataResourceId");
        Debug.logInfo("persistCompDocContent(0.2).persistMap : " + persistMap, MODULE);
        Object newDataResourceId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistDataResourceAndData", persistMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            newDataResourceId = serviceResult.get("dataResourceId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistDataResourceAndData: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("persistCompDocContent(1).newDataResourceId : " + newDataResourceId, MODULE);
        content.put("dataResourceId", newDataResourceId);
        try {
            delegator.store(content);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> revisionMap = new HashMap<>();
        revisionMap.put("contentId", context.get("rootContentId"));
        revisionMap.put("itemContentId", context.get("contentId"));
        revisionMap.put("userLogin", context.get("userLogin"));
        revisionMap.put("oldDataResourceId", oldDataResourceId);
        revisionMap.put("newDataResourceId", newDataResourceId);
        Debug.logInfo("persistCompDocContent(2).revisionMap : " + revisionMap, MODULE);
        Object contentRevisionSeqId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentRevisionAndItem", revisionMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentRevisionSeqId = serviceResult.get("contentRevisionSeqId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentRevisionAndItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("persistCompDocContent(3).contentRevisionSeqId : " + contentRevisionSeqId, MODULE);

        return "success";
    }


    /**
     * Upload/save PDF, create Survey, populate Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String persistCompDocPdf2Survey(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> pdfMap = new HashMap<>();
        pdfMap.put("contentTypeId", "TEMPLATE");
        pdfMap.put("dataResourceTypeId", "IMAGE_OBJECT");
        pdfMap.put("mimeTypeId", "application/vnd.ofbiz.survey");
        pdfMap.put("drMimeTypeId", "application/vnd.ofbiz.survey");
        pdfMap.put("imageData", context.get("imageData"));
        pdfMap.put("_imageData_contentType", context.get("_imageData_contentType"));
        pdfMap.put("_imageData_fileName", context.get("_imageData_fileName"));
        pdfMap.put("contentName", context.get("pdfName"));
        Debug.logInfo("persistCompDocPdf2Survey(1).pdfMap : " + pdfMap, MODULE);
        Object acroFormContentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentAndAssoc", pdfMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            acroFormContentId = serviceResult.get("contentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentAndAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("persistCompDocPdf2Survey(2).acroFormContentId : " + acroFormContentId, MODULE);
        Map<String, Object> acroMap = new HashMap<>();
        acroMap.put("contentId", acroFormContentId);
        Object surveyId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("buildSurveyFromPdf", acroMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            surveyId = serviceResult.get("surveyId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling buildSurveyFromPdf: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("persistCompDocPdf2Survey(3).surveyId : " + surveyId, MODULE);
        Map<String, Object> persistMap = new HashMap<>();
        // set-service-fields from "parameters" to "persistMap" for service "persistCompDocContent"
        persistMap.putAll(UtilMisc.toMap(context));
        persistMap.put("relatedDetailId", surveyId);
        persistMap.put("mimeTypeId", "application/vnd.ofbiz.survey");
        persistMap.remove("_imageData_contentType");
        persistMap.remove("_imageData_fileName");
        persistMap.remove("imageData");
        Debug.logInfo("persistCompDocPdf2Survey(4)persistMap : " + persistMap, MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistCompDocContent", persistMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistCompDocContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create ContentRevision
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentRevision(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue newEntity = delegator.makeValue("ContentRevision");
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
     * Update ContentRevision
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentRevision(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = delegator.makeValue("ContentRevision");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentRevision")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentRevision: " + e.getMessage(), MODULE);
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
     * Remove ContentRevision
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentRevision(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = delegator.makeValue("ContentRevision");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentRevision")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentRevision: " + e.getMessage(), MODULE);
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
     * Create ContentRevisionItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentRevisionItem(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue newEntity = delegator.makeValue("ContentRevisionItem");
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
     * Update ContentRevisionItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentRevisionItem(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = delegator.makeValue("ContentRevisionItem");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentRevisionItem")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentRevisionItem: " + e.getMessage(), MODULE);
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
     * Remove ContentRevisionItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentRevisionItem(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = delegator.makeValue("ContentRevisionItem");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentRevisionItem")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentRevisionItem: " + e.getMessage(), MODULE);
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
     * Update ContentRevision and ContentRevisionItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String persistContentRevisionAndItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object incrementedSeq = null;
        GenericValue newEntity = null;
        List<GenericValue> contentRevisionList = null;
        try {
            contentRevisionList = EntityQuery.use(delegator)
                    .from("ContentRevision")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentRevision: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("contentRevisionList: " + contentRevisionList, MODULE);
        if (UtilValidate.isNotEmpty(contentRevisionList)) {
            newEntity = (GenericValue) ((List<?>) contentRevisionList).get(0);
            incrementedSeq = ((Map<String, Object>) newEntity).get("contentRevisionSeqId");
        } else {
            newEntity = delegator.makeValue("ContentRevision");
        }
        Debug.logInfo("incrementedSeq(0): " + incrementedSeq, MODULE);
        Debug.logInfo("ContentRevision(0): " + newEntity, MODULE);
        if (UtilValidate.isNotEmpty(incrementedSeq)) {
            incrementedSeq = new BigDecimal(incrementedSeq.toString());
        } else {
            incrementedSeq = 1L;
        }
        Object numericPadding = 6;
        Object paddedSeqId = null;
        try {
            paddedSeqId = CompDocEvents.padNumberWithLeadingZeros((Long) incrementedSeq, (Integer) numericPadding);
        } catch (Exception e) {
            Debug.logError(e, "Error calling CompDocEvents.padNumberWithLeadingZeros: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("incrementedSeq(1): " + incrementedSeq, MODULE);
        Debug.logInfo("numericPadding(1): " + numericPadding, MODULE);
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.put("contentRevisionSeqId", paddedSeqId);
        Debug.logInfo("ContentRevision(1): " + newEntity, MODULE);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(context.get("itemContentId"))) {
            newEntity = delegator.makeValue("ContentRevisionItem");
            newEntity.setPKFields((Map<String, Object>) context);
            newEntity.put("contentRevisionSeqId", paddedSeqId);
            newEntity.setNonPKFields((Map<String, Object>) context);
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("ContentRevisionItem(1): " + newEntity, MODULE);
        }
        result.put("contentRevisionSeqId", paddedSeqId);
        Debug.logInfo("paddedSeqId: " + paddedSeqId, MODULE);

        return "success";
    }


    /**
     * Get version of DataResource that fits overall revision
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getRevisionDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object contentRevisionItem = null;
        GenericValue content = null;
        GenericValue dataResource = null;
        List<GenericValue> contentRevisionItems = null;
        try {
            contentRevisionItems = EntityQuery.use(delegator)
                    .from("ContentRevisionItem")
                    .cache()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentRevisionItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(contentRevisionItems)) {
            contentRevisionItem = ((List<?>) contentRevisionItems).get(0);
            try {
                content = EntityQuery.use(delegator)
                        .from("Content")
                        .where(UtilMisc.toMap("contentId", ((Map<String, Object>) contentRevisionItem).get("itemContentId")))
                        .cache()
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(content)) {
                try {
                    dataResource = EntityQuery.use(delegator)
                            .from("DataResource")
                            .where(UtilMisc.toMap("dataResourceId", ((Map<String, Object>) content).get("dataResourceId")))
                            .cache()
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                result.put("dataResource", dataResource);
            }
        }

        return "success";
    }


    /**
     * Get version of DataResource that fits overall revision
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getRevisionItemDataResource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue dataResource = null;
        GenericValue contentRevisionItem = null;
        try {
            contentRevisionItem = EntityQuery.use(delegator)
                    .from("ContentRevisionItem")
                    .where(UtilMisc.toMap("contentId", context.get("contentId"), "itemContentId", context.get("itemContentId"), "contentRevisionSeqId", context.get("contentRevisionSeqId")))
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentRevisionItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue content = null;
        try {
            content = EntityQuery.use(delegator)
                    .from("Content")
                    .where(UtilMisc.toMap("contentId", ((Map<String, Object>) contentRevisionItem).get("itemContentId")))
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(content)) {
            try {
                dataResource = EntityQuery.use(delegator)
                        .from("DataResource")
                        .where(UtilMisc.toMap("dataResourceId", ((Map<String, Object>) content).get("dataResourceId")))
                        .cache()
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("dataResource", dataResource);
        }

        return "success";
    }


    /**
     * Create ContentApproval
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentApproval(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Debug.logInfo("got into createContentApproval(4)", MODULE);
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("ContentApproval");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("contentApprovalId"))) {
            ((GenericValue) newEntity).put("contentApprovalId", delegator.getNextSeqId("ContentApproval"));
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        result.put("contentApprovalId", ((Map<String, Object>) newEntity).get("contentApprovalId"));

        return "success";
    }


    /**
     * Update ContentApproval
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentApproval(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Debug.logInfo("got into updateContentApproval(4)", MODULE);
        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue lookupKeyValue = delegator.makeValue("ContentApproval");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentApproval")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentApproval: " + e.getMessage(), MODULE);
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
     * Remove ContentApproval
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContentApproval(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = delegator.makeValue("ContentApproval");
        lookupKeyValue.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContentApproval")
                    .where(lookupKeyValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentApproval: " + e.getMessage(), MODULE);
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
     * Get ContentApprovals for approval process
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getApprovalsWithPermissions(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object contentApprovalId = null;
        List<Object> contentApprovalList = null;
        Object contentApproval = null;
        List<GenericValue> contentApprovalList2 = null;
        List<GenericValue> instanceApprovalList = null;
        try {
            instanceApprovalList = EntityQuery.use(delegator)
                    .from("MaxContentApprovalView")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying MaxContentApprovalView: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("instanceApprovalList: " + instanceApprovalList, MODULE);
        Debug.logInfo("rootContentId: " + context.get("rootContentId"), MODULE);
        Debug.logInfo("contentRevisionSeqId: " + context.get("contentRevisionSeqId"), MODULE);
        Map<String, Object> inMap2 = new HashMap<>();
        inMap2.put("userLogin", userLogin);
        if (instanceApprovalList != null) {
            for (GenericValue maxContentApproval : instanceApprovalList) {
                Debug.logInfo("maxContentApproval: " + maxContentApproval, MODULE);
                try {
                    contentApprovalList2 = EntityQuery.use(delegator)
                            .from("ContentApproval")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ContentApproval: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                Debug.logInfo("contentApprovalList2: " + contentApprovalList2, MODULE);
                if (UtilValidate.isNotEmpty(contentApprovalList2)) {
                    contentApprovalId = ((GenericValue) ((List<?>) contentApprovalList2).get(0)).get("contentApprovalId");
                    contentApproval = ((List<?>) contentApprovalList2).get(0);
                    Debug.logInfo("contentApproval: " + contentApproval, MODULE);
                    contentApprovalList.add(contentApproval);
                }
            }
        }
        result.put("contentApprovalList", contentApprovalList);

        return "success";
    }


    /**
     * Bump the previous ContentApproval approvals up to current CDI
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String cloneTemplateContentApprovals(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue contentApproval = null;
        Debug.logInfo("cloneTemplateContentApprovals-parameters: " + context, MODULE);
        List<GenericValue> maxContentApprovalList = null;
        try {
            maxContentApprovalList = EntityQuery.use(delegator)
                    .from("MaxContentApprovalView")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying MaxContentApprovalView: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object latestContentRevisionSeqId = ((GenericValue) ((List<?>) maxContentApprovalList).get(0)).get("maxContentRevisionSeqId");
        Debug.logInfo("latestContentRevisionSeqId 0aa: " + latestContentRevisionSeqId, MODULE);
        List<GenericValue> templateContentApprovalList = null;
        try {
            templateContentApprovalList = EntityQuery.use(delegator)
                    .from("ContentApproval")
                    .where(UtilMisc.toMap("contentId", context.get("contentId"), "contentRevisionSeqId", latestContentRevisionSeqId))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentApproval: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("templateContentApprovalList 0aa: " + templateContentApprovalList, MODULE);
        if (templateContentApprovalList != null) {
            for (GenericValue templateContentApproval : templateContentApprovalList) {
                contentApproval = GenericValue.create((GenericValue) templateContentApproval);
                contentApproval.put("contentRevisionSeqId", context.get("contentRevisionSeqId"));
                contentApproval.put("contentId", context.get("contentId"));
                ((GenericValue) contentApproval).put("contentApprovalId", delegator.getNextSeqId("ContentApproval"));
                contentApproval.remove("approvalStatusId");
                Debug.logInfo("contentApproval 2b: " + contentApproval, MODULE);
                try {
                    delegator.create(contentApproval);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Bump the previous ContentApproval approvals up to current CDI
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String cloneInstanceContentApprovals(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object contentApprovalList = null;
        List<GenericValue> templateContentApprovalList = null;
        GenericValue contentApproval = null;
        Object rootTemplateContentId = null;
        List<GenericValue> templateContentRevisionList = null;
        Object latestContentRevisionSeqId = null;
        List<GenericValue> newContentApprovalList = null;
        Map<String, Object> map = null;
        Object finalApprovalStatusId = null;
        GenericValue content = null;
        try {
            content = EntityQuery.use(delegator)
                    .from("Content")
                    .where(UtilMisc.toMap("contentId", context.get("contentId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object thisContentId = context.get("contentId");
        Object thisContentRevisionSeqId = context.get("contentRevisionSeqId");
        Debug.logInfo("cloneContentApprovals(0)- thisContentRevisionSeqId : " + thisContentRevisionSeqId, MODULE);
        Debug.logInfo("cloneContentApprovals(0b)- parameters : " + context + " ", MODULE);
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        List<GenericValue> maxContentApprovalList = null;
        try {
            maxContentApprovalList = EntityQuery.use(delegator)
                    .from("MaxContentApprovalView")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying MaxContentApprovalView: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object maxContentRevisionSeqId = ((GenericValue) ((List<?>) maxContentApprovalList).get(0)).get("maxContentRevisionSeqId");
        Object contentApproval_contentRevisionSeqId = null;
        Object contentApproval_contentId = null;
        Object contentApproval_approvalStatusId = null;
        Object contentApproval_approvalDate = null;
        Object map_contentId = null;
        Object map_contentRevisionSeqId = null;
        if (UtilValidate.isEmpty(maxContentRevisionSeqId)) {
            rootTemplateContentId = ((Map<String, Object>) content).get("instanceOfContentId");
            Debug.logInfo("rootTemplateContentId 0aa: " + rootTemplateContentId, MODULE);
            try {
                templateContentRevisionList = EntityQuery.use(delegator)
                        .from("ContentRevision")
                        .cache()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContentRevision: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            latestContentRevisionSeqId = ((GenericValue) ((List<?>) templateContentRevisionList).get(0)).get("contentRevisionSeqId");
            Debug.logInfo("latestContentRevisionSeqId 0aa: " + latestContentRevisionSeqId, MODULE);
            try {
                templateContentApprovalList = EntityQuery.use(delegator)
                        .from("ContentApproval")
                        .where(UtilMisc.toMap("contentId", rootTemplateContentId, "contentRevisionSeqId", latestContentRevisionSeqId))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContentApproval: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("templateContentApprovalList 0aa: " + templateContentApprovalList, MODULE);
            if (templateContentApprovalList != null) {
                for (GenericValue templateContentApproval : templateContentApprovalList) {
                    contentApproval = GenericValue.create((GenericValue) templateContentApproval);
                    contentApproval.put("contentRevisionSeqId", thisContentRevisionSeqId);
                    contentApproval.put("contentId", thisContentId);
                    ((GenericValue) contentApproval).put("contentApprovalId", delegator.getNextSeqId("ContentApproval"));
                    contentApproval.put("approvalStatusId", "CNTAP_READY");
                    contentApproval.put("approvalDate", nowTimestamp);
                    Debug.logInfo("contentApproval 2b: " + contentApproval, MODULE);
                    try {
                        delegator.create(contentApproval);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        } else {
            map.put("contentId", thisContentId);
            map.put("contentRevisionSeqId", thisContentRevisionSeqId);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getFinalApprovalStatus", map);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                finalApprovalStatusId = serviceResult.get("approvalStatusId");
                contentApprovalList = serviceResult.get("contentApprovalList");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getFinalApprovalStatus: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("cloneContentApprovals(2)- finalApprovalStatusId : " + finalApprovalStatusId + " ", MODULE);
            Debug.logInfo("cloneContentApprovals(2b)- contentApprovalList : " + contentApprovalList + " ", MODULE);
            if (contentApprovalList != null) {
                for (Object existingContentApproval : (List<Object>) contentApprovalList) {
                    contentApproval = GenericValue.create((GenericValue) existingContentApproval);
                    contentApproval.put("contentRevisionSeqId", thisContentRevisionSeqId);
                    ((GenericValue) contentApproval).put("contentApprovalId", delegator.getNextSeqId("ContentApproval"));
                    contentApproval.put("approvalDate", nowTimestamp);
                    if ("COMPDOC_INSTANCE".equals(((Map<String, Object>) content).get("contentTypeId"))) {
                        if (("CNTAP_REJECTED".equals(finalApprovalStatusId) || "CNTAP_APPROVED".equals(finalApprovalStatusId))) {
                            contentApproval.remove("approvalStatusId");
                        }
                    } else {
                        contentApproval.put("approvalStatusId", "CNTAP_READY");
                    }
                    Debug.logInfo("contentApproval 2: " + contentApproval, MODULE);
                    try {
                        delegator.create(contentApproval);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
            rootTemplateContentId = ((Map<String, Object>) content).get("instanceOfContentId");
            Debug.logInfo("rootTemplateContentId 0aa: " + rootTemplateContentId, MODULE);
            try {
                templateContentRevisionList = EntityQuery.use(delegator)
                        .from("ContentRevision")
                        .cache()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContentRevision: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            latestContentRevisionSeqId = ((GenericValue) ((List<?>) templateContentRevisionList).get(0)).get("contentRevisionSeqId");
            Debug.logInfo("latestContentRevisionSeqId 0aa: " + latestContentRevisionSeqId, MODULE);
            try {
                templateContentApprovalList = EntityQuery.use(delegator)
                        .from("ContentApproval")
                        .where(UtilMisc.toMap("contentId", rootTemplateContentId, "contentRevisionSeqId", latestContentRevisionSeqId))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContentApproval: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("templateContentApprovalList 0aa: " + templateContentApprovalList, MODULE);
            if (templateContentApprovalList != null) {
                for (GenericValue templateContentApprovalEntry : templateContentApprovalList) {
                    try {
                        newContentApprovalList = EntityQuery.use(delegator)
                                .from("ContentApproval")
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ContentApproval: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isEmpty(newContentApprovalList)) {
                        contentApproval = GenericValue.create((GenericValue) templateContentApprovalEntry);
                        contentApproval.put("contentRevisionSeqId", thisContentRevisionSeqId);
                        contentApproval.put("contentId", thisContentId);
                        ((GenericValue) contentApproval).put("contentApprovalId", delegator.getNextSeqId("ContentApproval"));
                        contentApproval.put("approvalStatusId", "CNTAP_READY");
                        Debug.logInfo("contentApproval 2b: " + contentApproval, MODULE);
                        try {
                            delegator.create(contentApproval);
                        } catch (Exception e) {
                            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
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
     * Determine ContentApproval permission from passed value
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String hasApprovalPermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object approvalPermExists = null;
        List<GenericValue> contentRoleList = null;
        Timestamp nowTimestamp = null;
        GenericValue contentApprovalPK = delegator.makeValue("ContentApproval");
        contentApprovalPK.put("contentApprovalId", context.get("contentApprovalId"));
        GenericValue contentApproval = null;
        try {
            contentApproval = EntityQuery.use(delegator)
                    .from("ContentApproval")
                    .where(contentApprovalPK)
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContentApproval: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object partyId = ((Map<String, Object>) context.get("userLogin")).get("partyId");
        Debug.logInfo("contentApproval: " + contentApproval, MODULE);
        if ("${partyId}".equals(((Map<String, Object>) contentApproval).get("partyId"))) {
            approvalPermExists = "true";
            Debug.logInfo("approvalPermExists: " + approvalPermExists, MODULE);
            result.put("approvalPermExists", approvalPermExists);
            return "success";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) contentApproval).get("roleTypeId"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            try {
                contentRoleList = EntityQuery.use(delegator)
                        .from("ContentRole")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContentRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(contentRoleList)) {
                approvalPermExists = "true";
                result.put("approvalPermExists", approvalPermExists);
                return "success";
            }
        }
        approvalPermExists = "false";
        result.put("approvalPermExists", approvalPermExists);
        return "success";
    }


    /**
     * Set ContentApprovals for approval process
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String prepForApproval(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Debug.logInfo("got into prepForApproval- parameters: " + context, MODULE);
        Object rootContentId = context.get("rootContentId");
        Map<String, Object> context2 = new HashMap<>();
        context2.put("contentId", context.get("rootContentId"));
        Object contentRevisionSeqId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("persistContentRevisionAndItem", context2);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentRevisionSeqId = serviceResult.get("contentRevisionSeqId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling persistContentRevisionAndItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("got into prepForApproval- contentRevisionSeqId: " + contentRevisionSeqId, MODULE);
        Map<String, Object> context3 = new HashMap<>();
        context3.put("contentId", rootContentId);
        context3.put("contentRevisionSeqId", contentRevisionSeqId);
        Debug.logInfo("got into prepForApproval(3)- context3: " + context3, MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("cloneInstanceContentApprovals", context3);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling cloneInstanceContentApprovals: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Check to see if any open approval conditions exist
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getFinalApprovalStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object finalApprovalStatusId = null;
        List<GenericValue> contentApprovalList = null;
        try {
            contentApprovalList = EntityQuery.use(delegator)
                    .from("ContentApproval")
                    .where(UtilMisc.toMap("contentId", context.get("contentId"), "contentRevisionSeqId", context.get("contentRevisionSeqId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentApproval: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (!(UtilValidate.isEmpty(contentApprovalList))) {
            finalApprovalStatusId = "CNTAP_READY";
            if (contentApprovalList != null) {
                for (GenericValue existingContentApproval : contentApprovalList) {
                    if (("CNTAP_SOFT_REJ".equals(((Map<String, Object>) existingContentApproval).get("approvalStatusId")) || "CNTAP_REJECTED".equals(((Map<String, Object>) existingContentApproval).get("approvalStatusId")))) {
                        finalApprovalStatusId = ((Map<String, Object>) existingContentApproval).get("approvalStatusId");
                    }
                }
            }
            result.put("approvalStatusId", finalApprovalStatusId);
            result.put("contentApprovalList", contentApprovalList);
        } else {
            finalApprovalStatusId = "CNTAP_NOT_READY";
            result.put("approvalStatusId", finalApprovalStatusId);
        }

        return "success";
    }


    /**
     * Check to see if any approval conditions exist for the passed in user
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkForWaitingApprovals(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<Object> roles = null;
        Object contentApprovalId = null;
        List<Object> contentApprovalIdList = null;
        List<GenericValue> contentApprovalList2 = null;
        Object partyId = ((Map<String, Object>) userLogin).get("partyId");
        List<GenericValue> partyRoleList = null;
        try {
            partyRoleList = EntityQuery.use(delegator)
                    .from("PartyRole")
                    .where(UtilMisc.toMap("partyId", partyId))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("userLogin.partyId: " + partyId, MODULE);
        if (partyRoleList != null) {
            for (GenericValue partyRole : partyRoleList) {
                roles.add(((Map<String, Object>) partyRole).get("roleTypeId"));
            }
        }
        Debug.logInfo("roles: " + roles, MODULE);
        List<GenericValue> compdocApprovalList = null;
        try {
            compdocApprovalList = EntityQuery.use(delegator)
                    .from("MaxContentApprovalView")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying MaxContentApprovalView: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("compdocApprovalList: " + compdocApprovalList, MODULE);
        if (compdocApprovalList != null) {
            for (GenericValue maxContentApproval : compdocApprovalList) {
                Debug.logInfo("maxContentApproval: " + maxContentApproval, MODULE);
                try {
                    contentApprovalList2 = EntityQuery.use(delegator)
                            .from("ContentApproval")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ContentApproval: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                Debug.logInfo("contentApprovalList2: " + contentApprovalList2, MODULE);
                if (UtilValidate.isNotEmpty(contentApprovalList2)) {
                    contentApprovalId = ((GenericValue) ((List<?>) contentApprovalList2).get(0)).get("contentApprovalId");
                    Debug.logInfo("contentApproval: " + context.get("contentApproval"), MODULE);
                    contentApprovalIdList.add(contentApprovalId);
                }
            }
        }
        Debug.logInfo("contentApprovalIdList: " + contentApprovalIdList, MODULE);
        List<GenericValue> contentApprovalList = null;
        try {
            contentApprovalList = EntityQuery.use(delegator)
                    .from("ContentApproval")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentApproval: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("contentApprovalList: " + contentApprovalList, MODULE);
        result.put("contentApprovalList", contentApprovalList);

        return "success";
    }


    /**
     * Look for most recent revision for contentId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getMostRecentRevision(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object mostRecentRevisionSeqId = null;
        List<GenericValue> contentRevisions = null;
        try {
            contentRevisions = EntityQuery.use(delegator)
                    .from("ContentRevision")
                    .cache()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentRevision: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("contentRevisions: " + contentRevisions, MODULE);
        if (UtilValidate.isNotEmpty(contentRevisions)) {
            mostRecentRevisionSeqId = ((GenericValue) ((List<?>) contentRevisions).get(0)).get("contentRevisionSeqId");
        }
        result.put("mostRecentRevisionSeqId", mostRecentRevisionSeqId);

        return "success";
    }

}

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
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://content/script/org/ofbiz/content/survey/SurveyServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SurveyServices {

    private static final String MODULE = SurveyServices.class.getName();


    /**
     * Create Survey
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSurvey(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("Survey");
        ((GenericValue) newEntity).put("surveyId", delegator.getNextSeqId("Survey"));
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("surveyId", ((Map<String, Object>) newEntity).get("surveyId"));

        return "success";
    }


    /**
     * Update Survey
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSurvey(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("Survey")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Survey: " + e.getMessage(), MODULE);
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
     * Delete Survey
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSurvey(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("Survey")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Survey: " + e.getMessage(), MODULE);
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
     * Create Survey Multi-Response
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSurveyMultiResp(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("SurveyMultiResp");
        newEntity.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("surveyMultiRespId"))) {
            delegator.setNextSubSeqId(newEntity, "surveyMultiRespId", 2, 1);
            Object surveyMultiRespId = newEntity.get("surveyMultiRespId");
        }
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("surveyMultiRespId", ((Map<String, Object>) newEntity).get("surveyMultiRespId"));

        return "success";
    }


    /**
     * Update Survey Multi-Response
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSurveyMultiResp(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyMultiResp")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyMultiResp: " + e.getMessage(), MODULE);
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
     * Delete Survey Multi-Response
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSurveyMultiResp(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyMultiResp")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyMultiResp: " + e.getMessage(), MODULE);
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
     * Create Survey Multi-Response Column/Category
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSurveyMultiRespColumn(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("SurveyMultiRespColumn");
        newEntity.setPKFields((Map<String, Object>) context);
        delegator.setNextSubSeqId(newEntity, "surveyMultiRespColId", 2, 1);
        Object surveyMultiRespColId = newEntity.get("surveyMultiRespColId");
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("surveyMultiRespColId", ((Map<String, Object>) newEntity).get("surveyMultiRespColId"));

        return "success";
    }


    /**
     * Update Survey Multi-Response Column/Category
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSurveyMultiRespColumn(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyMultiRespColumn")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyMultiRespColumn: " + e.getMessage(), MODULE);
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
     * Delete Survey Multi-Response Column/Category
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSurveyMultiRespColumn(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyMultiRespColumn")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyMultiRespColumn: " + e.getMessage(), MODULE);
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
     * Create Survey Page
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSurveyPage(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("SurveyPage");
        newEntity.setPKFields((Map<String, Object>) context);
        delegator.setNextSubSeqId(newEntity, "surveyPageSeqId", 2, 1);
        Object surveyPageSeqId = newEntity.get("surveyPageSeqId");
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("surveyPageSeqId", ((Map<String, Object>) newEntity).get("surveyPageSeqId"));

        return "success";
    }


    /**
     * Update Survey Page
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSurveyPage(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyPage")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyPage: " + e.getMessage(), MODULE);
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
     * Delete Survey Page
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSurveyPage(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyPage")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyPage: " + e.getMessage(), MODULE);
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
     * Create SurveyApplType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSurveyApplType(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue newEntity = delegator.makeValue("SurveyApplType");
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
     * Update SurveyApplType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSurveyApplType(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("Survey")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Survey: " + e.getMessage(), MODULE);
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
     * Delete SurveyApplType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSurveyApplType(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = null;
        try {
            lookupKeyValue = EntityQuery.use(delegator)
                    .from("SurveyApplType")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyApplType: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookupKeyValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create Survey Question
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSurveyQuestion(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object requiredField = null;
        Object questionType = null;
        if (("ENUMERATION".equals(context.get("surveyQuestionTypeId")) && UtilValidate.isEmpty(context.get("enumTypeId")))) {
            questionType = "ENUMERATION";
            requiredField = "enumTypeId";
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentQuestionTypeRequiredField", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (("GEO".equals(context.get("surveyQuestionTypeId")) && UtilValidate.isEmpty(context.get("geoId")))) {
            questionType = "GEO";
            requiredField = "geoId";
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentQuestionTypeRequiredField", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("SurveyQuestion");
        newEntity.setNonPKFields((Map<String, Object>) context);
        ((GenericValue) newEntity).put("surveyQuestionId", delegator.getNextSeqId("SurveyQuestion"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("surveyQuestionId", ((Map<String, Object>) newEntity).get("surveyQuestionId"));

        return "success";
    }


    /**
     * Update Survey Question
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSurveyQuestion(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object requiredField = null;
        Object questionType = null;
        if (("ENUMERATION".equals(context.get("surveyQuestionTypeId")) && UtilValidate.isEmpty(context.get("enumTypeId")))) {
            questionType = "ENUMERATION";
            requiredField = "enumTypeId";
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentQuestionTypeRequiredField", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (("GEO".equals(context.get("surveyQuestionTypeId")) && UtilValidate.isEmpty(context.get("geoId")))) {
            questionType = "GEO";
            requiredField = "geoId";
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentQuestionTypeRequiredField", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyQuestion")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyQuestion: " + e.getMessage(), MODULE);
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
     * Delete Survey Question
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSurveyQuestion(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyQuestion")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyQuestion: " + e.getMessage(), MODULE);
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
     * Create Survey Question Option
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSurveyQuestionOption(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("SurveyQuestionOption");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        delegator.setNextSubSeqId(newEntity, "surveyOptionSeqId", 5, 1);
        Object surveyOptionSeqId = newEntity.get("surveyOptionSeqId");
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("surveyOptionSeqId", ((Map<String, Object>) newEntity).get("surveyOptionSeqId"));

        return "success";
    }


    /**
     * Update Survey Question Option
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSurveyQuestionOption(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyQuestionOption")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyQuestionOption: " + e.getMessage(), MODULE);
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
     * Delete Survey Question Option
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSurveyQuestionOption(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyQuestionOption")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyQuestionOption: " + e.getMessage(), MODULE);
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
     * Create Survey Question Application
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSurveyQuestionAppl(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue newEntity = delegator.makeValue("SurveyQuestionAppl");
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

        return "success";
    }


    /**
     * Update Survey Question Application
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSurveyQuestionAppl(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyQuestionAppl")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyQuestionAppl: " + e.getMessage(), MODULE);
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
     * Delete Survey Question Application
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSurveyQuestionAppl(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyQuestionAppl")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyQuestionAppl: " + e.getMessage(), MODULE);
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
     * Create Survey QuestionCategory
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSurveyQuestionCategory(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("SurveyQuestionCategory");
        newEntity.setNonPKFields((Map<String, Object>) context);
        ((GenericValue) newEntity).put("surveyQuestionCategoryId", delegator.getNextSeqId("SurveyQuestionCategory"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("surveyQuestionCategoryId", ((Map<String, Object>) newEntity).get("surveyQuestionCategoryId"));

        return "success";
    }


    /**
     * Update Survey QuestionCategory
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSurveyQuestionCategory(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = null;
        try {
            lookupKeyValue = EntityQuery.use(delegator)
                    .from("SurveyQuestionCategory")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyQuestionCategory: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookupKeyValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookupKeyValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete Survey QuestionCategory
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSurveyQuestionCategory(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = null;
        try {
            lookupKeyValue = EntityQuery.use(delegator)
                    .from("SurveyQuestionCategory")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyQuestionCategory: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookupKeyValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create SurveyQuestionType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSurveyQuestionType(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue newEntity = delegator.makeValue("SurveyQuestionType");
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
     * Update SurveyQuestionType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSurveyQuestionType(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = null;
        try {
            lookupKeyValue = EntityQuery.use(delegator)
                    .from("SurveyQuestionType")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyQuestionType: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookupKeyValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookupKeyValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete SurveyQuestionType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSurveyQuestionType(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookupKeyValue = null;
        try {
            lookupKeyValue = EntityQuery.use(delegator)
                    .from("SurveyQuestionType")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyQuestionType: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(lookupKeyValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create SurveyTrigger
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSurveyTrigger(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        // TODO: Convert <check-permission> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("SurveyTrigger");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
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
        result.put("fromDate", ((Map<String, Object>) newEntity).get("fromDate"));

        return "success";
    }


    /**
     * Update SurveyTrigger
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSurveyTrigger(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyTrigger")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyTrigger: " + e.getMessage(), MODULE);
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
     * Delete SurveyTrigger
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSurveyTrigger(HttpServletRequest request, HttpServletResponse response) {
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
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SurveyTrigger")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyTrigger: " + e.getMessage(), MODULE);
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
     * Create Survey Response
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSurveyResponse(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> existingResponses = null;
        GenericValue surveyQuestionAndAppl = null;
        Object answerFieldName = null;
        GenericValue surveyMultiResp = null;
        List<GenericValue> surveyMultiRespColumnList = null;
        GenericValue surveyResponse = null;
        GenericValue dataResource = null;
        Object currentAnswersFieldName = null;
        Map<String, Object> currentAnswers = null;
        Object currentFieldName = null;
        GenericValue survey = null;
        try {
            survey = EntityQuery.use(delegator)
                    .from("Survey")
                    .where(UtilMisc.toMap())
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Survey: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(survey)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoSurveyFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        List<GenericValue> surveyQuestionAndApplList = null;
        try {
            surveyQuestionAndApplList = EntityQuery.use(delegator)
                    .from("SurveyQuestionAndAppl")
                    .where(UtilMisc.toMap("surveyId", ((Map<String, Object>) survey).get("surveyId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SurveyQuestionAndAppl: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(surveyQuestionAndApplList)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoQuestionsSurveyFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        if (!"Y".equals(((Map<String, Object>) survey).get("isAnonymous"))) {
            if (UtilValidate.isEmpty(context.get("partyId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentSurveyAnonymousResponse", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        if (!"Y".equals(((Map<String, Object>) survey).get("allowMultiple"))) {
            if (UtilValidate.isNotEmpty(context.get("partyId"))) {
                try {
                    existingResponses = EntityQuery.use(delegator)
                            .from("SurveyResponse")
                            .where(UtilMisc.toMap("partyId", context.get("partyId"), "surveyId", ((Map<String, Object>) survey).get("surveyId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying SurveyResponse: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(existingResponses)) {
                    if (!"Y".equals(((Map<String, Object>) survey).get("allowUpdate"))) {
                        {
                            String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentSurveyAlreadyResponded", locale);
                            error_list.add(errorMsg);
                            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                        }
                        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                            return "error";
                        }
                    }
                }
            }
        }
        if (UtilValidate.isEmpty(context.get("answers"))) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentSurveyAnswersNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        Object answers = context.get("answers");
        if (surveyQuestionAndApplList != null) {
            for (GenericValue surveyQuestionAndAppl_iter : surveyQuestionAndApplList) {
                surveyQuestionAndAppl = surveyQuestionAndAppl_iter;
                try {
                    surveyMultiResp = surveyQuestionAndAppl.getRelatedOne("SurveyMultiResp", true);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one SurveyMultiResp: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                GenericValue surveyMultiRespColumn = null;
                if (UtilValidate.isNotEmpty(surveyMultiResp)) {
                    if (UtilValidate.isEmpty(((Map<String, Object>) surveyQuestionAndAppl).get("surveyMultiRespColId"))) {
                        try {
                            surveyMultiRespColumnList = surveyMultiResp.getRelated("SurveyMultiRespColumn", null, null, true);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related SurveyMultiRespColumn: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (surveyMultiRespColumnList != null) {
                            for (GenericValue surveyMultiRespColumnEntry : surveyMultiRespColumnList) {
                                answerFieldName = "answers[\"" + ((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionId") + "_" + ((Map<String, Object>) surveyMultiRespColumnEntry).get("surveyMultiRespColId") + "\"]";
                                validateSurveyResponseInline(request, response);
                            }
                        }
                    } else {
                        answerFieldName = "answers[\"" + ((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionId") + "_" + ((Map<String, Object>) surveyQuestionAndAppl).get("surveyMultiRespColId") + "\"]";
                        validateSurveyResponseInline(request, response);
                    }
                } else {
                    answerFieldName = "answers[\"" + ((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionId") + "\"]";
                    validateSurveyResponseInline(request, response);
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        if ("Y".equals(((Map<String, Object>) survey).get("allowUpdate"))) {
            if (UtilValidate.isNotEmpty(context.get("surveyResponseId"))) {
                try {
                    surveyResponse = EntityQuery.use(delegator)
                            .from("SurveyResponse")
                            .where(UtilMisc.toMap())
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying SurveyResponse: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isEmpty(surveyResponse)) {
            surveyResponse = delegator.makeValue("SurveyResponse");
            ((GenericValue) surveyResponse).put("surveyResponseId", delegator.getNextSeqId("SurveyResponse"));
            surveyResponse.setNonPKFields((Map<String, Object>) context);
            try {
                delegator.create(surveyResponse);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) surveyResponse).get("responseDate"))) {
            surveyResponse.put("responseDate", nowTimestamp);
        }
        surveyResponse.put("lastModifiedDate", nowTimestamp);
        try {
            delegator.store(surveyResponse);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("surveyResponseId", ((Map<String, Object>) surveyResponse).get("surveyResponseId"));
        if (UtilValidate.isNotEmpty(context.get("dataResourceId"))) {
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
            dataResource.put("relatedDetailId", ((Map<String, Object>) surveyResponse).get("surveyResponseId"));
            try {
                delegator.store(dataResource);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        List<GenericValue> existingAnswers = null;
        try {
            existingAnswers = surveyResponse.getRelated("SurveyResponseAnswer", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related SurveyResponseAnswer: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (existingAnswers != null) {
            for (GenericValue existingAnswer : existingAnswers) {
                currentAnswersFieldName = ((Map<String, Object>) existingAnswer).get("surveyQuestionId");
                if ((!(UtilValidate.isEmpty(((Map<String, Object>) existingAnswer).get("surveyMultiRespColId"))) && !"_NA_".equals(((Map<String, Object>) existingAnswer).get("surveyMultiRespColId")))) {
                    currentAnswersFieldName = currentAnswersFieldName + "_" + ((Map<String, Object>) existingAnswer).get("surveyMultiRespColId");
                }
                currentAnswers.put((String) currentAnswersFieldName, existingAnswer);
            }
        }
        if (surveyQuestionAndApplList != null) {
            for (GenericValue surveyQuestionAndAppl_iter : surveyQuestionAndApplList) {
                surveyQuestionAndAppl = surveyQuestionAndAppl_iter;
                try {
                    surveyMultiResp = surveyQuestionAndAppl.getRelatedOne("SurveyMultiResp", true);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one SurveyMultiResp: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(surveyMultiResp)) {
                    if (UtilValidate.isEmpty(((Map<String, Object>) surveyQuestionAndAppl).get("surveyMultiRespColId"))) {
                        try {
                            surveyMultiRespColumnList = surveyMultiResp.getRelated("SurveyMultiRespColumn", null, null, true);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related SurveyMultiRespColumn: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (surveyMultiRespColumnList != null) {
                            for (GenericValue surveyMultiRespColumnEntry : surveyMultiRespColumnList) {
                                currentFieldName = ((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionId") + "_" + ((Map<String, Object>) surveyMultiRespColumnEntry).get("surveyMultiRespColId");
                                processSurveyResponseInline(request, response);
                            }
                        }
                    } else {
                        currentFieldName = ((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionId") + "_" + ((Map<String, Object>) surveyQuestionAndAppl).get("surveyMultiRespColId");
                        processSurveyResponseInline(request, response);
                    }
                } else {
                    currentFieldName = ((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionId");
                    processSurveyResponseInline(request, response);
                }
            }
        }
        Map<String, Object> respServiceCtx = new HashMap<>();
        respServiceCtx.put("surveyResponseId", ((Map<String, Object>) surveyResponse).get("surveyResponseId"));
        if (UtilValidate.isNotEmpty(((Map<String, Object>) survey).get("responseService"))) {
            // TODO: Convert <call-service-asynch> element
        }
        result.put("surveyResponseId", ((Map<String, Object>) surveyResponse).get("surveyResponseId"));
        result.put("productStoreSurveyId", context.get("productStoreSurveyId"));
        result.put("surveyId", context.get("surveyId"));

        return "success";
    }


    /**
     * validateSurveyResponseInline
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String validateSurveyResponseInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object codeOk = null;
        Map<String, Object> finAccountMap = null;
        Object answerFieldName = context.get("answerFieldName");
        Object surveyQuestionAndAppl = null;
        if ("Y".equals(((Map<String, Object>) surveyQuestionAndAppl).get("requiredField"))) {
            if (UtilValidate.isEmpty(answerFieldName)) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentSurveyQuestionRequiresAResponse", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        if ("CREDIT_CARD".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
            // TODO: Convert <if-validate-method> element
        }
        if ("GIFT_CARD".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
            finAccountMap.put("finAccountTypeId", "GIFTCERT_ACCOUNT");
            finAccountMap.put("statusId", "FNACT_ACTIVE");
            codeOk = "false";
            // TODO: Convert <find-by-and> element
            if (context.get("finAccountList") != null) {
                for (Object finAccount : (List<Object>) context.get("finAccountList")) {
                    if ("${${answerFieldName}}".equals(((Map<String, Object>) finAccount).get("finAccountCode"))) {
                        codeOk = "true";
                    }
                }
            }
            if (!"true".equals(codeOk)) {
                // TODO: Convert <if-validate-method> element
            }
        }
        if ("DATE".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
            // TODO: Convert <if-validate-method> element
        }
        if ("EMAIL".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
            // TODO: Convert <if-validate-method> element
        }
        if ("NUMBER_CURRENCY".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
            // TODO: Convert <if-validate-method> element
        }
        if ("NUMBER_FLOAT".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
            // TODO: Convert <if-validate-method> element
        }
        if ("NUMBER_LONG".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
            // TODO: Convert <if-validate-method> element
        }
        if ("URL".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
            // TODO: Convert <if-validate-method> element
        }

        return "success";
    }


    /**
     * processSurveyResponseInline
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String processSurveyResponseInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue responseAnswer = null;
        Map<String, Object> partyContent = null;
        Object dataResourceId = null;
        Map<String, Object> imageDataResource = null;
        GenericValue content = null;
        Map<String, Object> dataResource = null;
        Map<String, Object> currentAnswers = (Map<String, Object>) context.get("currentAnswers");
        Object currentFieldName = context.get("currentFieldName");
        if (UtilValidate.isNotEmpty(context.get("currentAnswers"))) {
            responseAnswer = (GenericValue) currentAnswers.get(context.get("currentFieldName"));
        }
        Object surveyQuestionAndAppl = null;
        Object surveyMultiRespColumn = null;
        Object responseAnswer_surveyResponseId = null;
        Object responseAnswer_surveyQuestionId = null;
        Object responseAnswer_surveyMultiRespId = null;
        Object responseAnswer_surveyMultiRespColId = null;
        if ((UtilValidate.isEmpty(responseAnswer) || !java.util.Objects.equals(((Map<String, Object>) responseAnswer).get("surveyQuestionId"), ((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionId")) || (!(UtilValidate.isEmpty(surveyMultiRespColumn)) && !java.util.Objects.equals(((Map<String, Object>) responseAnswer).get("surveyMultiRespColId"), ((Map<String, Object>) surveyMultiRespColumn).get("surveyMultiRespColId"))))) {
            responseAnswer = delegator.makeValue("SurveyResponseAnswer");
            responseAnswer.put("surveyResponseId", ((Map<String, Object>) context.get("surveyResponse")).get("surveyResponseId"));
            responseAnswer.put("surveyQuestionId", ((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionId"));
            if (UtilValidate.isNotEmpty(surveyMultiRespColumn)) {
                responseAnswer.put("surveyMultiRespId", ((Map<String, Object>) surveyMultiRespColumn).get("surveyMultiRespId"));
                responseAnswer.put("surveyMultiRespColId", ((Map<String, Object>) surveyMultiRespColumn).get("surveyMultiRespColId"));
            } else {
                responseAnswer.put("surveyMultiRespColId", "_NA_");
            }
            try {
                delegator.create(responseAnswer);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        Map<String, Object> answers = new HashMap<>();
        if (UtilValidate.isNotEmpty(((Map<String, Object>) answers).get(context.get("currentFieldName")))) {
            if ("BOOLEAN".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("booleanResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("EMAIL".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("textResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("DATE".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("textResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("URL".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("textResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("CREDIT_CARD".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("textResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("GIFT_CARD".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("textResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("PASSWORD".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("textResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("TEXT_SHORT".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("textResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("TEXT_LONG".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("textResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("TEXTAREA".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("textResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("NUMBER_CURRENCY".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("currencyResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("NUMBER_FLOAT".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("floatResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("NUMBER_LONG".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("numericResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("OPTION".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("surveyOptionSeqId", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("GEO".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("textResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("ENUMERATION".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                responseAnswer.put("textResponse", ((Map<String, Object>) answers).get(context.get("currentFieldName")));
            }
            if ("CONTENT".equals(((Map<String, Object>) surveyQuestionAndAppl).get("surveyQuestionTypeId"))) {
                // TODO: Convert <if-instance-of> element
            }
            if (UtilValidate.isEmpty(((Map<String, Object>) responseAnswer).get("sequenceNum"))) {
                responseAnswer.put("sequenceNum", ((Map<String, Object>) surveyQuestionAndAppl).get("sequenceNum"));
            }
            responseAnswer.put("answeredDate", context.get("nowTimestamp"));
            try {
                delegator.store(responseAnswer);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }

}

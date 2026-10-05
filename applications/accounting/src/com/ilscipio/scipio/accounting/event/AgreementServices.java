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
package com.ilscipio.scipio.accounting.event;

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
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/agreement/AgreementServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class AgreementServices {

    private static final String MODULE = AgreementServices.class.getName();


    /**
     * Create an Agreement
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAgreement(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("Agreement");
        newEntity.setNonPKFields((Map<String, Object>) context);
        String agreementId = delegator.getNextSeqId("Agreement");
        newEntity.put("agreementId", agreementId);
        result.put("agreementId", agreementId);
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
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Update an existing Agreement
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAgreement(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreement = null;
        try {
            agreement = EntityQuery.use(delegator)
                    .from("Agreement")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Agreement: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        agreement.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(agreement);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
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
     * Copy an existing Agreement
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String copyAgreement(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createAgreementItemInMap = null;
        List<GenericValue> agreementTerms = null;
        Map<String, Object> createAgreementTermInMap = null;
        Map<String, Object> createAgreementProductApplInMap = null;
        List<GenericValue> agreementProductAppls = null;
        List<GenericValue> agreementFacilityAppls = null;
        Map<String, Object> createAgreementFacilityApplInMap = null;
        Map<String, Object> createAgreementPartyApplicInMap = null;
        GenericValue agreement = null;
        try {
            agreement = EntityQuery.use(delegator)
                    .from("Agreement")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Agreement: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> createAgreementInMap = new HashMap<>();
        // set-service-fields from "agreement" to "createAgreementInMap" for service "createAgreement"
        createAgreementInMap.putAll(UtilMisc.toMap(agreement));
        Object agreementIdTo = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAgreement", createAgreementInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            agreementIdTo = serviceResult.get("agreementId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAgreement: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> agreementItems = null;
        try {
            agreementItems = agreement.getRelated("AgreementItem", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related AgreementItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (agreementItems != null) {
            for (GenericValue agreementItem : agreementItems) {
                createAgreementItemInMap = null;
                // set-service-fields from "agreementItem" to "createAgreementItemInMap" for service "createAgreementItem"
                createAgreementItemInMap.putAll(UtilMisc.toMap(agreementItem));
                createAgreementItemInMap.put("agreementId", agreementIdTo);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createAgreementItem", createAgreementItemInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createAgreementItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if ("Y".equals(context.get("copyAgreementTerms"))) {
            try {
                agreementTerms = agreement.getRelated("AgreementTerm", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related AgreementTerm: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (agreementTerms != null) {
                for (GenericValue agreementTerm : agreementTerms) {
                    createAgreementTermInMap = null;
                    // set-service-fields from "agreementTerm" to "createAgreementTermInMap" for service "createAgreementTerm"
                    createAgreementTermInMap.putAll(UtilMisc.toMap(agreementTerm));
                    createAgreementTermInMap.put("agreementId", agreementIdTo);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createAgreementTerm", createAgreementTermInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createAgreementTerm: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        if ("Y".equals(context.get("copyAgreementProducts"))) {
            try {
                agreementProductAppls = agreement.getRelated("AgreementProductAppl", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related AgreementProductAppl: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (agreementProductAppls != null) {
                for (GenericValue agreementProductAppl : agreementProductAppls) {
                    createAgreementProductApplInMap = null;
                    // set-service-fields from "agreementProductAppl" to "createAgreementProductApplInMap" for service "createAgreementProductAppl"
                    createAgreementProductApplInMap.putAll(UtilMisc.toMap(agreementProductAppl));
                    createAgreementProductApplInMap.put("agreementId", agreementIdTo);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createAgreementProductAppl", createAgreementProductApplInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createAgreementProductAppl: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        if ("Y".equals(context.get("copyAgreementFacilities"))) {
            try {
                agreementFacilityAppls = agreement.getRelated("AgreementFacilityAppl", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related AgreementFacilityAppl: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (agreementFacilityAppls != null) {
                for (GenericValue agreementFacilityAppl : agreementFacilityAppls) {
                    createAgreementFacilityApplInMap = null;
                    // set-service-fields from "agreementFacilityAppl" to "createAgreementFacilityApplInMap" for service "createAgreementFacilityAppl"
                    createAgreementFacilityApplInMap.putAll(UtilMisc.toMap(agreementFacilityAppl));
                    createAgreementFacilityApplInMap.put("agreementId", agreementIdTo);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createAgreementFacilityAppl", createAgreementFacilityApplInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createAgreementFacilityAppl: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        if ("Y".equals(context.get("copyAgreementParties"))) {
            List<GenericValue> agreementPartyApplic = null;
            try {
                agreementPartyApplic = agreement.getRelated("AgreementPartyApplic", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related AgreementPartyApplic: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (agreementPartyApplic != null) {
                for (GenericValue agreementPartyApplicEntry : agreementPartyApplic) {
                    createAgreementPartyApplicInMap = null;
                    // set-service-fields from "agreementPartyApplic" to "createAgreementPartyApplicInMap" for service "createAgreementPartyApplic"
                    createAgreementPartyApplicInMap.putAll(UtilMisc.toMap(agreementPartyApplicEntry));
                    createAgreementPartyApplicInMap.put("agreementId", agreementIdTo);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createAgreementPartyApplic", createAgreementPartyApplicInMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createAgreementPartyApplic: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        result.put("agreementId", agreementIdTo);

        return "success";
    }


    /**
     * Create an AgreementItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAgreementItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        newEntity = delegator.makeValue("AgreementItem");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.put("agreementId", context.get("agreementId"));
        if (UtilValidate.isEmpty(context.get("agreementItemSeqId"))) {
            delegator.setNextSubSeqId(newEntity, "agreementItemSeqId", 5, 1);
            Object agreementItemSeqId = newEntity.get("agreementItemSeqId");
            newEntity.put("agreementItemSeqId", agreementItemSeqId);
        } else {
            newEntity.put("agreementItemSeqId", context.get("agreementItemSeqId"));
        }
        result.put("agreementId", ((Map<String, Object>) newEntity).get("agreementId"));
        result.put("agreementItemSeqId", ((Map<String, Object>) newEntity).get("agreementItemSeqId"));
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

        return "success";
    }


    /**
     * Update an existing AgreementItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAgreementItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementItem = null;
        try {
            agreementItem = EntityQuery.use(delegator)
                    .from("AgreementItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        agreementItem.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(agreementItem);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
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
     * Remove an AgreementItem
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeAgreementItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementItem = null;
        try {
            agreementItem = EntityQuery.use(delegator)
                    .from("AgreementItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(agreementItem);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create an AgreementTerm
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAgreementTerm(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("AgreementTerm");
        newEntity.setNonPKFields((Map<String, Object>) context);
        String agreementTermId = delegator.getNextSeqId("AgreementTerm");
        newEntity.put("agreementId", context.get("agreementId"));
        newEntity.put("agreementItemSeqId", context.get("agreementItemSeqId"));
        newEntity.put("agreementTermId", agreementTermId);
        result.put("agreementTermId", agreementTermId);
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

        return "success";
    }


    /**
     * Update an existing AgreementTerm
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAgreementTerm(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementTerm = null;
        try {
            agreementTerm = EntityQuery.use(delegator)
                    .from("AgreementTerm")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementTerm: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        agreementTerm.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(agreementTerm);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
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
     * Delete an existing AgreementTerm
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteAgreementTerm(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementTerm = null;
        try {
            agreementTerm = EntityQuery.use(delegator)
                    .from("AgreementTerm")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementTerm: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(agreementTerm);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
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
     * Create an AgreementPromoAppl
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAgreementPromoAppl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("AgreementPromoAppl");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
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

        return "success";
    }


    /**
     * Update an existing AgreementPromoAppl
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAgreementPromoAppl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementPromoAppl = null;
        try {
            agreementPromoAppl = EntityQuery.use(delegator)
                    .from("AgreementPromoAppl")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementPromoAppl: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        agreementPromoAppl.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(agreementPromoAppl);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
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
     * Remove an existing AgreementPromoAppl
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeAgreementPromoAppl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementPromoAppl = null;
        try {
            agreementPromoAppl = EntityQuery.use(delegator)
                    .from("AgreementPromoAppl")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementPromoAppl: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(agreementPromoAppl);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
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
     * Create an AgreementProductAppl
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAgreementProductAppl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("AgreementProductAppl");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
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

        return "success";
    }


    /**
     * Update an existing AgreementProductAppl
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAgreementProductAppl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementProductAppl = null;
        try {
            agreementProductAppl = EntityQuery.use(delegator)
                    .from("AgreementProductAppl")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementProductAppl: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        agreementProductAppl.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(agreementProductAppl);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
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
     * Remove an existing AgreementProductAppl
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeAgreementProductAppl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementProductAppl = null;
        try {
            agreementProductAppl = EntityQuery.use(delegator)
                    .from("AgreementProductAppl")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementProductAppl: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(agreementProductAppl);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
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
     * Create an AgreementPartyApplic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAgreementPartyApplic(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("AgreementPartyApplic");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
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

        return "success";
    }


    /**
     * Update an existing AgreementPartyApplic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAgreementPartyApplic(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementPartyApplic = null;
        try {
            agreementPartyApplic = EntityQuery.use(delegator)
                    .from("AgreementPartyApplic")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementPartyApplic: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        agreementPartyApplic.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(agreementPartyApplic);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
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
     * Remove an existing AgreementPartyApplic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeAgreementPartyApplic(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementPartyApplic = null;
        try {
            agreementPartyApplic = EntityQuery.use(delegator)
                    .from("AgreementPartyApplic")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementPartyApplic: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(agreementPartyApplic);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
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
     * Create an AgreementGeographicalApplic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAgreementGeographicalApplic(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("AgreementGeographicalApplic");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
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

        return "success";
    }


    /**
     * Update an existing AgreementGeographicalApplic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAgreementGeographicalApplic(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementGeographicalApplic = null;
        try {
            agreementGeographicalApplic = EntityQuery.use(delegator)
                    .from("AgreementGeographicalApplic")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementGeographicalApplic: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        agreementGeographicalApplic.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(agreementGeographicalApplic);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
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
     * Remove an existing AgreementGeographicalApplic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeAgreementGeographicalApplic(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementGeographicalApplic = null;
        try {
            agreementGeographicalApplic = EntityQuery.use(delegator)
                    .from("AgreementGeographicalApplic")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementGeographicalApplic: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(agreementGeographicalApplic);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
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
     * Create an Agreement Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAgreementRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("AgreementRole");
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
     * Update an Agreement Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAgreementRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("AgreementRole")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setPKFields((Map<String, Object>) context);
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
     * Delete an Agreement Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteAgreementRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementRole = null;
        try {
            agreementRole = EntityQuery.use(delegator)
                    .from("AgreementRole")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(agreementRole);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create a link between a WorkEffort and a Agreement Appl
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAgreementWorkEffortApplic(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        GenericValue agreementWorkEffortApplic = null;
        try {
            agreementWorkEffortApplic = EntityQuery.use(delegator)
                    .from("AgreementWorkEffortApplic")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementWorkEffortApplic: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(agreementWorkEffortApplic)) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingAgreementWorkEffortApplicAlreadyExists", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        } else {
            newEntity = delegator.makeValue("AgreementWorkEffortApplic");
            newEntity.setPKFields((Map<String, Object>) context);
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
     * Remove a link between a WorkEffort and a Agreement Appl
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteAgreementWorkEffortApplic(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue agreementWorkEffortApplic = null;
        try {
            agreementWorkEffortApplic = EntityQuery.use(delegator)
                    .from("AgreementWorkEffortApplic")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AgreementWorkEffortApplic: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(agreementWorkEffortApplic);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }

}

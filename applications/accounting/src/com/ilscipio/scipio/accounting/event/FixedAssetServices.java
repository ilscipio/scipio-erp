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
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/fixedasset/FixedAssetServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class FixedAssetServices {

    private static final String MODULE = FixedAssetServices.class.getName();


    /**
     * Create an FixedAsset
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFixedAsset(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        GenericValue fixedAsset = null;
        newEntity = delegator.makeValue("FixedAsset");
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(context.get("fixedAssetId"))) {
            ((GenericValue) newEntity).put("fixedAssetId", delegator.getNextSeqId("FixedAsset"));
        } else {
            try {
                fixedAsset = EntityQuery.use(delegator)
                        .from("FixedAsset")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FixedAsset: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(fixedAsset)) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingFixedAssetIdAlreadyExists", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                Debug.logInfo(UtilProperties.getMessage("AccountingUiLabels", "AccountingFixedAssetIdAlreadyExists", locale), MODULE);
            } else {
                // TODO: Convert <check-id> element
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            newEntity.put("fixedAssetId", context.get("fixedAssetId"));
        }
        result.put("fixedAssetId", ((Map<String, Object>) newEntity).get("fixedAssetId"));
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
     * Update an existing FixedAsset
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFixedAsset(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FixedAsset")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAsset: " + e.getMessage(), MODULE);
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
     * Add Product to FixedAsset
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String addFixedAssetProduct(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("FixedAssetProduct");
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
     * Update Products of a FixedAsset
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFixedAssetProduct(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FixedAssetProduct")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetProduct: " + e.getMessage(), MODULE);
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
     * Remove Product From FixedAsset
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeFixedAssetProduct(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FixedAssetProduct")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetProduct: " + e.getMessage(), MODULE);
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
     * Create a FixedAssetStdCost
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFixedAssetStdCost(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue fixedAssetStdCost = null;
        try {
            fixedAssetStdCost = EntityQuery.use(delegator)
                    .from("FixedAssetStdCost")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetStdCost: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(fixedAssetStdCost)) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingFixedAssetStdCostAlreadyExists", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("FixedAssetStdCost");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
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
     * Update an existing FixedAssetStdCost
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFixedAssetStdCost(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue fixedAssetStdCost = null;
        try {
            fixedAssetStdCost = EntityQuery.use(delegator)
                    .from("FixedAssetStdCost")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetStdCost: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        fixedAssetStdCost.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(fixedAssetStdCost);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Cancel an existing FixedAssetStdCost
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String cancelFixedAssetStdCost(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue fixedAssetStdCost = null;
        try {
            fixedAssetStdCost = EntityQuery.use(delegator)
                    .from("FixedAssetStdCost")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetStdCost: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Timestamp fixedAssetStdCost_thruDate = new Timestamp(System.currentTimeMillis());
        try {
            delegator.store(fixedAssetStdCost);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create an FixedAssetIdent
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFixedAssetIdent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("FixedAssetIdent");
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
     * Update an existing FixedAssetIdent
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFixedAssetIdent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FixedAssetIdent")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetIdent: " + e.getMessage(), MODULE);
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
     * Remove Fixed Assets Idents FixedAssetIdent
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeFixedAssetIdent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FixedAssetIdent")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetIdent: " + e.getMessage(), MODULE);
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
     * Create FixedAsset Registration
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFixedAssetRegistration(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("FixedAssetRegistration");
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

        return "success";
    }


    /**
     * Update an existing FixedAsset Registration
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFixedAssetRegistration(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FixedAssetRegistration")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetRegistration: " + e.getMessage(), MODULE);
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
     * Delete FixedAsset Registration
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteFixedAssetRegistration(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FixedAssetRegistration")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetRegistration: " + e.getMessage(), MODULE);
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
     * create a FixedAssetMaint
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFixedAssetMaint(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        GenericValue fixedAsset = null;
        GenericValue productMaint = null;
        Object maintTemplateWorkEffortId = null;
        Map<String, Object> duplicateTemplateWorkEffortMap = null;
        GenericValue productMaintType = null;
        Map<String, Object> maintWorkEffortMap = null;
        String workEffortName = null;
        newEntity = delegator.makeValue("FixedAssetMaint");
        newEntity.setPKFields((Map<String, Object>) context);
        delegator.setNextSubSeqId(newEntity, "maintHistSeqId", 5, 1);
        Object maintHistSeqId = newEntity.get("maintHistSeqId");
        result.put("maintHistSeqId", ((Map<String, Object>) newEntity).get("maintHistSeqId"));
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isNotEmpty(context.get("productMaintSeqId"))) {
            try {
                fixedAsset = EntityQuery.use(delegator)
                        .from("FixedAsset")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FixedAsset: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                productMaint = EntityQuery.use(delegator)
                        .from("ProductMaint")
                        .where(UtilMisc.toMap("productId", ((Map<String, Object>) fixedAsset).get("instanceOfProductId"), "productMaintSeqId", context.get("productMaintSeqId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductMaint: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            newEntity.put("productMaintTypeId", ((Map<String, Object>) productMaint).get("productMaintTypeId"));
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) productMaint).get("maintTemplateWorkEffortId"))) {
            maintTemplateWorkEffortId = ((Map<String, Object>) productMaint).get("maintTemplateWorkEffortId");
        } else {
            maintTemplateWorkEffortId = context.get("maintTemplateWorkEffortId");
        }
        if (UtilValidate.isNotEmpty(maintTemplateWorkEffortId)) {
            duplicateTemplateWorkEffortMap.put("oldWorkEffortId", maintTemplateWorkEffortId);
            ((GenericValue) duplicateTemplateWorkEffortMap).put("workEffortId", delegator.getNextSeqId("WorkEffort"));
            duplicateTemplateWorkEffortMap.put("duplicateWorkEffortAssocs", "Y");
            duplicateTemplateWorkEffortMap.put("duplicateWorkEffortNotes", "Y");
            duplicateTemplateWorkEffortMap.put("duplicateWorkEffortContents", "Y");
            duplicateTemplateWorkEffortMap.put("duplicateWorkEffortAssignmentRates", "Y");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("duplicateWorkEffort", duplicateTemplateWorkEffortMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling duplicateWorkEffort: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            newEntity.put("scheduleWorkEffortId", ((Map<String, Object>) duplicateTemplateWorkEffortMap).get("workEffortId"));
        } else {
            try {
                fixedAsset = EntityQuery.use(delegator)
                        .from("FixedAsset")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FixedAsset: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            workEffortName = UtilProperties.getMessage("AccountingUiLabels", "AccountingFixedAssetMaintWorkEffortName", locale);
            maintWorkEffortMap.put("workEffortName", workEffortName);
            maintWorkEffortMap.put("workEffortTypeId", "TASK");
            maintWorkEffortMap.put("workEffortPurposeTypeId", "WEPT_MAINTENANCE");
            maintWorkEffortMap.put("currentStatusId", "CAL_TENTATIVE");
            maintWorkEffortMap.put("quickAssignPartyId", ((Map<String, Object>) userLogin).get("partyId"));
            maintWorkEffortMap.put("fixedAssetId", ((Map<String, Object>) newEntity).get("fixedAssetId"));
            try {
                productMaintType = newEntity.getRelatedOne("ProductMaintType", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one ProductMaintType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            maintWorkEffortMap.put("description", ((Map<String, Object>) productMaintType).get("description"));
            maintWorkEffortMap.put("estimatedStartDate", context.get("estimatedStartDate"));
            maintWorkEffortMap.put("estimatedCompletionDate", context.get("estimatedCompletionDate"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffort", maintWorkEffortMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                newEntity.put("scheduleWorkEffortId", serviceResult.get("workEffortId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createWorkEffort: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
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
        Object workEffortId = ((Map<String, Object>) newEntity).get("scheduleWorkEffortId");
        String inlineResult = autoAssignFixedAssetPartiesToMaintenance(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }

        return "success";
    }


    /**
     * Update an existing FixedAsset Maintenance
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFixedAssetMaint(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue lookedUpValue = null;
        GenericValue fixedAsset = null;
        GenericValue productMaint = null;
        Object workEffortId = null;
        Map<String, Object> updateWorkEffortCtx = null;
        GenericValue workEffort = null;
        List<GenericValue> wepas = null;
        Timestamp nowTimestamp = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FixedAssetMaint")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetMaint: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object oldStatusId = ((Map<String, Object>) lookedUpValue).get("statusId");
        result.put("oldStatusId", oldStatusId);
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isNotEmpty(context.get("productMaintSeqId"))) {
            try {
                fixedAsset = EntityQuery.use(delegator)
                        .from("FixedAsset")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FixedAsset: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                productMaint = EntityQuery.use(delegator)
                        .from("ProductMaint")
                        .where(UtilMisc.toMap("productId", ((Map<String, Object>) fixedAsset).get("instanceOfProductId"), "productMaintSeqId", context.get("productMaintSeqId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ProductMaint: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            lookedUpValue.put("productMaintTypeId", ((Map<String, Object>) productMaint).get("productMaintTypeId"));
        }
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object updateWorkEffortCtx_workEffortId = null;
        Object updateWorkEffortCtx_currentStatusId = null;
        Object updateWorkEffortCtx_actualCompletionDate = null;
        Object wepa_thruDate = null;
        if (("FAM_COMPLETED".equals(((Map<String, Object>) lookedUpValue).get("statusId")) && !java.util.Objects.equals(oldStatusId, ((Map<String, Object>) lookedUpValue).get("statusId")))) {
            workEffortId = ((Map<String, Object>) lookedUpValue).get("scheduleWorkEffortId");
            try {
                workEffort = EntityQuery.use(delegator)
                        .from("WorkEffort")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if ((!(UtilValidate.isEmpty(workEffort)) && UtilValidate.isEmpty(((Map<String, Object>) workEffort).get("actualCompletionDate")) && !"CAL_COMPLETED".equals(((Map<String, Object>) workEffort).get("currentStatusId")))) {
                nowTimestamp = new Timestamp(System.currentTimeMillis());
                updateWorkEffortCtx.put("workEffortId", workEffortId);
                updateWorkEffortCtx.put("currentStatusId", "CAL_ACCEPTED");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateWorkEffort", updateWorkEffortCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateWorkEffort: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
                updateWorkEffortCtx.put("currentStatusId", "CAL_COMPLETED");
                updateWorkEffortCtx.put("actualCompletionDate", nowTimestamp);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateWorkEffort", updateWorkEffortCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateWorkEffort: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
                try {
                    wepas = EntityQuery.use(delegator)
                            .from("WorkEffortPartyAssignment")
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying WorkEffortPartyAssignment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (wepas != null) {
                    for (GenericValue wepa : wepas) {
                        wepa.put("thruDate", nowTimestamp);
                        try {
                            delegator.store(wepa);
                        } catch (Exception e) {
                            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                            return "error";
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Delete FixedAsset Maintenance
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteFixedAssetMaint(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FixedAssetMaint")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetMaint: " + e.getMessage(), MODULE);
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
     * Create a Fixed Asset Meter Reading
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFixedAssetMeter(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("FixedAssetMeter");
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
        Object meterValue = newEntity;
        String result = createMaintsFromMeterReading(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Update a Fixed Asset Meter Reading
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFixedAssetMeter(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FixedAssetMeter")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetMeter: " + e.getMessage(), MODULE);
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
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object meterValue = lookedUpValue;
        String result = createMaintsFromMeterReading(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Delete a Fixed Asset Meter Reading
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteFixedAssetMeter(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FixedAssetMeter")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetMeter: " + e.getMessage(), MODULE);
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
     * Create Fixed Asset Maintenances From A Meter Reading
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createMaintsFromMeterReading(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        BigDecimal nextIntervalQty = null;
        Object maintDue = null;
        List<GenericValue> maintList = null;
        Map<String, Object> createMaintCxt = null;
        Long listSize = null;
        BigDecimal maxIntervalQty = null;
        Long repeatCount = null;
        Object meterValue = null;
        if (UtilValidate.isNotEmpty(((Map<String, Object>) meterValue).get("maintHistSeqId"))) {
            return "success";
        }
        GenericValue fixedAssetValue = null;
        try {
            fixedAssetValue = EntityQuery.use(delegator)
                    .from("FixedAsset")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAsset: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) fixedAssetValue).get("instanceOfProductId"))) {
            return "success";
        }
        List<GenericValue> productMaintList = null;
        try {
            productMaintList = EntityQuery.use(delegator)
                    .from("ProductMaint")
                    .where(UtilMisc.toMap("productId", ((Map<String, Object>) fixedAssetValue).get("instanceOfProductId"), "intervalMeterTypeId", ((Map<String, Object>) meterValue).get("productMeterTypeId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductMaint: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (productMaintList != null) {
            for (GenericValue productMaintValue : productMaintList) {
                repeatCount = (Long) ((Map<String, Object>) productMaintValue).get("repeatCount");
                try {
                    maintList = EntityQuery.use(delegator)
                            .from("FixedAssetMaint")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying FixedAssetMaint: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                listSize = 0L;
                if (UtilValidate.isNotEmpty(maintList)) {
                    listSize = (Long) (long) (maintList != null ? ((java.util.List<?>) maintList).size() : 0);
                }
                maxIntervalQty = BigDecimal.ZERO;
                if (maintList != null) {
                    for (GenericValue maintValue : maintList) {
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) maintValue).get("intervalQuantity"))) {
                            if (((Map<String, Object>) maintValue).get("intervalQuantity") != null /* TODO: field compare operator greater */) {
                                maxIntervalQty = (BigDecimal) ((Map<String, Object>) maintValue).get("intervalQuantity");
                            }
                        }
                    }
                }
                nextIntervalQty = (BigDecimal) GroovyUtil.eval("maxIntervalQty.add(productMaintValue.getBigDecimal(\"intervalQuantity\"));", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                if (UtilValidate.isNotEmpty(((Map<String, Object>) meterValue).get("meterValue"))) {
                    if (nextIntervalQty != null /* TODO: field compare operator less-equals */) {
                        maintDue = "false";
                        if (((Comparable) repeatCount).compareTo(0L) > 0) {
                            if (listSize != null /* TODO: field compare operator less */) {
                                maintDue = "true";
                            }
                        } else {
                            maintDue = "true";
                        }
                        if ("true".equals(maintDue)) {
                            // set-service-fields from "productMaintValue" to "createMaintCxt" for service "createFixedAssetMaint"
                            createMaintCxt.putAll(UtilMisc.toMap(productMaintValue));
                            createMaintCxt.put("fixedAssetId", ((Map<String, Object>) fixedAssetValue).get("fixedAssetId"));
                            createMaintCxt.put("intervalQuantity", ((Map<String, Object>) meterValue).get("meterValue"));
                            createMaintCxt.put("statusId", "FAM_CREATED");
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createFixedAssetMaint", createMaintCxt);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createFixedAssetMaint: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                                return "error";
                            }
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Create Fixed Asset Maintenances From A Product Maint Time Interval
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createMaintsFromTimeInterval(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp lastSvcDate = null;
        List<GenericValue> productMaints = null;
        Object maintDue = null;
        List<GenericValue> maintList = null;
        Integer intervalQuantity = null;
        Map<String, Object> createMaintCxt = null;
        Long listSize = null;
        Long lastSvcLong = null;
        Long repeatCount = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        List<GenericValue> fixedAssets = null;
        try {
            fixedAssets = EntityQuery.use(delegator)
                    .from("FixedAsset")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAsset: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (fixedAssets != null) {
            for (GenericValue fixedAsset : fixedAssets) {
                try {
                    productMaints = EntityQuery.use(delegator)
                            .from("ProductMaint")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProductMaint: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (productMaints != null) {
                    for (GenericValue productMaint : productMaints) {
                        repeatCount = (Long) ((Map<String, Object>) productMaint).get("repeatCount");
                        try {
                            maintList = EntityQuery.use(delegator)
                                    .from("FixedAssetMaintWorkEffort")
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying FixedAssetMaintWorkEffort: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        intervalQuantity = (Integer) ((Map<String, Object>) productMaint).get("intervalQuantity");
                        if ("TF_day".equals(((Map<String, Object>) productMaint).get("intervalUomId"))) {
                            // TODO: Convert <set-calendar> element
                        } else {
                            if ("TF_mon".equals(((Map<String, Object>) productMaint).get("intervalUomId"))) {
                                // TODO: Convert <set-calendar> element
                            } else {
                                if ("TF_yr".equals(((Map<String, Object>) productMaint).get("intervalUomId"))) {
                                    // TODO: Convert <set-calendar> element
                                }
                            }
                        }
                        if (UtilValidate.isNotEmpty(context.get("compareDate"))) {
                            listSize = 0L;
                            if (UtilValidate.isNotEmpty(maintList)) {
                                listSize = (Long) (long) (maintList != null ? ((java.util.List<?>) maintList).size() : 0);
                            }
                            lastSvcLong = 0L;
                            lastSvcDate = (lastSvcLong == 0L ? null : new Timestamp(lastSvcLong));
                            if (maintList != null) {
                                for (GenericValue maintValue : maintList) {
                                    lastSvcDate = (Timestamp) ((Map<String, Object>) maintValue).get("actualCompletionDate");
                                }
                            }
                            if (UtilValidate.isNotEmpty(lastSvcDate)) {
                                if (lastSvcDate != null /* TODO: field compare operator less */) {
                                    maintDue = "false";
                                    if (((Comparable) repeatCount).compareTo(0L) > 0) {
                                        if (listSize != null /* TODO: field compare operator less */) {
                                            maintDue = "true";
                                        }
                                    } else {
                                        maintDue = "true";
                                    }
                                    if ("true".equals(maintDue)) {
                                        // set-service-fields from "productMaint" to "createMaintCxt" for service "createFixedAssetMaint"
                                        createMaintCxt.putAll(UtilMisc.toMap(productMaint));
                                        createMaintCxt.put("fixedAssetId", ((Map<String, Object>) fixedAsset).get("fixedAssetId"));
                                        createMaintCxt.put("statusId", "FAM_CREATED");
                                        try {
                                            Map<String, Object> serviceResult = dispatcher.runSync("createFixedAssetMaint", createMaintCxt);
                                            if (ServiceUtil.isError(serviceResult)) {
                                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                                return "error";
                                            }
                                        } catch (Exception e) {
                                            Debug.logError(e, "Error calling createFixedAssetMaint: " + e.getMessage(), MODULE);
                                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                            return "error";
                                        }
                                        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                                                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                                            return "error";
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Create a FixedAsset Maintenance Order
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFixedAssetMaintOrder(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object orderId = null;
        GenericValue lookedUpValue = null;
        Object orderItemSeqId = null;
        Object orderItem = null;
        List<GenericValue> orderItems = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(lookedUpValue)) {
            orderId = context.get("orderId");
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingOrderWithIdNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("orderItemSeqId"))) {
            try {
                orderItems = EntityQuery.use(delegator)
                        .from("OrderItem")
                        .where(UtilMisc.toMap("orderId", context.get("orderId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(orderItems)) {
                orderItem = ((List<?>) orderItems).get(0);
                if (UtilValidate.isNotEmpty(orderItem)) {
                    context.put("orderItemSeqId", ((Map<String, Object>) orderItem).get("orderItemSeqId"));
                }
            }
        } else {
            try {
                lookedUpValue = EntityQuery.use(delegator)
                        .from("OrderItem")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(lookedUpValue)) {
                orderItemSeqId = context.get("orderItemSeqId");
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingOrderItemWithIdNotFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("FixedAssetMaintOrder");
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
     * Delete FixedAsset Maintenance Order
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteFixedAssetMaintOrder(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("FixedAssetMaintOrder")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetMaintOrder: " + e.getMessage(), MODULE);
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
     * Associate Party to Fixed Asset
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyFixedAssetAssignment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("PartyFixedAssetAssignment");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
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

        return "success";
    }


    /**
     * Update Party to Fixed Asset
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyFixedAssetAssignment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        try {
            newEntity = EntityQuery.use(delegator)
                    .from("PartyFixedAssetAssignment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyFixedAssetAssignment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete Party to Fixed Asset
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deletePartyFixedAssetAssignment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        try {
            newEntity = EntityQuery.use(delegator)
                    .from("PartyFixedAssetAssignment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyFixedAssetAssignment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Auto-assign Fixed Asset Parties to a Fixed Asset Maintenance
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String autoAssignFixedAssetPartiesToMaintenance(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object maintHistSeqId = null;
        Object fixedAssetId = null;
        Object workEffortId = null;
        Map<String, Object> assignPartyCtx = null;
        if (UtilValidate.isEmpty(maintHistSeqId)) {
            maintHistSeqId = context.get("maintHistSeqId");
        }
        if (UtilValidate.isEmpty(fixedAssetId)) {
            fixedAssetId = context.get("fixedAssetId");
        }
        GenericValue maintValue = null;
        try {
            maintValue = EntityQuery.use(delegator)
                    .from("FixedAssetMaint")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetMaint: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(workEffortId)) {
            workEffortId = ((Map<String, Object>) maintValue).get("scheduleWorkEffortId");
        }
        List<GenericValue> assignedParties = null;
        try {
            assignedParties = EntityQuery.use(delegator)
                    .from("PartyFixedAssetAssignAndRole")
                    .where(UtilMisc.toMap("fixedAssetId", fixedAssetId, "parentTypeId", "FAM_ASSIGNEE"))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyFixedAssetAssignAndRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (assignedParties != null) {
            for (GenericValue assignedParty : assignedParties) {
                assignPartyCtx.put("partyId", ((Map<String, Object>) assignedParty).get("partyId"));
                assignPartyCtx.put("roleTypeId", ((Map<String, Object>) assignedParty).get("roleTypeId"));
                assignPartyCtx.put("workEffortId", workEffortId);
                assignPartyCtx.put("statusId", "PRTYASGN_ASSIGNED");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("assignPartyToWorkEffort", assignPartyCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling assignPartyToWorkEffort: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Calculate straight line depreciation to Fixed Asset[ (PC-SV)/expLife ]
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String straightLineDepreciation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object depreciationYear = null;
        List<Object> assetDepreciationInfoList = null;
        Object purchaseCost = null;
        List<Object> assetDepreciationTillDate = null;
        Object numberOfYears = null;
        List<Object> assetNBVAfterDepreciation = null;
        Object assetDepreciationInfo = null;
        Object depreciation = null;
        Object depreciationTotal = null;
        GenericValue fixedAsset = null;
        Object remainingYears = null;
        Object nextDepreciationAmount = null;
        Integer expEndOfLifeYear = (Integer) context.get("expEndOfLifeYear");
        Integer assetAcquiredYear = (Integer) context.get("assetAcquiredYear");
        purchaseCost = context.get("purchaseCost");
        Object salvageValue = context.get("salvageValue");
        int intUsageYears = ((Number) context.get("usageYears")).intValue();
        depreciationTotal = new BigDecimal("0.0");
        Object assetDepreciationInfo_year = null;
        Object assetDepreciationInfo_depreciation = null;
        Object assetDepreciationInfo_depreciationTotal = null;
        Object assetDepreciationInfo_nbv = null;
        if ((((Comparable) intUsageYears).compareTo(new BigDecimal("0.0")) > 0 && !(UtilValidate.isEmpty(context.get("fixedAssetId"))))) {
            depreciation = new BigDecimal("0.0");
            numberOfYears = (new BigDecimal(expEndOfLifeYear.toString())).subtract(new BigDecimal(assetAcquiredYear.toString()));
            if (((Comparable) numberOfYears).compareTo(new BigDecimal("0.0")) > 0) {
                depreciation = ((new BigDecimal(purchaseCost.toString())).subtract(new BigDecimal(salvageValue.toString()))).divide(new BigDecimal(numberOfYears.toString()), java.math.RoundingMode.HALF_UP);
            }
            depreciationYear = assetAcquiredYear;
            // TODO: Convert <loop> element
        }
        if (UtilValidate.isEmpty(assetDepreciationTillDate)) {
            depreciation = new BigDecimal("0.0");
            assetDepreciationTillDate.add(depreciation);
            assetNBVAfterDepreciation.add(purchaseCost);
            ((Map<String, Object>) assetDepreciationInfo).put("year", assetAcquiredYear);
            ((Map<String, Object>) assetDepreciationInfo).put("depreciation", depreciation);
            ((Map<String, Object>) assetDepreciationInfo).put("depreciationTotal", depreciationTotal);
            ((Map<String, Object>) assetDepreciationInfo).put("nbv", purchaseCost);
            assetDepreciationInfoList.add(assetDepreciationInfo);
        }
        Debug.logInfo("Using straight line formula depreciation calculated for fixedAsset (" + context.get("fixedAssetId") + ") is " + depreciation, MODULE);
        result.put("assetDepreciationTillDate", assetDepreciationTillDate);
        result.put("assetNBVAfterDepreciation", assetNBVAfterDepreciation);
        result.put("assetDepreciationInfoList", assetDepreciationInfoList);
        nextDepreciationAmount = new BigDecimal("0.0");
        if (!(UtilValidate.isEmpty(context.get("fixedAssetId")))) {
            try {
                fixedAsset = EntityQuery.use(delegator)
                        .from("FixedAsset")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FixedAsset: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            remainingYears = new BigDecimal(expEndOfLifeYear.toString());
            if (((Comparable) remainingYears).compareTo(new BigDecimal("0.0")) > 0) {
                nextDepreciationAmount = (new BigDecimal(((Map<String, Object>) fixedAsset).get("purchaseCost").toString())).divide(new BigDecimal(remainingYears.toString()), java.math.RoundingMode.HALF_UP);
            }
        }
        result.put("nextDepreciationAmount", nextDepreciationAmount);
        result.put("plannedPastDepreciationTotal", depreciationTotal);

        return "success";
    }


    /**
     * Calculate double declining balance depreciation to Fixed Asset
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String doubleDecliningBalanceDepreciation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue fixedAsset = null;
        Object remainingYears = null;
        Object nextDepreciationAmount = null;
        Object depreciationYear = null;
        List<Object> assetDepreciationInfoList = null;
        Object purchaseCost = null;
        Object assetAcquiredYear = null;
        List<Object> assetDepreciationTillDate = null;
        Object numberOfYears = null;
        List<Object> assetNBVAfterDepreciation = null;
        Object assetDepreciationInfo = null;
        Object depreciation = null;
        Object depreciationTotal = null;
        Integer expEndOfLifeYear = (Integer) context.get("expEndOfLifeYear");
        assetAcquiredYear = context.get("assetAcquiredYear");
        purchaseCost = context.get("purchaseCost");
        Object salvageValue = context.get("salvageValue");
        int intUsageYears = ((Number) context.get("usageYears")).intValue();
        nextDepreciationAmount = new BigDecimal("0.0");
        if (!(UtilValidate.isEmpty(context.get("fixedAssetId")))) {
            try {
                fixedAsset = EntityQuery.use(delegator)
                        .from("FixedAsset")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying FixedAsset: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            remainingYears = new BigDecimal(expEndOfLifeYear.toString());
            if (((Comparable) remainingYears).compareTo(new BigDecimal("0.0")) > 0) {
                nextDepreciationAmount = (new BigDecimal(((Map<String, Object>) fixedAsset).get("purchaseCost").toString())).divide(new BigDecimal(remainingYears.toString()), java.math.RoundingMode.HALF_UP);
            }
        }
        result.put("nextDepreciationAmount", nextDepreciationAmount);
        depreciationTotal = new BigDecimal("0.0");
        Object assetDepreciationInfo_year = null;
        Object assetDepreciationInfo_depreciation = null;
        Object assetDepreciationInfo_depreciationTotal = null;
        Object assetDepreciationInfo_nbv = null;
        if ((((Comparable) intUsageYears).compareTo(new BigDecimal("0.0")) > 0 && !(UtilValidate.isEmpty(context.get("fixedAssetId"))))) {
            depreciationYear = assetAcquiredYear;
            // TODO: Convert <loop> element
        }
        if (UtilValidate.isEmpty(assetDepreciationTillDate)) {
            depreciation = new BigDecimal("0.0");
            assetDepreciationTillDate.add(depreciation);
            assetNBVAfterDepreciation.add(purchaseCost);
            ((Map<String, Object>) assetDepreciationInfo).put("year", assetAcquiredYear);
            ((Map<String, Object>) assetDepreciationInfo).put("depreciation", depreciation);
            ((Map<String, Object>) assetDepreciationInfo).put("depreciationTotal", depreciationTotal);
            ((Map<String, Object>) assetDepreciationInfo).put("nbv", purchaseCost);
            assetDepreciationInfoList.add(assetDepreciationInfo);
        }
        Debug.logInfo("Using double decline formula depreciation calculated for fixedAsset (" + context.get("fixedAssetId") + ") is " + assetDepreciationTillDate, MODULE);
        result.put("assetDepreciationTillDate", assetDepreciationTillDate);
        result.put("assetNBVAfterDepreciation", assetNBVAfterDepreciation);
        result.put("assetDepreciationInfoList", assetDepreciationInfoList);
        result.put("plannedPastDepreciationTotal", depreciationTotal);

        return "success";
    }


    /**
     * Service to calculate the yearly depreciation from dateAcquired year to current financial year
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String calculateFixedAssetDepreciation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object expEndOfLifeYear = null;
        Object expectedEndOfLife = null;
        String successMessageList__ = null;
        Object dateAcquired = null;
        Object assetAcquiredYear = null;
        BigDecimal salvageValue = null;
        Object assetDepreciationInfoList = null;
        Object assetDepreciationTillDate = null;
        GenericValue fixedAssetDepMethod = null;
        Object assetNBVAfterDepreciation = null;
        Object nextDepreciationAmount = null;
        Object plannedPastDepreciationTotal = null;
        GenericValue customMethod = null;
        Map<String, Object> serviceInMap = null;
        GenericValue fixedAsset = null;
        try {
            fixedAsset = EntityQuery.use(delegator)
                    .from("FixedAsset")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAsset: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(fixedAsset)) {
            {
                String errorMsg = UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingFixedAssetNotExist", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        Integer startIndex = 0;
        Integer endIndex = 4;
        if (UtilValidate.isNotEmpty(((Map<String, Object>) fixedAsset).get("expectedEndOfLife"))) {
            expectedEndOfLife = ((Map<String, Object>) fixedAsset).get("expectedEndOfLife");
            expectedEndOfLife = expectedEndOfLife != null ? expectedEndOfLife.toString() : null;
            expEndOfLifeYear = ((String) expectedEndOfLife).substring(startIndex, endIndex);
        } else {
            successMessageList__ = UtilProperties.getMessage("AccountingUiLabels", "AccountingExpEndOfLifeIsEmpty", locale);
            return "success";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) fixedAsset).get("dateAcquired"))) {
            dateAcquired = ((Map<String, Object>) fixedAsset).get("dateAcquired");
            dateAcquired = dateAcquired != null ? dateAcquired.toString() : null;
            assetAcquiredYear = ((String) dateAcquired).substring(startIndex, endIndex);
        } else {
            successMessageList__ = UtilProperties.getMessage("AccountingUiLabels", "AccountingDateAcquiredIsEmpty", locale);
            return "success";
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) fixedAsset).get("salvageValue"))) {
            salvageValue = new BigDecimal("0.0");
        } else {
            salvageValue = (BigDecimal) ((Map<String, Object>) fixedAsset).get("salvageValue");
        }
        Object nowTimestamp = new Timestamp(System.currentTimeMillis());
        nowTimestamp = nowTimestamp != null ? nowTimestamp.toString() : null;
        String currentYear = ((String) nowTimestamp).substring(startIndex, endIndex);
        Object usageYears = (new BigDecimal(currentYear.toString())).subtract(new BigDecimal(assetAcquiredYear.toString()));
        List<GenericValue> fixedAssetDepMethods = null;
        try {
            fixedAssetDepMethods = EntityQuery.use(delegator)
                    .from("FixedAssetDepMethod")
                    .where(UtilMisc.toMap("fixedAssetId", context.get("fixedAssetId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FixedAssetDepMethod: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(fixedAssetDepMethods)) {
            fixedAssetDepMethod = EntityUtil.getFirst((List<GenericValue>) fixedAssetDepMethods);
            try {
                customMethod = fixedAssetDepMethod.getRelatedOne("CustomMethod", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one CustomMethod: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Depreciation service name for the FixedAsset " + context.get("fixedAssetId") + " is " + ((Map<String, Object>) customMethod).get("customMethodName"), MODULE);
            serviceInMap.put("fixedAssetId", context.get("fixedAssetId"));
            serviceInMap.put("expEndOfLifeYear", expEndOfLifeYear);
            serviceInMap.put("assetAcquiredYear", assetAcquiredYear);
            serviceInMap.put("purchaseCost", ((Map<String, Object>) fixedAsset).get("purchaseCost"));
            serviceInMap.put("salvageValue", salvageValue);
            serviceInMap.put("usageYears", usageYears);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("${customMethod.customMethodName}", serviceInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                assetDepreciationTillDate = serviceResult.get("assetDepreciationTillDate");
                assetNBVAfterDepreciation = serviceResult.get("assetNBVAfterDepreciation");
                assetDepreciationInfoList = serviceResult.get("assetDepreciationInfoList");
                nextDepreciationAmount = serviceResult.get("nextDepreciationAmount");
                plannedPastDepreciationTotal = serviceResult.get("plannedPastDepreciationTotal");
            } catch (Exception e) {
                Debug.logError(e, "Error calling ${customMethod.customMethodName}: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Asset's depreciation calculated till date are " + assetDepreciationTillDate, MODULE);
            Debug.logInfo("Asset's Net Book Values (NBV) from acquired date after deducting depreciation are " + assetNBVAfterDepreciation, MODULE);
            result.put("assetDepreciationTillDate", assetDepreciationTillDate);
            result.put("assetNBVAfterDepreciation", assetNBVAfterDepreciation);
            result.put("assetDepreciationInfoList", assetDepreciationInfoList);
            result.put("nextDepreciationAmount", nextDepreciationAmount);
            result.put("plannedPastDepreciationTotal", plannedPastDepreciationTotal);
        } else {
            successMessageList__ = UtilProperties.getMessage("AccountingUiLabels", "AccountingFixedAssetDepreciationMethodNotFound", locale);
            return "success";
        }

        return "success";
    }


    /**
     * If the accounting transaction is a depreciation transaction for a fixed asset, update the depreciation amount in the FixedAsset entity.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkUpdateFixedAssetDepreciation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> creditTransactions = null;
        Map<String, Object> creditCondition = null;
        GenericValue fixedAsset = null;
        Object depreciation = null;
        Object depreciationTotal = null;
        GenericValue acctgTrans = null;
        try {
            acctgTrans = EntityQuery.use(delegator)
                    .from("AcctgTrans")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object creditCondition_debitCreditFlag = null;
        Object fixedAsset_purchaseCostUomId = null;
        Object fixedAsset_depreciation = null;
        if (("DEPRECIATION".equals(((Map<String, Object>) acctgTrans).get("acctgTransTypeId")) && !(UtilValidate.isEmpty(((Map<String, Object>) acctgTrans).get("fixedAssetId"))))) {
            try {
                fixedAsset = acctgTrans.getRelatedOne("FixedAsset", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one FixedAsset: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            creditCondition.put("debitCreditFlag", "C");
            try {
                creditTransactions = acctgTrans.getRelated("AcctgTransEntry", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related AcctgTransEntry: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            depreciation = new BigDecimal("0.0");
            if (creditTransactions != null) {
                for (GenericValue creditTransaction : creditTransactions) {
                    if (UtilValidate.isEmpty(((Map<String, Object>) fixedAsset).get("purchaseCostUomId"))) {
                        Debug.logWarning("Found empty purchaseCostUomId for FixedAsset [" + ((Map<String, Object>) fixedAsset).get("fixedAssetId") + "]: setting it to " + ((Map<String, Object>) creditTransaction).get("currencyUomId") + " to match the one used in the gl.", MODULE);
                        fixedAsset.put("purchaseCostUomId", ((Map<String, Object>) creditTransaction).get("currencyUomId"));
                        try {
                            delegator.store(fixedAsset);
                        } catch (Exception e) {
                            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                    if (java.util.Objects.equals(((Map<String, Object>) fixedAsset).get("purchaseCostUomId"), ((Map<String, Object>) creditTransaction).get("currencyUomId"))) {
                        depreciation = (new BigDecimal(depreciation.toString())).add(new BigDecimal(((Map<String, Object>) creditTransaction).get("amount").toString()));
                    } else {
                        if (java.util.Objects.equals(((Map<String, Object>) fixedAsset).get("purchaseCostUomId"), ((Map<String, Object>) creditTransaction).get("origCurrencyUomId"))) {
                            depreciation = (new BigDecimal(depreciation.toString())).add(new BigDecimal(((Map<String, Object>) creditTransaction).get("origAmount").toString()));
                        } else {
                            Debug.logWarning("Found an accounting transaction for depreciation of FixedAsset [" + ((Map<String, Object>) fixedAsset).get("fixedAssetId") + "] with a cuurency that doesn't match the currency used in the fixed asset: the depreciation total in the fixed asset will not be updated.", MODULE);
                            return "success";
                        }
                    }
                }
            }
            depreciationTotal = ((Map<String, Object>) fixedAsset).get("depreciation");
            depreciationTotal = (new BigDecimal(depreciation.toString())).add(new BigDecimal(depreciationTotal.toString()));
            fixedAsset.put("depreciation", depreciationTotal);
            try {
                delegator.store(fixedAsset);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create a Fixed Asset Type Gl Account Mapping
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFixedAssetTypeGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        newEntity = delegator.makeValue("FixedAssetTypeGlAccount");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fixedAssetId"))) {
            newEntity.put("fixedAssetId", "_NA_");
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fixedAssetTypeId"))) {
            newEntity.put("fixedAssetTypeId", "_NA_");
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

}

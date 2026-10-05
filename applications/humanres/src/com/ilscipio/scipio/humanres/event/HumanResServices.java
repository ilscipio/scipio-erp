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
package com.ilscipio.scipio.humanres.event;

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
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://humanres/script/org/ofbiz/humanres/HumanResServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class HumanResServices {

    private static final String MODULE = HumanResServices.class.getName();


    /**
     * Create Party Skills
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartySkill(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = delegator.makeValue("PartySkill");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        GenericValue partySkill = null;
        try {
            partySkill = EntityQuery.use(delegator)
                    .from("PartySkill")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartySkill: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ((java.util.Objects.equals(((Map<String, Object>) partySkill).get("partyId"), context.get("partyId")) && java.util.Objects.equals(((Map<String, Object>) partySkill).get("skillTypeId"), context.get("skillTypeId")))) {
            {
                String errorMsg = UtilProperties.getMessage("HumanResUiLabels", "HumanResPartySkillsAlreadyExists", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        } else {
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
     * Create Employment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createEmployment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        Timestamp nowTimeStamp = null;
        newEntity = delegator.makeValue("Employment");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fromDate"))) {
            nowTimeStamp = new Timestamp(System.currentTimeMillis());
            newEntity.put("fromDate", nowTimeStamp);
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createPayHistoryMap = new HashMap<>();
        // set-service-fields from "newEntity" to "createPayHistoryMap" for service "createPayHistory"
        createPayHistoryMap.putAll(UtilMisc.toMap(newEntity));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPayHistory", createPayHistoryMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPayHistory: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete Pay History
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deletePayHistory(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp nowTimeStamp = new Timestamp(System.currentTimeMillis());
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PayHistory")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PayHistory: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.put("thruDate", nowTimeStamp);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
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
     * Create a Employee Position Reporting Structure
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createEmplPositionReportingStruct(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue newEntity = null;
        Timestamp nowTimeStamp = null;
        newEntity = delegator.makeValue("EmplPositionReportingStruct");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fromDate"))) {
            nowTimeStamp = new Timestamp(System.currentTimeMillis());
            newEntity.put("fromDate", nowTimeStamp);
        }
        if (!java.util.Objects.equals(context.get("emplPositionIdManagedBy"), context.get("emplPositionIdReportingTo"))) {
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            {
                String errorMsg = UtilProperties.getMessage("HumanResUiLabels", "HumanResEmplPostitionIdReportingToAndEmplPositionIdManagedByMustBeDiff", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create New Employee
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createEmployee(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> partyRelationshipCtx = null;
        Timestamp nowTimestamp = null;
        context.put("roleTypeId", "EMPLOYEE");
        // TODO: Call simple-method "createPersonRoleAndContactMechs" from "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml"
        // Original: call-simple-method method-name="createPersonRoleAndContactMechs" xml-resource="component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml"
        if (UtilValidate.isNotEmpty(context.get("partyIdFrom"))) {
            partyRelationshipCtx.put("partyId", context.get("partyId"));
            partyRelationshipCtx.put("partyIdFrom", context.get("partyIdFrom"));
            partyRelationshipCtx.put("partyIdTo", context.get("partyId"));
            partyRelationshipCtx.put("roleTypeIdFrom", "INTERNAL_ORGANIZATIO");
            partyRelationshipCtx.put("roleTypeIdTo", "EMPLOYEE");
            partyRelationshipCtx.put("relationshipName", "EMPLOYMENT");
            partyRelationshipCtx.put("fromDate", context.get("fromDate"));
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            if (UtilValidate.isEmpty(((Map<String, Object>) partyRelationshipCtx).get("fromDate"))) {
                partyRelationshipCtx.put("fromDate", nowTimestamp);
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyRelationship", partyRelationshipCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyRelationship: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("partyId", context.get("partyId"));
        String successMessageList__ = UtilProperties.getMessage("PartyUiLabels", "PartyUserCreated", locale);

        return "success";
    }


    /**
     * Update/create EmplPositionTypeRate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateEmplPositionTypeRate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue emplPositionTypeRate = null;
        Map<String, Object> updRate = null;
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("EmplPositionTypeRate")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying EmplPositionTypeRate: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> emplPositionTypeRates = null;
        try {
            emplPositionTypeRates = EntityQuery.use(delegator)
                    .from("EmplPositionTypeRate")
                    .where(UtilMisc.toMap("emplPositionTypeId", context.get("emplPositionTypeId"), "rateTypeId", context.get("rateTypeId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying EmplPositionTypeRate: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(emplPositionTypeRates)) {
            emplPositionTypeRate = EntityUtil.getFirst((List<GenericValue>) emplPositionTypeRates);
            Timestamp emplPositionTypeRate_thruDate = new Timestamp(System.currentTimeMillis());
            try {
                delegator.store(emplPositionTypeRate);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        GenericValue newEntity = delegator.makeValue("EmplPositionTypeRate");
        newEntity.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
        }
        newEntity.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(context.get("rateAmount"))) {
            // set-service-fields from "parameters" to "updRate" for service "updateRateAmount"
            updRate.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateRateAmount", updRate);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateRateAmount: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Delete EmplPositionTypeRate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteEmplPositionTypeRate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("EmplPositionTypeRate")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying EmplPositionTypeRate: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Timestamp lookedUpValue_thruDate = new Timestamp(System.currentTimeMillis());
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> delRateAmount = new HashMap<>();
        // set-service-fields from "parameters" to "delRateAmount" for service "expireRateAmount"
        delRateAmount.putAll(UtilMisc.toMap(context));
        delRateAmount.put("fromDate", context.get("rateAmountFromDate"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("expireRateAmount", delRateAmount);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling expireRateAmount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create Employee Leave
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createEmplLeave(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("EmplLeave");
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
        String successMessage = UtilProperties.getMessage("HumanResUiLabels", "HumanResLeaveCreationSuccess", locale);

        return "success";
    }


    /**
     * Get all current employment information for a certain partyId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getCurrentPartyEmploymentData(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue emplPositionType = null;
        List<GenericValue> partyBenefitTypes = null;
        try {
            partyBenefitTypes = EntityQuery.use(delegator)
                    .from("BenefitTypeAndParty")
                    .where(UtilMisc.toMap("partyIdTo", context.get("partyId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying BenefitTypeAndParty: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("partyBenefitTypes", partyBenefitTypes);
        List<GenericValue> employments = null;
        try {
            employments = EntityQuery.use(delegator)
                    .from("Employment")
                    .where(UtilMisc.toMap("partyIdTo", context.get("partyId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Employment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue employment = EntityUtil.getFirst((List<GenericValue>) employments);
        result.put("employment", employment);
        List<GenericValue> emplPositionAndFulfillments = null;
        try {
            emplPositionAndFulfillments = EntityQuery.use(delegator)
                    .from("EmplPositionAndFulfillment")
                    .where(UtilMisc.toMap("employeePartyId", context.get("partyId"), "partyId", ((Map<String, Object>) employment).get("partyIdFrom")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying EmplPositionAndFulfillment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue emplPositionAndFulfillment = EntityUtil.getFirst((List<GenericValue>) emplPositionAndFulfillments);
        result.put("emplPosition", emplPositionAndFulfillment);
        if (UtilValidate.isNotEmpty(emplPositionAndFulfillment)) {
            try {
                emplPositionType = emplPositionAndFulfillment.getRelatedOne("EmplPositionType", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one EmplPositionType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("emplPositionType", emplPositionType);
        }
        GenericValue partyAcctgPreference = null;
        try {
            partyAcctgPreference = EntityQuery.use(delegator)
                    .from("PartyAcctgPreference")
                    .where(UtilMisc.toMap("partyId", ((Map<String, Object>) employment).get("partyIdFrom")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyAcctgPreference: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> rateAmounts = null;
        try {
            rateAmounts = EntityQuery.use(delegator)
                    .from("RateAmount")
                    .where(UtilMisc.toMap("emplPositionTypeId", ((Map<String, Object>) emplPositionType).get("emplPositionTypeId"), "rateCurrencyUomId", ((Map<String, Object>) partyAcctgPreference).get("baseCurrencyUomId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying RateAmount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue rateAmount = EntityUtil.getFirst((List<GenericValue>) rateAmounts);
        result.put("emplPositionRateAmount", rateAmount);

        return "success";
    }


    /**
     * Apply Training
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String applyTraining(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue trainingRequest = delegator.makeValue("TrainingRequest");
        delegator.setNextSubSeqId(trainingRequest, "trainingRequestId", 5, 1);
        Object trainingRequestId = trainingRequest.get("trainingRequestId");
        try {
            delegator.create(trainingRequest);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue personTraining = delegator.makeValue("PersonTraining");
        personTraining.setPKFields((Map<String, Object>) context);
        personTraining.setNonPKFields((Map<String, Object>) context);
        personTraining.put("trainingRequestId", ((Map<String, Object>) trainingRequest).get("trainingRequestId"));
        personTraining.put("fromDate", context.get("fromDate"));
        personTraining.put("thruDate", context.get("thruDate"));
        try {
            delegator.create(personTraining);
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
     * Assign Training
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String assignTraining(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue trainingRequest = delegator.makeValue("TrainingRequest");
        delegator.setNextSubSeqId(trainingRequest, "trainingRequestId", 5, 1);
        Object trainingRequestId = trainingRequest.get("trainingRequestId");
        try {
            delegator.create(trainingRequest);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue personTraining = delegator.makeValue("PersonTraining");
        personTraining.setPKFields((Map<String, Object>) context);
        personTraining.setNonPKFields((Map<String, Object>) context);
        personTraining.put("trainingRequestId", ((Map<String, Object>) trainingRequest).get("trainingRequestId"));
        personTraining.put("fromDate", context.get("fromDate"));
        personTraining.put("thruDate", context.get("thruDate"));
        try {
            delegator.create(personTraining);
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
     * Create a Salary Step
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSalaryStep(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("SalaryStep");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.put("createdByUserLogin", ((Map<String, Object>) context.get("userLogin")).get("userLoginId"));
        ((GenericValue) newEntity).put("salaryStepSeqId", delegator.getNextSeqId("SalaryStep"));
        result.put("salaryStepSeqId", ((Map<String, Object>) newEntity).get("salaryStepSeqId"));
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
     * Update Salary Step
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSalaryStep(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SalaryStep")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SalaryStep: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        lookedUpValue.put("lastModifiedByUserLogin", ((Map<String, Object>) context.get("userLogin")).get("userLoginId"));
        // TODO: Convert <now> element
        lookedUpValue.put("dateModified", context.get("fromDate"));
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
     * SCIPIO: Create Employment App
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createEmploymentApp(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        String inlineResult = validateEmploymentAppParams(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue emplAppValue = delegator.makeValue("EmploymentApp");
        emplAppValue.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(context.get("applicationId"))) {
            delegator.setNextSubSeqId(emplAppValue, "applicationId", 5, 1);
            Object applicationId = emplAppValue.get("applicationId");
        }
        emplAppValue.setNonPKFields((Map<String, Object>) context);
        inlineResult = validateEmploymentApp(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.create(emplAppValue);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        result.put("applicationId", ((Map<String, Object>) emplAppValue).get("applicationId"));

        return "success";
    }


    /**
     * SCIPIO: Update Employment App
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateEmploymentApp(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        result.put("applicationId", context.get("applicationId"));
        String inlineResult = validateEmploymentAppParams(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue emplAppValue = null;
        try {
            emplAppValue = EntityQuery.use(delegator)
                    .from("EmploymentApp")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying EmploymentApp: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        emplAppValue.setNonPKFields((Map<String, Object>) context);
        inlineResult = validateEmploymentApp(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.store(emplAppValue);
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
     * SCIPIO: Validate Employment App Params
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String validateEmploymentAppParams(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue emplPosValue = null;
        GenericValue jobReqValue = null;
        if (UtilValidate.isNotEmpty(context.get("emplPositionId"))) {
            try {
                emplPosValue = EntityQuery.use(delegator)
                        .from("EmplPosition")
                        .where(UtilMisc.toMap("emplPositionId", context.get("emplPositionId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying EmplPosition: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(emplPosValue)) {
                {
                    String errorMsg = UtilProperties.getMessage("HumanResErrorUiLabels", "HumanResErrorInvalidEmplPosition", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("jobRequisitionId"))) {
            try {
                jobReqValue = EntityQuery.use(delegator)
                        .from("JobRequisition")
                        .where(UtilMisc.toMap("jobRequisitionId", context.get("jobRequisitionId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying JobRequisition: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(jobReqValue)) {
                {
                    String errorMsg = UtilProperties.getMessage("HumanResErrorUiLabels", "HumanResErrorInvalidJobRequisition", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }

        return "success";
    }


    /**
     * SCIPIO: Validate Employment App
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String validateEmploymentApp(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Map<String, Object> emplAppValue = null;
        GenericValue emplPosValue = null;
        GenericValue jobReqValue = null;
        if (UtilValidate.isEmpty(((Map<String, Object>) emplAppValue).get("emplPositionId"))) {
            if (UtilValidate.isEmpty(((Map<String, Object>) emplAppValue).get("jobRequisitionId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("HumanResErrorUiLabels", "HumanResErrorPositionOrRequisitionMustBeSpecified", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            } else {
                try {
                    jobReqValue = EntityQuery.use(delegator)
                            .from("JobRequisition")
                            .where(UtilMisc.toMap("jobRequisitionId", ((Map<String, Object>) emplAppValue).get("jobRequisitionId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying JobRequisition: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
                if (UtilValidate.isEmpty(jobReqValue)) {
                    {
                        String errorMsg = UtilProperties.getMessage("HumanResErrorUiLabels", "HumanResErrorInvalidJobRequisition", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                    if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                            UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                        return "error";
                    }
                } else {
                    if (UtilValidate.isNotEmpty(((Map<String, Object>) jobReqValue).get("emplPositionId"))) {
                        emplAppValue.put("emplPositionId", ((Map<String, Object>) jobReqValue).get("emplPositionId"));
                    }
                    if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                            UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                        return "error";
                    }
                }
            }
        } else {
            try {
                emplPosValue = EntityQuery.use(delegator)
                        .from("EmplPosition")
                        .where(UtilMisc.toMap("emplPositionId", ((Map<String, Object>) emplAppValue).get("emplPositionId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying EmplPosition: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            if (UtilValidate.isEmpty(emplPosValue)) {
                {
                    String errorMsg = UtilProperties.getMessage("HumanResErrorUiLabels", "HumanResErrorInvalidEmplPosition", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            } else {
                if (UtilValidate.isNotEmpty(((Map<String, Object>) emplAppValue).get("jobRequisitionId"))) {
                    try {
                        jobReqValue = EntityQuery.use(delegator)
                                .from("JobRequisition")
                                .where(UtilMisc.toMap("jobRequisitionId", ((Map<String, Object>) emplAppValue).get("jobRequisitionId")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying JobRequisition: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                            UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                        return "error";
                    }
                    if (UtilValidate.isEmpty(jobReqValue)) {
                        {
                            String errorMsg = UtilProperties.getMessage("HumanResErrorUiLabels", "HumanResErrorInvalidJobRequisition", locale);
                            error_list.add(errorMsg);
                            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                        }
                        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                            return "error";
                        }
                    } else {
                        if (UtilValidate.isNotEmpty(((Map<String, Object>) jobReqValue).get("emplPositionId"))) {
                            if (!java.util.Objects.equals(((Map<String, Object>) emplAppValue).get("emplPositionId"), ((Map<String, Object>) jobReqValue).get("emplPositionId"))) {
                                {
                                    String errorMsg = UtilProperties.getMessage("HumanResErrorUiLabels", "HumanResErrorAppPositionMustEqualRequisitionPosition", locale);
                                    error_list.add(errorMsg);
                                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                                }
                                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                                    return "error";
                                }
                            }
                        } else {
                            Debug.logWarning("WARNING: Job requisition " + ((Map<String, Object>) emplAppValue).get("jobRequisitionId") + " is not linked to an employee position;                                 this is allowed by stock ofbiz, but in most cases you want to a link to a position (emplPositionId)", MODULE);
                        }
                    }
                }
            }
        }

        return "success";
    }

}

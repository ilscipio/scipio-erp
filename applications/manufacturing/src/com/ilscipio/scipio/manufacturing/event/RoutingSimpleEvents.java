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
/*
 * SCIPIO: Hand-written replacement for the RoutingSimpleEvents.xml / RoutingMapProcs.xml simple-methods.
 */
package com.ilscipio.scipio.manufacturing.event;

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GeneralException;
import org.ofbiz.base.util.ObjectType;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;
import org.ofbiz.service.ServiceUtil;

/**
 * Manufacturing routing task association request events (addRoutingTaskAssoc / updateRoutingTaskAssoc).
 *
 * <p>SCIPIO: Hand-written replacement for RoutingSimpleEvents.xml, inlining the RoutingMapProcs.xml map
 * processors ({@link #copyRoutingTask(GenericValue)}, {@link #routingTaskAssoc(Map, List, Locale)}) as
 * private helpers.</p>
 */
public class RoutingSimpleEvents {

    private static final String MODULE = RoutingSimpleEvents.class.getName();

    /**
     * If copyTask = "Y" creates a copy of the routing task (WorkEffort), then in all cases adds a
     * RoutingTaskAssociation (WorkEffortAssoc) between the routing and the task.
     */
    public static String addRoutingTaskAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> params = UtilGenerics.cast(UtilHttp.getCombinedMap(request));

        Map<String, Object> result = addRoutingTaskAssoc(delegator, dispatcher, userLogin, params, locale);
        return handleResult(request, result);
    }

    /**
     * Checks there is no clash with the date and SeqId of another routing task assoc, then calls
     * updateWorkEffortAssoc.
     */
    public static String updateRoutingTaskAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> params = UtilGenerics.cast(UtilHttp.getCombinedMap(request));

        Map<String, Object> result = updateRoutingTaskAssoc(delegator, dispatcher, userLogin, params, locale);
        return handleResult(request, result);
    }

    /**
     * Public core logic of {@link #addRoutingTaskAssoc(HttpServletRequest, HttpServletResponse)},
     * callable directly (e.g. from a test) without a servlet request.
     */
    public static Map<String, Object> addRoutingTaskAssoc(Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin,
            Map<String, Object> params, Locale locale) {
        Map<String, Object> workingParams = new HashMap<>(params);
        if ("Y".equals(workingParams.get("copyTask"))) {
            GenericValue lookedUpValue;
            try {
                lookedUpValue = EntityQuery.use(delegator).from("WorkEffort")
                        .where("workEffortId", workingParams.get("workEffortIdTo"))
                        .queryOne();
            } catch (GenericEntityException e) {
                Debug.logError(e, "Error finding WorkEffort to copy: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            Map<String, Object> copyContext = copyRoutingTask(lookedUpValue);
            copyContext.put("userLogin", userLogin);
            Map<String, Object> createResult;
            try {
                createResult = dispatcher.runSync("createWorkEffort", copyContext);
            } catch (GenericServiceException e) {
                Debug.logError(e, "Error calling createWorkEffort: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (ServiceUtil.isError(createResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(createResult));
            }
            workingParams.put("workEffortIdTo", createResult.get("workEffortId"));
        }
        if (UtilValidate.isEmpty(workingParams.get("fromDate"))) {
            workingParams.put("fromDate", new Timestamp(System.currentTimeMillis()));
        }

        List<String> errorMessages = new LinkedList<>();
        Map<String, Object> context1 = routingTaskAssoc(workingParams, errorMessages, locale);
        if (!errorMessages.isEmpty()) {
            return ServiceUtil.returnError(errorMessages);
        }
        context1.put("create", "Y");
        context1.put("userLogin", userLogin);
        Map<String, Object> checkResult;
        try {
            checkResult = dispatcher.runSync("checkRoutingTaskAssoc", context1);
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling checkRoutingTaskAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (ServiceUtil.isError(checkResult)) {
            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(checkResult));
        }
        if ("Y".equals(checkResult.get("sequenceNumNotOk"))) {
            errorMessages.add(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingTwoRoutingTaskWithSameSeqId", locale));
            return ServiceUtil.returnError(errorMessages);
        }

        Map<String, Object> context2 = routingTaskAssoc(workingParams, errorMessages, locale);
        if (!errorMessages.isEmpty()) {
            return ServiceUtil.returnError(errorMessages);
        }
        context2.put("userLogin", userLogin);
        Map<String, Object> createAssocResult;
        try {
            createAssocResult = dispatcher.runSync("createWorkEffortAssoc", context2);
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling createWorkEffortAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (ServiceUtil.isError(createAssocResult)) {
            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(createAssocResult));
        }
        return ServiceUtil.returnSuccess(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingRoutingTaskAssocCreateSuccessfully", locale));
    }

    /**
     * Public core logic of {@link #updateRoutingTaskAssoc(HttpServletRequest, HttpServletResponse)},
     * callable directly (e.g. from a test) without a servlet request.
     */
    public static Map<String, Object> updateRoutingTaskAssoc(Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin,
            Map<String, Object> params, Locale locale) {
        List<String> errorMessages = new LinkedList<>();
        Map<String, Object> context1 = routingTaskAssoc(params, errorMessages, locale);
        if (!errorMessages.isEmpty()) {
            return ServiceUtil.returnError(errorMessages);
        }
        context1.put("userLogin", userLogin);
        Map<String, Object> checkResult;
        try {
            checkResult = dispatcher.runSync("checkRoutingTaskAssoc", context1);
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling checkRoutingTaskAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (ServiceUtil.isError(checkResult)) {
            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(checkResult));
        }
        if ("Y".equals(checkResult.get("sequenceNumNotOk"))) {
            errorMessages.add(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingTwoRoutingTaskWithSameSeqId", locale));
            return ServiceUtil.returnError(errorMessages);
        }

        Map<String, Object> context2 = routingTaskAssoc(params, errorMessages, locale);
        if (!errorMessages.isEmpty()) {
            return ServiceUtil.returnError(errorMessages);
        }
        context2.put("userLogin", userLogin);
        Map<String, Object> updateResult;
        try {
            updateResult = dispatcher.runSync("updateWorkEffortAssoc", context2);
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling updateWorkEffortAssoc: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (ServiceUtil.isError(updateResult)) {
            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(updateResult));
        }
        return ServiceUtil.returnSuccess(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingRoutingTaskAssocCreateSuccessfully", locale));
    }

    /**
     * RoutingMapProcs.xml "copyRoutingTask" map processor: copies the fields needed to create a WorkEffort
     * copy of a routing task from the looked-up source WorkEffort.
     */
    private static Map<String, Object> copyRoutingTask(GenericValue lookedUpValue) {
        Map<String, Object> context = new HashMap<>();
        context.put("workEffortName", lookedUpValue.get("workEffortName"));
        context.put("description", lookedUpValue.get("description"));
        context.put("workEffortPurposeTypeId", lookedUpValue.get("workEffortPurposeTypeId"));
        context.put("fixedAssetId", lookedUpValue.get("fixedAssetId"));
        context.put("estimatedSetupMillis", lookedUpValue.get("estimatedSetupMillis"));
        context.put("estimatedMilliSeconds", lookedUpValue.get("estimatedMilliSeconds"));
        context.put("workEffortTypeId", lookedUpValue.get("workEffortTypeId"));
        context.put("currentStatusId", lookedUpValue.get("currentStatusId"));
        return context;
    }

    /**
     * RoutingMapProcs.xml "routingTaskAssoc" map processor: builds the WorkEffortAssoc in-map from the request
     * params, converting sequenceNum to Long and fromDate/thruDate to Timestamp, appending a localized message
     * to {@code errorMessages} for each missing or unparsable field (same label keys as the original XML).
     */
    private static Map<String, Object> routingTaskAssoc(Map<String, Object> params, List<String> errorMessages, Locale locale) {
        Map<String, Object> context = new HashMap<>();
        context.put("workEffortIdFrom", params.get("workEffortId"));

        Object workEffortIdTo = params.get("workEffortIdTo");
        context.put("workEffortIdTo", workEffortIdTo);
        if (UtilValidate.isEmpty(workEffortIdTo)) {
            errorMessages.add(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingRoutingTaskToMissing", locale));
        }

        context.put("workEffortAssocTypeId", params.get("workEffortAssocTypeId"));

        Object sequenceNum = params.get("sequenceNum");
        try {
            sequenceNum = ObjectType.simpleTypeConvert(sequenceNum, "Long", null, locale, false);
        } catch (GeneralException e) {
            errorMessages.add(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingRoutingSeqIdFormatNotCorrect", locale));
            sequenceNum = null;
        }
        context.put("sequenceNum", sequenceNum);
        if (UtilValidate.isEmpty(sequenceNum)) {
            errorMessages.add(UtilProperties.getMessage("ManufacturingUiLabels", "ManufacturingRoutingSeqIdMissing", locale));
        }

        Object fromDate = params.get("fromDate");
        try {
            fromDate = ObjectType.simpleTypeConvert(fromDate, "Timestamp", null, locale, false);
        } catch (GeneralException e) {
            errorMessages.add(UtilProperties.getMessage("ManufacturingUiLabels", "CommonFormatDateFieldNotCorrect", locale));
            fromDate = null;
        }
        context.put("fromDate", fromDate);

        Object thruDate = params.get("thruDate");
        try {
            thruDate = ObjectType.simpleTypeConvert(thruDate, "Timestamp", null, locale, false);
        } catch (GeneralException e) {
            errorMessages.add(UtilProperties.getMessage("ManufacturingUiLabels", "CommonFormatDateFieldNotCorrect", locale));
            thruDate = null;
        }
        context.put("thruDate", thruDate);

        return context;
    }

    /** Translates a service-style result map into the request error/success attributes and the event response. */
    private static String handleResult(HttpServletRequest request, Map<String, Object> result) {
        if (ServiceUtil.isError(result)) {
            List<Object> errorMessageList = UtilGenerics.cast(result.get(ModelService.ERROR_MESSAGE_LIST));
            if (UtilValidate.isNotEmpty(errorMessageList)) {
                request.setAttribute("_ERROR_MESSAGE_LIST_", errorMessageList);
            } else {
                request.setAttribute("_ERROR_MESSAGE_", ServiceUtil.getErrorMessage(result));
            }
            return "error";
        }
        String successMessage = (String) result.get(ModelService.SUCCESS_MESSAGE);
        if (UtilValidate.isNotEmpty(successMessage)) {
            request.setAttribute("_EVENT_MESSAGE_", successMessage);
        }
        return "success";
    }

    private RoutingSimpleEvents() {}
}

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
package com.ilscipio.scipio.manufacturing.event;

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
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * SCIPIO: Hand-written replacement for the ProductionRunSimpleEvents.xml simple-methods
 * (createProductionRun, addProductionRunRoutingTask, editProductionRunRoutingTask).
 */
public class ProductionRunSimpleEvents {

    private static final String MODULE = ProductionRunSimpleEvents.class.getName();
    private static final String RESOURCE = "ManufacturingUiLabels";

    // ==================== Events ====================

    /** Based on selected options, forwards to one of the specialized createProductionRun* requests. */
    public static String createProductionRun(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> params = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = createProductionRun(delegator, dispatcher, userLogin, params, locale);
        if (result.get("pRQuantity") != null) {
            request.setAttribute("pRQuantity", result.get("pRQuantity"));
        }
        return (String) result.get("responseCode");
    }

    /** Checks parameters and adds a routing task to an existing production run. */
    public static String addProductionRunRoutingTask(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> params = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = addProductionRunRoutingTask(delegator, dispatcher, userLogin, params, locale);
        return applyMessages(request, result);
    }

    /** Checks parameters and edits an existing production run routing task. */
    public static String editProductionRunRoutingTask(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> params = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = editProductionRunRoutingTask(delegator, dispatcher, userLogin, params, locale);
        return applyMessages(request, result);
    }

    private static String applyMessages(HttpServletRequest request, Map<String, Object> result) {
        if (result.get("errorMessageList") != null) {
            request.setAttribute("_ERROR_MESSAGE_LIST_", result.get("errorMessageList"));
        }
        if (result.get("errorMessage") != null) {
            request.setAttribute("_ERROR_MESSAGE_", result.get("errorMessage"));
        }
        return (String) result.get("responseCode");
    }

    // ==================== Logic (package-visible, servlet-free) ====================

    /**
     * Based on selected options, returns the response code for one of the specialized services to
     * create a production run ("createProductionRunsForProductBom" or "createProductionRunSingle").
     */
    public static Map<String, Object> createProductionRun(Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin,
            Map<String, Object> params, Locale locale) {
        Map<String, Object> result = new HashMap<>();
        if ("Y".equals(params.get("createDependentProductionRuns"))) {
            result.put("responseCode", "createProductionRunsForProductBom");
            return result;
        }
        BigDecimal pRQuantity = toBigDecimal(params.get("quantity"));
        result.put("pRQuantity", pRQuantity);
        result.put("responseCode", "createProductionRunSingle");
        return result;
    }

    /** Validates parameters (simple-map-processor "prepareAddRoutingTask") and calls addProductionRunRoutingTask. */
    public static Map<String, Object> addProductionRunRoutingTask(Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin,
            Map<String, Object> params, Locale locale) {
        List<String> errorMessages = new LinkedList<>();
        Map<String, Object> context = new HashMap<>();
        context.put("userLogin", userLogin);

        context.put("productionRunId", params.get("productionRunId"));

        Object routingTaskId = params.get("routingTaskId");
        if (UtilValidate.isEmpty(routingTaskId)) {
            errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingRoutingTaskIdMissing", locale));
        }
        context.put("routingTaskId", routingTaskId);

        Object priority = params.get("priority");
        if (UtilValidate.isEmpty(priority)) {
            errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunPriorityMissing", locale));
        } else {
            try {
                context.put("priority", toLong(priority));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingRoutingSeqIdFormatNotCorrect", locale));
            }
        }
        if (UtilValidate.isNotEmpty(params.get("estimatedStartDate"))) {
            try {
                context.put("estimatedStartDate", toTimestamp(params.get("estimatedStartDate")));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunStartDateNotCorrect", locale));
            }
        }
        if (UtilValidate.isNotEmpty(params.get("estimatedCompletionDate"))) {
            try {
                context.put("estimatedCompletionDate", toTimestamp(params.get("estimatedCompletionDate")));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunCompletionDateNotCorrect", locale));
            }
        }
        if (UtilValidate.isNotEmpty(params.get("estimatedSetupMillis"))) {
            try {
                context.put("estimatedSetupMillis", toBigDecimal(params.get("estimatedSetupMillis")));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunQuantityNotCorrect", locale));
            }
        }
        if (UtilValidate.isNotEmpty(params.get("estimatedMilliSeconds"))) {
            try {
                context.put("estimatedMilliSeconds", toBigDecimal(params.get("estimatedMilliSeconds")));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunQuantityNotCorrect", locale));
            }
        }
        context.put("workEffortName", params.get("workEffortName"));
        context.put("description", params.get("description"));

        Map<String, Object> result = new HashMap<>();
        if (!errorMessages.isEmpty()) {
            result.put("responseCode", "error");
            result.put("errorMessageList", errorMessages);
            return result;
        }

        try {
            Map<String, Object> serviceResult = dispatcher.runSync("addProductionRunRoutingTask", context);
            if (ServiceUtil.isError(serviceResult)) {
                result.put("responseCode", "error");
                result.put("errorMessage", ServiceUtil.getErrorMessage(serviceResult));
                return result;
            }
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling addProductionRunRoutingTask: " + e.getMessage(), MODULE);
            result.put("responseCode", "error");
            result.put("errorMessage", e.getMessage());
            return result;
        }

        result.put("responseCode", "success");
        return result;
    }

    /**
     * Validates parameters (simple-map-processors "prCheckUpdatePrunRoutingTask" and
     * "prepareUpdateRoutingTask"), calls checkUpdatePrunRoutingTask then updateWorkEffort.
     */
    public static Map<String, Object> editProductionRunRoutingTask(Delegator delegator, LocalDispatcher dispatcher, GenericValue userLogin,
            Map<String, Object> params, Locale locale) {
        List<String> errorMessages = new LinkedList<>();
        Map<String, Object> context = new HashMap<>();
        context.put("userLogin", userLogin);

        context.put("productionRunId", params.get("productionRunId"));
        context.put("routingTaskId", params.get("routingTaskId"));
        if (UtilValidate.isNotEmpty(params.get("priority"))) {
            try {
                context.put("priority", toLong(params.get("priority")));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingRoutingSeqIdFormatNotCorrect", locale));
            }
        }
        Object startDate = params.get("estimatedStartDate");
        if (UtilValidate.isEmpty(startDate)) {
            errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunStartDateMissing", locale));
        } else {
            try {
                context.put("estimatedStartDate", toTimestamp(startDate));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunStartDateNotCorrect", locale));
            }
        }
        if (UtilValidate.isNotEmpty(params.get("estimatedSetupMillis"))) {
            try {
                context.put("estimatedSetupMillis", toBigDecimal(params.get("estimatedSetupMillis")));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunEstimatedSetupMillisNotCorrect", locale));
            }
        }
        if (UtilValidate.isNotEmpty(params.get("estimatedMilliSeconds"))) {
            try {
                context.put("estimatedMilliSeconds", toBigDecimal(params.get("estimatedMilliSeconds")));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunEstimatedMilliSecondsNotCorrect", locale));
            }
        }

        Map<String, Object> result = new HashMap<>();
        if (!errorMessages.isEmpty()) {
            result.put("responseCode", "error");
            result.put("errorMessageList", errorMessages);
            return result;
        }

        try {
            Map<String, Object> serviceResult = dispatcher.runSync("checkUpdatePrunRoutingTask", context);
            if (ServiceUtil.isError(serviceResult)) {
                result.put("responseCode", "error");
                result.put("errorMessage", ServiceUtil.getErrorMessage(serviceResult));
                return result;
            }
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling checkUpdatePrunRoutingTask: " + e.getMessage(), MODULE);
            result.put("responseCode", "error");
            result.put("errorMessage", e.getMessage());
            return result;
        }

        Map<String, Object> context1 = new HashMap<>();
        context1.put("userLogin", userLogin);
        context1.put("workEffortParentId", params.get("productionRunId"));
        context1.put("workEffortId", params.get("routingTaskId"));
        if (UtilValidate.isNotEmpty(params.get("priority"))) {
            try {
                context1.put("priority", toLong(params.get("priority")));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingRoutingSeqIdFormatNotCorrect", locale));
            }
        }
        context1.put("workEffortName", params.get("workEffortName"));
        context1.put("description", params.get("description"));
        if (UtilValidate.isNotEmpty(params.get("estimatedSetupMillis"))) {
            try {
                context1.put("estimatedSetupMillis", Double.valueOf(toBigDecimal(params.get("estimatedSetupMillis")).doubleValue()));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunEstimatedSetupMillisNotCorrect", locale));
            }
        }
        if (UtilValidate.isNotEmpty(params.get("estimatedMilliSeconds"))) {
            try {
                context1.put("estimatedMilliSeconds", Double.valueOf(toBigDecimal(params.get("estimatedMilliSeconds")).doubleValue()));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunEstimatedMilliSecondsNotCorrect", locale));
            }
        }
        if (UtilValidate.isNotEmpty(params.get("reservPersons"))) {
            try {
                context1.put("reservPersons", toBigDecimal(params.get("reservPersons")));
            } catch (Exception e) {
                errorMessages.add(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunReservPersonNotCorrect", locale));
            }
        }

        if (!errorMessages.isEmpty()) {
            result.put("responseCode", "error");
            result.put("errorMessageList", errorMessages);
            return result;
        }

        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateWorkEffort", context1);
            if (ServiceUtil.isError(serviceResult)) {
                result.put("responseCode", "error");
                result.put("errorMessage", ServiceUtil.getErrorMessage(serviceResult));
                return result;
            }
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling updateWorkEffort: " + e.getMessage(), MODULE);
            result.put("responseCode", "error");
            result.put("errorMessage", e.getMessage());
            return result;
        }

        result.put("responseCode", "success");
        return result;
    }

    // ==================== Conversion helpers (mirror simple-map-processor <convert>) ====================

    private static Long toLong(Object value) {
        if (value instanceof Long) {
            return (Long) value;
        }
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }
        return Long.valueOf(value.toString().trim());
    }

    private static BigDecimal toBigDecimal(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof BigDecimal) {
            return (BigDecimal) value;
        }
        if (value instanceof Number) {
            return new BigDecimal(value.toString());
        }
        return new BigDecimal(value.toString().trim());
    }

    private static Timestamp toTimestamp(Object value) {
        if (value instanceof Timestamp) {
            return (Timestamp) value;
        }
        return Timestamp.valueOf(value.toString().trim());
    }
}

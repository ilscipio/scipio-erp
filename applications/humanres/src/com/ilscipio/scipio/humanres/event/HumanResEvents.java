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
import org.ofbiz.base.util.UtilDateTime;
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
 * <p>Generated from: component://humanres/script/org/ofbiz/humanres/HumanResEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class HumanResEvents {

    private static final String MODULE = HumanResEvents.class.getName();


    /**
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createInternalOrg(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> internalCtx = new HashMap<>();
        internalCtx.put("partyIdFrom", context.get("headpartyId"));
        internalCtx.put("partyIdTo", context.get("partyId"));
        internalCtx.put("partyRelationshipTypeId", "GROUP_ROLLUP");
        internalCtx.put("roleTypeIdFrom", "INTERNAL_ORGANIZATIO");
        internalCtx.put("roleTypeIdTo", "INTERNAL_ORGANIZATIO");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyRelationship", internalCtx);
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

        return "success";
    }


    /**
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeEmlpPosition(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> delFullfillCtx = null;
        List<GenericValue> emplPositionFulfillment = null;
        try {
            emplPositionFulfillment = EntityQuery.use(delegator)
                    .from("EmplPositionFulfillment")
                    .where(UtilMisc.toMap("emplPositionId", context.get("emplPositionId"), "partyId", context.get("partyId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying EmplPositionFulfillment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(emplPositionFulfillment)) {
            context.put("fromDate", ((GenericValue) ((List<?>) emplPositionFulfillment).get(0)).get("fromDate"));
            // set-service-fields from "parameters" to "delFullfillCtx" for service "deleteEmplPositionFulfillment"
            delFullfillCtx.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("deleteEmplPositionFulfillment", delFullfillCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling deleteEmplPositionFulfillment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        Map<String, Object> delEmlpCtx = new HashMap<>();
        // set-service-fields from "parameters" to "delEmlpCtx" for service "deleteEmplPosition"
        delEmlpCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteEmplPosition", delEmlpCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteEmplPosition: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeInternalOrg(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> deletePartyRelationship = null;
        List<GenericValue> partyRelationship = null;
        try {
            partyRelationship = EntityQuery.use(delegator)
                    .from("PartyRelationship")
                    .where(UtilMisc.toMap("partyIdTo", context.get("partyId"), "partyIdFrom", context.get("parentpartyId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRelationship: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(partyRelationship)) {
            // set-service-fields from "partyRelationship[0]" to "deletePartyRelationship" for service "deletePartyRelationship"
            deletePartyRelationship.putAll(UtilMisc.toMap(((List<?>) partyRelationship).get(0)));
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deletePartyRelationship", deletePartyRelationship);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deletePartyRelationship: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create Public Holiday
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPublicHoliday(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        if (UtilValidate.isEmpty(context.get("workEffortName"))) {
            error_list.add("The Holiday Name is missing.");
            request.setAttribute("_ERROR_MESSAGE_", "The Holiday Name is missing.");
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        if (UtilValidate.isEmpty(context.get("estimatedStartDate"))) {
            error_list.add("The FromDate is missing");
            request.setAttribute("_ERROR_MESSAGE_", "The FromDate is missing");
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        Timestamp dateValue = (Timestamp) context.get("estimatedStartDate");
        Object dateEnd = null;
        try {
            dateEnd = UtilDateTime.getDayEnd((Timestamp) dateValue);
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilDateTime.getDayEnd: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("estimatedCompletionDate"))) {
            context.put("estimatedCompletionDate", dateEnd);
        }
        List<GenericValue> workEffortList = null;
        try {
            workEffortList = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(UtilMisc.toMap("workEffortTypeId", "PUBLIC_HOLIDAY", "estimatedStartDate", context.get("estimatedStartDate")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(workEffortList)) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createWorkEffortAndPartyAssign", context);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createWorkEffortAndPartyAssign: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            error_list.add("This FromDate : ${parameters.estimatedStartDate} already exist.");
            request.setAttribute("_ERROR_MESSAGE_", "This FromDate : ${parameters.estimatedStartDate} already exist.");
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }

        return "success";
    }

}

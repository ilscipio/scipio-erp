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
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/budget/BudgetServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class BudgetServices {

    private static final String MODULE = BudgetServices.class.getName();


    /**
     * Create an Budget
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createBudget(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("Budget");
        newEntity.setNonPKFields((Map<String, Object>) context);
        ((GenericValue) newEntity).put("budgetId", delegator.getNextSeqId("Budget"));
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> setStatus = new HashMap<>();
        setStatus.put("budgetId", ((Map<String, Object>) newEntity).get("budgetId"));
        setStatus.put("statusId", "BG_CREATED");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateBudgetStatus", setStatus);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateBudgetStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("budgetId", ((Map<String, Object>) newEntity).get("budgetId"));

        return "success";
    }


    /**
     * Update Budget Status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateBudgetStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        GenericValue budgetStatus = null;
        GenericValue statusValidChange = null;
        Timestamp nowDate = new Timestamp(System.currentTimeMillis());
        List<GenericValue> budgetStatuses = null;
        try {
            budgetStatuses = EntityQuery.use(delegator)
                    .from("BudgetStatus")
                    .where(UtilMisc.toMap("budgetId", context.get("budgetId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying BudgetStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(budgetStatuses)) {
            budgetStatus = EntityUtil.getFirst((List<GenericValue>) budgetStatuses);
            try {
                statusValidChange = EntityQuery.use(delegator)
                        .from("StatusValidChange")
                        .where(UtilMisc.toMap("statusId", ((Map<String, Object>) budgetStatus).get("statusId"), "statusIdTo", context.get("statusId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(statusValidChange)) {
                newEntity = delegator.makeValue("BudgetStatus");
                newEntity.setPKFields((Map<String, Object>) context);
                newEntity.setNonPKFields((Map<String, Object>) context);
                newEntity.put("statusDate", nowDate);
                try {
                    delegator.create(newEntity);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        } else {
            newEntity = delegator.makeValue("BudgetStatus");
            newEntity.setPKFields((Map<String, Object>) context);
            newEntity.setNonPKFields((Map<String, Object>) context);
            newEntity.put("statusDate", nowDate);
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
     * Create a Budget Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createBudgetRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createPartyRole = null;
        GenericValue partyRole = null;
        try {
            partyRole = EntityQuery.use(delegator)
                    .from("PartyRole")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(partyRole)) {
            createPartyRole.put("partyId", context.get("partyId"));
            createPartyRole.put("roleTypeId", context.get("roleTypeId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyRole", createPartyRole);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        GenericValue newEntity = delegator.makeValue("BudgetRole");
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

}

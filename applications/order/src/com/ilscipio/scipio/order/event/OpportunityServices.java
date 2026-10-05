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
package com.ilscipio.scipio.order.event;

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
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://order/script/org/ofbiz/order/opportunity/OpportunityServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class OpportunityServices {

    private static final String MODULE = OpportunityServices.class.getName();


    /**
     * Creates a sales forecast and stores a copy in sales forecast history.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSalesForecast(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue salesForecast = delegator.makeValue("SalesForecast");
        String salesForecastId = delegator.getNextSeqId("SalesForecast");
        salesForecast.put("salesForecastId", salesForecastId);
        salesForecast.setNonPKFields((Map<String, Object>) context);
        salesForecast.put("createdByUserLoginId", ((Map<String, Object>) userLogin).get("userLoginId"));
        salesForecast.put("modifiedByUserLoginId", ((Map<String, Object>) userLogin).get("userLoginId"));
        try {
            delegator.create(salesForecast);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        String inlineResult = saveSalesForecastHistory(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        result.put("salesForecastId", salesForecastId);

        return "success";
    }


    /**
     * Updates a sales forecast and stores a copy in sales forecast history.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSalesForecast(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue salesForecast = null;
        try {
            salesForecast = EntityQuery.use(delegator)
                    .from("SalesForecast")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SalesForecast: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        salesForecast.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(salesForecast);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        String result = saveSalesForecastHistory(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Given a salesForecast map, saves the data in the history.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String saveSalesForecastHistory(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue salesForecastHistory = delegator.makeValue("SalesForecastHistory");
        String salesForecastHistoryId = delegator.getNextSeqId("SalesForecastHistory");
        salesForecastHistory.put("salesForecastHistoryId", salesForecastHistoryId);
        salesForecastHistory.setNonPKFields((Map<String, Object>) context.get("salesForecast"));
        salesForecastHistory.put("changeNote", context.get("changeNote"));
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        salesForecastHistory.put("modifiedTimestamp", nowTimestamp);
        Map<String, Object> salesForecast = new HashMap<>();
        salesForecast.put("modifiedByUserLoginId", ((Map<String, Object>) userLogin).get("userLoginId"));
        try {
            delegator.create(salesForecastHistory);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Creates a sales forecast detail
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSalesForecastDetail(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("SalesForecastDetail");
        ((GenericValue) newEntity).put("salesForecastDetailId", delegator.getNextSeqId("SalesForecastDetail"));
        result.put("salesForecastDetailId", ((Map<String, Object>) newEntity).get("salesForecastDetailId"));
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
     * Updates a sales forecast detail
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSalesForecastDetail(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SalesForecastDetail")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SalesForecastDetail: " + e.getMessage(), MODULE);
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
     * Delete a Sales Forecast Detail
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteSalesForecastDetail(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SalesForecastDetail")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SalesForecastDetail: " + e.getMessage(), MODULE);
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
     * Create Sales Opportunity
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSalesOpportunity(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = delegator.makeValue("SalesOpportunity");
        if (UtilValidate.isEmpty(context.get("salesOpportunityId"))) {
            ((GenericValue) context).put("salesOpportunityId", delegator.getNextSeqId("SalesOpportunity"));
        }
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.put("createdByUserLogin", ((Map<String, Object>) userLogin).get("userLoginId"));
        result.put("salesOpportunityId", context.get("salesOpportunityId"));
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
     * Update an Sales opportunity
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSalesOpportunity(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp nowTimestamp = null;
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("SalesOpportunity")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SalesOpportunity: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("SOSTG_CLOSED".equals(context.get("opportunityStageId"))) {
            if (UtilValidate.isEmpty(context.get("estimatedCloseDate"))) {
                nowTimestamp = new Timestamp(System.currentTimeMillis());
                try {
                    ((Map<String, Object>) context).put("estimatedCloseDate", UtilDateTime.getDayStart((Timestamp) nowTimestamp));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling UtilDateTime.getDayStart: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
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
     * Create Sales Opportunity Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSalesOpportunityRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("SalesOpportunityRole");
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
     * Create Sales Opportunity Account Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSalesOpportunityAccountRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> salesOpportunityRole = null;
        List<GenericValue> salesOpportunityRoles = null;
        try {
            salesOpportunityRoles = EntityQuery.use(delegator)
                    .from("SalesOpportunityRole")
                    .where(UtilMisc.toMap("salesOpportunityId", context.get("salesOpportunityId"), "roleTypeId", "ACCOUNT"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SalesOpportunityRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue lookedUpValue = EntityUtil.getFirst((List<GenericValue>) salesOpportunityRoles);
        if (!java.util.Objects.equals(((Map<String, Object>) lookedUpValue).get("partyId"), context.get("accountPartyId"))) {
            if (UtilValidate.isNotEmpty(lookedUpValue)) {
                try {
                    delegator.removeValue(lookedUpValue);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            if (UtilValidate.isNotEmpty(context.get("accountPartyId"))) {
                salesOpportunityRole.put("roleTypeId", "ACCOUNT");
                salesOpportunityRole.put("partyId", context.get("accountPartyId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("ensurePartyRole", salesOpportunityRole);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling ensurePartyRole: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            salesOpportunityRole.put("salesOpportunityId", context.get("salesOpportunityId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createSalesOpportunityRole", salesOpportunityRole);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createSalesOpportunityRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create Sales Opportunity Lead Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSalesOpportunityLeadRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> salesOpportunityRole = null;
        List<GenericValue> salesOpportunityRoles = null;
        try {
            salesOpportunityRoles = EntityQuery.use(delegator)
                    .from("SalesOpportunityRole")
                    .where(UtilMisc.toMap("salesOpportunityId", context.get("salesOpportunityId"), "roleTypeId", "LEAD"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SalesOpportunityRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue lookedUpValue = EntityUtil.getFirst((List<GenericValue>) salesOpportunityRoles);
        if (!java.util.Objects.equals(((Map<String, Object>) lookedUpValue).get("partyId"), context.get("leadPartyId"))) {
            if (UtilValidate.isNotEmpty(lookedUpValue)) {
                try {
                    delegator.removeValue(lookedUpValue);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            salesOpportunityRole.put("roleTypeId", "LEAD");
            salesOpportunityRole.put("partyId", context.get("leadPartyId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("ensurePartyRole", salesOpportunityRole);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling ensurePartyRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            salesOpportunityRole.put("salesOpportunityId", context.get("salesOpportunityId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createSalesOpportunityRole", salesOpportunityRole);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createSalesOpportunityRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * find party in role specified by in parameter
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String findPartyInSalesOpportunityRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<GenericValue> salesOpportunityRoles = null;
        try {
            salesOpportunityRoles = EntityQuery.use(delegator)
                    .from("SalesOpportunityRole")
                    .where(UtilMisc.toMap("roleTypeId", context.get("roleTypeId"), "salesOpportunityId", context.get("salesOpportunityId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SalesOpportunityRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue lookedUpValue = EntityUtil.getFirst((List<GenericValue>) salesOpportunityRoles);
        result.put("partyId", ((Map<String, Object>) lookedUpValue).get("partyId"));

        return "success";
    }

}

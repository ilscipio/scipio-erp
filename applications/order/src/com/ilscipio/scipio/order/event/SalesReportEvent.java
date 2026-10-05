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

import java.math.BigDecimal;
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
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://order/script/org/ofbiz/order/reports/SalesReportEvent.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SalesReportEvent {

    private static final String MODULE = SalesReportEvent.class.getName();


    /**
     * Get Orders
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getOrder(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> starschemas = null;
        Object lastYear = null;
        Object fromWeek = null;
        Object fromMonth = null;
        if ("day".equals(context.get("reportBy"))) {
            if (UtilValidate.isNotEmpty(context.get("thruDate"))) {
                try {
                    starschemas = EntityQuery.use(delegator)
                            .from("SalesOrderItemStarSchema")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying SalesOrderItemStarSchema: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if ("week".equals(context.get("reportBy"))) {
            if (UtilValidate.isNotEmpty(context.get("thruWeek"))) {
                if (((Comparable) context.get("thruWeek")).compareTo(3) <= 0) {
                    lastYear = new BigDecimal(context.get("thisYear").toString());
                    try {
                        starschemas = EntityQuery.use(delegator)
                                .from("SalesOrderItemStarSchema")
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying SalesOrderItemStarSchema: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                } else {
                    fromWeek = new BigDecimal(context.get("thruWeek").toString());
                    try {
                        starschemas = EntityQuery.use(delegator)
                                .from("SalesOrderItemStarSchema")
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying SalesOrderItemStarSchema: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        if ("month".equals(context.get("reportBy"))) {
            if (UtilValidate.isNotEmpty(context.get("thruMonth"))) {
                if (((Comparable) context.get("thruMonth")).compareTo(3) <= 0) {
                    lastYear = new BigDecimal(context.get("thisYear").toString());
                    if ("3".equals(context.get("thruMonth"))) {
                        try {
                            starschemas = EntityQuery.use(delegator)
                                    .from("SalesOrderItemStarSchema")
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying SalesOrderItemStarSchema: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                    if ("2".equals(context.get("thruMonth"))) {
                        try {
                            starschemas = EntityQuery.use(delegator)
                                    .from("SalesOrderItemStarSchema")
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying SalesOrderItemStarSchema: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                    if ("1".equals(context.get("thruMonth"))) {
                        try {
                            starschemas = EntityQuery.use(delegator)
                                    .from("SalesOrderItemStarSchema")
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying SalesOrderItemStarSchema: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                } else {
                    fromMonth = new BigDecimal(context.get("thruMonth").toString());
                    try {
                        starschemas = EntityQuery.use(delegator)
                                .from("SalesOrderItemStarSchema")
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying SalesOrderItemStarSchema: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }

        return "success";
    }

}

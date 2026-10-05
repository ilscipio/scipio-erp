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
 * <p>Generated from: component://order/script/org/ofbiz/order/reports/NetBeforeOverheadMonthlyEvent.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class NetBeforeOverheadMonthlyEvent {

    private static final String MODULE = NetBeforeOverheadMonthlyEvent.class.getName();


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

        Object DateYear = null;
        List<GenericValue> countdates = null;
        List<GenericValue> starschemas = null;
        Object DateMonth = null;
        List<GenericValue> saleschannels = null;
        Object count1 = null;
        Object salesChannelId = null;
        Object checksalesChannel1 = null;
        Object count2 = null;
        Object checksalesChannel2 = null;
        Object starschemacountdate = null;
        Object count3 = null;
        Object count4 = null;
        Map<String, Object> countdateMap_countsalesChannel = new HashMap<>();
        if (UtilValidate.isNotEmpty(DateMonth)) {
            DateYear = "2009";
            DateMonth = "12";
            try {
                countdates = EntityQuery.use(delegator)
                        .from("SalesOrderItemStarSchema")
                        .distinct()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying SalesOrderItemStarSchema: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                starschemas = EntityQuery.use(delegator)
                        .from("SalesOrderItemStarSchema")
                        .distinct()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying SalesOrderItemStarSchema: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                saleschannels = EntityQuery.use(delegator)
                        .from("SalesOrderItemStarSchema")
                        .distinct()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying SalesOrderItemStarSchema: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        count1 = "-1";
        if (saleschannels != null) {
            for (GenericValue saleschannel : saleschannels) {
                count1 = new BigDecimal(count1.toString());
                salesChannelId = ((Map<String, Object>) saleschannel).get("salesChannelEnumId");
                List<Object> salesChannelMap_salesChannelList = new LinkedList<>();
                salesChannelMap_salesChannelList.add(salesChannelId);
            }
        }
        count2 = "0";
        if (countdates != null) {
            for (GenericValue countdate : countdates) {
                checksalesChannel1 = ((Map<String, Object>) context.get("salesChannelMap")).get("salesChannelList[count2]");
                if (java.util.Objects.equals(((Map<String, Object>) countdate).get("salesChannelEnumId"), checksalesChannel1)) {
                    countdateMap_countsalesChannel.put("count2", new BigDecimal(((Map<String, Object>) context.get("countdateMap")).get("countsalesChannel[count2]").toString()));
                }
                if (!java.util.Objects.equals(((Map<String, Object>) countdate).get("salesChannelEnumId"), checksalesChannel1)) {
                    count2 = new BigDecimal(count2.toString());
                    countdateMap_countsalesChannel.put("count2", new BigDecimal(((Map<String, Object>) context.get("countdateMap")).get("countsalesChannel[count2]").toString()));
                }
            }
        }
        count3 = "-1";
        count4 = "0";
        if (starschemas != null) {
            for (GenericValue starschema : starschemas) {
                count3 = new BigDecimal(count3.toString());
                count4 = new BigDecimal(count4.toString());
                checksalesChannel2 = ((Map<String, Object>) context.get("salesChannelMap")).get("salesChannelList[count3]");
                if (java.util.Objects.equals(((Map<String, Object>) starschema).get("salesChannelEnumId"), checksalesChannel2)) {
                    starschemacountdate = ((Map<String, Object>) context.get("countdateMap")).get("countsalesChannel[count4]");
                    starschema.put("CountDate", starschemacountdate);
                }
            }
        }

        return "success";
    }

}

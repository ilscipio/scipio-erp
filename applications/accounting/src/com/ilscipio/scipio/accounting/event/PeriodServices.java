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
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/period/PeriodServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PeriodServices {

    private static final String MODULE = PeriodServices.class.getName();


    /**
     * Find a CustomTimePeriod
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String findCustomTimePeriods(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object parentOrganizationPartyIdList = null;
        Map<String, Object> getParentOrganizationsCallMap = null;
        List<GenericValue> orgTimePeriodList = null;
        List<GenericValue> generalCustomTimePeriodList = null;
        if (UtilValidate.isNotEmpty(context.get("organizationPartyId"))) {
            getParentOrganizationsCallMap.put("organizationPartyId", context.get("organizationPartyId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getParentOrganizations", getParentOrganizationsCallMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                parentOrganizationPartyIdList = serviceResult.get("parentOrganizationPartyIdList");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getParentOrganizations: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (parentOrganizationPartyIdList != null) {
                for (Object curOrganizationPartyId : (List<Object>) parentOrganizationPartyIdList) {
                    orgTimePeriodList = null;
                    try {
                        orgTimePeriodList = EntityQuery.use(delegator)
                                .from("CustomTimePeriod")
                                .cache()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying CustomTimePeriod: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    // TODO: Convert <list-to-list> element
                }
            }
        }
        if (!"Y".equals(context.get("excludeNoOrganizationPeriods"))) {
            try {
                generalCustomTimePeriodList = EntityQuery.use(delegator)
                        .from("CustomTimePeriod")
                        .cache()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying CustomTimePeriod: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // TODO: Convert <list-to-list> element
        }
        result.put("customTimePeriodList", context.get("listSoFar"));

        return "success";
    }


    /**
     * Return previous time period
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPreviousTimePeriod(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue previousTimePeriod = null;
        List<GenericValue> customTimePeriodList = null;
        GenericValue currentTimePeriod = null;
        try {
            currentTimePeriod = EntityQuery.use(delegator)
                    .from("CustomTimePeriod")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CustomTimePeriod: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Integer periodNum = ((Number) ((Map<String, Object>) currentTimePeriod).get("periodNum")).intValue() - 1;
        if (((Comparable) periodNum).compareTo("-1") > 0) {
            try {
                customTimePeriodList = EntityQuery.use(delegator)
                        .from("CustomTimePeriod")
                        .where(UtilMisc.toMap("organizationPartyId", ((Map<String, Object>) currentTimePeriod).get("organizationPartyId"), "periodTypeId", ((Map<String, Object>) currentTimePeriod).get("periodTypeId"), "periodNum", periodNum))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying CustomTimePeriod: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            previousTimePeriod = EntityUtil.getFirst((List<GenericValue>) customTimePeriodList);
            result.put("previousTimePeriod", previousTimePeriod);
        }

        return "success";
    }

}

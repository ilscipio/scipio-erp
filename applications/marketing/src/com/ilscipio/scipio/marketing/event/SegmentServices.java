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
package com.ilscipio.scipio.marketing.event;

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
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://marketing/script/org/ofbiz/marketing/segment/SegmentServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class SegmentServices {

    private static final String MODULE = SegmentServices.class.getName();


    /**
     * Determine: are Parties Related Through SegmentGroup?
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String arePartiesRelatedThroughSegmentGroup(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object areRelated = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        List<GenericValue> segmentGroupViewRelatedPartiesList = null;
        try {
            segmentGroupViewRelatedPartiesList = EntityQuery.use(delegator)
                    .from("SegmentGroupViewRelatedParties")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying SegmentGroupViewRelatedParties: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ((UtilValidate.isEmpty(segmentGroupViewRelatedPartiesList))) {
            areRelated = "N";
            result.put("areRelated", areRelated);
        } else {
            areRelated = "Y";
            result.put("areRelated", areRelated);
        }

        return "success";
    }

}

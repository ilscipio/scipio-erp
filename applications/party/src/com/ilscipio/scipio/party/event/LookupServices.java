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
package com.ilscipio.scipio.party.event;

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
import org.ofbiz.service.LocalDispatcher;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://party/script/org/ofbiz/party/LookupServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class LookupServices {

    private static final String MODULE = LookupServices.class.getName();


    /**
     * Lookup a party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String lookupParty(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> LookupMap = null;
        List<Object> lookupResult = null;
        Map<String, Object> resultEntry = null;
        if (UtilValidate.isNotEmpty(context.get("firstName"))) {
            LookupMap.put("firstName", context.get("firstName"));
        }
        if (UtilValidate.isNotEmpty(context.get("lastName"))) {
            LookupMap.put("lastName", context.get("lastName"));
        }
        // TODO: Convert <find-by-and> element
        if (context.get("parties") != null) {
            for (Object party : (List<Object>) context.get("parties")) {
                resultEntry.put("label", ((Map<String, Object>) party).get("firstName"));
                resultEntry.put("value", ((Map<String, Object>) party).get("partyId"));
                lookupResult.add(resultEntry);
            }
        }
        if (UtilValidate.isEmpty(context.get("parties"))) {
            resultEntry.put("label", "No match");
            resultEntry.put("value", new HashMap<String, Object>());
            lookupResult.add(resultEntry);
        }
        result.put("lookupResult", lookupResult);

        return "success";
    }

}

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
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://marketing/script/org/ofbiz/sfa/account/AccountServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class AccountServices {

    private static final String MODULE = AccountServices.class.getName();


    /**
     * create Account Group
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        context.put("roleTypeId", "ACCOUNT");
        // TODO: Call simple-method "createPartyGroupRoleAndContactMechs" from "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml"
        // Original: call-simple-method method-name="createPartyGroupRoleAndContactMechs" xml-resource="component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml"
        result.put("partyId", context.get("partyId"));
        Map<String, Object> ensurePartyRoleCtx = new HashMap<>();
        ensurePartyRoleCtx.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        ensurePartyRoleCtx.put("roleTypeId", "OWNER");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("ensurePartyRole", ensurePartyRoleCtx);
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
        Map<String, Object> partyRelationshipCtx = new HashMap<>();
        partyRelationshipCtx.put("partyIdFrom", ((Map<String, Object>) userLogin).get("partyId"));
        partyRelationshipCtx.put("partyIdTo", context.get("partyId"));
        partyRelationshipCtx.put("roleTypeIdFrom", "OWNER");
        partyRelationshipCtx.put("roleTypeIdTo", "ACCOUNT");
        partyRelationshipCtx.put("partyRelationshipTypeId", "ACCOUNT");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyRelationship", partyRelationshipCtx);
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

}

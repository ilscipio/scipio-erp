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
package com.ilscipio.scipio.shop.event;

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
 * <p>Generated from: component://shop/script/com/ilscipio/scipio/shop/misc/AffiliateSimpleEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class AffiliateSimpleEvents {

    private static final String MODULE = AffiliateSimpleEvents.class.getName();


    /**
     * Create Affiliate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAffiliate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert call-map-processor (in-map: parameters, out-map: ul_context)
        Map<String, Object> ul_context = new HashMap<>(context);
        // TODO: Convert call-map-processor (in-map: parameters, out-map: ul_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createUserLogin", ul_context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createUserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> newUserLoginMap = new HashMap<>();
        newUserLoginMap.put("userLoginId", ((Map<String, Object>) context.get("ul_context")).get("userLoginId"));
        newUserLoginMap.put("currentPassword", ((Map<String, Object>) context.get("ul_context")).get("currentPassword"));
        newUserLoginMap.put("passwordHint", ((Map<String, Object>) context.get("ul_context")).get("passwordHint"));
        newUserLoginMap.put("partyId", ((Map<String, Object>) context.get("ul_context")).get("partyId"));
        GenericValue newUserLogin = delegator.makeValue("UserLogin");
        // TODO: Convert <set-current-user-login> element
        Map<String, Object> pg_context = new HashMap<>(context);
        Map<String, Object> ct_context = new HashMap<>(context);
        Map<String, Object> pa_context = new HashMap<>(context);
        Map<String, Object> pr_context = new HashMap<>(context);
        Map<String, Object> ph_context = new HashMap<>(context);
        Map<String, Object> phr_context = new HashMap<>(context);
        Map<String, Object> em_context = new HashMap<>(context);
        Map<String, Object> emp_context = new HashMap<>(context);
        Map<String, Object> url_context = new HashMap<>(context);
        Map<String, Object> af_context = new HashMap<>(context);
        // TODO: Convert call-map-processor (in-map: parameters, out-map: pg_context)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: pg_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyGroup", (Map<String, Object>) pg_context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyGroup: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: ct_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPerson", (Map<String, Object>) ct_context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPerson: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: pa_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPostalAddress", (Map<String, Object>) pa_context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPostalAddress: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: pr_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", (Map<String, Object>) pr_context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyContactMechPurpose: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: ph_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createTelecomNumber", (Map<String, Object>) ph_context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createTelecomNumber: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: phr_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", (Map<String, Object>) phr_context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyContactMechPurpose: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: fax_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object fax_context = null;
        if (UtilValidate.isNotEmpty(((Map<String, Object>) fax_context).get("contactNumber"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createTelecomNumber", (Map<String, Object>) fax_context);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createTelecomNumber: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: faxp_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object faxp_context = null;
        if (UtilValidate.isNotEmpty(((Map<String, Object>) faxp_context).get("contactNumber"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", (Map<String, Object>) faxp_context);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyContactMechPurpose: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: em_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createEmailAddress", (Map<String, Object>) em_context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createEmailAddress: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: emp_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", (Map<String, Object>) emp_context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyContactMechPurpose: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: url_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContactMech", (Map<String, Object>) url_context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: af_context)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: af_context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createAffiliate", (Map<String, Object>) af_context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createAffiliate: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }

}

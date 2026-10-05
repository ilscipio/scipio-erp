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
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://party/script/org/ofbiz/party/party/PartyPermissionServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PartyPermissionServices {

    private static final String MODULE = PartyPermissionServices.class.getName();


    /**
     * Party Manager base permission logic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String basePermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object primaryPermission = "PARTYMGR";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"

        return "success";
    }


    /**
     * Party ID Permission Check
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String partyIdPermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object partyId = null;
        Boolean hasPermission = null;
        String resourceDescription = null;
        String failMessage = null;
        if (UtilValidate.isEmpty(partyId)) {
            partyId = context.get("partyId");
        }
        if ((!(UtilValidate.isEmpty(partyId)) && !(UtilValidate.isEmpty(((Map<String, Object>) userLogin).get("partyId"))) && java.util.Objects.equals(partyId, ((Map<String, Object>) userLogin).get("partyId")))) {
            hasPermission = Boolean.TRUE;
        } else {
            resourceDescription = (String) context.get("resourceDescription");
            if (UtilValidate.isEmpty(resourceDescription)) {
                resourceDescription = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionThisOperation", locale);
            }
            failMessage = UtilProperties.getMessage("PartyUiLabels", "PartyPermissionErrorPartyId", locale);
            hasPermission = Boolean.FALSE;
            result.put("failMessage", failMessage);
        }
        result.put("hasPermission", hasPermission);

        return "success";
    }


    /**
     * Base Permission Plus Party ID Permission Check
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String basePlusPartyIdPermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = basePermissionCheck(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (!"true".equals(context.get("hasPermission"))) {
            String checkResult = partyIdPermissionCheck(request, response);
            if (!"success".equals(checkResult)) {
                return checkResult;
            }
        }

        return "success";
    }


    /**
     * Party status permission logic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String partyStatusPermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Boolean hasPermission = null;
        Object altPermission = null;
        hasPermission = Boolean.FALSE;
        if (UtilValidate.isNotEmpty(context.get("partyId"))) {
            if (java.util.Objects.equals(context.get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) {
                hasPermission = Boolean.TRUE;
                result.put("hasPermission", hasPermission);
            }
        }
        if (!Boolean.TRUE.equals(hasPermission)) {
            altPermission = "PARTYMGR_STS";
            String checkResult = basePermissionCheck(request, response);
            if (!"success".equals(checkResult)) {
                return checkResult;
            }
        }

        return "success";
    }


    /**
     * Party group permission logic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String partyGroupPermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object altPermission = "PARTYMGR_GRP";
        String result = basePlusPartyIdPermissionCheck(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Party datasource permission logic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String partyDatasourcePermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object altPermission = "PARTYMGR_SRC";
        String result = basePermissionCheck(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Party role permission logic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String partyRolePermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object altPermission = "PARTYMGR_ROLE";
        String result = basePlusPartyIdPermissionCheck(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Party relationship permission logic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String partyRelationshipPermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Boolean hasPermission = null;
        Object altPermission = null;
        if (UtilValidate.isEmpty(context.get("partyIdFrom"))) {
            context.put("partyIdFrom", ((Map<String, Object>) userLogin).get("partyId"));
            hasPermission = Boolean.TRUE;
            result.put("hasPermission", hasPermission);
        } else {
            altPermission = "PARTYMGR_REL";
            String checkResult = basePermissionCheck(request, response);
            if (!"success".equals(checkResult)) {
                return checkResult;
            }
        }

        return "success";
    }


    /**
     * Party contact mech permission logic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String partyContactMechPermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Boolean hasPermission = null;
        Object altPermission = null;
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        if (java.util.Objects.equals(context.get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) {
            hasPermission = Boolean.TRUE;
            result.put("hasPermission", hasPermission);
        } else {
            altPermission = "PARTYMGR_PCM";
            String checkResult = basePermissionCheck(request, response);
            if (!"success".equals(checkResult)) {
                return checkResult;
            }
        }

        return "success";
    }


    /**
     * Accept and Decline PartyInvitation Permission Logic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String accAndDecPartyInvitationPermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Boolean hasPermission = null;
        GenericValue partyInvitation = null;
        Map<String, Object> findPartyCtx = null;
        Object partyId = null;
        String failMessage = null;
        hasPermission = Boolean.FALSE;
        // TODO: Convert <if-has-permission> element
        if (!"true".equals(hasPermission)) {
            try {
                partyInvitation = EntityQuery.use(delegator)
                        .from("PartyInvitation")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyInvitation: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(((Map<String, Object>) partyInvitation).get("partyId"))) {
                if (UtilValidate.isEmpty(((Map<String, Object>) partyInvitation).get("emailAddress"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyInvitationNotValidError", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                } else {
                    findPartyCtx.put("address", ((Map<String, Object>) partyInvitation).get("emailAddress"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("findPartyFromEmailAddress", findPartyCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        partyId = serviceResult.get("partyId");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling findPartyFromEmailAddress: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isNotEmpty(partyId)) {
                        if (java.util.Objects.equals(partyId, ((Map<String, Object>) userLogin).get("partyId"))) {
                            hasPermission = Boolean.TRUE;
                            result.put("hasPermission", hasPermission);
                        }
                    } else {
                        {
                            String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyInvitationNotValidError", locale);
                            error_list.add(errorMsg);
                            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                        }
                    }
                }
            } else {
                if (java.util.Objects.equals(((Map<String, Object>) partyInvitation).get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) {
                    hasPermission = Boolean.TRUE;
                    result.put("hasPermission", hasPermission);
                }
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        if (!"true".equals(hasPermission)) {
            failMessage = UtilProperties.getMessage("PartyUiLabels", "PartyInvitationAccAndDecPermissionError", locale);
            result.put("hasPermission", hasPermission);
            result.put("failMessage", failMessage);
        }

        return "success";
    }


    /**
     * Cancel PartyInvitation Permission Logic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String cancelPartyInvitationPermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Boolean hasPermission = null;
        GenericValue partyInvitation = null;
        Map<String, Object> findPartyCtx = null;
        Object partyId = null;
        String failMessage = null;
        hasPermission = Boolean.FALSE;
        // TODO: Convert <if-has-permission> element
        if (!"true".equals(hasPermission)) {
            try {
                partyInvitation = EntityQuery.use(delegator)
                        .from("PartyInvitation")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyInvitation: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) partyInvitation).get("partyIdFrom"))) {
                if (java.util.Objects.equals(((Map<String, Object>) partyInvitation).get("partyIdFrom"), ((Map<String, Object>) userLogin).get("partyId"))) {
                    hasPermission = Boolean.TRUE;
                    result.put("hasPermission", hasPermission);
                }
            }
            if (!"true".equals(hasPermission)) {
                if (UtilValidate.isEmpty(((Map<String, Object>) partyInvitation).get("partyId"))) {
                    if (UtilValidate.isEmpty(((Map<String, Object>) partyInvitation).get("emailAddress"))) {
                        {
                            String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyInvitationNotValidError", locale);
                            error_list.add(errorMsg);
                            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                        }
                    } else {
                        findPartyCtx.put("address", ((Map<String, Object>) partyInvitation).get("emailAddress"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("findPartyFromEmailAddress", findPartyCtx);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                            partyId = serviceResult.get("partyId");
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling findPartyFromEmailAddress: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (UtilValidate.isNotEmpty(partyId)) {
                            if (java.util.Objects.equals(partyId, ((Map<String, Object>) userLogin).get("partyId"))) {
                                hasPermission = Boolean.TRUE;
                                result.put("hasPermission", hasPermission);
                            }
                        } else {
                            {
                                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyInvitationNotValidError", locale);
                                error_list.add(errorMsg);
                                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                            }
                        }
                    }
                } else {
                    if (java.util.Objects.equals(((Map<String, Object>) partyInvitation).get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) {
                        hasPermission = Boolean.TRUE;
                        result.put("hasPermission", hasPermission);
                    }
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        if (!"true".equals(hasPermission)) {
            failMessage = UtilProperties.getMessage("PartyUiLabels", "PartyInvitationCancelPermissionError", locale);
            result.put("hasPermission", hasPermission);
            result.put("failMessage", failMessage);
        }

        return "success";
    }


    /**
     * Communication Event permission logic
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String partyCommunicationEventPermissionCheck(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object altPermission = null;
        Boolean hasPermission = null;
        if (("EMAIL_COMMUNICATION".equals(context.get("communicationEventTypeId")) && "CREATE".equals(context.get("action")))) {
            altPermission = "PARTYMGR_CME-EMAIL";
            String checkResult = basePermissionCheck(request, response);
            if (!"success".equals(checkResult)) {
                return checkResult;
            }
        } else {
            hasPermission = Boolean.TRUE;
            result.put("hasPermission", hasPermission);
        }

        return "success";
    }

}

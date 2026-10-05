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
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://party/script/org/ofbiz/party/party/PartyInvitationServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PartyInvitationServices {

    private static final String MODULE = PartyInvitationServices.class.getName();


    /**
     * Create a PartyInvitation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyInvitation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue person = null;
        GenericValue newEntity = delegator.makeValue("PartyInvitation");
        ((GenericValue) newEntity).put("partyInvitationId", delegator.getNextSeqId("PartyInvitation"));
        result.put("partyInvitationId", ((Map<String, Object>) newEntity).get("partyInvitationId"));
        if (UtilValidate.isEmpty(context.get("toName"))) {
            if (UtilValidate.isNotEmpty(context.get("partyId"))) {
                try {
                    person = EntityQuery.use(delegator)
                            .from("Person")
                            .where(UtilMisc.toMap("partyId", context.get("partyId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Person: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                context.put("toName", ((Map<String, Object>) person).get("firstName") + " " + ((Map<String, Object>) person).get("middleName") + " " + ((Map<String, Object>) person).get("lastName"));
            }
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        newEntity.put("lastInviteDate", nowTimestamp);
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
     * Update a PartyInvitation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyInvitation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue person = null;
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PartyInvitation")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyInvitation: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("toName"))) {
            if (UtilValidate.isNotEmpty(context.get("partyId"))) {
                try {
                    person = EntityQuery.use(delegator)
                            .from("Person")
                            .where(UtilMisc.toMap("partyId", context.get("partyId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Person: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                context.put("toName", ((Map<String, Object>) person).get("firstName") + " " + ((Map<String, Object>) person).get("middleName") + " " + ((Map<String, Object>) person).get("lastName"));
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
     * Remove a PartyInvitation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deletePartyInvitation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PartyInvitation")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyInvitation: " + e.getMessage(), MODULE);
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
     * Create a PartyInvitationGroupAssoc
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyInvitationGroupAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("PartyInvitationGroupAssoc");
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
     * Remove a PartyInvitationGroupAssoc
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deletePartyInvitationGroupAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PartyInvitationGroupAssoc")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyInvitationGroupAssoc: " + e.getMessage(), MODULE);
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
     * Create a PartyInvitationRoleAssoc
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyInvitationRoleAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("PartyInvitationRoleAssoc");
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
     * Remove a PartyInvitationRoleAssoc
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deletePartyInvitationRoleAssoc(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PartyInvitationRoleAssoc")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyInvitationRoleAssoc: " + e.getMessage(), MODULE);
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
     * Accept Party Invitation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String acceptPartyInvitation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createPartyRelationshipCtx = null;
        GenericValue partyRole = null;
        Map<String, Object> createPartyRoleCtx = null;
        List<GenericValue> partyInvitationGroupAssocs = null;
        try {
            partyInvitationGroupAssocs = EntityQuery.use(delegator)
                    .from("PartyInvitationGroupAssoc")
                    .where(UtilMisc.toMap("partyInvitationId", context.get("partyInvitationId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyInvitationGroupAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(partyInvitationGroupAssocs)) {
            createPartyRelationshipCtx.put("partyIdTo", context.get("partyId"));
            createPartyRelationshipCtx.put("partyRelationshipTypeId", "GROUP_ROLLUP");
            if (partyInvitationGroupAssocs != null) {
                for (GenericValue partyInvitationGroupAssoc : partyInvitationGroupAssocs) {
                    createPartyRelationshipCtx.put("partyIdFrom", ((Map<String, Object>) partyInvitationGroupAssoc).get("partyIdTo"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPartyRelationship", createPartyRelationshipCtx);
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
                }
            }
        }
        List<GenericValue> partyInvitationRoleAssocs = null;
        try {
            partyInvitationRoleAssocs = EntityQuery.use(delegator)
                    .from("PartyInvitationRoleAssoc")
                    .where(UtilMisc.toMap("partyInvitationId", context.get("partyInvitationId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyInvitationRoleAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(partyInvitationRoleAssocs)) {
            createPartyRoleCtx.put("partyId", context.get("partyId"));
            if (partyInvitationRoleAssocs != null) {
                for (GenericValue partyInvitationRoleAssoc : partyInvitationRoleAssocs) {
                    try {
                        partyRole = EntityQuery.use(delegator)
                                .from("PartyRole")
                                .where(UtilMisc.toMap("roleTypeId", ((Map<String, Object>) partyInvitationRoleAssoc).get("roleTypeId")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PartyRole: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isEmpty(partyRole)) {
                        createPartyRoleCtx.put("roleTypeId", ((Map<String, Object>) partyInvitationRoleAssoc).get("roleTypeId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createPartyRole", createPartyRoleCtx);
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
                    }
                }
            }
        }
        Map<String, Object> updatePartyInvitationCtx = new HashMap<>();
        updatePartyInvitationCtx.put("partyInvitationId", context.get("partyInvitationId"));
        updatePartyInvitationCtx.put("statusId", "PARTYINV_ACCEPTED");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePartyInvitation", updatePartyInvitationCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePartyInvitation: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Decline Party Invitation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String declinePartyInvitation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> updatePartyInvitationCtx = new HashMap<>();
        updatePartyInvitationCtx.put("partyInvitationId", context.get("partyInvitationId"));
        updatePartyInvitationCtx.put("statusId", "PARTYINV_DECLINED");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePartyInvitation", updatePartyInvitationCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePartyInvitation: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Cancel Party Invitation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String cancelPartyInvitation(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> updatePartyInvitationCtx = new HashMap<>();
        updatePartyInvitationCtx.put("partyInvitationId", context.get("partyInvitationId"));
        updatePartyInvitationCtx.put("statusId", "PARTYINV_CANCELLED");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePartyInvitation", updatePartyInvitationCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePartyInvitation: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }

}

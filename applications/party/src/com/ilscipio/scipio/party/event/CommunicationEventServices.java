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
import org.ofbiz.base.util.GroovyUtil;
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
 * <p>Generated from: component://party/script/org/ofbiz/party/communication/CommunicationEventServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CommunicationEventServices {

    private static final String MODULE = CommunicationEventServices.class.getName();


    /**
     * Create a CommunicationEvent with permission check
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCommunicationEventWithPermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        context.put("permission", "true");
        String result = createCommunicationEvent(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Create a CommunicationEvent without permission check
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCommunicationEventWithoutPermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        context.put("permission", "false");
        String result = createCommunicationEvent(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Create a CommunicationEvent with or w/o permission check
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCommunicationEvent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        GenericValue role = null;
        List<GenericValue> roles = null;
        GenericValue partyNameView = null;
        GenericValue parentCommEvent = null;
        Map<String, Object> newStat = null;
        GenericValue partyContactMech = null;
        List<GenericValue> partyContactMechs = null;
        Map<String, Object> getEmail = null;
        Map<String, Object> newRole = null;
        GenericValue eventProduct = null;
        GenericValue eventOrder = null;
        GenericValue eventRequest = null;
        Map<String, Object> commRole = null;
        Map<String, Object> contentAssoc = null;
        List<GenericValue> commEventContentAssoc = null;
        if ("FORWARD".equals(context.get("action"))) {
            if (UtilValidate.isNotEmpty(context.get("origCommEventId"))) {
                try {
                    newEntity = EntityQuery.use(delegator)
                            .from("CommunicationEvent")
                            .where(UtilMisc.toMap("communicationEventId", context.get("origCommEventId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                newEntity.remove("communicationEventId");
                newEntity.remove("messageId");
                newEntity.remove("partyIdTo");
                newEntity.put("partyIdFrom", context.get("partyIdFrom"));
                newEntity.put("subject", "Forw: " + ((Map<String, Object>) newEntity).get("subject"));
                newEntity.put("origCommEventId", context.get("origCommEventId"));
            }
        }
        if (UtilValidate.isEmpty(newEntity)) {
            newEntity = delegator.makeValue("CommunicationEvent");
        }
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(context.get("communicationEventId"))) {
            ((GenericValue) newEntity).put("communicationEventId", delegator.getNextSeqId("CommunicationEvent"));
        } else {
            newEntity.put("communicationEventId", context.get("communicationEventId"));
        }
        result.put("communicationEventId", ((Map<String, Object>) newEntity).get("communicationEventId"));
        Object newEntity_communicationEventTypeId = null;
        Object newEntity_partyIdFrom = null;
        Object newEntity_partyIdTo = null;
        Object newEntity_parentCommEventId = null;
        Object newEntity_subject = null;
        Object newEntity_contentMimeTypeId = null;
        Object newEntity_content = null;
        Object newStat_statusId = null;
        if ((!(UtilValidate.isEmpty(context.get("parentCommEventId"))) && ("REPLY".equals(context.get("action")) || "REPLYALL".equals(context.get("action"))))) {
            try {
                parentCommEvent = EntityQuery.use(delegator)
                        .from("CommunicationEvent")
                        .where(UtilMisc.toMap("communicationEventId", context.get("parentCommEventId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                partyNameView = EntityQuery.use(delegator)
                        .from("PartyNameView")
                        .where(UtilMisc.toMap("partyId", ((Map<String, Object>) parentCommEvent).get("partyIdFrom")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyNameView: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            newEntity.put("communicationEventTypeId", ((Map<String, Object>) parentCommEvent).get("communicationEventTypeId"));
            if ("AUTO_EMAIL_COMM".equals(((Map<String, Object>) newEntity).get("communicationEventTypeId"))) {
                newEntity.put("communicationEventTypeId", "EMAIL_COMMUNICATION");
            }
            newEntity.put("partyIdFrom", context.get("partyIdFrom"));
            newEntity.put("partyIdTo", ((Map<String, Object>) parentCommEvent).get("partyIdFrom"));
            newEntity.put("parentCommEventId", ((Map<String, Object>) parentCommEvent).get("communicationEventId"));
            newEntity.put("subject", "RE: " + ((Map<String, Object>) parentCommEvent).get("subject"));
            newEntity.put("contentMimeTypeId", ((Map<String, Object>) parentCommEvent).get("contentMimeTypeId"));
            newEntity.put("content", GroovyUtil.eval("def localContent = parentCommEvent.content;                     if (!localContent) return(\"\");                      resultLine = \"\\n\\n\\n\"                     + (partyNameView.firstName!=null?partyNameView.firstName:\"\")                     + \" \"                     + (partyNameView.middleName!=null?partyNameView.middleName+\" \":\"\")                     + \" \"                     + (partyNameView.lastName!=null?partyNameView.lastName:\"\")                     + (partyNameView.groupName!=null?partyNameView.groupName:\"\")                     + \" wrote:\";                     resultLine += \"\\n -------------------------------------------------------------------- \";                     resultLine += \"\\n> \" + localContent.substring(0, localContent.indexOf(\"\\n\",0) == -1 ? localContent.length() : localContent.indexOf(\"\\n\",0));                     startChar = localContent.indexOf(\"\\n\",0);                     while(startChar != -1 && (startChar = localContent.indexOf(\"\\n\",startChar) + 1) != 0)                     resultLine += \"\\n> \" + localContent.substring(startChar, localContent.indexOf(\"\\n\",startChar)==-1 ? localContent.length() : localContent.indexOf(\"\\n\",startChar));                     return(resultLine);", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context)));
            try {
                roles = EntityQuery.use(delegator)
                        .from("CommunicationEventRole")
                        .where(UtilMisc.toMap("communicationEventId", ((Map<String, Object>) parentCommEvent).get("communicationEventId"), "partyId", ((Map<String, Object>) newEntity).get("partyIdFrom")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying CommunicationEventRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(roles)) {
                role = EntityUtil.getFirst((List<GenericValue>) roles);
                // set-service-fields from "role" to "newStat" for service "setCommunicationEventRoleStatus"
                newStat.putAll(UtilMisc.toMap(role));
                newStat.put("statusId", "COM_ROLE_COMPLETED");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setCommunicationEventRoleStatus", newStat);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling setCommunicationEventRoleStatus: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("statusId"))) {
            newEntity.put("statusId", "COM_ENTERED");
        }
        if ("EMAIL_COMMUNICATION".equals(((Map<String, Object>) newEntity).get("communicationEventTypeId"))) {
            if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("partyIdFrom"))) {
                if (UtilValidate.isNotEmpty(((Map<String, Object>) newEntity).get("contactMechIdFrom"))) {
                    try {
                        partyContactMechs = EntityQuery.use(delegator)
                                .from("PartyAndContactMech")
                                .where(UtilMisc.toMap("contactMechId", ((Map<String, Object>) newEntity).get("contactMechIdFrom"), "contactMechTypeId", "EMAIL_ADDRESS"))
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PartyAndContactMech: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    partyContactMech = EntityUtil.getFirst((List<GenericValue>) partyContactMechs);
                    newEntity.put("partyIdFrom", ((Map<String, Object>) partyContactMech).get("partyId"));
                }
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) newEntity).get("partyIdFrom"))) {
                if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("contactMechIdFrom"))) {
                    getEmail.put("partyId", ((Map<String, Object>) newEntity).get("partyIdFrom"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("getPartyEmail", getEmail);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        newEntity.put("contactMechIdFrom", serviceResult.get("contactMechId"));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling getPartyEmail: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
            if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("partyIdTo"))) {
                if (UtilValidate.isNotEmpty(((Map<String, Object>) newEntity).get("contactMechIdTo"))) {
                    try {
                        partyContactMechs = EntityQuery.use(delegator)
                                .from("PartyAndContactMech")
                                .where(UtilMisc.toMap("contactMechId", ((Map<String, Object>) newEntity).get("contactMechIdTo"), "contactMechTypeId", "EMAIL_ADDRESS"))
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PartyAndContactMech: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    partyContactMech = EntityUtil.getFirst((List<GenericValue>) partyContactMechs);
                    newEntity.put("partyIdTo", ((Map<String, Object>) partyContactMech).get("partyId"));
                }
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) newEntity).get("partyIdTo"))) {
                if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("contactMechIdTo"))) {
                    getEmail.put("partyId", ((Map<String, Object>) newEntity).get("partyIdTo"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("getPartyEmail", getEmail);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        newEntity.put("contactMechIdTo", serviceResult.get("contactMechId"));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling getPartyEmail: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        Timestamp newEntity_entryDate = new Timestamp(System.currentTimeMillis());
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("REPLYALL".equals(context.get("action"))) {
            try {
                roles = EntityQuery.use(delegator)
                        .from("CommunicationEventRole")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying CommunicationEventRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(roles)) {
                if (roles != null) {
                    for (GenericValue role_iter : roles) {
                        role = role_iter;
                        // set-service-fields from "role" to "newRole" for service "createCommunicationEventRole"
                        newRole.putAll(UtilMisc.toMap(role));
                        newRole.put("communicationEventId", ((Map<String, Object>) newEntity).get("communicationEventId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createCommunicationEventRole", newRole);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createCommunicationEventRole: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("productId"))) {
            eventProduct = delegator.makeValue("CommunicationEventProduct");
            eventProduct.put("productId", context.get("productId"));
            eventProduct.put("communicationEventId", ((Map<String, Object>) newEntity).get("communicationEventId"));
            try {
                delegator.create(eventProduct);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(context.get("orderId"))) {
            eventOrder = delegator.makeValue("CommunicationEventOrder");
            eventOrder.put("orderId", context.get("orderId"));
            eventOrder.put("communicationEventId", ((Map<String, Object>) newEntity).get("communicationEventId"));
            try {
                delegator.create(eventOrder);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(context.get("custRequestId"))) {
            eventRequest = delegator.makeValue("CustRequestCommEvent");
            eventRequest.put("custRequestId", context.get("custRequestId"));
            eventRequest.put("communicationEventId", ((Map<String, Object>) newEntity).get("communicationEventId"));
            try {
                delegator.create(eventRequest);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) newEntity).get("partyIdTo"))) {
            commRole.put("communicationEventId", ((Map<String, Object>) newEntity).get("communicationEventId"));
            commRole.put("partyId", ((Map<String, Object>) newEntity).get("partyIdTo"));
            commRole.put("roleTypeId", "ADDRESSEE");
            commRole.put("contactMechId", ((Map<String, Object>) newEntity).get("contactMechIdTo"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createCommunicationEventRoleWithoutPermission", commRole);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createCommunicationEventRoleWithoutPermission: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) newEntity).get("partyIdFrom"))) {
            commRole.put("communicationEventId", ((Map<String, Object>) newEntity).get("communicationEventId"));
            commRole.put("partyId", ((Map<String, Object>) newEntity).get("partyIdFrom"));
            commRole.put("roleTypeId", "ORIGINATOR");
            commRole.put("contactMechId", ((Map<String, Object>) newEntity).get("contactMechIdFrom"));
            commRole.put("statusId", "COM_ROLE_COMPLETED");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createCommunicationEventRoleWithoutPermission", commRole);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createCommunicationEventRoleWithoutPermission: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if ("FORWARD".equals(context.get("action"))) {
            try {
                commEventContentAssoc = EntityQuery.use(delegator)
                        .from("CommEventContentAssoc")
                        .where(UtilMisc.toMap("communicationEventId", context.get("origCommEventId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying CommEventContentAssoc: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (commEventContentAssoc != null) {
                for (GenericValue createcommEventContentAssoc : commEventContentAssoc) {
                    if (UtilValidate.isNotEmpty(createcommEventContentAssoc)) {
                        contentAssoc.put("contentId", ((Map<String, Object>) createcommEventContentAssoc).get("contentId"));
                        contentAssoc.put("communicationEventId", ((Map<String, Object>) newEntity).get("communicationEventId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createCommEventContentAssoc", contentAssoc);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createCommEventContentAssoc: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Update a CommunicationEvent
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCommunicationEvent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object newStatusId = null;
        GenericValue partyContactMech = null;
        List<GenericValue> partyContactMechs = null;
        GenericValue roleFrom = null;
        Map<String, Object> newRoleFrom = null;
        Map<String, Object> newRoleTo = null;
        GenericValue roleTo = null;
        Map<String, Object> newStat = null;
        GenericValue event = null;
        try {
            event = EntityQuery.use(delegator)
                    .from("CommunicationEvent")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (!java.util.Objects.equals(((Map<String, Object>) event).get("statusId"), context.get("statusId"))) {
            newStatusId = context.get("statusId");
            context.put("statusId", ((Map<String, Object>) event).get("statusId"));
        }
        if (UtilValidate.isEmpty(context.get("partyIdTo"))) {
            if (UtilValidate.isNotEmpty(context.get("contactMechIdTo"))) {
                try {
                    partyContactMechs = EntityQuery.use(delegator)
                            .from("PartyAndContactMech")
                            .where(UtilMisc.toMap("contactMechId", context.get("contactMechIdTo"), "contactMechTypeId", "EMAIL_ADDRESS"))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PartyAndContactMech: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                partyContactMech = EntityUtil.getFirst((List<GenericValue>) partyContactMechs);
                context.put("partyIdTo", ((Map<String, Object>) partyContactMech).get("partyId"));
            }
        }
        Object newRoleFrom_partyId = null;
        Object newRoleFrom_contactMechPurposeTypeId = null;
        Object newRoleFrom_contactMechId = null;
        Object newRoleFrom_communicationEventId = null;
        Object newRoleFrom_roleTypeId = null;
        Object parameters_contactMechIdFrom = null;
        if ((!(UtilValidate.isEmpty(context.get("partyIdFrom"))) && !java.util.Objects.equals(context.get("partyIdFrom"), ((Map<String, Object>) event).get("partyIdFrom")))) {
            if (UtilValidate.isNotEmpty(((Map<String, Object>) event).get("partyIdFrom"))) {
                try {
                    roleFrom = EntityQuery.use(delegator)
                            .from("CommunicationEventRole")
                            .where(UtilMisc.toMap("communicationEventId", ((Map<String, Object>) event).get("communicationEventId"), "partyId", ((Map<String, Object>) event).get("partyIdFrom"), "roleTypeId", "ORIGINATOR"))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying CommunicationEventRole: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(roleFrom)) {
                    try {
                        delegator.removeValue(roleFrom);
                    } catch (Exception e) {
                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
            newRoleFrom.put("partyId", context.get("partyIdFrom"));
            newRoleFrom.put("contactMechPurposeTypeId", context.get("contactMechPurposeTypeIdFrom"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getPartyEmail", newRoleFrom);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                newRoleFrom.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling getPartyEmail: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            newRoleFrom.put("communicationEventId", ((Map<String, Object>) event).get("communicationEventId"));
            newRoleFrom.put("roleTypeId", "ORIGINATOR");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createCommunicationEventRole", newRoleFrom);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createCommunicationEventRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            context.put("contactMechIdFrom", ((Map<String, Object>) newRoleFrom).get("contactMechId"));
        }
        Object newRoleTo_partyId = null;
        Object newRoleTo_contactMechId = null;
        Object newRoleTo_communicationEventId = null;
        Object newRoleTo_roleTypeId = null;
        Object parameters_contactMechIdTo = null;
        if ((!(UtilValidate.isEmpty(context.get("partyIdTo"))) && !java.util.Objects.equals(context.get("partyIdTo"), ((Map<String, Object>) event).get("partyIdTo")))) {
            if (UtilValidate.isNotEmpty(((Map<String, Object>) event).get("partyIdTo"))) {
                try {
                    roleTo = EntityQuery.use(delegator)
                            .from("CommunicationEventRole")
                            .where(UtilMisc.toMap("communicationEventId", ((Map<String, Object>) event).get("communicationEventId"), "partyId", ((Map<String, Object>) event).get("partyIdTo"), "roleTypeId", "ADDRESSEE"))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying CommunicationEventRole: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(roleTo)) {
                    try {
                        delegator.removeValue(roleTo);
                    } catch (Exception e) {
                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
            newRoleTo.put("partyId", context.get("partyIdTo"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getPartyEmail", newRoleTo);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                newRoleTo.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling getPartyEmail: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            newRoleTo.put("communicationEventId", ((Map<String, Object>) event).get("communicationEventId"));
            newRoleTo.put("roleTypeId", "ADDRESSEE");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createCommunicationEventRole", newRoleTo);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createCommunicationEventRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            context.put("contactMechIdTo", ((Map<String, Object>) newRoleTo).get("contactMechId"));
        }
        event.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(event);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(newStatusId)) {
            context.put("statusId", newStatusId);
            // set-service-fields from "parameters" to "newStat" for service "setCommunicationEventStatus"
            newStat.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("setCommunicationEventStatus", newStat);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling setCommunicationEventStatus: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Delete a CommunicationEvent
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteCommunicationEvent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> removeContentAndRelatedInmap = null;
        List<GenericValue> relatedToContentassocs = null;
        List<GenericValue> commEvents = null;
        List<GenericValue> contents = null;
        Integer commEventsSize = null;
        GenericValue relatedFromContentassoc = null;
        List<GenericValue> relatedFromContentassocs = null;
        GenericValue event = null;
        try {
            event = EntityQuery.use(delegator)
                    .from("CommunicationEvent")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(event)) {
            return "success";
        }
        // TODO: Convert <remove-related> element
        // TODO: Convert <remove-related> element
        List<GenericValue> contentAssocs = null;
        try {
            contentAssocs = event.getRelated("CommEventContentAssoc", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related CommEventContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(contentAssocs)) {
            if (contentAssocs != null) {
                for (GenericValue contentAssoc : contentAssocs) {
                    try {
                        delegator.removeValue(contentAssoc);
                    } catch (Exception e) {
                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if ("Y".equals(context.get("delContentDataResource"))) {
                        try {
                            contents = contentAssoc.getRelated("FromContent", null, null, false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related FromContent: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (UtilValidate.isNotEmpty(contents)) {
                            if (contents != null) {
                                for (GenericValue content : contents) {
                                    // TODO: Convert <remove-related> element
                                    // TODO: Convert <remove-related> element
                                    try {
                                        relatedFromContentassocs = content.getRelated("FromContentAssoc", null, null, false);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error getting related FromContentAssoc: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                    if (relatedFromContentassocs != null) {
                                        for (GenericValue relatedFromContentassoc_iter : relatedFromContentassocs) {
                                            relatedFromContentassoc = relatedFromContentassoc_iter;
                                            removeContentAndRelatedInmap.put("contentId", ((Map<String, Object>) relatedFromContentassoc).get("contentIdTo"));
                                            try {
                                                Map<String, Object> serviceResult = dispatcher.runSync("removeContentAndRelated", removeContentAndRelatedInmap);
                                                if (ServiceUtil.isError(serviceResult)) {
                                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                                    return "error";
                                                }
                                            } catch (Exception e) {
                                                Debug.logError(e, "Error calling removeContentAndRelated: " + e.getMessage(), MODULE);
                                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                                return "error";
                                            }
                                        }
                                    }
                                    // TODO: Convert <remove-related> element
                                    try {
                                        relatedToContentassocs = content.getRelated("ToContentAssoc", null, null, false);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error getting related ToContentAssoc: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                    if (relatedToContentassocs != null) {
                                        for (GenericValue relatedToContentassoc : relatedToContentassocs) {
                                            removeContentAndRelatedInmap.put("contentId", ((Map<String, Object>) relatedFromContentassoc).get("contentIdFrom"));
                                            try {
                                                Map<String, Object> serviceResult = dispatcher.runSync("removeContentAndRelated", removeContentAndRelatedInmap);
                                                if (ServiceUtil.isError(serviceResult)) {
                                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                                    return "error";
                                                }
                                            } catch (Exception e) {
                                                Debug.logError(e, "Error calling removeContentAndRelated: " + e.getMessage(), MODULE);
                                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                                return "error";
                                            }
                                        }
                                    }
                                    // TODO: Convert <remove-related> element
                                    try {
                                        delegator.removeValue(content);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                    try {
                                        commEvents = EntityQuery.use(delegator)
                                                .from("CommEventContentAssoc")
                                                .where(UtilMisc.toMap("contentId", ((Map<String, Object>) content).get("contentId")))
                                                .queryList();
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error querying CommEventContentAssoc: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                    commEventsSize = (Integer) GroovyUtil.eval("return(commEvents.size())", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                                    if ("1".equals(commEventsSize)) {
                                        removeContentAndRelatedInmap.put("contentId", ((Map<String, Object>) content).get("contentId"));
                                        try {
                                            Map<String, Object> serviceResult = dispatcher.runSync("removeContentAndRelated", removeContentAndRelatedInmap);
                                            if (ServiceUtil.isError(serviceResult)) {
                                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                                return "error";
                                            }
                                        } catch (Exception e) {
                                            Debug.logError(e, "Error calling removeContentAndRelated: " + e.getMessage(), MODULE);
                                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                            return "error";
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        // TODO: Convert <remove-related> element
        try {
            delegator.removeValue(event);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * delete commEvent and workEffort
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteCommunicationEventWorkEffort(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue workEffort = null;
        List<GenericValue> otherComs = null;
        GenericValue event = null;
        try {
            event = EntityQuery.use(delegator)
                    .from("CommunicationEvent")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> workEffortComs = null;
        try {
            workEffortComs = event.getRelated("CommunicationEventWorkEff", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related CommunicationEventWorkEff: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(workEffortComs)) {
            if (workEffortComs != null) {
                for (GenericValue workEffortCom : workEffortComs) {
                    try {
                        delegator.removeValue(workEffortCom);
                    } catch (Exception e) {
                        Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    try {
                        workEffort = workEffortCom.getRelatedOne("WorkEffort", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one WorkEffort: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    try {
                        otherComs = workEffort.getRelated("CommunicationEventWorkEff", null, null, false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related CommunicationEventWorkEff: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isEmpty(otherComs)) {
                        Debug.logInfo("remove workeffort " + ((Map<String, Object>) workEffort).get("workEffortId") + " and related parties and status", MODULE);
                        // TODO: Convert <remove-related> element
                        // TODO: Convert <remove-related> element
                        // TODO: Convert <remove-related> element
                        // TODO: Convert <remove-related> element
                        try {
                            delegator.removeValue(workEffort);
                        } catch (Exception e) {
                            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
            }
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("deleteCommunicationEvent", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling deleteCommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create a CommunicationEventPurpose
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCommunicationEventPurpose(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("CommunicationEventPurpose");
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
     * Remove a CommunicationEventPurpose
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeCommunicationEventPurpose(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue eventPurpose = null;
        try {
            eventPurpose = EntityQuery.use(delegator)
                    .from("CommunicationEventPurpose")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEventPurpose: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(eventPurpose);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create a CommunicationEventRole
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCommunicationEventRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> contactMechs = null;
        GenericValue contactMech = null;
        GenericValue sysUserLogin = null;
        GenericValue newEntity = null;
        Map<String, Object> partyRole = null;
        GenericValue communicationEventType = null;
        GenericValue communicationEvent = null;
        GenericValue communicationEventRole = null;
        try {
            communicationEventRole = EntityQuery.use(delegator)
                    .from("CommunicationEventRole")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEventRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(communicationEventRole)) {
            try {
                sysUserLogin = EntityQuery.use(delegator)
                        .from("UserLogin")
                        .where(UtilMisc.toMap("userLoginId", "system"))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // set-service-fields from "parameters" to "partyRole" for service "ensurePartyRole"
            partyRole.putAll(UtilMisc.toMap(context));
            partyRole.put("userLogin", sysUserLogin);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("ensurePartyRole", partyRole);
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
            newEntity = delegator.makeValue("CommunicationEventRole");
            newEntity.setPKFields((Map<String, Object>) context);
            newEntity.setNonPKFields((Map<String, Object>) context);
            if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("statusId"))) {
                newEntity.put("statusId", "COM_ROLE_CREATED");
            }
            if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("contactMechId"))) {
                try {
                    communicationEvent = EntityQuery.use(delegator)
                            .from("CommunicationEvent")
                            .where(UtilMisc.toMap())
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                try {
                    communicationEventType = communicationEvent.getRelatedOne("CommunicationEventType", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one CommunicationEventType: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(((Map<String, Object>) communicationEventType).get("contactMechTypeId"))) {
                    try {
                        contactMechs = EntityQuery.use(delegator)
                                .from("PartyAndContactMech")
                                .where(UtilMisc.toMap("partyId", ((Map<String, Object>) newEntity).get("partyId"), "contactMechTypeId", ((Map<String, Object>) communicationEventType).get("contactMechTypeId")))
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PartyAndContactMech: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    contactMech = EntityUtil.getFirst((List<GenericValue>) contactMechs);
                    newEntity.put("contactMechId", ((Map<String, Object>) contactMech).get("contactMechId"));
                }
            }
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create a CommunicationEventRole
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCommunicationEventRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue eventRole = null;
        try {
            eventRole = EntityQuery.use(delegator)
                    .from("CommunicationEventRole")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEventRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(eventRole)) {
            eventRole.setNonPKFields((Map<String, Object>) context);
            try {
                delegator.store(eventRole);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Remove a CommunicationEventRole
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeCommunicationEventRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> roles = null;
        Map<String, Object> inMapDel = null;
        GenericValue eventRole = null;
        try {
            eventRole = EntityQuery.use(delegator)
                    .from("CommunicationEventRole")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEventRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(eventRole)) {
            try {
                delegator.removeValue(eventRole);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if ("Y".equals(context.get("deleteCommEventIfLast"))) {
                try {
                    roles = EntityQuery.use(delegator)
                            .from("CommunicationEventRole")
                            .where(UtilMisc.toMap("communicationEventId", ((Map<String, Object>) eventRole).get("communicationEventId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying CommunicationEventRole: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(roles)) {
                    // set-service-fields from "parameters" to "inMapDel" for service "deleteCommunicationEvent"
                    inMapDel.putAll(UtilMisc.toMap(context));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("deleteCommunicationEvent", inMapDel);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling deleteCommunicationEvent: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Checks for email communication events with the status COM_IN_PROGRESS and a startdate which is expired, then send the email
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sendEmailDated(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue communicationEvent = null;
        Map<String, Object> inMap = null;
        Map<String, Object> updCommEventStatus = null;
        List<GenericValue> contactListParties = null;
        Map<String, Object> communicationEventRole = null;
        Timestamp nowDate = new Timestamp(System.currentTimeMillis());
        List<GenericValue> communicationEvents = null;
        try {
            communicationEvents = EntityQuery.use(delegator)
                    .from("CommunicationEvent")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (communicationEvents != null) {
            for (GenericValue communicationEvent_iter : communicationEvents) {
                communicationEvent = communicationEvent_iter;
                // set-service-fields from "communicationEvent" to "inMap" for service "sendCommEventAsEmail"
                inMap.putAll(UtilMisc.toMap(communicationEvent));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("sendCommEventAsEmail", inMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling sendCommEventAsEmail: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        try {
            communicationEvents = EntityQuery.use(delegator)
                    .from("CommunicationEvent")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (communicationEvents != null) {
            for (GenericValue communicationEvent_iter : communicationEvents) {
                communicationEvent = communicationEvent_iter;
                try {
                    contactListParties = EntityQuery.use(delegator)
                            .from("ContactListParty")
                            .where(UtilMisc.toMap("contactListId", ((Map<String, Object>) communicationEvent).get("contactListId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ContactListParty: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                communicationEventRole.put("communicationEventId", ((Map<String, Object>) communicationEvent).get("communicationEventId"));
                communicationEventRole.put("roleTypeId", "ADDRESSEE");
                if (contactListParties != null) {
                    for (GenericValue contactListParty : contactListParties) {
                        communicationEventRole.put("partyId", ((Map<String, Object>) contactListParty).get("partyId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createCommunicationEventRole", communicationEventRole);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createCommunicationEventRole: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
                // set-service-fields from "communicationEvent" to "updCommEventStatus" for service "setCommunicationEventStatus"
                updCommEventStatus.putAll(UtilMisc.toMap(communicationEvent));
                updCommEventStatus.put("statusId", "COM_COMPLETE");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setCommunicationEventStatus", updCommEventStatus);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling setCommunicationEventStatus: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Create CustRequestCommEvent
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustRequestCommEvent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("CustRequestCommEvent");
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
     * Set The Communication Event Status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setCommunicationEventStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue statusChange = null;
        Map<String, Object> updateRole = null;
        List<GenericValue> roles = null;
        GenericValue communicationEventRole = null;
        GenericValue communicationEvent = null;
        try {
            communicationEvent = EntityQuery.use(delegator)
                    .from("CommunicationEvent")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("oldStatusId", ((Map<String, Object>) communicationEvent).get("statusId"));
        if (!java.util.Objects.equals(((Map<String, Object>) communicationEvent).get("statusId"), context.get("statusId"))) {
            try {
                statusChange = EntityQuery.use(delegator)
                        .from("StatusValidChange")
                        .where(UtilMisc.toMap("statusId", ((Map<String, Object>) communicationEvent).get("statusId"), "statusIdTo", context.get("statusId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            GenericValue role = null;
            if (UtilValidate.isEmpty(statusChange)) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyErrorUiLabels", "commeventservices.communication_event_status", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                Debug.logError("Cannot change from " + ((Map<String, Object>) communicationEvent).get("statusId") + " to " + context.get("statusId"), MODULE);
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            } else {
                communicationEvent.put("statusId", context.get("statusId"));
                try {
                    delegator.store(communicationEvent);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if ("COM_COMPLETE".equals(context.get("statusId"))) {
                    if ("Y".equals(context.get("setRoleStatusToComplete"))) {
                        try {
                            roles = communicationEvent.getRelated("CommunicationEventRole", null, null, false);
                        } catch (Exception e) {
                            Debug.logError(e, "Error getting related CommunicationEventRole: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (roles != null) {
                            for (GenericValue roleEntry : roles) {
                                if (!java.util.Objects.equals(((Map<String, Object>) roleEntry).get("statusId"), context.get("COM_ROLE_COMPLETED"))) {
                                    roleEntry.put("statusId", "COM_ROLE_COMPLETED");
                                    try {
                                        delegator.store(roleEntry);
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                        return "error";
                                    }
                                }
                            }
                        }
                    } else {
                        try {
                            communicationEventRole = EntityQuery.use(delegator)
                                    .from("CommunicationEventRole")
                                    .where(UtilMisc.toMap("communicationEventId", ((Map<String, Object>) communicationEvent).get("communicationEventId"), "partyId", ((Map<String, Object>) communicationEvent).get("partyIdFrom"), "roleTypeId", "ORIGINATOR"))
                                    .queryOne();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying CommunicationEventRole: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (UtilValidate.isNotEmpty(communicationEventRole)) {
                            Object comunnicationEventRole = null;
                            if (!"COM_ROLE_COMPLETED".equals(((Map<String, Object>) comunnicationEventRole).get("statusId"))) {
                                // set-service-fields from "communicationEventRole" to "updateRole" for service "updateCommunicationEventRole"
                                updateRole.putAll(UtilMisc.toMap(communicationEventRole));
                                updateRole.put("statusId", "COM_ROLE_COMPLETED");
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("updateCommunicationEventRole", updateRole);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                        return "error";
                                    }
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling updateCommunicationEventRole: " + e.getMessage(), MODULE);
                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                    return "error";
                                }
                            }
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * set the status for a particular party role to the status COM_ROLE_READ
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setCommEventRoleToRead(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue eventRole = null;
        List<GenericValue> communicationEventRoles = null;
        Map<String, Object> updStat = null;
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) context.get("userLogin")).get("partyId"));
        }
        if (UtilValidate.isEmpty(context.get("roleTypeId"))) {
            try {
                communicationEventRoles = EntityQuery.use(delegator)
                        .from("CommunicationEventRole")
                        .where(UtilMisc.toMap("communicationEventId", context.get("communicationEventId"), "partyId", context.get("partyId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying CommunicationEventRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            eventRole = EntityUtil.getFirst((List<GenericValue>) communicationEventRoles);
            context.put("roleTypeId", ((Map<String, Object>) eventRole).get("roleTypeId"));
        } else {
            try {
                eventRole = EntityQuery.use(delegator)
                        .from("CommunicationEventRole")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying CommunicationEventRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(eventRole)) {
            if ("COM_ROLE_CREATED".equals(((Map<String, Object>) eventRole).get("statusId"))) {
                try {
                    userLogin = EntityQuery.use(delegator)
                            .from("UserLogin")
                            .where(UtilMisc.toMap("userLoginId", "system"))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                // set-service-fields from "parameters" to "updStat" for service "setCommunicationEventRoleStatus"
                updStat.putAll(UtilMisc.toMap(context));
                updStat.put("statusId", "COM_ROLE_READ");
                updStat.put("userLogin", userLogin);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setCommunicationEventRoleStatus", updStat);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling setCommunicationEventRoleStatus: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Set The Communication Event Status for a specific role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setCommunicationEventRoleStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue statusChange = null;
        GenericValue communicationEventRole = null;
        try {
            communicationEventRole = EntityQuery.use(delegator)
                    .from("CommunicationEventRole")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEventRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("oldStatusId", ((Map<String, Object>) communicationEventRole).get("statusId"));
        if (!java.util.Objects.equals(((Map<String, Object>) communicationEventRole).get("statusId"), context.get("statusId"))) {
            try {
                statusChange = EntityQuery.use(delegator)
                        .from("StatusValidChange")
                        .where(UtilMisc.toMap("statusId", ((Map<String, Object>) communicationEventRole).get("statusId"), "statusIdTo", context.get("statusId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(statusChange)) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyErrorUiLabels", "commeventservices.communication_event_role_status", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                Debug.logError("Cannot change from " + ((Map<String, Object>) communicationEventRole).get("statusId") + " to " + context.get("statusId"), MODULE);
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            } else {
                communicationEventRole.put("statusId", context.get("statusId"));
                try {
                    delegator.store(communicationEventRole);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Create communication event and send mail to company
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sendContactUsEmailToCompany(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue person = null;
        Map<String, Object> emailParams = null;
        GenericValue productStore = null;
        GenericValue systemUserLogin = null;
        try {
            systemUserLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("firstName"))) {
            if (UtilValidate.isEmpty(context.get("lastName"))) {
                try {
                    person = EntityQuery.use(delegator)
                            .from("Person")
                            .where(UtilMisc.toMap("partyId", context.get("partyIdFrom")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Person: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                context.put("firstName", ((Map<String, Object>) person).get("firstName"));
                context.put("lastName", ((Map<String, Object>) person).get("lastName"));
            }
        }
        Map<String, Object> contactUsMap = new HashMap<>();
        // set-service-fields from "parameters" to "contactUsMap" for service "createCommunicationEventWithoutPermission"
        contactUsMap.putAll(UtilMisc.toMap(context));
        contactUsMap.put("userLogin", systemUserLogin);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCommunicationEventWithoutPermission", contactUsMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCommunicationEventWithoutPermission: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> getPartyEmailMap = new HashMap<>();
        getPartyEmailMap.put("partyId", context.get("partyIdTo"));
        getPartyEmailMap.put("userLogin", systemUserLogin);
        Object emailAddress = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getPartyEmail", getPartyEmailMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            emailAddress = serviceResult.get("emailAddress");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getPartyEmail: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue productStoreEmailSetting = null;
        try {
            productStoreEmailSetting = EntityQuery.use(delegator)
                    .from("ProductStoreEmailSetting")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStoreEmailSetting: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> bodyParameters = new HashMap<>();
        bodyParameters.put("partyId", context.get("partyIdTo"));
        bodyParameters.put("partyIdFrom", context.get("partyIdFrom"));
        bodyParameters.put("email", context.get("emailAddress"));
        bodyParameters.put("firstName", context.get("firstName"));
        bodyParameters.put("lastName", context.get("lastName"));
        bodyParameters.put("postalCode", context.get("postalCode"));
        bodyParameters.put("countryCode", context.get("countryCode"));
        bodyParameters.put("message", context.get("content"));
        if (UtilValidate.isNotEmpty(((Map<String, Object>) productStoreEmailSetting).get("bodyScreenLocation"))) {
            emailParams.put("bodyParameters", bodyParameters);
            emailParams.put("userLogin", systemUserLogin);
            if (UtilValidate.isNotEmpty(emailAddress)) {
                emailParams.put("sendTo", emailAddress);
            } else {
                emailParams.put("sendTo", ((Map<String, Object>) productStoreEmailSetting).get("fromAddress"));
            }
            emailParams.put("subject", ((Map<String, Object>) productStoreEmailSetting).get("subject"));
            emailParams.put("sendFrom", context.get("emailAddress"));
            emailParams.put("contentType", ((Map<String, Object>) productStoreEmailSetting).get("contentType"));
            emailParams.put("bodyScreenUri", ((Map<String, Object>) productStoreEmailSetting).get("bodyScreenLocation"));
            emailParams.put("webSiteId", context.get("webSiteId"));
            emailParams.put("bodyParameters.webSiteId", context.get("webSiteId"));
            emailParams.put("bodyParameters.productStoreId", context.get("productStoreId"));
            emailParams.put("replyTo", context.get("replyTo"));
            emailParams.put("sendAs", ((Map<String, Object>) productStoreEmailSetting).get("sendAs"));
            if (UtilValidate.isNotEmpty(context.get("productStoreId"))) {
                try {
                    productStore = EntityQuery.use(delegator)
                            .from("ProductStore")
                            .where(UtilMisc.toMap("productStoreId", context.get("productStoreId")))
                            .cache()
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ProductStore: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(((Map<String, Object>) productStore).get("defaultLocaleString"))) {
                    emailParams.put("locale", ((Map<String, Object>) productStore).get("defaultLocaleString"));
                }
                if (UtilValidate.isNotEmpty(((Map<String, Object>) productStore).get("storeName"))) {
                    emailParams.put("storeName", ((Map<String, Object>) productStore).get("storeName"));
                }
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("sendMailFromScreen", emailParams);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling sendMailFromScreen: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }

}

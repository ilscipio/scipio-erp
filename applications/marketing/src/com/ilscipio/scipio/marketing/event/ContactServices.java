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
 * <p>Generated from: component://marketing/script/org/ofbiz/sfa/contact/ContactServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ContactServices {

    private static final String MODULE = ContactServices.class.getName();


    /**
     * Create Contact
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContact(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object emailContactMechId = null;
        Map<String, Object> emailAddressCtx = null;
        Object partyId = null;
        Map<String, Object> createPartyRoleCtx = null;
        context.put("roleTypeId", "CONTACT");
        if (!"Y".equals(context.get("quickAdd"))) {
            // TODO: Call simple-method "createPersonRoleAndContactMechs" from "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml"
            // Original: call-simple-method method-name="createPersonRoleAndContactMechs" xml-resource="component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml"
        } else {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: personCtx)
            Map<String, Object> personCtx = new HashMap<>(context);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPerson", personCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                partyId = serviceResult.get("partyId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPerson: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(context.get("roleTypeId"))) {
                createPartyRoleCtx.put("partyId", partyId);
                createPartyRoleCtx.put("roleTypeId", context.get("roleTypeId"));
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
            if (UtilValidate.isNotEmpty(context.get("emailAddress"))) {
                // TODO: Convert call-map-processor (in-map: parameters, out-map: emailAddressCtx)
                // simple-map-processor name: emailAddress
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
                emailAddressCtx.put("partyId", partyId);
                emailAddressCtx.put("contactMechPurposeTypeId", "PRIMARY_EMAIL");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyEmailAddress", emailAddressCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    emailContactMechId = serviceResult.get("contactMechId");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyEmailAddress: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        partyId = context.get("partyId");
        Map<String, Object> partyRelationCtx = new HashMap<>();
        partyRelationCtx.put("partyIdFrom", ((Map<String, Object>) userLogin).get("partyId"));
        partyRelationCtx.put("partyIdTo", partyId);
        partyRelationCtx.put("roleTypeIdTo", context.get("roleTypeId"));
        partyRelationCtx.put("roleTypeIdFrom", "_NA_");
        partyRelationCtx.put("partyRelationshipTypeId", "CONTACT_REL");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyRelationship", partyRelationCtx);
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
        result.put("partyId", partyId);
        result.put("roleTypeId", context.get("roleTypeId"));

        return "success";
    }


    /**
     * Merge two Contacts
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String mergeContacts(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> updatePartyContactMechCtx = null;
        Map<String, Object> deletePartyContactMechCtx = null;
        Map<String, Object> updatePartyCtx = null;
        Object partyIdTo = context.get("partyIdTo");
        Object addrContactMechIdTo = context.get("addrContactMechIdTo");
        Object phoneContactMechIdTo = context.get("phoneContactMechIdTo");
        Object emailContactMechIdTo = context.get("emailContactMechIdTo");
        Object partyId = context.get("partyId");
        Object addrContactMechId = context.get("addrContactMechId");
        Object phoneContactMechId = context.get("phoneContactMechId");
        Object emailContactMechId = context.get("emailContactMechId");
        Object infoString = context.get("infoString");
        if (!java.util.Objects.equals(partyIdTo, partyId)) {
            if ("Y".equals(context.get("useAddress2"))) {
                if (UtilValidate.isNotEmpty(addrContactMechId)) {
                    if (UtilValidate.isNotEmpty(addrContactMechIdTo)) {
                        updatePartyContactMechCtx.put("partyId", partyIdTo);
                        updatePartyContactMechCtx.put("contactMechTypeId", "POSTAL_ADDRESS");
                        updatePartyContactMechCtx.put("contactMechId", addrContactMechIdTo);
                        updatePartyContactMechCtx.put("newContactMechId", addrContactMechId);
                        updatePartyContactMechCtx.put("contactMechPurposeTypeId", "GENERAL_LOCATION");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("updatePartyContactMech", updatePartyContactMechCtx);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling updatePartyContactMech: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                    updatePartyContactMechCtx = null;
                    deletePartyContactMechCtx.put("partyId", partyId);
                    deletePartyContactMechCtx.put("contactMechId", addrContactMechId);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("deletePartyContactMech", deletePartyContactMechCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling deletePartyContactMech: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    deletePartyContactMechCtx = null;
                }
            }
            if ("Y".equals(context.get("useContactNum2"))) {
                if (UtilValidate.isNotEmpty(phoneContactMechId)) {
                    if (UtilValidate.isNotEmpty(phoneContactMechIdTo)) {
                        updatePartyContactMechCtx.put("partyId", partyIdTo);
                        updatePartyContactMechCtx.put("contactMechId", phoneContactMechIdTo);
                        updatePartyContactMechCtx.put("contactMechTypeId", "TELECOM_NUMBER");
                        updatePartyContactMechCtx.put("newContactMechId", phoneContactMechId);
                        updatePartyContactMechCtx.put("contactMechPurposeTypeId", "PRIMARY_PHONE");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("updatePartyContactMech", updatePartyContactMechCtx);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling updatePartyContactMech: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                    updatePartyContactMechCtx = null;
                    deletePartyContactMechCtx.put("partyId", partyId);
                    deletePartyContactMechCtx.put("contactMechId", phoneContactMechId);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("deletePartyContactMech", deletePartyContactMechCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling deletePartyContactMech: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    deletePartyContactMechCtx = null;
                }
            }
            if ("Y".equals(context.get("useEmail2"))) {
                if (UtilValidate.isNotEmpty(emailContactMechId)) {
                    if (UtilValidate.isNotEmpty(emailContactMechIdTo)) {
                        updatePartyContactMechCtx.put("partyId", partyIdTo);
                        updatePartyContactMechCtx.put("contactMechId", emailContactMechIdTo);
                        updatePartyContactMechCtx.put("contactMechTypeId", "EMAIL_ADDRESS");
                        updatePartyContactMechCtx.put("contactMechPurposeTypeId", "PRIMARY_EMAIL");
                        updatePartyContactMechCtx.put("newContactMechId", emailContactMechId);
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("updatePartyContactMech", updatePartyContactMechCtx);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling updatePartyContactMech: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                    deletePartyContactMechCtx.put("partyId", partyId);
                    deletePartyContactMechCtx.put("contactMechId", emailContactMechId);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("deletePartyContactMech", deletePartyContactMechCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling deletePartyContactMech: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
            updatePartyCtx.put("partyId", partyId);
            updatePartyCtx.put("statusId", "PARTY_DISABLED");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("setPartyStatus", updatePartyCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling setPartyStatus: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("partyId", partyIdTo);
            request.setAttribute("partyId", partyIdTo);
        }

        return "success";
    }

}

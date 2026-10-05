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
 * <p>Generated from: component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PartySimpleMethods {

    private static final String MODULE = PartySimpleMethods.class.getName();


    /**
     * Create/Update The AVS Override String
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateAVSOverride(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("PartyIcsAvsOverride");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PartyIcsAvsOverride")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key PartyIcsAvsOverride: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) lookedUpValue).get("partyId"))) {
            lookedUpValue.setNonPKFields((Map<String, Object>) context);
            try {
                delegator.store(lookedUpValue);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) lookedUpValue).get("partyId"))) {
            lookupPKMap.setNonPKFields((Map<String, Object>) context);
            try {
                delegator.create(lookupPKMap);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Delete The AVS Override String
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteAVSOverride(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("PartyIcsAvsOverride");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PartyIcsAvsOverride")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key PartyIcsAvsOverride: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) lookedUpValue).get("partyId"))) {
            try {
                delegator.removeValue(lookedUpValue);
            } catch (Exception e) {
                Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Ensure Party is in _NA_ or the specified Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String ensureNaPartyRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> lookupPKMap = null;
        GenericValue lookedUpValue = null;
        GenericValue partyRole = null;
        if (UtilValidate.isNotEmpty(context.get("partyId"))) {
            lookupPKMap.put("partyId", context.get("partyId"));
        } else {
            if (UtilValidate.isNotEmpty(context.get("partyIdFrom"))) {
                lookupPKMap.put("partyId", context.get("partyIdFrom"));
            } else {
                if (UtilValidate.isNotEmpty(context.get("partyIdTo"))) {
                    lookupPKMap.put("partyId", context.get("partyIdTo"));
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("roleTypeIdFrom"))) {
            lookupPKMap.put("roleTypeId", context.get("roleTypeIdFrom"));
        } else {
            if (UtilValidate.isNotEmpty(context.get("roleTypeIdTo"))) {
                lookupPKMap.put("roleTypeId", context.get("roleTypeIdTo"));
            } else {
                if (UtilValidate.isNotEmpty(context.get("roleTypeId"))) {
                    lookupPKMap.put("roleTypeId", context.get("roleTypeId"));
                } else {
                    lookupPKMap.put("roleTypeId", "_NA_");
                }
            }
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) lookupPKMap).get("partyId"))) {
            try {
                lookedUpValue = EntityQuery.use(delegator)
                        .from("PartyRole")
                        .where(lookupPKMap)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key PartyRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(lookedUpValue)) {
                partyRole = delegator.makeValue("PartyRole");
                try {
                    delegator.create(partyRole);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Creates a person and userlogin
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPersonAndUserLogin(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createUlInMap = new HashMap<>();
        // set-service-fields from "parameters" to "createUlInMap" for service "createUserLogin"
        createUlInMap.putAll(UtilMisc.toMap(context));
        Map<String, Object> createPersonCtx = new HashMap<>();
        // set-service-fields from "parameters" to "createPersonCtx" for service "createPerson"
        createPersonCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPerson", createPersonCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            createUlInMap.put("partyId", serviceResult.get("partyId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPerson: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue createUlInMap_userLogin = null;
        try {
            createUlInMap_userLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createUserLogin", createUlInMap);
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
        GenericValue newUserLogin = null;
        try {
            newUserLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("newUserLogin", newUserLogin);
        result.put("partyId", ((Map<String, Object>) createUlInMap).get("partyId"));

        return "success";
    }


    /**
     * Creates a person, role and contactMechs
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPersonRoleAndContactMechs(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createPartyRoleCtx = new HashMap<>();
        // TODO: Convert call-map-processor (in-map: parameters, out-map: personContext)
        Map<String, Object> personContext = new HashMap<>(context);
        if (UtilValidate.isNotEmpty(context.get("address1"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: postalAddressContext)
        }
        if (UtilValidate.isNotEmpty(context.get("contactNumber"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: telecomNumberContext)
        }
        if (UtilValidate.isNotEmpty(context.get("emailAddress"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: emailAddressContext)
            // simple-map-processor name: emailAddress
            Map<String, Object> emailAddressContext = new HashMap<>();
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object partyId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPerson", (Map<String, Object>) personContext);
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
        Object postalAddContactMechPurpTypeId = context.get("postalAddContactMechPurpTypeId");
        Object contactNumber = context.get("contactNumber");
        Object phoneContactMechPurpTypeId = context.get("phoneContactMechPurpTypeId");
        Object emailAddress = context.get("emailAddress");
        Object emailContactMechPurpTypeId = context.get("emailContactMechPurpTypeId");
        String result = createPartyContactMechs(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Creates a party group, role and contactMechs
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyGroupRoleAndContactMechs(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createPartyRoleCtx = null;
        // TODO: Convert call-map-processor (in-map: parameters, out-map: partyGroupContext)
        if (UtilValidate.isNotEmpty(context.get("address1"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: postalAddressContext)
        }
        if (UtilValidate.isNotEmpty(context.get("contactNumber"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: telecomNumberContext)
        }
        if (UtilValidate.isNotEmpty(context.get("emailAddress"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: emailAddressContext)
            // simple-map-processor name: emailAddress
            Map<String, Object> emailAddressContext = new HashMap<>();
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> partyGroupContext = new HashMap<>();
        partyGroupContext.put("partyTypeId", "PARTY_GROUP");
        Object partyId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyGroup", partyGroupContext);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            partyId = serviceResult.get("partyId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyGroup: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
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
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object postalAddContactMechPurpTypeId = context.get("postalAddContactMechPurpTypeId");
        Object contactNumber = context.get("contactNumber");
        Object phoneContactMechPurpTypeId = context.get("phoneContactMechPurpTypeId");
        Object emailAddress = context.get("emailAddress");
        Object emailContactMechPurpTypeId = context.get("emailContactMechPurpTypeId");
        String result = createPartyContactMechs(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Create Contact Mechs
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyContactMechs(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> postalAddressContext = null;
        Map<String, Object> serviceCtx = null;
        Map<String, Object> telecomNumberContext = null;
        Map<String, Object> emailAddressContext = null;
        if (UtilValidate.isNotEmpty(postalAddressContext)) {
            postalAddressContext.put("partyId", context.get("partyId"));
            postalAddressContext.put("contactMechPurposeTypeId", "GENERAL_LOCATION");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", postalAddressContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                postalAddressContext.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(context.get("postalAddContactMechPurpTypeId"))) {
            // set-service-fields from "postalAddressContext" to "serviceCtx" for service "createPartyContactMechPurpose"
            serviceCtx.putAll(UtilMisc.toMap(postalAddressContext));
            serviceCtx.put("contactMechPurposeTypeId", context.get("postalAddContactMechPurpTypeId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", serviceCtx);
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
        if (UtilValidate.isNotEmpty(telecomNumberContext)) {
            telecomNumberContext.put("partyId", context.get("partyId"));
            telecomNumberContext.put("contactMechPurposeTypeId", "PRIMARY_PHONE");
            if (UtilValidate.isNotEmpty(context.get("phoneContactMechPurpTypeId"))) {
                telecomNumberContext.put("contactMechPurposeTypeId", context.get("phoneContactMechPurpTypeId"));
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", telecomNumberContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(emailAddressContext)) {
            emailAddressContext.put("partyId", context.get("partyId"));
            emailAddressContext.put("contactMechPurposeTypeId", "PRIMARY_EMAIL");
            if (UtilValidate.isNotEmpty(context.get("emailContactMechPurpTypeId"))) {
                emailAddressContext.put("contactMechPurposeTypeId", context.get("emailContactMechPurpTypeId"));
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyEmailAddress", emailAddressContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyEmailAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * delete billing account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteBillingAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        context.put("thruDate", nowTimestamp);
        Map<String, Object> deleteBillingAccountCtx = new HashMap<>();
        // set-service-fields from "parameters" to "deleteBillingAccountCtx" for service "updateBillingAccount"
        deleteBillingAccountCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateBillingAccount", deleteBillingAccountCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateBillingAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }

}

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
 * <p>Generated from: component://party/script/org/ofbiz/party/contact/PartyContactMechServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PartyContactMechServices {

    private static final String MODULE = PartyContactMechServices.class.getName();


    /**
     * Create a PartyContactMech
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyContactMech(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue contactMechType = null;
        Map<String, Object> createContactMechMap = null;
        GenericValue newValue = null;
        newValue = delegator.makeValue("PartyContactMech");
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        List<GenericValue> partyAndContactMechs = null;
        try {
            partyAndContactMechs = EntityQuery.use(delegator)
                    .from("PartyAndContactMech")
                    .where(UtilMisc.toMap("partyId", context.get("partyId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyAndContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (partyAndContactMechs != null) {
            for (GenericValue partyAndContactMech : partyAndContactMechs) {
                try {
                    contactMechType = EntityQuery.use(delegator)
                            .from("ContactMechType")
                            .where(UtilMisc.toMap("contactMechTypeId", ((Map<String, Object>) partyAndContactMech).get("contactMechTypeId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ContactMechType: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (("N".equals(((Map<String, Object>) contactMechType).get("hasTable")) && java.util.Objects.equals(context.get("infoString"), ((Map<String, Object>) partyAndContactMech).get("infoString")) && java.util.Objects.equals(context.get("contactMechTypeId"), ((Map<String, Object>) partyAndContactMech).get("contactMechTypeId")))) {
                    Debug.logInfo("ContactMechId: " + ((Map<String, Object>) partyAndContactMech).get("contactMechId") + " already exists with value: " + ((Map<String, Object>) partyAndContactMech).get("infoString") + " for party: " + context.get("partyId") + " and ContactMechTypeId: " + ((Map<String, Object>) partyAndContactMech).get("contactMechTypeId"), MODULE);
                    result.put("contactMechId", ((Map<String, Object>) partyAndContactMech).get("contactMechId"));
                    return "success";
                }
            }
        }
        if (UtilValidate.isEmpty(context.get("contactMechId"))) {
            // set-service-fields from "parameters" to "createContactMechMap" for service "createContactMech"
            createContactMechMap.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createContactMech", createContactMechMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                newValue.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("ContactMech created", MODULE);
            Debug.logInfo("Creating a PartyContactMech with id: " + ((Map<String, Object>) newValue).get("contactMechId"), MODULE);
        } else {
            newValue.put("contactMechId", context.get("contactMechId"));
            Debug.logInfo("Creating a PartyContactMech with id: " + context.get("contactMechId"), MODULE);
        }
        newValue.put("partyId", context.get("partyId"));
        result.put("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));
        request.setAttribute("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));
        newValue.setNonPKFields((Map<String, Object>) context);
        Timestamp newValue_fromDate = new Timestamp(System.currentTimeMillis());
        try {
            delegator.create(newValue);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Lookup a PartyContactMech (logic from updatePartyContactMech)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String lookupPartyContactMech(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue partyContactMechMap = delegator.makeValue("PartyContactMech");
        partyContactMechMap.setPKFields((Map<String, Object>) context);
        // TODO: Convert <find-by-and> element
        if (UtilValidate.isEmpty(context.get("partyContactMechs"))) {
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyCannotUpdateContactBecauseNotWithSpecifiedParty", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        List<GenericValue> validPartyContactMechs = EntityUtil.filterByDate((List<GenericValue>) context.get("partyContactMechs"));
        GenericValue partyContactMech = EntityUtil.getFirst((List<GenericValue>) validPartyContactMechs);
        if (UtilValidate.isEmpty(partyContactMech)) {
            {
                String errorMsg = UtilProperties.getMessage("PartyErrorUiLabels", "contactmechservices.cannot_update_specified_contact_info_expired", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Update a PartyContactMech
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyContactMech(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue partyContactMech = null;
        GenericValue newPartyContactMech = null;
        Map<String, Object> updateContactMechMap = null;
        GenericValue partyContactMechPurpose = null;
        List<GenericValue> emptyField = null;
        List<GenericValue> partyContactMechPurposes = null;
        Map<String, Object> purposeMap = null;
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        if (UtilValidate.isNotEmpty(context.get("partyContactMech"))) {
            partyContactMech = (GenericValue) context.get("partyContactMech");
        } else {
            String lookupResult = lookupPartyContactMech(request, response);
            if (!"success".equals(lookupResult)) {
                return lookupResult;
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        newPartyContactMech = GenericValue.create((GenericValue) partyContactMech);
        if (UtilValidate.isEmpty(context.get("newContactMechId"))) {
            // set-service-fields from "parameters" to "updateContactMechMap" for service "updateContactMech"
            updateContactMechMap.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateContactMech", updateContactMechMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                newPartyContactMech.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            newPartyContactMech.put("contactMechId", context.get("newContactMechId"));
            Debug.logInfo("Using supplied new contact mech id: " + ((Map<String, Object>) newPartyContactMech).get("contactMechId"), MODULE);
        }
        GenericValue partyContactMechPurposeOld = null;
        if (!java.util.Objects.equals(context.get("contactMechId"), ((Map<String, Object>) newPartyContactMech).get("contactMechId"))) {
            newPartyContactMech.setNonPKFields((Map<String, Object>) context);
            Timestamp newPartyContactMech_fromDate = new Timestamp(System.currentTimeMillis());
            Timestamp partyContactMech_thruDate = new Timestamp(System.currentTimeMillis());
            try {
                delegator.store(partyContactMech);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                delegator.create(newPartyContactMech);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                partyContactMechPurposes = partyContactMech.getRelated("PartyContactMechPurpose", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related PartyContactMechPurpose: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            emptyField = EntityUtil.filterByDate((List<GenericValue>) partyContactMechPurposes);
            if (partyContactMechPurposes != null) {
                for (GenericValue partyContactMechPurposeOldEntry : partyContactMechPurposes) {
                    partyContactMechPurpose = GenericValue.create((GenericValue) partyContactMechPurposeOldEntry);
                    Timestamp partyContactMechPurposeOld_thruDate = new Timestamp(System.currentTimeMillis());
                    try {
                        delegator.store(partyContactMechPurposeOldEntry);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    partyContactMechPurpose.put("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));
                    purposeMap.put("partyId", ((Map<String, Object>) partyContactMechPurpose).get("partyId"));
                    purposeMap.put("contactMechPurposeTypeId", ((Map<String, Object>) partyContactMechPurpose).get("contactMechPurposeTypeId"));
                    purposeMap.put("contactMechId", ((Map<String, Object>) partyContactMechPurpose).get("contactMechId"));
                    // TODO: Convert <find-by-and> element
                    if (UtilValidate.isEmpty(context.get("purposeResult"))) {
                        try {
                            delegator.create(partyContactMechPurpose);
                        } catch (Exception e) {
                            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
            }
            Debug.logInfo("Setting id to result: " + ((Map<String, Object>) newPartyContactMech).get("contactMechId"), MODULE);
            result.put("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));
            request.setAttribute("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));
        } else {
            partyContactMech.setNonPKFields((Map<String, Object>) context);
            try {
                delegator.store(partyContactMech);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Setting id to result: " + ((Map<String, Object>) partyContactMech).get("contactMechId"), MODULE);
            result.put("contactMechId", ((Map<String, Object>) partyContactMech).get("contactMechId"));
            request.setAttribute("contactMechId", ((Map<String, Object>) partyContactMech).get("contactMechId"));
        }

        return "success";
    }


    /**
     * Delete a PartyContactMech only
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deletePartyContactMechOnly(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue partyContactMech = null;
        GenericValue newPartyContactMech = delegator.makeValue("PartyContactMech");
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        GenericValue partyContactMechMap = delegator.makeValue("PartyContactMech");
        partyContactMechMap.setPKFields((Map<String, Object>) context);
        // TODO: Convert <find-by-and> element
        List<GenericValue> validPartyContactMechs = EntityUtil.filterByDate((List<GenericValue>) context.get("partyContactMechs"));
        partyContactMech = EntityUtil.getFirst((List<GenericValue>) validPartyContactMechs);
        if (UtilValidate.isEmpty(partyContactMech)) {
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyContactMechNotFoundCannotDelete", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            return "success";
        }
        if (UtilValidate.isNotEmpty(context.get("nowTimestamp"))) {
            partyContactMech.put("thruDate", context.get("nowTimestamp"));
        } else {
            Timestamp partyContactMech_thruDate = new Timestamp(System.currentTimeMillis());
        }
        try {
            delegator.store(partyContactMech);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete a PartyContactMech and purposes
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deletePartyContactMechAndPurposes(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        String result = deletePartyContactMechOnly(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        List<GenericValue> partyContactMechPurposes = null;
        try {
            partyContactMechPurposes = ((GenericValue) context.get("partyContactMech")).getRelated("PartyContactMechPurpose", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related PartyContactMechPurpose: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> emptyField = EntityUtil.filterByDate((List<GenericValue>) partyContactMechPurposes, nowTimestamp);
        if (partyContactMechPurposes != null) {
            for (GenericValue partyContactMechPurposeOld : partyContactMechPurposes) {
                partyContactMechPurposeOld.put("thruDate", nowTimestamp);
                try {
                    delegator.store(partyContactMechPurposeOld);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Delete a PartyContactMech and purposes
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deletePartyContactMech(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = deletePartyContactMechAndPurposes(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Create a PostalAddress for party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyPostalAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        Map<String, Object> createPostalAddressMap = new HashMap<>();
        // set-service-fields from "parameters" to "createPostalAddressMap" for service "createPostalAddress"
        createPostalAddressMap.putAll(UtilMisc.toMap(context));
        Map<String, Object> newPartyContactMech = new HashMap<>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPostalAddress", createPostalAddressMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            newPartyContactMech.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPostalAddress: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> createPartyContactMechMap = new HashMap<>();
        // set-service-fields from "parameters" to "createPartyContactMechMap" for service "createPartyContactMech"
        createPartyContactMechMap.putAll(UtilMisc.toMap(context));
        createPartyContactMechMap.put("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));
        createPartyContactMechMap.put("contactMechTypeId", "POSTAL_ADDRESS");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMech", createPartyContactMechMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        request.setAttribute("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));
        result.put("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));

        return "success";
    }


    /**
     * Update a PostalAddress for party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyPostalAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        result.put("oldContactMechId", context.get("contactMechId"));
        GenericValue newPartyContactMech = delegator.makeValue("PartyContactMech");
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        Map<String, Object> updatePostalAddressMap = new HashMap<>();
        // set-service-fields from "parameters" to "updatePostalAddressMap" for service "updatePostalAddress"
        updatePostalAddressMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePostalAddress", updatePostalAddressMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            newPartyContactMech.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePostalAddress: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> updatePartyContactMechMap = new HashMap<>();
        // set-service-fields from "parameters" to "updatePartyContactMechMap" for service "updatePartyContactMech"
        updatePartyContactMechMap.putAll(UtilMisc.toMap(context));
        updatePartyContactMechMap.put("newContactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));
        updatePartyContactMechMap.put("contactMechTypeId", "POSTAL_ADDRESS");
        Debug.logInfo("Copied id to updatePartyContactMechMap: " + ((Map<String, Object>) updatePartyContactMechMap).get("newContactMechId"), MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePartyContactMech", updatePartyContactMechMap);
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
        request.setAttribute("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));
        result.put("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));

        return "success";
    }


    /**
     * Create a TelecomNumber for party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyTelecomNumber(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        Debug.logInfo("Creating telecom number", MODULE);
        Map<String, Object> createTelecomNumberMap = new HashMap<>();
        // set-service-fields from "parameters" to "createTelecomNumberMap" for service "createTelecomNumber"
        createTelecomNumberMap.putAll(UtilMisc.toMap(context));
        Map<String, Object> newPartyContactMech = new HashMap<>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createTelecomNumber", createTelecomNumberMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            newPartyContactMech.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createTelecomNumber: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createPartyContactMechMap = new HashMap<>();
        // set-service-fields from "parameters" to "createPartyContactMechMap" for service "createPartyContactMech"
        createPartyContactMechMap.putAll(UtilMisc.toMap(context));
        createPartyContactMechMap.put("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));
        createPartyContactMechMap.put("contactMechTypeId", "TELECOM_NUMBER");
        Debug.logInfo("Copied id to createPartyContactMechMap: " + ((Map<String, Object>) createPartyContactMechMap).get("contactMechId"), MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMech", createPartyContactMechMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        request.setAttribute("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));
        result.put("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));

        return "success";
    }


    /**
     * Update a TelecomNumber for party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyTelecomNumber(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> updateTelecomNumberMap = null;
        GenericValue newPartyContactMech = delegator.makeValue("PartyContactMech");
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        String inlineResult = lookupPartyContactMech(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> updatePartyContactMechMap = new HashMap<>();
        updatePartyContactMechMap.put("partyContactMech", context.get("partyContactMech"));
        Object extensionPresent = (Boolean) GroovyUtil.eval("parameters.containsKey('extension')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        if (Boolean.TRUE.equals(extensionPresent)) {
            Object partyContactMech = null;
            if (!java.util.Objects.equals(context.get("extension"), ((Map<String, Object>) partyContactMech).get("extension"))) {
                updateTelecomNumberMap.put("forceNewRecord", Boolean.TRUE);
            }
        }
        // set-service-fields from "parameters" to "updateTelecomNumberMap" for service "updateTelecomNumber"
        updateTelecomNumberMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateTelecomNumber", updateTelecomNumberMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            newPartyContactMech.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateTelecomNumber: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // set-service-fields from "parameters" to "updatePartyContactMechMap" for service "updatePartyContactMechGiven"
        updatePartyContactMechMap.putAll(UtilMisc.toMap(context));
        updatePartyContactMechMap.put("newContactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));
        updatePartyContactMechMap.put("contactMechTypeId", "TELECOM_NUMBER");
        Debug.logInfo("Copied id to updatePartyContactMechMap: " + ((Map<String, Object>) updatePartyContactMechMap).get("newContactMechId"), MODULE);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePartyContactMechGiven", updatePartyContactMechMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePartyContactMechGiven: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("Setting result id: " + ((Map<String, Object>) newPartyContactMech).get("contactMechId"), MODULE);
        request.setAttribute("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));
        result.put("contactMechId", ((Map<String, Object>) newPartyContactMech).get("contactMechId"));

        return "success";
    }


    /**
     * Create an email address for party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyEmailAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue existsPartyAndContactMech = null;
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        // TODO: Convert <if-validate-method> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        List<GenericValue> partyAndContactMechs = null;
        try {
            partyAndContactMechs = EntityQuery.use(delegator)
                    .from("PartyAndContactMech")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyAndContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> emptyField = EntityUtil.filterByDate((List<GenericValue>) partyAndContactMechs);
        if (UtilValidate.isNotEmpty(partyAndContactMechs)) {
            Debug.logInfo("E-mail address: " + context.get("emailAddress") + " already exists, did not add again..", MODULE);
            existsPartyAndContactMech = EntityUtil.getFirst((List<GenericValue>) partyAndContactMechs);
            result.put("contactMechId", ((Map<String, Object>) existsPartyAndContactMech).get("contactMechId"));
            request.setAttribute("contactMechId", ((Map<String, Object>) existsPartyAndContactMech).get("contactMechId"));
            return "success";
        }
        Map<String, Object> createPartyContactMechMap = new HashMap<>();
        // set-service-fields from "parameters" to "createPartyContactMechMap" for service "createPartyContactMech"
        createPartyContactMechMap.putAll(UtilMisc.toMap(context));
        createPartyContactMechMap.put("infoString", context.get("emailAddress"));
        createPartyContactMechMap.put("contactMechTypeId", "EMAIL_ADDRESS");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMech", createPartyContactMechMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update an email address for party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyEmailAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        // TODO: Convert <if-validate-method> element
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> updatePartyContactMechMap = new HashMap<>();
        // set-service-fields from "parameters" to "updatePartyContactMechMap" for service "updatePartyContactMech"
        updatePartyContactMechMap.putAll(UtilMisc.toMap(context));
        updatePartyContactMechMap.put("infoString", context.get("emailAddress"));
        updatePartyContactMechMap.put("contactMechTypeId", "EMAIL_ADDRESS");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePartyContactMech", updatePartyContactMechMap);
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
        result.put("oldContactMechId", context.get("contactMechId"));

        return "success";
    }


    /**
     * Find partyId from email address
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String findPartyFromEmailAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        String caseInsensitive = null;
        Map<String, Object> input = null;
        input.put("filterByDate", "Y");
        input.put("inputFields.infoString", context.get("address"));
        caseInsensitive = (String) context.get("caseInsensitive");
        if (UtilValidate.isEmpty(caseInsensitive)) {
            caseInsensitive = UtilProperties.getMessage("general.properties", "mail.address.caseInsensitive", locale);
        }
        input.put("inputFields.infoString_ic", caseInsensitive);
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            input.put("filterByDate", "Y");
        } else {
            input.put("filterByDateValue", context.get("fromDate"));
        }
        input.put("inputFields.contactMechPurposeTypeId", "PRIMARY_EMAIL");
        input.put("entityName", "PartyContactDetailByPurpose");
        Map<String, Object> results = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("performFindItem", input);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            results = serviceResult;
        } catch (Exception e) {
            Debug.logError(e, "Error calling performFindItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) results).get("item"))) {
            input.put("entityName", "PartyAndContactMech");
            input.remove("inputFields.contactMechPurposeTypeId");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("performFindItem", input);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                results = serviceResult;
            } catch (Exception e) {
                Debug.logError(e, "Error calling performFindItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) results).get("item"))) {
            result.put("partyId", ((Map<String, Object>) results.get("item")).get("partyId"));
            result.put("contactMechId", ((Map<String, Object>) results.get("item")).get("contactMechId"));
        }

        return "success";
    }


    /**
     * Find partyId from the telephone number
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String findPartyFromTelephone(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue contactMech = null;
        Object partyId = null;
        String telno = null;
        List<GenericValue> contactMechs = null;
        try {
            contactMechs = EntityQuery.use(delegator)
                    .from("PartyAndContactMech")
                    .where(UtilMisc.toMap("contactMechTypeId", "TELECOM_NUMBER"))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyAndContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object dash = "-";
        Object emptyString = context.get("''");
        Object inputTelno = ((Map<String, Object>) context.get("str:replaceAll(parameters")).get("telno, dash, emptyString)");
        if (contactMechs != null) {
            for (GenericValue contactMech_iter : contactMechs) {
                contactMech = contactMech_iter;
                telno = (String) contactMech.get("tnContactNumber");
                if (telno != null) { telno = telno.replace("-", ""); }
                if (java.util.Objects.equals(inputTelno, telno)) {
                    partyId = contactMech.get("partyId");
                }
                telno = "" + contactMech.get("tnAreaCode") + telno;
                if (java.util.Objects.equals(inputTelno, telno)) {
                    partyId = contactMech.get("partyId");
                }
                telno = "" + contactMech.get("tnCountryCode") + telno;
                if (java.util.Objects.equals(inputTelno, telno)) {
                    partyId = contactMech.get("partyId");
                }
                telno = "+" + telno;
                if (java.util.Objects.equals(inputTelno, telno)) {
                    partyId = contactMech.get("partyId");
                }
            }
        }
        if (UtilValidate.isNotEmpty(partyId)) {
            result.put("partyId", partyId);
            result.put("contactMechId", contactMech.get("contactMechId"));
        }

        return "success";
    }


    /**
     * Find partyId from the telephone number
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String findPartyFromTelephoneComplete(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue contactMech = null;
        Object partyId = null;
        String telno = null;
        List<GenericValue> contactMechs = null;
        try {
            contactMechs = EntityQuery.use(delegator)
                    .from("PartyAndContactMech")
                    .where(UtilMisc.toMap("contactMechTypeId", "TELECOM_NUMBER"))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyAndContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object dash = "-";
        Object emptyString = context.get("''");
        Object inputTelno = context.get("telno");
        if (contactMechs != null) {
            for (GenericValue contactMech_iter : contactMechs) {
                contactMech = contactMech_iter;
                telno = (String) contactMech.get("tnContactNumber");
                if (java.util.Objects.equals(inputTelno, telno)) {
                    partyId = contactMech.get("partyId");
                }
                telno = "" + contactMech.get("tnAreaCode") + telno;
                if (java.util.Objects.equals(inputTelno, telno)) {
                    partyId = contactMech.get("partyId");
                }
                telno = "" + contactMech.get("tnCountryCode") + telno;
                if (java.util.Objects.equals(inputTelno, telno)) {
                    partyId = contactMech.get("partyId");
                }
                telno = "+" + telno;
                if (java.util.Objects.equals(inputTelno, telno)) {
                    partyId = contactMech.get("partyId");
                }
            }
        }
        if (UtilValidate.isNotEmpty(partyId)) {
            result.put("partyId", partyId);
            result.put("contactMechId", contactMech.get("contactMechId"));
        }

        return "success";
    }


    /**
     * Create postal address, purposes and set them defaults
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPostalAddressAndPurposes(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object roleTypeId = null;
        GenericValue partyRole = null;
        Map<String, Object> partyProfileDefaultsCtx = null;
        Map<String, Object> serviceContext = null;
        List<GenericValue> pcmpList = null;
        Map<String, Object> serviceInMap = null;
        if (UtilValidate.isNotEmpty(context.get("roleTypeId"))) {
            try {
                partyRole = EntityQuery.use(delegator)
                        .from("PartyRole")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(partyRole)) {
                roleTypeId = context.get("roleTypeId");
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyRoleTypeNotFoundForTheParty", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("contactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object serviceContext_partyId = null;
        Object serviceContext_contactMechPurposeTypeId = null;
        Object partyProfileDefaultsCtx_defaultShipAddr = null;
        Object partyProfileDefaultsCtx_partyId = null;
        Object partyProfileDefaultsCtx_defaultBillAddr = null;
        if ((!(UtilValidate.isEmpty(context.get("setShippingPurpose"))) || !(UtilValidate.isEmpty(context.get("setBillingPurpose"))))) {
            // set-service-fields from "parameters" to "serviceContext" for service "createPartyContactMechPurpose"
            serviceContext.putAll(UtilMisc.toMap(context));
            serviceContext.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
            if ("Y".equals(context.get("setShippingPurpose"))) {
                try {
                    pcmpList = EntityQuery.use(delegator)
                            .from("PartyContactMechPurpose")
                            .where(UtilMisc.toMap("partyId", ((Map<String, Object>) userLogin).get("partyId"), "contactMechPurposeTypeId", "SHIPPING_LOCATION"))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (pcmpList != null) {
                    for (GenericValue pcmp : pcmpList) {
                        // set-service-fields from "pcmp" to "serviceInMap" for service "deletePartyContactMechPurposeIfExists"
                        serviceInMap.putAll(UtilMisc.toMap(pcmp));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("deletePartyContactMechPurposeIfExists", serviceInMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling deletePartyContactMechPurposeIfExists: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        serviceInMap = null;
                    }
                }
                serviceContext.put("contactMechPurposeTypeId", "SHIPPING_LOCATION");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", serviceContext);
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
                // set-service-fields from "parameters" to "partyProfileDefaultsCtx" for service "setPartyProfileDefaults"
                partyProfileDefaultsCtx.putAll(UtilMisc.toMap(context));
                partyProfileDefaultsCtx.put("defaultShipAddr", context.get("contactMechId"));
                partyProfileDefaultsCtx.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setPartyProfileDefaults", partyProfileDefaultsCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling setPartyProfileDefaults: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            if ("Y".equals(context.get("setBillingPurpose"))) {
                try {
                    pcmpList = EntityQuery.use(delegator)
                            .from("PartyContactMechPurpose")
                            .where(UtilMisc.toMap("partyId", ((Map<String, Object>) userLogin).get("partyId"), "contactMechPurposeTypeId", "BILLING_LOCATION"))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (pcmpList != null) {
                    for (GenericValue pcmpEntry : pcmpList) {
                        // set-service-fields from "pcmp" to "serviceInMap" for service "deletePartyContactMechPurposeIfExists"
                        serviceInMap.putAll(UtilMisc.toMap(pcmpEntry));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("deletePartyContactMechPurposeIfExists", serviceInMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling deletePartyContactMechPurposeIfExists: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
                serviceContext.put("contactMechPurposeTypeId", "BILLING_LOCATION");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", serviceContext);
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
                // set-service-fields from "parameters" to "partyProfileDefaultsCtx" for service "setPartyProfileDefaults"
                partyProfileDefaultsCtx.putAll(UtilMisc.toMap(context));
                partyProfileDefaultsCtx.put("defaultBillAddr", context.get("contactMechId"));
                partyProfileDefaultsCtx.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setPartyProfileDefaults", partyProfileDefaultsCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling setPartyProfileDefaults: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Update postal address, purposes and set them defaults
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePostalAddressAndPurposes(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createPartyContactMechMap = null;
        Map<String, Object> updatePostalAddressMap = null;
        Map<String, Object> partyProfileDefaultsCtx = null;
        List<GenericValue> pcmpShipList = null;
        List<GenericValue> pcmpBillList = null;
        Map<String, Object> serviceContext = null;
        List<GenericValue> pcmpList = null;
        Map<String, Object> serviceInMap = null;
        GenericValue partyProfileDefault = null;
        try {
            partyProfileDefault = EntityQuery.use(delegator)
                    .from("PartyProfileDefault")
                    .where(UtilMisc.toMap("partyId", ((Map<String, Object>) userLogin).get("partyId"), "productStoreId", context.get("productStoreId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyProfileDefault: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object parameters_contactMechId = null;
        Object parameters_newContactMechId = null;
        Object createPartyContactMechMap_contactMechId = null;
        Object createPartyContactMechMap_contactMechTypeId = null;
        if ((java.util.Objects.equals(context.get("contactMechId"), ((Map<String, Object>) partyProfileDefault).get("defaultBillAddr")) || java.util.Objects.equals(context.get("contactMechId"), ((Map<String, Object>) partyProfileDefault).get("defaultShipAddr")))) {
            if (!java.util.Objects.equals(((Map<String, Object>) partyProfileDefault).get("defaultBillAddr"), ((Map<String, Object>) partyProfileDefault).get("defaultShipAddr"))) {
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePartyPostalAddress", context);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("contactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePartyPostalAddress: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                // set-service-fields from "parameters" to "updatePostalAddressMap" for service "updatePostalAddress"
                updatePostalAddressMap.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePostalAddress", updatePostalAddressMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("newContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePostalAddress: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (!java.util.Objects.equals(context.get("contactMechId"), context.get("newContactMechId"))) {
                    // set-service-fields from "parameters" to "createPartyContactMechMap" for service "createPartyContactMech"
                    createPartyContactMechMap.putAll(UtilMisc.toMap(context));
                    createPartyContactMechMap.put("contactMechId", context.get("newContactMechId"));
                    createPartyContactMechMap.put("contactMechTypeId", "POSTAL_ADDRESS");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMech", createPartyContactMechMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createPartyContactMech: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                context.put("contactMechId", context.get("newContactMechId"));
            }
        } else {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePartyPostalAddress", context);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling updatePartyPostalAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        Object serviceContext_partyId = null;
        Object serviceContext_contactMechPurposeTypeId = null;
        Object partyProfileDefaultsCtx_defaultShipAddr = null;
        Object partyProfileDefaultsCtx_partyId = null;
        Object partyProfileDefaultsCtx_defaultBillAddr = null;
        if ((!(UtilValidate.isEmpty(context.get("setShippingPurpose"))) || !(UtilValidate.isEmpty(context.get("setBillingPurpose"))))) {
            if ("Y".equals(context.get("setShippingPurpose"))) {
                try {
                    pcmpShipList = EntityQuery.use(delegator)
                            .from("PartyContactMechPurpose")
                            .where(UtilMisc.toMap("partyId", ((Map<String, Object>) userLogin).get("partyId"), "contactMechId", context.get("contactMechId"), "contactMechPurposeTypeId", "SHIPPING_LOCATION"))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(pcmpShipList)) {
                    // set-service-fields from "parameters" to "serviceContext" for service "createPartyContactMechPurpose"
                    serviceContext.putAll(UtilMisc.toMap(context));
                    serviceContext.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                    try {
                        pcmpList = EntityQuery.use(delegator)
                                .from("PartyContactMechPurpose")
                                .where(UtilMisc.toMap("partyId", ((Map<String, Object>) userLogin).get("partyId"), "contactMechPurposeTypeId", "SHIPPING_LOCATION"))
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (pcmpList != null) {
                        for (GenericValue pcmp : pcmpList) {
                            // set-service-fields from "pcmp" to "serviceInMap" for service "deletePartyContactMechPurposeIfExists"
                            serviceInMap.putAll(UtilMisc.toMap(pcmp));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("deletePartyContactMechPurposeIfExists", serviceInMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling deletePartyContactMechPurposeIfExists: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            serviceInMap = null;
                        }
                    }
                    serviceContext.put("contactMechPurposeTypeId", "SHIPPING_LOCATION");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", serviceContext);
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
                    pcmpList = null;
                    serviceContext = null;
                }
                // set-service-fields from "parameters" to "partyProfileDefaultsCtx" for service "setPartyProfileDefaults"
                partyProfileDefaultsCtx.putAll(UtilMisc.toMap(context));
                partyProfileDefaultsCtx.put("defaultShipAddr", context.get("contactMechId"));
                partyProfileDefaultsCtx.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setPartyProfileDefaults", partyProfileDefaultsCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling setPartyProfileDefaults: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            if ("Y".equals(context.get("setBillingPurpose"))) {
                try {
                    pcmpBillList = EntityQuery.use(delegator)
                            .from("PartyContactMechPurpose")
                            .where(UtilMisc.toMap("partyId", ((Map<String, Object>) userLogin).get("partyId"), "contactMechId", context.get("contactMechId"), "contactMechPurposeTypeId", "BILLING_LOCATION"))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(pcmpBillList)) {
                    // set-service-fields from "parameters" to "serviceContext" for service "createPartyContactMechPurpose"
                    serviceContext.putAll(UtilMisc.toMap(context));
                    serviceContext.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                    try {
                        pcmpList = EntityQuery.use(delegator)
                                .from("PartyContactMechPurpose")
                                .where(UtilMisc.toMap("partyId", ((Map<String, Object>) userLogin).get("partyId"), "contactMechPurposeTypeId", "BILLING_LOCATION"))
                                .filterByDate()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (pcmpList != null) {
                        for (GenericValue pcmpEntry : pcmpList) {
                            // set-service-fields from "pcmp" to "serviceInMap" for service "deletePartyContactMechPurposeIfExists"
                            serviceInMap.putAll(UtilMisc.toMap(pcmpEntry));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("deletePartyContactMechPurposeIfExists", serviceInMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling deletePartyContactMechPurposeIfExists: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                    }
                    serviceContext.put("contactMechPurposeTypeId", "BILLING_LOCATION");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", serviceContext);
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
                // set-service-fields from "parameters" to "partyProfileDefaultsCtx" for service "setPartyProfileDefaults"
                partyProfileDefaultsCtx.putAll(UtilMisc.toMap(context));
                partyProfileDefaultsCtx.put("defaultBillAddr", context.get("contactMechId"));
                partyProfileDefaultsCtx.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setPartyProfileDefaults", partyProfileDefaultsCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling setPartyProfileDefaults: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Update postal address, telecom number and purposes
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContactMechAndPurposes(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> updatePartyTelecomNumberCtx = null;
        Map<String, Object> updatePostalAddressAndPurposesCtx = new HashMap<>();
        // set-service-fields from "parameters" to "updatePostalAddressAndPurposesCtx" for service "updatePostalAddressAndPurposes"
        updatePostalAddressAndPurposesCtx.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePostalAddressAndPurposes", updatePostalAddressAndPurposesCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePostalAddressAndPurposes: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(context.get("phoneContactMechId"))) {
            // set-service-fields from "parameters" to "updatePartyTelecomNumberCtx" for service "updatePartyTelecomNumber"
            updatePartyTelecomNumberCtx.putAll(UtilMisc.toMap(context));
            updatePartyTelecomNumberCtx.put("contactMechId", context.get("phoneContactMechId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePartyTelecomNumber", updatePartyTelecomNumberCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updatePartyTelecomNumber: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create and update email address
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdatePartyEmailAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> emailAddressContext = null;
        Object contactMechId = null;
        if (UtilValidate.isEmpty(context.get("contactMechId"))) {
            // set-service-fields from "parameters" to "emailAddressContext" for service "createPartyEmailAddress"
            emailAddressContext.putAll(UtilMisc.toMap(context));
            if (UtilValidate.isEmpty(context.get("partyId"))) {
                emailAddressContext.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyEmailAddress", emailAddressContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                contactMechId = serviceResult.get("contactMechId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyEmailAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Email Contact Created emailContactMechId is " + contactMechId, MODULE);
        } else {
            // set-service-fields from "parameters" to "emailAddressContext" for service "updatePartyEmailAddress"
            emailAddressContext.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePartyEmailAddress", emailAddressContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                contactMechId = serviceResult.get("contactMechId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling updatePartyEmailAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Email Contact updated emailContactMechId is " + contactMechId, MODULE);
        }
        GenericValue contactMech = null;
        try {
            contactMech = EntityQuery.use(delegator)
                    .from("ContactMech")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("emailAddress", ((Map<String, Object>) contactMech).get("infoString"));
        result.put("contactMechId", contactMechId);

        return "success";
    }


    /**
     * Create and update phone number
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdatePartyTelecomNumber(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> phoneContext = null;
        Object contactMechId = null;
        if (UtilValidate.isEmpty(context.get("contactMechId"))) {
            // set-service-fields from "parameters" to "phoneContext" for service "createPartyTelecomNumber"
            phoneContext.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", phoneContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                contactMechId = serviceResult.get("contactMechId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Phone Contact created phoneContactMechId is " + contactMechId, MODULE);
        } else {
            // set-service-fields from "parameters" to "phoneContext" for service "updatePartyTelecomNumber"
            phoneContext.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePartyTelecomNumber", phoneContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                contactMechId = serviceResult.get("contactMechId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling updatePartyTelecomNumber: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Phone Contact updated phoneContactMechId is " + contactMechId, MODULE);
        }
        result.put("contactMechId", contactMechId);

        return "success";
    }


    /**
     * Create or update postal address
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdatePartyPostalAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> postalAddressContext = null;
        Object contactMechId = null;
        if (UtilValidate.isEmpty(context.get("contactMechId"))) {
            // set-service-fields from "parameters" to "postalAddressContext" for service "createPartyPostalAddress"
            postalAddressContext.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", postalAddressContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                contactMechId = serviceResult.get("contactMechId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Postal address created, contactMechId is " + contactMechId, MODULE);
        } else {
            // set-service-fields from "parameters" to "postalAddressContext" for service "updatePartyPostalAddress"
            postalAddressContext.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePartyPostalAddress", postalAddressContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                contactMechId = serviceResult.get("contactMechId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling updatePartyPostalAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Postal address updated, contactMechId is " + contactMechId, MODULE);
        }
        result.put("contactMechId", contactMechId);

        return "success";
    }

}

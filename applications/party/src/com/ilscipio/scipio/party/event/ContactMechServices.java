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
 * <p>Generated from: component://party/script/org/ofbiz/party/contact/ContactMechServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ContactMechServices {

    private static final String MODULE = ContactMechServices.class.getName();


    /**
     * Create Contact Mechanism
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContactMech(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newValue = null;
        newValue = delegator.makeValue("ContactMech");
        if (UtilValidate.isEmpty(context.get("contactMechId"))) {
            ((GenericValue) newValue).put("contactMechId", delegator.getNextSeqId("ContactMech"));
        } else {
            newValue.put("contactMechId", context.get("contactMechId"));
        }
        newValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newValue);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));
        request.setAttribute("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));
        Debug.logInfo("Contact mech created with id " + ((Map<String, Object>) newValue).get("contactMechId"), MODULE);

        return "success";
    }


    /**
     * Update Contact Mechanism
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContactMech(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object successMessageProperty = null;
        GenericValue newValue = null;
        successMessageProperty = "PartyContactMechanismSuccessfullyUpdated";
        if ("EMAIL_ADDRESS".equals(context.get("contactMechTypeId"))) {
            successMessageProperty = "PartyEmailAddressSuccessfullyUpdated";
        }
        if ("WEB_ADDRESS".equals(context.get("contactMechTypeId"))) {
            successMessageProperty = "PartyWebAddressSuccessfullyUpdated";
        }
        if ("IP_ADDRESS".equals(context.get("contactMechTypeId"))) {
            successMessageProperty = "PartyIpAddressSuccessfullyUpdated";
        }
        if ("ELECTRONIC_ADDRESS".equals(context.get("contactMechTypeId"))) {
            successMessageProperty = "PartyElectronicAddressSuccessfullyUpdated";
        }
        if ("DOMAIN_NAME".equals(context.get("contactMechTypeId"))) {
            successMessageProperty = "PartyDomainNameSuccessfullyUpdated";
        }
        GenericValue ContactMechMap = delegator.makeValue("ContactMech");
        ContactMechMap.setPKFields((Map<String, Object>) context);
        GenericValue oldValue = null;
        try {
            oldValue = EntityQuery.use(delegator)
                    .from("ContactMech")
                    .where(ContactMechMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (!java.util.Objects.equals(context.get("infoString"), ((Map<String, Object>) oldValue).get("infoString"))) {
            Debug.logInfo("Contact mech need updating", MODULE);
            newValue = GenericValue.create((GenericValue) oldValue);
            newValue.setNonPKFields((Map<String, Object>) context);
            context.put("contactMechTypeId", context.get("contactMechTypeId"));
            context.put("infoString", context.get("infoString"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createContactMech", context);
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
            result.put("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));
            request.setAttribute("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));
        } else {
            Debug.logInfo("Contact mech not changed", MODULE);
            result.put("contactMechId", ((Map<String, Object>) oldValue).get("contactMechId"));
            request.setAttribute("contactMechId", ((Map<String, Object>) oldValue).get("contactMechId"));
        }

        return "success";
    }


    /**
     * Create Contact Mechanism with PostalAddress
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPostalAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        if ("USA".equals(context.get("countryGeoId"))) {
            if (UtilValidate.isEmpty(context.get("stateProvinceGeoId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyStateInUsMissing", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        if ("CAN".equals(context.get("countryGeoId"))) {
            if (UtilValidate.isEmpty(context.get("stateProvinceGeoId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyProvinceInCanadaMissing", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newValue = delegator.makeValue("PostalAddress");
        context.put("contactMechTypeId", "POSTAL_ADDRESS");
        context.put("contactMechId", context.get("contactMechId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContactMech", context);
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
        newValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newValue);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));
        request.setAttribute("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));

        return "success";
    }


    /**
     * Update Contact Mechanism with PostalAddress
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePostalAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        if ("USA".equals(context.get("countryGeoId"))) {
            if (UtilValidate.isEmpty(context.get("stateProvinceGeoId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyStateInUsMissing", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        if ("CAN".equals(context.get("countryGeoId"))) {
            if (UtilValidate.isEmpty(context.get("stateProvinceGeoId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyProvinceInCanadaMissing", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newValue = delegator.makeValue("PostalAddress");
        newValue.setPKFields((Map<String, Object>) context);
        GenericValue oldValue = null;
        try {
            oldValue = EntityQuery.use(delegator)
                    .from("PostalAddress")
                    .where(newValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key PostalAddress: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        newValue.setNonPKFields((Map<String, Object>) context);
        context.put("contactMechTypeId", "POSTAL_ADDRESS");
        if (!java.util.Objects.equals(oldValue, newValue)) {
            Debug.logInfo("Postal address need updating", MODULE);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createContactMech", context);
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
            try {
                delegator.create(newValue);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            context.put("contactMechId", context.get("contactMechId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateContactMech", context);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                newValue.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (!java.util.Objects.equals(((Map<String, Object>) oldValue).get("contactMechId"), ((Map<String, Object>) newValue).get("contactMechId"))) {
                Debug.logInfo("Postal address need updating, contact mech changed", MODULE);
                try {
                    delegator.create(newValue);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                Debug.logInfo("Postal address unchanged", MODULE);
            }
        }
        request.setAttribute("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));
        result.put("oldContactMechId", context.get("contactMechId"));
        result.put("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));

        return "success";
    }


    /**
     * Create Contact Mechanism with Telecom Number
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createTelecomNumber(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newValue = delegator.makeValue("TelecomNumber");
        context.put("contactMechTypeId", "TELECOM_NUMBER");
        context.put("contactMechId", context.get("contactMechId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContactMech", context);
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
        newValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newValue);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));
        request.setAttribute("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));

        return "success";
    }


    /**
     * Update Contact Mechanism with Telecom Number
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateTelecomNumber(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Boolean doUpdate = null;
        GenericValue newValue = delegator.makeValue("TelecomNumber");
        newValue.setPKFields((Map<String, Object>) context);
        GenericValue oldValue = null;
        try {
            oldValue = EntityQuery.use(delegator)
                    .from("TelecomNumber")
                    .where(newValue)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key TelecomNumber: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        newValue.setNonPKFields((Map<String, Object>) context);
        context.put("contactMechTypeId", "TELECOM_NUMBER");
        doUpdate = Boolean.FALSE;
        if (Boolean.TRUE.equals(context.get("forceNewRecord"))) {
            doUpdate = Boolean.TRUE;
        } else {
            if (!java.util.Objects.equals(oldValue, newValue)) {
                doUpdate = Boolean.TRUE;
            }
        }
        if (Boolean.TRUE.equals(doUpdate)) {
            Debug.logInfo("Telecom number needs updating", MODULE);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createContactMech", context);
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
            try {
                delegator.create(newValue);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            context.put("contactMechId", context.get("contactMechId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateContactMech", context);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                newValue.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (!java.util.Objects.equals(((Map<String, Object>) oldValue).get("contactMechId"), ((Map<String, Object>) newValue).get("contactMechId"))) {
                Debug.logInfo("Telecom Number need updating, contact mech changed", MODULE);
                try {
                    delegator.create(newValue);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                Debug.logInfo("Telecom Number unchanged", MODULE);
            }
        }
        result.put("contactMechId", ((Map<String, Object>) newValue).get("contactMechId"));
        result.put("oldContactMechId", context.get("contactMechId"));

        return "success";
    }


    /**
     * Create an email address contact mechanism
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createEmailAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        context.put("contactMechTypeId", "EMAIL_ADDRESS");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContactMech", context);
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

        return "success";
    }


    /**
     * Update an email address contact mechanism
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateEmailAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        // TODO: Convert call-map-processor (in-map: parameters, out-map: context)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        context.put("contactMechTypeId", "EMAIL_ADDRESS");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateContactMech", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create a link between two ContactMechs, such as PostalAddress and TelecomNumber or email
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContactMechLink(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("ContactMechLink");
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
     * Delete a link between two ContactMechs
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteContactMechLink(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue lookupPKMap = delegator.makeValue("ContactMechLink");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue contactMechLinkInstance = null;
        try {
            contactMechLinkInstance = EntityQuery.use(delegator)
                    .from("ContactMechLink")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key ContactMechLink: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            delegator.removeValue(contactMechLinkInstance);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * createContactMechAttribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContactMechAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("ContactMechAttribute");
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
     * updateContactMechAttribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContactMechAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContactMechAttribute")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactMechAttribute: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
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
     * removeContactMechAttribute
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeContactMechAttribute(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContactMechAttribute")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactMechAttribute: " + e.getMessage(), MODULE);
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
     * Send an email to the person for Verification of his Email Address
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sendVerifyEmailAddressNotification(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> emailParams = null;
        List<GenericValue> productStoreEmailSettings = null;
        try {
            productStoreEmailSettings = EntityQuery.use(delegator)
                    .from("ProductStoreEmailSetting")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStoreEmailSetting: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> lookupHash = new HashMap<>();
        lookupHash.put("emailAddress", context.get("emailAddress"));
        GenericValue emailAddressVerification = null;
        try {
            emailAddressVerification = EntityQuery.use(delegator)
                    .from("EmailAddressVerification")
                    .where(lookupHash)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key EmailAddressVerification: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> bodyParameters = new HashMap<>();
        bodyParameters.put("verifyHash", ((Map<String, Object>) emailAddressVerification).get("verifyHash"));
        GenericValue storeEmail = EntityUtil.getFirst((List<GenericValue>) productStoreEmailSettings);
        if (UtilValidate.isNotEmpty(((Map<String, Object>) storeEmail).get("bodyScreenLocation"))) {
            try {
                Map<String, Object> scriptContext = new HashMap<>();
                scriptContext.put("delegator", delegator);
                scriptContext.put("dispatcher", dispatcher);
                scriptContext.put("locale", locale);
                scriptContext.put("userLogin", userLogin);
                scriptContext.put("context", context);
                scriptContext.put("parameters", context);
                scriptContext.put("request", request);
                scriptContext.put("response", response);
                Object scriptResult = GroovyUtil.eval("context.webSiteId = org.ofbiz.product.store.ProductStoreWorker.getStoreWebSiteIdForEmail(delegator,\n                    context.storeEmail?.productStoreId, null, true);", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            emailParams.put("sendTo", context.get("emailAddress"));
            emailParams.put("subject", ((Map<String, Object>) storeEmail).get("subject"));
            emailParams.put("sendFrom", ((Map<String, Object>) storeEmail).get("fromAddress"));
            emailParams.put("sendCc", ((Map<String, Object>) storeEmail).get("ccAddress"));
            emailParams.put("sendBcc", ((Map<String, Object>) storeEmail).get("bccAddress"));
            emailParams.put("contentType", ((Map<String, Object>) storeEmail).get("contentType"));
            emailParams.put("bodyParameters", bodyParameters);
            emailParams.put("bodyScreenUri", ((Map<String, Object>) storeEmail).get("bodyScreenLocation"));
            Map<String, Object> emailParams_bodyParameters = new HashMap<>((Map<String, Object>) bodyParameters);
            emailParams.put("webSiteId", context.get("webSiteId"));
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


    /**
     * Verify an Email Address through verifyHash and expireDate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String verifyEmailAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Timestamp nowTimestamp = null;
        List<GenericValue> emailAddressVerifications = null;
        try {
            emailAddressVerifications = EntityQuery.use(delegator)
                    .from("EmailAddressVerification")
                    .where(UtilMisc.toMap("verifyHash", context.get("verifyHash")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying EmailAddressVerification: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue emailAddressVerification = EntityUtil.getFirst((List<GenericValue>) emailAddressVerifications);
        if (UtilValidate.isNotEmpty(emailAddressVerification)) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            if (((Map<String, Object>) emailAddressVerification).get("expireDate") != null /* TODO: field compare operator less */) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyEmailAddressVerificationExpired", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        } else {
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyEmailAddressNotExist", locale);
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

}

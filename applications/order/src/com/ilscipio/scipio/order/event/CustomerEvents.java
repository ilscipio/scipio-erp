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
package com.ilscipio.scipio.order.event;

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
 * <p>Generated from: component://order/script/org/ofbiz/order/customer/CustomerEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CustomerEvents {

    private static final String MODULE = CustomerEvents.class.getName();


    /**
     * validateCustomerInfo
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String validateCustomerInfo(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        // TODO: Convert call-map-processor (in-map: parameters, out-map: personMap)
        // simple-map-processor name: newPerson
        Map<String, Object> personMap = new HashMap<>();
        // TODO: Convert call-map-processor (in-map: parameters, out-map: emailMap)
        // simple-map-processor name: newEmail
        Map<String, Object> emailMap = new HashMap<>();
        if ("true".equals(context.get("useAddress"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: addressContext)
            // simple-map-processor name: newAddress
            Map<String, Object> addressContext = new HashMap<>();
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
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: homePhoneMap)
        // simple-map-processor name: newTelecomNumber
        Map<String, Object> homePhoneMap = new HashMap<>();
        // TODO: Convert call-map-processor (in-map: parameters, out-map: workPhoneMap)
        // simple-map-processor name: newTelecomNumber
        Map<String, Object> workPhoneMap = new HashMap<>();

        return "success";
    }


    /**
     * Create Customer
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdateCustomerInfo(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> emailMap = null;
        Map<String, Object> homePhoneMap = null;
        Map<String, Object> workPhoneMap = null;
        Map<String, Object> addressPurposeContext = null;
        Map<String, Object> addressContext = null;
        if (UtilValidate.isNotEmpty(context.get("emailContactMechId"))) {
            emailMap.put("partyId", context.get("partyId"));
            emailMap.put("contactMechId", context.get("emailContactMechId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePartyEmailAddress", emailMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updatePartyEmailAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            if (UtilValidate.isNotEmpty(context.get("emailAddress"))) {
                emailMap.put("partyId", context.get("partyId"));
                emailMap.put("contactMechPurposeTypeId", "PRIMARY_EMAIL");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyEmailAddress", emailMap);
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
        }
        if (UtilValidate.isNotEmpty(context.get("homePhoneContactMechId"))) {
            homePhoneMap.put("partyId", context.get("partyId"));
            homePhoneMap.put("contactMechId", context.get("homePhoneContactMechId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePartyTelecomNumber", homePhoneMap);
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
        } else {
            if (UtilValidate.isNotEmpty(context.get("homeContactNumber"))) {
                homePhoneMap.put("partyId", context.get("partyId"));
                homePhoneMap.put("contactMechPurposeTypeId", "PHONE_HOME");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", homePhoneMap);
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
        }
        if (UtilValidate.isNotEmpty(context.get("workPhoneContactMechId"))) {
            workPhoneMap.put("partyId", context.get("partyId"));
            workPhoneMap.put("contactMechId", context.get("workPhoneContactMechId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePartyTelecomNumber", workPhoneMap);
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
        } else {
            if (UtilValidate.isNotEmpty(context.get("workContactNumber"))) {
                workPhoneMap.put("partyId", context.get("partyId"));
                workPhoneMap.put("contactMechPurposeTypeId", "PHONE_WORK");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", workPhoneMap);
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
        }
        if (UtilValidate.isNotEmpty(context.get("shippingContactMechId"))) {
            addressContext.put("partyId", context.get("partyId"));
            addressContext.put("contactMechId", context.get("shippingContactMechId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePartyPostalAddress", addressContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updatePartyPostalAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            if ("true".equals(context.get("useAddress"))) {
                addressContext.put("partyId", context.get("partyId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", addressContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    addressPurposeContext.put("contactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                addressPurposeContext.put("partyId", ((Map<String, Object>) context.get("tempMap")).get("partyId"));
                addressPurposeContext.put("contactMechPurposeTypeId", "SHIPPING_LOCATION");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", addressPurposeContext);
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
                addressPurposeContext.put("contactMechPurposeTypeId", "GENERAL_LOCATION");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", addressPurposeContext);
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
        }

        return "success";
    }


    /**
     * Create Customer
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustomer(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> ulLookup = null;
        Map<String, Object> loginMap = null;
        Map<String, Object> personMap = new HashMap<>(context);
        String result = validateCustomerInfo(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> tempMap = new HashMap<>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPerson", personMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            tempMap.put("partyId", serviceResult.get("partyId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPerson: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(userLogin)) {
            ulLookup.put("userLoginId", "anonymous");
            try {
                userLogin = EntityQuery.use(delegator)
                        .from("UserLogin")
                        .where(ulLookup)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key UserLogin: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            userLogin.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
            // TODO: Convert <set-current-user-login> element
        } else {
            if ("anonymous".equals(((Map<String, Object>) userLogin).get("userLoginId"))) {
                userLogin.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
            }
        }
        Debug.logInfo("UserLogin : " + userLogin, MODULE);
        Map<String, Object> roleMap = new HashMap<>();
        roleMap.put("roleTypeId", "CUSTOMER");
        roleMap.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyRole", roleMap);
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
        Object partyId = ((Map<String, Object>) tempMap).get("partyId");
        result = createUpdateCustomerInfo(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(context.get("userLoginId"))) {
            loginMap.put("userLoginId", context.get("userLoginId"));
        }

        return "success";
    }


    /**
     * Update Customer
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCustomer(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        if (UtilValidate.isEmpty(context.get("partyId"))) {
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyNoPartyForUpdateCustomer", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        String result = validateCustomerInfo(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> personMap = new HashMap<>();
        personMap.put("partyId", context.get("partyId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePerson", personMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePerson: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object partyId = context.get("partyId");
        result = createUpdateCustomerInfo(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }

}

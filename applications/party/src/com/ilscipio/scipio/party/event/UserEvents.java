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
import org.ofbiz.common.login.LoginServices;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://party/script/org/ofbiz/party/user/UserEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class UserEvents {

    private static final String MODULE = UserEvents.class.getName();


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

        Object require_email = "true";
        Object require_phone = "false";
        Object require_login = "true";
        Object create_allow_password = "true";
        context.put("roleTypeId", "CUSTOMER");
        String result = createUser(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        String successMessage = UtilProperties.getMessage("PartyUiLabels", "CustomerCreatedSuccessfully", locale);
        request.setAttribute("_EVENT_MESSAGE_", successMessage);

        return "success";
    }


    /**
     * Create Prospect
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createProspect(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object require_email = "true";
        Object require_phone = "false";
        Object require_login = "false";
        Object create_allow_password = "true";
        String username_lowercase = UtilProperties.getMessage("security", "username.lowercase", locale);
        String password_lowercase = UtilProperties.getMessage("security", "password.lowercase", locale);
        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        context.put("roleTypeId", "PROSPECT");
        String result = createUser(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        String newProspectMessage = UtilProperties.getMessage("PartyUiLabels", "NewProspect", locale);
        request.setAttribute("_EVENT_MESSAGE_", newProspectMessage);

        return "success";
    }


    /**
     * Create Employee
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createEmployee(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object require_email = "false";
        Object require_phone = "true";
        Object require_login = "true";
        Object create_allow_password = "true";
        String username_lowercase = UtilProperties.getMessage("security", "username.lowercase", locale);
        String password_lowercase = UtilProperties.getMessage("security", "password.lowercase", locale);
        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        context.put("roleTypeId", "EMPLOYEE");
        String result = createUser(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Create User
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUser(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        String tempErrorMessage = null;
        Map<String, Object> addressPurposeContext = null;
        Map<String, Object> homePhonePurposeContext = null;
        Map<String, Object> workPhonePurposeContext = null;
        Map<String, Object> faxPhonePurposeContext = null;
        Map<String, Object> mobilePhonePurposeContext = null;
        Map<String, Object> emailPurposeContext = null;
        Map<String, Object> createProductStoreRoleMap = null;
        String username_lowercase = UtilProperties.getMessage("security", "username.lowercase", locale);
        String password_lowercase = UtilProperties.getMessage("security", "password.lowercase", locale);
        Object default_user_password = "ungssblepsswd";
        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        if (!"true".equals(context.get("create_allow_password"))) {
            context.put("PASSWORD", default_user_password);
            context.put("CONFIRM_PASSWORD", default_user_password);
            context.put("PASSWORD_HINT", "No hint set, account not yet enabled");
        }
        if ("true".equals(username_lowercase)) {
            String parameters_USERNAME = ((String) context.get("USERNAME")).toLowerCase();
        }
        if ("true".equals(password_lowercase)) {
            String parameters_PASSWORD = ((String) context.get("PASSWORD")).toLowerCase();
            String parameters_CONFIRM_PASSWORD = ((String) context.get("CONFIRM_PASSWORD")).toLowerCase();
        }
        if (UtilValidate.isNotEmpty(context.get("USERNAME"))) {
            String result = makeNewUserLogin(request, response);
            if (!"success".equals(result)) {
                return result;
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        } else {
            if ("true".equals(context.get("require_login"))) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyUserNameMissing", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: personContext)
        // simple-map-processor name: newPerson
        Map<String, Object> personContext = new HashMap<>();
        Map<String, Object> partyRoleContext = new HashMap<>();
        Map<String, Object> addressContext = new HashMap<>();
        Map<String, Object> homePhoneContext = new HashMap<>();
        Map<String, Object> workPhoneContext = new HashMap<>();
        Map<String, Object> faxPhoneContext = new HashMap<>();
        Map<String, Object> mobilePhoneContext = new HashMap<>();
        partyRoleContext.put("roleTypeId", context.get("roleTypeId"));
        if ("false".equals(context.get("USE_ADDRESS"))) {
        } else {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: addressContext)
            // simple-map-processor name: newPerson
            addressContext = new HashMap<>();
            if ("USA".equals(context.get("USER_COUNTRY"))) {
                if (UtilValidate.isEmpty(context.get("USER_STATE"))) {
                    tempErrorMessage = UtilProperties.getMessage("PartyUiLabels", "PartyStateInUsMissing", locale);
                    error_list.add(tempErrorMessage);
                }
            }
            if ("CAN".equals(context.get("USER_COUNTRY"))) {
                if (UtilValidate.isEmpty(context.get("USER_STATE"))) {
                    tempErrorMessage = UtilProperties.getMessage("PartyUiLabels", "PartyStateInCanadaMissing", locale);
                    error_list.add(tempErrorMessage);
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("USER_HOME_CONTACT"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: homePhoneContext)
            // simple-map-processor name: newTelecomNumber
            homePhoneContext = new HashMap<>();
        }
        if (UtilValidate.isNotEmpty(context.get("USER_WORK_CONTACT"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: workPhoneContext)
            // simple-map-processor name: newTelecomNumber
            workPhoneContext = new HashMap<>();
        }
        if (UtilValidate.isNotEmpty(context.get("USER_FAX_CONTACT"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: faxPhoneContext)
            // simple-map-processor name: newTelecomNumber
            faxPhoneContext = new HashMap<>();
        }
        if (UtilValidate.isNotEmpty(context.get("USER_MOBILE_CONTACT"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: mobilePhoneContext)
            // simple-map-processor name: newTelecomNumber
            mobilePhoneContext = new HashMap<>();
        }
        if ("true".equals(context.get("require_phone"))) {
            if (UtilValidate.isEmpty(context.get("USER_HOME_CONTACT"))) {
                if (UtilValidate.isEmpty(context.get("USER_WORK_CONTACT"))) {
                    if (UtilValidate.isEmpty(context.get("USER_MOBILE_CONTACT"))) {
                        // TODO: Convert call-map-processor (in-map: parameters, out-map: dummymap)
                        // simple-map-processor name: checkRequiredPhone
                        Map<String, Object> dummymap = new HashMap<>();
                    }
                }
            }
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: emailContext)
        // simple-map-processor name: newEmail
        Map<String, Object> emailContext = new HashMap<>();
        if ("true".equals(context.get("require_email"))) {
            if (UtilValidate.isEmpty(((Map<String, Object>) emailContext).get("emailAddress"))) {
                // TODO: Convert call-map-processor (in-map: emailContext, out-map: dummymap)
                // simple-map-processor name: checkRequiredEmail
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) emailContext).get("emailAddress"))) {
                // TODO: Convert call-map-processor (in-map: emailContext, out-map: dummymap)
                // simple-map-processor name: checkRequiredEmailFormat
            }
        }
        GenericValue partyDataSource = delegator.makeValue("PartyDataSource");
        partyDataSource.put("dataSourceId", "ECOMMERCE_SITE");
        partyDataSource.put("fromDate", nowStamp);
        partyDataSource.put("isCreate", "Y");
        Object visit = request.getSession().getAttribute("visit");
        partyDataSource.put("visitId", ((Map<String, Object>) visit).get("visitId"));
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> tempMap = new HashMap<>();
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPerson", (Map<String, Object>) personContext);
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
        partyDataSource.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        Map<String, Object> userLoginContext = new HashMap<>();
        userLoginContext.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        personContext = new HashMap<>();
        ((Map<String, Object>) personContext).put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        partyRoleContext.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        addressContext = new HashMap<>();
        ((Map<String, Object>) addressContext).put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        homePhoneContext = new HashMap<>();
        ((Map<String, Object>) homePhoneContext).put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        workPhoneContext = new HashMap<>();
        ((Map<String, Object>) workPhoneContext).put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        faxPhoneContext = new HashMap<>();
        ((Map<String, Object>) faxPhoneContext).put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        mobilePhoneContext = new HashMap<>();
        ((Map<String, Object>) mobilePhoneContext).put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        emailContext = new HashMap<>();
        ((Map<String, Object>) emailContext).put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        if (UtilValidate.isNotEmpty(context.get("newUserLogin"))) {
            String result = storeNewUserLogin(request, response);
            if (!"success".equals(result)) {
                return result;
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        try {
            delegator.create(partyDataSource);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyRole", partyRoleContext);
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
        GenericValue purposeTypeId = null;
        if ("false".equals(context.get("USE_ADDRESS"))) {
        } else {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", (Map<String, Object>) addressContext);
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
            addressPurposeContext.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
            if (UtilValidate.isNotEmpty(context.get("USER_ADDR_PURPOSE"))) {
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
                    Object scriptResult = GroovyUtil.eval("context.purposeTypeIdList = (parameters.USER_ADDR_PURPOSE instanceof List) ? parameters.USER_ADDR_PURPOSE : [parameters.USER_ADDR_PURPOSE];", scriptContext);
                } catch (Exception e) {
                    Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
                }
                if (context.get("purposeTypeIdList") != null) {
                    for (Object purposeTypeIdEntry : (List<Object>) context.get("purposeTypeIdList")) {
                        addressPurposeContext.put("contactMechPurposeTypeId", purposeTypeIdEntry);
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
            } else {
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
        if (UtilValidate.isNotEmpty(context.get("USER_HOME_CONTACT"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", (Map<String, Object>) homePhoneContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                homePhonePurposeContext.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            homePhonePurposeContext.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
            homePhonePurposeContext.put("contactMechPurposeTypeId", "PHONE_HOME");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", homePhonePurposeContext);
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
        if (UtilValidate.isNotEmpty(context.get("USER_WORK_CONTACT"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", (Map<String, Object>) workPhoneContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                workPhonePurposeContext.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            workPhonePurposeContext.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
            workPhonePurposeContext.put("contactMechPurposeTypeId", "PHONE_WORK");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", workPhonePurposeContext);
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
        if (UtilValidate.isNotEmpty(context.get("USER_FAX_CONTACT"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", (Map<String, Object>) faxPhoneContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                faxPhonePurposeContext.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            faxPhonePurposeContext.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
            faxPhonePurposeContext.put("contactMechPurposeTypeId", "FAX_NUMBER");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", faxPhonePurposeContext);
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
        if (UtilValidate.isNotEmpty(context.get("USER_MOBILE_CONTACT"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", (Map<String, Object>) mobilePhoneContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                mobilePhonePurposeContext.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            mobilePhonePurposeContext.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
            mobilePhonePurposeContext.put("contactMechPurposeTypeId", "PHONE_MOBILE");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", mobilePhonePurposeContext);
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
        if (UtilValidate.isNotEmpty(context.get("USER_EMAIL"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyEmailAddress", (Map<String, Object>) emailContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                emailPurposeContext.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyEmailAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            emailPurposeContext.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
            emailPurposeContext.put("contactMechPurposeTypeId", "PRIMARY_EMAIL");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMechPurpose", emailPurposeContext);
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
        request.setAttribute("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        if (UtilValidate.isNotEmpty(context.get("PRODUCT_STORE_ID"))) {
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
            createProductStoreRoleMap.put("userLogin", userLogin);
            createProductStoreRoleMap.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
            createProductStoreRoleMap.put("roleTypeId", context.get("roleTypeId"));
            createProductStoreRoleMap.put("productStoreId", context.get("PRODUCT_STORE_ID"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createProductStoreRole", createProductStoreRoleMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createProductStoreRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        emailContext = new HashMap<>();
        ((Map<String, Object>) emailContext).put("partyId", context.get("partyId"));
        emailContext = new HashMap<>();
        ((Map<String, Object>) emailContext).put("productStoreId", context.get("PRODUCT_STORE_ID"));
        Map<String, Object> serviceInMap = new HashMap<>();
        // set-service-fields from "emailContext" to "serviceInMap" for service "sendCreatePartyEmailNotification"
        serviceInMap.putAll(UtilMisc.toMap(emailContext));
        // TODO: Convert <call-service-asynch> element

        return "success";
    }


    /**
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String makeNewUserLogin(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue existingUserLogin = null;
        List<Object> error_list = null;
        String tempErrorMessage = null;
        Map<String, Object> userLoginExistsMap = null;
        // TODO: Convert call-map-processor (in-map: parameters, out-map: userLoginContext)
        // simple-map-processor name: newUserLogin
        Map<String, Object> userLoginContext = new HashMap<>();
        if (UtilValidate.isNotEmpty(((Map<String, Object>) userLoginContext).get("userLoginId"))) {
            userLoginExistsMap.put("userLoginId", ((Map<String, Object>) userLoginContext).get("userLoginId"));
            try {
                existingUserLogin = EntityQuery.use(delegator)
                        .from("UserLogin")
                        .where(userLoginExistsMap)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key UserLogin: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(existingUserLogin)) {
                tempErrorMessage = UtilProperties.getMessage("PartyUiLabels", "PartyUserNameInUse", locale);
                error_list.add(tempErrorMessage);
            }
        }
        GenericValue newUserLogin = delegator.makeValue("UserLogin");
        newUserLogin.put("userLoginId", ((Map<String, Object>) userLoginContext).get("userLoginId"));
        newUserLogin.put("currentPassword", ((Map<String, Object>) userLoginContext).get("currentPassword"));
        newUserLogin.put("passwordHint", ((Map<String, Object>) userLoginContext).get("passwordHint"));
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
            Object scriptResult = GroovyUtil.eval("String password = (String) userLoginContext.get(\"currentPassword\")\n            String confirmPassword = (String) userLoginContext.get(\"currentPasswordVerify\")\n            String passwordHint = (String) userLoginContext.get(\"passwordHint\")\n            org.ofbiz.common.login.LoginServices.checkNewPassword(newUserLogin, null, password, confirmPassword, passwordHint, error_list, true, locale)", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }

        return "success";
    }


    /**
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String storeNewUserLogin(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object emptyField = null;
        delegator = (Delegator) context.get("delegator");
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
            Object scriptResult = GroovyUtil.eval("boolean useEncryption = \"true\".equals(org.ofbiz.entity.util.EntityUtilProperties.getPropertyValue(\"security\", \"password.encrypt\", delegator))\n            if (useEncryption) {\n                String hashType = org.ofbiz.common.login.LoginServices.getHashType()\n                newUserLogin.set(\"currentPassword\", org.ofbiz.base.crypto.HashCrypt.digestHash(hashType, null, (String) newUserLogin.get(\"currentPassword\"))) \n            }", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        GenericValue newUserLoginGv = delegator.makeValue("UserLogin");
        newUserLoginGv.put("partyId", ((Map<String, Object>) context.get("tempMap")).get("partyId"));
        try {
            delegator.create(newUserLoginGv);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (!Boolean.TRUE.equals(context.get("snulSkipSetCurrent"))) {
            // TODO: Convert <set-current-user-login> element
        }
        Object autoPasswordChangeEnabled = null;
        try {
            autoPasswordChangeEnabled = UtilProperties.getPropertyValue("security", "user.auto.change.password.enable");
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilProperties.getPropertyValue: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (java.util.Objects.equals(autoPasswordChangeEnabled, context.get("true"))) {
            try {
                LoginServices.createUserLoginPasswordHistory(delegator, "${newUserLogin.userLoginId}", "${newUserLogin.currentPassword}");
            } catch (Exception e) {
                Debug.logError(e, "Error calling LoginServices.createUserLoginPasswordHistory: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }

}

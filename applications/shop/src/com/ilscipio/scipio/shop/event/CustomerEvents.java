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

import java.math.BigDecimal;
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
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.product.store.ProductStoreWorker;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://shop/script/com/ilscipio/scipio/shop/customer/CustomerEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CustomerEvents {

    private static final String MODULE = CustomerEvents.class.getName();


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

        List<String> error_list = new LinkedList<>();

        Object allowPassword = null;
        Object defaultPassword = null;
        GenericValue existingUserLogin = null;
        String tempErrorMessage = null;
        Map<String, Object> userLoginExistsMap = null;
        Map<String, Object> addressPurposeContext = null;
        Map<String, Object> homePhonePurposeContext = null;
        Map<String, Object> workPhonePurposeContext = null;
        Map<String, Object> faxPhonePurposeContext = null;
        Map<String, Object> mobilePhonePurposeContext = null;
        Map<String, Object> emailPurposeContext = null;
        GenericValue personVo = null;
        Map<String, Object> personLookup = null;
        Map<String, Object> bodyParameters = null;
        Map<String, Object> storeEmailLookup = null;
        GenericValue person = null;
        Map<String, Object> emailParams = null;
        GenericValue storeEmail = null;
        Object productStore = null;
        try {
            productStore = ProductStoreWorker.getProductStore(request);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ProductStoreWorker.getProductStore: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        allowPassword = ((Map<String, Object>) productStore).get("allowPassword");
        defaultPassword = ((Map<String, Object>) productStore).get("defaultPassword");
        if (UtilValidate.isEmpty(allowPassword)) {
            allowPassword = "Y";
        }
        if (UtilValidate.isEmpty(defaultPassword)) {
            defaultPassword = "ungssblepswd";
        }
        String username_lowercase = UtilProperties.getMessage("security", "username.lowercase", locale);
        String password_lowercase = UtilProperties.getMessage("security", "password.lowercase", locale);
        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        context.put("roleTypeId", "CUSTOMER");
        if (!"Y".equals(allowPassword)) {
            context.put("PASSWORD", defaultPassword);
            context.put("CONFIRM_PASSWORD", defaultPassword);
            context.put("PASSWORD_HINT", "No hint set, account not yet enabled");
        }
        if ("Y".equals(((Map<String, Object>) productStore).get("usePrimaryEmailUsername"))) {
            context.put("USERNAME", context.get("CUSTOMER_EMAIL"));
        }
        if ("true".equals(username_lowercase)) {
            String parameters_USERNAME = ((String) context.get("USERNAME")).toLowerCase();
        }
        if ("true".equals(password_lowercase)) {
            String parameters_PASSWORD = ((String) context.get("PASSWORD")).toLowerCase();
            String parameters_CONFIRM_PASSWORD = ((String) context.get("CONFIRM_PASSWORD")).toLowerCase();
        }
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
                error_list.add("${tempErrorMessage}");
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
            Object scriptResult = GroovyUtil.eval("String password = (String) userLoginContext.get(\"currentPassword\");\n            String confirmPassword = (String) userLoginContext.get(\"currentPasswordVerify\");\n            String passwordHint = (String) userLoginContext.get(\"passwordHint\");\n            org.ofbiz.common.login.LoginServices.checkNewPassword(newUserLogin, null, password, confirmPassword, passwordHint, error_list, true, locale);", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: personContext)
        // simple-map-processor name: newPerson
        Map<String, Object> personContext = new HashMap<>();
        Debug.logInfo("Creating new customer, newUserLogin=" + newUserLogin, MODULE);
        Map<String, Object> partyRoleContext = new HashMap<>();
        Map<String, Object> addressContext = new HashMap<>();
        Map<String, Object> homePhoneContext = new HashMap<>();
        Map<String, Object> workPhoneContext = new HashMap<>();
        Map<String, Object> faxPhoneContext = new HashMap<>();
        Map<String, Object> mobilePhoneContext = new HashMap<>();
        Map<String, Object> emailContext = new HashMap<>();
        partyRoleContext.put("roleTypeId", context.get("roleTypeId"));
        if ("false".equals(context.get("USE_ADDRESS"))) {
        } else {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: addressContext)
            // simple-map-processor name: newPerson
            addressContext = new HashMap<>();
            if ("USA".equals(context.get("CUSTOMER_COUNTRY"))) {
                if (UtilValidate.isEmpty(context.get("CUSTOMER_STATE"))) {
                    tempErrorMessage = UtilProperties.getMessage("PartyUiLabels", "PartyStateInUsMissing", locale);
                    error_list.add("${tempErrorMessage}");
                }
            }
            if ("CAN".equals(context.get("CUSTOMER_COUNTRY"))) {
                if (UtilValidate.isEmpty(context.get("CUSTOMER_STATE"))) {
                    tempErrorMessage = UtilProperties.getMessage("PartyUiLabels", "PartyStateInCanadaMissing", locale);
                    error_list.add("${tempErrorMessage}");
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("CUSTOMER_HOME_CONTACT"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: homePhoneContext)
            // simple-map-processor name: newTelecomNumber
            homePhoneContext = new HashMap<>();
        }
        if (UtilValidate.isNotEmpty(context.get("CUSTOMER_WORK_CONTACT"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: workPhoneContext)
            // simple-map-processor name: newTelecomNumber
            workPhoneContext = new HashMap<>();
        }
        if (UtilValidate.isNotEmpty(context.get("CUSTOMER_FAX_CONTACT"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: faxPhoneContext)
            // simple-map-processor name: newTelecomNumber
            faxPhoneContext = new HashMap<>();
        }
        if (UtilValidate.isNotEmpty(context.get("CUSTOMER_MOBILE_CONTACT"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: mobilePhoneContext)
            // simple-map-processor name: newTelecomNumber
            mobilePhoneContext = new HashMap<>();
        }
        if ("true".equals(context.get("REQUIRE_PHONE"))) {
            if (UtilValidate.isEmpty(context.get("CUSTOMER_HOME_CONTACT"))) {
                if (UtilValidate.isEmpty(context.get("CUSTOMER_WORK_CONTACT"))) {
                    if (UtilValidate.isEmpty(context.get("CUSTOMER_MOBILE_CONTACT"))) {
                        // TODO: Convert call-map-processor (in-map: parameters, out-map: dummymap)
                        // simple-map-processor name: checkRequiredPhone
                        Map<String, Object> dummymap = new HashMap<>();
                    }
                }
            }
        }
        if (!"false".equals(context.get("REQUIRE_EMAIL"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: emailContext)
            // simple-map-processor name: newEmail
            emailContext = new HashMap<>();
        } else {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: emailContext)
            // simple-map-processor name: newEmail
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
        Map<String, Object> personUserLoginContext = new HashMap<>();
        // set-service-fields from "personContext" to "personUserLoginContext" for service "createPersonAndUserLogin"
        personUserLoginContext.putAll(UtilMisc.toMap(personContext));
        // set-service-fields from "newUserLogin" to "personUserLoginContext" for service "createPersonAndUserLogin"
        personUserLoginContext.putAll(UtilMisc.toMap(newUserLogin));
        personUserLoginContext.put("currentPasswordVerify", ((Map<String, Object>) newUserLogin).get("currentPassword"));
        Map<String, Object> tempMap = new HashMap<>();
        Object createdUserLogin = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPersonAndUserLogin", personUserLoginContext);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            tempMap.put("partyId", serviceResult.get("partyId"));
            createdUserLogin = serviceResult.get("newUserLogin");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPersonAndUserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert <set-current-user-login> element
        request.setAttribute("createdUserLogin", createdUserLogin);
        partyDataSource.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        userLoginContext = new HashMap<>();
        ((Map<String, Object>) userLoginContext).put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
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
        if (UtilValidate.isNotEmpty(context.get("CUSTOMER_HOME_CONTACT"))) {
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
            homePhonePurposeContext.put("contactMechPurposeTypeId", "PRIMARY_PHONE");
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
        if (UtilValidate.isNotEmpty(context.get("CUSTOMER_WORK_CONTACT"))) {
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
        if (UtilValidate.isNotEmpty(context.get("CUSTOMER_FAX_CONTACT"))) {
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
        if (UtilValidate.isNotEmpty(context.get("CUSTOMER_MOBILE_CONTACT"))) {
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
        if (UtilValidate.isNotEmpty(((Map<String, Object>) emailContext).get("emailAddress"))) {
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
        if (UtilValidate.isNotEmpty(context.get("REQUIRE_CLUB"))) {
            personLookup.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
            try {
                personVo = EntityQuery.use(delegator)
                        .from("Person")
                        .where(personLookup)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key Person: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(context.get("CLUB_NUMBER"))) {
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
                    Object scriptResult = GroovyUtil.eval("clubId = org.ofbiz.party.party.PartyWorker.createClubId(delegator, \"999\", 13);\n                    parameters.put(\"CLUB_NUMBER\", clubId);", scriptContext);
                } catch (Exception e) {
                    Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
                }
            }
            personVo.put("memberId", context.get("CLUB_NUMBER"));
            try {
                delegator.store(personVo);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
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
        Map<String, Object> createProductStoreRoleMap = new HashMap<>();
        createProductStoreRoleMap.put("userLogin", systemUserLogin);
        createProductStoreRoleMap.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        createProductStoreRoleMap.put("roleTypeId", context.get("roleTypeId"));
        createProductStoreRoleMap.put("productStoreId", context.get("emailProductStoreId"));
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
        Map<String, Object> securityParams = new HashMap<>();
        securityParams.put("userLoginId", ((Map<String, Object>) createdUserLogin).get("userLoginId"));
        securityParams.put("groupId", "ECOMMERCE_CUSTOMER");
        securityParams.put("userLogin", systemUserLogin);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("addUserLoginToSecurityGroup", securityParams);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling addUserLoginToSecurityGroup: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(createdUserLogin)) {
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
                Object scriptResult = GroovyUtil.eval("context.convertListResult = org.ofbiz.order.shoppinglist.ShoppingListEvents.convertAnonShoppingListsToRegisteredForStore(request, response, context.createdUserLogin, context.systemUserLogin);", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            if (!"success".equals(context.get("convertListResult"))) {
                {
                    String errorMsg = UtilProperties.getMessage("OrderErrorUiLabels", "shoppinglistevents.error_converting_wishlist_contact_support", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) emailContext).get("emailAddress"))) {
            storeEmailLookup.put("productStoreId", context.get("emailProductStoreId"));
            storeEmailLookup.put("emailType", "PRDS_CUST_REGISTER");
            try {
                storeEmail = EntityQuery.use(delegator)
                        .from("ProductStoreEmailSetting")
                        .where(storeEmailLookup)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key ProductStoreEmailSetting: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) storeEmail).get("bodyScreenLocation"))) {
                try {
                    person = ((GenericValue) createdUserLogin).getRelatedOne("Person", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one Person: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                bodyParameters.put("person", person);
                emailParams.put("bodyParameters", bodyParameters);
                emailParams.put("sendTo", ((Map<String, Object>) emailContext).get("emailAddress"));
                emailParams.put("subject", ((Map<String, Object>) storeEmail).get("subject"));
                emailParams.put("sendFrom", ((Map<String, Object>) storeEmail).get("fromAddress"));
                emailParams.put("sendCc", ((Map<String, Object>) storeEmail).get("ccAddress"));
                emailParams.put("sendBcc", ((Map<String, Object>) storeEmail).get("bccAddress"));
                emailParams.put("contentType", ((Map<String, Object>) storeEmail).get("contentType"));
                emailParams.put("bodyScreenUri", ((Map<String, Object>) storeEmail).get("bodyScreenLocation"));
                emailParams.put("sendAs", ((Map<String, Object>) storeEmail).get("sendAs"));
                emailParams.put("emailType", ((Map<String, Object>) storeEmailLookup).get("emailType"));
                emailParams.put("webSiteId", GroovyUtil.eval("org.ofbiz.webapp.website.WebSiteWorker.getWebSiteId(request)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context)));
                // TODO: Convert <call-service-asynch> element
            }
        }
        String successMessage = UtilProperties.getMessage("CommonUiLabels", "CommonAccountRegistered", locale);
        request.setAttribute("_EVENT_MESSAGE_", successMessage);

        return "success";
    }


    /**
     * Create Customer Success, executed if createCustomer succeeds (SCIPIO)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustomerSuccess(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object allowPassword = null;
        Object productStore = null;
        try {
            productStore = ProductStoreWorker.getProductStore(request);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ProductStoreWorker.getProductStore: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        allowPassword = ((Map<String, Object>) productStore).get("allowPassword");
        if (UtilValidate.isEmpty(allowPassword)) {
            allowPassword = "Y";
        }
        if ("Y".equals(allowPassword)) {
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
                Object scriptResult = GroovyUtil.eval("import org.ofbiz.order.shoppingcart.*;\n                createdUserLogin = request.getAttribute(\"createdUserLogin\");\n                if (!createdUserLogin) {\n                    return;\n                }\n                org.ofbiz.webapp.control.LoginWorker.doBasicLogin(createdUserLogin, request);\n\n                // SCIPIO: Tell login passed _before_ the after-login events, so they can tell\n                request.setAttribute(\"_LOGIN_PASSED_\", \"TRUE\");\n\n                // SCIPIO: Run after-login events (based on doMainLogin)\n                org.ofbiz.webapp.control.RequestHandler.getRequestHandler(request.getServletContext()).runAfterLoginEvents(request, response);\n\n                org.ofbiz.webapp.control.LoginWorker.autoLoginSet(request, response);\n\n                // TODO: REVIEW: Is it really necessary to set the setOrderPartyId here? I'm not sure is done by after-login events, but\n                //  why is it needed to be done here manually at all?\n                CartUpdate cartUpdate = CartUpdate.updateSection(request);\n                try { // SCIPIO\n                    cart = cartUpdate.getCartForUpdate();\n\n                    cart.setOrderPartyId(createdUserLogin.partyId);\n\n                    cart = cartUpdate.commit(cart); // SCIPIO\n                } finally {\n                    cartUpdate.close();\n                }", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
        }

        return "success";
    }


    /**
     * SCIPIO: Check and Update Basic Anon Customer Settings; to be called as a Request Event
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkCreateUpdateAnonUser(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        if ("Y".equals(context.get("createUpdateAnonUser"))) {
            String result = processCustomerSettings(request, response);
            if (!"success".equals(result)) {
                return result;
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Process Customer Settings; to be called as a Request Event
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String processCustomerSettings(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        if (UtilValidate.isEmpty(context.get("partyId"))) {
            String checkResult1 = createAnonymousCustomer(request, response);
            if (!"success".equals(checkResult1)) {
                return checkResult1;
            }
        } else {
            String checkResult2 = updateCustomer(request, response);
            if (!"success".equals(checkResult2)) {
                return checkResult2;
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Debug.logInfo("Setting up party " + ((Map<String, Object>) context.get("tempMap")).get("partyId") + " and shipping address " + context.get("addressPurposeContext") + " in cart", MODULE);
        // TODO: Convert <try> element
        Debug.logInfo("If anonymous, user-login has been activated", MODULE);
        request.setAttribute("_EVENT_MESSAGE_", "");
        result.put("_event_message_", context.get("_event_message_"));
        Object _event_message_list_ = null;
        result.put("_event_message_list_", _event_message_list_);
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
            Object scriptResult = GroovyUtil.eval("groovy:\n            // SCIPIO: Also have to clear the previous request attribs manually because the above is not enough (both required)\n            request.removeAttribute(\"_EVENT_MESSAGE_\");\n            request.removeAttribute(\"_EVENT_MESSAGE_LIST_\");", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }

        return "success";
    }


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

        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        if (UtilValidate.isNotEmpty(context.get("birthDateYear"))) {
            context.put("birthDate", (Long) context.get("birthDateYear") + "-" + context.get("birthDateMonth") + "-" + context.get("birthDateDay"));
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: personMap)
        // simple-map-processor name: newPerson
        Map<String, Object> personMap = new HashMap<>();
        // TODO: Convert call-map-processor (in-map: parameters, out-map: emailMap)
        // simple-map-processor name: newEmail
        Map<String, Object> emailMap = new HashMap<>();
        // TODO: Convert call-map-processor (in-map: parameters, out-map: homePhoneMap)
        // simple-map-processor name: newTelecomNumber
        Map<String, Object> homePhoneMap = new HashMap<>();
        // TODO: Convert call-map-processor (in-map: parameters, out-map: workPhoneMap)
        // simple-map-processor name: newTelecomNumber
        Map<String, Object> workPhoneMap = new HashMap<>();
        // TODO: Convert call-map-processor (in-map: parameters, out-map: quickCallsPhoneMap)
        // simple-map-processor name: newTelecomNumber
        Map<String, Object> quickCallsPhoneMap = new HashMap<>();

        return "success";
    }


    /**
     * Create or Update Customer Info
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


    /**
     * Create Customer
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAnonymousCustomer(HttpServletRequest request, HttpServletResponse response) {
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
        Debug.logInfo("CreatePerson : " + ((Map<String, Object>) tempMap).get("partyId"), MODULE);
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
        Object productStore = null;
        try {
            productStore = ProductStoreWorker.getProductStore(request);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ProductStoreWorker.getProductStore: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
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
        Map<String, Object> createProductStoreRoleMap = new HashMap<>();
        createProductStoreRoleMap.put("userLogin", systemUserLogin);
        createProductStoreRoleMap.put("partyId", ((Map<String, Object>) tempMap).get("partyId"));
        createProductStoreRoleMap.put("roleTypeId", "CUSTOMER");
        createProductStoreRoleMap.put("productStoreId", ((Map<String, Object>) productStore).get("productStoreId"));
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
     * Process Ship Settings; to be called as a Request Event
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String processShipSettings(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = null;
        String tempErrorMessage = null;
        Map<String, Object> addressPurposeContext = null;
        Map<String, Object> addressContext = null;
        // TODO: Convert call-map-processor (in-map: parameters, out-map: addressContext)
        // simple-map-processor name: newAddress
        if ("USA".equals(context.get("countryGeoId"))) {
            if (UtilValidate.isEmpty(context.get("stateProvinceGeoId"))) {
                tempErrorMessage = UtilProperties.getMessage("PartyUiLabels", "PartyStateInUsMissing", locale);
                error_list.add("${tempErrorMessage}");
            }
        }
        if ("CAN".equals(context.get("countryGeoId"))) {
            if (UtilValidate.isEmpty(context.get("stateProvinceGeoId"))) {
                tempErrorMessage = UtilProperties.getMessage("PartyUiLabels", "PartyStateInCanadaMissing", locale);
                error_list.add("${tempErrorMessage}");
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
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
                addressContext.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling updatePartyPostalAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            addressContext.put("partyId", context.get("partyId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", addressContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                addressContext.put("contactMechId", serviceResult.get("contactMechId"));
                addressPurposeContext.put("contactMechId", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            addressPurposeContext.put("partyId", context.get("partyId"));
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
        // TODO: Convert <try> element

        return "success";
    }


    /**
     * Process Ship Options; to be called as a Request Event
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String processShipOptions(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

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
            Object scriptResult = GroovyUtil.eval("shipmentMethod = parameters.get(\"shipping_method\");\n           if(shipmentMethod != null){\n              parameters.put(\"shipmentMethodTypeId\", shipmentMethod.substring(0, shipmentMethod.indexOf(\"@\")));\n              parameters.put(\"carrierPartyId\", shipmentMethod.substring(shipmentMethod.indexOf(\"@\")+1));\n           }", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        // TODO: Convert <try> element

        return "success";
    }


    /**
     * Create and update a user login
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdateUserLogin(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object emptyField = null;
        Object productStore = null;
        GenericValue existingUserLogin = null;
        Map<String, Object> userLoginCtx = null;
        Map<String, Object> passwordCtx = null;
        Object updatedUserLogin = null;
        GenericValue newUserLogin = null;
        Object user = request.getSession().getAttribute("userLogin");
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
            Object scriptResult = GroovyUtil.eval("cart = org.ofbiz.order.shoppingcart.ShoppingCartEvents.getCartObject(request); // SCIPIO\n            context.cart = cart;", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        Object cart = context.get("cart");
        if (("anonymous".equals(userLogin != null ? userLogin.getString("userLoginId") : null) && "anonymous".equals(userLogin != null ? userLogin.getString("userLoginId") : null))) {
            userLogin = null;
            request.getSession().removeAttribute("userLogin");
        }
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            Object parameters_partyId = request.getAttribute("partyId");
        }
        if (UtilValidate.isEmpty(context.get("username"))) {
            try {
                productStore = ProductStoreWorker.getProductStore(request);
            } catch (Exception e) {
                Debug.logError(e, "Error calling ProductStoreWorker.getProductStore: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if ("Y".equals(((Map<String, Object>) productStore).get("usePrimaryEmailUsername"))) {
                context.put("username", context.get("emailAddress"));
            }
        }
        if (UtilValidate.isEmpty(userLogin)) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: userLoginMap)
            Object userLoginMap = null;
            if (UtilValidate.isNotEmpty(((Map<String, Object>) userLoginMap).get("userLoginId"))) {
                try {
                    existingUserLogin = EntityQuery.use(delegator)
                            .from("UserLogin")
                            .where(UtilMisc.toMap("userLoginId", "${userLoginMap.userLoginId}"))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(existingUserLogin)) {
                    {
                        String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyUserNameInUse", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
            }
            newUserLogin = delegator.makeValue("UserLogin");
            newUserLogin.put("userLoginId", ((Map<String, Object>) userLoginMap).get("userLoginId"));
            newUserLogin.put("currentPassword", ((Map<String, Object>) userLoginMap).get("currentPassword"));
            newUserLogin.put("passwordHint", ((Map<String, Object>) userLoginMap).get("passwordHint"));
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
                Object scriptResult = GroovyUtil.eval("String password = (String) userLoginMap.get(\"currentPassword\");\n                String confirmPassword = (String) userLoginMap.get(\"currentPasswordVerify\");\n                String passwordHint = (String) userLoginMap.get(\"passwordHint\");\n                org.ofbiz.common.login.LoginServices.checkNewPassword(newUserLogin, null, password, confirmPassword, passwordHint, error_list, true, locale);", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            // set-service-fields from "userLoginMap" to "userLoginCtx" for service "createUserLogin"
            userLoginCtx.putAll(UtilMisc.toMap(userLoginMap));
            GenericValue userLoginCtx_userLogin = null;
            try {
                userLoginCtx_userLogin = EntityQuery.use(delegator)
                        .from("UserLogin")
                        .where(UtilMisc.toMap("userLoginId", "system"))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createUserLogin", userLoginCtx);
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
            try {
                userLogin = EntityQuery.use(delegator)
                        .from("UserLogin")
                        .where(UtilMisc.toMap("userLoginId", ((Map<String, Object>) userLoginMap).get("userLoginId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
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
                Object scriptResult = GroovyUtil.eval("org.ofbiz.webapp.control.LoginWorker.doBasicLogin(userLogin, request);\n                    org.ofbiz.webapp.control.LoginWorker.autoLoginSet(request, response);", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            context.put("userLogin", userLogin);
        } else {
            if ((!(UtilValidate.isEmpty(context.get("currentPassword"))) || !(UtilValidate.isEmpty(context.get("newPassword"))) || !(UtilValidate.isEmpty(context.get("newPasswordVerify"))))) {
                // TODO: Convert call-map-processor (in-map: parameters, out-map: passwordMap)
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
                // set-service-fields from "passwordMap" to "passwordCtx" for service "updatePassword"
                passwordCtx.putAll(UtilMisc.toMap(context.get("passwordMap")));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePassword", passwordCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    updatedUserLogin = serviceResult.get("updatedUserLogin");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePassword: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                // TODO: Convert <set-current-user-login> element
                userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
                if (updatedUserLogin != null && userLogin != null && java.util.Objects.equals(((Map<String, Object>) updatedUserLogin).get("userLoginId"), userLogin.get("userLoginId"))) {
                    request.getSession().setAttribute("userLogin", updatedUserLogin);
                }
                userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
            }
        }

        return "success";
    }


    /**
     * Get shipping options
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getShipOptions(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object chosenShippingMethod = null;
        Map<String, Object> shippingOptionMap = null;
        BigDecimal negValue = null;
        BigDecimal shippingEst = null;
        Object shippingMethod = null;
        Object shippingDesc = null;
        String calcOfflineLabel = null;
        List<Object> shippingOptions = null;
        dispatcher = (LocalDispatcher) context.get("dispatcher");
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
            Object scriptResult = GroovyUtil.eval("import org.ofbiz.order.shoppingcart.shipping.ShippingEstimateWrapper;\n                shoppingCart = org.ofbiz.order.shoppingcart.ShoppingCartEvents.getCartObject(request); // SCIPIO\n                context.shoppingCart = shoppingCart;\n                shippingEstWpr = ShippingEstimateWrapper.getWrapper(dispatcher, shoppingCart, 0);\n                parameters.put(\"shippingEstWpr\", shippingEstWpr);", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        Object shippingEstWpr = context.get("shippingEstWpr");
        Object carrierShipmentMethodList = null;
        try { carrierShipmentMethodList = shippingEstWpr.getClass().getMethod("getShippingMethods").invoke(shippingEstWpr); } catch (Exception e) { Debug.logError(e, MODULE); }
        Object shoppingCartForShip = context.get("shoppingCart");
        Object shipmentMethodTypeId = null;
        Object carrierPartyId = null;
        Object currency = null;
        try {
            if (shoppingCartForShip != null) {
                shipmentMethodTypeId = shoppingCartForShip.getClass().getMethod("getShipmentMethodTypeId").invoke(shoppingCartForShip);
                carrierPartyId = shoppingCartForShip.getClass().getMethod("getCarrierPartyId").invoke(shoppingCartForShip);
                currency = shoppingCartForShip.getClass().getMethod("getCurrency").invoke(shoppingCartForShip);
            }
        } catch (Exception e) { Debug.logError(e, MODULE); }
        if (UtilValidate.isNotEmpty(shipmentMethodTypeId)) {
            if (UtilValidate.isNotEmpty(carrierPartyId)) {
                chosenShippingMethod = shipmentMethodTypeId + "@" + carrierPartyId;
            }
        }
        if (carrierShipmentMethodList != null) {
            for (Object carrierShipmentMethod : (List<Object>) carrierShipmentMethodList) {
                try { shippingEst = (BigDecimal) shippingEstWpr.getClass().getMethod("getShippingEstimate", org.ofbiz.entity.GenericValue.class).invoke(shippingEstWpr, carrierShipmentMethod); } catch (Exception e) { Debug.logError(e, MODULE); }
                if (UtilValidate.isEmpty(shippingEst)) {
                    shippingEst = new BigDecimal("-1");
                }
                negValue = new BigDecimal("-1");
                shippingMethod = ((Map<String, Object>) carrierShipmentMethod).get("shipmentMethodTypeId") + "@" + ((Map<String, Object>) carrierShipmentMethod).get("partyId");
                if (shippingEst != null /* TODO: field compare operator greater */) {
                    shippingDesc = ((Map<String, Object>) carrierShipmentMethod).get("description") + " - " + context.get("shippingEst?currency(${currency") + ")}";
                } else {
                    calcOfflineLabel = UtilProperties.getMessage("OrderUiLabels", "OrderCalculatedOffline", locale);
                    if ("NO_SHIPPING".equals(((Map<String, Object>) carrierShipmentMethod).get("shipmentMethodTypeId"))) {
                        shippingDesc = ((Map<String, Object>) carrierShipmentMethod).get("description");
                    } else {
                        shippingDesc = ((Map<String, Object>) carrierShipmentMethod).get("description") + " - " + calcOfflineLabel;
                    }
                }
                if (!"_NA_".equals(((Map<String, Object>) carrierShipmentMethod).get("partyId"))) {
                    shippingDesc = ((Map<String, Object>) carrierShipmentMethod).get("partyId") + " " + shippingDesc;
                }
                shippingOptionMap.put("shippingMethod", shippingMethod);
                shippingOptionMap.put("shippingDesc", shippingDesc);
                if (UtilValidate.isNotEmpty(((Map<String, Object>) carrierShipmentMethod).get("productStoreShipMethId"))) {
                    shippingOptionMap.put("productStoreShipMethId", ((Map<String, Object>) carrierShipmentMethod).get("productStoreShipMethId"));
                }
                shippingOptions.add(shippingOptionMap);
                shippingOptionMap = null;
            }
        }
        context.put("shippingOptions", shippingOptions);
        request.setAttribute("shippingOptions", context.get("shippingOptions"));

        return "success";
    }


    /**
     * Set shipping method
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setShippingOption(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object shippingDescription = null;
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
            Object scriptResult = GroovyUtil.eval("String shippingMethod = parameters.get(\"shipMethod\");\n            String carrierPartyId = \"\";\n            if(shippingMethod != null) {\n                shipmentMethodTypeId = shippingMethod.substring(0, shippingMethod.indexOf(\"@\"));\n                if (shippingMethod.indexOf(\":\") != -1) {\n                    carrierPartyId = shippingMethod.substring(shippingMethod.indexOf(\"@\")+1, shippingMethod.indexOf(\":\"));\n                    productStoreShipMethId =  shippingMethod.substring(shippingMethod.indexOf(\":\")+1);\n                    parameters.put(\"productStoreShipMethId\", productStoreShipMethId);\n                } else {\n                    carrierPartyId = shippingMethod.substring(shippingMethod.indexOf(\"@\")+1);\n                }\n                parameters.put(\"shipmentMethodTypeId\", shipmentMethodTypeId);\n                parameters.put(\"carrierPartyId\", carrierPartyId);\n            }", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        Object shipmentMethodTypeId = context.get("shipmentMethodTypeId");
        Object carrierPartyId = context.get("carrierPartyId");
        Object productStoreShipMethId = context.get("productStoreShipMethId");
        Debug.logInfo(" shipmentMethodTypeId is " + shipmentMethodTypeId + " ", MODULE);
        Debug.logInfo(" carrierPartyId is " + carrierPartyId, MODULE);
        Debug.logInfo(" productStoreShipMethId is " + productStoreShipMethId, MODULE);
        GenericValue shipmentMethod = null;
        try {
            shipmentMethod = EntityQuery.use(delegator)
                    .from("CarrierAndShipmentMethod")
                    .where(UtilMisc.toMap("shipmentMethodTypeId", shipmentMethodTypeId, "partyId", carrierPartyId, "roleTypeId", "CARRIER"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CarrierAndShipmentMethod: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert <try> element
        request.setAttribute("shippingTotal", context.get("shippingTotal"));
        request.setAttribute("cartGrandTotal", context.get("cartGrandTotal"));
        request.setAttribute("totalSalesTax", context.get("totalSalesTax"));
        Debug.logInfo("Shipping total is : " + context.get("shippingTotal"), MODULE);
        Debug.logInfo("Cart Grand total is : " + context.get("cartGrandTotal"), MODULE);
        Debug.logInfo("Total sale tax is : " + context.get("totalSalesTax"), MODULE);
        shippingDescription = ((Map<String, Object>) shipmentMethod).get("description");
        if (!"_NA_".equals(((Map<String, Object>) shipmentMethod).get("partyId"))) {
            shippingDescription = ((Map<String, Object>) shipmentMethod).get("partyId") + " " + shippingDescription;
        }
        Object shipCost = null;
        if (UtilValidate.isNotEmpty(((Map<String, Object>) shipCost).get("shippingTotal"))) {
            shippingDescription = shippingDescription + " - " + ((Map<String, Object>) shipCost).get("shippingTotal?currency(${isoCode") + ")}";
        }
        request.setAttribute("shippingDescription", shippingDescription);

        return "success";
    }


    /**
     * create a customer profile
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCustomerProfile(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> shipToAddressCtx = null;
        Map<String, Object> billToAddressCtx = null;
        Object billToContactMechId = null;
        // TODO: Convert call-map-processor (in-map: parameters, out-map: personCtx)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: emailAddressCtx)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: shipToAddressCtx)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: shipToTelecomNumberCtx)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> createPersonCtx = new HashMap<>();
        // set-service-fields from "personCtx" to "createPersonCtx" for service "createPerson"
        createPersonCtx.putAll(UtilMisc.toMap(context.get("personCtx")));
        Object partyId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPerson", createPersonCtx);
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
        context.put("partyId", partyId);
        String result = createUpdateUserLogin(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        Map<String, Object> partyRoleContext = new HashMap<>();
        partyRoleContext.put("partyId", context.get("partyId"));
        partyRoleContext.put("roleTypeId", context.get("roleTypeId"));
        partyRoleContext.put("userLogin", context.get("userLogin"));
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
        Map<String, Object> emailAddressCtx = new HashMap<>();
        emailAddressCtx.put("partyId", context.get("partyId"));
        emailAddressCtx.put("userLogin", context.get("userLogin"));
        emailAddressCtx.put("contactMechPurposeTypeId", context.get("emailContactMechPurposeTypeId"));
        Object emailContactMechId = null;
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
        Debug.logInfo("Email Contact Created emailContactMechId is " + emailContactMechId, MODULE);
        shipToAddressCtx.put("partyId", context.get("partyId"));
        shipToAddressCtx.put("userLogin", context.get("userLogin"));
        shipToAddressCtx.put("setShippingPurpose", "Y");
        shipToAddressCtx.put("productStoreId", context.get("productStoreId"));
        if ("Y".equals(context.get("useShippingAddressForBilling"))) {
            shipToAddressCtx.put("setBillingPurpose", "Y");
        }
        Object shipToContactMechId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPostalAddressAndPurposes", shipToAddressCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            shipToContactMechId = serviceResult.get("contactMechId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPostalAddressAndPurposes: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("Shipping address created with contactMechId " + shipToContactMechId, MODULE);
        Map<String, Object> shipToTelecomNumberCtx = new HashMap<>();
        shipToTelecomNumberCtx.put("partyId", context.get("partyId"));
        shipToTelecomNumberCtx.put("userLogin", context.get("userLogin"));
        shipToTelecomNumberCtx.put("contactMechPurposeTypeId", "PHONE_SHIPPING");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", shipToTelecomNumberCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("shipToTelecomContactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("Shipping telecom number is created with contactMechId " + context.get("shipToTelecomContactMechId"), MODULE);
        if (!"Y".equals(context.get("useShippingAddressForBilling"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: billToAddressCtx)
            billToAddressCtx.put("partyId", context.get("partyId"));
            billToAddressCtx.put("userLogin", context.get("userLogin"));
            billToAddressCtx.put("setBillingPurpose", "Y");
            billToAddressCtx.put("productStoreId", context.get("productStoreId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPostalAddressAndPurposes", billToAddressCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                billToContactMechId = serviceResult.get("contactMechId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPostalAddressAndPurposes: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Billing address created with contactMechId " + billToContactMechId, MODULE);
        } else {
            Debug.logInfo("Billing address created same as Shipping address with contactMechId " + shipToContactMechId, MODULE);
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: billToTelecomNumberCtx)
        Map<String, Object> billToTelecomNumberCtx = new HashMap<>();
        billToTelecomNumberCtx.put("partyId", context.get("partyId"));
        billToTelecomNumberCtx.put("userLogin", context.get("userLogin"));
        billToTelecomNumberCtx.put("contactMechPurposeTypeId", "PHONE_BILLING");
        Object billToTelecomContactMechId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", billToTelecomNumberCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            billToTelecomContactMechId = serviceResult.get("contactMechId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
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
        Map<String, Object> createProductStoreRoleMap = new HashMap<>();
        createProductStoreRoleMap.put("userLogin", userLogin);
        createProductStoreRoleMap.put("partyId", context.get("partyId"));
        createProductStoreRoleMap.put("roleTypeId", context.get("roleTypeId"));
        createProductStoreRoleMap.put("productStoreId", context.get("productStoreId"));
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
        emailAddressCtx.put("productStoreId", context.get("productStoreId"));
        Map<String, Object> serviceInMap = new HashMap<>();
        // set-service-fields from "emailAddressCtx" to "serviceInMap" for service "sendCreatePartyEmailNotification"
        serviceInMap.putAll(UtilMisc.toMap(emailAddressCtx));
        // TODO: Convert <call-service-asynch> element

        return "success";
    }


    /**
     * Update a customer profile
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCustomerProfile(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object productStore = null;
        try {
            productStore = ProductStoreWorker.getProductStore(request);
        } catch (Exception e) {
            Debug.logError(e, "Error calling ProductStoreWorker.getProductStore: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createUpdatePersonCtx = new HashMap<>();
        // set-service-fields from "parameters" to "createUpdatePersonCtx" for service "createUpdatePerson"
        createUpdatePersonCtx.putAll(UtilMisc.toMap(context));
        createUpdatePersonCtx.put("userLogin", userLogin);
        createUpdatePersonCtx.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createUpdatePerson", createUpdatePersonCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("partyId", serviceResult.get("partyId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createUpdatePerson: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("Y".equals(((Map<String, Object>) productStore).get("usePrimaryEmailUsername"))) {
            context.put("userLoginId", context.get("emailAddress"));
            String result = setUserLoginFromEmail(request, response);
            if (!"success".equals(result)) {
                return result;
            }
        }
        String result = createUpdateUserLogin(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: emailAddressContext)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> createUpdatePartyEmailCtx = new HashMap<>();
        // set-service-fields from "emailAddressContext" to "createUpdatePartyEmailCtx" for service "createUpdatePartyEmailAddress"
        createUpdatePartyEmailCtx.putAll(UtilMisc.toMap(context.get("emailAddressContext")));
        createUpdatePartyEmailCtx.put("contactMechPurposeTypeId", "PRIMARY_EMAIL");
        createUpdatePartyEmailCtx.put("contactMechId", context.get("emailContactMechId"));
        createUpdatePartyEmailCtx.put("userLogin", userLogin);
        createUpdatePartyEmailCtx.put("partyId", context.get("partyId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createUpdatePartyEmailAddress", createUpdatePartyEmailCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("emailContactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createUpdatePartyEmailAddress: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Set userloginId from email. If user edit email address then set it as a new userLoginId and disabled date to far in the future for existing userLoginId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setUserLoginFromEmail(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object disabledDateTime = null;
        Object disableForYears = null;
        Object newUserLogin = null;
        Timestamp nowTimeStamp = null;
        GenericValue loggedInUser = null;
        Map<String, Object> serviceContext = null;
        if (!java.util.Objects.equals(context.get("userLoginId"), ((Map<String, Object>) userLogin).get("userLoginId"))) {
            loggedInUser = userLogin;
            // set-service-fields from "parameters" to "serviceContext" for service "updateUserLoginId"
            serviceContext.putAll(UtilMisc.toMap(context));
            serviceContext.put("userLogin", userLogin);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateUserLoginId", serviceContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                newUserLogin = serviceResult.get("newUserLogin");
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateUserLoginId: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // TODO: Convert <set-current-user-login> element
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
                Object scriptResult = GroovyUtil.eval("org.ofbiz.webapp.control.LoginWorker.doBasicLogin(newUserLogin, request);\n                    org.ofbiz.webapp.control.LoginWorker.autoLoginSet(request, response);", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            if (UtilValidate.isNotEmpty(context.get("disabledDateTime"))) {
                disabledDateTime = context.get("disabledDateTime");
            } else {
                nowTimeStamp = new Timestamp(System.currentTimeMillis());
                disableForYears = context.get("disableForYears");
                // TODO: Convert <set-calendar> element
            }
            loggedInUser.put("disabledDateTime", disabledDateTime);
            loggedInUser.put("enabled", "N");
            try {
                delegator.store(loggedInUser);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create a non Contact
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAnonContact(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object hasPartyIdPerm = null;
        Map<String, Object> newPerson = null;
        Map<String, Object> newContact = null;
        Map<String, Object> partyIdPermCheckMap = null;
        String partyErrMsg = null;
        String reloginErrMsg = null;
        if (UtilValidate.isEmpty(context.get("partyIdFrom"))) {
            context.put("partyIdFrom", context.get("partyId"));
        }
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
        context.put("userLogin", systemUserLogin);
        Boolean isExistingEmail = Boolean.FALSE;
        if (UtilValidate.isEmpty(context.get("subject"))) {
            {
                String errorMsg = UtilProperties.getMessage("EcommerceUiLabels", "EcommerceSubjectMissing", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(context.get("content"))) {
            {
                String errorMsg = UtilProperties.getMessage("CommonUiLabels", "CommonMessageMissing", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(context.get("emailAddress"))) {
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyEmailAddressMissingError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        } else {
            // TODO: Convert <if-validate-method> element
        }
        if (UtilValidate.isEmpty(context.get("captcha"))) {
            {
                String errorMsg = UtilProperties.getMessage("PartyErrorUiLabels", "PartyCaptchaMissingError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        String submittedCaptcha = ((String) context.get("captcha")).toLowerCase();
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
            Object scriptResult = GroovyUtil.eval("context.actualCaptcha = request.getSession().getAttribute(\"_CAPTCHA_CODE_\")?.get(\"captchaImage\")?.toLowerCase()", scriptContext);
        } catch (Exception e) {
            Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
        }
        if (!java.util.Objects.equals(submittedCaptcha, context.get("actualCaptcha"))) {
            {
                String errorMsg = UtilProperties.getMessage("PartyErrorUiLabels", "PartyCaptchaMissingError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("partyIdFrom"))) {
            // set-service-fields from "parameters" to "newPerson" for service "createPerson"
            newPerson.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPerson", newPerson);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("partyIdFrom", serviceResult.get("partyId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPerson: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // set-service-fields from "parameters" to "newContact" for service "createPartyContactMech"
            newContact.putAll(UtilMisc.toMap(context));
            newContact.put("infoString", context.get("emailAddress"));
            newContact.put("contactMechTypeId", "EMAIL_ADDRESS");
            newContact.put("contactMechPurposeTypeId", "PRIMARY_EMAIL");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyContactMech", newContact);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("contactMechIdFrom", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            context.put("contactMechIdFrom", ((Map<String, Object>) context.get("contact")).get("contactMechId"));
            // set-service-fields from "parameters" to "partyIdPermCheckMap" for service "partyIdPermissionCheck"
            partyIdPermCheckMap.putAll(UtilMisc.toMap(context));
            partyIdPermCheckMap.put("mainAction", "VIEW");
            partyIdPermCheckMap.put("partyId", "parameters.partyIdFrom");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("partyIdPermissionCheck", partyIdPermCheckMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                hasPartyIdPerm = serviceResult.get("hasPermission");
            } catch (Exception e) {
                Debug.logError(e, "Error calling partyIdPermissionCheck: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (!Boolean.TRUE.equals(hasPartyIdPerm)) {
                partyErrMsg = UtilProperties.getMessage("PartyUiLabels", "PartyPermissionErrorForThisParty", locale);
                reloginErrMsg = UtilProperties.getMessage("CommonUiLabels", "CommonTryLoginAgainContactSupport", locale);
                error_list.add("${partyErrMsg} (${reloginErrMsg})");
                request.setAttribute("_ERROR_MESSAGE_", "${partyErrMsg} (${reloginErrMsg})");
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        Map<String, Object> newComm = new HashMap<>();
        // set-service-fields from "parameters" to "newComm" for service "sendContactUsEmailToCompany"
        newComm.putAll(UtilMisc.toMap(context));
        newComm.put("emailType", "CONT_NOTI_EMAIL");
        List<Object> newComm_replyTo = new LinkedList<>();
        newComm_replyTo.add(context.get("emailAddress"));
        Map<String, Object> sendResult = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("sendContactUsEmailToCompany", newComm);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            sendResult = serviceResult;
        } catch (Exception e) {
            Debug.logError(e, "Error calling sendContactUsEmailToCompany: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // Check if send result was successful (converted from Groovy expression)
        if (sendResult != null && !ServiceUtil.isSuccess(sendResult)) {
            {
                String errorMsg = UtilProperties.getMessage("CommonErrorUiLabels", "CommonErrorOccurredContactTryAgain", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }

        return "success";
    }


    /**
     * Add field to request for redirection after SetSessionLocale
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String fromSetSessionLocale(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object fromSetSessionLocale = "true";
        request.setAttribute("fromSetSessionLocale", fromSetSessionLocale);

        return "success";
    }

}

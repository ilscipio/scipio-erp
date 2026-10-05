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
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://shop/script/com/ilscipio/scipio/shop/customer/QuickAnonCustomerEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class QuickAnonCustomerEvents {

    private static final String MODULE = QuickAnonCustomerEvents.class.getName();


    /**
     * Create update customer and settings
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdateCustomer(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue existingUserLogin = null;
        Map<String, Object> _userLoginExistsMap = null;
        GenericValue newUserLogin = null;
        String tempErrorMessage = null;
        Map<String, Object> personContext = null;
        Map<String, Object> ulLookup = null;
        Object partyId = null;
        Map<String, Object> partyRoleContext = null;
        Map<String, Object> userLoginContext = null;
        Map<String, Object> addressPurposeContext = null;
        Map<String, Object> shippingAddressContext = null;
        Map<String, Object> billingAddressContext = null;
        Map<String, Object> homePhoneContext = null;
        Map<String, Object> workPhoneContext = null;
        Map<String, Object> faxPhoneContext = null;
        Map<String, Object> mobilePhoneContext = null;
        Map<String, Object> emailContext = null;
        Object require_email = "true";
        Object require_phone = "true";
        Object require_login = "false";
        Object shipToPostalAddress = "true";
        Object billToPostalAddress = "true";
        Object create_allow_password = "true";
        context.put("roleTypeId", "CUSTOMER");
        String username_lowercase = UtilProperties.getMessage("security", "username.lowercase", locale);
        String password_lowercase = UtilProperties.getMessage("security", "password.lowercase", locale);
        Object default_user_password = "ungssblepsswd";
        Timestamp nowStamp = new Timestamp(System.currentTimeMillis());
        if (!"true".equals(create_allow_password)) {
            context.put("password", default_user_password);
            context.put("confirmPassword", default_user_password);
            context.put("passwordHint", "No hint set, account not yet enabled");
        }
        if ("true".equals(username_lowercase)) {
            String parameters_username = ((String) context.get("username")).toLowerCase();
        }
        if ("true".equals(password_lowercase)) {
            String parameters_password = ((String) context.get("password")).toLowerCase();
            String parameters_confirmPassword = ((String) context.get("confirmPassword")).toLowerCase();
        }
        if (UtilValidate.isNotEmpty(context.get("userLoginId"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: userLoginContext)
            // simple-map-processor name: newUserLogin
            if (UtilValidate.isNotEmpty(((Map<String, Object>) userLoginContext).get("userLoginId"))) {
                _userLoginExistsMap.put("userLoginId", ((Map<String, Object>) userLoginContext).get("userLoginId"));
                try {
                    existingUserLogin = EntityQuery.use(delegator)
                            .from("UserLogin")
                            .where(context.get("userLoginExistsMap"))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error finding by primary key UserLogin: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(existingUserLogin)) {
                    tempErrorMessage = (String) ((Map<String, Object>) context.get("uiLabelMap")).get("PartyUserNameInUse");
                    error_list.add(tempErrorMessage);
                }
            }
            newUserLogin = delegator.makeValue("UserLogin");
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
                Object scriptResult = GroovyUtil.eval("String password = (String) userLoginContext.get(\"currentPassword\");\n                String confirmPassword = (String) userLoginContext.get(\"currentPasswordVerify\");\n                String passwordHint = (String) userLoginContext.get(\"passwordHint\");\n                org.ofbiz.common.login.LoginServices.checkNewPassword(newUserLogin, null, password, confirmPassword, passwordHint, error_list, true, locale);", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
        } else {
            if ("true".equals(require_login)) {
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
        if (UtilValidate.isNotEmpty(context.get("homeContactNumber"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: homePhoneContext)
            // simple-map-processor name: newTelecomNumber
        }
        if (UtilValidate.isNotEmpty(context.get("workContactNumber"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: workPhoneContext)
            // simple-map-processor name: newTelecomNumber
        }
        if (UtilValidate.isNotEmpty(context.get("faxContactNumber"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: faxPhoneContext)
            // simple-map-processor name: newTelecomNumber
        }
        if (UtilValidate.isNotEmpty(context.get("mobileContactNumber"))) {
            // TODO: Convert call-map-processor (in-map: parameters, out-map: mobilePhoneContext)
            // simple-map-processor name: newTelecomNumber
        }
        if ("true".equals(require_phone)) {
            if (UtilValidate.isEmpty(context.get("homeContactNumber"))) {
                if (UtilValidate.isEmpty(context.get("workContactNumber"))) {
                    if (UtilValidate.isEmpty(context.get("mobileContactNumber"))) {
                        // TODO: Convert call-map-processor (in-map: parameters, out-map: dummymap)
                        // simple-map-processor name: checkRequiredPhone
                        Map<String, Object> dummymap = new HashMap<>();
                    }
                }
            }
        }
        // TODO: Convert call-map-processor (in-map: parameters, out-map: emailContext)
        // simple-map-processor name: newEmail
        if ("true".equals(require_email)) {
            if (UtilValidate.isEmpty(((Map<String, Object>) emailContext).get("emailAddress"))) {
                // TODO: Convert call-map-processor (in-map: emailContext, out-map: dummymap)
                // simple-map-processor name: checkRequiredEmail
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) emailContext).get("emailAddress"))) {
                // TODO: Convert call-map-processor (in-map: emailContext, out-map: dummymap)
                // simple-map-processor name: checkRequiredEmailFormat
            }
        }
        partyRoleContext.put("roleTypeId", context.get("roleTypeId"));
        if ("true".equals(shipToPostalAddress)) {
            if (UtilValidate.isEmpty(context.get("shipToName"))) {
                context.put("shipToName", ((Map<String, Object>) personContext).get("firstName") + " " + ((Map<String, Object>) personContext).get("middleName") + " " + ((Map<String, Object>) personContext).get("lastName"));
            }
            // TODO: Convert call-map-processor (in-map: parameters, out-map: shippingAddressContext)
            // simple-map-processor name: postalAddress
            if ("USA".equals(context.get("shipToCountryGeoId"))) {
                if (UtilValidate.isEmpty(context.get("shipToStateProvinceGeoId"))) {
                    tempErrorMessage = UtilProperties.getMessage("PartyUiLabels", "PartyStateInUsMissing", locale);
                    error_list.add(tempErrorMessage);
                }
            }
            if ("CAN".equals(context.get("shipToCountryGeoId"))) {
                if (UtilValidate.isEmpty(context.get("shipToStateProvinceGeoId"))) {
                    tempErrorMessage = UtilProperties.getMessage("PartyUiLabels", "PartyStateInCanadaMissing", locale);
                    error_list.add(tempErrorMessage);
                }
            }
        }
        if (!"Y".equals(context.get("useShippingPostalAddressForBilling"))) {
            if ("true".equals(billToPostalAddress)) {
                if (UtilValidate.isEmpty(context.get("billToName"))) {
                    context.put("billToName", ((Map<String, Object>) personContext).get("firstName") + " " + ((Map<String, Object>) personContext).get("middleName") + " " + ((Map<String, Object>) personContext).get("lastName"));
                }
                // TODO: Convert call-map-processor (in-map: parameters, out-map: billingAddressContext)
                // simple-map-processor name: postalAddress
                if ("USA".equals(context.get("billToCountryGeoId"))) {
                    if (UtilValidate.isEmpty(context.get("billToStateProvinceGeoId"))) {
                        tempErrorMessage = UtilProperties.getMessage("PartyUiLabels", "PartyStateInUsMissing", locale);
                        error_list.add(tempErrorMessage);
                    }
                }
                if ("CAN".equals(context.get("billToCountryGeoId"))) {
                    if (UtilValidate.isEmpty(context.get("billToStateProvinceGeoId"))) {
                        tempErrorMessage = UtilProperties.getMessage("PartyUiLabels", "PartyStateInCanadaMissing", locale);
                        error_list.add(tempErrorMessage);
                    }
                }
            }
        }
        GenericValue partyDataSource = delegator.makeValue("PartyDataSource");
        partyDataSource.put("dataSourceId", "ECOMMERCE_SITE");
        partyDataSource.put("fromDate", nowStamp);
        partyDataSource.put("isCreate", "Y");
        Object visit = request.getSession().getAttribute("visit");
        partyDataSource.put("visitId", ((Map<String, Object>) visit).get("visitId"));
        Debug.logInfo("Setting up party " + error_list + " ", MODULE);
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPerson", personContext);
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
            if (UtilValidate.isEmpty(userLogin)) {
                if (UtilValidate.isEmpty(newUserLogin)) {
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
                    userLogin.put("partyId", partyId);
                    // TODO: Convert <set-current-user-login> element
                } else {
                    // TODO: Convert <set-current-user-login> element
                }
            } else {
                if ("anonymous".equals(((Map<String, Object>) userLogin).get("userLoginId"))) {
                    userLogin.put("partyId", partyId);
                }
            }
            partyRoleContext.put("partyId", partyId);
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
        } else {
            personContext.put("partyId", context.get("partyId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePerson", personContext);
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
            partyId = context.get("partyId");
        }
        partyDataSource.put("partyId", partyId);
        userLoginContext.put("partyId", partyId);
        personContext.put("partyId", partyId);
        shippingAddressContext.put("partyId", partyId);
        billingAddressContext.put("partyId", partyId);
        homePhoneContext.put("partyId", partyId);
        workPhoneContext.put("partyId", partyId);
        faxPhoneContext.put("partyId", partyId);
        mobilePhoneContext.put("partyId", partyId);
        emailContext.put("partyId", partyId);
        if (UtilValidate.isNotEmpty(newUserLogin)) {
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
                Object scriptResult = GroovyUtil.eval("boolean useEncryption = \"true\".equals(org.ofbiz.entity.util.EntityUtilProperties.getPropertyValue(\"security\", \"password.encrypt\", delegator)) // SCIPIO: use delegator\n                if (useEncryption) {\n                    String hashType = org.ofbiz.common.login.LoginServices.getHashType()\n                    newUserLogin.set(\"currentPassword\", org.ofbiz.base.crypto.HashCrypt.digestHash(hashType, null, (String) newUserLogin.get(\"currentPassword\")))\n                }", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            userLoginContext.put("partyId", partyId);
            try {
                delegator.create(newUserLogin);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
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
        if ("true".equals(shipToPostalAddress)) {
            if (UtilValidate.isEmpty(context.get("shippingContactMechId"))) {
                shippingAddressContext.put("contactMechPurposeTypeId", context.get("shippingContactMechPurposeTypeId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", shippingAddressContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("shippingContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if ("Y".equals(context.get("useShippingPostalAddressForBilling"))) {
                    addressPurposeContext.put("partyId", partyId);
                    addressPurposeContext.put("contactMechPurposeTypeId", context.get("billingContactMechPurposeTypeId"));
                    addressPurposeContext.put("contactMechId", context.get("shippingContactMechId"));
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
            } else {
                shippingAddressContext.put("contactMechId", context.get("shippingContactMechId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePartyPostalAddress", shippingAddressContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("shippingContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePartyPostalAddress: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (!"Y".equals(context.get("useShippingPostalAddressForBilling"))) {
            if (UtilValidate.isEmpty(context.get("billingContactMechId"))) {
                billingAddressContext.put("contactMechPurposeTypeId", context.get("billingContactMechPurposeTypeId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyPostalAddress", billingAddressContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("billingContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyPostalAddress: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                billingAddressContext.put("contactMechId", context.get("billingContactMechId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePartyPostalAddress", billingAddressContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("billingContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePartyPostalAddress: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("homeContactNumber"))) {
            if (UtilValidate.isEmpty(context.get("homePhoneContactMechId"))) {
                homePhoneContext.put("contactMechPurposeTypeId", "PHONE_HOME");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", homePhoneContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("homePhoneContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                homePhoneContext.put("contactMechId", context.get("homePhoneContactMechId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePartyTelecomNumber", homePhoneContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("homePhoneContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePartyTelecomNumber: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("workContactNumber"))) {
            if (UtilValidate.isEmpty(context.get("workPhoneContactMechId"))) {
                workPhoneContext.put("contactMechPurposeTypeId", "PHONE_WORK");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", workPhoneContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("workPhoneContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                workPhoneContext.put("contactMechId", context.get("workPhoneContactMechId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePartyTelecomNumber", workPhoneContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("workPhoneContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePartyTelecomNumber: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("faxContactNumber"))) {
            if (UtilValidate.isEmpty(context.get("faxPhoneContactMechId"))) {
                faxPhoneContext.put("contactMechPurposeTypeId", "FAX_NUMBER");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", faxPhoneContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("faxPhoneContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                faxPhoneContext.put("contactMechId", context.get("faxPhoneContactMechId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePartyTelecomNumber", faxPhoneContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("faxPhoneContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePartyTelecomNumber: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("mobileContactNumber"))) {
            if (UtilValidate.isEmpty(context.get("mobilePhoneContactMechId"))) {
                mobilePhoneContext.put("contactMechPurposeTypeId", "PHONE_MOBILE");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyTelecomNumber", mobilePhoneContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("mobilePhoneContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyTelecomNumber: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                mobilePhoneContext.put("contactMechId", context.get("mobilePhoneContactMechId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePartyTelecomNumber", mobilePhoneContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("mobilePhoneContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePartyTelecomNumber: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("emailAddress"))) {
            if (UtilValidate.isEmpty(context.get("emailContactMechId"))) {
                emailContext.put("contactMechPurposeTypeId", "PRIMARY_EMAIL");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyEmailAddress", emailContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("emailContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyEmailAddress: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                emailContext.put("contactMechId", context.get("emailContactMechId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updatePartyEmailAddress", emailContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("emailContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updatePartyEmailAddress: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
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

}

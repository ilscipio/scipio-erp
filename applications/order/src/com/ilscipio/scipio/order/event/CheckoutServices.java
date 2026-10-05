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
import org.ofbiz.order.shoppingcart.ShoppingCart;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://order/script/org/ofbiz/order/order/CheckoutServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CheckoutServices {

    private static final String MODULE = CheckoutServices.class.getName();


    /**
     * Create/Update Customer, Shipping Address and other contact details.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdateCustomerAndShippingAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object partyId = null;
        Object emptyField = null;
        // TODO: Convert call-map-processor (in-map: parameters, out-map: shipToPhoneCtx)
        // TODO: Convert call-map-processor (in-map: parameters, out-map: emailAddressCtx)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        ShoppingCart shoppingCart = (ShoppingCart) context.get("shoppingCart");
        partyId = context.get("partyId");
        userLogin = shoppingCart.getUserLogin();
        Object userLogin____null__and_userLogin = null;
        if (Boolean.TRUE.equals(((Map<String, Object>) userLogin____null__and_userLogin).get("partyId != null @and partyId == null"))) {
            partyId = ((Map<String, Object>) userLogin).get("partyId");
        }
        Map<String, Object> createUpdatePersonCtx = new HashMap<>();
        // set-service-fields from "parameters" to "createUpdatePersonCtx" for service "createUpdatePerson"
        createUpdatePersonCtx.putAll(UtilMisc.toMap(context));
        createUpdatePersonCtx.put("userLogin", userLogin);
        createUpdatePersonCtx.put("partyId", partyId);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createUpdatePerson", createUpdatePersonCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            partyId = serviceResult.get("partyId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createUpdatePerson: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(userLogin)) {
            if ("anonymous".equals(((Map<String, Object>) userLogin).get("userLoginId"))) {
                userLogin.put("partyId", partyId);
            }
        }
        Map<String, Object> partyRoleCtx = new HashMap<>();
        partyRoleCtx.put("partyId", partyId);
        partyRoleCtx.put("roleTypeId", "CUSTOMER");
        partyRoleCtx.put("userLogin", userLogin);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("ensurePartyRole", partyRoleCtx);
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
        Map<String, Object> shipToAddressCtx = new HashMap<>();
        // set-service-fields from "parameters" to "shipToAddressCtx" for service "createUpdateShippingAddress"
        shipToAddressCtx.putAll(UtilMisc.toMap(context));
        shipToAddressCtx.put("userLogin", userLogin);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createUpdateShippingAddress", shipToAddressCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("shipToContactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createUpdateShippingAddress: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createUpdatePartyTelecomNumberCtx = new HashMap<>();
        // set-service-fields from "shipToPhoneCtx" to "createUpdatePartyTelecomNumberCtx" for service "createUpdatePartyTelecomNumber"
        createUpdatePartyTelecomNumberCtx.putAll(UtilMisc.toMap(context.get("shipToPhoneCtx")));
        createUpdatePartyTelecomNumberCtx.put("userLogin", userLogin);
        createUpdatePartyTelecomNumberCtx.put("partyId", partyId);
        createUpdatePartyTelecomNumberCtx.put("roleTypeId", "CUSTOMER");
        createUpdatePartyTelecomNumberCtx.put("contactMechPurposeTypeId", "PHONE_SHIPPING");
        createUpdatePartyTelecomNumberCtx.put("contactMechId", context.get("shipToPhoneContactMechId"));
        Object shipToPhoneContactMechId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createUpdatePartyTelecomNumber", createUpdatePartyTelecomNumberCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            shipToPhoneContactMechId = serviceResult.get("contactMechId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createUpdatePartyTelecomNumber: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(shipToPhoneContactMechId)) {
            shoppingCart.addContactMech("PHONE_SHIPPING", (String) shipToPhoneContactMechId);
        }
        Map<String, Object> createUpdatePartyEmailCtx = new HashMap<>();
        // set-service-fields from "emailAddressCtx" to "createUpdatePartyEmailCtx" for service "createUpdatePartyEmailAddress"
        createUpdatePartyEmailCtx.putAll(UtilMisc.toMap(context.get("emailAddressCtx")));
        createUpdatePartyEmailCtx.put("contactMechPurposeTypeId", "PRIMARY_EMAIL");
        createUpdatePartyEmailCtx.put("userLogin", userLogin);
        createUpdatePartyEmailCtx.put("partyId", partyId);
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
        result.put("partyId", partyId);
        if (UtilValidate.isNotEmpty(context.get("emailContactMechId"))) {
            shoppingCart.addContactMech("ORDER_EMAIL", (String) context.get("emailContactMechId"));
        }
        try {
            shoppingCart.setUserLogin(userLogin, dispatcher);
        } catch (Exception e) {
            Debug.logError(e, "Error setting user login on cart: " + e.getMessage(), MODULE);
        }
        shoppingCart.addContactMech("SHIPPING_LOCATION", (String) context.get("shipToContactMechId"));
        shoppingCart.setAllShippingContactMechId((String) context.get("shipToContactMechId"));
        shoppingCart.setOrderPartyId((String) partyId);

        return "success";
    }


    /**
     * Create/update billing address and payment information
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdateBillingAddressAndPaymentMethod(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object partyId = null;
        Object shipToContactMechId = null;
        Object emptyField = null;
        // TODO: Convert call-map-processor (in-map: parameters, out-map: billToPhoneContext)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        ShoppingCart shoppingCart = (ShoppingCart) context.get("shoppingCart");
        userLogin = shoppingCart.getUserLogin();
        partyId = context.get("partyId");
        if (Boolean.TRUE.equals(context.get("userLogin != null @and partyId == null"))) {
            partyId = ((Map<String, Object>) userLogin).get("partyId");
        }
        shipToContactMechId = context.get("shipToContactMechId");
        if (UtilValidate.isNotEmpty(shoppingCart)) {
            if (UtilValidate.isEmpty(partyId)) {
                partyId = shoppingCart.getPartyId();
            }
            if (UtilValidate.isEmpty(shipToContactMechId)) {
                shipToContactMechId = shoppingCart.getShippingContactMechId();
            }
        }
        if (UtilValidate.isNotEmpty(partyId)) {
            if ("anonymous".equals(((Map<String, Object>) userLogin).get("userLoginId"))) {
                userLogin.put("partyId", partyId);
            }
        }
        Map<String, Object> billToAddressCtx = new HashMap<>();
        // set-service-fields from "parameters" to "billToAddressCtx" for service "createUpdateBillingAddress"
        billToAddressCtx.putAll(UtilMisc.toMap(context));
        billToAddressCtx.put("userLogin", userLogin);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createUpdateBillingAddress", billToAddressCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("billToContactMechId", serviceResult.get("contactMechId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createUpdateBillingAddress: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(context.get("billToContactMechId"))) {
            shoppingCart.addContactMech("BILLING_LOCATION", (String) context.get("billToContactMechId"));
        }
        Map<String, Object> createUpdatePartyTelecomNumberCtx = new HashMap<>();
        // set-service-fields from "billToPhoneContext" to "createUpdatePartyTelecomNumberCtx" for service "createUpdatePartyTelecomNumber"
        createUpdatePartyTelecomNumberCtx.putAll(UtilMisc.toMap(context.get("billToPhoneContext")));
        createUpdatePartyTelecomNumberCtx.put("userLogin", userLogin);
        createUpdatePartyTelecomNumberCtx.put("partyId", partyId);
        createUpdatePartyTelecomNumberCtx.put("roleTypeId", "CUSTOMER");
        createUpdatePartyTelecomNumberCtx.put("contactMechPurposeTypeId", "PHONE_BILLING");
        createUpdatePartyTelecomNumberCtx.put("contactMechId", context.get("billToPhoneContactMechId"));
        Object billToPhoneContactMechId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createUpdatePartyTelecomNumber", createUpdatePartyTelecomNumberCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            billToPhoneContactMechId = serviceResult.get("contactMechId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createUpdatePartyTelecomNumber: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(billToPhoneContactMechId)) {
            shoppingCart.addContactMech("PHONE_BILLING", (String) billToPhoneContactMechId);
        }
        Map<String, Object> creditCardCtx = new HashMap<>();
        // set-service-fields from "parameters" to "creditCardCtx" for service "createUpdateCreditCard"
        creditCardCtx.putAll(UtilMisc.toMap(context));
        creditCardCtx.put("contactMechId", context.get("billToContactMechId"));
        creditCardCtx.put("userLogin", userLogin);
        Object paymentMethodId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createUpdateCreditCard", creditCardCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            paymentMethodId = serviceResult.get("paymentMethodId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createUpdateCreditCard: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object cardSecurityCode = context.get("billToCardSecurityCode");
        // TODO: Convert <create-object> element
        Object callResult = GroovyUtil.eval("checkOutHelper.finalizeOrderEntryPayment(paymentMethodId, null, false, false)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        Object cartPaymentInfo = GroovyUtil.eval("org.ofbiz.order.shoppingcart.ShoppingCart.CartPaymentInfo cpi = shoppingCart.getPaymentInfo(paymentMethodId, null, null, null, true); cpi.securityCode = cardSecurityCode; return cpi;", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Set user login in the session
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setAnonUserLogin(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        ShoppingCart shoppingCart = (ShoppingCart) context.get("shoppingCart");
        userLogin = shoppingCart.getUserLogin();
        if (UtilValidate.isEmpty(userLogin)) {
            try {
                userLogin = EntityQuery.use(delegator)
                        .from("UserLogin")
                        .where(UtilMisc.toMap("userLoginId", "anonymous"))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            if ("anonymous".equals(((Map<String, Object>) userLogin).get("userLoginId"))) {
                userLogin.put("partyId", context.get("partyId"));
            }
        }
        try {
            shoppingCart.setUserLogin(userLogin, dispatcher);
        } catch (Exception e) {
            Debug.logError(e, "Error setting user login on cart: " + e.getMessage(), MODULE);
        }

        return "success";
    }

}

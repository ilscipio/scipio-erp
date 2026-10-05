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
package com.ilscipio.scipio.accounting.event;

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

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/payment/PaymentGatewayConfigServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PaymentGatewayConfigServices {

    private static final String MODULE = PaymentGatewayConfigServices.class.getName();


    /**
     * Update Payment Gateway Config
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentGatewayConfig(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentGatewayConfig")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGatewayConfig: " + e.getMessage(), MODULE);
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
     * Update Payment Gateway Config SagePay
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentGatewayConfigSagePay(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = updatePaymentGatewayConfig(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentGatewaySagePay")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGatewaySagePay: " + e.getMessage(), MODULE);
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
     * Update Payment Gateway Config Authorize Dot Net
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentGatewayConfigAuthorizeNet(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = updatePaymentGatewayConfig(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentGatewayAuthorizeNet")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGatewayAuthorizeNet: " + e.getMessage(), MODULE);
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
     * Update Payment Gateway Config Clear Commerce
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentGatewayConfigClearCommerce(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = updatePaymentGatewayConfig(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentGatewayClearCommerce")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGatewayClearCommerce: " + e.getMessage(), MODULE);
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
     * Update Payment Gateway Config CyberSource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentGatewayConfigCyberSource(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = updatePaymentGatewayConfig(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentGatewayCyberSource")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGatewayCyberSource: " + e.getMessage(), MODULE);
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
     * Update Payment Gateway Config Payflow Pro
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentGatewayConfigPayflowPro(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = updatePaymentGatewayConfig(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentGatewayPayflowPro")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGatewayPayflowPro: " + e.getMessage(), MODULE);
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
     * Update Payment Gateway Config PayPal
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentGatewayConfigPayPal(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = updatePaymentGatewayConfig(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentGatewayPayPal")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGatewayPayPal: " + e.getMessage(), MODULE);
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
     * Update Payment Gateway Config RBS WorldPay
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentGatewayConfigWorldPay(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = updatePaymentGatewayConfig(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentGatewayWorldPay")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGatewayWorldPay: " + e.getMessage(), MODULE);
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
     * Update Payment Gateway Config Type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentGatewayConfigType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = updatePaymentGatewayConfig(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentGatewayConfigType")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGatewayConfigType: " + e.getMessage(), MODULE);
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
     * Update Payment Gateway Config SecurePay
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentGatewayConfigSecurePay(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = updatePaymentGatewayConfig(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentGatewaySecurePay")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGatewaySecurePay: " + e.getMessage(), MODULE);
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
     * Update Payment Gateway Config iDEAL
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentGatewayConfigiDEAL(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = updatePaymentGatewayConfig(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentGatewayiDEAL")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGatewayiDEAL: " + e.getMessage(), MODULE);
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
     * Update Payment Gateway Config Orbital
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentGatewayConfigOrbital(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = updatePaymentGatewayConfig(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentGatewayOrbital")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGatewayOrbital: " + e.getMessage(), MODULE);
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

}

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
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/payment/PaymentMethodServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PaymentMethodServices {

    private static final String MODULE = PaymentMethodServices.class.getName();


    /**
     * Set the initial payment method address
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setPaymentMethodAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue savedValue = null;
        GenericValue mainValue = null;
        GenericValue lookupPKMap = delegator.makeValue("PaymentMethod");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentMethod")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key PaymentMethod: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("CREDIT_CARD".equals(((Map<String, Object>) lookedUpValue).get("paymentMethodTypeId"))) {
            try {
                mainValue = EntityQuery.use(delegator)
                        .from("CreditCard")
                        .where(lookupPKMap)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key CreditCard: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            savedValue = GenericValue.create((GenericValue) mainValue);
            mainValue.setNonPKFields((Map<String, Object>) context);
            if (!java.util.Objects.equals(mainValue, savedValue)) {
                try {
                    delegator.store(mainValue);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if ("EFT_ACCOUNT".equals(((Map<String, Object>) lookedUpValue).get("paymentMethodTypeId"))) {
            try {
                mainValue = EntityQuery.use(delegator)
                        .from("EftAccount")
                        .where(lookupPKMap)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key EftAccount: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            savedValue = GenericValue.create((GenericValue) mainValue);
            mainValue.setNonPKFields((Map<String, Object>) context);
            if (!java.util.Objects.equals(mainValue, savedValue)) {
                try {
                    delegator.store(mainValue);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Update payment method addresses
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentMethodAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object isNotExpired = null;
        Map<String, Object> uccMap = null;
        Map<String, Object> ueaMap = null;
        GenericValue paymentMethod = null;
        Map<String, Object> lookupMap = new HashMap<>();
        lookupMap.put("contactMechId", context.get("oldContactMechId"));
        // TODO: Convert <find-by-and> element
        if (context.get("creditCards") != null) {
            for (Object creditCard : (List<Object>) context.get("creditCards")) {
                try {
                    isNotExpired = UtilValidate.isDateAfterToday((String) ((Map<String, Object>) creditCard).get("expireDate"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling UtilValidate.isDateAfterToday: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (Boolean.TRUE.equals(isNotExpired)) {
                    // set-service-fields from "creditCard" to "uccMap" for service "updateCreditCard"
                    uccMap.putAll(UtilMisc.toMap(creditCard));
                    uccMap.put("contactMechId", context.get("contactMechId"));
                    uccMap.put("partyId", context.get("partyId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updateCreditCard", uccMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updateCreditCard: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        // TODO: Convert <find-by-and> element
        if (context.get("eftAccounts") != null) {
            for (Object eftAccount : (List<Object>) context.get("eftAccounts")) {
                try {
                    paymentMethod = ((GenericValue) eftAccount).getRelatedOne("PaymentMethod", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one PaymentMethod: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(((Map<String, Object>) paymentMethod).get("thruDate"))) {
                    // set-service-fields from "eftAccount" to "ueaMap" for service "updateEftAccount"
                    ueaMap.putAll(UtilMisc.toMap(eftAccount));
                    ueaMap.put("contactMechId", context.get("contactMechId"));
                    ueaMap.put("partyId", context.get("partyId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updateEftAccount", ueaMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updateEftAccount: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Create a Credit Card Gl Account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCreditCardTypeGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = delegator.makeValue("CreditCardTypeGlAccount");
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
     * Update a Credit Card Gl Account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCreditCardTypeGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("CreditCardTypeGlAccount")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CreditCardTypeGlAccount: " + e.getMessage(), MODULE);
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
     * Delete a Credit Card Gl Account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteCreditCardTypeGlAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("CreditCardTypeGlAccount")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CreditCardTypeGlAccount: " + e.getMessage(), MODULE);
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
     * Updates a Payment Method Type default glAccountId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentMethodType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PaymentMethodType")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentMethodType: " + e.getMessage(), MODULE);
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
     * expire a Payment Group Member
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String expirePaymentGroupMember(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue paymentGroupMember = null;
        try {
            paymentGroupMember = EntityQuery.use(delegator)
                    .from("PaymentGroupMember")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGroupMember: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> updatePaymentGroupMemberMap = new HashMap<>();
        // set-service-fields from "paymentGroupMember" to "updatePaymentGroupMemberMap" for service "updatePaymentGroupMember"
        updatePaymentGroupMemberMap.putAll(UtilMisc.toMap(paymentGroupMember));
        Timestamp updatePaymentGroupMemberMap_thruDate = new Timestamp(System.currentTimeMillis());
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePaymentGroupMember", updatePaymentGroupMemberMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePaymentGroupMember: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create a PayPal Payment Method
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPayPalPaymentMethod(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newPaymentMethod = delegator.makeValue("PaymentMethod");
        newPaymentMethod.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newPaymentMethod).get("paymentMethodId"))) {
            ((GenericValue) newPaymentMethod).put("paymentMethodId", delegator.getNextSeqId("PaymentMethod"));
        }
        newPaymentMethod.setNonPKFields((Map<String, Object>) context);
        newPaymentMethod.put("paymentMethodTypeId", "EXT_PAYPAL");
        try {
            delegator.create(newPaymentMethod);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue newPayPalPaymentMethod = delegator.makeValue("PayPalPaymentMethod");
        newPayPalPaymentMethod.put("paymentMethodId", ((Map<String, Object>) newPaymentMethod).get("paymentMethodId"));
        newPayPalPaymentMethod.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.create(newPayPalPaymentMethod);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("paymentMethodId", ((Map<String, Object>) newPaymentMethod).get("paymentMethodId"));

        return "success";
    }


    /**
     * Update a PayPal Payment Method
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePayPalPaymentMethod(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue payPalPaymentMethod = null;
        try {
            payPalPaymentMethod = EntityQuery.use(delegator)
                    .from("PayPalPaymentMethod")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PayPalPaymentMethod: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        payPalPaymentMethod.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(payPalPaymentMethod);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("paymentMethodId", ((Map<String, Object>) payPalPaymentMethod).get("paymentMethodId"));

        return "success";
    }


    /**
     * Check For Outgoing/Incoming Payment And Create Payment Group Member
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPaymentGroupMember(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Boolean isDisbursement = null;
        Boolean isReceipt = null;
        GenericValue newPaymentGroupMember = delegator.makeValue("PaymentGroupMember");
        newPaymentGroupMember.setPKFields((Map<String, Object>) context);
        newPaymentGroupMember.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            Timestamp newPaymentGroupMember_fromDate = new Timestamp(System.currentTimeMillis());
        }
        GenericValue paymentGroup = null;
        try {
            paymentGroup = EntityQuery.use(delegator)
                    .from("PaymentGroup")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGroup: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue payment = null;
        try {
            payment = EntityQuery.use(delegator)
                    .from("Payment")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("CHECK_RUN".equals(((Map<String, Object>) paymentGroup).get("paymentGroupTypeId"))) {
            isDisbursement = (Boolean) GroovyUtil.eval("org.ofbiz.accounting.util.UtilAccounting.isDisbursement(payment)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
            if (Boolean.TRUE.equals(isDisbursement)) {
                try {
                    delegator.create(newPaymentGroupMember);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingCannotCreateIncomingPaymentError", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        } else {
            if (Boolean.FALSE.equals(((Map<String, Object>) paymentGroup).get("paymentGroupTypeId"))) {
                isReceipt = (Boolean) GroovyUtil.eval("org.ofbiz.accounting.util.UtilAccounting.isReceipt(payment)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                if ("true".equals(isReceipt)) {
                    try {
                        delegator.create(newPaymentGroupMember);
                    } catch (Exception e) {
                        Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                } else {
                    {
                        String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingCannotCreateOutgoingPaymentError", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }

}

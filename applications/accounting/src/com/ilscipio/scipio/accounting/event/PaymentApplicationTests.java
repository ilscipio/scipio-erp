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

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.accounting.invoice.InvoiceWorker;
import org.ofbiz.accounting.payment.PaymentWorker;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.order.order.OrderReadHelper;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/test/PaymentApplicationTests.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PaymentApplicationTests {

    private static final String MODULE = PaymentApplicationTests.class.getName();


    /**
     * test the application of a payment against an invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testInvoiceAppl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> serviceInMap = new HashMap<>();
        serviceInMap.put("invoiceId", "appltest10000");
        serviceInMap.put("paymentId", "appltest10000");
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
        serviceInMap.put("userLogin", userLogin);
        Object amountApplied = null;
        Object paymentApplicationId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPaymentApplication", serviceInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            amountApplied = serviceResult.get("amountApplied");
            paymentApplicationId = serviceResult.get("paymentApplicationId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue paymentApplication = null;
        try {
            paymentApplication = EntityQuery.use(delegator)
                    .from("PaymentApplication")
                    .where(UtilMisc.toMap("paymentApplicationId", paymentApplicationId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue payment = null;
        try {
            payment = EntityQuery.use(delegator)
                    .from("Payment")
                    .where(UtilMisc.toMap("paymentId", ((Map<String, Object>) serviceInMap).get("paymentId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert !(UtilValidate.isEmpty(paymentApplication)) : "Assertion failed: not if-empty";
        assert java.util.Objects.equals(((Map<String, Object>) paymentApplication).get("invoiceId"), ((Map<String, Object>) serviceInMap).get("invoiceId")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(((Map<String, Object>) paymentApplication).get("paymentId"), ((Map<String, Object>) serviceInMap).get("paymentId")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(((Map<String, Object>) paymentApplication).get("amountApplied"), ((Map<String, Object>) payment).get("amount")) : "Assertion failed: if-compare-field";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object notAppliedPayment = null;
        try {
            notAppliedPayment = PaymentWorker.getPaymentNotApplied(delegator, (String) ((Map<String, Object>) serviceInMap).get("paymentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling PaymentWorker.getPaymentNotApplied: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object notAppliedInvoice = null;
        try {
            notAppliedInvoice = InvoiceWorker.getInvoiceNotApplied(delegator, (String) ((Map<String, Object>) serviceInMap).get("invoiceId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling InvoiceWorker.getInvoiceNotApplied: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        BigDecimal zero = BigDecimal.ZERO;
        assert java.util.Objects.equals(notAppliedPayment, zero) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(notAppliedInvoice, zero) : "Assertion failed: if-compare-field";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(paymentApplication);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * test the application of a payment against an billing account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testBillingAppl(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> serviceInMap = new HashMap<>();
        serviceInMap.put("paymentId", "appltest10000");
        serviceInMap.put("billingAccountId", "appltest10000");
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
        serviceInMap.put("userLogin", userLogin);
        Object amountApplied = null;
        Object paymentApplicationId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPaymentApplication", serviceInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            amountApplied = serviceResult.get("amountApplied");
            paymentApplicationId = serviceResult.get("paymentApplicationId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue paymentApplication = null;
        try {
            paymentApplication = EntityQuery.use(delegator)
                    .from("PaymentApplication")
                    .where(UtilMisc.toMap("paymentApplicationId", paymentApplicationId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object notAppliedPayment = null;
        try {
            notAppliedPayment = PaymentWorker.getPaymentNotApplied(delegator, (String) ((Map<String, Object>) serviceInMap).get("paymentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling PaymentWorker.getPaymentNotApplied: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue payment = null;
        try {
            payment = EntityQuery.use(delegator)
                    .from("Payment")
                    .where(UtilMisc.toMap("paymentId", ((Map<String, Object>) serviceInMap).get("paymentId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert !(UtilValidate.isEmpty(paymentApplication)) : "Assertion failed: not if-empty";
        assert java.util.Objects.equals(((Map<String, Object>) paymentApplication).get("billingAccountId"), ((Map<String, Object>) serviceInMap).get("billingAccountId")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(((Map<String, Object>) paymentApplication).get("paymentId"), ((Map<String, Object>) serviceInMap).get("paymentId")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(((Map<String, Object>) paymentApplication).get("amountApplied"), ((Map<String, Object>) payment).get("amount")) : "Assertion failed: if-compare-field";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            notAppliedPayment = PaymentWorker.getPaymentNotApplied(delegator, (String) ((Map<String, Object>) serviceInMap).get("paymentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling PaymentWorker.getPaymentNotApplied: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue billingAccount = null;
        try {
            billingAccount = EntityQuery.use(delegator)
                    .from("BillingAccount")
                    .where(UtilMisc.toMap("billingAccountId", ((Map<String, Object>) serviceInMap).get("billingAccountId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying BillingAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object appliedBillling = null;
        try {
            appliedBillling = OrderReadHelper.getBillingAccountBalance((GenericValue) billingAccount);
        } catch (Exception e) {
            Debug.logError(e, "Error calling OrderReadHelper.getBillingAccountBalance: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        BigDecimal zero = BigDecimal.ZERO;
        assert java.util.Objects.equals(notAppliedPayment, zero) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(context.get("appliedBilling"), context.get("paymentAmount")) : "Assertion failed: if-compare-field";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(paymentApplication);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * test the application of a payment against anotherpayment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testToPayment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> serviceInMap = new HashMap<>();
        serviceInMap.put("paymentId", "appltest10000");
        serviceInMap.put("toPaymentId", "appltest10001");
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
        serviceInMap.put("userLogin", userLogin);
        Object amountApplied = null;
        Object paymentApplicationId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPaymentApplication", serviceInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            amountApplied = serviceResult.get("amountApplied");
            paymentApplicationId = serviceResult.get("paymentApplicationId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue paymentApplication = null;
        try {
            paymentApplication = EntityQuery.use(delegator)
                    .from("PaymentApplication")
                    .where(UtilMisc.toMap("paymentApplicationId", paymentApplicationId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object notAppliedPayment = null;
        try {
            notAppliedPayment = PaymentWorker.getPaymentNotApplied(delegator, (String) ((Map<String, Object>) serviceInMap).get("paymentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling PaymentWorker.getPaymentNotApplied: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue payment = null;
        try {
            payment = EntityQuery.use(delegator)
                    .from("Payment")
                    .where(UtilMisc.toMap("paymentId", ((Map<String, Object>) serviceInMap).get("paymentId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert !(UtilValidate.isEmpty(paymentApplication)) : "Assertion failed: not if-empty";
        assert java.util.Objects.equals(((Map<String, Object>) paymentApplication).get("toPaymentId"), ((Map<String, Object>) serviceInMap).get("toPaymentId")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(((Map<String, Object>) paymentApplication).get("paymentId"), ((Map<String, Object>) serviceInMap).get("paymentId")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(((Map<String, Object>) paymentApplication).get("amountApplied"), ((Map<String, Object>) payment).get("amount")) : "Assertion failed: if-compare-field";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            notAppliedPayment = PaymentWorker.getPaymentNotApplied(delegator, (String) ((Map<String, Object>) serviceInMap).get("paymentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling PaymentWorker.getPaymentNotApplied: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object notAppliedToPayment = null;
        try {
            notAppliedToPayment = PaymentWorker.getPaymentNotApplied(delegator, (String) ((Map<String, Object>) serviceInMap).get("toPaymentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling PaymentWorker.getPaymentNotApplied: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        BigDecimal zero = BigDecimal.ZERO;
        assert java.util.Objects.equals(notAppliedPayment, zero) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(notAppliedToPayment, zero) : "Assertion failed: if-compare-field";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(paymentApplication);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * test the application of a payment against a tax geo id
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String testTaxGeoId(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> serviceInMap = new HashMap<>();
        serviceInMap.put("paymentId", "appltest10000");
        serviceInMap.put("taxAuthGeoId", "UT");
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
        serviceInMap.put("userLogin", userLogin);
        Object amountApplied = null;
        Object paymentApplicationId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPaymentApplication", serviceInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            amountApplied = serviceResult.get("amountApplied");
            paymentApplicationId = serviceResult.get("paymentApplicationId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue paymentApplication = null;
        try {
            paymentApplication = EntityQuery.use(delegator)
                    .from("PaymentApplication")
                    .where(UtilMisc.toMap("paymentApplicationId", paymentApplicationId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue payment = null;
        try {
            payment = EntityQuery.use(delegator)
                    .from("Payment")
                    .where(UtilMisc.toMap("paymentId", ((Map<String, Object>) serviceInMap).get("paymentId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        assert !(UtilValidate.isEmpty(paymentApplication)) : "Assertion failed: not if-empty";
        assert java.util.Objects.equals(((Map<String, Object>) paymentApplication).get("taxAuthGeoId"), ((Map<String, Object>) serviceInMap).get("taxAuthGeoId")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(((Map<String, Object>) paymentApplication).get("paymentId"), ((Map<String, Object>) serviceInMap).get("paymentId")) : "Assertion failed: if-compare-field";
        assert java.util.Objects.equals(((Map<String, Object>) paymentApplication).get("amountApplied"), ((Map<String, Object>) payment).get("amount")) : "Assertion failed: if-compare-field";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object notAppliedPayment = null;
        try {
            notAppliedPayment = PaymentWorker.getPaymentNotApplied(delegator, (String) ((Map<String, Object>) serviceInMap).get("paymentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling PaymentWorker.getPaymentNotApplied: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        BigDecimal zero = BigDecimal.ZERO;
        assert java.util.Objects.equals(notAppliedPayment, zero) : "Assertion failed: if-compare-field";
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        try {
            delegator.removeValue(paymentApplication);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }

}

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
import java.sql.Timestamp;
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
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.UtilDateTime;
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
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/payment/PaymentServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PaymentServices {

    private static final String MODULE = PaymentServices.class.getName();


    /**
     * Create a Payment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPayment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue payment = null;
        GenericValue paymentMethod = null;
        GenericValue orderPaymentPreference = null;
        if ((!(true /* TODO: if-has-permission */) && !(true /* TODO: if-has-permission */) && !(java.util.Objects.equals(((Map<String, Object>) userLogin).get("partyId"), context.get("partyIdFrom"))) && !(java.util.Objects.equals(((Map<String, Object>) userLogin).get("partyId"), context.get("partyIdTo"))))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingCreatePaymentPermissionError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        payment = delegator.makeValue("Payment");
        if (UtilValidate.isEmpty(context.get("paymentId"))) {
            ((GenericValue) payment).put("paymentId", delegator.getNextSeqId("Payment"));
        } else {
            payment.put("paymentId", context.get("paymentId"));
        }
        result.put("paymentId", ((Map<String, Object>) payment).get("paymentId"));
        if (UtilValidate.isEmpty(context.get("statusId"))) {
            context.put("statusId", "PMNT_NOT_PAID");
        }
        if (UtilValidate.isNotEmpty(context.get("paymentMethodId"))) {
            try {
                paymentMethod = EntityQuery.use(delegator)
                        .from("PaymentMethod")
                        .where(UtilMisc.toMap("paymentMethodId", context.get("paymentMethodId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentMethod: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (!java.util.Objects.equals(context.get("paymentMethodTypeId"), ((Map<String, Object>) paymentMethod).get("paymentMethodTypeId"))) {
                Debug.logInfo("Replacing passed payment method type [" + context.get("paymentMethodTypeId") + "] with payment method type [" + ((Map<String, Object>) paymentMethod).get("paymentMethodTypeId") + "] for payment method [" + context.get("paymentMethodId") + "]", MODULE);
                context.put("paymentMethodTypeId", ((Map<String, Object>) paymentMethod).get("paymentMethodTypeId"));
            }
        }
        if (UtilValidate.isNotEmpty(context.get("paymentPreferenceId"))) {
            try {
                orderPaymentPreference = EntityQuery.use(delegator)
                        .from("OrderPaymentPreference")
                        .where(UtilMisc.toMap("orderPaymentPreferenceId", context.get("paymentPreferenceId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderPaymentPreference: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(context.get("paymentMethodId"))) {
                context.put("paymentMethodId", ((Map<String, Object>) orderPaymentPreference).get("paymentMethodId"));
            }
            if (UtilValidate.isEmpty(context.get("paymentMethodTypeId"))) {
                context.put("paymentMethodTypeId", ((Map<String, Object>) orderPaymentPreference).get("paymentMethodTypeId"));
            }
        }
        if (UtilValidate.isEmpty(context.get("paymentMethodTypeId"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentMethodIdPaymentMethodTypeIdNullError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        payment.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) payment).get("effectiveDate"))) {
            Timestamp payment_effectiveDate = new Timestamp(System.currentTimeMillis());
        }
        try {
            delegator.create(payment);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update a Payment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePayment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue newPayment = null;
        GenericValue oldPayment = null;
        GenericValue paymentMethod = null;
        GenericValue payment = null;
        Map<String, Object> param = null;
        GenericValue lookupPayment = delegator.makeValue("Payment");
        lookupPayment.setPKFields((Map<String, Object>) context);
        try {
            payment = EntityQuery.use(delegator)
                    .from("Payment")
                    .where(lookupPayment)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ((!(true /* TODO: if-has-permission */) && !(true /* TODO: if-has-permission */) && !(java.util.Objects.equals(((Map<String, Object>) userLogin).get("partyId"), ((Map<String, Object>) payment).get("partyIdFrom"))) && !(java.util.Objects.equals(((Map<String, Object>) userLogin).get("partyId"), ((Map<String, Object>) payment).get("partyIdTo"))))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingUpdatePaymentPermissionError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (!"PMNT_NOT_PAID".equals(((Map<String, Object>) payment).get("statusId"))) {
            newPayment = delegator.makeValue("Payment");
            oldPayment = delegator.makeValue("Payment");
            newPayment.setNonPKFields((Map<String, Object>) payment);
            oldPayment.setNonPKFields((Map<String, Object>) payment);
            newPayment.setNonPKFields((Map<String, Object>) context);
            oldPayment.put("statusId", ((Map<String, Object>) newPayment).get("statusId"));
            oldPayment.put("comments", ((Map<String, Object>) newPayment).get("comments"));
            oldPayment.put("paymentRefNum", ((Map<String, Object>) newPayment).get("paymentRefNum"));
            oldPayment.put("finAccountTransId", ((Map<String, Object>) newPayment).get("finAccountTransId"));
            if (!java.util.Objects.equals(oldPayment, newPayment)) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPSUpdateNotAllowedBecauseOfStatus", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object statusIdSave = ((Map<String, Object>) payment).get("statusId");
        payment.setNonPKFields((Map<String, Object>) context);
        payment.put("statusId", statusIdSave);
        if (UtilValidate.isEmpty(((Map<String, Object>) payment).get("effectiveDate"))) {
            Timestamp payment_effectiveDate = new Timestamp(System.currentTimeMillis());
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) payment).get("paymentMethodId"))) {
            try {
                paymentMethod = EntityQuery.use(delegator)
                        .from("PaymentMethod")
                        .where(UtilMisc.toMap("paymentMethodId", ((Map<String, Object>) payment).get("paymentMethodId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentMethod: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (!java.util.Objects.equals(((Map<String, Object>) payment).get("paymentMethodTypeId"), ((Map<String, Object>) paymentMethod).get("paymentMethodTypeId"))) {
                Debug.logInfo("Replacing passed payment method type [" + ((Map<String, Object>) payment).get("paymentMethodTypeId") + "] with payment method type [" + ((Map<String, Object>) paymentMethod).get("paymentMethodTypeId") + "] for payment method [" + ((Map<String, Object>) payment).get("paymentMethodId") + "]", MODULE);
            }
            payment.put("paymentMethodTypeId", ((Map<String, Object>) paymentMethod).get("paymentMethodTypeId"));
        }
        try {
            delegator.store(payment);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(context.get("statusId"))) {
            if (!java.util.Objects.equals(context.get("statusId"), statusIdSave)) {
                // set-service-fields from "parameters" to "param" for service "setPaymentStatus"
                param.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setPaymentStatus", param);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling setPaymentStatus: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Create a Payment Application
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPaymentApplication(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object notAppliedInvoice = null;
        Boolean actual = null;
        Object notAppliedPayment = null;
        GenericValue paymentAppl = null;
        GenericValue invoice = null;
        GenericValue toPayment = null;
        Object notAppliedToPayment = null;
        GenericValue toPaymentType = null;
        GenericValue payment = null;
        GenericValue paymentType = null;
        if (UtilValidate.isEmpty(context.get("invoiceId"))) {
            if (UtilValidate.isEmpty(context.get("billingAccountId"))) {
                if (UtilValidate.isEmpty(context.get("taxAuthGeoId"))) {
                    if (UtilValidate.isEmpty(context.get("toPaymentId"))) {
                        {
                            String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentApplicationParameterMissing", locale);
                            error_list.add(errorMsg);
                            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                        }
                        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                            return "error";
                        }
                    }
                }
            }
        }
        paymentAppl = delegator.makeValue("PaymentApplication");
        paymentAppl.setNonPKFields((Map<String, Object>) context);
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
        if (UtilValidate.isEmpty(payment)) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentApplicationParameterMissing", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        try {
            notAppliedPayment = PaymentWorker.getPaymentNotApplied((GenericValue) payment);
        } catch (Exception e) {
            Debug.logError(e, "Error calling PaymentWorker.getPaymentNotApplied: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(context.get("invoiceId"))) {
            try {
                invoice = EntityQuery.use(delegator)
                        .from("Invoice")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if ((!java.util.Objects.equals(((Map<String, Object>) invoice).get("currencyUomId"), ((Map<String, Object>) payment).get("currencyUomId")) && !java.util.Objects.equals(((Map<String, Object>) invoice).get("currencyUomId"), ((Map<String, Object>) payment).get("actualCurrencyUomId")))) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingCurrenciesOfInvoiceAndPaymentNotCompatible", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            if ((!java.util.Objects.equals(((Map<String, Object>) invoice).get("currencyUomId"), ((Map<String, Object>) payment).get("currencyUomId")) && java.util.Objects.equals(((Map<String, Object>) invoice).get("currencyUomId"), ((Map<String, Object>) payment).get("actualCurrencyUomId")))) {
                actual = Boolean.TRUE;
                try {
                    notAppliedPayment = PaymentWorker.getPaymentNotApplied((GenericValue) payment, (Boolean) actual);
                } catch (Exception e) {
                    Debug.logError(e, "Error calling PaymentWorker.getPaymentNotApplied: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            try {
                notAppliedInvoice = InvoiceWorker.getInvoiceNotApplied((GenericValue) invoice);
            } catch (Exception e) {
                Debug.logError(e, "Error calling InvoiceWorker.getInvoiceNotApplied: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (notAppliedInvoice != null /* TODO: field compare operator less-equals */) {
                paymentAppl.put("amountApplied", notAppliedInvoice);
            } else {
                paymentAppl.put("amountApplied", notAppliedPayment);
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) invoice).get("billingAccountId"))) {
                paymentAppl.put("billingAccountId", ((Map<String, Object>) invoice).get("billingAccountId"));
            }
        }
        if (UtilValidate.isNotEmpty(context.get("toPaymentId"))) {
            try {
                toPayment = EntityQuery.use(delegator)
                        .from("Payment")
                        .where(UtilMisc.toMap("paymentId", context.get("toPaymentId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                toPaymentType = EntityQuery.use(delegator)
                        .from("PaymentType")
                        .where(UtilMisc.toMap("paymentTypeId", ((Map<String, Object>) toPayment).get("paymentTypeId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                payment = EntityQuery.use(delegator)
                        .from("Payment")
                        .where(UtilMisc.toMap("paymentId", context.get("paymentId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                paymentType = EntityQuery.use(delegator)
                        .from("PaymentType")
                        .where(UtilMisc.toMap("paymentTypeId", ((Map<String, Object>) payment).get("paymentTypeId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(context.get("amountApplied"))) {
                try {
                    notAppliedPayment = PaymentWorker.getPaymentNotApplied((GenericValue) payment);
                } catch (Exception e) {
                    Debug.logError(e, "Error calling PaymentWorker.getPaymentNotApplied: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                try {
                    notAppliedToPayment = PaymentWorker.getPaymentNotApplied((GenericValue) toPayment);
                } catch (Exception e) {
                    Debug.logError(e, "Error calling PaymentWorker.getPaymentNotApplied: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (notAppliedPayment != null /* TODO: field compare operator less */) {
                    paymentAppl.put("amountApplied", notAppliedPayment);
                } else {
                    paymentAppl.put("amountApplied", notAppliedToPayment);
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("billingAccountId"))) {
            if (UtilValidate.isEmpty(((Map<String, Object>) paymentAppl).get("amountApplied"))) {
                paymentAppl.put("amountApplied", notAppliedPayment);
            }
        }
        if (UtilValidate.isNotEmpty(context.get("taxAuthGeoId"))) {
            if (UtilValidate.isEmpty(((Map<String, Object>) paymentAppl).get("amountApplied"))) {
                paymentAppl.put("amountApplied", notAppliedPayment);
            }
        }
        ((GenericValue) paymentAppl).put("paymentApplicationId", delegator.getNextSeqId("PaymentApplication"));
        result.put("amountApplied", ((Map<String, Object>) paymentAppl).get("amountApplied"));
        result.put("paymentApplicationId", ((Map<String, Object>) paymentAppl).get("paymentApplicationId"));
        try {
            delegator.create(paymentAppl);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
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
        result.put("paymentTypeId", ((Map<String, Object>) payment).get("paymentTypeId"));

        return "success";
    }


    /**
     * Set The Payment Status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setPaymentStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> paymentApplications = null;
        GenericValue statusChange = null;
        Map<String, Object> updateOrderPaymentPreferenceMap = null;
        GenericValue orderPaymentPreference = null;
        Map<String, Object> removePaymentApplicationMap = null;
        GenericValue payment = null;
        Object notYetApplied = null;
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
        GenericValue statusItem = null;
        try {
            statusItem = EntityQuery.use(delegator)
                    .from("StatusItem")
                    .where(UtilMisc.toMap("statusId", context.get("statusId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying StatusItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("oldStatusId", ((Map<String, Object>) payment).get("statusId"));
        if (!java.util.Objects.equals(((Map<String, Object>) payment).get("statusId"), context.get("statusId"))) {
            try {
                statusChange = EntityQuery.use(delegator)
                        .from("StatusValidChange")
                        .where(UtilMisc.toMap("statusId", ((Map<String, Object>) payment).get("statusId"), "statusIdTo", context.get("statusId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            GenericValue paymentApplication = null;
            if (UtilValidate.isEmpty(statusChange)) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPSInvalidStatusChange", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                Debug.logError("Cannot change from " + ((Map<String, Object>) payment).get("statusId") + " to " + context.get("statusId"), MODULE);
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            } else {
                if ((("PMNT_RECEIVED".equals(context.get("statusId")) || "PMNT_SENT".equals(context.get("statusId"))) && UtilValidate.isEmpty(((Map<String, Object>) payment).get("paymentMethodId")))) {
                    {
                        String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingMissingPaymentMethod", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                    Debug.logError("Cannot set status to " + context.get("statusId") + " on payment " + ((Map<String, Object>) payment).get("paymentId") + ": payment method is missing", MODULE);
                    if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                            UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                        return "error";
                    }
                }
                if ("PMNT_CONFIRMED".equals(context.get("statusId"))) {
                    notYetApplied = GroovyUtil.eval("org.ofbiz.accounting.payment.PaymentWorker.getPaymentNotApplied(payment)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                    if (((Comparable) notYetApplied).compareTo(new BigDecimal("0.00")) > 0) {
                        {
                            String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPSNotConfirmedNotFullyApplied", locale);
                            error_list.add(errorMsg);
                            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                        }
                        Debug.logError("Cannot change from " + ((Map<String, Object>) payment).get("statusId") + " to " + context.get("statusId") + ", payment not fully applied: " + context.get("notYetapplied"), MODULE);
                        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                            return "error";
                        }
                    }
                }
                if ("PMNT_CANCELLED".equals(context.get("statusId"))) {
                    try {
                        paymentApplications = payment.getRelated("PaymentApplication", null, null, false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related PaymentApplication: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (paymentApplications != null) {
                        for (GenericValue paymentApplicationEntry : paymentApplications) {
                            removePaymentApplicationMap.put("paymentApplicationId", ((Map<String, Object>) paymentApplicationEntry).get("paymentApplicationId"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("removePaymentApplication", removePaymentApplicationMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling removePaymentApplication: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                    }
                    try {
                        orderPaymentPreference = payment.getRelatedOne("OrderPaymentPreference", false);
                    } catch (Exception e) {
                        Debug.logError(e, "Error getting related one OrderPaymentPreference: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isNotEmpty(orderPaymentPreference)) {
                        updateOrderPaymentPreferenceMap.put("orderPaymentPreferenceId", ((Map<String, Object>) orderPaymentPreference).get("orderPaymentPreferenceId"));
                        updateOrderPaymentPreferenceMap.put("statusId", "PAYMENT_CANCELLED");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("updateOrderPaymentPreference", updateOrderPaymentPreferenceMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling updateOrderPaymentPreference: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
                payment.put("statusId", context.get("statusId"));
                try {
                    delegator.store(payment);
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
     * Update a Payment then set it to status PMNT_SENT
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String quickSendPayment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePayment", context);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePayment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> param = new HashMap<>();
        param.put("paymentId", context.get("paymentId"));
        param.put("statusId", "PMNT_SENT");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("setPaymentStatus", param);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling setPaymentStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create a payment and a payment application for the full amount
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPaymentAndApplication(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> createPaymentInMap = new HashMap<>();
        // set-service-fields from "parameters" to "createPaymentInMap" for service "createPayment"
        createPaymentInMap.putAll(UtilMisc.toMap(context));
        Object paymentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPayment", createPaymentInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            paymentId = serviceResult.get("paymentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPayment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> createPaymentAppInMap = new HashMap<>();
        // set-service-fields from "parameters" to "createPaymentAppInMap" for service "createPaymentApplication"
        createPaymentAppInMap.putAll(UtilMisc.toMap(context));
        createPaymentAppInMap.put("paymentId", paymentId);
        createPaymentAppInMap.put("amountApplied", context.get("amount"));
        Object paymentApplicationId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPaymentApplication", createPaymentAppInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            paymentApplicationId = serviceResult.get("paymentApplicationId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        result.put("paymentId", paymentId);
        result.put("paymentApplicationId", paymentApplicationId);

        return "success";
    }


    /**
     * Create a list with information on payment due dates and amounts for the invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getInvoicePaymentInfoList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue invoice = null;
        BigDecimal computedTotalAmount = null;
        BigDecimal invoiceTermAmount = null;
        GenericValue invoiceTerm = null;
        Object invoicePaymentInfo = null;
        GenericValue termType = null;
        BigDecimal remainingAppliedAmount = null;
        List<Object> invoicePaymentInfoList = null;
        Map<String, Object> andMap = null;
        if (UtilValidate.isEmpty(context.get("invoice"))) {
            try {
                invoice = EntityQuery.use(delegator)
                        .from("Invoice")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            invoice = (GenericValue) context.get("invoice");
        }
        Object invoiceTotalAmount = null;
        try {
            invoiceTotalAmount = InvoiceWorker.getInvoiceTotal(invoice);
        } catch (Exception e) {
            Debug.logError(e, "Error calling InvoiceWorker.getInvoiceTotal: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object invoiceTotalAmountPaid = null;
        try {
            invoiceTotalAmountPaid = InvoiceWorker.getInvoiceApplied(invoice);
        } catch (Exception e) {
            Debug.logError(e, "Error calling InvoiceWorker.getInvoiceApplied: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> invoiceTerms = null;
        try {
            invoiceTerms = invoice.getRelated("InvoiceTerm", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related InvoiceTerm: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        remainingAppliedAmount = BigDecimal.valueOf(((Number) invoiceTotalAmountPaid).doubleValue());
        computedTotalAmount = new BigDecimal("0.0");
        if (invoiceTerms != null) {
            for (GenericValue invoiceTerm_iter : invoiceTerms) {
                invoiceTerm = invoiceTerm_iter;
                try {
                    termType = invoiceTerm.getRelatedOne("TermType", true);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one TermType: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if ("FIN_PAYMENT_TERM".equals(((Map<String, Object>) termType).get("parentTypeId"))) {
                    invoicePaymentInfo = null;
                    ((Map<String, Object>) invoicePaymentInfo).put("invoiceId", ((Map<String, Object>) invoice).get("invoiceId"));
                    ((Map<String, Object>) invoicePaymentInfo).put("invoiceTermId", ((Map<String, Object>) invoiceTerm).get("invoiceTermId"));
                    ((Map<String, Object>) invoicePaymentInfo).put("termTypeId", ((Map<String, Object>) invoiceTerm).get("termTypeId"));
                    try {
                        ((Map<String, Object>) invoicePaymentInfo).put("dueDate", UtilDateTime.getDayEnd((Timestamp) ((Map<String, Object>) invoice).get("invoiceDate"), (Long) ((Map<String, Object>) invoiceTerm).get("termDays")));
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling UtilDateTime.getDayEnd: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    invoiceTermAmount = new BigDecimal(((Map<String, Object>) invoiceTerm).get("termValue").toString());
                    invoiceTermAmount = new BigDecimal(invoiceTermAmount.toString());
                    ((Map<String, Object>) invoicePaymentInfo).put("amount", invoiceTermAmount);
                    computedTotalAmount = new BigDecimal(((Map<String, Object>) invoicePaymentInfo).get("amount").toString());
                    if (remainingAppliedAmount != null /* TODO: field compare operator greater-equals */) {
                        ((Map<String, Object>) invoicePaymentInfo).put("paidAmount", invoiceTermAmount);
                        remainingAppliedAmount = new BigDecimal(invoiceTermAmount.toString());
                    } else {
                        ((Map<String, Object>) invoicePaymentInfo).put("paidAmount", remainingAppliedAmount);
                        remainingAppliedAmount = new BigDecimal("0.0");
                    }
                    ((Map<String, Object>) invoicePaymentInfo).put("outstandingAmount", new BigDecimal(((Map<String, Object>) invoicePaymentInfo).get("paidAmount").toString()));
                    invoicePaymentInfoList.add(invoicePaymentInfo);
                }
            }
        }
        Object andMap_termTypeId = null;
        Object invoicePaymentInfo_termTypeId = null;
        Object invoicePaymentInfo_invoiceId = null;
        BigDecimal invoicePaymentInfo_paidAmount = null;
        Object invoicePaymentInfoList__ = null;
        if ((((Comparable) remainingAppliedAmount).compareTo(new BigDecimal("0.0")) > 0 || ((Comparable) invoiceTotalAmount).compareTo(new BigDecimal("0.0")) <= 0 || computedTotalAmount != null /* TODO: field compare operator less */)) {
            invoicePaymentInfo = null;
            andMap.put("termTypeId", "FIN_PAYMENT_TERM");
            // TODO: Convert <filter-list-by-and> element
            invoiceTerm = EntityUtil.getFirst((List<GenericValue>) invoiceTerms);
            if (UtilValidate.isNotEmpty(invoiceTerm)) {
                ((Map<String, Object>) invoicePaymentInfo).put("termTypeId", ((Map<String, Object>) invoiceTerm).get("termTypeId"));
                try {
                    ((Map<String, Object>) invoicePaymentInfo).put("dueDate", UtilDateTime.getDayEnd((Timestamp) ((Map<String, Object>) invoice).get("invoiceDate"), (Long) ((Map<String, Object>) invoiceTerm).get("termDays")));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling UtilDateTime.getDayEnd: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                try {
                    ((Map<String, Object>) invoicePaymentInfo).put("dueDate", UtilDateTime.getDayEnd((Timestamp) ((Map<String, Object>) invoice).get("invoiceDate")));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling UtilDateTime.getDayEnd: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            ((Map<String, Object>) invoicePaymentInfo).put("invoiceId", ((Map<String, Object>) invoice).get("invoiceId"));
            ((Map<String, Object>) invoicePaymentInfo).put("amount", new BigDecimal(computedTotalAmount.toString()));
            ((Map<String, Object>) invoicePaymentInfo).put("paidAmount", remainingAppliedAmount);
            ((Map<String, Object>) invoicePaymentInfo).put("outstandingAmount", new BigDecimal(((Map<String, Object>) invoicePaymentInfo).get("paidAmount").toString()));
            invoicePaymentInfoList.add(invoicePaymentInfo);
        }
        result.put("invoicePaymentInfoList", invoicePaymentInfoList);

        return "success";
    }


    /**
     * Select a list with information on payment due dates and amounts for invoices.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getInvoicePaymentInfoListByDueDateOffset(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object getInvoicePaymentInfoListInMap = null;
        Object invoicePaymentInfoList = null;
        List<Object> selectedInvoicePaymentInfoList = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        Object asOfDate = null;
        try {
            asOfDate = UtilDateTime.getDayEnd((Timestamp) nowTimestamp, (Long) context.get("daysOffset"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilDateTime.getDayEnd: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> invoices = null;
        try {
            invoices = EntityQuery.use(delegator)
                    .from("Invoice")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (invoices != null) {
            for (GenericValue invoice : invoices) {
                getInvoicePaymentInfoListInMap = null;
                ((Map<String, Object>) getInvoicePaymentInfoListInMap).put("invoice", invoice);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("getInvoicePaymentInfoList", (Map<String, Object>) getInvoicePaymentInfoListInMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    invoicePaymentInfoList = serviceResult.get("invoicePaymentInfoList");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling getInvoicePaymentInfoList: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (invoicePaymentInfoList != null) {
                    for (Object invoicePaymentInfo : (List<Object>) invoicePaymentInfoList) {
                        Object selectedInvoicePaymentInfoList__ = null;
                        if ((((Comparable) ((Map<String, Object>) invoicePaymentInfo).get("outstandingAmount")).compareTo(new BigDecimal("0.0")) > 0 && ((Map<String, Object>) invoicePaymentInfo).get("dueDate") != null /* TODO: field compare operator less */)) {
                            selectedInvoicePaymentInfoList.add(invoicePaymentInfo);
                        }
                    }
                }
            }
        }
        result.put("invoicePaymentInfoList", selectedInvoicePaymentInfoList);

        return "success";
    }


    /**
     * Service to void a payment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String voidPayment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> invoiceStatusCtx = null;
        GenericValue updateInvoiceCtx = null;
        Map<String, Object> removePaymentApplicationCtx = null;
        Object copyAcctgTransCtx = null;
        Object postAcctgTransMap = null;
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
        result.put("finAccountTransId", ((Map<String, Object>) payment).get("finAccountTransId"));
        Object transStatusId = "FINACT_TRNS_CANCELED";
        result.put("statusId", transStatusId);
        if (UtilValidate.isEmpty(payment)) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingNoPaymentsfound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        Object paymentId = context.get("paymentId");
        Map<String, Object> paymentStatusCtx = new HashMap<>();
        paymentStatusCtx.put("paymentId", paymentId);
        paymentStatusCtx.put("statusId", "PMNT_VOID");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("setPaymentStatus", paymentStatusCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling setPaymentStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> paymentApplications = null;
        try {
            paymentApplications = payment.getRelated("PaymentApplication", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related PaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (paymentApplications != null) {
            for (GenericValue paymentApplication : paymentApplications) {
                try {
                    updateInvoiceCtx = paymentApplication.getRelatedOne("Invoice", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one Invoice: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if ("INVOICE_PAID".equals(((Map<String, Object>) updateInvoiceCtx).get("statusId"))) {
                    // set-service-fields from "updateInvoiceCtx" to "invoiceStatusCtx" for service "updateInvoice"
                    invoiceStatusCtx.putAll(UtilMisc.toMap(updateInvoiceCtx));
                    invoiceStatusCtx.put("paidDate", new HashMap<String, Object>());
                    invoiceStatusCtx.put("statusId", "INVOICE_READY");
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("setInvoiceStatus", invoiceStatusCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling setInvoiceStatus: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                removePaymentApplicationCtx.put("paymentApplicationId", ((Map<String, Object>) paymentApplication).get("paymentApplicationId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("removePaymentApplication", removePaymentApplicationCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling removePaymentApplication: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        List<GenericValue> acctgTransPaymentList = null;
        try {
            acctgTransPaymentList = EntityQuery.use(delegator)
                    .from("AcctgTrans")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AcctgTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (acctgTransPaymentList != null) {
            for (GenericValue acctgTransPayment : acctgTransPaymentList) {
                copyAcctgTransCtx = null;
                ((Map<String, Object>) copyAcctgTransCtx).put("fromAcctgTransId", ((Map<String, Object>) acctgTransPayment).get("acctgTransId"));
                ((Map<String, Object>) copyAcctgTransCtx).put("revert", "Y");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("copyAcctgTransAndEntries", (Map<String, Object>) copyAcctgTransCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    ((Map<String, Object>) postAcctgTransMap).put("acctgTransId", serviceResult.get("acctgTransId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling copyAcctgTransAndEntries: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if ("Y".equals(((Map<String, Object>) acctgTransPayment).get("isPosted"))) {
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("postAcctgTrans", (Map<String, Object>) postAcctgTransMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling postAcctgTrans: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                postAcctgTransMap = null;
            }
        }

        return "success";
    }


    /**
     * calculate running total for payments
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPaymentRunningTotal(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        BigDecimal runningTotal = null;
        String currencyUomId = null;
        Object paymentIds = context.get("paymentIds");
        runningTotal = BigDecimal.ZERO;
        List<GenericValue> payments = null;
        try {
            payments = EntityQuery.use(delegator)
                    .from("Payment")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (payments != null) {
            for (GenericValue payment : payments) {
                runningTotal = (BigDecimal) ((BigDecimal) runningTotal).add((BigDecimal) ((Map<String, Object>) payment).get("amount"));
            }
        }
        Map<String, Object> getPartyAccountingPreferencesMap = new HashMap<>();
        // set-service-fields from "parameters" to "getPartyAccountingPreferencesMap" for service "getPartyAccountingPreferences"
        getPartyAccountingPreferencesMap.putAll(UtilMisc.toMap(context));
        Object partyAccountingPreference = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getPartyAccountingPreferences", getPartyAccountingPreferencesMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            partyAccountingPreference = serviceResult.get("partyAccountingPreference");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getPartyAccountingPreferences: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        currencyUomId = (String) ((Map<String, Object>) partyAccountingPreference).get("baseCurrencyUomId");
        if (UtilValidate.isEmpty(currencyUomId)) {
            currencyUomId = UtilProperties.getMessage("general", "currency.uom.id.default", locale);
        }
        Object paymentRunningTotal = GroovyUtil.eval("org.ofbiz.base.util.UtilFormatOut.formatCurrency(runningTotal, currencyUomId, parameters.locale)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        result.put("paymentRunningTotal", paymentRunningTotal);

        return "success";
    }


    /**
     * cancel payment batch
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String cancelPaymentBatch(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue paymentGroupMemberAndTrans = null;
        Map<String, Object> setFinAccountTransStatusMap = null;
        GenericValue finAccountTrans = null;
        Map<String, Object> expirePaymentGroupMemberMap = null;
        List<GenericValue> paymentGroupMemberAndTransList = null;
        try {
            paymentGroupMemberAndTransList = EntityQuery.use(delegator)
                    .from("PmtGrpMembrPaymentAndFinAcctTrans")
                    .where(UtilMisc.toMap("paymentGroupId", context.get("paymentGroupId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PmtGrpMembrPaymentAndFinAcctTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(paymentGroupMemberAndTransList)) {
            paymentGroupMemberAndTrans = EntityUtil.getFirst((List<GenericValue>) paymentGroupMemberAndTransList);
            if ("FINACT_TRNS_APPROVED".equals(((Map<String, Object>) paymentGroupMemberAndTrans).get("finAccountTransStatusId"))) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingTransactionIsAlreadyReconciled", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            if (paymentGroupMemberAndTransList != null) {
                for (GenericValue paymentGroupMemberAndTrans_iter : paymentGroupMemberAndTransList) {
                    paymentGroupMemberAndTrans = paymentGroupMemberAndTrans_iter;
                    // set-service-fields from "paymentGroupMemberAndTrans" to "expirePaymentGroupMemberMap" for service "expirePaymentGroupMember"
                    expirePaymentGroupMemberMap.putAll(UtilMisc.toMap(paymentGroupMemberAndTrans));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("expirePaymentGroupMember", expirePaymentGroupMemberMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling expirePaymentGroupMember: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    try {
                        finAccountTrans = EntityQuery.use(delegator)
                                .from("FinAccountTrans")
                                .where(UtilMisc.toMap("finAccountTransId", ((Map<String, Object>) paymentGroupMemberAndTrans).get("finAccountTransId")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying FinAccountTrans: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isNotEmpty(finAccountTrans)) {
                        // set-service-fields from "finAccountTrans" to "setFinAccountTransStatusMap" for service "setFinAccountTransStatus"
                        setFinAccountTransStatusMap.putAll(UtilMisc.toMap(finAccountTrans));
                        setFinAccountTransStatusMap.put("statusId", "FINACT_TRNS_CANCELED");
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("setFinAccountTransStatus", setFinAccountTransStatusMap);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling setFinAccountTransStatus: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Creates Payments, PaymentApplications and PaymentGroup for the same
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPaymentAndPaymentGroupForInvoices(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> partyInvoices = null;
        GenericValue invoice = null;
        Map<String, Object> createPaymentAndApplicationForPartyMap = null;
        List<Object> paymentIds = null;
        Object paymentId = null;
        Object paymentGroupId = null;
        Map<String, Object> createPaymentGroupAndMemberMap = null;
        String errorMessage = null;
        GenericValue paymentMethod = null;
        try {
            paymentMethod = EntityQuery.use(delegator)
                    .from("PaymentMethod")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentMethod: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue finAccount = null;
        try {
            finAccount = EntityQuery.use(delegator)
                    .from("FinAccount")
                    .where(UtilMisc.toMap("finAccountId", ((Map<String, Object>) paymentMethod).get("finAccountId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying FinAccount: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("FNACT_MANFROZEN".equals(((Map<String, Object>) finAccount).get("statusId"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels.xml", "AccountingFinAccountInactiveStatusError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if ("FNACT_CANCELLED".equals(((Map<String, Object>) finAccount).get("statusId"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingErrorUiLabels.xml", "AccountingFinAccountStatusNotValidError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object invoices = null;
        if (context.get("invoiceIds") != null) {
            for (GenericValue invoiceId : (List<GenericValue>) context.get("invoiceIds")) {
                try {
                    invoice = EntityQuery.use(delegator)
                            .from("Invoice")
                            .where(UtilMisc.toMap())
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                invoices = null;
                invoices = ((Map<String, Object>) partyInvoices.get("${invoice")).get("partyIdFrom}");
                ((List<Object>) invoices).add(invoice);
                partyInvoices.put((String) ((Map<String, Object>) invoice).get("partyIdFrom"), invoices);
            }
        }
        invoices = null;
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) partyInvoices).entrySet()) {
            String partyId = entry.getKey();
            invoices = entry.getValue();
            // set-service-fields from "parameters" to "createPaymentAndApplicationForPartyMap" for service "createPaymentAndApplicationForParty"
            createPaymentAndApplicationForPartyMap.putAll(UtilMisc.toMap(context));
            createPaymentAndApplicationForPartyMap.put("paymentMethodTypeId", ((Map<String, Object>) paymentMethod).get("paymentMethodTypeId"));
            createPaymentAndApplicationForPartyMap.put("finAccountId", ((Map<String, Object>) paymentMethod).get("finAccountId"));
            createPaymentAndApplicationForPartyMap.put("partyId", partyId);
            createPaymentAndApplicationForPartyMap.put("invoices", invoices);
            if (UtilValidate.isNotEmpty(context.get("checkStartNumber"))) {
                context.put("checkStartNumber", ((Number) context.get("checkStartNumber")).longValue() + 1L);
            }
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPaymentAndApplicationForParty", createPaymentAndApplicationForPartyMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                paymentId = serviceResult.get("paymentId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPaymentAndApplicationForParty: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            paymentIds.add(paymentId);
        }
        if (UtilValidate.isNotEmpty(paymentIds)) {
            createPaymentGroupAndMemberMap.put("paymentIds", paymentIds);
            createPaymentGroupAndMemberMap.put("paymentGroupTypeId", "CHECK_RUN");
            createPaymentGroupAndMemberMap.put("paymentGroupName", "Payment group for Check Run(InvoiceIds-" + context.get("invoiceIds") + ")");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPaymentGroupAndMember", createPaymentGroupAndMemberMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                paymentGroupId = serviceResult.get("paymentGroupId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPaymentGroupAndMember: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isEmpty(paymentGroupId)) {
            errorMessage = UtilProperties.getMessage("AccountingUiLabels", "AccountingNoInvoicesReadyOrOutstandingAmountZero", locale);
            result.put("errorMessage", errorMessage);
        }

        return "success";
    }


    /**
     * create Payment and PaymentApplications for multiple invoices for one party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPaymentAndApplicationForParty(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> getInvoicePaymentInfoListCtx = null;
        GenericValue invoicePaymentInfo = null;
        GenericValue invoice = null;
        Object invoicePaymentInfoList = null;
        Object paymentAmount = null;
        Map<String, Object> createPaymentMap = null;
        Map<String, Object> createPaymentApplicationMap = null;
        Object paymentId = null;
        Map<String, Object> getPartyAccountingPreferencesMap = null;
        Object partyAcctgPreference = null;
        List<Object> invoiceIds = null;
        paymentAmount = BigDecimal.ZERO;
        if (context.get("invoices") != null) {
            for (GenericValue invoice_iter : (List<GenericValue>) context.get("invoices")) {
                invoice = invoice_iter;
                if ("INVOICE_READY".equals(((Map<String, Object>) invoice).get("statusId"))) {
                    // set-service-fields from "invoice" to "getInvoicePaymentInfoListCtx" for service "getInvoicePaymentInfoList"
                    getInvoicePaymentInfoListCtx.putAll(UtilMisc.toMap(invoice));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("getInvoicePaymentInfoList", getInvoicePaymentInfoListCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                        invoicePaymentInfoList = serviceResult.get("invoicePaymentInfoList");
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling getInvoicePaymentInfoList: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    invoicePaymentInfo = EntityUtil.getFirst((List<GenericValue>) invoicePaymentInfoList);
                    paymentAmount = ((BigDecimal) paymentAmount).add((BigDecimal) ((Map<String, Object>) invoicePaymentInfo).get("outstandingAmount"));
                } else {
                    {
                        String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingInvoicesRequiredInReadyStatus", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                    if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                            UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                        return "error";
                    }
                }
            }
        }
        if (((Comparable) paymentAmount).compareTo(BigDecimal.ZERO) > 0) {
            // set-service-fields from "parameters" to "getPartyAccountingPreferencesMap" for service "getPartyAccountingPreferences"
            getPartyAccountingPreferencesMap.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getPartyAccountingPreferences", getPartyAccountingPreferencesMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                partyAcctgPreference = serviceResult.get("partyAccountingPreference");
            } catch (Exception e) {
                Debug.logError(e, "Error calling getPartyAccountingPreferences: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            createPaymentMap.put("paymentTypeId", "VENDOR_PAYMENT");
            createPaymentMap.put("partyIdFrom", context.get("organizationPartyId"));
            createPaymentMap.put("currencyUomId", ((Map<String, Object>) partyAcctgPreference).get("baseCurrencyUomId"));
            createPaymentMap.put("partyIdTo", context.get("partyId"));
            createPaymentMap.put("statusId", "PMNT_SENT");
            createPaymentMap.put("amount", paymentAmount);
            createPaymentMap.put("paymentMethodTypeId", context.get("paymentMethodTypeId"));
            createPaymentMap.put("paymentMethodId", context.get("paymentMethodId"));
            createPaymentMap.put("paymentRefNum", context.get("checkStartNumber"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPayment", createPaymentMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                paymentId = serviceResult.get("paymentId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPayment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (context.get("invoices") != null) {
                for (GenericValue invoice_iter : (List<GenericValue>) context.get("invoices")) {
                    invoice = invoice_iter;
                    if ("INVOICE_READY".equals(((Map<String, Object>) invoice).get("statusId"))) {
                        // set-service-fields from "invoice" to "getInvoicePaymentInfoListCtx" for service "getInvoicePaymentInfoList"
                        getInvoicePaymentInfoListCtx.putAll(UtilMisc.toMap(invoice));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("getInvoicePaymentInfoList", getInvoicePaymentInfoListCtx);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                            invoicePaymentInfoList = serviceResult.get("invoicePaymentInfoList");
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling getInvoicePaymentInfoList: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        invoicePaymentInfo = EntityUtil.getFirst((List<GenericValue>) invoicePaymentInfoList);
                        if (((Comparable) ((Map<String, Object>) invoicePaymentInfo).get("outstandingAmount")).compareTo("0") > 0) {
                            createPaymentApplicationMap.put("paymentId", paymentId);
                            createPaymentApplicationMap.put("amountApplied", ((Map<String, Object>) invoicePaymentInfo).get("outstandingAmount"));
                            createPaymentApplicationMap.put("invoiceId", ((Map<String, Object>) invoice).get("invoiceId"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createPaymentApplication", createPaymentApplicationMap);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createPaymentApplication: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                        }
                    }
                    invoiceIds.add(((Map<String, Object>) invoice).get("invoiceId"));
                    createPaymentApplicationMap = null;
                }
            }
        }
        result.put("invoiceIds", invoiceIds);
        BigDecimal amount = BigDecimal.valueOf(((Number) paymentAmount).doubleValue());
        result.put("amount", amount);

        return "success";
    }


    /**
     * Creates a record for FinAccountTrans on creation of payment.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createFinAccoutnTransFromPayment(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createFinAccountTransMap = new HashMap<>();
        // set-service-fields from "parameters" to "createFinAccountTransMap" for service "createFinAccountTrans"
        createFinAccountTransMap.putAll(UtilMisc.toMap(context));
        createFinAccountTransMap.put("finAccountTransTypeId", "WITHDRAWAL");
        createFinAccountTransMap.put("partyId", context.get("organizationPartyId"));
        Timestamp createFinAccountTransMap_transactionDate = new Timestamp(System.currentTimeMillis());
        Timestamp createFinAccountTransMap_entryDate = new Timestamp(System.currentTimeMillis());
        createFinAccountTransMap.put("comments", "Pay to " + context.get("partyId") + " for invoice Ids - " + context.get("invoiceIds"));
        Object finAccountTransId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createFinAccountTrans", createFinAccountTransMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            finAccountTransId = serviceResult.get("finAccountTransId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createFinAccountTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> updatePaymentMap = new HashMap<>();
        updatePaymentMap.put("finAccountTransId", finAccountTransId);
        updatePaymentMap.put("paymentId", context.get("paymentId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePayment", updatePaymentMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePayment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * creates PaymentGroup and PaymentGroupMembers
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPaymentGroupAndMember(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createPaymentGroupMap = null;
        Map<String, Object> createPaymentGroupMemberMap = null;
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            Timestamp parameters_fromDate = new Timestamp(System.currentTimeMillis());
        }
        // set-service-fields from "parameters" to "createPaymentGroupMap" for service "createPaymentGroup"
        createPaymentGroupMap.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isEmpty(context.get("paymentGroupName"))) {
            createPaymentGroupMap.put("paymentGroupName", "Payment Group Name");
        }
        Object paymentGroupId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPaymentGroup", createPaymentGroupMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            paymentGroupId = serviceResult.get("paymentGroupId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPaymentGroup: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        createPaymentGroupMemberMap.put("paymentGroupId", paymentGroupId);
        createPaymentGroupMemberMap.put("fromDate", context.get("fromDate"));
        if (context.get("paymentIds") != null) {
            for (GenericValue paymentId : (List<GenericValue>) context.get("paymentIds")) {
                createPaymentGroupMemberMap.put("paymentId", paymentId);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPaymentGroupMember", createPaymentGroupMemberMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPaymentGroupMember: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Cancel all payments for payment group
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String cancelCheckRunPayments(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue paymentGroupMemberAndTrans = null;
        GenericValue payment = null;
        Map<String, Object> voidPaymentMap = null;
        Map<String, Object> expirePaymentGroupMemberMap = null;
        List<GenericValue> paymentGroupMemberAndTransList = null;
        try {
            paymentGroupMemberAndTransList = EntityQuery.use(delegator)
                    .from("PmtGrpMembrPaymentAndFinAcctTrans")
                    .where(UtilMisc.toMap("paymentGroupId", context.get("paymentGroupId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PmtGrpMembrPaymentAndFinAcctTrans: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        paymentGroupMemberAndTrans = EntityUtil.getFirst((List<GenericValue>) paymentGroupMemberAndTransList);
        if (!"FINACT_TRNS_APPROVED".equals(((Map<String, Object>) paymentGroupMemberAndTrans).get("finAccountTransStatusId"))) {
            if (paymentGroupMemberAndTransList != null) {
                for (GenericValue paymentGroupMemberAndTrans_iter : paymentGroupMemberAndTransList) {
                    paymentGroupMemberAndTrans = paymentGroupMemberAndTrans_iter;
                    try {
                        payment = EntityQuery.use(delegator)
                                .from("Payment")
                                .where(UtilMisc.toMap("paymentId", ((Map<String, Object>) paymentGroupMemberAndTrans).get("paymentId")))
                                .queryOne();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    // set-service-fields from "payment" to "voidPaymentMap" for service "voidPayment"
                    voidPaymentMap.putAll(UtilMisc.toMap(payment));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("voidPayment", voidPaymentMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling voidPayment: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    // set-service-fields from "paymentGroupMemberAndTrans" to "expirePaymentGroupMemberMap" for service "expirePaymentGroupMember"
                    expirePaymentGroupMemberMap.putAll(UtilMisc.toMap(paymentGroupMemberAndTrans));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("expirePaymentGroupMember", expirePaymentGroupMemberMap);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling expirePaymentGroupMember: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    voidPaymentMap = null;
                    expirePaymentGroupMemberMap = null;
                }
            }
        } else {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingCheckIsAlreadyIssued", locale);
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
     * Get list of payment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPayments(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object paymentIds = null;
        List<GenericValue> payments = null;
        List<GenericValue> paymentGroupMembers = null;
        Object paymentGroupId = context.get("paymentGroupId");
        if (UtilValidate.isNotEmpty(paymentGroupId)) {
            try {
                paymentGroupMembers = EntityQuery.use(delegator)
                        .from("PaymentGroupMember")
                        .where(UtilMisc.toMap("paymentGroupId", paymentGroupId))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PaymentGroupMember: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            paymentIds = GroovyUtil.eval("org.ofbiz.entity.util.EntityUtil.getFieldListFromEntityList(paymentGroupMembers, 'paymentId', true);", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
            try {
                payments = EntityQuery.use(delegator)
                        .from("Payment")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        Object finAccountTransId = context.get("finAccountTransId");
        if (UtilValidate.isNotEmpty(finAccountTransId)) {
            try {
                payments = EntityQuery.use(delegator)
                        .from("Payment")
                        .where(UtilMisc.toMap("finAccountTransId", finAccountTransId))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("payments", payments);

        return "success";
    }


    /**
     * Get ReconciliationId associated to paymentGroup
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPaymentGroupReconciliationId(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue paymentGroupMember = null;
        Object glReconciliationId = null;
        GenericValue payment = null;
        GenericValue finAccountTrans = null;
        Object paymentGroupId = context.get("paymentGroupId");
        List<GenericValue> paymentGroupMembers = null;
        try {
            paymentGroupMembers = EntityQuery.use(delegator)
                    .from("PaymentGroupMember")
                    .where(UtilMisc.toMap("paymentGroupId", paymentGroupId))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGroupMember: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(paymentGroupMembers)) {
            paymentGroupMember = EntityUtil.getFirst((List<GenericValue>) paymentGroupMembers);
            try {
                payment = paymentGroupMember.getRelatedOne("Payment", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            try {
                finAccountTrans = payment.getRelatedOne("FinAccountTrans", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one FinAccountTrans: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(finAccountTrans)) {
                if (UtilValidate.isNotEmpty(((Map<String, Object>) finAccountTrans).get("glReconciliationId"))) {
                    glReconciliationId = ((Map<String, Object>) finAccountTrans).get("glReconciliationId");
                }
            }
        }
        result.put("glReconciliationId", glReconciliationId);

        return "success";
    }


    /**
     * Check the valid(unbatched) payment and create batch for same
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkAndCreateBatchForValidPayments(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        List<Object> disbursementPaymentIds = null;
        Boolean isReceipt = null;
        Object batchPaymentIds = null;
        Map<String, Object> createPaymentGroupAndMemberMap = null;
        Object paymentIds = context.get("paymentIds");
        List<GenericValue> payments = null;
        try {
            payments = EntityQuery.use(delegator)
                    .from("Payment")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (payments != null) {
            for (GenericValue payment : payments) {
                isReceipt = (Boolean) GroovyUtil.eval("org.ofbiz.accounting.util.UtilAccounting.isReceipt(payment)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                if (Boolean.FALSE.equals(isReceipt)) {
                    disbursementPaymentIds.add(((Map<String, Object>) payment).get("paymentId"));
                }
            }
        }
        if (UtilValidate.isNotEmpty(disbursementPaymentIds)) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingCannotIncludeApPaymentError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        List<GenericValue> paymentGroupMembers = null;
        try {
            paymentGroupMembers = EntityQuery.use(delegator)
                    .from("PaymentGroupMember")
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentGroupMember: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(paymentGroupMembers)) {
            batchPaymentIds = GroovyUtil.eval("org.ofbiz.entity.util.EntityUtil.getFieldListFromEntityList(paymentGroupMembers, 'paymentId', true);", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentsAreAlreadyBatchedError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        } else {
            // set-service-fields from "parameters" to "createPaymentGroupAndMemberMap" for service "createPaymentGroupAndMember"
            createPaymentGroupAndMemberMap.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPaymentGroupAndMember", createPaymentGroupAndMemberMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPaymentGroupAndMember: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Service set status of Payments in bulk.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String massChangePaymentStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> setPaymentStatusMap = null;
        if (context.get("paymentIds") != null) {
            for (GenericValue paymentId : (List<GenericValue>) context.get("paymentIds")) {
                setPaymentStatusMap.put("paymentId", paymentId);
                setPaymentStatusMap.put("statusId", context.get("statusId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setPaymentStatus", setPaymentStatusMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling setPaymentStatus: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                setPaymentStatusMap = null;
            }
        }

        return "success";
    }


    /**
     * Service auto create Payment from Order when payment does exist yet and not disabled by accounting config
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPaymentFromOrder(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        String purchaseAutoCreate = null;
        String salesAutoCreate = null;
        List<GenericValue> agreementList = null;
        Object organizationPartyId = null;
        List<GenericValue> orderTermList = null;
        GenericValue orderTerm = null;
        Timestamp start = null;
        Integer days = null;
        Map<String, Object> convertUomInMap = null;
        List<GenericValue> invoices = null;
        GenericValue invoice = null;
        GenericValue orderHeader = null;
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("PURCHASE_ORDER".equals(((Map<String, Object>) orderHeader).get("orderTypeId"))) {
            purchaseAutoCreate = UtilProperties.getMessage("AccountingConfig", "accounting.payment.purchaseorder.autocreate", locale);
            if (!"Y".equals(purchaseAutoCreate)) {
                Debug.logInfo("payment not created from approved order because config (accounting.payment.purchaseorder.autocreate) is not set to Y (AccountingConfig.properties)", MODULE);
                return "success";
            }
        }
        if ("SALES_ORDER".equals(((Map<String, Object>) orderHeader).get("orderTypeId"))) {
            salesAutoCreate = UtilProperties.getMessage("AccountingConfig", "accounting.payment.salesorder.autocreate", locale);
            if (!"Y".equals(salesAutoCreate)) {
                Debug.logInfo("payment not created from approved order because config (accounting.payment.salesorder.autocreate) is not set to Y (AccountingConfig.properties)", MODULE);
                return "success";
            }
        }
        List<GenericValue> orderPaymentPrefAndPayments = null;
        try {
            orderPaymentPrefAndPayments = EntityQuery.use(delegator)
                    .from("OrderPaymentPrefAndPayment")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderPaymentPrefAndPayment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(orderPaymentPrefAndPayments)) {
            Debug.logInfo("Payment not created for order " + ((Map<String, Object>) orderHeader).get("orderId") + ", at least a single payment already exists", MODULE);
            return "success";
        }
        List<GenericValue> orderRoleToList = null;
        try {
            orderRoleToList = EntityQuery.use(delegator)
                    .from("OrderRole")
                    .where(UtilMisc.toMap("orderId", ((Map<String, Object>) orderHeader).get("orderId"), "roleTypeId", "BILL_FROM_VENDOR"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue orderRoleTo = EntityUtil.getFirst((List<GenericValue>) orderRoleToList);
        List<GenericValue> orderRoleFromList = null;
        try {
            orderRoleFromList = EntityQuery.use(delegator)
                    .from("OrderRole")
                    .where(UtilMisc.toMap("orderId", ((Map<String, Object>) orderHeader).get("orderId"), "roleTypeId", "BILL_TO_CUSTOMER"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue orderRoleFrom = EntityUtil.getFirst((List<GenericValue>) orderRoleFromList);
        if ("PURCHASE_ORDER".equals(((Map<String, Object>) orderHeader).get("orderTypeId"))) {
            try {
                agreementList = EntityQuery.use(delegator)
                        .from("Agreement")
                        .where(UtilMisc.toMap("partyIdFrom", ((Map<String, Object>) orderRoleFrom).get("partyId"), "partyIdTo", ((Map<String, Object>) orderRoleTo).get("partyId"), "agreementTypeId", "PURCHASE_AGREEMENT"))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Agreement: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            context.put("paymentTypeId", "VENDOR_PAYMENT");
            organizationPartyId = ((Map<String, Object>) orderRoleFrom).get("partyId");
        } else {
            try {
                agreementList = EntityQuery.use(delegator)
                        .from("Agreement")
                        .where(UtilMisc.toMap("partyIdFrom", ((Map<String, Object>) orderRoleFrom).get("partyId"), "partyIdTo", ((Map<String, Object>) orderRoleTo).get("partyId"), "agreementTypeId", "SALES_AGREEMENT"))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Agreement: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            context.put("paymentTypeId", "CUSTOMER_PAYMENT");
            organizationPartyId = ((Map<String, Object>) orderRoleTo).get("partyId");
        }
        GenericValue agreement = EntityUtil.getFirst((List<GenericValue>) agreementList);
        if (UtilValidate.isNotEmpty(agreement)) {
            try {
                orderTermList = EntityQuery.use(delegator)
                        .from("OrderTerm")
                        .where(UtilMisc.toMap("orderId", ((Map<String, Object>) orderHeader).get("orderId"), "termTypeId", "FIN_PAYMENT_TERM"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderTerm: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            orderTerm = EntityUtil.getFirst((List<GenericValue>) orderTermList);
            if (UtilValidate.isNotEmpty(((Map<String, Object>) orderTerm).get("termDays"))) {
                days = (Integer) ((Map<String, Object>) orderTerm).get("termDays");
                start = new Timestamp(System.currentTimeMillis());
                try {
                    ((Map<String, Object>) context).put("effectiveDate", UtilDateTime.addDaysToTimestamp((Timestamp) start, days));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling UtilDateTime.addDaysToTimestamp: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isEmpty(context.get("effectiveDate"))) {
            Timestamp parameters_effectiveDate = new Timestamp(System.currentTimeMillis());
        }
        GenericValue permUserLogin = null;
        try {
            permUserLogin = EntityQuery.use(delegator)
                    .from("UserLogin")
                    .where(UtilMisc.toMap("userLoginId", "system"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> partyAccountingPreferencesMap = new HashMap<>();
        partyAccountingPreferencesMap.put("userLogin", permUserLogin);
        partyAccountingPreferencesMap.put("organizationPartyId", organizationPartyId);
        Object partyAcctgPreference = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getPartyAccountingPreferences", partyAccountingPreferencesMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            partyAcctgPreference = serviceResult.get("partyAccountingPreference");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getPartyAccountingPreferences: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object parameters_currencyUomId = null;
        Object parameters_amount = null;
        Object convertUomInMap_asOfDate = null;
        Object convertUomInMap_originalValue = null;
        Object convertUomInMap_uomId = null;
        Object convertUomInMap_uomIdTo = null;
        Object parameters_actualCurrencyAmount = null;
        Object parameters_actualCurrencyUomId = null;
        if ((!(UtilValidate.isEmpty(((Map<String, Object>) partyAcctgPreference).get("baseCurrencyUomId"))) && java.util.Objects.equals(((Map<String, Object>) orderHeader).get("currencyUom"), ((Map<String, Object>) partyAcctgPreference).get("baseCurrencyUomId")))) {
            context.put("currencyUomId", ((Map<String, Object>) orderHeader).get("currencyUom"));
            context.put("amount", ((Map<String, Object>) orderHeader).get("grandTotal"));
            try {
                invoices = EntityQuery.use(delegator)
                        .from("OrderItemBillingAndInvoiceAndItem")
                        .where(UtilMisc.toMap("orderId", context.get("orderId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderItemBillingAndInvoiceAndItem: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(invoices)) {
                try {
                    invoice = EntityQuery.use(delegator)
                            .from("Invoice")
                            .where(UtilMisc.toMap("invoiceId", ((GenericValue) ((List<?>) invoices).get(0)).get("invoiceId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                convertUomInMap.put("asOfDate", ((Map<String, Object>) invoice).get("invoiceDate"));
            }
            convertUomInMap.put("originalValue", ((Map<String, Object>) orderHeader).get("grandTotal"));
            convertUomInMap.put("uomId", ((Map<String, Object>) orderHeader).get("currencyUom"));
            convertUomInMap.put("uomIdTo", ((Map<String, Object>) partyAcctgPreference).get("baseCurrencyUomId"));
            Debug.logInfo("convertUomInMap = " + convertUomInMap, MODULE);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("convertUom", convertUomInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("amount", serviceResult.get("convertedValue"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling convertUom: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            context.put("actualCurrencyAmount", ((Map<String, Object>) orderHeader).get("grandTotal"));
            context.put("actualCurrencyUomId", ((Map<String, Object>) orderHeader).get("currencyUom"));
            context.put("currencyUomId", ((Map<String, Object>) partyAcctgPreference).get("baseCurrencyUomId"));
        } else {
            context.put("currencyUomId", ((Map<String, Object>) orderHeader).get("currencyUom"));
            context.put("amount", ((Map<String, Object>) orderHeader).get("grandTotal"));
        }
        context.put("partyIdFrom", ((Map<String, Object>) orderRoleFrom).get("partyId"));
        context.put("partyIdTo", ((Map<String, Object>) orderRoleTo).get("partyId"));
        context.put("paymentMethodTypeId", "COMPANY_ACCOUNT");
        context.put("statusId", "PMNT_NOT_PAID");
        Map<String, Object> createPayment = new HashMap<>();
        // set-service-fields from "parameters" to "createPayment" for service "createPayment"
        createPayment.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPayment", createPayment);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("paymentId", serviceResult.get("paymentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPayment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        context.put("orderId", ((Map<String, Object>) orderHeader).get("orderId"));
        context.put("maxAmount", ((Map<String, Object>) orderHeader).get("grandTotal"));
        Map<String, Object> newOrderPaymentPreference = new HashMap<>();
        // set-service-fields from "parameters" to "newOrderPaymentPreference" for service "createOrderPaymentPreference"
        newOrderPaymentPreference.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createOrderPaymentPreference", newOrderPaymentPreference);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            context.put("paymentPreferenceId", serviceResult.get("orderPaymentPreferenceId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createOrderPaymentPreference: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> updatePayment = new HashMap<>();
        // set-service-fields from "parameters" to "updatePayment" for service "updatePayment"
        updatePayment.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updatePayment", updatePayment);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updatePayment: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("paymentId", context.get("paymentId"));
        Debug.logInfo("payment " + context.get("paymentId") + " with the not-paid status automatically created from order: " + context.get("orderId") + " (can be disabled in AccountingConfig.properties)", MODULE);

        return "success";
    }


    /**
     * Create a payment application if either the invoice of payment could be found
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createMatchingPaymentApplication(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createAppl = null;
        Object isForeign = null;
        List<GenericValue> payments = null;
        List<GenericValue> paymentAppls = null;
        Map<String, Object> checkInvoice = null;
        Object invoiceTotal = null;
        List<GenericValue> invoices = null;
        Object amountApplied = null;
        GenericValue payment = null;
        Object invoiceId = null;
        Boolean isPurchaseInvoice = null;
        Boolean isSalesInvoice = null;
        String autoCreate = UtilProperties.getMessage("AccountingConfig", "accounting.payment.application.autocreate", locale);
        if (!"Y".equals(autoCreate)) {
            Debug.logInfo("payment application not automatically created because config is not set to Y", MODULE);
            return "success";
        }
        if (UtilValidate.isNotEmpty(context.get("invoiceId"))) {
            GenericValue invoice = null;
            try {
                invoice = EntityQuery.use(delegator)
                        .from("Invoice")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(invoice)) {
                try {
                    invoiceTotal = InvoiceWorker.getInvoiceTotal((GenericValue) invoice);
                } catch (Exception e) {
                    Debug.logError(e, "Error calling InvoiceWorker.getInvoiceTotal: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                checkInvoice.put("invoiceId", null);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("isInvoiceInForeignCurrency", checkInvoice);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    isForeign = serviceResult.get("isForeign");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling isInvoiceInForeignCurrency: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if ("true".equals(isForeign)) {
                    try {
                        payments = EntityQuery.use(delegator)
                                .from("Payment")
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                } else {
                    try {
                        payments = EntityQuery.use(delegator)
                                .from("Payment")
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
                if (UtilValidate.isNotEmpty(payments)) {
                    try {
                        paymentAppls = EntityQuery.use(delegator)
                                .from("PaymentApplication")
                                .where(UtilMisc.toMap("paymentId", ((GenericValue) ((List<?>) payments).get(0)).get("paymentId")))
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isEmpty(paymentAppls)) {
                        createAppl.put("paymentId", ((GenericValue) ((List<?>) payments).get(0)).get("paymentId"));
                        createAppl.put("invoiceId", context.get("invoiceId"));
                        if ("true".equals(isForeign)) {
                            createAppl.put("amountApplied", ((GenericValue) ((List<?>) payments).get(0)).get("actualCurrencyAmount"));
                        } else {
                            createAppl.put("amountApplied", ((GenericValue) ((List<?>) payments).get(0)).get("amount"));
                        }
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("paymentId"))) {
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
            if (UtilValidate.isNotEmpty(payment)) {
                try {
                    invoices = EntityQuery.use(delegator)
                            .from("Invoice")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (invoices != null) {
                    for (GenericValue invoiceEntry : invoices) {
                        isPurchaseInvoice = (Boolean) GroovyUtil.eval("org.ofbiz.entity.util.EntityTypeUtil.hasParentType(delegator, 'InvoiceType', 'invoiceTypeId', invoice.getString('invoiceTypeId'), 'parentTypeId', 'PURCHASE_INVOICE')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                        isSalesInvoice = (Boolean) GroovyUtil.eval("org.ofbiz.entity.util.EntityTypeUtil.hasParentType(delegator, 'InvoiceType', 'invoiceTypeId', invoice.getString('invoiceTypeId'), 'parentTypeId', 'SALES_INVOICE')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                        Object checkInvoice_invoiceId = null;
                        if ((Boolean.TRUE.equals(isPurchaseInvoice) || Boolean.TRUE.equals(isSalesInvoice))) {
                            try {
                                invoiceTotal = InvoiceWorker.getInvoiceTotal((GenericValue) invoiceEntry);
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling InvoiceWorker.getInvoiceTotal: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            checkInvoice.put("invoiceId", null);
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("isInvoiceInForeignCurrency", checkInvoice);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                                isForeign = serviceResult.get("isForeign");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling isInvoiceInForeignCurrency: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            if ("true".equals(isForeign)) {
                                if (java.util.Objects.equals(invoiceTotal, ((Map<String, Object>) payment).get("actualCurrencyAmount"))) {
                                    if (java.util.Objects.equals(((Map<String, Object>) invoiceEntry).get("currencyUomId"), ((Map<String, Object>) payment).get("actualCurrencyUomId"))) {
                                        invoiceId = ((Map<String, Object>) invoiceEntry).get("invoiceId");
                                        amountApplied = ((Map<String, Object>) payment).get("actualCurrencyAmount");
                                    }
                                }
                            } else {
                                if (java.util.Objects.equals(invoiceTotal, ((Map<String, Object>) payment).get("amount"))) {
                                    if (java.util.Objects.equals(((Map<String, Object>) invoiceEntry).get("currencyUomId"), ((Map<String, Object>) payment).get("currencyUomId"))) {
                                        invoiceId = ((Map<String, Object>) invoiceEntry).get("invoiceId");
                                        amountApplied = ((Map<String, Object>) payment).get("amount");
                                    }
                                }
                            }
                        }
                    }
                }
                if (UtilValidate.isNotEmpty(invoiceId)) {
                    try {
                        paymentAppls = EntityQuery.use(delegator)
                                .from("PaymentApplication")
                                .where(UtilMisc.toMap("invoiceId", invoiceId))
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isEmpty(paymentAppls)) {
                        createAppl.put("paymentId", context.get("paymentId"));
                        createAppl.put("invoiceId", invoiceId);
                        createAppl.put("amountApplied", amountApplied);
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) createAppl).get("paymentId"))) {
            if (UtilValidate.isNotEmpty(((Map<String, Object>) createAppl).get("invoiceId"))) {
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPaymentApplication", createAppl);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPaymentApplication: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                Debug.logInfo("payment application automatically created between invoiceId: " + ((Map<String, Object>) createAppl).get("invoiceId") + " and paymentId: " + ((Map<String, Object>) createAppl).get("paymentId") + " for the amount: " + ((Map<String, Object>) createAppl).get("appliedAmount") + " (can be disabled in AccountingConfig.properties)", MODULE);
            }
        }

        return "success";
    }


    /**
     * Create Content For Payment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPaymentContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("PaymentContent");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fromDate"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            newEntity.put("fromDate", nowTimestamp);
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> updateContent = new HashMap<>();
        // set-service-fields from "parameters" to "updateContent" for service "updateContent"
        updateContent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateContent", updateContent);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contentId", ((Map<String, Object>) newEntity).get("contentId"));
        result.put("paymentId", ((Map<String, Object>) newEntity).get("paymentId"));
        result.put("paymentContentTypeId", ((Map<String, Object>) newEntity).get("paymentContentTypeId"));

        return "success";
    }


    /**
     * Update Content For Payment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePaymentContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("PaymentContent");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
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
        Map<String, Object> updateContent = new HashMap<>();
        // set-service-fields from "parameters" to "updateContent" for service "updateContent"
        updateContent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateContent", updateContent);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Remove Content From Payment
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removePaymentContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("PaymentContent");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key : " + e.getMessage(), MODULE);
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

}

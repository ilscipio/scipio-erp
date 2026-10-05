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
import java.math.RoundingMode;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.ofbiz.accounting.invoice.InvoiceWorker;
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
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/invoice/InvoiceServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class InvoiceServices {

    private static final String MODULE = InvoiceServices.class.getName();


    /**
     * Get Next invoiceId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getNextInvoiceId(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue customMethod = null;
        Object customMethodName = null;
        String invoiceIdTemp = null;
        Map<String, Object> customMethodMap = null;
        GenericValue partyAcctgPreference = null;
        try {
            partyAcctgPreference = EntityQuery.use(delegator)
                    .from("PartyAcctgPreference")
                    .where(UtilMisc.toMap("partyId", context.get("partyId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyAcctgPreference: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logInfo("In getNextInvoiceId partyId is [" + context.get("partyId") + "], partyAcctgPreference: " + partyAcctgPreference, MODULE);
        if (UtilValidate.isNotEmpty(partyAcctgPreference)) {
            try {
                customMethod = partyAcctgPreference.getRelatedOne("InvoiceCustomMethod", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one InvoiceCustomMethod: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            Debug.logWarning("Acctg preference not defined for partyId [" + context.get("partyId") + "]", MODULE);
        }
        if (UtilValidate.isNotEmpty(customMethod)) {
            customMethodName = ((Map<String, Object>) customMethod).get("customMethodName");
        } else {
            if ("INVSQ_ENF_SEQ".equals(((Map<String, Object>) partyAcctgPreference).get("oldInvoiceSequenceEnumId"))) {
                customMethodName = "invoiceSequenceEnforced";
            }
            if ("INVSQ_RESTARTYR".equals(((Map<String, Object>) partyAcctgPreference).get("oldInvoiceSequenceEnumId"))) {
                customMethodName = "invoiceSequenceRestart";
            }
        }
        if (UtilValidate.isNotEmpty(customMethodName)) {
            // set-service-fields from "parameters" to "customMethodMap" for service "${customMethodName}"
            customMethodMap.putAll(UtilMisc.toMap(context));
            customMethodMap.put("partyAcctgPreference", partyAcctgPreference);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("${customMethodName}", customMethodMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                invoiceIdTemp = (String) serviceResult.get("invoiceId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling ${customMethodName}: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            Debug.logInfo("In createInvoice sequence enum Standard", MODULE);
            invoiceIdTemp = (String) context.get("invoiceId");
            if (UtilValidate.isEmpty(invoiceIdTemp)) {
                invoiceIdTemp = delegator.getNextSeqId("Invoice");
            } else {
                // TODO: Convert <check-id> element
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        Object invoiceId = ((Map<String, Object>) partyAcctgPreference).get("invoiceIdPrefix") + String.valueOf(invoiceIdTemp);
        result.put("invoiceId", invoiceId);

        return "success";
    }


    /**
     * Enforced Sequence (no gaps, per organization)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String invoiceSequenceEnforced(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Debug.logInfo("In createInvoice sequence enum Enforced", MODULE);
        GenericValue partyAcctgPreference = (GenericValue) context.get("partyAcctgPreference");
        if (UtilValidate.isNotEmpty(((Map<String, Object>) partyAcctgPreference).get("lastInvoiceNumber"))) {
            partyAcctgPreference.set("lastInvoiceNumber", new BigDecimal(((Map<String, Object>) partyAcctgPreference).get("lastInvoiceNumber").toString()));
        } else {
            partyAcctgPreference.set("lastInvoiceNumber", 1);
        }
        try {
            delegator.store(partyAcctgPreference);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object invoiceId = ((Map<String, Object>) partyAcctgPreference).get("lastInvoiceNumber");
        result.put("invoiceId", invoiceId);

        return "success";
    }


    /**
     * Restart on Fiscal Year (no gaps, per org, reset to 1 each year)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String invoiceSequenceRestart(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object curYearFiscalStartDate = null;
        Object zeroLong = null;
        GenericValue partyAcctgPreference = null;
        Debug.logInfo("In createInvoice sequence enum Restart", MODULE);
        partyAcctgPreference = (GenericValue) context.get("partyAcctgPreference");
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        if (UtilValidate.isEmpty(((Map<String, Object>) partyAcctgPreference).get("lastInvoiceRestartDate"))) {
            partyAcctgPreference.set("lastInvoiceNumber", 1);
            partyAcctgPreference.put("lastInvoiceRestartDate", nowTimestamp);
        } else {
            zeroLong = 0;
            try {
                curYearFiscalStartDate = UtilDateTime.getYearStart((Timestamp) nowTimestamp, (Number) ((Map<String, Object>) partyAcctgPreference).get("fiscalYearStartDay"), (Number) ((Map<String, Object>) partyAcctgPreference).get("fiscalYearStartMonth"), (Number) zeroLong);
            } catch (Exception e) {
                Debug.logError(e, "Error calling UtilDateTime.getYearStart: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Object partyAcctgPreference_lastInvoiceRestartDate = null;
            if ((((Map<String, Object>) partyAcctgPreference).get("lastInvoiceRestartDate") != null /* TODO: field compare operator less */ && nowTimestamp != null /* TODO: field compare operator greater-equals */)) {
                partyAcctgPreference.set("lastInvoiceNumber", 1);
                partyAcctgPreference.put("lastInvoiceRestartDate", nowTimestamp);
            } else {
                partyAcctgPreference.set("lastInvoiceNumber", new BigDecimal(((Map<String, Object>) partyAcctgPreference).get("lastInvoiceNumber").toString()));
            }
        }
        try {
            delegator.store(partyAcctgPreference);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object curYearString = String.valueOf(UtilDateTime.getYear((java.sql.Timestamp) ((Map<String, Object>) partyAcctgPreference).get("lastInvoiceRestartDate"), java.util.TimeZone.getDefault(), locale));
        Object invoiceId = curYearString + "-" + String.valueOf(((Map<String, Object>) partyAcctgPreference).get("lastInvoiceNumber"));
        result.put("invoiceId", invoiceId);

        return "success";
    }


    /**
     * Create a new Invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object partyId = null;
        Map<String, Object> getNextInvoiceIdMap = null;
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        GenericValue party = null;
        try {
            party = EntityQuery.use(delegator)
                    .from("Party")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Party: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(party)) {
            partyId = context.get("partyId");
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyPartyNotFoundPartyId", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        GenericValue fromParty = null;
        try {
            fromParty = EntityQuery.use(delegator)
                    .from("Party")
                    .where(UtilMisc.toMap("partyId", context.get("partyIdFrom")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Party: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(fromParty)) {
            partyId = context.get("partyIdFrom");
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyPartyNotFoundPartyId", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        GenericValue newEntity = delegator.makeValue("Invoice");
        if (UtilValidate.isEmpty(context.get("invoiceId"))) {
            // set-service-fields from "parameters" to "getNextInvoiceIdMap" for service "getNextInvoiceId"
            getNextInvoiceIdMap.putAll(UtilMisc.toMap(context));
            getNextInvoiceIdMap.put("partyId", context.get("partyIdFrom"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("getNextInvoiceId", getNextInvoiceIdMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("invoiceId", serviceResult.get("invoiceId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling getNextInvoiceId: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("invoiceId", context.get("invoiceId"));
        if (UtilValidate.isEmpty(context.get("invoiceDate"))) {
            context.put("invoiceDate", nowTimestamp);
        }
        if (UtilValidate.isEmpty(context.get("statusId"))) {
            context.put("statusId", "INVOICE_IN_PROCESS");
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) party).get("preferredCurrencyUomId"))) {
            context.put("currencyUomId", ((Map<String, Object>) party).get("preferredCurrencyUomId"));
        }
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue newInvoiceStatus = delegator.makeValue("InvoiceStatus");
        newInvoiceStatus.put("invoiceId", ((Map<String, Object>) newEntity).get("invoiceId"));
        newInvoiceStatus.put("statusId", ((Map<String, Object>) newEntity).get("statusId"));
        newInvoiceStatus.put("statusDate", nowTimestamp);
        try {
            delegator.create(newInvoiceStatus);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Create a new Invoice from an existing invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String copyInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> invoice = null;
        Map<String, Object> createInvoiceItem = null;
        Map<String, Object> invoiceLookup = new HashMap<>();
        invoiceLookup.put("invoiceId", context.get("invoiceIdToCopyFrom"));
        Object invoiceItems = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getInvoice", invoiceLookup);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            invoice = (Map<String, Object>) serviceResult.get("invoice");
            invoiceItems = serviceResult.get("invoiceItems");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getInvoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        invoice.put("invoiceId", context.get("invoiceId"));
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        invoice.put("invoiceDate", nowTimestamp);
        invoice.put("statusId", "INVOICE_IN_PROCESS");
        if (UtilValidate.isNotEmpty(context.get("invoiceTypeId"))) {
            invoice.put("invoiceTypeId", context.get("invoiceTypeId"));
        }
        Map<String, Object> newInvoice = new HashMap<>();
        // set-service-fields from "invoice" to "newInvoice" for service "createInvoice"
        newInvoice.putAll(UtilMisc.toMap(invoice));
        newInvoice.remove("invoiceId");
        Object invoiceId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInvoice", newInvoice);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            invoiceId = serviceResult.get("invoiceId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInvoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("invoiceId", invoiceId);
        request.setAttribute("invoiceId", invoiceId);
        if (invoiceItems != null) {
            for (Object invoiceItem : (List<Object>) invoiceItems) {
                // set-service-fields from "invoiceItem" to "createInvoiceItem" for service "createInvoiceItem"
                createInvoiceItem.putAll(UtilMisc.toMap(invoiceItem));
                createInvoiceItem.put("invoiceId", invoiceId);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createInvoiceItem", createInvoiceItem);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createInvoiceItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Retrieve an invoice and the items
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue lookupPKMap = delegator.makeValue("Invoice");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        GenericValue invoiceValue = null;
        try {
            invoiceValue = EntityQuery.use(delegator)
                    .from("Invoice")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key Invoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("invoice", invoiceValue);
        List<GenericValue> invoiceItemValues = null;
        try {
            invoiceItemValues = invoiceValue.getRelated("InvoiceItem", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related InvoiceItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("invoiceItems", invoiceItemValues);

        return "success";
    }


    /**
     * Update the header of an existing Invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue savedValue = null;
        GenericValue lookedUpValue = null;
        Map<String, Object> inputMap = null;
        String result = InvoiceStatusInProgress(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookupPKMap = delegator.makeValue("Invoice");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("Invoice")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key Invoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("INVOICE_IN_PROCESS".equals(((Map<String, Object>) lookedUpValue).get("statusId"))) {
            savedValue = GenericValue.create((GenericValue) lookedUpValue);
            lookedUpValue.setNonPKFields((Map<String, Object>) context);
            lookedUpValue.put("statusId", ((Map<String, Object>) savedValue).get("statusId"));
            if (!java.util.Objects.equals(lookedUpValue, savedValue)) {
                try {
                    delegator.store(lookedUpValue);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        } else {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingInvoiceUpdateOnlyWithInProcessStatus", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            Debug.logError("Can only update Invoice, when status is in-process...current Status: " + ((Map<String, Object>) lookedUpValue).get("statusId"), MODULE);
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(context.get("statusId"))) {
            if (!java.util.Objects.equals(context.get("statusId"), ((Map<String, Object>) savedValue).get("statusId"))) {
                inputMap.put("invoiceId", context.get("invoiceId"));
                inputMap.put("statusId", context.get("statusId"));
                Timestamp inputMap_statusDate = new Timestamp(System.currentTimeMillis());
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setInvoiceStatus", inputMap);
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
        }

        return "success";
    }


    /**
     * Send an invoice per Email
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sendInvoicePerEmail(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> emailParams = new HashMap<>();
        // set-service-fields from "parameters" to "emailParams" for service "sendMailFromScreen"
        emailParams.putAll(UtilMisc.toMap(context));
        emailParams.put("xslfoAttachScreenLocation", "component://accounting/widget/AccountingPrintScreens.xml#InvoicePDF");
        emailParams.put("bodyParameters.invoiceId", context.get("invoiceId"));
        emailParams.put("bodyParameters.userLogin", context.get("userLogin"));
        emailParams.put("bodyParameters.other", context.get("other"));
        // TODO: Convert <call-service-asynch> element
        String successMessage = UtilProperties.getMessage("AccountingUiLabels", "AccountingEmailScheduledToSend", locale);

        return "success";
    }


    /**
     * Create a new Invoice Item
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createInvoiceItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue product = null;
        GenericValue newEntity = null;
        Map<String, Object> calculateProductPriceMap = null;
        Object invoiceId = context.get("invoiceId");
        String inlineResult = InvoiceStatusInProgress(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        newEntity = delegator.makeValue("InvoiceItem");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("invoiceItemSeqId"))) {
            delegator.setNextSubSeqId(newEntity, "invoiceItemSeqId", 5, 1);
            Object invoiceItemSeqId = newEntity.get("invoiceItemSeqId");
            result.put("invoiceItemSeqId", ((Map<String, Object>) newEntity).get("invoiceItemSeqId"));
        }
        if (UtilValidate.isEmpty(context.get("amount"))) {
            if (UtilValidate.isNotEmpty(context.get("productId"))) {
                try {
                    product = EntityQuery.use(delegator)
                            .from("Product")
                            .where(UtilMisc.toMap())
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                newEntity.put("description", ((Map<String, Object>) product).get("description"));
                if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("description"))) {
                    newEntity.put("description", ((Map<String, Object>) product).get("productName"));
                }
                calculateProductPriceMap.put("product", product);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("calculateProductPrice", calculateProductPriceMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    newEntity.put("amount", serviceResult.get("price"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling calculateProductPrice: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("productId"))) {
            if (UtilValidate.isEmpty(context.get("quantity"))) {
                newEntity.put("quantity", new BigDecimal("1.0"));
            }
        }
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("amount"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingInvoiceAmountIsMandatory", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
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
     * Update an existing Invoice Item
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateInvoiceItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        GenericValue lookedUpValue = null;
        GenericValue product = null;
        Map<String, Object> calculateProductPriceMap = null;
        String inlineResult = InvoiceStatusInProgress(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        GenericValue lookupPKMap = delegator.makeValue("InvoiceItem");
        lookupPKMap.setPKFields((Map<String, Object>) context);
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("InvoiceItem")
                    .where(lookupPKMap)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key InvoiceItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue savedValue = GenericValue.create((GenericValue) lookedUpValue);
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        if (UtilValidate.isNotEmpty(context.get("productId"))) {
            if (!java.util.Objects.equals(((Map<String, Object>) savedValue).get("productId"), ((Map<String, Object>) lookedUpValue).get("productId"))) {
                try {
                    product = EntityQuery.use(delegator)
                            .from("Product")
                            .where(UtilMisc.toMap())
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                lookedUpValue.put("description", ((Map<String, Object>) product).get("description"));
                calculateProductPriceMap.put("product", product);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("calculateProductPrice", calculateProductPriceMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    lookedUpValue.put("amount", serviceResult.get("price"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling calculateProductPrice: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        Object newEntity = null;
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("amount"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingInvoiceAmountIsMandatory", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (!java.util.Objects.equals(lookedUpValue, savedValue)) {
            try {
                delegator.store(lookedUpValue);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("invoiceItemSeqId", ((Map<String, Object>) lookedUpValue).get("invoiceItemSeqId"));
        result.put("invoiceId", ((Map<String, Object>) lookedUpValue).get("invoiceId"));

        return "success";
    }


    /**
     * Remove an existing Invoice Item
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeInvoiceItem(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = InvoiceStatusInProgress(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        Object invoiceId = context.get("invoiceId");
        result = InvoiceStatusInProgress(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        Map<String, Object> paymentApplicationMap = new HashMap<>();
        paymentApplicationMap.put("invoiceId", context.get("invoiceId"));
        paymentApplicationMap.put("invoiceItemSeqId", context.get("invoiceItemSeqId"));
        if (UtilValidate.isNotEmpty(context.get("invoiceItemSeqId"))) {
            // TODO: Convert <remove-by-and> element
        } else {
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("InvoiceItem")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InvoiceItem: " + e.getMessage(), MODULE);
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
     * Remove an existing payment application
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removePaymentApplication(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object currencyUomId = null;
        GenericValue payment = null;
        String toMessage = null;
        Map<String, Object> invoiceStatusMap = null;
        GenericValue invoice = null;
        Timestamp nowTimestamp = null;
        GenericValue toPayment = null;
        GenericValue billingAccount = null;
        GenericValue paymentApplication = null;
        try {
            paymentApplication = EntityQuery.use(delegator)
                    .from("PaymentApplication")
                    .where(UtilMisc.toMap("paymentApplicationId", "${parameters.paymentApplicationId}"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(paymentApplication)) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentApplicationNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        currencyUomId = null;
        if (UtilValidate.isNotEmpty(((Map<String, Object>) paymentApplication).get("paymentId"))) {
            try {
                payment = EntityQuery.use(delegator)
                        .from("Payment")
                        .where(UtilMisc.toMap("paymentId", "${paymentApplication.paymentId}"))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(payment)) {
                if ("PMNT_CONFIRMED".equals(((Map<String, Object>) payment).get("statusId"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentApplicationCannotRemovedWithConfirmedStatus", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            result.put("paymentId", ((Map<String, Object>) paymentApplication).get("paymentId"));
            currencyUomId = ((Map<String, Object>) context.get("paymentId")).get("currencyUomId");
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) paymentApplication).get("invoiceId"))) {
            try {
                invoice = EntityQuery.use(delegator)
                        .from("Invoice")
                        .where(UtilMisc.toMap("invoiceId", "${paymentApplication.invoiceId}"))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(invoice)) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingInvoiceNotFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                Debug.logInfo("Invoice not found, invoice Id: " + context.get("invoiceId"), MODULE);
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            currencyUomId = ((Map<String, Object>) invoice).get("currencyUomId");
            if ("INVOICE_PAID".equals(((Map<String, Object>) invoice).get("statusId"))) {
                invoiceStatusMap.put("invoiceId", ((Map<String, Object>) paymentApplication).get("invoiceId"));
                invoiceStatusMap.put("statusId", "INVOICE_READY");
                nowTimestamp = new Timestamp(System.currentTimeMillis());
                invoiceStatusMap.put("statusDate", nowTimestamp);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setInvoiceStatus", invoiceStatusMap);
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
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            result.put("invoiceId", ((Map<String, Object>) paymentApplication).get("invoiceId"));
            toMessage = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentApplToInvoice", locale);
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) paymentApplication).get("invoiceItemSeqId"))) {
            result.put("invoiceItemSeqId", ((Map<String, Object>) paymentApplication).get("invoiceItemSeqId"));
            toMessage = UtilProperties.getMessage("AccountingUiLabels", "AccountingApplicationToInvoiceItem", locale);
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) paymentApplication).get("toPaymentId"))) {
            try {
                toPayment = EntityQuery.use(delegator)
                        .from("Payment")
                        .where(UtilMisc.toMap("paymentId", "${paymentApplication.toPaymentId}"))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Payment: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(toPayment)) {
                if ("PMNT_CONFIRMED".equals(((Map<String, Object>) toPayment).get("statusId"))) {
                    {
                        String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentApplicationCannotRemovedWithConfirmedStatus", locale);
                        error_list.add(errorMsg);
                        request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                    }
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            toMessage = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentApplToPayment", locale);
            result.put("toPaymentId", ((Map<String, Object>) paymentApplication).get("toPaymentId"));
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) paymentApplication).get("billingAccountId"))) {
            result.put("billingAccountId", ((Map<String, Object>) paymentApplication).get("billingAccountId"));
            toMessage = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentApplToBillingAccount", locale);
            try {
                billingAccount = EntityQuery.use(delegator)
                        .from("BillingAccount")
                        .where(UtilMisc.toMap("billingAccountId", ((Map<String, Object>) paymentApplication).get("billingAccountId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying BillingAccount: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            currencyUomId = ((Map<String, Object>) billingAccount).get("accountCurrencyUomId");
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) paymentApplication).get("taxAuthGeoId"))) {
            result.put("taxAuthGeoId", ((Map<String, Object>) paymentApplication).get("taxAuthGeoId"));
            toMessage = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentApplToTaxAuth", locale);
        }
        String successMessage = UtilProperties.getMessage("AccountingUiLabels", "AccountingPaymentApplRemoved", locale);
        // TODO: Convert <string-append> element
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
     * Create a Invoice Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createInvoiceRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = InvoiceStatusInProgress(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue newEntity = delegator.makeValue("InvoiceRole");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("datetimePerformed"))) {
            Timestamp newEntity_datetimePerformed = new Timestamp(System.currentTimeMillis());
        }
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
     * Remove existing Invoice Role
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeInvoiceRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String result = InvoiceStatusInProgress(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("InvoiceRole")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InvoiceRole: " + e.getMessage(), MODULE);
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
     * Set The Invoice Status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setInvoiceStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> paymentApplications = null;
        Map<String, Object> newp = null;
        GenericValue statusChange = null;
        GenericValue newEntity = null;
        BigDecimal notApplied = null;
        GenericValue invoice = null;
        Map<String, Object> payAppl = null;
        Timestamp nowTimestamp = null;
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
        result.put("oldStatusId", ((Map<String, Object>) invoice).get("statusId"));
        result.put("invoiceTypeId", ((Map<String, Object>) invoice).get("invoiceTypeId"));
        if (!java.util.Objects.equals(((Map<String, Object>) invoice).get("statusId"), context.get("statusId"))) {
            try {
                statusChange = EntityQuery.use(delegator)
                        .from("StatusValidChange")
                        .where(UtilMisc.toMap("statusId", ((Map<String, Object>) invoice).get("statusId"), "statusIdTo", context.get("statusId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying StatusValidChange: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(statusChange)) {
                {
                    String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPSInvalidStatusChange", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                Debug.logError("Cannot change from " + ((Map<String, Object>) invoice).get("statusId") + " to " + context.get("statusId"), MODULE);
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            } else {
                if ("INVOICE_PAID".equals(context.get("statusId"))) {
                    notApplied = (BigDecimal) GroovyUtil.eval("org.ofbiz.accounting.invoice.InvoiceWorker.getInvoiceNotApplied(invoice)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                    if (!"0.00".equals(notApplied)) {
                        {
                            String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingInvoiceCannotChangeStatusToPaid", locale);
                            error_list.add(errorMsg);
                            request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                        }
                        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                            return "error";
                        }
                    }
                    if (UtilValidate.isEmpty(context.get("paidDate"))) {
                        nowTimestamp = new Timestamp(System.currentTimeMillis());
                        invoice.put("paidDate", nowTimestamp);
                    } else {
                        invoice.put("paidDate", context.get("paidDate"));
                    }
                }
                if (UtilValidate.isNotEmpty(((Map<String, Object>) invoice).get("paidDate"))) {
                    if ("INVOICE_READY".equals(context.get("statusId"))) {
                        invoice.remove("paidDate");
                    }
                }
                invoice.put("statusId", context.get("statusId"));
                try {
                    delegator.store(invoice);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                newEntity = delegator.makeValue("InvoiceStatus");
                newEntity.setNonPKFields((Map<String, Object>) context);
                newEntity.setPKFields((Map<String, Object>) context);
                if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("statusDate"))) {
                    Timestamp newEntity_statusDate = new Timestamp(System.currentTimeMillis());
                }
                try {
                    delegator.create(newEntity);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if ("PAYROL_INVOICE".equals(((Map<String, Object>) invoice).get("invoiceTypeId"))) {
                    Object newp_partyIdFrom = null;
                    Object newp_partyIdTo = null;
                    Object newp_paymentMethodTypeId = null;
                    Object newp_paymentTypeId = null;
                    Object newp_statusId = null;
                    Object newp_currencyUomId = null;
                    Object payment_paymentId = null;
                    Object payAppl_invoiceId = null;
                    Object payAppl_paymentId = null;
                    Object payAppl_amountApplied = null;
                    if (("INVOICE_APPROVED".equals(context.get("statusId")) || "INVOICE_READY".equals(context.get("statusId")))) {
                        try {
                            paymentApplications = EntityQuery.use(delegator)
                                    .from("PaymentApplication")
                                    .where(UtilMisc.toMap("invoiceId", context.get("invoiceId")))
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying PaymentApplication: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (UtilValidate.isEmpty(paymentApplications)) {
                            newp.put("partyIdFrom", ((Map<String, Object>) invoice).get("partyId"));
                            newp.put("partyIdTo", ((Map<String, Object>) invoice).get("partyIdFrom"));
                            newp.put("paymentMethodTypeId", "COMPANY_CHECK");
                            newp.put("paymentTypeId", "PAYROL_PAYMENT");
                            newp.put("statusId", "PMNT_NOT_PAID");
                            newp.put("currencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
                            try {
                                ((Map<String, Object>) newp).put("amount", InvoiceWorker.getInvoiceTotal((GenericValue) invoice));
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling InvoiceWorker.getInvoiceTotal: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            Map<String, Object> payment = new HashMap<>();
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createPayment", newp);
                                if (ServiceUtil.isError(serviceResult)) {
                                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                    return "error";
                                }
                                payment.put("paymentId", serviceResult.get("paymentId"));
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling createPayment: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            payAppl.put("invoiceId", ((Map<String, Object>) invoice).get("invoiceId"));
                            payAppl.put("paymentId", ((Map<String, Object>) payment).get("paymentId"));
                            payAppl.put("amountApplied", ((Map<String, Object>) newp).get("amount"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("createPaymentApplication", payAppl);
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
                }
            }
        }

        return "success";
    }


    /**
     * Create a Invoice Term
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createInvoiceTerm(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        String inlineResult = InvoiceStatusInProgress(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        GenericValue newEntity = delegator.makeValue("InvoiceTerm");
        newEntity.setNonPKFields((Map<String, Object>) context);
        ((GenericValue) newEntity).put("invoiceTermId", delegator.getNextSeqId("InvoiceTerm"));
        result.put("invoiceTermId", ((Map<String, Object>) newEntity).get("invoiceTermId"));
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
     * copy a invoice to a InvoiceType starting with 'template'
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String copyInvoiceToTemplate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        context.put("invoiceIdToCopyFrom", context.get("invoiceId"));
        if ("SALES_INVOICE".equals(context.get("invoiceTypeId"))) {
            context.put("invoiceTypeId", "SALES_INV_TEMPLATE");
        }
        if ("PURCHASE_INVOICE".equals(context.get("invoiceTypeId"))) {
            context.put("invoiceTypeId", "PUR_INV_TEMPLATE");
        }
        String result = copyInvoice(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * Check if the invoiceStatus is in progress
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String InvoiceStatusInProgress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue headerValue = null;
        try {
            headerValue = EntityQuery.use(delegator)
                    .from("Invoice")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(headerValue)) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingInvoiceNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            Debug.logInfo("Invoice not found, invoice Id: " + context.get("invoiceId"), MODULE);
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        if (!"INVOICE_IN_PROCESS".equals(((Map<String, Object>) headerValue).get("statusId"))) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingInvoiceUpdateOnlyWithInProcessStatus", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            Debug.logInfo("Can only update Invoice, when status is in-process...is now: " + ((Map<String, Object>) headerValue).get("statusId"), MODULE);
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }

        return "success";
    }


    /**
     * Create a ContactMech for an invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createInvoiceContactMech(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue invoiceContactMech = delegator.makeValue("InvoiceContactMech");
        invoiceContactMech.setPKFields((Map<String, Object>) context);
        try {
            delegator.create(invoiceContactMech);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("invoiceContactMech", context.get("contactMechId"));

        return "success";
    }


    /**
     * Updates a InvoiceItemType Record
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateInvoiceItemType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("InvoiceItemType")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InvoiceItemType: " + e.getMessage(), MODULE);
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
     * Scheduled service to generate Invoice from an existing Invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String autoGenerateInvoiceFromExistingInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> updateInvoiceCtx = null;
        Map<String, Object> copyInvoiceCtx = null;
        Object invoiceId = null;
        List<GenericValue> invoices = null;
        try {
            invoices = EntityQuery.use(delegator)
                    .from("Invoice")
                    .where(UtilMisc.toMap("recurrenceInfoId", context.get("recurrenceInfoId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (invoices != null) {
            for (GenericValue invoice : invoices) {
                // set-service-fields from "invoice" to "copyInvoiceCtx" for service "copyInvoice"
                copyInvoiceCtx.putAll(UtilMisc.toMap(invoice));
                copyInvoiceCtx.put("invoiceIdToCopyFrom", ((Map<String, Object>) invoice).get("invoiceId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("copyInvoice", copyInvoiceCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    invoiceId = serviceResult.get("invoiceId");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling copyInvoice: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                // set-service-fields from "invoice" to "updateInvoiceCtx" for service "updateInvoice"
                updateInvoiceCtx.putAll(UtilMisc.toMap(invoice));
                updateInvoiceCtx.put("invoiceId", invoiceId);
                if ("SALES_INV_TEMPLATE".equals(((Map<String, Object>) updateInvoiceCtx).get("invoiceTypeId"))) {
                    updateInvoiceCtx.put("invoiceTypeId", "SALES_INVOICE");
                }
                if ("PUR_INV_TEMPLATE".equals(((Map<String, Object>) updateInvoiceCtx).get("invoiceTypeId"))) {
                    updateInvoiceCtx.put("invoiceTypeId", "PURCHASE_INVOICE");
                }
                invoice = null;
                context.remove("invoiceIdToCopyFrom");
                updateInvoiceCtx.remove("recurrenceInfoId");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateInvoice", updateInvoiceCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateInvoice: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Service to cancel the Invoices
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String cancelInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> paymentStatusMap = null;
        GenericValue payment = null;
        Boolean isDisbursement = null;
        Boolean isReceipt = null;
        Map<String, Object> removePaymentApplicationCtx = null;
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
        if (UtilValidate.isEmpty(invoice)) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingInvoiceNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        List<GenericValue> paymentApplications = null;
        try {
            paymentApplications = invoice.getRelated("PaymentApplication", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related PaymentApplication: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (paymentApplications != null) {
            for (GenericValue paymentApplication : paymentApplications) {
                try {
                    payment = paymentApplication.getRelatedOne("Payment", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one Payment: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if ("PMNT_CONFIRMED".equals(((Map<String, Object>) payment).get("statusId"))) {
                    paymentStatusMap.put("paymentId", ((Map<String, Object>) payment).get("paymentId"));
                    isReceipt = (Boolean) GroovyUtil.eval("org.ofbiz.accounting.util.UtilAccounting.isReceipt(payment)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                    isDisbursement = (Boolean) GroovyUtil.eval("org.ofbiz.accounting.util.UtilAccounting.isDisbursement(payment)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
                    if (Boolean.TRUE.equals(isReceipt)) {
                        paymentStatusMap.put("statusId", "PMNT_RECEIVED");
                    } else {
                        if (Boolean.TRUE.equals(isDisbursement)) {
                            paymentStatusMap.put("statusId", "PMNT_SENT");
                        }
                    }
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("setPaymentStatus", paymentStatusMap);
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
        result.put("invoiceTypeId", ((Map<String, Object>) invoice).get("invoiceTypeId"));

        return "success";
    }


    /**
     * calculate running total for Invoices
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getInvoiceRunningTotal(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        BigDecimal runningTotal = null;
        Map<String, Object> getInvoicePaymentInfoListCtx = null;
        GenericValue invoicePaymentInfo = null;
        Object invoicePaymentInfoList = null;
        String currencyUomId = null;
        Object invoiceIds = context.get("invoiceIds");
        runningTotal = BigDecimal.ZERO;
        List<GenericValue> invoiceList = null;
        try {
            invoiceList = EntityQuery.use(delegator)
                    .from("Invoice")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (invoiceList != null) {
            for (GenericValue invoice : invoiceList) {
                getInvoicePaymentInfoListCtx.put("invoiceId", ((Map<String, Object>) invoice).get("invoiceId"));
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
                runningTotal = (BigDecimal) ((BigDecimal) runningTotal).add((BigDecimal) ((Map<String, Object>) invoicePaymentInfo).get("outstandingAmount"));
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
        Object invoiceRunningTotal = GroovyUtil.eval("org.ofbiz.base.util.UtilFormatOut.formatCurrency(runningTotal, currencyUomId, parameters.locale)", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        result.put("invoiceRunningTotal", invoiceRunningTotal);

        return "success";
    }


    /**
     * Filter invoices by invoiceItemAssocTypeId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getInvoicesFilterByAssocType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<GenericValue> invoiceItemAssocList = null;
        List<Object> filteredInvoiceList = null;
        Object invoiceList = context.get("invoiceList");
        Object invoiceItemAssocTypeId = context.get("invoiceItemAssocTypeId");
        if (invoiceList != null) {
            for (Object invoice : (List<Object>) invoiceList) {
                try {
                    invoiceItemAssocList = EntityQuery.use(delegator)
                            .from("InvoiceItemAssoc")
                            .where(UtilMisc.toMap("invoiceIdFrom", ((Map<String, Object>) invoice).get("invoiceId"), "invoiceItemAssocTypeId", invoiceItemAssocTypeId))
                            .filterByDate()
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying InvoiceItemAssoc: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (UtilValidate.isEmpty(invoiceItemAssocList)) {
                    filteredInvoiceList.add(invoice);
                }
            }
        }
        result.put("filteredInvoiceList", filteredInvoiceList);

        return "success";
    }


    /**
     * Remove invoiceItemAssoc record on cancel invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeInvoiceItemAssocOnCancelInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> deleteInvoiceItemAssocMap = null;
        List<GenericValue> invoiceItemAssocs = null;
        try {
            invoiceItemAssocs = EntityQuery.use(delegator)
                    .from("InvoiceItemAssoc")
                    .where(UtilMisc.toMap("invoiceIdTo", context.get("invoiceId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InvoiceItemAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (invoiceItemAssocs != null) {
            for (GenericValue invoiceItemAssoc : invoiceItemAssocs) {
                // set-service-fields from "invoiceItemAssoc" to "deleteInvoiceItemAssocMap" for service "deleteInvoiceItemAssoc"
                deleteInvoiceItemAssocMap.putAll(UtilMisc.toMap(invoiceItemAssoc));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("deleteInvoiceItemAssoc", deleteInvoiceItemAssocMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling deleteInvoiceItemAssoc: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                Debug.logInfo("Removed invoiceItemAssoc: " + invoiceItemAssoc, MODULE);
            }
        }

        return "success";
    }


    /**
     * Reset OrderItemBilling and OrderAdjustmentBilling records on cancel invoice, so it is isn't considered invoiced any more by createInvoiceForOrder service
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String resetOrderItemBillingAndOrderAdjustmentBillingOnCancelInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> orderItemBillings = null;
        try {
            orderItemBillings = EntityQuery.use(delegator)
                    .from("OrderItemBilling")
                    .where(UtilMisc.toMap("invoiceId", context.get("invoiceId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderItemBilling: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (orderItemBillings != null) {
            for (GenericValue orderItemBilling : orderItemBillings) {
                orderItemBilling.put("quantity", BigDecimal.ZERO);
                try {
                    delegator.store(orderItemBilling);
                } catch (Exception e) {
                    Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        List<GenericValue> orderAdjustmentBillings = null;
        try {
            orderAdjustmentBillings = EntityQuery.use(delegator)
                    .from("OrderAdjustmentBilling")
                    .where(UtilMisc.toMap("invoiceId", context.get("invoiceId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderAdjustmentBilling: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (orderAdjustmentBillings != null) {
            for (GenericValue orderAdjustmentBilling : orderAdjustmentBillings) {
                orderAdjustmentBilling.put("amount", BigDecimal.ZERO);
                try {
                    delegator.store(orderAdjustmentBilling);
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
     * Service set status of Invoices in bulk.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String massChangeInvoiceStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> setInvoiceStatusMap = null;
        if (context.get("invoiceIds") != null) {
            for (GenericValue invoiceId : (List<GenericValue>) context.get("invoiceIds")) {
                setInvoiceStatusMap.put("invoiceId", invoiceId);
                setInvoiceStatusMap.put("statusId", context.get("statusId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setInvoiceStatus", setInvoiceStatusMap);
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
                setInvoiceStatusMap = null;
            }
        }

        return "success";
    }


    /**
     * Set Parameter And Call Tax Calculate Service
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String addtax(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        List<GenericValue> contacts = null;
        Map<String, Object> addtaxMap = null;
        BigDecimal totalAmount = null;
        List<GenericValue> product = null;
        BigDecimal total = null;
        List<GenericValue> findinvoiceItems = null;
        BigDecimal itemAmount = null;
        GenericValue itemProduct = null;
        BigDecimal itemPrice = null;
        Object itemAdjustments = null;
        Map<String, Object> InvoiceItemContext = null;
        Object productId = null;
        Object countItemId = null;
        Map<String, Object> createInvoiceItemContext = null;
        Object invoiceItemSeqId = null;
        Object orderAdjustments = null;
        GenericValue invoice = null;
        try {
            invoice = EntityQuery.use(delegator)
                    .from("Invoice")
                    .where(UtilMisc.toMap("invoiceId", context.get("invoiceId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> invoiceItems = null;
        try {
            invoiceItems = EntityQuery.use(delegator)
                    .from("InvoiceItem")
                    .where(UtilMisc.toMap("invoiceId", ((Map<String, Object>) invoice).get("invoiceId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InvoiceItem: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            contacts = EntityQuery.use(delegator)
                    .from("PartyContactMechPurpose")
                    .where(UtilMisc.toMap("partyId", ((Map<String, Object>) invoice).get("partyId"), "contactMechPurposeTypeId", "SHIPPING_LOCATION"))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(contacts)) {
            try {
                contacts = EntityQuery.use(delegator)
                        .from("PartyContactMechPurpose")
                        .where(UtilMisc.toMap("partyId", ((Map<String, Object>) invoice).get("partyId"), "contactMechPurposeTypeId", "GENERAL_LOCATION"))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyContactMechPurpose: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isEmpty(contacts)) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingTaxCannotCalculate", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        GenericValue contactMech = EntityUtil.getFirst((List<GenericValue>) contacts);
        GenericValue postalAddress = null;
        try {
            postalAddress = EntityQuery.use(delegator)
                    .from("PostalAddress")
                    .where(UtilMisc.toMap("contactMechId", ((Map<String, Object>) contactMech).get("contactMechId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PostalAddress: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("SALES_INVOICE".equals(((Map<String, Object>) invoice).get("invoiceTypeId"))) {
            addtaxMap.put("billToPartyId", ((Map<String, Object>) invoice).get("partyId"));
        }
        if ("PURCHASE_INVOICE".equals(((Map<String, Object>) invoice).get("invoiceTypeId"))) {
            addtaxMap.put("billToPartyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
        }
        addtaxMap.put("payToPartyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
        if (invoiceItems != null) {
            for (GenericValue invoiceItem : invoiceItems) {
                try {
                    product = EntityQuery.use(delegator)
                            .from("Product")
                            .where(UtilMisc.toMap("productId", ((Map<String, Object>) invoiceItem).get("productId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying Product: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                itemProduct = EntityUtil.getFirst((List<GenericValue>) product);
                if (UtilValidate.isNotEmpty(((Map<String, Object>) invoiceItem).get("productId"))) {
                    try {
                        findinvoiceItems = EntityQuery.use(delegator)
                                .from("InvoiceItem")
                                .where(UtilMisc.toMap("invoiceId", ((Map<String, Object>) invoice).get("invoiceId"), "productId", ((Map<String, Object>) invoiceItem).get("productId"), "invoiceItemTypeId", "ITM_PROMOTION_ADJ"))
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying InvoiceItem: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isNotEmpty(findinvoiceItems)) {
                        total = ((new BigDecimal(((Map<String, Object>) invoiceItem).get("quantity").toString())).multiply(new BigDecimal(((Map<String, Object>) invoiceItem).get("amount").toString()))).setScale(((Number) context.get("roundingDecimals")).intValue(), RoundingMode.valueOf(String.valueOf(context.get("roundingMode")).toUpperCase().replace("-", "_")));
                        totalAmount = total;
                        totalAmount = ((new BigDecimal(totalAmount.toString())).subtract(new BigDecimal(((Map<String, Object>) invoiceItem).get("amount").toString()))).setScale(((Number) context.get("roundingDecimals")).intValue(), RoundingMode.valueOf(String.valueOf(context.get("roundingMode")).toUpperCase().replace("-", "_")));
                    } else {
                        total = ((new BigDecimal(((Map<String, Object>) invoiceItem).get("quantity").toString())).multiply(new BigDecimal(((Map<String, Object>) invoiceItem).get("amount").toString()))).setScale(((Number) context.get("roundingDecimals")).intValue(), RoundingMode.valueOf(String.valueOf(context.get("roundingMode")).toUpperCase().replace("-", "_")));
                        totalAmount = total;
                    }
                } else {
                    totalAmount = BigDecimal.ZERO;
                }
                itemAmount = totalAmount;
                itemPrice = (BigDecimal) ((Map<String, Object>) invoiceItem).get("amount");
                List<Object> addtaxMap_itemProductList = new LinkedList<>();
                addtaxMap_itemProductList.add(itemProduct);
                List<Object> addtaxMap_itemAmountList = new LinkedList<>();
                addtaxMap_itemAmountList.add(itemAmount);
                List<Object> addtaxMap_itemPriceList = new LinkedList<>();
                addtaxMap_itemPriceList.add(itemPrice);
                List<Object> addtaxMap_itemQuantityList = new LinkedList<>();
                addtaxMap_itemQuantityList.add(((Map<String, Object>) invoiceItem).get("quantity"));
                List<Object> addtaxMap_itemShippingList = new LinkedList<>();
                addtaxMap_itemShippingList.add(BigDecimal.ZERO);
            }
        }
        addtaxMap.put("orderShippingAmount", BigDecimal.ZERO);
        addtaxMap.put("orderPromotionsAmount", BigDecimal.ZERO);
        addtaxMap.put("shippingAddress", postalAddress);
        Object itemMap_itemSeqIdList__ = null;
        Object itemMap_productList__ = null;
        Object createInvoiceItemContext_invoiceId = null;
        Object createInvoiceItemContext_invoiceItemTypeId = null;
        Object createInvoiceItemContext_overrideGlAccountId = null;
        Object createInvoiceItemContext_productId = null;
        Object createInvoiceItemContext_taxAuthPartyId = null;
        Object createInvoiceItemContext_taxAuthGeoId = null;
        BigDecimal createInvoiceItemContext_amount = null;
        Object createInvoiceItemContext_quantity = null;
        Object createInvoiceItemContext_parentInvoiceItemSeqId = null;
        Object createInvoiceItemContext_taxAuthorityRateSeqId = null;
        Object createInvoiceItemContext_description = null;
        Object InvoiceItemContext_invoiceId = null;
        Object InvoiceItemContext_invoiceItemTypeId = null;
        Object InvoiceItemContext_overrideGlAccountId = null;
        Object InvoiceItemContext_taxAuthPartyId = null;
        Object InvoiceItemContext_taxAuthGeoId = null;
        BigDecimal InvoiceItemContext_amount = null;
        Object InvoiceItemContext_quantity = null;
        Object InvoiceItemContext_taxAuthorityRateSeqId = null;
        if (!(UtilValidate.isEmpty(((Map<String, Object>) addtaxMap).get("itemProductList")))) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("calcTax", addtaxMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                itemAdjustments = serviceResult.get("itemAdjustments");
                orderAdjustments = serviceResult.get("orderAdjustments");
            } catch (Exception e) {
                Debug.logError(e, "Error calling calcTax: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (invoiceItems != null) {
                for (GenericValue findItem : invoiceItems) {
                    if (!(UtilValidate.isEmpty(((Map<String, Object>) findItem).get("productId")))) {
                        invoiceItemSeqId = ((Map<String, Object>) findItem).get("invoiceItemSeqId");
                        productId = ((Map<String, Object>) findItem).get("productId");
                        List<Object> itemMap_itemSeqIdList = new LinkedList<>();
                        itemMap_itemSeqIdList.add(invoiceItemSeqId);
                        List<Object> itemMap_productList = new LinkedList<>();
                        itemMap_productList.add(productId);
                    }
                }
            }
            countItemId = -1L;
            if (itemAdjustments != null) {
                for (Object itemAdjustment : (List<Object>) itemAdjustments) {
                    countItemId = new BigDecimal(countItemId.toString());
                    if (UtilValidate.isNotEmpty(itemAdjustment)) {
                        if (itemAdjustment != null) {
                            for (Object orderAdjustment : (List<Object>) itemAdjustment) {
                                createInvoiceItemContext.put("invoiceId", ((Map<String, Object>) invoice).get("invoiceId"));
                                if ("PURCHASE_INVOICE".equals(((Map<String, Object>) invoice).get("invoiceTypeId"))) {
                                    createInvoiceItemContext.put("invoiceItemTypeId", "PITM_SALES_TAX");
                                } else {
                                    createInvoiceItemContext.put("invoiceItemTypeId", "ITM_SALES_TAX");
                                }
                                createInvoiceItemContext.put("overrideGlAccountId", ((Map<String, Object>) orderAdjustment).get("overrideGlAccountId"));
                                createInvoiceItemContext.put("productId", ((Map<String, Object>) context.get("itemMap")).get("productList[countItemId]"));
                                createInvoiceItemContext.put("taxAuthPartyId", ((Map<String, Object>) orderAdjustment).get("taxAuthPartyId"));
                                createInvoiceItemContext.put("taxAuthGeoId", ((Map<String, Object>) orderAdjustment).get("taxAuthGeoId"));
                                createInvoiceItemContext.put("amount", ((Map<String, Object>) orderAdjustment).get("amount"));
                                createInvoiceItemContext.put("quantity", "1");
                                createInvoiceItemContext.put("parentInvoiceItemSeqId", ((Map<String, Object>) context.get("itemMap")).get("itemSeqIdList[countItemId]"));
                                createInvoiceItemContext.put("taxAuthorityRateSeqId", ((Map<String, Object>) orderAdjustment).get("taxAuthorityRateSeqId"));
                                createInvoiceItemContext.put("description", ((Map<String, Object>) orderAdjustment).get("comments"));
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("createInvoiceItem", createInvoiceItemContext);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                        return "error";
                                    }
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling createInvoiceItem: " + e.getMessage(), MODULE);
                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                    return "error";
                                }
                            }
                        }
                    }
                }
            }
            if (orderAdjustments != null) {
                for (Object Adjustment : (List<Object>) orderAdjustments) {
                    if (UtilValidate.isNotEmpty(Adjustment)) {
                        InvoiceItemContext.put("invoiceId", ((Map<String, Object>) invoice).get("invoiceId"));
                        if ("PURCHASE_INVOICE".equals(((Map<String, Object>) invoice).get("invoiceTypeId"))) {
                            InvoiceItemContext.put("invoiceItemTypeId", "PITM_SALES_TAX");
                        } else {
                            InvoiceItemContext.put("invoiceItemTypeId", "ITM_SALES_TAX");
                        }
                        InvoiceItemContext.put("overrideGlAccountId", ((Map<String, Object>) Adjustment).get("overrideGlAccountId"));
                        InvoiceItemContext.put("taxAuthPartyId", ((Map<String, Object>) Adjustment).get("taxAuthPartyId"));
                        InvoiceItemContext.put("taxAuthGeoId", ((Map<String, Object>) Adjustment).get("taxAuthGeoId"));
                        InvoiceItemContext.put("amount", ((Map<String, Object>) Adjustment).get("amount"));
                        InvoiceItemContext.put("quantity", "1");
                        InvoiceItemContext.put("taxAuthorityRateSeqId", ((Map<String, Object>) Adjustment).get("taxAuthorityRateSeqId"));
                        try {
                            Map<String, Object> serviceResult = dispatcher.runSync("createInvoiceItem", InvoiceItemContext);
                            if (ServiceUtil.isError(serviceResult)) {
                                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                                return "error";
                            }
                        } catch (Exception e) {
                            Debug.logError(e, "Error calling createInvoiceItem: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                    }
                }
            }
        } else {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingTaxProductIdCannotCalculate", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            Debug.logError("Cannot call calcTax service, when don't have productId", MODULE);
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }

        return "success";
    }


    /**
     * Create an invoice from existing order when invoicePerShipment is N
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createInvoiceFromOrder(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        String invoicePerShipment = null;
        Map<String, Object> createInvoiceContext = null;
        List<GenericValue> orderItemBilling = null;
        List<GenericValue> checkOrderItem = null;
        Object invoiceId = null;
        List<Object> billItems = null;
        List<GenericValue> orderItems = null;
        GenericValue orderHeader = null;
        try {
            orderHeader = EntityQuery.use(delegator)
                    .from("OrderHeader")
                    .where(UtilMisc.toMap("orderId", context.get("orderId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying OrderHeader: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        invoicePerShipment = (String) ((Map<String, Object>) orderHeader).get("invoicePerShipment");
        if (UtilValidate.isEmpty(invoicePerShipment)) {
            invoicePerShipment = UtilProperties.getMessage("AccountingConfig", "create.invoice.per.shipment", locale);
        }
        if ("N".equals(invoicePerShipment)) {
            try {
                orderItemBilling = EntityQuery.use(delegator)
                        .from("OrderItemBilling")
                        .where(UtilMisc.toMap("orderId", context.get("orderId")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying OrderItemBilling: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            GenericValue orderItem = null;
            if (UtilValidate.isEmpty(orderItemBilling)) {
                createInvoiceContext.put("orderId", context.get("orderId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createInvoiceForOrderAllItems", createInvoiceContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    invoiceId = serviceResult.get("invoiceId");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createInvoiceForOrderAllItems: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                try {
                    orderItems = EntityQuery.use(delegator)
                            .from("OrderItem")
                            .where(UtilMisc.toMap("orderId", context.get("orderId")))
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying OrderItem: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                if (orderItems != null) {
                    for (GenericValue orderItemEntry : orderItems) {
                        try {
                            checkOrderItem = EntityQuery.use(delegator)
                                    .from("OrderItemBilling")
                                    .where(UtilMisc.toMap("orderId", context.get("orderId"), "orderItemSeqId", ((Map<String, Object>) orderItemEntry).get("orderItemSeqId")))
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying OrderItemBilling: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (UtilValidate.isEmpty(checkOrderItem)) {
                            billItems.add(orderItemEntry);
                        }
                        createInvoiceContext.put("orderId", context.get("orderId"));
                        createInvoiceContext.put("billItems", billItems);
                    }
                }
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createInvoiceForOrder", createInvoiceContext);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    invoiceId = serviceResult.get("invoiceId");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createInvoiceForOrder: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            result.put("invoiceId", invoiceId);
        }

        return "success";
    }


    /**
     * Create Content For Invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createInvoiceContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        Timestamp nowTimestamp = null;
        newEntity = delegator.makeValue("InvoiceContent");
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
        result.put("invoiceId", ((Map<String, Object>) newEntity).get("invoiceId"));
        result.put("invoiceContentTypeId", ((Map<String, Object>) newEntity).get("invoiceContentTypeId"));

        return "success";
    }


    /**
     * Update Content For Invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateInvoiceContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("InvoiceContent");
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
     * Remove Content From Invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String removeInvoiceContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookupPKMap = delegator.makeValue("InvoiceContent");
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


    /**
     * Create Simple Text Content For Invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createSimpleTextContentForInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> createInvoiceContentMap = new HashMap<>();
        // set-service-fields from "parameters" to "createInvoiceContentMap" for service "createInvoiceContent"
        createInvoiceContentMap.putAll(UtilMisc.toMap(context));
        Map<String, Object> createSimpleTextMap = new HashMap<>();
        // set-service-fields from "parameters" to "createSimpleTextMap" for service "createSimpleTextContent"
        createSimpleTextMap.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createSimpleTextContent", createSimpleTextMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            createInvoiceContentMap.put("contentId", serviceResult.get("contentId"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling createSimpleTextContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createInvoiceContent", createInvoiceContentMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createInvoiceContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update Simple Text Content For Invoice
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateSimpleTextContentForInvoice(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> updateInvoiceContent = new HashMap<>();
        // set-service-fields from "parameters" to "updateInvoiceContent" for service "updateInvoiceContent"
        updateInvoiceContent.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateInvoiceContent", updateInvoiceContent);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateInvoiceContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> updateSimpleText = new HashMap<>();
        // set-service-fields from "parameters" to "updateSimpleText" for service "updateSimpleTextContent"
        updateSimpleText.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateSimpleTextContent", updateSimpleText);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateSimpleTextContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * check if a invoice is in a foreign currency related to the accounting company.
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String isInvoiceInForeignCurrency(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> pref = null;
        Boolean isForeign = null;
        GenericValue invoice = null;
        try {
            invoice = EntityQuery.use(delegator)
                    .from("InvoiceAndType")
                    .where(UtilMisc.toMap("invoiceId", context.get("invoiceId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying InvoiceAndType: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(invoice)) {
            Debug.logError("Invoice not found", MODULE);
            return "success";
        }
        Object isPurchaseInvoice = (Boolean) GroovyUtil.eval("org.ofbiz.entity.util.EntityTypeUtil.hasParentType(delegator, 'InvoiceType', 'invoiceTypeId', invoice.getString('invoiceTypeId'), 'parentTypeId', 'PURCHASE_INVOICE')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        Object isSalesInvoice = (Boolean) GroovyUtil.eval("org.ofbiz.entity.util.EntityTypeUtil.hasParentType(delegator, 'InvoiceType', 'invoiceTypeId', invoice.getString('invoiceTypeId'), 'parentTypeId', 'SALES_INVOICE')", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context));
        if (Boolean.TRUE.equals(isPurchaseInvoice)) {
            pref.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyId"));
        }
        if (Boolean.TRUE.equals(isSalesInvoice)) {
            pref.put("organizationPartyId", ((Map<String, Object>) invoice).get("partyIdFrom"));
        }
        Object partyAccountingPreference = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getPartyAccountingPreferences", pref);
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
        if (java.util.Objects.equals(((Map<String, Object>) invoice).get("currencyUomId"), ((Map<String, Object>) partyAccountingPreference).get("baseCurrencyUomId"))) {
            isForeign = Boolean.FALSE;
        } else {
            isForeign = Boolean.TRUE;
        }
        result.put("isForeign", isForeign);

        return "success";
    }

}

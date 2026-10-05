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
package com.ilscipio.scipio.workeffort.event;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.GroovyUtil;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://workeffort/script/org/ofbiz/workeffort/timesheet/TimesheetServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class TimesheetServices {

    private static final String MODULE = TimesheetServices.class.getName();


    /**
     * Create Timesheet
     */
    public static Map<String, Object> createTimesheet(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue newEntity = null;
        newEntity = delegator.makeValue("Timesheet");
        ((GenericValue) newEntity).put("timesheetId", delegator.getNextSeqId("Timesheet"));
        result.put("timesheetId", newEntity.get("timesheetId"));
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("statusId"))) {
            newEntity.put("statusId", "TIMESHEET_IN_PROCESS");
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update Timesheet
     */
    public static Map<String, Object> updateTimesheet(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue timesheet = null;
        GenericValue statusItem = null;
        Map<String, Object> inlineResult = checkTimesheetStatus(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        timesheet.setNonPKFields(context);
        try {
            delegator.store(timesheet);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Updates the Timesheet status back to in process to be able to correct errors
     */
    public static Map<String, Object> updateTimesheetToInProcess(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue timesheet = null;
        try {
            timesheet = EntityQuery.use(delegator)
                    .from("Timesheet")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Timesheet: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        timesheet.put("statusId", "TIMESHEET_IN_PROCESS");
        try {
            delegator.store(timesheet);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete Timesheet
     */
    public static Map<String, Object> deleteTimesheet(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue timesheet = null;
        GenericValue statusItem = null;
        Map<String, Object> inlineResult = checkTimesheetStatus(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        try {
            delegator.removeValue(timesheet);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create Timesheet For This Week of no date provided, otherwise for a specific week
     */
    public static Map<String, Object> createTimesheetForThisWeek(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Timestamp nowTimestamp = null;
        Map<String, Object> inlineResult = null;
        GenericValue newEntity = null;
        if (UtilValidate.isEmpty(context.get("requiredDate"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
        } else {
            nowTimestamp = (Timestamp) context.get("requiredDate");
        }
        try {
            ((Map<String, Object>) context).put("fromDate", UtilDateTime.getWeekStart((Timestamp) nowTimestamp));
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilDateTime.getWeekStart: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            ((Map<String, Object>) context).put("thruDate", UtilDateTime.getWeekEnd((Timestamp) nowTimestamp));
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilDateTime.getWeekEnd: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        List<GenericValue> timesheets = null;
        try {
            timesheets = EntityQuery.use(delegator)
                    .from("Timesheet")
                    .where(UtilMisc.toMap("partyId", context.get("partyId"), "fromDate", context.get("fromDate"), "thruDate", context.get("thruDate")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(timesheets)) {
            inlineResult = createTimesheet(dctx, context);
            if (ServiceUtil.isError(inlineResult)) {
                return inlineResult;
            }
        } else {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortTimesheetAlreadyExists", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }

        return result;
    }


    /**
     * Creates Timesheet multiple parties at a time
     */
    public static Map<String, Object> createTimesheets(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> createParams = null;
        if (context.get("partyIdList") != null) {
            for (GenericValue partyId : (List<GenericValue>) context.get("partyIdList")) {
                context.put("partyId", partyId);
                // set-service-fields from "parameters" to "createParams" for service "createTimesheet"
                createParams.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createTimesheet", createParams);
                    if (ServiceUtil.isError(serviceResult)) {
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createTimesheet: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
            }
        }

        return result;
    }


    /**
     * Add Timesheet to Invoice
     */
    public static Map<String, Object> addTimesheetToInvoice(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> createInvoiceMap = null;
        Object invoiceId = null;
        Map<String, Object> updTimeEntry = null;
        Map<String, Object> invoiceItemMap = null;
        GenericValue custRequestWorkEffort = null;
        Object rateAmount = null;
        Object errMsg = null;
        List<Object> orderBy = null;
        GenericValue partyRate = null;
        GenericValue timesheet = null;
        List<GenericValue> timeEntryList = null;
        GenericValue custRequest = null;
        Object oldRateAmount = null;
        Object invoiceItemDescription = null;
        List<GenericValue> partyRates = null;
        List<GenericValue> custRequestWorkEfforts = null;
        GenericValue timeEntry = null;
        Map<String, Object> getTimeEntryRate = null;
        List<GenericValue> existAmountAndDescriptionInvoiceItems = null;
        try {
            timesheet = EntityQuery.use(delegator)
                    .from("Timesheet")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Timesheet: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            timeEntryList = timesheet.getRelated("TimeEntry", null, null, false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related TimeEntry: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(context.get("invoiceId"))) {
            // set-service-fields from "parameters" to "createInvoiceMap" for service "createInvoice"
            createInvoiceMap.putAll(UtilMisc.toMap(context));
            createInvoiceMap.put("invoiceTypeId", "SALES_INVOICE");
            createInvoiceMap.put("statusId", "INVOICE_IN_PROCESS");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createInvoice", createInvoiceMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                invoiceId = serviceResult.get("invoiceId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createInvoice: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            result.put("invoiceId", invoiceId);
        } else {
            invoiceId = context.get("invoiceId");
        }
        GenericValue invoice = null;
        try {
            invoice = EntityQuery.use(delegator)
                    .from("Invoice")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(invoice)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortTimesheetCannotFindInvoice", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        context.put("updTimeEntry", updTimeEntry);
        context.put("invoiceItemMap", invoiceItemMap);
        context.put("custRequestWorkEffort", custRequestWorkEffort);
        context.put("rateAmount", rateAmount);
        context.put("errMsg", errMsg);
        context.put("orderBy", orderBy);
        context.put("partyRate", partyRate);
        context.put("timesheet", timesheet);
        context.put("timeEntryList", timeEntryList);
        context.put("custRequest", custRequest);
        context.put("oldRateAmount", oldRateAmount);
        context.put("invoiceItemDescription", invoiceItemDescription);
        context.put("partyRates", partyRates);
        context.put("custRequestWorkEfforts", custRequestWorkEfforts);
        context.put("timeEntry", timeEntry);
        context.put("invoice", invoice);
        context.put("getTimeEntryRate", getTimeEntryRate);
        context.put("existAmountAndDescriptionInvoiceItems", existAmountAndDescriptionInvoiceItems);
        createTimeEntryInvoiceItemsInline(dctx, context);

        return result;
    }


    /**
     * Add Work Effort Time to Invoice
     */
    public static Map<String, Object> addWorkEffortTimeToInvoice(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> createInvoiceMap = null;
        Map<String, Object> updTimeEntry = null;
        Map<String, Object> invoiceItemMap = null;
        GenericValue custRequestWorkEffort = null;
        Object rateAmount = null;
        Object errMsg = null;
        List<Object> orderBy = null;
        GenericValue partyRate = null;
        GenericValue timesheet = null;
        List<GenericValue> timeEntryList = null;
        GenericValue custRequest = null;
        Object oldRateAmount = null;
        Object invoiceItemDescription = null;
        List<GenericValue> partyRates = null;
        List<GenericValue> custRequestWorkEfforts = null;
        GenericValue timeEntry = null;
        Map<String, Object> getTimeEntryRate = null;
        List<GenericValue> existAmountAndDescriptionInvoiceItems = null;
        GenericValue workEffort = null;
        try {
            workEffort = EntityQuery.use(delegator)
                    .from("WorkEffort")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WorkEffort: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(context.get("invoiceId"))) {
            // set-service-fields from "parameters" to "createInvoiceMap" for service "createInvoice"
            createInvoiceMap.putAll(UtilMisc.toMap(context));
            createInvoiceMap.put("invoiceTypeId", "SALES_INVOICE");
            createInvoiceMap.put("statusId", "INVOICE_IN_PROCESS");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createInvoice", createInvoiceMap);
                if (ServiceUtil.isError(serviceResult)) {
                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                }
                context.put("invoiceId", serviceResult.get("invoiceId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createInvoice: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            result.put("invoiceId", context.get("invoiceId"));
        }
        GenericValue invoice = null;
        try {
            invoice = EntityQuery.use(delegator)
                    .from("Invoice")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(invoice)) {
            {
                String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortTimesheetCannotFindInvoice", locale);
                error_list.add(errorMsg);
            }
            if (!error_list.isEmpty()) {
                return ServiceUtil.returnError(error_list);
            }
        }
        GenericValue party = null;
        try {
            party = EntityQuery.use(delegator)
                    .from("Party")
                    .where(UtilMisc.toMap("partyId", "${invoice.partyId}"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Party: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        if (UtilValidate.isEmpty(party.get("preferredCurrencyUomId"))) {
            Object party_preferredCurrencyUomId = UtilProperties.getMessage("general", "currency.uom.id.default", locale);
        }
        Map<String, Object> updateInvoiceMap = new HashMap<String, Object>();
        updateInvoiceMap.put("invoiceId", context.get("invoiceId"));
        updateInvoiceMap.put("currencyUomId", party.get("preferredCurrencyUomId"));
        Timestamp updateInvoiceMap_invoiceDate = new Timestamp(System.currentTimeMillis());
        if (UtilValidate.isEmpty(((Map<String, Object>) updateInvoiceMap).get("currencyUomId"))) {
            Object invoice_currencyUomId = UtilProperties.getMessage("general", "currency.uom.id.default", locale);
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateInvoice", updateInvoiceMap);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateInvoice: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            invoice = EntityQuery.use(delegator)
                    .from("Invoice")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Invoice: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        context.put("workEffort", workEffort);
        context.put("updTimeEntry", updTimeEntry);
        context.put("invoiceItemMap", invoiceItemMap);
        context.put("custRequestWorkEffort", custRequestWorkEffort);
        context.put("rateAmount", rateAmount);
        context.put("errMsg", errMsg);
        context.put("orderBy", orderBy);
        context.put("partyRate", partyRate);
        context.put("timesheet", timesheet);
        context.put("timeEntryList", timeEntryList);
        context.put("custRequest", custRequest);
        context.put("oldRateAmount", oldRateAmount);
        context.put("invoiceItemDescription", invoiceItemDescription);
        context.put("partyRates", partyRates);
        context.put("custRequestWorkEfforts", custRequestWorkEfforts);
        context.put("timeEntry", timeEntry);
        context.put("invoice", invoice);
        context.put("getTimeEntryRate", getTimeEntryRate);
        context.put("existAmountAndDescriptionInvoiceItems", existAmountAndDescriptionInvoiceItems);
        createTimeEntryInvoiceItemsInline(dctx, context);

        return result;
    }


    /**
     * createTimeEntryInvoiceItemsInline
     */
    public static Map<String, Object> createTimeEntryInvoiceItemsInline(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        Object invoiceId = context.get("invoiceId");
        Map<String, Object> invoiceItemMap = null;
        List<GenericValue> timeEntryList = null;
        GenericValue custRequest = null;
        GenericValue custRequestWorkEffort = null;
        Object invoiceItemDescription = null;
        List<GenericValue> custRequestWorkEfforts = null;
        GenericValue timesheet = null;
        Map<String, Object> updTimeEntry = null;
        Object rateAmount = null;
        Object oldRateAmount = null;
        Object errMsg = null;
        List<GenericValue> partyRates = null;
        GenericValue partyRate = null;
        Map<String, Object> getTimeEntryRate = null;
        List<GenericValue> existAmountAndDescriptionInvoiceItems = null;
        List<Object> orderBy = new LinkedList<>();
        orderBy.add("rateTypeId");
        invoiceItemMap.put("invoiceId", context.get("invoiceId"));
        invoiceItemMap.put("taxableFlag", "N");
        invoiceItemMap.put("invoiceItemTypeId", "INV_TE_ITEM");
        invoiceItemMap.put("uomId", "TF_hr");
        if (UtilValidate.isNotEmpty(timesheet)) {
            invoiceItemMap.put("description", "[Timesheet:" + timesheet.get("timesheetId") + "]");
        }
        if (UtilValidate.isNotEmpty(context.get("workEffort"))) {
            try {
                timeEntryList = ((GenericValue) context.get("workEffort")).getRelated("TimeEntry", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related TimeEntry: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            invoiceItemMap.put("description", "" + ((Map<String, Object>) context.get("workEffort")).get("workEffortName") + " [Task:" + ((Map<String, Object>) context.get("workEffort")).get("workEffortId") + "]");
            try {
                custRequestWorkEfforts = ((GenericValue) context.get("workEffort")).getRelated("CustRequestWorkEffort", null, null, false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related CustRequestWorkEffort: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(custRequestWorkEfforts)) {
                custRequestWorkEffort = EntityUtil.getFirst((List<GenericValue>) custRequestWorkEfforts);
                try {
                    custRequest = custRequestWorkEffort.getRelatedOne("CustRequest", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one CustRequest: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                if (UtilValidate.isNotEmpty(custRequest)) {
                    invoiceItemDescription = "" + custRequest.get("custRequestName") + " [CRQ:" + custRequest.get("custRequestId") + "] " + custRequest.get("description");
                    invoiceItemMap.put("description", GroovyUtil.eval("invoiceItemDescription.size()>255?invoiceItemDescription.substring(0,251)+\" ...\":invoiceItemDescription", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context)));
                }
            }
        }
        if (timeEntryList != null) {
            for (GenericValue timeEntry : timeEntryList) {
                Object timeEntry_partyId = null;
                Object getTimeEntryRate_timeEntryId = null;
                Object getTimeEntryRate_currencyUomId = null;
                Object invoiceItemMap_invoiceItemSeqId = null;
                BigDecimal invoiceItemMap_amount = null;
                BigDecimal invoiceItemMap_quantity = null;
                Object invoiceItemMap_description = null;
                Object updTimeEntry_timeEntryId = null;
                Object updTimeEntry_invoiceId = null;
                Object updTimeEntry_invoiceItemSeqId = null;
                if (((!(UtilValidate.isEmpty(context.get("thruDate"))) && timeEntry.get("fromDate") != null /* TODO: field compare operator less */) || UtilValidate.isEmpty(context.get("thruDate")))) {
                    Map<String, Object> invoice = new HashMap<String, Object>();
                    if ("INVOICE_IN_PROCESS".equals(((Map<String, Object>) invoice).get("statusId"))) {
                        if (UtilValidate.isEmpty(timeEntry.get("invoiceId"))) {
                            if (UtilValidate.isEmpty(timeEntry.get("partyId"))) {
                                if (UtilValidate.isNotEmpty(timeEntry.get("timesheetId"))) {
                                    try {
                                        timesheet = EntityQuery.use(delegator)
                                                .from("Timesheet")
                                                .where(UtilMisc.toMap("timesheetId", timeEntry.get("timesheetId")))
                                                .queryOne();
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error querying Timesheet: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                    timeEntry.put("partyId", timesheet.get("partyId"));
                                }
                            }
                            if (UtilValidate.isNotEmpty(timeEntry.get("partyId"))) {
                                try {
                                    partyRates = EntityQuery.use(delegator)
                                            .from("PartyRate")
                                            .where(UtilMisc.toMap("rateTypeId", timeEntry.get("rateTypeId"), "partyId", timeEntry.get("partyId")))
                                            .filterByDate()
                                            .queryList();
                                } catch (Exception e) {
                                    Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                                if (UtilValidate.isNotEmpty(partyRates)) {
                                    partyRate = EntityUtil.getFirst((List<GenericValue>) partyRates);
                                    if (UtilValidate.isNotEmpty(partyRate.get("percentageUsed"))) {
                                        timeEntry.set("hours", (new BigDecimal(partyRate.get("percentageUsed").toString())).doubleValue());
                                        timeEntry.set("hours", (new BigDecimal(timeEntry.get("hours").toString())).doubleValue());
                                    }
                                }
                            }
                            getTimeEntryRate.put("timeEntryId", timeEntry.get("timeEntryId"));
                            getTimeEntryRate.put("currencyUomId", ((Map<String, Object>) invoice).get("currencyUomId"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("getTimeEntryRate", getTimeEntryRate);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                                rateAmount = serviceResult.get("rateAmount");
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling getTimeEntryRate: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            try {
                                existAmountAndDescriptionInvoiceItems = EntityQuery.use(delegator)
                                        .from("InvoiceItem")
                                        .where(UtilMisc.toMap("invoiceId", ((Map<String, Object>) invoiceItemMap).get("invoiceId"), "amount", rateAmount, "description", ((Map<String, Object>) invoiceItemMap).get("description")))
                                        .queryList();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying entity: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                            invoiceItemMap.put("invoiceItemSeqId", ((GenericValue) ((List<?>) existAmountAndDescriptionInvoiceItems).get(0)).get("invoiceItemSeqId"));
                            if (((UtilValidate.isEmpty(oldRateAmount) || !java.util.Objects.equals(rateAmount, oldRateAmount)) && UtilValidate.isEmpty(existAmountAndDescriptionInvoiceItems))) {
                                invoiceItemMap.put("amount", rateAmount);
                                if ("Y".equals(context.get("combineInvoiceItem"))) {
                                    invoiceItemMap.put("quantity", timeEntry.get("hours"));
                                    invoiceItemMap.remove("invoiceItemSeqId");
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("createInvoiceItem", invoiceItemMap);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                        }
                                        invoiceItemMap.put("invoiceItemSeqId", serviceResult.get("invoiceItemSeqId"));
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling createInvoiceItem: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                }
                            } else {
                                if ("Y".equals(context.get("combineInvoiceItem"))) {
                                    invoiceItemMap.put("quantity", ((GenericValue) ((List<?>) existAmountAndDescriptionInvoiceItems).get(0)).get("quantity"));
                                    ((Map<String, Object>) invoiceItemMap).put("quantity", new BigDecimal(((Map<String, Object>) invoiceItemMap).get("quantity").toString()));
                                    try {
                                        Map<String, Object> serviceResult = dispatcher.runSync("updateInvoiceItem", invoiceItemMap);
                                        if (ServiceUtil.isError(serviceResult)) {
                                            return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                        }
                                    } catch (Exception e) {
                                        Debug.logError(e, "Error calling updateInvoiceItem: " + e.getMessage(), MODULE);
                                        return ServiceUtil.returnError(e.getMessage());
                                    }
                                }
                            }
                            oldRateAmount = rateAmount;
                            if (!"Y".equals(context.get("combineInvoiceItem"))) {
                                invoiceItemMap.put("description", timeEntry.get("comments"));
                                if (UtilValidate.isEmpty(((Map<String, Object>) invoiceItemMap).get("description"))) {
                                    invoiceItemMap.put("description", ((Map<String, Object>) context.get("workEffort")).get("workEffortName"));
                                }
                                invoiceItemMap.put("quantity", timeEntry.get("hours"));
                                invoiceItemMap.remove("invoiceItemSeqId");
                                try {
                                    Map<String, Object> serviceResult = dispatcher.runSync("createInvoiceItem", invoiceItemMap);
                                    if (ServiceUtil.isError(serviceResult)) {
                                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                    }
                                    invoiceItemMap.put("invoiceItemSeqId", serviceResult.get("invoiceItemSeqId"));
                                } catch (Exception e) {
                                    Debug.logError(e, "Error calling createInvoiceItem: " + e.getMessage(), MODULE);
                                    return ServiceUtil.returnError(e.getMessage());
                                }
                            }
                            updTimeEntry.put("timeEntryId", timeEntry.get("timeEntryId"));
                            updTimeEntry.put("invoiceId", ((Map<String, Object>) invoiceItemMap).get("invoiceId"));
                            updTimeEntry.put("invoiceItemSeqId", ((Map<String, Object>) invoiceItemMap).get("invoiceItemSeqId"));
                            try {
                                Map<String, Object> serviceResult = dispatcher.runSync("updateTimeEntry", updTimeEntry);
                                if (ServiceUtil.isError(serviceResult)) {
                                    return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
                                }
                            } catch (Exception e) {
                                Debug.logError(e, "Error calling updateTimeEntry: " + e.getMessage(), MODULE);
                                return ServiceUtil.returnError(e.getMessage());
                            }
                        }
                    } else {
                        errMsg = "Invoice " + invoiceId + " should have the status 'in progress', the status is however: " + ((Map<String, Object>) invoice).get("statusId");
                        Debug.logError(String.valueOf(errMsg), MODULE);
                        {
                            String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortTimesheetInvoiceShuoldBeInProgressStatus", locale);
                            error_list.add(errorMsg);
                        }
                        return ServiceUtil.returnError(ServiceUtil.getErrorMessage(result));
                    }
                }
            }
        }

        return result;
    }


    /**
     * Create TimesheetRole
     */
    public static Map<String, Object> createTimesheetRole(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> ensurePartyRoleCtx = new HashMap<String, Object>();
        ensurePartyRoleCtx.put("partyId", context.get("partyId"));
        ensurePartyRoleCtx.put("roleTypeId", context.get("roleTypeId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("ensurePartyRole", ensurePartyRoleCtx);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling ensurePartyRole: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        GenericValue newEntity = delegator.makeValue("TimesheetRole");
        newEntity.setPKFields(context);
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete TimesheetRole
     */
    public static Map<String, Object> deleteTimesheetRole(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("TimesheetRole")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying TimesheetRole: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Create TimeEntry
     */
    public static Map<String, Object> createTimeEntry(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue timesheet = null;
        GenericValue statusItem = null;
        Map<String, Object> inlineResult = checkTimesheetStatus(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        GenericValue newEntity = delegator.makeValue("TimeEntry");
        ((GenericValue) newEntity).put("timeEntryId", delegator.getNextSeqId("TimeEntry"));
        result.put("timeEntryId", newEntity.get("timeEntryId"));
        newEntity.setNonPKFields(context);
        if (UtilValidate.isEmpty(newEntity.get("fromDate"))) {
            try {
                ((Map<String, Object>) newEntity).put("fromDate", UtilDateTime.getDayStart((Timestamp) nowTimestamp));
            } catch (Exception e) {
                Debug.logError(e, "Error calling UtilDateTime.getDayStart: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Update TimeEntry
     */
    public static Map<String, Object> updateTimeEntry(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Map<String, Object> fieldsToCopy = null;
        GenericValue lookedUpValue = null;
        GenericValue timesheet = null;
        GenericValue statusItem = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("TimeEntry")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying TimeEntry: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> inlineResult = checkTimesheetStatus(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        fieldsToCopy.putAll((Map<String, Object>) context);
        fieldsToCopy.remove("invoiceId");
        fieldsToCopy.remove("invoiceItemSeqId");
        Double fieldsToCopy_hours = null;
        if ((UtilValidate.isEmpty(((Map<String, Object>) fieldsToCopy).get("hours")) && (!(UtilValidate.isEmpty(((Map<String, Object>) fieldsToCopy).get("fromDate"))) || !(UtilValidate.isEmpty(((Map<String, Object>) fieldsToCopy).get("thruDate")))) && (!java.util.Objects.equals(((Map<String, Object>) fieldsToCopy).get("fromDate"), lookedUpValue.get("fromDate")) || !java.util.Objects.equals(((Map<String, Object>) fieldsToCopy).get("thruDate"), lookedUpValue.get("thruDate"))))) {
            fieldsToCopy.put("hours", (Double) GroovyUtil.eval("org.ofbiz.base.util.UtilDateTime.getInterval((fieldsToCopy.fromDate ? fieldsToCopy.fromDate : lookedUpValue.fromDate), (fieldsToCopy.thruDate ? fieldsToCopy.thruDate : lookedUpValue.thruDate))/3600000", UtilMisc.toMap("delegator", delegator, "dispatcher", dispatcher, "locale", locale, "userLogin", userLogin, "context", context, "parameters", context)));
        }
        lookedUpValue.setNonPKFields(fieldsToCopy);
        if (UtilValidate.isNotEmpty(context.get("invoiceId"))) {
            if (UtilValidate.isEmpty(lookedUpValue.get("invoiceId"))) {
                lookedUpValue.put("invoiceId", context.get("invoiceId"));
                lookedUpValue.put("invoiceItemSeqId", context.get("invoiceItemSeqId"));
            }
        }
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete TimeEntry
     */
    public static Map<String, Object> deleteTimeEntry(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue timesheet = null;
        GenericValue statusItem = null;
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("TimeEntry")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying TimeEntry: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        Map<String, Object> inlineResult = checkTimesheetStatus(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }
        try {
            delegator.removeValue(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Delete TimeEntry
     */
    public static Map<String, Object> unlinkInvoiceFromTimeEntry(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("TimeEntry")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying TimeEntry: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        result.put("invoiceId", lookedUpValue.get("invoiceId"));
        lookedUpValue.remove("invoiceId");
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Get TimeEntry Rate
     */
    public static Map<String, Object> getTimeEntryRate(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        GenericValue timesheet = null;
        Map<String, Object> getRate = null;
        GenericValue timeEntry = null;
        try {
            timeEntry = EntityQuery.use(delegator)
                    .from("TimeEntry")
                    .where(context)
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying TimeEntry: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        // set-service-fields from "parameters" to "getRate" for service "getRateAmount"
        getRate.putAll(UtilMisc.toMap(context));
        getRate.put("rateCurrencyUomId", context.get("currencyUomId"));
        getRate.put("rateTypeId", timeEntry.get("rateTypeId"));
        if (UtilValidate.isEmpty(timeEntry.get("partyId"))) {
            try {
                timesheet = timeEntry.getRelatedOne("Timesheet", false);
            } catch (Exception e) {
                Debug.logError(e, "Error getting related one Timesheet: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isNotEmpty(timesheet)) {
                getRate.put("partyId", timesheet.get("partyId"));
            }
        } else {
            getRate.put("partyId", timeEntry.get("partyId"));
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getRateAmount", getRate);
            if (ServiceUtil.isError(serviceResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(serviceResult));
            }
            result.put("rateAmount", serviceResult.get("rateAmount"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling getRateAmount: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return result;
    }


    /**
     * Check access and if the timesheet is in progress, however do allow invoiceId to be updated when completed (need to invoice completed timesheets)
     */
    public static Map<String, Object> checkTimesheetStatus(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        List<String> error_list = new LinkedList<>();

        GenericValue timesheet = null;
        GenericValue statusItem = null;
        Map<String, Object> lookedUpValue = new HashMap<String, Object>();
        Object parameters_timesheetId = null;
        if (((!(UtilValidate.isEmpty(context.get("timesheetId"))) || !(UtilValidate.isEmpty(((Map<String, Object>) lookedUpValue).get("timesheetId")))) && UtilValidate.isEmpty(context.get("invoiceId")))) {
            if (UtilValidate.isEmpty(context.get("timesheetId"))) {
                context.put("timesheetId", ((Map<String, Object>) lookedUpValue).get("timesheetId"));
            }
            try {
                timesheet = EntityQuery.use(delegator)
                        .from("Timesheet")
                        .where(context)
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Timesheet: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
            if (UtilValidate.isEmpty(timesheet)) {
                {
                    String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortTimesheetCouldNotBeFound", locale);
                    error_list.add(errorMsg);
                }
                Debug.logInfo("Timesheet not found, timesheet: " + context.get("timesheetId"), MODULE);
                if (!error_list.isEmpty()) {
                    return ServiceUtil.returnError(error_list);
                }
            }
            if (!"TIMESHEET_IN_PROCESS".equals(timesheet.get("statusId"))) {
                try {
                    statusItem = timesheet.getRelatedOne("StatusItem", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one StatusItem: " + e.getMessage(), MODULE);
                    return ServiceUtil.returnError(e.getMessage());
                }
                {
                    String errorMsg = UtilProperties.getMessage("WorkEffortUiLabels", "WorkEffortTimesheetNotInProcessStatus", locale);
                    error_list.add(errorMsg);
                }
                Debug.logInfo("Can only update Timesheet, when status is in-process...is now: " + timesheet.get("statusId"), MODULE);
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }

        return result;
    }

}

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
import org.ofbiz.base.util.Debug;
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
 * <p>Generated from: component://accounting/script/org/ofbiz/accounting/admin/AcctgAdminServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class AcctgAdminServices {

    private static final String MODULE = AcctgAdminServices.class.getName();


    /**
     * Create accounting preference settings for a party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyAcctgPreference(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Map<String, Object> lookupParams = new HashMap<>();
        lookupParams.put("partyId", context.get("partyId"));
        lookupParams.put("roleTypeId", "INTERNAL_ORGANIZATIO");
        GenericValue partyRole = null;
        try {
            partyRole = EntityQuery.use(delegator)
                    .from("PartyRole")
                    .where(lookupParams)
                    .cache()
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error finding by primary key PartyRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(partyRole)) {
            {
                String errorMsg = UtilProperties.getMessage("AccountingUiLabels", "AccountingPartyMustBeInternalOrganization", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
        GenericValue newEntity = delegator.makeValue("PartyAcctgPreference");
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
     * Get the accounting preference settings for a party (organization)
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPartyAccountingPreferences(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue currentPartyAcctgPref = null;
        Object currentOrganizationPartyId = null;
        GenericValue parentPartyRelationship = null;
        Boolean containsEmptyFields = null;
        GenericValue aggregatedPartyAcctgPref = null;
        List<GenericValue> parentPartyRelationships = null;
        aggregatedPartyAcctgPref = delegator.makeValue("PartyAcctgPreference");
        currentOrganizationPartyId = context.get("organizationPartyId");
        containsEmptyFields = Boolean.TRUE;
        while ((!(UtilValidate.isEmpty(currentOrganizationPartyId)) && containsEmptyFields == true)) {
            parentPartyRelationship = null;
            Object entityKey = null;
            Object entityValue = null;
            try {
                currentPartyAcctgPref = EntityQuery.use(delegator)
                        .from("PartyAcctgPreference")
                        .where(UtilMisc.toMap("partyId", currentOrganizationPartyId))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyAcctgPreference: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            containsEmptyFields = Boolean.FALSE;
            if (UtilValidate.isNotEmpty(currentPartyAcctgPref)) {
                for (Map.Entry<String, Object> entry : currentPartyAcctgPref.getAllFields().entrySet()) {
                    entityKey = entry.getKey();
                    entityValue = entry.getValue();
                    if (UtilValidate.isEmpty(aggregatedPartyAcctgPref.get((String) entityKey))) {
                        if (UtilValidate.isNotEmpty(entityValue)) {
                            aggregatedPartyAcctgPref.put((String) entityKey, entityValue);
                        } else {
                            containsEmptyFields = Boolean.TRUE;
                        }
                    }
                }
            } else {
                containsEmptyFields = Boolean.TRUE;
            }
            try {
                parentPartyRelationships = EntityQuery.use(delegator)
                        .from("PartyRelationship")
                        .where(UtilMisc.toMap("partyIdTo", currentOrganizationPartyId, "partyRelationshipTypeId", "GROUP_ROLLUP", "roleTypeIdFrom", "_NA_", "roleTypeIdTo", "_NA_"))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyRelationship: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(parentPartyRelationships)) {
                parentPartyRelationship = EntityUtil.getFirst((List<GenericValue>) parentPartyRelationships);
                currentOrganizationPartyId = ((Map<String, Object>) parentPartyRelationship).get("partyIdFrom");
            } else {
                currentOrganizationPartyId = null;
            }
        }
        if (UtilValidate.isNotEmpty(aggregatedPartyAcctgPref)) {
            aggregatedPartyAcctgPref.put("partyId", context.get("organizationPartyId"));
            result.put("partyAccountingPreference", aggregatedPartyAcctgPref);
        }

        return "success";
    }


    /**
     * Update Foreign Exchange conversion rate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateFXConversion(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp nowTimestamp = null;
        Map<String, Object> createParams = null;
        if (UtilValidate.isEmpty(context.get("asOfTimestamp"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
        } else {
            nowTimestamp = (Timestamp) context.get("asOfTimestamp");
        }
        List<GenericValue> uomConversions = null;
        try {
            uomConversions = EntityQuery.use(delegator)
                    .from("UomConversionDated")
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UomConversionDated: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (uomConversions != null) {
            for (GenericValue uomConversion : uomConversions) {
                if (UtilValidate.isEmpty(context.get("fromDate"))) {
                    uomConversion.put("thruDate", nowTimestamp);
                } else {
                    uomConversion.put("thruDate", context.get("fromDate"));
                }
            }
        }
        try {
            delegator.storeAll((List<GenericValue>) uomConversions);
        } catch (Exception e) {
            Debug.logError(e, "Error storing list: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // set-service-fields from "parameters" to "createParams" for service "createUomConversionDated"
        createParams.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            createParams.put("fromDate", nowTimestamp);
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createUomConversionDated", createParams);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createUomConversionDated: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Get Foreign Exchange conversion rate
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getFXConversion(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Timestamp asOfTimestamp = null;
        Object conversionFactor = null;
        BigDecimal originalValue = null;
        BigDecimal conversionRate = null;
        if (UtilValidate.isEmpty(context.get("asOfTimestamp"))) {
            asOfTimestamp = new Timestamp(System.currentTimeMillis());
        } else {
            asOfTimestamp = (Timestamp) context.get("asOfTimestamp");
        }
        List<GenericValue> rates = null;
        try {
            rates = EntityQuery.use(delegator)
                    .from("UomConversionDated")
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying UomConversionDated: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        conversionRate = BigDecimal.ONE;
        if (UtilValidate.isNotEmpty(rates)) {
            conversionFactor = ((GenericValue) ((List<?>) rates).get(0)).get("conversionFactor");
            originalValue = BigDecimal.ONE;
            conversionRate = ((new BigDecimal(originalValue.toString())).divide(new BigDecimal(conversionFactor.toString()), java.math.RoundingMode.HALF_UP)).setScale(2, RoundingMode.HALF_UP);
        } else {
            Debug.logError("Could not find conversion rate", MODULE);
        }
        result.put("conversionRate", conversionRate);

        return "success";
    }

}

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
package com.ilscipio.scipio.marketing.event;

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
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.entity.util.EntityUtil;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://marketing/script/org/ofbiz/sfa/lead/LeadServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class LeadServices {

    private static final String MODULE = LeadServices.class.getName();


    /**
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createLead(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> updatePartyStatusCtx = null;
        Map<String, Object> partyRelationshipCtx = null;
        Object leadContactPartyId = null;
        Object partyId = null;
        Map<String, Object> partyDataSourceCtx = null;
        Object partyGroupPartyId = null;
        Map<String, Object> createPartyRoleCtx = null;
        if (((UtilValidate.isEmpty(context.get("firstName")) || UtilValidate.isEmpty(context.get("lastName"))) && UtilValidate.isEmpty(context.get("groupName")))) {
            {
                String errorMsg = UtilProperties.getMessage("MarketingUiLabels", "SfaFirstNameLastNameAndCompanyNameMissingError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> ensurePartyRoleCtx = new HashMap<>();
        ensurePartyRoleCtx.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        ensurePartyRoleCtx.put("roleTypeId", "OWNER");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("ensurePartyRole", ensurePartyRoleCtx);
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
        Object parameters_roleTypeId = null;
        Object partyRelationshipCtx_partyIdFrom = null;
        Object partyRelationshipCtx_partyIdTo = null;
        Object partyRelationshipCtx_roleTypeIdFrom = null;
        Object partyRelationshipCtx_roleTypeIdTo = null;
        Object partyRelationshipCtx_partyRelationshipTypeId = null;
        Object updatePartyStatusCtx_partyId = null;
        Object updatePartyStatusCtx_statusId = null;
        if ((!(UtilValidate.isEmpty(context.get("firstName"))) && !(UtilValidate.isEmpty(context.get("lastName"))))) {
            context.put("roleTypeId", "LEAD");
            // TODO: Call simple-method "createPersonRoleAndContactMechs" from "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml"
            // Original: call-simple-method method-name="createPersonRoleAndContactMechs" xml-resource="component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml"
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            leadContactPartyId = partyId;
            partyId = null;
            partyRelationshipCtx.put("partyIdFrom", ((Map<String, Object>) userLogin).get("partyId"));
            partyRelationshipCtx.put("partyIdTo", leadContactPartyId);
            partyRelationshipCtx.put("roleTypeIdFrom", "OWNER");
            partyRelationshipCtx.put("roleTypeIdTo", "LEAD");
            partyRelationshipCtx.put("partyRelationshipTypeId", "LEAD_OWNER");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyRelationship", partyRelationshipCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyRelationship: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            updatePartyStatusCtx.put("partyId", leadContactPartyId);
            updatePartyStatusCtx.put("statusId", "LEAD_ASSIGNED");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("setPartyStatus", updatePartyStatusCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling setPartyStatus: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(context.get("groupName"))) {
            context.put("partyTypeId", "PARTY_GROUP");
            if (UtilValidate.isEmpty(leadContactPartyId)) {
                context.put("roleTypeId", "ACCOUNT_LEAD");
                // TODO: Call simple-method "createPartyGroupRoleAndContactMechs" from "component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml"
                // Original: call-simple-method method-name="createPartyGroupRoleAndContactMechs" xml-resource="component://party/script/org/ofbiz/party/party/PartySimpleMethods.xml"
                partyGroupPartyId = partyId;
                partyId = null;
                updatePartyStatusCtx.put("partyId", partyGroupPartyId);
                updatePartyStatusCtx.put("statusId", "LEAD_ASSIGNED");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("setPartyStatus", updatePartyStatusCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling setPartyStatus: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                // TODO: Convert call-map-processor (in-map: parameters, out-map: partyGroupCtx)
                Map<String, Object> partyGroupCtx = new HashMap<>(context);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyGroup", partyGroupCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    partyGroupPartyId = serviceResult.get("partyId");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyGroup: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                createPartyRoleCtx.put("partyId", partyGroupPartyId);
                createPartyRoleCtx.put("roleTypeId", "ACCOUNT_LEAD");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyRole", createPartyRoleCtx);
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
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            if (UtilValidate.isNotEmpty(leadContactPartyId)) {
                partyRelationshipCtx.put("partyIdFrom", partyGroupPartyId);
                partyRelationshipCtx.put("partyIdTo", leadContactPartyId);
                partyRelationshipCtx.put("roleTypeIdFrom", "ACCOUNT_LEAD");
                partyRelationshipCtx.put("roleTypeIdTo", "LEAD");
                partyRelationshipCtx.put("positionTitle", context.get("title"));
                partyRelationshipCtx.put("partyRelationshipTypeId", "EMPLOYMENT");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyRelationship", partyRelationshipCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyRelationship: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
            partyRelationshipCtx.put("partyIdFrom", ((Map<String, Object>) userLogin).get("partyId"));
            partyRelationshipCtx.put("partyIdTo", partyGroupPartyId);
            partyRelationshipCtx.put("roleTypeIdFrom", "OWNER");
            partyRelationshipCtx.put("roleTypeIdTo", "ACCOUNT_LEAD");
            partyRelationshipCtx.put("partyRelationshipTypeId", "LEAD_OWNER");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyRelationship", partyRelationshipCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyRelationship: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(context.get("dataSourceId"))) {
                partyDataSourceCtx.put("partyId", partyGroupPartyId);
                partyDataSourceCtx.put("dataSourceId", context.get("dataSourceId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyDataSource", partyDataSourceCtx);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createPartyDataSource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        result.put("partyId", leadContactPartyId);
        result.put("partyGroupPartyId", partyGroupPartyId);
        result.put("roleTypeId", context.get("roleTypeId"));
        String successMessage = UtilProperties.getMessage("MarketingUiLabels", "SfaLeadCreatedSuccessfully", locale);

        return "success";
    }


    /**
     * Convert a lead person into a contact and associated lead group to an account
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String convertLeadToContact(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> deletePartyRelationship = null;
        Object partyRelationship = null;
        Object partyId = context.get("partyId");
        Object partyGroupId = context.get("partyGroupId");
        Timestamp nowTimestamp = new Timestamp(System.currentTimeMillis());
        List<GenericValue> partyRelationships = null;
        try {
            partyRelationships = EntityQuery.use(delegator)
                    .from("PartyRelationship")
                    .where(UtilMisc.toMap("partyIdTo", partyId, "roleTypeIdFrom", "OWNER", "roleTypeIdTo", "LEAD", "partyRelationshipTypeId", "LEAD_OWNER"))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRelationship: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        partyRelationship = EntityUtil.getFirst((List<GenericValue>) partyRelationships);
        if (UtilValidate.isNotEmpty(partyRelationship)) {
            // set-service-fields from "partyRelationship" to "deletePartyRelationship" for service "updatePartyRelationship"
            deletePartyRelationship.putAll(UtilMisc.toMap(partyRelationship));
            deletePartyRelationship.put("thruDate", nowTimestamp);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePartyRelationship", deletePartyRelationship);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updatePartyRelationship: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("Expiring relationship  " + deletePartyRelationship, MODULE);
            deletePartyRelationship = null;
            partyRelationship = null;
        }
        try {
            partyRelationships = EntityQuery.use(delegator)
                    .from("PartyRelationship")
                    .where(UtilMisc.toMap("partyIdFrom", partyGroupId, "roleTypeIdTo", "LEAD", "roleTypeIdFrom", "ACCOUNT_LEAD", "partyRelationshipTypeId", "EMPLOYMENT"))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRelationship: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        partyRelationship = EntityUtil.getFirst((List<GenericValue>) partyRelationships);
        if (UtilValidate.isNotEmpty(partyRelationship)) {
            // set-service-fields from "partyRelationship" to "deletePartyRelationship" for service "updatePartyRelationship"
            deletePartyRelationship.putAll(UtilMisc.toMap(partyRelationship));
            deletePartyRelationship.put("thruDate", nowTimestamp);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePartyRelationship", deletePartyRelationship);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updatePartyRelationship: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            deletePartyRelationship = null;
            partyRelationship = null;
        }
        try {
            partyRelationships = EntityQuery.use(delegator)
                    .from("PartyRelationship")
                    .where(UtilMisc.toMap("partyIdFrom", ((Map<String, Object>) userLogin).get("partyId"), "partyIdTo", partyGroupId, "roleTypeIdTo", "ACCOUNT_LEAD", "roleTypeIdFrom", "OWNER"))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRelationship: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        partyRelationship = EntityUtil.getFirst((List<GenericValue>) partyRelationships);
        if (UtilValidate.isNotEmpty(partyRelationship)) {
            // set-service-fields from "partyRelationship" to "deletePartyRelationship" for service "updatePartyRelationship"
            deletePartyRelationship.putAll(UtilMisc.toMap(partyRelationship));
            deletePartyRelationship.put("thruDate", nowTimestamp);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePartyRelationship", deletePartyRelationship);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updatePartyRelationship: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            deletePartyRelationship = null;
            partyRelationship = null;
        }
        Map<String, Object> partyRoleCtx = new HashMap<>();
        partyRoleCtx.put("partyId", partyGroupId);
        partyRoleCtx.put("roleTypeId", "ACCOUNT");
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
        Map<String, Object> partyRelationshipCtx = new HashMap<>();
        partyRelationshipCtx.put("partyIdFrom", ((Map<String, Object>) userLogin).get("partyId"));
        partyRelationshipCtx.put("partyIdTo", partyGroupId);
        partyRelationshipCtx.put("roleTypeIdFrom", "OWNER");
        partyRelationshipCtx.put("roleTypeIdTo", "ACCOUNT");
        partyRelationshipCtx.put("partyRelationshipTypeId", "ACCOUNT");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyRelationship", partyRelationshipCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyRelationship: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        partyRelationshipCtx = null;
        Map<String, Object> updatePartyCtx = new HashMap<>();
        updatePartyCtx.put("partyId", partyGroupId);
        updatePartyCtx.put("statusId", "LEAD_CONVERTED");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("setPartyStatus", updatePartyCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling setPartyStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createPartyRoleCtx = new HashMap<>();
        createPartyRoleCtx.put("partyId", partyId);
        createPartyRoleCtx.put("roleTypeId", "CONTACT");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyRole", createPartyRoleCtx);
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
        partyRelationshipCtx.put("partyIdFrom", partyGroupId);
        partyRelationshipCtx.put("roleTypeIdFrom", "ACCOUNT");
        partyRelationshipCtx.put("partyIdTo", partyId);
        partyRelationshipCtx.put("roleTypeIdTo", "CONTACT");
        partyRelationshipCtx.put("partyRelationshipTypeId", "EMPLOYMENT");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyRelationship", partyRelationshipCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyRelationship: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        partyRelationshipCtx = null;
        updatePartyCtx.put("partyId", partyId);
        updatePartyCtx.put("statusId", "LEAD_CONVERTED");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("setPartyStatus", updatePartyCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling setPartyStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("partyId", partyId);
        result.put("partyGroupId", partyGroupId);
        Object successMessage = "Lead " + partyGroupId + " " + partyId + "  succesfully converted to Account/Contact";

        return "success";
    }

}

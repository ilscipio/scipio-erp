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
import org.ofbiz.base.util.GroovyUtil;
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
 * <p>Generated from: component://marketing/script/org/ofbiz/marketing/contact/ContactListServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ContactListServices {

    private static final String MODULE = ContactListServices.class.getName();


    /**
     * Create an ContactList
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContactList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue newEntity = null;
        newEntity = delegator.makeValue("ContactList");
        if (UtilValidate.isEmpty(context.get("contactListId"))) {
            ((GenericValue) newEntity).put("contactListId", delegator.getNextSeqId("ContactList"));
        } else {
            newEntity.put("contactListId", context.get("contactListId"));
        }
        result.put("contactListId", ((Map<String, Object>) newEntity).get("contactListId"));
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
     * inlineCheckContactListMechType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String inlineCheckContactListMechType(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue preferredContactMech = null;
        GenericValue listContactMechType = null;
        GenericValue preferredContactMechType = null;
        if (UtilValidate.isNotEmpty(context.get("preferredContactMechId"))) {
            try {
                preferredContactMech = EntityQuery.use(delegator)
                        .from("ContactMech")
                        .where(UtilMisc.toMap("contactMechId", context.get("preferredContactMechId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Object contactList = null;
            if (!java.util.Objects.equals(((Map<String, Object>) preferredContactMech).get("contactMechTypeId"), ((Map<String, Object>) contactList).get("contactMechTypeId"))) {
                try {
                    preferredContactMechType = EntityQuery.use(delegator)
                            .from("ContactMechType")
                            .where(UtilMisc.toMap("contactMechTypeId", ((Map<String, Object>) preferredContactMech).get("contactMechTypeId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ContactMechType: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                try {
                    listContactMechType = EntityQuery.use(delegator)
                            .from("ContactMechType")
                            .where(UtilMisc.toMap("contactMechTypeId", ((Map<String, Object>) contactList).get("contactMechTypeId")))
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ContactMechType: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                {
                    String errorMsg = UtilProperties.getMessage("MarketingUiLabels.xml", "MarketingContactMechNotRightForContactList", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
        }

        return "success";
    }


    /**
     * inlineCheckContactListMechType
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String inlineCheckContactListStatusParameter(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object contactList = null;
        Object parameters_statusId = null;
        if ((!"EMAIL_ADDRESS".equals(((Map<String, Object>) contactList).get("contactMechTypeId")) && "CLPT_PENDING".equals(context.get("statusId")))) {
            context.put("statusId", "CLPT_ACCEPTED");
        }

        return "success";
    }


    /**
     * Add Party To ContactList
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContactListParty(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Map<String, Object> partyEmail = null;
        if ((!java.util.Objects.equals(((Map<String, Object>) userLogin).get("partyId"), context.get("partyId")) && !(true /* TODO: if-has-permission */))) {
            {
                String errorMsg = UtilProperties.getMessage("MarketingUiLabels.xml", "MarketingCreatePermissionError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        GenericValue contactList = null;
        try {
            contactList = EntityQuery.use(delegator)
                    .from("ContactList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        String result = inlineCheckContactListMechType(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        result = inlineCheckContactListStatusParameter(request, response);
        if (!"success".equals(result)) {
            return result;
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("preferredContactMechId"))) {
            if (UtilValidate.isEmpty(context.get("contactMechId"))) {
                partyEmail.put("partyId", context.get("partyId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("getPartyEmail", partyEmail);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    context.put("preferredContactMechId", serviceResult.get("contactMechId"));
                } catch (Exception e) {
                    Debug.logError(e, "Error calling getPartyEmail: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        GenericValue newEntity = delegator.makeValue("ContactListParty");
        newEntity.setNonPKFields((Map<String, Object>) context);
        newEntity.setPKFields((Map<String, Object>) context);
        if (UtilValidate.isEmpty(((Map<String, Object>) newEntity).get("fromDate"))) {
            Timestamp newEntity_fromDate = new Timestamp(System.currentTimeMillis());
        }
        try {
            delegator.create(newEntity);
        } catch (Exception e) {
            Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createContactListPartyStatusMap = new HashMap<>();
        // set-service-fields from "newEntity" to "createContactListPartyStatusMap" for service "createContactListPartyStatus"
        createContactListPartyStatusMap.putAll(UtilMisc.toMap(newEntity));
        createContactListPartyStatusMap.put("baseLocation", context.get("baseLocation"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContactListPartyStatus", createContactListPartyStatusMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContactListPartyStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update Add Party To ContactList
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContactListParty(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        List<GenericValue> lookupList = null;
        GenericValue lastRecord = null;
        Map<String, Object> createContactListPartyStatusMap = null;
        if ((!java.util.Objects.equals(((Map<String, Object>) userLogin).get("partyId"), context.get("partyId")) && !(true /* TODO: if-has-permission */))) {
            {
                String errorMsg = UtilProperties.getMessage("MarketingUiLabels.xml", "MarketingUpdatePermissionError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            try {
                lookupList = EntityQuery.use(delegator)
                        .from("ContactListPartyStatus")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContactListPartyStatus: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            lastRecord = EntityUtil.getFirst((List<GenericValue>) lookupList);
            context.put("fromDate", ((Map<String, Object>) lastRecord).get("fromDate"));
        }
        GenericValue contactList = null;
        try {
            contactList = EntityQuery.use(delegator)
                    .from("ContactList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        String inlineResult = inlineCheckContactListMechType(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        inlineResult = inlineCheckContactListStatusParameter(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContactListParty")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactListParty: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(context.get("statusId"))) {
            if (!java.util.Objects.equals(context.get("statusId"), ((Map<String, Object>) lookedUpValue).get("statusId"))) {
                // set-service-fields from "parameters" to "createContactListPartyStatusMap" for service "createContactListPartyStatus"
                createContactListPartyStatusMap.putAll(UtilMisc.toMap(context));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createContactListPartyStatus", createContactListPartyStatusMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createContactListPartyStatus: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        lookedUpValue.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(lookedUpValue);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("productStoreId", context.get("productStoreId"));
        result.put("contactListId", context.get("contactListId"));

        return "success";
    }


    /**
     * Update Add Party To ContactList No User Login
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContactListPartyNoUserLogin(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Map<String, Object> updateContactListPartyMap = null;
        GenericValue systemUserLogin = null;
        GenericValue contactList = null;
        try {
            contactList = EntityQuery.use(delegator)
                    .from("ContactList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> contactListPartyStatusList = null;
        try {
            contactListPartyStatusList = EntityQuery.use(delegator)
                    .from("ContactListPartyStatus")
                    .where(UtilMisc.toMap("contactListId", context.get("contactListId"), "partyId", context.get("partyId"), "optInVerifyCode", context.get("optInVerifyCode"), "fromDate", context.get("fromDate")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactListPartyStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(contactListPartyStatusList)) {
            // set-service-fields from "parameters" to "updateContactListPartyMap" for service "updateContactListParty"
            updateContactListPartyMap.putAll(UtilMisc.toMap(context));
            try {
                systemUserLogin = EntityQuery.use(delegator)
                        .from("UserLogin")
                        .where(UtilMisc.toMap("userLoginId", "system"))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            updateContactListPartyMap.put("userLogin", systemUserLogin);
            updateContactListPartyMap.put("baseLocation", context.get("baseLocation"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateContactListParty", updateContactListPartyMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateContactListParty: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            error_list.add("Invalid verify code for the ${contactList.contactListName}");
            request.setAttribute("_ERROR_MESSAGE_", "Invalid verify code for the ${contactList.contactListName}");
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }

        return "success";
    }


    /**
     * Unsubscribe for contact list
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String unsubscribeContactListParty(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        List<GenericValue> emptyField = null;
        List<GenericValue> partyContactWithPurposes = null;
        Map<String, Object> updateContactListPartyMap = null;
        boolean isEmail = false;
        try {
            isEmail = UtilValidate.isEmail((String) context.get("email"));
        } catch (Exception e) {
            Debug.logError(e, "Error calling UtilValidate.isEmail: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (!isEmail) {
            {
                String errorMsg = UtilProperties.getMessage("MarketingUiLabels", "MarketingCampaignInvalidEmailInput", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }
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
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", "_NA_");
        }
        try {
            partyContactWithPurposes = EntityQuery.use(delegator)
                    .from("PartyContactWithPurpose")
                    .where(UtilMisc.toMap("partyId", context.get("partyId"), "infoString", context.get("email"), "contactMechTypeId", "EMAIL_ADDRESS", "contactMechPurposeTypeId", "OTHER_EMAIL"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyContactWithPurpose: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        emptyField = EntityUtil.filterByDate((List<GenericValue>) partyContactWithPurposes);
        emptyField = EntityUtil.filterByDate((List<GenericValue>) partyContactWithPurposes);
        if (UtilValidate.isNotEmpty(partyContactWithPurposes)) {
            updateContactListPartyMap.put("contactListId", context.get("contactListId"));
            updateContactListPartyMap.put("partyId", context.get("partyId"));
            updateContactListPartyMap.put("preferredContactMechId", ((GenericValue) ((List<?>) partyContactWithPurposes).get(0)).get("contactMechId"));
            updateContactListPartyMap.put("statusId", "CLPT_UNSUBS_PENDING");
            updateContactListPartyMap.put("userLogin", userLogin);
            updateContactListPartyMap.put("baseLocation", context.get("baseLocation"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateContactListParty", updateContactListPartyMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateContactListParty: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            try {
                partyContactWithPurposes = EntityQuery.use(delegator)
                        .from("PartyContactWithPurpose")
                        .where(UtilMisc.toMap("infoString", context.get("email"), "contactMechTypeId", "EMAIL_ADDRESS"))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyContactWithPurpose: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            emptyField = EntityUtil.filterByDate((List<GenericValue>) partyContactWithPurposes);
            emptyField = EntityUtil.filterByDate((List<GenericValue>) partyContactWithPurposes);
            if (UtilValidate.isNotEmpty(partyContactWithPurposes)) {
                updateContactListPartyMap.put("contactListId", context.get("contactListId"));
                updateContactListPartyMap.put("partyId", ((GenericValue) ((List<?>) partyContactWithPurposes).get(0)).get("partyId"));
                updateContactListPartyMap.put("preferredContactMechId", ((GenericValue) ((List<?>) partyContactWithPurposes).get(0)).get("contactMechId"));
                updateContactListPartyMap.put("statusId", "CLPT_UNSUBS_PENDING");
                updateContactListPartyMap.put("userLogin", userLogin);
                updateContactListPartyMap.put("baseLocation", context.get("baseLocation"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateContactListParty", updateContactListPartyMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateContactListParty: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                error_list.add("The email address (${parameters.email}) does not have the Other Email Address as contact purpose.");
                request.setAttribute("_ERROR_MESSAGE_", "The email address (${parameters.email}) does not have the Other Email Address as contact purpose.");
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Find email by contactMechId then call unsubscribeContactListParty service
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String unsubscribeContactListPartyContachMech(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> partyContactWithPurposes = null;
        try {
            partyContactWithPurposes = EntityQuery.use(delegator)
                    .from("PartyContactWithPurpose")
                    .where(UtilMisc.toMap("contactMechId", context.get("preferredContactMechId"), "partyId", context.get("partyId"), "contactMechTypeId", "EMAIL_ADDRESS"))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyContactWithPurpose: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue partyContactWithPurpose = EntityUtil.getFirst((List<GenericValue>) partyContactWithPurposes);
        Object email = ((Map<String, Object>) partyContactWithPurpose).get("infoString");
        Map<String, Object> unsubscribeContactListPartyCtx = new HashMap<>();
        // set-service-fields from "parameters" to "unsubscribeContactListPartyCtx" for service "unsubscribeContactListParty"
        unsubscribeContactListPartyCtx.putAll(UtilMisc.toMap(context));
        unsubscribeContactListPartyCtx.put("email", email);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("unsubscribeContactListParty", unsubscribeContactListPartyCtx);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling unsubscribeContactListParty: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Update ContactList Party Contact Mech
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyEmailContactListParty(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> lookupMap = new HashMap<>();
        lookupMap.put("preferredContactMechId", context.get("oldContactMechId"));
        // TODO: Convert <find-by-and> element
        if (context.get("ContactListParties") != null) {
            for (Object contactlistparty : (List<Object>) context.get("ContactListParties")) {
                ((Map<String, Object>) contactlistparty).put("preferredContactMechId", context.get("contactMechId"));
                Debug.logInfo("Replacing preferredContactMechId: " + context.get("oldContactMechId") + " of the ContactList: " + ((Map<String, Object>) contactlistparty).get("contactListId") + " with new preferredContactMechId: " + context.get("contactMechId"), MODULE);
                try {
                    delegator.store((GenericValue) contactlistparty);
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
     * Create ContactListParty Status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContactListPartyStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue lastContactListPartyStatus = null;
        List<GenericValue> lastContactListPartyStatusList = null;
        if ((!java.util.Objects.equals(((Map<String, Object>) userLogin).get("partyId"), context.get("partyId")) && !(true /* TODO: if-has-permission */) && !(true /* TODO: if-has-permission */))) {
            {
                String errorMsg = UtilProperties.getMessage("MarketingUiLabels.xml", "MarketingCreatePermissionError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue newEntity = delegator.makeValue("ContactListPartyStatus");
        newEntity.setPKFields((Map<String, Object>) context);
        newEntity.setNonPKFields((Map<String, Object>) context);
        Timestamp newEntity_statusDate = new Timestamp(System.currentTimeMillis());
        newEntity.put("setByUserLoginId", ((Map<String, Object>) userLogin).get("userLoginId"));
        if ("CLPT_PENDING".equals(((Map<String, Object>) newEntity).get("statusId"))) {
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
                Object scriptResult = GroovyUtil.eval("newEntity.set(\"optInVerifyCode\", Long.toString(Math.round(9999999999L * Math.random())))", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
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
     * Send ContactListParty Verify Email
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sendContactListPartyVerifyEmail(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Map<String, Object> sendMailFromScreenMap = null;
        GenericValue contactListPartyStatus = null;
        List<GenericValue> lastContactListPartyStatusList = null;
        if ((!java.util.Objects.equals(((Map<String, Object>) userLogin).get("partyId"), context.get("partyId")) && !(true /* TODO: if-has-permission */))) {
            {
                String errorMsg = UtilProperties.getMessage("MarketingUiLabels.xml", "MarketingViewPermissionError", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue contactList = null;
        try {
            contactList = EntityQuery.use(delegator)
                    .from("ContactList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue contactListParty = null;
        try {
            contactListParty = EntityQuery.use(delegator)
                    .from("ContactListParty")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactListParty: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue preferredContactMech = null;
        try {
            preferredContactMech = contactListParty.getRelatedOne("PreferredContactMech", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one PreferredContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object sendMailFromScreenMap_sendTo = null;
        Object sendMailFromScreenMap_sendFrom = null;
        Object sendMailFromScreenMap_subject = null;
        Object sendMailFromScreenMap_bodyScreenUri = null;
        Object sendMailFromScreenMap_webSiteId = null;
        Object sendMailFromScreenMap_contentType = null;
        Object sendMailFromScreenMap_bodyParameters_contactList = null;
        Object sendMailFromScreenMap_bodyParameters_contactListParty = null;
        Object sendMailFromScreenMap_bodyParameters_contactListPartyStatus = null;
        Object sendMailFromScreenMap_bodyParameters_baseLocation = null;
        if ((!(UtilValidate.isEmpty(((Map<String, Object>) contactList).get("verifyEmailFrom"))) && !(UtilValidate.isEmpty(((Map<String, Object>) contactList).get("verifyEmailSubject"))) && !(UtilValidate.isEmpty(((Map<String, Object>) contactList).get("verifyEmailScreen"))) && !(UtilValidate.isEmpty(((Map<String, Object>) contactList).get("verifyEmailWebSiteId"))))) {
            try {
                lastContactListPartyStatusList = EntityQuery.use(delegator)
                        .from("ContactListPartyStatus")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContactListPartyStatus: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            contactListPartyStatus = EntityUtil.getFirst((List<GenericValue>) lastContactListPartyStatusList);
            sendMailFromScreenMap.put("sendTo", ((Map<String, Object>) preferredContactMech).get("infoString"));
            sendMailFromScreenMap.put("sendFrom", ((Map<String, Object>) contactList).get("verifyEmailFrom"));
            sendMailFromScreenMap.put("subject", ((Map<String, Object>) contactList).get("verifyEmailSubject"));
            sendMailFromScreenMap.put("bodyScreenUri", ((Map<String, Object>) contactList).get("verifyEmailScreen"));
            sendMailFromScreenMap.put("webSiteId", ((Map<String, Object>) contactList).get("verifyEmailWebSiteId"));
            sendMailFromScreenMap.put("contentType", "text/html");
            sendMailFromScreenMap.put("bodyParameters.contactList", contactList);
            sendMailFromScreenMap.put("bodyParameters.contactListParty", contactListParty);
            sendMailFromScreenMap.put("bodyParameters.contactListPartyStatus", contactListPartyStatus);
            sendMailFromScreenMap.put("bodyParameters.baseLocation", context.get("baseLocation"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("sendMailFromScreen", sendMailFromScreenMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling sendMailFromScreen: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            Debug.logWarning("WARNING: Not sending subscription verify email because verifyEmail* settings are missing on the ContactList record: " + contactList, MODULE);
        }

        return "success";
    }


    /**
     * Add WebSite ContactList
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createWebSiteContactList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue webSiteContactList = null;
        try {
            webSiteContactList = EntityQuery.use(delegator)
                    .from("WebSiteContactList")
                    .where(UtilMisc.toMap("webSiteId", context.get("webSiteId"), "contactListId", context.get("contactListId"), "fromDate", context.get("fromDate")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WebSiteContactList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(webSiteContactList)) {
            webSiteContactList = delegator.makeValue("WebSiteContactList");
            webSiteContactList.setPKFields((Map<String, Object>) context);
            webSiteContactList.setNonPKFields((Map<String, Object>) context);
            try {
                delegator.create(webSiteContactList);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            return "success";
        }
        Object message = "This webSiteContactList (webSiteId[" + context.get("webSiteId") + "], contactListId[" + context.get("contactListId") + "]) already exists.";
        result.put("errorMessage", message);

        return "success";
    }


    /**
     * Update WebSite ContactList
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateWebSiteContactList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> webSiteContactList = null;
        try {
            webSiteContactList = EntityQuery.use(delegator)
                    .from("WebSiteContactList")
                    .where(UtilMisc.toMap("webSiteId", context.get("webSiteId"), "contactListId", context.get("contactListId"), "fromDate", context.get("fromDate")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WebSiteContactList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue entryWebSiteContactList = EntityUtil.getFirst((List<GenericValue>) webSiteContactList);
        entryWebSiteContactList.setPKFields((Map<String, Object>) context);
        entryWebSiteContactList.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(entryWebSiteContactList);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Delete WebSite ContactList
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteWebSiteContactList(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> webSiteContactList = null;
        try {
            webSiteContactList = EntityQuery.use(delegator)
                    .from("WebSiteContactList")
                    .where(UtilMisc.toMap("webSiteId", context.get("webSiteId"), "contactListId", context.get("contactListId"), "fromDate", context.get("fromDate")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying WebSiteContactList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // TODO: Convert <remove-list> element

        return "success";
    }


    /**
     * Contact List Opt Out From Communication Event
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String optOutOfListFromCommEvent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<GenericValue> emptyField = null;
        List<GenericValue> contactListPartyList = null;
        Timestamp nowTimestamp = null;
        GenericValue commEvent = null;
        try {
            commEvent = EntityQuery.use(delegator)
                    .from("CommunicationEvent")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object contactListParty_thruDate = null;
        if ((!(UtilValidate.isEmpty(commEvent)) && !(UtilValidate.isEmpty(((Map<String, Object>) commEvent).get("contactListId"))) && !(UtilValidate.isEmpty(((Map<String, Object>) commEvent).get("partyIdTo"))) && !(UtilValidate.isEmpty(((Map<String, Object>) commEvent).get("contactMechIdTo"))))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            try {
                contactListPartyList = EntityQuery.use(delegator)
                        .from("ContactListParty")
                        .where(UtilMisc.toMap("contactListId", ((Map<String, Object>) commEvent).get("contactListId"), "preferredContactMechId", ((Map<String, Object>) commEvent).get("contactMechIdTo"), "partyId", ((Map<String, Object>) commEvent).get("partyIdTo")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContactListParty: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            emptyField = EntityUtil.filterByDate((List<GenericValue>) contactListPartyList);
            if (contactListPartyList != null) {
                for (GenericValue contactListParty : contactListPartyList) {
                    contactListParty.put("thruDate", nowTimestamp);
                    try {
                        delegator.store(contactListParty);
                    } catch (Exception e) {
                        Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }
        result.put("contactListId", ((Map<String, Object>) commEvent).get("contactListId"));

        return "success";
    }


    /**
     * Update ContactList Communication Status
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContactListCommStatus(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("ContactListCommStatus")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactListCommStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(lookedUpValue)) {
            lookedUpValue = delegator.makeValue("ContactListCommStatus");
            lookedUpValue.setPKFields((Map<String, Object>) context);
            lookedUpValue.setNonPKFields((Map<String, Object>) context);
            lookedUpValue.put("changeByUserLoginId", ((Map<String, Object>) userLogin).get("userLoginId"));
            try {
                delegator.create(lookedUpValue);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            lookedUpValue.setNonPKFields((Map<String, Object>) context);
            lookedUpValue.put("changeByUserLoginId", ((Map<String, Object>) userLogin).get("userLoginId"));
            try {
                delegator.store(lookedUpValue);
            } catch (Exception e) {
                Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Update ContactList Comm Status from CommunicationEvent
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateCommStatusFromCommEvent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> clcs = null;
        Map<String, Object> updateStatusCtx = null;
        GenericValue commEvent = null;
        try {
            commEvent = EntityQuery.use(delegator)
                    .from("CommunicationEvent")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(commEvent)) {
            try {
                clcs = EntityQuery.use(delegator)
                        .from("ContactListCommStatus")
                        .where(UtilMisc.toMap("communicationEventId", ((Map<String, Object>) commEvent).get("parentCommEventId"), "partyId", ((Map<String, Object>) commEvent).get("partyIdTo"), "contactMechId", ((Map<String, Object>) commEvent).get("contactMechIdTo")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContactListCommStatus: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (clcs != null) {
                for (GenericValue commStatus : clcs) {
                    // set-service-fields from "commStatus" to "updateStatusCtx" for service "updateContactListCommStatus"
                    updateStatusCtx.putAll(UtilMisc.toMap(commStatus));
                    updateStatusCtx.put("statusId", context.get("statusId"));
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("updateContactListCommStatus", updateStatusCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling updateContactListCommStatus: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Send contact list party subscribe email
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sendContactListPartySubscribeEmail(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue webSite = null;
        Object productStoreId = null;
        Map<String, Object> bodyParameters = null;
        Map<String, Object> emailParams = null;
        productStoreId = context.get("productStoreId");
        GenericValue contactList = null;
        try {
            contactList = EntityQuery.use(delegator)
                    .from("ContactList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue contactListParty = null;
        try {
            contactListParty = EntityQuery.use(delegator)
                    .from("ContactListParty")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactListParty: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> lastContactListPartyStatusList = null;
        try {
            lastContactListPartyStatusList = EntityQuery.use(delegator)
                    .from("ContactListPartyStatus")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactListPartyStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue contactListPartyStatus = EntityUtil.getFirst((List<GenericValue>) lastContactListPartyStatusList);
        if (UtilValidate.isEmpty(productStoreId)) {
            try {
                webSite = EntityQuery.use(delegator)
                        .from("WebSite")
                        .where(UtilMisc.toMap("webSiteId", ((Map<String, Object>) contactList).get("verifyEmailWebSiteId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WebSite: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            productStoreId = ((Map<String, Object>) webSite).get("productStoreId");
        }
        GenericValue storeEmail = null;
        try {
            storeEmail = EntityQuery.use(delegator)
                    .from("ProductStoreEmailSetting")
                    .where(UtilMisc.toMap("productStoreId", productStoreId, "emailType", "SUB_CONT_LIST_NOTI"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStoreEmailSetting: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue contactMech = null;
        try {
            contactMech = EntityQuery.use(delegator)
                    .from("ContactMech")
                    .where(UtilMisc.toMap("contactMechId", context.get("preferredContactMechId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) storeEmail).get("bodyScreenLocation"))) {
            bodyParameters.put("contactListId", context.get("contactListId"));
            bodyParameters.put("partyId", context.get("partyId"));
            bodyParameters.put("contactList", contactList);
            bodyParameters.put("contactListParty", contactListParty);
            bodyParameters.put("contactListPartyStatus", contactListPartyStatus);
            bodyParameters.put("baseLocation", context.get("baseLocation"));
            emailParams.put("bodyParameters", bodyParameters);
            emailParams.put("userLogin", userLogin);
            emailParams.put("webSiteId", ((Map<String, Object>) contactList).get("verifyEmailWebSiteId"));
            emailParams.put("sendTo", ((Map<String, Object>) contactMech).get("infoString"));
            emailParams.put("sendFrom", ((Map<String, Object>) storeEmail).get("fromAddress"));
            emailParams.put("subject", ((Map<String, Object>) storeEmail).get("subject") + " " + ((Map<String, Object>) contactList).get("contactListName"));
            emailParams.put("bodyScreenUri", ((Map<String, Object>) storeEmail).get("bodyScreenLocation"));
            emailParams.put("sendAs", ((Map<String, Object>) storeEmail).get("sendAs"));
            emailParams.put("contentType", "text/html");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("sendMailFromScreen", emailParams);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling sendMailFromScreen: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Send contact list party unsubscribe verify email
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sendContactListPartyUnSubscribeVerifyEmail(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        GenericValue webSite = null;
        Object productStoreId = null;
        Map<String, Object> bodyParameters = null;
        Map<String, Object> createCommunicationEventInMap = null;
        Map<String, Object> emailParams = null;
        Object communicationEventId = null;
        if ((!(true /* TODO: if-has-permission */))) {
            error_list.add("Security Error: to run sendContactListPartyVerifyEmail you must have the MARKETING_VIEW or MARKETING_ADMIN permissions.");
            request.setAttribute("_ERROR_MESSAGE_", "Security Error: to run sendContactListPartyVerifyEmail you must have the MARKETING_VIEW or MARKETING_ADMIN permissions.");
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        GenericValue contactList = null;
        try {
            contactList = EntityQuery.use(delegator)
                    .from("ContactList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue contactListParty = null;
        try {
            contactListParty = EntityQuery.use(delegator)
                    .from("ContactListParty")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactListParty: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue preferredContactMech = null;
        try {
            preferredContactMech = contactListParty.getRelatedOne("PreferredContactMech", false);
        } catch (Exception e) {
            Debug.logError(e, "Error getting related one PreferredContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> lastContactListPartyStatusList = null;
        try {
            lastContactListPartyStatusList = EntityQuery.use(delegator)
                    .from("ContactListPartyStatus")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactListPartyStatus: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue contactListPartyStatus = EntityUtil.getFirst((List<GenericValue>) lastContactListPartyStatusList);
        productStoreId = context.get("productStoreId");
        try {
            contactList = EntityQuery.use(delegator)
                    .from("ContactList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(productStoreId)) {
            try {
                webSite = EntityQuery.use(delegator)
                        .from("WebSite")
                        .where(UtilMisc.toMap("webSiteId", ((Map<String, Object>) contactList).get("verifyEmailWebSiteId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WebSite: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            productStoreId = ((Map<String, Object>) webSite).get("productStoreId");
        }
        GenericValue storeEmail = null;
        try {
            storeEmail = EntityQuery.use(delegator)
                    .from("ProductStoreEmailSetting")
                    .where(UtilMisc.toMap("productStoreId", productStoreId, "emailType", "UNSUB_CONT_LIST_VERI"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStoreEmailSetting: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue contactMech = null;
        try {
            contactMech = EntityQuery.use(delegator)
                    .from("ContactMech")
                    .where(UtilMisc.toMap("contactMechId", context.get("preferredContactMechId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) storeEmail).get("bodyScreenLocation"))) {
            createCommunicationEventInMap.put("contactListId", ((Map<String, Object>) contactList).get("contactListId"));
            createCommunicationEventInMap.put("partyIdTo", ((Map<String, Object>) contactListParty).get("partyId"));
            createCommunicationEventInMap.put("contactMechIdTo", ((Map<String, Object>) contactListParty).get("preferredContactMechId"));
            createCommunicationEventInMap.put("fromString", ((Map<String, Object>) storeEmail).get("fromAddress"));
            createCommunicationEventInMap.put("toString", ((Map<String, Object>) contactMech).get("infoString"));
            createCommunicationEventInMap.put("userLogin", ((Map<String, Object>) contactMech).get("userLogin"));
            createCommunicationEventInMap.put("subject", ((Map<String, Object>) storeEmail).get("subject") + " " + ((Map<String, Object>) contactList).get("contactListName"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createCommunicationEvent", createCommunicationEventInMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                communicationEventId = serviceResult.get("communicationEventId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createCommunicationEvent: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            bodyParameters.put("contactListId", context.get("contactListId"));
            bodyParameters.put("partyId", context.get("partyId"));
            bodyParameters.put("contactList", contactList);
            bodyParameters.put("contactListParty", contactListParty);
            bodyParameters.put("contactListPartyStatus", contactListPartyStatus);
            bodyParameters.put("baseLocation", context.get("baseLocation"));
            emailParams.put("webSiteId", ((Map<String, Object>) contactList).get("verifyEmailWebSiteId"));
            emailParams.put("bodyParameters", bodyParameters);
            emailParams.put("communicationEventId", communicationEventId);
            emailParams.put("userLogin", userLogin);
            emailParams.put("sendTo", ((Map<String, Object>) contactMech).get("infoString"));
            emailParams.put("sendFrom", ((Map<String, Object>) storeEmail).get("fromAddress"));
            emailParams.put("subject", ((Map<String, Object>) storeEmail).get("subject") + " " + ((Map<String, Object>) contactList).get("contactListName"));
            emailParams.put("bodyScreenUri", ((Map<String, Object>) storeEmail).get("bodyScreenLocation"));
            emailParams.put("sendAs", ((Map<String, Object>) storeEmail).get("sendAs"));
            emailParams.put("contentType", "text/html");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("sendMailFromScreen", emailParams);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling sendMailFromScreen: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * Send contact list party unsubscribe email
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sendContactListPartyUnSubscribeEmail(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue webSite = null;
        Object productStoreId = null;
        Map<String, Object> bodyParameters = null;
        Map<String, Object> emailParams = null;
        productStoreId = context.get("productStoreId");
        GenericValue contactList = null;
        try {
            contactList = EntityQuery.use(delegator)
                    .from("ContactList")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactList: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(productStoreId)) {
            try {
                webSite = EntityQuery.use(delegator)
                        .from("WebSite")
                        .where(UtilMisc.toMap("webSiteId", ((Map<String, Object>) contactList).get("verifyEmailWebSiteId")))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying WebSite: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            productStoreId = ((Map<String, Object>) webSite).get("productStoreId");
        }
        GenericValue storeEmail = null;
        try {
            storeEmail = EntityQuery.use(delegator)
                    .from("ProductStoreEmailSetting")
                    .where(UtilMisc.toMap("productStoreId", productStoreId, "emailType", "UNSUB_CONT_LIST_NOTI"))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStoreEmailSetting: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue contactMech = null;
        try {
            contactMech = EntityQuery.use(delegator)
                    .from("ContactMech")
                    .where(UtilMisc.toMap("contactMechId", context.get("preferredContactMechId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContactMech: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) storeEmail).get("bodyScreenLocation"))) {
            bodyParameters.put("contactListId", context.get("contactListId"));
            bodyParameters.put("partyId", context.get("partyId"));
            emailParams.put("webSiteId", ((Map<String, Object>) contactList).get("verifyEmailWebSiteId"));
            emailParams.put("bodyParameters", bodyParameters);
            emailParams.put("userLogin", userLogin);
            emailParams.put("sendTo", ((Map<String, Object>) contactMech).get("infoString"));
            emailParams.put("sendFrom", ((Map<String, Object>) storeEmail).get("fromAddress"));
            emailParams.put("subject", ((Map<String, Object>) storeEmail).get("subject") + " " + ((Map<String, Object>) contactList).get("contactListName"));
            emailParams.put("bodyScreenUri", ((Map<String, Object>) storeEmail).get("bodyScreenLocation"));
            emailParams.put("sendAs", ((Map<String, Object>) storeEmail).get("sendAs"));
            emailParams.put("contentType", "text/html");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("sendMailFromScreen", emailParams);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling sendMailFromScreen: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }

}

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
package com.ilscipio.scipio.party.event;

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
import org.ofbiz.content.data.DataResourceWorker;
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
 * <p>Generated from: component://party/script/org/ofbiz/party/party/PartyServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class PartyServices {

    private static final String MODULE = PartyServices.class.getName();


    /**
     * Save Party Name Change
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String savePartyNameChange(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue person = null;
        GenericValue partyGroup = null;
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        GenericValue partyNameHistory = delegator.makeValue("PartyNameHistory");
        partyNameHistory.setPKFields((Map<String, Object>) context);
        Timestamp partyNameHistory_changeDate = new Timestamp(System.currentTimeMillis());
        if (!(UtilValidate.isEmpty(context.get("groupName")))) {
            try {
                partyGroup = EntityQuery.use(delegator)
                        .from("PartyGroup")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyGroup: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (!java.util.Objects.equals(((Map<String, Object>) partyGroup).get("groupName"), context.get("groupName"))) {
                partyNameHistory.setNonPKFields((Map<String, Object>) partyGroup);
                try {
                    delegator.create(partyNameHistory);
                } catch (Exception e) {
                    Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * Get Party Name For Date
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPartyNameForDate(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Timestamp nowTimestamp = null;
        Object partyNameHistoryCurrent = null;
        Object fullName = null;
        List<GenericValue> partyNameHistoryList = null;
        try {
            partyNameHistoryList = EntityQuery.use(delegator)
                    .from("PartyNameHistory")
                    .where(UtilMisc.toMap("partyId", context.get("partyId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyNameHistory: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue person = null;
        try {
            person = EntityQuery.use(delegator)
                    .from("Person")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Person: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        GenericValue partyGroup = null;
        try {
            partyGroup = EntityQuery.use(delegator)
                    .from("PartyGroup")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyGroup: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("compareDate"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            context.put("compareDate", nowTimestamp);
        }
        if (partyNameHistoryList != null) {
            for (GenericValue partyNameHistory : partyNameHistoryList) {
                if (((Map<String, Object>) partyNameHistory).get("changeDate") != null /* TODO: field compare operator greater */) {
                    partyNameHistoryCurrent = partyNameHistory;
                }
            }
        }
        if (UtilValidate.isEmpty(partyNameHistoryCurrent)) {
            if (UtilValidate.isNotEmpty(person)) {
                result.put("firstName", ((Map<String, Object>) person).get("firstName"));
                if (UtilValidate.isNotEmpty(((Map<String, Object>) person).get("middleName"))) {
                    result.put("middleName", ((Map<String, Object>) person).get("middleName"));
                }
                result.put("lastName", ((Map<String, Object>) person).get("lastName"));
                if (UtilValidate.isNotEmpty(((Map<String, Object>) person).get("personalTitle"))) {
                    result.put("personalTitle", ((Map<String, Object>) person).get("personalTitle"));
                }
                if (UtilValidate.isNotEmpty(((Map<String, Object>) person).get("gender"))) {
                    result.put("gender", ((Map<String, Object>) person).get("gender"));
                }
                if (UtilValidate.isNotEmpty(((Map<String, Object>) person).get("suffix"))) {
                    result.put("suffix", ((Map<String, Object>) person).get("suffix"));
                }
                if ("Y".equals(context.get("lastNameFirst"))) {
                    fullName = ((Map<String, Object>) person).get("personalTitle") + " " + ((Map<String, Object>) person).get("lastName") + ", " + ((Map<String, Object>) person).get("firstName") + " " + ((Map<String, Object>) person).get("middleName") + " " + ((Map<String, Object>) person).get("suffix");
                } else {
                    fullName = ((Map<String, Object>) person).get("personalTitle") + " " + ((Map<String, Object>) person).get("firstName") + " " + ((Map<String, Object>) person).get("middleName") + " " + ((Map<String, Object>) person).get("lastName") + " " + ((Map<String, Object>) person).get("suffix");
                }
                result.put("fullName", fullName);
            } else {
                if (UtilValidate.isNotEmpty(partyGroup)) {
                    result.put("groupName", ((Map<String, Object>) partyGroup).get("groupName"));
                    result.put("fullName", ((Map<String, Object>) partyGroup).get("groupName"));
                }
            }
        } else {
            if (UtilValidate.isNotEmpty(person)) {
                result.put("firstName", ((Map<String, Object>) partyNameHistoryCurrent).get("firstName"));
                if (UtilValidate.isNotEmpty(((Map<String, Object>) partyNameHistoryCurrent).get("middleName"))) {
                    result.put("middleName", ((Map<String, Object>) partyNameHistoryCurrent).get("middleName"));
                }
                result.put("lastName", ((Map<String, Object>) partyNameHistoryCurrent).get("lastName"));
                if (UtilValidate.isNotEmpty(((Map<String, Object>) partyNameHistoryCurrent).get("personalTitle"))) {
                    result.put("personalTitle", ((Map<String, Object>) partyNameHistoryCurrent).get("personalTitle"));
                }
                if (UtilValidate.isNotEmpty(((Map<String, Object>) partyNameHistoryCurrent).get("suffix"))) {
                    result.put("suffix", ((Map<String, Object>) partyNameHistoryCurrent).get("suffix"));
                }
                if (UtilValidate.isNotEmpty(((Map<String, Object>) person).get("gender"))) {
                    result.put("gender", ((Map<String, Object>) person).get("gender"));
                }
                if ("Y".equals(context.get("lastNameFirst"))) {
                    fullName = ((Map<String, Object>) partyNameHistoryCurrent).get("personalTitle") + " " + ((Map<String, Object>) partyNameHistoryCurrent).get("lastName") + ", " + ((Map<String, Object>) partyNameHistoryCurrent).get("firstName") + " " + ((Map<String, Object>) partyNameHistoryCurrent).get("middleName") + " " + ((Map<String, Object>) partyNameHistoryCurrent).get("suffix");
                } else {
                    fullName = ((Map<String, Object>) partyNameHistoryCurrent).get("personalTitle") + " " + ((Map<String, Object>) partyNameHistoryCurrent).get("firstName") + " " + ((Map<String, Object>) partyNameHistoryCurrent).get("middleName") + " " + ((Map<String, Object>) partyNameHistoryCurrent).get("lastName") + " " + ((Map<String, Object>) partyNameHistoryCurrent).get("suffix");
                }
                result.put("fullName", fullName);
            } else {
                if (UtilValidate.isNotEmpty(partyGroup)) {
                    result.put("groupName", ((Map<String, Object>) partyNameHistoryCurrent).get("groupName"));
                    result.put("fullName", ((Map<String, Object>) partyNameHistoryCurrent).get("groupName"));
                }
            }
        }

        return "success";
    }


    /**
     * Get Postal Address Boundary
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPostalAddressBoundary(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        GenericValue geo = null;
        List<Object> geos = null;
        GenericValue postalAddressBoundaryLookupMap = delegator.makeValue("PostalAddressBoundary");
        postalAddressBoundaryLookupMap.put("geoId", context.get("geoId"));
        // TODO: Convert <find-by-and> element
        if (context.get("postalAddressBoundaries") != null) {
            for (Object postalAddressBoundary : (List<Object>) context.get("postalAddressBoundaries")) {
                try {
                    geo = ((GenericValue) postalAddressBoundary).getRelatedOne("Geo", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one Geo: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                geos.add(geo);
            }
        }
        result.put("geos", geos);

        return "success";
    }


    /**
     * create mass party identification with association between value and type
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyIdentifications(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> partyIdentCtx = null;
        GenericValue identificationType = null;
        Object idValue = null;
        partyIdentCtx.put("partyId", context.get("partyId"));
        for (Map.Entry<String, Object> entry : ((Map<String, Object>) context.get("identifications")).entrySet()) {
            String key = entry.getKey();
            Object value = entry.getValue();
            try {
                identificationType = EntityQuery.use(delegator)
                        .from("PartyIdentificationType")
                        .where(UtilMisc.toMap("partyIdentificationTypeId", value))
                        .cache()
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyIdentificationType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(identificationType)) {
                idValue = ((Map<String, Object>) context.get("identifications")).get("${identificationType.partyIdentificationTypeId") + "}";
                if (UtilValidate.isNotEmpty(idValue)) {
                    partyIdentCtx.put("partyIdentificationTypeId", ((Map<String, Object>) identificationType).get("partyIdentificationTypeId"));
                    partyIdentCtx.put("idValue", idValue);
                    try {
                        Map<String, Object> serviceResult = dispatcher.runSync("createPartyIdentification", partyIdentCtx);
                        if (ServiceUtil.isError(serviceResult)) {
                            String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                            request.setAttribute("_ERROR_MESSAGE_", errMsg);
                            return "error";
                        }
                    } catch (Exception e) {
                        Debug.logError(e, "Error calling createPartyIdentification: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Sets Party Profile Defaults
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String setPartyProfileDefaults(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue partyProfileDefault = null;
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
        }
        try {
            partyProfileDefault = EntityQuery.use(delegator)
                    .from("PartyProfileDefault")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyProfileDefault: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(partyProfileDefault)) {
            partyProfileDefault = delegator.makeValue("PartyProfileDefault");
            partyProfileDefault.setPKFields((Map<String, Object>) context);
            try {
                delegator.create(partyProfileDefault);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        partyProfileDefault.setNonPKFields((Map<String, Object>) context);
        try {
            delegator.store(partyProfileDefault);
        } catch (Exception e) {
            Debug.logError(e, "Error storing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * Creates Party Associated Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> lookup = null;
        GenericValue extension = null;
        Boolean absolute = null;
        Object uploadPath = null;
        Object dataResourceId = null;
        GenericValue dataResourceMap = null;
        Map<String, Object> extenLookup = null;
        Map<String, Object> dataResource = null;
        Map<String, Object> createContentMap = null;
        Map<String, Object> contentRole = null;
        GenericValue partyRole = null;
        Timestamp nowTimestamp = null;
        Map<String, Object> fileCtx = null;
        if (UtilValidate.isNotEmpty(context.get("partyId"))) {
            if (UtilValidate.isEmpty(context.get("userLogin"))) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyPermissionErrorForThisParty", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            if (UtilValidate.isNotEmpty(context.get("userLogin"))) {
                context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                if (!java.util.Objects.equals(context.get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) {
                    // TODO: Convert <check-permission> element
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("_uploadedFile_fileName"))) {
            absolute = Boolean.TRUE;
            try {
                uploadPath = DataResourceWorker.getDataResourceContentUploadPath(delegator, absolute);
            } catch (Exception e) {
                Debug.logError(e, "Error calling DataResourceWorker.getDataResourceContentUploadPath: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo("[createPartyContent] - Found Subdir : " + uploadPath, MODULE);
            extenLookup.put("mimeTypeId", context.get("_uploadedFile_contentType"));
            // TODO: Convert <find-by-and> element
            extension = EntityUtil.getFirst((List<GenericValue>) context.get("extensions"));
            // set-service-fields from "parameters" to "dataResource" for service "createDataResource"
            dataResource.putAll(UtilMisc.toMap(context));
            dataResource.put("dataResourceName", context.get("_uploadedFile_fileName"));
            dataResource.put("mimeTypeId", context.get("_uploadedFile_contentType"));
            dataResource.put("dataResourceTypeId", "LOCAL_FILE");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createDataResource", dataResource);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                dataResourceId = serviceResult.get("dataResourceId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // set-service-fields from "dataResource" to "dataResource" for service "updateDataResource"
            dataResource.putAll(UtilMisc.toMap(dataResource));
            dataResource.put("dataResourceId", dataResourceId);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updateDataResource", dataResource);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updateDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            lookup.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
            try {
                dataResourceMap = EntityQuery.use(delegator)
                        .from("DataResource")
                        .where(lookup)
                        .cache()
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key DataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        // set-service-fields from "parameters" to "createContentMap" for service "createContent"
        createContentMap.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isNotEmpty(context.get("_uploadedFile_fileName"))) {
            createContentMap.put("dataResourceId", dataResourceId);
        }
        Object contentId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createContent", createContentMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            contentId = serviceResult.get("contentId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(context.get("partyId"))) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
            // set-service-fields from "parameters" to "contentRole" for service "createContentRole"
            contentRole.putAll(UtilMisc.toMap(context));
            contentRole.put("contentId", contentId);
            contentRole.put("partyId", context.get("partyId"));
            contentRole.put("fromDate", nowTimestamp);
            contentRole.put("roleTypeId", "OWNER");
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createContentRole", contentRole);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createContentRole: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            partyRole = delegator.makeValue("PartyRole");
            partyRole.setPKFields((Map<String, Object>) contentRole);
            // TODO: Convert <find-by-and> element
            if (UtilValidate.isEmpty(context.get("pRoles"))) {
                Map<String, Object> partyRoleMap = new HashMap<>();
                // set-service-fields from "contentRole" to "partyRole" for service "createPartyRole"
                partyRoleMap.putAll(UtilMisc.toMap(contentRole));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createPartyRole", partyRoleMap);
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
            }
        }
        if (UtilValidate.isNotEmpty(context.get("_uploadedFile_fileName"))) {
            // set-service-fields from "dataResourceMap" to "fileCtx" for service "createAnonFile"
            fileCtx.putAll(UtilMisc.toMap(dataResourceMap));
            fileCtx.put("binData", context.get("uploadedFile"));
            fileCtx.put("dataResource", dataResourceMap);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createAnonFile", fileCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createAnonFile: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("contentId", contentId);

        return "success";
    }


    /**
     * Creates Party Associated Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Map<String, Object> lookup = null;
        GenericValue extension = null;
        Boolean absolute = null;
        Object uploadPath = null;
        GenericValue dataResourceMap = null;
        Object dataResourceId = null;
        Map<String, Object> lookupParam = null;
        Map<String, Object> extenLookup = null;
        GenericValue content = null;
        Map<String, Object> dataResource = null;
        Map<String, Object> updateContentMap = null;
        Map<String, Object> fileCtx = null;
        if (UtilValidate.isNotEmpty(context.get("partyId"))) {
            if (UtilValidate.isEmpty(context.get("userLogin"))) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyPermissionErrorForThisParty", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            if (UtilValidate.isNotEmpty(context.get("userLogin"))) {
                context.put("partyId", ((Map<String, Object>) userLogin).get("partyId"));
                if (!java.util.Objects.equals(context.get("partyId"), ((Map<String, Object>) userLogin).get("partyId"))) {
                    // TODO: Convert <check-permission> element
                }
            }
        }
        if (UtilValidate.isNotEmpty(context.get("_uploadedFile_fileName"))) {
            lookupParam.put("contentId", context.get("contentId"));
            try {
                content = EntityQuery.use(delegator)
                        .from("Content")
                        .where(lookupParam)
                        .cache()
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error finding by primary key Content: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) content).get("dataResourceId"))) {
                // set-service-fields from "parameters" to "dataResource" for service "updateDataResource"
                dataResource.putAll(UtilMisc.toMap(context));
                dataResource.put("dataResourceId", ((Map<String, Object>) content).get("dataResourceId"));
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateDataResource", dataResource);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateDataResource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                lookup.put("dataResourceId", ((Map<String, Object>) content).get("dataResourceId"));
                try {
                    dataResourceMap = EntityQuery.use(delegator)
                            .from("DataResource")
                            .where(lookup)
                            .cache()
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error finding by primary key DataResource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            } else {
                absolute = Boolean.TRUE;
                try {
                    uploadPath = DataResourceWorker.getDataResourceContentUploadPath(delegator, absolute);
                } catch (Exception e) {
                    Debug.logError(e, "Error calling DataResourceWorker.getDataResourceContentUploadPath: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                Debug.logInfo("[createPartyContent] - Found Subdir : " + uploadPath, MODULE);
                extenLookup.put("mimeTypeId", context.get("_uploadedFile_contentType"));
                // TODO: Convert <find-by-and> element
                extension = EntityUtil.getFirst((List<GenericValue>) context.get("extensions"));
                // set-service-fields from "parameters" to "dataResource" for service "createDataResource"
                dataResource.putAll(UtilMisc.toMap(context));
                dataResource.put("dataResourceName", context.get("_uploadedFile_fileName"));
                dataResource.put("mimeTypeId", context.get("_uploadedFile_contentType"));
                dataResource.put("dataResourceTypeId", "LOCAL_FILE");
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createDataResource", dataResource);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                    dataResourceId = serviceResult.get("dataResourceId");
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createDataResource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                dataResource.put("objectInfo", uploadPath + "/" + dataResourceId);
                if (UtilValidate.isNotEmpty(extension)) {
                    dataResource.put("objectInfo", uploadPath + "/" + dataResourceId + "." + ((Map<String, Object>) extension).get("fileExtensionId"));
                }
                // set-service-fields from "dataResource" to "dataResource" for service "updateDataResource"
                dataResource.putAll(UtilMisc.toMap(dataResource));
                dataResource.put("dataResourceId", dataResourceId);
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("updateDataResource", dataResource);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling updateDataResource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                lookup.put("dataResourceId", ((Map<String, Object>) dataResource).get("dataResourceId"));
                try {
                    dataResourceMap = EntityQuery.use(delegator)
                            .from("DataResource")
                            .where(lookup)
                            .cache()
                            .queryOne();
                } catch (Exception e) {
                    Debug.logError(e, "Error finding by primary key DataResource: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        // set-service-fields from "parameters" to "updateContentMap" for service "updateContent"
        updateContentMap.putAll(UtilMisc.toMap(context));
        if (UtilValidate.isNotEmpty(dataResourceId)) {
            updateContentMap.put("dataResourceId", dataResourceId);
        }
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateContent", updateContentMap);
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
        if (UtilValidate.isNotEmpty(context.get("_uploadedFile_fileName"))) {
            // set-service-fields from "dataResourceMap" to "fileCtx" for service "createAnonFile"
            fileCtx.putAll(UtilMisc.toMap(dataResourceMap));
            fileCtx.put("binData", context.get("uploadedFile"));
            fileCtx.put("dataResource", dataResourceMap);
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createAnonFile", fileCtx);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling createAnonFile: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("contentId", context.get("contentId"));

        return "success";
    }


    /**
     * Gets all parties related to partyIdFrom using the PartyRelationship entity
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPartiesByRelationship(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<Object> parties = null;
        GenericValue party = null;
        Map<String, Object> lookupMap = new HashMap<>();
        lookupMap.put("partyIdFrom", context.get("partyIdFrom"));
        lookupMap.put("partyIdTo", context.get("partyIdTo"));
        lookupMap.put("roleTypeIdFrom", context.get("roleTypeIdFrom"));
        lookupMap.put("roleTypeIdTo", context.get("roleTypeIdTo"));
        lookupMap.put("statusId", context.get("statusId"));
        lookupMap.put("priorityTypeId", context.get("priorityTypeId"));
        lookupMap.put("partyRelationshipTypeId", context.get("partyRelationshipTypeId"));
        // TODO: Convert <find-by-and> element
        if (context.get("partyRelationships") != null) {
            for (Object partyRelationship : (List<Object>) context.get("partyRelationships")) {
                try {
                    party = ((GenericValue) partyRelationship).getRelatedOne("ToParty", false);
                } catch (Exception e) {
                    Debug.logError(e, "Error getting related one ToParty: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
                parties.add(party);
            }
        }
        if (UtilValidate.isNotEmpty(parties)) {
            result.put("parties", parties);
        }

        return "success";
    }


    /**
     * Gets Parent Organizations for an Organization Party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getParentOrganizations(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object recurse = null;
        List<Object> relatedPartyIdList = new LinkedList<>();
        relatedPartyIdList.add(context.get("organizationPartyId"));
        recurse = context.get("getParentsOfParents");
        if (UtilValidate.isEmpty(recurse)) {
            recurse = "Y";
        }
        Object partyRelationshipTypeId = "GROUP_ROLLUP";
        Object roleTypeIdFrom = "ORGANIZATION_UNIT";
        Object roleTypeIdTo = "PARENT_ORGANIZATION";
        Object roleTypeIdFromInclueAllChildTypes = "Y";
        Object includeFromToSwitched = "Y";
        Object useCache = "true";
        followPartyRelationshipsInline(request, response);
        result.put("parentOrganizationPartyIdList", relatedPartyIdList);

        return "success";
    }


    /**
     * Get Parties Related to a Party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getRelatedParties(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<Object> relatedPartyIdList = new LinkedList<>();
        relatedPartyIdList.add(context.get("partyIdFrom"));
        Object partyRelationshipTypeId = context.get("partyRelationshipTypeId");
        Object roleTypeIdFrom = context.get("roleTypeIdFrom");
        Object roleTypeIdFromInclueAllChildTypes = context.get("roleTypeIdFromInclueAllChildTypes");
        Object roleTypeIdTo = context.get("roleTypeIdTo");
        Object roleTypeIdToIncludeAllChildTypes = context.get("roleTypeIdToIncludeAllChildTypes");
        Object includeFromToSwitched = context.get("includeFromToSwitched");
        Object recurse = context.get("recurse");
        Object useCache = context.get("useCache");
        followPartyRelationshipsInline(request, response);
        result.put("relatedPartyIdList", relatedPartyIdList);

        return "success";
    }


    /**
     * followPartyRelationshipsInline
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String followPartyRelationshipsInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Timestamp nowTimestamp = null;
        Object roleTypeIdListName = null;
        List<Object> _inline_roleTypeIdFromList = null;
        List<Object> _inline_roleTypeIdToList = null;
        if (UtilValidate.isEmpty(nowTimestamp)) {
            nowTimestamp = new Timestamp(System.currentTimeMillis());
        }
        if (UtilValidate.isEmpty(_inline_roleTypeIdFromList)) {
            _inline_roleTypeIdFromList.add(context.get("roleTypeIdFrom"));
            if ("Y".equals(context.get("roleTypeIdFromInclueAllChildTypes"))) {
                roleTypeIdListName = "_inline_roleTypeIdFromList";
                getChildRoleTypesInline(request, response);
            }
        }
        if (UtilValidate.isEmpty(_inline_roleTypeIdToList)) {
            _inline_roleTypeIdToList.add(context.get("roleTypeIdTo"));
            if ("Y".equals(context.get("roleTypeIdToInclueAllChildTypes"))) {
                roleTypeIdListName = "_inline_roleTypeIdToList";
                getChildRoleTypesInline(request, response);
            }
        }
        String result = followPartyRelationshipsInlineRecurse(request, response);
        if (!"success".equals(result)) {
            return result;
        }

        return "success";
    }


    /**
     * followPartyRelationshipsInlineRecurse
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String followPartyRelationshipsInlineRecurse(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<Object> _inline_relatedPartyIdAlreadySearchedList = null;
        List<GenericValue> _inline_PartyRelationshipList = null;
        GenericValue _inline_PartyRelationship = null;
        List<Object> _inline_NewRelatedPartyIdList = null;
        _inline_NewRelatedPartyIdList = null;
        if (context.get("relatedPartyIdList") != null) {
            for (Object relatedPartyId : (List<Object>) context.get("relatedPartyIdList")) {
                if (!(_inline_relatedPartyIdAlreadySearchedList != null /* TODO: field compare operator contains */)) {
                    _inline_relatedPartyIdAlreadySearchedList.add(relatedPartyId);
                    _inline_PartyRelationshipList = null;
                    try {
                        _inline_PartyRelationshipList = EntityQuery.use(delegator)
                                .from("PartyRelationship")
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying PartyRelationship: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (_inline_PartyRelationshipList != null) {
                        for (GenericValue _inline_PartyRelationship_iter : _inline_PartyRelationshipList) {
                            _inline_PartyRelationship = _inline_PartyRelationship_iter;
                            if ((!(context.get("relatedPartyIdList") != null /* TODO: field compare operator contains */) && !(_inline_NewRelatedPartyIdList != null /* TODO: field compare operator contains */))) {
                                _inline_NewRelatedPartyIdList.add(((Map<String, Object>) _inline_PartyRelationship).get("partyIdTo"));
                            }
                        }
                    }
                    if ("Y".equals(context.get("includeFromToSwitched"))) {
                        _inline_PartyRelationshipList = null;
                        try {
                            _inline_PartyRelationshipList = EntityQuery.use(delegator)
                                    .from("PartyRelationship")
                                    .queryList();
                        } catch (Exception e) {
                            Debug.logError(e, "Error querying PartyRelationship: " + e.getMessage(), MODULE);
                            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                            return "error";
                        }
                        if (_inline_PartyRelationshipList != null) {
                            for (GenericValue _inline_PartyRelationship_iter : _inline_PartyRelationshipList) {
                                _inline_PartyRelationship = _inline_PartyRelationship_iter;
                                if ((!(context.get("relatedPartyIdList") != null /* TODO: field compare operator contains */) && !(_inline_NewRelatedPartyIdList != null /* TODO: field compare operator contains */))) {
                                    _inline_NewRelatedPartyIdList.add(((Map<String, Object>) _inline_PartyRelationship).get("partyIdFrom"));
                                }
                            }
                        }
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(_inline_NewRelatedPartyIdList)) {
            // TODO: Convert <list-to-list> element
            if ("Y".equals(context.get("recurse"))) {
                Debug.logVerbose("Recursively calling followPartyRelationshipsInlineRecurse _inline_NewRelatedPartyIdList=" + _inline_NewRelatedPartyIdList, MODULE);
                String result = followPartyRelationshipsInlineRecurse(request, response);
                if (!"success".equals(result)) {
                    return result;
                }
            }
        }

        return "success";
    }


    /**
     * Get Child RoleTypes
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getChildRoleTypes(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<Object> childRoleTypeIdList = new LinkedList<>();
        childRoleTypeIdList.add(context.get("roleTypeId"));
        Object roleTypeIdListName = "childRoleTypeIdList";
        getChildRoleTypesInline(request, response);
        result.put("childRoleTypeIdList", childRoleTypeIdList);

        return "success";
    }


    /**
     * getChildRoleTypes
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getChildRoleTypesInline(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> _inline_RoleTypeList = null;
        List<Object> _inline_roleTypeIdAlreadySearchedList = new LinkedList<>();
        List<Object> _inline_NewRoleTypeIdList = new LinkedList<>();
        @SuppressWarnings("unchecked")
        List<Object> roleTypeIdListName = (List<Object>) context.get("roleTypeIdListName");
        if (roleTypeIdListName != null) {
            for (Object roleTypeId : roleTypeIdListName) {
                if (!_inline_roleTypeIdAlreadySearchedList.contains(roleTypeId)) {
                    _inline_roleTypeIdAlreadySearchedList.add(roleTypeId);
                    _inline_RoleTypeList = null;
                    try {
                        _inline_RoleTypeList = EntityQuery.use(delegator)
                                .from("RoleType")
                                .cache()
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying RoleType: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (_inline_RoleTypeList != null) {
                        for (GenericValue newRoleType : _inline_RoleTypeList) {
                            Object newRoleTypeId = newRoleType.get("roleTypeId");
                            if (!roleTypeIdListName.contains(newRoleTypeId) && !_inline_NewRoleTypeIdList.contains(newRoleTypeId)) {
                                _inline_NewRoleTypeIdList.add(newRoleTypeId);
                            }
                        }
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(_inline_NewRoleTypeIdList)) {
            // TODO: Convert <list-to-list> element
            Debug.logVerbose("Recursively calling getChildRoleTypesInline roleTypeIdListName=" + context.get("roleTypeIdListName") + ", _inline_NewRoleTypeIdList=" + _inline_NewRoleTypeIdList, MODULE);
            getChildRoleTypesInline(request, response);
        }

        return "success";
    }


    /**
     * Get the email of the party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPartyEmail(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<GenericValue> emailAddresses = null;
        GenericValue emailAddress = null;
        List<GenericValue> emailAddressesPurposes = null;
        try {
            emailAddressesPurposes = EntityQuery.use(delegator)
                    .from("PartyContactWithPurpose")
                    .where(UtilMisc.toMap("partyId", context.get("partyId"), "contactMechPurposeTypeId", context.get("contactMechPurposeTypeId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyContactWithPurpose: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        List<GenericValue> emailAddressesPurposes1 = EntityUtil.filterByDate((List<GenericValue>) emailAddressesPurposes);
        emailAddresses = EntityUtil.filterByDate((List<GenericValue>) emailAddressesPurposes1);
        if (UtilValidate.isEmpty(emailAddresses)) {
            try {
                emailAddresses = EntityQuery.use(delegator)
                        .from("PartyAndContactMech")
                        .where(UtilMisc.toMap("partyId", context.get("partyId"), "contactMechTypeId", "EMAIL_ADDRESS"))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyAndContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isEmpty(emailAddresses)) {
            try {
                emailAddresses = EntityQuery.use(delegator)
                        .from("PartyAndContactMech")
                        .where(UtilMisc.toMap("partyId", context.get("partyId"), "contactMechTypeId", "ELECTRONIC_ADDRESS"))
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyAndContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(emailAddresses)) {
            emailAddress = EntityUtil.getFirst((List<GenericValue>) emailAddresses);
            result.put("emailAddress", ((Map<String, Object>) emailAddress).get("infoString"));
            result.put("contactMechId", ((Map<String, Object>) emailAddress).get("contactMechId"));
        }

        return "success";
    }


    /**
     * Get the telephone number of the party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPartyTelephone(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<Object> types = null;
        Object found = null;
        List<GenericValue> telephoneAll2 = null;
        GenericValue telephone = null;
        Map<String, Object> findMap = new HashMap<>();
        findMap.put("partyId", context.get("partyId"));
        Object type = null;
        if (UtilValidate.isEmpty(context.get("contactMechPurposeTypeId"))) {
            type = "PRIMARY_PHONE";
            types.add(type);
            type = "PHONE_MOBILE";
            types.add(type);
            type = "PHONE_WORK";
            types.add(type);
            type = "PHONE_QUICK";
            types.add(type);
            type = "PHONE_HOME";
            types.add(type);
            type = "PHONE_BILLING";
            types.add(type);
            type = "PHONE_SHIPPING";
            types.add(type);
            type = "PHONE_SHIP_ORIG";
            types.add(type);
        } else {
            type = context.get("contactMechPurposeTypeId");
            types.add(type);
        }
        findMap.put("contactMechTypeId", "TELECOM_NUMBER");
        // TODO: Convert <find-by-and> element
        telephoneAll2 = EntityUtil.filterByDate((List<GenericValue>) context.get("telephoneAll1"));
        List<GenericValue> telephoneAll3 = EntityUtil.filterByDate((List<GenericValue>) telephoneAll2);
        if (UtilValidate.isNotEmpty(telephoneAll3)) {
            if (types != null) {
                for (Object typeEntry : types) {
                    if (telephoneAll3 != null) {
                        for (Object telephone_iter : (List<?>) telephoneAll3) {
                            telephone = (GenericValue) telephone_iter;
                            if (UtilValidate.isEmpty(found)) {
                                if (java.util.Objects.equals(((Map<String, Object>) telephone).get("contactMechPurposeTypeId"), typeEntry)) {
                                    found = "notImportant";
                                    result.put("contactMechId", ((Map<String, Object>) telephone).get("contactMechId"));
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) telephone).get("countryCode"))) {
                                        result.put("countryCode", ((Map<String, Object>) telephone).get("countryCode"));
                                    }
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) telephone).get("areaCode"))) {
                                        result.put("areaCode", ((Map<String, Object>) telephone).get("areaCode"));
                                    }
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) telephone).get("contactNumber"))) {
                                        result.put("contactNumber", ((Map<String, Object>) telephone).get("contactNumber"));
                                    }
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) telephone).get("extension"))) {
                                        result.put("extension", ((Map<String, Object>) telephone).get("extension"));
                                    }
                                    result.put("contactMechPurposeTypeId", ((Map<String, Object>) telephone).get("contactMechPurposeTypeId"));
                                }
                            }
                        }
                    }
                }
            }
        } else {
            // TODO: Convert <find-by-and> element
            telephoneAll2 = EntityUtil.filterByDate((List<GenericValue>) context.get("telephoneAll1"));
            telephone = EntityUtil.getFirst((List<GenericValue>) telephoneAll2);
            result.put("contactMechId", ((Map<String, Object>) telephone).get("contactMechId"));
            if (UtilValidate.isNotEmpty(((Map<String, Object>) telephone).get("tnCountryCode"))) {
                result.put("countryCode", ((Map<String, Object>) telephone).get("tnCountryCode"));
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) telephone).get("tnAreaCode"))) {
                result.put("areaCode", ((Map<String, Object>) telephone).get("tnAreaCode"));
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) telephone).get("tnContactNumber"))) {
                result.put("contactNumber", ((Map<String, Object>) telephone).get("tnContactNumber"));
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) telephone).get("extension"))) {
                result.put("extension", ((Map<String, Object>) telephone).get("extension"));
            }
        }

        return "success";
    }


    /**
     * Get the postal address of the party
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPartyPostalAddress(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<Object> types = null;
        GenericValue address = null;
        Object found = null;
        List<GenericValue> addressAll2 = null;
        Map<String, Object> findMap = new HashMap<>();
        findMap.put("partyId", context.get("partyId"));
        Object type = null;
        if (UtilValidate.isEmpty(context.get("contactMechPurposeTypeId"))) {
            type = "GENERAL_LOCATION";
            types.add(type);
            type = "BILLING_LOCATION";
            types.add(type);
            type = "PAYMENT_LOCATION";
            types.add(type);
            type = "SHIPPING_LOCATION";
            types.add(type);
        } else {
            type = context.get("contactMechPurposeTypeId");
            types.add(type);
        }
        findMap.put("contactMechTypeId", "POSTAL_ADDRESS");
        // TODO: Convert <find-by-and> element
        addressAll2 = EntityUtil.filterByDate((List<GenericValue>) context.get("addressAll1"));
        List<GenericValue> addressAll3 = EntityUtil.filterByDate((List<GenericValue>) addressAll2);
        if (UtilValidate.isNotEmpty(addressAll3)) {
            if (types != null) {
                for (Object typeEntry : types) {
                    if (addressAll3 != null) {
                        for (Object address_iter : (List<?>) addressAll3) {
                            address = (GenericValue) address_iter;
                            if (UtilValidate.isEmpty(found)) {
                                if (java.util.Objects.equals(((Map<String, Object>) address).get("contactMechPurposeTypeId"), typeEntry)) {
                                    found = "notImportant";
                                    result.put("contactMechId", ((Map<String, Object>) address).get("contactMechId"));
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("address1"))) {
                                        result.put("address1", ((Map<String, Object>) address).get("address1"));
                                    }
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("address2"))) {
                                        result.put("address2", ((Map<String, Object>) address).get("address2"));
                                    }
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("directions"))) {
                                        result.put("directions", ((Map<String, Object>) address).get("directions"));
                                    }
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("city"))) {
                                        result.put("city", ((Map<String, Object>) address).get("city"));
                                    }
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("postalCode"))) {
                                        result.put("postalCode", ((Map<String, Object>) address).get("postalCode"));
                                    }
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("stateProvinceGeoId"))) {
                                        result.put("stateProvinceGeoId", ((Map<String, Object>) address).get("stateProvinceGeoId"));
                                    }
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("countyGeoId"))) {
                                        result.put("countyGeoId", ((Map<String, Object>) address).get("countyGeoId"));
                                    }
                                    if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("countryGeoId"))) {
                                        result.put("countryGeoId", ((Map<String, Object>) address).get("countryGeoId"));
                                    }
                                    result.put("contactMechPurposeTypeId", ((Map<String, Object>) address).get("contactMechPurposeTypeId"));
                                }
                            }
                        }
                    }
                }
            }
        } else {
            // TODO: Convert <find-by-and> element
            addressAll2 = EntityUtil.filterByDate((List<GenericValue>) context.get("addressAll1"));
            address = EntityUtil.getFirst((List<GenericValue>) addressAll2);
            result.put("contactMechId", ((Map<String, Object>) address).get("contactMechId"));
            if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("paAddress1"))) {
                result.put("address1", ((Map<String, Object>) address).get("paAddress1"));
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("paAddress2"))) {
                result.put("address2", ((Map<String, Object>) address).get("paAddress2"));
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("paDirections"))) {
                result.put("directions", ((Map<String, Object>) address).get("paDirections"));
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("paCity"))) {
                result.put("city", ((Map<String, Object>) address).get("paCity"));
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("paPostalCode"))) {
                result.put("postalCode", ((Map<String, Object>) address).get("paPostalCode"));
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("paStateProvinceGeoId"))) {
                result.put("stateProvinceGeoId", ((Map<String, Object>) address).get("paStateProvinceGeoId"));
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("paCountyGeoId"))) {
                result.put("countyGeoId", ((Map<String, Object>) address).get("paCountyGeoId"));
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) address).get("paCountryGeoId"))) {
                result.put("countryGeoId", ((Map<String, Object>) address).get("paCountryGeoId"));
            }
        }

        return "success";
    }


    /**
     * create a AddressMatchMap
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createAddressMatchMap(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        String parameters_mapValue = ((String) context.get("mapValue")).toUpperCase();
        String parameters_mapKey = ((String) context.get("mapKey")).toUpperCase();
        GenericValue newEntity = delegator.makeValue("AddressMatchMap");
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
     * remove a AddressMatchMap
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deleteAddressMatchMap(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue fieldMap = delegator.makeValue("AddressMatchMap");
        fieldMap.setPKFields((Map<String, Object>) context);
        try {
            delegator.removeValue(fieldMap);
        } catch (Exception e) {
            Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        return "success";
    }


    /**
     * remove all AddressMatchMap
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String clearAddressMatchMap(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<GenericValue> addrs = null;
        try {
            addrs = EntityQuery.use(delegator)
                    .from("AddressMatchMap")
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying AddressMatchMap: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (addrs != null) {
            for (GenericValue addr : addrs) {
                try {
                    delegator.removeValue(addr);
                } catch (Exception e) {
                    Debug.logError(e, "Error removing value: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }

        return "success";
    }


    /**
     * createPartyRelationship
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyRelationship(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        GenericValue newEntity = null;
        if (UtilValidate.isEmpty(context.get("roleTypeIdFrom"))) {
            context.put("roleTypeIdFrom", "_NA_");
        }
        if (UtilValidate.isEmpty(context.get("roleTypeIdTo"))) {
            context.put("roleTypeIdTo", "_NA_");
        }
        if (UtilValidate.isEmpty(context.get("partyIdFrom"))) {
            context.put("partyIdFrom", ((Map<String, Object>) userLogin).get("partyId"));
        }
        if (UtilValidate.isEmpty(context.get("fromDate"))) {
            Timestamp parameters_fromDate = new Timestamp(System.currentTimeMillis());
        }
        List<GenericValue> partyRels = null;
        try {
            partyRels = EntityQuery.use(delegator)
                    .from("PartyRelationship")
                    .where(UtilMisc.toMap("partyIdFrom", context.get("partyIdFrom"), "roleTypeIdFrom", context.get("roleTypeIdFrom"), "partyIdTo", context.get("partyIdTo"), "roleTypeIdTo", context.get("roleTypeIdTo")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRelationship: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(partyRels)) {
            newEntity = delegator.makeValue("PartyRelationship");
            newEntity.setPKFields((Map<String, Object>) context);
            newEntity.setNonPKFields((Map<String, Object>) context);
            try {
                delegator.create(newEntity);
            } catch (Exception e) {
                Debug.logError(e, "Error creating value: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        return "success";
    }


    /**
     * updatePartyRelationship
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyRelationship(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        if (UtilValidate.isEmpty(context.get("roleTypeIdFrom"))) {
            context.put("roleTypeIdFrom", "_NA_");
        }
        if (UtilValidate.isEmpty(context.get("roleTypeIdTo"))) {
            context.put("roleTypeIdTo", "_NA_");
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PartyRelationship")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRelationship: " + e.getMessage(), MODULE);
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
     * deletePartyRelationship
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String deletePartyRelationship(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        if (UtilValidate.isEmpty(context.get("roleTypeIdFrom"))) {
            context.put("roleTypeIdFrom", "_NA_");
        }
        if (UtilValidate.isEmpty(context.get("roleTypeIdTo"))) {
            context.put("roleTypeIdTo", "_NA_");
        }
        GenericValue lookedUpValue = null;
        try {
            lookedUpValue = EntityQuery.use(delegator)
                    .from("PartyRelationship")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRelationship: " + e.getMessage(), MODULE);
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
     * create a company/contact relationship and add the related roles
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createPartyRelationshipContactAccount(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> roleMap = new HashMap<>();
        roleMap.put("partyId", context.get("accountPartyId"));
        roleMap.put("roleTypeId", "ACCOUNT");
        GenericValue partyRole = null;
        try {
            partyRole = EntityQuery.use(delegator)
                    .from("PartyRole")
                    .where(UtilMisc.toMap("partyId", ((Map<String, Object>) roleMap).get("partyId"), "roleTypeId", ((Map<String, Object>) roleMap).get("roleTypeId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(partyRole)) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyRole", roleMap);
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
        }
        roleMap.put("partyId", context.get("contactPartyId"));
        roleMap.put("roleTypeId", "CONTACT");
        try {
            partyRole = EntityQuery.use(delegator)
                    .from("PartyRole")
                    .where(UtilMisc.toMap("partyId", ((Map<String, Object>) roleMap).get("partyId"), "roleTypeId", ((Map<String, Object>) roleMap).get("roleTypeId")))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(partyRole)) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyRole", roleMap);
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
        }
        Map<String, Object> relMap = new HashMap<>();
        relMap.put("partyIdFrom", context.get("accountPartyId"));
        relMap.put("roleTypeIdFrom", "ACCOUNT");
        relMap.put("partyIdTo", context.get("contactPartyId"));
        relMap.put("roleTypeIdTo", "CONTACT");
        relMap.put("partyRelationshipTypeId", "EMPLOYMENT");
        relMap.put("comments", context.get("comments"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyRelationship", relMap);
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

        return "success";
    }


    /**
     * Notification email on party creation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sendCreatePartyEmailNotification(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> bodyParameters = null;
        GenericValue person = null;
        Map<String, Object> emailParams = null;
        bodyParameters.putAll((Map<String, Object>) context);
        Object emailType = "PARTY_REGIS_CONFIRM";
        Object productStoreId = context.get("productStoreId");
        if (UtilValidate.isEmpty(productStoreId)) {
            Debug.logWarning("No productStoreId specified.", MODULE);
        }
        GenericValue storeEmail = null;
        try {
            storeEmail = EntityQuery.use(delegator)
                    .from("ProductStoreEmailSetting")
                    .where(UtilMisc.toMap("emailType", emailType, "productStoreId", productStoreId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStoreEmailSetting: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) storeEmail).get("bodyScreenLocation"))) {
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
                Object scriptResult = GroovyUtil.eval("context.webSiteId = org.ofbiz.product.store.ProductStoreWorker.getStoreWebSiteIdForEmail(delegator,\n                    context.storeEmail?.productStoreId, null, true);", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            try {
                person = EntityQuery.use(delegator)
                        .from("Person")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Person: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            bodyParameters.put("person", person);
            emailParams.put("bodyParameters", bodyParameters);
            emailParams.put("sendTo", context.get("emailAddress"));
            emailParams.put("subject", ((Map<String, Object>) storeEmail).get("subject"));
            emailParams.put("sendFrom", ((Map<String, Object>) storeEmail).get("fromAddress"));
            emailParams.put("sendCc", ((Map<String, Object>) storeEmail).get("ccAddress"));
            emailParams.put("sendBcc", ((Map<String, Object>) storeEmail).get("bccAddress"));
            emailParams.put("contentType", ((Map<String, Object>) storeEmail).get("contentType"));
            emailParams.put("bodyScreenUri", ((Map<String, Object>) storeEmail).get("bodyScreenLocation"));
            emailParams.put("webSiteId", context.get("webSiteId"));
            emailParams.put("emailType", emailType);
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
     * Send the Notification email on personal information updation
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sendUpdatePersonalInfoEmailNotification(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> bodyParameters = null;
        GenericValue contactMech = null;
        Map<String, Object> emailParams = null;
        Object partyId = null;
        List<GenericValue> partyContactDetailByPurposes = null;
        GenericValue partyContactDetailByPurpose = null;
        Object contactMechId = null;
        GenericValue partyAndPerson = null;
        bodyParameters.putAll((Map<String, Object>) context);
        Object productStoreId = context.get("productStoreId");
        if (UtilValidate.isEmpty(productStoreId)) {
            Debug.logWarning("No productStoreId specified.", MODULE);
        }
        GenericValue storeEmail = null;
        try {
            storeEmail = EntityQuery.use(delegator)
                    .from("ProductStoreEmailSetting")
                    .where(UtilMisc.toMap("emailType", "UPD_PRSNL_INF_CNFRM", "productStoreId", productStoreId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStoreEmailSetting: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) storeEmail).get("bodyScreenLocation"))) {
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
                Object scriptResult = GroovyUtil.eval("context.webSiteId = org.ofbiz.product.store.ProductStoreWorker.getStoreWebSiteIdForEmail(delegator,\n                    context.storeEmail?.productStoreId, null, true);", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            if (UtilValidate.isNotEmpty(((Map<String, Object>) context.get("updatedUserLogin")).get("partyId"))) {
                partyId = ((Map<String, Object>) context.get("updatedUserLogin")).get("partyId");
            } else {
                partyId = context.get("partyId");
            }
            try {
                partyContactDetailByPurposes = EntityQuery.use(delegator)
                        .from("PartyContactDetailByPurpose")
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyContactDetailByPurpose: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            partyContactDetailByPurpose = EntityUtil.getFirst((List<GenericValue>) partyContactDetailByPurposes);
            try {
                partyAndPerson = EntityQuery.use(delegator)
                        .from("PartyAndPerson")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyAndPerson: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            bodyParameters.put("partyAndPerson", partyAndPerson);
            contactMechId = ((Map<String, Object>) partyContactDetailByPurpose).get("contactMechId");
            try {
                contactMech = EntityQuery.use(delegator)
                        .from("ContactMech")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            emailParams.put("sendTo", ((Map<String, Object>) contactMech).get("infoString"));
            emailParams.put("subject", ((Map<String, Object>) storeEmail).get("subject"));
            emailParams.put("sendFrom", ((Map<String, Object>) storeEmail).get("fromAddress"));
            emailParams.put("sendCc", ((Map<String, Object>) storeEmail).get("ccAddress"));
            emailParams.put("sendBcc", ((Map<String, Object>) storeEmail).get("bccAddress"));
            emailParams.put("contentType", ((Map<String, Object>) storeEmail).get("contentType"));
            emailParams.put("bodyParameters", bodyParameters);
            emailParams.put("bodyScreenUri", ((Map<String, Object>) storeEmail).get("bodyScreenLocation"));
            emailParams.put("webSiteId", context.get("webSiteId"));
            if (UtilValidate.isNotEmpty(((Map<String, Object>) emailParams).get("sendTo"))) {
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
            } else {
                Debug.logWarning("Tried to send Update Personal Info Notifcation with no to address; partyId is [" + partyId + "], subject is: " + ((Map<String, Object>) emailParams).get("subject"), MODULE);
            }
        }

        return "success";
    }


    /**
     * Create and update a person
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createUpdatePerson(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Map<String, Object> personContext = null;
        Object partyId = null;
        // TODO: Convert call-map-processor (in-map: parameters, out-map: personMap)
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        partyId = context.get("partyId");
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
        personContext.put("partyId", partyId);
        // set-service-fields from "personMap" to "personContext" for service "createPerson"
        personContext.putAll(UtilMisc.toMap(context.get("personMap")));
        if (UtilValidate.isEmpty(party)) {
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPerson", personContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                partyId = serviceResult.get("partyId");
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPerson: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            personContext.put("userLogin", context.get("userLogin"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("updatePerson", personContext);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling updatePerson: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        result.put("partyId", partyId);

        return "success";
    }


    /**
     * Create customer profile on basis of First Name ,Last Name and Email Address
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String quickCreateCustomer(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> signUpForContactListMap = null;
        // TODO: Convert call-map-processor (in-map: parameters, out-map: personInMap)
        Map<String, Object> personInMap = new HashMap<>(context);
        // TODO: Convert call-map-processor (in-map: parameters, out-map: emailAddressInMap)
        Map<String, Object> emailAddressInMap = new HashMap<>(context);
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Object partyId = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPerson", personInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            partyId = serviceResult.get("partyId");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPerson: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
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
        emailAddressInMap = new HashMap<>();
        emailAddressInMap.put("partyId", partyId);
        emailAddressInMap.put("userLogin", userLogin);
        emailAddressInMap.put("contactMechPurposeTypeId", "PRIMARY_EMAIL");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyEmailAddress", emailAddressInMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyEmailAddress: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if ("Y".equals(context.get("subscribeContactList"))) {
            signUpForContactListMap.put("partyId", partyId);
            signUpForContactListMap.put("contactListId", context.get("contactListId"));
            signUpForContactListMap.put("email", context.get("emailAddress"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("signUpForContactList", signUpForContactListMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling signUpForContactList: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        Map<String, Object> createPartyRoleInMap = new HashMap<>();
        createPartyRoleInMap.put("partyId", partyId);
        createPartyRoleInMap.put("roleTypeId", "CUSTOMER");
        createPartyRoleInMap.put("userLogin", userLogin);
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyRole", createPartyRoleInMap);
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

        return "success";
    }


    /**
     * Get the main role of this party which is a child of the MAIN_ROLE roletypeId
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String getPartyMainRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<GenericValue> roleTypeIn3Levels = null;
        Object mainRoleTypeId = null;
        GenericValue roleType = null;
        List<GenericValue> partyRoles = null;
        try {
            partyRoles = EntityQuery.use(delegator)
                    .from("PartyRole")
                    .where(UtilMisc.toMap("partyId", context.get("partyId")))
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        mainRoleTypeId = null;
        if (partyRoles != null) {
            for (GenericValue partyRole : partyRoles) {
                if (UtilValidate.isEmpty(mainRoleTypeId)) {
                    try {
                        roleTypeIn3Levels = EntityQuery.use(delegator)
                                .from("RoleTypeIn3Levels")
                                .where(UtilMisc.toMap("topRoleTypeId", "MAIN_ROLE", "lowRoleTypeId", ((Map<String, Object>) partyRole).get("roleTypeId")))
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying RoleTypeIn3Levels: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    if (UtilValidate.isNotEmpty(roleTypeIn3Levels)) {
                        mainRoleTypeId = ((Map<String, Object>) partyRole).get("roleTypeId");
                    }
                }
            }
        }
        if (UtilValidate.isNotEmpty(mainRoleTypeId)) {
            result.put("roleTypeId", mainRoleTypeId);
            try {
                roleType = EntityQuery.use(delegator)
                        .from("RoleType")
                        .where(UtilMisc.toMap("roleTypeId", mainRoleTypeId))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying RoleType: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            result.put("description", ((Map<String, Object>) roleType).get("description"));
        }

        return "success";
    }


    /**
     * Notification email on account activated
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String sendAccountActivatedEmailNotification(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> bodyParameters = null;
        GenericValue contactMech = null;
        GenericValue person = null;
        Map<String, Object> emailParams = null;
        GenericValue userLoginParty = null;
        List<GenericValue> partyContactDetailByPurposes = null;
        GenericValue partyContactDetailByPurpose = null;
        Object contactMechId = null;
        bodyParameters.putAll((Map<String, Object>) context);
        Object emailType = "PRDS_CUST_ACTIVATED";
        Object productStoreId = context.get("productStoreId");
        GenericValue storeEmail = null;
        try {
            storeEmail = EntityQuery.use(delegator)
                    .from("ProductStoreEmailSetting")
                    .where(UtilMisc.toMap("emailType", emailType, "productStoreId", productStoreId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ProductStoreEmailSetting: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isNotEmpty(((Map<String, Object>) storeEmail).get("bodyScreenLocation"))) {
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
                Object scriptResult = GroovyUtil.eval("context.webSiteId = org.ofbiz.product.store.ProductStoreWorker.getStoreWebSiteIdForEmail(delegator,\n                    context.storeEmail?.productStoreId, null, true);", scriptContext);
            } catch (Exception e) {
                Debug.logError(e, "Error executing Groovy script: " + e.getMessage(), MODULE);
            }
            try {
                userLoginParty = EntityQuery.use(delegator)
                        .from("UserLogin")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying UserLogin: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            context.put("partyId", ((Map<String, Object>) userLoginParty).get("partyId"));
            try {
                partyContactDetailByPurposes = EntityQuery.use(delegator)
                        .from("PartyContactDetailByPurpose")
                        .filterByDate()
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyContactDetailByPurpose: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            partyContactDetailByPurpose = EntityUtil.getFirst((List<GenericValue>) partyContactDetailByPurposes);
            try {
                person = EntityQuery.use(delegator)
                        .from("Person")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Person: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            bodyParameters.put("person", person);
            emailParams.put("bodyParameters", bodyParameters);
            contactMechId = ((Map<String, Object>) partyContactDetailByPurpose).get("contactMechId");
            try {
                contactMech = EntityQuery.use(delegator)
                        .from("ContactMech")
                        .where(UtilMisc.toMap())
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContactMech: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            emailParams.put("sendTo", ((Map<String, Object>) contactMech).get("infoString"));
            emailParams.put("subject", ((Map<String, Object>) storeEmail).get("subject"));
            emailParams.put("sendFrom", ((Map<String, Object>) storeEmail).get("fromAddress"));
            emailParams.put("sendCc", ((Map<String, Object>) storeEmail).get("ccAddress"));
            emailParams.put("sendBcc", ((Map<String, Object>) storeEmail).get("bccAddress"));
            emailParams.put("contentType", ((Map<String, Object>) storeEmail).get("contentType"));
            emailParams.put("bodyScreenUri", ((Map<String, Object>) storeEmail).get("bodyScreenLocation"));
            emailParams.put("webSiteId", context.get("webSiteId"));
            emailParams.put("emailType", emailType);
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
     * Updates PartyProfileDefault defaultBillAddr and defaultShipAddr if ID changed
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updatePartyProfileDefaultPostalAddressIds(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object partyId = null;
        List<GenericValue> ppdList = null;
        partyId = context.get("partyId");
        if (UtilValidate.isEmpty(partyId)) {
            partyId = ((Map<String, Object>) context.get("userLogin")).get("partyId");
        }
        if (UtilValidate.isEmpty(partyId)) {
            return "success";
        }
        Object ppd_defaultBillAddr = null;
        Object ppd_defaultShipAddr = null;
        if ((!(UtilValidate.isEmpty(context.get("oldContactMechId"))) && !java.util.Objects.equals(context.get("oldContactMechId"), context.get("contactMechId")))) {
            try {
                ppdList = EntityQuery.use(delegator)
                        .from("PartyProfileDefault")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying PartyProfileDefault: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (ppdList != null) {
                for (GenericValue ppd : ppdList) {
                    if (java.util.Objects.equals(((Map<String, Object>) ppd).get("defaultBillAddr"), context.get("oldContactMechId"))) {
                        ppd.put("defaultBillAddr", context.get("contactMechId"));
                    }
                    if (java.util.Objects.equals(((Map<String, Object>) ppd).get("defaultShipAddr"), context.get("oldContactMechId"))) {
                        ppd.put("defaultShipAddr", context.get("contactMechId"));
                    }
                }
            }
            try {
                delegator.storeAll((List<GenericValue>) ppdList);
            } catch (Exception e) {
                Debug.logError(e, "Error storing list: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
        }

        return "success";
    }

}

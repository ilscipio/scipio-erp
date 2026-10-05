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
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.content.layout.LayoutWorker;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://party/script/org/ofbiz/party/communication/CommunicationEventEvents.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CommunicationEventEvents {

    private static final String MODULE = CommunicationEventEvents.class.getName();


    /**
     * Upload Content and Create Communication Content Association
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCommunicationEventContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Map<String, Object> data = new HashMap<>();
        Map<String, Object> attachMap = new HashMap<>();
        Map<String, Object> contentMap = new HashMap<>();
        List<GenericValue> contentAssoList = null;
        Map<String, Object> formInput = null;
        try {
            formInput = LayoutWorker.uploadImageAndParameters(request, "uploadedFile");
        } catch (Exception e) {
            Debug.logError(e, "Error calling LayoutWorker.uploadImageAndParameters: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        @SuppressWarnings("unchecked")
        Map<String, Object> formInputMap = (Map<String, Object>) formInput.get("formInput");
        if (UtilValidate.isEmpty(formInputMap.get("contentId"))) {
            if (UtilValidate.isEmpty(formInput.get("imageFileName"))) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyCreateCommunicationEventUploadFileMissing", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            context.put("imageFileName", formInput.get("imageFileName"));
            // set-service-fields from "formInput.formInput" to "data" for service "createContentFromUploadedFile"
            data.putAll(UtilMisc.toMap(formInput.get("formInput")));
            data.put("dataResourceTypeId", "LOCAL_FILE");
            data.put("dataTemplateTypeId", "NONE");
            data.put("dataCategoryId", formInputMap.get("dataCategoryId"));
            data.put("statusId", formInputMap.get("resourceStatusId"));
            data.put("dataResourceName", formInput.get("imageFileName"));
            data.put("mimeTypeId", ((Map<String, Object>) context.get("mimeType")).get("mimeTypeId"));
            data.put("uploadedFile", formInput.get("imageData"));
            data.put("_uploadedFile_fileName", formInput.get("imageFileName"));
            data.put("_uploadedFile_contentType", formInputMap.get("mimeTypeId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createDataResource", data);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("dataResourceId", serviceResult.get("dataResourceId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // set-service-fields from "formInput.formInput" to "attachMap" for service "attachUploadToDataResource"
            attachMap.putAll(UtilMisc.toMap(formInput.get("formInput")));
            attachMap.put("uploadedFile", formInput.get("imageData"));
            attachMap.put("_uploadedFile_fileName", formInput.get("imageFileName"));
            attachMap.put("_uploadedFile_contentType", formInputMap.get("mimeTypeId"));
            attachMap.put("dataResourceId", context.get("dataResourceId"));
            attachMap.put("mimeTypeId", ((Map<String, Object>) context.get("mimeType")).get("mimeTypeId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("attachUploadToDataResource", attachMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
            } catch (Exception e) {
                Debug.logError(e, "Error calling attachUploadToDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            // set-service-fields from "formInput.formInput" to "contentMap" for service "createContentFromDataResource"
            contentMap.putAll(UtilMisc.toMap(formInput.get("formInput")));
            contentMap.put("roleTypeId", formInputMap.get("roleTypeId"));
            contentMap.put("partyId", formInputMap.get("partyId"));
            contentMap.put("contentTypeId", formInputMap.get("contentTypeId"));
            contentMap.put("dataResourceId", context.get("dataResourceId"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createContentFromDataResource", contentMap);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("contentId", serviceResult.get("contentId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createContentFromDataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            Debug.logInfo(" Content : " + context.get("contentId"), MODULE);
        }
        Map<String, Object> partycontent = new HashMap<>();
        // set-service-fields from "formInput.formInput" to "partycontent" for service "createPartyContent"
        partycontent.putAll(UtilMisc.toMap(formInput.get("formInput")));
        partycontent.put("contentId", context.get("contentId"));
        partycontent.put("partyContentTypeId", formInputMap.get("partyContentTypeId"));
        partycontent.put("partyId", formInputMap.get("partyId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createPartyContent", partycontent);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createPartyContent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> updateMap = new HashMap<>();
        // set-service-fields from "formInput.formInput" to "updateMap" for service "updateCommunicationEvent"
        updateMap.putAll(UtilMisc.toMap(formInput.get("formInput")));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateCommunicationEvent", updateMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateCommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> contentAssoc = new HashMap<>();
        // set-service-fields from "formInput.formInput" to "contentAssoc" for service "createCommEventContentAssoc"
        contentAssoc.putAll(UtilMisc.toMap(formInput.get("formInput")));
        contentAssoc.put("contentId", context.get("contentId"));
        contentAssoc.put("communicationEventId", formInputMap.get("communicationEventId"));
        Object fromDate = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCommEventContentAssoc", contentAssoc);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            fromDate = serviceResult.get("fromDate");
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCommEventContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        // set-service-fields from "formInput.formInput" to "contentMap" for service "createContentAssoc"
        contentMap.putAll(UtilMisc.toMap(formInput.get("formInput")));
        if (UtilValidate.isNotEmpty(formInputMap.get("contentIdFrom"))) {
            contentMap.put("contentAssocTypeId", "SUB_CONTENT");
            contentMap.put("contentIdFrom", formInputMap.get("contentIdFrom"));
            contentMap.put("contentId", formInputMap.get("contentIdFrom"));
            contentMap.put("contentIdTo", context.get("contentId"));
            Timestamp contentMap_fromDate = new Timestamp(System.currentTimeMillis());
            try {
                contentAssoList = EntityQuery.use(delegator)
                        .from("ContentAssoc")
                        .where(UtilMisc.toMap("contentId", ((Map<String, Object>) contentMap).get("contentId"), "contentIdTo", ((Map<String, Object>) contentMap).get("contentIdTo")))
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(context.get("contentAssonList"))) {
                try {
                    Map<String, Object> serviceResult = dispatcher.runSync("createContentAssoc", contentMap);
                    if (ServiceUtil.isError(serviceResult)) {
                        String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                        request.setAttribute("_ERROR_MESSAGE_", errMsg);
                        return "error";
                    }
                } catch (Exception e) {
                    Debug.logError(e, "Error calling createContentAssoc: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        request.setAttribute("communicationEventId", formInputMap.get("communicationEventId"));
        Object my = "My";
        request.setAttribute("my", my);

        return "success";
    }


    /**
     * Add a role to a communictaion event and save communication event itseld
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createCommunicationEventRole(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> updateMap = new HashMap<>();
        // set-service-fields from "formInput" to "updateMap" for service "updateCommunicationEvent"
        updateMap.putAll(UtilMisc.toMap(context.get("formInput")));
        updateMap.put("communicationEventId", context.get("communicationEventId"));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateCommunicationEvent", updateMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateCommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Map<String, Object> createRole = new HashMap<>();
        // set-service-fields from "parameters" to "createRole" for service "createCommunicationEventRole"
        createRole.putAll(UtilMisc.toMap(context));
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("createCommunicationEventRole", createRole);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling createCommunicationEventRole: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        request.setAttribute("communicationEventId", context.get("communicationEventId"));
        Object my = "My";
        request.setAttribute("my", my);

        return "success";
    }


    /**
     * Allocate an emailaddress to an existing/new party, update the communication event accordingly
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String allocateMsgToParty(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Map<String, Object> newParty = new HashMap<>();
        Map<String, Object> newEmail = new HashMap<>();
        Map<String, Object> inCom = new HashMap<>();
        GenericValue party = null;
        GenericValue communicationEvent = null;
        try {
            communicationEvent = EntityQuery.use(delegator)
                    .from("CommunicationEvent")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying CommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        if (UtilValidate.isEmpty(communicationEvent)) {
            {
                String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyCommunicationEventNotFound", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            if (UtilValidate.isEmpty(context.get("emailAddress"))) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyEmailAddressRequired", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isEmpty(context.get("lastName"))) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyLastNameRequested", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isEmpty(context.get("firstName"))) {
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyFirstNameRequested", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(context.get("partyId"))) {
            // set-service-fields from "parameters" to "newParty" for service "createPerson"
            newParty.putAll(UtilMisc.toMap(context));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPerson", newParty);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                context.put("partyId", serviceResult.get("partyId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPerson: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        if (UtilValidate.isNotEmpty(context.get("emailAddress"))) {
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
                {
                    String errorMsg = UtilProperties.getMessage("PartyUiLabels", "PartyPartyIdMissing", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            newEmail.put("contactMechPurposeTypeId", "PRIMARY_EMAIL");
            newEmail.put("partyId", context.get("partyId"));
            newEmail.put("emailAddress", context.get("emailAddress"));
            try {
                Map<String, Object> serviceResult = dispatcher.runSync("createPartyEmailAddress", newEmail);
                if (ServiceUtil.isError(serviceResult)) {
                    String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                    request.setAttribute("_ERROR_MESSAGE_", errMsg);
                    return "error";
                }
                inCom.put("contactMechIdFrom", serviceResult.get("contactMechId"));
            } catch (Exception e) {
                Debug.logError(e, "Error calling createPartyEmailAddress: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }
        inCom.put("communicationEventId", context.get("communicationEventId"));
        inCom.put("partyIdFrom", context.get("partyId"));
        inCom.put("statusId", "COM_ENTERED");
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("updateCommunicationEvent", inCom);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
        } catch (Exception e) {
            Debug.logError(e, "Error calling updateCommunicationEvent: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        request.setAttribute("communicationEventId", context.get("communicationEventId"));
        GenericValue nameView = null;
        try {
            nameView = EntityQuery.use(delegator)
                    .from("PartyNameView")
                    .where(UtilMisc.toMap())
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying PartyNameView: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        request.setAttribute("_EVENT_MESSAGE_", "Email addres: " + context.get("emailAddress") + " allocated to party: " + (nameView != null ? nameView.get("groupName") : "") + (nameView != null ? nameView.get("firstName") : "") + " " + (nameView != null ? nameView.get("middleName") : "") + " " + (nameView != null ? nameView.get("lastName") : "") + "[" + context.get("partyId") + "]");

        return "success";
    }

}

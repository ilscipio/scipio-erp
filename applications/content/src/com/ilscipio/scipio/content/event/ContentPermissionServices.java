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
package com.ilscipio.scipio.content.event;

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
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://content/script/org/ofbiz/content/permission/ContentPermissionServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class ContentPermissionServices {

    private static final String MODULE = ContentPermissionServices.class.getName();


    /**
     * Check user has Content Management permission
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String contentManagerPermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object primaryPermission = "CONTENTMGR";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"

        return "success";
    }


    /**
     * Check user has Content Management permission
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String contentManagerRolePermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object primaryPermission = "CONTENTMGR";
        Object altPermission = "CONTENTMGR_ROLE";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"

        return "success";
    }


    /**
     * Generic Service for Content Permissions
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String genericContentPermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object ownerContentId = null;
        Object contentOperationId = null;
        Object primaryPermission = "CONTENTMGR";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        Object roleEntityField = "contentId";
        Object roleEntity = "ContentRole";
        if ((UtilValidate.isEmpty(context.get("ownerContentId")) && !(UtilValidate.isEmpty(context.get("contentIdFrom"))))) {
            ownerContentId = context.get("contentIdFrom");
        }
        if (!(Boolean.TRUE.equals(context.get("hasPermission")))) {
            if ("VIEW".equals(context.get("mainAction"))) {
                String result2 = viewContentPermission(request, response);
                if (!"success".equals(result2)) {
                    return result2;
                }
            }
        } else {
            Debug.logInfo("Admin permission found: " + primaryPermission + "_" + context.get("mainAction"), MODULE);
        }
        Debug.logInfo("Permission service [" + context.get("mainAction") + " / " + context.get("contentId") + "] completed; returning hasPermission = " + context.get("hasPermission"), MODULE);
        result.put("hasPermission", context.get("hasPermission"));

        return "success";
    }


    /**
     * Check user can view content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String viewContentPermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object primaryPermission = null;
        Object mainAction = null;
        Object contentOperationId = null;
        Object contentId = null;
        Object checkId = null;
        GenericValue content = null;
        if (UtilValidate.isEmpty(context.get("hasPermission"))) {
            primaryPermission = "CONTENTMGR";
            mainAction = "VIEW";
            // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
            // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        }
        primaryPermission = "CONTENTMGR_ROLE";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        Object parameters_contentOperationId = null;
        if (Boolean.TRUE.equals(context.get("hasPermission"))) {
            if (UtilValidate.isEmpty(context.get("contentOperationId"))) {
                context.put("contentOperationId", "CONTENT_VIEW");
            }
            if (UtilValidate.isEmpty(contentId)) {
                contentId = context.get("contentId");
            }
            if (UtilValidate.isEmpty(contentId)) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentViewPermissionError", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
            }
            if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                    UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                return "error";
            }
            try {
                content = EntityQuery.use(delegator)
                        .from("Content")
                        .where(UtilMisc.toMap("contentId", contentId))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            contentOperationId = context.get("contentOperationId");
            content = content;
            checkId = contentId;
            String result = checkContentOperationSecurity(request, response);
            if (!"success".equals(result)) {
                return result;
            }
        }

        return "success";
    }


    /**
     * Check user can create new content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createContentPermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object primaryPermission = null;
        Object mainAction = null;
        Object ownerContentId = null;
        Object contentOperationId = null;
        Object statusId = null;
        GenericValue currentContent = null;
        Object checkId = null;
        if (UtilValidate.isEmpty(context.get("hasPermission"))) {
            primaryPermission = "CONTENTMGR";
            mainAction = "CREATE";
            // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
            // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        }
        if (UtilValidate.isEmpty(ownerContentId)) {
            ownerContentId = context.get("ownerContentId");
        }
        if (UtilValidate.isEmpty(contentOperationId)) {
            contentOperationId = context.get("contentOperationId");
        }
        if (UtilValidate.isEmpty(statusId)) {
            statusId = context.get("statusId");
        }
        primaryPermission = "CONTENTMGR_ROLE";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        if (Boolean.TRUE.equals(context.get("hasPermission"))) {
            Debug.logVerbose("Found necessary ROLE permission: " + primaryPermission + "_" + mainAction + " :: " + contentOperationId, MODULE);
            String inlineResult;
            if (!(UtilValidate.isEmpty(contentOperationId))) {
                checkId = ownerContentId;
                inlineResult = checkContentOperationSecurity(request, response);
                if (!"success".equals(inlineResult)) {
                    return inlineResult;
                }
            }
            if ((UtilValidate.isEmpty(contentOperationId) || Boolean.FALSE.equals(context.get("hasPermission")))) {
                if (!(UtilValidate.isEmpty(ownerContentId))) {
                    Debug.logVerbose("No operation found; but ownerContentId [" + ownerContentId + "] was; checking ownership", MODULE);
                    checkId = ownerContentId;
                    Debug.logVerbose("Checking Parent Ownership [" + checkId + "]", MODULE);
                    inlineResult = checkOwnership(request, response);
                    if (!"success".equals(inlineResult)) {
                        return inlineResult;
                    }
                    if (Boolean.FALSE.equals(context.get("hasPermission"))) {
                        while ((Boolean.FALSE.equals(context.get("hasPermission")) && !(UtilValidate.isEmpty(checkId)))) {
                            try {
                                currentContent = EntityQuery.use(delegator)
                                        .from("Content")
                                        .where(UtilMisc.toMap("contentId", checkId))
                                        .queryOne();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            if (!(UtilValidate.isEmpty(((Map<String, Object>) currentContent).get("ownerContentId")))) {
                                checkId = ((Map<String, Object>) currentContent).get("ownerContentId");
                                Debug.logVerbose("Checking Parent(s) Ownership [" + checkId + "]", MODULE);
                                inlineResult = checkOwnership(request, response);
                                if (!"success".equals(inlineResult)) {
                                    return inlineResult;
                                }
                            } else {
                                checkId = null;
                            }
                        }
                    } else {
                        Debug.logVerbose("Permission set to TRUE; granting access", MODULE);
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Check user can update existing content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateContentPermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object primaryPermission = null;
        Object mainAction = null;
        Object contentId = null;
        Object ownerContentId = null;
        Object contentOperationId = null;
        GenericValue currentContent = null;
        GenericValue thisContent = null;
        Object checkId = null;
        if (UtilValidate.isEmpty(context.get("hasPermission"))) {
            primaryPermission = "CONTENTMGR";
            mainAction = "UPDATE";
            // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
            // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        }
        if (UtilValidate.isEmpty(contentId)) {
            contentId = context.get("contentId");
        }
        if (UtilValidate.isEmpty(contentId)) {
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentSecurityUpdatePermission", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        if (UtilValidate.isEmpty(ownerContentId)) {
            ownerContentId = context.get("ownerContentId");
        }
        if (UtilValidate.isEmpty(contentOperationId)) {
            contentOperationId = context.get("contentOperationId");
        }
        primaryPermission = "CONTENTMGR_ROLE";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        if (Boolean.TRUE.equals(context.get("hasPermission"))) {
            Debug.logVerbose("Found necessary ROLE permission: " + primaryPermission + "_" + mainAction, MODULE);
            try {
                thisContent = EntityQuery.use(delegator)
                        .from("Content")
                        .where(UtilMisc.toMap("contentId", contentId))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(thisContent)) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentNoContentFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            String inlineResult;
            if (!(UtilValidate.isEmpty(contentOperationId))) {
                Debug.logVerbose("Checking content operation for UPDATE: " + contentOperationId, MODULE);
                checkId = contentId;
                inlineResult = checkContentOperationSecurity(request, response);
                if (!"success".equals(inlineResult)) {
                    return inlineResult;
                }
            }
            if ((UtilValidate.isEmpty(contentOperationId) || Boolean.FALSE.equals(context.get("hasPermission")))) {
                Debug.logVerbose("No valid operation for UPDATE; checking ownership instead!", MODULE);
                checkId = contentId;
                inlineResult = checkOwnership(request, response);
                if (!"success".equals(inlineResult)) {
                    return inlineResult;
                }
                if ((!(UtilValidate.isEmpty(ownerContentId)) && !java.util.Objects.equals(((Map<String, Object>) thisContent).get("ownerContentId"), ownerContentId))) {
                    Debug.logVerbose("Updating content ownership; need to verify permision on parent(s)", MODULE);
                    checkId = ownerContentId;
                    inlineResult = checkOwnership(request, response);
                    if (!"success".equals(inlineResult)) {
                        return inlineResult;
                    }
                    if (Boolean.FALSE.equals(context.get("hasPermission"))) {
                        while ((Boolean.FALSE.equals(context.get("hasPermission")) && !(UtilValidate.isEmpty(checkId)))) {
                            try {
                                currentContent = EntityQuery.use(delegator)
                                        .from("Content")
                                        .where(UtilMisc.toMap("contentId", checkId))
                                        .queryOne();
                            } catch (Exception e) {
                                Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                return "error";
                            }
                            if (!(UtilValidate.isEmpty(((Map<String, Object>) currentContent).get("ownerContentId")))) {
                                checkId = ((Map<String, Object>) currentContent).get("ownerContentId");
                                inlineResult = checkOwnership(request, response);
                                if (!"success".equals(inlineResult)) {
                                    return inlineResult;
                                }
                            } else {
                                checkId = null;
                            }
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Checks for Operation defined security
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkContentOperationSecurity(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object requiredField = null;
        Object contentPurposeTypeId = null;
        List<GenericValue> operations = null;
        List<GenericValue> currentOperations = null;
        GenericValue currentContent = null;
        Object checkPartyId = null;
        Boolean hasPermission = null;
        Object checkRoleTypeId = null;
        Object checkId = null;
        hasPermission = Boolean.FALSE;
        if (UtilValidate.isEmpty(context.get("contentOperationId"))) {
            requiredField = "contentOperationId";
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentRequiredField", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(contentPurposeTypeId)) {
            contentPurposeTypeId = context.get("contentPurposeTypeId");
        }
        if (UtilValidate.isEmpty(contentPurposeTypeId)) {
            contentPurposeTypeId = "_NA_";
        }
        GenericValue checkContent = null;
        try {
            checkContent = EntityQuery.use(delegator)
                    .from("Content")
                    .where(UtilMisc.toMap("contentId", checkId))
                    .queryOne();
        } catch (Exception e) {
            Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Object statusId = ((Map<String, Object>) checkContent).get("statusId");
        if (("CONTENT_CREATE".equals(context.get("contentOperationId")) && !(UtilValidate.isEmpty(contentPurposeTypeId)))) {
            try {
                operations = EntityQuery.use(delegator)
                        .from("ContentPurposeOperation")
                        .queryList();
            } catch (Exception e) {
                Debug.logError(e, "Error querying ContentPurposeOperation: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        } else {
            String result = findAllContentPurposes(request, response);
            if (!"success".equals(result)) {
                return result;
            }
            if (context.get("contentPurposes") != null) {
                for (Object currentPurpose : (List<Object>) context.get("contentPurposes")) {
                    try {
                        currentOperations = EntityQuery.use(delegator)
                                .from("ContentPurposeOperation")
                                .queryList();
                    } catch (Exception e) {
                        Debug.logError(e, "Error querying ContentPurposeOperation: " + e.getMessage(), MODULE);
                        request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                        return "error";
                    }
                    // TODO: Convert <list-to-list> element
                }
            }
            if (UtilValidate.isEmpty(context.get("contentPurposes"))) {
                try {
                    operations = EntityQuery.use(delegator)
                            .from("ContentPurposeOperation")
                            .queryList();
                } catch (Exception e) {
                    Debug.logError(e, "Error querying ContentPurposeOperation: " + e.getMessage(), MODULE);
                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                    return "error";
                }
            }
        }
        Object toCheckContentId = checkId;
        Debug.logVerbose("[" + checkId + "] Found Operations [" + contentPurposeTypeId + "/" + context.get("contentOperationId") + "] :: " + operations, MODULE);
        if (UtilValidate.isEmpty(operations)) {
            Debug.logVerbose("No operations found; permission granted!", MODULE);
            hasPermission = Boolean.TRUE;
        } else {
            String inlineResult = findAllAssociatedPartyIds(request, response);
            if (!"success".equals(inlineResult)) {
                return inlineResult;
            }
            if (operations != null) {
                for (GenericValue operation : operations) {
                    if (Boolean.FALSE.equals(hasPermission)) {
                        if ((UtilValidate.isEmpty(checkId) && !(UtilValidate.isEmpty(toCheckContentId)))) {
                            checkId = toCheckContentId;
                        }
                        Debug.logVerbose("Testing [" + checkId + "] [" + statusId + "] OPERATION: " + operation, MODULE);
                        if (("_NA_".equals(((Map<String, Object>) operation).get("statusId")) || (!(UtilValidate.isEmpty(statusId)) && java.util.Objects.equals(((Map<String, Object>) operation).get("statusId"), statusId)))) {
                            Debug.logVerbose("Passed status check; now checking role(s)", MODULE);
                            if (context.get("partyIdList") != null) {
                                for (Object thisPartyId : (List<Object>) context.get("partyIdList")) {
                                    if (Boolean.FALSE.equals(hasPermission)) {
                                        checkRoleTypeId = ((Map<String, Object>) operation).get("roleTypeId");
                                        checkPartyId = thisPartyId;
                                        if ((UtilValidate.isEmpty(checkId) && !(UtilValidate.isEmpty(toCheckContentId)))) {
                                            checkId = toCheckContentId;
                                        }
                                        inlineResult = checkRoleSecurity(request, response);
                                        if (!"success".equals(inlineResult)) {
                                            return inlineResult;
                                        }
                                        if ((Boolean.FALSE.equals(hasPermission) && !(UtilValidate.isEmpty(checkId)))) {
                                            Debug.logVerbose("Starting loop; checking operation: " + ((Map<String, Object>) operation).get("contentOperationId"), MODULE);
                                            while ((Boolean.FALSE.equals(hasPermission) && !(UtilValidate.isEmpty(checkId)))) {
                                                try {
                                                    currentContent = EntityQuery.use(delegator)
                                                            .from("Content")
                                                            .where(UtilMisc.toMap("contentId", checkId))
                                                            .queryOne();
                                                } catch (Exception e) {
                                                    Debug.logError(e, "Error querying Content: " + e.getMessage(), MODULE);
                                                    request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                                                    return "error";
                                                }
                                                if (!(UtilValidate.isEmpty(((Map<String, Object>) currentContent).get("ownerContentId")))) {
                                                    checkId = ((Map<String, Object>) currentContent).get("ownerContentId");
                                                    inlineResult = checkRoleSecurity(request, response);
                                                    if (!"success".equals(inlineResult)) {
                                                        return inlineResult;
                                                    }
                                                } else {
                                                    checkId = null;
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        return "success";
    }


    /**
     * Checks the (role) ownership of a record
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkOwnership(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object requiredField = null;
        Object partyId = null;
        Object checkPartyId = null;
        Boolean hasPermission = Boolean.FALSE;
        if (UtilValidate.isEmpty(context.get("checkId"))) {
            requiredField = "checkId";
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentRequiredField", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(partyId)) {
            partyId = ((Map<String, Object>) userLogin).get("partyId");
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        String inlineResult = findAllAssociatedPartyIds(request, response);
        if (!"success".equals(inlineResult)) {
            return inlineResult;
        }
        Object checkRoleTypeId = "OWNER";
        if (context.get("partyIdList") != null) {
            for (Object thisPartyId : (List<Object>) context.get("partyIdList")) {
                if (!("true".equals(hasPermission))) {
                    Debug.logVerbose("Checking to see if party [" + thisPartyId + "] has ownership of " + context.get("checkId") + " :: " + hasPermission, MODULE);
                    checkPartyId = thisPartyId;
                    inlineResult = checkRoleSecurity(request, response);
                    if (!"success".equals(inlineResult)) {
                        return inlineResult;
                    }
                } else {
                    Debug.logVerbose("Field hasPermission is TRUE [" + hasPermission + "] did not test!", MODULE);
                }
            }
        }

        return "success";
    }


    /**
     * Check users role associations with Content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String checkRoleSecurity(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object requiredField = null;
        Map<String, Object> lookup = null;
        Boolean hasPermission = null;
        hasPermission = Boolean.FALSE;
        Debug.logVerbose("checkRoleSecurity: just reset hasPermission value to false!", MODULE);
        if (UtilValidate.isEmpty(context.get("roleEntity"))) {
            requiredField = "roleEntity";
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentRequiredField", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(context.get("roleEntityField"))) {
            requiredField = "roleEntityField";
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentRequiredField", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(context.get("checkId"))) {
            requiredField = "checkId";
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentRequiredField", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isEmpty(context.get("checkPartyId"))) {
            requiredField = "checkPartyId";
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentRequiredField", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Debug.logVerbose("About to test of checkRoleTypeId is empty... " + context.get("checkRoleTypeId"), MODULE);
        Object lookup___roleEntityField_ = null;
        Object lookup_roleTypeId = null;
        Object lookup_partyId = null;
        List<GenericValue> foundRoles = null;
        if ((!(UtilValidate.isEmpty(context.get("checkRoleTypeId"))) && "_NA_".equals(context.get("checkRoleTypeId")))) {
            hasPermission = Boolean.TRUE;
        } else {
            if (!(UtilValidate.isEmpty(context.get("checkRoleTypeId")))) {
                Debug.logVerbose("Doing lookup [" + context.get("roleEntity") + "] with roleTypeId : " + context.get("checkRoleTypeId"), MODULE);
                lookup.put((String) context.get("roleEntityField"), context.get("checkId"));
                lookup.put("roleTypeId", context.get("checkRoleTypeId"));
                lookup.put("partyId", context.get("checkPartyId"));
                // TODO: Convert <find-by-and> element
            } else {
                Debug.logVerbose("Doing lookup without roleTypeId", MODULE);
                lookup.put((String) context.get("roleEntityField"), context.get("checkId"));
                lookup.put("partyId", context.get("checkPartyId"));
                // TODO: Convert <find-by-and> element
            }
            Debug.logVerbose("Checking for ContentRole: [party] - " + context.get("checkPartyId") + " [role] - " + context.get("checkRoleTypeId") + " [content] - " + context.get("checkId") + " :: " + foundRoles, MODULE);
            if (!(UtilValidate.isEmpty(foundRoles))) {
                hasPermission = Boolean.TRUE;
            }
        }

        return "success";
    }


    /**
     * Find all content purposes for the specified content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String findAllContentPurposes(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object requiredField = null;
        if (UtilValidate.isEmpty(context.get("checkId"))) {
            requiredField = "checkId";
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentRequiredField", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        Map<String, Object> purposeLookup = new HashMap<>();
        purposeLookup.put("contentId", context.get("checkId"));
        // TODO: Convert <find-by-and> element

        return "success";
    }


    /**
     * Finds all associated party Ids for a user
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String findAllAssociatedPartyIds(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> lookupMap = new HashMap<>();
        lookupMap.put("partyIdFrom", ((Map<String, Object>) userLogin).get("partyId"));
        lookupMap.put("partyRelationshipTypeId", "GROUP_ROLLUP");
        lookupMap.put("includeFromToSwitched", "Y");
        Object partyIdList = null;
        try {
            Map<String, Object> serviceResult = dispatcher.runSync("getRelatedParties", lookupMap);
            if (ServiceUtil.isError(serviceResult)) {
                String errMsg = ServiceUtil.getErrorMessage(serviceResult);
                request.setAttribute("_ERROR_MESSAGE_", errMsg);
                return "error";
            }
            partyIdList = serviceResult.get("relatedPartyIdList");
        } catch (Exception e) {
            Debug.logError(e, "Error calling getRelatedParties: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        Debug.logVerbose("Got list of associated parties: " + partyIdList, MODULE);

        return "success";
    }


    /**
     * Finds all associated parent content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String findAllParentContent(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        List<String> error_list = new LinkedList<>();

        Object requiredField = null;
        if (UtilValidate.isEmpty(context.get("contentId"))) {
            requiredField = "contentId";
            {
                String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentRequiredField", locale);
                error_list.add(errorMsg);
                request.setAttribute("_ERROR_MESSAGE_", errorMsg);
            }
        }
        if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
            return "error";
        }
        List<GenericValue> assocs = null;
        try {
            assocs = EntityQuery.use(delegator)
                    .from("ContentAssoc")
                    .where(UtilMisc.toMap("contentIdTo", context.get("contentId")))
                    .filterByDate()
                    .queryList();
        } catch (Exception e) {
            Debug.logError(e, "Error querying ContentAssoc: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        result.put("contentAssocList", assocs);

        return "success";
    }

}

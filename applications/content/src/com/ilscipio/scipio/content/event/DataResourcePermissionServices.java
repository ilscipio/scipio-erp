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
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.LocalDispatcher;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://content/script/org/ofbiz/content/permission/DataResourcePermissionServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class DataResourcePermissionServices {

    private static final String MODULE = DataResourcePermissionServices.class.getName();


    /**
     * Generic Service for DataResource Permissions
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String genericDataResourcePermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Map<String, Object> result = new HashMap<>();

        Object primaryPermission = "CONTENTMGR";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        Object roleEntityField = "dataResourceId";
        Object roleEntity = "DataResourceRole";
        if (!(Boolean.TRUE.equals(context.get("hasPermission")))) {
            if ("VIEW".equals(context.get("mainAction"))) {
                String inlineResult = viewDataResourcePermission(request, response);
                if (!"success".equals(inlineResult)) {
                    return inlineResult;
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
     * Check user can view data resource
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String viewDataResourcePermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object primaryPermission = null;
        Object mainAction = null;
        if (UtilValidate.isEmpty(context.get("hasPermission"))) {
            primaryPermission = "CONTENTMGR";
            mainAction = "VIEW";
            // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
            // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        }
        primaryPermission = "CONTENTMGR_ROLE";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"

        return "success";
    }


    /**
     * Check user can create new content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String createDataResourcePermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        Object primaryPermission = null;
        Object mainAction = null;
        if (UtilValidate.isEmpty(context.get("hasPermission"))) {
            primaryPermission = "CONTENTMGR";
            mainAction = "CREATE";
            // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
            // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        }
        primaryPermission = "CONTENTMGR_ROLE";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"

        return "success";
    }


    /**
     * Check user can update existing content
     *
     * @param request The HTTP request
     * @param response The HTTP response
     * @return Event result ("success", "error", etc.)
     */
    public static String updateDataResourcePermission(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> context = UtilHttp.getCombinedMap(request);

        List<String> error_list = new LinkedList<>();

        Object primaryPermission = null;
        Object mainAction = null;
        Object dataResourceId = null;
        GenericValue thisDataResource = null;
        Object checkId = null;
        if (UtilValidate.isEmpty(context.get("hasPermission"))) {
            primaryPermission = "CONTENTMGR";
            mainAction = "UPDATE";
            // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
            // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        }
        if (UtilValidate.isEmpty(dataResourceId)) {
            dataResourceId = context.get("dataResourceId");
        }
        if (UtilValidate.isEmpty(dataResourceId)) {
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
        primaryPermission = "CONTENTMGR_ROLE";
        // TODO: Call simple-method "genericBasePermissionCheck" from "component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        // Original: call-simple-method method-name="genericBasePermissionCheck" xml-resource="component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml"
        if (Boolean.TRUE.equals(context.get("hasPermission"))) {
            Debug.logVerbose("Found necessary ROLE permission: " + primaryPermission + "_" + mainAction, MODULE);
            try {
                thisDataResource = EntityQuery.use(delegator)
                        .from("DataResource")
                        .where(UtilMisc.toMap("dataResourceId", dataResourceId))
                        .queryOne();
            } catch (Exception e) {
                Debug.logError(e, "Error querying DataResource: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
            if (UtilValidate.isEmpty(thisDataResource)) {
                {
                    String errorMsg = UtilProperties.getMessage("ContentUiLabels", "ContentDataResourceNotFound", locale);
                    error_list.add(errorMsg);
                    request.setAttribute("_ERROR_MESSAGE_", errorMsg);
                }
                if (UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_")) ||
                        UtilValidate.isNotEmpty(request.getAttribute("_ERROR_MESSAGE_LIST_"))) {
                    return "error";
                }
            }
            checkId = dataResourceId;
            // TODO: Call simple-method "checkOwnership" from "component://content/script/org/ofbiz/content/permission/ContentPermissionServices.xml"
            // Original: call-simple-method method-name="checkOwnership" xml-resource="component://content/script/org/ofbiz/content/permission/ContentPermissionServices.xml"
        }

        return "success";
    }

}

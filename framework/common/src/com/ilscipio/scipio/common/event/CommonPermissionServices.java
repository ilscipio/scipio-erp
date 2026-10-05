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
package com.ilscipio.scipio.common.event;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.security.Security;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Auto-generated event class from Simple Method XML.
 *
 * <p>Generated from: component://common/script/org/ofbiz/common/permission/CommonPermissionServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CommonPermissionServices {

    private static final String MODULE = CommonPermissionServices.class.getName();


    /**
     * Basic Permission check
     */
    public static Map<String, Object> genericBasePermissionCheck(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        Object mainAction = null;
        Object primaryPermission = null;
        Object altPermission = null;
        Object altPermissionList = null;
        String resourceDescription = null;
        Boolean hasPermission = null;
        String failMessage = null;
        if (UtilValidate.isEmpty(mainAction)) {
            mainAction = context.get("mainAction");
            if (UtilValidate.isEmpty(mainAction)) {
                {
                    String errorMsg = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionMainActionAttributeMissing", locale);
                    error_list.add(errorMsg);
                }
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        if (UtilValidate.isEmpty(primaryPermission)) {
            primaryPermission = context.get("primaryPermission");
            if (UtilValidate.isEmpty(primaryPermission)) {
                {
                    String errorMsg = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionPrimaryPermissionMissing", locale);
                    error_list.add(errorMsg);
                }
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        Debug.logVerbose("Checking for primary permission " + primaryPermission + "_" + mainAction, MODULE);
        if (UtilValidate.isEmpty(altPermission)) {
            altPermission = context.get("altPermission");
        }
        if (UtilValidate.isNotEmpty(altPermission)) {
            Debug.logVerbose("Checking for alternate permission " + altPermission + "_" + mainAction, MODULE);
            altPermissionList = ", " + altPermission + "_" + mainAction + ", " + altPermission + "_ADMIN";
        }
        resourceDescription = (String) context.get("resourceDescription");
        if (UtilValidate.isEmpty(resourceDescription)) {
            resourceDescription = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionThisOperation", locale);
        }
        String mainActionStr = (mainAction == null) ? "" : mainAction.toString();
        boolean primaryOk = UtilValidate.isNotEmpty(primaryPermission)
                && security.hasEntityPermission(primaryPermission.toString(), "_" + mainActionStr, userLogin);
        boolean altOk = UtilValidate.isNotEmpty(altPermission)
                && security.hasEntityPermission(altPermission.toString(), "_" + mainActionStr, userLogin);
        if (primaryOk || altOk) {
            hasPermission = Boolean.TRUE;
            result.put("hasPermission", hasPermission);
        } else {
            failMessage = UtilProperties.getMessage("CommonUiLabels", "CommonGenericPermissionError", locale);
            hasPermission = Boolean.FALSE;
            result.put("hasPermission", hasPermission);
            result.put("failMessage", failMessage);
        }

        return result;
    }


    /**
     * Get All CRUD and View Permissions
     */
    public static Map<String, Object> getAllCrudPermissions(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        List<String> error_list = new LinkedList<>();

        Object primaryPermission = null;
        Boolean hasCreatePermission = null;
        Boolean hasUpdatePermission = null;
        Boolean hasDeletePermission = null;
        Boolean hasViewPermission = null;
        Object altPermission = null;
        if (UtilValidate.isEmpty(primaryPermission)) {
            primaryPermission = context.get("primaryPermission");
            if (UtilValidate.isEmpty(primaryPermission)) {
                {
                    String errorMsg = UtilProperties.getMessage("CommonUiLabels", "CommonPermissionPrimaryPermissionMissing", locale);
                    error_list.add(errorMsg);
                }
            }
        }
        if (!error_list.isEmpty()) {
            return ServiceUtil.returnError(error_list);
        }
        hasCreatePermission = Boolean.FALSE;
        hasUpdatePermission = Boolean.FALSE;
        hasDeletePermission = Boolean.FALSE;
        hasViewPermission = Boolean.FALSE;
        Debug.logInfo("Getting all CRUD permissions for " + primaryPermission, MODULE);
        if (security.hasEntityPermission("${primaryPermission}", "_CREATE", userLogin)) {
            hasCreatePermission = Boolean.TRUE;
        }
        if (security.hasEntityPermission("${primaryPermission}", "_UPDATE", userLogin)) {
            hasUpdatePermission = Boolean.TRUE;
        }
        if (security.hasEntityPermission("${primaryPermission}", "_DELETE", userLogin)) {
            hasDeletePermission = Boolean.TRUE;
        }
        if (security.hasEntityPermission("${primaryPermission}", "_VIEW", userLogin)) {
            hasViewPermission = Boolean.TRUE;
        }
        if (UtilValidate.isEmpty(altPermission)) {
            altPermission = context.get("altPermission");
        }
        if (UtilValidate.isNotEmpty(altPermission)) {
            Debug.logInfo("Getting all CRUD permissions for " + altPermission, MODULE);
            if (security.hasEntityPermission("${altPermission}", "_CREATE", userLogin)) {
                hasCreatePermission = Boolean.TRUE;
            }
            if (security.hasEntityPermission("${altPermission}", "_UPDATE", userLogin)) {
                hasUpdatePermission = Boolean.TRUE;
            }
            if (security.hasEntityPermission("${altPermission}", "_DELETE", userLogin)) {
                hasDeletePermission = Boolean.TRUE;
            }
            if (security.hasEntityPermission("${altPermission}", "_VIEW", userLogin)) {
                hasViewPermission = Boolean.TRUE;
            }
        }
        result.put("hasCreatePermission", hasCreatePermission);
        result.put("hasUpdatePermission", hasUpdatePermission);
        result.put("hasDeletePermission", hasDeletePermission);
        result.put("hasViewPermission", hasViewPermission);

        return result;
    }


    /**
     * Visual Theme permission logic
     */
    public static Map<String, Object> visualThemePermissionCheck(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Object primaryPermission = null;
        Object altPermission = null;
        Boolean hasPermission = null;
        Object altPermissionList = null;
        String resourceDescription = null;
        String failMessage = null;
        Object mainAction = null;
        primaryPermission = "VISUALTHEME";
        Map<String, Object> inlineResult = genericBasePermissionCheck(dctx, context);
        if (ServiceUtil.isError(inlineResult)) {
            return inlineResult;
        }

        return result;
    }

}

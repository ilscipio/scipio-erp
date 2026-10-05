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
package com.ilscipio.scipio.cms.event;

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
 * <p>Generated from: component://cms/script/com/ilscipio/scipio/cms/CmsServices.xml</p>
 *
 * <p>SCIPIO: 4.0.0: Auto-generated.</p>
 */
public class CmsServices {

    private static final String MODULE = CmsServices.class.getName();


    /**
     * Main permission logic
     */
    public static Map<String, Object> cmsGenericPermission(DispatchContext dctx, Map<String, Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        Map<String, Object> result = ServiceUtil.returnSuccess();

        Security security = dctx.getSecurity();
        String primaryPermission = "CMS";
        String mainAction = (String) context.get("mainAction");
        if (mainAction == null) {
            return ServiceUtil.returnError(UtilProperties.getMessage("CommonUiLabels", "CommonPermissionMainActionAttributeMissing", locale));
        }
        if (security.hasPermission(primaryPermission + "_" + mainAction, userLogin) || security.hasPermission(primaryPermission + "_ADMIN", userLogin)) {
            result.put("hasPermission", Boolean.TRUE);
        } else {
            result.put("hasPermission", Boolean.FALSE);
            result.put("failMessage", UtilProperties.getMessage("CommonUiLabels", "CommonGenericPermissionError", locale));
        }

        return result;
    }

}

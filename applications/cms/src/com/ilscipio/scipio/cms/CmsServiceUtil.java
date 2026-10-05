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
package com.ilscipio.scipio.cms;

import java.util.Map;

import javax.servlet.http.HttpServletRequest;

import org.ofbiz.base.util.PropertyMessage;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

public abstract class CmsServiceUtil {

    //private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private static final ServiceErrorFormatter errorFmt = new ServiceErrorFormatter("Cms: ", ServiceErrorFormatter.Precision.DETAILED);

    private CmsServiceUtil() {
    }

    /**
     * Generic error formatter for service exception handling.
     * NOTE: each *Services class can derive its own for more precise messages.
     */
    public static ServiceErrorFormatter getErrorFormatter() {
        return errorFmt;
    }

    /**
     * NOTE: better to use cmsGenericPermission on service def because self-documenting.
     */
    public static void checkCmsPermission(DispatchContext dctx, Map<String, ?> context,
            String permAction) throws CmsPermissionException {
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (!dctx.getSecurity().hasEntityPermission("CMS", permAction, userLogin)) {
            PropertyMessage propMsg = PropertyMessage.make(ServiceUtil.resource, "serviceUtil.no_permission_to_run", null, " (CMS" + permAction + ")");
            throw new CmsPermissionException(propMsg);
        }
    }

    public static String getUserId(Map<String, ?> context) {
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        String userId = userLogin.getString("userLoginId");
        return userId;
    }

    public static String getUserPartyId(Map<String, ?> context) {
        HttpServletRequest request = (HttpServletRequest) context.get("request");
        GenericValue userLogin;
        if (request != null) {
            userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        } else {
            userLogin = (GenericValue) context.get("userLogin");
        }
        String partyId = userLogin.getString("partyId");
        return partyId;
    }

    public static GenericValue getUserLoginOrSystem(DispatchContext dctx, Map<String, ?> context) throws GenericEntityException {
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (userLogin == null) {
            userLogin = dctx.getDelegator().findOne("UserLogin", true, UtilMisc.toMap("userLoginId", "system"));
        }
        return userLogin;
    }

}

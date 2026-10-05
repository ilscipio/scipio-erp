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
package com.ilscipio.scipio.commerceprofile.service;

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.ServiceUtil;

import org.ofbiz.entity.tenant.TenantFiles;
import org.ofbiz.entity.util.EntityUtilProperties;

import com.ilscipio.scipio.commerceprofile.HostedGuard;
import com.ilscipio.scipio.commerceprofile.HostedProfile;

/**
 * Implementation of the guard services (see {@link HostedServices}).
 *
 * <p>SCIPIO: 4.0.0: Added (W1-02).</p>
 */
public class HostedServiceImpl {

    private static final String MODULE = HostedServiceImpl.class.getName();

    private HostedServiceImpl() {}

    public static Map<String, Object> guardCode(DispatchContext dctx, Map<String, ? extends Object> context) {
        return guard(HostedGuard.Rule.CODE, dctx, context);
    }

    public static Map<String, Object> guardCmsCode(DispatchContext dctx, Map<String, ? extends Object> context) {
        return guard(HostedGuard.Rule.CMS_CODE, dctx, context);
    }

    public static Map<String, Object> guardMail(DispatchContext dctx, Map<String, ? extends Object> context) {
        return guard(HostedGuard.Rule.MAIL_PARAMS, dctx, context);
    }

    public static Map<String, Object> guardCmsView(DispatchContext dctx, Map<String, ? extends Object> context) {
        return guard(HostedGuard.Rule.CMS_VIEW, dctx, context);
    }

    public static Map<String, Object> guardImport(DispatchContext dctx, Map<String, ? extends Object> context) {
        return guard(HostedGuard.Rule.IMPORT, dctx, context);
    }

    /** As the other guards, and the file lands under the store output folder (the same folder that createFileFromScreen picks). */
    public static Map<String, Object> guardScreenFile(DispatchContext dctx, Map<String, ? extends Object> context) {
        Map<String, Object> res = guard(HostedGuard.Rule.SCREEN_FILE, dctx, context);
        if (ServiceUtil.isError(res) || !HostedProfile.isHosted()) return res;
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        if (HostedGuard.isOperator(userLogin, dctx.getSecurity())) return res;
        String base = TenantFiles.scopePath(EntityUtilProperties.getPropertyValue("content", "content.output.path", "/output", dctx.getDelegator()), dctx.getDelegator());
        Object fileName = context.get("fileName");
        if (!HostedGuard.staysUnder(base, fileName != null ? fileName.toString() : null)) {
            return ServiceUtil.returnError("The file must stay in the store folder.");
        }
        return res;
    }

    private static Map<String, Object> guard(HostedGuard.Rule rule, DispatchContext dctx, Map<String, ? extends Object> context) {
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        String reason = HostedGuard.denyReason(rule, userLogin, context, dctx.getSecurity());
        if (reason == null) return ServiceUtil.returnSuccess();
        Debug.logWarning("Hosted profile: refused (" + rule + ") for login "
                + (userLogin != null ? userLogin.getString("userLoginId") : "(none)"), MODULE);
        return ServiceUtil.returnError(reason);
    }
}

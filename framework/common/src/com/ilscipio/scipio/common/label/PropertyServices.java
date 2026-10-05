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
package com.ilscipio.scipio.common.label;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.DistributedCacheClear;
import org.ofbiz.service.ServiceContext;
import org.ofbiz.service.ServiceUtil;

import java.util.Map;

/**
 * Property and label services (SCIPIO).
 * NOTE: Does not support tenant delegator.
 */
public class PropertyServices {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    public static Map<String, Object> updateLocalizedProperty(ServiceContext ctx) {
        boolean preventEmpty = ctx.attr("preventEmpty", true);
        Delegator delegator = ctx.delegator();
        try {
            GenericValue prop = delegator.from("LocalizedProperty").where("resourceId", ctx.attr("resourceId"),
                    "propertyId", ctx.attr("propertyId"), "lang", ctx.attr("lang")).queryOne();
            if (prop != null) {
                prop.setNonPKFields(ctx);
                if (preventEmpty && UtilValidate.isEmpty(prop.getString("value")) && !Boolean.TRUE.equals(prop.getBoolean("useEmpty"))) {
                    prop.remove();
                } else {
                    prop.store();
                }
            } else {
                prop = delegator.makeValidValue("LocalizedProperty", ctx);
                if (preventEmpty && UtilValidate.isEmpty(prop.getString("value")) && !Boolean.TRUE.equals(prop.getBoolean("useEmpty"))) {
                    ; // don't create the property
                } else {
                    prop.create();
                }
            }
            return ServiceUtil.returnSuccessReadOnly();
        } catch (GenericEntityException e) {
            Debug.logError(e, module);
            return ServiceUtil.returnError(e.toString());
        }
    }

    public static Map<String, Object> updateLocalizedPropertyOptional(ServiceContext ctx) {
        if (UtilValidate.isEmpty((String) ctx.get("lang"))) {
            return ServiceUtil.returnSuccessReadOnly();
        }
        return updateLocalizedProperty(ctx);
    }

    public static Map<String, Object> clearLocalizedPropertyCaches(ServiceContext ctx) {
        String resourceId = ctx.attr("resourceId");
        UtilProperties.clearCachesForResourceBundle(resourceId);
        if (Boolean.TRUE.equals(ctx.attr("distribute"))) {
            DistributedCacheClear dcc = ctx.delegator().getDistributedCacheClear();
            if (dcc != null) {
                Map<String, Object> distCtx = UtilMisc.toMap("resourceId", resourceId);
                dcc.runDistributedService("distributedClearLocalizedPropertyCaches", distCtx);
            }
        }
        return ServiceUtil.returnSuccessReadOnly();
    }
}

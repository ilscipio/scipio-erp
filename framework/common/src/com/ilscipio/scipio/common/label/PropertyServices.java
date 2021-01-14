/*******************************************************************************
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *******************************************************************************/
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

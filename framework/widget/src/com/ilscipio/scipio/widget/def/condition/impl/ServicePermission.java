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
package com.ilscipio.scipio.widget.def.condition.impl;

import java.util.HashMap;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that checks permission via a service call.
 *
 * <p>Params: [serviceName, mainAction?, contextMap?, resourceDescription?]</p>
 * <ul>
 *   <li>serviceName (required): The permission service to call</li>
 *   <li>mainAction (optional): ADMIN, CREATE, UPDATE, DELETE, or VIEW</li>
 *   <li>contextMap (optional): Context field containing service parameters</li>
 *   <li>resourceDescription (optional): Resource description (defaults to serviceName)</li>
 * </ul>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ServicePermission implements WidgetCondition {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private String serviceName;
    private String mainAction;
    private String contextMap;
    private String resourceDescription;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 1) {
            throw new IllegalArgumentException("ServicePermission requires at least 1 parameter: [serviceName]");
        }
        this.serviceName = params[0];
        this.mainAction = params.length > 1 ? params[1] : null;
        this.contextMap = params.length > 2 ? params[2] : null;
        this.resourceDescription = params.length > 3 ? params[3] : serviceName;
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        LocalDispatcher dispatcher = (LocalDispatcher) context.get("dispatcher");
        GenericValue userLogin = (GenericValue) context.get("userLogin");

        if (dispatcher == null || userLogin == null) {
            return false;
        }

        try {
            // Build service context
            Map<String, Object> serviceContext = new HashMap<>();
            serviceContext.put("userLogin", userLogin);

            if (UtilValidate.isNotEmpty(mainAction)) {
                serviceContext.put("mainAction", mainAction);
            }
            if (UtilValidate.isNotEmpty(resourceDescription)) {
                serviceContext.put("resourceDescription", resourceDescription);
            }

            // Add parameters from contextMap if specified
            if (UtilValidate.isNotEmpty(contextMap)) {
                Object ctxMapObj = context.get(contextMap);
                if (ctxMapObj instanceof Map) {
                    serviceContext.putAll(UtilGenerics.<String, Object>checkMap(ctxMapObj));
                }
            }

            // Call permission service
            Map<String, Object> result = dispatcher.runSync(serviceName, serviceContext);

            if (ServiceUtil.isError(result)) {
                Debug.logWarning("Service permission check failed for [" + serviceName + "]: " +
                        ServiceUtil.getErrorMessage(result), module);
                return false;
            }

            // Check hasPermission result
            Boolean hasPermission = (Boolean) result.get("hasPermission");
            return Boolean.TRUE.equals(hasPermission);

        } catch (Exception e) {
            Debug.logError(e, "Error checking service permission [" + serviceName + "]", module);
            return false;
        }
    }
}

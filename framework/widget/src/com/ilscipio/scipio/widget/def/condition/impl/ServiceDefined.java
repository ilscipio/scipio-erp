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

import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.string.FlexibleStringExpander;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ModelService;

import com.ilscipio.scipio.widget.def.condition.WidgetCondition;

/**
 * Condition that checks if a service is defined.
 *
 * <p>Params: [serviceName]</p>
 *
 * <p>Returns true if the service exists in the service model.</p>
 *
 * <p>SCIPIO: 4.0.0: Added.</p>
 */
public class ServiceDefined implements WidgetCondition {

    private static final Debug.OfbizLogger module = Debug.getOfbizLogger(java.lang.invoke.MethodHandles.lookup().lookupClass());

    private String serviceName;
    private FlexibleStringExpander serviceExpander;

    @Override
    public void init(String[] params) {
        if (params == null || params.length < 1) {
            throw new IllegalArgumentException("ServiceDefined requires 1 parameter: [serviceName]");
        }
        this.serviceName = params[0];
        this.serviceExpander = FlexibleStringExpander.getInstance(serviceName);
    }

    @Override
    public boolean evaluate(Map<String, Object> context) {
        try {
            String expandedName = serviceExpander.expandString(context);

            LocalDispatcher dispatcher = (LocalDispatcher) context.get("dispatcher");
            if (dispatcher == null) {
                Debug.logWarning("ServiceDefined: dispatcher not found in context", module);
                return false;
            }

            ModelService modelService = dispatcher.getDispatchContext().getModelService(expandedName);
            return modelService != null;

        } catch (Exception e) {
            Debug.logVerbose("ServiceDefined: service not found or error: " + serviceName + " - " + e.getMessage(), module);
            return false;
        }
    }
}

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
package com.ilscipio.scipio.manufacturing.fabrication;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Fabrication order events: creating a new production run directly inside a fabrication order.
 *
 * <p>SCIPIO: 4.0.0: New.</p>
 */
public class FabricationEvents {

    private static final String MODULE = FabricationEvents.class.getName();

    private FabricationEvents() {}

    /**
     * Creates a production run (via the existing createProductionRun service) and immediately adds it
     * to the given fabrication order (via addProductionRunToFabricationOrder).
     */
    public static String createProductionRunInFabricationOrder(HttpServletRequest request, HttpServletResponse response) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> params = UtilHttp.getCombinedMap(request);

        String fabricationOrderId = (String) params.get("fabricationOrderId");
        if (UtilValidate.isEmpty(fabricationOrderId)) {
            request.setAttribute("_ERROR_MESSAGE_", "fabricationOrderId is required");
            return "error";
        }

        try {
            Map<String, Object> createCtx = new HashMap<>();
            createCtx.put("productId", params.get("productId"));
            createCtx.put("pRQuantity", toBigDecimal(params.get("quantity")));
            createCtx.put("startDate", toTimestamp(params.get("startDate")));
            createCtx.put("facilityId", params.get("facilityId"));
            createCtx.put("routingId", params.get("routingId"));
            createCtx.put("workEffortName", params.get("workEffortName"));
            createCtx.put("userLogin", userLogin);
            Map<String, Object> createResult = dispatcher.runSync("createProductionRun", createCtx);
            if (ServiceUtil.isError(createResult)) {
                request.setAttribute("_ERROR_MESSAGE_", ServiceUtil.getErrorMessage(createResult));
                return "error";
            }
            String productionRunId = (String) createResult.get("productionRunId");

            Map<String, Object> addCtx = new HashMap<>();
            addCtx.put("fabricationOrderId", fabricationOrderId);
            addCtx.put("productionRunId", productionRunId);
            addCtx.put("userLogin", userLogin);
            Map<String, Object> addResult = dispatcher.runSync("addProductionRunToFabricationOrder", addCtx);
            if (ServiceUtil.isError(addResult)) {
                request.setAttribute("_ERROR_MESSAGE_", ServiceUtil.getErrorMessage(addResult));
                return "error";
            }

            request.setAttribute("productionRunId", productionRunId);
            request.setAttribute("fabricationOrderId", fabricationOrderId);
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error creating production run in fabrication order: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }
        return "success";
    }

    private static BigDecimal toBigDecimal(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof BigDecimal) {
            return (BigDecimal) value;
        }
        String str = value.toString().trim();
        if (str.isEmpty()) {
            return null;
        }
        return new BigDecimal(str);
    }

    private static Timestamp toTimestamp(Object value) {
        if (value == null) {
            return null;
        }
        if (value instanceof Timestamp) {
            return (Timestamp) value;
        }
        String str = value.toString().trim();
        if (str.isEmpty()) {
            return null;
        }
        return Timestamp.valueOf(str);
    }

}

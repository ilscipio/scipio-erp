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
package com.ilscipio.scipio.manufacturing.shopfloor;

import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilHttp;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Shop floor task declaration events, used by the ShopFloor screen to declare produced/rejected
 * quantities and setup/task time for a production run task.
 *
 * <p>SCIPIO: 4.0.0: Added for the manufacturing shop floor screen.</p>
 */
public class ShopFloorEvents {

    private static final String MODULE = ShopFloorEvents.class.getName();
    private static final String RESOURCE = "ManufacturingUiLabels";

    private ShopFloorEvents() {}

    /**
     * Declares produced/rejected quantity and setup/task time for a production run task
     * (updateProductionRunTask), and, when a quantity was rejected, records a rejection reason
     * (declareProductionRunTaskReject). Used by the shopFloorDeclareTask and
     * updateProductionRunTaskDeclaration requests.
     */
    public static String declareTask(HttpServletRequest request, HttpServletResponse response) {
        Delegator delegator = (Delegator) request.getAttribute("delegator");
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> params = UtilHttp.getCombinedMap(request);

        String productionRunId = (String) params.get("productionRunId");
        String workEffortId = (String) params.get("workEffortId");

        BigDecimal quantityProduced;
        BigDecimal quantityRejected;
        BigDecimal setupMillis;
        BigDecimal taskMillis;
        try {
            quantityProduced = toBigDecimal(params.get("quantityProduced"));
            quantityRejected = toBigDecimal(params.get("quantityRejected"));
            setupMillis = toMillis(params.get("setupMinutes"));
            taskMillis = toMillis(params.get("taskMinutes"));
        } catch (Exception e) {
            Debug.logWarning(e, "Invalid numeric input for shop floor task declaration: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunQuantityNotCorrect", locale));
            return "error";
        }

        String reasonEnumId = (String) params.get("reasonEnumId");
        String comments = (String) params.get("comments");
        boolean issueRequiredComponents = "Y".equals(params.get("issueRequiredComponents"));

        Map<String, Object> updateContext = new HashMap<>();
        updateContext.put("productionRunId", productionRunId);
        updateContext.put("productionRunTaskId", workEffortId);
        updateContext.put("addQuantityProduced", quantityProduced);
        updateContext.put("addSetupTime", setupMillis);
        updateContext.put("addTaskTime", taskMillis);
        updateContext.put("comments", comments);
        updateContext.put("issueRequiredComponents", Boolean.valueOf(issueRequiredComponents));
        updateContext.put("userLogin", userLogin);

        try {
            Map<String, Object> updateResult = dispatcher.runSync("updateProductionRunTask", updateContext);
            if (ServiceUtil.isError(updateResult)) {
                request.setAttribute("_ERROR_MESSAGE_", ServiceUtil.getErrorMessage(updateResult));
                return "error";
            }
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling updateProductionRunTask: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
            return "error";
        }

        if (quantityRejected != null && quantityRejected.compareTo(BigDecimal.ZERO) > 0) {
            if (UtilValidate.isEmpty(reasonEnumId)) {
                request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(RESOURCE, "ManufacturingRejectReasonRequired", locale));
                return "error";
            }
            Map<String, Object> rejectContext = new HashMap<>();
            rejectContext.put("productionRunId", productionRunId);
            rejectContext.put("workEffortId", workEffortId);
            rejectContext.put("quantity", quantityRejected);
            rejectContext.put("reasonEnumId", reasonEnumId);
            rejectContext.put("comments", comments);
            rejectContext.put("userLogin", userLogin);
            try {
                Map<String, Object> rejectResult = dispatcher.runSync("declareProductionRunTaskReject", rejectContext);
                if (ServiceUtil.isError(rejectResult)) {
                    request.setAttribute("_ERROR_MESSAGE_", ServiceUtil.getErrorMessage(rejectResult));
                    return "error";
                }
            } catch (GenericServiceException e) {
                Debug.logError(e, "Error calling declareProductionRunTaskReject: " + e.getMessage(), MODULE);
                request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
                return "error";
            }
        }

        request.setAttribute("_EVENT_MESSAGE_", UtilProperties.getMessage("CommonUiLabels", "CommonServiceSuccessMessage", locale));
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

    private static BigDecimal toMillis(Object value) {
        BigDecimal minutes = toBigDecimal(value);
        if (minutes == null) {
            return null;
        }
        return minutes.multiply(BigDecimal.valueOf(60000));
    }
}

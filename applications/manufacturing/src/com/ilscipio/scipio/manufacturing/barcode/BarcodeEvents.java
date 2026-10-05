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
package com.ilscipio.scipio.manufacturing.barcode;

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
import org.ofbiz.entity.GenericValue;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Barcode/QR scan events for the shop floor ScanTask screen.
 *
 * <p>SCIPIO: 4.0.0: Added for the barcode capture feature.</p>
 */
public class BarcodeEvents {

    private static final String MODULE = BarcodeEvents.class.getName();
    private static final String RESOURCE = "ManufacturingUiLabels";

    private BarcodeEvents() {}

    /**
     * Reads the scan form (scanCode, scanAction, quantity, lotId, comments), calls
     * recordProductionRunScan, and exposes the result to the ScanTask screen while keeping the
     * scanned code and chosen action in the form fields.
     */
    public static String scanTaskCode(HttpServletRequest request, HttpServletResponse response) {
        LocalDispatcher dispatcher = (LocalDispatcher) request.getAttribute("dispatcher");
        Locale locale = UtilHttp.getLocale(request);
        GenericValue userLogin = (GenericValue) request.getSession().getAttribute("userLogin");
        Map<String, Object> params = UtilHttp.getCombinedMap(request);

        String scanCode = (String) params.get("scanCode");
        String scanAction = (String) params.get("scanAction");
        if (UtilValidate.isEmpty(scanAction)) {
            scanAction = "INFO";
        }
        String lotId = (String) params.get("lotId");
        String comments = (String) params.get("comments");

        // SCIPIO: keep the scanned code and chosen action in the field on every outcome
        request.setAttribute("scanCode", scanCode);
        request.setAttribute("scanAction", scanAction);

        if (UtilValidate.isEmpty(scanCode)) {
            request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(RESOURCE, "ManufacturingScanCodeRequired", locale));
            return "success";
        }

        BigDecimal quantity;
        try {
            quantity = toBigDecimal(params.get("quantity"));
        } catch (Exception e) {
            request.setAttribute("_ERROR_MESSAGE_", UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunQuantityNotCorrect", locale));
            return "success";
        }

        Map<String, Object> ctx = new HashMap<>();
        ctx.put("scanCode", scanCode);
        ctx.put("scanAction", scanAction);
        ctx.put("quantity", quantity);
        ctx.put("lotId", lotId);
        ctx.put("comments", comments);
        ctx.put("userLogin", userLogin);

        try {
            Map<String, Object> result = dispatcher.runSync("recordProductionRunScan", ctx);
            request.setAttribute("scanResult", result);
            if (ServiceUtil.isError(result)) {
                request.setAttribute("_ERROR_MESSAGE_", ServiceUtil.getErrorMessage(result));
            } else {
                request.setAttribute("_EVENT_MESSAGE_", (String) result.get("message"));
            }
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling recordProductionRunScan: " + e.getMessage(), MODULE);
            request.setAttribute("_ERROR_MESSAGE_", e.getMessage());
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
}

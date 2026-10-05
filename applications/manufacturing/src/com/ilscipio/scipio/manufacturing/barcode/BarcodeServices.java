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
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.ofbiz.base.util.Debug;
import org.ofbiz.base.util.UtilDateTime;
import org.ofbiz.base.util.UtilGenerics;
import org.ofbiz.base.util.UtilMisc;
import org.ofbiz.base.util.UtilProperties;
import org.ofbiz.base.util.UtilValidate;
import org.ofbiz.entity.Delegator;
import org.ofbiz.entity.GenericEntityException;
import org.ofbiz.entity.GenericValue;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Barcode/QR scan services: records shop floor scans and performs the requested task action.
 *
 * <p>SCIPIO: 4.0.0: Added for the barcode capture feature.</p>
 */
public class BarcodeServices {

    private static final String MODULE = BarcodeServices.class.getName();
    private static final String RESOURCE = "ManufacturingUiLabels";
    private static final String CODE_PREFIX = "PRUN:";

    private BarcodeServices() {}

    /**
     * Parses the scanned code, resolves the task/run, performs the requested action (START,
     * COMPLETE, PRODUCE, or INFO), and always records a ProductionRunScan row - including a
     * failed action, with the error appended to comments.
     */
    public static Map<String, Object> recordProductionRunScan(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        String scanCode = (String) parameters.get("scanCode");
        String scanAction = (String) parameters.get("scanAction");
        if (UtilValidate.isEmpty(scanAction)) {
            scanAction = "INFO";
        }
        BigDecimal quantity = (BigDecimal) parameters.get("quantity");
        String lotId = (String) parameters.get("lotId");
        String comments = (String) parameters.get("comments");

        String productionRunId = null;
        String workEffortId = null;
        String taskName = null;
        String productId = null;
        String productName = null;
        String statusId = null;
        String message = null;
        String errorMessage = null;

        GenericValue task = null;
        GenericValue run = null;

        try {
            String code = scanCode != null ? scanCode.trim() : "";
            if (code.isEmpty()) {
                errorMessage = UtilProperties.getMessage(RESOURCE, "ManufacturingScanCodeRequired", locale);
            } else {
                String parsedRunId = null;
                String parsedTaskId = null;
                if (code.startsWith(CODE_PREFIX)) {
                    String[] parts = code.substring(CODE_PREFIX.length()).split(":");
                    if (parts.length >= 1 && UtilValidate.isNotEmpty(parts[0])) {
                        parsedRunId = parts[0].trim();
                    }
                    if (parts.length >= 2 && UtilValidate.isNotEmpty(parts[1])) {
                        parsedTaskId = parts[1].trim();
                    }
                } else {
                    // SCIPIO: accept a bare task work effort id, as scanned/typed without the PRUN: prefix
                    parsedTaskId = code;
                }

                if (UtilValidate.isNotEmpty(parsedTaskId)) {
                    task = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", parsedTaskId).queryOne();
                    if (task == null) {
                        errorMessage = UtilProperties.getMessage(RESOURCE, "ManufacturingScanTaskNotFound",
                                UtilMisc.toMap("workEffortId", parsedTaskId), locale);
                    } else {
                        workEffortId = task.getString("workEffortId");
                        taskName = task.getString("workEffortName");
                        productionRunId = task.getString("workEffortParentId");
                        statusId = task.getString("currentStatusId");
                    }
                } else {
                    productionRunId = parsedRunId;
                }

                if (errorMessage == null) {
                    if (UtilValidate.isNotEmpty(productionRunId)) {
                        run = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", productionRunId).queryOne();
                        if (run == null) {
                            errorMessage = UtilProperties.getMessage(RESOURCE, "ManufacturingScanRunNotFound",
                                    UtilMisc.toMap("productionRunId", productionRunId), locale);
                        } else if (task == null) {
                            statusId = run.getString("currentStatusId");
                        }
                    } else {
                        errorMessage = UtilProperties.getMessage(RESOURCE, "ManufacturingScanCodeNotRecognized", locale);
                    }
                }

                if (errorMessage == null && run != null) {
                    GenericValue producedGood = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                            .where("workEffortId", productionRunId, "workEffortGoodStdTypeId", "PRUN_PROD_DELIV").queryFirst();
                    if (producedGood != null) {
                        productId = producedGood.getString("productId");
                        GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).queryOne();
                        productName = product != null ? product.getString("internalName") : null;
                    }
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error resolving scanned code '" + scanCode + "': " + e.getMessage(), MODULE);
            errorMessage = e.getMessage();
        }

        if (errorMessage == null && task == null
                && ("START".equals(scanAction) || "COMPLETE".equals(scanAction) || "PRODUCE".equals(scanAction))) {
            errorMessage = UtilProperties.getMessage(RESOURCE, "ManufacturingScanTaskRequired", locale);
        }

        if (errorMessage == null) {
            try {
                if ("START".equals(scanAction)) {
                    Map<String, Object> ctx = new HashMap<>();
                    ctx.put("productionRunId", productionRunId);
                    ctx.put("workEffortId", workEffortId);
                    ctx.put("statusId", "PRUN_RUNNING");
                    ctx.put("userLogin", userLogin);
                    Map<String, Object> res = dispatcher.runSync("changeProductionRunTaskStatus", ctx);
                    if (ServiceUtil.isError(res)) {
                        errorMessage = ServiceUtil.getErrorMessage(res);
                    } else {
                        statusId = (String) res.get("newStatusId");
                        message = UtilProperties.getMessage(RESOURCE, "ManufacturingScanTaskStarted", locale);
                    }
                } else if ("COMPLETE".equals(scanAction)) {
                    Map<String, Object> ctx = new HashMap<>();
                    ctx.put("productionRunId", productionRunId);
                    ctx.put("workEffortId", workEffortId);
                    ctx.put("statusId", "PRUN_COMPLETED");
                    ctx.put("userLogin", userLogin);
                    Map<String, Object> res = dispatcher.runSync("changeProductionRunTaskStatus", ctx);
                    if (ServiceUtil.isError(res)) {
                        errorMessage = ServiceUtil.getErrorMessage(res);
                    } else {
                        statusId = (String) res.get("newStatusId");
                        message = UtilProperties.getMessage(RESOURCE, "ManufacturingScanTaskCompleted", locale);
                    }
                } else if ("PRODUCE".equals(scanAction)) {
                    if (quantity == null || quantity.compareTo(BigDecimal.ZERO) <= 0) {
                        errorMessage = UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunQuantityNotCorrect", locale);
                    } else if (UtilValidate.isEmpty(productId)) {
                        errorMessage = UtilProperties.getMessage(RESOURCE, "ManufacturingScanProductNotFound", locale);
                    } else {
                        Map<String, Object> ctx = new HashMap<>();
                        ctx.put("workEffortId", workEffortId);
                        ctx.put("productId", productId);
                        ctx.put("quantity", quantity);
                        ctx.put("lotId", lotId);
                        ctx.put("facilityId", run != null ? run.getString("facilityId") : null);
                        ctx.put("userLogin", userLogin);
                        Map<String, Object> res = dispatcher.runSync("productionRunTaskProduce", ctx);
                        if (ServiceUtil.isError(res)) {
                            errorMessage = ServiceUtil.getErrorMessage(res);
                        } else {
                            message = UtilProperties.getMessage(RESOURCE, "ManufacturingScanTaskProduced", locale);
                            GenericValue refreshed = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", workEffortId).queryOne();
                            if (refreshed != null) {
                                statusId = refreshed.getString("currentStatusId");
                            }
                        }
                    }
                } else {
                    message = UtilProperties.getMessage(RESOURCE, "ManufacturingScanInfoOnly", locale);
                }
            } catch (GenericServiceException | GenericEntityException e) {
                Debug.logError(e, "Error performing scan action '" + scanAction + "': " + e.getMessage(), MODULE);
                errorMessage = e.getMessage();
            }
        }

        // SCIPIO: always store the scan, even a failed action - the error goes into comments
        String finalComments = comments;
        if (errorMessage != null) {
            finalComments = UtilValidate.isNotEmpty(comments) ? (comments + " | ERROR: " + errorMessage) : ("ERROR: " + errorMessage);
        }
        String scanId = delegator.getNextSeqId("ProductionRunScan");
        try {
            GenericValue scan = delegator.makeValue("ProductionRunScan");
            scan.set("scanId", scanId);
            scan.set("productionRunId", productionRunId);
            scan.set("workEffortId", workEffortId);
            scan.set("scanCode", scanCode);
            scan.set("scanAction", scanAction);
            scan.set("lotId", lotId);
            scan.set("quantity", quantity);
            scan.set("scanDate", UtilDateTime.nowTimestamp());
            scan.set("userLoginId", userLogin != null ? userLogin.getString("userLoginId") : null);
            scan.set("comments", finalComments);
            delegator.create(scan);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error creating ProductionRunScan record: " + e.getMessage(), MODULE);
            if (errorMessage == null) {
                errorMessage = e.getMessage();
            }
        }

        Map<String, Object> result = (errorMessage != null) ? ServiceUtil.returnError(errorMessage) : ServiceUtil.returnSuccess(message);
        result.put("scanId", scanId);
        result.put("productionRunId", productionRunId);
        result.put("workEffortId", workEffortId);
        result.put("taskName", taskName);
        result.put("productId", productId);
        result.put("productName", productName);
        result.put("statusId", statusId);
        result.put("message", errorMessage != null ? errorMessage : message);
        return result;
    }

    /** Returns the most recent ProductionRunScan records for a production run and/or task, newest first. */
    public static Map<String, Object> getProductionRunScans(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_VIEW", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingViewPermissionError", locale));
        }

        String productionRunId = (String) parameters.get("productionRunId");
        String workEffortId = (String) parameters.get("workEffortId");
        Integer limit = (Integer) parameters.get("limit");
        if (limit == null || limit <= 0) {
            limit = 20;
        }

        List<GenericValue> scans = new ArrayList<>();
        if (UtilValidate.isNotEmpty(productionRunId) || UtilValidate.isNotEmpty(workEffortId)) {
            Map<String, Object> cond = new HashMap<>();
            if (UtilValidate.isNotEmpty(productionRunId)) {
                cond.put("productionRunId", productionRunId);
            }
            if (UtilValidate.isNotEmpty(workEffortId)) {
                cond.put("workEffortId", workEffortId);
            }
            try {
                List<GenericValue> all = EntityQuery.use(delegator).from("ProductionRunScan")
                        .where(cond).orderBy("-scanDate").queryList();
                for (GenericValue scan : all) {
                    if (scans.size() >= limit) {
                        break;
                    }
                    scans.add(scan);
                }
            } catch (GenericEntityException e) {
                Debug.logError(e, "Error looking up ProductionRunScan records: " + e.getMessage(), MODULE);
                return ServiceUtil.returnError(e.getMessage());
            }
        }

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("scans", scans);
        return result;
    }

}

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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import org.ofbiz.base.util.Debug;
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
 * Fabrication order services: a fabrication order (WorkEffort of type FAB_ORDER) groups several
 * production runs (linked through WorkEffort.workEffortParentId on the run header) so a planner can
 * see their combined quantity, planned time, machines, and status, and move them through statuses
 * together.
 *
 * <p>SCIPIO: 4.0.0: New.</p>
 */
public class FabricationServices {

    private static final String MODULE = FabricationServices.class.getName();
    private static final String RESOURCE = "ManufacturingUiLabels";

    /** Status ranks used to derive the fabrication order's overall status; PRUN_CANCELLED is treated as terminal (ranked last). */
    private static final List<String> STATUS_ORDER = Arrays.asList(
            "PRUN_CREATED", "PRUN_SCHEDULED", "PRUN_DOC_PRINTED", "PRUN_RUNNING", "PRUN_COMPLETED", "PRUN_CLOSED");

    private FabricationServices() {}

    /** Creates a fabrication order header (WorkEffort of type FAB_ORDER). */
    public static Map<String, Object> createFabricationOrder(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingUpdatePermissionError", locale));
        }

        String fabricationOrderId = delegator.getNextSeqId("WorkEffort");
        GenericValue fabOrder = delegator.makeValue("WorkEffort");
        fabOrder.set("workEffortId", fabricationOrderId);
        fabOrder.set("workEffortTypeId", "FAB_ORDER");
        fabOrder.set("currentStatusId", "PRUN_CREATED");
        fabOrder.set("workEffortName", parameters.get("workEffortName"));
        fabOrder.set("description", parameters.get("description"));
        fabOrder.set("facilityId", parameters.get("facilityId"));
        fabOrder.set("estimatedStartDate", parameters.get("estimatedStartDate"));
        fabOrder.set("estimatedCompletionDate", parameters.get("estimatedCompletionDate"));
        try {
            delegator.create(fabOrder);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error creating fabrication order: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("fabricationOrderId", fabricationOrderId);
        return result;
    }

    /** Updates a fabrication order header. */
    public static Map<String, Object> updateFabricationOrder(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingUpdatePermissionError", locale));
        }

        String fabricationOrderId = (String) parameters.get("fabricationOrderId");
        try {
            GenericValue fabOrder = EntityQuery.use(delegator).from("WorkEffort")
                    .where("workEffortId", fabricationOrderId, "workEffortTypeId", "FAB_ORDER").queryOne();
            if (fabOrder == null) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingFabricationOrderNotFound", locale) + " " + fabricationOrderId);
            }
            if (parameters.get("workEffortName") != null) {
                fabOrder.set("workEffortName", parameters.get("workEffortName"));
            }
            if (parameters.get("description") != null) {
                fabOrder.set("description", parameters.get("description"));
            }
            if (parameters.get("facilityId") != null) {
                fabOrder.set("facilityId", parameters.get("facilityId"));
            }
            if (parameters.get("estimatedStartDate") != null) {
                fabOrder.set("estimatedStartDate", parameters.get("estimatedStartDate"));
            }
            if (parameters.get("estimatedCompletionDate") != null) {
                fabOrder.set("estimatedCompletionDate", parameters.get("estimatedCompletionDate"));
            }
            delegator.store(fabOrder);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error updating fabrication order: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

    /** Adds an existing production run to a fabrication order by setting the run header's workEffortParentId. */
    public static Map<String, Object> addProductionRunToFabricationOrder(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingUpdatePermissionError", locale));
        }

        String fabricationOrderId = (String) parameters.get("fabricationOrderId");
        String productionRunId = (String) parameters.get("productionRunId");
        try {
            GenericValue fabOrder = EntityQuery.use(delegator).from("WorkEffort")
                    .where("workEffortId", fabricationOrderId, "workEffortTypeId", "FAB_ORDER").queryOne();
            if (fabOrder == null) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingFabricationOrderNotFound", locale) + " " + fabricationOrderId);
            }
            GenericValue run = EntityQuery.use(delegator).from("WorkEffort")
                    .where("workEffortId", productionRunId, "workEffortTypeId", "PROD_ORDER_HEADER").queryOne();
            if (run == null) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingFabricationOrderProductionRunNotFound", locale) + " " + productionRunId);
            }
            run.set("workEffortParentId", fabricationOrderId);
            delegator.store(run);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error adding production run to fabrication order: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

    /** Removes a production run from its fabrication order (clears the run header's workEffortParentId). */
    public static Map<String, Object> removeProductionRunFromFabricationOrder(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingUpdatePermissionError", locale));
        }

        String productionRunId = (String) parameters.get("productionRunId");
        try {
            GenericValue run = EntityQuery.use(delegator).from("WorkEffort")
                    .where("workEffortId", productionRunId, "workEffortTypeId", "PROD_ORDER_HEADER").queryOne();
            if (run == null) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingFabricationOrderProductionRunNotFound", locale) + " " + productionRunId);
            }
            run.set("workEffortParentId", null);
            delegator.store(run);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error removing production run from fabrication order: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
        return ServiceUtil.returnSuccess();
    }

    /** Returns the fabrication order header, its production runs (with product/quantity/status), and totals computed over them. */
    public static Map<String, Object> getFabricationOrder(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_VIEW", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingViewPermissionError", locale));
        }

        String fabricationOrderId = (String) parameters.get("fabricationOrderId");
        try {
            GenericValue fabOrder = EntityQuery.use(delegator).from("WorkEffort")
                    .where("workEffortId", fabricationOrderId, "workEffortTypeId", "FAB_ORDER").queryOne();
            if (fabOrder == null) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingFabricationOrderNotFound", locale) + " " + fabricationOrderId);
            }

            Map<String, Object> header = new HashMap<>(fabOrder.getAllFields());

            List<GenericValue> runEntities = EntityQuery.use(delegator).from("WorkEffort")
                    .where("workEffortParentId", fabricationOrderId, "workEffortTypeId", "PROD_ORDER_HEADER")
                    .orderBy("estimatedStartDate").queryList();

            List<Map<String, Object>> runs = new ArrayList<>();
            BigDecimal totalQuantity = BigDecimal.ZERO;
            long plannedMinutes = 0L;
            Timestamp earliestStart = null;
            Timestamp latestCompletion = null;
            Map<String, Integer> statusCounts = new LinkedHashMap<>();
            Set<String> workCenters = new LinkedHashSet<>();

            for (GenericValue run : runEntities) {
                String runId = run.getString("workEffortId");
                GenericValue producedGood = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                        .where("workEffortId", runId, "workEffortGoodStdTypeId", "PRUN_PROD_DELIV").queryFirst();
                String productId = producedGood != null ? producedGood.getString("productId") : null;

                Map<String, Object> runMap = new LinkedHashMap<>();
                runMap.put("productionRunId", runId);
                runMap.put("workEffortName", run.get("workEffortName"));
                runMap.put("productId", productId);
                runMap.put("quantityToProduce", run.get("quantityToProduce"));
                runMap.put("quantityProduced", run.get("quantityProduced"));
                runMap.put("currentStatusId", run.get("currentStatusId"));
                runMap.put("estimatedStartDate", run.get("estimatedStartDate"));
                runMap.put("estimatedCompletionDate", run.get("estimatedCompletionDate"));
                runs.add(runMap);

                BigDecimal quantityToProduce = run.getBigDecimal("quantityToProduce");
                if (quantityToProduce != null) {
                    totalQuantity = totalQuantity.add(quantityToProduce);
                }
                Timestamp start = run.getTimestamp("estimatedStartDate");
                if (start != null && (earliestStart == null || start.before(earliestStart))) {
                    earliestStart = start;
                }
                Timestamp completion = run.getTimestamp("estimatedCompletionDate");
                if (completion != null && (latestCompletion == null || completion.after(latestCompletion))) {
                    latestCompletion = completion;
                }
                String statusId = run.getString("currentStatusId");
                statusCounts.merge(statusId, 1, Integer::sum);

                List<GenericValue> tasks = EntityQuery.use(delegator).from("WorkEffort")
                        .where("workEffortParentId", runId, "workEffortTypeId", "PROD_ORDER_TASK").queryList();
                for (GenericValue task : tasks) {
                    Double setupMillis = task.getDouble("estimatedSetupMillis");
                    Double runMillis = task.getDouble("estimatedMilliSeconds");
                    double millis = (setupMillis != null ? setupMillis : 0.0) + (runMillis != null ? runMillis : 0.0);
                    plannedMinutes += Math.round(millis / 60000.0);
                    String fixedAssetId = task.getString("fixedAssetId");
                    if (UtilValidate.isNotEmpty(fixedAssetId)) {
                        workCenters.add(fixedAssetId);
                    }
                }
            }

            Map<String, Object> totals = new LinkedHashMap<>();
            totals.put("runCount", runs.size());
            totals.put("totalQuantity", totalQuantity);
            totals.put("plannedMinutes", plannedMinutes);
            totals.put("earliestStart", earliestStart);
            totals.put("latestCompletion", latestCompletion);
            totals.put("statusCounts", statusCounts);
            totals.put("workCenters", new ArrayList<>(workCenters));
            totals.put("derivedStatus", deriveStatus(runEntities));

            Map<String, Object> result = ServiceUtil.returnSuccess();
            result.put("header", header);
            result.put("runs", runs);
            result.put("totals", totals);
            return result;
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error looking up fabrication order: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }
    }

    /** Derives the fabrication order's overall status from the statuses of its runs. */
    private static String deriveStatus(List<GenericValue> runs) {
        if (runs.isEmpty()) {
            return "CREATED";
        }
        boolean allClosed = true;
        boolean allCompletedOrClosed = true;
        boolean anyRunning = false;
        boolean allScheduledOrLater = true;
        for (GenericValue run : runs) {
            String statusId = run.getString("currentStatusId");
            if (!"PRUN_CLOSED".equals(statusId)) {
                allClosed = false;
            }
            if (!("PRUN_COMPLETED".equals(statusId) || "PRUN_CLOSED".equals(statusId))) {
                allCompletedOrClosed = false;
            }
            if ("PRUN_RUNNING".equals(statusId)) {
                anyRunning = true;
            }
            if (statusRank(statusId) < statusRank("PRUN_SCHEDULED")) {
                allScheduledOrLater = false;
            }
        }
        if (allClosed) {
            return "CLOSED";
        }
        if (allCompletedOrClosed) {
            return "COMPLETED";
        }
        if (anyRunning) {
            return "RUNNING";
        }
        if (allScheduledOrLater) {
            return "SCHEDULED";
        }
        return "CREATED";
    }

    /** Rank of a production run status for status-progression comparisons; unknown/cancelled statuses rank last (treated as "later"). */
    private static int statusRank(String statusId) {
        int idx = STATUS_ORDER.indexOf(statusId);
        return idx >= 0 ? idx : STATUS_ORDER.size();
    }

    /**
     * Moves every production run of a fabrication order that is not yet at the given status (skipping
     * closed and cancelled runs) to that status, via the existing quickChangeProductionRunStatus service.
     * Collects errors per run and continues with the rest.
     */
    public static Map<String, Object> changeFabricationOrderStatus(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingUpdatePermissionError", locale));
        }

        String fabricationOrderId = (String) parameters.get("fabricationOrderId");
        String statusId = (String) parameters.get("statusId");

        List<GenericValue> runEntities;
        try {
            runEntities = EntityQuery.use(delegator).from("WorkEffort")
                    .where("workEffortParentId", fabricationOrderId, "workEffortTypeId", "PROD_ORDER_HEADER")
                    .queryList();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error looking up fabrication order runs: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        int updatedCount = 0;
        List<String> errorMessageList = new ArrayList<>();
        for (GenericValue run : runEntities) {
            String runStatusId = run.getString("currentStatusId");
            if ("PRUN_CLOSED".equals(runStatusId) || "PRUN_CANCELLED".equals(runStatusId)) {
                continue;
            }
            if (statusId.equals(runStatusId)) {
                continue;
            }
            try {
                Map<String, Object> serviceContext = UtilMisc.toMap(
                        "productionRunId", run.getString("workEffortId"),
                        "statusId", statusId,
                        "userLogin", userLogin);
                Map<String, Object> serviceResult = dispatcher.runSync("quickChangeProductionRunStatus", serviceContext);
                if (ServiceUtil.isError(serviceResult)) {
                    errorMessageList.add(run.getString("workEffortId") + ": " + ServiceUtil.getErrorMessage(serviceResult));
                } else {
                    updatedCount++;
                }
            } catch (GenericServiceException e) {
                Debug.logError(e, "Error changing status of production run " + run.getString("workEffortId") + ": " + e.getMessage(), MODULE);
                errorMessageList.add(run.getString("workEffortId") + ": " + e.getMessage());
            }
        }

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("updatedCount", updatedCount);
        result.put("errorMessageList", errorMessageList);
        if (!errorMessageList.isEmpty()) {
            result.put("errorMessage", UtilProperties.getMessage(RESOURCE, "ManufacturingFabricationOrderStatusChangeErrors", locale)
                    + " " + String.join("; ", errorMessageList));
        }
        return result;
    }

}

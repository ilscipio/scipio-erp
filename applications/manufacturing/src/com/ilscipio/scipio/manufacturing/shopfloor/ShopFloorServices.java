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
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
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
import org.ofbiz.entity.condition.EntityCondition;
import org.ofbiz.entity.condition.EntityOperator;
import org.ofbiz.entity.util.EntityQuery;
import org.ofbiz.service.DispatchContext;
import org.ofbiz.service.GenericServiceException;
import org.ofbiz.service.LocalDispatcher;
import org.ofbiz.service.ServiceUtil;

/**
 * Shop floor services: production run task reject (scrap) declaration and shop floor task listing.
 *
 * <p>SCIPIO: 4.0.0: New.</p>
 */
public class ShopFloorServices {

    private static final String MODULE = ShopFloorServices.class.getName();
    private static final String RESOURCE = "ManufacturingUiLabels";

    private ShopFloorServices() {}

    /** Declares a rejected (scrapped) quantity on a running production run task and records the reason. */
    public static Map<String, Object> declareProductionRunTaskReject(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        LocalDispatcher dispatcher = dctx.getDispatcher();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingUpdatePermissionError", locale));
        }

        String productionRunId = (String) parameters.get("productionRunId");
        String workEffortId = (String) parameters.get("workEffortId");
        BigDecimal quantity = (BigDecimal) parameters.get("quantity");
        String reasonEnumId = (String) parameters.get("reasonEnumId");
        String lotId = (String) parameters.get("lotId");
        String comments = (String) parameters.get("comments");

        if (quantity == null || quantity.compareTo(BigDecimal.ZERO) <= 0) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunQuantityNotCorrect", locale));
        }

        GenericValue task;
        GenericValue enumeration;
        GenericValue producedGood;
        try {
            task = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", workEffortId).queryOne();
            if (task == null || !productionRunId.equals(task.getString("workEffortParentId"))) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunTaskNotFound",
                        UtilMisc.toMap("productionRunTaskId", workEffortId), locale));
            }
            if (!"PRUN_RUNNING".equals(task.getString("currentStatusId"))) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunTaskNotRunning",
                        UtilMisc.toMap("productionRunTaskId", workEffortId), locale));
            }
            enumeration = EntityQuery.use(delegator).from("Enumeration")
                    .where("enumId", reasonEnumId, "enumTypeId", "PRUN_REJECT_REASON").queryFirst();
            if (enumeration == null) {
                return ServiceUtil.returnError("Invalid rejection reasonEnumId: " + reasonEnumId);
            }
            producedGood = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                    .where("workEffortId", productionRunId, "workEffortGoodStdTypeId", "PRUN_PROD_DELIV").queryFirst();
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error looking up production run task or reject reason: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        String rejectSeqId = delegator.getNextSeqId("ProductionRunReject");
        GenericValue reject = delegator.makeValue("ProductionRunReject");
        reject.set("workEffortId", workEffortId);
        reject.set("rejectSeqId", rejectSeqId);
        reject.set("productionRunId", productionRunId);
        reject.set("productId", producedGood != null ? producedGood.getString("productId") : null);
        reject.set("quantity", quantity);
        reject.set("reasonEnumId", reasonEnumId);
        reject.set("lotId", lotId);
        reject.set("rejectDate", UtilDateTime.nowTimestamp());
        reject.set("comments", comments);
        reject.set("userLoginId", userLogin != null ? userLogin.getString("userLoginId") : null);
        try {
            delegator.create(reject);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error creating ProductionRunReject: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        Map<String, Object> updateCtx = new HashMap<>();
        updateCtx.put("productionRunId", productionRunId);
        updateCtx.put("productionRunTaskId", workEffortId);
        updateCtx.put("addQuantityRejected", quantity);
        updateCtx.put("userLogin", userLogin);
        try {
            Map<String, Object> updateResult = dispatcher.runSync("updateProductionRunTask", updateCtx);
            if (ServiceUtil.isError(updateResult)) {
                return ServiceUtil.returnError(ServiceUtil.getErrorMessage(updateResult));
            }
        } catch (GenericServiceException e) {
            Debug.logError(e, "Error calling updateProductionRunTask: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("rejectSeqId", rejectSeqId);
        return result;
    }

    /** Returns the rejected-quantity records for a production run and/or task, with reason descriptions and task names. */
    public static Map<String, Object> getProductionRunRejects(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_VIEW", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingViewPermissionError", locale));
        }

        String productionRunId = (String) parameters.get("productionRunId");
        String workEffortId = (String) parameters.get("workEffortId");
        if (UtilValidate.isEmpty(productionRunId) && UtilValidate.isEmpty(workEffortId)) {
            // SCIPIO: no key means nothing to list; screens call this before a run is chosen
            Map<String, Object> empty = ServiceUtil.returnSuccess();
            empty.put("rejects", new ArrayList<Map<String, Object>>());
            empty.put("totalRejected", BigDecimal.ZERO);
            return empty;
        }

        Map<String, Object> cond = new HashMap<>();
        if (UtilValidate.isNotEmpty(productionRunId)) {
            cond.put("productionRunId", productionRunId);
        }
        if (UtilValidate.isNotEmpty(workEffortId)) {
            cond.put("workEffortId", workEffortId);
        }

        List<Map<String, Object>> rejects = new ArrayList<>();
        BigDecimal totalRejected = BigDecimal.ZERO;
        try {
            List<GenericValue> rejectEntities = EntityQuery.use(delegator).from("ProductionRunReject")
                    .where(cond).orderBy("rejectDate").queryList();
            for (GenericValue rejectEntity : rejectEntities) {
                Map<String, Object> row = new HashMap<>(rejectEntity.getAllFields());
                GenericValue enumeration = EntityQuery.use(delegator).from("Enumeration")
                        .where("enumId", rejectEntity.getString("reasonEnumId")).queryFirst();
                row.put("reasonDescription", enumeration != null ? enumeration.getString("description") : null);
                GenericValue task = EntityQuery.use(delegator).from("WorkEffort")
                        .where("workEffortId", rejectEntity.getString("workEffortId")).queryOne();
                row.put("taskName", task != null ? task.getString("workEffortName") : null);
                rejects.add(row);
                BigDecimal quantity = rejectEntity.getBigDecimal("quantity");
                if (quantity != null) {
                    totalRejected = totalRejected.add(quantity);
                }
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error looking up ProductionRunReject records: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("rejects", rejects);
        result.put("totalRejected", totalRejected);
        return result;
    }

    /** Returns the active production run tasks for the shop floor, optionally filtered by work center or facility. */
    public static Map<String, Object> getShopFloorTasks(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_VIEW", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingViewPermissionError", locale));
        }

        String fixedAssetId = (String) parameters.get("fixedAssetId");
        String facilityId = (String) parameters.get("facilityId");
        Boolean includeCompleted = (Boolean) parameters.get("includeCompleted");
        if (includeCompleted == null) {
            includeCompleted = Boolean.FALSE;
        }

        List<String> statuses = new ArrayList<>(UtilMisc.toList("PRUN_SCHEDULED", "PRUN_DOC_PRINTED", "PRUN_RUNNING"));
        if (includeCompleted) {
            statuses.add("PRUN_COMPLETED");
        }

        List<EntityCondition> taskConds = new ArrayList<>();
        taskConds.add(EntityCondition.makeCondition("workEffortTypeId", "PROD_ORDER_TASK"));
        taskConds.add(EntityCondition.makeCondition("currentStatusId", EntityOperator.IN, statuses));

        List<Map<String, Object>> tasks = new ArrayList<>();
        Map<String, String> workCenterNames = new LinkedHashMap<>();
        try {
            // SCIPIO: a chosen fixedAssetId that is a production line (has member machines) includes the
            // line's own tasks plus the tasks of every member machine.
            if (UtilValidate.isNotEmpty(fixedAssetId)) {
                List<GenericValue> lineMembers = getLineMembers(delegator, fixedAssetId);
                if (UtilValidate.isNotEmpty(lineMembers)) {
                    List<String> lineFixedAssetIds = new ArrayList<>();
                    lineFixedAssetIds.add(fixedAssetId);
                    for (GenericValue member : lineMembers) {
                        lineFixedAssetIds.add(member.getString("fixedAssetId"));
                    }
                    taskConds.add(EntityCondition.makeCondition("fixedAssetId", EntityOperator.IN, lineFixedAssetIds));
                } else {
                    taskConds.add(EntityCondition.makeCondition("fixedAssetId", fixedAssetId));
                }
            }
            List<GenericValue> taskEntities = EntityQuery.use(delegator).from("WorkEffort")
                    .where(EntityCondition.makeCondition(taskConds))
                    .orderBy("fixedAssetId", "estimatedStartDate", "priority")
                    .queryList();

            for (GenericValue task : taskEntities) {
                String productionRunId = task.getString("workEffortParentId");
                if (UtilValidate.isEmpty(productionRunId)) {
                    continue;
                }
                GenericValue run = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", productionRunId).queryOne();
                if (run == null || "PRUN_CANCELLED".equals(run.getString("currentStatusId"))) {
                    continue;
                }
                if (UtilValidate.isNotEmpty(facilityId) && !facilityId.equals(run.getString("facilityId"))) {
                    continue;
                }
                String runStatusId = run.getString("currentStatusId");
                String taskStatusId = task.getString("currentStatusId");
                boolean runActive = "PRUN_DOC_PRINTED".equals(runStatusId) || "PRUN_RUNNING".equals(runStatusId);
                if (!runActive && !"PRUN_RUNNING".equals(taskStatusId)) {
                    continue;
                }

                String taskFixedAssetId = task.getString("fixedAssetId");
                if (UtilValidate.isNotEmpty(taskFixedAssetId) && !workCenterNames.containsKey(taskFixedAssetId)) {
                    GenericValue fixedAsset = EntityQuery.use(delegator).from("FixedAsset").where("fixedAssetId", taskFixedAssetId).queryOne();
                    workCenterNames.put(taskFixedAssetId, fixedAsset != null ? fixedAsset.getString("fixedAssetName") : null);
                }

                BigDecimal quantityToProduce = run.getBigDecimal("quantityToProduce");

                GenericValue producedGood = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                        .where("workEffortId", productionRunId, "workEffortGoodStdTypeId", "PRUN_PROD_DELIV").queryFirst();
                String productId = producedGood != null ? producedGood.getString("productId") : null;
                String productName = null;
                if (UtilValidate.isNotEmpty(productId)) {
                    GenericValue product = EntityQuery.use(delegator).from("Product").where("productId", productId).queryOne();
                    productName = product != null ? product.getString("internalName") : null;
                }

                Map<String, Object> row = new HashMap<>();
                row.put("workEffortId", task.get("workEffortId"));
                row.put("workEffortName", task.get("workEffortName"));
                row.put("currentStatusId", taskStatusId);
                row.put("fixedAssetId", taskFixedAssetId);
                row.put("fixedAssetName", workCenterNames.get(taskFixedAssetId));
                row.put("priority", task.get("priority"));
                row.put("estimatedStartDate", task.get("estimatedStartDate"));
                row.put("estimatedCompletionDate", task.get("estimatedCompletionDate"));
                row.put("actualStartDate", task.get("actualStartDate"));
                row.put("estimatedMinutes", computeMinutes(task.getDouble("estimatedSetupMillis"), task.getDouble("estimatedMilliSeconds"), quantityToProduce, true));
                row.put("actualMinutes", computeMinutes(task.getDouble("actualSetupMillis"), task.getDouble("actualMilliSeconds"), quantityToProduce, false));
                row.put("quantityProduced", task.get("quantityProduced"));
                row.put("quantityRejected", task.get("quantityRejected"));
                row.put("productionRunId", productionRunId);
                row.put("productionRunName", run.get("workEffortName"));
                row.put("runStatusId", runStatusId);
                row.put("productId", productId);
                row.put("productName", productName);
                row.put("quantityToProduce", quantityToProduce);
                row.put("runQuantityProduced", run.get("quantityProduced"));
                row.put("currentFixedAssetId", taskFixedAssetId);
                row.put("currentFixedAssetName", workCenterNames.get(taskFixedAssetId));
                row.put("machineOptions", getMachineOptions(delegator, taskFixedAssetId));

                boolean canStart = false;
                if ("PRUN_SCHEDULED".equals(taskStatusId) || "PRUN_DOC_PRINTED".equals(taskStatusId)) {
                    canStart = !hasEarlierOpenTask(delegator, productionRunId, task.getLong("priority"));
                }
                row.put("canStart", canStart);
                boolean running = "PRUN_RUNNING".equals(taskStatusId);
                row.put("canDeclare", running);
                row.put("canComplete", running);

                row.put("components", getTaskComponents(delegator, task.getString("workEffortId")));

                tasks.add(row);
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error building shop floor task list: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        List<Map<String, Object>> workCenters = new ArrayList<>();
        try {
            for (Map.Entry<String, String> entry : workCenterNames.entrySet()) {
                Map<String, Object> workCenter = new HashMap<>();
                workCenter.put("fixedAssetId", entry.getKey());
                workCenter.put("fixedAssetName", entry.getValue());
                workCenter.put("isLine", UtilValidate.isNotEmpty(getLineMembers(delegator, entry.getKey())));
                workCenters.add(workCenter);
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error checking work centers for production lines: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        Map<String, Object> result = ServiceUtil.returnSuccess();
        result.put("tasks", tasks);
        result.put("workCenters", workCenters);
        return result;
    }

    /** Returns true if an earlier (lower priority) task of the run is not completed, closed, or cancelled. */
    private static boolean hasEarlierOpenTask(Delegator delegator, String productionRunId, Long priority) throws GenericEntityException {
        List<EntityCondition> earlierConds = new ArrayList<>();
        earlierConds.add(EntityCondition.makeCondition("workEffortParentId", productionRunId));
        earlierConds.add(EntityCondition.makeCondition("workEffortTypeId", "PROD_ORDER_TASK"));
        earlierConds.add(EntityCondition.makeCondition("currentStatusId", EntityOperator.NOT_IN,
                UtilMisc.toList("PRUN_COMPLETED", "PRUN_CLOSED", "PRUN_CANCELLED")));
        if (priority != null) {
            earlierConds.add(EntityCondition.makeCondition("priority", EntityOperator.LESS_THAN, priority));
        }
        List<GenericValue> earlierOpenTasks = EntityQuery.use(delegator).from("WorkEffort")
                .where(EntityCondition.makeCondition(earlierConds)).queryList();
        return !earlierOpenTasks.isEmpty();
    }

    /** Returns the required components of a production run task with the issued quantity summed from inventory assignments. */
    private static List<Map<String, Object>> getTaskComponents(Delegator delegator, String taskId) throws GenericEntityException {
        List<Map<String, Object>> components = new ArrayList<>();
        List<GenericValue> componentGoods = EntityQuery.use(delegator).from("WorkEffortGoodStandard")
                .where("workEffortId", taskId, "workEffortGoodStdTypeId", "PRUNT_PROD_NEEDED").queryList();
        for (GenericValue componentGood : componentGoods) {
            String componentProductId = componentGood.getString("productId");
            Map<String, Object> component = new HashMap<>();
            component.put("productId", componentProductId);
            GenericValue componentProduct = EntityQuery.use(delegator).from("Product").where("productId", componentProductId).queryOne();
            component.put("internalName", componentProduct != null ? componentProduct.getString("internalName") : null);
            component.put("estimatedQuantity", componentGood.get("estimatedQuantity"));

            List<GenericValue> assigns = EntityQuery.use(delegator).from("WorkEffortAndInventoryAssign")
                    .where("workEffortId", taskId, "productId", componentProductId).queryList();
            BigDecimal issuedQuantity = BigDecimal.ZERO;
            for (GenericValue assign : assigns) {
                BigDecimal assignQty = assign.getBigDecimal("quantity");
                if (assignQty != null) {
                    issuedQuantity = issuedQuantity.add(assignQty);
                }
            }
            component.put("issuedQuantity", issuedQuantity);
            components.add(component);
        }
        return components;
    }

    /** Converts setup + run time (in millis) to whole minutes; perUnit multiplies the run time by the run quantity. */
    private static Long computeMinutes(Double setupMillis, Double milliSeconds, BigDecimal quantityToProduce, boolean perUnit) {
        double setup = (setupMillis != null) ? setupMillis : 0.0;
        double time = (milliSeconds != null) ? milliSeconds : 0.0;
        double total;
        if (perUnit) {
            double qty = (quantityToProduce != null) ? quantityToProduce.doubleValue() : 0.0;
            total = setup + (time * qty);
        } else {
            total = setup + time;
        }
        return (long) (total / 60000);
    }

    /**
     * Switches the machine (or line) assigned to a production run task.
     *
     * <p>The new asset is accepted when it is the task's current asset, the current asset's parent
     * (a line), a child of the current asset (a line member), or a sibling under the same parent line.</p>
     */
    public static Map<String, Object> switchProductionRunTaskMachine(DispatchContext dctx, Map<String, ? extends Object> context) {
        Delegator delegator = dctx.getDelegator();
        Locale locale = (Locale) context.get("locale");
        GenericValue userLogin = (GenericValue) context.get("userLogin");
        Map<String, Object> parameters = UtilGenerics.cast(context);

        if (!dctx.getSecurity().hasEntityPermission("MANUFACTURING", "_UPDATE", userLogin)) {
            return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingUpdatePermissionError", locale));
        }

        String workEffortId = (String) parameters.get("workEffortId");
        String fixedAssetId = (String) parameters.get("fixedAssetId");

        GenericValue task;
        String currentFixedAssetId;
        try {
            task = EntityQuery.use(delegator).from("WorkEffort").where("workEffortId", workEffortId).queryOne();
            if (task == null) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunTaskNotFound",
                        UtilMisc.toMap("productionRunTaskId", workEffortId), locale));
            }
            String taskStatusId = task.getString("currentStatusId");
            if ("PRUN_COMPLETED".equals(taskStatusId) || "PRUN_CLOSED".equals(taskStatusId)) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingProductionRunTaskNotRunning",
                        UtilMisc.toMap("productionRunTaskId", workEffortId), locale));
            }
            currentFixedAssetId = task.getString("fixedAssetId");
            if (!isAcceptableMachineSwitch(delegator, currentFixedAssetId, fixedAssetId)) {
                return ServiceUtil.returnError(UtilProperties.getMessage(RESOURCE, "ManufacturingMachineNotInLine",
                        UtilMisc.toMap("fixedAssetId", fixedAssetId), locale));
            }
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error looking up production run task for machine switch: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        Timestamp now = UtilDateTime.nowTimestamp();
        try {
            task.set("fixedAssetId", fixedAssetId);
            delegator.store(task);

            String statusId = "FA_ASGN_ASSIGNED";
            String availabilityStatusId = null;
            BigDecimal allocatedCost = null;
            String comments = null;
            if (UtilValidate.isNotEmpty(currentFixedAssetId)) {
                List<GenericValue> currentAssigns = EntityQuery.use(delegator).from("WorkEffortFixedAssetAssign")
                        .where("workEffortId", workEffortId, "fixedAssetId", currentFixedAssetId, "thruDate", null).queryList();
                for (GenericValue currentAssign : currentAssigns) {
                    statusId = currentAssign.getString("statusId");
                    availabilityStatusId = currentAssign.getString("availabilityStatusId");
                    allocatedCost = currentAssign.getBigDecimal("allocatedCost");
                    comments = currentAssign.getString("comments");
                    currentAssign.set("thruDate", now);
                    delegator.store(currentAssign);
                }
            }

            GenericValue newAssign = delegator.makeValue("WorkEffortFixedAssetAssign");
            newAssign.set("workEffortId", workEffortId);
            newAssign.set("fixedAssetId", fixedAssetId);
            newAssign.set("fromDate", now);
            newAssign.set("statusId", statusId);
            newAssign.set("availabilityStatusId", availabilityStatusId);
            newAssign.set("allocatedCost", allocatedCost);
            newAssign.set("comments", comments);
            delegator.create(newAssign);
        } catch (GenericEntityException e) {
            Debug.logError(e, "Error switching production run task machine: " + e.getMessage(), MODULE);
            return ServiceUtil.returnError(e.getMessage());
        }

        return ServiceUtil.returnSuccess();
    }

    /** Returns true when newFixedAssetId is acceptable as a replacement for currentFixedAssetId: itself, its parent, a child, or a sibling. */
    private static boolean isAcceptableMachineSwitch(Delegator delegator, String currentFixedAssetId, String newFixedAssetId) throws GenericEntityException {
        if (UtilValidate.isEmpty(newFixedAssetId)) {
            return false;
        }
        if (newFixedAssetId.equals(currentFixedAssetId)) {
            return true;
        }
        if (UtilValidate.isEmpty(currentFixedAssetId)) {
            return false;
        }
        GenericValue newAsset = EntityQuery.use(delegator).from("FixedAsset").where("fixedAssetId", newFixedAssetId).queryOne();
        if (newAsset == null) {
            return false;
        }
        GenericValue currentAsset = EntityQuery.use(delegator).from("FixedAsset").where("fixedAssetId", currentFixedAssetId).queryOne();
        if (currentAsset == null) {
            return false;
        }
        String currentParentId = currentAsset.getString("parentFixedAssetId");
        String newParentId = newAsset.getString("parentFixedAssetId");
        // new asset is the line the current machine belongs to
        if (newFixedAssetId.equals(currentParentId)) {
            return true;
        }
        // new asset is a member machine of the current line
        if (currentFixedAssetId.equals(newParentId)) {
            return true;
        }
        // new asset is a sibling member under the same line
        return UtilValidate.isNotEmpty(currentParentId) && currentParentId.equals(newParentId);
    }

    /** Returns the member FixedAsset rows of a production line (empty if fixedAssetId is not a line). */
    private static List<GenericValue> getLineMembers(Delegator delegator, String fixedAssetId) throws GenericEntityException {
        if (UtilValidate.isEmpty(fixedAssetId)) {
            return new ArrayList<>();
        }
        return EntityQuery.use(delegator).from("FixedAsset").where("parentFixedAssetId", fixedAssetId).orderBy("fixedAssetId").queryList();
    }

    /**
     * Returns the machine options (id, name) an operator may switch a task to: the members of the task's
     * fixed asset when it is itself a line, otherwise the members of its parent line when it belongs to one.
     */
    private static List<Map<String, Object>> getMachineOptions(Delegator delegator, String fixedAssetId) throws GenericEntityException {
        List<Map<String, Object>> options = new ArrayList<>();
        if (UtilValidate.isEmpty(fixedAssetId)) {
            return options;
        }
        List<GenericValue> members = getLineMembers(delegator, fixedAssetId);
        if (UtilValidate.isEmpty(members)) {
            GenericValue asset = EntityQuery.use(delegator).from("FixedAsset").where("fixedAssetId", fixedAssetId).queryOne();
            String parentFixedAssetId = (asset != null) ? asset.getString("parentFixedAssetId") : null;
            if (UtilValidate.isNotEmpty(parentFixedAssetId)) {
                members = getLineMembers(delegator, parentFixedAssetId);
            }
        }
        for (GenericValue member : members) {
            Map<String, Object> option = new HashMap<>();
            option.put("fixedAssetId", member.get("fixedAssetId"));
            option.put("fixedAssetName", member.get("fixedAssetName"));
            options.add(option);
        }
        return options;
    }

}
